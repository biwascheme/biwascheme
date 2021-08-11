import { nil } from "../../header.js";
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError } from "../error.js"
import { List, array_to_list, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { Environment } from "./environment.js"
import { isMacro } from "./macro.js"

// A R7RS library.
class Library {
  // Maps (stringified) library spec to Library
  static _libraryTable = new Map();
  // Holds spec (not Library)
  static currentLibrary = null;
  static featureList = [];

  static _assocLibrary(spec) {
    return Library._libraryTable[spec.to_write()];
  }

  // Evaluate `thunk` with the library specified with `spec` as the current
  // library
  static withLibrary(spec, thunk) {
    const origLib = this.currentLibrary;
    this.currentLibrary = spec;
    // TODO: original impl uses `parameterize`
    const ret = Environment.withToplevelEnvironment(this._libraryEnvironment(spec), thunk);
    this.currentLibrary = origLib;
    return ret;
  }

  static makeLibrary(spec) {
    const prefix = spec.map(x => to_write(x)).join(".");
    const env = Environment.makeToplevelEnvironment(sym => {
      return Sym(`${prefix}:${sym.name}`)
    }, prefix);
    this._libraryTable[spec.to_write()] = new Library(env);
  }

  // Find the library corresponds to `spec` and return its `Environment`.
  static _libraryEnvironment(spec) {
    const libObj = this._assocLibrary(spec);
    if (!libObj) throw new BiwaError("_libraryEnvironment: library not found", spec);
    return libObj.environment;
  }

  // Find the library corresponds to `spec` and return its `Exports`.
  static _libraryExports(spec) {
    const libObj = this._assocLibrary(spec);
    if (!libObj) throw new BiwaError("_libraryExports: library not found", spec);
    return libObj.exports;
  }

  // Returns whether there is a library specified with `spec`
  static libraryExists(spec) {
    const libObj = this._assocLibrary(spec);
    return !!libObj;
  }

  static expandLibrary(form) {
    const spec = form.cdr.car;
    this.makeLibrary(spec);
    return this.withLibrary(spec, () => {
      const decls = form.cdr.cdr;
      const forms = decls.map(x => this._interpretLibraryDeclaration(x).to_array()).flat();
      return this._expandToplevel(forms);
    });
  }

  static _interpretLibraryDeclaration(decl) {
    switch (decl.car) {
      case Sym("begin"):
        return decl.cdr;
      case Sym("import"):
        decl.cdr.forEach(x => this.libraryImport(x));
        return nil
      case Sym("export"):
        decl.cdr.forEach(x => this.libraryExport(x));
        return nil
      case Sym("cond-expand"):
        return this._interpretCondExpand(decl.cdr);
      case Sym("include"):
        TODO
      case Sym("include-library-declarations"):
        TODO
      default:
        throw new BiwaError("_interpretLibraryDeclaration: unknown decl.car", decl);
    }
  }

  // Import the library specified with `spec` into the current environment.
  static libraryImport(spec) {
    const nameMap = this._makeNameMap(spec);
    const env = Environment.currentToplevelEnvironment;
    nameMap.forEach(pair => env.installToplevelBinding(pair[0], pair[1])); // r7expander says "TODO redefinition of macros"
  }

  static _makeNameMap(spec) {
    switch (spec.car) {
      case Sym("prefix"):
      case Sym("only"):
      case Sym("except"):
      case Sym("rename"):
        TODO
      default:
        const exports = this._libraryExports(spec);
        const env = this._libraryEnvironment(spec);
        const ret = [];
        exports.forEach((nickname, id) => {
          ret.push([nickname, env.assq(id)])
        });
        return ret;
    }
  }

  // Register an export item of the current library
  // `spec` is either a symbol or `(id nickname)`
  static libraryExport(spec) {
    let id, nickname;
    if (isSymbol(spec)) {
      id = spec; nickname = spec;
    }
    else {
      id = spec.cdr.car; nickname = spec.cdr.cdr.car;
    }
    const obj = this._assocLibrary(this.currentLibrary);
    obj.addExport(nickname, id);
  }

  static _interpretCondExpand(clauses) {
    TODO
  }

  static expandProgram(forms) {
    const env = Environment.makeToplevelEnvironment(sym => {
      return Sym(`user:${sym.name}`)
    }, "user");
    return Environment.withToplevelEnvironment(env, () => {
      let _forms = forms;
      while (true) {
        if (isPair(_forms.car) && 
            _forms.car.car === Sym("import") &&
            env.assq(Sym("import")) === Sym("user:import")) { // r7expander/library.sld says 'FIXME'
          _forms.car.cdr.forEach(x => this.libraryImport(x));
          _forms = _forms.cdr;
        }
        else {
          return this._expandToplevel(_forms);
        }
      }
    });
  }

  // Expand toplevel forms (eg. user program, library body, etc.)
  static _expandToplevel(forms) {
    const expanded = forms.mapList(x => Environment.expand(x));
    const flattened = this._flattenBegin(Cons(Sym("begin"), expanded));
    const postProcessed = flattened.mapList(x => this._postExpand(x, true));
    if (postProcessed.cdr == nil) {
      return postProcessed.car;
    }
    else {
      return Cons(Sym("begin"), postProcessed);
    }
  }

  static _flattenBegin(form) {
    if (isPair(form) && form.car === Sym("begin")) {
      return array_to_list(
        form.cdr.to_array().flatMap(x => this._flattenBegin(x))
      );
    }
    else {
      return List(form);
    }
  }

  static _postExpand(form, allowDefinition) {
    switch (true) {
      case isSymbol(form):
        return form;
      case isVector(form):
        return List(Sym("quote"), form);
      case isMacro(form):
        throw new BiwaError("_postExpand: invalid use of keyword", form);
      case !isList(form):
        return form;
    }
    // When form is a list
    switch (form.car) {
      case Sym("quote"):
        return form;
      case Sym("begin"):
        if (form.cdr === nil) throw new BiwaError("_postExpand: malformed begin", form);
        return Cons(Sym("begin"),
                    form.cdr.mapList(form => this._postExpand(form, false)));
      case Sym("define"):
        if (!allowDefinition) throw new BiwaError("_postExpand: invalid definition", form);
        return List(Sym("define"), form.cdr.car,
          this._postExpand(form.cdr.cdr.car, false));
      case Sym("define-record-type"):
        TODO
      case Sym("lambda"):
        return Cons(Sym("lambda"),
                 Cons(form.cdr.car,
                   form.cdr.cdr.mapList(x => this._postExpand(x, true))));
      default:
        return form.mapList(x => this._postExpand(x, false));
    }
  }

  constructor(environment) {
    this.environment = environment;
    this.exports = new Map();
  }

  // Register `id` as exported item of this library
  addExport(nickname, id) {
    this.exports.set(nickname, id);
  }

  to_write() {
    return "#<Library>"
  }
}

const isLibrary = obj => obj instanceof Library;

export { Library, isLibrary };
