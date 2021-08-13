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

  constructor(environment) {
    this.environment = environment;
    this.exports = new Map();
  }

  static create(spec) {
    const prefix = spec.map(x => to_write(x)).join(".");
    const env = Environment.makeToplevelEnvironment(prefix, sym => {
      return Sym(`${prefix}:${sym.name}`)
    });
    return new Library(env);
  }

  // ---

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

//  // Find the library corresponds to `spec` and return its `Environment`.
//  static _libraryEnvironment(spec) {
//    const libObj = this._assocLibrary(spec);
//    if (!libObj) throw new BiwaError("_libraryEnvironment: library not found", spec);
//    return libObj.environment;
//  }

//  // Returns whether there is a library specified with `spec`
//  static libraryExists(spec) {
//    const libObj = this._assocLibrary(spec);
//    return !!libObj;
//  }

  // Called when loading a library from file (or something)
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

  import(lib) {
    this.environment.importLibrary(lib);
  }

  nameMap() {
    const ret = [];
    this.exports.forEach((nickname, id) => {
      ret.push([nickname, this.environment.assq(id)])
    });
    return ret;
  }

  // Register an export item of the current library
  // `spec` is either a symbol or `(id nickname)`
  export(spec) {
    let id, nickname;
    if (isSymbol(spec)) {
      id = spec; nickname = spec;
    } else {
      id = spec.cdr.car; nickname = spec.cdr.cdr.car;
    }
    this.addExport(nickname, id);
  }

  static _interpretCondExpand(clauses) {
    TODO
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
