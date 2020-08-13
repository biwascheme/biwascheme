import { nil } from "../header.js";
import { isSymbol, isPair, isList, isVector } from "./_types.js"
import { to_write } from "./_writer.js"
import { BiwaError, Bug } from "./error.js"
import { List, array_to_list, Cons } from "./pair.js"
import { Sym } from "./symbol.js"

class Environment {
  static currentToplevelEnvironment = null;
  static currentMetaEnvironment = null;

  static withToplevelEnvironment(topEnv, thunk) {
    const origEnv = this.currentToplevelEnvironment;
    this.currentToplevelEnvironment = topEnv;
    const ret = thunk();
    this.currentToplevelEnvironment = origEnv;
    return ret;
  }

  static withMetaEnvironment(metaEnv, thunk) {
    const origEnv = this.currentMetaEnvironment;
    this.currentMetaEnvironment = metaEnv;
    const ret = thunk();
    this.currentMetaEnvironment = origEnv;
    return ret;
  }

  static makeToplevelEnvironment(renamer, name="") {
    return new Environment(name, null, new Map(), renamer);
  }

  static expand(form, env=this.currentToplevelEnvironment) {
    let ret;
    if (isIdentifier(form)) {
      ret = this._expandIdentifier(form, env);
    }
    else if (isSyntacticClosure(form)) {
      ret = this._expandSyntacticClosure(form, env);
    }
    else if (isPair(form) && isList(form)) {
      if (isIdentifier(form.car)) {
        const e = this.expand(form.car);
        if (isExpander(e)) {
          ret = this._expandMacro(e, form, env);
        }
        else {
          const mapped = form.cdr.mapList(x => this.expand(x));
          ret = Cons(e, mapped);
        }
      }
      else {
        ret = form.mapList(x => this.expand(x));
      }
    }
    else if (!isPair(form)) {
      ret = form;
    }
    else {
      throw new BiwaError("expand: invalid expression", form);
    }
    console.log(to_write(form), "=>", to_write(ret));
    return ret;
  }

  static _expandMacro(expander, form, env) {
    return this.withMetaEnvironment(expander.environment, () =>
      // TODO: We cannot evaluate Scheme expr with this style (CPS needed)
      expander.transformer(form, env)
    );
  }

  static _expandSyntacticClosure(sc, env) {
    const newEnv = new Environment("", sc.environment);
    sc.freeNames.forEach(id => newEnv.set(id, expand(id, env)));
    expand(sc.form, newEnv);
  }

  static _expandIdentifier(id, env) {
    const found = env.assq(id);
    if (found) return found;
    return this._expandIdentifier(id.form, id.env);
  }

  constructor(name, base, frame=new Map(), renamer=null) {
    this.name = name,
    this.base = base;
    this.frame = frame;
    this.renamer = renamer;
  }

  get(key) {
    return this.frame.get(key);
  }

  set(key, value) {
    this.frame.set(key, value);
  }

  enclosingEnvironment() {
    return this.base;
  }

  isToplevel() {
    return this.enclosingEnvironment() === null;
  }

  assq(id) {
    const found = this.get(id);
    if (found) return found;
    if (this.isToplevel()) {
      if (isSymbol(id)) {
        const newName = this.renamer(id);
        this.set(id, newName);
        return this.assq(id); // Q: Why not just return [id, newName] ?
      }
      else {
        return null;
      }
    }
    else {
      return this.enclosingEnvironment().assq(id);
    }
  }

  // assq-environment + set-cdr!
  update(id, expander) {
    if (this.get(id)) {
      this.set(id, expander);
    }
    else if (this.isToplevel()) {
      if (isSymbol(id)) {
        this.set(id, expander);  // Not sure
      }
      else {
        throw new BiwaError("update: failed", id) // Not sure
      }
    }
    else {
      this.enclosingEnvironment().update(id, expander);
    }
  }

  installToplevelBinding(id, name) {
    if (!this.isToplevel()) {
      throw new Bug("installToplevelBinding: not a toplevel env")
    }
    this.set(id, name);
  }

  closeSyntax(form) {
    return new SyntacticClosure(this, [], form);
  }

  makeIdentifier(id) {
    return closeSyntax(id);
  }

  extend(id) {
    if (this.isToplevel() && isSymbol(id)) return;
    if (this.get(id)) {
      // Q: or Bug?
      throw new BiwaError("extend: duplicate binding", id);
    }
    this.set(id, generateName(id));
  }

  extended(ids) {
    const newEnv = new Environment("", this);
    ids.forEach(id => newEnv.extend(id));
    return newEnv;
  }

  installExpander(keyword, expander) {
    this.extend(keyword);
    this.update(keyword, expander);
  }

  expand(form) {
    return Environment.expand(form, this);
  }
}

class SyntacticClosure {
  // freeNamesはArrayか？
  constructor(env, freeNames, form) {
    this.environment = env;
    this.freeNames = freeNames;
    this.form = form;
  }
}

const isSyntacticClosure = obj => obj instanceof SyntacticClosure;

class Expander {
  constructor(transformer, environment) {
    this.transformer = transformer;
    this.environment = environment;
  }
}

const isExpander = obj => obj instanceof Expander;

class Library {
  // Maps library spec to Library
  static _libraryTable = new Map();
  // Holds spec (not Library)
  static currentLibrary = null;
  static featureList = [];

  static _assocLibrary(spec) {
    return Library._libraryTable[spec];
  }

  static withLibrary(spec, thunk) {
    const origLib = this.currentLibrary;
    this.currentLibrary = spec;
    const ret = Environment.withToplevelEnvironment(this._libraryEnvironment(spec), thunk);
    this.currentLibrary = origLib;
    return ret;
  }

  static makeLibrary(spec) {
    const prefix = spec.map(x => to_write(x)).join(".");
    const env = Environment.makeToplevelEnvironment(sym => {
      return Sym(`${prefix}:${sym.name}`)
    }, prefix);
    this._libraryTable[spec] = new Library(env);
  }

  static _libraryEnvironment(spec) {
    const libObj = this._assocLibrary(spec);
    if (!libObj) throw new BiwaError("_libraryEnvironment: library not found", spec);
    return libObj.environment;
  }

  static _libraryExports(spec) {
    const libObj = this._assocLibrary(spec);
    if (!libObj) throw new BiwaError("_libraryExports: library not found", spec);
    return libObj.exports;
  }

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
      return this.expandToplevel(forms);
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
          return this.expandToplevel(_forms);
        }
      }
    });
  }

  static expandToplevel(forms) {
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
      case isExpander(form):
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

  addExport(nickname, id) {
    this.exports.set(nickname, id);
  }
}

const isLibrary = obj => obj instanceof Library;

const unwrapSyntax = obj => {
  if (isSyntacticClosure(obj)) {
    return unwrapSyntax(obj.form);
  }
  else if (isPair(obj)) {
    return Cons(unwrapSyntax(obj.car), unwrapSyntax(obj.cdr));
  }
  else if (isVector(obj)) {
    return obj.map(x => unwrapSyntax(x));
  }
  else {
    return obj;
  }
}

const isIdentifier = obj => {
  if (isSymbol(obj)) return true;
  if (isSyntacticClosure(obj) && isIdentifier(obj.form)) return true;
  return false;
};

const identifierEquals = (id1, env1, id2, env2) => {
  TODO
};

// Return a new unique symbol
let generateNameN = 0;
const generateName = id => {
  const n = generateNameN;
  generateNameN++;
  return Sym(`%${unwrapSyntax(id)}.${n}`);
}

const libBsCore = List(Sym('biwascheme'), Sym('core'));
Library.makeLibrary(libBsCore);
Library.withLibrary(libBsCore, () => {
  const env = Environment.currentToplevelEnvironment;
  const ifExpander = new Expander((form, env) => {
    const l = form.to_array();
    switch (l.length) {
      case 3:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]));
      case 4:
        return List(Sym("if"), env.expand(l[1]), env.expand(l[2]), env.expand(l[3]));
      default:
        throw new BiwaError("ifExpander: malformed if", form);
    }
  }, env);
  env.installExpander(Sym("if"), ifExpander);
  Library.libraryExport(Sym("if"));
});

const libSchemeBase = List(Sym('scheme'), Sym('base'));
Library.makeLibrary(libSchemeBase);
Library.withLibrary(libSchemeBase, () => {
  Library.libraryImport(libBsCore);
  Library.libraryExport(Sym("if"));
});

export { Environment, Library };
