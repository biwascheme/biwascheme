import { undef } from "../../header.js"
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { SyntacticClosure, isSyntacticClosure } from "./syntactic_closure.js"
import { isMacro } from "./macro.js"

class Environment {
  // Create a toplevel binding
  static makeToplevelEnvironment(name, renamer) {
    return new Environment(name, null, new Map(), renamer);
  }

  constructor(dbgName, base, frame=new Map(), renamer=null) {
    this.dbgName = dbgName; // String (optional; may be `""`)
    this.base = base; // Enclosing `Environment`. `null` if this is toplevel
    this.frame = frame; // Key-value pair (A js `Map`)
    // `null` or js function `(BiwaSymbol) => BiwaSymbol`
    this.renamer = renamer;
  }

  // Get the value bound to `key`
  get(key) {
    return this.frame.get(key);
  }

  // Set the value bound to `key`
  set(key, value) {
    this.frame.set(key, value);
  }

  // Returns the enclosing environment. `null` if this is toplevel
  enclosingEnvironment() {
    return this.base;
  }

  // Returns whether this is toplevel environment
  isToplevel() {
    return this.enclosingEnvironment() === null;
  }

  // Get the value bound to `id`
  // Lookup outer environment if not found
  // Returns `#<undef>` if not found in toplevel environment
  // original: assq-environment
  assq(id) {
    const found = this.get(id);
    if (found) return found;
    if (this.isToplevel()) {
      if (isSymbol(id)) {
        const newName = this.renamer(id); // Q: What if renamer is null
        this.set(id, newName);
        return this.assq(id); // Q: Why not just return [id, newName] ?
      }
      else {
        return undef;
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

  // Create a syntactic closure form `this` and `form`
  closeSyntax(form) {
    return new SyntacticClosure(this, [], form);
  }

  // Create an identifier in this environment
  makeIdentifier(id) {
    return closeSyntax(id);
  }

  // Destructively add `id` to this environment
  // original: `extend-environment!`
  extend(id) {
    if (this.isToplevel() && isSymbol(id)) return;
    if (this.get(id)) {
      // Q: or Bug?
      throw new BiwaError("extend: duplicate binding", id);
    }
    this.set(id, generateName(id));
  }

  // Create an enviroment by adding `ids` to this enviroment
  // original: `extend-enviroment`
  extended(ids) {
    const newEnv = new Environment("", this);
    ids.forEach(id => newEnv.extend(id));
    return newEnv;
  }

  // Bind `expander` to a `keyword`.
  // Error if `keyword` is already bound
  installExpander(keyword, expander) {
    this.extend(keyword);
    this.update(keyword, expander);
  }

  importLibrary(lib) {
    this._installNameMap(lib.nameMap());
  }

  _installNameMap(nameMap) {
    nameMap.forEach(([nickname, id]) => this.installToplevelBinding(nickname, id)); // r7expander says "TODO redefinition of macros"
  }

  // Import the library specified with `spec` into the current environment.
  findAndImportLibrary(spec, engine) {
    this._installNameMap(this._makeNameMap(spec, engine));
  }

  _makeNameMap(spec, engine) {
    switch (spec.car) {
      case Sym("prefix"):
      case Sym("only"):
      case Sym("except"):
      case Sym("rename"):
        TODO
      default:
        return engine.getLibrary(spec).nameMap();
    }
  }
}

// Strip syntax closures from `obj`
const unwrapSyntax = obj => {
  if (isSyntacticClosure(obj)) {
    return unwrapSyntax(obj.form);
  }
  else if (isPair(obj)) {
    return Cons(unwrapSyntax(obj.car), unwrapSyntax(obj.cdr));
  }
  else if (isVector(obj)) {
    return obj.map(unwrapSyntax);
  }
  else {
    return obj;
  }
}

// Returns whether `id1` in `env1` and `id2` in `id2` is the same identifier
const identifierEquals = (id1, env1, id2, env2) => {
  return env1.expand(id1) === env2.expand(id2);
};

// Returns a newly created unique symbol
let generateNameN = 0;
const generateName = id => {
  const n = generateNameN;
  generateNameN++;
  return Sym(`%${unwrapSyntax(id)}.${n}`);
}

export { Environment, identifierEquals };
