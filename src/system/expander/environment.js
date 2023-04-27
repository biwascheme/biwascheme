import { undef } from "../../header.js"
import { isString, isSymbol, isVector } from "../_types.js"
import { to_write, inspect } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons, isPair, isList } from "../pair.js"
import { Sym, BiwaSymbol } from "../symbol.js"
import { SyntacticClosure, isSyntacticClosure, isIdentifier, unwrapSyntax } from "./syntactic_closure.js"
import { isMacro } from "./macro.js"

/** @typedef {BiwaSymbol | SyntacticClosure} Identifier */

/**
 * Create unique string from an identifier
 * @param {Identifier} x
 */
function to_key(x) {
  if (isSymbol(x)) {
    return x.name;
  } else if (isIdentifier(x)) {
    const form = unwrapSyntax(x);
    const envId = x.environment.id;
    //console.log(`-- ${to_write(form)} ${envId}`);
    return `${to_write(form)} ${envId}`; // Is this really unique?
  } else {
    throw new Bug(`not an identifier: ${inspect(x)}`);
  }
}

/** @type {number} */
let lastId = 0;

class Environment {
  /**
   * Create a toplevel binding
   * @param {?string} name
   * @param {?string} prefix (pass `null` when specifying renamer)
   * @param {?(BiwaSymbol) => BiwaSymbol} renamer
   */
  static makeToplevelEnvironment(name, prefix = null, renamer = null) {
    if (prefix !== null) {
      renamer = sym => Sym(`${prefix}${sym.name}`);
    }
    return new Environment(name, null, new Map(), renamer);
  }

  /**
   * @param {string} name
   * @param {?Environment} base
   * @param {?(BiwaSymbol) => BiwaSymbol} renamer
   */
  constructor(name, base, frame=new Map(), renamer=null) {
    this.id = (++lastId); 
    this.name = name; // String (optional; may be `""`)
    this.base = base; // Enclosing `Environment`. `null` if this is toplevel
    this.frame = frame; // Key-value pair (A js `Map`)
    // `null` or js function `(BiwaSymbol) => BiwaSymbol`
    this.renamer = renamer;
  }

  toString() {
    return `#<Environment ${this.name}>`
  }

  has(key) {
    return this.frame.has(to_key(key));
  }

  // Get the value bound to `key`
  get(key) {
    return this.frame.get(to_key(key));
  }

  // Set the value bound to `key`
  set(key, value) {
    this.frame.set(to_key(key), value);
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
  // Returns `#f` if not found in toplevel environment
  // original: assq-environment
  assq(id) {
    let ret;
    if (this.has(id)) {
      ret = this.get(id);
    } else if (!this.isToplevel()) {
      ret = this.enclosingEnvironment().assq(id);
    } else {
      if (isSymbol(id)) {
        const newName = this.renamer(id); // Q: What if renamer is null
        this.set(id, newName);
        ret = newName; // Q: Original impl returns this.assq(id)
      }
      else {
        ret = false;
      }
    }
    //console.log(`- assq ${id} in ${this} => ${to_write(ret)}`)
    return ret;
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
    return this.closeSyntax(id);
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

  /* Create an enviroment by adding `ids` to this enviroment
   * original: `extend-enviroment`
   * @param {ids} [Identifier]
   * @return Environment
   */
  extended(ids) {
    const newEnv = new Environment("", this);
    ids.forEach(id => newEnv.extend(id));
    return newEnv;
  }

  clone() {
    return this.extended([]);
  }

  // Bind `expander` to a `keyword`.
  // Error if `keyword` is already bound
  installExpander(keyword, expander) {
    if (!isSymbol(keyword)) throw new Bug(`expected String but got ${inspect(keyword)}`)
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

// Returns a newly created unique symbol
let generateNameN = 0;
const generateName = id => {
  const n = generateNameN;
  generateNameN++;
  return Sym(`%${unwrapSyntax(id)}.${n}`);
}

export { Environment };