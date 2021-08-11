import { undef } from "../../header.js"
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons, isPair, isList } from "../pair.js"
import { SyntacticClosure, isSyntacticClosure } from "./syntactic_closure.js"
import { isMacro } from "./macro.js"

class Environment {
  static currentToplevelEnvironment = null;
  static currentMetaEnvironment = null;

  static withToplevelEnvironment(topEnv, thunk) {
    // TODO: original impl uses `parameterize`
    const origEnv = this.currentToplevelEnvironment;
    this.currentToplevelEnvironment = topEnv;
    const ret = thunk(); // TODO: is thunk a scheme proc?
    this.currentToplevelEnvironment = origEnv;
    return ret;
  }

  static withMetaEnvironment(metaEnv, thunk) {
    // TODO: original impl uses `parameterize`
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
        if (isMacro(e)) {
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

  static _expandIdentifier(id, env) {
    const found = env.assq(id);
    if (found) return found;
    return this._expandIdentifier(id.form, id.env);
  }

  static _expandSyntacticClosure(sc, env) {
    const frame = new Map();
    sc.freeNames.forEach(id => frame[id] = env.expand(id));
    const newEnv = new Environment("", sc.environment, frame);
    return newEnv.expand(sc.form);
  }

  static _expandMacro(macro, form, env) {
    return this.withMetaEnvironment(macro.environment, () =>
      // TODO: We cannot evaluate Scheme expr with this style (CPS needed)
      macro.transform(form, env)
    );
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
        const newName = this.renamer(id);
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

  // Expands `form` under this enviroment
  expand(form) {
    return Environment.expand(form, this);
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

// Returns whether `obj` is an identifier
const isIdentifier = obj => {
  if (isSymbol(obj)) return true;
  if (isSyntacticClosure(obj) && isIdentifier(obj.form)) return true;
  return false;
};

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
