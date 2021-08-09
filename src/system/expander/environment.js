import { isSymbol, isPair, isList, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons } from "../pair.js"
import { SyntacticClosure, isSyntacticClosure } from "./syntactic_closure.js"
import { isExpander } from "./expander.js"

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

const isIdentifier = obj => {
  if (isSymbol(obj)) return true;
  if (isSyntacticClosure(obj) && isIdentifier(obj.form)) return true;
  return false;
};

// Return a new unique symbol
let generateNameN = 0;
const generateName = id => {
  const n = generateNameN;
  generateNameN++;
  return Sym(`%${unwrapSyntax(id)}.${n}`);
}

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


export { Environment };
