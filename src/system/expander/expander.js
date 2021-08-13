import { undef, nil } from "../../header.js"
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons, List, isPair, isList, array_to_list } from "../pair.js"
import { Sym } from "../symbol.js"
import { SyntacticClosure, isSyntacticClosure } from "./syntactic_closure.js"
import { isMacro } from "./macro.js"

class Expander {
  constructor(engine) {
    this.engine = engine;
  }

  // Expand toplevel forms (eg. user program, library body, etc.)
  async expandToplevel(forms) {
    const expanded = await forms.mapAsync(x => this.expand(x));
    const flattened = this._flattenBegin(Cons(Sym("begin"), expanded));
    const postProcessed = flattened.mapList(x => this._postExpand(x, true));
    if (postProcessed.cdr == nil) {
      return postProcessed.car;
    }
    else {
      return Cons(Sym("begin"), postProcessed);
    }
  }

  _flattenBegin(form) {
    if (isPair(form) && form.car === Sym("begin")) {
      return array_to_list(
        form.cdr.to_array().flatMap(x => this._flattenBegin(x))
      );
    }
    else {
      return List(form);
    }
  }

  _postExpand(form, allowDefinition) {
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

  async expand(form, env=this.engine.currentToplevelEnvironment) {
    let ret;
    if (isIdentifier(form)) {
      ret = this._expandIdentifier(form, env);
    }
    else if (isSyntacticClosure(form)) {
      ret = this._expandSyntacticClosure(form, env);
    }
    else if (isPair(form) && isList(form)) {
      if (isIdentifier(form.car)) {
        const e = await this.expand(form.car);
        if (isMacro(e)) {
          ret = await this._expandMacro(e, form, env);
        }
        else {
          const mapped = await form.cdr.mapAsync(x => this.expand(x));
          ret = Cons(e, mapped);
        }
      }
      else {
        ret = await form.mapAsync(x => this.expand(x));
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

  _expandIdentifier(id, env) {
    const found = env.assq(id);
    if (found) return found;
    return this._expandIdentifier(id.form, id.env);
  }

  async _expandSyntacticClosure(sc, env) {
    const frame = new Map();
    sc.freeNames.forEach(async id => frame[id] = await this.expand(id, env));
    const newEnv = new Environment("", sc.environment, frame);
    return await this.expand(sc.form, newEnv);
  }

  async _expandMacro(macro, form, env) {
    return await macro.transform(form, env, macro.environment, this);
  }
}

// Returns whether `obj` is an identifier
const isIdentifier = obj => {
  if (isSymbol(obj)) return true;
  if (isSyntacticClosure(obj) && isIdentifier(obj.form)) return true;
  return false;
};

export { Expander };
