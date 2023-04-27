import { undef, nil } from "../../header.js"
import { isSymbol, isVector } from "../_types.js"
import { to_write } from "../_writer.js"
import { BiwaError, Bug } from "../error.js"
import { Cons, List, isPair, isList, array_to_list, mapAsync } from "../pair.js"
import { Sym } from "../symbol.js"
import { isSyntacticClosure, isIdentifier } from "./syntactic_closure.js"
import { isMacro } from "./macro.js"

class Expander {
  constructor(engine) {
    this.engine = engine;
  }

  /** 
   * Expand toplevel forms (eg. user program, library body, etc.)
   * @param {List<Form>} forms
   */
  async expandToplevel(forms) {
    console.log("- expandToplevel", to_write(forms));
    const expandedForms = await forms.mapAsync(x => this.expand(x));
    // Merge nested `begin`
    const mergedForms = this._flattenBegin(Cons(Sym("begin"), expandedForms));
    const postProcessed = mergedForms.map(x => this._postExpand(x, true));
    if (postProcessed.length === 1) {
      // `begin` is not needed; just return the form
      return postProcessed[0];
    }
    else {
      // Wrap with `begin`
      return Cons(Sym("begin"), array_to_list(postProcessed));
    }
  }

  /**
   * Merge nested `begin` i.e. `(begin (begin ...`
   * @param {Form} form
   * @return {Array<Form>}
   */
  _flattenBegin(form) {
    if (isPair(form) && form.car === Sym("begin")) {
      return form.cdr.to_array().flatMap(x => this._flattenBegin(x));
    }
    else {
      return [form];
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
      ret = await this._expandSyntacticClosure(form, env);
    }
    else if (isPair(form) && isList(form)) {
      if (isIdentifier(form.car)) {
        const e = await this.expand(form.car);
        if (isMacro(e)) {
          ret = await this._expandMacro(e, form, env);
        } else {
          const mapped = await mapAsync(form.cdr, x => this.expand(x));
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
    if (!isSyntacticClosure(id)) {
      throw new BiwaError("_expandIdentifier: id not in env", id);
    }
    return this._expandIdentifier(id.form, id.environment);
  }

  async _expandSyntacticClosure(sc, env) {
    const frame = new Map();
    sc.freeNames.forEach(async id => frame[id] = await this.expand(id, env));
    const newEnv = new Environment("", sc.environment, frame);
    return this.expand(sc.form, newEnv);
  }

  async _expandMacro(macro, form, env) {
    return await macro.transform(form, env, macro.environment, this);
  }

  /** Called when loading a library from file (or something)
   * @param {form} Original program
   * @param {lib} Library
   * @return [string] The expanded Scheme program and a Library
   */
  async expandLibrary(form, lib) {
    const spec = form.cdr.car;
    return this.engine.withToplevelEnvironment(lib.environment, () => {
      const decls = form.cdr.cdr;
      const forms = decls.map(x => lib.interpretLibraryDeclaration(x, this.engine).to_array()).flat();
      return this.expandToplevel(array_to_list(forms));
    });
  }

  // Returns whether `id1` in `env1` and `id2` in `id2` is the same identifier
  async identifierEquals(id1, env1, id2, env2) {
    if (!isIdentifier(id1) || !isIdentifier(id2)) return false;

    const l = await this.expand(id1, env1);
    const r = await this.expand(id2, env2);
    return l === r;
  }
}

export { Expander };