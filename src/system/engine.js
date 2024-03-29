import { undef } from "../header.js";
import { inspect } from "./_writer.js";
import { isFunction } from "./_types.js"
import { BiwaError } from "./error.js"
import { Cons, Pair, isPair, array_to_list } from "./pair.js"
import { Sym } from "./symbol.js"
import Compiler from "./compiler.js"
import Interpreter from "./interpreter.js"
import Parser from "./parser.js";
import { Environment } from "./expander/environment.js"
import { Expander } from "./expander/expander.js"
import { Library } from "./expander/library.js"
import { Libraries, stdLibraries } from "./libraries.js"
/** @typedef {import("./_types.js").Form} Form */
/** @typedef {import("./_types.js").Procedure} Procedure */
/** @typedef {Pair} List */

class Engine {
  constructor() {
    /** @type {Libraries} */
    this.libraries = stdLibraries;
    this.currentLibrary = null;
    this.currentToplevelEnvironment = null;
    this.currentMetaEnvironment = null;
    this.compiler = new Compiler();
    this.expander = new Expander(this);
    this.vm = {};
  }

  /** Get the library specified with `spec`
   * Error if not loaded to this engine
   * @param {Pair} spec
   * @returns {Library}
   */
  getLibrary(spec) {
    return this.libraries.get(spec);
  }

  /** Compile a Scheme program and execute it
   * @param {String} scmTxt
   * @returns {Promise<any>}
   */
  async run(scmTxt) {
    const parser = new Parser(scmTxt);
    const exprs = [];
    let expr;
    while ((expr = parser.getObject()) != Parser.EOS) {
      exprs.push(expr);
    }
    return this.executeScm(array_to_list(exprs)); 
  }

  /** Compile a Scheme program and execute it
   * @param {List} forms
   * @returns {Promise<any>}
   */
  async executeScm(forms) {
    const expanded = await this.expandToplevelProgram(forms);
    return this.evalExpandedForm(expanded);
  }

  /** 
   * @param {List} forms
   * @returns {Promise<any>}
   */
  async evalExpandedForms(forms) {
    let ret = undef;
    for(let o = forms; o instanceof Pair; o=o.cdr){
      ret = await this.evalExpandedForm(o.car);
    }
    return ret;
  }

  /** 
   * @param {Form} form
   * @returns {Promise<any>}
   */
  async evalExpandedForm(form)
  {
    const vmcode = this.compiler.run(form);
    const intp = new Interpreter();
    intp.on_error = (e) => { throw e };
    return intp.evaluate_vmcode(vmcode); //TODO: this should return Promise
  }

  /** Invoke a procedure 
   * @param {Procedure} proc
   * @param {Array<any>} args An array of Scheme values
   * @returns {Promise<any>}
   */
  async invoke(proc, args) {
    if (typeof proc === "function") {
      return proc(args, this);
    } else {
      const intp = new Interpreter();
      intp.on_error = (e) => { throw e };
      return intp.invoke_closure(proc, args);
    }
  }

  /** 
   * @param {List} forms
   * @returns {Promise<Form>}
   */
  async expandToplevelProgram(forms) {
    const topEnv = Environment.makeToplevelEnvironment("user", "user:");
    return this.withToplevelEnvironment(topEnv, async () => {
      let _forms = forms;
      while (true) {
        if (isPair(_forms.car) && 
            _forms.car.car === Sym("import") &&
           topEnv.assq(Sym("import")) === Sym("user:import")) { // r7expander/library.sld says 'FIXME'
          _forms.car.cdr.forEach(x => topEnv.findAndImportLibrary(x, this));
          _forms = _forms.cdr;
        } else {
          return this.expander.expandToplevel(_forms);
        }
      }
    });
  }

  /**
   * @param {Environment} env
   * @param {function} jsThunk
   * @returns {Promise<any>}
   */
  async withToplevelEnvironment(env, jsThunk) {
    const origEnv = this.currentToplevelEnvironment;
    this.currentToplevelEnvironment = env;
    const result = await jsThunk();
    this.currentToplevelEnvironment = origEnv;
    return result;
  }

  /** Compile a Scheme program and register it as library
   * @param {Form} spec Library spec
   * @param {String} scmTxt Scheme program (must start with `(define-library ...`)
   */
  async defineLibrary(spec, scmTxt) {
    const lib = Library.create(spec);
    const parser = new Parser(scmTxt);
    const form = parser.getObject();
    const expandedForm = await this.expander.expandLibrary(form, lib)
    this.libraries.add(lib);
    await this.evalExpandedForms(expandedForm);
  }
}

export { Engine };
