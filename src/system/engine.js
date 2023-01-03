import { isFunction } from "./_types.js"
import { BiwaError } from "./error.js"
import { isPair, array_to_list } from "./pair.js"
import { Sym } from "./symbol.js"
import Compiler from "./compiler.js"
import Interpreter from "./interpreter.js"
import Parser from "./parser.js";
import { Environment } from "./expander/environment.js"
import { Expander } from "./expander/expander.js"
import { stdLibraries } from "./libraries.js"

class Engine {
  constructor() {
    this.libraries = stdLibraries;
    this.currentLibrary = null;
    this.currentToplevelEnvironment = null;
    this.currentMetaEnvironment = null;
    this.compiler = new Compiler();
    this.expander = new Expander(this);
    this.vm = {};
  }

  // Get the library specified with `spec`
  // Error if not loaded to this engine
  getLibrary(spec) {
    return this.libraries.get(spec);
  }

  /** Compile a Scheme program and execute it
   * @param {String} scmTxt
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
   * @param {List<Form>} forms
   */
  async executeScm(forms) {
    const expanded = await this.expandToplevelProgram(forms);
    const vmcode = this.compiler.run(expanded);
    const intp = new Interpreter();
    intp.on_error = (e) => { throw e };
    return intp.evaluate_vmcode(vmcode);
  }

  async invoke(proc, args) {
    if (isFunction(proc)) {
      return proc(args, this);
    } else {
      TODO
    }
  }

  async expandToplevelProgram(forms) {
    const topEnv = Environment.makeToplevelEnvironment(
      "user",
      sym => Sym(`user:${sym.name}`),
    );
    return this.withToplevelEnvironment(topEnv, async () => {
      let _forms = forms;
      while (true) {
        if (isPair(_forms.car) && 
            _forms.car.car === Sym("import") &&
           topEnv.assq(Sym("import")) === Sym("user:import")) { // r7expander/library.sld says 'FIXME'
          _forms.car.cdr.forEach(x => topEnv.findAndImportLibrary(x, this));
          _forms = _forms.cdr;
        }
        else {
          return this.expander.expandToplevel(_forms);
        }
      }
    });
  }

  async withToplevelEnvironment(env, jsThunk) {
    const origEnv = this.currentToplevelEnvironment;
    this.currentToplevelEnvironment = env;
    const result = await jsThunk();
    this.currentToplevelEnvironment = origEnv;
    return result;
  }
}

export { Engine };
