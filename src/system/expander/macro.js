import Call from "../call.js"
import { isFunction } from "../_types.js"

// A macro expander (pair of transformer and environment)
class Macro {
  // original: make-expander
  constructor(dbgName, environment, transformer) {
    this.environment = environment; // An `Environment`
    this.dbgName = dbgName; // String (for debugging use; may be empty)
    // Either of
    // - Js function `([form, expander, env, metaEnv]) => newForm`
    // - Scheme proc
    this.transformer = transformer;
  }

  async transform(form, env, metaEnv, expander) {
    const args = [form, expander, env, metaEnv];
    if (isFunction(this.transformer)) {
      return this.transformer(args);
    } else {
      return expander.engine.invoke(this.transformer, args);
    }
  }

  to_write() {
    return `#<Macro ${this.dbgName}>`
  }
}

const isMacro = obj => obj instanceof Macro;

export { Macro, isMacro };
