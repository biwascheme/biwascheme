import Call from "../call.js"
import { inspect } from "../_writer.js";
import { List } from "../pair.js"
import { Sym } from "../symbol.js"
import { isFunction } from "../_types.js"

// A macro expander (pair of transformer and environment)
class Macro {
  // original: make-expander
  constructor(dbgName, environment, transformer) {
    this.environment = environment; // An `Environment`
    this.dbgName = dbgName; // String (for debugging use; may be empty)
    // Either of
    // - Js function `([form, expander, env, metaEnv]) => newForm`
    // - Scheme proc `(lambda (form rename compare)) ... newForm)`
    this.transformer = transformer;
  }

  async transform(form, env, metaEnv, expander) {
    if (isFunction(this.transformer)) {
      const args = [form, expander, env, metaEnv];
      return this.transformer(args);
    } else {
      const rename = "TODO: rename"
      const compare = "TODO: compare"
      const args = List(List(Sym("quote"), form), rename, compare);
      const expanded = await expander.engine.invoke(this.transformer, args);
      console.log(inspect(form), "~>", inspect(expanded));
      return expanded;
    }
  }

  toString() {
    return `#<Macro ${this.dbgName}>`
  }
}

const isMacro = obj => obj instanceof Macro;

export { Macro, isMacro };
