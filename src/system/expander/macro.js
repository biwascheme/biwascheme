import Call from "../call.js"

// A macro expander (pair of transformer and environment)
class Macro {
  constructor(dbgName, environment, transformer) {
    this.environment = environment; // An `Environment`
    this.dbgName = dbgName; // String (for debugging use; may be empty)
    // Either of
    // - Js function `(form, env) => newForm`
    // - Scheme proc
    this.transformer = transformer;
  }

  transform(form, env, after) {
    return new Call(this.transformer, [form, env], after);
  }

  to_write() {
    return `#<Macro ${this.dbgName}>`
  }
}

const isMacro = obj => obj instanceof Macro;

export { Macro, isMacro };
