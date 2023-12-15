import Call from "../call.js"
import { inspect, to_write } from "../_writer.js";
import { List } from "../pair.js"
import { Sym } from "../symbol.js"
import { isFunction } from "../_types.js"

// A macro expander (pair of transformer and environment)
class Macro {
  // original: make-expander
  constructor(dbgName, environment, transformer, isCoreSyntax) {
    this.environment = environment; // An `Environment`
    this.dbgName = dbgName; // String (for debugging use; may be empty)
    this.transformer = transformer; // A MacroTransformer
    // True if this is a core syntax like `if`, `begin`, etc.
    this.isCoreSyntax = isCoreSyntax;
  }

  toString() {
    return `#<Macro ${this.dbgName} ${this.transformer}>`
  }
}

const isMacro = obj => obj instanceof Macro;

export { Macro, isMacro };
