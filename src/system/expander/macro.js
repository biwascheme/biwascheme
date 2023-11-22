import Call from "../call.js"
import { inspect } from "../_writer.js";
import { List } from "../pair.js"
import { Sym } from "../symbol.js"
import { isFunction } from "../_types.js"

function makeCompareAndRename(xp, env, metaEnv) {
  const compare = async (x, y) => xp.identifierEquals(x, env, y, env);
  const table = new Map();
  const rename = (x) => {
    if (table.has(x)) {
      return table.get(x)
    } else {
      const id = metaEnv.makeIdentifier(x);
      table.set(x, id);
      return id
    }
  };
  return [compare, rename]
}

// A macro expander (pair of transformer and environment)
class Macro {
  // original: make-expander
  constructor(dbgName, environment, transformer, isCoreSyntax) {
    this.environment = environment; // An `Environment`
    this.dbgName = dbgName; // String (for debugging use; may be empty)
    // Either of
    // - Js function `([form, expander, env, metaEnv]) => newForm`
    // - Scheme proc `(lambda (form rename compare)) ... newForm)`
    this.transformer = transformer;
    // True if this is a core syntax like `if`, `begin`, etc.
    this.isCoreSyntax = isCoreSyntax;
  }

  async transform(form, env, metaEnv, expander) {
    if (isFunction(this.transformer)) {
      // transformer is a JS function
      const args = [form, expander, env, metaEnv];
      return this.transformer(args);
    } else {
      // transformer is a Scheme proc
      const [compare_, rename_] = makeCompareAndRename(expander, env, metaEnv)
      const compare = async ([x, y]) => await compare_(x, y)
      const rename = ([x]) => rename_(x)
      const args = List(List(Sym("quote"), form), rename, compare);
      const expanded = await expander.engine.invoke(this.transformer, args);
      //console.log(inspect(form), "~>", inspect(expanded));
      return expanded;
    }
  }

  toString() {
    return `#<Macro ${this.dbgName}>`
  }
}

const isMacro = obj => obj instanceof Macro;

export { Macro, isMacro, makeCompareAndRename };
