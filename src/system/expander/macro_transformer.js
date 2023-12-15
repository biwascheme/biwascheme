import { to_write, inspect } from "../_writer.js"
import { List, Pair, isPair, array_to_list } from "../pair.js"
import { Sym } from "../symbol.js"

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


class MacroTransformer {
  async transform(form, xp, env, metaEnv) {
    throw "override me"
  }
}

// For core syntaxes
class NativeMacroTransformer extends MacroTransformer {
  constructor(f) {
    super();
    this.f = f;
  }

  async transform(form, xp, env, metaEnv) {
    return this.f([form, xp, env, metaEnv])
  }

  toString() {
    return "#<NativeMacroTransformer>"
  }
}

// Explicit renaming macros
class ErMacroTransformer extends MacroTransformer {
  constructor(proc) {
    super();
    // A Scheme procedure i.e. either of
    // - (lambda (form rename compare) ...)
    // - function([form, rename, compare]){ ... }
    this.proc = proc;
  }

  async transform(form, xp, env, metaEnv) {
    const [compare_, rename_] = makeCompareAndRename(xp, env, metaEnv)
    const compare = async ([x, y]) => compare_(x, y);
    const rename = ([x]) => rename_(x);
    const result = await xp.engine.invoke(this.proc, [form, rename, compare]);
    return xp.expand(result);
  }

  toString() {
    return "#<ErMacroTransformer>"
  }
}

// Explicit renaming macros, but intended to be used via JavaScript
class JsErMacroTransformer extends MacroTransformer {
  constructor(func) {
    super();
    // async function(form, rename, compare){ ... }
    this.func = func;
  }

  async transform(form, xp, env, metaEnv) {
    const [compare, rename] = makeCompareAndRename(xp, env, metaEnv)
    const result = await this.func(form, rename, compare);
    return xp.expand(result);
  }

  toString() {
    return "#<JsErMacroTransformer>"
  }
}

export { MacroTransformer, NativeMacroTransformer, ErMacroTransformer, JsErMacroTransformer };
