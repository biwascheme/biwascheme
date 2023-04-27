import { Environment } from "./environment.js"

// Explicit-renaming macro
function makeErMacroTransformer(proc) {
  return async function([form, xp, env, metaEnv]) {
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
    const compare = (x, y) => xp.identifierEquals(x, env, y, env);
    const result = await xp.engine.invoke(proc, [form, rename, compare]);
    return xp.expand(result);
  };
}

export { makeErMacroTransformer };
