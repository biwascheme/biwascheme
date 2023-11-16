import { makeCompareAndRename } from "./macro.js";

// Explicit-renaming macro
function makeErMacroTransformer(proc) {
  return async function([form, xp, env, metaEnv]) {
    const [compare, rename] = makeCompareAndRename(xp, env, metaEnv)
    const result = await xp.engine.invoke(proc, [form, rename, compare]);
    return xp.expand(result);
  };
}

export { makeErMacroTransformer };
