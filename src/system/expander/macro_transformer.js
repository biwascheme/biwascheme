import Call from "../call.js"
import { Environment, identifierEquals } from "./environment.js"

// TODO: impl. `capture-syntactic-environment` in r7expander/syntactic-closure.sld
function makeScMacroTransformer(proc) {
  return function(form, env, metaEnv) {
    return new Call(proc, [form, env], result => {
      return metaEnv.expand(result);
    });
  };
}

function makeRscMacroTransformer(proc) {
  return function(form, env, metaEnv) {
    return new Call(proc, [form, metaEnv], result => {
      return env.expand(result);
    });
  };
}

function makeErMacroTransformer(proc) {
  return async function(form, env, metaEnv) {
    const table = new Map();
    const rename = ([x]) => {
      if (table.has(x)) {
        return table.get(x)
      } else {
        const id = metaEnv.makeIdentifier(x);
        table.set(x, id);
        return id
      }
    };
    const compare = ([x, y]) => identifierEquals(x, env, y, env);
    return new Call(proc, [form, rename, compare], result => {
      return env.expand(result);
    });
  };
}

export { makeScMacroTransformer, makeRscMacroTransformer, makeErMacroTransformer };
