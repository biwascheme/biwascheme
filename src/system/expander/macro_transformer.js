import Call from "../call.js"
import { Environment, identifierEquals } from "./environment.js"

function makeScMacroTransformer(proc) {
  return function(form, env) {
    return new Call(proc, [form, env], result => {
      return Environment.currentMetaEnvironment.expand(result);
    });
  };
}

function makeRscMacroTransformer(proc) {
  return function(form, env) {
    return new Call(proc, [form, Environment.currentMetaEnvironment], result => {
      return env.expand(result);
    });
  };
}

function makeErMacroTransformer(proc) {
  return function(form, env) {
    const table = new Map();
    const rename = ([x]) => {
      if (table.has(x)) {
        return table.get(x)
      } else {
        const id = Environment.currentMetaEnvironment.makeIdentifier(x);
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
