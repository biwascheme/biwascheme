// Expanders for core syntaxes
import { List, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { isIdentifier } from "./syntactic_closure.js"

const lambdaExpander = async ([form, xp]) => {
  const err = new BiwaError("malformed lambda", form);
  const l = form.to_array();
  if (l.length < 3) throw err;
  const formals = form.cadr(err);
  if (!isPair(formals)) throw err;
  const body = form.cddr(err);

  const newEnv = env.extend(formal-list);

  // TODO

  return Cons(Sym("lambda"),
           Cons(newFormals, newBody));
};

const installCore = (lib) => {
  lib.exportMacro(Sym("if"), async ([form, xp]) => {
    const l = form.to_array();
    switch (l.length) {
      case 3:
        return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]));
      case 4:
        return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]), await xp.expand(l[3]));
      default:
        throw new BiwaError("malformed if", form);
    }
  });

  lib.exportMacro(Sym("lambda"), lambdaExpander);

  //TODO
  //lib.environment.installToplevelBinding(Sym("cons"), CoreEnv["cons"]);
  //lib.export(Sym("cons"));
};

export { installCore };
