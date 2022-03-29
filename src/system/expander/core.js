//
// Expanders for core syntaxes
//
import { List, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { isIdentifier, unwrapSyntax } from "./syntactic_closure.js"

const expandLambda = async ([form, xp]) => {
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

const expandIf = async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 3:
      return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]));
    case 4:
      return List(Sym("if"), await xp.expand(l[1]), await xp.expand(l[2]), await xp.expand(l[3]));
    default:
      throw new BiwaError("malformed if", form);
  }
};

const expandQuote = async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 2:
      return List(Sym("quote"), unwrapSyntax(l[1]));
    default:
      throw new BiwaError("malformed quote", form);
  }
};

// TODO
// const expandDefine
// const expandBegin
// const expandSet
// const expandCallCc

// TODO
// const expandDefineRecordType
// const expandParameterize
// const expandDefineSyntax
// const expandLetSyntax
// const expandLetrecSyntax
// const expandSyntaxError
// const expandInclude
// const expandIncludeCi
// const expandIfExpand
// const expandCaseLambda

// Install core expanders into `lib`
const installCore = (lib) => {
  lib.exportMacro(Sym("lambda"), expandLambda);
  lib.exportMacro(Sym("if"), expandIf);
  lib.exportMacro(Sym("quote"), expandQuote);
};
export { installCore };
