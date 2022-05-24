//
// Expanders for core syntaxes
//
import { List, Cons, isPair, isList } from "../pair.js"
import { Sym } from "../symbol.js"
import { isIdentifier, unwrapSyntax } from "./syntactic_closure.js"

// `begin`
const expandBegin = async ([form, xp]) => {
  const body = await form.cdr.mapAsync(item => xp.expand(item));
  return Cons(Sym("begin"), body);
};

// `call/cc`
const expandCallCc = async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 2:
      return List(Sym("call/cc"), await xp.expand(l[1]));
    default:
      throw new BiwaError("malformed call/cc", form);
  }
};

// `define`
// Destructively modifies `env`
const expandDefine = async ([form, xp, env]) => {
  const l = form.to_array();
  switch (l.length) {
    case 3:
      const [_, formal, expr] = l;
      if (!isIdentifier(formal)) {
        throw new BiwaError("malformed define", form);
      }
      env.extend(formal);
      const id = await xp.expand(formal);
      const body = env.isToplevel() ? (await xp.expand(expr))
                                    : expr; // Expand later on
      return List(Sym("define"), id, body);
    default:
      throw new BiwaError("malformed define", form);
  }
};

// `if`
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

// `lambda`
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

// `quote`
const expandQuote = async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 2:
      return List(Sym("quote"), unwrapSyntax(l[1]));
    default:
      throw new BiwaError("malformed quote", form);
  }
};

// `set!`
const expandSet = async ([form, xp]) => {
  const l = form.to_array();
  switch (l.length) {
    case 3:
      return List(Sym("set!"), await xp.expand(l[1]), await xp.expand(l[2]));
    default:
      throw new BiwaError("malformed set!", form);
  }
};

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
  lib.exportMacro(Sym("begin"), expandBegin);
  lib.exportMacro(Sym("call/cc"), expandCallCc);
  lib.exportMacro(Sym("define"), expandDefine);
  lib.exportMacro(Sym("if"), expandIf);
  lib.exportMacro(Sym("lambda"), expandLambda);
  lib.exportMacro(Sym("quote"), expandQuote);
  lib.exportMacro(Sym("set!"), expandSet);
};
export { installCore };
