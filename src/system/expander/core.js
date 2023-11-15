//
// Expanders for core syntaxes
//
import { nil } from "../../header.js"
import { List, Cons, isPair, isList, array_to_list, mapCarAndCdrAsync, collectCarAndCdr } from "../pair.js"
import { Sym } from "../symbol.js"
import { inspect, to_write } from "../_writer.js"
import { Bug, BiwaError } from "../error.js"
import { isIdentifier, unwrapSyntax } from "./syntactic_closure.js"
import { makeErMacroTransformer } from "./macro_transformer.js"
import { libBiwaschemeErMacro } from "../../lib/biwascheme/er-macro.js"
import { Macro } from "./macro.js"
import { Engine } from "../engine.js"

// Creates macro transformer written in syntax-rules.
// @return { Macro } A macro transformer bound with `env`
async function interpretTransformerSpec(xp, spec, env, metaEnv)
{
  if (isIdentifier(spec.car)) {
    if (await xp.identifierEquals(spec.car, env, Sym("syntax-rules"), metaEnv)) {
      return new Macro("(syntax-rules)", env, interpretSyntaxRules(spec));
    } else if (await xp.identifierEquals(spec.car, env, Sym("er-macro-transformer"), libBiwaschemeErMacro.environment)) {
      const expanded = xp.expand(spec.cadr())
      const proc = await xp.engine.evalExpandedForm(expanded);
      return new Macro("(er-macro-transformer)", env, proc);
    }
  }
  throw new BiwaError("unknown macro transformer spec", spec);
}

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
  const [_, formal, ...exprs] = l;
  if (l.length < 3) {
    throw new BiwaError("malformed define", form);
  } else if (isIdentifier(formal) && exprs.length == 1) {
    // (define var expr)
    env.extend(formal);
    const id = await xp.expand(formal, env);
    const body = env.isToplevel() ? (await xp.expand(exprs[0]))
                                  : exprs[0]; // Expand later on
    return List(Sym("define"), id, body);
  } else if (isPair(formal) && isIdentifier(formal.car)) {
    // (define (f x) a b c...)
    // => (define f (lambda (x) a b c...))
    const id = formal.car;
    const args = formal.cdr;
    const form = List(Sym("define"), id,
      Cons(Sym("lambda"), Cons(args, array_to_list(exprs))));
    return expandDefine([form, xp, env]);
  } else {
    throw new BiwaError("malformed define", form);
  }
};

// `define-syntax`
const expandDefineSyntax = async ([form, xp, env, metaEnv]) => {
  const l = form.to_array();
  switch (l.length) {
    case 3:
      const [_, keyword, transformerSpec] = l;
      if (!isIdentifier(keyword)) {
        throw new BiwaError("malformed define-syntax", form);
      }
      const expander = await interpretTransformerSpec(xp, transformerSpec, env, metaEnv);
      env.installExpander(keyword, expander);
      return List(Sym("begin"));
    default:
      throw new BiwaError("malformed define-syntax", form);
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
// - (lambda (x y) ...)
// - (lambda (x y . rest) ...)
// - (lambda args ...)
const expandLambda = async ([form, xp, env]) => {
  const err = new BiwaError("malformed lambda", form);
  const l = form.to_array();
  if (l.length < 3) throw err;

  const formals = form.cadr(err);
  if (!isList(formals)) throw err;
  const newIds = collectCarAndCdr(formals);
  const newEnv = env.extended(newIds);
  
  const newFormals = formals === nil
    ? nil
    : await mapCarAndCdrAsync(formals, async (x) => { return await xp.expand(x) });

  const body = form.cddr(err);
  let newBody = await body.mapAsync(async form => { return await xp.expand(form, newEnv) });
  newBody = _expandDefinitionBodyInLambda(newBody);
  newBody = _spliceDefinitionInLambda(newBody);

  return Cons(Sym("lambda"), Cons(newFormals, newBody));
}

/** Process bodies of internal define
 * @return Form
 */
function _expandDefinitionBodyInLambda(body) {
  const err = new BiwaError("malformed define in lambda", body);
  return body.mapList(form => {
    if (!isPair(form)) return form;
    const car = form.car;
    if (car === Sym("quote") || car == Sym("lambda")) return form;
    if (car === Sym("define")) {
      const k = form.cadr(err);
      const v = form.caddr(err);
      return List(Sym("define"), k, _expandDefinitionBodyInLambda(v));
    } else {
      return _expandDefinitionBodyInLambda(form);
    }
  });
}

// Splice internal defines enclosed with `begin`
function _spliceDefinitionInLambda(body) {
  const definitions = [];
  const rest = [];
  for (const form of body.to_array()) {
    if (rest.length > 0 || !_isDefinition(form)) {
      rest.push(form);
    } else {
      definitions = definitions.concat(_spliceDefinition(form));
    }
  }
  return array_to_list(definitions.concat(rest))
}

function _isDefinition(form) {
  if (!isPair(form)) return false;
  if (form.car === Sym("define") ||
      form.car === Sym("define-record-type")) {
    return true;
  } else if (form.car === Sym("begin") && 
             form.cdr.to_array().all(_isDefinition)) {
    return true;
  } else {
    return false;
  }
}

// Removes `begin`. Returns an array of define/define-record-type
function _spliceDefinition(def) {
  if (form.car === Sym("define") ||
      form.car === Sym("define-record-type")) {
    return [form];
  } else {
    if (form.car !== Sym("begin")) throw new Bug("unexpected");
    return form.cdr.to_array().map(_spliceDefinition);
  }
}

// `let-syntax`
const expandLetSyntax = async ([form, xp, env, metaEnv]) => {
  const err = new BiwaError("malformed let-syntax", form);
  const l = form.to_array();
  if (l.length < 3) throw err;
  const bindings = form.cdr.car;
  const body = form.cdr.cdr;

  const newEnv = env.clone();
  for (let pair of bindings) {
    const [keyword, transformerSpec] = pair;
    const expander = await interpretTransformerSpec(transformerSpec, env, metaEnv);
    newEnv.installExpander(keyword, expander);
  }

  const _lambda = metaEnv.makeIdentifier(Sym('lambda'));
  return xp.expand(List(Cons(_lambda, Cons(List(), body)), newEnv));
};

// `letrec-syntax`
const expandLetRecSyntax = async ([form, xp, env, metaEnv]) => {
  const err = new BiwaError("malformed letrec-syntax", form);
  const l = form.to_array();
  if (l.length < 3) throw err;
  const bindings = form.cdr.car;
  const body = form.cdr.cdr;

  const newEnv = env.clone();
  for (let pair of bindings) {
    const [keyword, transformerSpec] = pair;
    const expander = await interpretTransformerSpec(transformerSpec, newEnv, metaEnv);
    newEnv.installExpander(keyword, expander);
  }

  const _lambda = metaEnv.makeIdentifier(Sym('lambda'));
  return xp.expand(List(Cons(_lambda, Cons(List(), body)), newEnv));
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
// const expandSyntaxError
// const expandInclude
// const expandIncludeCi
// const expandIfExpand
//   memo: if-expand is not defined in R7RS. Maybe used to implement cond-expand in r7expander?
// const expandCaseLambda

// Install core expanders into `lib`
const installCore = (lib) => {
  lib.exportCoreSyntax(Sym("begin"), expandBegin);
  lib.exportCoreSyntax(Sym("call/cc"), expandCallCc);
  lib.exportCoreSyntax(Sym("define"), expandDefine);
  lib.exportCoreSyntax(Sym("define-syntax"), expandDefineSyntax);
  lib.exportCoreSyntax(Sym("if"), expandIf);
  lib.exportCoreSyntax(Sym("lambda"), expandLambda);
  lib.exportCoreSyntax(Sym("let-syntax"), expandLetSyntax);
  lib.exportCoreSyntax(Sym("letrec-syntax"), expandLetRecSyntax);
  lib.exportCoreSyntax(Sym("quote"), expandQuote);
  lib.exportCoreSyntax(Sym("set!"), expandSet);
};
export { installCore };
