import * as _ from "../deps/underscore-1.10.2-esm.js"
import { CoreEnv, suppress_deprecation_warning } from "../header.js";
import { isString, isFunction, isChar, isSymbol, isPort, isPair, isList,
         isVector, isHashtable, isMutableHashtable, isClosure } from "../system/_types.js"
import { to_write } from "../system/_writer.js"
import { make_assert, make_simple_assert } from "../system/assert.js"
import { BiwaError } from "../system/error.js"
import Interpreter from "../system/interpreter.js"
import { Complex } from "../system/number.js"
import { isPromise } from "../system/promise.js"
import Syntax from "../system/syntax.js"

///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
const check_arity = function(fname, len, min, max){
  if(len < min){
    if(max && max == min)
      throw new BiwaError(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaError(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaError(fname+": too many arguments (at most: "+max+" got: "+len+")");
}

const define_libfunc = function(fname, min, max, func){
  var f = function(ar, intp){
    check_arity(fname, ar.length, min, max);
    return func(ar, intp);
  };

  func["fname"] = fname; // for assert_*
  f["inspect"] = function(){ return this.fname; }
  CoreEnv[fname] = f;
}

const alias_libfunc = function(fname, aliases) {
  if (CoreEnv[fname]) {
    if (_.isArray(aliases)) {
      _.map(aliases, function(a) { alias_libfunc(fname, a); });
    } else if (_.isString(aliases)) {
      CoreEnv[aliases] = CoreEnv[fname];
    } else {
      console.error("[BUG] bad alias for library function " +
                    "`" + fname + "': " + aliases.toString());
    }
  } else {
    console.error("[BUG] library function " +
                  "`" + fname + "'" +
                  " does not exist, so can't alias it.");
  }
};

const define_syntax = function(sname, func) {
  var s = new Syntax(sname, func);
  CoreEnv[sname] = s;
}

const define_scmfunc = function(fname, min, max, str){
  (new Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//

const assert_number = make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof Complex);
});

const assert_integer = make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

const assert_real = make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

const assert_between = make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaError(fname + ": " +
                               "number required, but got " +
                               to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaError(fname + ": " +
                               "number must be between " +
                               from + " and " + to + ", but got " +
                               to_write(obj));
  }
});

const assert_string = make_simple_assert("string", isString);
const assert_char = make_simple_assert("character", isChar);
const assert_symbol = make_simple_assert("symbol", isSymbol);
const assert_port = make_simple_assert("port", isPort);
const assert_pair = make_simple_assert("pair", isPair);
const assert_list = make_simple_assert("list", isList);
const assert_vector = make_simple_assert("vector", isVector);
const assert_hashtable = make_simple_assert("hashtable", isHashtable);
const assert_mutable_hashtable = make_simple_assert("mutable hashtable", isMutableHashtable); 
const assert_promise = make_simple_assert("promise", isPromise);

const assert_function = make_simple_assert("JavaScript function", isFunction);
const assert_closure = make_simple_assert("scheme function", isClosure);
const assert_procedure = make_simple_assert("scheme/js function", function(obj){
  return isClosure(obj) || isFunction(obj);
});

const assert_date = make_simple_assert("date", function(obj){
  // FIXME: this is not accurate (about cross-frame issue)
  // https://prototype.lighthouseapp.com/projects/8886/tickets/443
  return obj instanceof Date;
});

//var assert_instance_of = BiwaScheme.make_assert(function(fname, type, obj, klass){
//  if(!(obj instanceof klass)){
//    throw new BiwaScheme.Error(fname + ": " +
//                               type + " required, but got " +
//                               BiwaScheme.to_write(obj));
//  }
//});

const assert = make_assert(function(fname, success, message, _fname){
  if(!success){
    throw new BiwaError((_fname || fname)+": "+message);
  }
});

//
// deprecation
//

// Show deprecation warnig
// @param {string} title - feature to be deprecated
// @param {string} ver - when it will be removed (eg. "1.0")
// @param {string} alt - alternatives
const deprecate = function(title, ver, alt){
  if(suppress_deprecation_warning) return;

  var msg = title+" is deprecated and will be removed in BiwaScheme "+ver+ ". "+
            "Please use "+alt+" instead";
  console.warn(msg); 
};

//
// utils
//

// Parses a fractional notation in the format: <num>/<denom> (e.g. 3/7, -9/4),
// where <num> is a valid integer notation, and <denom> is a valid notation
// for a positive integer.
//
// Returns a float if the notation is valid, otherwise false.
//
// @param {string} rep - the string representation of the fraction
// @return {float|false}
const parse_fraction = function(rep) {
  assert_string(rep);

  var frac_parts = rep.split('/');

  if (frac_parts.length !== 2)
    return false;

  var num_rep = frac_parts[0];
  var denom_rep = frac_parts[1];

  var num = parse_integer(num_rep, 10);
  var denom = parse_integer(denom_rep, 10);

  if (num === false || denom === false)
    return false;

  if (denom <= 0)
    return false;

  return num / denom;
};

// Given a string notation of an integer, and the radix, validates the
// notation: returns true if the notation is valid, otherwise false.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {boolean}
const is_valid_integer_notation = function(rep, rdx) {
  assert_string(rep);
  assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  var rdx_symbols = '0123456789abcdefghijklmnopqrstuvwxyz';

  var valid_symbols = rdx_symbols.slice(0, rdx);
  var sym_regex = new RegExp('^[+-]?' + '[' + valid_symbols + ']+$', 'ig');

  return sym_regex.test(rep);
};

// Parse an integer. If the integer does not have a valid representation, or
// produces NaN, - false is returned. If the radix is not within [2..36]
// range, false is returned as well.
//
// @param {string} rep - the string representation of the integer
// @param {integer} rdx - the radix, where 2 <= rdx <= 36
// @return {integer|false}
const parse_integer = function(rep, rdx) {
  assert_string(rep);
  assert_integer(rdx);

  if (rdx < 2 || rdx > 36)
    return false;

  if (!is_valid_integer_notation(rep, rdx))
    return false;

  var res = parseInt(rep, rdx);

  if (Number.isNaN(res))
    return false;

  return res;
};

// Given a string notation of a floating-point number in the standard or
// scientific notation, returns true if the notation valid, otherwise false.
//
// For example:
// "1"      -> true
// "1."     -> true
// "1.23"   -> true
// "1e4"    -> true
// "1E4"    -> true
// "1E4.34" -> false
// "e34"    -> false
//
// @param {string} rep - the string representation of the float.
// @return {boolean}
const is_valid_float_notation = function(rep) {
  assert_string(rep);

  var sci_regex = /^[+-]?[0-9]+[.]?[0-9]*e[+-]?[0-9]+$/i;
  var fp_regex  = /(^[+-]?[0-9]*[.][0-9]+$)|(^[+-]?[0-9]+[.][0-9]*$)/;

  if (sci_regex.test(rep) || fp_regex.test(rep))
    return true;

  return is_valid_integer_notation(rep, 10);
};

// Parse a floating-point number. If the floating-point number does not have a
// valid representation, or produces -Infinity, +Infinity or NaN, - false is
// returned.
//
// @param {string} rep - the string representation of the floating-point value
// @return {float|false}
const parse_float = function(rep) {
  assert_string(rep);

  if (!is_valid_float_notation(rep))
    return false;

  var res = new Number(rep).valueOf();

  if (Number.isNaN(res))
    return false;

  if (!Number.isFinite(res))
    return false;

  return res;
};

export { define_libfunc, alias_libfunc, define_syntax, define_scmfunc,
         assert_number, assert_integer, assert_real, assert_between, assert_string,
         assert_char, assert_symbol, assert_port, assert_pair, assert_list,
         assert_vector, assert_hashtable, assert_mutable_hashtable, assert_promise,
         assert_function, assert_closure, assert_procedure, assert_date, assert, deprecate,
         parse_fraction, is_valid_integer_notation, parse_integer, is_valid_float_notation, parse_float }; 
