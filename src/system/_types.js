//
// types.js - type predicators, equality, compare
//
import * as _ from "../deps/underscore-1.10.2-esm.js"
import { nil, undef } from "../header.js"
import { to_write_ss } from "./write_ss.js"
import Char from "./char.js"
import { BiwaSymbol } from "./symbol.js"
import { Port } from "./port.js"

const isNil = function(obj){
  return (obj === nil);
};

const isUndef = function(obj){
  return (obj === undef);
};

const isBoolean = _.isBoolean; // Return true if arg is either true or false

//isNumber is defined in number.js (Return true if arg is scheme number)

const isString = _.isString;

const isFunction = _.isFunction;

const isChar = function(obj){
  return (obj instanceof Char);
};

const isSymbol = function(obj){
  return (obj instanceof BiwaSymbol);
};

const isPort = function(obj){
  return (obj instanceof Port);
};

const isVector = function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
};

// Returns true if `obj` is a Scheme closure.
const isClosure = function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
};

// Change `ary` into a Scheme closure (destructive).
const makeClosure = function(ary) {
  ary.closure_p = true;
  return ary;
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
const isProcedure = function(obj){
  return isClosure(obj) || _.isFunction(obj);
};

// Return true if obj is a scheme value which evaluates to itself
const isSelfEvaluating = function(obj) {
  return isBoolean(obj) ||
         isNumber(obj) ||
         isString(obj) ||
         isChar(obj);
};

//
// equality
//
const eq = function(a, b){
  return a === b;
};
// TODO: Records (etc.)
const eqv = function(a, b){
  return a == b && (typeof(a) == typeof(b));
};
const equal = function(a, b){
  return to_write_ss(a) == to_write_ss(b);
};

//
// comaprator
//
// Return true when a < b
const lt = function(a, b) {
  if(typeof a !== typeof b){
    return compareFn(typeof a, typeof b); 	
  }
  return a < b;
};

export { isNil, isUndef, isBoolean, isString, isFunction, isChar, isSymbol, isPort,
         isVector, isClosure, makeClosure, isProcedure,
         isSelfEvaluating, eq, eqv, equal, lt };
