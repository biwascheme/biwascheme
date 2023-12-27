//
// types.js - type predicators, equality, compare
//
import { nil, undef } from "../header.js"
import { BiwaSymbol } from "./symbol.js"
import { Port } from "./port.js"
import { Closure, isClosure } from "./closure.js"

/**
 * An S-expression.
 * @typedef {(Pair|BiwaSymbol|number|string)} Form
 * A Scheme procedure.
 * @typedef {function|Closure} Procedure
 **/

const isNil = function(obj){
  return (obj === nil);
};

const isUndef = function(obj){
  return (obj === undef);
};

const isBoolean = (s) => typeof s === "boolean"; // Return true if arg is either true or false

//isNumber is defined in number.js (Return true if arg is scheme number)

const isString = (s) => typeof s === "string";

const isFunction = (s) => typeof s === "function";

const isSymbol = function(obj){
  return (obj instanceof BiwaSymbol);
};

const isPort = function(obj){
  return (obj instanceof Port);
};

const isVector = function(obj){
  return (obj instanceof Array);
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
const isProcedure = function(obj){
  return isClosure(obj) || typeof obj === "function";
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

export { isNil, isUndef, isBoolean, isString, isFunction, isSymbol, isPort,
         isVector, isProcedure,
         lt };
