import * as _ from "../deps/underscore-1.10.2-esm.js"
import { nil, undef } from "../header.js"
import { isClosure } from "./_types.js"
import { BiwaSymbol } from "./symbol.js"
import { Pair } from "./pair.js"

//
// write.js: Functions to convert objects to strings
//

//
// write
//

const truncate = function(str, length) {
  const truncateStr = '...';
  return str.length > length ? str.slice(0, length) + truncateStr : str;
};

const to_write = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(_.isFunction(obj))
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? truncate(obj.toSource(), 40) :
                          "")+">";
  else if(_.isString(obj))
    return '"' +
           obj.replace(/\\|\"/g,function($0){return'\\'+$0;})
              .replace(/\x07/g, "\\a")
              .replace(/\x08/g, "\\b")
              .replace(/\t/g, "\\t")
              .replace(/\n/g, "\\n")
              .replace(/\v/g, "\\v")
              .replace(/\f/g, "\\f")
              .replace(/\r/g, "\\r") +
           '"';
  else if(isClosure(obj))
    return "#<Closure>";
  else if(_.isArray(obj))
    return "#(" + _.map(obj, function(e) { return to_write(e); }).join(" ") + ")";
  else if(typeof(obj.to_write) == 'function')
    return obj.to_write();
  else if(isNaN(obj) && typeof(obj) == 'number')
    return "+nan.0";
  else{
    switch(obj){
      case true: return "#t";
      case false: return "#f";
      case Infinity: return "+inf.0";
      case -Infinity: return "-inf.0";
    }
  }
  return inspect(obj);
}

//
// display
//

const to_display = function(obj){
  if(obj.to_display)
    return obj.to_display();
  if(_.isUndefined(obj))
    return 'undefined';
  else if(_.isNull(obj))
    return 'null';
  else if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaSymbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + _.map(obj, to_display).join(' ') + ')';
  else if(obj instanceof Pair)
    return obj.inspect(to_display);
  else
    return to_write(obj);
}

//
// inspect
//
const inspect = function(object, opts) {
  try {
    if (_.isUndefined(object)) return 'undefined';
    if (object === null) return 'null';
    if (object === true) return '#t';
    if (object === false) return '#f';
    if (object.inspect) return object.inspect();
    if (_.isString(object)) {
      return '"' + object.replace(/"/g, '\\"') + '"';
    }
    if (_.isArray(object)) {
      return '[' + _.map(object, inspect).join(', ') + ']';
    }

    if (opts && opts["fallback"]){
      return opts["fallback"];
    }
    else {
      return object.toString();
    }
  } catch (e) {
    if (e instanceof RangeError) return '...';
    throw e;
  }
};

export { to_write, to_display, inspect, truncate };
