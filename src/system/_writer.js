import * as _ from "../deps/underscore-esm.js"
import { isVector } from "./_types.js"
import { nil, undef } from "../header.js"
import { BiwaSymbol } from "./symbol.js"
import { isPair } from "./pair.js"

//
// _writer.js: Functions to convert objects to strings
//

// Truncate long string with '...'
const truncate = function(str, length) {
  const truncateStr = '...';
  return str.length > length ? str.slice(0, length) + truncateStr : str;
};

//
// write
//

const write_simple = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(typeof obj === "function")
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? truncate(obj.toSource(), 40) :
                          "")+">";
  else if(typeof obj === "string")
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
  else if(Array.isArray(obj))
    return "#(" + obj.map(function(e) { return write_simple(e); }).join(" ") + ")";
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
  if(typeof obj === "undefined")
    return 'undefined';
  else if(obj === null)
    return 'null';
  else if(obj.to_display)
    return obj.to_display(to_display);
  else if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaSymbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + obj.map(to_display).join(' ') + ')';
  else
    return write_simple(obj);
}

//
// inspect (for debugging)
//

const inspect = function(object, opts) {
  try {
    if (typeof object === "undefined") return 'undefined';
    if (object === null) return 'null';
    if (object === true) return '#t';
    if (object === false) return '#f';
    if (object.inspect) return object.inspect();
    if (typeof object === "string") {
      return '"' + object.replace(/"/g, '\\"') + '"';
    }
    if (Array.isArray(object)) {
      return '[' + object.map(inspect).join(', ') + ']';
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

//
// write/ss (write with substructure)
//

// Uses datum label if cyclic. Otherwise does not
function write(obj) {
  const wstate = _preprocess(obj);
  if (wstate.cyclic) {
    return _write_shared(obj, wstate);
  } else {
    return write_simple(obj);
  }
}

function _preprocess(obj) {
  const state = {
    objs: new Set(),
    shared_objs: new Set(),
    parents: new Set(),
    cyclic: false,
  };
  _gather_information(obj, state);

  // Create initial writer state
  const ids = new Map();
  for (const o of state.shared_objs) {
    ids.set(o, null);
  }
  const wstate = {
    ids: ids,
    last_id: -1,
    cyclic: state.cyclic,
  };
  return wstate;
}

function _gather_information(obj, state) {
  if (state.parents.has(obj)) {
    // Already seen and this is a cyclic object
    state.cyclic = true;
  }
  if (state.shared_objs.has(obj)) {
    return;
  } else if (state.objs.has(obj)) {
    state.shared_objs.add(obj);
    return;
  }
  // Found a new object
  state.objs.add(obj);
  if (isPair(obj)) {
    state.parents.add(obj);
    _gather_information(obj.car, state);
    _gather_information(obj.cdr, state);
    state.parents.delete(obj);
  } else if (isVector(obj)) {
    state.parents.add(obj);
    obj.forEach((item) => {
      _gather_information(item, state);
    });
    state.parents.delete(obj);
  }
}

// Always use datum label
function write_shared(obj) {
  const wstate = _preprocess(obj);
  return _write_shared(obj, wstate);
}

function _write_shared(obj, wstate) {
  let s = "";
  if (wstate.ids.has(obj)) {
    const id = wstate.ids.get(obj);
    if (id === null) {
      // First occurrence of a shared object; Give it a number
      const new_id = wstate.last_id + 1;
      wstate.ids.set(obj, new_id);
      wstate.last_id = new_id;
      s += `#${new_id}=`;
    } else {
      // Already printed. Just show the reference
      return `#${id}#`;
    }
  }
  if (isPair(obj)) {
    const a = [];
    // Note that we cannot use obj.forEach (because it does not stop)
    a.push(_write_shared(obj.car, wstate));
    for (let o=obj.cdr; o !== nil; o=o.cdr) {
      if (!isPair(o) || wstate.ids.has(o)) {
        a.push(".");
        a.push(_write_shared(o, wstate));
        break;
      }
      a.push(_write_shared(o.car, wstate));
    }
    s += "(" + a.join(" ") + ")";
  } else if (isVector(obj)) {
    const a = obj.map((item) => _write_shared(item, wstate));
    s += "#(" + a.join(" ") + ")";
  } else {
    s += write_simple(obj);
  }
  return s;
}

const to_write = write;

export { to_write, to_display, inspect, truncate, write_shared, write_simple };
