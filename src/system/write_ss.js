import * as _ from "../deps/underscore-1.10.2-esm.js"
import { to_write } from "./_writer.js"
import { isVector } from "./_types.js"
import { nil, undef } from "../header.js"
import { BiwaSymbol } from "./symbol.js"
import { Pair, isPair } from "./pair.js"

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
    s += to_write(obj);
  }
  return s;
}

// Never use datum label
function write_simple(obj) {
  return to_write(obj);
}

export { write, write_shared, write_simple };
