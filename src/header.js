import BiwaSet from "./system/set.js"
import { inspect } from "./system/_writer.js"
import { Bug } from "./system/error.js"
// 
// Heap based scheme from 3imp.pdf
//

//
// variables
//
const TopEnv = {};
const CoreEnv = {};

//
// Nil
// javascript representation of empty list( '() )
//
const nil = {
  toString: function() { return "nil"; },
  to_write: function() { return "()"; },
  to_array: function() { return []; },
  to_set: function() { return new BiwaSet(); },
  length: function() { return 0; }
};

//
// #<undef> (The undefined value)
// also used as #<unspecified> values
//
const undef = new Object();
undef.toString = function(){ return "#<undef>"; }

// Prints the arguments to console.debug.
const debug = function(/*arguments*/){
  var args = _.toArray(arguments);
  console.debug.apply(console, _.map(args, inspect));
}

//
// Assertion
//
const assert = function(cond, desc) {
  if (!cond) {
    throw new Bug("[BUG] Assertion failed: "+desc);
  }
}

//
// Configurations
//

// Maximum depth of stack trace
// (You can also set Interpreter#max_trace_size for each Interpreter)
const max_trace_size = 40;

// Stop showing deprecation warning
const suppress_deprecation_warning = false;

export { TopEnv, CoreEnv, nil, undef, debug, max_trace_size, suppress_deprecation_warning };
