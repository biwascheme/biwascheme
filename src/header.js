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
  length: function() { return 0; },
  // Moved to main.js to avoid circular dependency
  //to_set: function() { return new BiwaSet(); },
};

//
// #<undef> (The undefined value)
// also used as #<unspecified> values
//
const undef = new Object();
undef.toString = function(){ return "#<undef>"; }

//
// Configurations
//

// Maximum depth of stack trace
// (You can also set Interpreter#max_trace_size for each Interpreter)
const max_trace_size = 40;

// Stop showing deprecation warning
const suppress_deprecation_warning = false;

export { TopEnv, CoreEnv, nil, undef, max_trace_size, suppress_deprecation_warning };
