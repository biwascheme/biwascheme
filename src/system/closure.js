import Class from "./class.js"

// A Scheme lambda
const Closure = Class.create({
  // body: Intermediate Language (JS array) 
  // freevars: Captured free variables
  // dotpos: The position of `.` in the parameter list. -1 if none
  // expected_args: Expected number of args or `undefined`
  initialize: function(body, freevars, dotpos, expected_args){
    this.body = body;
    this.freevars = freevars;
    this.dotpos = dotpos;
    this.expected_args = expected_args;
  },

  to_write: function() {
    return "#<Closure>";
  }
})

const isClosure = function(obj){
  return (obj instanceof Closure);
};

export { Closure, isClosure };
