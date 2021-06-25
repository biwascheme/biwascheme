import Class from "./class.js"

// A Scheme lambda
const Closure = Class.create({
  // il: Intermediate Language (JS array)
  //     [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
  // expected_args: Expected number of args or `undefined`
  initialize: function(il, expected_args){
    this.il = il;
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
