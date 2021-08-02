// A Scheme lambda
class Closure {
  // body: Intermediate Language (JS array) 
  // freevars: Captured free variables
  // dotpos: The position of `.` in the parameter list. -1 if none
  // expected_args: Expected number of args or `undefined`
  constructor(body, freevars, dotpos, expected_args){
    this.body = body;
    this.freevars = freevars;
    this.dotpos = dotpos;
    this.expected_args = expected_args;
  }

  to_write() {
    return "#<Closure>";
  }
}

const isClosure = function(obj){
  return (obj instanceof Closure);
};

export { Closure, isClosure };
