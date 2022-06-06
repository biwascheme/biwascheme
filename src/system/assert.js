import { to_write } from "./_writer.js"
import { BiwaError } from "./error.js"
//
// assertions - type checks
//

const make_assert = function(check){
  return function(/*args*/){
    // We cannot use callee/caller in ESM (=JS strict mode)
    //var fname = arguments.callee.caller
    //              ? arguments.callee.caller.fname
    //              : "";
    const fname = "";
    check.apply(this, [fname].concat(Array.from(arguments)));
  }
}

const make_simple_assert = function(type, test, _fname){
  return make_assert(function(fname, obj, opt){
    if(_fname) fname = _fname;
    const option = opt ? ("("+opt+"): ") : ""
    if(!test(obj)){
      throw new BiwaError(option +
                          type + " required, but got " +
                          to_write(obj));
    }
  })
}

export { make_assert, make_simple_assert }
