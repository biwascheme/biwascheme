import * as _ from "../deps/underscore-1.10.2-esm.js"
import { to_write } from "./_writer.js"
import { BiwaError } from "./error.js"
//
// assertions - type checks
//

const make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname
                  : "";
    check.apply(this, [fname].concat(_.toArray(arguments)));
  }
}

const make_simple_assert = function(type, test, _fname){
  return make_assert(function(fname, obj, opt){
    if(_fname) fname = _fname;
    option = opt ? ("("+opt+")") : ""
    if(!test(obj)){
      throw new BiwaError(fname + option + ": " +
                          type + " required, but got " +
                          to_write(obj));
    }
  })
}

export { make_assert, make_simple_assert }
