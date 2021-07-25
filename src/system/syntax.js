import * as _ from "../deps/underscore-esm.js"
import { CoreEnv } from "../header.js"
import Class from "./class.js"
import { Bug } from "./error.js"

//
// Syntax
//
const Syntax = Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },
  transform: function(x){
    if (!this.func){
      throw new Bug("sorry, syntax "+this.sname+
                    " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
})

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
CoreEnv["define"] = new Syntax("define");
CoreEnv["begin"]  = new Syntax("begin");
CoreEnv["quote"]  = new Syntax("quote");
CoreEnv["lambda"] = new Syntax("lambda");
CoreEnv["if"]     = new Syntax("if");
CoreEnv["set!"]   = new Syntax("set!");

export default Syntax;
