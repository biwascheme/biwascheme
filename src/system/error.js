import Class from "./class.js"
import * as _ from "../deps/underscore-1.10.2-esm.js"

//
// Errors
//

const BiwaError = Class.create({
  initialize: function(msg){
    this.message = "Error: "+msg;
  },
  toString: function(){
    return this.message;
  }
});

const Bug = Class.extend(new BiwaError(), {
  initialize: function(msg){
    this.message = "[BUG] "+msg;
  }
});

// currently used by "raise"
const UserError = Class.extend(new BiwaError(), {
  initialize: function(msg){
    this.message = msg;
  }
});

export { BiwaError, Bug, UserError };
