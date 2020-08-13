import * as _ from "../deps/underscore-1.10.2-esm.js"
import { to_write } from "./_writer.js"
import Class from "./class.js"

//
// Errors
//

const BiwaError = Class.create({
  initialize: function(msg, form=null){
    const info = (form === null ? "" : `: ${to_write(form)}`);
    this.message = `Error: ${msg}${info}`;
    this.form = form;
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
