import * as _ from "../deps/underscore-esm.js"
import { to_write } from "./_writer.js"

//
// Errors
//

class BiwaError {
  constructor(msg, form=null){
    const info = (form === null ? "" : `: ${to_write(form)}`);
    this.message = `Error: ${msg}${info}`;
    this.form = form;
  }

  toString(){
    return this.message;
  }
}

class Bug extends BiwaError {
  constructor(msg){
    this.message = "[BUG] "+msg;
  }
}

// currently used by "raise"
class UserError extends BiwaError {
  constructor(msg){
    this.message = msg;
  }
}

export { BiwaError, Bug, UserError };
