import { to_write } from "./_writer.js"

//
// Errors
//

class BiwaError extends Error {
  constructor(msg, form=null){
    super(msg)
    const info = (form === null ? "" : `: ${to_write(form)}`);
    const message = `${msg}${info}`;
    super(message);
    this.form = form;
  }

  toString(){
    return this.message;
  }
}

class Bug extends BiwaError {
  constructor(msg){
    super("[BUG] " + msg)
  }
}

// currently used by "raise"
class UserError extends BiwaError {
  constructor(msg){
    super(msg)
    this.message = msg;
  }
}

export { BiwaError, Bug, UserError };
