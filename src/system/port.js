import Pause from "./pause.js"

//
// Port
//

// (eof-object)
const eof = new Object;

class Port {
  constructor(is_in, is_out){
    this.is_open = true;
    this.is_binary = false; //??
    this.is_input = is_in;
    this.is_output = is_out;
  }

  close(){
    // close port
    this.is_open = false;
  }

  inspect(){
    return "#<Port>";
  }

  to_write(){
    return "#<Port>";
  }
}

//
// string ports (srfi-6)
//
Port.StringOutput = class extends Port {
  constructor(){
    super(false, true);
    this.buffer = [];
  }

  put_string(str){
    this.buffer.push(str);
  }

  output_string(str){
    return this.buffer.join("");
  }
};

Port.StringInput = class extends Port {
  constructor(str){
    super(true, false);
    this.str = str;
  }

  get_string(after){
    return after(this.str);
  }
};

Port.NullInput = class extends Port {
  constructor(){
    super(true, false);
  }

  get_string(after){
    // Never give them anything!
    return after('');
  }
};

Port.NullOutput = class extends Port {
  constructor(output_function){
    super(false, true);
    this.output_function = output_function;
  }

  put_string(str){}
};

Port.CustomOutput = class extends Port {
  constructor(output_function){
    super(false, true);
    this.output_function = output_function;
  }

  put_string(str){
    this.output_function(str);
  }
};

Port.CustomInput = class extends Port {
  constructor(input_function){
    super(true, false);
    this.input_function = input_function;
  }

  get_string(after){
    var input_function = this.input_function;
    return new Pause(function(pause) {
      input_function(function(input) {
        pause.resume(after(input));
      });
    });
  }
};

// User must set the current input/output
Port.current_input  = new Port.NullInput();
Port.current_output = new Port.NullOutput();
Port.current_error  = new Port.NullOutput();

export { Port, eof };
