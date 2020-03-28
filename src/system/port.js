import Class from "./class.js"
import Pause from "./pause.js"

//
// Port
//

// (eof-object)
const eof = new Object;

const Port = Class.create({
  initialize: function(is_in, is_out){
    this.is_open = true;
    this.is_binary = false; //??
    this.is_input = is_in;
    this.is_output = is_out;
  },
  close: function(){
    // close port
    this.is_open = false;
  },
  inspect: function(){
    return "#<Port>";
  },
  to_write: function(){
    return "#<Port>";
  }
});

//
// string ports (srfi-6)
//
Port.StringOutput = Class.extend(new Port(false, true), {
  initialize: function(){
    this.buffer = [];
  },
  put_string: function(str){
    this.buffer.push(str);
  },
  output_string: function(str){
    return this.buffer.join("");
  }
});

Port.StringInput = Class.extend(new Port(true, false), {
  initialize: function(str){
    this.str = str;
  },
  get_string: function(after){
    return after(this.str);
  }
});

Port.NullInput = Class.extend(new Port(true, false), {
  initialize: function(){
  },
  get_string: function(after){
    // Never give them anything!
    return after('');
  }
});

Port.NullOutput = Class.extend(new Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){}
});

Port.CustomOutput = Class.extend(new Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){
    this.output_function(str);
  }
});

Port.CustomInput = Class.extend(new Port(true, false), {
  initialize: function(input_function){
    this.input_function = input_function;
  },
  get_string: function(after){
    var input_function = this.input_function;
    return new Pause(function(pause) {
      input_function(function(input) {
        pause.resume(after(input));
      });
    });
  }
});

// User must set the current input/output
Port.current_input  = new Port.NullInput();
Port.current_output = new Port.NullOutput();
Port.current_error  = new Port.NullOutput();

export { Port, eof };
