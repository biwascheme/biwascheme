//
// Port
//

// (eof-object)
BiwaScheme.eof = new Object;

BiwaScheme.Port = BiwaScheme.Class.create({
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
BiwaScheme.Port.StringOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
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

BiwaScheme.Port.StringInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(str){
    this.str = str;
  },
  get_string: function(after){
    return after(this.str);
  }
});

BiwaScheme.Port.NullInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(){
  },
  get_string: function(after){
    // Never give them anything!
    return after('');
  }
});

BiwaScheme.Port.NullOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){}
});

BiwaScheme.Port.CustomOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(output_function){
    this.output_function = output_function;
  },
  put_string: function(str){
    this.output_function(str);
  }
});

BiwaScheme.Port.CustomInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(input_function){
    this.input_function = input_function;
  },
  get_string: function(after){
    var input_function = this.input_function;
    return new BiwaScheme.Pause(function(pause) {
      input_function(function(input) {
        pause.resume(after(input));
      });
    });
  }
});

// User must set the current input/output
BiwaScheme.Port.current_input  = new BiwaScheme.Port.NullInput();
BiwaScheme.Port.current_output = new BiwaScheme.Port.NullOutput();
BiwaScheme.Port.current_error  = new BiwaScheme.Port.NullOutput();
