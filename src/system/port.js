//
// Port
//
BiwaScheme.Port = Class.create({
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
BiwaScheme.Port.BrowserInput = Class.create(BiwaScheme.Port, {
  initialize: function($super){
    $super(true, false);
  },
  get_string: function(after){
    var form = document.createElement("div")
    form.innerHTML = "<input id='webscheme-read-line' type='text'><input id='webscheme-read-line-submit' type='button' value='ok'>";
    $('bs-console').appendChild(form)

    return new BiwaScheme.Pause(function(pause){
      Event.observe($('webscheme-read-line-submit'), 'click', function(){
        var input = $('webscheme-read-line').value;
        form.parentNode.removeChild(form);
        puts(input);
        pause.resume(after(input));
      });
    });
  }
})
BiwaScheme.Port.DefaultOutput = Class.create(BiwaScheme.Port, {
  initialize: function($super){
    $super(false, true);
  },
  put_string: function(str){
    puts(str, true);
  }
})

//
// string ports (srfi-6)
//
BiwaScheme.Port.StringOutput = Class.create(BiwaScheme.Port, {
  initialize: function($super){
    this.buffer = [];
    $super(false, true);
  },
  put_string: function(str){
    this.buffer.push(str);
  },
  output_string: function(str){
    return this.buffer.join("");
  }
});
BiwaScheme.Port.StringInput = Class.create(BiwaScheme.Port, {
  initialize: function($super, str){
    this.str = str;
    $super(true, false);
  },
  get_string: function(after){
    return after(this.str);
  }
});
BiwaScheme.Port.current_input  = new BiwaScheme.Port.BrowserInput();
BiwaScheme.Port.current_output = new BiwaScheme.Port.DefaultOutput();
BiwaScheme.Port.current_error  = new BiwaScheme.Port.DefaultOutput();
