//
// pause object (facility to stop/resume interpreting)
//
BiwaScheme.Pause = BiwaScheme.Class.create({
  //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
  initialize: function(on_pause){
    this.on_pause = on_pause;
  },

  //save state of interpreter
  set_state: function(intp, x, f, c, s){
    this.interpreter = intp;
    this.x = x;
    this.f = f;
    this.c = c;
    this.s = s;
  },

  //call this when ready (to fire setTimeout, Ajax.Request..)
  ready: function(){
    this.on_pause(this);
  },

  // Save state of expander
  set_expander_state(expanding) {
    this.expanding = expanding;
  },

  //restart calculation
  resume: function(value){
    if (this.expanding) {
      return this.interpreter.resume("expanding", value, this.x, this.f, this.c, this.s, this.expanding);
    }
    else {
      return this.interpreter.resume("evaluating", value, this.x, this.f, this.c, this.s)
    }
  }
});

