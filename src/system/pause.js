//
// pause object (facility to stop/resume interpreting)
//
class Pause {
  //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
  constructor(on_pause){
    this.on_pause = on_pause;
  }

  //save state of interpreter
  set_state(intp, x, f, c, s){
    this.interpreter = intp;
    this.x = x;
    this.f = f;
    this.c = c;
    this.s = s;
  }

  //call this when ready (to fire setTimeout, Ajax.Request..)
  ready(){
    this.on_pause(this);
  }

  //restart calculation
  resume(value){
    return this.interpreter.resume(true, value, this.x, this.f, this.c, this.s)
  }
}

export default Pause;
