//
// Syntax
//
BiwaScheme.Syntax = BiwaScheme.Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },
  transform: function(x){
    if (!this.func){
      throw new BiwaScheme.Bug("sorry, syntax "+this.sname+
                               " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
})

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
BiwaScheme.TopEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.TopEnv["begin"]  = new BiwaScheme.Syntax("begin");
BiwaScheme.TopEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.TopEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.TopEnv["if"]     = new BiwaScheme.Syntax("if");
BiwaScheme.TopEnv["set!"]   = new BiwaScheme.Syntax("set!");
