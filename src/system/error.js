//
// Errors
//

BiwaScheme.Error = Error; 
/*BiwaScheme.Class.extend(new Error(), {
  initialize: function(msg){
    this.message = "Error: "+msg;
    this.stack = (new Error()).stack;
  },
  toString: function(){
    return this.message;
  }
});*/

BiwaScheme.Bug = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = "[BUG] "+msg;
    //console.log(this.message);
    this.stack = (new Error()).stack;
  },

  toString: function(){
    return this.message;
  }
});

// currently used by "raise"
BiwaScheme.UserError = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = msg;
  }
});

