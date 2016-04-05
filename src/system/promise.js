//
// R7RS Promise
//
BiwaScheme.Promise = BiwaScheme.Class.create({
  initialize : function(proc){
    // Scheme procedure
    this.proc = proc;
    // Denotes this promise is not computed yet
    this.fresh = true;
    // The value of this promise
    this.value = null;
  },

  // Set value to this promise
  set_value: function(value) {
    this.fresh = false;
    this.value = value;
  }
});
