//
// Values
//
BiwaScheme.Values = Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             this.content.map(BiwaScheme.to_write).join(" ") +
           ">";
  }
});

//
// Nil
// javascript representation of empty list( '() )
//
BiwaScheme.inner_of_nil = new Object();
BiwaScheme.inner_of_nil.inspect = function(){
  // Note: should raise error when car of nil is referenced,
  // not when printed
  throw new BiwaScheme.Error("cannot take car/cdr of '() in Scheme");
};
BiwaScheme.nil = new BiwaScheme.Pair(BiwaScheme.inner_of_nil, 
                          BiwaScheme.inner_of_nil);
BiwaScheme.nil.toString = function(){ return "nil"; }
BiwaScheme.nil.to_array = function(){ return [] };

//
// #<undef> (The undefined value)
//
BiwaScheme.undef = new Object();
BiwaScheme.undef.toString = function(){ return "#<undef>"; }

// (eof-object)
BiwaScheme.eof = new Object;
