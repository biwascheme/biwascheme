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
BiwaScheme.Nil = Class.create({
  toString: function() { return "nil"; },
  to_array: function() { return []; },
  length: function() { return 0; }
});

BiwaScheme.nil = new BiwaScheme.Nil();
console.log("blah blah blah");
console.log("BiwaScheme.nil is now " + BiwaScheme.nil);

//
// #<undef> (The undefined value)
//
BiwaScheme.undef = new Object();
BiwaScheme.undef.toString = function(){ return "#<undef>"; }

// (eof-object)
BiwaScheme.eof = new Object;
