//
// Pair 
// cons cell
//

BiwaScheme.Pair = BiwaScheme.Class.create({
  initialize: function(car, cdr){
    this.car = car;
    this.cdr = cdr;
  },

  caar: function(){ return this.car.car; },
  cadr: function(){ return this.cdr.car; },
  cdar: function(){ return this.cdr.car; },
  cddr: function(){ return this.cdr.cdr; },

  first:  function(){ return this.car; },
  second: function(){ return this.cdr.car; },
  third:  function(){ return this.cdr.cdr.car; },
  fourth: function(){ return this.cdr.cdr.cdr.car; },
  fifth:  function(){ return this.cdr.cdr.cdr.cdr.car; },

  // returns array containing all the car's of list
  // '(1 2 3) => [1,2,3]
  // '(1 2 . 3) => [1,2]
  to_array: function(){
    var ary = [];
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  },

  to_set: function(){
    var set = new BiwaScheme.Set();
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  },

  length: function(){
    var n = 0;
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      n++;
    }
    return n;
  },

  // Return the last cdr
  last_cdr: function(){
    var o;
    for(o = this; o instanceof BiwaScheme.Pair; o = o.cdr)
      ;
    return o;
  },

  // calls the given func passing each car of list
  // returns cdr of last Pair
  foreach: function(func){
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  },

  // Returns an array which contains the resuls of calling func
  // with the car's as an argument.
  // If the receiver is not a proper list, the last cdr is ignored.
  // The receiver must not be a cyclic list.
  map: function(func){
    var ary = [];
    for(var o = this; BiwaScheme.isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  },

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat: function(list){
    var o = this;
    while(o instanceof BiwaScheme.Pair && o.cdr != BiwaScheme.nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  },

  // returns human-redable string of pair
  inspect: function(conv){
    conv || (conv = BiwaScheme.inspect);
    var a = [];
    var last = this.foreach(function(o){
      a.push(conv(o));
    });
    if(last != BiwaScheme.nil){
      a.push(".");
      a.push(conv(last));
    }
    return "(" + a.join(" ") + ")";
  },
  toString : function(){
    return this.inspect();
  },

  to_write: function(){
    return this.inspect(BiwaScheme.to_write);
  }
});

// Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
//   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
var array_to_list = function(ary, deep) {
  var list = BiwaScheme.nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && _.isArray(obj) && !obj.is_vector){
      obj = array_to_list(obj, deep);
    }
    list = new BiwaScheme.Pair(obj, list);
  }
  return list;
}

// Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.List = function() {
  var ary = _.toArray(arguments);
  return array_to_list(ary, false);
};

// Shallow: array_to_list(1, 2, [3]) == (list 1 2 (vector 3 4))
BiwaScheme.array_to_list = function(ary) {
  return array_to_list(ary, false);
};

// Deep: deep_array_to_list(1, 2, [3, 4]) == (list 1 2 (list 3 4))
// deep_array_to_list([1, 2, 3]) - deep
BiwaScheme.deep_array_to_list = function(ary) {
  return array_to_list(ary, true);
};

BiwaScheme.Cons = function(car, cdr) {
  return new BiwaScheme.Pair(car, cdr);
};
