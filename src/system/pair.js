import * as _ from "../deps/underscore-esm.js"
import { nil } from "../header.js"
import { to_write, inspect } from "./_writer.js"
import Class from "./class.js"
import BiwaSet from "./set.js"

//
// Pair 
// cons cell
//

const Pair = Class.create({
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
    for(var o = this; o instanceof Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  },

  to_set: function(){
    var set = new BiwaSet();
    for(var o = this; o instanceof Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  },

  length: function(){
    var n = 0;
    for(var o = this; o instanceof Pair; o=o.cdr){
      n++;
    }
    return n;
  },

  // Return the last cdr
  last_cdr: function(){
    var o;
    for(o = this; o instanceof Pair; o = o.cdr)
      ;
    return o;
  },

  // calls the given func passing each car of list
  // returns cdr of last Pair
  forEach: function(func){
    for(var o = this; o instanceof Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  },

  // Alias of `forEach` (for backward compatibility)
  foreach: function(func){
    for(var o = this; o instanceof Pair; o=o.cdr){
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
    for(var o = this; isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  },

  // Returns a new list made by applying `func` to each element
  mapList: function(func) {
    return array_to_list(this.map(func));
  },

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat: function(list){
    var o = this;
    while(o instanceof Pair && o.cdr != nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  },

  // returns human-redable string of pair
  inspect: function(conv){
    conv || (conv = inspect);
    var a = [];
    var last = this.foreach(function(o){
      a.push(conv(o));
    });
    if(last != nil){
      a.push(".");
      a.push(conv(last));
    }
    return "(" + a.join(" ") + ")";
  },
  toString : function(){
    return this.inspect();
  },

  to_display: function(to_display) {
    return this.inspect(to_display);
  },

  to_write: function(){
    return this.inspect(to_write);
  }
});

// Note: '() is not a pair in scheme
const isPair = function(obj){
  return (obj instanceof Pair);
};

// Returns true if obj is a proper list
// Note: isList returns true for '()
const isList = function(obj){
  if (obj === nil) { // Empty list
    return true;
  }
  if (!(obj instanceof Pair)) { // Argument isn't even a pair
    return false;
  }

  var tortoise = obj;
  var hare = obj.cdr;
  while (true) {
    if (hare === nil) { // End of list
      return true;
    }
    if (hare === tortoise) { // Cycle
      return false;
    }
    if (!(hare instanceof Pair)) { // Improper list
      return false;
    }

    if (hare.cdr === nil) { // End of list
      return true;
    }
    if (!(hare.cdr instanceof Pair)) { // Improper list
      return false;
    }

    hare = hare.cdr.cdr;
    tortoise = tortoise.cdr;
  }
};

// Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
//   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
const array_to_list_ = function(ary, deep) {
  var list = nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && _.isArray(obj) && !obj.is_vector){
      obj = array_to_list_(obj, deep);
    }
    list = new Pair(obj, list);
  }
  return list;
}

// Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
const List = function() {
  var ary = _.toArray(arguments);
  return array_to_list_(ary, false);
};

// Shallow: array_to_list(1, 2, [3]) == (list 1 2 (vector 3 4))
const array_to_list = function(ary) {
  return array_to_list_(ary, false);
};

// Deep: deep_array_to_list(1, 2, [3, 4]) == (list 1 2 (list 3 4))
// deep_array_to_list([1, 2, 3]) - deep
const deep_array_to_list = function(ary) {
  return array_to_list_(ary, true);
};

const Cons = function(car, cdr) {
  return new Pair(car, cdr);
};

const js_obj_to_alist = function(obj) {
  if (obj === undefined) {
    return nil;
  }
  var arr = [];
  _.each(obj, function(val, key) {
    arr.push(new Pair(key, val));
  });
  var alist = array_to_list(arr);
  return alist;
};

const alist_to_js_obj = function(alist) {
  if (alist === nil) {
    return {} ;
  }
  var obj = {};
  alist.foreach(function(item){
    obj[item.car] = item.cdr;
  });
  return obj;
};


export { Pair, List, isPair, isList, array_to_list, deep_array_to_list, Cons,
         js_obj_to_alist, alist_to_js_obj };
