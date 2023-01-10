import { nil } from "../header.js"
import { to_write, inspect } from "./_writer.js"
import BiwaSet from "./set.js"

//
// Pair 
// cons cell
//

class Pair {
  constructor(car, cdr){
    this.car = car;
    this.cdr = cdr;
  }

  // Returns `this.car.car`. If `err` is given and `this.car` is not a pair,
  // throws `err`.
  caar(err){ return this._get(["car", "car"], err) }
  cadr(err){ return this._get(["cdr", "car"], err) }
  cdar(err){ return this._get(["car", "cdr"], err) }
  cddr(err){ return this._get(["cdr", "cdr"], err) }
  _get(props, err="unexpected") {
    let x = this;
    for (const p of props) {
      if (x.hasOwnProperty(p)) {
        x = x[p];
      } else {
        throw err;
      }
    }
    return x;
  }

  first(){ return this.car; }
  second(){ return this.cdr.car; }
  third(){ return this.cdr.cdr.car; }
  fourth(){ return this.cdr.cdr.cdr.car; }
  fifth(){ return this.cdr.cdr.cdr.cdr.car; }

  // returns array containing all the car's of list
  // '(1 2 3) => [1,2,3]
  // '(1 2 . 3) => [1,2]
  to_array(){
    var ary = [];
    for(var o = this; o instanceof Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  }

  to_set(){
    var set = new BiwaSet();
    for(var o = this; o instanceof Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  }

  length(){
    var n = 0;
    for(var o = this; o instanceof Pair; o=o.cdr){
      n++;
    }
    return n;
  }

  // Return the last cdr
  last_cdr(){
    var o;
    for(o = this; o instanceof Pair; o = o.cdr)
      ;
    return o;
  }

  // calls the given func passing each car of list
  // returns cdr of last Pair
  forEach(func){
    for(var o = this; o instanceof Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  }

  // Alias of `forEach` (for backward compatibility)
  foreach(func){
    for(var o = this; o instanceof Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  }

  // Returns an array which contains the resuls of calling func
  // with the car's as an argument.
  // If the receiver is not a proper list, the last cdr is ignored.
  // The receiver must not be a cyclic list.
  map(func){
    var ary = [];
    for(var o = this; isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  }

  // Returns a new list made by applying `func` to each element
  mapList(func) {
    return array_to_list(this.map(func));
  }

  async mapAsync(func) {
    const ary = [];
    for(var o = this; isPair(o); o = o.cdr){
      ary.push(await func(o.car));
    }
    return array_to_list(ary);
  }

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat(list){
    var o = this;
    while(o instanceof Pair && o.cdr != nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  }

  // returns human-redable string of pair
  inspect(conv){
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
  }

  toString (){
    return this.inspect();
  }

  to_display(to_display) {
    return this.inspect(to_display);
  }

  to_write(){
    return this.inspect(to_write);
  }
};

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

// Takes a JS Array of lists and destructively concatenates them.
// Returns a concatenated list.
const concatLists = (lists) => {
  if (lists.length == 0) return nil;
  let dst = nil;
  let tail;
  for (const list of lists) { 
    if (dst === nil) {
      dst = list;
    } else {
      tail.cdr = list;
    }
    tail = list;
    while(tail instanceof Pair && tail.cdr != nil){
      tail = tail.cdr;
    }
  }
  return dst;
}

// Creates a list out of the arguments, optionally converting any nested arrays into nested lists if the deep argument is true.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (list 1 2 (vector 3 4))
//   BiwaScheme.deep_array_to_list(1, 2, [3, 4]) ;=> (list 1 2 (list 3 4))
const array_to_list_ = function(ary, deep) {
  var list = nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && Array.isArray(obj) && !obj.is_vector){
      obj = array_to_list_(obj, deep);
    }
    list = new Pair(obj, list);
  }
  return list;
}

// Shallow: List(1, 2, [3]) == (list 1 2 (vector 3 4))
const List = function() {
  var ary = Array.from(arguments);
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
  Object.keys(obj).forEach(function(key) {
    arr.push(new Pair(key, obj[key]));
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

// Returns a new list made by applying `f` to each `car` and the last `cdr`
const mapCarAndCdr = function(ls, f) {
  if (ls === nil) {
    return f(nil);
  } else {
    return Cons(f(ls.car), mapCarAndCdr(ls.cdr, f));
  }
};

// Returns an array of each `car` and the last `cdr`, if it is not nil.
const collectCarAndCdr = function(ls, f) {
  if (ls === nil) {
    return [];
  } else {
    return [ls.car].concat(collectCarAndCdr(ls.cdr));
  }
};

/** Apply async function to each item of the list one by one.
 * Returns a list of the results.
 */
async function mapAsync(ls, func) {
  const ary = [];
  for (var o = ls; isPair(o); o = o.cdr) {
    ary.push(await func(o.car));
  }
  return array_to_list(ary);
}

export { Pair, List, isPair, isList, concatLists, array_to_list, deep_array_to_list, Cons,
         js_obj_to_alist, alist_to_js_obj, mapCarAndCdr, collectCarAndCdr, mapAsync };
