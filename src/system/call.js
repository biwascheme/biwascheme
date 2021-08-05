import * as _ from "../deps/underscore-esm.js"
import { nil } from "../header.js";
import { inspect } from "./_writer.js"
import { Char } from "./char.js"
import { Bug } from "./error.js"
import { Pair } from "./pair.js"

// The class Call is used to invoke scheme closure from 
// library functions.
//
// Call#constructor takes three arguments: proc, args and after.
//   * proc is the scheme closure to invoke.
//   * args is an Array (not list!) of arguments for the invocation.
//   * after is a javascript function which is invoked when 
//     returned from the proc.
//
//     after takes two arguments: ar and intp.
//       * ar is an Array which contains the result of the invocation.
//       * intp is an Interpreter which is running.
//
//     If after returns another Call object, another invocation
//     happens. If after returns a normal value, it is the value
//     of the library function.
//
// example:
//   return new Call(proc, [x, y], function(ar){ ar[0] });
//
class Call {
  constructor(proc, args, after){
    this.proc = proc;
    this.args = args;
    this.after = after || function(ar){
      // just return result which closure returned
      return ar[0];
    };
  }

  inspect(){
    return "#<Call args=" + this.args.inspect() + ">";
  }

  toString(){
    return "#<Call>";
  }

  to_write(){
    return "#<Call>";
  }
}

//
// Iterator - external iterator for Call.foreach
//
const Iterator = {
  ForArray: class {
    constructor(arr){
      this.arr = arr;
      this.i = 0;
    }
    has_next(){
      return this.i < this.arr.length;
    }
    next(){
      return this.arr[this.i++];
    }
  },

  ForString: class {
    constructor(str){
      this.str = str;
      this.i = 0;
    }
    has_next(){
      return this.i < this.str.length;
    }
    next(){
      return Char.get(this.str.charAt(this.i++));
    }
  },

  ForList: class {
    constructor(ls){
      this.ls = ls;
    }
    has_next(){
      return (this.ls instanceof Pair) &&
             this.ls != nil;
    }
    next(){
      var pair = this.ls;
      this.ls = this.ls.cdr;
      return pair;
    }
  },

  ForMulti: class {
    constructor(objs){
      this.objs = objs;
      this.size = objs.length;
      this.iterators = _.map(objs, function(x){
        return Iterator.of(x);
      })
    }
    has_next(){
      for(var i=0; i<this.size; i++)
        if(!this.iterators[i].has_next())
          return false;
      
      return true;
    }
    next(){
      return _.map(this.iterators, function(ite){
        return ite.next();
      })
    }
  },

  of: function(obj){
    switch(true){
      case (obj instanceof Array):
        return new this.ForArray(obj);
      case (typeof(obj) == "string"):
        return new this.ForString(obj);
      case (obj instanceof Pair):
      case (obj === nil):
        return new this.ForList(obj);
      default:
        throw new Bug("Iterator.of: unknown class: "+inspect(obj));
    }
  }
}

//
// Call.foreach - shortcut for successive Calls
//
// Some library functions, such as for-each or map,
// call a closure for each element. Call.foreach is 
// a utility to help defining such methods.
//
// Call.foreach takes a sequence and some callbacks.
// Sequence is an Array, String, or list.
//
// Example:
//   return Call.foreach(sequence, {
//     // before each call
//     call: function(elem){
//       return new Call(proc, [elem]);
//     },
//     // after each call
//     result: function(value, elem){
//       ary.push(value);
//       // you can return a value to terminate the loop
//     },
//     // after all the calls
//     finish: function(){
//       return ary;
//     }
//   });

Call.default_callbacks = {
  call: function(x){ return new Call(this.proc, [x]) },
  result: function(){},
  finish: function(){}
}
Call.foreach = function(obj, callbacks, is_multi){
  is_multi || (is_multi = false);
  _.each(["call", "result", "finish"], function(key){
    if(!callbacks[key])
      callbacks[key] = Call.default_callbacks[key];
  })
  
  var iterator = null;
  var x = null;

  var loop = function(ar){
    if(iterator){
      var ret = callbacks["result"](ar[0], x);
      if(ret !== undefined) return ret;
    }
    else{ // first lap
      if(is_multi)
        iterator = new Iterator.ForMulti(obj);
      else
        iterator = Iterator.of(obj);
    }

    if(!iterator.has_next()){
      return callbacks["finish"]();
    }
    else{
      x = iterator.next();
      var result = callbacks["call"](x);
      result.after = loop;
      return result;
    }
  }
  return loop(null);
}
Call.multi_foreach = function(obj, callbacks){
  return Call.foreach(obj, callbacks, true);
}

export default Call;
