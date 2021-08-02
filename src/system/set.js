import * as _ from "../deps/underscore-esm.js"
import { BiwaError } from "./error.js"

//
// Set - set of string
// contents must be string (or at least sortable)
//
class BiwaSet {
  constructor(/*args*/){
    this.arr = [];
    var i;
    for(i=0; i<arguments.length; i++)
      this.arr[i] = arguments[i];
  }

  equals(other){
    if(this.arr.length != other.arr.length)
      return false;

    var a1 = _.clone(this.arr);
    var a2 = _.clone(other.arr);
    a1.sort();
    a2.sort();
    for(var i=0; i<this.arr.length; i++){
      if(a1[i] != a2[i]) return false;
    }
    return true;
  }

  set_cons(item){
    var o = new BiwaSet(item);
    o.arr = _.clone(this.arr);
    o.arr.push(item);
    return o;
  }

  set_union(/*args*/){
    var o = new BiwaSet();
    o.arr = _.clone(this.arr);

    for(var k=0; k<arguments.length; k++){
      var s2 = arguments[k];
      if(!(s2 instanceof BiwaSet))
        throw new BiwaError("set_union: arguments must be a set");

      for(var i=0; i<s2.arr.length; i++)
        o.add(s2.arr[i]);
    }
    return o;
  }

  set_intersect(s2){
    if(!(s2 instanceof BiwaSet))
      throw new BiwaError("set_intersect: arguments must be a set");

    var o = new BiwaSet();
    for(var i=0; i<this.arr.length; i++)
      if(s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  }

  set_minus(s2){
    if(!(s2 instanceof BiwaSet))
      throw new BiwaError("set_minus: arguments must be a set");

    var o = new BiwaSet();
    for(var i=0; i<this.arr.length; i++)
      if(!s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  }

  add(item){
    if(!this.member(item)){
      this.arr.push(item);
    }
  }

  member(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return true;

    return false;
  }

  rindex(item){
    for(var i=this.arr.length-1; i>=0 ; i--)
      if(this.arr[i] == item) return (this.arr.length-1-i);

    return null;
  }

  index(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return i;

    return null;
  }

  inspect(){
    return "Set(" + this.arr.join(", ") + ")";
  }

  toString(){
    return this.inspect();
  }

  size(){
    return this.arr.length;
  }
}

export default BiwaSet;
