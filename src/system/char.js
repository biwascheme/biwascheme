import * as _ from "../deps/underscore-esm.js"
import { inspect } from "./_writer.js"
import Class from "./class.js"
import { Bug } from "./error.js"

//
// Char
//

const Chars = {};

const Char = Class.create({
  initialize: function(c){
    Chars[ this.value = c ] = this;
  },
  to_write: function(){
    switch(this.value){
      case '\n': return "#\\newline";
      case ' ':  return "#\\space";
      case '\t': return "#\\tab";
      default:   return "#\\"+this.value;
    }
  },
  to_display: function(){
    return this.value;
  },
  inspect: function(){
    return this.to_write();
  }
});

Char.get = function(c) {
  if(typeof(c) != "string") {
    throw new Bug("Char.get: " + inspect(c) + " is not a string");
  }
  if( Chars[c] === undefined )
    return new Char(c);
  else
    return Chars[c];
};

export default Char;
