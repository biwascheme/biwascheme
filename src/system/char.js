import Class from "./class.js"

//
// Char
//
import _ from "../deps/underscore-1.10.2-esm.js"

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
  inspect: function(){
    return this.to_write();
  }
});

Char.get = function(c) {
  if(typeof(c) != "string") {
    throw new BiwaScheme.Bug("Char.get: " +
                             BiwaScheme.inspect(c) + " is not a string");
  }
  if( Chars[c] === undefined )
    return new Char(c);
  else
    return Chars[c];
};

export default Char;
