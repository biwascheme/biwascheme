import Class from "./class.js"
import _ from "../deps/underscore-1.10.2-esm.js"

//
// Symbol
//

const Symbols = {};

const Symbol = Class.create({
  initialize: function(str){
    this.name = str;
    Symbols[ str ] = this;
  },

  inspect: function(){
    return "'"+this.name;
    //return "#<Symbol '"+this.name+"'>";
  },

  toString: function(){
    return "'"+this.name;
  },

  to_write: function(){
    return this.name;
  }
});

const Sym = function(name,leaveCase){
  if( Symbols[name] === undefined ){
    return new Symbol(name);
  }
  else if( ! (Symbols[name] instanceof Symbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new Symbol(name);
  }
  else{
    return Symbols[name];
  }
}

const gensym = function(){
  return Sym(_.uniqueId("__gensym"));
};

export {Symbol, Symbols, Sym, gensym};
