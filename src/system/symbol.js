import Class from "./class.js"
import * as _ from "../deps/underscore-esm.js"

//
// Symbol
//

const Symbols = {};

const BiwaSymbol = Class.create({
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
    return new BiwaSymbol(name);
  }
  else if( ! (Symbols[name] instanceof BiwaSymbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new BiwaSymbol(name);
  }
  else{
    return Symbols[name];
  }
}

const gensym = function(){
  return Sym(_.uniqueId("__gensym"));
};

export {BiwaSymbol, Symbols, Sym, gensym};
