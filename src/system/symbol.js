import * as _ from "../deps/underscore-esm.js"

//
// Scheme symbols
//

const Symbols = {};

class BiwaSymbol {
  constructor(str){
    this.name = str;
    Symbols[ str ] = this;
  }

  inspect(){
    return "'"+this.name;
    //return "#<Symbol '"+this.name+"'>";
  }

  toString(){
    return "'"+this.name;
  }

  to_write(){
    return this.name;
  }
}

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

const gensym = function(prefix="__gensym" ){
  return Sym(_.uniqueId(prefix));
};

export {BiwaSymbol, Symbols, Sym, gensym};
