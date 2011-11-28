//
// Symbol
//

BiwaScheme.Symbol = BiwaScheme.Class.create({
  initialize: function(str){
    this.name = str;
    BiwaScheme.Symbols[ str ] = this;
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
BiwaScheme.Symbols = {};
BiwaScheme.Sym = function(name,leaveCase){
  if( BiwaScheme.Symbols[name] === undefined ){
    return new BiwaScheme.Symbol(name);
  }
  else if( ! (BiwaScheme.Symbols[name] instanceof BiwaScheme.Symbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new BiwaScheme.Symbol(name);
  }
  else{
    return BiwaScheme.Symbols[name];
  }
}

BiwaScheme.gensym = function(){
  return BiwaScheme.Sym(_.uniqueId("__gensym"));
};
