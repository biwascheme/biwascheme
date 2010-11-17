//
// types.js - type predicators
//

BiwaScheme.isNil = function(obj){
  return (obj === BiwaScheme.nil);
};

BiwaScheme.isUndef = function(obj){
  return (obj === BiwaScheme.undef);
};

BiwaScheme.isChar = function(obj){
  return (obj instanceof BiwaScheme.Char);
};

BiwaScheme.isSymbol = function(obj){
  return (obj instanceof BiwaScheme.Symbol);
};

BiwaScheme.isPort = function(obj){
  return (obj instanceof BiwaScheme.Port);
};

// Note: '() is not a pair in scheme
BiwaScheme.isPair = function(obj){
  return (obj instanceof BiwaScheme.Pair) && (obj !== BiwaScheme.nil);
};

// Note: isList returns true for '()
BiwaScheme.isList = function(obj){
  return (obj instanceof BiwaScheme.Pair);
  // should check it is proper and not cyclic..
};

BiwaScheme.isVector = function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
};

BiwaScheme.isHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable);
};

BiwaScheme.isMutableHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.mutable;
};

BiwaScheme.isClosure = function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
BiwaScheme.isProcedure = function(obj){
  return BiwaScheme.isClosure(obj) || Object.isFunction(obj);
};
