//
// types.js - type predicators
//

// Return true if obj is a Pair (note that nil is not a pair in scheme)
BiwaScheme.isPair = function(obj){
  return (obj instanceof BiwaScheme.Pair) && (obj !== BiwaScheme.nil);
};

BiwaScheme.isNil = function(obj){
  return (obj === BiwaScheme.nil);
};

BiwaScheme.isUndef = function(obj){
  return (obj === BiwaScheme.undef);
};

BiwaScheme.isChar = function(obj){
  return (obj instanceof BiwaScheme.Char);
};

BiwaScheme.isPort = function(obj){
  return (obj instanceof BiwaScheme.Port);
};

BiwaScheme.isHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable);
};

BiwaScheme.isMutableHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.is_mutable;
};
