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
  return (obj instanceof BiwaScheme.Pair);
};

// Returns true if obj is a proper list
// Note: isList returns true for '()
BiwaScheme.isList = function(obj){
  var nil = BiwaScheme.nil, Pair = BiwaScheme.Pair;

  if (obj === nil) { // Empty list
    return true;
  }
  if (!(obj instanceof Pair)) { // Argument isn't even a pair
    return false;
  }

  var tortoise = obj;
  var hare = obj.cdr;
  while (true) {
    if (hare === nil) { // End of list
      return true;
    }
    if (hare === tortoise) { // Cycle
      return false;
    }
    if (!(hare instanceof Pair)) { // Improper list
      return false;
    }

    if (hare.cdr === nil) { // End of list
      return true;
    }
    if (!(hare.cdr instanceof Pair)) { // Improper list
      return false;
    }

    hare = hare.cdr.cdr;
    tortoise = tortoise.cdr;
  }
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
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
};
