//
// write.js: Functions to convert objects to strings
//

//
// write
//

BiwaScheme.to_write = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(_.isFunction(obj))
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? _.str.truncate(obj.toSource(), 40) :
                          "")+">";
  else if(_.isString(obj))
    return '"' +
           obj.replace(/\\|\"/g,function($0){return'\\'+$0;})
              .replace(/\x07/g, "\\a")
              .replace(/\x08/g, "\\b")
              .replace(/\t/g, "\\t")
              .replace(/\n/g, "\\n")
              .replace(/\v/g, "\\v")
              .replace(/\f/g, "\\f")
              .replace(/\r/g, "\\r") +
           '"';
  else if(_.isArray(obj) && obj.closure_p)
    return "#<Closure>";
  else if(_.isArray(obj))
    return "#(" + _.map(obj, function(e) { return BiwaScheme.to_write(e); }).join(" ") + ")";
  else if(typeof(obj.to_write) == 'function')
    return obj.to_write();
  else if(isNaN(obj) && typeof(obj) == 'number')
    return "+nan.0";
  else{
    switch(obj){
      case true: return "#t";
      case false: return "#f";
      case Infinity: return "+inf.0";
      case -Infinity: return "-inf.0";
    }
  }
  return BiwaScheme.inspect(obj);
}

//
// display
//

BiwaScheme.to_display = function(obj){
  if(_.isUndefined(obj))
    return 'undefined';
  else if(_.isNull(obj))
    return 'null';
  else if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaScheme.Symbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + _.map(obj, BiwaScheme.to_display).join(' ') + ')';
  else if(obj instanceof BiwaScheme.Pair)
    return obj.inspect(BiwaScheme.to_display);
  else if(obj instanceof BiwaScheme.Char)
    return obj.value;
  else
    return BiwaScheme.to_write(obj);
}

//
// write/ss (write with substructure)
//

// example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
// 2-pass algorithm.
// (1) detect all the objects which appears more than once
//     (find_cyclic, reduce_cyclic_info)
// (2) write object using this information
//   * add prefix '#n=' for first appearance
//   * just write '#n#' for other appearance

//TODO: support Values
BiwaScheme.write_ss = function(obj, array_mode){
  var known = [obj], used = [false];
  BiwaScheme.find_cyclic(obj, known, used);
  var cyclic   = BiwaScheme.reduce_cyclic_info(known, used);
  var appeared = new Array(cyclic.length);
  for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

  return BiwaScheme.to_write_ss(obj, cyclic, appeared, array_mode);
}
BiwaScheme.to_write_ss = function(obj, cyclic, appeared, array_mode){
  var ret = "";
  var i = cyclic.indexOf(obj);
  if(i >= 0){
    if(appeared[i]){
      return "#"+i+"#";
    }
    else{
      appeared[i] = true;
      ret = "#"+i+"=";
    }
  }

  if(obj instanceof BiwaScheme.Pair){
    var a = [];
    a.push(BiwaScheme.to_write_ss(obj.car, cyclic, appeared, array_mode));
    for(var o=obj.cdr; o != BiwaScheme.nil; o=o.cdr){
      if(!(o instanceof BiwaScheme.Pair) || cyclic.indexOf(o) >= 0){
        a.push(".");
        a.push(BiwaScheme.to_write_ss(o, cyclic, appeared, array_mode));
        break;
      }
      a.push(BiwaScheme.to_write_ss(o.car, cyclic, appeared, array_mode));
    }
    ret += "(" + a.join(" ") + ")";
  }
  else if(obj instanceof Array){
    var a = _.map(obj, function(item){
      return BiwaScheme.to_write_ss(item, cyclic, appeared, array_mode);
    })
    if(array_mode)
      ret += "[" + a.join(", ") + "]";
    else
      ret += "#(" + a.join(" ") + ")";
  }
  else{
    ret += BiwaScheme.to_write(obj);
  }
  return ret;
}
BiwaScheme.reduce_cyclic_info = function(known, used){
  var n_used = 0;
  for(var i=0; i<used.length; i++){
    if(used[i]){
      known[n_used] = known[i];
      n_used++;
    }
  }
  return known.slice(0, n_used);
}
BiwaScheme.find_cyclic = function(obj, known, used){
  var items = (obj instanceof BiwaScheme.Pair)  ? [obj.car, obj.cdr] :
              (obj instanceof Array) ? obj :
              null;
  if(!items) return;

  _.each(items, function(item){
    if(typeof(item)=='number' || typeof(item)=='string' ||
      item === BiwaScheme.undef || item === true || item === false ||
      item === BiwaScheme.nil || item instanceof BiwaScheme.Symbol) return;

    var i = known.indexOf(item);
    if(i >= 0)
      used[i] = true;
    else{
      known.push(item);
      used.push(false);
      BiwaScheme.find_cyclic(item, known, used);
    }
  });
};

//
// inspect
//
BiwaScheme.inspect = function(object, opts) {
  try {
    if (_.isUndefined(object)) return 'undefined';
    if (object === null) return 'null';
    if (object === true) return '#t';
    if (object === false) return '#f';
    if (object.inspect) return object.inspect();
    if (_.isString(object)) {
      return '"' + object.replace(/"/g, '\\"') + '"';
    }
    if (_.isArray(object)) {
      return '[' + _.map(object, BiwaScheme.inspect).join(', ') + ']';
    }

    if (opts && opts["fallback"]){
      return opts["fallback"];
    }
    else {
      return object.toString();
    }
  } catch (e) {
    if (e instanceof RangeError) return '...';
    throw e;
  }
};
