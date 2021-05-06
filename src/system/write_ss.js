import * as _ from "../deps/underscore-1.10.2-esm.js"
import { to_write } from "./_writer.js"
import { nil, undef } from "../header.js"
import { BiwaSymbol } from "./symbol.js"
import { Pair } from "./pair.js"

//
// write/ss (write with substructure)
//

// example:  > (let ((x (list 'a))) (list x x))
//           (#0=(a) #0#)
// 2-pass algorithm.
// (1) detect all the objects which appears more than once
//     (find_cyclic, reduce_cyclic_info)
// (2) write object using this information
//   * add prefix '#n=' for first appearance
//   * just write '#n#' for other appearance

const write_ss = function(obj, array_mode){
  var known = [obj], used = [false];
  find_cyclic(obj, known, used);
  var cyclic   = reduce_cyclic_info(known, used);
  var appeared = new Array(cyclic.length);
  for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

  return _write_ss(obj, cyclic, appeared, array_mode);
}

const _write_ss = function(obj, cyclic, appeared, array_mode){
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

  if(obj instanceof Pair){
    var a = [];
    a.push(_write_ss(obj.car, cyclic, appeared, array_mode));
    for(var o=obj.cdr; o != nil; o=o.cdr){
      if(!(o instanceof Pair) || cyclic.indexOf(o) >= 0){
        a.push(".");
        a.push(_write_ss(o, cyclic, appeared, array_mode));
        break;
      }
      a.push(_write_ss(o.car, cyclic, appeared, array_mode));
    }
    ret += "(" + a.join(" ") + ")";
  }
  else if(obj instanceof Array){
    var a = _.map(obj, function(item){
      return _write_ss(item, cyclic, appeared, array_mode);
    })
    if(array_mode)
      ret += "[" + a.join(", ") + "]";
    else
      ret += "#(" + a.join(" ") + ")";
  }
  else{
    ret += to_write(obj);
  }
  return ret;
}

const reduce_cyclic_info = function(known, used){
  var n_used = 0;
  for(var i=0; i<used.length; i++){
    if(used[i]){
      known[n_used] = known[i];
      n_used++;
    }
  }
  return known.slice(0, n_used);
}

const find_cyclic = function(obj, known, used){
  var items = (obj instanceof Pair)  ? [obj.car, obj.cdr] :
              (obj instanceof Array) ? obj :
              null;
  if(!items) return;

  _.each(items, function(item){
    if(typeof(item)=='number' || typeof(item)=='string' ||
      item === undef || item === true || item === false ||
      item === nil || item instanceof BiwaSymbol) return;

    var i = known.indexOf(item);
    if(i >= 0)
      used[i] = true;
    else{
      known.push(item);
      used.push(false);
      find_cyclic(item, known, used);
    }
  });
};

export { write_ss, reduce_cyclic_info, find_cyclic };
