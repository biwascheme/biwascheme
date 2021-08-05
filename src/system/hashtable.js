import * as _ from "../deps/underscore-esm.js"
import { eq, eqv } from "./_equality.js"
import { to_write } from "./_writer.js"
import { Bug } from "./error.js"
import { BiwaSymbol } from "./symbol.js"

//
// Hashtable
//
// TODO: Reimplement with JavaScript Map
//
// Based on the base JavaScript Object class, but
//  * Object takes only strings as keys
//  * R6RS hashtable needs its own hash function
// so some hacks are needed.

class Hashtable {
  constructor(_hash_proc, _equiv_proc, mutable){
    this.mutable = (mutable === undefined) ? true :
                   mutable ? true : false;

    this.hash_proc = _hash_proc;
    this.equiv_proc = _equiv_proc;

    // Hash (hashed) => (array of (key and value))
    this.pairs_of = {};
  }

  clear(){
    this.pairs_of = {};
  }

  candidate_pairs(hashed){
    return this.pairs_of[hashed];
  }

  add_pair(hashed, key, value){
    var pairs = this.pairs_of[hashed];

    if (pairs) {
      pairs.push([key, value]);
    }
    else {
      this.pairs_of[hashed] = [[key, value]];
    }
  }

  remove_pair(hashed, pair){
    var pairs = this.pairs_of[hashed];
    var i = pairs.indexOf(pair);
    if (i == -1){
      throw new Bug("Hashtable#remove_pair: pair not found!");
    }
    else {
      pairs.splice(i, 1); //remove 1 element from i-th index
    }
  }

  create_copy(mutable){
    var copy = new Hashtable(this.hash_proc, this.equiv_proc,
                                        mutable);
    // clone the pairs to copy
    _.each(_.keys(this.pairs_of), _.bind(function(hashed){
      var pairs = this.pairs_of[hashed];
      var cloned = _.map(pairs, function(pair){
        return _.clone(pair);
      });
      copy.pairs_of[hashed] = cloned;
    }, this));

    return copy;
  }

  size(){
    var n = 0;
    this._apply_pair(function(pair){
      n++;
    });
    return n;
  }

  keys(){
    return this._apply_pair(function(pair){
      return pair[0];
    });
  }

  values(){
    return this._apply_pair(function(pair){
      return pair[1];
    });
  }

  _apply_pair(func){
    var a = [];
    _.each(_.values(this.pairs_of), function(pairs){
      _.each(pairs, function(pair){
        a.push(func(pair));
      });
    });
    return a;
  }

  to_write(){
    return "#<Hashtable size=" + this.size() + ">";
  }
}

const isHashtable = function(obj){
  return (obj instanceof Hashtable);
};

const isMutableHashtable = function(obj){
  return (obj instanceof Hashtable) && obj.mutable;
};

//
// Hash functions
//

Hashtable.equal_hash = function(ar){
  return to_write(ar[0]);
};
Hashtable.eq_hash = Hashtable.equal_hash;
Hashtable.eqv_hash = Hashtable.equal_hash;

Hashtable.string_hash = function(ar){
  return ar[0];
};

Hashtable.string_ci_hash = function(ar){
  return _.isString(ar[0]) ? ar[0].toLowerCase() : ar[0];
};

Hashtable.symbol_hash = function(ar){
  return (ar[0] instanceof BiwaSymbol) ? ar[0].name : ar[0];
};

//
// Equivalence functions
//

Hashtable.eq_equiv = function(ar){
  return eq(ar[0], ar[1]);
};

Hashtable.eqv_equiv = function(ar){
  return eqv(ar[0], ar[1]);
};

export { Hashtable, isHashtable, isMutableHashtable };
