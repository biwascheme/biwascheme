//
// Hashtable
//
// Based on the class Hash of prototype.js, but
//  * Hash takes only strings as keys
//  * R6RS hashtable needs its own hash function
// so some hacks are needed.

BiwaScheme.Hashtable = Class.create({
  initialize: function(_hash_proc, _equiv_proc, mutable){
    this.mutable = (mutable === undefined) ? true :
                   mutable ? true : false;

    this.hash_proc = _hash_proc;
    this.equiv_proc = _equiv_proc;

    // Hash (hashed) => (array of (key and value))
    this.pairs_of = new Hash();
  },

  clear: function(){
    this.pairs_of = new Hash();
  },

  candidate_pairs: function(hashed){
    return this.pairs_of.get(hashed);
  },

  add_pair: function(hashed, key, value){
    var pairs = this.pairs_of.get(hashed);

    if (pairs) {
      pairs.push([key, value]);
    }
    else {
      this.pairs_of.set(hashed, [[key, value]]);
    }
  },

  remove_pair: function(hashed, pair){
    var pairs = this.pairs_of.get(hashed);
    var i = pairs.indexOf(pair);
    if (i == -1){
      throw new BiwaScheme.Bug("Hashtable#remove_pair: pair not found!");
    }
    else {
      pairs.splice(i, 1); //remove 1 element from i-th index
    }
  },

  create_copy: function(mutable){
    var copy = new BiwaScheme.Hashtable(this.hash_proc, this.equiv_proc,
                                        mutable);
    // clone the pairs to copy
    this.pairs_of.each(function(hashed_and_pairs){
      var cloned = hashed_and_pairs[1].map(function(pair){
        return pair.clone();
      });
      copy.pairs_of.set(hashed_and_pairs[0], cloned);
    });

    return copy;
  },

  size: function(){
    var n = 0;
    this._apply_pair(function(pair){
      n++;
    });
    return n;
  },

  keys: function(){
    return this._apply_pair(function(pair){
      return pair[0];
    });
  },

  values: function(){
    return this._apply_pair(function(pair){
      return pair[1];
    });
  },

  _apply_pair: function(func){
    var a = [];
    this.pairs_of.values().each(function(pairs){
      pairs.each(function(pair){
        a.push(func(pair));
      });
    });
    return a;
  },

  to_write: function(){
    return "#<Hashtable size=" + this.size() + ">";
  }
});

//
// Hash functions
//

BiwaScheme.Hashtable.equal_hash = function(ar){
  return BiwaScheme.to_write(ar[0]);
};
BiwaScheme.Hashtable.eq_hash = BiwaScheme.Hashtable.equal_hash;
BiwaScheme.Hashtable.eqv_hash = BiwaScheme.Hashtable.equal_hash;

BiwaScheme.Hashtable.string_hash = function(ar){
  return ar[0];
};

BiwaScheme.Hashtable.string_ci_hash = function(ar){
  return Object.isString(ar[0]) ? ar[0].toLowerCase() : ar[0];
};

BiwaScheme.Hashtable.symbol_hash = function(ar){
  return (ar[0] instanceof BiwaScheme.Symbol) ? ar[0].name : ar[0];
};

//
// Equivalence functions
//

BiwaScheme.Hashtable.eq_equiv = function(ar){
  return BiwaScheme.eq(ar[0], ar[1]);
};

BiwaScheme.Hashtable.eqv_equiv = function(ar){
  return BiwaScheme.eqv(ar[0], ar[1]);
};
