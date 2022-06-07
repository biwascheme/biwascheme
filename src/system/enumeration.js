import { any, all, bind, include, indexOf, uniq } from "../deps/underscore-esm.js"
import { inspect } from "./_writer.js"
import { make_simple_assert } from "./assert.js"
import { array_to_list } from "./pair.js"
import { assert_symbol, assert_list } from "../library/infra.js"

// 
// R6RS Enumerations
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-15.html#node_chap_14
//
// Example
//
//   (define-enumeration color
//     (black white purple maroon)
//     color-set)
//   
//   (color black)                  ;=> 'black
//   (color purpel)                 ;=> &syntax exception
//   (enum-set->list
//     (color-set maroon white))    ;=> #<enum-set (white maroon)>

const Enumeration = {};

// Represents an enum_type.
//
// Becuase there is no way to access an EnumType directly from Scheme,
// EnumType#to_write is not defined.
//
// Properties
//
// members - Array of symbols (no duplicate)
//
Enumeration.EnumType = class {
  // Creates a new enum_type.
  //
  // members - Array of symbols.
  //           Symbols may be duplicate (I think you shouldn't, though :-p).
  constructor(members){
    this.members = uniq(members);
  }

  // Returns an EnumSet.
  universe(){
    return new Enumeration.EnumSet(this, this.members);
  }

  // Returns a function which map a symbol to an integer (or #f, if 
  // the symbol is out of the universe).
  // 
  // Implementation note: don't forget this.members may have duplicates.
  indexer(){
    // ar[0] - a symbol
    // Returns an integer or #f.
    return bind(function(ar){
      assert_symbol(ar[0], "(enum-set indexer)");
      var idx = indexOf(this.members, ar[0]);
      return (idx === -1) ? false : idx;
    }, this);
  }

  // Retuns a function which creates an enum_set from a list of
  // symbols (Symbols may be duplicate.)
  constructor_(){
    // ar[0] - a list of symbol
    // Returns a enum_set.
    return bind(function(ar){
      assert_list(ar[0], "(enum-set constructor)");
      var symbols = ar[0].to_array();
      symbols.forEach(function(arg){
        assert_symbol(arg, "(enum-set constructor)");
      });

      return new Enumeration.EnumSet(this, symbols);
    }, this);
  }
}

// Represents an enum_set of an enum_type.
//
// Properties
//
// enum_type - The enum_type.
// symbols   - Array of symbols (no duplicate, properly ordered)
//
Enumeration.EnumSet = class {
  // Creates a new enum_set.
  //
  // enum_type - An EnumType
  // symbols   - Array of symbols.
  //
  // initialize normalizes symbols.
  //   - remove duplicates
  //   - order by universe
  constructor(enum_type, symbols){
    this.enum_type = enum_type;
    this.symbols = enum_type.members.filter(function(sym){
      return include(symbols, sym);
    });
  }

  // Returns a list of symbols.
  symbol_list(){
    return array_to_list(this.symbols); 
  }
  
  // Returns true if the enum_set includes the symbol.
  // 'symbol' is allowed to be a symbol which is not included in the universe.
  is_member(symbol){
    return include(this.symbols, symbol);
  }
  
  // Returns true if:
  // - the enum_set is a subset of the enum_set 'other', and
  // - the universe of the enum_set is a subset of 
  //   the universe of 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  is_subset(other){
    // Check elements
    if(any(this.symbols, function(sym){
         return !include(other.symbols, sym);
       })){
      return false;
    }

    // Check universe
    if(this.enum_type === other.enum_type){
      return true;
    }
    else{
      return all(this.enum_type.members, function(sym){
               return include(other.enum_type.members, sym);
             });
    }
  }

  // Returns true if:
  //   - the enum_set contains the same set of symbols as 'other', and
  //   - universe of the enum_set contains the same set of symbols
  //     as the universe of 'other'.
  //
  // The enum_set and 'other' may belong to different enum_type.
  equal_to(other){
    return this.is_subset(other) && other.is_subset(this);
  }

  // Returns a enum_set which has:
  // - all the symbols included in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  union(other){
    var syms = this.enum_type.members.filter(bind(function(sym){
                 return include(this.symbols, sym) ||
                        include(other.symbols, sym);
               }, this));
    return new Enumeration.EnumSet(this.enum_type, syms);
  }

  // Returns a enum_set which has:
  // - the symbols included both in the enum_set or the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  intersection(other){
    var syms = this.symbols.filter(function(sym){
                 return include(other.symbols, sym);
               });
    return new Enumeration.EnumSet(this.enum_type, syms);
  }

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and not in the enum_set 'other'.
  // The enum_set and 'other' *must* belong to the same enum_type.
  difference(other){
    var syms = this.symbols.filter(function(sym){
                 return !include(other.symbols, sym);
               });
    return new Enumeration.EnumSet(this.enum_type, syms);
  }

  // Returns a enum_set which has:
  // - the symbols included in the universe but not in the enum_set.
  complement(){
    var syms = this.enum_type.members.filter(bind(function(sym){
                 return !include(this.symbols, sym);
               }, this));
    return new Enumeration.EnumSet(this.enum_type, syms);
  }

  // Returns a enum_set which has:
  // - the symbols included in the enum_set and the universe of the enum_set 'other'.
  // The enum_set and 'other' may belong to different enum_type.
  projection(other){
    var syms = this.symbols.filter(function(sym){
                 return include(other.enum_type.members, sym);
               });
    return new Enumeration.EnumSet(other.enum_type, syms);
  }

  // Returns a string which represents the enum_set.
  toString(){
    return "#<EnumSet "+inspect(this.symbols)+">";
  }
}

const isEnumSet = function(obj){
  return (obj instanceof Enumeration.EnumSet);
};

const assert_enum_set = make_simple_assert("enum_set", isEnumSet);

//
// Memoize
//
const memoize = function(klass, names){
  const proto = klass.prototype;
  names.forEach(name => {
    // Copy original function foo as 'compute_foo'
    proto["compute_"+name] = proto[name];
    // Define memoized version
    proto[name] = function(/*arguments*/){
      if(!this.hasOwnProperty("cached_"+name)){
        this["cached_"+name] = this["compute_"+name].apply(this, Array.from(arguments));
      }
      return this["cached_"+name];
    }
  });
};
memoize(Enumeration.EnumSet, ["symbol_list"]);
memoize(Enumeration.EnumType, ["universe", "indexer", "constructor_"]); 

export { Enumeration, isEnumSet, assert_enum_set };
