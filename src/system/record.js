import * as _ from "../deps/underscore-esm.js"
import { to_write } from "./_writer.js"
import { make_simple_assert } from "./assert.js"
import Call from "./call.js"
import { Sym } from "./symbol.js"
import { assert_procedure } from "../library/infra.js"

//
// R6RS Records
// http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-7.html#node_chap_6
//
// Record is like struct in C, but supports more feature like inheritance.
// see also: src/library/r6rs_lib.js

//
// Record 
// represents each instance of record type
//
class Record {
  constructor(rtd, values){
    assert_record_td(rtd, "new Record");

    this.rtd = rtd;
    this.fields = values;
  }

  get(k){
    return this.fields[k]
  }

  set(k, v){
    this.fields[k] = v;
  }

  toString(){
    var contents = to_write(this.fields);
    return "#<Record "+this.rtd.name+" "+contents+">";
  }
}

const isRecord = function(o){
  return (o instanceof Record);
};

// Defined record types
Record._DefinedTypes = {};

Record.define_type = function(name_str, rtd, cd){
  return Record._DefinedTypes[name_str] = {rtd: rtd, cd: cd};
};

Record.get_type = function(name_str){
  return Record._DefinedTypes[name_str];
};

//
// RTD (Record type descriptor)
//
Record.RTD = class {
  //          Symbol RTD        Symbol Bool  Bool    Array
  constructor(name, parent_rtd, uid, sealed, opaque, fields){
    this.name = name;
    this.parent_rtd = parent_rtd;
    this.is_base_type = !parent_rtd;

    if(uid){
      this.uid = uid;
      this.generative = false;
    }
    else{
      this.uid = this._generate_new_uid();;
      this.generative = true;
    }

    this.sealed = !!sealed;
    this.opaque = parent_rtd.opaque || (!!opaque);

    this.fields = fields.map(function(field){
      return {name: field[0], mutable: !!field[1]};
    });
  }

  // Returns the name of the k-th field.
  // Only used for error messages.
  field_name(k){
    var names = this._field_names();

    for(var par = this.parent_rtd; par; par = par.parent_rtd){
      names = par._field_names() + names;
    }

    return names[k];
  }

  _field_names(){
    return this.fields.map(function(spec){
        return spec.name;
      });
  }

  _generate_new_uid(){
    return Sym(_.uniqueId("__record_td_uid"));
  }

  toString(){
    return "#<RecordTD "+name+">";
  }
};

Record.RTD.NongenerativeRecords = {};
const isRecordTD = function(o){
  return (o instanceof Record.RTD);
};

//
// CD (Record constructor descriptor)
//
Record.CD = class {
  constructor(rtd, parent_cd, protocol){
    this._check(rtd, parent_cd, protocol);
    this.rtd = rtd;
    this.parent_cd = parent_cd;
    if(protocol){
      this.has_custom_protocol = true;
      this.protocol = protocol;
    }
    else{
      this.has_custom_protocol = false;
      if(rtd.parent_rtd)
        this.protocol = this._default_protocol_for_derived_types();
      else
        this.protocol = this._default_protocol_for_base_types();
    }
  }

  _check(rtd, parent_cd, protocol){
    if(rtd.is_base_type && parent_cd)
      throw new Error("Record.CD.new: cannot specify parent cd of a base type");

    if(parent_cd && rtd.parent_rtd && (parent_cd.rtd != rtd.parent_rtd))
      throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd");

    if(rtd.parent_rtd && !parent_cd && protocol)
      throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given");

    if(parent_cd && parent_cd.has_custom_protocol && !protocol)
      throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol");
  }
  
  _default_protocol_for_base_types(){
    // (lambda (p) p)
    // called with `p' as an argument
    return function(ar){
      var p = ar[0];
      assert_procedure(p, "_default_protocol/base");
      return p;
    };
  }

  _default_protocol_for_derived_types(){
    // (lambda (n) 
    //   (lambda (a b x y s t)
    //     (let1 p (n a b x y) (p s t))))
    // called with `n' as an argument
    var rtd = this.rtd;
    return function(ar){
      var n = ar[0];
      assert_procedure(n, "_default_protocol/n");

      var ctor = function(args){
        var my_argc = rtd.fields.length;
        var ancestor_argc = args.length - my_argc;

        var ancestor_values = args.slice(0, ancestor_argc);
        var my_values       = args.slice(ancestor_argc);

        // (n a b x y) => p
        return new Call(n, ancestor_values, function(ar){
          var p = ar[0];
          assert_procedure(p, "_default_protocol/p");

          // (p s t) => record
          return new Call(p, my_values, function(ar){
            var record = ar[0];
            assert_record(record, "_default_protocol/result");

            return record;
          });
        });
      };
      return ctor;
    };
  }

  toString(){
    return "#<RecordCD "+this.rtd.name+">";
  }

  record_constructor(){
    var arg_for_protocol = (this.parent_cd ? this._make_n([], this.rtd)
                                           : this._make_p());
    arg_for_protocol = _.bind(arg_for_protocol, this);

    return new Call(this.protocol, [arg_for_protocol], function(ar){
      var ctor = ar[0];
      assert_procedure(ctor, "record_constructor");
      return ctor;
    });
  }

  // Create the function `p' which is given to the protocol.
  _make_p(){
    return function(values){
      return new Record(this.rtd, values);
      // TODO: check argc 
    };
  }

  // Create the function `n' which is given to the protocol.
  // When creating an instance of a derived type,
  // _make_n is called for each ancestor rtd's.
  _make_n(children_values, rtd){
    var parent_cd = this.parent_cd;

    if(parent_cd){
      // called from protocol (n)
      var n = function(args_for_n){

        // called from protocol (p)
        var p = function(args_for_p){
          var values = [].concat(args_for_p[0]).concat(children_values)
          var parent_n = parent_cd._make_n(values, rtd);

          return new Call(parent_cd.protocol, [parent_n], function(ar){
            var ctor = ar[0];
            assert_procedure(ctor, "_make_n");

            return new Call(ctor, args_for_n, function(ar){
              var record = ar[0];
              assert_record(record);
              return record;
            });
          });
        };
        return p;
      };
      return n;
    }
    else{
      var n = function(my_values){
        var values = my_values.concat(children_values);
        return new Record(rtd, values);
        // TODO: check argc 
      };
      return n;
    }
  }
};

const isRecordCD = function(o){
  return (o instanceof Record.CD);
};

const assert_record = make_simple_assert("record", isRecord);
const assert_record_td = make_simple_assert("record type descriptor", isRecordTD);
const assert_record_cd = make_simple_assert("record constructor descriptor", isRecordCD);

export { Record, isRecord, isRecordTD, isRecordCD,
         assert_record, assert_record_td, assert_record_cd };
