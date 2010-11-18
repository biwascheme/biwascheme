//
// Record
//
BiwaScheme.Record = Class.create({
  initialize: function(rtd, values){
    assert_record_td(rtd, "new Record");

    this.rtd = rtd;
    this.fields = values;
  },
  get: function(k){
    return this.fields[k]
  },
  set: function(k, v){
    this.fields[k] = v;
  },
  toString: function(){
    return "#<Record "+this.rtd.name+">";
  }
});
BiwaScheme.isRecord = function(o){
  return (o instanceof BiwaScheme.Record);
};

// Record types
BiwaScheme.Record._DefinedTypes = new Hash();

BiwaScheme.Record.define_type = function(name_str, rtd, cd){
  return BiwaScheme.Record._DefinedTypes.set(name_str, {rtd: rtd, cd: cd});
};
BiwaScheme.Record.get_type = function(name_str){
  return BiwaScheme.Record._DefinedTypes.get(name_str);
};

// Record type descriptor
BiwaScheme.Record.RTD = Class.create({
  //                   Symbol RTD        Symbol Bool  Bool    Array
  initialize: function(name, parent_rtd, uid, sealed, opaque, fields){
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
    this.opaque = !!opaque;

    this.field_names =        fields.map(function(field){ return field[0]; });
    this.field_mutabilities = fields.map(function(field){ return field[1]; });
  },
  _generate_new_uid: function(){
    var n = (BiwaScheme.Record.RTD.last_uid++);
    return BiwaScheme.Sym("__record_td_uid_"+n);
  },
  toString: function(){
    return "#<RecordTD "+name+">";
  }
});
BiwaScheme.Record.RTD.last_uid = 0;
BiwaScheme.isRecordTD = function(o){
  return (o instanceof BiwaScheme.Record.RTD);
};

// Record constructor descriptor
BiwaScheme.Record.CD = Class.create({
  initialize: function(rtd, parent_cd, protocol){
    this._check(rtd, parent_cd, protocol);
    this.rtd = rtd;
    this.parent_cd = parent_cd;
    if(protocol){
      this.protocol = protocol;
      this.has_custom_protocol = true;
    }
    else{
      this.protocol = this._default_protocol();
      this.has_custom_protocol = false;
    }
  },

  _check: function(rtd, parent_cd, protocol){
    if(rtd.is_base_type && parent_cd)
      throw new Error("Record.CD.new: cannot specify parent cd of a base type");

    if(parent_cd && rtd.parent_rtd && (parent_cd.rtd != rtd.parent_rtd))
      throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd");

    if(rtd.parent_rtd && !parent_cd && protocol)
      throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given");

    if(parent_cd && parent_cd.has_custom_protocol && !protocol)
      throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol");
  },
  
  _default_protocol: function(){
    if(this.parent_cd){
      // Default protocol for derived types:
      // (lambda (n) 
      //   (lambda (a b x y s t)
      //     ((n a b x y) s t))) 

      // called with `n' as an argument
      return function(ar){
        var n = ar[0];
        assert_procedure(n, "_default_protocol/n");

        // this is the ctor; will be named as make-*
        // all the values are given as `ar'
        return function(ar){
          var my_argc = this.rtd.field_names.length;
          var ancestor_argc = ar.length - my_argc;

          var ancestor_values = ar.slice(ancestor_argc);
          var my_values       = ar.slice(0, ancestor_argc);

          // call n with (a b x y)
          return new BiwaScheme.Call(n, ancestor_values, function(ar){
            var p = ar[0];
            assert_procedure(p, "_default_protocol/p");

            // call p with (s t)
            return new BiwaScheme.Call(p, my_values, function(ar){
              assert_record(ar[0]);

              // return the new born record
              return ar[0];
            });
          });
        };
      };
    }
    else{
      // Default protocol for base types:
      // (lambda (p) p)
      
      // called with `p' as an argument
      return function(ar){
        assert_procedure(ar[0], "_default_protocol/base");
        return ar[0];
      };
    }
  },

  toString: function(){
    return "#<RecordCD "+this.rtd.name+">";
  },

  record_constructor: function(){
    var arg_for_protocol = (this.parent_cd ? this._make_n()
                                           : this._make_p()).bind(this);

    return new BiwaScheme.Call(this.protocol, [arg_for_protocol], function(ar){
      assert_procedure(ar[0], "record_constructor");
      return ar[0];
    });
  },
  // Create the function `p' which is given to the protocol.
  _make_p: function(){
    return function(values){
      return new BiwaScheme.Record(this.rtd, values);
      // TODO: check argc 
    };
  },
  // Create the function `n' which is given to the protocol.
  // Used only when the rtd is a derived type
  _make_n: function(children_values){
    var parent_cd = this.parent_cd;

    if(parent_cd){
      // called from protocol (n)
      return function(args_for_n){
        // called from protocol (p)
        return function(args_for_p){
          var n = parent_cd._make_n([args_for_p[0] + children_values]);
          return new BiwaScheme.Call(parent_cd.protocol, [n], function(ar){
            assert_procedure(ar[0], "_make_n");
            return new BiwaScheme.Call(ar[0], args_for_n, function(ar){
              assert_record(ar[0]);
              return ar[0];
            });
          });
        };
      };
    }
    else{
      return function(my_values){
        var values = my_values + children_values;
        return new BiwaScheme.Record(this.rtd, values);
        // TODO: check argc 
      };
    }
  }
});

BiwaScheme.isRecordCD = function(o){
  return (o instanceof BiwaScheme.Record.CD);
};
