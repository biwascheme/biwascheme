if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {

  //
  // tuple space
  //
  BiwaScheme.TupleSpaceClient = BiwaScheme.Class.create({
    initialize: function(server_path){
      this.server_path = server_path; //must end with '/'
    },

    nonblocking_request: function(name, data){
      var path = this.server_path + name + "?" 
                   + encodeURIComponent(to_write(data));
      return this.connect(path);
    },
    blocking_request: function(name, data){
      this.assert_init();
      var path = this.server_path + name + "?"
                   + encodeURIComponent(to_write(data))
                   + "&cid=" + this.client_id;
      return new BiwaScheme.Pause(_.bind(function(pause){
        this.ajax(path, _.bind(function(ticket){
          this.observe(ticket, function(tuple){
            pause.resume(tuple);
          });
        }, this));
      }, this));
    },

    write: function(value){
      return this.nonblocking_request("write", value);
    },
    readp: function(query){
      return this.nonblocking_request("readp", query);
    },
    takep: function(query){
      return this.nonblocking_request("takep", query);
    },
    dump: function(){
      return this.nonblocking_request("dump", "");
    },
    
    read: function(query){
      return this.blocking_request("read", query);
    },
    take: function(query){
      return this.blocking_request("take", query);
    },

    ajax: function(path, callback){
      $.ajax(path, {
        success: function(data) {
          callback(data);
        },
        error: function() {
          puts("error: failed to access " + path);
        }
      });
    },

    init_connection: function(){
      if(this.client_id){
        return this.client_id;
      }
      else{
        return new Pause(_.bind(function(pause){
          var path = this.server_path + "init_connection";
          this.ajax(path, _.bind(function(str){
            this.client_id = str;
            this.start_connection();
            pause.resume(this.client_id);
          }, this));
        }, this));
      }
    },
    assert_init: function(){
      if(!this.client_id){
        puts("ts-init not called:"+BiwaScheme.inspect(this.client_id));
        throw new Error("ts-init not called");
      }
    },


    // returns nothing
    start_connection: function(){
      var path = this.server_path + "connection?"
                   + "cid=" + this.client_id;
      var loop = _.bind(function(){
        this.ajax(path, _.bind(function(str){
          var result = Interpreter.read(str);
          if (result){
            var ticket = result.car, tuple  = result.cdr;

            this.notify(ticket, tuple);
            loop();
          }
        }, this));
      }, this);
      loop();
    },

    waiters: [],
    too_early_tuples: [],
    observe: function(ticket, callback){
      if(this.too_early_tuples[ticket]){
        //alert("1 too early resolved");
        callback(this.too_early_tuples[ticket]);
        this.too_early_tuples[ticket] = undefined;
      }
      else if(this.waiters[ticket]){
        puts("Bug: ticket conflicted");
      }
      else{
        this.waiters[ticket] = callback;
      }
    },
    notify: function(ticket, tuple){
      var callback = this.waiters[ticket];
      if(callback){
        this.waiters[ticket] = undefined; //or use 'delete'?
        return callback(tuple);
      }
      else{
        this.too_early_tuples[ticket] = tuple;
      }
    },

    connect: function(path, on_result){
      path += "&time=" + (new Date()).getTime();
      return new BiwaScheme.Pause(function(pause){
        $.ajax(path, {
          success: function(transport){
            var result = transport.responseText;
            if(on_result)
              result = on_result(result);
            else
              result = Interpreter.read(result);

            if(result == undefined)
              pause.resume(false)
            else
              pause.resume(result)
          },
          error: function(transport){
            throw new Error("ts_client.connect: failed to access"+path);
            pause.resume(false)
          }
        });
      });
    }
  });
  BiwaScheme.ts_client = new TupleSpaceClient("/ts/");

  define_libfunc("ts-init", 0, 0, function(ar){
    return ts_client.init_connection();
  });
  define_libfunc("ts-write", 1, 1, function(ar){
    return ts_client.write(ar[0]);
  });

  define_libfunc("ts-read", 1, 1, function(ar){
    var query = ar[0];
    return ts_client.read(query); //returns a Pause
  });
  define_libfunc("ts-readp", 1, 1, function(ar){
    return ts_client.readp(ar[0]);
  });
  define_libfunc("ts-take", 1, 1, function(ar){
    return ts_client.take(ar[0]);
  });
  define_libfunc("ts-takep", 1, 1, function(ar){
    return ts_client.takep(ar[0]);
  });
  define_libfunc("ts-dump", 0, 0, function(ar){
    return ts_client.dump();
  });

}
