
if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  define_libfunc("read-line", 0, 1, function(ar){
    var port = ar[0] || Port.current_input;
    assert_port(port);
    return port.get_string();
  });
  
  //
  // element
  //
  define_libfunc("element-clear!", 1, 1, function(ar){
    return $(ar[0]).update();
  });
  define_libfunc("element-empty!", 1, 1, function(ar){
    return $(ar[0]).update();
  });

  define_libfunc("element-visible?", 1, 1, function(ar){
    return $(ar[0]).visible();
  });
  define_libfunc("element-toggle!", 1, 1, function(ar){
    return $(ar[0]).toggle();
  })
  define_libfunc("element-hide!", 1, 1, function(ar){
    return $(ar[0]).hide();
  })
  define_libfunc("element-show!", 1, 1, function(ar){
    return $(ar[0]).show();
  })
  define_libfunc("element-remove!", 1, 1, function(ar){
    return $(ar[0]).remove("");
  })
  define_libfunc("element-update!", 2, 2, function(ar){
    return $(ar[0]).update(ar[1]);
  })
  define_libfunc("element-replace!", 2, 2, function(ar){
    return $(ar[0]).replace(ar[1]);
  });
  define_libfunc("element-insert!", 2, 2, function(ar){
    return $(ar[0]).insert(ar[1]);
  });
  define_libfunc("element-wrap!", 3, 3, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-ancestors", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-descendants", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-first-descendant", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-immediate-descendants", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-previous-sibling", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-next-sibling", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-siblings", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-match?", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-up", 3, 3, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-down", 3, 3, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-previous", 3, 3, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-next", 3, 3, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-select", 0, 0, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-adjacent", 0, 0, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-identify", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-read-attribute", 2, 2, function(ar){
    assert_string(ar[1]);
    return $(ar[0]).readAttribute(ar[1]);
  });
  define_libfunc("element-write-attribute", 3, 3, function(ar){
    assert_string(ar[1]);
    return $(ar[0]).readAttribute(ar[1], ar[2]);
  });
  define_libfunc("element-height", 1, 1, function(ar){
    return $(ar[0]).getHeight();
  });
  define_libfunc("element-width", 1, 1, function(ar){
    return $(ar[0]).getWidth();
  });
  define_libfunc("element-class-names", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-has-class-name?", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-add-class-name", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-remove-class-name", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-toggle-class-name", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-clean-whitespace!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-empty?", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-descendant-of!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("scroll-to-element!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-style", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-opacity", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-style-set!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-opacity-set!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-dimensions", 1, 1, function(ar){
    var dimensions = $(ar[0]).getDimensions();
    return new Values(dimensions.width, dimensions.height);
  })
  define_libfunc("element-make-positioned!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-undo-positioned!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-make-clipping!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-undo-clipping!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-cumulative-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-positioned-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-absolutize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-relativize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-cumulative-scroll-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-offset-parent", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-viewport-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-clone-position!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })
  define_libfunc("element-absolutize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  })

  // usage:
  //  (element-new '(div "foo"))        => <div>foo</div>
  //  (element-new '("div#main" "foo")) => <div id='main'>foo</div>
  //  (element-new '("div.red" "foo"))  => <div class='red'>foo</div>
  //  (element-new '(div align right))  => <div align='right'>foo</div>
  //  (element-new '(div (span "foo"))  => <div><span>foo</span></div>
  //

  // This function is not used; slower than by_string
  BiwaScheme.create_elements_by_dom = function(spec){
    var create_element = function(name, attrs, children){
      var tag = new Element(name, attrs); //attrs is an Object
      children.each(function(child){
        tag.insert({bottom: child});
      });
      return tag;
    };
    var spec = spec.to_array();
    var name = spec[0].name || spec[0]; // Symbol or String
    var attrs = {};
    var children = [];
    for(var i=1; i<spec.length; i++){
      if(spec[i] instanceof Symbol){ //attribute
        attrs[ spec[i].name ] = spec[i+1];
        i++;
      }
      else{
        if(spec[i] instanceof Pair)
          children.push(create_elements_by_dom(spec[i]));
        else
          children.push(spec[i].toString());
      }
    }
    return create_element(name, attrs, children);
  };

  BiwaScheme.create_elements_by_string = function(spec){
    var spec = spec.to_array();
    var name = spec.shift();
    if(name instanceof Symbol) name = name.name;
    if(name.match(/(.*)\.(.*)/)){
      name = RegExp.$1;
      spec.unshift(Sym("class"), RegExp.$2);
    }
    if(name.match(/(.*)\#(.*)/)){
      name = RegExp.$1;
      spec.unshift(Sym("id"), RegExp.$2);
    }
    var children = [];
    var s = ["<" + name];
    for(var i=0; i<spec.length; i++){
      if(spec[i] instanceof Symbol){
        s.push(' ' + spec[i].name + '="' + spec[i+1] + '"');
        i++;
      }
      else{
        if(spec[i] instanceof Pair)
          children.push(create_elements_by_string(spec[i]));
        else
          children.push(spec[i]); // String
      }
    }
    s.push(">");
    s.push( children.join("") );
    s.push("</" + name + ">");
    return s.join("");
  }
  BiwaScheme.tree_all = function(tree, pred){
    if(tree === nil)
      return true;
    else if(pred(tree.car) === false)
      return false;
    else
      return BiwaScheme.tree_all(tree.cdr, pred); 
  }
  define_libfunc("element-new", 1, 1, function(ar){
    var string_or_symbol = function(item){
      return Object.isString(item) || 
             (item instanceof Symbol) || 
             (item instanceof Pair);
    };
    if(BiwaScheme.tree_all(ar[0], string_or_symbol)){
      var div = new Element("div");
      div.update( create_elements_by_string(ar[0]) );
      return div.firstChild;
    }
    else
      return nil //create_elements_by_dom(ar[0]);
  });
  define_libfunc("element-content", 1, 1, function(ar){
    return ar[0].value || (ar[0].innerHTML).unescapeHTML();
  });

  //
  // load from network
  //
  define_libfunc("load", 1, 1, function(ar, intp){
    var path = ar[0];
    assert_string(path);
    return new BiwaScheme.Pause(function(pause){
      new Ajax.Request(path, {
        method: 'get',
        evalJSON: false,
        evalJS: false,
        onSuccess: function(transport) {
          // create new interpreter not to destroy current stack.
          var local_intp = new Interpreter(intp.on_error);
          local_intp.evaluate(transport.responseText,
                              function(){
                                return pause.resume(BiwaScheme.undef);
                              });
        },
        onFailure: function(transport){
          throw new Error("load: network error: failed to load"+path);
        }
      });
    });
  })

  // (js-load "lib/foo.js" "Foo")
  define_libfunc("js-load", 2, 2, function(ar){
    var path = ar[0];
    var check = ar[1];
    assert_string(path);
    assert_string(check);

    return new BiwaScheme.Pause(function(pause){
      BiwaScheme.require(path, "window." + check, function(){
        pause.resume(BiwaScheme.undef);
      });
    });
  });

  //
  // html modification
  //

  BiwaScheme.getelem = function(ar){
    var x = $(ar[0]); 
    if (x===undefined || x===null)
      return false;
    else
      return x;
  };
  define_libfunc("$",       1, 1, BiwaScheme.getelem);
  define_libfunc("getelem", 1, 1, BiwaScheme.getelem); 

  define_libfunc("set-style!", 3, 3, function(ar){
    assert_string(ar[1]);
    ar[0].style[ar[1]] = ar[2];
    return BiwaScheme.undef;
  });
  define_libfunc("get-style", 2, 2, function(ar){
    assert_string(ar[1]);
    return ar[0].style[ar[1]];
  });
  define_libfunc("set-content!", 2, 2, function(ar){
    assert_string(ar[1]);
    var str = ar[1].replace(/\n/g,"<br>").replace(/\t/g,"&nbsp;&nbsp;&nbsp;");
    ar[0].innerHTML = str;
    return BiwaScheme.undef;
  });
  define_libfunc("get-content", 1, 1, function(ar){
    return ar[0].value || (ar[0].innerHTML).unescapeHTML();
  });

  //
  // timer, sleep, handlers
  //
  define_libfunc("timer", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    setTimeout(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc) }, sec * 1000);
    return BiwaScheme.undef;
  })
  define_libfunc("set-timer!", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    return setInterval(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc) }, sec * 1000);
  })
  define_libfunc("clear-timer!", 1, 1, function(ar){
    var timer_id = ar[0];
    clearInterval(timer_id);
    return BiwaScheme.undef;
  })

  define_libfunc("sleep", 1, 1, function(ar){
    var sec = ar[0];
    assert_real(sec);
    return new BiwaScheme.Pause(function(pause){
      setTimeout(function(){ pause.resume(nil) }, sec * 1000);
    });
  });

  define_libfunc("set-handler!", 3, 3, function(ar, intp){
    throw new Error("set-handler! is obsolete, please use add-handler! instead");
  })
  define_libfunc("add-handler!", 3, 3, function(ar, intp){
    var elem = ar[0], evtype = ar[1], proc = ar[2];
    var on_error = intp.on_error;
    Event.observe(elem, evtype, function(event){
      var intp = new Interpreter(on_error);
      intp.invoke_closure(proc, [event||Window.event]);
    });
    return BiwaScheme.undef;
  })
  define_libfunc("wait-for", 2, 2, function(ar){
    var elem = ar[0], evtype = ar[1];
    elem.biwascheme_wait_for = elem.biwascheme_wait_for || {};

    var prev_handler = elem.biwascheme_wait_for[evtype];
    if (prev_handler) {
      // Maybe a wait-for is called more than once,
      // and previous handler is not consumed.
      Event.stopObserving(elem, evtype, prev_handler);
    }

    return new BiwaScheme.Pause(function(pause){
      var handler = function(event){ 
        elem.biwascheme_wait_for[evtype] = undefined;
        Event.stopObserving(elem, evtype, handler);
        return pause.resume(BiwaScheme.undef); 
      };

      elem.biwascheme_wait_for[evtype] = handler;
      Event.observe(elem, evtype, handler);
    });
  });

  //
  // dom
  //
  define_libfunc("domelem", 1, null, function(ar){
    throw new Error("obsolete");
  });
  define_libfunc("dom-remove-children!", 1, 1, function(ar){
    puts("warning: dom-remove-children! is obsolete. use element-empty! instead");
    $(ar[0]).update("");
    return BiwaScheme.undef;
  });
  define_libfunc("dom-create-element", 1, 1, function(ar){
    throw new Error("obsolete");
  });
  define_libfunc("element-append-child!", 2, 2, function(ar){
    return $(ar[0]).appendChild(ar[1]);
  });
  define_libfunc("dom-remove-child!", 2, 2, function(ar){
    throw new Error("obsolete");
  });
//  define_libfunc("dom-get-attribute", 2, 2, function(ar){
//  });
//  define_libfunc("dom-remove-child!", 2, 2, function(ar){
//  });
  
  //
  // interface to javascript
  //
  define_libfunc_raw("js-eval", 1, 1, function(ar){
    return eval(ar[0]);
  });
  define_libfunc_raw("js-ref", 2, 2, function(ar){
    assert_string(ar[1]);
    return ar[0][ar[1]]; //todo: first-class js function
  });
  define_libfunc("js-set!", 3, 3, function(ar){
    assert_string(ar[1]);
    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  });

  // (js-call (js-eval "Math.pow") 2 4)
  define_libfunc_raw("js-call", 1, null, function(ar){
    var js_func = ar.shift();
    assert_function(js_func);

    var receiver = null;
    return js_func.apply(receiver, ar);
  });
  // (js-invoke (js-new "Date") "getTime")
  define_libfunc_raw("js-invoke", 2, null, function(ar){
    var js_obj = ar.shift();
    var func_name = ar.shift();
    assert_string(func_name);
    if(js_obj[func_name])
      return js_obj[func_name].apply(js_obj, ar);
    else
      throw new Error("js-invoke: function "+func_name+" is not defined");
  });

  // (js-new "Date" 2005 1 1)
  // (js-new "Draggable elem 'onEnd (lambda (drg) ...))
  //   If symbol is given, following arguments are converted to 
  //   an js object. If any of them is a scheme closure,
  //   it is converted to js function which invokes that closure.
  //
  define_libfunc("js-new", 1, null, function(ar, intp){
    // make js object from key-value pair
    var array_to_obj = function(ary){
      if((ary.length % 2) != 0)
        throw new Error("js-new: odd number of key-value pair");

      var obj = {};
      for(var i=0; i<ary.length; i+=2){
        var key = ary[i], value = ary[i+1];
        assert_symbol(key);
        if(value.closure_p === true)
          value = BiwaScheme.js_closure(value, intp);

        obj[key.name] = value;
      }
      return obj;
    };

    var ctor = ar.shift();
    assert_string(ctor);
    if(ar.length == 0){
      return eval("new " + ctor + "()");
    }
    else{
      // pack args to js object, if symbol appears
      var args = [];
      for(var i=0; i<ar.length; i++){
        if(ar[i] instanceof Symbol){
          args.push(array_to_obj(ar.slice(i)));
          break;
        }
        else{
          args.push(ar[i]);
        }
      }
      // construct js code to create new object
      var args_str = ar.map(function(value, i){
        return "args['" + i + "']";
      }).join(",");
      return eval("new " + ctor + "(" + args_str + ")");
    }
  });

  // (js-obj "foo" 1 "bar" 2)
  // -> {"foo": 1, "bar": 2}
  define_libfunc("js-obj", 0, null, function(ar){
    if(ar.length % 2 != 0){
      throw new Error("js-obj: number of arguments must be even");
    }

    var obj = {};
    for(i=0; i<ar.length/2; i++){
      assert_string(ar[i*2]);
      obj[ar[i*2]] = ar[i*2+1];
    }
    return obj;
  });

  // (js-closure (lambda (event) ..))
  BiwaScheme.js_closure = function(proc, intp){
    var on_error = intp.on_error;
    return function(/*args*/){
      var intp = new Interpreter(on_error);
      intp.invoke_closure(proc, $A(arguments));
    };
  };
  define_libfunc("js-closure", 1, 1, function(ar, intp){
    assert_closure(ar[0]);
    return BiwaScheme.js_closure(ar[0], intp);
  });

  define_libfunc("js-null?", 1, 1, function(ar){
    return ar[0] === null;
  });

  define_libfunc("js-undefined?", 1, 1, function(ar){
    return ar[0] === undefined;
  });

  //
  // communication to server
  //
  define_libfunc("http-request", 1, 1, function(ar){
    var path = ar[0];
    assert_string(path);

    return new BiwaScheme.Pause(function(pause){
      new Ajax.Request(path, {
        method: 'get',
        onSuccess: function(transport) {
          pause.resume(transport.responseText)
        }
      });
    });
  });
  // (http-post "/foo" '(("x" . 1) ("y" . 2)))
  define_libfunc("http-post", 2, 2, function(ar){
    var path = ar[0];
    assert_string(path);
    var alist = ar[1];
    assert_list(alist);
    var h = {};

    alist.foreach(function(item){
      assert_string(item.car);
      h[item.car] = item.cdr;
    })

    return new BiwaScheme.Pause(function(pause){
      new Ajax.Request(path, {
        method: 'post',
        postBody: $H(h).toQueryString(),
        onSuccess: function(transport) {
          pause.resume(transport.responseText)
        }
      });
    });
  });

  BiwaScheme.jsonp_receiver = [];
  define_libfunc("receive-jsonp", 1, 1, function(ar){
    var url = ar[0];
    assert_string(url);

    var receives = BiwaScheme.jsonp_receiver;
    for(var i=0; i<receives.length; i++)
      if(receives[i] === null) break;
    var receiver_id = i;
    url += "?callback=BiwaScheme.jsonp_receiver[" + receiver_id + "]"

    return new BiwaScheme.Pause(function(pause){
      receives[receiver_id] = function(data){
        pause.resume(data);
        receives[receiver_id] = null;
      }
      var script = document.createElement('script')
      script.src = url;
      document.body.appendChild(script);
    })
  });

  //
  // dialog, debug
  //
  define_libfunc("alert", 1, 1, function(ar){
    alert(ar[0]);
    return BiwaScheme.undef;
  });
  define_libfunc("confirm", 1, 1, function(ar){
    return confirm(ar[0]);
  });
//  define_libfunc("dumpobj", 1, 1, function(ar){
//    return eval(ar[0]);
//  });

  //
  // tuple space
  //
  BiwaScheme.TupleSpaceClient = Class.create({
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
      return new BiwaScheme.Pause(function(pause){
        this.ajax(path, function(ticket){
          this.observe(ticket, function(tuple){
            pause.resume(tuple);
          });
        }.bind(this));
      }.bind(this));
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
      new Ajax.Request(path, {
        onSuccess: function(transport){
          callback(transport.responseText);
        },
        onFailure: function(){
          puts("error: failed to access " + path);
        }
      });
    },

    init_connection: function(){
      if(this.client_id){
        return this.client_id;
      }
      else{
        return new Pause(function(pause){
          var path = this.server_path + "init_connection";
          this.ajax(path, function(str){
            this.client_id = str;
            this.start_connection();
            pause.resume(this.client_id);
          }.bind(this));
        }.bind(this));
      }
    },
    assert_init: function(){
      if(!this.client_id){
        puts("ts-init not called:"+Object.inspect(this.client_id));
        throw new Error("ts-init not called");
      }
    },


    // returns nothing
    start_connection: function(){
      var path = this.server_path + "connection?"
                   + "cid=" + this.client_id;
      var loop = function(){
        this.ajax(path, function(str){
          var result = Interpreter.read(str);
          if (result){
            var ticket = result.car, tuple  = result.cdr;

            this.notify(ticket, tuple);
            loop();
          }
        }.bind(this));
      }.bind(this);
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
        new Ajax.Request(path, {
          method: 'get',
          onSuccess: function(transport){
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
          onFailure: function(transport){
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

