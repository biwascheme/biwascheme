if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {

  define_libfunc("read-line", 0, 1, function(ar){
    var port = ar[0] || Port.current_input;
    assert_port(port);
    return port.get_string();
  });

  //
  // element
  //
  define_libfunc("element-empty!", 1, 1, function(ar){
    if ($(ar[0]).attr("value")) {
      return $(ar[0]).val("");
    } else {
      return $(ar[0]).empty();
    }
  });
  alias_libfunc("element-empty!", "element-clear!");
  define_libfunc("element-visible?", 1, 1, function(ar){
    return $(ar[0]).is(":visible");
  });
  define_libfunc("element-toggle!", 1, 1, function(ar){
    return $(ar[0]).toggle();
  });
  define_libfunc("element-hide!", 1, 1, function(ar){
    return $(ar[0]).hide();
  });
  define_libfunc("element-show!", 1, 1, function(ar){
    return $(ar[0]).show();
  });
  define_libfunc("element-remove!", 1, 1, function(ar){
    return $(ar[0]).remove();
  });
  define_libfunc("element-update!", 2, 2, function(ar){
    return $(ar[0]).html(ar[1]);
  });
  define_libfunc("element-replace!", 2, 2, function(ar){
    return $(ar[0]).replaceWith(ar[1]);
  });
  define_libfunc("element-insert!", 2, 2, function(ar){
    return $(ar[0]).append(ar[1]);
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
  define_libfunc("element-select", 1, 1, function(ar){
    $(ar[0]).select();
  });
  define_libfunc("element-adjacent", 0, 0, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-identify", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-read-attribute", 2, 2, function(ar){
    assert_string(ar[1]);
    return $(ar[0]).attr(ar[1]);
  });

  var element_write_attribute = function(ar) {
    assert_string(ar[1]);
    return $(ar[0]).attr(ar[1], ar[2]);
  };
  define_libfunc("element-write-attribute", 3, 3, function(ar){
    BiwaScheme.deprecate("element-write-attribute", "1.0",
                         "element-write-attribute!");
    return element_write_attribute(ar);
  });
  define_libfunc("element-write-attribute!", 3, 3, element_write_attribute);

  define_libfunc("element-height", 1, 1, function(ar){
    return $(ar[0]).height();
  });
  define_libfunc("element-width", 1, 1, function(ar){
    return $(ar[0]).width();
  });

  define_libfunc("element-class-names", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-has-class-name?", 2, 2, function(ar){
    assert_string(ar[1]);
    return $(ar[0]).hasClass(ar[1]);
  });

  var element_add_class_name = function(ar) {
    assert_string(ar[1]);
    return $(ar[0]).addClass(ar[1]);
  };
  define_libfunc("element-add-class-name", 2, 2, function(ar){
    BiwaScheme.deprecate("element-add-class-name", "1.0",
                         "element-add-class-name!");
    return element_add_class_name(ar);
  });
  define_libfunc("element-add-class-name!", 2, 2, element_add_class_name);

  var element_remove_class_name = function(ar) {
    assert_string(ar[1]);
    return $(ar[0]).removeClass(ar[1]);
  };
  define_libfunc("element-remove-class-name", 2, 2, function(ar){
    BiwaScheme.deprecate("element-remove-class-name", "1.0",
                         "element-remove-class-name!");
    return element_remove_class_name(ar);
  });
  define_libfunc("element-remove-class-name!", 2, 2, element_remove_class_name);

  var element_toggle_class_name = function(ar) {
    assert_string(ar[1]);
    return $(ar[0]).toggleClass(ar[1]);
  };
  define_libfunc("element-toggle-class-name", 2, 2, function(ar){
    BiwaScheme.deprecate("element-toggle-class-name", "1.0",
                         "element-toggle-class-name!");
    return element_toggle_class_name(ar);
  });
  define_libfunc("element-toggle-class-name!", 2, 2, element_toggle_class_name);

  define_libfunc("element-clean-whitespace!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-empty?", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-descendant-of!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("scroll-to-element!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-style", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-opacity", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-style-set!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-opacity-set!", 2, 2, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-dimensions", 1, 1, function(ar){
    return new Values($(ar[0]).width(), $(ar[0]).height());
  });
  define_libfunc("element-make-positioned!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-undo-positioned!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-make-clipping!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-undo-clipping!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-cumulative-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-positioned-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-absolutize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-relativize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-cumulative-scroll-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-offset-parent", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-viewport-offset", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-clone-position!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-absolutize!", 1, 1, function(ar){
    throw new Bug("not yet implemented");
  });
  define_libfunc("element-focus!", 1, 1, function(ar){
    return $(ar[0]).focus();
  });

  // usage:
  //  (element-new '(div "foo"))        => <div>foo</div>
  //  (element-new '("div#main" "foo")) => <div id='main'>foo</div>
  //  (element-new '("div.red" "foo"))  => <div class='red'>foo</div>
  //  (element-new '(div align "right" "foo"))  => <div align='right'>foo</div>
  //  (element-new '(div (span "foo"))  => <div><span>foo</span></div>
  //

  BiwaScheme.create_elements_by_string = function(spec){
    spec = spec.to_array();
    var name = spec.shift();
    if(name instanceof Symbol) name = name.name;
    var m = name.match(/(.*)\.(.*)/);
    if (m) {
      name = m[1];
      spec.unshift(Sym("class"), m[2]);
    }
    m = name.match(/(.*)\#(.*)/);
    if (m) {
      name = m[1];
      spec.unshift(Sym("id"), m[2]);
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
  };

  BiwaScheme.tree_all = function(tree, pred){
    if(tree === nil)
      return true;
    else if(pred(tree.car) === false)
      return false;
    else
      return BiwaScheme.tree_all(tree.cdr, pred); 
  };
  define_libfunc("element-new", 1, 1, function(ar){
    var string_or_symbol = function(item){
      return _.isString(item) ||
             (item instanceof Symbol) ||
             (item instanceof Pair);
    };
    if(BiwaScheme.tree_all(ar[0], string_or_symbol)){
      return $(create_elements_by_string(ar[0]))[0];
    } else {
      return nil;
    }
  });
  BiwaScheme.element_content = function(selector) {
    if ($(selector).attr("value")) {
      return $(selector).val();
    } else {
      return _.escape($(selector).html());
    }
  };
  define_libfunc("element-content", 1, 1, function(ar){
    return BiwaScheme.element_content(ar[0]);
  });

  //
  // load from network
  //
  define_libfunc("load", 1, 1, function(ar, intp){
    var path = ar[0];
    assert_string(path);
    var intp2 = new Interpreter(intp);
    return new BiwaScheme.Pause(function(pause){
      $.ajax(path, {
        dataType: "text",
        mimeType: "text/plain; charset=x-user-defined", // For Firefox (#88)
        success: function(data) {
          // create new interpreter not to destroy current stack.
          intp2.evaluate(data,
                         function(){
                           return pause.resume(BiwaScheme.undef);
                         });
        },
        error: function() {
          throw new Error("load: network error: failed to load "+path);
        }
      });
    });
  });

  // Load javascript file on the server
  _require = function(src, check, proc){
    var script = $("<script/>", { src: src });
    $("body").append(script);

    var checker = new Function("return !!(" + check + ")");

    if(checker()) proc();
    else          setTimeout(function(){ checker() ? proc() : setTimeout(arguments.callee, 10); }, 10);
  };
  // (js-load "lib/foo.js" "Foo")
  define_libfunc("js-load", 2, 2, function(ar){
    var path = ar[0];
    var check = ar[1];
    assert_string(path);
    assert_string(check);

    return new BiwaScheme.Pause(function(pause){
      _require(path, "window." + check, function(){
        pause.resume(BiwaScheme.undef);
      });
    });
  });

  //
  // html modification
  //

  BiwaScheme.getelem = function(ar){
    // account for getelem returning false when no results (and that getting passed back in)
    if (ar.length > 1 && ar[1] === false) {
      ar[1] = [];
    }

    var x = $.apply(this, ar);
    if (x.length > 0) {
      return x;
    } else {
      return false;
    }
  };
  define_libfunc("$",       1, 2, BiwaScheme.getelem);
  define_libfunc("getelem", 1, 2, BiwaScheme.getelem);
  define_libfunc("dom-element", 1, 1, function(ar) {
    return $(ar[0])[0];
  });

  define_libfunc("set-style!", 3, 3, function(ar){
    assert_string(ar[1]);
    $(ar[0]).css(ar[1], ar[2]);
    return BiwaScheme.undef;
  });
  define_libfunc("get-style", 2, 2, function(ar){
    assert_string(ar[1]);
    return $(ar[0]).css(ar[1]);
  });
  define_libfunc("set-content!", 2, 2, function(ar){
    assert_string(ar[1]);
    var str = ar[1].replace(/\n/g,"<br>").replace(/\t/g,"&nbsp;&nbsp;&nbsp;");
    $(ar[0]).html(str);
    return BiwaScheme.undef;
  });
  define_libfunc("get-content", 1, 1, function(ar){
    return BiwaScheme.element_content(ar[0]);
  });

  //
  // handlers
  //
  define_libfunc("set-handler!", 3, 3, function(ar, intp){
    throw new Error("set-handler! is obsolete, please use add-handler! instead");
  });
  define_libfunc("add-handler!", 3, 3, function(ar, intp){
    var selector = ar[0], evtype = ar[1], proc = ar[2];
    var intp2 = new Interpreter(intp);
    var handler = function(event){
      return _.clone(intp2).invoke_closure(proc, [event]);
    };
    $(selector).on(evtype, handler);
    return handler;
  });
  define_libfunc("remove-handler!", 3, 3, function(ar, intp){
    var selector = ar[0], evtype = ar[1], handler = ar[2];
    $(selector).off(evtype, handler);
    return BiwaScheme.undef;
  });
  define_libfunc("wait-for", 2, 2, function(ar){
    var selector = ar[0], evtype = ar[1];
    var elem = $(selector);
    elem.biwascheme_wait_for = elem.biwascheme_wait_for || {};

    var prev_handler = elem.biwascheme_wait_for[evtype];
    if (prev_handler) {
      // Maybe a wait-for is called more than once,
      // and previous handler is not consumed.
      elem.off(evtype, prev_handler);
    }

    return new BiwaScheme.Pause(function(pause){
      var handler = function(event){
        elem.biwascheme_wait_for[evtype] = undefined;
        elem.off(evtype, handler);
        return pause.resume(event);
      };

      elem.biwascheme_wait_for[evtype] = handler;
      elem.on(evtype, handler);
    });
  });

  //
  // dom
  //
  define_libfunc("domelem", 1, null, function(ar){
    throw new Error("obsolete");
  });
  define_libfunc("dom-remove-children!", 1, 1, function(ar){
    Console.puts("warning: dom-remove-children! is obsolete. use element-empty! instead");
    $(ar[0]).empty();
    return BiwaScheme.undef;
  });
  define_libfunc("dom-create-element", 1, 1, function(ar){
    throw new Error("obsolete");
  });
  define_libfunc("element-append-child!", 2, 2, function(ar){
    return $(ar[0]).append(ar[1]);
  });
  define_libfunc("dom-remove-child!", 2, 2, function(ar){
    throw new Error("obsolete");
  });
//  define_libfunc("dom-get-attribute", 2, 2, function(ar){
//  });
//  define_libfunc("dom-remove-child!", 2, 2, function(ar){
//  });

  //
  // communication to server
  //
  define_libfunc("http-request", 1, 1, function(ar){
    var path = ar[0];
    assert_string(path);

    return new BiwaScheme.Pause(function(pause){
      $.get(path, function(data) {
        pause.resume(data);
      }, "text");
    });
  });
  // (http-post "/foo" '(("x" . 1) ("y" . 2)))
  define_libfunc("http-post", 2, 2, function(ar){
    var path = ar[0];
    assert_string(path);
    var alist = ar[1];
    assert_list(alist);
    var h = alist_to_js_obj(alist);

    return new BiwaScheme.Pause(function(pause){
      $.post(path, h, function(data) {
        pause.resume(data);
      }, "text");
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
    url += "?callback=BiwaScheme.jsonp_receiver[" + receiver_id + "]";

    return new BiwaScheme.Pause(function(pause){
      receives[receiver_id] = function(data){
        pause.resume(data);
        receives[receiver_id] = null;
      };
      var script = $("<script/>", { src: url });
      $("body").append(script);
    });
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

}
