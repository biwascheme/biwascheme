function unbalanced_parentheses(text_code) {
    var tokens = (new BiwaScheme.Parser(text_code)).tokens;
    var parentheses = 0;
    var brakets = 0;
    for(var i=0; i<tokens.length; ++i) {
        switch(tokens[i]) {
            case "[": ++brakets; break;
            case "]": --brakets; break;
            case "(": ++parentheses; break;
            case ")": --parentheses; break;
        }
    }
    return parentheses != 0 || brakets != 0;
}


//--------------------------------------------------------------------------
jQuery(document).ready(function($, undefined) {
    //NOTE: $ is jQuery in this scope
    var trace = false;
    var bscheme = new BiwaScheme.Interpreter(function(e, state) {
       term.error(e.message);
    });

    Console.puts = function(string) {
        term.echo(string);
    };
    BiwaScheme.Port.current_output = new BiwaScheme.Port.CustomOutput(
        Console.puts
    );
    var code_to_evaluate = '';
    var term = $('#term').terminal(function(command, term) {
        code_to_evaluate += ' ' + command;
        if (!unbalanced_parentheses(code_to_evaluate)) {
            try {
                if (trace) {
                    var opc = biwascheme.compile(code_to_evaluate);
                    var dump_opc = (new BiwaScheme.Dumper()).dump_opc(opc);
                    term.echo(dump_opc, {raw: true});
                }
                bscheme.evaluate(code_to_evaluate, function(result) {
                    if (result !== undefined && result !== BiwaScheme.undef) {
                        term.echo('=> ' + BiwaScheme.to_write(result));
                    }
                });
            } catch(e) {
                term.error(e.message);
                code_to_evaluate = '';
                throw(e);
            }
            term.set_prompt('biwascheme>');
            code_to_evaluate = '';
        } else {
            term.set_prompt('...            ');
        }
    }, {
        greetings: 'BiwaScheme Interpreter version ' + BiwaScheme.Version,
        width: 500,
        height: 250,
        name: 'biwa',
        exit: false,
        prompt: 'biwascheme>'
    });

    // run trace mode
    BiwaScheme.define_libfunc("trace", 0, 0, function(args) {
        trace = !trace;
        return BiwaScheme.undef;
    });
    // redefine sleep it sould pause terminal
    BiwaScheme.define_libfunc("sleep", 1, 1, function(ar){
        var sec = ar[0];
        assert_real(sec);
        term.pause();
        return new BiwaScheme.Pause(function(pause){
            setTimeout(function(){
                term.resume();
                pause.resume(nil);
            }, sec * 1000);
        });
    });
    /*
    // load should pause terminal
    BiwaScheme.define_libfunc("load", 1, 1, function(ar, intp){
        var path = ar[0];
        assert_string(path);
        term.pause();
        return new BiwaScheme.Pause(function(pause){
            $.ajax({
                url: path,
                processData: false,
                success: function(data) {
                    term.resume();
                    term.echo(data);
                    term.echo(pause instanceof BiwaScheme.Pause);
                    try {
                        // throw too much recursion here
                        bscheme.evaluate(data, function() {
                            return pause.resume(BiwaScheme.undef);
                        });
                        term.echo(path + ' loaded');
                    } catch(e) {
                        term.error(e.message);
                        throw(e);
                    }
                },
                error: function(xhr, stat) {
                    term.error("[AJAX] " + stat + " server reponse: \n" + 
                               xhr.reponseText);
                }});
        });
    });
    */
    // clear terminal
    BiwaScheme.define_libfunc('clear', 0, 0, function(args) {
        term.clear();
        return BiwaScheme.undef;
    });

    // return all procedures from global environment
    BiwaScheme.define_libfunc('env', 0, 0, function(args) {
        var result = new Array();
        for(fun in window.BiwaScheme.CoreEnv) {
            result[result.length] = fun;
        }
        return BiwaScheme.array_to_list(result);
    });

    // return list of object properties like dir from python
    BiwaScheme.define_libfunc('dir', 1, 1, function(args) {
        var result = [];
        var object = args[0];
        for (i in object) {
            result.push(i);
        }
        return BiwaScheme.array_to_list(result);
    });

    // check if object is in list
    BiwaScheme.define_libfunc('contains?', 2, 2, function(args) {
        assert_list(args[1]);
        return $.inArray(args[0], args[1].to_array()) != -1;
    });
    
    // concatenate two or more string
    BiwaScheme.define_libfunc("concat", 1, null, function(args) {
        for (var i=args.length; i--;) {
            assert_string(args[i]);
        }
        return args.length == 1 ? args[0] : args.join('');
    });

    BiwaScheme.define_libfunc("join", 2, 2, function(args) {
        assert_list(args[1]);
        assert_string(args[0]);
        var array = args[1].to_array();
        for (var i=array.length; i--;) {
            assert_string(array[i]);
        }
        return array.join(args[0]);
    });

    BiwaScheme.define_libfunc("split", 2, 2, function(args) {
        assert_string(args[0]);
        assert_string(args[1]);
        var result = args[1].split(args[0]);
        return result.to_list();
    });

});
