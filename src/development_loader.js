var BiwaScheme = BiwaScheme || {}; 

(function(){ //local namespace
  // Find the script tag
  var find_script_tag = function(e){
    if(e.nodeName.toLowerCase() == 'script'){
      return e;
    }
    else if(e.id == '_firebugConsole'){
      if(e.previousSibling.nodeName.toLowerCase() == 'script')
        return e.previousSibling;
      else
        console.error("BiwaScheme could not find the script tag... please use firebug 1.5.0");
    }
    else{
      return find_script_tag(e.lastChild);
    }
  };

  var script = find_script_tag(document);

  var src = script.getAttribute("src");
  var dir = src.match(/(.*)src\/development_loader.js/)[1];

  var script_tag = function(path){
    return '<script type="text/javascript" src="' +
             path +
           '"><\/script>';
  };
  document.write(script_tag(dir+"src/version.js"));
  document.write(script_tag(dir+"src/deps/jquery.js"));
  document.write(script_tag(dir+"src/deps/underscore.js"));
  document.write(script_tag(dir+"src/deps/underscore.string.js"));
  document.write(script_tag(dir+"src/platforms/browser/console.js"));
  document.write(script_tag(dir+"src/system/class.js"));
  document.write(script_tag(dir+"src/stackbase.js"));
  document.write(script_tag(dir+"src/system/set.js"));
  document.write(script_tag(dir+"src/system/write.js"));
  document.write(script_tag(dir+"src/system/pair.js"));
  document.write(script_tag(dir+"src/system/value.js"));
  document.write(script_tag(dir+"src/system/symbol.js"));
  document.write(script_tag(dir+"src/system/char.js"));
  document.write(script_tag(dir+"src/system/number.js"));
  document.write(script_tag(dir+"src/system/port.js"));
  document.write(script_tag(dir+"src/system/record.js"));
  document.write(script_tag(dir+"src/system/enumeration.js"));
  document.write(script_tag(dir+"src/system/hashtable.js"));
  document.write(script_tag(dir+"src/system/syntax.js"));
  document.write(script_tag(dir+"src/system/types.js"));
  document.write(script_tag(dir+"src/system/parser.js"));
  document.write(script_tag(dir+"src/system/compiler.js"));
  document.write(script_tag(dir+"src/system/pause.js"));
  document.write(script_tag(dir+"src/system/call.js"));
  document.write(script_tag(dir+"src/system/interpreter.js"));
  document.write(script_tag(dir+"src/library/infra.js"));
  document.write(script_tag(dir+"src/library/r6rs_lib.js"));
  document.write(script_tag(dir+"src/library/js_interface.js"));
  document.write(script_tag(dir+"src/library/webscheme_lib.js"));
  document.write(script_tag(dir+"src/library/extra_lib.js"));
  document.write(script_tag(dir+"src/library/srfi.js"));
  document.write(script_tag(dir+"src/platforms/browser/dumper.js"));
  document.write("<script type='text/javascript'>" +
    script.innerHTML +
    "<\/script>");
})();
