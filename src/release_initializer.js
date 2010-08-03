//
// release_initializer.js - read user's program and eval it (if it exists)
//
// This file is put on the end the lib/biwascheme.js.
//
(function(){ //local namespace

  // readAttribute (taken from prototype.js 1.6.0)
  var readAttribute = function(element, name) {
    if (/*Prototype.Browser.IE*/ !!(window.attachEvent && !window.opera)){
      var t = {
        names: {
          'class': 'className',
          'for':   'htmlFor'
        },
        values: {
          _getAttr: function(element, attribute) {
            return element.getAttribute(attribute, 2);
          },
          _getAttrNode: function(element, attribute) {
            var node = element.getAttributeNode(attribute);
            return node ? node.value : "";
          },
          _getEv: function(element, attribute) {
            var attribute = element.getAttribute(attribute);
            return attribute ? attribute.toString().slice(23, -2) : null;
          },
          _flag: function(element, attribute) {
            return $(element).hasAttribute(attribute) ? attribute : null;
          },
          style: function(element) {
            return element.style.cssText.toLowerCase();
          },
          title: function(element) {
            return element.title;
          }
        }
      };
      if (t.values[name]) return t.values[name](element, name);
      if (t.names[name]) name = t.names[name];
      if (name.indexOf(':') > -1){
        return (!element.attributes || !element.attributes[name]) ? null :
         element.attributes[name].value;
      }
    }
    return element.getAttribute(name);
  }

  // Find the script tag
  var find_script_tag = function(e){
    if (e == null) {
	return e;
    }
    else if(e.nodeName.toLowerCase() == 'script'){
      return e;
    }
    else if(e.id == '_firebugConsole'){
      if(e.previousSibling.nodeName.toLowerCase() == 'script')
        return e.previousSibling;
      else
        console.error("BiwaScheme could not find the script tag... please use firebug 1.5.0")
    }
    else{
      return find_script_tag(e.lastChild);
    }
  };
  var script = find_script_tag(document);

  // Error handler (show message to console div)
  var onError = function(e, state){
    puts(e.message); 
    if($("biwascheme-debugger")){
      var dumper = new BiwaScheme.Dumper($("biwascheme-debugger"));
      dumper.dump(new Hash(state));
      dumper.dump_move(1);
    }
    throw(e);
  }

  // Start user's program
  if (script) {
    var intp = new BiwaScheme.Interpreter(onError);
    try{
      intp.evaluate(script.innerHTML, Prototype.emptyFunction);
    }
    catch(e){
      onError(e);
    }
  }
})();
