var BiwaScheme = BiwaScheme || {}; 

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
    if(e.nodeName.toLowerCase() == 'script'){
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

  var src = readAttribute(script, 'src');
  var dir = src.match(/(.*)src\/development_loader.js/)[1];

  var script_tag = function(path){
    return '<script type="text/javascript" src="' +
             path +
           '"><\/script>';
  }
  document.write(script_tag(dir+"src/version.js"));
  document.write(script_tag(dir+"src/console/web-console.js"));
  document.write(script_tag(dir+"src/prototype.js"));
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
  document.write(script_tag(dir+"src/library/webscheme_lib.js"));
  document.write(script_tag(dir+"src/library/extra_lib.js"));
  document.write(script_tag(dir+"src/library/srfi.js"));
  document.write(script_tag(dir+"src/dumper.js"));
  document.write("<script type='text/javascript'>" +
    script.innerHTML +
    "<\/script>");
})();
