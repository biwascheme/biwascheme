// 
// Heap based scheme from 3imp.pdf
//

// default definition of puts: should be overriden for console interpreters
function puts(str, no_newline){
  var text = (str + (no_newline ? "" : "\n")).escapeHTML();
  var span = document.createElement("span");
  span.innerHTML = text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;");
  $('bs-console').insert(span);
}
function p(/*args*/){
  puts("p> "+$A(arguments).map(Object.inspect).join(" "));
}

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  /* --------------------------------------- namespace webscheme */ 

  //
  // variables
  //
  BiwaScheme.TopEnv = {};
  BiwaScheme.CoreEnv = {};

  //
  // Classes
  //

  BiwaScheme.Error = Class.create({
    initialize: function(msg){
      this.message = "Error: "+msg;
    },
    toString: function(){
      return this.message;
    }
  });

  BiwaScheme.Bug = Class.create(Object.extend(new Error(), {
    initialize: function(msg){
      this.message = "[BUG] "+msg;
    }
  }));
}
