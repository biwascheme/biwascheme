//
// see also:
// * src/library/webscheme_lib.js : library functions
// * test/browser_functions/server.js : server side helpers
//

function scm_eval(str, func){
  var intp = new BiwaScheme.Interpreter(function(e){ throw e; });

  return intp.evaluate(str, func || function(result){});
}

describe("http-request", function(){
  it("should get a text file", function(){
    var result = null;

    scm_eval('(http-request "testdata.txt")', function(text){
      result = text;
    });

    waitsFor(function(){ return result }, 500);

    runs(function(){
      expect(result).toEqual("ok!\n");
    });
  });

  it("should get a url with param", function(){
    var result = null;

    scm_eval('(http-request "/greet?name=yhara")', function(text){
      result = text;
    });

    waitsFor(function(){ return result }, 500);

    runs(function(){
      expect(result).toEqual("Hello, yhara!");
    });
  });
});

describe("http-post", function(){
  it("should issue a post request", function(){
    var result = null;

    scm_eval('(http-post "/length" \'(("str" . "asdf")))', function(text){
      result = text;
    });

    waitsFor(function(){ return result }, 500);

    runs(function(){
      expect(result).toEqual("4");
    });
  });
});

describe("receive-jsonp", function(){
  it("should get a json", function(){
    var result = null;

    scm_eval('(receive-jsonp "/jsonp")', function(text){
      result = text;
    });

    waitsFor(function(){ return result }, 500);

    runs(function(){
      expect(result).toEqual("ok");
    });
  });
});
