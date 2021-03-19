//
// see also:
// * src/library/webscheme_lib.js : library functions
// * test/browser_functions/server.js : server side helpers
//

function scm_eval(str, func){
  var intp = new BiwaScheme.Interpreter(function(e){ throw e; });

  return intp.evaluate(str, func || function(result){});
}

function div1() { return document.querySelector("#div1") }
function input1() { return document.querySelector("#input1") }

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

describe("element-content", () => {
  it("should get the content of a div", () => {
    div1().innerHTML = "ok"
    const result = scm_eval('(element-content ($ "#div1"))')
    expect(result).toBe("ok")
  })

  it("should get the value of an input", () => {
    input1().value = "ok"
    const result = scm_eval('(element-content ($ "#input1"))')
    expect(result).toBe("ok")
  })
})

describe("element-read-attribute", () => {
  it("should get the attribute of a div", () => {
    const result = scm_eval('(element-read-attribute ($ "#div1") "class")')
    expect(result).toBe("class1")
  })

  it("should get the attribute of an input", () => {
    input1().value = "ok"
    const result = scm_eval('(element-read-attribute ($ "#input1") "value")')
    expect(result).toBe("ok")
  })
})

describe("element-write-attribute!", () => {
  it("should set the attribute of a div", () => {
    scm_eval('(element-write-attribute! ($ "#div1") "class" "wrote")')
    const result = div1().classList[0]
    expect(result).toBe("wrote")
  })

  it("should set the attribute of an input", () => {
    scm_eval('(element-write-attribute! ($ "#input1") "value" "wrote")')
    const result = input1().value
    expect(result).toBe("wrote")
  })
})

describe("element-empty!", () => {
  it("should clear the content of a div", () => {
    scm_eval('(element-empty! ($ "#div1"))')
    const result = div1().innerHTML
    expect(result).toBe("")
  })

  it("should clear the value of an input", () => {
    scm_eval('(element-empty! ($ "#input1"))')
    const result = input1().value
    expect(result).toBe("")
  })
})
