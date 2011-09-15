function scm_eval(str, func){
  var intp = new BiwaScheme.Interpreter(function(e){ throw e; });

  return intp.evaluate(str, func || function(result){});
}

describe("http-request", function(){
  it("should get a text file", function(){
    var text = null;

    scm_eval('(http-request "testdata.txt")', function(result){
      text = result;
    });

    waitsFor(function(){ return text }, 500);

    runs(function(){
      expect(text).toEqual("ok!\n");
    });
  });
});
