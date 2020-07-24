import { Port } from "../../system/port.js"

const current_input = new Port.CustomInput(
  function (callback) {
    var form = $("<form/>");
    form.html("<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>");
    $("#bs-console").append(form);
    form.submit(function(){
      var input = $("#webscheme-read-line").val();
      form.remove();
      callback(input);
      return false;
    });
  }
);

const current_output = new Port.CustomOutput(
  function (str) {
    var console;
    var text;
    console = $("#bs-console");
    if (console[0]) {
      text = _.escape(str);
      var span = $("<span>");
      span.html(text.replace(/\n/g,"<br>").replace(/ /g,"&nbsp;"));
      console.append(span);
    }
  }
);

const current_error = current_output;

export { current_input, current_output, current_error }
