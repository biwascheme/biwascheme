import { escape } from "../../utils.js"
import { Port } from "../../system/port.js"

const current_input = new Port.CustomInput(
  function (callback) {
    const out = document.querySelector("#bs-console");
    const form = document.createElement("form");
    form.innerHTML = "<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>";
    out.appendChild(form);
    form.addEventListener("submit", function(){
      const input = document.querySelector("#webscheme-read-line").value;
      form.remove();
      callback(input);
      return false;
    });
  }
);

const current_output = new Port.CustomOutput(
  function (str) {
    const out = document.querySelector("#bs-console");
    if (!out) return;
    const span = document.createElement("span");
    span.innerHTML = escape(str)
      .replace(/\n/g,"<br>")
      .replace(/ /g,"&nbsp;");
    out.appendChild(span);
  }
);

const current_error = current_output;

export { current_input, current_output, current_error }
