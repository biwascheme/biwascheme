import { Port } from "../../system/port.js"

const current_input = new Port.CustomInput(
  function (callback) {
    var rl = require('readline').createInterface({
      input: process.stdin
    });
    rl.on('line', function (line) {
      rl.close();
      callback(line);
    });
    rl.setPrompt('', 0);
    rl.prompt()
  }
);

const current_output = new Port.CustomOutput(
  function (str) {
    process.stdout.write(str)
  }
);

const current_error = current_output;

export { current_input, current_output, current_error }
