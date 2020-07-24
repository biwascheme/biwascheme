import BiwaScheme from "./main.js";
BiwaScheme.on_node = true;

import "./platforms/node/node_functions.js"

import { run, run_file, node_setup } from "./platforms/node/module_postamble.js"
BiwaScheme.run = run;
BiwaScheme.run_file = run_file;
node_setup(BiwaScheme);

import Console from "./platforms/node/console.js"
BiwaScheme.Console = Console;

import { current_input, current_output, current_error } from "./platforms/node/default_ports.js"
BiwaScheme.Port.current_input = current_input;
BiwaScheme.Port.current_output = current_output;
BiwaScheme.Port.current_error = current_error;

module.exports = BiwaScheme;
