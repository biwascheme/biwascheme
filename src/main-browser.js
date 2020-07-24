import BiwaScheme from "./main.js";
BiwaScheme.on_node = false;

import { jsonp_receiver } from "./library/webscheme_lib.js"
BiwaScheme.jsonp_receiver = jsonp_receiver;

import Console from "./platforms/node/console.js"
BiwaScheme.Console = Console;

import { current_input, current_output, current_error } from "./platforms/browser/default_ports.js"
BiwaScheme.Port.current_input = current_input;
BiwaScheme.Port.current_output = current_output;
BiwaScheme.Port.current_error = current_error;

// TODO: ideally this should just be `window.BiwaScheme = BiwaScheme` but it will break test/spec.html (grep with `register_tests`)
window.BiwaScheme = window.BiwaScheme || {};
Object.assign(window.BiwaScheme, BiwaScheme);
