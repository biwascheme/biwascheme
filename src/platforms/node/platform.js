BiwaScheme.on_node = true;

BiwaScheme.NodeJS.fs = require('fs');
BiwaScheme.NodeJS.path = require('path');
BiwaScheme.NodeJS.process = process;

//
// Node.js interface
// see also: src/system/_nodejs.js
//
// Chapter 9 File system

// Check the file exists.
//
// filename - string
// Returns true or false.
BiwaScheme.NodeJS.file_exists = function(filename){
  return this.path.existsSync(filename);
};

// Delete a file.
//
// filename - string
// Returns undefined.
BiwaScheme.NodeJS.delete_file = function(filename){
  this.fs.unlinkSync(filename);
};

// Chapter 10 Command-line access and exit values

// Returns array of strings.
// First element is the process name, rest are command-line arguments.
BiwaScheme.NodeJS.command_line = function(){
  return this.process.argv;
};

// Exits the running program.
// obj - Exit code (default: 0)
//       When false(#f) is given, the exit is assumed to be abnormal.
BiwaScheme.NodeJS.exit = function(obj){
  var code = _.isUndefined(obj) ? 0 :
             (obj === false)    ? 1 :
             Number(obj);

  this.process.exit(code);
}

