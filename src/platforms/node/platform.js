BiwaScheme.on_node = true;

//
// Node.js interface
// see also: src/system/_nodejs.js
//
_.extend(BiwaScheme.NodeJS, {
  fs: require('fs'),
  path: require('path'),
  process: process,

  //
  // R6RS Stdlib 9 File system
  // 

  // Check the file exists.
  //
  // filename - string
  // Returns true or false.
  file_exists: function(filename){
    return this.path.existsSync(filename);
  };

  // Delete a file.
  //
  // filename - string
  // Returns undefined.
  delete_file: function(filename){
    this.fs.unlinkSync(filename);
  };

  //
  // R6RS Stdlib 10 Command-line access and exit values
  //

  // Returns array of strings.
  // First element is the process name, rest are command-line arguments.
  command_line: function(){
    return this.process.argv;
  };

  // Exits the running program.
  // obj - Exit code (default: 0)
  //       When false(#f) is given, the exit is assumed to be abnormal.
  exit: function(obj){
    var code = _.isUndefined(obj) ? 0 :
               (obj === false)    ? 1 :
               Number(obj);

    this.process.exit(code);
  }

  //
  // SRFI 98 - get-environment-variable(s)
  //
  //BiwaScheme.NodeJS.
}
