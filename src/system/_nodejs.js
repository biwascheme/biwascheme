
BiwaScheme.NodeJS = function(/*arguments*/){
  var args = Array.prototype.slice.call(arguments);
  var funcname = args.shift();

  if(BiwaScheme.on_node){
    var nodejs = BiwaScheme.NodeJS;
    return nodejs[funcname].apply(nodejs, args);
  }
  else{
    throw new BiwaScheme.Error("Node.JS required");
  }
};
