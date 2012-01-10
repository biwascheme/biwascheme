var express = require('express'),
    app = require('express').createServer(),
    fs = require('fs');

app.configure(function(){
  app.use(express.static(__dirname + '/website'));
  //app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
});

//app.get('/', function(req, res){
//  res.send('hello world');
//});

var port = process.env.PORT || 7001;
app.listen(port, function(){
  console.log("Listening on " + port);
});

