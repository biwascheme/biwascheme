#!/usr/bin/env node
//
// local_website.js : Start http server on localhost:7001
// (You can preview www.biwascheme.org with this)
//

var express = require('express'),
    app = require('express').createServer(),
    fs = require('fs');

app.configure(function(){
  app.use(express.static(__dirname));
  //app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
});

//app.get('/', function(req, res){
//  res.send('hello world');
//});

var port = process.env.PORT || 7001;
app.listen(port, function(){
  console.log("Listening on " + port);
});

