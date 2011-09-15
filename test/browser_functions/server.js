#!/usr/bin/env node
// vim: set ft=javascript:

var PORT = 7001;

var express = require('express'),
    app = require('express').createServer(),
    fs = require('fs');

app.configure(function(){
  app.use(express.static(__dirname + '/public'));
  app.use(express.errorHandler({
    dumpExceptions: true,
    showStack: true 
  }));
});

app.get('/fs/spec.js', function(req, res){
  res.send(fs.readFileSync("spec.js"),
           { 'Content-Type': 'text/javascript' });
});

app.get('/fs/biwascheme-min.js', function(req, res){
  res.send(fs.readFileSync("../../release/biwascheme-min.js"),
           { 'Content-Type': 'text/javascript' });
});

app.listen(PORT);

require('sys').puts("Open http://localhost:"+PORT+"/");
