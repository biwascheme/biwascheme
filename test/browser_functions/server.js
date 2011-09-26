#!/usr/bin/env node
// vim: set ft=javascript:

var PORT = 7001;

var express = require('express'),
    app = require('express').createServer(),
    fs = require('fs');

app.configure(function(){
  app.use(express.static(__dirname + '/public'));
  app.use(express.bodyParser());
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

// GET /greet?name=foo
app.get('/greet', function(req, res){
 res.send("Hello, " + req.param("name") + "!");
});

// POST /length (str=...)
app.post('/length', function(req, res){
 var str = req.param("str");
 if(str){
   res.send("" + str.length);
 }
 else{
   res.send("ERROR: str not given");
 }
});

app.listen(PORT);

require('sys').puts("Open http://localhost:"+PORT+"/");
