#!/usr/bin/env node
// vim: set ft=javascript:

var PORT = 7001;

var express = require('express'),
    app = require('express')(),
    bodyParser = require('body-parser'),
    fs = require('fs');

app.use(express.static(__dirname + '/public'));
app.use(bodyParser.urlencoded({ extended: true }));

app.get('/fs/spec.js', function(req, res){
  res.set('Content-Type', 'text/javascript');
  res.send(fs.readFileSync("spec.js"))
});

app.get('/fs/biwascheme-min.js', function(req, res){
  res.set('Content-Type', 'text/javascript');
  res.send(fs.readFileSync("../../release/biwascheme-min.js"));
});

// GET /greet (name)
app.get('/greet', function(req, res){
  res.send("Hello, " + req.param("name") + "!");
});

// POST /length (str)
app.post('/length', function(req, res){
  var str = req.body["str"];
  if(str){
    res.send(String(str.length));
  }
  else{
    res.send("ERROR: str not given");
  }
});

// GET /jsonp (callback)
app.get('/jsonp', function(req, res){
  var callback_name = req.param("callback");
  console.log("jsonp", {callback_name: callback_name});

  if(callback_name){ //.match(/^[A-Za-z0-9_]+$/)){
    res.send(callback_name + "('ok')");
  }
  else{
    res.send("ERROR: invalid callback");
  }
});

app.listen(PORT);

console.log("Open http://localhost:"+PORT+"/");
