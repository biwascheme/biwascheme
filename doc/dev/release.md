How to make a release
=====================

1. Edit History.txt
2. Edit VERSION
3. Edit node_modules/biwascheme/package.json
4. $ make -B
5. Make sure it is working:
   + run tests
     - open test/spec.html
     - open test/spec.html#release
     - open test/spec.html#min
     - run test/browser_functions/
       - $ cd test/browser_functions && node server.js && open http://localhost:7001/
   + run demos
     - demo/*
     - open repl.html
     - open website/i.html
     - open test/tracer.html
     - open test/tracer_auto.html
   + try npm package
       - $ npm install node_modules/biwascheme/ -g
       - $ biwas -v
6. Commit and create tag ($ git tag x.y.z)
7. Push changes to github with --tags
8. Update website on web server (git pull)
9. Publish npm package
  - $ npm publish node_modules/biwascheme/
10. Copy generated files (release/*.js, src/version.js) to web server
  - copy src/deps/underscore(.string).js, too

