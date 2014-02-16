How to make a release
=====================

1. Edit History.txt
2. Edit VERSION
3. Edit package.json
4. Build
  - $ make -B
  - Commit as release/biwascheme-x.y.z.js
5. Make sure it is working:
  - run tests
    - open test/spec.html
    - open test/spec.html#release
    - open test/spec.html#min
    - run test/browser_functions/
      - $ cd test/browser_functions && node server.js && open http://localhost:7001/
  - run demos
    - demo/*
    - open repl.html
    - open website/i.html
    - open test/tracer.html
    - open test/tracer_auto.html
  - try npm package
    - $ npm install node_modules/biwascheme/ -g
    - $ biwas -v
6. Push tag
  - $ git tag x.y.z
  - $ git push origin x.y.z
7. Update website
  - checkout biwascheme/biwascheme.github.io
  - Merge master
  - Copy release/biwascheme-x.y.z.js to release/biwascheme.js
  - $ push bs-web web:master
8. Publish npm package
  - $ npm publish
