## Getting Started

### HTML and Browser

1. Download biwascheme.js
2. Save the following as foo.html and open it

```

<!DOCTYPE html> 
<html> 
  <head> 
    <meta charset="utf-8" /> 
    <title>BiwaScheme example</title>
  </head>
  <body>
    <div id="bs-console">
    </div>
    <script src="biwascheme.js">
      (print "Hello, world!")
      (print (current-date))
      (console-log "ok.")
    </script>
  </body>
</html>
```

### Node.js

<a name="nodejs" />

There is a npm package of BiwaScheme for Node.js.

```
$ npm install biwascheme -g
$ biwas
> (+ 1 2)
3
```
