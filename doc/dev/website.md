## Website

Website of BiwaScheme (http://www.biwascheme.org) is hosted by yhara's personal server.

### How to build *.html

Execute `$ make website -B` to 

1. convert `doc/**/*.md` to HTML, and
2. build index.html from index_.html, doc/_header.html and doc/_footer.html.

### Browse locally (with Node)

```
$ ./local_website.js`
$ open http://localhost:7001/
```

### Browse locally (with [Pow](http://pow.cx))

```
$ ln -s . ~/.pow/biwascheme
$ open http://biwascheme.dev/index.html
```

### Memo for me: how to publish changes

```
$ rake publish        # only doc/ and index.html
$ rake publish ALL=1  # all files including js (used for releases)
```
