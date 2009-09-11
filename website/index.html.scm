(html-doctype)
(html
  (convert-file "_head.html.scm")
  (body
    (convert-file "_header.html.scm")

    (div :id "content"
      (h2 "About")
      (p "BiwaScheme is a Scheme interpreter written in JavaScript.")

      (h2 "Example")
      (pre "<font color='purple'>&lt;script src=\"lib/biwascheme.js\"&gt;</font>
<font color='purple'>(</font><font color=blue>display</font> <font color=red>\"hello, world!\"</font><font color='purple'>)</font>
<font color='purple'>&lt;/script&gt;</font>")
      
      (p "Try it now: "
         (a :href "repos/repl.html"
            "BiwaScheme REPL"))

      (h2 "Demo")
      (ul
        (link-list '(("repos/repl.html"
                      . "REPL")
                     ("repos/demo/pictlang.html"
                      . "SICP's picture language")
                     ("http://lambda.bugyo.tk/cdr/dobon/"
                      . "Dobon (a card game)"))))

      (h2 "Features")
      (ul
        (li "Most syntax/base library of R6RS (see "
            (a :href "status.html" "Status") ")")
        (li "Functions for web application (Ajax, DOM manipulation, etc.)")
        (li "Calling JavaScript functions from Scheme")
        (li "Comprehensive " (link-to "unit test" "repos/test/spec.html"))
        (li "Tiny " (link-to "interpreter debugger" "repos/test/tracer.html")))

      (h2 "Contact")

      (p "see " (link-to "Development" "development.html")
         " for ITS and mailing lists.")

      (p "Yutaka HARA (yutaka.hara.gmail.com)" (br)
         (link-to "http://route477.net/"))

      (p (link-to "日本語の情報はこちら"
                  "http://route477.net/w/?BiwaScheme"))

      (convert-file "_footer.html.scm")

    )))
