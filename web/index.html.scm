(html-doctype)
(html
  (head
    (title "BiwaScheme : Scheme interpreter for browsers")
    (meta :http-equiv "Content-Type"
          :content "text/html; charset=utf-8" )
    (link :href "css/screen.css" :rel "stylesheet" :type "text/css")

    (link :href "css/jquery.terminal.css" :rel "stylesheet" :type "text/css")
    (script :src "repos/release/biwascheme.js" :type "text/javascript")
    (script :src "js/jquery.mousewheel.min.js" :type "text/javascript")
    (script :src "js/jquery.timers.min.js" :type "text/javascript")
    (script :src "js/jquery.cookie.min.js" :type "text/javascript")
    (script :src "js/jquery.terminal-0.3.6.js" :type "text/javascript")
    (script :src "js/biwascheme_terminal.js" :type "text/javascript"))

  (body
    (convert-file "_header.html.scm")

    (div :id "content"
      (h2 "About")
      (p "BiwaScheme is a Scheme interpreter written in JavaScript.")

      (h2 "Try it now")
      (div :id "term")

      (h2 "Download")

      (ul
        (li
          (link-to "biwascheme.js"
                   "repos/release/biwascheme.js")
          " (version " (span :id "ver" "--") ")")
        (li
          (link-to "biwascheme-min.js"
                   "repos/release/biwascheme-min.js")))

      (script :type "text/javascript"
              "jQuery('#ver').html(BiwaScheme.Version)")

      (p
        (link-to "Older versions"
                 "http://github.com/biwascheme/biwascheme/tags")
        " and the "

        (link-to "latest version"
                 "http://github.com/biwascheme/biwascheme")
        " are on github.")

      (h2 "Example")
      (pre "<font color='purple'>&lt;script src=\"biwascheme.js\"&gt;</font>
<font color='purple'>(</font><font color=blue>display</font> <font color=red>\"hello, world!\"</font><font color='purple'>)</font>
<font color='purple'>&lt;/script&gt;</font>")
      
      (h2 "Demo")
      (ul
        (link-list '(("repos/repl.html"
                      . "REPL")
                     ("repos/demo/pictlang.html"
                      . "SICP's picture language")
                     ("http://lambda.bugyo.tk/cdr/hockey/"
                      . "Hockey (pong-like game)")
                     ("http://lambda.bugyo.tk/cdr/dobon/"
                      . "Dobon (a card game)"))))

      (h2 "Features")
      (ul
        (li "Most syntax/base library of R6RS (see "
            (a :href "status.html" "Status") ")")
        (li "Support for "
            (a :title "Wikipedia article about Lisp Macros"
               :href "http://en.wikipedia.org/wiki/Macro_%28computer_science%29#Lisp_macros"
               "Lisp Macros")
            " and Quasiquotation")
        (li "Functions for web application (Ajax, DOM manipulation, etc.)")
        (li "Calling JavaScript functions from Scheme and Scheme from JavaScript")
        (li "Extending scheme interpreter in Javascipt")
        (li "Comprehensive " (link-to "unit test" "repos/test/spec.html"))
        (li "Tiny " (link-to "interpreter debugger" "repos/test/tracer.html"))
        (li (link-to "Mobile version" "i.html")))

      (h2 "Links")

      (ul
        (li
          (a :href "http://www.r6rs.org/" "R6RS")
          " (" (a :href "http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-2.html#node_toc_start"
                 "Language") " / "
               (a :href "http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-1.html#node_toc_start"
                 "Library") ")" ))

      (h2 "Licence")

      (ul
        (li "BiwaScheme: "
            (link-to "MIT License"
                     "http://github.com/biwascheme/biwascheme/blob/master/MIT-LICENSE.txt"))
        (li "BiwaScheme Logo"
            " (by " (link-to "Jakub Jankiewicz" "http://jcubic.pl/") "): "
            (link-to "Creative Commons Attribution 3.0"
                     "http://creativecommons.org/licenses/by/3.0/")
        ))


      (h2 "Contact")

      (p "see " (link-to "Development" "development.html")
         " for ITS and mailing lists.")

      (p "Yutaka HARA (yutaka.hara.gmail.com)" (br)
         (link-to "http://twitter.com/#!/yhara_en"))

      (convert-file "_footer.html.scm")

    )))
