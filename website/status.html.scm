(define (make-table-rows url-maker rows)
  (map (lambda (row)
         (let ((section (car row))
               (text (cadr row))
               (stat (caddr row)))
           (tr
             (td section)
             (td (a :href (url-maker section) text))
             (td (if (#/ok/ stat)
                   (em stat)
                   stat)))))
       rows))

(define make-r6rs-base-rows 
  (pa$ make-table-rows
    (lambda (section)
      #`"http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-14.html#node_sec_,|section|")))

(define make-r6rs-library-rows
  (pa$ make-table-rows
    (lambda (section)
      #`"http://www.r6rs.org/final/html/r6rs-lib/r6rs-lib-Z-H-,(+ section 1).html#node_chap_,|section|")))

(define make-srfi-rows
  (pa$ make-table-rows
    (lambda (section)
      #`"http://srfi.schemers.org/srfi-,|section|/srfi-,|section|.html")))

(html-doctype)
(html
  (convert-file "_head.html.scm")
  (body
    (convert-file "_header.html.scm")

    (div :id "content"
      (h2 "Implementation Status")

      (p "BiwaScheme aims to conform to the latest
          specification of the Scheme language, "
          (link-to "R6RS" "http://www.r6rs.org/") ".")

      (p "Most of the important features are already supported.
          The biggest features not implemented are
          errors and hygenic macros
          (for now, BiwaScheme has 'define-macro' instead).")

      (h3 "R6RS Base library")

      (table
        (tr
          (map th '("section" "title" "status")))

        (make-r6rs-base-rows
          '(("11.4" "Expressions"
             "ok")
            ("11.5" "Equivalence predicates"
             "ok")
            ("11.6" "Procedure predicate"
             "ok")
            ("11.7.4.1" "Numerical operations"
             "almost ok")
            ("11.8" "Booleans"
             "ok")
            ("11.9" "Paris and lists"
             "ok")
            ("11.10" "Symbols"
             "ok")
            ("11.11" "Characters"
             "ok")
            ("11.12" "Strings"
             "ok")
            ("11.13" "Vectors"
             "ok")
            ("11.14" "Errors and violations"
             "not yet")
            ("11.15" "Control features"
             "ok")
            ("11.16" "Iteration"
             "ok")
            ("11.17" "Quasiquotation"
             "almost ok (except vector quasiquotation)")
            ("11.18" "Binding constructs for syntactic keywords"
             "not yet")
            ("11.19" "Macro transformers"
             "not yet")
            ("11.20" "Tail calls and tail contexts"
             "ok")
          )))

      (h3 "R6RS Standard Libraries")
      
      (table
        (make-r6rs-library-rows
          '((1 "Unicode"
            "not yet")
            (2 "Bytevectors"
             "(no plan)")
            (3 "List utilities"
             "ok")
            (4 "Sorting"
             "partially ok")
            (5 "Control structures"
             "partially ok (except case-lambda)")
            (6 "Records"
             "ok")
            (7 "Exceptions and conditions"
             "not yet")
            (8 "I/O"
             "partially ok (output to 'stdout')")
            (9 "File system"
             "not yet")
            (10 "Command-line access and exit values"
             "not yet")
            (11 "Arithmetic"
             "not yet")
            (12 "syntax-case"
             "not yet")
            (13 "Hashtables"
             "almost ok")
            (14 "Enumerations"
             "not yet")
            (15 "Composite library"
             "not yet")
            (16 "eval"
             "almost ok (eval)")
            (17 "Mutable pairs"
             "(no plan)")
            (18 "Mutable strings"
             "(no plan) # because JavaScript does not provide mutable strings")
            (19 "R5RS compatibility"
             "(no plan)"))))

      (p "'(no plan)' means that I have no plan to support it,
         but does not mean will never include it; 
         patches are welcome.")

      (h3 "SRFIs")

      (p "See " (link-to "http://srfi.schemers.org/")
         " for SRFI (Scheme Requests for Implementation).")

      (table
        (make-srfi-rows
          '((1 "list utilities"
             "partially (iota)")
            (6 "string port"
             "ok")
            (13 "string utilities"
             "not yet")
            (19 "time"
             "partially")
            (27 "random"
             "partially (random-integer)")
            (30 "multi-line comment"
             "ok")
            (38 "write/ss"
             "partially (write/ss)")
            (43 "vector"
             "partially (vector-append)")
            (62 "s-expr comment"
             "ok"))))

      (convert-file "_footer.html.scm")
    )))

