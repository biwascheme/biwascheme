#!/usr/bin/env gosh
; usage: gosh converter.scm .
; needs Gauche
(use text.html-lite)
(use text.tree)
(use file.util)
(use srfi-1)

; utilities for htmls

;(define (r6rs-page path)
;  (string-append "http://www.r6rs.org/final/html/r6rs/" path))

(define (link-to text . rest)
  (if (null? rest)
    (html:a :href text text)
    (html:a :href (car rest) text)))

;(define (link-to-r6rs text path)
;  (link-to text (r6rs-page path)))

;(define (make-table rows)
;  (define (make-tr row)
;    (html:tr
;      (map (compose html:td tree->string) row)))
;
;  (html:table
;    (map make-tr rows)))

(define (link-list pairs)
  (map (lambda (pair)
         (html:li (html:a :href (car pair) (cdr pair))))
       pairs))

; converter

(define TAGS
  '(a        abbr       acronym    address     area      b
    base     bdo        big        blockquote  body      br
    button   caption    cite       code        col       colgroup
    dd       del        dfn        div         dl        dt
    em       fieldset   form       frame       frameset  
    h1       h2         h3         h4          h5        h6
    head     hr         html       i           iframe    img
    input    ins        kbd        label       legend    li
    link     meta       nofrmaes    noscript  object
    ol       optgroup   option     p           param     pre
    q        samp       script     select      small     span
    strong   style      sub        sup         table     tbody
    td       textarea   tfoot      th          thead     title
    tr       tt         ul         var))
  ; map

(define (add-html: sym)
  (if (member sym TAGS)
    (string->symbol #`"html:,|sym|")
    sym))

(define (add-htmls sexp)
  (if (pair? sexp)
    (if (symbol? (car sexp))
      (cons (add-html: (car sexp))
            (add-htmls (cdr sexp)))
      (cons (add-htmls (car sexp))
            (add-htmls (cdr sexp))))
    sexp))

(define (sexp->tree sexp)
  (eval (add-htmls sexp) (interaction-environment)))

(define (sexp->string sexp)
  (let1 tree (sexp->tree sexp)
    (if (eq? 'define (car sexp))
      ""
      (tree->string tree))))

(define (convert-file path)
  (apply string-append
    (map sexp->string (file->sexp-list path))))

(define (convert-file! path)
  (define (destname path)
    ((#/(.*)\.scm$/ path) 1))

  (let1 destfile (destname path)
    (with-output-to-file destfile
      (compose display (pa$ convert-file path)))
    (print #`"wrote ,|destfile|")))

; main

(define (main args)
  (define (html-scm? path)
    (#/^[^_](.*)\.html\.scm$/ path))

  (case (length args)
    ((2)
     (for-each convert-file!
               (directory-list (cadr args) :filter html-scm?))
     0)
    (else
      (print "usage: converter.scm dir"))))
