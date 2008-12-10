#!/usr/bin/env gosh
(use gauche.test)
(use gauche.threads)
(use rfc.http)
(use srfi-11) ; let-values
;(use srfi-13) ; ??

(test-start "sack")
(load "./sack.scm")
(import sack)
 
;----------------------------------------------------------
(test-section "module code")

(test-module 'sack)

;----------------------------------------------------------
(test-section "instantiation") 

(test* "sack-new" 
  #t 
  (is-a? (sack-new) <sack>))

;----------------------------------------------------------
(test-section "request") 

(test* "sack-query-ref (GET)"
  '("123" . "456")
  (let1 req (make <http-request> 
                  :method "GET" 
                  :path "/foo?abc=123&cde=456")
    (cons (sack-query-ref req "abc")
          (sack-query-ref req "cde"))))

;----------------------------------------------------------
(test-section "network") 

(define *sack* (sack-new :log-level 0))

(sack-add-routing *sack* 
  #/myname/
  (lambda (req)
    "my name is sack"))

(define server-thread 
  (make-thread 
    (lambda () 
      (sack-start! *sack* 12346 'default))))

(define tester-thread
  (make-thread
    (lambda ()
      (test* "GET for routed path"
        "my name is sack"
        (let-values (((status-code headers body)
                      (http-get "localhost:12346" "/myname")))
          body))
      (test-end)
      )))

(thread-start! server-thread)
(sys-sleep 1)
(thread-start! tester-thread)
(thread-join! tester-thread)
