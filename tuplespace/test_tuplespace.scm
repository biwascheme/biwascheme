(use gauche.test)
(use gauche.threads)

(test-start "tuple space")
(load "./tuplespace.scm")
(import tuplespace)

(test-module 'tuplespace)

(define *ts* #f)

(define-syntax test-ts
  (syntax-rules ()
    ((test-ts msg expected tester ...)
     (test msg expected
       (lambda ()
         (set! *ts* (tuplespace-init))
         tester
         ...)))))

(test-section "init")
(test* "tuplespace-init should make instance of <tuplespace>" 
       #t 
       (is-a? (set! *ts* (tuplespace-init)) <tuplespace>))

(test-section "write")
(test* "tuplespace-write should return value wrote"
       '(1 2 3) 
       (tuplespace-write *ts* '(1 2 3)))

(test-section "dump")
(test* "tuplespace-dump should dump all tuples"
       "((1 2 3))"
       (tuplespace-dump *ts*))

(test-section "clear")
(test* "tuplespace-clear should remove all tuples"
       "()"
       (begin
         (tuplespace-clear *ts*)
         (tuplespace-dump *ts*)))

(test-section "readp")
(test-ts "tuplespace-readp should return matched value" 
         '(1 2 3) 
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-readp *ts* '(1 2 3))))

(test-ts "tuplespace-readp should return #f when not found" 
         #f 
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-readp *ts* '(4 5))))

(test-ts "tuplespace-readp should recognize wild card" 
         '(1 2 3) 
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-write *ts* '(4 5 6))
           (tuplespace-write *ts* '(1 2))
           (tuplespace-readp *ts* '(1 _ _))))

(test-ts "tuplespace-readp should recognize 'not' pattern" 
         '(3 4 5)
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-write *ts* '(3 4 5))
           (tuplespace-readp *ts* '((not 1) _ _))))

(test-ts "tuplespace-readp should return #f when not found" 
         #f 
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-write *ts* '(3 4 5))
           (tuplespace-readp *ts* '(4 5))))

(test-section "read")
(test-ts "tuplespace-read should return immediately if found"
         '(1 2 3)
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-read *ts* '(1 2 3))))


(test-section "read")
(define (make-waiter-thread ts-method query)
  (make-thread
    (lambda ()
      (ts-method *ts* query))))

(test-ts "tuplespace-read should wait until tuple is found"
         '(1 2 3)
         (begin
           (let ((result #f)
                 (waiter-thread (make-waiter-thread tuplespace-read '(1 2 3))))
             (thread-start! waiter-thread)
             (tuplespace-write *ts* '(1 2 3))
             (thread-join! waiter-thread))))

(test-section "takep")
(test-ts "tuplespace-takep should return matched value"
         '(1 2 3)
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-takep *ts* '(1 2 3))))

(test-ts "tuplespace-takep should remove matched value"
         #f
         (begin
           (tuplespace-write *ts* '(1 2 3))
           (tuplespace-takep *ts* '(1 2 3))
           (tuplespace-takep *ts* '(1 2 3))))

(test-ts "tuplespace-take should wait until tuple is found"
         '((1 2 3) "()")
         (begin
           (let ((result #f)
                 (waiter-thread (make-waiter-thread tuplespace-take '(1 2 3))))
             (thread-start! waiter-thread)
             (tuplespace-write *ts* '(1 2 3))
             (list (thread-join! waiter-thread)
                   (tuplespace-dump *ts*)))))

(test-section "tuplespace-unsafe-query?")
(test* "tuplespace-unsafe-query? should detect unsafe query"
  #t
  (tuplespace-unsafe-query? '(_ (? (lambda (x) (bomb))) _)
           '(lambda x)))

(test* "tuplespace-unsafe-query? should pass safe query"
  #f
  (tuplespace-unsafe-query? '(_ (? (pa$ < 100)) _ _ _)
           '(pa$ <)))
       
;(test-ts "tuplespace-read should raise error for unsafe query"
;         #f
;         (guard (e ((is-a? e <error>) 1)
;                   (else 2))
;           (slot-set! *ts* 'whitelist '(lambda x))
;           (tuplespace-write *ts* '(99)))
;           (tuplespace-read *ts* '((? number?))))

(test-end)

