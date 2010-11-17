;;;
;;; tuplespace.scm
;;; 

(define-module tuplespace
  (use util.match)
  (use srfi-1)
  (use gauche.threads)
  (export 
    tuplespace-unsafe-query?
    <tuplespace>
    tuplespace-init tuplespace-write 
    tuplespace-read tuplespace-readp tuplespace-read-async
    tuplespace-take tuplespace-takep tuplespace-take-async
    tuplespace-dump tuplespace-clear)
  )
(select-module tuplespace)

(define *debug* #t)

;;
;; class
;;
(define-class <tuplespace> () ; () = no super class
  ((tuples  :init-value '()      :accessor tuples-of) 
   (waiters :init-value '()      :accessor waiters-of)
   (whitelist :init-value #f     :accessor whitelist-of)
   (filter  :init-value identity :accessor filter-of) 
   (mutex   :init-form (make-mutex) :accessor mutex-of)
   ))
(define-class <waiter> ()     ; () = no super class
  ((query    :init-keyword :query    :accessor query-of)
   (notifier :init-keyword :notifier :accessor notifier-of)
   (type     :init-keyword :type     :accessor type-of)
   ))
(define-method inspect-waiters ((ts <tuplespace>))
  (map (lambda (waiter) 
         (write-to-string (query-of waiter)))
    (waiters-of ts)))

;;
;; methods
;;

;utility
(define (tuplespace-unsafe-query? query whitelist)
  (define (unsafe-question-item? x)
    (cond ((pair? x)
           (any unsafe-question-item? x))
          ((symbol? x)
           (if (memq x whitelist) 
             #f 
             x))
          (else 
            #f)))
  (define (unsafe-pair? ls)
    (and (pair? ls)
         (eq? (car ls) '?)
         (any unsafe-question-item? (cdr ls))))
  (define (unsafe-query? ls)
    (any unsafe-pair? ls))

  (if whitelist
    (unsafe-query? query)
    #f))

(define (matches? ts value query)
  (cond ((eq? query '_)
         #t)
        ((tuplespace-unsafe-query? query (whitelist-of ts))
         => (lambda (sym) (error #`"unsafe query: ,sym")))
        (else
          (let1 matcher `(match ',value (,query #t) (_ #f))
            (eval matcher (current-module))))))

(define-method tuplespace-add-waiter ((ts <tuplespace>) query type notifier)
  (with-locking-mutex (mutex-of ts)
    (lambda ()
      (let1 waiter (make <waiter> :query query :type type
                                  :notifier notifier)
        (set! (waiters-of ts) (cons waiter (waiters-of ts)))
        (when *debug* (print "\nwaiter+ " (inspect-waiters ts)))))))


;init 
(define (tuplespace-init . args)
  (apply make <tuplespace> args))

;write

(define (notify-waiters value ts)
  (with-locking-mutex (mutex-of ts)
    (lambda ()                      
      (set! (waiters-of ts)
        (let loop ((waiters (waiters-of ts)))
          (if (null? waiters)
            '()
            (let1 waiter (car waiters)
              (if (matches? ts value (query-of waiter))
                (begin
                  ((notifier-of waiter) value)
                  (when *debug* (print "\nwaiter- " (inspect-waiters ts)))
                  (if (eq? (type-of waiter) 'read)
                    (loop (cdr waiters))
                    (cdr waiters)))
                (begin
                  (cons waiter (loop (cdr waiters))))))))))))

(define-method tuplespace-write ((ts <tuplespace>) value)
  (let1 value ((filter-of ts) value)
    (when *debug* (print "write " value "; " (ref ts 'tuples)))
    (slot-set! ts 'tuples 
               (cons value (ref ts 'tuples)))
    (notify-waiters value ts)
    value))

;readp
(define (find-tuple ts query)
  (find (cut matches? ts <> query)
        (ref ts 'tuples)))

(define-method tuplespace-readp ((ts <tuplespace>) query)
  (find-tuple ts query))

;read

(define-method tuplespace-async ((ts <tuplespace>) query type notifier)
  (let1 found (find-tuple ts query)
    (if found
      (notifier found)
      (tuplespace-add-waiter ts query type notifier))))

(define-method tuplespace-read-async ((ts <tuplespace>) query notifier)
  (tuplespace-async ts query 'read notifier))

(define-method tuplespace-read ((ts <tuplespace>) query)
  (tuplespace-wait ts tuplespace-read-async query))

;takep
(define (remove-tuple! ts tuple)
  (slot-set! ts 'tuples
             (remove (cut eq? tuple <>) (ref ts 'tuples))))

(define-method tuplespace-takep ((ts <tuplespace>) query)
  (let1 found (find-tuple ts query)
    (when found (remove-tuple! ts found))
    found))

;take
(define-method tuplespace-take-async ((ts <tuplespace>) query notifier)
  (tuplespace-async ts query 'take
    (lambda (result)
      (remove-tuple! ts result)
      (when *debug* (print "took " result "; " (ref ts 'tuples)))
      (notifier result))))

(define-method tuplespace-take ((ts <tuplespace>) query)
  (tuplespace-wait ts tuplespace-take-async query))

;wait
(define-method tuplespace-wait ((ts <tuplespace>) ts-method query)
  (let1 mutex (make-mutex "mutex for tuplespace-wait")
    (mutex-lock! mutex)
    (ts-method ts query
               (lambda (result) ;; will be called when a tuple is found
                 (mutex-specific-set! mutex result)
                 (mutex-unlock! mutex)))
    (mutex-lock! mutex) ;blocks until mutex is unlocked
    (mutex-specific mutex)))

;dump
(define-method tuplespace-dump ((ts <tuplespace>))
  (x->string (ref ts 'tuples)))

;clear
(define-method tuplespace-clear ((ts <tuplespace>))
  (set! (tuples-of ts) '()))

(provide "tuplespace")

