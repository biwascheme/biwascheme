#!/usr/bin/env gosh
(use srfi-13) ; string
(use file.util)
(use util.match)
(use util.list) ; assoc-ref
(use util.queue) ; queue
(use www.cgi) ; cgi-parse-parameters
(use gauche.threads)

(load "./sack.scm")
(import sack)
(load "./tuplespace.scm")
(import tuplespace)

;;;
;;; httpd
;;;

(define *sack* (sack-new :log-level 1))

(define *httpd-document-root* "../")

(sack-add-routing *sack*
  #/(\.html|\.js|\.css|\.scm)/
  (lambda (req)
    (file->string (build-path *httpd-document-root*
                              (string-drop (base-path-of req) 1)))))

(sack-add-routing *sack*
  #/^\/reload$/
  (lambda (req)
    (load "./sack.scm")
    (load "./tuplespace.scm")
    (load #`"./,*program-name*")
    #`"reloaded at ,(sys-ctime (sys-time))"))

(sack-add-routing *sack*
  #/time/
  (lambda (req)
    (sys-sleep 1) (sys-ctime (sys-time))))

(sack-add-routing *sack*
  #/time5/
  (lambda (req)
    (sys-sleep 6) (sys-ctime (sys-time))))

;;;
;;; tuple space
;;;

(define *ts* (tuplespace-init))

;;
;; non-blocking requests
;;

; ts-init (obsolete)
(define *max-id* 100)
(sack-add-routing *sack*
  #/^\/ts\/init/
  (lambda (req)
    (inc! *max-id*)
    (log-debug #`"new ts connection: id ,|*max-id*|")
    (write-to-string (list *max-id* *count*)))) ;;temp

(define (get-sexp req)
  (read-from-string (caar (cgi-parse-parameters :query-string
                                            (after-of req)))))

; ts-write
(sack-add-routing *sack*
  #/^\/ts\/write\?/
  (lambda (req)
    (let1 sexp (get-sexp req)
      (log-debug sexp)
      (tuplespace-write *ts* sexp)
      (write-to-string sexp))))

; ts-dump
(sack-add-routing *sack*
  #/^\/ts\/dump/
  (lambda (req)
    ;(log-debug (tuplespace-dump *ts*))
    (tuplespace-dump *ts*)))

; ts-readp

(sack-add-routing *sack*
  #/^\/ts\/readp\?/
  (lambda (req)
    (let1 sexp (get-sexp req)
      (write-to-string (tuplespace-readp *ts* sexp)))))

; ts-takep
(sack-add-routing *sack*
  #/^\/ts\/takep\?/
  (lambda (req)
    (let1 sexp (get-sexp req)
      (write-to-string (tuplespace-takep *ts* sexp)))))

;;
;; blocking requests
;;

(define *clients* (make-hash-table)) ;; hash from client-id to <client>

(define (find-client cid-str)
  (let1 cid (string->number cid-str)
    (and (hash-table-exists? *clients* cid)
      (hash-table-get *clients* cid))))

(define-class <client> () ; () = no super class
  ((mutex :accessor mutex-of
          :init-form (make-mutex "mutex of <client>"))
   (cv    :accessor cv-of
          :init-form (make-condition-variable
                       "condition variable of <client>"))
   (queue :accessor queue-of
          :init-form (make-queue))))

(define-method wait-tuple ((client <client>))
  (let loop ()
    (mutex-lock! (mutex-of client))
    (if (not (queue-empty? (queue-of client)))
      (begin
        (let1 result (dequeue! (queue-of client))
          (mutex-unlock! (mutex-of client))
          result))
      (begin
        (mutex-unlock! (mutex-of client) (cv-of client))
        (loop)))))

(define-method notify-tuple ((client <client>) result)
  (with-locking-mutex (mutex-of client)
    (lambda ()                      
      (enqueue! (queue-of client) result)
      (condition-variable-signal! (cv-of client)))))

; init_connection
(define *client-id* 300)
(define (make-client-id)
  (inc! *client-id*))

(sack-add-routing *sack*
  #/^\/ts\/init_connection/
  (lambda (req)
    (let1 cid (make-client-id)
      (hash-table-put! *clients* cid (make <client>))
      (number->string cid))))

; connection
(sack-add-routing *sack*
  #/^\/ts\/connection/
  (lambda (req)
    ;; find client and wait for tuple
    (let* ((cid (sack-query-ref req "cid"))
           (client (find-client cid)))
      (if client
        (match (wait-tuple client)
          ((tuple . ticket)
           (write-to-string (cons ticket tuple))))
        "#f"))))

; ts-read, ts-take
(define last-ticket 200)
(define ticket-mutex (make-mutex "mutex for ticket id"))
(define (make-ticket)
  (with-locking-mutex ticket-mutex
    (lambda ()
      (inc! last-ticket)))) ;; this inc! is atomic

(define *ts-methods*
  `((read . ,tuplespace-read-async)
    (take . ,tuplespace-take-async)))

(define (register-request req method)
  (let ((query (get-sexp req))
        (ticket #`"ticket-,(make-ticket)")
        (ts-method (assoc-ref *ts-methods* method)))
    (let* ((cid (sack-query-ref req "cid"))
           (client (find-client cid)))
      (if client
        (begin
          ;; register request
          (ts-method *ts* query
                     (lambda (tuple)
                       (notify-tuple client (cons tuple ticket))))
          ticket)
        "#f"))))

(sack-add-routing *sack*
  #/^\/ts\/read\?/
  (lambda (req)
    (register-request req 'read)))

(sack-add-routing *sack*
  #/^\/ts\/take\?/
  (lambda (req)
    (register-request req 'take)))

(define *sended-log* 
  (open-output-file "sended_log.txt" :if-exists :append))
(sack-add-routing *sack*
  #/^\/log/
  (lambda (req)
    (let1 data (sack-query-ref req "data")
      (display "\n-------\n" *sended-log*)
      (display data *sended-log*)
      data)))

;;;
;;; manager
;;;
(define *managers* '())

(define (add-manager proc)
  (push! *managers* proc))

(define (start-managers!)
  (for-each
    (lambda (proc)
      (thread-start! (make-thread proc)))
    *managers*))

(define whitelist #f) ;;TODO: better API

;;;
;;; main
;;;
(define (run-comet-server nport log-path)
  (when (not (eq? log-path 'default))
    (close-input-port  (current-input-port))
    (close-output-port (current-output-port))
    (close-output-port (current-error-port)))
  (start-managers!)
  (slot-set! *ts* 'whitelist whitelist)
  (sack-start! *sack* nport log-path))

(define (main args)
  (match (cdr args)
    ((nport log-path) (run-comet-server (string->number nport) log-path))
    ((nport)          (run-comet-server (string->number nport) 'default))
    (()               (run-comet-server 9999 'default))
    (else             (display "Usage: httpd [port] [logfile]\n" (current-error-port)))))

