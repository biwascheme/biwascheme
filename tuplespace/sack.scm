;;;
;;; sack.scm
;;; 

(define-module sack
  (use gauche.net)
  (use gauche.logger)
  (use gauche.threads)
;  (use srfi-11) ; let-values
  (use srfi-13) ; string-tokenize
  (use rfc.822) ; internet message format
  (use util.match)
  (use util.queue)
  (use util.list) ; assoc-ref
  (use www.cgi) ; cgi-parse-parameters, etc
  (export 
    <sack> base-path-of after-of log-debug log-info log-connection log-error
    sack-query-ref
    sack-new sack-start! sack-add-routing
    ;; for test
    <http-request>
    )
  )
(select-module sack)
;; module start -------------------------------------------
(define *sack* #f) ; very tenuki :-(

;;;
;;; macro
;;;
; (try (begin ...) (rescue ...))
(define-syntax try
  (syntax-rules (rescue)
    ((try body (rescue e exp ...))
     (guard (e (else e exp ...))
       body))))

(define-syntax thread-new!
  (syntax-rules ()
    ((start-new-thread! body ...)
     (thread-start!
       (make-thread
         (lambda ()
           body ...))))))

;;;
;;; settings
;;;

(define server-name "BiwaTuples/0.0.2")
(define document-root "/home/www/htdocs/")
(define log-level 4)

;;;
;;; logger
;;;
                            ;    0: No log messages at all
(define-constant LOG_ERR 1) ;    1: log on error
(define-constant LOG_CON 2) ;    2: log on connection (For normal use)
(define-constant LOG_INF 3) ;    3: verbose message
(define-constant LOG_DBG 4) ;    4: debug message (everything)

(define (print-log level obj)
  (if (>= log-level level)
    (log-format (format "~,,,,50a~%" obj))))

(define (log-error obj) (print-log 1 obj))
(define (log-connection obj) (print-log 2 obj))
(define (log-info obj) (print-log 3 obj))
(define (log-debug obj) (print-log 4 obj))

;;;
;;; dispatch
;;;

(define *sack-default-routing*
  (list (cons #// (lambda (req) 
                    #`"hello, I'm cometserv. ,(ref req 'path)"))))

(define (httpd-dispatch req)
  (let1 path (ref req 'path)
    (let1 item (find (lambda (x) (rxmatch (car x) path))
                     (ref *sack* 'routing))
      (log-debug item)
      (let ((rexp (car item))
            (proc (cdr item)))
        (slot-set! req 'base-path (let1 m (rxmatch rexp path) (string-append (rxmatch-before m) (rxmatch-substring m))))
        (slot-set! req 'after (rxmatch-after (rxmatch rexp path)))
        (proc req)))))

;;;
;;; http responce
;;;
;(define (httpd-400-bad-request msg )
;  (let ((tree (cons (cgi-header)
;                    (html:html
;                      (html:head (html:title "Error: Bad request"))
;                      (html:body
;                        (html:h1 "Bad request")
;                        (html:p msg "\n"))))))
;    (reply-with-tree 400 tree out)))
;
;(define (httpd-501-not-implemented method)
;  (let ((tree (cons (cgi-header)
;                    (html:html
;                      (html:head (html:title "Error"))
;                      (html:body
;                        (html:h1 "Error")
;                        (html:p "Method " method " is not implemented\n"))))))
;    (reply-with-tree 501 tree out)))

;;;
;;; http
;;;

(define (httpd-send-responce text port)
  (log-connection "200 OK")
  (display 
    (string-append 
      #`"HTTP/1.0 200 OK\r\nServer: ,|server-name|\r\n"
      #`"Content-type: text/html\r\n"
      #`"Content-Length: ,(string-size text)\r\n"
      #`"Cache-Control: no-cache\r\n"
      #`"\r\n"
      text)
    port)
  (flush port))

(define (httpd-parse-request req-line)
  (log-debug (string-tokenize req-line))
  (match (string-tokenize req-line)
         ((method path protocol)
          (make <http-request> :method method :path path :protocol protocol) )
         ((method path)
          (make <http-request> :method method :path path))
         ((method)
          (log-error #`"400 Bad Request (path unspecified)"))))
                
(define (httpd-recieve cs)
  (thread-start!
    (make-thread
      (lambda ()
        (let* ((in (socket-input-port cs))
               (req (httpd-parse-request (read-line in))))
          (slot-set! req 'header (rfc822-header->list in))
          (let1 cl (assoc "content-length" (header-of req))
            (slot-set! req 'body
              (if cl
                (let1 len (string->number (cadr cl))
                  (string-incomplete->complete
                    (read-block len in)))
                "")))

          (dynamic-wind ;;TODO: really needs this?
            (lambda () #f)
            (lambda ()
              (try
                (let loop ((cont httpd-dispatch))
                  (let1 res (cont req)
                    (if (string? res)
                      (httpd-send cs res)
                      (loop res))))
                (rescue e 
                        (log-error #`"My handler: ,(slot-ref e 'message)")
                        (print (ref e 'message))
                        (report-error e))))
            (lambda ()
              (socket-close cs)
              (thread-terminate! (current-thread)))))))))

(define (httpd-send cs res)
  (let1 out (socket-output-port cs)
    (httpd-send-responce res out)
    (log-debug "connection end.")))

(define (httpd-main nport)
  (let1 ss (make-server-socket 'inet nport :reuse-addr? #t)
    (while #t
      (try (begin 
             (log-debug "Waiting for a client")
             (let1 cs (socket-accept ss)
               (log-debug "A Client accepted")
               (httpd-recieve cs))
             (log-debug "One reqest pushed"))
           (rescue e
             (log-error #`"My handler: ,(slot-ref e 'message)")
             (print (ref e 'message))
             (report-error e))))))

(define (httpd-start nport log-path)
  (log-open (cond 
	      ((eq?      log-path 'default) #t); log to current error port
	      ((string=? log-path "syslog") 'syslog)
	      (else                         log-path)))
  (log-connection #`"server started with port ,|nport|")

  (try (begin
         (set-signal-handler!  SIGPIPE 
           (lambda (n) (log-error #`"Catched signal ,|n|")))
          (set-signal-handler!  SIGINT
            (lambda (n) (log-error #`"Received SIGINT") (exit 0))))
       (rescue e #f))


  (httpd-main nport))

;;;
;;; request
;;;
(define-class <http-request> () ; () = no super class
  ((method   :init-keyword :method   :accessor method-of)
   (path     :init-keyword :path     :accessor path-of) 
   (protocol :init-keyword :protocol :accessor protocol-of) 
   (base-path :accessor base-path-of)
   (after  :accessor after-of)
   (body   :accessor body-of)
   (header :accessor header-of)
   ))

(define-method sack-query-ref ((req <http-request>) key)
  (define (query-ref str key)
    (and str
      (car (assoc-ref (cgi-parse-parameters :query-string str)
                      key))))
  (query-ref (if (#/\?/ (path-of req))
               (rxmatch-after (#/.*?\?/ (path-of req)))
               (body-of req))
             key))

;;;
;;; responce
;;;
(define-class <http-responce> () ; () = no super class
  (body status-code status-message) ; slots
  ) 

;;;
;;; sack
;;;

(define-class <sack> () ; () = no super class
  ((routing   :init-value *sack-default-routing*)
   (log-level :init-value 2                      :init-keyword :log-level)
  ))

(define (sack-new . init-keywords)
  (let1 sack (apply make <sack> init-keywords)
    (set! *sack* sack)
    (set! log-level (ref sack 'log-level))
    sack))

(define-method sack-add-routing ((sack <sack>) rexp proc)
  (slot-set! sack 'routing
             (append (list (cons rexp proc)) 
                     (ref sack 'routing))))

(define-method sack-start! ((sack <sack>) nport log-path)
  (httpd-start nport log-path))

;; module end -------------------------------------------
(provide "sack")
