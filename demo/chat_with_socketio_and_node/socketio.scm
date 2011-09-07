;; Cross-platform Socket.IO helper functions

;; socket.io operations have 0..N arguments followed by 0..1 callback functions
(define (socketio-serialize-args args)
  (if (null? args)
      '()
    (let* ((revargs (reverse args))
           (last (car revargs))
           (rest (cdr revargs)))
      (reverse
       (cons
        ;; if last arg is a procedure, wrap it. else, serialize it like everything else
        (if (procedure? last)
            (js-closure last)
          (socketio-serialize last))
        ;; serialize all the non-last args
        (map socketio-serialize rest))))))

(define (socketio-deserialize-args args)
  (if (null? args)
      '()
    (let* ((revargs (reverse args))
           (last (car revargs))
           (rest (cdr revargs)))
      (reverse
       (cons
        ;; if last arg is a js function, wrap it. else, deserialize it like everything else
        (if (procedure? last)
            (lambda args (apply js-call last args))
          (socketio-deserialize last))
        ;; deserialize all the non-last args
        (map socketio-deserialize rest))))))

(define socketio-serialize write-to-string)
(define (socketio-deserialize elem)
  (if (string? elem)
      (read-from-string elem)
    elem))

(define (socketio-emit target event . args)
  (apply js-invoke (append (list target "emit" event)
                           (socketio-serialize-args args))))

(define (socketio-on socket event callback)
  (js-invoke socket "on" event
             (js-closure (lambda args (apply callback (socketio-deserialize-args args))))))
