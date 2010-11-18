//
// test code (also used by browser_test.html)
//
p("BiwaScheme" + " version " + BiwaScheme.Version + ".git" + BiwaScheme.GitCommit)
//p(ev("(macroexpand-1 '(define-record-type point (fields xx)))").to_write());
//p(ev("(define-record-type point (fields xx))"));
//p(ev("(make-point 1)"));

ev("(define __rtd (make-record-type-descriptor (quote point) #f #f #f #f #((immutable xx)))) \
(define __cd (make-record-constructor-descriptor __rtd #f #f))\
(_define-record-type (quote point) __rtd __cd)");
ev(" (define make-point (record-constructor (_record-constructor-descriptor 'point)))");
ev(" (define point? (record-predicate (record-type-descriptor point))) (define point-xx (record-accessor (record-type-descriptor point) 0))");



//p(    ev("(let ((a 'a) (b 'b) (x 'x) (y 'y))  " +
//      "  (let-values (((a b) (values x y)) " +
//      "               ((x y) (values a b)))" +
//      "    (list a b x y)))                "))
//p(    ev("(let ((a 'a) (b 'b) (x 'x) (y 'y))  " +
//      "  (let*-values (((a b) (values x y)) " +
//      "               ((x y) (values a b)))" +
//      "    (list a b x y)))                "))
//
//p(    ev("(let-values (((a b) (values 1 2)) " +
//       "             ((c d) (values 3 4)))" +
//       "  (list a b c d))                 "))

//p(ev("(let-values (((a b) (values 1 2))) (print a b))"));
//p(ev("(macroexpand-1 '(let-values (((a b) (values 1 2))) (print a b)))"));

//p(ev("(case (car '(c d)) ((a e i o u) 'vowel) ((w y) 'semivowel) (else 'consonant))"));
//p("hoge");

    //p(ev("(define (compose f g) (lambda (args) (f (apply g args)))) ((compose sqrt *) 12 75)"));
//p(ev("(apply vector? '(#(1 2)))"));

//p(ev("(map + '(1 2 3) '(2 3 4))"));
    //p(ev("(let1 a 1 (for-all < '(1 2 3) '(2 3 4)))"))
//var intp = new BiwaScheme.Interpreter();
//p(ev("(define-macro (foo x y) `(+ ,x ,y)) (foo 8 9)"))
//p(Object.inspect(intp.evaluate("(define-macro (foo x) x) (foo 9)")))

//p(ev("(remq 'foo '(bar foo baz))"));
//p(ev("(write (map number->string '(1 2 3)))"));
//p(ev(" ((lambda () (set! a 3))) a"))
//p(ev("(define a 1) ((lambda () (set! a 3))) a"))

//p(ev("(let ((v (make-vector 5))) (for-each (lambda (i) (vector-set! v i (* i i))) '(0 1 2 3 4)) v)"))
//  p(ev(" (letrec* ((p (lambda (x) (+ 1 (q (- x 1))))) (q (lambda (y) (if (zero? y) 0 (+ 1 (p (- y 1)))))) (x (p 5)) (y x)) y)"))
//p(ev("((lambda (y x q p) (set! p (lambda (x) (+ 1 (q (- x 1))))) (set! q (lambda (y) (if (zero? y) 0 (+ 1 (p (- y 1)))))) (set! x (p 5)) (set! y x) y) #f #f #f #f)"))
//p(ev("(letrec ((even?  (lambda (n) (if (zero? n) #t (odd? (- n 1))))) (odd?   (lambda (n) (if (zero? n) #f (even? (- n 1)))))) (alert (even? 88)))  "));
//p(ev("(define *a* 1) (define (foo) (set! *a* 0))"));

//p(ev("(when #t 1 2)"))
//p(ev("(let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (list x y z)))"))
//p(ev("`(list ,(+ 1 2) 4)"))
//    p(ev("((lambda (x) (begin (set! x 3) (set! x 2) (+ 1 3) x)) 1)"))
    //p(ev("(let ((x '(a))) (eq? x x))"))
    //p(ev("((lambda (a) (let ((x 5) (y 2)) (set! y 1) (- x y a))) 1)"))
//ev("((lambda (x y) x) 1 2)")
//p(ev("(map (lambda (x) (+ x 1)) '(1 2 3)"));

//p(ev('(define k (call/cc (lambda (cc) cc))) (k 1)'))
//p(ev('(display (call/cc (lambda (cc) (set-handler! (getelem "console") "click" (lambda (e) (cc e))))))'))
//p(ev('(set-handler! (getelem "console") "click" (lambda (e) (+ 1 "a")))'))

//p(ev('((lambda (x) (call/cc (lambda (cc) (cc 1) (display "hoge")))  ) 3)'));
//p(ev('((lambda (x) ((lambda (y) x y) 2)   ) 3)'));

//p(ev('(timer (lambda () (display "hoge")) 1000)'));
//p(ev('((lambda (n k) (lambda () (display (+ n k)))) 10 7)'))
//p(ev("((lambda () 1))"))

//p(ev('(begin (display "a")(sleep 1000)(display "b"))'));
//p(ev("((lambda (a) (lambda () a)) 10)"));
//with(BiwaScheme){
//  var i=new Pair(true, new Pair(1, 2));
//  puts(to_scm(i));
//}
//p(ev("(define (len ls) (if (null? ls) 0 (+ 1 (len (cdr ls))))) (len '(1 2 3))"))
//p(ev("(define x 1) (define (f) x) (set! x 2) (f)"))
//p(ev("(define (l) (sleep 1000) (display 1) (l)) (l)"))
//p(ev("(define l #f) (set! l (lambda () (sleep 1) (l)))"))
//    ev("((lambda (x) (set! x 3) x) 4)").should_be(3);

//p(ev('(display "a")'));

//p(ev('((lambda (x y) x) 3 4)'));
//p(ev('(display ((lambda (x y) x) "Nice boat." 4))'));
//p(ev('((lambda (x y z) x) 1 2 3)'));
//p(ev('((lambda (x y) (x y)) (lambda (z) z) 99)'));
//ev('(display 99)');
//ev('display');
//p(ev("((lambda (x) (car x)) '(5 6 7)))"));
//p(ev('((lambda (x) ((lambda (y) x) 2)) 3)'))
//p(ev("((lambda (i) ((lambda (x) (i x)) '(5 6 7))) (lambda (y) y))"));
//p(ev('(begin (display "a") (sleep 1000) (display "b"))'))
//p(ev("(define (fact n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5)"))
//p(ev('(+ 1 2)'));
//p(ev('((lambda (x y) y) 99 100)'));
//p(ev('((lambda (x y) (+ x y)) 99 100)'));
//puts(typeof(typeof(window)));
//p(ev('(call/cc (lambda (cc) (display 1) (cc "Nice boat") 2))'));
//(((lambda (f) 
//   ((lambda (proc) (f (lambda (arg) ((proc proc) arg)))) 
//    (lambda (proc) (f (lambda (arg) ((proc proc) arg)))))) 
//  (lambda (self) 
//    (lambda (ls) (if (null? ls) 0 (+ 1 (self (cdr ls))))))) 
// '(1 2 3 4 5))

//p(ev("(((lambda (f) ((lambda (proc) (f (lambda (arg) ((proc proc) arg)))) (lambda (proc) (f (lambda (arg) ((proc proc) arg)))))) (lambda (self) (lambda (ls) (if (null? ls) 0 (+ 1 (self (cdr ls))))))) '(1 2 3 4 5))"))

//p(ev("(((lambda (f) ((lambda (proc) (f (lambda (arg1 arg2) ((proc proc) arg1 arg2)))) (lambda (proc) (f (lambda (arg1 arg2) ((proc proc) arg1 arg2)))))) (lambda (self) (lambda (ls acc) (if (null? ls) acc (self (cdr ls) (+ 1 acc)))))) '(1 2 3 4 5) 0)"))

/// "closure and saved env"
//p(ev("((lambda (x) (car x)) '(5 6 7)))"));
//p(ev("(car '(5 6 7))"));
//p(ev("(+ 5 6 7)"));
//p(ev("((lambda (x y) ((lambda (z) (* (car z) (cdr z))) (cons x y))) 3 4)"));

//p(ev("((lambda (x y) ((lambda (z) z) (car (cons x y)))) 1 2)"));
//p(ev("((lambda (x y) ((lambda (a b) a) x y)) 1 2)"))
//p(ev("((call/cc (lambda (cc) cc)) (lambda (x) 4))"))

//p(ev("((lambda (x) (set! x 3) x) 4)"))

//p(ev('((lambda () (begin (display "hoge") (display "Nice boat") (display "moge") "foo")))'));
//p(ev('(display "hoge") (display "Nice boat") (display "moge")'));
//p(ev("((lambda (x) (begin (set! x 3) (set! x 2) (+ 1 3) x)) 1)"))
//p(ev("(define a 1) (display (+ a 1))"))
//ev("(let ((x 1) (y 2)) (+ x y))")

//p(ev("((lambda (a) (let ((x 5) (y 2)) (set! y 1) (- x y a))) 1)"))
