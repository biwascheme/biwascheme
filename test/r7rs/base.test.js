import { ev } from "../helper.js"

describe('11.5 Equivalence predicates', () => {
  test('eqv?', function(){
    ev("(eqv? 'a 'a)").toBe(true);
    ev("(eqv? 'a 'b)").toBe(false);
    ev("(eqv? 2 2)").toBe(true);
    ev("(eqv? '() '())").toBe(true);
    ev("(eqv? 100000000 100000000)").toBe(true)
    ev("(eqv? (cons 1 2) (cons 1 2))").toBe(false)
    ev("(eqv? (lambda () 1) (lambda () 2))").toBe(false);
    ev("(eqv? #f 'nil)").toBe(false);
  });
  test('eq?', function(){
    ev("(eq? 'a 'a)").toBe(true);
    ev("(eq? (list 'a) (list 'a))").toBe(false);
    ev("(eq? '() '())").toBe(true);
    ev("(eq? car car)").toBe(true);
    ev("(let ((x '(a))) (eq? x x))").toBe(true)
  });
  test('equal?', function(){
    ev("(equal? 'a 'a)").toBe(true);
    ev("(equal? '(a) '(a))").toBe(true);
    ev("(equal? '(a (b) c) '(a (b) c))").toBe(true);
    ev('(equal? "abc" "abc")').toBe(true);
    ev("(equal? 2 2)").toBe(true);
    ev("(equal? (make-vector 5 'a) (make-vector 5 'a))").toBe(true);
    //ev("(equal? '#vu8(1 2 3 4 5)
    //        (u8-list->bytevector
    //         '(1 2 3 4 5))                  ⇒  #t
    ev(`(let* ((x (list 'a)) (y (list 'a)) (z (list x y)))
          (vector (equal? z (list y x))
          (equal? z (list x x))))`).toEqual([true,true]);
  });
})
