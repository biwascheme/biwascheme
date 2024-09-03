import { ev, ew } from "../helper.js"

describe('4.2.8 Quasiquotation', () => {
  test('simple', () => {
    ew("`(list ,(+ 1 2) 4)").toBe("(list 3 4)");
  })
  test('binding', () => {
    ew("(let ((name 'a)) `(list ,name ',name))").toBe("(list a (quote a))"); 
  })
  test('splicing', () => {
    ew("`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)").toBe("(a 3 4 5 6 b)");
  })
  test('improper list', () => {
    ew("`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))").toBe("((foo 7) . cons)");
  })
  test('nested', () => {
    ew("`(a b `(c d))").toBe("(a b (quasiquote (c d)))");
  })
  test('nested + splicing', () => {
    ew("(let1 ls '(4) `(1 `(2 ,(3 ,@ls))))")
      .toBe("(1 (quasiquote (2 (unquote (3 4)))))");
  })
  test('vector quasiquotation', () => {
    ew("`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)").toBe("#(10 5 2 4 3 8)");
  })
  test('vector (nested)', () => {
    ew("(let1 ls '(4) `#(1 `#(2 ,#(3 ,@ls))))")
      .toBe("#(1 (quasiquote #(2 (unquote #(3 4)))))");
  })
  test('not a list', () => {
    ew("`(,1 ,@2)").toBe("(1 . 2)");
  })
  test('bug #346', () => {
    ew("(define l '(x y)) ``(,@,@l ,@,@l)").toBe(
      "(quasiquote ((unquote-splicing x y) (unquote-splicing x y)))");
    ew("(define x '(1 2)) \
        (define y '(3 4)) \
        (quasiquote ((unquote-splicing x y) (unquote-splicing x y)))")
    .toBe("(1 2 3 4 1 2 3 4)");
  })
})

describe('6.6 Characters', () => {
  test('char<?', function(){
    ev("(char<? #\\a #\\z)").toBe(true);
    ev("(char<? #\\z #\\Z)").toBe(false);
  })
})

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
