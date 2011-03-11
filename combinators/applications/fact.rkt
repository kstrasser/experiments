#lang typed/racket

(require "../abstract-nonsense.rkt")

(: factg (AnaG Real Real))
(define (factg m) (values m (sub1 m)))

(: factp (AnaP Real))
(define factp zero?)

(: factana (ListAna Real Real))
(define (factana n)
  (unfoldl factg factp n))

(: hfact (Hylo Real Real))
(define (hfact n)
  (hylo * 1 factg factp n))

(: pfact (Real -> Real))
(define (pfact n)
  (: f (ParaF Real Real))
  (define/match* (f n m)
    [(0 _) 1]
    [(_ _) (* n m)])
  (para/real f n 1))

(: fact* (Real -> Real))
(define (fact* n)
  (: fact** (Real Real -> Real))
  (define (fact** n acc)
    (cond [(zero? n) acc]
          [else (fact** (sub1 n) (* n acc))]))
  (fact** n 1))

(define n 6000)

(timetest-suite
 (format "factorial(~a)" n)
 (timetest " ...hylomorphically" (hfact n) 'x)
 (timetest " ...paramorphically" (pfact n) 'x)
 (timetest " ...with separate anamorphism and left fold" (foldl * 1 (factana n)) 'x)
 (timetest " ...with primitive tail recursion" (fact* n) 'x))