#lang typed/racket

(require "../abstract-nonsense.rkt")

(: factg (AnaG Real Real))
(define (factg m) (values m (sub1 m)))

(: factp (AnaP Real))
(define factp zero?)

(: factana (ListAna Real Real))
(define (factana n)
  (unfold factg factp n))

(: fact (Hylo Real Real))
(define (fact n)
  (hylo * 1 factg factp n))

(: sum (Hylo Real Real))
(define (sum n)
  (hylo + 0 factg factp n))

(: fact* (Real -> Real))
(define (fact* n)
  (: fact** (Real Real -> Real))
  (define (fact** n acc)
    (cond [(zero? n) acc]
          [else (fact** (sub1 n) (* n acc))]))
  (fact** n 1))

(: sum* (Real -> Real))
(define (sum* n)
  (: sum** (Real Real -> Real))
  (define (sum** n acc)
    (cond [(zero? n) acc]
          [else (sum** (sub1 n) (+ n acc))]))
  (sum** n 0))

(define n 5000)

(timetest-suite "factorials: they age like fine wine"
                (timetest " ...hylomorphically"
                          (fact n) 'x)
                (timetest " ...with separate anamorphism and left fold"
                          (foldl * 1 (factana n)) 'x)
                (timetest " ...with primitive tail recursion"
                          (fact* n) 'x))

(timetest-suite "_ ways to sum"
                (timetest " ...hylomorphically" 
                          (sum n))
                (timetest " ...with separate anamorphism and left fold"
                          (foldl + 0 (factana n)))
                (timetest " ...with primitive tail recursion"
                          (sum* n)))