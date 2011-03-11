#lang typed/racket

(require "../abstract-nonsense.rkt")

(: factg (AnaG Real Real))
(define (factg m) (values m (sub1 m)))

(: factp (AnaP Real))
(define factp zero?)

(: factana (ListAna Real Real))
(define (factana n)
  (unfoldl factg factp n))

(: hsum (Hylo Real Real))
(define (hsum n)
  (hylo + 0 factg factp n))

(: psum (Real -> Real))
(define (psum n)
  (para/real + n 0))

(: sum* (Real -> Real))
(define (sum* n)
  (: sum** (Real Real -> Real))
  (define (sum** n acc)
    (cond [(zero? n) acc]
          [else (sum** (sub1 n) (+ n acc))]))
  (sum** n 0))

(define n 500000)

(timetest-suite
 (format "sum(~a)" n)
 (timetest " ...hylomorphically" (hsum n))
 (timetest " ...paramorphically" (psum n))
 (timetest " ...with separate anamorphism and left fold" (foldl + 0 (factana n)))
 (timetest " ...with primitive tail recursion" (sum* n)))