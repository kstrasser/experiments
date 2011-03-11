#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

; h = {b, f}

; for Lists where l is (Listof a), = (cons a as)
; h Empty = b
; h l = (f a as (h as))

;(: para-list (All (a) a -> (Listof a)))
;(: tails (All (a) (Listof a) -> (Listof (Listof a))))

;fac = {1, f}
;(f n m) = (* m (* 1 n))

;h 0 = 1
;h n = (f (sub1 n) (h (sub1 n)))

(define-type (ParaH a) (a -> a))
(define-type (ParaF a b) (a a -> b))

(: para (All (a b) (ParaH a) (ParaF a b) a -> b))
(define (para h f b)
  (f b (h b)))

(: para/real ((ParaF Real Real) Real Real -> Real))
(define (para/real f n b)
  (: h (ParaH Real))
  (define/match (h n)
    [0 b]
    [_ (f (sub1 n) (h (sub1 n)))])
  (para h f n))