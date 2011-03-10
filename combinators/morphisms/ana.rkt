#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

;; version for two types: ana a b :: b -> [a]
(define-type (AnaG a b) (b -> (values a b)))
(define-type (AnaP b) (b -> Boolean))
(define-type (ListAna a b) (b -> (Listof a)))

(: unfold (All (a b) (AnaG a b) (AnaP b) b -> (Listof a)))
(define (unfold g p x)
  (: unfold* (b (Listof a) -> (Listof a)))
  (define (unfold* x acc)
    (if (p x)
        acc
        (let-values ([(a y) (g x)])
          (unfold* y (cons a acc)))))
  (unfold* x '()))

;; version for three types: ana3 a b c :: b c -> [a]
(define-type (AnaG3 a b c) (b c -> (values a b c)))
(define-type (AnaP3 b c) (b c -> Boolean))
(define-type (ListAna3 a b c) (b c -> (Listof a)))

(: unfold3 (All (a b c) (AnaG3 a b c) (AnaP3 b c) b c -> (Listof a)))
(define (unfold3 g p x1 x2)
  (: unfold3* (b c (Listof a) -> (Listof a)))
  (define (unfold3* x1 x2 acc)
    (if (p x1 x2)
        acc
        (let-values ([(a y z) (g x1 x2)])
          (unfold3* y z (cons a acc)))))
  (unfold3* x1 x2 '()))