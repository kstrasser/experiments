#lang typed/racket

(require "ana.rkt"
         "cata.rkt")
(provide (all-defined-out))

(define-type (Hylo a c) (a -> c))

(: hylo (All (a b c) (CataFold a c) c (AnaG a b) (AnaP b) b -> c))
(define (hylo fold init g p x)
  
  (: hylo* (b c -> c))
  (define (hylo* x acc)
    (if (p x)
        acc
        (let-values ([(a y) (g x)])
          (hylo* y (fold a acc)))))
  
  (hylo* x init))