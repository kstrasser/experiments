#lang typed/racket

(require/typed "untyped.rkt"
               [pair (All (a b) a b -> (Pair a b))])
; pair is like cons, but always preserves specificity of types for pairs, 
; so that (pair (Listof a) (Listof b)) => (Pair (Listof a) (Listof b)) 
; rather than (cons (Listof a) (Listof b)) => (Listof (U b (Listof a))

(provide (all-defined-out)
         (all-from-out "untyped.rkt"))

(define-syntax-rule (timetest str act ...)
  (begin str
         (time act ...)
         (collect-garbage)))

(define-syntax-rule (timetest-suite str test ...)
  (begin (collect-garbage)
         str
         test ...))

;; Following two macros borrowed from Matt Might's article, 
;; http://matt.might.net/articles/red-black-delete/
(define-syntax define/match 
  (syntax-rules ()
    [(_ (id name) clause ...)
     ; =>
     (define (id name)
       (match name clause ...))]))

(define-syntax define/match*
  (syntax-rules ()
    [(_ (id name ...) clause ...)
     ; =>
     (define (id name ...)
       (match* (name ...)
               clause ...))]))