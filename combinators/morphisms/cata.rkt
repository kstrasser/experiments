#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

(define-type (CataFold a b) (a b -> b))
(define-type (Cata a b c) ((CataFold a b) b c -> b))

(: fold (All (a b) (Cata a b (Listof a))))
(define (fold f b as)
  (foldr f b as))

(: fold/reverse (All (a b) (Cata a b (Listof a))))
(define (fold/reverse f b as)
  (foldl f b as))

; friendly types, for writing functions whose implementations use fold lists
(define-type (ListCata a b) ((Listof a) -> b))
(define-type (ListCata* a b) (b (Listof a) -> b))