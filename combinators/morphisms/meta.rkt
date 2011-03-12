#lang typed/racket

(require "util.rkt"
         "ana.rkt"
         "cata.rkt")

(provide (all-defined-out))

(: metafoldr (All (a b c) (b c -> c) c (Listof b) (AnaG a c) (AnaP c) -> (Listof a)))
(define (metafoldr f i as g p)
  (unfoldr g p (foldr f i as)))

(: metafoldl (All (a b c) (b c -> c) c (Listof b) (AnaG a c) (AnaP c) -> (Listof a)))
(define (metafoldl f i as g p)
  (unfoldl g p (foldl f i as)))