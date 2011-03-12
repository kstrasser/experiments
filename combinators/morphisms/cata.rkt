#lang typed/racket

(require "util.rkt"
         "hylo.rkt"
         "ana.rkt")

(provide (all-defined-out))

(: foldl* (All (a b) (a b -> b) b (Listof a) -> b))
(define (foldl* f b as)
  (: g (AnaG a (Listof a)))
  (define (g as) (values (car as) (cdr as)))
  (hylo f b g empty? as))

(: foldr* (All (a b) (a b -> b) b (Listof a) -> b))
(define (foldr* f b as)
  (foldl* f b (reverse as)))

; friendly types, for writing functions whose implementations use fold lists
(define-type (ListCata a b) ((Listof a) -> b))
(define-type (ListCata* a b) (b (Listof a) -> b))

(: scanl (All (a b) (a b -> a) a (Listof b) -> (Listof a)))
(define (scanl f a bs)
  
  (: p (AnaP3 a (Listof b)))
  (define (p a bs) (empty? bs))
  
  (: g (a (Listof b) -> (values a a (Listof b))))
  (define (g a bs)
    (let ([r (f a (car bs))])
      (values r r (cdr bs))))
  
  (cons a (unfoldr3 g p a bs)))

(: scanr (All (a b) (a b -> b) b (Listof a) -> (Listof b)))
(define (scanr f b as)
  
  (: p (AnaP3 b (Listof a)))
  (define (p b as) (empty? as))
  
  (: g (b (Listof a) -> (values b b (Listof a))))
  (define (g b as)
    (let ([r (f (car as) b)])
      (values r r (cdr as))))
  
  (append (unfoldl3 g p b (reverse as)) (list b)))