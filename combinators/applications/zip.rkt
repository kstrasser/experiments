#lang typed/racket

(require "../abstract-nonsense.rkt")

(: zip (All (c d) (ListAna3 (Pair c d) (Listof c) (Listof d))))
(define (zip j k)
  
  (: g (AnaG3 (Pair c d) (Listof c) (Listof d)))
  (define (g j k)
    (values (pair (car j) (car k))
            (cdr j) 
            (cdr k)))
  
  (: p (AnaP3 (Listof c) (Listof d)))
  (define (p j k) 
    (or (empty? j) (empty? k)))
  
  (unfoldl3 g p j k))

(define n 100)
(define l1 (make-list n 1))
(define l2 (make-list n 2))

(timetest-suite "zip(2) implementations"
                (timetest " ...anamorphically"
                          (zip l1 l2) 'x)
                (timetest " ...with left fold"
                          (foldl (λ (i1 i2 l)
                                   (cons (cons i1 i2) l))
                                 '() l1 l2) 'x))