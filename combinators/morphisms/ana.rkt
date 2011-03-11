#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

;; version for two types: ana a b :: b -> m a
(define-type (AnaG a b) (b -> (values a b)))
(define-type (AnaP b) (b -> Boolean))
(define-type (Ana a b ca) ((AnaG a b) (AnaP b) b -> ca))

; anas where m = List
;(: unfoldr (All (a b) (Ana a b (Listof a))))
(: unfoldr (All (a b) (b -> (values a b)) (b -> Boolean) b -> (Listof a)))
(define (unfoldr g p x)
  (if (p x)
      '()
      (let-values ([(a y) (g x)])
        (cons a (unfoldr g p y)))))

(: unfoldl (All (a b) (Ana a b (Listof a))))
(define (unfoldl g p x)
  (: unfoldl* (b (Listof a) -> (Listof a)))
  (define (unfoldl* x acc)
    (if (p x)
        acc
        (let-values ([(a y) (g x)])
          (unfoldl* y (cons a acc)))))
  (unfoldl* x '()))

; type for use in describing specific list anas.
(define-type (ListAna a b) (b -> (Listof a)))

;; --------------------------------------------

;; version for three types: ana3 a b c :: b c -> m a.
(define-type (AnaG3 a b c) (b c -> (values a b c)))
(define-type (AnaP3 b c) (b c -> Boolean))
(define-type (Ana3 a b c ca) ((AnaG3 a b c) (AnaP3 b c) b c -> ca))

; anas where m = List
(: unfoldr3 (All (a b c) (Ana3 a b c (Listof a))))
(define (unfoldr3 g p x1 x2)
  (if (p x1 x2)
      '()
      (let-values ([(a y z) (g x1 x2)])
        (cons a (unfoldr3 g p y z)))))

(: unfoldl3 (All (a b c) (Ana3 a b c (Listof a))))
(define (unfoldl3 g p x1 x2)
  (: unfoldl3* (b c (Listof a) -> (Listof a)))
  (define (unfoldl3* x1 x2 acc)
    (if (p x1 x2)
        acc
        (let-values ([(a y z) (g x1 x2)])
          (unfoldl3* y z (cons a acc)))))
  (unfoldl3* x1 x2 '()))

; type for use in describing specific list ana3s.
(define-type (ListAna3 a b c) (b c -> (Listof a)))

;;; notes on future macro-abstraction for anaN where N = Num >= 2
;(define-ana id N ...) =>
;(define (idrN g p x ...)
;  (if (p x ...)
;      '()
;      (let-values ([(a y ...) (g x ...)])
;        (cons a (idrN g p y ...)))))
;(define (idlN g p x ...)
;  (define (f x ... acc)
;    (if (p x ...)
;        acc
;        (let-values ([(a y ...) (g x...)])
;          (f y ... (cons a acc)))))
;  (f x ... '()))
#|
(define-for-syntax st->sy string->symbol)
(define-for-syntax nm->st number->string)
(define-for-syntax sy->st symbol->string)
(define-for-syntax s->d syntax->datum)
(define-for-syntax d->s datum->syntax)

(define-syntax (define-anas stx)
  (syntax-case stx ()
    [(_ id N id1 ...)
     (with-syntax ([(arglist ...) (generate-temporaries #'(id1 ...))]
                   [(vallist ...) (generate-temporaries #'(id1 ...))]
                   [idrN (d->s #'id (st->sy (string-append (sy->st (s->d #'id)) "r" (nm->st (s->d #'N)))))])
       #'(define idrN N))]))

(define-anas unfold 4 a b c d)
unfoldr4
|#