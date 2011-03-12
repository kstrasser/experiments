#lang typed/racket

(require "../abstract-nonsense.rkt")

(scanl / 64 '(4 2 4))
(scanl max 5 '(1 2 3 4))
(scanl max 5 '(1 2 3 4 5 6 7))
(scanl (λ: ([x : Number] [y : Number]) (+ y (* 2 x))) 4 '(1 2 3))

(scanr + 5 '(1 2 3 4))
(scanr / 2 '(8 12 24 4))
(scanr max 18 '(3 6 12 4 55 11))
(scanr max 111 '(3 6 12 4 55 11))
(scanr (λ: ([x : Number] [y : Number]) (/ (+ x y) 2)) 54 '(12 4 10 6))

(foldr* (λ: ([x : Number] [y : Number]) (/ (+ x y) 2)) 54 '(12 4 10 6))

(foldl* (λ: ([x : Number] [y : Number]) (/ (+ x y) 2)) 54 '(12 4 10 6))
