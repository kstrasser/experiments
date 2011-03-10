#lang typed/racket

(require "util.rkt")

(provide (all-defined-out))

(define-type (CataFold a c) (a c -> c))