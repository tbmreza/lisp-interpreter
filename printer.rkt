#lang racket/base

(define (pr-str ast #:readably [readably #t])
  (cond [readably  (println ast)]
        [else      (displayln ast)]))

(provide (all-defined-out))
