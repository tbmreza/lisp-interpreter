#lang racket

(require racket/generic)

(define-generics envself
  [env-get envself k])

(struct Env ([data #:mutable])
  #:transparent
  #:methods gen:envself
  [(define (env-get envself k)
     (hash-ref (Env-data envself) k #f))
   ; (define (env-find envself k))
   ])

(provide (all-defined-out))
