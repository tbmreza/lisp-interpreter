#lang racket

(require racket/generic)

(define-generics envself
  [env-get envself k] [env-set! envself k v])

(struct Env ([data #:mutable])
  #:transparent
  #:methods gen:envself
  [(define (env-get envself k)
     (hash-ref (Env-data envself) k #f))

   (define (env-set! envself k v)
     (Env (hash-set! (Env-data envself) k v)))
   ])

(define repl-env
  (let ([h (make-hash)])
    (hash-set! h '+ +) (hash-set! h '- -) (hash-set! h '* *)
    (Env h)))

(provide (all-defined-out))
