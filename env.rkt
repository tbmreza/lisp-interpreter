#lang racket

(require racket/generic)

(define-generics envself
  [env-get   envself k]
  [env-set!  envself k v]
  [env-find  envself k]
  [env-nest  envself binding-vars binding-exprs])

(struct Env ([data #:mutable] outer)
  #:transparent
  #:methods gen:envself
  [(define (env-get envself k)
     (hash-ref (Env-data envself) k #f))

   (define (env-set! envself k v)
     (Env (hash-set! (Env-data envself) k v)
          (Env-outer envself)))

   (define (env-find envself k)
     (define maybe-v (hash-ref (Env-data envself) k #f))
     (cond [(not maybe-v)
             (match (Env-outer envself)
               [#f     false]
               [outer  (env-find outer k)])]
           [else
             maybe-v]))

   (define (env-nest envself binding-vars binding-exprs)
     (define data
       (for/hash ([i  (in-naturals)]
                  [v  binding-vars]
                  [e  binding-exprs])
         #:final (eq? v '&)  ; final evaluation before iteration breaks
         (match v
           ; packed rest arguments
           ['&  (values (list-ref binding-vars (add1 i))
                        (list-tail binding-exprs i))]
           [_   (values v e)])))
     (Env data envself))])

(define (split-to-pairs l)
  (cond [(empty? l)        empty]
        [(empty? (cdr l))  (list l)]
        [else              (cons (list (car l) (cadr l))
                                 (split-to-pairs (cddr l)))]))

(define (let*-env outer alternating)
  ; (define splitted (split-to-pairs alternating))  ; ??
  ; (env-nest outer (evens alternating) (odds alternating))
  (define data
    (let ([h (make-hash)])
      (define (mut-h kv) (hash-set! h (first kv) (second kv)))
      (map mut-h (split-to-pairs alternating)) h))
  (Env data outer))

(define/contract (read-file filepath) (-> string? string?)
  (read-line (open-input-file filepath)))
  ; (define close-me (open-input-file filepath))  ; ??
  ; (define s (read-line close-me))
  ; (close-input-port close-me) s)

(define repl-env
  (let ([core-module  (make-hash)])  ; ?? instantiate mutable hash without sets
    (define (add k proc) (hash-set! core-module k proc))

    (add '+ +) (add '- -) (add '* *)
    (add 'slurp read-file)

    (Env core-module false)))

(provide (all-defined-out))
