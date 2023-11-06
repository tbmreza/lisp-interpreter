#lang racket

(require racket/generic)

(define-generics envself
  [env-get   envself k]
  [env-set!  envself k v]
  [env-find  envself k])

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
             maybe-v]))])

  ; (define data  ; ?? packed args
  ;   (for/hash ([i  (in-naturals)]
  ;        [b  binds]
  ;        [e  exprs])
  ;   #:final (eq? b '&)
  ;   (match b
  ;     ; clojure-style packed arguments
  ;     ['&  (values (list-ref binds (add1 i))
  ;            (list-tail exprs i))]
  ;     [_   (values b e)])))
  ; (env data outer))

(define (split-to-pairs l)
  (cond [(empty? l)        empty]
        [(empty? (cdr l))  (list l)]
        [else              (cons (list (car l) (cadr l))
                                 (split-to-pairs (cddr l)))]))

(define (make-env #:outer outer #:binds binds)  ; ?? packed args
  (define kvs (split-to-pairs binds))

  (define data
    (let ([h (make-hash)])
      (define (mut-h kv) (hash-set! h (first kv) (second kv)))
      (map mut-h kvs) h))

  (Env data outer))

(define repl-env
  (let ([h (make-hash)])  ; ?? use make-env
    (hash-set! h '+ +) (hash-set! h '- -) (hash-set! h '* *)
    (Env h false)))

(provide (all-defined-out))
