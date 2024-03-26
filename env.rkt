#lang racket

(require racket/generic)
(require "reader.rkt")

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
               [#f     #false]
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

(define/contract (read-file filepath) (-> string? string?)
  (read-line (open-input-file filepath)))

;; swap! :: box? procedure? rest-args v
(define (swap! b p . rest-args)
  (define v
    (cond [(empty? rest-args)  (curry p (unbox b))]
          [else                (apply (curry p (unbox b)) rest-args)]))
  (set-box! b v) v)

(define repl-env
  (let ([core-module  (make-hash)])
    (define (procedure k proc) (hash-set! core-module k proc))

    (procedure '< <) (procedure '<= <=) (procedure '> >) (procedure '>= >=)
    (procedure '+ +) (procedure '- -) (procedure '* *) (procedure '/ /)

    (procedure 'str  (lambda es (string-join es "")))  ;; racket syntax for variadic es

    (procedure 'read-string  read-mal-syntax)
    (procedure 'slurp        read-file)  ;  slurp :: filepath -> string

    (procedure 'empty?  empty?)
    (procedure 'count   length)

    (procedure 'atom    box)
    (procedure 'atom?   box?)
    (procedure 'deref   unbox)
    (procedure 'reset!  set-box!)
    (procedure 'swap!   swap!)

    (Env core-module #false)))

(module+ test (require rackunit)
  (define (fn k) (env-get repl-env k))

  (check-equal?
    ((fn 'str) "hel" "lo")
    "hello")

  (define _doesn′t-panic
    ((fn 'read-string) "(+ 1 3)"))
; (define does-panic
;   ((fn 'unimplemented) "(+ 1 3)"))

  (check-equal?
    ((fn 'slurp) "input.mal")
    "12")

  (check-equal?
    ((fn 'read-string) ((fn 'str) "aa" "bb"))
    'aabb)

  ;; slurp |> str |> read-string
  (check-equal?
    ((fn 'read-string) ((fn 'str) "(do " ((fn 'slurp) "input.mal") "\nnil)"))
    '(do 12 nil))

  (check-equal?
    ((fn 'swap!) (box 12) (lambda (x) (+ x 1)))
    13)

  (check-equal?
    ((fn 'swap!) (box 12) (lambda (x y) (+ x y)) 4)
    16)

  )


(provide (all-defined-out))
