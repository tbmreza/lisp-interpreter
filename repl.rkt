#lang racket

(require "reader.rkt")
(require "env.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (READ input)
  (read-mal-syntax input))

(define variable-name? symbol?)

(define (interpret env ast)
  (match ast
    [(? variable-name? var)  (env-get env var)]
    [`(lambda ,_ ,_)         ast]

    [(? list? nodes)
     (map (lambda (node) (eval/p env node))
          nodes)]

    [value value]))

(define (eval/p env ast)
    (cond [(not (list? ast))  (interpret env ast)]
          [(empty? ast)       ast]
          [else
            (match (car ast)
              ['list  (interpret env (cdr ast))]

              ['def!
               (let ([k  (first (cdr ast))]
                     [v  (eval/p env (second (cdr ast)))])
                 (env-set! env k v) v)]

              ['let*
               (let* ([bindings  (second ast)]
                      [env+      (let*-env env bindings)]
                      [body      (third ast)])
                 (eval/p env+ body))]

              ['fn*
               (let* ([binding-vars  (second ast)]
                      [body          (third ast)]
                      [f             `(lambda ,(values binding-vars) ,body)])
                 ; Defer nesting env because binding-exprs is only available on fn* application.
                 (eval/p env f))]

              [_
                (match ast
                  [`(lambda ,_ ,_)  (eval ast ns)]
                  [_
                    (let* ([nodes  (interpret env ast)]
                           [proc   (car nodes)]
                           [args   (cdr nodes)])
                      (match proc
                        [`(lambda ,binding-vars ,body)
                          (eval/p (env-nest env binding-vars args) body)]
                        [_  (apply proc args)]))])])]))

(define EVAL
  ((curry eval/p) repl-env))

(define (PRINT ast)
  (println ast))  ; ?? readably is displayln, otherwise println

(define (loop)
  (display "user> ")
  (define input (read-line))
  (unless (eof-object? input)  ; input eof with <c-d>
    ((compose1 PRINT EVAL READ) input)
    (loop)))

(loop)
