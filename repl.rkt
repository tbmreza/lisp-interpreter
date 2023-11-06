#lang racket

#| A lisp interpreter that aims to be a working (superset of) mal whose implementation isn't
   necessarily inspiring for translation from racket to non-language-oriented programming
   languages. ?? to main.rkt |#

(require "reader.rkt")
(require "env.rkt")

(define (READ input)
  (read-mal-syntax input))

(define variable-name? symbol?)

(define (interpret env ast)
  (match ast
    [(? variable-name? var)
     (env-get env var)]

    [(? list? nodes)
     (map (lambda (node) (eval env node))
          nodes)]

    [value value]))

(define (eval env ast)
    (cond [(not (list? ast))  (interpret env ast)]
          [(empty? ast)       ast]
          [else
            (match (car ast)
              ['def!
               (let ([k  (first (cdr ast))]
                     [v  (eval env (second (cdr ast)))])
                 (env-set! env k v) v)]

              ['let*
               (let* ([binds  (second ast)]
                      [env+   (make-env #:outer env #:binds binds)]
                      [body   (third ast)])
                 (eval env+ body))]

              [_
                (let* ([nodes  (interpret env ast)]
                       [proc   (car nodes)]
                       [args   (cdr nodes)])
                  (apply proc args))])]))

(define EVAL
  ((curry eval) repl-env))

(define (PRINT ast)
  (println ast))  ; ?? readably is displayln, otherwise println

(define (loop)
  (display "user> ")
  (define input (read-line))
  (unless (eof-object? input)  ; input eof with <c-d>
    ((compose1 PRINT EVAL READ) input)
    (loop)))

(loop)
