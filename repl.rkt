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
              ['list  (interpret env (cdr ast))]

              ['def!
               (let ([k  (first (cdr ast))]
                     [v  (eval env (second (cdr ast)))])
                 (env-set! env k v) v)]

              ['let*
               (let* ([binds  (second ast)]
                      [env+   (make-env #:outer env #:binds binds)]
                      [body   (third ast)])
                 (eval env+ body))]

              ; ( (fn* (a b) (+ a b)) 2 3)
  ; (define (fn*-special ast)
  ;   (let* ([binds  (second ast)]
  ;          [body   (caddr ast)]
  ;          [f      `(lambda ,(values binds) ,body)])
  ;   (eval f ns)))

  ; PICKUP skip to tco fn*
              ; ['fn*
              ;  (let* ([binds  (second ast)]
              ;         [env+   (make-env #:outer env #:binds binds)]
              ;         [clo    (make-env #:outer env #:binds binds)]
              ;         ; (eval env+ body))]
              ;         [body   (third ast)])
              ;    (eval env+ binds)

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
