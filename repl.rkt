#lang racket

#| A lisp interpreter that aims to be a working (superset of) mal whose implementation isn't
   necessarily inspiring for translation from racket to non-language-oriented programming
   languages. |#

(require "reader.rkt")
(require "ast.rkt")
(require "env.rkt")

(define (READ input)
  (Ast (read-mal-syntax input)))

(define variable-name? string?)

; (eval env '(if true 2 4))
(define (eval env ast)
    (cond [(not (ast-list? ast))  (interpret env ast)]
          ; [(ast-list? ast)  ; ?? step 8
          ;  (match (macroexpand env ast)
          ;    [(? (ast-list? expanded))  (interpret env ast)]
          ;    [(? ast-empty? expanded)   expanded])]
          [else
            (match (first (Ast-data ast))
              ['if  (cond [(eval env (if-pred ast))  (eval env (if-true ast))]
                          [else                      (eval env (if-false ast))])])]))

(define (interpret env ast)
  (match ast
    [(? variable-name? var)  (env-get env var)]
    [(? list? nodes)         (define (fn node) (eval env node))
                             (list)]
    ))

(define (PRINT ast)
  (void))

(define repl-env (Env (hash)))

(define EVAL
  ((curry eval) repl-env))

(define (loop)
  (display "> ")
  (define input (read-line))
  (unless (eof-object? input)  ; input eof with <c-d>
    ((compose1 PRINT EVAL READ) input)
    (loop)))

; (loop)
