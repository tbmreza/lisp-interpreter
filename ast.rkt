#lang racket

(require racket/generic)

(define-generics self
  [ast-list? self] [if-pred self] [if-true self] [if-false self])

(struct Ast (data)
  #:transparent
  #:methods gen:self
  [(define (ast-list? self)
     (list? (Ast-data self)))
   ; (define (ast-empty? self) )  ; ?? hash can also be empty

   (define (if-pred self)
     (Ast (second (Ast-data self))))
   (define (if-true self)
     (Ast (third (Ast-data self))))
   (define (if-false self)
     (Ast (fourth (Ast-data self))))])

(provide (all-defined-out))
