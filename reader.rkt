#lang racket/base

; ?? #lang mal  deferrable until this fails to lex a valid mal program
(define (read-mal-syntax input)
  (define (default-read-syntax input-port)
    (read-syntax #f input-port))

  ((compose1 syntax->datum
             default-read-syntax
             open-input-string) input))

(provide (all-defined-out))
