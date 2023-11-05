#lang racket

#| a lisp interpreter |#

(define (PRINT printable) 12)
(define (EVAL evalable) 12)
(define (READ evalable) 12)

(define (loop)
  (display "> ")
  (define input (read-line))
  (unless (eof-object? input)  ; input eof with <c-d>
    ((compose1 PRINT EVAL READ) input)
    (loop)))

(loop)
