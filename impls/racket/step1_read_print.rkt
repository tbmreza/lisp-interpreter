#lang racket

; (require "reader.rkt")

(define (READ str) str)
(define (EVAL str) str)
(define (PRINT str) str)
(define (rep input) (PRINT (EVAL (READ input))))

(define (loop)
	(display "user>")
	(define input (read-line))
	(unless (eof-object? input)
		(displayln (rep input))
		(loop)))

(loop)
