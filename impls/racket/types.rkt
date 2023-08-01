#lang racket

; symbol is an object that contains a string name value
(define (ast? v)
	(or (symbol? v) (list? v) (hash? v)))

(provide ast?)
