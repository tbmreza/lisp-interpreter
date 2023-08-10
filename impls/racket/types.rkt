#lang racket

(define (scalar? v)
	; (or (number? v) (symbol? v)))
	(or (number? v) (symbol? v) (boolean? v)))

(define (ast? v)
	(or (scalar? v) (list? v) (hash? v)))

(provide scalar? ast?)
