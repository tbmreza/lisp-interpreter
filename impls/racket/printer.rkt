#lang racket

(define (ast? v)
	(or (symbol? v) (list? v) (hash? v)))

(provide pr-str)
(define/contract (pr-str expr)
	(-> ast? string?)
	(match expr
		[(? number? expr) (number->string expr)]
		[(? list? expr) (foldl (lambda (e str) (string-append str (pr-str e))) "" expr)]
		[_ (string expr)]))

; (pr-str (list #\2 #\4))
; (pr-str (list 2 4 (list 88 77)))
