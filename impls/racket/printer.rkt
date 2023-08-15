#lang racket

(require "types.rkt")

(provide pr-str)
; (define/contract (pr-str expr)
; (define (pr-str expr #:print-readably)
(define (pr-str expr)
	; (-> ast? string?)
	(match expr
		[(? number? expr)  (number->string expr)]
		[(? list? expr)    (foldl (lambda (e str) (string-append str (pr-str e))) "" expr)]
		[(? symbol? expr)  (symbol->string expr)]
		[expr expr]))

; (pr-str (list #\2 #\4))
; (pr-str (list 2 4 (list 88 77)))
