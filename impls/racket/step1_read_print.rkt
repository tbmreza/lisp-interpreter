#lang racket

(require "reader.rkt")
(require "printer.rkt")
(require "env.rkt")
(require "types.rkt")

(define (empty-ast? ast)
	(match ast
		[(? list? ast) (empty? ast)]
		[(? hash? ast) (empty? (hash-keys ast))]
		[_ #f]))

(define/contract (READ str)
	(-> string? ast?)
	(define rs (reader-state (list (make-token 'splice-unquote "~@")
				       (make-token 'special "]")) 0))
	; (define rs (reader-state (list (make-token "splice-unquote" "~@")
	; 			       (make-token "special" "]")) 0))
	(read-str rs str))

(define/contract (eval-ast ast env)
	(-> ast? hash? ast?)
	(match ast
		; mal-symbol?
		[(? list? ast)  (map (lambda (a) (EVAL a env)) ast)]
		[_ ast]))

(define/contract (EVAL ast env)
	(-> ast? env? ast?)
	(match ast
		[(not (? list? ast)) (list #\a)]
		; [(not (? list? ast)) (eval-ast ast env)]
		[(? empty-ast? ast) ast]
		[_ (match (first ast)
		     [_ (list #\z)]
		     )]
		))

(define/contract (PRINT expr)
	(-> ast? string?)
	(pr-str expr))

(define (rep input) (PRINT (EVAL (READ input) repl-env)))

(define (loop)
	(display "user> ")
	(define input (read-line))
	(unless (eof-object? input)
		(displayln (rep input))
		(loop)))

; (loop)
