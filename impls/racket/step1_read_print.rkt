#lang racket

(require rackunit)

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
	(read-str str))

(define (trim ls) (drop-right (cdr ls) 1))

(define (eval-ast ast env)
	; (-> ast? hash? ast?)
	; contract?: list includes open and close parens
	(define (eval-list ast)
		(map	(lambda (a) (EVAL a env))
			(match (first ast)
				['\(	(trim ast)]
				[_	ast])))

	(match ast
		[(? symbol? ast)	(env-get env ast)]
		[(? list? ast)		(eval-list ast)]
		[_ ast]))

(define (do-apply f-args env)
	(define f (env-get env (car f-args)))
	(define args (cdr f-args))
	(apply f (eval-ast args env)))

(define/contract (EVAL ast env)
	(-> ast? env? ast?)
	(define (f) (eval-ast ast env))
	(match ast
		[(not (? list? _))	(f)]
		[(? empty-ast? ast)	ast]
		[_ (match (first ast)
		     ; def! let*
		     [_ (do-apply ast env)])]))

(check-equal? 10 (do-apply '(- 12 2) repl-env))
(check-equal? 11 (do-apply (read-str "(- 12 1)") repl-env))

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

(loop)
; (define (test)
; 	(displayln (rep "(* 2 pi)"))
; 	(displayln (rep "(+ (- 11 2) 5)"))
; 	)
; (test)
