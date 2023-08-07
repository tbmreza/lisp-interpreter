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

(define (eval-ast ast env)
	; (-> ast? hash? ast?)
	(match ast
		[(? symbol? ast)	(env-get env ast)]
		[(? list? ast)		(map (lambda (a) (EVAL a env)) ast)]
		[_ ast]))

(define (do-apply f-args env)
	(define k (car f-args))
	(define f (env-get env k))
	(apply f (cdr f-args)))
(check-equal? 10 (do-apply '(- 12 2) repl-env))
(check-equal? 11 (do-apply (read-str "(- 12 1)") repl-env))

(define/contract (EVAL ast env)
	(-> ast? env? ast?)
	(match ast
		[(not (? list? _)) (eval-ast ast env)]
		[(? empty-ast? ast) ast]
		[_ (match (first ast)
		     ; def! let*
		     [_ (do-apply ast env)])]))

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
