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

; helper
(define (split-to-pairs lst)
	(cond	[(empty? lst) empty]
		[(empty? (cdr lst)) (list lst)]
		[else (cons (list (car lst) (cadr lst)) (split-to-pairs (cddr lst)))]))

; (define/contract (EVAL ast env)
(define (EVAL ast env)
	; (-> ast? env? ast?)
	(define (def!-special)
		; 'def! is	(first ast)
		(define k	(second ast))
		(define v (EVAL (third ast) env))
		(env-set! env k v) v)

	; Example mal source:
	;
	; (let* ( key (first kvs)
	; 	  rst (rest kvs)
	; 	  val (first rst)
	; 	  acc (_foldr_pairs f init (rest rst)))
	; 	(f key val acc))
	;
	(define (let*-special)
		(define (f p env) (env-set! env (first p) (EVAL (second p) env)))

		; (third ast) is the body of this form, whose env is folded (second ast).
		(EVAL (third ast) (foldl f env (split-to-pairs (second ast)))))

	(match ast
		[(not (? list? _))	(eval-ast ast env)]
		[(? empty-ast? ast)	ast]
		[_ (match (first ast)
		     ['def!	(def!-special)]
		     ['let*	(let*-special)]
		     [_		(do-apply ast env)])]))

(check-equal? 10 (do-apply '(- 12 2) repl-env))
(check-equal? 11 (do-apply (read-str "(- 12 1)") repl-env))

; (define/contract (PRINT expr)
(define (PRINT expr)
	; (-> ast? string?)
	(pr-str expr))

(define (rep input) (PRINT (EVAL (READ input) repl-env)))

(define (loop)
	(display "user> ")
	(define input (read-line))
	(unless (eof-object? input)
		(displayln (rep input))
		(loop)))

; (loop)
(define (test)
	(displayln (rep "(* 2 pi)"))
	(displayln (rep "(+ (- 11 2) 5)"))
	(displayln (rep "(def! c (+ (- 11 2) 5))"))
	(displayln (rep "(let* (c 2 d (+ 1 2)) (+ c d))"))
	(displayln (rep "(let* (c 2 d c) (+ c d))"))
	)
(test)
