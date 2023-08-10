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

	; (define (b) (displayln "b..") 12)
	(match ast
		; [(? boolean? ast)  (b)]
		[(? symbol? ast)  (env-get env ast)]
		[(? list? ast)    (eval-list ast)]
		[_ ast]))

(define (apply-head-on-rest f-args env)
; (define (do-apply f-args env)
	(define f (env-get env (car f-args)))
	(define args (cdr f-args))
	(apply f (eval-ast args env)))

; helper
(define (split-to-pairs l)
	(cond	[(empty? l)        empty]
		[(empty? (cdr l))  (list l)]
		[else              (cons (list (car l) (cadr l))
		                         (split-to-pairs (cddr l)))]))

; (define/contract (EVAL ast env)
(define (EVAL ast env)
	; (-> ast? env? ast?)
	(define (def!-special)
		; 'def! is      (first ast)
		(define k       (second ast))
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

	; Example mal source:
	;
	; (def! present
	;   (fn* (slides)
	;     (if (> (count slides) 0)
	;       (do
	;         (println (clear))
	;
	;         (apply println (map (fn* (line) (str "\n        " line)) (first slides)))
	;         (println "\n\n\n")
	;         (readline "")
	;         (present (rest slides))))))
	(define (do-special)
		(eval-ast (rest ast) env))

	(define (if-special)
		(let ([pred  (second ast)]
		      [then  (third ast)]
		      [els   (fourth ast)])
		(match (EVAL pred env)
			[#t  (EVAL then env)]
			[#f  (EVAL els env)])))

	(define (fn*-special)
		(env-get env 'id))

	(match ast
		[(not (? list? _))	(eval-ast ast env)]
		[(? empty-ast? ast)	ast]

		[_ (match (first ast)
			['def!  (def!-special)]
			['let*  (let*-special)]
			['do    (do-special)]
			['if    (if-special)]
			['fn*   (fn*-special)]
			[_      (apply-head-on-rest ast env)])]))

(check-equal? 10 (apply-head-on-rest '(- 12 2) repl-env))
(check-equal? 11 (apply-head-on-rest (read-str "(- 12 1)") repl-env))

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
	(displayln (rep "(fn* (a) a)"))
	(displayln (rep "(if true (+ 0 1) (+ 0 2))"))
	(displayln (rep "(if false (+ 0 1) (+ 0 2))"))
	)
(test)
