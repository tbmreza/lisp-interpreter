#lang racket

(require rackunit)

(require "reader.rkt")
(require "printer.rkt")
(require "env.rkt")
(require "types.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

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
	(define (eval-list ast) (map
		(lambda (a) (EVAL a #:env env))
		(match (first ast)
			['\(  (trim ast)]
			[_    ast])))

	(match ast
		[(? symbol? ast)  (env-get env ast "eval-ast")]
		[(? list? ast)    (eval-list ast)]
		[_ ast]))

(define (apply-head-on-rest ast env)
	(define h (car ast))
	(define args (cdr ast))

	(if (symbol? h) (apply (env-get env h "apply-head-on-rest") (eval-ast args env))
	                (apply (EVAL h #:env (make-env #:outer env
						       ; PICKUP magic binds and exprs, tests passing means these aren't used
						       #:binds (list 'a 'b)
						       #:exprs (list 21 23)))
			       args)))

; helper
(define (split-to-pairs l)
	(cond [(empty? l)        empty]
	      [(empty? (cdr l))  (list l)]
	      [else              (cons (list (car l) (cadr l))
				       (split-to-pairs (cddr l)))]))

(define (EVAL ast #:env env)
	; (-> ast? env? ast?)
	(define (def!-special)
		; 'def! is      (first ast)
		(define k       (second ast))
		(define v (EVAL (third ast) #:env env))
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
		(define (f p env)
			(env-set! env (first p)
				  (EVAL (second p) #:env env)))

		(EVAL (third ast)
		      #:env (foldl f env (split-to-pairs (second ast)))))

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
		(match (EVAL pred #:env env)
			[#t  (EVAL then #:env env)]
			[#f  (EVAL els #:env env)])))


	(define (fn*-special)
		(let* ([binds  (second ast)]
		       [body   (caddr ast)]
		       [f      `(lambda ,(values binds) ,body)])
		(eval f ns)))

	(match ast
		[(not (? list? _))   (eval-ast ast env)]
		[(? empty-ast? ast)  ast]

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
	; (pr-str expr #:print-readably))
	(pr-str expr))

(define (rep input)
	(define (repl-eval read-ast) (EVAL read-ast #:env repl-env))
	((compose1 PRINT repl-eval READ) input))

; The life of `repl-env:`
; - core.rkt defines map of procedures for env.rkt,
; - env.rkt exports `repl-env`,
; - core.mal then mutates it,
; - finally main EVAL uses it.
;
; core.mal
; prints #<procedure> somewhere along
(rep "(def! not (fn* (a) (if a false true)))")
(rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

(define (loop)
	(display "user> ")
	(define input (read-line))
	(unless (eof-object? input)
		(displayln (rep input))
		(loop)))

; (loop)
(define (test)
	(check-equal? (rep "(* 2 pi)") (number->string 6))
	(check-equal? (rep "(+ (- 11 2) 5)") (number->string 14))
	(check-equal? (rep "(def! c (+ (- 11 2) 5))") (number->string 14))
	(check-equal? (rep "(let* (c 2 d (+ 1 2)) (+ c d))") (number->string 5))
	(check-equal? (rep "(if false (+ 0 1) (+ 0 2))") (number->string 2))
	(check-equal? (rep "(if (not true) (+ 0 1) (+ 0 2))") (number->string 2))
	(check-equal? (rep "(if (= 11 11) (+ 0 1) (+ 0 2))") (number->string 1))
	(check-equal? (rep "((fn* (a) a) 22)") (number->string 22))
	(check-equal? (rep "((fn* (a) (* a 2)) 22)") (number->string 44))
	(check-equal? (rep "((fn* (aa bb) (+ aa bb)) 0 9)") (number->string 9))
	(check-equal? (rep "((fn* (x y z) (+ x y z)) 100 10 1)") (number->string 111))
	)
(test)

(define (test2)
	(define in (open-input-file "tests/one_plus_one.mal"))
	(check-equal? (rep (read-line in)) "2")
	(close-input-port in)

	)
(test2)
