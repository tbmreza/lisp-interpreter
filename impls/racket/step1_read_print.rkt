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

(struct mal-func (ast params env fn) #:transparent)
; (define mf (mal-func 11 22 33 44))
; (displayln (mal-func-fn mf))

(define (eval-ast ast env)
	; if input hardcode, what is the f.fn (the final evaluation result)?
	(define (eval-list ast) (map
		(lambda (a) (EVAL a #:env env))
		(match (first ast)
			['\(  (trim ast)]
			[_    ast])))

	(match ast
		[(? symbol? ast)  (env-get env ast "eval-ast")]
		[(? list? ast)    (eval-list ast)]
		[_ ast]))

; helper
(define (split-to-pairs l)
	(cond [(empty? l)        empty]
	      [(empty? (cdr l))  (list l)]
	      [else              (cons (list (car l) (cadr l))
				       (split-to-pairs (cddr l)))]))

; h: e tidak direturn karena e dipakai along the way
; (define (tco ast #:env env)
; 	(define (let*-mkenv a e)
; 		...)
; 	(define (evall a e)
; 		ast
; 		; let*
; 		(evall (third a) (let*-mkenv e)))
; 	(evall ast e))

; (define (TCO ast #:env env)
; 	(define (def!-special)
; 		; 'def! is      (first ast)
; 		(define k       (second ast))
; 		(define v (EVAL (third ast) #:env env))
; 		(env-set! env k v) v)
;
; 	(display "before:")
; 	  (displayln ast)
; 	(let loop () (when #t
;        ; mutate TCO args in loop body
;        ; 2 ways: loop return args; reassignment of local var
; 	; (match ast
; 	; 	[(not (? list? _))   (eval-ast ast env)]
; 	; 	[(? empty-ast? ast)  ast]
;         ;
; 	; 	[_  (match (first ast)
; 	; 		['def!  (def!-special)]
; 	; 		['let*  (def!-special)])]
; 	; 	)
; 	(display "after:")
; 	  (displayln ast)
; 	(loop)12)))
; (TCO '(12) #:env #f)

; (define (def!-special a e)
; 	; 'def! is      (first a)
; 	(define k       (second a))
; 	(define v (EVAL (third a) #:env e))
; 	(env-set! e k v) v)
(define (if-special ast env)
	(let ([pred  (second ast)]
	      [then  (third ast)]
	      [els   (fourth ast)])
	(match (EVAL pred #:env env)
		[#t  (EVAL then #:env env)]
		[#f  (EVAL els #:env env)])))
; (define (fn*-special ast)
; 	(let* ([binds  (second ast)]
; 	       [body   (caddr ast)]
; 	       [f      `(lambda ,(values binds) ,body)])
; 	(eval f ns)))

(define (let*-special ast env)
	(define (f p env)
		(env-set! env (first p)
			  (EVAL (second p) #:env env)))
	(EVAL (third ast)
	      #:env (foldl f env (split-to-pairs (second ast)))))


(define (EVAL #:env env ast)
	(define (def!-sp a e)
		; 'def! is      (first a)
		(define k       (second a))
		(define v (EVAL (third a) #:env e))
		(env-set! e k v) v)
	(define (let*-sp ast env)
		(define (f p env)
			(env-set! env (first p)
			(EVAL (second p) #:env env)))
		(h (foldl f env (split-to-pairs (second ast)))
		   (third ast)))
	(define (if-sp ast env)
		; Prepare so that the next iteration concludes in the default
		; match arm.
		(let ([pred  (second ast)]
		      [then  (third ast)]
		      [els   (fourth ast)])
		(match (h env pred)
			[#t  (h env then)]
			[#f  (h env els)])))
	(define (fn*-special ast)
		(let* ([binds  (second ast)]
		       [body   (caddr ast)]
		       [f      `(lambda ,(values binds) ,body)])
		(eval f ns)))

	(define (apply-head-on-rest ast env)
		(define hd (car ast))
		(define rs (cdr ast))

		(define tco-evala (eval-ast ast env))

		; (define f (car tco-evala))
		(define f '(11 22 33 44))

		(define (handle-malfunc)
			(define mf (apply mal-func (values f)))
			; (define mf (mal-func (third ast) (second ast) env original-function-value))
			; PICKUP test case
			(h env (mal-func-fn mf)))

		(cond [(equal? (length f) (procedure-arity mal-func)) (handle-malfunc)]
		      [#f  (displayln "apply...")])

		(define args (cdr tco-evala))

		(define (parse fn)
			(define fn-env (make-env #:outer env #:binds (list) #:exprs (list)))
			(apply (EVAL fn #:env fn-env) rs))

		(define evala (eval-ast rs env))
		(match hd
			[(? symbol? hd)  (apply (env-get env hd "apply-head-on-rest") evala)]
			[fn             (parse fn)]))

	(define (h env ast)
		(match ast
			[(not (? list? _))   (eval-ast ast env)]
			[(? empty-ast? ast)  ast]

			[_ (match (first ast)
				['def!  (def!-sp ast env)]
				['let*  (let*-sp ast env)]
				['do    (last (eval-ast (rest ast) env))]
				['if    (if-sp ast env)]
				['fn*   (fn*-special ast)]
				[_      (apply-head-on-rest ast env)])]))
	(h env ast))
	; (match ast
	; 	[(not (? list? _))   (eval-ast ast env)]
	; 	[(? empty-ast? ast)  ast]
        ;
	; 	[_ (match (first ast)
	; 		['def!  (def!-special ast env)]
	; 		['let*  (let*-special ast env)]
	; 		['do    (do-special ast env)]
	; 		['if    (if-special ast env)]
	; 		['fn*   (fn*-special ast)]
	; 		[_      (apply-head-on-rest ast env)])]))

; (define (EVAL ast #:env env)
; 	; (-> ast? env? ast?)
; 	; (define (def!-special a e)
; 	; 	; 'def! is      (first a)
; 	; 	(define k       (second a))
; 	; 	(define v (EVAL (third a) #:env e))
; 	; 	(env-set! env k v) v)
; 	; (define (def!-special)
; 	; 	; 'def! is      (first ast)
; 	; 	(define k       (second ast))
; 	; 	(define v (EVAL (third ast) #:env env))
; 	; 	(env-set! env k v) v)
;
; 	; Example mal source:
; 	;
; 	; (let* ( key (first kvs)
; 	; 	  rst (rest kvs)
; 	; 	  val (first rst)
; 	; 	  acc (_foldr_pairs f init (rest rst)))
; 	; 	(f key val acc))
; 	;
; 	; (define (let*-special)
; 	; 	(define (f p env)
; 	; 		(env-set! env (first p)
; 	; 			  (EVAL (second p) #:env env)))
;         ;
; 	; 	(EVAL (third ast)
; 	; 	      #:env (foldl f env (split-to-pairs (second ast)))))
;
; 	; Example mal source:
; 	;
; 	; (def! present
; 	;   (fn* (slides)
; 	;     (if (> (count slides) 0)
; 	;       (do
; 	;         (println (clear))
; 	;
; 	;         (apply println (map (fn* (line) (str "\n        " line)) (first slides)))
; 	;         (println "\n\n\n")
; 	;         (readline "")
; 	;         (present (rest slides))))))
; 	; (define (do-special)
; 	; 	(eval-ast (rest ast) env))
;
; 	; (define (if-special)
; 	; 	(let ([pred  (second ast)]
; 	; 	      [then  (third ast)]
; 	; 	      [els   (fourth ast)])
; 	; 	(match (EVAL pred #:env env)
; 	; 		[#t  (EVAL then #:env env)]
; 	; 		[#f  (EVAL els #:env env)])))
;
;
; 	; (define (fn*-special)
; 	; 	(let* ([binds  (second ast)]
; 	; 	       [body   (caddr ast)]
; 	; 	       [f      `(lambda ,(values binds) ,body)])
; 	; 	(eval f ns)))
;
; 	(define (aa)
; 		(eval-ast ast env))
;
; 	(define (bb)
; 		ast)
;
; 	(define (cc)
; 		(apply-head-on-rest ast env))
;
; 	(match ast
; 		; [(not (? list? _))   (eval-ast ast env)]
; 		[(not (? list? _))   (aa)]
; 		; [(? empty-ast? ast)  ast]
; 		[(? empty-ast? ast)  (bb)]
;
; 		[_ (match (first ast)
; 			['def!  (def!-special ast env)]
; 			['let*  (let*-special ast env)]
; 			['do    (do-special ast env)]
; 			['if    (if-special ast env)]
; 			['fn*   (fn*-special ast)]
; 			; [_      (apply-head-on-rest ast env)])]))
; 			[_      (cc)])]))

; (check-equal? 10 (apply-head-on-rest '(- 12 2) repl-env))
; (check-equal? 11 (apply-head-on-rest (read-str "(- 12 1)") repl-env))

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
; (rep "(def! not (fn* (a) (if a false true)))")
; (rep "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))")

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
	; (check-equal? (rep "(if (not true) (+ 0 1) (+ 0 2))") (number->string 2))
	(check-equal? (rep "(if (= 11 11) (+ 0 1) (+ 0 2))") (number->string 1))
	(check-equal? (rep "((fn* (a) a) 22)") (number->string 22))
	(check-equal? (rep "((fn* (a) (* a 2)) 22)") (number->string 44))
	(check-equal? (rep "((fn* (aa bb) (+ aa bb)) 0 9)") (number->string 9))
	(check-equal? (rep "((fn* (x y z) (+ x y z)) 100 10 1)") (number->string 111))
	(check-equal? (rep "(do (/ 5 10) (* 12 2))") (number->string 24))
	)
(test)

(define (test2)
	(define in (open-input-file "tests/one_plus_one.mal"))
	(check-equal? (rep (read-line in)) "2")
	(close-input-port in)

	)
(test2)
