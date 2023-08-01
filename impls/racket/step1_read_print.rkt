#lang racket

; PICKUP "env.rkt" https://github.com/kanaka/mal/blob/master/process/guide.md#step-3-environments
(require "reader.rkt")
(require "printer.rkt")

(define repl-env (make-hash))
(hash-set! repl-env "+" (lambda (a b) (+ a b)))
(hash-set! repl-env "-" (lambda (a b) (- a b)))
(hash-set! repl-env "*" (lambda (a b) (* a b)))
(hash-set! repl-env "/" (lambda (a b) (floor (/ a b))))

(define (ast? v)
	(or (symbol? v) (list? v) (hash? v)))

(define (empty-ast? ast)
	(match ast
		[(? list? ast) (empty? ast)]
		[(? hash? ast) (empty? (hash-keys ast))]
		[_ #f]))

(define/contract (READ str)
	(-> string? ast?)
	(define rs (reader-state (list (make-token "splice-unquote" "~@")
				       (make-token "special" "]")) 0))
	(read-str rs str))

(define/contract (eval-ast ast env)
  (-> ast? hash? ast?)
  (match ast
    ; mal-symbol?
    [(? list? ast)  (map (lambda (a) (EVAL a env)) ast)]
    [_ ast]))

(define/contract (EVAL ast env)
	(-> ast? hash? ast?)
	(match ast
		; [(? (not (list? ast))) (eval-ast ast env)]
		; [(? empty-ast? ast) ast]
		; [_ (apply ast)]))
		[(? list? ast) (list #\c)]))

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

;  [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
; (regexp-split #rx" +" "split pea     soup")
; (regexp-split #px"[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)" "split pea     soup")
