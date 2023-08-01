#lang racket

(require racket/generic)
(define-generics self
	[env-set self k v]
	[env-find self k]
	[env-get self k])

(struct env ([data #:mutable])
	#:transparent
	#:methods gen:self [
		(define/contract (env-set self k v)
			(-> struct? string? procedure? void?)
			(set-env-data! self (hash-set (env-data self) k v)))

		(define/contract (env-find self k)
			(-> struct? string? struct?)
			(define has-k (not (hash-ref (env-data self) k  #f)))
			; find outer
			(if has-k self (env (hash)))
			self)

		(define/contract (env-get self k)
			(-> struct? string? procedure?)
			(define env-containing-k (env-find self k))
			(hash-ref (env-data env-containing-k) k))])

(define/contract (make-env outer)
  (-> env? env?)
  (env (hash)))

(define repl-env (make-env (env 12)))
(env-set repl-env "+" (lambda (a b) (+ a b)))
(env-set repl-env "-" (lambda (a b) (- a b)))
(env-set repl-env "*" (lambda (a b) (* a b)))
(env-set repl-env "/" (lambda (a b) (floor (/ a b))))
; (env-find repl-env "/")
; ((env-get repl-env "/") 15 2)
; ((env-get repl-env "*") 15 2)

(provide repl-env (struct-out env))
