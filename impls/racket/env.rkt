#lang racket

(require rackunit)
(require racket/generic)

(define-generics self
	[env-set! self k v]
	[env-find self k]
	[env-get self k])

(define (env-value? n) (or (procedure? n) (number? n)))

(define (env-add! e k v)
	(set-env-data! e (hash-set (env-data e) k v)))

(struct env ([data #:mutable])
	#:transparent
	#:methods gen:self [
		(define/contract (env-set! self k v)
			(-> struct? symbol? env-value? struct?)
			(env-add! self k v)
			self)

		(define/contract (env-find self k)
			(-> struct? symbol? struct?)
			(define has-k (not (hash-ref (env-data self) k  #f)))
			; find outer
			(if has-k self (env (hash)))
			self)

		(define/contract (env-get self k)
			; (-> struct? symbol? procedure?)
			(-> struct? symbol? env-value?)
			(define env-containing-k (env-find self k))
			(hash-ref (env-data env-containing-k) k))])

(define/contract (make-env outer)
  (-> env? env?)
  (env (hash)))

(define repl-env (make-env (env 12)))
(env-add! repl-env 'pi 3)
(env-add! repl-env '+ (lambda (a b) (+ a b)))
(env-add! repl-env '- (lambda (a b) (- a b)))
(env-add! repl-env '* (lambda (a b) (* a b)))
(env-add! repl-env '/ (lambda (a b) (floor (/ a b))))
(check-true (env? (env-find repl-env '/)))
(check-equal? ((env-get repl-env '+) 2 4) 6)

(provide repl-env (struct-out env) env-get env-set!)
