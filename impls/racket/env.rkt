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

(define (maybe-struct? v) (or (struct? v) (void? v)))

(struct env ([data #:mutable] outer)
	#:transparent
	#:methods gen:self [
		(define/contract (env-set! self k v)
			(-> struct? symbol? env-value? struct?)
			(env-add! self k v) self)

		; (define/contract (env-find self k)
		(define (env-find self k)
			; (-> struct? symbol? struct?)
			(define has-k (hash-ref (env-data self) k  #f))
			(if has-k
				self
				(match (env-outer self)
					[#f  (void)]
					[o   (env-find o k)])))

		; (define/contract (env-get self k)
		(define (env-get self k)
			; (-> struct? symbol? env-value?)
			(define env-containing-k (env-find self k))
			(hash-ref (env-data env-containing-k) k))])

(define (maybe-hash? v) (or (hash? v) (not v)))
(define/contract (make-env outer binds exprs)
	(-> maybe-hash? list? list? struct?)
	(define data (hash))
	; else: data[binds[i]] = exprs[i]
	(env data outer))

; (define repl-env (make-env #f (second ast) (list)))
(define repl-env (make-env #f (list) (list)))
(env-add! repl-env 'pi 3)
(env-add! repl-env '+ (lambda (a b) (+ a b)))
(env-add! repl-env '- (lambda (a b) (- a b)))
(env-add! repl-env '* (lambda (a b) (* a b)))
(env-add! repl-env '/ (lambda (a b) (floor (/ a b))))
(env-add! repl-env 'id (lambda (a) a))

(check-true (env? (env-find repl-env '/)))
(check-equal? ((env-get repl-env '+) 2 4) 6)

(provide repl-env (struct-out env) env-get env-set! make-env)
