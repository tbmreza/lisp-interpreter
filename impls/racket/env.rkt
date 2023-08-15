#lang racket

(require "core.rkt")
(require rackunit)
(require racket/generic)

(define-generics self
	[env-set! self k v]
	[env-find self k caller]
	[env-get self k caller])

(define (env-value? n) (or (string? n) (procedure? n) (number? n)))

(define (env-add! e k v)
	(set-env-data! e (hash-set (env-data e) k v)))

; (define (maybe-struct? v) (or (struct? v) (void? v)))
(define (maybe-struct? v) (or (struct? v) (not v)))

(struct env ([data #:mutable] outer)
	#:transparent
	#:methods gen:self [
		(define/contract (env-set! self k v)
			(-> struct? symbol? env-value? struct?)
			(env-add! self k v) self)

		(define/contract (env-find self k caller)
			(-> struct? symbol? string? struct?)
			(define has-k (hash-ref (env-data self) k  #f))
			(if has-k
				self
				(match (env-outer self)
					[#f  (void)]
					[o   (env-find o k caller)])))

		(define/contract (env-get self k caller)
			(-> struct? symbol? string? env-value?)
			(define env-containing-k (env-find self k caller))
			(hash-ref (env-data env-containing-k) k))])

(define (maybe-hash? v) (or (hash? v) (not v)))
(define (symbols? v) (andmap symbol? v))

; exprs? list of terminals

(define/contract (make-env #:outer outer #:binds binds #:exprs exprs)
	(-> #:outer maybe-struct?  #:binds symbols?  #:exprs list?  struct?)
	(define data
		(for/hash ([i  (in-naturals)]
			   [b  binds]
			   [e  exprs])
		#:final (eq? b '&)
		(match b
			; clojure-style packed arguments
			['&  (values (list-ref binds (add1 i))
				     (list-tail exprs i))]
			[_   (values b e)])))

	(env data outer))

	; (define (associate p data)
	; 	(define b (car p))
	; 	(match b
	; 		; the b after this is bind to rest of exprs (as one list)
	; 		['&  (hash-set data b (cdr p))]
	; 		[_   (hash-set data b (cdr p))]))
	; (env (foldl associate (hash) (map cons binds exprs))
	;      outer))

(check-equal? (make-env #:outer #f #:binds (list) #:exprs (list))
	      (env (hash) #f))
(check-equal? (make-env #:outer #f #:binds (list 'a) #:exprs (list 21))
	      (env (hash 'a 21) #f))
(check-equal? (make-env #:outer #f #:binds (list 'a 'b) #:exprs (list 21 23))
	      (env (hash 'a 21 'b 23) #f))
(check-equal? (make-env #:outer #f #:binds (list '& 'args) #:exprs (list 11 22 33))
	      (env (hash 'args '(11 22 33)) #f))
(check-equal? (make-env #:outer #f #:binds (list 'age '& 'args) #:exprs (list 20 11 22 33))
	      (env (hash 'age 20 'args '(11 22 33)) #f))


(define repl-env (make-env #:outer #f #:binds (list) #:exprs (list)))
(for ([(k v) ns]) (env-add! repl-env k v))
(env-add! repl-env '/ (lambda (a b) (floor (/ a b))))
(env-add! repl-env 'id (lambda (a) a))

(check-true (env? (env-find repl-env '/ "")))
(check-equal? ((env-get repl-env '+ "") 2 4) 6)

(provide repl-env (struct-out env) env-get env-set! make-env)
