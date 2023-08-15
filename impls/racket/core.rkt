#lang racket

(require rackunit)

(require "reader.rkt")

(define/contract (read-file filepath)
	(-> string? string?)
	(define close-me (open-input-file filepath))
	(define s (read-line close-me))
	(close-input-port close-me) s)

(check-equal? (string-length (read-file "tests/a.mal")) 3)

(define ns (hash
	'read-string  read-str
	'slurp        read-file

	'pi  3
	'=   =
	'+   +
	'-   -
	'*   *))

(check-eq?
	((hash-ref ns '-) 111 10 1)
	100)

(provide ns)
