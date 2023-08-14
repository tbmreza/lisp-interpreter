#lang racket

(require rackunit)

(define ns (hash
	'pi  3
	'=   =
	'+   +
	'-   -
	'*   *))

(check-eq?
	((hash-ref ns '-) 111 10 1)
	100)

(provide ns)
