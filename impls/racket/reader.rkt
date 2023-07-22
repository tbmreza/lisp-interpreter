#lang racket

;  [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
; (define (read-str str)
; 	(display str))
;

(require racket/generic)

(define-generics self
	[next self]
	[peek self])

(struct reader-state (tokens [pos #:mutable])
	#:transparent
	#:methods gen:self [
		(define (peek self)
			(list-ref	(reader-state-tokens self)
					(reader-state-pos self)))
		(define (next self)
			(set-reader-state-pos!	self
						(add1 (reader-state-pos self)))
			(peek self))
		])

(define rs (reader-state (list 11 12 14) 0))
(next rs)
(next rs)

(provide reader-state)
