#lang racket

;  [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
; (define (read-str str)
; 	(display str))
;

(require racket/generic)

(define/contract (tokenize str)
  (-> string? list)
  (list))

(define (ast? v)
  (or (symbol? v) (list? v) (hash? v)))

(define-generics self
	[read-str self input]

	[read-form self]
	[peek self]
	[next self])

(struct token (variant data) #:transparent)

(define (variant? v)
	(set-member? (set "splice-unquote"
			  "special"
			  "double-quoted-string"
			  "comment"
			  "word") v))

(define/contract (make-token variant data)
  (-> variant? string? token?)
  (token variant data))

(struct reader-state (tokens [pos #:mutable])
	#:transparent
	#:methods gen:self [
		(define/contract (read-str self input)
			(-> struct? string? ast?)
			(list))
		; (define (read-list)
		;   (read-form)
		; read anything but parens?
		(define (read-form self)
			(define first-char (string-ref (peek self) 0))
			(match first-char
			  ["(" #t]
			  ; ["(" (read-list self)]
			  [_ #f]
			  )
			"hah"
		)
		(define (peek self)
			(list-ref	(reader-state-tokens self)
					(reader-state-pos self)))
		(define (next self)
			(set-reader-state-pos!	self
						(add1 (reader-state-pos self)))
			(peek self))])

; (provide reader-state read-str)
(provide reader-state read-str read-form make-token)
