#lang racket

(require racket/generic)
(require "types.rkt")

(define pat
	(pregexp (string-append
		   "[\\s,]*"                   ; ws and comma not captured
		   "("                         ; begin capturing group
		   "~@"                        ; either special two-char ~@
		   "|[\\[\\]{}\\(\\)'`~^@]"    ; or special char []{}()'`~^@
		   "|\"(?:\\.|[^\\\"])*\"?"    ; or maybe-unclosed string
		   "|;.*"                      ; or comment
		   "|[^\\s\\[\\]{}('\"`,;)]*"  ; or word
		   ")"                         ; close capturing group
		   )))

(define/contract (tokenize str)
	(-> string? list?)
	(define tokens (regexp-match* pat str #:match-select cadr))
	(filter (lambda (x) (not (equal? x ""))) tokens))

(tokenize "~@  bool")
(tokenize "~@  ]")
(tokenize ",,(  + 2   (*  3  4)  )")

(define-generics self
	[read-str self input]

	[read-form self]
	[peek self]
	[next self])

(struct token (token-variant data) #:transparent)

(define (token-variant? v)
	(set-member? (set 'splice-unquote
			  'special
			  'double-quoted-string
			  'comment
			  'word) v))

(define/contract (make-token token-variant data)
  (-> token-variant? string? token?)
  (token token-variant data))

(struct reader-state (tokens [pos #:mutable])
	#:transparent
	#:methods gen:self [
		(define/contract (read-str self input)
			(-> struct? string? ast?)
			; (list 12 22))
			(hash-set (hash) "hh" "12"))
			; (hash))
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

(provide reader-state read-str read-form make-token)
