#lang racket

(require rackunit)
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

(check-equal? 9 (length (tokenize ",,(  + 2   (*  3  4)  )")))

(define-generics self
	[read-form self]
	[peek self]
	[next self])

(define (first-is-doublequote? v) (equal? (string-ref v 0) #\"))
(define (string-content v)
	(substring v 1 (sub1 (string-length v))))

(define/contract (read-atom v)
	(-> string? scalar?)
	(match v
		["nil"    (void)]
		["true"   #t]
		["false"  #f]

		[(? first-is-doublequote? v)  (string-content v)]
		[_ (match (string->number v)
			[#f      (string->symbol v)]
			[parsed  parsed])]))


(define/contract (read-str input)
	(-> string? ast?)
	(define rs (reader-state (tokenize input) 0))
	(read-form rs))

; "repeatedly call `read_form` with the Reader object until it
; encounters a ')' token"
; contract: pos is open paren
; -> list of tokens between parens
(define (read-list rs)
	(define (h rs acc)
		(next rs)
		(define (f)
			(append (list (read-form rs))
				(h rs acc)))
		(match (peek rs)
			[")"  acc]
			[_    (f)]))
	(h rs '()))

(struct reader-state (tokens [pos #:mutable])
	#:transparent
	#:methods gen:self [
		(define/contract (read-form self)
			(-> struct? ast?)
			(define current-token (peek self))
			(match (string-ref current-token 0)
				[#\(  (read-list self)]
				[_    (read-atom (peek self))])
			)

		(define (peek self)
			(list-ref (reader-state-tokens self)
				  (reader-state-pos self)))

		(define (next self)
			(set-reader-state-pos! self
					       (add1 (reader-state-pos self)))
			(peek self))])

(check-equal? (read-str ",,(+ 15 16)") '(+ 15 16))

(provide reader-state read-str tokenize)

(check-equal?
	(read-list (reader-state '("(" "+" "16" "12" ")") 0))
	'(+ 16 12))

(check-equal?
	(read-list (reader-state '("(" "+" "(" "+" "16" "12" ")" "11" ")") 0))
	'(+ (+ 16 12) 11))
