#lang racket

(require "reader.rkt")
(require "env.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (READ input)
  (read-mal-syntax input))

(define variable-name? symbol?)

(define (interpret env ast)
  (match ast
    [keyword
      #:when (member keyword `(true false))
      (eval keyword ns)]

    [keyword
      #:when (member keyword `(nil))
      (eval '(void) ns)]

    [(? variable-name? var)  (env-get env var)]
    [`(lambda ,_ ,_)         ast]

    [(? list? nodes)
     (map (lambda (node) (eval/p env node))
          nodes)]

    [value value]))

(define (eval/p env ast)
    (cond [(not (list? ast))  (interpret env ast)]
          [(empty? ast)       ast]
          [else
            (match (car ast)
              ['list  (interpret env (cdr ast))]

              ['def!
               (let ([k  (first (cdr ast))]
                     [v  (eval/p env (second (cdr ast)))])
                 (env-set! env k v) v)]

              ['let*
               (let* ([bindings  (second ast)]
                      [env+      (let*-env env bindings)]
                      [body      (third ast)])
                 (eval/p env+ body))]

              ['if
               (cond [(eval/p env (second ast))  (eval/p env (third ast))]
                     [else                       (eval/p env (fourth ast))])]

              ['do  (last (interpret env (cdr ast)))]

              ['fn*
               (let* ([binding-vars  (second ast)]
                      [body          (third ast)]
                      [f             `(lambda ,(values binding-vars) ,body)])
                 ; Defer nesting env because binding-exprs is only available on fn* application.
                 (eval/p env f))]

              [_
                (match ast
                  [`(lambda ,_ ,_)  (eval ast ns)]
                  [_
                    (let* ([nodes  (interpret env ast)]
                           [proc   (car nodes)]
                           [args   (cdr nodes)])
                      (match proc
                        [`(lambda ,binding-vars ,body)
                          (eval/p (env-nest env binding-vars args) body)]
                        [_  (apply proc args)]))])])]))

(define EVAL
  ((curry eval/p) repl-env))

(define (PRINT ast)
  ; ?? readably is displayln, otherwise println
  (println ast)) 
  ; (displayln ast))

(define rep (compose1 PRINT EVAL READ))

(define (silence go) (begin go (void)))  ; ?? inline in exec
(define exec (compose1 silence EVAL READ))

; core.mal {  ?? rep defs and repl is separate modules. repl.rkt reads core.mal line by line before it starts looping
(exec "(def! not (fn* (a) (if a #f #t)))")
; }

; Use racket's here string to sidestep escaping " and \ literals.
(exec #<<unnamed
(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))
unnamed
)

(define (loop)
  (display "user> ")
  (define input (read-line))
  (unless (eof-object? input)  ; input eof with <c-d>
    (rep input)
    (loop)))

(loop)

; (require readline/readline)
;
; (define (repl-loop)  ; ?? history ok, but panics on user break. replacing <cr> with \n is separate issue
;   (let ([line (readline "user> ")])
;     (add-history line)
;     ; (printf "~a~n" (rep line)))
;     (rep line))
;   (repl-loop))
;
; (repl-loop)
