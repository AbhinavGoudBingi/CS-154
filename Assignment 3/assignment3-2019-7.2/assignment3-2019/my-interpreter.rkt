#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)

;The stack and its operations. I have decided to make the stack a global.
(define stack '())
(define (push frame) (set! stack (cons frame stack)))
(define (pop) (if (null? stack) (error "Empty stack")
                        (set! stack (cdr stack))))
(define (top) (if (null? stack) (error "Empty stack")
                        (car stack)))


;createframe creates a new frame. It gets its number from
;;framenumber and as a side effect increases framenumber
(define (createframe hashtable parent) ;hastable gives the initial bindings
   ...)

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
         (match prog
           [(pgm deflist) ...]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) ...]))

;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (hash-ref! (frame-bindings frame) 'main "main not found"))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) ...]
        [(boolean? exp) ...]
        [(number? exp) ...]
        [(list? exp) ...]
        [(string? exp)...]
        [else (match exp
                [(uexp op exp1) ...]
                [(bexp op exp1 exp2) ...]
                [(lam var _) ...]
                [(app exp1 explist)...]
                ...and so on, fill in these...
                [(debugexp) (begin
                              (print-current-environment (top))
                              )])]))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match expllist
   ...

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    ...

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  
  ...)

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
...
               


