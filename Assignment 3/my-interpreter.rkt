#lang racket
(require racket/struct)
(provide (all-defined-out))
(require "defs.rkt")
(require "examples.rkt")

(define stacks (make-vector 100))
(define stacksindex 0)


;;Global definitions. A counter that tracks the framenumber
(define framenumber 0)
(define (singleton? l) (null? (cdr l)))

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
 (set! framenumber (+ framenumber 1)) (frame (- framenumber 1) hashtable parent)
  )

;This creates the global frame, which, in our case, contains
;empty bindings.
(push (createframe (make-hash '()) (emptyframe)))

;This interprets a program. It uses the following function processdef.
(define (eval-program prog)
  (define t (top))
         (match prog
          [(pgm deflist) (for ([x deflist])
                            (processdef x (car (begin (set! stack (cons t (remove t stack))) stack))))
                         (let [(main (return-value-of-main (msearch 'main)))]
                                (if (equal? main "main not found") (begin (hash-set*! (frame-bindings t) 'main "main not found")
                                                                                                            "main not found")
                                    main))]))

;;processdef describes how each definition is processed and added to
;;the frame fr.
(define (processdef defn fr)
  (match defn    
    [(def v/f exp) (hash-set*! (frame-bindings fr) v/f (eval-exp exp))]))
 
;; We have said that the result of the program is the value of
;; the symbol main. main must be a defined symbol in each program.
(define (return-value-of-main frame)
  (if(emptyframe? frame) "main not found" (hash-ref! (frame-bindings frame) 'main "main not found")))

;; The expression evaluator is the heart of the interpreter.
;; It will use the functions below
(define (eval-exp exp)
  (cond [(symbol? exp) (let [(sr (search exp (top)))]
                         (if(emptyframe? sr) (begin (displayln exp) (error "Symbol not found")) (hash-ref (frame-bindings sr) exp (lambda () (begin (displayln exp) (error "Symbol not found"))))))]
        [(boolean? exp) exp]
        [(number? exp) exp]
        [(list? exp) exp]
        [(string? exp) exp]
        [else (match exp
                [(uexp op exp1) (op (eval-exp exp1))]
                [(bexp op exp1 exp2) (op (eval-exp exp1) (eval-exp exp2))]
                [(lam var exp1) (closure exp (top))]
                [(app exp1 explist) (let [(ev (eval-exp exp1))]
                                      (if (closure? ev) (match ev [(closure lamb fram) (push (createframe (make-hash (map cons (lam-varlist lamb) (map eval-exp explist))) fram)) (let ((x (eval-exp (lam-exp lamb))))
                                                                                                                                                                             (begin (pop) x))])
                                        (error "Not a function")))]
                [(sett var exp1) (let [(x (search var (top)))]
                                   (if (equal? (emptyframe) x) (error "Symbol not found") (hash-set*! (frame-bindings (search var (top))) var (eval-exp exp1))))] 
                [(lett deflist exp1) (let* [(bind (make-hash))                                          
                                            (letf (createframe bind (top)))]
                                       (for ([i deflist]) (processdef i letf))
                                       (push letf)
                                       (let ((x (eval-exp exp1)))
                                         (begin (pop) x)))]                                                
                [(lets deflist exp1)  (cond ((null? deflist) (eval-exp exp1))
                                            ((singleton? deflist) (eval-exp (lett deflist exp1)))
                                            (else (eval-exp (lett (list (car deflist)) (lets (cdr deflist) exp1)))))]        
                [(beginexp explist) (process-beginexp explist)]
                [(iff con exp11 exp22) (if(eval-exp con) (eval-exp exp11) (eval-exp exp22))]
                [(defexp defl exp1) (begin (for [(i defl)] (processdef i (top))) (eval-exp exp1))]
                 [(debugexp) (begin
                 (vector-set! stacks stacksindex stack)
                 (set! stacksindex (+ 1 stacksindex)))])]))

;;An auxilliary function that processes a begin expression
(define (process-beginexp explist)
  (match explist
   ['() (void)] 
   [(list a ... b) (begin (for ([i a]) (eval-exp i)) (eval-exp b))]))

;;An auxilliary function that processes a let expression.
;;The let definitions are in deflist, and the let body is exp.
(define (process-lets deflist exp)
  (match deflist
    [(list a b ...) (if(null? b) (lett a exp) (lett (list a) (process-lets b exp)))]))

;;Prints the current environment running through a chain of frames.
;;Note that my struct definitions are such that if fr is a frame,
;;then (displayln fr) will print the frame in the format that I have
;;shown. 
(define (print-current-environment fr)
  (define (iter fr)
    (displayln "@@@@@@@@@@@@@@@@@@@@@@@")
    (displayln fr)
  (cond ((not (emptyframe? (frame-parent fr))) (iter (frame-parent fr)))))
  (iter fr)
  (displayln "@@@@@@@@@@@@@@@@@@@@@@@"))

;;Search for the symbol sym in an environment that starts with the frame
;;fr. We shall make search return either the  emptyframe
;;or the frame containing the symbol (note, not the value of the
;;symbol itself.

(define (search sym fr)
(cond ((emptyframe? fr) fr)
      ((hash-has-key? (frame-bindings fr) sym) fr)
      (else (search sym (frame-parent fr)))))

(define (msearch sym)
  (define (msearch-iter i)
    (cond((= i (length stack)) (emptyframe))
    (else (if(hash-has-key? (frame-bindings (list-ref stack i)) sym) (list-ref stack i) (msearch-iter (+ i 1))))))
  (msearch-iter 0))

(define (cleanup)
  (set!  stacks (make-vector 100))
  (set! stacksindex 0)
  (set! framenumber 0)
  (set! stack '())
  (push (createframe (make-hash '()) (emptyframe))))
       