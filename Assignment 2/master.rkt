#lang racket

(require "decision_functions.rkt")
(require "testdata.rkt")
(require 2htdp/batch-io)
(require math/flonum)

(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

 (define(sorted-data data f)
  (define(sorting-iter data f ans)
    (cond((null? data) ans)
         (else (let [(l (lc y : y <- data @(equal? (f (caar data)) (f (car y)))))]
                 (sorting-iter (remq* l data) f (append ans (list l)))))))
  (sorting-iter data f '()))

;input dataset
(provide toytrain)
(define toytrain "../data/toy_train.csv")

(provide titanictrain)
(define titanictrain "../data/titanic_train.csv")

(provide mushroomtrain)
(define mushroomtrain "../data/mushrooms_train.csv")

;output tree (dot file)
(provide toyout)
(define toyout "../output/toy-decision-tree.dot")

;reading input datasets
;read the csv file myfile as a list of strings
;with each line of the original file as an element of the list
;further split each line at commas
;so then we have a list of list of strings
(provide toy-raw)
(define toy-raw (cdr (read-csv-file toytrain)))

(provide titanic-raw)
(define titanic-raw (map cddr (cdr (read-csv-file titanictrain))))

(provide mushroom-raw)
(define mushroom-raw (cdr (read-csv-file mushroomtrain)))

;function to convert data to internal numerical format
;(features . result)
(provide format)
(define (format data) (let [(l (map string->number data))]
                        (cons (cdr l) (car l))))

;list of (features . result)
(provide toy)
(define toy (map format toy-raw))

(provide titanic)
(define titanic (map format titanic-raw))

(provide mushroom)
(define mushroom (map format mushroom-raw))

;============================================================================================================
;============================================================================================================
;============================================================================================================

;get fraction of result fields that are 1
;used to find probability value at leaf
(provide get-leaf-prob)
(define (get-leaf-prob data)
 (/ (length (filter (lambda (x) (= (cdr x) 1)) data)) (length data))
  )

;get entropy of dataset
(provide get-entropy)
(define (get-entropy data)
 (let [(p (/ (length (filter (lambda (x) (= (cdr x) 1)) data)) (exact->inexact (length data))))]
   (if(or (= p 0) (= p 1)) 0
   (abs (+ (* p (fllog2 p)) (* (- 1 p) (fllog2 (- 1 p)))))))
  )

;find the difference in entropy achieved
;by applying a decision function f to the data
(provide entropy-diff) 
(define (entropy-diff f data)
  (define (g x) (* (exact->inexact (/ (length x) (length data))) (get-entropy x)))
  (- (get-entropy data) (foldl + 0 (map g (sorted-data data f))))
  )

;choose the decision function that most reduces entropy of the data
(provide choose-f)
(define (choose-f candidates data) ; returns a decision function
 (argmax (lambda (x) (entropy-diff (cdr x) data)) candidates)
  )

(provide DTree)
(struct DTree (desc func kids))

;build a decision tree (depth limited) from the candidate decision functions and data
(provide build-tree)
(define (build-tree candidates data depth)
  (let [(p (get-leaf-prob data))]
    (cond((= p 1) (DTree "1" '() '()))
         ((= p 0) (DTree "0" '() '()))
         ((null? candidates) (DTree (~a p) '() '()))
         ((= depth 0) (DTree (~a p) '() '()))
         (else (let* [(f (choose-f candidates data))
                     (sd (sorted-data data (cdr f)))]
                 (DTree (car f) (cons (cdr f) (map (lambda (x) ((cdr f) (caar x))) sd))
                        (map (lambda (x) (build-tree (remove f candidates) x (- depth 1))) sd))))))
  )

;given a test data (features only), make a decision according to a decision tree
;returns probability of the test data being classified as 1
(provide make-decision)
(define (make-decision tree test)
                    (match tree [(DTree d f k)  (if(null? k) (string->number d) (let* [(i (index-of (cdr f) ((car f) test)))]
                 (make-decision (list-ref k i) test)))])
)

;============================================================================================================
;============================================================================================================
;============================================================================================================

;annotate list with indices
(define (pair-idx lst n)
  (if (empty? lst) `() (cons (cons (car lst) n) (pair-idx (cdr lst) (+ n 1))))
  )

;generate tree edges (parent to child) and recurse to generate sub trees
(define (dot-child children prefix tabs)
  (apply string-append
         (map (lambda (t)
                (string-append tabs
                               "r" prefix
                               "--"
                               "r" prefix "t" (~a (cdr t))
                               "[label=\"" (~a (cdr t)) "\"];" "\n"
                               (dot-helper (car t)
                                           (string-append prefix "t" (~a (cdr t)))
                                           (string-append tabs "\t")
                                           )
                               )
                ) children
                  )
         )
  )

;generate tree nodes and call function to generate edges
(define (dot-helper tree prefix tabs)
  (let* ([node (match tree [(DTree d f c) (cons d c)])]
         [d (car node)]
         [c (cdr node)])
    (string-append tabs
                   "r"
                   prefix
                   "[label=\"" d "\"];" "\n\n"
                   (dot-child (pair-idx c 0) prefix tabs)
                   )
    )
  )

;output tree (dot file)
(provide display-tree)
(define (display-tree tree outfile)
  (write-file outfile (string-append "graph \"decision-tree\" {" "\n"
                                     (dot-helper tree "" "\t")
                                     "}"
                                     )
              )
  )
;============================================================================================================
;============================================================================================================
;============================================================================================================

(define dotfile
  (display-tree (build-tree (list cshape csurf bruise odor gatch gspace gsize sshape nring pop hab) mushroom 8) toyout)
  
  )
