#lang racket

;candidate functions for the toy dataset
(provide y1)
(provide y2)
(provide y3)
(provide y4>62)

(define y1 (cons "feature1" (lambda (record) (list-ref record 0)))) ; returns the value of feature 1 for a given test sample
(define y2 (cons "feature2" (lambda (record) (list-ref record 1))))
(define y3 (cons "feature3" (lambda (record) (list-ref record 2))))
(define y4>62 (cons "feature4>62" (lambda (record) (if(> (list-ref record 3) 62) 1 0)))) ; returns 1 if the value of feature 4 > 62, else 0

;candidate functions for the titanic dataset
(provide pclass)
(provide sex)
(provide age>25)
(provide sibsp)
(provide parch)
(provide fare>50)
(provide emb)

(define pclass (cons "pclass" (lambda (record) (list-ref record 0)))) ; returns the value of pclass for a given test sample
(define sex (cons "sex" (lambda (record) (list-ref record 1))))
(define age>25 (cons "age>25" (lambda (record) (if(> (list-ref record 2) 25) 1 0))))
(define sibsp (cons "sibsp" (lambda (record) (list-ref record 3))))
(define parch (cons "parch" (lambda (record) (list-ref record 4))))
(define fare>50 (cons "fare>50" (lambda (record) (if(> (list-ref record 5) 50) 1 0))))
(define emb (cons "emb" (lambda (record) (list-ref record 6))))

;candidate functions for the mushroom dataset
(provide cshape)
(provide csurf)
(provide bruise)
(provide odor)
(provide gatch)
(provide gspace)
(provide gsize)
(provide sshape)
(provide nring)
(provide pop)
(provide hab)

(define cshape (cons "cshape" (lambda (record) (list-ref record 0))))
(define csurf (cons "csurf" (lambda (record) (list-ref record 1))))
(define bruise (cons "bruise" (lambda (record) (list-ref record 2))))
(define odor (cons "odor" (lambda (record) (list-ref record 3))))
(define gatch (cons "gatch" (lambda (record) (list-ref record 4))))
(define gspace (cons "gspace" (lambda (record) (list-ref record 5))))
(define gsize (cons "gsize" (lambda (record) (list-ref record 6))))
(define sshape (cons "sshape" (lambda (record) (list-ref record 7))))
(define nring (cons "nring" (lambda (record) (list-ref record 8))))
(define pop (cons "pop" (lambda (record) (list-ref record 9))))
(define hab (cons "hab" (lambda (record) (list-ref record 10))))
