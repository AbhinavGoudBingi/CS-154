#lang racket

;; You can require more modules of your choice.
(require racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in stats: "statistics.rkt"))

(define l (stats:cipher-common-words-single utils:cipher-word-list))
(define m (drop-right (stats:cipher-monograms utils:ciphertext) (- (length (stats:cipher-monograms utils:ciphertext)) 5)))
(define n (stats:arrange (stats:cipher-unique-neighbourhood (stats:cipher-bigrams utils:cipher-word-list) 'both)))
(define list-ai (if(= (length l) 2)
   (list (list (cons #\A (car (string->list (car l)))) (cons #\I (car (string->list (cadr l))))) (list (cons #\A (car (string->list (cadr l)))) (cons #\I (car (string->list (car l))))))
    (list (cons #\A (car l)) (cons #\I (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                     ;;
;; Strategies                                                                          ;;
;; ==========                                                                          ;;
;; For the purpose of this assignment, just the `etai` strategy is expected, since     ;;
;; we have dictionary-closure and secret-word-enumeration to leap-frog to the right    ;;
;; key. This technique would fail for harder keys which are arbitrary permutations of  ;;
;; the alphabet. We will be forced to explore many more strategies (along with         ;;
;; dictionary-closure of course).                                                      ;;
;;                                                                                     ;;
;; Strategies to guess substitutions for the key using statistical information about   ;;
;; - the English language from utils.rkt                                               ;;
;; - the cipher text      from statistics.rkt                                          ;;
;;                                                                                     ;;
;; Follow the function signature as indicated below. Deviations will make it           ;;
;; impossible for automatic grading of your submission.                                ;;
;; Moreover, we do not expect your strategies to require any more/different            ;;
;; arguments. Note that you recieve the key as argument, so you can at the very        ;;
;; least ensure that all the substitutions are monoalphabetic wrt this key.            ;;
;;                                                                                     ;;
;; Signature:                                                                          ;;
;; ```                                                                                 ;;
;; (define (my-fundoo-strategy key)                                                    ;;
;;   ;; Make use of `utils:ciphertext`, `utils:cipher-word-list`                       ;;
;;   ...)                                                                              ;;
;; ```                                                                                 ;;
;;                                                                                     ;;
;; Substitutions                                                                       ;;
;; -------------                                                                       ;;
;; In order to extend the key incrementally, we use `utils:add-substitution` to        ;;
;; extend a given key with a substitution.                                             ;;
;;                                                                                     ;;
;; A substitution is a list of pairs, each pair mapping a plaintext char to a          ;;
;; ciphertext char. For example, to extend the key with T -> a and O -> r              ;;
;; (simultaneously), we use the substitution:                                          ;;
;; ```                                                                                 ;;
;; (list (cons #\T #\a) (cons #\O #\r))                                                ;;
;; ```                                                                                 ;;
;; For a single substitution use a singleton list (containing just one pair).          ;;
;;                                                                                     ;;
;; **CAUTION**                                                                         ;;
;; -----------                                                                         ;;
;; 1. Note that add-substitution does not do sanity checks on the substitution and use ;;
;;    of `utils:is-monoalphabetic` is recommended to ensure that you don't             ;;
;;    inadvertently create invalid keys.                                               ;;
;; 2. You must provide a list called `compositions` in this module.                    ;;
;;                                                                                     ;;
;; See docs in "utils.rkt" for more information.                                       ;;
;;                                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You must add "public" functions of this module to this list.
(provide etai
         insert
         ;; Some more suggested strategies:
         
         ;; common-words-double
         ;; bigrams
         ;; common-initial-letters
         ;; common-final-letters
         ;; common-words-triple
         ;; trigrams
         ;; common-double-letters
         ;; common-words-quadruple
         ;; quadgrams
         
         ;; lists of strategies
         composition
         )
         

;; A strategy that uses some statistical information to generate potential
;; substitutions for E, T, A and I.
;; Refer the assignment manual for tips on developing this strategy. You can
;; interact with our etai with the executable we provide.

(define(perms lis ll)
  (if(null? lis) ll (perms (cdr lis) (append ll (permutations (car lis))))))

(define(insert a n l)
  (append (slice l 1 (- n 1)) (list a) (slice l n  (length l))))

(define(kth-element k l)
  (if(= k 1) (car l) (kth-element (- k 1) (cdr l))))
(define(slice l i k)
  (cond((> i k) '())
       (else(cons (kth-element i l) (slice l (+ i 1) k)))))

(define (etai key)     

  (define refine-m (cond((= (length l) 1) (remove (car (string->list (car l))) m))
                        ((= (length l) 2) (remove (car (string->list (car l))) (remove (car (string->list (cadr l))) m)))
                        ((= (length l) 0) m)))
(cond((= (length refine-m) 3)(let* ((dm (sort refine-m #:key (lambda (x) (index-of n x)) <))
                                   (a (car dm))
                                   (b (cadr dm))
                                   (c (remove b (remove a refine-m))))
               ;(define dm (sort refine-m #:key (lambda (x) (index-of n x)) <))
               (define lc1 (map (lambda (l) (append (list (cons #\E a)) (list l))) (map (lambda (x) (cons #\T x)) (cdr dm))))
               (define lc2 (map (lambda (l) (append (list l) (list (cons #\T a)))) (map (lambda (x) (cons #\E x)) (cdr dm))))
               (define lc (append lc1 lc2))
              (define f (map (lambda (y) (map (lambda (x) (append x y)) lc)) list-ai))
             (append (car f) (cadr f))))
     ((= (length refine-m) 5) (cond((= (length l) 0) (define(etai-iter ll llc)
                                  (if(null? ll ) llc (etai-iter (cdr ll) (append llc (list (map cons '(#\E #\T #\A #\I) (car ll)))))))
                                (etai-iter (perms (combinations refine-m 4) '()) '()))
                                   ((= (length l) 2) (define(etai-iter ll llc)
                                  (if(null? ll ) llc (etai-iter (cdr ll) (append llc (list (map cons '(#\E #\T) (car ll)))))))
                                        (define f (map (lambda (y) (map (lambda (x) (append x y)) (etai-iter (perms (combinations refine-m 2) '()) '()))) list-ai))
                                        (append (car f) (cadr f)))
                                   ((= (length l) 1) (define(etai-iter1 ll llc)
                                  (if(null? ll ) llc (etai-iter1 (cdr ll) (append llc (list (map cons '(#\E #\T #\I) (car ll)))))))
                                       (define part1 (map (lambda (x) (insert (car list-ai) 3 x)) (etai-iter1 (perms (combinations refine-m 3) '()) '())))

                                       (define(etai-iter2 ll llc)
                                  (if(null? ll ) llc (etai-iter2 (cdr ll) (append llc (list (map cons '(#\E #\T #\A) (car ll)))))))
                                       (define part2 (map (lambda (x) (append x (list (cdr list-ai)))) (etai-iter2 (perms (combinations refine-m 3) '()) '())))

                                       (append part1 part2))))
                                
      (else(cond((= (length l) 2) (define(etai-iter ll llc)
                                  (if(null? ll ) llc (etai-iter (cdr ll) (append llc (list (map cons '(#\E #\T) (car ll)))))))
                                        (define f (map (lambda (y) (map (lambda (x) (append x y)) (etai-iter (perms (combinations refine-m 2) '()) '()))) list-ai))
                                        (append (car f) (cadr f)))
                 ((= (length l) 1) (define(etai-iter1 ll llc)
                                  (if(null? ll ) llc (etai-iter1 (cdr ll) (append llc (list (map cons '(#\E #\T #\I) (car ll)))))))
                                       (define part1 (map (lambda (x) (insert (car list-ai) 3 x)) (etai-iter1 (perms (combinations refine-m 3) '()) '())))

                                       (define(etai-iter2 ll llc)
                                  (if(null? ll ) llc (etai-iter2 (cdr ll) (append llc (list (map cons '(#\E #\T #\A) (car ll)))))))
                                       (define part2 (map (lambda (x) (append x (list (cdr list-ai)))) (etai-iter2 (perms (combinations refine-m 3) '()) '())))

                                       (append part1 part2)))))) 
                

;; A suggested composition of strategies that might work well. Has not been
;; exhaustively tested by us. Be original ;)
(define composition (etai '()))
                  ;; common-words-double
                  ;; bigrams
                  ;; common-initial-letters
                  ;; common-final-letters
                  ;; common-words-triple
                  ;; trigrams
                  ;; common-double-letters))
                  ;; common-words-quadruple
                  ;; quadgrams))




