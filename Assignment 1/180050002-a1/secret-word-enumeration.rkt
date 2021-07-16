#lang racket

;; You can require more modules of your choice.
(require racket/string
         racket/list
         (prefix-in utils: "utils.rkt")
         (prefix-in dict-close: "dict-closure.rkt"))

(provide secret-word-enumeration)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                           ;;
;; Secret Word Enumeration                                                                   ;;
;; =======================                                                                   ;;
;;                                                                                           ;;
;; This step exploits the fact that all keys begin with a secret word that is a              ;;
;; proper SIX-LETTER word from the English dictionary.                                       ;;
;;                                                                                           ;;
;; Given a partial key, we can query the dictionary for a list of 6 letter words             ;;
;; that can potentially begin the key.                                                       ;;
;; We can then filter out potential candidates whose keys do not agree with our partial key. ;;
;;                                                                                           ;;
;; It is possible that given a (wrong) partial key, we discover that none of these           ;;
;; potential candidates can agree with our partial key. This really implies a                ;;
;; mistake in the choice of substitution, since dictionary-closure is completely             ;;
;; deterministic (modulo any bugs in your implementation :)                                  ;;
;;                                                                                           ;;
;; Hence, this function must return either of:                                               ;;
;; a. `#f` if there are no consistent candidates for this key.                               ;;
;; b. the original key if there are multiple consistent candidates.                          ;;
;; c. the complete key if there's only one consistent candidate!                             ;;
;;                                                                                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define(final-maker ls key1 key2 count)
    (cond((> count 1) key1)
         ((null? ls) (if(= count 1) key2 #f))
         (else(if(lcs2 key1 (utils:encryption-key (string-downcase (car ls)))) (final-maker (cdr ls) key1 (utils:encryption-key (string-downcase (car ls))) (+ count 1)) (final-maker (cdr ls) key1 key2 count))))) 

  (define (lcs1 lst1 lst2)
(define(lcs-iter ls1 ls2 i)
  (cond((= i (length ls1)) #t)
       (else(if(= (char->integer (list-ref ls1 i)) 95) (lcs-iter ls1 ls2 (+ i 1)) (if(equal? (char-upcase (list-ref ls1 i))  (list-ref ls2 i)) (lcs-iter ls1 ls2 (+ i 1))  
                                                                                         #f)))))
  (lcs-iter lst1 lst2 0))

  (define (lcs2 lst1 lst2)
(define(lcs-iter2 ls1 ls2 i)
  (cond((= i (length ls1)) #t)
       (else(if(equal? (list-ref ls1 i) (list-ref ls2 i)) (lcs-iter2 ls1 ls2 (+ i 1)) (if(= (char->integer (list-ref ls1 i)) 95) 
                                                                                        (lcs-iter2 ls1 ls2 (+ i 1)) #f)))))
  (lcs-iter2 lst1 lst2 0))

(define(match-words3 s)
 (define d (filter (lambda (x) (= (string-length x) (string-length s))) utils:dictionary))
  
  (define(mw3i s dic ls)
    (cond((null? dic) ls)
         (else(if(lcs1 (string->list s) (string->list (car dic))) (mw3i s (cdr dic) (append ls (list (car dic)))) (mw3i s (cdr dic) ls)))))
  (mw3i s d '()))

(define (secret-word-enumeration key-after-dictionary-closure)
  (cond((equal? key-after-dictionary-closure #f) #f);; Returns a key or false (#f)
       (else (define l (drop-right key-after-dictionary-closure 20))
  
    (cond((= (length (match-words3 (list->string l))) 1) (if(lcs2 key-after-dictionary-closure (utils:encryption-key (string-downcase (car (match-words3 (list->string l))))))
                                                            (utils:encryption-key (string-downcase (car (match-words3 (list->string l))))) #f))
         ((= (length (match-words3 (list->string l))) 0) #f)
         (else(final-maker (match-words3 (list->string l)) key-after-dictionary-closure '() 0))))))

  

  
  
  
