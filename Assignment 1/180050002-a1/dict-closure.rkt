#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt")
         (prefix-in strats: "strategies.rkt")
         (prefix-in stats: "statistics.rkt"))

(provide  dictionary-closure
         lcs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                  ;;
;; Dictionary Closure                                                               ;;
;; ==================                                                               ;;
;;                                                                                  ;;
;; A choice of substitution can really trigger more substitutions by looking at the ;;
;; partially decrypted text - surely there will be some words which can be uniquely ;;
;; determined using the dictionary. It is prudent to concretize this "choice" as    ;;
;; this purely deterministic (involving absolutely no guess-work). In more          ;;
;; technical terms, this is called "maintaining arc-consistency" (look it up on     ;;
;; Wikipedia).                                                                      ;;
;;                                                                                  ;;
;; This function must utilise the dictionary and the cipher-word-list. Decrypt each ;;
;; word (`utils:decrypt`) and check if the dictionary has:                          ;;
;;                                                                                  ;;
;; 1. a unique completetion!                                                        ;;
;;    - Extend your key using the information with this match. Continue exploring   ;;
;;      the words under the extended key.                                           ;;
;; 2. many completions.                                                             ;;
;;    - Do nothing, just continue exploring the words under the same key. If none   ;;
;;      of the words fall into (1), return with the key that you have built so far. ;;
;; 3. no completions!                                                               ;;
;;    - Return `#f` (false), indicating that this partial key is wrong, and we must ;;
;;      revert to the original key.                                                 ;;
;;                                                                                  ;;
;; Returns either of:                                                               ;;
;; a. a (possibly) extended-key.                                                    ;;
;; b. `#f` if this key led to case (3) for some word.                               ;;
;;                                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dictionary-closure key)
 (define(improve-iter key i)
(let* ((decrypted-word-list
         (regexp-match* (pregexp "\\w+") (utils:sanitize-text (utils:decrypt key (utils:sanitize-text utils:ciphertext)))))
       (l (length decrypted-word-list)))
  (cond((= i l) key)
  (else 
(let*  ((pdw  (list-ref decrypted-word-list i)))
       (cond((= 0 (match-words3 pdw)) #f)
            ((and (check pdw) (= (match-words3 pdw) 1)) 
             (if(utils:is-monoalphabetic? (remove-duplicates (sub-improve (string->list pdw) '() 0)) key)
                                                           
                                                                       (improve-iter (utils:add-substitution (remove-duplicates (sub-improve (string->list pdw) '() 0)) key) 0)
                                                                       #f))
            (else(improve-iter key (+ i 1)))))))))
(improve-iter key 0))

(define(d s) (filter (lambda (x) (= (string-length x) (string-length s))) utils:dictionary))

(define(match-words3 s)
  
  (define(mw3i s dic count)
    (cond((null? dic) count)
         (else
          (if(check s)
          
          (if(lcs (string->list s) (string->list (car dic)))
             (if(utils:is-monoalphabetic? (remove-duplicates (sub-improve2 (string->list s) (string->list (car dic)) '() 0)) (build-list 26 (lambda (_) #\_)))
                                                                    (mw3i s (cdr dic) (+ count 1)) (mw3i s (cdr dic) count))
             (mw3i s (cdr dic) count))
          
          (if(lcs (string->list s) (string->list (car dic))) 2 (mw3i s (cdr dic) count))))))
                
  (mw3i s (d s) 0))

(define(mmwi3 s)
  
  (define(mmwii3 s dic)
  (if(and (utils:is-monoalphabetic? (remove-duplicates (sub-improve2 (string->list s) (string->list (car dic)) '() 0)) (build-list 26 (lambda (_) #\_)))
     (lcs (string->list s) (string->list (car dic)))) (car dic) (mmwii3 s (cdr dic))))
 (mmwii3 s (d s)))

(define (lcs lst1 lst2)
(define(lcs-iter ls1 ls2 i)
  (cond((= i (length ls1)) #t)
       (else(if(equal? (list-ref ls1 i) (list-ref ls2 i)) (lcs-iter ls1 ls2 (+ i 1)) (if(and (<= (char->integer (list-ref ls1 i)) 122) (>= (char->integer (list-ref ls1 i)) 97))
                                                                                        (lcs-iter ls1 ls2 (+ i 1)) #f)))))
  (lcs-iter lst1 lst2 0))

(define(check s)
  (not(equal? (string-upcase s) s)))

(define(filter2 ls2 i fl)
  (cond((= i (length ls2)) fl)
       (else(if(check (list-ref ls2 i)) (filter2 ls2  (+ i 1) (append (list (list-ref ls2 i)) fl)) (filter2 ls2 (+ i 1) fl)))))

  (define(sub-improve l al i)
    (cond((= i (length l)) al)
         (else(if(and (<= (char->integer (list-ref l i)) 122) (>= (char->integer (list-ref l i)) 97)) (sub-improve l (append al (list (cons (list-ref (string->list (mmwi3 (list->string l))) i) (list-ref l i)))) (+ i 1))
                 (sub-improve l al (+ i 1))))))

;  (define(complete-improve ls lal i)
;    (cond((= i (length ls)) lal)
;         (else(complete-improve ls (append lal (sub-improve (string->list (list-ref ls i)) '() 0)) (+ i 1)))))


;(define(improve key)
;;  (displayln key)
;;  (let* ((decrypted-word-list
;;  (regexp-match* (pregexp "\\w+") (utils:sanitize-text (utils:decrypt key (utils:sanitize-text utils:ciphertext)))))
;;         (l
;;;        (check-case (length (filter (lambda (x) (= (match-words3 x) 0)) decrypted-word-list)))
;;;        (work-case (filter2 (filter (lambda (x) (= (match-words3 x) 1)) decrypted-word-list) 0 '()))
;;;        (lc (utils:add-substitution (complete-improve work-case '() 0) key)))
;;  
;;;  (cond((< 0 check-case) #f)
;;;       ((< 0 (length work-case))
;;;         (improve lc))
;;;       
;;;       (else key))))
;;
;(define(improve-iter key i)
;(let* ((decrypted-word-list
;         (regexp-match* (pregexp "\\w+") (utils:sanitize-text (utils:decrypt key (utils:sanitize-text utils:ciphertext)))))
;       (l (length decrypted-word-list)))
;  (cond((= i l) key)
;  (else
;(let*  ((pdw  (list-ref decrypted-word-list i)))
;       (cond((= 0 (match-words3 pdw)) #f)
;            ((and (check pdw) (= (match-words3 pdw) 1)) (if(utils:is-monoalphabetic? (sub-improve (string->list pdw) '() 0) key)
;                                                                       (improve-iter (utils:add-substitution (sub-improve (string->list pdw) '() 0) key) 0)
;                                                                       #f))
;            (else(improve-iter key (+ i 1)))))))))
;(improve-iter key 0))

(define(kth-element k l)
  (if(= k 1) (car l) (kth-element (- k 1) (cdr l))))

(define(filter1 l k)
   (define(filter2 m)
      (cond((= m 0) (filter1 l (- k 1))) 
           ((equal? (list-ref l (- k 1)) (list-ref l (- m 1))) (filter1 (remove (list-ref l (- k 1)) l) (- k 1)))
           (else(filter2 (- m 1)))))
  (cond((= k 1) l)
       (else(filter2 (- k 1)))))
(define(remove-duplicates l)
  
  (filter1 l (length l)))


 (define(sub-improve2 l dwl al i)
    (cond((= i (length l)) al)
         (else(if(and (<= (char->integer (list-ref l i)) 122) (>= (char->integer (list-ref l i)) 97)) (sub-improve2 l dwl (append al (list (cons (list-ref dwl i) (list-ref l i)))) (+ i 1))
                 (sub-improve2 l dwl al (+ i 1))))))

            
             
             
             


  
  
  