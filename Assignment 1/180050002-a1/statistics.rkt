#lang racket/base

;; You can require more modules of your choice.
(require racket/list
         racket/string
         (prefix-in utils: "utils.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                                 ;;
;; Ciphertext Statistics                                                           ;;
;; =====================                                                           ;;
;;                                                                                 ;;
;; The module should provide a bunch of functions that do statistical analysis of  ;;
;; ciphertext. The output is almost always just an ordered list (counts are        ;;
;; omitted).                                                                       ;;
;;                                                                                 ;;
;; Fill in the body for the skeletons and do not change the arguments. You can     ;;
;; define as many functions as you require, there's no special credit for          ;;
;; implementing/using all of them.                                                 ;;
;;                                                                                 ;;
;; CAUTION:                                                                        ;;
;; 1. Be mindful that bi, tri and quadgrams do not cross word boundaries. Hence,   ;;
;; you must process each word separately.                                          ;;
;;                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Analyses
(provide cipher-monograms
         cipher-bigrams
         cipher-unique-neighbourhood
         cipher-neighbourhood
         cipher-trigrams
         cipher-quadgrams
         cipher-common-words-single
         cipher-common-words-double
         cipher-common-words-triple
         cipher-common-words-quadruple
         cipher-common-initial-letters
         cipher-common-final-letters
         cipher-common-double-letters
         ;; any other functions of your design come below:
         checker
         filter
         arrange
         ;; my-fundoo-analysis
         )

;; Takes ciphertext and produces a list of cipher chars sorted in decreasing
;; order of frequency.
(define (cipher-monograms ciphertext)
  (define(iter4 ct lc i)
    (cond((= i (length ct)) lc)
         ((and(>= (char->integer (list-ref ct i)) 97) (<= (char->integer (list-ref ct i)) 122)) (if(checkerc (list-ref ct i) lc) (iter4 ct (append (remove (list-ref lc (findc (list-ref ct i) lc)) lc) (list (cons (list-ref ct i) (+ 1 (cdr (list-ref lc (findc (list-ref ct i) lc))))))) (+ i 1))
                                         (iter4 ct (append lc (list (cons (list-ref ct i) 1))) (+ i 1))))
         (else(iter4 ct lc (+ i 1)))))
  (arrange (iter4 (string->list (sanitize-text ciphertext)) '() 0)))

;; Takes the cipher-word-list and produces a list of 2-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-bigrams cipher-word-list)
  (arrange (n-grams (word-list2 cipher-word-list) 2 0 '())))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter. Only unique
;; neighbours are to be counted.
;; Consider the character #\o.
;;
;; Takes an argument `mode`:
;; 1. (cipher-unique-neighbourhood cipher-bigrams 'predecessor)
;;    - Count only those unique occurrences where the (given) char preceeds
;;      other char.
;;    - Patterns of the form: "o?"
;; 2. (cipher-unique-neighbourhood cipher-bigrams 'successor)
;;    - Count only those unique occurrences where the (given) char succeeds
;;      other char.
;;    - Patterns of the form: "?o"
;; 3. (cipher-unique-neighbourhood cipher-bigrams 'both)
;;    - Count all unique occurrences where the (given) char neighbours the other
;;      char.
;;    - Patterns of the form "?o" and "o?". Note that this is not sum of (1) and
;;    (2) always.
;;
;; The output is a list of pairs of cipher char and the count of it's
;; neighbours. The list must be in decreasing order of the neighbourhood count.
(define (cipher-unique-neighbourhood cipher-bigrams-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define(list-chars ls i lc)
    (if(= i (length ls)) lc (list-chars ls (+ i 1) (append lc (list (string->list (list-ref ls i)))))))
  (cond((equal? mode 'both) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(or (char=? (car (list-ref lc j)) (list-ref lm i)) (char=? (cadr (list-ref lc j)) (list-ref lm i)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer (list-chars cipher-bigrams-list 0 '()) (cipher-monograms utils:ciphertext) 0 '() 0)))
       
       ((equal? mode 'successor) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(char=? (cadr (list-ref lc j)) (list-ref lm i))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer (list-chars cipher-bigrams-list 0 '()) (cipher-monograms utils:ciphertext) 0 '() 0)))

       ((equal? mode 'predecessor) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(char=? (car (list-ref lc j)) (list-ref lm i))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer (list-chars cipher-bigrams-list 0 '()) (cipher-monograms utils:ciphertext) 0 '() 0)))))

;; Takes the bigram frequency order (output of `cipher-bigrams`) and computes
;; the neighbourhood of each letter with every other letter, but counts each
;; occurrence distinctly. This comment contains 6 bigrams with "w", all with "i" or "h".
;; So "w" has:
;; when mode is 'both,        6 neighbours
;; when mode is 'predecessor, 6 neighbours
;; when mode is 'successor,   0 neighbours
(define (cipher-neighbourhood cipher-word-list mode)
  ;; You must match against or test (using cond) for the `mode` argument. Possibilities are:
  ;; 'predecessor, 'successor, 'both
  ;; Figure out experimentally which of these is a good indicator for E vs T.
  (define lc (word-list2 cipher-word-list))
  (define lb (n-grams2 lc 2 0 '()))
  (cond((equal? mode 'both) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(or (char=? (car (list-ref lc j)) (list-ref lm i)) (char=? (cadr (list-ref lc j)) (list-ref lm i)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer lb (cipher-monograms utils:ciphertext) 0 '() 0)))
       
       ((equal? mode 'successor) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(char=? (cadr (list-ref lc j)) (list-ref lm i))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer lb (cipher-monograms utils:ciphertext) 0 '() 0)))

       ((equal? mode 'predecessor) (define(iter-outer lc lm i llc j)
    (cond((= (length lm) i) llc)
         ((= j (length lc)) (iter-outer lc lm (+ i 1) llc 0))
         (else(if(char=? (car (list-ref lc j)) (list-ref lm i))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i (append (remove (list-ref llc (findc (list-ref lm i) llc)) llc) (list (cons (list-ref lm i) (+ 1 (cdr (list-ref llc (findc (list-ref lm i) llc))))))) (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 1))) (+ j 1)))
                                                                (if(checkerc (list-ref lm i) llc) (iter-outer lc lm i llc (+ j 1))
                                                                   (iter-outer lc lm i (append llc (list (cons (list-ref lm i) 0))) (+ j 1)))))))
  (order (iter-outer lb (cipher-monograms utils:ciphertext) 0 '() 0)))))

;; Takes the cipher-word-list and produces a list of 3-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-trigrams cipher-word-list)
  (arrange (n-grams (word-list2 cipher-word-list) 3 0 '())))

;; Takes the cipher-word-list and produces a list of 4-letter bigram (strings)
;; sorted in decreasing order of frequency. Each element must be a string!
(define (cipher-quadgrams cipher-word-list)
   (arrange (n-grams (word-list2 cipher-word-list) 4 0 '())))

;; Takes the cipher word list and produces a list of single letter words, sorted
;; in decreasing order of frequency. Each element must be a string!
(define (cipher-common-words-single cipher-word-list)
  (arrange (iter cipher-word-list 0 '() 1)))

;; Takes the cipher word list and produces a list of double letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-double cipher-word-list)
  (arrange (iter cipher-word-list 0 '() 2)))

;; Takes the cipher word list and produces a list of triple letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-triple cipher-word-list)
  (arrange (iter cipher-word-list 0 '() 3)))

;; Takes the cipher word list and produces a list of four letter words, sorted
;; in decreasing order of frequency.
(define (cipher-common-words-quadruple cipher-word-list)
  (arrange (iter cipher-word-list 0 '() 4)))

;; Takes the cipher word list and produces a list of chars that appear at the
;; start of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-initial-letters cipher-word-list)
  (define ad-cwl (word-list2 cipher-word-list))
  (define(iter2 ad-cwl l i)
    (if(= i (length ad-cwl)) l (iter2 ad-cwl (append l (list (car (list-ref ad-cwl i)))) (+ i 1))))
  (arrange (rle (iter2 ad-cwl '() 0) 0 '())))

;; Takes the cipher word list and produces a list of chars that appear at the
;; end of words, sorted in decreasing order of frequency. Each element must be
;; a char!
(define (cipher-common-final-letters cipher-word-list)
  (define ad-cwl (word-list2 cipher-word-list))
  (define(iter3 ad-cwl l i)
    (if(= i (length ad-cwl)) l (iter3 ad-cwl (append l (list (list-ref (list-ref ad-cwl i) (- (length (list-ref ad-cwl i)) 1)))) (+ i 1))))
  (arrange (rle (iter3 ad-cwl '() 0) 0 '())))

;; Takes the cipher word list and produces a list of chars that appear as
;; consecutive double letters in some word, sorted in decreasing order of
;; frequency. Each element must thus be a char!
(define (cipher-common-double-letters cipher-word-list)
  (define bg (cipher-bigrams cipher-word-list))
  (define (iter6 i l lc)
    (cond((= i (length l)) lc)
         ((char=? (car (string->list (list-ref l i))) (cadr (string->list (list-ref l i)))) (if(checker (list-ref l i) lc) (iter6 (+ i 1) l (append (remove (list-ref lc (find (list-ref l i) lc)) lc) (list (cons (list-ref l i) (+ 1 (cdr (list-ref lc (find (list-ref l i) lc))))))))
                                                                                               (iter6 (+ i 1) l (append lc (list (cons (list-ref l i) 1))))))
         (else(iter6 (+ i 1) l lc))))
  (arrange (iter6 0 bg '())))

 (define(iter s i lc j)
    (define l (word-list2 s))
    (cond((= i (length l)) lc)
         ((= (length (list-ref l i)) j) (if(checker (list->string (list-ref l i)) lc)
                                           (iter s (+ i 1) (append (remove (list-ref lc (find (list->string (list-ref l i)) lc)) lc) (list (cons (list->string (list-ref l i)) (+ 1 (cdr (list-ref lc (find (list->string (list-ref l i)) lc))))))) j)
                                         (iter s (+ i 1) (append lc (list (cons (list->string (list-ref l i)) 1))) j)))
         (else(iter s (+ i 1) lc j))))
         
(define(checker a lc)
  (cond((null? lc) #f)
       ((string=? a (caar lc)) #t)
       (else(checker a (cdr lc)))))

(define(checkerc a lc)
  (cond((null? lc) #f)
       ((char=? a (caar lc)) #t)
       (else(checkerc a (cdr lc)))))

(define(find a lc)
  (define(find-iter a lc k)
    (cond((string=? a (caar lc)) k)
         (else(find-iter a (cdr lc) (+ k 1)))))
  (find-iter a lc 0))

(define(findc a lc)
  (define(find-iter a lc k)
    (cond((char=? a (caar lc)) k)
         (else(find-iter a (cdr lc) (+ k 1)))))
  (find-iter a lc 0))

(define(order lc)
  (sort lc #:key (lambda (x) (cdr x)) >))
(define(arrange lc)
  (define olc (order lc))
  (if(null? olc) '() (append (list (caar olc)) (arrange (cdr olc)))))

(define(filter l)
  (define(filter-helper l n)
    (cond((< n 0) l)
         ((not(number? (list-ref l n))) (filter-helper (remove '() l) (- n 1)))
         (else(filter-helper l (- n 1)))))
  (filter-helper l (- (length l) 1)))

                           (define(word-list sl)
                      
                             (define(word-list-iter l lw i j)
                               (cond((= j (- (length l) 1)) (append lw (list (slice l i (- j 1)))))
                                    ((and (>= (char->integer (list-ref l j)) 97)
                                          (<= (char->integer (list-ref l j)) 122))
                                     (word-list-iter l lw i (+ j 1)))
                                    ((or (< (char->integer (list-ref l j)) 97)
                                          (> (char->integer (list-ref l j)) 122)) (if(= j 0)
                                                                                      (word-list-iter l lw (+ j 1) (+ j 1))
                                                                                     (word-list-iter l (append lw (list (slice l i (- j 1)))) (+ j 1) (+ j 1))))))
                               
                             (filter (word-list-iter sl '() 0 0)))
(define(slice l i j)
  (if(> i j) '() (cons (list-ref l i) (slice l (+ i 1) j))))

(define(rle l i lc)
  (cond((= i (length l)) lc)
       ((checkerc (list-ref l i) lc) (rle l (+ i 1) (append (remove (list-ref lc (findc (list-ref l i) lc)) lc) (list (cons (list-ref l i) (+ 1 (cdr (list-ref lc (findc (list-ref l i) lc))))))))) 
       (else(rle l (+ i 1) (append lc (list (cons (list-ref l i) 1)))))))


(define(n-grams lw n i lc)
  (cond((= i (length lw)) lc)
       ((>= (length (list-ref lw i)) n)    
             (n-grams lw n (+ i 1) (higher-append (iter5 '() 0 (lin-comb (list-ref lw i) n)) lc 0)))
       (else(n-grams lw n (+ i 1) lc))))

(define(lin-comb l n)
  (define(lin-comb-iter l n i al)
    (if(= (- (+ i n) 1) (length l)) al
       (lin-comb-iter l n (+ i 1) (append al (list (slice l i (- (+ i n) 1)))))))
  (lin-comb-iter l n 0 '()))

(define(iter5 lc i l)
  (cond((= i (length l)) lc)
       ((checker (list->string (list-ref l i)) lc) (iter5 (append (list (cons (list->string (list-ref l i)) (+ 1 (cdr (list-ref lc (find (list->string (list-ref l i)) lc)))))) (remove (list-ref lc (find (list->string (list-ref l i)) lc)) lc)) (+ i 1) l))
       (else(iter5 (append (list (cons (list->string (list-ref l i)) 1)) lc) (+ i 1) l))))

(define(higher-append lc1 lc2 i)
  (cond((null? lc2) lc1)
       ((= i (length lc1)) lc2)
       ((checker (car (list-ref lc1 i)) lc2) (higher-append lc1 (append (list (cons (car (list-ref lc1 i)) (+ (cdr (list-ref lc1 i)) (cdr (list-ref lc2 (find (car (list-ref lc1 i)) lc2)))))) (remove (list-ref lc2 (find (car (list-ref lc1 i)) lc2)) lc2)) (+ i 1))) 
       (else(higher-append lc1 (append lc2 (list (list-ref lc1 i))) (+ i 1)))))

 (define (sanitize-text ciphertext)
  (regexp-replace* (pregexp "'\\w+") ciphertext ""))
 (define(word-list2 ls)
   (if(null? ls) '() (append (list (string->list (car ls))) (word-list2 (cdr ls)))))

(define(n-grams2 lw n i lc)
  (cond((= i (length lw)) lc)
       ((>= (length (list-ref lw i)) n)    
             (n-grams lw n (+ i 1) (append lc (lin-comb (list-ref lw i) n))))
       (else(n-grams lw n (+ i 1) lc))))