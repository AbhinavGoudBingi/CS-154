#lang racket
(provide (all-defined-out))
;(require racket/mpair)

 (define (update-sudoku sudoku row col val)
     (list-set sudoku row (list-set (list-ref sudoku row) col val)))

(define (square-row row) (cond((< (/ row 3) 1) 1)
                              ((< (/ row 3) 2) 2)
                              (else 3)))

(define (square-col col) (cond((< (/ col 3) 1) 1)
                              ((< (/ col 3) 2) 2)
                              (else 3)))

(define(kth-element k l)
  (if(= k 1) (car l) (kth-element (- k 1) (cdr l))))

(define(higher-kth-element k ll i j l)
  (cond((> i j) l)
       (else(higher-kth-element k ll (+ i 1) j (cons (kth-element k (kth-element i ll)) l)))))

(define(square-maker ll i j k l)
  (cond((> j k) l)
       ((square-maker ll i (+ j 1) k (append (higher-kth-element j ll i (+ i 2) '()) l)))));

(define (3*3square sudoku row col)
  (let* ((sq-row (square-row row))
        (sq-col (square-col col))
        (i (cond((= sq-row 1) 1)
                ((= sq-row 2) 4)
                (else 7)))
        (j (cond((= sq-col 1) 1)
                ((= sq-col 2) 4)
                (else 7))))
    (square-maker sudoku i j (+ j 2) '())));
    
(define (list-of-list-ref ll row col)  (list-ref (list-ref ll row) col))

(define (sudoku-solver sudoku)
  (define ans '())
(define (solver-helper sud r c)
       (let ((next (lambda (updated) 
                     (cond [(< c 8)  (solver-helper updated r (+ 1 c))];
                           [(< r 8)  (solver-helper updated (+ 1 r) 0)];
                           [else (set! ans (append ans (list updated)))]))))
;                           (displayln "SOLUTION:")
;                                 (for ((rowline updated)) 
;                                     (println rowline))
         (if (= 0 (list-of-list-ref sud r c))
             (for ((i (range 1 10)));
;applying the brute force method here
                   (when (not (or (member i (list-ref sud r))   ;checking in row              
                                  (member i (map (lambda (r) (list-ref r c)) sud))  ;checking in column
                                  (member i (3*3square sud r c))))  ;checking in square            
                      (next (update-sudoku sud r c i))))
                 (next sud))))
  (solver-helper sudoku 0 0) ans)

