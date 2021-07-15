#lang racket

(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/universe)
(require "Outgame_common.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;IN GAME STRUCTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct ingame_world
  (my_grid curr_blk/but hbuttons vbuttons background inpause? game)
  #:transparent #:mutable)
(struct grid (size block_2d) #:transparent #:mutable)
(struct block (numtemp orgnum size x y flag) #:transparent #:mutable)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;'
;define defalt size for vertical blocks
(define vdef-shap 
  (overlay/xy
   (circle 37.5 "solid" "orange") 37.5 0
   (overlay/xy (rectangle  75 75 "solid" "orange")
               37.5 0 (circle 37.5 "solid" "orange"))))

(define hdef-shap
  (overlay/xy
             (circle 50 "solid" "orange") 50 0
             (overlay/xy (rectangle  100 100 "solid" "orange")
                         50 0 (circle 50 "solid" "orange"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2D vector;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))

(define (get-pair a 2dvec S)
  (let ([ind (index-of (flatten (map (lambda (x) (vector->list x)) (vector->list 2dvec))) a)])
    (cons (quotient ind S) (remainder ind S))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;REQUIRED FUNCIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1.NEWGRID HAS TO BE ASSIGNED WITH BLOCKS
(define (assign_blocks thing)
  (let* ([S (grid-size thing)]
         [si (/ 500 (+ 0.1 (* 1.1 S)))])
    (assg (grid-block_2d thing) (- S 1) (- S 1) si (- S 1))))

(define (assg vec N M si X)
  (cond [(and (equal? N 0) (equal? M 0))
         (begin
           (2d-vector-set! vec N M
                           (block "" 0 si (* (+ 0.1 (* 1.1 N)) si) (* (+ 0.1 (* 1.1 M)) si) #f)))]
        [(equal? N 0)
         (begin ;(set-block-size! (2d-vector-ref vec N M) si) 
           ;(set-block-x! (2d-vector-ref vec N M) (* (+ 0.1 (* 1.1 N)) si))
           ;(set-block-y! (2d-vector-ref vec N M) (* (+ 0.1 (* 1.1 M)) si))
           (2d-vector-set! vec N M
                           (block "" 0 si (* (+ 0.1 (* 1.1 N)) si) (* (+ 0.1 (* 1.1 M)) si) #f))
           (assg vec X (- M 1) si X))]
        [else 
         (begin 
           (2d-vector-set! vec N M
                           (block "" 0 si (* (+ 0.1 (* 1.1 N)) si) (* (+ 0.1 (* 1.1 M)) si) #f))
           (assg vec (- N 1) M si X))]))

; 2. A function that drawas this in_world
;;
(define (draw_butt_i lis bag thing)
  (match lis
    ['() bag]
    [(cons a b)
     (if (equal? a (ingame_world-curr_blk/but thing))
         (underlay/xy (draw_butt_i b bag thing) (button-x a) (button-y a)
                      (outline_i a))  
         (underlay/xy (draw_butt_i b bag thing) (button-x a) (button-y a)
                      (draw_shape_i a)))]))
; helper-1 for drawing button in outworld
(define (outline_i but/blk)
  (cond [(button? but/blk)
         (underlay (rectangle (+ 10 (button-width but/blk))
                              (+ 10 (button-height but/blk)) "solid" "pink")
                   (draw_shape_i but/blk))]
        [(block? but/blk)
         (underlay (rectangle (+ (* 0.1 (block-size but/blk)) (block-size but/blk))
                              (+ (* 0.1 (block-size but/blk)) (block-size but/blk))
                              "solid" "pink")
                   (block_sud but/blk))]))
;;;;draws button with state also
(define (draw_shape_i but)
         (cond [(button-special? but)
                (underlay (button-shape but)
                          (text (string-append "< " (button-label but)
                                               " : " (parameter (button-label but))
                                               " >")
                                18 "white"))]
               [else 
                (underlay (button-shape but)
                          (text (button-label but) 25 "white"))]))

(define (draw_blk lis bag thing)
  (match lis
    ['() bag]
    [(cons a b)
     (if (equal? a (ingame_world-curr_blk/but thing))
         (underlay/xy (draw_blk b bag thing) (block-x a) (block-y a)
                      (outline_i a))  
         (underlay/xy (draw_blk b bag thing) (block-x a) (block-y a)
                      (block_sud a)))]))

(define (draw_grid 2dvec bag thing)
  (match (vector->list 2dvec)
    ['() bag]
    [(cons a b) (draw_grid (list->vector b)
                           (draw_blk (vector->list a) bag thing) thing)]))
                   
; helper-2 draws outline to current button in outworld
(define (draw_i thing)
  (draw_butt_i (ingame_world-hbuttons thing)
                     (draw_butt_i (ingame_world-vbuttons thing)
                                (draw_grid (grid-block_2d (ingame_world-my_grid thing))
                                           (ingame_world-background thing) thing) thing) thing))
;;;draws outgame world for us
(define (block_sud X)
  (let ([dim (block-size X)] [mes (block-numtemp X)])
  (cond [(equal? mes (block-orgnum X))
         (underlay (rectangle dim dim "solid" col-grid)
                   (text(if (or (equal? 0 mes) (string? mes)) "" (number->string mes))
                         15 col-num))]
        [else
         (underlay (rectangle dim dim "solid" col-num)
                   (text (if (or (equal? 0 mes) (string? mes)) "" (number->string mes))
                         15 col-grid))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;DEFAULT NEW GAME SCREEN;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; can be done based on game with deafault 



