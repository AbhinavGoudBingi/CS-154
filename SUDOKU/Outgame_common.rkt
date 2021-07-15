#lang racket

(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;STRUCTS INVOLVED;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct outgame_world (buttons_list current_button background) #:transparent #:mutable)
(struct button (label shape x y height width special?) #:transparent #:mutable)
;special buttons are like which are editable/have a state viz
;colour gridsize highscore and difficulty
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; SCENE PARAMETERS FIXED NOT TO BE CHANGED ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SCEH 700) ;SCENE HEIGHT
(define SCEW 700) ;SCENE WIDTH
(define bg (empty-scene SCEH SCEW "green"))
(define bg-def (place-image (bitmap "test/back.png") 350 350 bg))
;DEFAULT BACKGROUND

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;; REQUIRED FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1. Assigning positions to button list of the world
;    can also be used by horizontal and vertical button lists
;    of the Ingame thing
(define (assign_posn lis screen)
  (cond [(or (equal? screen "Home") (equal? screen "Settings")
             (equal? screen "Credits"))
         (let ([n (length lis)])
           (cond [(< SCEH (* n (button-height (car lis))))
                  (begin (rescale lis)
                         (assign_posn lis screen))]
                 [else (map (lambda (x) (set-coordinates lis x))
                            (range n))]))]
        [(equal? screen "horz")
         (let ([n (length lis)])
           (cond [(< 500 (* n (button-width (car lis))))
                  (begin (rescale lis)
                         (assign_posn lis screen))]
                 [else (map (lambda (x) (set-cord-h lis x))
                            (range n))]))]
        [(equal? screen "vert")
         (let ([n (length lis)])
           (cond [(< 700 (* n (button-height (car lis))))
                  (begin (rescale lis)
                         (assign_posn lis screen))]
                 [else (map (lambda (x) (set-cord-v lis x))
                            (range n))]))]))
;helper-1 :rescale ;helper-2 set-coordinates ;ser-cord-h ;set-cord-v

(define (set-coordinates l i)
  (let* ([h (button-height (car l))]
         [w (button-width (car l))]
         [H SCEH] [W SCEW] [n (length l)]
         [spac (/ (- H (* n h)) (+ n 1))])
    (begin (set-button-x! (list-ref l i) (/ (- W w) 2))
           (set-button-y! (list-ref l i) (+ (* (+ 1 i) spac) (* i h))))))

(define (set-cord-h l i)
  (let* ([h (button-height (car l))]
         [w (button-width (car l))]
         [H 200] [W 500] [n (length l)]
         [spac (/ (- W (* n w)) (+ n 1))])
    (begin (set-button-y! (list-ref l i) (+ (/ (- H h) 2) 500))
           (set-button-x! (list-ref l i) (+ (* (+ 1 i) spac) (* i w))))))
    

(define (set-cord-v l i)
  (let* ([h (button-height (car l))]
         [w (button-width (car l))]
         [H 700] [W 200] [n (length l)]
         [spac (/ (- H (* n h)) (+ n 1))])
    (begin (set-button-x! (list-ref l i) (+ (/ (- W w) 2) 500))
           (set-button-y! (list-ref l i) (+ (* (+ 1 i) spac) (* i h))))))

(define (rescale lis)
  (map (lambda (x)
         (begin (set-button-shape! x (scale 0.9 (button-shape x)))
                (set-button-height! x (* 0.9 (button-shape x)))
                (set-button-width! x (* 0.9 (button-shape x)))))) lis)

;NOTE THAT WE HAVE PREDEFINED GAME SCREEN 500x500 in GAME WORLD
;with 200x700 vetrical button slot and 500x200 horizontal slot

; 2. A function that drawas this out_world
;;
(define (draw_butt_o lis bag thing)
  (match lis
    ['() bag]
    [(cons a b)
     (if (equal? a (outgame_world-current_button thing))
         (underlay/xy (draw_butt_o b bag thing) (button-x a) (button-y a)
                      (outline_o a))  
         (underlay/xy (draw_butt_o b bag thing) (button-x a) (button-y a)
                      (draw_shape_o a)))]))
; helper-1 for drawing button in outworld
(define (outline_o but)
  (underlay (rectangle (+ 10 (button-width but))
                       (+ 10 (button-height but)) "solid" "pink")
            (draw_shape_o but)))
;;;;draws button with state also
(define (draw_shape_o butt)
  (cond [(button-special? butt)
          (underlay (button-shape butt)
              (text (string-append "< " (button-label butt)
                                   " : " (parameter (button-label butt))
                                   " >")
                    28 "white"))]
        [else 
         (underlay (button-shape butt)
                   (text (button-label butt) 28 "white"))]))
                   
; helper-2 draws outline to current button in outworld
(define (draw_o thing)
  (draw_butt_o (outgame_world-buttons_list thing)
               (outgame_world-background thing) thing))
;;;draws outgame world for us
;; 3. A fuction that gives buttons its shape as the is fixed
(define ndef-shap  ;;;;;;;;;;;;FILL THIS
  (overlay/xy
             (circle 50 "solid" "orange") 50 0
             (overlay/xy (rectangle  200 100 "solid" "orange")
                         150 0 (circle 50 "solid" "orange"))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; PREDEFINED HELER FUNCTIONs TO DEFINE DEFAULT WORLDs ;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (homescreen)
  (begin
    (define play (button "PLAY" ndef-shap 0 1 100 300 #f))
    (define settings (button "SETTINGS" ndef-shap 0 1 100 300 #f))
    (define stats (button "STATS" ndef-shap 0 1 100 300 #f))
    ;(define credits (button "CREDITS" ndef-shap 0 1 100 300 #f))
    (define butt-lis (list play settings stats)); credits))
    (assign_posn butt-lis "Home")
    (outgame_world butt-lis (car butt-lis) bg-def)))
;;;;;;;;; homescreen is generally this and we can fix it as default
(define (settingscreen)
  (begin
    (define grid (button "GRID" ndef-shap 0 1 100 300 #t))
    (define colnum (button "COLNUM" ndef-shap 0 1 100 300 #t))
    (define colgrd (button "COLGRD" ndef-shap 0 1 100 300 #t))
    (define diffi (button "DIFF" ndef-shap 0 1 100 300 #t))
    (define back (button "BACK" ndef-shap 0 1 100 300 #f))
    (define butt-lis (list grid colnum colgrd diffi back))
    (assign_posn butt-lis "Settings")
    (outgame_world butt-lis (car butt-lis) bg-def)))
(define (statscreen)
  (begin
    (define hiscore (button "HISCORE" ndef-shap 0 1 100 300 #t))
    (define reset (button "RESET" ndef-shap 0 1 100 300 #f))
    (define back (button "BACK" ndef-shap 0 1 100 300 #f))
    (define butt_lis (list hiscore reset back))
    (assign_posn butt_lis "Settings")
    (outgame_world butt_lis (car butt_lis) bg-def)))
;;;;;;;;;;; settingscreen is also genarally the same for all grid problem
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;those that are variables to game that can be changed in settings viz
;;;;;;;;colours,gridsize,difficultylevel have to be given a fuction that can
;;;;;;;;set them from outside

(define GSIZE 9)
(define HIGHSCORE 0)
(define time 0)
(define score 0)
(define flag 0.1)
(define diff-level "NOOB")
(define col-num "black");for-grid in ingame
(define col-grid "white");for-grid in ingame
(define possible_flag (list 0.1 0.15 0.18 0.20 0.22))
(define possible_diff (list "NOOB" "ROOKIE" "AMETUER" "PRO"))
(define possible_gc (list "white" "gray" "green" "darkseagreen" "pink"))
(define possible_nc (list "black" "red" "goldenrod" "orange" "purple"))
(define poss_gsize '(8 9 16 25)) ; for compaitablitity to both games
(define (parameter srng)
  (cond [(equal? srng "GRID") (number->string GSIZE)]
        [(equal? srng "COLNUM") col-num]
        [(equal? srng "COLGRD") col-grid];;ADD BUTTON HIGHSCORE
        [(equal? srng "DIFF") diff-level]
        [(equal? srng "TIME") (number->string time)]
        [(equal? srng "SCORE") (number->string score)]
        [(equal? srng "HISCORE") (number->string HIGHSCORE)]
        [(equal? srng "FLAG") (number->string (floor (* flag (sqr GSIZE))))]))

;WRITING CHANGING FUNCTIONS ..... TO SET! PARAMETERS FROM OUTSIDE
(define (set_time n) (set! time n)) ;on tick
(define (set_hi) (set! HIGHSCORE (max (- score time) HIGHSCORE)));on tick
(define (reset_hi) (set! HIGHSCORE 0))

(define (chng_col-num+) (let ([i (index-of possible_nc col-num)])
                         (cond [(equal? i (- (length possible_nc) 1)) (set! col-num (car possible_nc))]
                               [else (set! col-num (list-ref possible_nc (+ i 1)))])))

(define (chng_col-grid+) (let ([i (index-of possible_gc col-grid)])
                         (cond [(equal? i (- (length possible_gc) 1)) (set! col-grid (car possible_gc))]
                               [else (set! col-grid (list-ref possible_gc (+ i 1)))])))

(define (chng_gsize+) (let ([i (index-of poss_gsize GSIZE)])
                         (cond [(equal? i (- (length poss_gsize) 1)) (set! GSIZE (car poss_gsize))]
                               [else (set! GSIZE (list-ref poss_gsize (+ i 1)))])))

(define (chng_diff+) (let ([i (index-of possible_diff diff-level)])
                         (cond [(equal? i (- (length possible_diff) 1)) (set! diff-level (car possible_diff))]
                               [else (set! diff-level (list-ref possible_diff (+ i 1)))])))

(define (chng_col-num-) (let ([i (index-of possible_nc col-num)])
                         (cond [(equal? i 3) (set! col-num (car possible_nc))]
                               [else (set! col-num (list-ref possible_nc (+ i 1)))])))

(define (chng_col-grid-) (let ([i (index-of possible_gc col-grid)])
                         (cond [(equal? i 0) (set! col-grid (car (reverse possible_gc)))]
                               [else (set! col-grid (list-ref possible_gc (- i 1)))])))

(define (chng_gsize-) (let ([i (index-of poss_gsize GSIZE)])
                         (cond [(equal? i 0) (set! GSIZE (car (reverse poss_gsize)))]
                               [else (set! GSIZE (list-ref poss_gsize (- i 1)))])))

(define (chng_diff-) (let ([i (index-of possible_diff diff-level)])
                         (cond [(equal? i 0) (set! diff-level (car (reverse possible_diff)))]
                               [else (set! diff-level (list-ref possible_diff (- i 1)))])))

(define (chng_flag) (let ([i (index-of possible_flag flag)])
                         (cond [(equal? i 0) (set! flag (car (reverse possible_flag)))]
                               [else (set! flag (list-ref possible_flag (- i 1)))])))

(define (chng_score n) (set! score (+ n score)))
(define (cleanup) (begin (set! time 0) (set! score 0)))



 