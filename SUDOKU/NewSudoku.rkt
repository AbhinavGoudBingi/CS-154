#lang racket

(provide (all-defined-out))
(require 2htdp/image)
(require 2htdp/universe)
(require "Outgame_common.rkt")
(require "Ingame_common.rkt")
(require "Sudokuexamples.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;TO CONVERT THE SUDUKO QUESTION AND ANSWER;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define sampleblock (block "" "" 1 1 1 #f))
(define (newgrid) (grid GSIZE (make-2d-vector GSIZE GSIZE sampleblock)))

(define (set-values X Y)
  (define (helper A B M N)
    (cond [(and (equal? N 0) (equal? M 0))
           (begin 
             (set-block-numtemp! (2d-vector-ref A N M) (car (list-ref (list-ref B M) N)))
             (set-block-orgnum! (2d-vector-ref A N M) (cdr (list-ref (list-ref B M) N)))
             A)]
          [(equal? N 0)
           (begin
             (set-block-numtemp! (2d-vector-ref A N M) (car (list-ref (list-ref B M) N)))
             (set-block-orgnum! (2d-vector-ref A N M) (cdr (list-ref (list-ref B M) N)))
             (helper A B (- M 1) (- GSIZE 1)))]
          [else 
           (begin
             (set-block-numtemp! (2d-vector-ref A N M) (car (list-ref (list-ref B M) N)))
             (set-block-orgnum! (2d-vector-ref A N M) (cdr (list-ref (list-ref B M) N)))
             (helper A B M (- N 1)))]))
  (begin (assign_blocks X)
         (let ([y (Solve (list-ref Y (random (length Y))))])
           (helper (grid-block_2d X) y (- GSIZE 1) (- GSIZE 1)))))

(define (question d-level grd)
  (cond [(equal? d-level "NOOB")
         (set-values grd NOOB)]
        [(equal? d-level "AMETUER")
         (set-values grd AMETUER)]
        [(equal? d-level "ROOKIE")
         (set-values grd ROOKIE)]
        [(equal? d-level "PRO")
         (set-values grd PRO)]))

(define (Sudokoscreen)
  (begin                    
    (define newgame (button "NEWGAME" vdef-shap 0 1 100 150 #f))
    ;(define pause (button "PAUSE" vdef-shap 0 1 100 150 #f))
    (define quit (button "QUIT" vdef-shap 0 1 100 150 #f))
    (define Time (button "TIME" hdef-shap 0 1 100 200 #t));set to true
    (define Score (button "SCORE" hdef-shap 0 1 100 200 #t));set to true
    (define vlist (list newgame quit))
    (define hlist (list Time Score))
    ; (assign_blocks newgrid)
    (define X (newgrid))
    (question diff-level X)
    (assign_posn hlist "horz")
    (assign_posn vlist "vert")
    (ingame_world X
                  (2d-vector-ref (grid-block_2d X) 0 0) hlist vlist bg-def #f "sudoku")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define HOME (homescreen))
(define SETTINGS (settingscreen))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;KEY OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (handle-enter world)
  (cond [(outgame_world? world)
         (let ([label (button-label (outgame_world-current_button world))])
           (cond [(equal? world HOME)
                  (cond [(equal? "PLAY" label) (Sudokoscreen)]
                        [(equal? "SETTINGS" label) SETTINGS]
                        [(equal? "STATS" label) (statscreen)]
                        ;add CREDITS
                        [else world])]
                 [else
                  (cond [(equal? "BACK" label) HOME]
                        [(equal? "RESET" label)
                         (begin (reset_hi) world)]
                        [else world])]))]
        [(and (ingame_world? world) (button? (ingame_world-curr_blk/but world)))
         (let ([label (button-label (ingame_world-curr_blk/but world))])
           (cond [(equal? label "NEWGAME") (begin (cleanup) (Sudokoscreen))]
                 [(equal? label "QUIT") (begin (cleanup) HOME)]
                 [else world]))]
        [else world]))

(define (handle-up world)
  (cond [(outgame_world? world)
         (let* ([l (outgame_world-buttons_list world)]
                [curb (outgame_world-current_button world)]
                [i (index-of l curb)])
           (cond [(equal? curb (car l))
                  (begin
                    (set-outgame_world-current_button! world (car (reverse l)))
                    world)]
                 [else
                  (begin
                    (set-outgame_world-current_button! world (list-ref l (- i 1)))
                    world)]))]
        [(ingame_world? world)
         (cond [(block? (ingame_world-curr_blk/but world))
                (let* ([curb (ingame_world-curr_blk/but world)]
                       [2dvec (grid-block_2d (ingame_world-my_grid world))]
                       [size (grid-size (ingame_world-my_grid world))]
                       [pair (get-pair curb 2dvec size)])
                  (cond
                    [(equal? (cdr pair) 0)
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (car pair) (- size 1)))
                       world)]
                    [else
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (car pair) (- (cdr pair) 1)))
                       world)]))]
               [(button? (ingame_world-curr_blk/but world))
                (let* ([l (ingame_world-vbuttons world)]
                       [curb (ingame_world-curr_blk/but world)]
                       [i (index-of l curb)])
                  (cond [(equal? curb (car l))
                         (begin
                           (set-ingame_world-curr_blk/but! world (car (reverse l)))
                           world)]
                        [else
                         (begin
                           (set-ingame_world-curr_blk/but! world (list-ref l (- i 1)))
                           world)]))])]))

(define (handle-down world)
  (cond [(outgame_world? world)
         (let* ([l (outgame_world-buttons_list world)]
                [curb (outgame_world-current_button world)]
                [i (index-of l curb)])
           (cond [(equal? curb (car (reverse l)))
                  (begin
                    (set-outgame_world-current_button! world (car l))
                    world)]
                 [else
                  (begin
                    (set-outgame_world-current_button! world (list-ref l (+ i 1)))
                    world)]))]
        [(ingame_world? world)
         (cond [(block? (ingame_world-curr_blk/but world))
                (let* ([curb (ingame_world-curr_blk/but world)]
                       [2dvec (grid-block_2d (ingame_world-my_grid world))]
                       [size (grid-size (ingame_world-my_grid world))]
                       [pair (get-pair curb 2dvec size)])
                  (cond
                    [(equal? (cdr pair) (- size 1))
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (car pair) 0))
                       world)]
                    [else
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (car pair) (+ 1 (cdr pair))))
                       world)]))]
               [(button? (ingame_world-curr_blk/but world))
                (let* ([l (ingame_world-vbuttons world)]
                       [curb (ingame_world-curr_blk/but world)]
                       [i (index-of l curb)])
                  (cond [(equal? curb (car (reverse l)))
                         (begin
                           (set-ingame_world-curr_blk/but! world (car l))
                           world)]
                        [else
                         (begin
                           (set-ingame_world-curr_blk/but! world (list-ref l (+ i 1)))
                           world)]))])]
        [else world]))

(define (handle-right world)
  (cond [(ingame_world? world)
         (cond [(block? (ingame_world-curr_blk/but world))
                (let* ([curb (ingame_world-curr_blk/but world)]
                       [2dvec (grid-block_2d (ingame_world-my_grid world))]
                       [size (grid-size (ingame_world-my_grid world))]
                       [pair (get-pair curb 2dvec size)])
                  (cond
                    [(equal? (car pair) (- size 1))
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec 0 (cdr pair)))
                       world)]
                    [else
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (+ 1 (car pair))  (cdr pair)))
                       world)]))]
               [else world])]
        [(and (outgame_world? world) (button-special? (outgame_world-current_button world)))
         (let ([label (button-label (outgame_world-current_button world))])
           (begin (cond [(equal? label "GRID") (chng_gsize+)]
                        [(equal? label "COLNUM") (chng_col-num+)]
                        [(equal? label "COLGRD") (chng_col-grid+)]
                        [(equal? label "DIFF") (chng_diff+)])
                  ; [(equal? label "") ]
                  ; [(equal? label "") ]
                  world))]
        [else world]))

(define (handle-left world)
  (cond [(ingame_world? world)
         (cond [(block? (ingame_world-curr_blk/but world))
                (let* ([curb (ingame_world-curr_blk/but world)]
                       [2dvec (grid-block_2d (ingame_world-my_grid world))]
                       [size (grid-size (ingame_world-my_grid world))]
                       [pair (get-pair curb 2dvec size)])
                  (cond
                    [(equal? (car pair) 0)
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (- size 1) (cdr pair)))
                       world)]
                    [else
                     (begin 
                       (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (- (car pair) 1)  (cdr pair)))
                       world)]))]
               [else world])]
        [(and (outgame_world? world) (button-special? (outgame_world-current_button world)))
         (let ([label (button-label (outgame_world-current_button world))])
           (begin (cond [(equal? label "GRID") (chng_gsize-)]
                        [(equal? label "COLNUM") (chng_col-num-)]
                        [(equal? label "COLGRD") (chng_col-grid-)]
                        [(equal? label "DIFF") (chng_diff-)])
                  ; [(equal? label "") ]
                  ; [(equal? label "") ]
                  world))]
        [else world]))

(define (handle-num n world)
  (cond [(and (ingame_world? world) (block? (ingame_world-curr_blk/but world)))
         (let* ([curb (ingame_world-curr_blk/but world)]
                [2dvec (grid-block_2d (ingame_world-my_grid world))]
                [size (grid-size (ingame_world-my_grid world))]
                [pair (get-pair curb 2dvec size)])
           (begin (if (equal? (block-numtemp curb) (block-orgnum curb)) (void)
                  (begin (if (equal? n (block-orgnum curb)) (chng_score 50) (chng_score -5))
                  (set-block-numtemp! (2d-vector-ref 2dvec (car pair) (cdr pair)) n)))
                  (set-ingame_world-curr_blk/but! world (2d-vector-ref 2dvec (car pair)
                                                                       (cdr pair)))
                  world))]
        [else world]))

(define (handle-tab world)
  (cond [(ingame_world? world)
         (cond [(block? (ingame_world-curr_blk/but world))
                (begin 
                  (set-ingame_world-curr_blk/but! world (car (ingame_world-vbuttons world)))
                  world)]
               [(button? (ingame_world-curr_blk/but world))
                (begin
                  (set-ingame_world-curr_blk/but! world
                                                  (2d-vector-ref (grid-block_2d (ingame_world-my_grid world)) 0 0))
                  world)])]
        [else world]))

(define (key_actions world key)
  (cond
    [(key=? "\r"  key) (handle-enter world)]
    [(key=? "up" key) (handle-up world)];makechanges to current button
    [(key=? "down" key) (handle-down world)]
    [(key=? "right" key) (handle-right world)]
    [(key=? "left" key) (handle-left world)]
    [(key=? "1" key) (handle-num 1 world)]
    [(key=? "2" key) (handle-num 2 world)]
    [(key=? "3" key) (handle-num 3 world)]
    [(key=? "4" key) (handle-num 4 world)]
    [(key=? "5" key) (handle-num 5 world)]
    [(key=? "6" key) (handle-num 6 world)]
    [(key=? "7" key) (handle-num 7 world)]
    [(key=? "8" key) (handle-num 8 world)]
    [(key=? "9" key) (handle-num 9 world)]
    [(key=? "\t" key) (handle-tab world)]
    [(key=? "0" key) (handle-num 0 world)]
    ;makechanges to currentbutton
    [else world]))

(define (draw world)
  (if (outgame_world? world) (draw_o world)
      (draw_i world)))

(define (done? world)
  (define (f x) (equal? (block-numtemp x) (block-orgnum x)))
  (let* ([vec (grid-block_2d (ingame_world-my_grid world))]
         [lis (map vector->list (vector->list vec))])
    (andmap (lambda (x) (andmap f x)) lis)))

(define (manage-tick world)
  (cond [(outgame_world? world) (begin (set_time 0) (set_hi) world)]
        [(and (ingame_world? world) (done? world)) world]
        [(ingame_world? world) (begin (set_time (+ 1 time)) (set_hi) world)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;BIG_BANG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(big-bang HOME
  (on-tick manage-tick 1)
  (on-key key_actions)
  (to-draw draw));;;;;;THINK ABOUT STOP WHEN

