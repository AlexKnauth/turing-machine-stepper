#lang racket/base

(require 2htdp/image
         2htdp/universe
         lang/posn
         racket/list
         racket/math
         racket/pretty
         "configuration/tm-configuration.rkt"
         "tm.rkt"
         "configuration/tm-table.rkt"
         "configuration/tm+table.rkt"
         "configuration/tm+table+history.rkt"
         "tape/tape-immutable.rkt"
         "utils/posn.rkt"
         "utils/image-posn.rkt"
         )

(define draw-next-arrow? #t)

(define SCENE-WIDTH  1200)
(define SCENE-HEIGHT 735)

(define SCENE-BACKGROUND-COLOR "white")

(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT SCENE-BACKGROUND-COLOR))

(define TAPE-POSN (make-posn 20 20))

(define TURING-MACHINE-TABLE-POSN
  (make-posn 20 90))

(define TABLE-CELL-WIDTH  100)
(define TABLE-CELL-HEIGHT 40)
(define TABLE-CELL-CENTER
  (make-posn (* 1/2 TABLE-CELL-WIDTH) (* 1/2 TABLE-CELL-HEIGHT)))

(define TABLE-ROW-WIDTH
  (* 4 TABLE-CELL-WIDTH))

(define TABLE-ROW-FIRST-CELL-CENTER
  (posn+ (make-posn (* 0 TABLE-CELL-WIDTH) 0) TABLE-CELL-CENTER))
(define TABLE-ROW-SECOND-CELL-CENTER
  (posn+ (make-posn (* 1 TABLE-CELL-WIDTH) 0) TABLE-CELL-CENTER))
(define TABLE-ROW-THIRD-CELL-CENTER
  (posn+ (make-posn (* 2 TABLE-CELL-WIDTH) 0) TABLE-CELL-CENTER))
(define TABLE-ROW-FOURTH-CELL-CENTER
  (posn+ (make-posn (* 3 TABLE-CELL-WIDTH) 0) TABLE-CELL-CENTER))

(define TURING-MACHINE-TABLE-MAX-ROWS
  (exact-floor
   (/ (- SCENE-HEIGHT (posn-y TURING-MACHINE-TABLE-POSN))
      TABLE-CELL-HEIGHT)))

(define TAPE-SQUARE-WIDTH  50)
(define TAPE-SQUARE-HEIGHT 50)
(define TAPE-SQUARE-CENTER
  (make-posn (* 1/2 TAPE-SQUARE-WIDTH) (* 1/2 TAPE-SQUARE-HEIGHT)))

(define TAPE-FONT-SIZE 20)
(define TABLE-FONT-SIZE 16)

(define TAPE-SQUARE-OUTLINE-COLOR "black")
(define TAPE-FONT-COLOR "blue")

(define TABLE-CELL-OUTLINE-COLOR "black")
(define TABLE-FONT-COLOR "black")

(define READ-WRITE-HEAD-COLOR "gray")
(define TABLE-CELL-SELECTED-COLOR "Lavender")

(define TABLE-CELL-BASE-IMAGE
  (rectangle TABLE-CELL-WIDTH
             TABLE-CELL-HEIGHT
             "outline"
             TABLE-CELL-OUTLINE-COLOR))

(define TAPE-SQUARE-BASE-IMAGE
  (rectangle TAPE-SQUARE-WIDTH
             TAPE-SQUARE-HEIGHT
             "outline"
             TAPE-SQUARE-OUTLINE-COLOR))

(define TABLE-CELL-SELECTED-IMAGE
  (overlay TABLE-CELL-BASE-IMAGE
           (rectangle TABLE-CELL-WIDTH
                      TABLE-CELL-HEIGHT
                      "solid"
                      TABLE-CELL-SELECTED-COLOR)))

(define TAPE-SQUARE-READ-WRITE-HEAD-IMAGE
  (overlay TAPE-SQUARE-BASE-IMAGE
           (rectangle TAPE-SQUARE-WIDTH
                      TAPE-SQUARE-HEIGHT
                      "solid"
                      READ-WRITE-HEAD-COLOR)))

(define HALTED-FONT-SIZE 120)
(define HALTED-FONT-COLOR "red")

(define HALTED-IMAGE
  (text "HALTED" HALTED-FONT-SIZE HALTED-FONT-COLOR))

;; A World is a TM-Configuration+Table+History

;; main : Turing-Machine+Table (Listof Any) -> World
(define (main tm input)
  (big-bang (tm-configuration+table-start-history
             (tm input))
            [on-tick tick]
            [to-draw render-world]
            [on-key handle-key]
            ))

;; render-world : World -> Image
(define (render-world w)
  (define img (render-world/not-halted w))
  (cond [(halted? w)
         (overlay HALTED-IMAGE img)]
        [else
         img]))

;; render-world/not-halted : World -> Image
(define (render-world/not-halted w)
  (add-tape w (add-turing-machine-table w EMPTY-SCENE)))

;; add-tape : World Image -> Image
(define (add-tape w scene)
  (place-image/align/posn (render-tape w)
                          TAPE-POSN
                          "left" "top"
                          scene))

;; add-turing-machine-table : World Image -> Image
(define (add-turing-machine-table w scene)
  (place-image/align/posn (render-turing-machine-table w)
                          TURING-MACHINE-TABLE-POSN
                          "left" "top"
                          scene))

;; render-tape : World -> Image
(define (render-tape w)
  (define pos (get-tape-position w))
  (frame
   (for/fold ([img empty-image])
             ([v (in-list (get-tape-list w))]
              [i (in-naturals)])
     (beside img (render-tape-square v (= i pos))))))

;; render-turing-machine-table : World -> Image
(define (render-turing-machine-table w)
  (define rows (table-rows w))
  (define selected-row (which-table-row w w))
  (define table-img (render-turing-machine-table-sections rows selected-row))
  (cond [draw-next-arrow?
         (add-next-arrow w rows selected-row table-img)]
        [else
         table-img]))

(define (render-turing-machine-table-sections rows selected-row)
  (cond
    [(<= (length rows) TURING-MACHINE-TABLE-MAX-ROWS)
     (render-turing-machine-table-section rows selected-row)]
    [else
     (beside/align
      "top"
      (render-turing-machine-table-section (take rows TURING-MACHINE-TABLE-MAX-ROWS) selected-row)
      (render-turing-machine-table-sections (drop rows TURING-MACHINE-TABLE-MAX-ROWS) selected-row))]
    ))

(define (render-turing-machine-table-section rows selected-row)
  (frame
   ;; for each row
   (for/fold ([img empty-image])
             ([row (in-list rows)])
     (define selected? (equal? row selected-row))
     (define row-img
       ;; for each cell in the row
       (for/fold ([img empty-image])
                 ([v (in-list row)])
         (beside img (render-turing-machine-table-cell v selected?))))
     (above img row-img))))

(define (render-turing-machine-table-cell v selected?)
  (place-image/align/posn (text (tape-value->string v) TABLE-FONT-SIZE TABLE-FONT-COLOR)
                          TABLE-CELL-CENTER
                          "center" "center"
                          (if selected?
                              TABLE-CELL-SELECTED-IMAGE
                              TABLE-CELL-BASE-IMAGE)))

(define (render-tape-square v read-write-head?)
  (place-image/align/posn (text (tape-value->string v) TAPE-FONT-SIZE TAPE-FONT-COLOR)
                          TAPE-SQUARE-CENTER
                          "center" "center"
                          (if read-write-head?
                              TAPE-SQUARE-READ-WRITE-HEAD-IMAGE
                              TAPE-SQUARE-BASE-IMAGE)))

(define (tape-value->string v)
  (cond [(blank? v)
         "#"]
        [else
         (pretty-format v #:mode 'display)]))

;; handle-key : World KeyEvent -> World
(define (handle-key w k)
  (cond [(key=? k "right")
         (cond [(halted? w) w]
               [else (next w)])]
        [(key=? k "left")
         (tm-configuration+table+history-previous-configuration w)]
        [else
         w]))

;; tick : World -> World
(define (tick w)
  w)

;; add-next-arrow :
;; World (Listof (List Symbol Any Symbol Any)) (List Symbol Any Symbol Any) Image -> Image
(define (add-next-arrow w rows selected-row table-img)
  (define next-row (which-table-row w (next w)))
  (define start
    (posn+ (find-row-corner-posn selected-row rows)
           TABLE-ROW-THIRD-CELL-CENTER))
  (define end
    (posn+ (find-row-corner-posn next-row rows)
           TABLE-ROW-FIRST-CELL-CENTER))
  (scene+line/posn table-img
                   start
                   end
                   "blue"))

;; find-row-corner-posn :
;; (List Symbol Any Symbol Any) (Listof (List Symbol Any Symbol Any)) -> Posn
(define (find-row-corner-posn row rows)
  (define i (find-index row rows))
  (define start-section-over
    (quotient i TURING-MACHINE-TABLE-MAX-ROWS))
  (define start-row-down
    (remainder i TURING-MACHINE-TABLE-MAX-ROWS))
  (make-posn
   (* start-section-over TABLE-ROW-WIDTH)
   (* start-row-down TABLE-CELL-HEIGHT)))

;; find-index : Any (Listof Any) -> (U Natural #f)
(define (find-index v lst)
  (for/first ([x (in-list lst)]
              [i (in-naturals)]
              #:when (equal? v x))
    i))

;; finds the first b
#;(main
 (turing-machine+table
  #:start-state q1
  #:halt [q2]
  [q1 (['a q1 →]
       ['b q2 (write! 'b)]
       [(? blank?) q1 →])])
 '(a a a b))

;; copies the tape
(main
 (turing-machine+table
  #:start-state q1
  #:halt [done]
  [q1 ([_ q2 →])]
  [q2 ([(? blank?) done →]
       ['a q3/σ=a (write! blank)]
       ['b q3/σ=b (write! blank)])]
  [q3/σ=a ([(? blank?) q4/σ=a →]
           [else q3/σ=a →])]
  [q3/σ=b ([(? blank?) q4/σ=b →]
           [else q3/σ=b →])]
  [q4/σ=a ([(? blank?) q5/σ=a →]
           [else q4/σ=a →])]
  [q4/σ=b ([(? blank?) q5/σ=b →]
           [else q4/σ=b →])]
  [q5/σ=a ([(? blank?) q6/σ=a (write! 'a)]
           [else q5/σ=a →])]
  [q5/σ=b ([(? blank?) q6/σ=b (write! 'b)]
           [else q5/σ=b →])]
  [q6/σ=a (['a q7/σ=a ←])]
  [q6/σ=b (['b q7/σ=b ←])]
  [q7/σ=a ([(? blank?) q8/σ=a ←]
           [else q7/σ=a ←])]
  [q7/σ=b ([(? blank?) q8/σ=b ←]
           [else q7/σ=b ←])]
  [q8/σ=a ([(? blank?) q1 (write! 'a)]
           [else q8/σ=a ←])]
  [q8/σ=b ([(? blank?) q1 (write! 'b)]
           [else q8/σ=b ←])]
  )
 '(a a b a))
