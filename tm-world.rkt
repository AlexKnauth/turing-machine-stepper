#lang racket/base

(require 2htdp/image
         2htdp/universe
         my-object
         racket/list
         racket/math
         racket/pretty
         "tm.rkt"
         "tm+table.rkt"
         "tape-mutable.rkt"
         )

(define SCENE-WIDTH  1200)
(define SCENE-HEIGHT 735)

(define SCENE-BACKGROUND-COLOR "white")

(define EMPTY-SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT SCENE-BACKGROUND-COLOR))

(define TAPE-X 20)
(define TAPE-Y 20)

(define TURING-MACHINE-TABLE-X 20)
(define TURING-MACHINE-TABLE-Y 90)

(define TABLE-CELL-WIDTH  100)
(define TABLE-CELL-HEIGHT 40)

(define TURING-MACHINE-TABLE-MAX-ROWS
  (exact-floor
   (/ (- SCENE-HEIGHT TURING-MACHINE-TABLE-Y)
      TABLE-CELL-HEIGHT)))

(define TAPE-SQUARE-WIDTH  50)
(define TAPE-SQUARE-HEIGHT 50)

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

;; World is
;; (Object [halted? (-> Boolean)]
;;         [next! (-> Void)]
;;         [get-tape (-> (Listof Any))]
;;         [get-position (-> Natural)]
;;         [get-current-state-name (-> Symbol)]
;;         [table-rows (-> (Listof (Listof Any)))]
;;         [which-table-row (-> (Listof Any))])

;; main : Turing-Machine+Table (Listof Any) -> World
(define (main tm input)
  (big-bang (tm input)
            [on-tick tick]
            [to-draw render-world]
            [stop-when world-halted?]
            [on-key handle-key]
            ))

;; world-halted? : World -> Boolean
(define (world-halted? w)
  (send w halted?))

;; render-world : World -> Image
(define (render-world w)
  (define img (render-world/not-halted w))
  (cond [(world-halted? w)
         (overlay HALTED-IMAGE img)]
        [else
         img]))

;; render-world/not-halted : World -> Image
(define (render-world/not-halted w)
  (add-tape w (add-turing-machine-table w EMPTY-SCENE)))

;; add-tape : World Image -> Image
(define (add-tape w scene)
  (place-image/align (render-tape w)
                     TAPE-X TAPE-Y
                     "left" "top"
                     scene))

;; add-turing-machine-table : World Image -> Image
(define (add-turing-machine-table w scene)
  (place-image/align (render-turing-machine-table w)
                     TURING-MACHINE-TABLE-X TURING-MACHINE-TABLE-Y
                     "left" "top"
                     scene))

;; render-tape : World -> Image
(define (render-tape w)
  (define pos (send w get-position))
  (frame
   (for/fold ([img empty-image])
             ([v (in-list (send w get-tape))]
              [i (in-naturals)])
     (beside img (render-tape-square v (= i pos))))))

;; render-turing-machine-table : World -> Image
(define (render-turing-machine-table w)
  (define rows (send w table-rows))
  (define selected-row (send w which-table-row))
  (render-turing-machine-table-sections rows selected-row))

(define (render-turing-machine-table-sections rows selected-row)
  (cond
    [(<= (length rows) TURING-MACHINE-TABLE-MAX-ROWS)
     (render-turing-machine-table-section rows selected-row)]
    [else
     (beside
      (render-turing-machine-table-section (take rows TURING-MACHINE-TABLE-MAX-ROWS) selected-row)
      (render-turing-machine-table-section (drop rows TURING-MACHINE-TABLE-MAX-ROWS) selected-row))]))

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
  (place-image/align (text (tape-value->string v) TABLE-FONT-SIZE TABLE-FONT-COLOR)
                     (* 1/2 TABLE-CELL-WIDTH) (* 1/2 TABLE-CELL-HEIGHT)
                     "center" "center"
                     (if selected?
                         TABLE-CELL-SELECTED-IMAGE
                         TABLE-CELL-BASE-IMAGE)))

(define (render-tape-square v read-write-head?)
  (place-image/align (text (tape-value->string v) TAPE-FONT-SIZE TAPE-FONT-COLOR)
                     (* 1/2 TAPE-SQUARE-WIDTH) (* 1/2 TAPE-SQUARE-HEIGHT)
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
         (send w next!)
         w]
        [else
         w]))

;; tick : World -> World
(define (tick w)
  w)

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
