#lang racket/base

(provide place-image/align/posn
         scene+line/posn
         scene+arrow/posn
         )

(require lang/posn
         2htdp/image
         "posn.rkt"
         )

;; place-image/align/posn : Image Posn X-Place Y-Place Image -> Image
(define (place-image/align/posn img p x-place y-place scene)
  (place-image/align img
                     (posn-x p) (posn-y p)
                     x-place y-place
                     scene))

;; scene+line/posn : Image Posn Posn (U Pen Color) -> Image
(define (scene+line/posn scene a b pen-or-color)
  (scene+line scene
              (posn-x a) (posn-y a)
              (posn-x b) (posn-y b)
              pen-or-color))

(define (scene+arrow/posn scene back tip head-size pen-or-color)
  (define v (posn∆ back tip))
  ;; smaller version of v
  (define sv (posn* head-size (posn-normalize v)))
  ;; the position slightly behind the tip
  (define sp (posn+ tip (posn* -1 sv)))
  ;; the vector pointing left from sp
  (define lv (posn* 1/2 (make-posn (- (posn-y sv)) (posn-x sv))))
  (define lp (posn+ sp lv))
  ;; the vector pointing right from sp
  (define rv (posn* -1 lv))
  (define rp (posn+ sp rv))
  (scene+line/posn
   (scene+line/posn
    (scene+line/posn
     scene
     back tip pen-or-color)
    rp tip pen-or-color)
   lp tip pen-or-color))

