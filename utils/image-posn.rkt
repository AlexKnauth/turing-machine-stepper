#lang racket/base

(provide place-image/align/posn
         scene+line/posn
         )

(require lang/posn
         2htdp/image
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

