#lang racket/base

(provide posn+
         posn*
         )

(require lang/posn)

;; posn+ : Posn Posn -> Posn
(define (posn+ a b)
  (make-posn (+ (posn-x a) (posn-x b))
             (+ (posn-y a) (posn-y b))))

;; posn* : Real Posn -> Posn
(define (posn* s a)
  (make-posn (+ s (posn-x a))
             (+ s (posn-y a))))

