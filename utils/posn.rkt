#lang racket/base

(provide posn+
         posn*
         posn∆
         posn-mag
         posn-normalize
         )

(require lang/posn
         racket/math)

;; posn+ : Posn Posn -> Posn
(define (posn+ a b)
  (make-posn (+ (posn-x a) (posn-x b))
             (+ (posn-y a) (posn-y b))))

;; posn* : Real Posn -> Posn
(define (posn* s a)
  (make-posn (* s (posn-x a))
             (* s (posn-y a))))

;; posn∆ : Posn Posn -> Posn
(define (posn∆ a b)
  (posn+ b (posn* -1 a)))

;; posn-mag^2 : Posn -> NonNegReal
(define (posn-mag^2 a)
  (+ (sqr (posn-x a)) (sqr (posn-y a))))

(define (posn-mag a)
  (sqrt (posn-mag^2 a)))

(define (posn-normalize a)
  (posn* (/ (posn-mag a)) a))

