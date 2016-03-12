#lang racket/base

(provide make-start-tape
         tape->list
         tape-position
         tape-read
         tape-write
         tape-move-left
         tape-move-right
         blank
         blank?
         )

(require racket/list
         racket/local
         racket/match
         "tape-blank.rkt"
         (only-in srfi/1 append-reverse)
         )

;; Tape is (tape (Listof Any) (Listof Any))
(struct tape (left right))

;; tape->list : Tape -> (Listof Any)
(define (tape->list tape)
  (append-reverse (tape-left tape)
                  (cond [(empty? (tape-right tape)) (list blank)]
                        [else (tape-right tape)])))

;; tape-position : Tape -> Natural
(define (tape-position tape)
  (length (tape-left tape)))

;; make-start-tape : (Listof Any) -> Tape
(define (make-start-tape input)
  (tape '() (cons blank input)))

;; tape-read : Tape -> Any
(define (tape-read tape)
  (first/blank (tape-right tape)))

;; tape-write : Tape Any -> Tape
(define (tape-write t v)
  (struct-copy tape t
               [right (cons v (maybe-rest (tape-right t)))]))

;; tape-move-left : Tape -> Tape
;; Uses cons/no-blank
(define (tape-move-left t)
  (match-define (tape left right) t)
  (tape (rest left)
        (cons/no-blank (first left) right)))

;; tape-move-right : Tape -> Tape
;; Uses first/blank and maybe-rest
(define (tape-move-right t)
  (match-define (tape left right) t)
  (tape (cons (first/blank right) left)
        (maybe-rest right)))
