#lang racket/base

(provide make-start-tape
         tape->list
         tape-position
         tape-read
         tape-write!
         tape-left!
         tape-right!
         blank
         blank?
         )

(require racket/list
         racket/match
         "tape-blank.rkt"
         (only-in srfi/1 append-reverse)
         )

;; Tape is (tape (Listof Any) (Listof Any))
(struct tape (left right) #:mutable)

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

;; tape-write! : Tape Any -> Void
(define (tape-write! tape v)
  (set-tape-right! tape
                   (cons v (maybe-rest (tape-right tape)))))

;; tape-left! : Tape -> Void
;; Uses cons/no-blank
(define (tape-left! t)
  (match-define (tape left right) t)
  (set-tape-right! t (cons/no-blank (first left) right))
  (set-tape-left!  t (rest left)))

;; tape-right! : Tape -> Void
;; Uses first/blank and maybe-rest
(define (tape-right! t)
  (match-define (tape left right) t)
  (set-tape-left!  t (cons (first/blank right) left))
  (set-tape-right! t (maybe-rest right)))

