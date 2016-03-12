#lang racket/base

(provide turing-machine
         → ->
         ← <-
         write!
         )

(require my-object
         racket/function
         racket/local
         racket/match
         racket/set
         racket/stxparam
         syntax/parse/define
         "tape-immutable.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit))

(define → tape-move-right)
(define ← tape-move-left)
(define ((write! char) tape)
  (tape-write tape char))

(define-syntax -> (make-rename-transformer #'→))
(define-syntax <- (make-rename-transformer #'←))

(define-syntax-parser turing-machine
  [(tm #:start-state start-state:id
       #:halt [halt-state:id ...]
       [state:id ([char-pat next-state:id action:expr]
                  ...)]
       ...)
   #'(lambda (input-list)
       (define (state char)
         (match char
           [char-pat
            (values
             action
             next-state)]
           ...))
       ...
       (define (halt-state char)
         (values
          identity
          halt-state))
       ...
       (define halting-states (seteq halt-state ...))
       (object
        [tape
         (make-start-tape input-list)]
        [current-state
         start-state]
        [halted?
         (λ ()
           (set-member? halting-states current-state))]
        [next
         (λ ()
           (define-values [tape-action next-current-state]
             (current-state (tape-read tape)))
           (define next-tape (tape-action tape))
           (object-extend this
                          [tape next-tape]
                          [current-state next-current-state]))]
        [get-tape
         (λ ()
           (tape->list tape))]
        [get-position
         (λ ()
           (tape-position tape))]
        [get-current-state-name
         (λ ()
           (object-name current-state))]
        ))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run machine input)
  (define obj (machine input))
  (define (loop obj)
    (cond
      [(send obj halted?)
       (printf "halted with:\n~v\n" (send obj get-tape))
       (send obj get-current-state-name)]
      [else
       (printf "~v\n" (send obj get-tape))
       (loop (send obj next))]))
  (loop obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  
  (define M
    (turing-machine
     #:start-state q1
     #:halt [q2]
     [q1 (['a q1 →]
          ['b q2 (write! 'b)]
          [(? blank?) q1 →])]
     ))
  
  (run M '(a a a b))
  
  )
