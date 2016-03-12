#lang racket/base

(provide turing-machine
         → ->
         ← <-
         write!
         gen:tm-configuration
         halted?
         next
         get-tape
         get-position
         get-current-state-name
         )

(require my-object
         racket/function
         racket/generic
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

(define-generics tm-configuration
  (halted? tm-configuration)
  (next tm-configuration)
  (get-tape tm-configuration)
  (get-position tm-configuration)
  (get-current-state-name tm-configuration))

(struct turing-machine-configuration (tape state halt-states)
  #:methods gen:tm-configuration
  [(define (halted? this)
     (match-define (turing-machine-configuration _ state halt-states) this)
     (set-member? halt-states state))
   (define (next this)
     (match-define (turing-machine-configuration tape state halt-states) this)
     (define-values [tape-action next-state]
        (state (tape-read tape)))
     (define next-tape (tape-action tape))
     (turing-machine-configuration next-tape next-state halt-states))
   (define (get-tape this)
     (tape->list (turing-machine-configuration-tape this)))
   (define (get-position this)
     (tape-position (turing-machine-configuration-tape this)))
   (define (get-current-state-name this)
     (object-name (turing-machine-configuration-state this)))])

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
       (turing-machine-configuration
        (make-start-tape input-list)
        start-state
        halting-states))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run machine input)
  (define obj
    (machine input))
  (define (loop obj)
    (cond
      [(halted? obj)
       (printf "halted with:\n~v\n" (get-tape obj))
       (get-current-state-name obj)]
      [else
       (printf "~v\n" (get-tape obj))
       (loop (next obj))]))
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
