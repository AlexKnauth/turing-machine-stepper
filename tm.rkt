#lang racket/base

(provide turing-machine
         → ->
         ← <-
         write!
         )

(require my-object
         racket/local
         racket/match
         racket/set
         racket/stxparam
         syntax/parse/define
         "tape-mutable.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))
(module+ test
  (require rackunit))

(define-syntax-parameter → #f)
(define-syntax-parameter ← #f)
(define-syntax-parameter write! #f)

(define-syntax -> (make-rename-transformer #'→))
(define-syntax <- (make-rename-transformer #'←))

(define-syntax-parser turing-machine
  [(tm #:start-state start-state:id
       #:halt [halt-state:id ...]
       [state:id ([char-pat next-state:id action:expr]
                  ...)]
       ...)
   #'(lambda (input-list)
       (define tape
         (make-start-tape input-list))
       (define write-char!
         (local [(define (write! char)
                   (tape-write! tape char))]
           write!))
       (define (state char)
         (syntax-parameterize
             ([→ (λ (stx) #'(tape-right! tape))]
              [← (λ (stx) #'(tape-left! tape))]
              [write! (make-rename-transformer #'write-char!)])
           (match char
             [char-pat
              action
              next-state]
             ...)))
       ...
       (define (halt-state char) halt-state)
       ...
       ; a mutable variable!
       (define current-state start-state)
       (define halting-states (seteq halt-state ...))
       (object
        [halted?
         (λ ()
           (set-member? halting-states current-state))]
        [next!
         (λ ()
           (set! current-state
                 (current-state
                  (tape-read tape))))]
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
  (define (loop)
    (cond
      [(send obj halted?)
       (printf "halted with:\n~v\n" (send obj get-tape))
       (send obj get-current-state-name)]
      [else
       (printf "~v\n" (send obj get-tape))
       (send obj next!)
       (loop)]))
  (loop))

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
