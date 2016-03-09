#lang racket/base

(provide turing-machine+table)

(require my-object
         racket/list
         racket/local
         racket/match
         syntax/parse/define
         unstable/sequence
         "tm.rkt"
         "tape.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-syntax-parser turing-machine+table
  [(tm+table #:start-state start-state:id
             #:halt [halt-state:id ...]
             [state:id ([char-pat next-state:id action:expr]
                        ...)]
             ...)
   #'(local [(define (which-row state-name char)
               (match state-name
                 ['halt-state
                  (list state-name '_ 'halt-state 'halt!)]
                 ...
                 ['state
                  (match char
                    [char-pat (list state-name 'char-pat 'next-state 'action)]
                    ...)]
                 ...))
             (define rows*
               (list (list 'halt-state (list (list '_ 'halt-state 'halt!)))
                     ...
                     (list 'state (list (list 'char-pat 'next-state 'action) ...))
                     ...))
             (define rows
               (for*/list ([row* (in-list rows*)]
                           [row-rest (in-list (second row*))])
                 (cons (first row*) row-rest)))
             (define tm
               (turing-machine #:start-state start-state
                               #:halt [halt-state ...]
                               [state ([char-pat next-state action]
                                       ...)]
                               ...))]
       (lambda (input-list)
         (object-extend (tm input-list)
                        #:inherit (get-tape get-position get-current-state-name)
                        [table-rows (lambda () rows)]
                        [which-table-row (lambda ()
                                           (define tape (get-tape))
                                           (define pos (get-position))
                                           (define char
                                             (if (< pos (length tape))
                                                 (list-ref tape pos)
                                                 (error "this should never happen")))
                                           (which-row (get-current-state-name) char))])))])

