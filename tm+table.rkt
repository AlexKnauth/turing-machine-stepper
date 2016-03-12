#lang racket/base

(provide turing-machine+table
         table-rows
         which-table-row
         )

(require racket/generic
         racket/list
         racket/local
         racket/match
         syntax/parse/define
         "tm.rkt"
         "tm-configuration.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(struct turing-machine-configunation+table
  (tm-configuration rows which-row)
  #:methods gen:tm-configuration
  [(define/generic gen-halted? halted?)
   (define/generic gen-next next)
   (define/generic gen-get-tape get-tape)
   (define/generic gen-get-position get-position)
   (define/generic gen-get-current-state-name get-current-state-name)
   (define (halted? this)
     (gen-halted? (turing-machine-configunation+table-tm-configuration this)))
   (define (next this)
     (match-define (turing-machine-configunation+table tm-configuration rows which-row) this)
     (turing-machine-configunation+table (gen-next tm-configuration) rows which-row))
   (define (get-tape this)
     (gen-get-tape (turing-machine-configunation+table-tm-configuration this)))
   (define (get-position this)
     (gen-get-position (turing-machine-configunation+table-tm-configuration this)))
   (define (get-current-state-name this)
     (gen-get-current-state-name (turing-machine-configunation+table-tm-configuration this)))])

(define (table-rows tm-configuration+table)
  (turing-machine-configunation+table-rows tm-configuration+table))

(define (which-table-row tm-configuration+table)
  (define which-row (turing-machine-configunation+table-which-row tm-configuration+table))
  (define tape (get-tape tm-configuration+table))
  (define pos (get-position tm-configuration+table))
  (define char
    (if (< pos (length tape))
        (list-ref tape pos)
        (error "this should never happen")))
  (which-row (get-current-state-name tm-configuration+table) char))

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
         (turing-machine-configunation+table
          (tm input-list)
          rows
          which-row)))])

