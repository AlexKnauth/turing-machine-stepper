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
         "delegate.rkt"
         (for-syntax racket/base
                     syntax/parse
                     ))

(define-generics tm-configuration-table
  ;; table-rows : TM-Configuration-Table -> (Listof (List Symbol Any Symbol Any))
  (table-rows tm-configuration-table)
  ;; which-table-row : TM-Configuration-Table TM-Configuration -> (List Symbol Any Symbol Any)
  (which-table-row tm-configuration-table tm-configuration))

(struct turing-machine-configunation+table
  (tm-configuration rows which-row)
  #:methods gen:tm-configuration
  [(delegate turing-machine-configunation+table-tm-configuration
             [halted? get-tape-list get-tape-position get-current-state-name])
   (define/generic gen-next next)
   ;; The next method for this struct returns not only a TM-Configuration,
   ;; but also a turing-machine-configuration+table struct instance.
   ;; That's why the simple delegate form doesn't work for this.
   (define (next this)
     (match-define (turing-machine-configunation+table tm-configuration rows which-row) this)
     (turing-machine-configunation+table (gen-next tm-configuration) rows which-row))]
  #:methods gen:tm-configuration-table
  [(define (table-rows tm-configuration+table)
     (turing-machine-configunation+table-rows tm-configuration+table))
   (define (which-table-row table tm-configuration)
     (define which-row (turing-machine-configunation+table-which-row table))
     (define tape (get-tape-list tm-configuration))
     (define pos (get-tape-position tm-configuration))
     (define char
       (if (< pos (length tape))
           (list-ref tape pos)
           (error "this should never happen")))
     (which-row (get-current-state-name tm-configuration) char))])

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

