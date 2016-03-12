#lang racket/base

(provide tm-configuration+table-start-history
         tm-configuration+table+history-previous-configuration
         )

(require racket/generic
         racket/list
         racket/match
         "tm-configuration.rkt"
         "tm-table.rkt"
         "delegate.rkt"
         )

;; tm-configuration+table-start-history :
;; (And TM-Configuration TM-Table) -> TM-Configuration+Table+History
(define (tm-configuration+table-start-history configuration+table)
  (tm-configuration+table+history (list configuration+table) configuration+table))

;; tm-configuration+table+history-previous-configuration :
;; TM-Configuration+Table+History -> TM-Configuration+Table+History
(define (tm-configuration+table+history-previous-configuration tm)
  (match-define (tm-configuration+table+history configurations table) tm)
  (tm-configuration+table+history (rest configurations) table))

(define (tm-configuration+table+history-current-configuration tm)
  (first (tm-configuration+table+history-configurations tm)))

(struct tm-configuration+table+history (configurations table)
  #:methods gen:tm-configuration
  [(delegate tm-configuration+table+history-current-configuration
             [halted? get-tape-list get-tape-position get-current-state-name])
   (define/generic gen-next next)
   ;; The next method for this struct returns not only a TM-Configuration,
   ;; but also a tm-configuration+table+history struct instance.
   ;; That's why the simple delegate form doesn't work for this.
   (define (next this)
     (match-define (tm-configuration+table+history configurations table) this)
     (tm-configuration+table+history
      (cons (gen-next (first configurations)) configurations)
      table))]
  #:methods gen:tm-table
  [(define/generic gen-table-rows table-rows)
   (define/generic gen-which-table-row which-table-row)
   (define (table-rows table)
     (gen-table-rows (tm-configuration+table+history-table table)))
   (define (which-table-row table tm-configuration)
     (define table* (tm-configuration+table+history-table table))
     (gen-which-table-row table* tm-configuration))])

