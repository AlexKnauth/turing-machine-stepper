#lang racket/base

(provide gen:tm-table
         table-rows
         which-table-row
         )

(require racket/generic
         )

(define-generics tm-table
  ;; table-rows : TM-Table -> (Listof (List Symbol Any Symbol Any))
  (table-rows tm-table)
  ;; which-table-row : TM-Table TM-Configuration -> (List Symbol Any Symbol Any)
  (which-table-row tm-table tm-configuration))

