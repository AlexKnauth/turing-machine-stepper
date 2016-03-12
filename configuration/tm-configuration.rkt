#lang racket/base

(provide gen:tm-configuration
         halted?
         next
         get-tape-list
         get-tape-position
         get-current-state-name
         )

(require racket/generic
         )

(define-generics tm-configuration
  ;; halted? : TM-Configuration -> Boolean
  (halted? tm-configuration)
  ;; next : TM-Configuration -> TM-Configuration
  (next tm-configuration)
  ;; get-tape-list : TM-Configuration -> (Listof Any)
  (get-tape-list tm-configuration)
  ;; get-tape-position : TM-Configuration -> Natural
  (get-tape-position tm-configuration)
  ;; get-current-state-name : TM-Configuration -> Symbol
  (get-current-state-name tm-configuration))

