#lang racket/base

(provide gen:tm-configuration
         halted?
         next
         get-tape
         get-position
         get-current-state-name
         )

(require racket/generic
         )

(define-generics tm-configuration
  (halted? tm-configuration)
  (next tm-configuration)
  (get-tape tm-configuration)
  (get-position tm-configuration)
  (get-current-state-name tm-configuration))

