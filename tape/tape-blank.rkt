#lang racket/base

(provide blank
         blank?
         first/blank
         maybe-rest
         cons/no-blank
         )

(require racket/list
         racket/local
         )

(define blank
  (local [(struct blank ())]
    (blank)))
(define (blank? v)
  (eq? v blank))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first/blank : (Listof Any) -> Any
;; If the list is non-empty, return the first, otherwise return blank.
(define (first/blank lst)
  (cond [(empty? lst) blank]
        [else (first lst)]))

;; maybe-rest : (Listof Any) -> (Listof Any)
(define (maybe-rest lst)
  (cond [(empty? lst) lst]
        [else (rest lst)]))

;; cons/no-blank : Any (Listof Any) -> (Listof Any)
;; If the list is empty and the value is blank, don't cons it.
(define (cons/no-blank v lst)
  (cond [(and (blank? v) (empty? lst)) lst]
        [else (cons v lst)]))

