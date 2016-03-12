#lang racket/base

(provide delegate)

(require racket/generic
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax
                     ))

(define-simple-macro
  (delegate accessor:id [method:id ...])
  #:with [gen-method:id ...] (generate-temporaries #'[method ...])
  (begin
    (define/generic gen-method method)
    ...
    (define (method this)
      (gen-method (accessor this)))
    ...))

