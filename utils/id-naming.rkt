#lang racket/base

(provide extend-base-name)

(require "stx-property.rkt"
         racket/syntax)

(define ((extend-base-name . suffixes)
         base-name
         #:len->start1 (len->start1 (λ (len1 len2) 0))
         #:len->span1 (len->span1 (λ (len1 len2) len2))
         #:frac-x1 (frac-x1 0.5)
         #:frac-y1 (frac-y1 0.5)
         #:true-base-name (true-base-name base-name)
         #:len->start2 (len->start2 (λ (len1 len2) 0))
         #:len->span2 (len->span2 (λ (len1 len2) len2))
         #:frac-x2 (frac-x2 0.5)
         #:frac-y2 (frac-y2 0.5))
  (define bn+ (format-id base-name
                         #:source base-name
                         (apply string-append "~a" suffixes)
                         (syntax-e base-name)))
  (add-sub-range-binding-vector bn+
                                #:len->start1 len->start1
                                #:len->span1 len->span1
                                #:frac-x1 frac-x1
                                #:frac-y1 frac-y1
                                true-base-name
                                #:len->start2 len->start2
                                #:len->span2 len->span2
                                #:frac-x2 frac-x2
                                #:frac-y2 frac-y2))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)

  )