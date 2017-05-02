#lang racket/base

(provide add-sub-range-binding-vector)

(require syntax/srcloc)

;; ---------------------------------------------------------------------------------------------------
;; SUB-RANGE-BINDING
;; ---------------------------------------------------------------------------------------------------
(define (sub-range-binding-vector stx1 len->start1 len->span1 frac-x1 frac-y1
                                  stx2 len->start2 len->span2 frac-x2 frac-y2)
  (let ((stx1-len (cond
                    [(symbol? (syntax-e stx1))
                     (string-length (symbol->string (syntax-e stx1)))]
                    [(source-location-known? stx1) (source-location-span stx1)]
                    [else (string-length (format "~s" (syntax-e stx1)))]))
        (stx2-len (cond
                    [(symbol? (syntax-e stx2))
                     (string-length (symbol->string (syntax-e stx2)))]
                    [(source-location-known? stx2) (source-location-span stx2)]
                    [else (string-length (format "~s" (syntax-e stx2)))])))

    (vector stx1 (len->start1 stx1-len stx2-len) (len->span1 stx1-len stx2-len) frac-x1 frac-y1
            stx2 (len->start2 stx1-len stx2-len) (len->span2 stx1-len stx2-len) frac-x2 frac-y2)))

(define (add-sub-range-binding-vector stx1
                                      #:len->start1 (len->start1 (λ (len1 len2) 0))
                                      #:len->span1 (len->span1 (λ (len1 len2) len2))
                                      #:frac-x1 (frac-x1 0.5)
                                      #:frac-y1 (frac-y1 0.5)
                                      stx2
                                      #:len->start2 (len->start2 (λ (len1 len2) 0))
                                      #:len->span2 (len->span2 (λ (len1 len2) len2))
                                      #:frac-x2 (frac-x2 0.5)
                                      #:frac-y2 (frac-y2 0.5))
                    
  (syntax-property
   stx1 'sub-range-binders
   (cons (syntax-property stx1 'sub-range-binders)
         (sub-range-binding-vector stx1 len->start1 len->span1 frac-x1 frac-y1
                                   stx2 len->start2 len->span2 frac-x2 frac-y2))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)

  )