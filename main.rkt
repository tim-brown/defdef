#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included a LICENSE.txt file, which links to
;; the GNU Lesser General Public License.
;; If you would prefer to use a different license, replace LICENSE.txt with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here
(require "utils/stx-property.rkt"
         "utils/id-naming.rkt"

         syntax/parse)

(provide (all-from-out "utils/stx-property.rkt"
                       "utils/id-naming.rkt"))

;; try to mirror (as best as) define as, er  defined, in
;; (secref "define" #:doc '(lib "scribblings/reference/reference.scrbl"))
;;
;; for now -- we ignore (define id expr); since this provides no challenge in terms of capturing
;; arguments. If we were trying that kind of thing, then other techniques (e.g. lambda) are probably
;; more useful.
;;
;; We *are* interested in currying forms.
;; We are potentially interested in an extension form (see defquery) like:
#;(define (id -> number->string) body ...)
;;             ^^ literal ->
;;
(struct arg-info (id kw default-expr-box) #:transparent)

(define-splicing-syntax-class compulsory-arg/cls
  (pattern arg-id:id
           #:with info #`#,(arg-info #'arg-id #f #f)))

(define-splicing-syntax-class optional-arg/cls
  (pattern (arg-id:id default-expr:expr)
           #:with info #`#,(arg-info #'arg-id #f #'default-expr)))

(define-splicing-syntax-class maybe-optional-arg/cls
  (pattern (~or arg:optional-arg/cls
                arg:compulsory-arg/cls)
           #:with info (attribute arg.info)
           #:with arg-id (attribute arg.arg-id)
           #:with default-expr (attribute arg.default-expr)))

(define-splicing-syntax-class compulsory-arg-maybe-kw/cls
  (pattern arg:compulsory-arg/cls
           #:with info (attribute arg.info)
           #:with arg-id (attribute arg.arg-id))

  ;; keyword arguments may still be optional
  (pattern (~seq kw:keyword arg:maybe-optional-arg/cls)
           #:with info (datum->syntax #'arg
                                      (struct-copy arg-info
                                                   (syntax->datum (attribute arg.info))
                                                   [kw #'kw]))
           #:with arg-id (attribute arg.arg-id)))

(define-splicing-syntax-class optional-arg-maybe-kw/cls
  (pattern arg:optional-arg/cls
           #:with info (attribute arg.info)
           #:with arg-id (attribute arg.arg-id))

  ;; keyword arguments may still be compulsory
  (pattern (~seq kw:keyword arg:maybe-optional-arg/cls)
           #:with info (datum->syntax #'arg
                                      (struct-copy arg-info
                                                   (syntax->datum (attribute arg.info))
                                                   [kw #'kw]))
           #:with arg-id (attribute arg.arg-id)))

(define-splicing-syntax-class args/stx
  (pattern (~seq cs:compulsory-arg-maybe-kw/cls ...
                 os:optional-arg-maybe-kw/cls ...)
           #:with (arg-id ...) #'(cs.arg-id ... os.arg-id ...)
           #:with (info ...) #'(cs.info ... os.info ...)))

(define-syntax-class head/cls
  (pattern (head:head/cls args:args/stx)
           #:attr top? #f
           #:with func-id #'head.func-id
           #:with (arg-id ...) (if (attribute head.arg-id)
                                   #`(head.arg-id ... (args.arg-id ...))
                                   #'((args.arg-id ...)))
           #:with (info ...) (if (attribute head.info)
                                 #`(head.info ... (args.info ...))
                                 #'((args.info ...))))
  (pattern func-id:id
           #:attr top? #t
           #:attr (arg-id 1) #f
           #:attr (info 1) #f))

(define-syntax-class define/cls
  #:datum-literals (define)
  (pattern (define (head:head/cls args:args/stx) body:expr ...+)
           #:with func-id #'head.func-id
           #:with (arg-id ...) (if (attribute head.arg-id)
                                   #`(head.arg-id ... (args.arg-id ...))
                                   #`((args.arg-id ...)))
           #:with (info ...) (if (attribute head.info)
                                 #`(head.info ... (args.info ...))
                                 #`((args.info ...)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  ;; Tests to be run with raco test
  (require (for-syntax racket/base)
           racket/match)

  ;; testing the clases
  (test-case
   "single arg matching syntax classes"
   (define-syntax (test-arg-info stx)
     (syntax-case stx (!splice)
       [(_ s (!splice c) m)
        #'(check-match (syntax-parse s [(~var d c)
                                        (list #'d.arg-id (syntax->datum (attribute d.info)))])
                       m)]
       [(_ s c m)
        #'(check-match (syntax-parse s [((~var d c))
                                        (list #'d.arg-id (syntax->datum (attribute d.info)))])
                       m)]
       [(_ s c) #'(test-arg-info s c (list i (arg-info i _ _)))]))


   (test-case
    "maybe-optional-arg/cls"
    (test-arg-info #'(a) maybe-optional-arg/cls)  
    (test-arg-info #'((a 42)) maybe-optional-arg/cls))

   (test-case
    "{compulsory,optional}-arg-maybe-kw/cls"
    (test-arg-info #'(a) compulsory-arg-maybe-kw/cls)
    (test-arg-info #'(#:foo a) compulsory-arg-maybe-kw/cls)
    (test-arg-info #'(#:foo (a 42)) compulsory-arg-maybe-kw/cls)
  
    (test-arg-info #'(#:foo a) optional-arg-maybe-kw/cls)
    (test-arg-info #'(#:foo (a 42)) optional-arg-maybe-kw/cls)
    (test-arg-info #'((a 42)) optional-arg-maybe-kw/cls)))


  (test-case
   "args list matching syntax classes"
   (define-syntax (test-args-info stx)
     (syntax-case stx (!splice)
       [(_ s c)
        #'(test-begin
           (match-define (list (list is (... ...)) (list (arg-info js _ _) (... ...)))
             (syntax-parse s
               [((~var d c)) (list (syntax->list #'(d.arg-id (... ...)))
                                   (map syntax->datum (attribute d.info)))]))
           (check-equal? is js))]))
   
   (test-args-info #'(a b) args/stx)
   (test-args-info #'((a 42) (b 33)) args/stx)
   (test-args-info #'(b (a 42)) args/stx)
   (test-args-info #'(#:foo a #:bar (b 22)) args/stx)
   (test-args-info #'(#:foo (a 42)) args/stx)
   (test-args-info #'(a (b 22) #:foo c #:bar (d 22)) args/stx))

  (define f.id:stx #'foo)
  
  (define q.id:stx #'q)
           
  (test-case
   "``head'' syntax class"
   (test-case
    "base head"
    (define l (syntax-parse f.id:stx [d:head/cls (list (attribute d.func-id)
                                                       (attribute d.arg-id)
                                                       (attribute d.info))]))
    (check-match l (list (== f.id:stx) #f #f)))

   (test-case
    "uncurried head"
    (define l (syntax-parse #`(#,f.id:stx x) [d:head/cls (list (attribute d.func-id)
                                                               (attribute d.arg-id)
                                                               (attribute d.info))]))
       
    (match-define
      (list f.id
            (list (app syntax->list (list (? syntax? argses) ...)) ...)
            (list (app syntax->list (list (? syntax? (app syntax->datum infoses)) ...)) ...))
      l)
       
    (match-define (cons (list (list x:stx)) (list (list xi:stx))) (cons argses infoses))

    (check-match f.id (== f.id:stx))
       
    (check-match xi:stx (arg-info (== x:stx) _ _)))

  
   (test-case
    "exercise full head"
    (define l (syntax-parse #`(((#,f.id:stx x) (y 14)) #:q #,q.id:stx #:z (z (+ #,q.id:stx 4)))
                [d:head/cls (list (attribute d.func-id)
                                  (attribute d.arg-id)
                                  (attribute d.info))]))
    (check-match (car l) (== f.id:stx))
       
    (match-define (list (app syntax->list (list (? syntax? argses) ...)) ...) (cadr l))

    (match-define (list (app syntax->list (list (? syntax? (app syntax->datum infoses)) ...)) ...)
      (caddr l))
       
    (match-define (list (list x:stx) (list y:stx) (list q:stx z:stx)) argses)

    (match-define (list (list xi:stx) (list yi:stx) (list qi:stx zi:stx)) infoses)

    (check-match xi:stx (arg-info (== x:stx) _ _))
    (check-match yi:stx (arg-info (== y:stx) _ _))
    (check-match qi:stx (arg-info (== q:stx) _ _))
    (check-match zi:stx (arg-info (== z:stx) _ _))))

  ;; -------------------------------------------------------------------------------------------------
  (test-case
   "``define'' syntax class"
   (test-case
    "base define"
    (define l (syntax-parse #`(define (#,f.id:stx) 42) [d:define/cls (list (attribute d.func-id)
                                                                           (attribute d.arg-id)
                                                                           (attribute d.info))]))
    (check-match (car l) (== f.id:stx))
       
    (match-define (list (app syntax->list (list (? syntax? argses) ...)) ...) (cadr l))

    (match-define (list (app syntax->list (list (? syntax? (app syntax->datum infoses)) ...)) ...)
      (caddr l))
       
    (check-match argses (list (list)))

    (check-match infoses (list (list))))

   (test-case
    "uncurried ``define'"
    (define l (syntax-parse #`(define (#,f.id:stx x) 84) [d:define/cls (list (attribute d.func-id)
                                                                             (attribute d.arg-id)
                                                                             (attribute d.info))]))
       
    (check-match (car l) (== f.id:stx))

    (match-define (list (app syntax->list (list (? syntax? argses) ...)) ...) (cadr l))

    (match-define (list (app syntax->list (list (? syntax? (app syntax->datum infoses)) ...)) ...)
      (caddr l))
       
    (match-define (list (list x:stx)) argses)

    (match-define (list (list xi:stx)) infoses)

    (check-match xi:stx (arg-info (== x:stx) _ _)))

  
   (test-case
    "full exercise ``define''"
    (define l (syntax-parse #`(define (((#,f.id:stx x) (y 14))
                                       #:q #,q.id:stx
                                       #:z (z (+ #,q.id:stx 4))) 126)
                [d:define/cls (list (attribute d.func-id)
                                    (attribute d.arg-id)
                                    (attribute d.info))]))
    (check-match (car l) (== f.id:stx))
       
    (match-define (list (app syntax->list (list (? syntax? argses) ...)) ...) (cadr l))

    (match-define (list (app syntax->list (list (? syntax? (app syntax->datum infoses)) ...)) ...)
      (caddr l))
       
    (match-define (list (list x:stx) (list y:stx) (list q:stx z:stx)) argses)

    (match-define (list (list xi:stx) (list yi:stx) (list qi:stx zi:stx)) infoses)

    (check-match xi:stx (arg-info (== x:stx) _ _))
    (check-match yi:stx (arg-info (== y:stx) _ (app syntax-e 14)))
    (check-match qi:stx (arg-info (== q:stx) (app syntax-e '#:q) #f))
    (check-match zi:stx (arg-info (== z:stx)
                                  (app syntax-e '#:z)
                                  (app syntax-e
                                       (list
                                        (? syntax? (app syntax-e '+))
                                        (== q:stx)
                                        (? syntax? (app syntax-e 4))))))))
  )

(module+ main
  ;; Main entry point, executed when run with the `racket` executable or DrRacket.
  )
