
;;;; Deps: none

;;;; This file contains bits of run-time code which are needed
;;;; by macro's to help perform expansions.
;;;; This file must therefore be compiled and loaded into
;;;; the interpreter before any of the macro's are.

;;;; apply is not be made hygienic because we need
;;;; to load this before the hmac system.

(define-macro (for-expand-time . expr) (map eval expr) '(begin))
(define-macro (for-run-time . expr) `(begin ,@expr))

(define-macro (for-both-times . expr)
  `(begin
     (for-expand-time ,@expr)
     (for-run-time ,@expr)))

(for-expand-time
  (define *hygienics* (make-table))

  ;; The not found value to be compared against
  ;; an unsuccessful lookup for a hygienic symbol.
  (define *gensym-nfv* (gensym))

  ;; Have we already made a hygienic synonym for this symbol?
  (define (hygienic? name)
    (not
      (eq?
        (table-ref *hygienics* name *gensym-nfv*)
        *gensym-nfv*)))

  ;; Gets the gensym associated with symbol "name".
  (define (hygienic-name name)
    (table-ref *hygienics* name))

  ;; A handy macro to get the symbol name for the hygienic copy.
  ;; This is what you use in your macros to insert the hygienic
  ;; symbol rather than use the unhygienic symbol "name".
  ;;(define-macro (hs name)
  ;;  `(quote ,(hygienic-name name)))

  (define (hs name)
    (table-ref *hygienics* name))

  (define (list-gen-var var)
    (list var '(gensym)))

  ;; Used by define-composed macro
  (define (build-composed fns args)
    (if (null? (cdr fns))
      `(apply ,(car fns) ,args)
      (list (car fns) (build-composed (cdr fns) args)))))

