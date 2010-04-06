
;;; Make some builtins hygienic for macros first.

(define-macro (w/gens vars . forms)
  `(let ,(map list-gen-var vars)
     ,@forms))

;;; Conditionals

(define-macro (when test . forms)
  `(if ,test
     (begin
       ,@forms)))

(define-macro (unless test . forms)
  `(if (,(hs 'not) ,test)
     (begin
       ,@forms)))

;;; Lambdas

(define-macro (define-partial name f . args)
  `(define ,name (,(hs 'partial) ,f ,@args)))

(define-macro (composed . fns)
  (w/gens (args)
    `(lambda ,args
       ,(build-composed fns args))))

(define-macro (define-composed name . fns)
  `(define ,name (composed ,@fns)))

(define-macro (thunk . forms)
  `(lambda ()
     ,@forms))

;;; Numbers

(define-macro (increment! var)
  `(set! ,var (,(hs 'inc) ,var)))

(define-macro (decrement! var)
  `(set! ,var (,(hs 'dec) ,var)))

;;; List modification (imperative)

(define-macro (cons! x lst)
  `(set! ,lst (,(hs 'cons) ,x ,lst)))

;;; Streams

(define-macro (delayed x)
  `(,(hs 'a-representation) 'promise (delay ,x)))

(define-macro (cons-stream x stream)
  `(,(hs 'cons) ,x (delayed ,stream)))

;;; Assoc lists

(define-macro (assoc-add! lst key val)
  `(set! ,lst (,(hs 'assoc-add) ,lst ,key ,val)))

(define-macro (assoc-set! lst key val)
  `(set! ,lst (,(hs 'assoc-set) ,lst ,key ,val)))

(include "hmac#.scm")

(add-hygienic! cons)
(add-hygienic! not)

