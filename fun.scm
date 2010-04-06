
;;;; Deps: alias,pre,lib

(include "macros#.scm")

;;; Lambdas

(define (identity x . rest) x)
(define (tautology . x) true)
(define (contradiction . x) false)

(define (partial function . early-args)
  (lambda late-args
    (apply function (append early-args late-args))))

(add-hygienic! partial)

(define (partial-right function . early-args)
  (lambda late-args
    (apply function (append late-args early-args))))

(define (negate func)
  (lambda args
    (not (apply func args))))

(define (compose . funs)
  (if (= (length funs) 1)
    (lambda args
      (apply (car funs) args))
    (lambda args
      ((car funs)
        (apply (apply compose (cdr funs)) args)))))

(define (repeated f n)
  (if (zero? n)
    identity
    (compose f (repeated f (- n 1)))))

(define (foldl fun init lst)
  (if (null? lst)
    init
    (fun (car lst) (foldl fun init (cdr lst)))))

(define (foldls fun init . lsts)
  (if (null? (car lst))
    init
    (apply fun (foldls fun init (map cdr lsts)) (map car lsts))))

