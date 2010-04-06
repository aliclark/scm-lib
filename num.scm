
;;;; Deps: fun

(include "macros#.scm")

;;; Numbers

(define (square x) (* x x))
(define-composed not-zero? not zero?)
(define-hygienic inc (partial-right + 1))
(define-hygienic dec (partial-right - 1))

(define (length-check lst num)
  (= (length lst) num))

