
;;;; Deps: fun

;;(require 'random)

;;; Randomising

(define (prob n1 n2) (< (random n2) n1))
(define (fifty-fifty) (= (random 2) 0))
(define (odds percentage) (> percentage (/ (random 100) 100)))

(define (either . func-list)
  ((pick-random func-list)))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (randomised-list-picker lst)
  (partial pick-random lst))

