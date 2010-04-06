
;;;; Deps: alias,fun,hmac

;;; Assoc lists

(define-partial pair-up map-two cons)

(define (pair l1 l2)
  (map-lists cons l1 l2))

;; This holds a pattern, not yet abstracted be.
(define (assoc-ref-nfv d n not-found-value)
  (if (null? d)
    not-found-value
    (let ((nex (car d)))
      (if (eq? (car nex) n)
        (cdr nex)
        (assoc-ref-nfv (cdr d) n not-found-value)))))

(define (assoc-ref-nil d n)
  (assoc-ref-nfv d n nil))

(define-hygienic (assoc-add lst key val)
  (cons (cons key val) lst))

;; If you know the element exists and is near the front,
;; you can use this one instead.
(define-hygienic (assoc-set lst key val)
  (if (null? lst)
    (list (cons key val))
    (let ((next (car lst)))
      (if (equal? key (car next))
        (cons (cons key val) (cdr lst))
        (cons next (assoc-set (cdr lst) key val))))))

