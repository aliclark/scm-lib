
;;;; Deps: none

;;; Abstract data types

(define *a-representation-type* (gensym))

(define-hygienic (a-representation type value)
  (cons *a-representation-type* (cons type value)))

(define (is-a-representation? data)
  (and
    (pair? data)
    (eq? (car data) *a-representation-type*)))

(define (representation-contents representation)
  (cdr representation))

(define (constructor data)
  (car (representation-contents data)))

(define (data-contents data)
  (cdr (representation-contents data)))

(define (constructed-by? data type)
  (and
    (is-a-representation? data)
    (eq? (constructor data) type)))

;;; Assoc objects

(define (assoc->obj l)
  (a-representation 'assoc-object l))

(define (make-obj . dat)
  (assoc->obj (pair-up dat)))

(define (obj? data)
  (constructed-by? data 'assoc-object))

(define (obj-ref-nfv d n nfv)
  (assoc-ref-nfv (data-contents d) n nfv))

(define (obj-set d n v)
  (assoc->obj (assoc-set (data-contents d) n v)))

