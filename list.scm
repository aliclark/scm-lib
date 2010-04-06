
;;;; Deps: alias,num,fun

;;; List boolean

(define-composed not-null? not null)

;;; List modification (functional)

(define (flatten list-of-lists) (apply append list-of-lists))

(define (map-two-r f l)
  (let loop ((acc '()) (rem l))
    (if (or (null? rem) (null? (cdr rem)))
      acc
      (loop (cons (f (car rem) (cadr rem)) acc) (cdr rem)))))

(define-composed map-two reverse map-two-r)

(define (map-lists-r f . ls)
  (let loop ((acc '()) (rem ls))
    (if (any? null? rem)
      acc
      (loop (cons (apply f (map car rem)) acc) (map cdr rem)))))

(define-composed map-lists reverse map-lists-r)

(define (numbered-map fun lst)
  (let loop ((acc '()) (la lst) (n 0))
    (if (null? la)
      (reverse acc)
      (loop (cons (fun (car la) n) acc) (cdr la) (inc n)))))

(define (replace pattern replacement lst)
  (cond
    ((null? lst) nil)
    ((equal? (car lst) pattern)
      (cons replacement (replace pattern replacement (cdr lst))))
    (else
      (cons (car lst) (replace pattern replacement (cdr lst))))))

(define (replace-word replacement-pairs word)
  (if (null? replacement-pairs) word
    (let* ((p (car replacement-pairs)) (a (car p)) (b (cadr p)))
      (cond
        ((equal? a word) b)
        ((equal? b word) a)
        (else (replace-word (cdr replacement-pairs) word))))))

(define (bi-directional-replace replacement-pairs phrase)
  (let loop ((acc '()) (p phrase))
    (if (null? p) (reverse acc)
      (loop (cons (replace-word replacement-pairs (car p)) acc)
        (cdr p)))))

;;; List searching

(define (find-match words phrase)
  (if (null? words)
    nil
    (let ((nex (car words)))
      (if (memq nex phrase)
        (list nex)
        (find-match (cdr words) phrase)))))

(define (response-pred-test lst phrase)
  (if (null? lst)
    nil
    (let ((nex (car lst)))
      (if ((car nex) phrase)
        ((cadr nex) phrase)
        (response-pred-test (cdr lst) phrase)))))

(define (find-first-nfv satisfies-test data-list not-found-value)
  (if (null? data-list)
    not-found-value
    (let ((next-element (car data-list)))
      (if (satisfies-test next-element)
        next-element
        (find-first-nfv satisfies-test (cdr data-list) not-found-value)))))

(define (conditional-nfv lst val nfv)
  (if (null? lst) nfv
    (let ((nex (car lst)))
      (if ((car nex) val) nex
        (conditional-nfv (cdr lst) val nfv)))))

(define (list-cond-nfv data x nfv)
  (cdr (conditional-nfv data x nfv)))

;; Assumes a list of at least one element.
(define (ordered-first func lst)
  (let loop ((current-first (car lst)) (la (cdr lst)))
    (if (null? la)
      current-first
      (loop
        (let ((nex (car la)))
          (if (func current-first nex) current-first nex))
        (cdr la)))))

;;; List testing

(define (memq-boolean it lst)
  (boolean (memq it lst)))

(define (any? satisfies-test testee-list)
  (if (null? testee-list)
    false
    (if (satisfies-test (car testee-list))
      true
      (any? satisfies-test (cdr testee-list)))))

;; Both lists must be at least the length of size.
(define (first-n-equal? list-a list-b size)
  (if (zero? size)
    true
    (if (equal? (car list-a) (car list-b))
      (first-n-equal? (cdr list-a) (cdr list-b) (dec size))
      false)))

;;; List subsets

(define (remove el lst)
  (let loop ((accum '()) (rem lst))
    (if (null? rem)
      (reverse accum)
      (let ((nex (car rem)))
        (if (equal? nex el)
          (loop accum (cdr rem))
          (loop (cons nex accum) (cdr rem)))))))

(define (filter test lst)
  (let loop ((acc '()) (la lst))
    (if (null? la)
      (reverse acc)
      (loop
        (if (test (car la))
          (cons (car la) acc)
          acc)
        (cdr la)))))

(define (filter-map fil ma lst)
  (map ma (filter fil lst)))

(define (first-n x n)
  (let loop ((acc nil) (xa x) (na n))
    (if (or (zero? na) (null? xa)) (reverse acc)
      (loop (cons (car xa) acc) (cdr xa) (dec na)))))

;; If size is greater than length of data-list,
;; the list is simply returned.
(define (last-n data-list size)
  (if (<= (length data-list) size)
    data-list
    (last-n (cdr data-list) size)))

(define (partition-from items-list nth)
  (map (partial cons (first-n items-list nth))
    (set-partitions
      (last-n items-list (- (length items-list) nth)))))

(define (set-partitions items-list)
  (if (null? items-list) (list nil)
    (apply append
      (map-rangen (partial partition-from items-list)
        (length items-list)))))

;;; List generation

(define (listifies-result func) (lambda () (list (func))))

(define (rangen n)
  (let loop ((acc '()) (na n))
    (if (zero? na) acc (loop (cons na acc) (dec na)))))

(define (map-rangen fn n) (map fn (rangen n)))

