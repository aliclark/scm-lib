
;;; Deps: alias,fun,adt,num

;;; Promises

(define (promise? x)
  (constructed-by? x 'promise))

(define (promised x)
  (data-contents x))

;;; Streams

(define (stream? x)
  (and (pair? x) (promise? (cdr x))))

(define (stream-cdr stream)
  (force (promised (cdr stream))))

;;; Stream generation

(define (repeat f a)
  (cons a (delayed (repeat f (f a)))))

;;; Pointereds

(define (pointered x n)
  (a-representation 'pointered (cons x n)))

(define (pointered? x)
  (constructed-by? x 'pointered))

(define (pointered-data x)
  (car (data-contents x)))

(define (pointered-num x)
  (cdr (data-contents x)))

(define (pointered-cdr x)
  (pointered (pointered-data x) (inc (pointered-num x))))

(define (pointered-first x)
  (ref (pointered-data x) (pointered-num x)))

(define (pointered-length x)
  (- (len (pointered-data x)) (pointered-num x)))

(define (pointered-ref x)
  (ref (pointered-data x) (+ (pointered-num x) n)))

;;; Ranges

(define (range s e)
  (a-representation 'range (cons s e)))

(define (range? x)
  (constructed-by? x 'range))

(define (range-current x)
  (car (data-contents x)))

(define (range-top x)
  (cdr (data-contents x)))

(define (range-cdr x)
  (range (inc (range-current x)) (range-top x)))

(define (range-length x)
  (- (range-top x) (range-current x)))

(define (range-ref x n)
  (+ (range-current x) n))

;;; Some extras

(define number-first identity)
(define number-length identity)

(define boolean-cdr contradiction)
(define boolean-first identity)

(define (boolean-length x)
  (if x 1 0))

(define (vector-first x)
  (vector-ref x 0))

(define (string-first x)
  (string-ref x 0))

;;; Polymorphic selectors
;;; Only use these when you don't know your data type
;;; or want to be flexible with it.

(define (linear-ref x n)
  (first ((repeated rest n) x)))

(define (vectored-cdr x)
  (pointered x 1))

(define (ref x n)
  ((cond
     ((vector? x) vector-ref)
     ((string? x) string-ref)
     ((range? x) range-ref)
     ((pointered? x) pointered-ref)
     (else linear-ref))
    x
    n))

(define (len x)
  ((cond
     ((range? x) range-length)
     ((pointered? x) pointered-length)
     ((list? x) length)
     ((vector? x) vector-length)
     ((string? x) string-length)
     ((number? x) number-length)
     ((boolean? x) boolean-length))
    x))

(define (rest x)
  ((cond
     ((stream? x) stream-cdr)
     ((range? x) range-cdr)
     ((pointered? x) pointered-cdr)
     ((pair? x) cdr)
     ((number? x) dec)
     ((boolean? x) boolean-cdr)
     (else vectored-cdr))
    x))

(define (first x)
  ((cond
     ((range? x) range-current)
     ((pointered? x) pointered-first)
     ((pair? x) car)
     ((vector? x) vector-first)
     ((string? x) string-first)
     ((number? x) number-first)
     ((boolean? x) boolean-first))
    x))

(define (nil? x)
  (if (stream? x)
    (null? x)
    (zero? (len x))))

(define-composed second first rest)
(define-composed third first rest rest)
(define-composed rfirst rest first)
(define-composed ffirst first first)
(define-composed sfirst first rest first)

;;; Higher Order
;;; These will be slow, so again only use these when you
;;; want to be flexible.

(define (each proc s)
  (if (nil? s)
    nil
    (cons-stream (proc (first s))
      (each proc (rest s)))))

(define (filtered p s)
  (if (nil? s)
    nil
    (let ((n (first s)))
      (if (p n)
        (cons-stream n (filtered p (rest s)))
        (filtered p (rest s))))))

(define (foldeld f i x)
  (if (nil? x)
    i
    (f (first x) (foldeld f i (rest x)))))

(define (for-every proc s)
  (if (nil? s)
    true
    (begin
      (proc (first s))
      (for-every proc (rest s)))))
