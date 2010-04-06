
;;;; Deps: alias

;;; Booleans

(define (to-boolean x)
  (if x true false))

;;; Strings

(define (symbols-to-string lst) (map symbol->string lst))

(define (string-list->number string-list)
  (string->number (apply string-append string-list)))

(define (string-join words glue)
  (let loop ((accum "") (lst words))
    (if (null? lst)
      accum
      (let ((remaining (cdr lst)))
        (loop
          (string-append accum (car lst)
            (if (null? remaining) "" glue))
          remaining)))))

;;; Output

(define (displays . args) (map display args))

(define (display-line str)
  (display str)
  (newline))

(define (print-line x)
  (print x)
  (newline))

