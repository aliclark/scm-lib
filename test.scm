
;;; Deps: alias,ass,list,lib

(define-macro (test-expression expr expected)
  (w/gens (expr-value)
    `(begin
       (print-line ',expr)
       (let ((,expr-value ,expr))
         (if (or (and (eq? ',expected '<unspecified>)
                   (eq? ,expr-value <unspecified>))
               (equal? ,expr-value ',expected))
           (begin
             (print-line ,expr-value)
             (display-line "OK")
             true)
           (begin
             (display-line "FAIL!")
             (display "Expected: ")
             (print-line ',expected)
             (display "Returned: ")
             (print-line ,expr-value)
             false))))))

(define (build-expression-test test-pair n)
  `(begin
     (display ,(string-append (number->string n) "> "))
     (test-expression ,(first test-pair) ,(rest test-pair))))

(define-macro (run-test . forms)
  `(and ,@(numbered-map build-expression-test (pair-up forms))))

