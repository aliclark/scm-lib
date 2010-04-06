
;;; Multimethods - for personal consumption.

(define *defmultis* nil)
(define *defmethods* nil)

(define-macro (defmulti name fun)
  `(define *defmultis* (it-put *defmultis* ',name ,fun)))

(define (method-cond methods result args)
  )

(define-macro (rebuildify nam)
  `(define (,nam . args)
     (let ((rsm (apply (it-prop *defmultis* ',nam) args)))
       (method-cond (nil-it-prop *defmethods* ',nam) rsm args))))

(define-macro (defmethod nam ret lam)
  `(begin
     (define *defmethods*
       (it-put
         *defmethods* ',nam
         (cons (cons ',ret ,lam) (nil-it-prop *defmethods* ',nam))))
     (rebuildify ,nam)))

