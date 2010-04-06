
;;;; deps: pre.

;;;; Hygienic Lisp Macros
;;;;
;;;; Basic workflow:
;;;;
;;;; The programmer decides they want to use some global value
;;;; eg. the procedure named "list":
;;;;
;;;; > (add-hygienic! list)
;;;;
;;;; The programmer writes a macro using this "list" procedure:
;;;;
;;;; > (define-macro (my-foo x)
;;;;     `(,(hs list) '|>~- ,x '-~<|))
;;;;
;;;; The programmer grudgingly knows how those nooby users
;;;; like to call their variables "list" instead of "lst"
;;;; and so makes his/her usage of the "list" symbol
;;;; hygienic by using ,(hs list) instead.
;;;;
;;;; A user now consumes the macro my-foo in their own code:
;;;;
;;;; > (define (my-fun list)
;;;;     (print (my-foo (sum-all list))))
;;;;
;;;; > (my-fun (list 1 2 3))
;;;; (|>~- 6 -~<|)
;;;;
;;;; Everyone is super happy and live happily ever after.
;;;;

;; Create and define gensym to copy the value
;; held by the symbol "name".
;; Add an entry in the table so we can find
;; this new symbol when we want to use the copied value
;; for symbol "name".
(define-macro (add-hygienic! name)
  (if (hygienic? name)
    `(define ,(hygienic-name name) ,name)
    (let ((new-name (gensym)))
      (table-set! *hygienics* name new-name)
      `(define ,new-name ,name))))

;; defines a new variable and automatically
;; backs-up a copy and adds it to the hygienic table.
(define-macro (define-hygienic formal value)
  `(begin
     (define ,formal ,value)
     (add-hygienic!
       ,(if (list? formal)
          (car formal)
          formal))))

;; Make sure to update the value of the hygienic
;; copy to. I'm not sure why anyone might want to
;; do this, but they can if they want I suppose.
(define-macro (set-hygienic! name value)
  `(begin
     (set! ,name ,value)
     (set! ,(hygienic-name name) ,name)))

;; A handy macro to get the value of a hygienic copy.
;; This allows procedures to access the hygienic copy value.
(define-macro (hv name)
  (hygienic-name name))

