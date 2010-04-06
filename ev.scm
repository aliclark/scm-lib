
;;;; Yet Another Lisp Interpreter
;;;;
;;;; The aim of this interpreter is to be incredibly Simple
;;;; whilst remaining true to an Empowering Core lisp
;;;; and being maximally Modifiable in lisp itself.
;;;; Speed is not an aim and the source will not be tweaked
;;;; or ported to C.
;;;; It is hoped that tinkerers will be able to
;;;; use this as their pizza base for testing a lisp design
;;;; and later compile their core into efficient code when happy.
;;;;
;;;; The source code is structured as follows:
;;;;
;;;; Abstract assoc data - Association lists code
;;;; Environments - Creating env's and binding variables to them
;;;; Data type testers - Predicates for different types of data
;;;; Clause type evaluators - Different actions for different function types
;;;; Data type evaluators - Different actions for different data types
;;;; Axioms - A minimal subset of inbuilt code to get off the ground with
;;;; eval - Finds the appropriate data type evaluator and runs it on the data.
;;;;
;;;; Some important ways this Lisp differs from others:
;;;;
;;;; 1) Symbols evaluate to themselves, and only coincidentally as identifiers 
;;;;    when at the car of an expression.
;;;;    This logically seems like a good idea to me and a reader macro
;;;;    like $myvar for (get myvar) would make it convenient enough to use.
;;;; 2) Environments are first class - get the current environment
;;;;    with (pseudo-current-environment)
;;;;    Could potentially be misused quite badly, but that's your choice.
;;;; 3) First-class macros - good for exploring, bad for compilation.
;;;;    Hygiene can be gained if you splice everything,
;;;;    but that wouldn't be compilable.
;;;;    Hygiene is no longer such a big issue with first-class environments
;;;;    though. What used to require macros can now be done with environments.
;;;; 4) quote is not a special keyword in eval, it is a procedure macro
;;;;    which wraps its argument in "quote data" which has its own predicate.
;;;;    When eval later sees quote data, it simply unwraps it.
;;;;    So whilst quote is still required by the core,
;;;;    it is not hardcoded and is first-class like "if".
;;;; 4) Doesn't have TCO. Damn. I'd love to add this if I can do it cleanly.
;;;; 5) No continuations, I have a very weak grasp of them.
;;;;
;;;; Todo:
;;;; TCO?, polymorphic car and cdr axioms, parameterised lambda list to dicts,
;;;; move as much as possible into the lisp env,
;;;; More polymorphism, more ability for lisp to work on  lisp.
;;;;
;;;; My positioning of the environment argument is inconsistent and I'm sorry
;;;; for that, but it just seemed the most natural way to roll.
;;;;

(define (parallel-map func lst)
  (map func lst))

;;; Abstract assoc data

(define *not-found-value* 'undefined)

(define (not-found? x)
  (eq? x *not-found-value*))

(define (found? x)
  (not (not-found? x)))

(define (assoc-ref d n)
  (assoc-ref-nfv d n *not-found-value*))

(define (obj-ref d n)
  (obj-ref-nfv d n *not-found-value*))

(define (list-cond d n)
  (list-cond-nfv d n *not-found-value*))

;;; Environments

(define (bind-pair-to-scope x acc)
  (bind-to-scope acc (pseudo-car x) (pseudo-cdr x)))

(define (bind-to-scope sc ll args)
  (if (list? ll)
    (foldl bind-pair-to-scope sc (pair ll args))
    (assoc-add sc ll args)))

;; bind-to-scope should become parameterised so the func
;; can specify its own list -> dict function on args.
(define (with-scope func args)
  (pseudo-eval
    (obj-ref func 'data-value)
    (pseudo-cons
      (bind-to-scope nil (obj-ref func 'lambda-list) args)
      (obj-ref func 'environment))))

;;; Data type testers

(define (of-pseudo-type? t x)
  (and
    (obj? x)
    (let ((ty (obj-ref x 'pseudo-type)))
      (and
        (found? ty)
        (eq? ty t)))))

(defmacro define-tester (d t)
  `(define (,d n) (of-pseudo-type? ,t n)))

(define-tester auto? 'auto)
(define-tester quote? 'quote)
(define-tester procedure-macro? 'procedure-macro)
(define-tester procedure-special? 'procedure-special)
(define-tester pseudo-macro? 'pseudo-macro)
(define-tester pseudo-lambda? 'pseudo-lambda)

(define (atom? x)
  (not (list? x)))

;;; Clause type evaluators

(define (clause-procedure-macro func args env)
  (pseudo-eval (apply (obj-ref func 'data-value) args) env))

(define (clause-procedure-special func args env)
  (apply (obj-ref func 'data-value)
    (pseudo-cons env (parallel-map (partial-right pseudo-eval env) args))))

(define (clause-procedure func args env)
  (apply func (parallel-map (partial-right pseudo-eval env) args)))

(define (clause-pseudo-macro orcam args env)
  (pseudo-eval (with-scope orcam args) env))

;; The order in which the args are mapped is unspecified.
(define (clause-pseudo-lambda func args env)
  (with-scope func (parallel-map (partial-right pseudo-eval env) args)))

(define (clause-string str args env)
  (string-ref str (first args)))

(define (clause-symbol sym args env)
  (let ((func (pseudo-get env sym)))
    (if (found? func)
      (eval-compound (pseudo-cons func args) env)
      (error "Clause function not found: " sym))))

(define *clause-types*
  (list
    (cons pseudo-macro? clause-pseudo-macro)
    (cons pseudo-lambda? clause-pseudo-lambda)
    (cons symbol? clause-symbol)
    (cons string? clause-string)
    (cons procedure-special? clause-procedure-special)
    (cons procedure-macro? clause-procedure-macro)
    (cons procedure? clause-procedure)
    (cons tautology identity)))

;;; Data type evaluators

(define (eval-quote x env)
  (obj-ref x 'data-value))

(define (eval-auto x env)
  (pseudo-get env (obj-ref x 'data-value)))

(define (eval-compound x env)
  (let ((head (pseudo-eval (pseudo-car x) env)))
    ((list-cond *clause-types* head) head (pseudo-cdr x) env)))

(define *data-types*
  (list
    (cons auto? eval-auto)
    (cons quote? eval-quote)
    (cons obj? identity)
    (cons atom? identity)
    (cons tautology eval-compound)))

;;; Axioms

(defmacro defprocspecial (name args . forms)
  `(define ,name
     (make-obj
       'pseudo-type 'procedure-special
       'data-value (lambda ,args ,@forms))))

(defmacro defprocmacro (name args . forms)
  `(define ,name
     (make-obj
       'pseudo-type 'procedure-macro
       'data-value (lambda ,args ,@forms))))

(defprocspecial pseudo-current-environment (env)
  env)

(defprocmacro pseudo-if (env test then els)
  (if (pseudo-eval test env)
    then
    els))

(defprocmacro pseudo-quote (x)
  (make-obj
    'pseudo-type 'quote
    'data-value x))

(define (pseudo-cons x y)
  (cons x y))

(define (pseudo-car x)
  (if (obj? x)
    (pseudo-car (obj-ref x 'data-value))
    (car x)))

(define (pseudo-cdr x)
  (if (obj? x)
    (pseudo-cdr (obj-ref x 'data-value))
    (cdr x)))

(define (pseudo-get scopes name)
  (let ((i (assoc-ref (pseudo-car scopes) name)))
    (if (found? i) i
      (let ((p (pseudo-cdr scopes)))
        (if (null? p)
          (eval name)
          (pseudo-get p name))))))

(define (pseudo-define sco nam dat)
  (set-car! sco (pseudo-cons (pseudo-cons nam dat) (pseudo-car sco))))

;;; eval

(define (pseudo-eval x env)
  ((list-cond *data-types* x) x env))

;;; Build an environment

(define *core-env* (list nil))

;; Requires:
;; pseudo-define
;; pseudo-current-environment
;; make-obj
;; pseudo-get - This wouldn't be required if we used auto's
;; pseudo-quote
;; pseudo-cons
;; nil
;; null?
;; pseudo-if

;; Probably also requires:
;; pseudo-set!
;; atom?
;; eq?

(map
  (partial-right pseudo-eval *core-env*)
  `(
     
     ;; Build a macro to build macro's and call it macro.
     (pseudo-define
       (pseudo-current-environment)
       macro
       (make-obj
         pseudo-type pseudo-macro
         data-value
         (pseudo-quote
           (pseudo-cons
             make-obj
             (pseudo-cons
               pseudo-type
               (pseudo-cons
                 pseudo-macro
                 (pseudo-cons
                   data-value
                   (pseudo-cons
                     (pseudo-cons
                       pseudo-quote
                       (pseudo-cons
                         (pseudo-get (pseudo-current-environment) form)
                         (pseudo-get (pseudo-current-environment) nil)))
                     (pseudo-cons
                       lambda-list
                       (pseudo-cons
                         (pseudo-cons
                           pseudo-quote
                           (pseudo-cons
                             (pseudo-get (pseudo-current-environment) args)
                             (pseudo-get (pseudo-current-environment) nil)))
                         (pseudo-cons
                           environment
                           (pseudo-cons
                             (pseudo-quote (pseudo-current-environment))
                             (pseudo-get (pseudo-current-environment) nil)))))))))))     
         lambda-list (pseudo-quote (args form))
         environment (pseudo-current-environment)))
     
     ;; Build a macro to define macros and call it defmac.
     (pseudo-define
       (pseudo-current-environment)
       defmac
       (macro (name args form)
         (pseudo-cons
           pseudo-define
           (pseudo-cons
             (pseudo-quote (pseudo-current-environment))
             (pseudo-cons
               (pseudo-get (pseudo-current-environment) name)
               (pseudo-cons
                 (pseudo-cons
                   macro
                   (pseudo-cons
                     (pseudo-get (pseudo-current-environment) args)
                     (pseudo-cons
                       (pseudo-get (pseudo-current-environment) form)
                       (pseudo-get (pseudo-current-environment) nil))))
                 (pseudo-get (pseudo-current-environment) nil)))))))
     
     ;; A macro to build lambda's
     (defmac fn (args form)
       (pseudo-cons
         make-obj
         (pseudo-cons
           pseudo-type
           (pseudo-cons
             pseudo-lambda
             (pseudo-cons
               data-value
               (pseudo-cons
                 (pseudo-cons
                   pseudo-quote
                   (pseudo-cons
                     (pseudo-get (pseudo-current-environment) form)
                     (pseudo-get (pseudo-current-environment) nil)))
                 (pseudo-cons
                   lambda-list
                   (pseudo-cons
                     (pseudo-cons
                       pseudo-quote
                       (pseudo-cons
                         (pseudo-get (pseudo-current-environment) args)
                         (pseudo-get (pseudo-current-environment) nil)))
                     (pseudo-cons
                       environment
                       (pseudo-cons
                         (pseudo-quote (pseudo-current-environment))
                         (pseudo-get (pseudo-current-environment) nil)))))))))))
     
     (defmac def (name args form)
       (pseudo-cons
         pseudo-define
         (pseudo-cons
           (pseudo-quote (pseudo-current-environment))
           (pseudo-cons
             (pseudo-get (pseudo-current-environment) name)
             (pseudo-cons
               (pseudo-cons
                 fn
                 (pseudo-cons
                   (pseudo-get (pseudo-current-environment) args)
                   (pseudo-cons
                     (pseudo-get (pseudo-current-environment) form)
                     (pseudo-get (pseudo-current-environment) nil))))
               (pseudo-get (pseudo-current-environment) nil))))))
     
     ;; The above code would be a lot simpler if I had defined this
     ;; as part of the language, but I'm a stubborn guy.
     (def list args
       (pseudo-get (pseudo-current-environment) args))
     
     (defmac var (name value)
       (list pseudo-define
         (pseudo-quote (pseudo-current-environment))
         (pseudo-get (pseudo-current-environment) name)
         (pseudo-get (pseudo-current-environment) value)))
     
     (defmac get (name)
       (list pseudo-get
         (pseudo-quote (pseudo-current-environment))
         (pseudo-get (pseudo-current-environment) name)))
     
     (var q (get pseudo-quote))
     (var current-env (get pseudo-current-environment))
     (var cons (get pseudo-cons))
     (var first (get pseudo-car))
     (var rest (get pseudo-cdr))
     
     (defmac if (test then else)
       (list pseudo-if
         (pseudo-quote (pseudo-current-environment))
         (get test)
         (get then)
         (get else)))
     
     (def map (func lst)
       (if (null? (get lst))
         (list)
         (cons ((get func) (first (get lst)))
           (map (get func) (rest (get lst))))))
     
     ))

(define *user-env* (cons nil *core-env*))

