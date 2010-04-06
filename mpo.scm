
;;; Deps: num,list

;;; Commonly used MPO macros

(define-macro (wrap x)
  `(lambda () ,x))

(define-macro (auto x)
  `(,x))

(define-macro (alias x)
  x)

(define-macro (inline x code)
  code)

;;; Message-passing objects

(define-macro (setter x . rest)
  (let* ((vname (if (not-null? rest) (car rest) 'v))
          (vformals (if (not-null? rest) '() `(,vname)))
          (rvval
            (if (and (not-null? rest)
                  (len-check rest 2))
              (cadr rest)
              vname)))
    `(lambda ,vformals
       (set! ,x ,vname)
       ,rvval)))

(define (exposed-builder transformer names)
  `(begin
     ,@(map
         (lambda (a)
           (let ((a-list? (list? a)))
             (transformer (if a-list? (car a) a)
               (if a-list? (cadr a) a))))
         names)))

(define (exposed-member-builder inner outer)
  `(define (,outer obj)
     (obj ',inner)))

(define (exposed-method-builder inner outer)
  `(define (,outer obj . args)
     (apply (obj ',inner) args)))

(define-macro (expose-members . names)
  (exposed-builder exposed-member-builder names))

(define-macro (expose-methods . names)
  (exposed-builder exposed-method-builder names))

(define (alias-builder message invar alias-part)
  (let ((alias-list? (list? alias-part)))
    `((eq? ,message
        ',(if alias-list?
            (car alias-part)
            alias-part))
       ,(if alias-list?
          (let ((action-part (cadr alias-part)))
            (if (list? action-part)
              `(,(car action-part)
                 ,invar
                 ,@(cdr action-part))
              `(,action-part ,invar)))
          invar))))

(define (var-list-builder invar alias-part)
  (let ((alias-list? (list? alias-part)))
    `(list ',(if alias-list?
               (car alias-part)
               alias-part)
       ,(if alias-list?
          (let ((action-part (cadr alias-part)))
            (if (list? action-part)
              `(,(car action-part)
                 ,invar
                 ,@(cdr action-part))
              `(,action-part ,invar)))
          invar))))

;; (varname (alias modifier) ...)
;; ((varname modifier)) -> (varname (varname modifier))
;; symbol -> ((symbol access))
(define (cond-expose message x)
  (if (list? x)
    (let* ((varpart (car x))
            (varpart-list? (list? varpart)))
      (map (partial alias-builder
             message
             (if varpart-list? (car varpart) varpart))
        (if varpart-list? x (cdr x))))
    `(((eq? ,message ',x) ,x))))

(define (list-instance-names entry)
  (if (list? entry)
    (let* ((var-part (car entry))
            (var-part-list? (list? var-part)))
      (map (partial var-list-builder
             (if var-part-list?
               (car var-part)
               var-part))
        (if var-part-list? entry (cdr entry))))
    (list `(list ',entry ,entry))))

(define (names-only public)
  (if (list? public)
    (map car
      (if (list? (car public))
        public
        (cdr public)))
    (list public)))

(define (var-only public)
  (if (list? public)
    (let ((var-part (car public)))
      (if (list? var-part)
        (car var-part)
        var-part))
    public))

(define-macro (expose name . publics)
  (let ((names (apply append (map names-only publics)))
         (vars (map var-only publics))
         (message (gensym)))
    `(define (,name ,message)
       (cond
         ,@(apply append
             (map (partial cond-expose message) publics))
         ((eq? ,message 'instance-names) ',names)
         ((eq? ,message 'respond-to?)
           (partial-right memq-boolean ',names))
         ((eq? ,message 'to-string)
           (lambda ()
             (object->string
               (list
                 ',name
                 ,@(apply append (map list-instance-names publics))))))
         (else (error "This object only responds to the following:"
                 ',names))))))

(define-macro (export name . publics)
  `(begin
     (expose ,name ,@publics)
     ,name))

(define-macro (export-this . publics)
  `(export this ,@publics))

