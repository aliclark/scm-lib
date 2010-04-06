
(define (n nth wrd)
  (car ((repeated cdr nth) wrd)))

(define (range-list start end)
  (if (= start end)
    '()
    (cons start (range (+ start 1) end))))

;;; DSL macros

(define (to-string x)
  (cond
    ((string? x) x)
    ((symbol? x) (symbol->string x))
    ((number? x) (number->string x))))

(define (notted var)
  (symb "¬" var))

(define (symb . vars)
  (string->symbol (apply string-append (map to-string vars))))

(define (it-or-first x)
  (if (list? x) (car x) x))

(define (line-vars var nth)
  (let ((vbit (symb var nth)))
    `((,vbit (n ,nth ,var))
      (,(notted vbit) (not ,vbit)))))

(define (word-line-vars-flat x)
  (apply append
    (map (partial line-vars (car x))
      (range-list 0 (cadr x)))))

(define-macro (word-circuit formals form)
  (let ((args (gensym)) (form-name (car formals)))
    `(begin
       (define ,(map it-or-first formals)
         (let ,(apply append
                 (map word-line-vars-flat
                   (filter list? (cdr formals))))
           ,form))
       (define (,(notted form-name) . ,args)
         (not (apply ,form-name ,args))))))

(define-macro (sub-circuit name form)
  `(begin
     (define-macro (,name) ',form)
     (define-macro (,(notted name)) '(not (,name)))))

;;; DSL procedures

(define (logic-and . rest)
  (if (null? rest)
    #t
    (if (car rest) (apply logic-and (cdr rest)) #f)))

(define (logic-or . rest)
  (if (null? rest)
    #f
    (if (car rest) #t (apply logic-and (cdr rest)))))

(define plus +)
(define times *)

(define (+ . args)
  (if (number? (car args)) (apply plus args) (apply logic-or args)))

(define (* . args)
  (if (number? (car args)) (apply times args) (apply logic-and args)))

;;; Aliases

(define t #t)
(define f #f)
(define ¬ not)
(define word list)
(define values list)

;;; Logical functions

(define (nand x y)
  (not (and x y)))

(define (nor x y)
  (not (or x y)))

(define (xor x y)
  (or
    (and x (not y))
    (and (not x) y)))

(define (xnor x y)
  (not (xor x y)))

