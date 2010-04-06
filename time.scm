
;;; Deps: none

(define (a-time? t)
  (and
    (list? t)
    (= (length t) 3)
    (equal? (car t) 'time)))

(define (a-time hour min)
  (if (and
        (integer? hour)
        (integer? min)
        (>= hour 0)
        (<= hour 23)
        (>= min 0)
        (<= 59))
    (list 'time hour min)
    (error "Hours and/or minutes not of required type in a-time: " hour min)))

(define (hours t)
  (if (a-time? t)
    (cadr t)
    (error "Cannot select hours, as argument is not time: " t)))

(define (minutes t)
  (if (a-time? t)
    (caddr t)
    (error "Cannot select minutes, as argument is not time: " t)))

(define (no-later-than? t1 t2)
  (or
    (< (hours t1) (hours t2))
    (and
      (= (hours t1) (hours t2))
      (<= (minutes t1) (minutes t2)))))

(define (add-minutes time min)
  (if (< (+ (remainder min 60) (minutes time)) 60)
    (a-time
      (+ (hours time) (quotient min 60)) 
      (+ (minutes time) (remainder min 60)))
    (a-time
      (+ (hours time) (quotient min 60) 1) 
      (remainder (+ (remainder min 60) (minutes time)) 60))))

(define (within-range? min t1 t2)
  (or
    (and
      (no-later-than? t1 t2)
      (no-later-than? t2 (add-minutes t1 min)))
    (and
      (no-later-than? t2 t1)
      (no-later-than? t1 (add-minutes t2 min)))))

(define (within-30? t1 t2)
  (within-range? 30 t1 t2))

