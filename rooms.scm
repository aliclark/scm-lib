
#|

74 * 5 = 370



Q) What do we do if a bid for a house has two or more equal high bids?

Pick a new room from the hat and allocate it.

If there are no more rooms in the hat, or they are all ties,
the default is for a coin to be flipped, but if the concerned
people can sort it out, then that's cool.


Q) To randomise, or not to randomise?

The order should be random. This helps ensure people are entirely honest
about the value they place on rooms. If they are not honest, it could cost them.



Theories behind this system:

Mathematical Induction:

This system is recursive. To bid for a 4 room house, you first bid for a 3 room
house, then you bid for another room. When you bid for a 2 person house,
either 1 of you will have a higher bid, or you sort it out between you,
or you flip a coin. Once you agree that this is a fair way to allocate a 2 room
house, you inductively agree this is a fair method for allocating a 5 room house.
This means the system is not only mathematically sound, but it is mathematically
sound in a beautiful way.


Opportunity cost:

At each point in time, this system forces you to look at the opportunity cost
of living in one room over another, and rewards you for making the opportunity
costs for all rooms equivilant. What this means is that the final result for
you will be maximally beneficial, as it will also be for everyone else.






Why is this system not used in the real world?

2 simple and valid reasons:

a) It is not scalable. This system has O(n) complexity, which takes time.

b) We are looking at different scales. A real auction runs within the scope
of the real economy. It tries to maximise revenue for the owners in relation
to the rest of the economy's capacity.

In contrast, our mini auction has an entirely different capacity. The capacity
of our economy is £370. This might sound like a joke, but it isn't -
our mini auction really is in its own bubble, and shouldn't pretend to be
part of the large economy, lest the guise fall down.
Accordingly, my system gives each of the rooms some kind of ownership from
the outset by giving each person the full £370 during the auction.




My system is fair because it asks everybody what they feel the market value of
each room is, in relation to each of the other rooms.

It asks, "If there were five of you, and you had to allocate yourselfs evenly
in a house for a certain amount of money, how would you do it?".

You confer with your four other alter-egos and decide whether one of you
is getting a better deal than the other versions of you.

You say: "Heck, if I were in this room paying this price, I would be paying
too much in relation to my other selves."

This leads to upping the price slightly of one or more other rooms and lowering
the price of the over-valued rooms.

Once you have done this, you look at your five prices and say: "Yeah, if I got
this room for this price, I'd be a happy guy.".

You do that for all of the rooms. And here's the beautiful part:
You will get one of those prices, no matter what it might be.

-------------------------------------------------------------------------------

So now let's add bring out a more personal example. Let's say that you are
a fairly easy going guy. You value all the rooms more or less evenly, except
one, which you would really be happy with, and one which you absolutely can't
see yourself living in for the next year.

So to reflect this you have put one slightly above three others in value,
and one of them significantly lower.

But hey, remember, this is the real world, and we are not all going to go
in the slightly better than average room, and one of us will go in the less
than average room.

|#

(define true #t)
(define false #f)
;;(define random random-integer)

(define (any satisfies-test testee-list)
  (if (null? testee-list)
    false
    (if (satisfies-test (car testee-list))
      true
      (any satisfies-test (cdr testee-list)))))

(define (does-not? x)
  (lambda args
    (not (apply x args))))

(define (assoc-ref-nfv d n not-found-value)
  (if (null? d)
    not-found-value
    (let ((nex (car d)))
      (if (eq? (car nex) n)
        (cdr nex)
        (assoc-ref-nfv (cdr d) n not-found-value)))))

(define (sum numbers)
  (apply + numbers))

(define (pick-random lst)
  (list-ref lst (random (length lst))))

(define (partial function . early-args)
  (lambda late-args
    (apply function (append early-args late-args))))

(define empty? null?)

(define (filter test lst)
  (let loop ((acc '()) (la lst))
    (if (null? la)
      (reverse acc)
      (loop
        (if (test (car la))
          (cons (car la) acc)
          acc)
        (cdr la)))))



(define-macro (define-room name)
  (let ((price (gensym)))
    `(define (,name ,price)
      (room ',name ,price))))

(define-macro (define-person name choices)
  `(define ,name (person ',name ,choices)))

(define (room-price pers room-n)
  (assoc-ref-nfv (person-choices pers) room-n #f))

(define room   cons)
(define room-name car)
(define choices list)

(define person cons)
(define person-name car)
(define person-choices cdr)
(define people  list)

(define (person-sum x)
  (sum (map cdr (person-choices x))))

(define (person-name-text person)
  (symbol->string (person-name person)))

(define-room top-front-small)
(define-room top-front-medium)
(define-room top-garden-side)
(define-room bottom-front)
(define-room bottom-kitchen)

(define (verify total-rent person)
  (let ((person-sum (person-sum person)))
    (if (not (= person-sum total-rent))
      (begin
        (display
          (string-append (person-name-text person)
            "'s sum only added to " (number->string person-sum)))
        false)
      true)))

(define (best-bids room-name people)
  (let loop ((acc (list))
             (rem (map (lambda (x) (cons (person-name x) (room-price x room-name))) people)))
    (if (empty? rem)
      acc
      (loop
        (let ((nex (cdr (car rem))))
          (cond
            ((empty? acc) (list (car rem)))
            ((> nex (cdr (car acc))) (list (car rem)))
            ((= nex (cdr (car acc))) (cons (car rem) acc))
            (else acc)))
        (cdr rem)))))

(define (remove-choice room-n pers)
  (person (person-name pers)
    (filter
      (lambda (x)
        (not (eq? room-n (room-name x))))
      (person-choices pers))))

(define (allocate-room people total-rent)
  (define (sub people)
    (let ((the-choices (person-choices (car people))))
      (if (empty? the-choices)
        false
        (let* ((room (pick-random the-choices))
               (room-name (room-name room)))
          (let ((best-bids (best-bids room-name people)))
            (if (null? best-bids)
              (display "An error occurred.")
              (if (= (length best-bids) 1)
                (cons room-name best-bids)
                (or
                  (sub (map (partial remove-choice room-name) people))
                  (list room-name (pick-random best-bids)
                    'randomly-from
                    (map car best-bids))))))))))
  (if (any (does-not? (partial verify total-rent)) people)
    false
    (sub people)))






(define-person ali
  (choices
    (top-front-small  72)
    (top-front-medium 75)
    (top-garden-side  75)
    (bottom-front     75)
    (bottom-kitchen   73)))

;; Note that while bob can ensure he doesn't get the smaller room,
;; he must pay more for this.
(define-person bob
  (choices
    (top-front-small  20)
    (top-front-medium 90)
    (top-garden-side  90)
    (bottom-front     85)
    (bottom-kitchen   85)))


(allocate-room (people ali bob) 370)


























