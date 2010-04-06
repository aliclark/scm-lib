
;;;; Copyright (c) 2009 Ali Clark
;;;; 
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;; 
;;;; Except as contained in this notice, the name(s) of the above
;;;; copyright holders shall not be used in advertising or otherwise to
;;;; promote the sale, use or other dealings in this Software without
;;;; prior written authorization.

;;;; Basic IO Monad for Scheme. ;;;;
;;;;
;;;; Contains just enough Haskell implementation
;;;; to implement getLine and putStrLn properly in Scheme.
;;;;
;;;; > (hello 0)
;;;; Hello, World!
;;;; (#!void . 14)
;;;; > (cat 0)
;;;; This is a line.
;;;; This is a line.
;;;; (#!void . 33)

;;; Language features ;;;

(define (partial f . early) (lambda late (apply f (append early late))))

;;; Misc library code ;;;

;(:: Unit ())
(define Unit (cond (#f #f)))

;(:: seq (-> a b b))
(define (seq a b) b)

;(:: == (=> ((Eq a)) a a Bool))
(define == equal?)

;(:: inc (=> ((Num a)) (-> a a)))
(define inc (partial + 1))

;(:: foldr (-> (-> a b b) b (List a) b))
(define (foldr f z xs) (if (null xs) z (f (head xs) (foldr f z (tail xs)))))

;;; Lists ;;;

;(:: Nil (List a))
(define Nil (list))

;(:: : (-> a (List a) (List a)))
(define :    cons)

;(:: tail (-> (List a) (List a)))
(define tail cdr)

;(:: head (-> (List a) a))
(define head car)

;(:: null (-> (List a) Bool))
(define null (partial == Nil))

;;; Tuples ;;;

;(:: Tup (-> a b (Tup a b)))
(define Tup cons)

;(:: fst (-> (Tup a b) a))
(define fst car)

;(:: snd (-> (Tup a b) b))
(define snd cdr)

;;; Monads ;;;

;(:: make->>
;  (=> ((Monad m)) (-> (-> (m a) (-> a (m b)) (m b)) (m a) (m b) (m b))))
(define (make->> f) (lambda (m1 m2) (f m1 (lambda (x) m2))))

;(:: make-sequence_
;  (=> ((Monad m)) (-> (-> (m a) (m b) (m b)) (-> b (m b)) (List (m a))
;    (m ()))))
(define (make-sequence_ f u) (partial foldr f (u Unit)))

;(:: make-mapM_
;  (=> ((Monad m)) (-> (-> (List (m b)) (m ())) (-> a (m b)) (List a) (m ()))))
(define (make-mapM_ s) (lambda (f as) (s (map f as))))

;;; Handles ;;;

;(:: stdin Handle)
(define stdin  (current-input-port))

;(:: stdout Handle)
(define stdout (current-output-port))

;;; IO Monad ;;;

;(:: IO.return (-> a (IO a)))
(define (IO.return x) (lambda (r) (Tup x r)))

;(:: IO.>>= (-> (IO a) (-> a (IO b)) (IO b)))
(define (IO.>>= m f)  (lambda (r) (let ((v1 (m r))) ((f (fst v1)) (snd v1)))))

;(:: IO.>> (-> (IO a) (IO b) (IO b)))
(define IO.>>        (make->> IO.>>=))

;(:: IO.sequence_ (-> (List (IO a)) (IO ())))
(define IO.sequence_ (make-sequence_ IO.>> IO.return))

;(:: IO.mapM_ (-> (-> a (IO b)) (List a) (IO ())))
(define IO.mapM_     (make-mapM_ IO.sequence_))

;;; Handle Input ;;;

;(:: hGetChar (-> Handle (IO Char)))
(define (hGetChar h) (lambda (r) (Tup (read-char h) (inc r))))

;; Unfortunately some Scheme implementations will return a new line first if
;; used at the REPL, so we first check if the first character is a new line and
;; if so discard it.
;; This could cause hGetLine to differ from the Haskell version if the user
;; enters a newline as the first character.

;(:: hGetLine (-> Handle (IO String)))
(define (hGetLine h)
  (define (hGetLine- h)
    (IO.>>= (hGetChar h)
      (lambda (c)
        (if (== c #\newline) (IO.return Nil)
          (IO.>>= (hGetLine- h) (lambda (l) (IO.return (: c l))))))))
  (hGetLine- h))

;(:: hGetLine2 (-> Handle (IO String)))
(define (hGetLine2 h)
  (IO.>>= (hGetLine h) (lambda (l) (if (null l) (hGetLine h) (IO.return l)))))

;;; Standard Input ;;;

;(:: getChar (IO Char))
(define getChar  (partial hGetChar  stdin))

;(:: getLine (IO String))
(define getLine  (partial hGetLine  stdin))

;(:: getLine2 (IO String))
(define getLine2 (partial hGetLine2 stdin))

;;; Handle Output ;;;

;(:: hPutChar (-> Handle Char (IO ())))
(define (hPutChar h c)  (lambda (r) (seq (write-char c h) (Tup Unit (inc r)))))

;(:: hPutStr (-> Handle String (IO ())))
(define (hPutStr h s)   (IO.mapM_ (partial hPutChar h) s))

;(:: hPutStrLn (-> Handle String (IO ())))
(define (hPutStrLn h s) (IO.>> (hPutStr h s) (hPutChar h #\newline)))

;;; Standard Output ;;;

;(:: putChar (-> Char (IO ())))
(define putChar  (partial hPutChar  stdout))

;(:: putStr (-> String (IO ())))
(define putStr   (partial hPutStr   stdout))

;(:: putStrLn (-> String (IO ())))
(define putStrLn (partial hPutStrLn stdout))

;;; Testing functions ;;;

;(:: cat (IO ()))
(define cat   (IO.>>= (getLine2) putStrLn))

;(:: hello (IO ()))
(define hello (putStrLn (string->list "Hello, World!")))

