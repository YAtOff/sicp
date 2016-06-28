#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define dupls-removed

(define (word-removed w s)
  (if (empty? s)
    s
    (if (equal? w (first s))
      (word-removed w (bf s))
      (se (first s) (word-removed w (bf s)))))
)
(define (dupls-removed sent)
  (if (empty? sent)
    sent
    (se (first sent) (dupls-removed (word-removed (first sent) (bf sent)))))
)

; Exercise 2 - Define count-word

(define (count-word sent wd)
  (if (empty? sent)
    0
    (+
      (if (equal? wd (first sent)) 1 0)
      (count-word (bf sent) wd)))
)

; Exercise 3

(define (pigl wd)
  (if (pl-done? wd)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(define (pl-done? wd)
  (vowel? (first wd)))

(define (vowel? letter)
  (member? letter '(a e i o u)))

; Explain what would happen if you used new-if instead of if below.
#|
Infinite recursion

|#

; Exercise 4 - Define squares

(define (squares sent)
  (if (empty? sent)
    sent
    (se (* (first sent) (first sent))
        (squares (bf sent))))
)

; Exercise 5 - Define switch

(define (switch sent)
  (define (switch-word w)
    (cond 
      ((equal? w 'you) 'me)
      ((equal? w 'I) 'you)
      ((equal? w 'me) 'you)
      (else w))
  )

  (define (switch-sent sent)
    (cond 
      ((empty? sent) sent)
      (else (se (switch-word (first sent)) (switch-sent (bf sent)))))
  )

  (cond 
    ((empty? sent) sent)
    ((equal? (first sent) 'you) (se 'I (switch-sent (bf sent))))
    (else (switch-sent sent)))
)

; Exercise 6 - Define ordered?

(define (ordered? sent)
  (cond 
    ((< (length sent) 2) #t)
    (else (and 
            (< (first sent) (first (bf sent)))
            (ordered? (bf (bf sent))))))
)

; Exercise 7 - Define ends-e

(define (ends-e sent)
  (cond 
    ((empty? sent) sent)
    ((equal? (last (first sent)) 'e) (se (first sent) (ends-e (bf sent))))
    (else (ends-e (bf sent))))
)

; Exercise 8

#|

Your explanation here

(define (inf-rec)
  (if #t (inf-rec) #f))

(or #t (inf-rec))
(and #f (inf-rec))

|#
