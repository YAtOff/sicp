#lang racket

(require berkeley)
(provide (all-defined-out))

; Exercise 1 - Define describe-time
(define seconds-in-minute 60.0)
(define seconds-in-hour (* 60 seconds-in-minute))
(define seconds-in-day (* 24 seconds-in-hour))
(define (div-time parts secs)
  (if (empty? parts)
    (se secs 'SECONDS)
    (se (quotient secs (first (first parts))) (last (first parts))
        (div-time (bf parts) (remainder secs (first (first parts))))))
)
(define (describe-time secs)
  (div-time (se 
              (se seconds-in-day 'DAYS)
              (se seconds-in-hour 'HOURS)
              (se seconds-in-minute 'MINUTES)) secs)
)

; Exercise 2 - Define remove-once
(define (remove-once wd sent)
  (if (empty? sent)
    sent
    (if (equal? (first sent) wd)
      (bf sent)
      (sentence (first sent) (remove-once wd (bf sent)))))
)

; Exercise 3 - Define differences
(define (diff-sents sent1 sent2)
  (if (empty? sent2)
    '()
    (sentence (- (first sent2) (first sent1))
       (diff-sents (bf sent1) (bf sent2))))
)
(define (differences nums)
  (diff-sents nums (bf nums))
)

; Exercise 4 - Define location
(define (find-location wd sent current)
  (if (empty? sent)
    #f
    (if (equal? wd (first sent))
      current
      (find-location wd (bf sent) (+ 1 current))))
)
(define (location small big)
  (find-location small big 1)
)

; Exercise 5 - Define initials
(define (initials sent)
  (if (empty? sent)
    '()
    (se (first (first sent)) (initials (bf sent))))
)

; Exercise 6 - Define copies
(define (copies num wd)
  (if (= 0 num)
    '()
    (se wd (copies (- num 1) wd)))
)

; Exercise 7 - Define gpa
(define (base-grade grade)
  (- (location grade '(E D C B A)) 1))
(define (grade-modifier sign)
  (cond ((equal? sign '-) -0.33)
        ((equal? sign '+) 0.33)
        (else 0)))
(define (sum-grades grades)
  (if (empty? grades)
    0
    (+ (base-grade (first (first grades))) (grade-modifier (last (first grades)))
       (sum-grades (bf grades))))
)
(define (gpa grades)
  (if (empty? grades)
    0
    (/ (sum-grades grades) (length grades)))
)

; Exercise 8 - Define repeat-words
(define (mul-word word times)
  (if (equal? times 0)
    '()
    (se word (mul-word word (- times 1))))
)
(define (repeat-words sent)
  (if (empty? sent)
    '()
    (if (number? (first sent)) 
      (se (mul-word (first (bf sent)) (first sent)) (repeat-words (bf (bf sent))))
      (se (first sent) (repeat-words (bf sent)))))
)

; Exercise 9 - Define same-shape?
(define (same-word-lengths sent1 sent2)
  (if (empty? sent1)
    #t
    (and (equal? (length (first sent1)) (length (first sent2)))
         (same-word-lengths (bf sent1) (bf sent2))))
)
(define (same-shape? sent1 sent2)
  (and (equal? (length sent1) (length sent2))
       (same-word-lengths sent1 sent2))
)
