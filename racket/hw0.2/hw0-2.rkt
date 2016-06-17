#lang racket

(require berkeley)
(provide (all-defined-out))

;Exercise 0
;Write 5 expressions whose values are the number ten:
;1. Atom

;2. Compound Expression (3 Atoms)

;3. Compound Expression (4 Atoms)

;4. Compound Expression (1 Atom and 2 subexpressions)

;5. Any Other Kind Expression


;Exercise 1
(define (second wd)
  (first (bf wd)))

;1. Define first-two
(define (first-two wd)
  (word (first wd) (second wd))
)

;;2. Define two-first
(define (two-first x y)
  (word (first x) (first y))
)

;;3. Define two-first-sent
(define (two-first-sent sent)
  (word (first (first sent)) (first (second sent)))
)

;Exercise 2 - Define teen?
(define (teen? num)
  (and (<= 13 num) (<= num 19))
)

;Exercise 3 - Define indef-article
(define (vowel? letter)
  (member? letter 'aeiou))
(define (indef-article wd)
  (sentence (if (vowel? (first wd)) 'an 'a) wd)
)

;Exercise 4 - Define insert-and
(define (insert-and sent)
  (sentence (butlast sent) 'and (last sent))
)

;Exercise 5 - Define query
(define (query sent)
  (sentence (second sent) (first sent) (bf (bf (bl sent))) (word (last sent) '?))
)

;Exercise 6 - Define european-time and american-time
(define (european-time time)
  (if (equal? (second time) 'am)
    (remainder (first time) 12)
    (- (+ (first time) 12) (if (= (first time) 12) 12 0)))
)

(define (american-time time)
  (cond ((= time 0) '(12 am))
        ((= time 12) '(12 pm))
        ((< time 12) (sentence time 'am))
        (else (sentence (- time 12) 'pm)))
)

;Exercise 7 - Define describe-time
(define (describe-time secs)
  (if (< secs 60) (sentence secs 'seconds)
    (if (< secs 3600) (sentence (/ secs 60.0) 'minutes)
      (if (< secs (* 3600 24)) (sentence (/ secs 3600.0) 'hours)
        (sentence (/ secs (* 3600.0 24)) 'days)))))

;Exercise 8 - Explain why superlative doesnt work:
(define (superlative adjective wd)
  (se (word adjective 'est) wd))

#|

Explanation here.

|#
