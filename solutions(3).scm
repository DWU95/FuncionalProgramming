#lang racket

(require srfi/1) ;; list functions
(require srfi/13) ;; string functions
(require srfi/48) ;; format function

(define (1..100)
  (cdr (iota 101)))

(define (fizzbuzz alon)
  (for-each
   (lambda (n)
     (cond
      ((zero? (remainder n 15))
       (format #t "fizzbuzz\n"))
      ((zero? (remainder n 3))
       (format #t "fizz\n"))
      ((zero? (remainder n 5))
       (format #t "buzz\n"))
      (else
       (format #t "~a\n" n))))
   alon))

;;(fizzbuzz (1..100))


(define (removefoo l)
  (remove (lambda (w) (string-prefix-ci? "foo" w)) l))

;;(removefoo '("foobar" "barfoo" "fooey"))

(define (foopet)
  (define (moo)
    "Moooooo")
  (define (bark)
    "Bark")
  (define (growl)
    "growl")
  (define (action m)
    (cond ((eq? m 'poke) (moo))
          ((eq? m 'stroke) (bark))
          ((eq? m 'kick) (growl))
          (else
           (error "Unknown action" m))))
  action)

;; ((foopet) 'kick)