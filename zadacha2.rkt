#lang racket
(define (removelast xs)
(cond
((null? (cdr xs)) '())
(else (cons (car xs) (removelast (cdr xs))))))
 
  
(define (prefixes xs)
  (cond
    ((null? xs) (list xs))
    (else  (append  (prefixes (removelast xs)) (list xs)))))
;;test
(require rackunit rackunit/text-ui)
(define prefixes-tests
  (test-suite "prefixes"
    (test-case "empty list" (check-equal? (prefixes '()) '(())))
    (test-case "123" (check-equal? (prefixes '(1 2 3)) '(() (1) (1 2) (1 2 3))))
    (test-case "deep list" (check-equal? (prefixes '((1 2) 3 4)) '(() ((1 2)) ((1 2) 3) ((1 2) 3 4)) ))
  )
)
(run-tests prefixes-tests 'verbose)

