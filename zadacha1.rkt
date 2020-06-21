#lang racket
(define (one . params)
  (cond ((null? params) 1)
    (else (define func (caar params)) (define number (cdar params))
          ( func (one) number))))  
(define (two . params)
  (cond ((null? params) 2)
    (else (define func (caar params)) (define number (cdar params))
          ( func (two) number))))
(define (three . params)
  (cond ((null? params) 3)
    (else (define func (caar params)) (define number (cdar params))
          ( func (three) number))))
(define (four . params)
  (cond ((null? params) 4)
    (else (define func (caar params)) (define number (cdar params))
          ( func (four) number))))
(define (five . params)
  (cond ((null? params) 5)
    (else (define func (caar params)) (define number (cdar params))
          ( func (five) number))))
(define (six . params)
  (cond ((null? params) 6)
    (else (define func (caar params)) (define number (cdar params))
          ( func (six) number))))
(define (seven . params)
  (cond ((null? params) 7)
    (else (define func (caar params)) (define number (cdar params))
          ( func (seven) number))))
(define (eight . params)
  (cond ((null? params) 8)
    (else (define func (caar params)) (define number (cdar params))
          ( func (eight) number))))
(define (nine . params)
  (cond ((null? params) 9)
    (else (define func (caar params)) (define number (cdar params))
          ( func (nine) number))))
          
(define (plus a) (cons + a))
(define (time a) (cons * a))
(define (div a) (cons / a))
(define (minus a) (cons - a))

;;tests
(require rackunit rackunit/text-ui)
(define digit-tests
  (test-suite "digit"
     (test-case "9" (check-eq? (nine) 9))
     (test-case "1" (check-eq? (one) 1))
     (test-case "5" (check-eq? (five) 5))))  
(run-tests digit-tests 'verbose)

(define +tests
  (test-suite "+"
     (test-case "2+1" (check-eq? (two (plus (one))) 3))
     (test-case "3+4" (check-eq? (three (plus (four))) 7))
     (test-case "8+7" (check-eq? (eight (plus (seven))) 15))))
(run-tests +tests 'verbose)

(define -tests
  (test-suite "-"
     (test-case "2-2" (check-eq? (two (minus (two))) 0))
     (test-case "3-5" (check-eq? (three (minus (five))) -2))
     (test-case "9-7" (check-eq? (nine (minus (seven))) 2)))) 
(run-tests -tests 'verbose)

(define time-tests
  (test-suite "time"
     (test-case "2*2" (check-eq? (two (time (two))) 4))
     (test-case "4*7" (check-eq? (four(time (seven))) 28))))
(run-tests time-tests 'verbose)

(define div-tests
  (test-suite "div"
     ;; some random calcs
     (test-case "8/8" (check-eq? (eight (div (eight))) 1))
     (test-case "6/2" (check-eq? (six (div (two))) 3))))

(run-tests div-tests 'verbose)




