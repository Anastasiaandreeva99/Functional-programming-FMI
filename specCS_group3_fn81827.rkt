#lang racket
;Задача -1-a 
(define (sum-common-divisors a b)
     (define (help i)
       (cond ((= i 0) 0)
         ((and (=(remainder a i)0) (=(remainder b i)0)) (+ i (help (- i 1))))
            (else (help (- i 1))))
       )
       (if (> a b) (help a) (help b))
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Задача-1-б
;връща максималния сбор на делители на дадено число с числата по-големи от него но по-малки от b 
(define (helpfunc curr b)
  (define (Max max i)
   (cond ((> i b) max)
         ((>(sum-common-divisors curr i) max) (Max (sum-common-divisors curr i) (+ 1 i)))
         (else (Max max (+ 1 i)))
                           ) )
  (Max 0 (+ 1 curr))
  )
 ;za vseki element ot intervala izvikva helpfunc,koiato vrushta maksimalnia sbor ,ako toi e po-goliam ot segashnia maks izvikvame funkciata s novoto maks
(define (greatestsum a b)
  (define (help maxx a)  
  (cond ((> a b) maxx)
  ( (> (helpfunc a b) maxx)(help (helpfunc a b) (+ a 1)))
        (else (help maxx (+ a 1)))   
    ))
    (help 0 a)
  ) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;задача 2

(define (prod l) (apply * l))
(define (sum l) (apply + l))

;vrushta spisuk ot resultatite na metrikite za vseki podspisuk na ll
(define (help ll m)
(cond ((null? ll)'())
      (else (cons ( m (car ll)) (help (cdr ll) m)))  
  )
  )
;;;проверява дали резултатите от метриките са еднакви
(define (equalresult x l)
  (cond ((null? l) #t)
  ((not (= (car l) x)) #f)
  (else (equalresult x (cdr l)))
  )
  )
;;ако метриката дава равни резултати за всички списъци увеличаваме броя с 1
(define (count-metric metr ll)
   (cond ((null? metr)0)
         ((equalresult (car(help ll (car metr))) (help ll (car metr))) (+ 1 (count-metric (cdr metr) ll)))
         (else (count-metric (cdr metr) ll))
         )
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;задача3-ok
;проверяваме атом ли е
(define (atom? x) 
(and (not (null? x)) (not (pair? x)))
  )
;i брои колко вътрешен е елемента
(define (level-flatten l)
  (define (help2 i l)
    (cond ((null? l)'())
          ((atom? l) (list (+ l  i ))) 
          (else (append (help2 (+ i 1) (car l)) (help2  1 (cdr l))))
  )
      )
  (help2 0 l)
  )
