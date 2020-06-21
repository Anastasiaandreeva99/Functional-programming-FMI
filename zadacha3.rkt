#lang racket
;;Имплементация на дата
;;1.make-date
(define(make-date day month year)
  (list day month year)) 
(define (day date)
  (car date)) 
(define (month date)
(cadr date))  
(define (year date)
(caddr date))  
(define (visokosna year)
 (or (and  (=(remainder year 4)0) (not(=(remainder year 100)0))) (=(remainder year 400)0)))  
(define (validatemonth2v date)
(and (< date 30) (> date 0)))  
(define (validatemonth2 date)
(and (< date 29) (> date 0)))  
(define (validatemonth31 date)
(and (< date 32) (> date 0)))  
(define (validatemonth30 date)
(and (< date 31) (> date 0))) 
(define (month30? Month)
   (if (or (= Month 11) (= Month 4) (= Month 6) (= Month 9)) #t #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;2.date?
(define(date? data)
  (define Month (month data))
  (define Day (day data))
  (define Year (year data))
   (and (< Month 13) (> Month 0) (> Day 0)
       (cond ((= Month 2) (if (visokosna Year) (validatemonth2v Day) (validatemonth2 Day)))
             ((month30? Month) (validatemonth30 Day))
             (else (validatemonth31 Day)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;Работа с дати
;;3.date-string
(define (Truemonth Month)
   (if (and (< Month 13) (> Month 0))  Month  1))  ;;if the input is not correct
(define (Trueday Day Month Year)
   (cond ((and (= Month 2) (visokosna Year) (validatemonth2v Day)) Day)
          ((and (= Month 2) (not(visokosna Year)) (validatemonth2 Day)) Day)
         ((and(month30? Month) (validatemonth30 Day)) Day)
         ((and(not(month30? Month)) (not (= Month 2)) (validatemonth31 Day)) Day)
         (else 1)))

(define (date-string data)
  (string-append (number->string (Trueday (day data) (month data) (year data)))
                 (string #\.)  (number->string (Truemonth(month data))) (string #\.)
                 (number->string ( year data))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;5.date<
 (define (date< data1 data2)
     (cond
       ((> (year data1) (year data2))#f)
       ((< (year data1) (year data2)) #t )
       ((< (month data1) (month data2)) #t)
       ((and (= (month data1) (month data2))(< (day data1) (day data2))) #t)
       (else #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;4.next-day
(define (Trueday? Day Month Year)
   (cond ((and (= Month 2) (visokosna Year) (validatemonth2v Day)) #t)
         ((and (= Month 2) (not(visokosna Year)) (validatemonth2 Day)) #t)
         ((and(month30? Month) (validatemonth30 Day)) #t)
         ((and(not(month30? Month)) (not (= Month 2)) (validatemonth31 Day)) #t)
         (else #f)))

(define (next-day data)
  (define Month (month data))
  (define Day (day data))
  (define Year (year data))
  (cond ((not(date? data)) date)
   ((and (= Month 12) (= Day 31))(make-date 1 1 (+ 1 Year)))
   ((not(Trueday? (+ 1 Day) Month Year ))(make-date 1 (+ 1 Month) Year))
   (else (make-date (+ 1 Day) Month Year))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
;;6.weekday
(define(monthkey month year)
(cond 
      ((or (and (= month 1) (visokosna year))(= month 4) (= month 7))0)
      ((or (= month 1) (= month 10))1)
      ((or(and (= month 2) (visokosna year))(= month 8))3)
      ((or (= month 2) (= month 11) (= month 3)) 4)
      ((= month 5)2)
      ((= month 6)5)
      ((or (= month 9) (= month 12))6)))
     
  (define (lasttwodigits number)
     (remainder number 100) )

  (define(findcentury year)
    (if (> year 1700)
        (cond ((= (remainder (quotient year 100)4)0)6)
              ((= (remainder (quotient year 100)4)1)4)
              ((= (remainder (quotient year 100)4)2)2)
              (else 0))
      (remainder (- 18 (quotient year 100)) 7)))

    (define (finddaybycode code)
       (cond ((= code 0) "Saturday" )
             ((= code 1)"Sunday")
             ((= code 2)"Monday")
             ((= code 3)"Tuesday")
             ((= code 4)"Wednesday")
             ((= code 5)"Thursday")
             ((= code 6)"Friday")))           
       
(define (weekday date)
  (define Monthkey (monthkey (month date) (year date)))
  (define Yearkey (quotient (lasttwodigits (year date))4))
  (define DayofMonth (day date))
  (define YearCode  (findcentury (year date)))
  (define DayCode (remainder(+ Monthkey Yearkey DayofMonth YearCode (lasttwodigits (year date)))7))
  (finddaybycode DayCode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;7.





(define (weekdaycode weekday)
  (cond ((or (equal? weekday "Saturday") (equal? weekday 'Saturday)) 0)
        ((or(equal? weekday "Sunday")(equal? weekday 'Sunday) ) 1)
        ((or(equal? weekday "Monday")(equal? weekday 'Monday)) 2)
        ((or(equal? weekday "Tuesday")(equal? weekday 'Tuesday)) 3)
        ((or(equal? weekday "Wednesday")(equal? weekday 'Wednesday)) 4)
        ((or(equal? weekday "Thursday")(equal? weekday 'Thursday)) 5)
        ((or(equal? weekday "Friday") (equal? weekday 'Friday)) 6)))

 (define (next-weekday Weekday date)  
   (define days (abs(- (weekdaycode Weekday) (weekdaycode (weekday date))))) 
   (if (= days 0) (date-string date) (next-weekday Weekday (next-day date))))  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Събития
;;8.events-for-day
(define (events-for-day date events)
   (cond ((null? events) '())
         ((equal? date (car (car events))) (cons (car events) (events-for-day date (cdr events))))
         (else (events-for-day date (cdr events)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;9.calendar
(define (event< event1 event2)
   (if(date< (car event1) (car event2)) #t #f ))

(define (find-smallest events)
  (cond ((null? (cdr events)) (car events))
         ((event< (car events) (find-smallest (cdr events))) (car events))
         (else (find-smallest (cdr events)))))

 (define (deletesmallest smallest events)
     (cond ((null? events) '())
          ( (equal? (car events) smallest)(deletesmallest smallest (cdr events)))
          (else (cons (car events) (deletesmallest smallest (cdr events) )))))
 
(define (sortevents events)
  (cond ((null? events) '()) 
    (else (define smallest (find-smallest events)) 
           (cons (car smallest)
                  (sortevents (deletesmallest smallest events))))))


(define (makepair date events)
  (define onthisdate (events-for-day date events))
  (define (help onthisday)
  (cond ((null? onthisday)  date)
  (else (cons (help (cdr onthisday)) (cdr(car onthisday))))))
  (help onthisdate)
  )

(define (calendar events) 
  (define (calendar-help Eventsdate lastone)
    (cond((null? Eventsdate) '())
         ((equal? lastone (car Eventsdate))(calendar-help (cdr Eventsdate) lastone)) 
         (else (cons (makepair (car Eventsdate) events) (calendar-help (cdr Eventsdate) (car Eventsdate))))))
  (calendar-help (sortevents events) '()))
;;calendar-map-in the output the date is more than once
(define (make-alist f keys events)(map (lambda (x) (cons x (f x events))) keys))
(define (calendar2 events)
    (make-alist events-for-day (sortevents events) events))
  
    
;;tests
(require rackunit rackunit/text-ui)
(define implementation-tests
  (test-suite "implementation"
    (test-case "make-date" (check-equal?(make-date 25 4 2019) '(25 4 2019)))
    (test-case "month" (check-equal? (month(make-date 25 4 2019))4))
    (test-case "year" (check-equal?  (year(make-date 25 4 2019))2019))))  
(run-tests implementation-tests 'verbose)

(define date?-tests
  (test-suite "date?"
    (test-case "#t" (check-equal?(date?(make-date 25 4 -2019)) #t))
    (test-case "#f" (check-equal? (date?(make-date 31 4 2019)) #f))))
(run-tests date?-tests 'verbose)
       
 (define string-tests
  (test-suite "date-string"
    (test-case "incorrect data" (check-equal?(date-string(make-date 31 4 2019)) "1.4.2019"));;when the month or the day is incorrect->1
    (test-case "correct data" (check-equal? (date-string(make-date 25 4 2019))"25.4.2019"))))
(run-tests string-tests 'verbose)

(define date<-tests
  (test-suite "date<"
    (test-case "#t" (check-equal?(date<(make-date 25 4 2019)(make-date 25 4 2020)) #t))
    (test-case "#f" (check-equal? (date<(make-date 30 4 2019) (make-date 29 4 2019)) #f))))
(run-tests date<-tests 'verbose)

(define nextday-tests
  (test-suite "nextday" 
    (test-case "last day of year" (check-equal?(next-day(make-date 31 12 2019)) '(1 1 2020)));;when the month or the day is incorrect->1
    (test-case "visokosna" (check-equal? (next-day(make-date 28 2 2016)) '(29 2 2016)))
      (test-case "not visokosna" (check-equal? (next-day(make-date 28 2 2017)) '(1 3 2017)))
      (test-case "30 days" (check-equal? (next-day(make-date 30 4 2017)) '(1 5 2017)))))
(run-tests nextday-tests 'verbose)

(define weekday-tests
  (test-suite "weekday" 
    (test-case "3.12.2019" (check-equal?(weekday(make-date 25 04 1999))"Sunday"))
      (test-case "9.12.2019" (check-equal? (weekday(make-date 9 12 2019)) "Monday"))
      (test-case "26.6.1999" (check-equal? (weekday(make-date 26 6 1999)) "Saturday"))))
(run-tests weekday-tests 'verbose)

(define nextweekday-tests
  (test-suite "weekday-next" 
    (test-case "3.12.2019" (check-equal?(next-weekday "Monday" (make-date 3 12 2019)) "9.12.2019"))
    (test-case "7.12.2019" (check-equal?(next-weekday "Friday" (make-date 7 12 2019)) "13.12.2019"))
    (test-case "28.2.2020" (check-equal?(next-weekday "Monday" (make-date 28 2 2020)) "2.3.2020"))
    (test-case "31.12.1999" (check-equal?(next-weekday "Wednesday" (make-date 31 12 2019)) "1.1.2020"))));;when the month or the day is incorrect->1
(run-tests nextweekday-tests 'verbose)

(define events-for-day-tests
  (test-suite "events"
    (test-case "githubb example" (check-equal? (events-for-day (make-date 27 11 2019)
                                                       (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                             (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                             (cons (make-date 28 11 2019) "Спират водата в Лозенец")))
                                       '(((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост"))))))  
(run-tests events-for-day-tests 'verbose)

(define calendar-tests
  (test-suite "calendar-tests"
    (test-case "github-example" (check-equal? (calendar (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                       (cons (make-date 25 12 2019) "Коледа")
                                                       (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                       (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                       '(((23 3 2018) . "Концерт на Лепа Брена")
  (((27 11 2019) . "Спират водата в Младост") . "Първа лекция за Хаскел")
  ((25 12 2019) . "Коледа"))))
(test-case "" (check-equal? (calendar (list (cons (make-date 8 12 2019) "Студунтски празник")
                                                       (cons (make-date 8 12 2019) "Спират водата в Перник")
                                                       (cons (make-date 19 4 2018) "Рожден ден")
                                                       (cons (make-date 23 3 2018) "Концерт")))
                                       '(((23 3 2018) . "Концерт")
    ((19 4 2018) . "Рожден ден")
    (((8 12 2019) . "Спират водата в Перник") . "Студунтски празник")))))) 
  (run-tests calendar-tests 'verbose)
(define calendar2-tests
  (test-suite "calendar2-tests"
    (test-case "github-example" (check-equal? (calendar2 (list (cons (make-date 27 11 2019) "Първа лекция за Хаскел")
                                                       (cons (make-date 25 12 2019) "Коледа")
                                                       (cons (make-date 27 11 2019) "Спират водата в Младост")
                                                       (cons (make-date 23 3 2018) "Концерт на Лепа Брена")))
                                       '(((23 3 2018) ((23 3 2018) . "Концерт на Лепа Брена"))
  ((27 11 2019) ((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост"))
  ((27 11 2019) ((27 11 2019) . "Първа лекция за Хаскел") ((27 11 2019) . "Спират водата в Младост"))
  ((25 12 2019) ((25 12 2019) . "Коледа")))))))
  (run-tests calendar2-tests 'verbose)

        
        
 

  