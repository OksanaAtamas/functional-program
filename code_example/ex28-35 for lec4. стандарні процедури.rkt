;#lang racket
;====================================
;    СТАНДАРТНІ ПРОЦЕДУРИ
;====================================
;------Ex28---define
(define (average x y)
    (/ (+ x y) 2))
(display "ex28 - define\n")
(average 7 10)

;---Ex29------let----set!
(let 
  (
    (a "My") 
    (b " name") 
    (c " is") 
    (d " Michael")
  ) 
  (set! d " Lesia")
  (display "Ex 29 let & set!\n")
  (display a)
  (display b)
  (display c)
  (display d)
  (display "\n")  ;(format "~a ~a ~a ~a" a b c d) ; implement for Racket 
)
;----Ex30----lambda---------
(define hello-world (lambda () "Hello World"))
(hello-world)

(define (fun g)
  (g 2))

(display "Ex30 якщо х=2, тоді (x+1)*x=")
(fun (lambda (x) (* x (+ x 1))))

;-----------------------------
(define add4
  (let ((x 4))
    (lambda (y) (+ x y))))
(display "x+y=")
(add4 6)

;----Ex31----if  cond  begin------------
(display "Ex31 - if: ")
(if (< 1 3) 
  (begin
    (display "one line")
    (newline)
    (display "two line\n")
    )
  (display "no line\n")
  )

(display "Ex31 - cond: ")
(cond 
  ((> 2 3) "wrong!")
  ((< 2 3) "wrong again!")
  ((= 2 3) "ok")
  (else "there is no other")
)
;Якщо код необхідно виконати лише у разі істини
;(when (< 1 3) "true") ; тільки для Racket

;----Ex32-----cycle with recursion--------------------
;(let loop ((i 0))                 ; definition
;  (when (< i 4)                  ; condition тільки для Racket
;    (display (format " i=~a\n" i)) ; body
;    (loop (+ 1 i))                ; next iteration
;  )
;)
(display "Ex32 \n")
(define (repeat number)
  (if (> number 0) ; если количество повторов не нулевое
           (begin (display "hello ") ; выполняем действие
                  (repeat (- number 1))) ; повторим действие на единицу меньшее количество раз
  (display "\n end loop\n"))
  )

(repeat 3)

;------Ex33 lambda in loop----------------------
(define (repeat number function)
  (if (> number 0)
    (begin (function)
           (repeat (- number 1) function))))
(display "Ex33 lambda in loop\n")
(repeat 3 (lambda() (display "2022 ")) )

;-------Ex34 and   or  not ----------------
(display "\n ex34 \n")
(not "hot")
(and (odd? 3) ;перевірка на непарність
     (even? 4))  ;перевірка на парність
(not #t)                   ; =>  #f
(not 3)                    ; =>  #f
(not (list 3))             ; =>  #f
(not 'nil)                 ; =>  #f

(and (= 2 2) (> 2 1))  ;#t
(or (= 2 2) (< 2 1))   ;#t
(and #f #t #t)         ; #f
(or #f #t #f )         ;#t

;------Ex 35 cond case --------------------
(define (classify x)
  (cond
    ((< x 0) "Negative")
    ((case x((13 42 100) #t)
       (else #f))
     "Special")
    (else "Unknown")))
(display "\n Ex 35 cond case\n" )
(classify 42)




