#lang racket

;=======================================
;         LAMBDA ФУНКЦІЇ та LET ФОРМА
;=======================================
; Ex18  Lambda як оператор у комбінації операторів
;==========================================
(define (square a)
 (* a a ))
(display "Ex18  Lambda як оператор у виразі 1+2+3^2=")
((lambda (x y w)
   (+ x y (square w)))
 1 2 3)

;======================================
;Ex19 розрахунок інтеграла з використанням
;процедур як параметрів та Lambda форми
;=====================================
;шаблон процедури для суми значень
(define (sum-integral term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum-integral term (next a) next b))))
; процедура розрахунку інтеграла
(define (integral f a b dx)
  (* (sum-integral f(+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;розрахунок кубу числа
(define (cub x)
     ( * x (* x x)))

(display "example 19 Lambda for integral of cube from 0 to 1 =")
(integral  cub  0  1  0.1);call procedure

;===============================================================
; Ex20 розрахунок числа пі за рядом Лейбніца
;1/(1*3)+1/(5*7)+1/(9*11)+1/(13*15)з використанням
;процедур як параметрів  та Lambda форми
;===============================================================
; шаблон процедури розрахунку суми
(define (sum-a-b term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum-a-b term (next a) next b))))
; процедура розрахуку числа пі
(define (pi-sum a b)
    (sum-a-b (lambda (x) 
          (/ 1.0 (* x (+ x 2))))
       a
     (lambda (x) (+ x 4))
       b))
;call procedure
(display "example 20 Pi=")
(* 8 (pi-sum 1 1000))

;============example 21==============

(define reverse-subtract
     (lambda (x y) (- y x)))

(display "example 21 \n")
(reverse-subtract 7 10);call procedure
;==================================
; Ex22  обчислити функцію x(1+xy)^3+y(1−y)+(1+xy)(1−y)
; використовуючи локальні змінні
;=====================================
(define (f x y)
    (define (f-helper a b)
         (+ (* x (square a))
         (* y b)
         (* a b)))

 (f-helper (+ 1 (* x y))
       (- 1 y)))
;call procedure
(display "example 22. f(1,2)=x(1+xy)^3+y(1−y)+(1+xy)(1−y)=")
(f 1 2)

;=================================
;Ex23  Lambda  для створення локальних іменувань (змінних)
;===============================
(define (ff x y)
  ((lambda (a b)
     (+ (* x (sqr a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (sqr x)
      (* x x))
;call procedure
(display "example 23. f(1,2)=x(1+xy)^3+y(1−y)+(1+xy)(1−y)=")
(ff 1 2)

;==============================================
; Ex24 локальні змінні через let
; для розрахунку функції  f(x,y)=x(1+xy^3+y(1−y)+(1+xy)(1−y)
;==============================================
(define (function x y)
      (let ((a (+ 1 (* x y)))
      (b (- 1 y)))
    (+ (* x (square a))
         (* y b)
         (* a b))))


(display "example 24. f(1,2)=x(1+xy)^3+y(1−y)+(1+xy)(1−y)=")
(function 1 2);call procedure

;=====================================
;Ex25 область визначення змінних в let формі 
;================================
(define x 5)
(+ (let ((x 3))
(+ x (* x 10)))
x)

(define x1 2)
(let ((x1 3)
     (y (+ x1 2)))
(* x1 y))
; виклик процедур не потрібний

;=========================================
;ex26  альтернатива let внутрішні визначення
;===========================================
(define (fff x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

(display "example 26. fff(1,2)=")
(fff 1 2)