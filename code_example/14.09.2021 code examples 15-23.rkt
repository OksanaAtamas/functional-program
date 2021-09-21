#lang racket

;*************ЛАБ 3****************
; Процедури як аргументи. Lambda функции

;============example 15===================
(define (inc n)   ; лічильник чисел
     (+ n 1))

(define (cube x); обчислення куба числа
     ( * x (* x x)))

(define (sum term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum term (next a) next b))))

(define (sum-cubes a b)
        (sum cube a inc b))

(display "example 15 \n")
(sum-cubes 1 100);call procedure
;===============example 16====================
(define (increment n) 
     (+ n 1))
(define (identity x)
      x)
(define (summa term a next b)
  (if (> a b)
    0
  (+ (term a)
  (summa term (next a) next b))))

(define (sum-integers a b)
      (summa identity a increment b))

(display "example 16 \n")
(sum-integers 1 10);call procedure
;===========example 17==========================
(define (sum-a-b term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum-a-b term (next a) next b))))

(define (pi-sum a b)
      (define (pi-term x)
           (/ 1.0 (* x (+ x 2))))
      (define (pi-next x)
           (+ x 4))
      (sum pi-term a pi-next b))

(display "example 17 \n")
(* 8 (pi-sum 1 1000));call procedure
;===============example 18===================
(define (integral  f  a  b  dx)
       (define (add-dx  x) 
              (+ x dx))
        (* (sum-integral f (+ a (/ dx 2)) add-dx b)
               dx))

(define (cub x)
     ( * x (* x x)))
(define (sum-integral term a next b)
  (if (> a b)
    0
  (+ (term a)
  (sum-integral term (next a) next b))))

(display "example 18 \n")
(integral  cub  0  10  0.1);call procedure
;============example 19==============
((lambda (x) (+ x x)) 4)  ; unnamed procedure

(define reverse-subtract
     (lambda (x y) (- y x)))

(display "example 19 \n")
(reverse-subtract 7 10);call procedure
;==========example 20============
(define (square x)
      (* x x))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))


(display "example 20 \n")
(f 2 10);call procedure

;================example 21 local variables=======
(define (ff x y)
    (define (f-helper a b)
         (+ (* x (square a))
         (* y b)
         (* a b)))

 (f-helper (+ 1 (* x y))
       (- 1 y)))


(display "example 21 \n")
(ff 1 2);call procedure

;=======example 22 local variables through  lambda=======
(define (fun x y)
     ((lambda (a b)
        (+ (* x (square a))
            (* y b)
            (* a b)))
     (+ 1 (* x y))
     (- 1 y)))


(display "example 22 \n")
(fun 1 2) ;call procedure

;=======example 23 local variables through let=======

(define (function x y)
      (let ((a (+ 1 (* x y)))
      (b (- 1 y)))
    (+ (* x (square a))
         (* y b)
         (* a b))))


(display "example 23 \n")
(function 1 2);call procedure


