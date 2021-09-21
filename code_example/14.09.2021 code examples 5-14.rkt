#lang racket
;************ Лаб 2 *********************8
;=================example 5===================
;Обчислення квадратного кореня методом Ньютона
(define (sqrt x)   ;корінь з х
  (sqrt-iter 1.0 x));початкове наближення корня = 1

;Під час розрахунку слід поліпшувати наближення до тих пір,
;поки його квадрат не співпаде з підкоренним числом в межах наперед заданого допуску,
;наприклад,  0.001

(define (sqrt-iter y x); рекурсивне наближення до корня з х
      (if (good-enough y x);
            y
            (sqrt-iter (improve y x) x)))

(define (good-enough y x);порівнянн квадрату наближення з підкоренним числом
        (< (abs (- (square y) x)) 0.001));точність наближення 0.001

(define (improve y x)
      (average y (/ x y)))

(define (average x y)
    (/ (+ x y) 2))

(define (square x) (* x x))

(sqrt 9);виклик процедури
;=================example 6========================
(define x 23) ; вкладені процедури = функції вищого порядку в математиці
     (* x 2)
(define (f x)
     (+ x 42))
(define (g p x)
     (p x))
(g f 23) ;call procedure
;========example 7  факторіaл числа - рекурсія=======
(define (factorial n);  факторіaл числа - рекурсія
      (if (= n 0)
         1
         (* n (factorial (- n 1)))))
(factorial 5) ;call procedure
;=======example 8 факторіaл числа - ітерація=================
(define (factoriall n);факторіaл числа - ітерація
     (fact-iter 1 1 n))

(define (fact-iter product  counter  max-count)
     (if (> counter  max-count)
           product
           (fact-iter (* counter  product)
                     (+ counter 1)
                     max-count)))

(factoriall 5) ;call procedure
;============example 9 рекурсія число Фібоначчі===============
(define (fib n) ;знайти число Фібоначчі на позиції n 
     (cond ((= n 0) 0)
          ((= n 1) 1)
            (else (+ (fib (- n 1))
                     (fib (- n 2))
                   )
             )
      )
  )
(fib 10);call procedure
;==============example 10 ітерація число Фібоначчі======================
(define (fibonachi n);знайти число Фібоначчі на позиції n 
     (fib-iter 1  0  n))

(define (fib-iter a  b  count)
    (if (= count 0)
          b
         (fib-iter (+ a  b)  a  (- count 1))
         )
  )

(fibonachi 10) ;call procedure
;==============example 11 рекурсія зведення числа в степінь========================
(define (expt  b  n);зведення числа в степінь
       (if (= n  0)
           1
           (* b (expt  b  (-  n  1)))))
(expt 2 5) ;call procedure
;=============example 12 ітерація зведення числа в степінь=====================
(define (exponenta  b  n)    ;зведення числа в степінь
     (expt-iter  b  n  1))

(define (expt-iter  b  counter product)
       (if (= counter 0)
             product
            (expt-iter  b (- counter 1) (* b product)))
  )

(exponenta 2 5)  ;call procedure
;========example 13 використання предикату=========================
(define (fast-expt b n)    ;зведення числа в степінь
       (cond ((= n 0) 1)
              ((even? n) (squareb (fast-expt b (/ n 2))))
               (else (* b (fast-expt b (- n 1))))))
(define (even? n); предикат перевірки на парність
      (= (remainder n 2) 0));остача від ділення  n  на 2 має дорівнювати 0

(define (squareb x) (* x x)) ; в прикладі 5 визначена процедура square, тому тут змінили назву 
                             ; процедури square на squareb
(fast-expt 2 5)  ;call procedure
;===============example 14 алгоритм Евкліда=====================
(define (nod a b); алгоритм Евкліда для найбільшого спільного дільника 
     (if (= b 0)
         a
         (nod b (remainder a b))))

(nod 206 40)

