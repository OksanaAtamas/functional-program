;теорія  
(+ 2 5) ; додавання
(* 3 4 5) ; множенняь
; comment - комментарій
(define a 3) ;зв'язування імені зі значенням
(+ a(* 3 2)) ;вкладеність
;================= розрахунок виразу============================
; X = (AsinB+BCosA)/(1-sinC*|B+D|) 
(define A 1)
(define B -12)
(define C 3)
(define D 4)
(/ (+ (* A (sin B))
      (* B (cos A))
   )
   (- 1 (* (sin C)(abs (+ B D))))
 )
;================ 1 recursion=========================
(define fact ; обчислення факторіалу цілого числа
  (lambda (n)
    (if (= n 0)
       1 ;Базовый случай: возвращает 1
    (* n (fact (- n 1))))))

(display "fact(6)=")
(fact 6) ; виклик процедури
;================ 2 recursion=======================
(define (sum-integers a b); знйти суму чисел в діапазоні
  (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))
(display "sum(1..10)=")
(sum-integers 1 10);call procedure
;============== 3 recursion+вложенность функций========================
;(define (cube x)
 ;   ( * x (* x x)))
(define (sum-cubes a b)
     (if (> a b)
        0
        (+ (cube a) (sum-cubes (+ a 1) b))))

(define (cube x)         ; порядок визначення вкладенної функції - несуттєвий
    ( * x (* x x)))

(display "sum-cubes(1..5)=")
(sum-cubes 1 5); call procedure

;=============== 4 рекурсія   обчислення числа пі =============
(define (pi-sum a b)
     (if (> a b)
       0
       (+ (/ 1.0 (* a (+ a 2)))
          (pi-sum (+ a 4) b))))

(display "4 example -  pi = ")
(* 8 (pi-sum 1 10000)); call procedure
