;========example 24==================
;Лаб 1 
;Ввести з клавіатури два натуральних числа   n   та   m.
;Обчислити кількість комбінацій з   n   різних елементів по   m.
;Кількість комбінацій визначається рекурентним співвідношенням:
;Визначити глибину рекурсії.
(define (enter-num)
  (display "Please, enter a number - first number>second number:")
  (let ((num (read)))
    (if (integer? num)
        num
        (enter-num))))

(define (С n k)
  (if (< k 0)
    0
    (if (or (= n k) (and (= k 0) (< n 0))) 
        1
        (+ (С (- n 1) (- k 1)) (С (- n 1) k))
    )
   )
)
;глибинa рекурсії
(define (depthС n k)
  (if (< k 0)
    1
    (if (or (= n k) (and (= k 0) (< n 0)))
        1
        (+ (max (depthС (- n 1) (- k 1)) (depthС (- n 1) k)) 1)
    )
   )
)
; головна програма
(define (main)
  (let ((n (enter-num)) (k (enter-num)))
     (cond ((< n k) (display "Невірний параметр!"))
           (else
            (display (С n k))
            (newline)
            (display (depthС n k)))
      )
  )
)
(display "\n=====Ex 24 for Лаб 1 вар.XXX ST-Name gr YYY Number of combinations")
(newline)
(main)  ;запуск програми
(newline)

;========example 25==================
;Лаб 1 
;Увести з клавіатури натуральне число n.
;Вивести всі його цифри по одній в прямому порядку,
;розділяючи їх пробілами або новими рядками.
;При розв'язанні цього дозволена тільки рекурсія і цілочисельна арифметика.
;Контрольний тест: введено число 123, отриманий результат: 1 2 3.

(define (print-num-by-chars num)
  (cond ((= (quotient num 10) 0) (display num))
  (else
    (print-num-by-chars (quotient num 10))
    (display " ")
    (display (- num (* (quotient num 10) 10))))))


(define (enter&print-num-by-chars)
  (display "Please, enter a number>=10: ")
  (let ((num (read)))
    ;==== перевірка, чи є число цілим
    (cond ((not (integer? num))
     ;===== якщо не ціле, то повторити введення
     (display "Ohh... I asked a number. Here we go again...")
     (newline)
     (enter&print-num-by-chars))
    ;==== else - making the stuff
    (else (print-num-by-chars num)))))

;====  calling main function
(newline)
(display "=====ex 25 for Лаб 1 вар.XXX ST-Name gr YYY selection of digits")
(newline)
(enter&print-num-by-chars)

;=============example 26 ===================================
;lab1.
;По колу стоять n людей, яким присвоєні номери від 1 до n.
;Починаючи відлік з першого і рухаючись по колу,
;кожна друга людина виходитиме з кола доти, поки не залишиться одна.
;Нехай номер того, хто залишився, x. Потім по колу стоятиме x людей
;і процедура виходу з колу людей повторюватиметься доти, поки не залишиться одна людина
;з номером y. Ці процедури повторюватимуться доти, поки номер тої людини,
;що залишиться, не стане рівним первинній кількості людей в потоковому раунді.
;Визначити номер людини, яка залишилася, і кількість повторів процедури.
;Номер людини f(n), що залишилася, обчислюється за рекурентним співвідношенням:
(define (skip-people n depth)
  (display "number of peoples:")
  (display n)
  (newline)
  (display "recursion depth:")
  (display depth)
  (newline)
  (if (= n 1)
       1 
       (if (even? n)
          (- (* 2 (skip-people(quotient n 2) (+ depth 1)))1) ;(quotient n m)  - целая часть от деления (/ n m)).
          (+ (* 2 (skip-people(quotient n 2) (+ depth 1)))1)
          ))
  )

(display "\n=====ex 26 for lab 1 var XXX Stname gr")
(newline)
(skip-people 11 0)  ;call procedure

(display "end")
;===============example 27 ========================
;lab2 
;Обчислити нескінчений ланцюговий дріб, задавши значення точності при виклику функції .

(define (calc-fraction-2 iter-count)
  (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t iter-count)))))

(define (calc-fraction acc)
  (calc-fraction-high-iter 0 acc))

(define (calc-fraction-high-iter iter-count acc)
  (let ((prev (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t iter-count)))))
        (next (+ 1 (/ 1 (+ 1 (calc-fraction-iter #t (+ iter-count 1)))))))
  (if (< (abs (- next prev)) acc)
      next
      (calc-fraction-high-iter (+ iter-count 1) acc))))

(define (calc-fraction-iter is-even iter-count)
  (let ((num (if is-even 2 1)))
  (cond ((equal? 0 iter-count) (/ 1 num))
        (else
         (/ 1 (+ num (calc-fraction-iter (not is-even) (- iter-count 1))))))))

(newline)
(display "======ex27 for lab2, var ХХХ chain fraction")
(newline)
(calc-fraction 0.00001);call procedure 0.00001 - calculation accuracy

;===========example 28 for lab2 ===========
;lab2
;використовується допоміжна функція fraction_divider,
;яка обраховує  дріб до певного кроку, передає у функції fraction_step,
;результат цього обрахування на кроці n порівнюється з результатом на кроці n+1,
;та якщо їх різниться менша за задану точність обрахунок зупиняється.
;================================

(define (fraction precision)
  (+ 3 (fraction_step 1 precision)) 
  )

;========== n  кількість кроків циклу ----------------
(define (fraction_step n precision)
  (let(
       (divider_result_for_n (fraction_divider 1 1 n))
       (divider_result_for_n1 (fraction_divider 1 1 (+ 1 n)))
       )
    (if (> (abs (- divider_result_for_n1 divider_result_for_n)) precision)
        (fraction_step(+ 1 n) precision)
        divider_result_for_n
        )
    )
  )

(define (fraction_divider dividend step_number n_precision)
  (let (
        (divider_part (* (+ step_number 2) (+ step_number 2)))
        )
    (if (= n_precision 0)
        divider_part
        (/ dividend (+ 6 (fraction_divider divider_part (+ 2 step_number) (- n_precision 1))))
        )
    )
)
(newline)
(display "lab 2. Обчислити нескінчений ланцюговий дріб, задавши значення точності")
(display "Введіть точність = ")
(define precision (read (current-input-port)))

(display (fraction precision)); вивести значення дробу

;==========example 29 for lab2 без циклу перебору значень аргументу==========
; розкладання  sin(х) в ряд Тейлора (Маклорена)
(define (factorial n)
(if (= n 0)
1
(* n (factorial (- n 1)))))

(define (sine x . n)   ;розкладання sin(x) в ряд Тейлора
  (cond ((not (null? n))
         (cond ((< 25 (car n))
                0)
               (else (- (/ (expt x (car n)) (factorial (car n))) 
                        (sine x (+ 2 (car n) ) ) ) ) ) )
        (else (- x (sine x 3)))))
 
(define (y x) ; обчислення функції sin(x) для x>2, x=2, x<2
  (if (< x 2)
       (sine (+ x 0.5 ) )
        (if (> x 2)
           (sine (/ x 2))
           (sine (- x 1))
        )
  ))

(define (yst x) ; обчислення  вбудованої функції sin(x) для x>2, x=2, x<2
  (if (< x 2)
       (sin (+ x 0.5 ) )
        (if (> x 2)
           (sin (/ x 2))
           (sin (- x 1))
        )
  ))

(display "фунция по ряду тейлора :")
(y 0.5)
(newline)
(display "встроенная фунция ")
(yst 0.5)
(newline)
(sine 0.5 1) ; виклик функцїї користувача
(sin 0.5 ) ;перевірка — виклик вбудованої функції SCHEME