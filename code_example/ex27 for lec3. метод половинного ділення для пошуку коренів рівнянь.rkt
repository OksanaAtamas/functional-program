#lang racket

;=============================================
;ex27 Знаходження коренів рівнянь методом половинного ділення
;=====================================================
(define (search f neg-point pos-point)
     (let ((midpoint (average neg-point pos-point)))
          (if (close-enough? neg-point pos-point)
              midpoint
              (let ((test-value (f midpoint)))
                  (cond ((positive? test-value)
                           (search f neg-point midpoint))
                          ((negative? test-value)
                           (search f midpoint pos-point))
                           (else midpoint))))))

(define (close-enough? x y)
      (< (abs (- x y)) 0.001))
(define (average x y)
     (/ (+ x y) 2))
(define (test-value f midpoint)
     ( sin midpoint );<розрахунок виразу>)
  )

(define (abs x)
  (if (positive? x )
       x
      (- x)))

(define (half-interval-method f a b)
       (let ((a-value (f a))
                (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
          (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else
             (error "У аргументов не різні знаки " a b)))))

  (half-interval-method sin 1.0 5.0)