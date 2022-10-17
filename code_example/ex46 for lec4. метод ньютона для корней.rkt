;=============================================
; процедури вищого порядку, що повертаються з інших процедур for lec4
;=====================================================
;
;Ex46 метод Ньютона для визначення квадратного кореня числа
;;======================================================================
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx))
          (g x))
       dx)))

(define dx 0.00001)

(define (cube x)
  (* x x x))
;call procedure
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next(try next))))
  (try first-guess))

(define (square x)
  (* x x))

(define (sqroot x)
  (newtons-method
   (lambda (y)
   (- (square y) x))
                1.0))

(sqroot 25)


(sqrt 25)
