;#lang racket
;================================
; ПРЕДИКАТИ ЕКВІВАЛЕНТНОСТІ for lec4
;===============================
;------ex 41 eqv?  -----------------
(display"ex41 \n")
(define a 4)
(eqv? a a)
(eqv? 'ab 'ab)               ; =>  #t
(eqv? 'a 'b)               ; =>  #f
(eqv? 2 2)                 ; =>  #t
(eqv? 2.50 2.51) 
(eqv? '() '())             ; =>  #t
(eqv? 100000000 100000000) ; =>  #t
(eqv? (cons 1 2) (cons 1 2));=>  #f
(eqv? (lambda () 1)
      (lambda () 2))        ;=>  #f
(eqv? #f 'nil)              ; =>  #f
(let ((p (lambda (x) x)))
  (eqv? p p))               ;=>  #t

;----------------------------
(display "ex41 continue \n")
(eqv? "" "")                ;=>  unspecified
(eqv? '#(1 2) '#(1 2))           ; =>  unspecified #f
(eqv? (lambda (x) x)
      (lambda (x) x))      ;=>  unspecified
(eqv? (lambda (x) x)
      (lambda (y) y))       ;=>  unspecified

;---------ex42 ---------------
(display "ex42 eqv?\n")
(define gen-counter
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(let ((g (gen-counter)))
  (eqv? g g))              ; =>  #t

(eqv? (gen-counter) (gen-counter))
                          ;  =>  #f
;------------------------
(define gen-loser
  (lambda ()
    (let ((n 0))
      (lambda () (set! n (+ n 1)) n))))

(let ((g (gen-loser)))
 (eqv? g g))              ; =>  #t

(eqv? (gen-loser) (gen-counter))    ; =>  unspecified #f
;------------------------------
(letrec ((f (lambda () (if (eqv? f g) 'both 'f)))
        (g (lambda () (if (eqv? f g) 'both 'f))))
  (eqv? f g))
                           ; =>  unspecified #f

(letrec ((f (lambda ()
              (if (eqv? f g) 'f 'both) ;
              ))
         (g (lambda () (if (eqv? f g) 'g 'both))))
  (eqv? f g))                            ; =>  #f

;=======ex43 eq? ----------------------
(display "ex43 eq? \n")
(eq? 'a 'a)                ;=>  #t
(eq? '(a) '(a))            ;=>  unspecified #f
(eqv? '(a) '(a)) 
(eq? (list 'a) (list 'a))  ; =>  #f
(eq? "a" "a")              ;=>  unspecified #t
(eq? "" "")                ; =>  unspecified #t
(eq? '() '())              ; =>  #t
(eq? 2 2)                  ; =>  unspecified #t
(eq? #\A #\A)              ; =>  unspecified
(eq? car car)              ; =>  #t
(let ((n (+ 2 3)))
  (eq? n n))               ; =>  unspecified #t
(let ((x '(a)))
  (eq? x x))               ; =>  #t
(let ((x '#()))
  (eq? x x))               ; =>  #t
(let ((p (lambda (x) x)))
  (eq? p p))               ; =>  #t

;---------ex44 equal?--------------------------
(display "ex44 equal? \n")
(equal? 'a 'a)              ;=>  #t
(equal? '(a) '(a))         ; =>  #t
(equal? '(a (b) c)
        '(a (b) c))        ; =>  #t
(equal? "abc" "abc")       ; =>  #t
(equal? 2 2)               ; =>  #t
(equal? (make-vector 5 'a)
        (make-vector 5 'a)); =>  #t
(equal? (lambda (x) x)
        (lambda (y) y))    ; =>  unspecified

;---------------------
(display "ex45 equal? \n")
(= 2 3)     ;=> #f
(= 2.5 2.5) ;=> #t
;(= '() '()) ;=> error
(define x '(2 3))
(define y '(2 3))
(eq? x y)         ;=> #f
(define y x)
(eq? x y)         ;=> #t
(eqv? 2 2)    ; => #t
(eqv? "a" "a") ;=> depends upon the implementation
;------------------------------
(define x '(2 3))
(define y '(2 3))
(equal? x y)     ; => #t
(eqv? x y)       ; => #f
(eq? x y)       ; => #f