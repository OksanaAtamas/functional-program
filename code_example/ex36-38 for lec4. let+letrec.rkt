;#lang racket
;====================================
;    СТАНДАРТНІ ПРОЦЕДУРИ for lec4
;====================================
;--------ex36   процедура lec ----------------
(display "Ex36 let* обчислює ")
(let ((x 2) (y 3))
  (let* ((x 7)
         (w (+ x y)))
    (* w x)))
;-----Ex37 процедура letrec-----------------
(display "ex37 letrec result=")
(letrec ((even?
          (lambda (n)
            (if (zero? n)
                #t
                (odd? (- n 1)))))
         (odd?
          (lambda (n)
            (if (zero? n)
                #f
                (even? (- n 1))))))
  (even? 88))   ;#t
;----------процедура lecrec for list---------------
(define (lenlist lst)
  (letrec ((length-tail
            (lambda (lst len)
              (if (null? lst) len
                  (length-tail (cdr lst)
                               (+ len 1))))))
    (length-tail lst 0)))
(display "length of list =")
(lenlist (list 1 2 3 'abc 8.5 'вап))
;-----------------------
(letrec ((sum (lambda (ls)
 (if (null? ls) 0
 (+ (car ls) (sum (cdr ls)))))))
 (sum '(1 2 3 4 5))) ;= 15

;--------ex 38  процедура letrec----------
(define (triple x)
  (letrec ((y (+ x 2))
           (f (lambda (xx)
                (+ xx y w x)))
           (w (+ x 7)))
    (f -9)))
(display "ex38 letrec result = ")
(triple 2)