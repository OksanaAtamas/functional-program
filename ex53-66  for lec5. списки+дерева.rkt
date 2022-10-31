;==================================
; ПРИКЛАДИ ОБРОБКИ СПИСКІВ
;
;;=================================

#lang racket
;==========example 53 визначти кількість елементів списку======================
(define x (cons (list 1 2 3) ;x-пара, що складається з двох підсписків
                (list 4 5 6)
           )
  )
(define (count-leaves x); підрахунок елементів, що складають пару
     (cond ((null? x) 0); якщо список пустий, кількість елементів =0
           ((not (pair? x)) 1); якщо не пара, кількість елементів 1
                (else (+ (count-leaves (car x))
                              (count-leaves (cdr x))))))

(display "ex53. number of pair elements\n")
(count-leaves x)
;===================example 54 кожен елемент дерева помножити на задане число=============================
(define (scale-tree tree factor);list - елементи пари factor- множник
     (cond ((null? tree) 0)
           ((not (pair? tree)) (* tree factor))
           (else (cons (scale-tree (car tree) factor)
                               (scale-tree (cdr tree) factor)))))

(display "ex54. mult each elements of tree in number\n")

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))10)
;======example 55 процедура map: кожен елемент дерева помножити на задане число ====
(define (scale-tree1 tree factor)
      (map (lambda (sub-tree)
                  (if (pair? sub-tree)
                       (scale-tree1 sub-tree factor)
                      (* sub-tree factor)))
             tree))

(display "ex55. using map and Lambda for mult each elements of tree in number\n")
(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))10)


;===========example 56 пошук у списку=======================
(define squares-list (list 1 4 9 16 25)); створити список квадратів чисел

(define (list-reference items n)
  (if (= n 1)
      (car items)
      (list-reference (cdr items) (- n 1 ))))

(display "ex56 - пошук числа на заданій позиції в списку\n")
(list-reference squares-list 3)
;==============example 57  довжина списку======================

(define (len items)
      (if (null? items) ; якщо список пустий
           0
          (+ 1 (len (cdr items))))) ; порахувати кількість

(define odds (list 1 3 5 7 9)) ; створити список

(display "ex57 - define length of list\n")
(len odds)

;===============example 58 склеїти списки====================
(define odds1 (list 1 3 5 7)) ; створити список непарних чисел
(define squares (list 1 4 9 16 25)) ;створити список квадратів чисел

(define (app list1 list2); склеїти списки
  (if (null? list1)
          list2
          (cons (car list1) 
               (app (cdr list1) list2))))

(display "ex58 - склеїти 2 списки\n")
(display (list 1 3 5 7))
(newline)
(display (list 1 4 9 16 25))
(newline)
(app squares odds)
;============example 59  помножити кожний елемент списку на число==============
(define (scale-list items factor)
  (if (null? items)
        0
         (cons (* (car items) factor)
                  (scale-list (cdr items) 
                           factor))))

(display "ex59 - помножити кожний елемент списку на число\n")
(scale-list (list 1 2 3 4 5)10)
;====================================
(cdr '( а ) ); символьні значення
(cons 'a 3)
(cons '(а Ь) 'с)
(cons 'а 'b)
'dfg
;dfg ;error
;===========example 60 процедури як аргументи через форму mmар============
(define (mmap proc items)     ;процедури як аргументи через форму mmар
 (if (null? items)
          0
         (cons (proc (car items))
                   (mmap proc (cdr items)))))

(display "ex60  отримати модуль чисел списку через форму mmар\n  ")
(mmap abs (list -10 2.5 -11.6 17))
;============example 61 зміна значень списку через форму mmар=========
(define (inc x)
  (+ x 1) )
(define (mod2 x)
  (/ x 2))

(define (mmap1 proc items)
     (if (null? items)
          0
         (cons (proc (car items))
                   (mmap proc (cdr items)))))

(display "ex61 зміна значень списку через форму mmар \n")
(mmap1 inc (list -10 2.5 -11.6 17)) ;  збільшити значення списку на 1 
(mmap1 mod2 (list -10 2.5 -11.6 17)); поділити значення списку на 2

;=================================
;ДЕРЕВА ЯК списки
;========example 62 кількість листків дерева=======================
(define (count-leaves1 x)
     (cond ((null? x) 0)
                ((not (pair? x)) 1)
                (else (+ (count-leaves1 (car x))
                              (count-leaves1 (cdr x))))))

(display "ex62  кількість листків дерева\n")
(count-leaves (list 1 (list 2 (list 3 4) 5) (list 'a 'b)))
;==============example 63 кількість листків дерева======================
(define (enumerate-tree tree)
      (cond ((null? tree) 0)
                ((not (pair? tree)) 1)
                (else (+ (enumerate-tree (car tree))
                         (enumerate-tree (cdr tree))))))

(display "ex63  кількість листків дерева\n")
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(enumerate-tree (list 5 4 3 2 1 6))
;===========example 64 Просіяти листя  дерева, відбираючи непарні=======
(define (filter predicate sequence)
     (cond ((null? sequence) 0)
                ((predicate (car sequence))
                  (cons (car sequence)
                           (filter predicate (cdr sequence))))
                  (else (filter predicate (cdr sequence)))))

(display "ex64 відібрати непарні листя дерева \n")
(filter odd? (list 1 2 3 4 5 7))
;============example 65 Звести в квадрат кожне з непарних листів========
(define (square x)
  (* x x) )

(define (mmap2 proc items)
     (if (null? items)
          0
         (cons (proc (car items))
                   (mmap2 proc (cdr items)))))

(display "ex65 Звести в квадрат кожне з непарних листів \n")
(mmap2 square (list 1 2 3 4 5))
;===========example 66 Підсумувати квадрати непарних листів  ====
(define (accumulate op initial sequence)
      (if (null? sequence)
             initial
            (op (car sequence)
                   (accumulate op initial (cdr sequence)))))

(display "ex66 Підсумувати  числа в списку\n")
(accumulate + 0 (list 1 2 3 4 5))
(display "ex66 перемножити числа в списку\n")
(accumulate * 1 (list 1 2 3 4 5))
(display "ex66 побудувати список\n")
(accumulate cons 0 (list 1 2 3 4 5 6))
