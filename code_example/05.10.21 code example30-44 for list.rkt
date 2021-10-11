#lang racket
;==========example 30 кількість елементів списку======================
(define x (cons (list 1 2 3) ;x-пара, що складається з двох підсписків
                (list 4 5 6)
           )
  )
(define (count-leaves x); підрахунок елементів, що складають пару
     (cond ((null? x) 0); якщо список пустий, кількість елементів =0
           ((not (pair? x)) 1); якщо не пара, кількість елементів 1
                (else (+ (count-leaves (car x))
                              (count-leaves (cdr x))))))

;(display "ex30. number of pair elements\n")
;(count-leaves x)
;===================example 31 кожен елемент дерева помножити на задане число=============================
(define (scale-tree tree factor);list - елементи пари factor- множник
     (cond ((null? tree) 0)
           ((not (pair? tree)) (* tree factor))
           (else (cons (scale-tree (car tree) factor)
                               (scale-tree (cdr tree) factor)))))

(display "ex31. mult each elements of tree in number\n")

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))10)
;======example 32 процедура map: кожен елемент дерева помножити на задане число ====
(define (scale-tree1 tree factor)
      (map (lambda (sub-tree)
                  (if (pair? sub-tree)
                       (scale-tree1 sub-tree factor)
                      (* sub-tree factor)))
             tree))

(display "ex32. using map and Lambda for mult each elements of tree in number\n")
(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))10)


;===========example 33 пошук у списку=======================
(define squares-list (list 1 4 9 16 25)); створити список квадратів чисел

(define (list-reference items n)
  (if (= n 1)
      (car items)
      (list-reference (cdr items) (- n 1 ))))

(display "ex33 - пошук числа на заданій позиції в списку\n")
(list-reference squares-list 3)
;==============example 34  довжина списку======================

(define (len items)
      (if (null? items) ; якщо список пустий
           0
          (+ 1 (len (cdr items))))) ; порахувати кількість

(define odds (list 1 3 5 7 9)) ; створити список

(display "ex34 - define length of list\n")
(len odds)

;===============example 35 склеїти списки====================
(define odds1 (list 1 3 5 7)) ; створити список непарних чисел
(define squares (list 1 4 9 16 25)) ;створити список квадратів чисел

(define (app list1 list2); склеїти списки
  (if (null? list1)
          list2
          (cons (car list1) 
               (app (cdr list1) list2))))

(display "ex35 - склеїти 2 списки\n")
(display (list 1 3 5 7))
(newline)
(display (list 1 4 9 16 25))
(newline)
(app squares odds)
;============example 36  помножити кожний елемент списку на число==============
(define (scale-list items factor)
  (if (null? items)
        0
         (cons (* (car items) factor)
                  (scale-list (cdr items) 
                           factor))))

(display "ex36 - помножити кожний елемент списку на число\n")
(scale-list (list 1 2 3 4 5)10)
;====================================
(cdr '( а ) ); символьні значення
(cons 'a 3)
(cons '(а Ь) 'с)
(cons 'а 'b)
'dfg
;dfg ;error
;===========example 37 процедури як аргументи через форму mmар============
(define (mmap proc items);процедури як аргументи через форму mmар
 (if (null? items)
          0
         (cons (proc (car items))
                   (mmap proc (cdr items)))))

(display "ex37  отримати модуль чисел списку через форму mmар\n  ")
(mmap abs (list -10 2.5 -11.6 17))
;============example 38 зміна значень списку через форму mmар=========
(define (inc x)
  (+ x 1) )
(define (mod2 x)
  (/ x 2))

(define (mmap1 proc items)
     (if (null? items)
          0
         (cons (proc (car items))
                   (mmap proc (cdr items)))))

(display "ex38   зміна значень списку через форму mmар \n")
(mmap1 inc (list -10 2.5 -11.6 17)) ;  збільшити значення списку на 1 
(mmap1 mod2 (list -10 2.5 -11.6 17)); поділити значення списку на 2

;=================================
;tree
;========example 39 кількість листків дерева=======================
(define (count-leaves1 x)
     (cond ((null? x) 0)
                ((not (pair? x)) 1)
                (else (+ (count-leaves1 (car x))
                              (count-leaves1 (cdr x))))))

(display "ex39  кількість листків дерева\n")
(count-leaves (list 1 (list 2 (list 3 4) 5) (list 'a 'b)))
;==============example 40 кількість листків дерева======================
(define (enumerate-tree tree)
      (cond ((null? tree) 0)
                ((not (pair? tree)) 1)
                (else (+ (enumerate-tree (car tree))
                         (enumerate-tree (cdr tree))))))

(display "ex40  кількість листків дерева\n")
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(enumerate-tree (list 5 4 3 2 1 6))
;===========example 41 Просіяти листя  дерева, відбираючи непарні=======
(define (filter predicate sequence)
     (cond ((null? sequence) 0)
                ((predicate (car sequence))
                  (cons (car sequence)
                           (filter predicate (cdr sequence))))
                  (else (filter predicate (cdr sequence)))))

(display "ex41 відібрати непарні листя дерева \n")
(filter odd? (list 1 2 3 4 5 7))
;============example 42 Звести в квадрат кожне з непарних листів========
(define (square x)
  (* x x) )

(define (mmap2 proc items)
     (if (null? items)
          0
         (cons (proc (car items))
                   (mmap2 proc (cdr items)))))

(display "ex42 Звести в квадрат кожне з непарних листів \n")
(mmap2 square (list 1 2 3 4 5))
;===========example 43 Підсумувати квадрати непарних листів  ====
(define (accumulate op initial sequence)
      (if (null? sequence)
             initial
            (op (car sequence)
                   (accumulate op initial (cdr sequence)))))

(display "ex43 Підсумувати  числа в списку\n")
(accumulate + 0 (list 1 2 3 4 5))
(display "ex43 перемножити числа в списку\n")
(accumulate * 1 (list 1 2 3 4 5))
(display "ex43 побудувати список\n")
(accumulate cons 0 (list 1 2 3 4 5 6))
;==========================================
;example 44 for lab3 
;===========================================
;пошук кореня рівняння sinx=0 методом половинного ділення
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
;-------------------------------
(define (average x y)
     (/ (+ x y) 2))
;-------------------------------
(define (close-enough? x y)
      (< (abs (- x y)) 0.001))
;---------------------------------
(define (half-interval-method f a b)
       (let ((a-value (f a))
                (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
          (search f a b))
          ((and (negative? b-value) (positive? a-value))
            (search f b a))
          (else
             (error "У аргументов не різні знаки " a b)))))

(display "ex44  пошук кореня рівняння sinx=0 методом половинного ділення \n")
(half-interval-method sin 2.0 4.0);виклик
(display "ex44 -перевірка достовірності результату \n")
(sin (half-interval-method sin 2.0 4.0))
;=================================