;=======================================
;   СТАНДАРТНІ ПРОЦЕДУРИ SCHEME (RACKET) 
;=======================================
;-------ex39 процедура set!-----------------
(define (make-serial-number-generator)
  (let ((current-serial-number 0))
    (lambda ()
      (set! current-serial-number (+ current-serial-number 1))
      current-serial-number)))

(define entry-sn-generator (make-serial-number-generator))
(entry-sn-generator)
(entry-sn-generator)
(entry-sn-generator)
;========= ex40 процедура set!================
(define get-balance #f)
(define deposit #f)

(let ((balance 0))
  (set! get-balance
        (lambda ()
          balance))
  (set! deposit
        (lambda (amount)
          (set! balance (+ balance amount))
          balance)))

(define (withdraw amount)
  (deposit (- amount)))

(get-balance)
(deposit 50)
(withdraw 75)