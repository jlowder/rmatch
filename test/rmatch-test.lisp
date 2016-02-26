(in-package :cl-user)

(defpackage :rmatch-test
  (:use :common-lisp :lisp-unit :rmatch))

(in-package :rmatch-test)

(defun/match tt1 (n)
  ((5) (+ n 2))
  ((6) (+ n 20))
  ((_) '30))

(define-test test1
    (assert-equal 7 (tt1 5))
    (assert-equal 26 (tt1 (+ 3 3)))
    (assert-equal 30 (tt1 7)))

(define-test simple-match
  (match-let (((x y) '(10 20))
               ((z) '(30)))
      (assert-equal 10 x)
      (assert-equal 20 y)
      (assert-equal 30 z)))

(define-test no-capture1
    (let ((b 100))
      (match-let (((a b) '(10 20))
                  ((x) (list b)))
        (assert-equal 10 a)
        (assert-equal 20 b)
        (assert-equal 100 x))))

(define-test no-capture2
    (let ((b 100))
      (match-let* (((a b) '(10 20))
                   ((x) (list b)))
        (assert-equal 10 a)
        (assert-equal 20 b)
        (assert-equal 20 x))))

(run-tests)
