(in-package :cl-user)

(defpackage :rmatch-test
  (:use :common-lisp :lisp-unit :rmatch))

(in-package :rmatch-test)

(defun/match tt1 (n)
  ((5) (+ n 2))
  ((6) (+ n 20))
  ((_) '30))

(define-test defunmatch1
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
      (match-let (((a b) (list b 20))
                  ((x) (list b)))
        (assert-equal 100 a)
        (assert-equal 20 b)
        (assert-equal 100 x))))

(define-test no-capture2
    (let ((b 100))
      (match-let* (((a b) (list b 20))
                   ((x) (list b)))
        (assert-equal 100 a)
        (assert-equal 20 b)
        (assert-equal 20 x))))

(define-test sublist1
    (match-let (((a b) '((1 2) (3 4)))
                ((x (y)) '((5 6 7) ((10 20)))))
      (assert-equal a '(1 2))
      (assert-equal b '(3 4))
      (assert-equal x '(5 6 7))
      (assert-equal y '(10 20))))

(define-test sublist2
    (match-let* (((a b) '((1 2) (3 4)))
                 ((x (y)) '((5 6 7) ((10 20)))))
      (assert-equal a '(1 2))
      (assert-equal b '(3 4))
      (assert-equal x '(5 6 7))
      (assert-equal y '(10 20))))

(define-test small1
    (match-let (((a b) '(100 200)))
      (assert-equal 100 a)
      (assert-equal 200 b)))

(define-test small2
    (match-let* (((a b) '(100 200)))
      (assert-equal 100 a)
      (assert-equal 200 b)))

(define-test repeated1
  (match-let (((a b a b) '(10 20 10 20))
              ((b a) '(20 10)))
             (assert-equal a 10)
             (assert-equal b 20)))

(define-test repeated2
  (match-let*  (((a b a b) '(10 20 10 20))
                ((b a) '(20 10)))
               (assert-equal a 10)
               (assert-equal b 20)))
             
(define-test style1
    (let ((n1 "this is a test"))
      (match-let (((a b c) (list n1 150 #(1 2 3)))
                  ((x 10) (list (+ 50 60) (+ 8 2))))
        (assert-true (string= a n1))
        (assert-equal b 150)
        (assert-true (equalp c #(1 2 3)))
        (assert-equal x 110))))

(define-test style2
    (let ((n1 "this is another test"))
      (match-let* (((a b c) (list n1 150 #(1 2 3)))
                   ((x 10) (list (+ 50 60) (+ 8 2))))
        (assert-true (string= a n1))
        (assert-equal b 150)
        (assert-true (equalp c #(1 2 3)))
        (assert-equal x 110))))

(defun/match factorial (n)
  ((0) 1)
  ((_) (* n (factorial (- n 1)))))

(define-test fact1
    (assert-equal (factorial 0) 1)
  (assert-equal (factorial 10) 3628800))

(defun/match tt2 (s)
  (("abc") 1)
  (("def") 2)
  (("xyz") 3)
  ((_) 4))

(define-test string1
    (assert-equal (tt2 "xyz") 3)
  (assert-equal (tt2 '(10 20 30)) 4))

(defun/match p2 (a b)
  ((10 20) "ten twenty")
  ((_ 30) (+ a b))
  ((_ _) "none"))

(define-test string2
  (assert-equal (p2 10 20) "ten twenty")
  (assert-equal (p2 20 30) 50)
  (assert-equal (p2 10 10) "none"))

(let ((*print-errors* t)
      (*print-failures* t))
  (run-tests))
