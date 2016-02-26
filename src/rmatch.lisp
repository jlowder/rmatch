(in-package :cl-user)

(defpackage :rmatch
  (:use :common-lisp)
  (:export :match-let
           :match-let*
           :defun/match))

(in-package :rmatch)

(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
             (let ((p (car pat))
                   (rec (destruc (cdr pat) seq atom? (1+ n))))
               (if (funcall atom? p)
                   (cons `(,p (elt ,seq ,n))
                          rec)
                   (let ((var (gensym)))
                     (cons (cons `(,var (elt ,seq ,n))
                                  (destruc p var atom?))
                           rec))))))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun var? (x)
  (and (symbolp x)
       (not (null x))))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defun match1 (refs then else)
  (destructuring-bind ((pat expr) . rest) refs
    (cond ((gensym? pat)
           `(let ((,pat ,expr))
              (if (and (typep ,pat 'sequence)
                       ,(length-test pat rest))
                  ,then
                  ,else)))
          ((eq pat '_) then)
          ((var? pat)
           (let ((ge (gensym)))
             `(let ((,ge ,expr))
                (if (or (gensym? ,pat) (equal ,pat ,ge))
                    (let ((,pat ,ge)) ,then)
                    ,else))))
          (t `(if (equal ,pat ,expr) ,then ,else)))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))

(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

(defmacro match-let* (patseqs &body body)
  (if (null patseqs)
      `(progn
         ,@body)
      `(if-match ,(caar patseqs) ,(cadar patseqs)
                 (match-let* ,(cdr patseqs) ,@body))))

(defmacro match-let (patseqs &body body)
  (labels ((rec (l)
             (when (not (null l))
               `(append ,(second (car l)) ,(rec (cdr l))))))
    (let ((gg (gensym)))
      `(let ((,gg ,(rec patseqs)))
         (if-match ,(loop for x in patseqs appending (car x))
                   ,gg
                   (progn ,@body))))))
                           
(defmacro defun/match (name (args) &body body)
  (labels ((rec (l pat)
             (when (not (null l))
               `(if-match ,(caar l) (list ,pat) (progn ,(cadar l)) ,(rec (cdr l) pat)))))
    `(defun ,name (,args)
       ,(rec body args))))

;; ;; (LABELS ((#:G1063 ()
;; ;;            NIL))
;; ;;   (LET ((#:G1062 '(1)))
;; ;;     (IF (IF (TYPEP #:G1062 'SEQUENCE)
;; ;;             (THE T (= (LENGTH #:G1062) 1))
;; ;;             NIL)
;; ;;         (LET ((#:G1064 (ELT #:G1062 0)))
;; ;;           (IF (LET ((#:G1065 (GENSYM? A)))
;; ;;                 (IF #:G1065
;; ;;                     #:G1065
;; ;;                     (THE T (EQUAL A #:G1064))))
;; ;;               (LET ((A #:G1064))
;; ;;                 (LIST A))
;; ;;               (#:G1063)))
;; ;;         (#:G1063))))
;; 
;; (match1 '(((a) (1))) (list 4) nil)
;; ;; (IF (EQUAL (A) (1))
;; ;;     (4)
;; ;;     NIL)
;; 
;; (match1 '((a (+ 1 2)) (b 5)) '(list 4) nil)
;; ;; (LET ((#:G1408 (+ 1 2)))
;; ;;   (IF (OR (GENSYM? A) (EQUAL A #:G1408))
;; ;;       (LET ((A #:G1408))
;; ;;         (LIST 4))
;; ;;       NIL))
;; 
;; (defun pair-cdrs (l)
;;   (when l
;;     (destructuring-bind ((a b) . rest) l
;;       (cons b (pair-cdrs rest)))))
;; 
;; (defun pair-cars (l)
;;   (when l
;;     (destructuring-bind ((a b) . rest) l
;;       (cons a (pair-cars rest)))))
;;   
;; (defun pairs (l)
;;   (when l
;;     (destructuring-bind ((a b) . rest) l
;;       (cons a (cons b (pairs rest))))))
;; 
;; (defun comb (l)
;;   (when (and (consp l)
;;              (consp (car l)))
;;     (append (car l) (comb (cdr l)))))
;; 
;; (pair-cdrs '((1 2) (3 4) (5 6)))
;; ;; (2 4 6)
;; 
;; (pair-cars '((1 2) (3 4) (5 6)))
;; ;; (1 3 5)
;; 
;; (pairs '((1 2) (3 4) (5 6)))
;; ;; (1 2 3 4 5 6)
;; 
;; (comb '((1 2 d) (3 4) (5 6)))
;; ;; (1 2 D 3 4 5 6)
;; 
;; (match-let (( (a _ a) '(10 20 10) )
;;             ( (x y) (list a 20) ))
;;   (format t "~a~%" x)
;;   (list a y))
;; 
;; 
