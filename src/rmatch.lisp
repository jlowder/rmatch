(in-package :cl-user)

(defpackage :rmatch
  (:use :common-lisp)
  (:export :match-let
           :match-let*
           :defun/match
           :match))

(in-package :rmatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From "On Lisp" by Paul Graham, mostly from Chapter 18 (Destructuring), with a few tweaks
;;; on variable name handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun simple? (x) (or (atom x) (eq (car x) 'quote)))

(defun var? (x)
  (and (symbolp x)
       (not (null x))))

(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun gensym? (s)
  (and (symbolp s) (not (symbol-package s))))

(defun length-test (pat rest)
  (let ((fin (caadar (last rest))))
    (if (or (consp fin) (eq fin 'elt))
        `(= (length ,pat) ,(length rest))
        `(> (length ,pat) ,(- (length rest) 2)))))

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

(defun gen-match (refs then else)
  (if (null refs)
      then
      (let ((then (gen-match (cdr refs) then else)))
        (if (simple? (caar refs))
            (match1 refs then else)
            (gen-match (car refs) then else)))))

(defmacro pat-match (pat seq then else)
  (if (simple? pat)
      (match1 `((,pat ,seq)) then else)
      (with-gensyms (gseq gelse)
        `(labels ((,gelse () ,else))
           ,(gen-match (cons (list gseq seq)
                             (destruc pat gseq #'simple?))
                       then
                       `(,gelse))))))

(defmacro if-match (pat seq then &optional else)
  `(let ,(mapcar #'(lambda (v) `(,v ',(gensym)))
                 (vars-in pat #'simple?))
     (pat-match ,pat ,seq ,then ,else)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End of "On Lisp" code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro match-let* (patseqs &body body)
  (if (null patseqs)
      `(progn
         ,@body)
    (let ((gg (gensym)))
      `(let ((,gg ,(cadar patseqs)))
         (if-match ,(caar patseqs) ,gg
                 (match-let* ,(cdr patseqs) ,@body))))))

(defmacro match-let (patseqs &body body)
  (labels ((rec (l)
             (when (not (null l))
               `(append ,(second (car l)) ,(rec (cdr l))))))
    (let ((gg (gensym)))
      `(let ((,gg ,(rec patseqs)))
         (if-match ,(loop for x in patseqs appending (car x))
                   ,gg
                   (progn ,@body))))))
                           
(defmacro defun/match (name args &body body)
  (labels ((rec (l pat)
             (when (not (null l))
               `(if-match ,(caar l) (list ,@pat) (progn ,(cadar l)) ,(rec (cdr l) pat)))))
    `(defun ,name ,args
       ,(rec body args))))

(defmacro match (args &body body)
  (labels ((rec (l pat)
             (when (not (null l))
               `(if-match ,(caar l) ,pat (progn ,(cadar l)) ,(rec (cdr l) pat)))))
    (let ((gg (gensym)))
      `(let ((,gg ,args))
         ,(rec body gg)))))
