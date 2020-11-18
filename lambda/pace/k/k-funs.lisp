;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:CL -*-

;;;; Interface to LM functions
;;; These versions get used when
;;; you are using Lisp Machine memory
;;; rather than simulating low level memory access

(eval-when (eval compile load)

(defpackage "KWRAP" :use nil)
(defvar *kwrap-package* (find-package 'kwrap))

)

(defmacro defkversion (name args &optional icount kname)
  `(defkfun ,(intern (symbol-name name) *kwrap-package*) ,args
     (setq A1 (,name . ,(firstn (length args)
                                *active-registers*)))
     ,(if (and *statistics-p*
               icount)
          `(incf *i-count* (1- ,icount)))
     (MOVE RETURN A1 CH-RETURN)))

(defkversion car (x) 2)    ;+ call = 3
(defkversion cdr (x) 2)    ;+ call = 3
(defkversion cddr (x) 4)   ;+ call = 5
(defkversion cadr (x) 4)   ;+ call = 5
(defkversion caddr (x) 6)  ;+ call = 7

(defkversion cons (car cdr) 11)
(defkversion ncons (x) 11)
(defkversion rplaca (cons new-car) 2) ;+ call = 3
(defkversion rplacd (cons new-cdr) 2) ;+ call = 3
(defkversion atom (x) 1)              ;+ call = 2

(deff list2 #'list)
(deff list3 #'list)
(deff list4 #'list)

(defkversion list2 (a0 a1) 22)
(defkversion list3 (a0 a1 a2) 33)
(defkversion list4 (a0 a1 a2 a3) 44)

(deff nconc2 #'si:%internal-nconc-2)

(defkversion nconc2 (l1 l2) (+ 3 (* 5 (length A0))))
