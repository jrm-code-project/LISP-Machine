;;; -*- Mode:LISP; Package:INTERPRETER; Base:10; Readtable:CL -*-

;;; INTERPRETER-TESTS.LISP
;;;


;; Self-evaluating forms

3
#\a
"foo"
#(7 a ('p . 'q))
:foo
()
t
nil


;; Lambda expressions

((lambda (a b) (+ a (* b 3))) 4 5)                   ;19
((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)     ;19
((lambda (a &optional (b 2)) (+ a (* b 3))) 4)       ;10
((lambda (&optional (a 2 b) (c 3 d) &rest x)
   (list a b c d x)))                                ;(2 nil 3 nil nil)
((lambda (&optional (a 2 b) (c 3 d) &rest x)
   (list a b c d x)) 6)                              ;(6 t 3 nil nil)
((lambda (&optional (a 2 b) (c 3 d) &rest x)
   (list a b c d x)) 6 3)                            ;(6 t 3 t nil)
((lambda (&optional (a 2 b) (c 3 d) &rest x)
   (list a b c d x)) 6 3 8)                          ;(6 t 3 t (8))
((lambda (&optional (a 2 b) (c 3 d) &rest x)
   (list a b c d x)) 6 3 8 9 10 11)                  ;(6 t 3 t (8 9 10 11))
((lambda (a b &key c d) (list a b c d))
 1 2)                                                ;(1 2 nil nil)
((lambda (a b &key c d) (list a b c d))
 1 2 :c 6)                                           ;(1 2 6 nil)
((lambda (a b &key c d) (list a b c d))
 1 2 :d 8)                                           ;(1 2 nil 8)
((lambda (a b &key c d) (list a b c d))
 1 2 :c 6 :d 8)                                      ;(1 2 6 8)
((lambda (a b &key c d) (list a b c d))
 1 2 :d 8 :c 6)                                      ;(1 2 6 8)
((lambda (a b &key c d) (list a b c d))
 :a 1 :d 8 :c 6)                                     ;(:a 1 6 8)
((lambda (a b &key c d) (list a b c d))
 :a :b :c :d)                                        ;(:a :b :d nil)
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) 1)                              ;(1 3 nil 1 nil)
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) 1 2)                            ;(1 2 nil 1 nil)
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) :c 7)                           ;(:c 7 nil :c nil)
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) 1 6 :c 7)                       ;(1 6 7 1 (:c 7))
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) 1 6 :d 8)                       ;(1 6 nil 8 (:d 8))
((lambda (a &optional (b 3) &rest x &key c (d a))
   (list a b c d x)) 1 6 :d 8 :c 9 :d 10)            ;(1 6 9 8 (:d 8 :c 9 :d 10))

((lambda (x &optional (y x))
   (declare (special x))
   (describe-frame)
   y)
 3)

((lambda () (let)))


;; Definitions

(kdefun f (x)
  (declare (special x))
  (* x x))

(progn
  (makunbound '*a*)
  (makunbound '*b*)
  (makunbound '*c*)
  (makunbound '*d*)
  (kdefvar *a*)
  (kdefvar *b*)
  (kdefvar *c* 4)
  (kdefvar *d* 5)
  (kdefvar *b* 7)
  (kdefvar *d* 8)
  (list (boundp '*a*) *b* *c* *d*))   ;(nil 7 4 5)

(progn
  (kdefvar *foo* 10 "Documentation of *foo*")
  (kdefvar *bar* :unbound "Documentation of *bar*")
  (kdefvar *baz* 'undocumented)
  (list (documentation '*foo* :variable)
        (documentation '*bar* :variable)
        (documentation '*baz* :variable)))

;; progv

(progv '(a b c) '(1 2) (list a b (boundp 'b) (boundp 'c)))    ;(1 2 T NIL)
(let ((a 3)) (progv '(a) '(4) a))                             ;3
(let ((a 3)) (declare (special a)) (progv '(a) '(4) a))       ;4
(let ((a 2))
  (let ((a 3))
    (declare (special a))
    (progv '(a) '(4) a)))                                     ;2? 4?
(let ((a 2))
  (declare (special a))
  (let ((a 3))
    (progv '(a) '(4) a)))                                     ;3


;; macros

(defmacro with-collection (&body body)
  (let ((var (gensym)))
    `(macrolet ((collect (argument)
                         `(push ,argument , ',var)))
       (let ((,var nil))
         ,@body
         (nreverse ,var)))))

;; gobbler

(si::gobble-declarations
  '((declare foo)
    (declare bar)
    "doc 1"
    (declare baz)
    "str 2"
    (+ 3 4))
  t)

(si::gobble-declarations '())

(si::gobble-declarations '("foo") t)

;; macros expanding into declarations

(funcall
  (let ((x 3))
    (declare (special x))
    #'(lambda () x)))

(macrolet ((baz () '(declare (special x))))
  (funcall
    (let ((x 3))
      (baz)
      #'(lambda () x))))

(defmacro declare-special-x () '(declare (special x)))

(funcall
  (let ((x 3))
    (declare-special-x)
    #'(lambda () x)))


(si::gobble-declarations '((declare-special-x) #'(lambda () x)))

;; Tail recursion in the call history

(defun fact (n)
  (if (= n 0)
      (progn (describe-frame) 1)
      (* n (fact (1- n)))))

(defun fact-iter (n)
  (labels ((iter (product counter)
             (if (> counter n)
                 (progn (describe-frame) product)
                 (iter (* counter product)
                       (+ counter 1)))))
    (iter 1 1)))
