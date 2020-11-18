;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8 -*-

(defvar eval-tests nil)

(defun setup-eval-tests ()
  (setq eval-tests nil)
  (do* ((i 1 (1+ i))
        (func (intern-soft (format nil "EVAL-TEST-~d" i) 'si)
              (intern-soft (format nil "EVAL-TEST-~d" i) 'si)))
       ((or (null func)
            (not (fboundp func))))
    (push func eval-tests))
  (setq eval-tests (reverse eval-tests)))

(defun test-evaluator ()
  (setup-eval-tests)
  (dolist (test eval-tests)
    (if (null (funcall test))
        (format t "~&Fail ~s" test))))

;keywords evaluate to themselves
(defun eval-test-1 ()
  (eq (eval ':foo)
      :foo))

;special variables work
(defun eval-test-2 ()
  (eq (eval '%%q-pointer)
      %%q-pointer))

;atoms self evaluate
(defun eval-test-3 ()
  (eq (eval current-stack-group)
      current-stack-group))

;quote
(defun eval-test-4 ()
  (eq (eval '(quote foo))
      'foo))

;test finding a function hidden in symbols, closures, and things that look microcoded but aren't

(defun eval-test-helper-no-args ()
  t)

(defun eval-test-helper-one-arg (arg)
  arg)

(defun eval-test-helper-one-optional-arg (&optional arg)
  arg)


(defun eval-test-5 ()
  (eq (eval '(eval-test-helper-no-args)) t))

(defun eval-test-6 ()
  (let ((x (list nil)))
    (eq (eval `(eval-test-helper-one-arg ',x)) x)))

(defun eval-test-7 ()
  (eq (eval '(eval-test-helper-one-optional-arg)) nil))

(defun eval-test-8 ()
  (let ((x (list nil)))
    (eq (eval `(eval-test-helper-one-optional-arg ',x)) x)))

(defun eval-test-helper-rest-arg (&rest rest)
  (copylist rest))

(defun eval-test-helper-one-arg-and-rest (arg &rest rest)
  (cons arg (copylist rest)))

(defun eval-test-9 ()
  (let* ((x (list nil))
         (y (eval `(eval-test-helper-one-arg-and-rest ',x))))
    (and (eq x (car y))
         (null (cdr y)))))

(defun eval-test-10 ()
  (let* ((x (list nil))
         (y (list nil))
         (z (eval `(eval-test-helper-one-arg-and-rest ',x ',y))))
    (and (eq x (car z))
         (equal y (cadr z))
         (null (cddr z)))))

(defun eval-test-11 ()
  (let* ((a (list nil))
         (b (list nil))
         (c (list nil))
         (z (eval `(eval-test-helper-one-arg-and-rest ',a ',b ',c))))
    (and (eq a (car z))
         (eq b (cadr z))
         (eq c (caddr z))
         (null (cdddr z)))))

(defun eval-test-helper-one-optional-arg-and-rest (&optional arg &rest rest)
  (cons arg (copylist rest)))

(defun eval-test-12 ()
  (equal (eval-test-helper-one-optional-arg-and-rest) '(nil)))

(defun eval-test-13 ()
  (let* ((x (list nil))
         (y (eval `(eval-test-helper-one-optional-arg-and-rest ',x))))
    (and (eq x (car y))
         (null (cdr y)))))

(defun eval-test-14 ()
  (let* ((x (list nil))
         (y (list nil))
         (z (eval `(eval-test-helper-one-optional-arg-and-rest ',x ',y))))
    (and (eq x (car z))
         (equal y (cadr z))
         (null (cddr z)))))

(defun eval-test-15 ()
  (let* ((a (list nil))
         (b (list nil))
         (c (list nil))
         (z (eval `(eval-test-helper-one-optional-arg-and-rest ',a ',b ',c))))
    (and (eq a (car z))
         (eq b (cadr z))
         (eq c (caddr z))
         (null (cdddr z)))))
