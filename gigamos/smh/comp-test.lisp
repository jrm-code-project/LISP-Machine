;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

;;; This should be in its own package.

(defvar *debug-compiler-errors* nil
  "If non-NIL, when an error is signalled during compilation a break loop
will be entered.  If NIL, the error is handled automatically, reported,
and the test bypassed.")

(defvar *all-compiler-test-sets* nil
  "The list of symbols which name compiler test sets.")

(defvar *test-success-count* 0
  "Count of all tests that have passed.")

(defvar *test-compilation-failure-count* 0
  "Count of all tests that failed during compilation.")

(defvar *test-execution-failure-count* 0
  "Count of all tests that have failed.")

(defmacro compiler-test-set (test-set-name &rest cases)
  (loop while (keywordp (car cases))
        (case (car cases)
          (:ignore (return-from compiler-test-set
                     `(eval-when (compile load eval)
                        (format t ,(format nil "~~%Skipping test set ~a" test-set-name)))))
          (:compile-only )
          (t (format t "Unrecognized COMPILER-TEST-SET keyword ~s in test set ~s"
                     (car cases) test-set-name)))
        (setq cases (cdr cases)))
  (do ((case cases (cdr case))
       (forms nil))
      ((null case)
       `(progn (eval-when (load eval)
                 (pushnew ',test-set-name *all-compiler-test-sets*))
               (eval-when (compile)
                 (format t ,(format nil "~~%Beginning compilation of test set ~a" test-set-name)))
               (eval-when (load eval)
                 (format t ,(format nil "~~%*** Beginning execution of test set ~a" test-set-name)))
               ,@forms))
    (push (compiler-test-set-1 (car case)) forms)))

(defun compiler-test-set-1 (form)
  `(progn (eval-when (compile) (setq *compilation-test-form* ,form))
          (format t "~%Running test ~s")
          ,form
          (eval-when (compile) (setq *compilation-test-form* nil))))

(defmacro test-eq (result &body body)
  (test-eq-com #'eq result body))

(defmacro test-eql (result &body body)
  (test-eq-com #'equal result body))

(defmacro test-equal (result &body body)
   (test-eq-com #'equal result body))

(defun test-eq-com (predicate result body)


(defun test-failure (test-id expected-list result-list


;; Lose:  COMPILER:WARN doesn't use real WARN, hence doesn't see *BREAK-ON-WARNINGS*.

(compiler-test-set foo 123)

(defun foo (x)
  (condition-case (err)
      (warn "Warn: ~s ~s" x 3)
    (error (format t "~%An error ~s has occured." err))))
