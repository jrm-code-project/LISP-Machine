;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-

;;;; Rewriters

(export '(defrewrite def-rewrite-patterns))

;;; A Rewriter is just like a macro
;;; except the definition is kept in this table
;;; instead of in the function cell because
;;; there needs to be a function of the same name defined

(defvar *rewriter-table* (make-table))

(defun expand-rewriter (exp env)
  (let ((entry (table-entry *rewriter-table* (car exp))))
    (if entry
        (funcall entry exp env)
      exp)))

(defmacro defrewrite (name lambda-list &body body)
  "WARNING: Defrewrites can not be used in the same file in which they are defined
They only take effect when the KENV is loaded."
  (let ((gen-name (gensym name)))
    `(progn
       (defun . ,(cdr (si:expand-defmacro (list* gen-name lambda-list body) nil)))
       (setf (table-entry *rewriter-table* ',name)
             (symbol-function ',gen-name)))))


;;;; Pattern matcher for rewriters

#||||||||||||||||||||||||||||||||||||||||

(def-rewrite-patterns count (item sequence)
  (()                  `(COUNT-EQL ,item ,sequence))
  ((&key (test #'eq))  `(COUNT-EQ ,item ,sequence))
  ((&key start end)    `(COUNT-1 ,item ,sequence ,start ,end))
  ((&key from-end test start end key)
                       `(COUNT-1 ,item ,sequence ,start ,end ,key ,test)))

||||||||||||||||||||||||||||||||||||||||#

(defun get-pattern (pattern)
  (setq pattern (lisp:copy-list pattern))
  (do ((keys (cdr (member '&key pattern))
             (cdr keys)))
      ((null keys))
    (if (consp (car keys))
        (setf (car keys) (cons (intern (symbol-name (caar keys)) 'keyword)
                               (cdar keys)))
      (setf (car keys) (intern (symbol-name (car keys)) 'keyword))))
  pattern)

(defun get-vars (pattern)
  (let ((vars '()))
    (dolist (elt pattern)
      (cond ((eq elt '&key))
            ((eq elt '&rest))
            ((consp elt)
             (push (car elt) vars))
            (t
             (push elt vars))))
    (nreverse vars)))

(defmacro def-rewrite-patterns (name required-args &body patterns)
  `(DEFREWRITE ,name (&WHOLE FORM ,@required-args &REST ARGS &AUX MATCH? VALUES)
     (COND
       ,@(mapcar #'(lambda (pattern)
                     `((MULTIPLE-VALUE-SETQ (MATCH? VALUES)
                         (MATCH? ',(get-pattern (car pattern)) ARGS))
                       (MULTIPLE-VALUE-BIND ,(get-vars (car pattern)) (VALUES-LIST VALUES)
                         ,@(cdr pattern))))
                 patterns)
       (T FORM))))

(defun match? (pattern args)
  (do ((values '()))
      ((null pattern) (if (null args) (values t (nreverse values))))
    (multiple-value-bind (match? rest-pattern rest-args value)
        (match-next pattern args)
      (if match?
          (progn (setq pattern rest-pattern
                       args rest-args)
                 (push value values))
        (return nil)))))


(defun all-keywords (args)
  (do ((args args (cddr args)))
      ((null args) t)
    (unless (keywordp (car args))
      (return nil))))


(defun match-next (pattern args)
  (cond ((eq (car pattern) '&key)
         (let* ((key (cadr pattern))
                (keyword (if (consp key) (car key) key))
                (value (if (consp key) (cdr key) nil)))
           (let ((tail (member keyword args)))
             (if tail
                 (if (or (null value)
                         (equal (cadr tail) (car value)))
                     (values t
                             (if (cddr pattern)
                                 (cons '&key (cddr pattern)))
                             (lisp:remove (cadr tail) (lisp:remove (car tail) args))
                             (cadr tail))
                   nil)
               (if (all-keywords args)
                   (values t
                           (if (cddr pattern)
                               (cons '&key (cddr pattern)))
                           args
                           nil)
                 nil)))))
        ((eq (car pattern) '&rest)
         (values t nil nil args))
        (args
         (values t (cdr pattern) (cdr args) (car args)))
        (t (values nil nil nil))))
