;;; -*- Mode:LISP; Package:CLC; Readtable:CL; Base:10 -*-

;;; OBJECT OF COMPILATION.
;;; transform to an anotated form of code
;;; suitable for good code generation.

;;; special forms are preserved. transform the code as little as possible

(defstruct (compiler-env (:constructor make-compiler-env-1))
  symbol-table
  generated-code)

(defun make-compiler-env ()
  (make-compiler-env-1 :symbol-table (make-hash-table :test 'eq)))

(defun cget (symbol key)
  (cond ((not *compiler-env*)
         (get symbol key))
        ('else
         (multiple-value-bind (alist foundp)
             (gethash symbol (compiler-env-symbol-table *compiler-env*))
           (cond ((not foundp)
                  (get symbol key))
                 ('else
                  (let ((cell (assq key alist)))
                    (cond (cell
                           (cdr cell))
                          ('else
                           (get symbol key))))))))))

(defun cput (symbol key value)
  (cond ((not *compiler-env*)
         (get symbol key))
        ('else
         (let ((table (compiler-env-symbol-table *compiler-env*)))
           (multiple-value-bind (alist foundp)
               (gethash symbol table)
             (cond ((not foundp)
                    (setf (gethash table symbol) (list (cons key value)))
                    value)
                   ('else
                    (let ((cell (assq key alist)))
                      (cond (cell
                             (setf (cdr cell) value))
                            ('else
                             (setf (gethash table symbol) (cons (cons key value) alist))
                             value))))))))))

(DEFVAR *BREAK-OFF-FORMS* NIL)

(DEFUN BREAK-OFF-FORM (X)
  (SETQ *BREAK-OFF-FORMS* (APPEND (LIST X) *BREAK-OFF-FORMS*)))

(defun cl-toplevel-form (form)
  (LET ((*BREAK-OFF-FORMS* NIL))
    (BREAK-OFF-FORM FORM)
    (DO ()
        ((NOT *BREAK-OFF-FORMS*))
      (CL-TOPLEVEL-FORM-1 (POP *BREAK-OFF-FORMS*)))))

(DEFUN CL-TOPLEVEL-UNHANDLED (FORM)
  (FORMAT T "~&Unhandled toplevel form: ~S~%" form))

(DEFUN CL-TOPLEVEL-FORM-1 (FORM)
  (TYPECASE FORM
    (CONS
     (LET ((OPER (CAR FORM)))
       (TYPECASE OPER
         (SYMBOL
          (LET (F)
            (COND ((SETQ F (CGET OPER 'CL-TOPLEVEL-FORM))
                   (FUNCALL F FORM))
                  ((SETQ F (CGET OPER 'MACRO))
                   (CL-TOPLEVEL-FORM-1 (FUNCALL F FORM)))
                  ((AND (FBOUNDP OPER) (CONSP (SETQ F (FSYMEVAL OPER))) (EQ (CAR F) 'MACRO))
                   (CL-TOPLEVEL-FORM-1 (FUNCALL (CDR F) FORM)))
                  ('ELSE
                   (CL-TOPLEVEL-UNHANDLED FORM)))))
         (t
          (CL-TOPLEVEL-UNHANDLED FORM)))))
    (T
     (CL-TOPLEVEL-UNHANDLED FORM))))


(DEF-CL-TOPLEVEL-FORM DEFUN (FORM)
  (format t "~&Compiling ~S~%" (cadr form)))


(def-cl-toplevel-form progn (form)
  (mapc #'cl-toplevel-form (cdr form)))


(defun cl-eval (form)
  (eval form))

(def-cl-toplevel-form eval-when (form)
  (let ((whens (cadr form))
        (body (cddr form)))
    (when (memq 'compile whens)
      (cl-eval (cons'progn body)))
    (when (memq 'load whens)
      (mapc #'cl-toplevel-form body))))
