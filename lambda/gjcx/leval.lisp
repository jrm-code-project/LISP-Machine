;;; -*- Mode:LISP; Package:USER; Base:10 -*-


;;; A hacked up lexical interpreter.
;;; 1-Sep-86 09:07:53. Doesnt handle special declarations
;;; since these *could* be preprocessed.
;;; Just to see how fast regular EVAL *could* be.

(defmacro defune (name args &body body)
  `(fset-carefully ',name (make-defune ',args ',body)))

(defun make-defune (formal-parameters code)
  #'(lambda (&rest actual-parameters)
      (levaln code (mapcar #'cons formal-parameters actual-parameters))))

(defun leval (exp env)
  (typecase exp
    (cons
     (let ((f (get (car exp) 'lfexpr)))
       (cond (f
              (funcall f (cdr exp) env))
             ((and (consp (setq f (fsymeval (car exp))))
                   (eq (car f) 'macro))
              (leval (funcall (cdr f) exp) env))
             ('else
              (%ASSURE-PDL-ROOM (length exp))
              (%OPEN-CALL-BLOCK f 0 2)
              (setq f (cdr exp))
              (do ()
                  ((null f))
                (%push (leval (pop f) env)))
              (%ACTIVATE-OPEN-CALL-BLOCK)))))
    (symbol
     (let ((f (assq exp env)))
       (cond (f
              (cdr f))
             ('else
              (symeval exp)))))
    (t
     exp)))

(defun levaln (l env)
  (do ((l l (cdr l)))
      ((null (cdr l))
       (leval (car l) env))
    (leval (car l) env)))


(defun (:property quote lfexpr) (l env)
  env
  (car l))

(defun (:property let lfexpr) (l env)
  (do ((args (car l) (cdr args))
       (p)
       (nenv env))
      ((null args)
       (levaln (cdr l) nenv))
    (setq p (car args))
    (cond ((atom p)
           (push (cons p nil) nenv))
          ('else
           (push (cons (car p) (leval (cadr p) env)) nenv)))))

(defun (:property do lfexpr) (l env)
  (let ((varp (car l))
        (testp (cadr l))
        (body (cddr l))
        (nenv env))
    (do ((args varp (cdr args))
         (p))
        ((null args)
         (catch 'return
           (do ()
               ((leval (car testp) nenv)
                (levaln (cdr testp) nenv))
             (levaln body nenv)
             (%OPEN-CALL-BLOCK #'lset-l 0 0)
             (%push nenv)
             (dolist (e varp)
               (setq p (cddr e))
               (when p
                 (%push (car e))
                 (%push (leval (car p) nenv))))
             (%ACTIVATE-OPEN-CALL-BLOCK))))
      (setq p (car args))
      (push (cons (car p) (leval (cadr p) env)) nenv))))

(defun lset-l (env &rest l)
  (do ()
      ((null l))
    (setf (cdr (assq (pop l) env)) (pop l))))

(defun (:property cond lfexpr) (l env)
  (dolist (c l)
    (let ((p (leval (car c) env)))
      (when p
        (if (cdr c)
            (return (levaln (cdr c) env))
          (return p))))))


(defun lsymeval (s env)
  (let ((cell (assq s env)))
    (if cell (cdr cell) (symeval s))))

(defun l-set (s env v)
  (let ((cell (assq s env)))
    (if cell
        (setf (cdr cell) v)
      (setf (symeval s) v))))

(defun (:property push lfexpr) (l env)
  (l-set (cadr l) env (cons (leval (car l) env) (lsymeval (cadr l) env))))

(defun (:property case lfexpr) (l env)
  (let ((value (leval (car l) env)))
    (dolist (c (cdr l))
      (when (if (consp (car c))
                (member value (car c))
              (or (eql value (car c))
                  (eql (car c) t)))
        (return (levaln (cdr c) env))))))


(defun (:property setq lfexpr) (l env)
  (do ((v))
      ((null l) v)
    (l-set (pop l) env (setq v (leval (pop l) env)))))


(defun (:property or lfexpr) (l env)
  (do ((v))
      ((null (cdr l))
       (leval (car l) env))
    (and (setq v (leval (pop l) env))
         (return v))))

(defun (:property and lfexpr) (l env)
  (do ()
      ((null (cdr l))
       (leval (car l) env))
    (or (leval (pop l) env)
        (return nil))))


(defun (:property return lfexpr) (l env)
  (throw 'return (leval (car l) env)))


(defun (:property if lfexpr) (l env)
  (if (leval (car l) env)
      (leval (cadr l) env)
    (levaln (cddr l) env)))
