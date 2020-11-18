;;; -*- Mode:LISP; Package:USER; Base:10 -*-

;;; this simple deep-binding (maclisp, lisp 1.5) style interpreter
;;; is about a factor of 7 times faster computing FIB than
;;; the present grossly-haxed lexical interpreter.
;;; 29-Aug-86 18:07:15 -gjc

(defmacro defoon (name args &body body)
  "A maclisp compatible form of DEFUN. The body is evaluated using dynamic scoping rules"
  `(fset-carefully ',name (make-defoon ',args ',body)))

(defun make-defoon (formal-parameters code)
  #'(lambda (&rest actual-parameters)
      (progv formal-parameters
             actual-parameters
        (devaln code))))

(defun deval (exp)
  (typecase exp
    (cons
     (let ((f (get (car exp) 'fexpr)))
       (cond (f
              (funcall f (cdr exp)))
             ((and (consp (setq f (fsymeval (car exp))))
                   (eq (car f) 'macro))
              (deval (funcall (cdr f) exp)))
             ('else
              (%ASSURE-PDL-ROOM (length exp))
              (%OPEN-CALL-BLOCK f 0 2)
              (setq f (cdr exp))
              (do ()
                  ((null f))
                (%push (deval (pop f))))
              (%ACTIVATE-OPEN-CALL-BLOCK)))))
    (symbol
     (symeval exp))
    (t
     exp)))

(defun devaln (l)
  (do ((l l (cdr l)))
      ((null (cdr l))
       (deval (car l)))
    (deval (car l))))


(defun (:property quote fexpr) (l)
  (car l))

(defun (:property let fexpr) (l)
  (do ((args (car l) (cdr args))
       (p))
      ((null args)
       (devaln (cdr l)))
    (setq p (car args))
    (cond ((atom p)
           (si:%bind (locf (symbol-value p)) nil))
          ('else
           (si:%bind (locf (symbol-value (car p))) (deval (cadr p)))))))

(defun (:property do fexpr) (l)
  (let ((varp (car l))
        (testp (cadr l))
        (body (cddr l)))
    (do ((args varp (cdr args))
         (p))
        ((null args)
         (catch 'return
           (do ()
               ((deval (car testp))
                (devaln (cdr testp)))
             (devaln body)
             (%OPEN-CALL-BLOCK #'set-l 0 0)
             (dolist (e varp)
               (setq p (cddr e))
               (when p
                 (%push (car e))
                 (%push (deval (car p)))))
             (%ACTIVATE-OPEN-CALL-BLOCK))))
      (setq p (car args))
      (si:%bind (locf (symbol-value (car p))) (deval (cadr p))))))

(defun set-l (&rest l)
  (do ()
      ((null l))
    (set (pop l) (pop l))))

(defun (:property cond fexpr) (l)
  (dolist (c l)
    (let ((p (deval (car c))))
      (when p
        (if (cdr c)
            (return (devaln (cdr c)))
          (return p))))))


(defun (:property push fexpr) (l)
  (set (cadr l) (cons (deval (car l)) (symeval (cadr l)))))

(defun (:property case fexpr) (l)
  (let ((value (deval (car l))))
    (dolist (c (cdr l))
      (when (if (consp (car c))
                (member value (car c))
              (or (eql value (car c))
                  (eql (car c) t)))
        (return (devaln (cdr c)))))))


(defun (:property setq fexpr) (l)
  (do ((v))
      ((null l) v)
    (set (pop l) (setq v (deval (pop l))))))


(defun (:property or fexpr) (l)
  (do ((v))
      ((null (cdr l))
       (deval (car l)))
    (and (setq v (deval (pop l)))
         (return v))))

(defun (:property and fexpr) (l)
  (do ()
      ((null (cdr l))
       (deval (car l)))
    (or (deval (pop l))
        (return nil))))


(defun (:property return fexpr) (l)
  (throw 'return (deval (car l))))


(defun (:property if fexpr) (l)
  (if (deval (car l))
      (deval (cadr l))
    (devaln (cddr l))))
