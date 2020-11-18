;;; -*- Mode:LISP; Package:SIM; Base:10 -*-

;;;; Memory Simulation

(eval-when (eval compile load)
  (defpackage "KSI" :use NIL))

(defvar *ksi-package* (find-package 'ksi))

(defvar *low-level-simulate* t
  "Set this to NIL to use Lambda memory primitives,
T to use simulated K machine memory")

;;;; System Constants

(defconstant %%ptr-data-type (byte 6. 19.))
(defconstant %%ptr-pointer (byte 18. 0))

(defconstant %header-type-list 3)

(defconstant %%array-type-field        (byte 5 19.))
(defconstant %%array-leader-bit        (byte 1 17.))
(defconstant %%array-number-dimensions (byte 3 12.))
(defconstant %%array-long-length-flag  (byte 1 11.))

(defconstant %array-max-short-index-length 1023.)

(defconstant ksi:maximum-area-number 255.)
(defconstant ksi:page-size 255.)

(defconstant ksi:working-storage-area 0)
(defconstant ksi:extra-pdl-area 1)


;;;; Simulation Stuff

(defvar *main-memory-size* 16384.)
(defvar *main-memory* (make-array *main-memory-size*))

(defun kptr (ptr)
  (format nil "{~a ~d}"
          (nth (ldb %%ptr-data-type ptr) q-data-types)
          (ldb %%ptr-pointer ptr)))

(defun lm (start &optional (end (+ start 8.)))
  (setq end (ldb %%ptr-pointer end))
  (do ((i (ldb %%ptr-pointer start) (1+ i)))
      ((> i end))
    (let ((v (aref *main-memory* i)))
      (format t "~&~a// ~a ~a" i v (if (integerp v)
                                       (kptr v)
                                     "")))))

(defreg K:MD nil nil)
(defreg K:VMA nil nil)

(defun K:MD (data)
  (setq K:MD data))

(defun K:VMA (addr)
  (setq K:VMA addr))

(defun K:VMA-START-WRITE (addr)
  (setq addr (ldb %%ptr-pointer addr))
  (if (> addr *main-memory-size*)
      (error "Non Existant Memory!"))
  (setq K:VMA addr)
  (setf (aref *main-memory* addr) K:MD))

(defun K:VMA-START-READ (addr)
  (setq addr (ldb %%ptr-pointer addr))
  (if (> addr *main-memory-size*)
      (error "Non Existant Memory!"))
  (setq K:VMA addr)
  (setq K:MD (aref *main-memory* addr)))
