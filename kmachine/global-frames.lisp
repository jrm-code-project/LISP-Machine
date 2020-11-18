;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-

;;;; Global Frames


;;; This is an extracted and slightly modified version of
;;; some code in the compiler (in nmacros)
;;; This will allow assembly code to use global frame variables

(export '(
          prims::define-global-frame
          prims::define-global-variable
          prims::define-global-constant
          )
        "PRIMS")


(defvar *global-frames* '() "A-list of global register frames")

(defvar *global-constants* '() "A-list of constants")

(defun frame-num (frame)
  (position frame *global-frames* :key #'car))

(defmacro prims:define-global-frame (frame-name)
  `(define-global-frame-1 ',frame-name))

(defun define-global-frame-1 (frame-name)
  (unless (member frame-name *global-frames* :key #'car)
    (if (< (length *global-frames*) hw:number-of-global-frames)
        (setq *global-frames*
              (nconc *global-frames* (list (list frame-name))))
      (error "No more global frames"))))

(defmacro prims:define-global-variable (frame-name register-name &optional value documentation)
  `(progn (define-global-register-1 ',frame-name ',register-name)
          (defvar ,register-name ,value ,documentation)))

(defmacro prims:define-global-constant (frame-name register-name &optional value documentation)
  `(progn (define-global-register-1 ',frame-name ',register-name)
          (pushnew (cons ',value ',register-name) *global-constants* :test #'equal)
          (defconstant ,register-name ,value ,documentation)))  ;temp, for simulation

(defun define-global-register-1 (frame-name register-name)
  (let ((frame (assoc frame-name *global-frames*)))
    (if frame
        (let ((frame-num (frame-num frame-name))
              (frame-length (length (cdr frame))))
          (if (< frame-length hw:frame-size)
              (unless (member register-name (cdr frame))
                (setf (cdr frame)
                      (nconc (cdr frame) (list register-name)))
;;; this is for the compiler
;               (setf (get register-name 'global-register)
;                     (let ((var (create-variable register-name)))
;                       (setf (variable-loc var)
;                             (list 'K:REGISTER
;                                   register-name
;                                   frame-num
;                                   frame-length))
;                       var))
                (setf (get register-name :register)
                      (list frame-name frame-num frame-length)))
            (error "No more slots in frame ~a" frame-name)))
      (error "No frame named ~a" frame-name))))

(defun global-register (register-name)
  (get register-name 'global-register))


(prims:define-global-frame lisp)

(prims:define-global-constant lisp constant-t   t)
(prims:define-global-constant lisp constant-nil nil)
(prims:define-global-constant lisp constant-0   0)
(prims:define-global-constant lisp constant-1   1)

(prims:define-global-variable lisp *stack-pointer*)
