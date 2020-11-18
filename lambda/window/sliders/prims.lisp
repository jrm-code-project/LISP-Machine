;;; -*- Mode:LISP; Package:OBIE; Readtable:CL; Base:10 -*-

;;; Macros and lisp hacks
;;; OBIE primitive classes

; A useful macro for defining a class with instace variables
(defmacro defobclass (name superiors &body varlist)
  `(progn
     (pushnew ',name *all-object-classes*)
     (defkind ,name ,@superiors)
     (let ((fs:inhibit-fdefine-warnings t))   ;Instance variable definition recording is broken, hence this.
       ,@(loop for var-form in varlist
               collect `(definstancevar (,(if (symbolp var-form) var-form (car var-form)) ,name)
                                        ,@(if (listp var-form) (cdr var-form)))))))

(defvar *all-object-classes* nil)                      ;For debugging only

;;; The current version of this is broken due to defsubst stupidity.
obj:
(defsubst object? (object)
  (typep object 'obj))

; This is in sources but hasn't made it to released system yet--allow list of classes
obj:
(defun ONEOF (class-obj &rest exist-args &aux instance-obj)
  (setq instance-obj (%kindof t nil t (if (listp class-obj) (copy-list class-obj) (ncons class-obj))))
  (let ((*object instance-obj))
    (apply-fcn-sym 'exist exist-args))
  instance-obj)

; Predicate to tell if an object incorporates a particular class.  Like (typep <instance> <flavor>).
(defobfun obj-classp (obclass &optional (obj obj:*object) &aux class-obj base-objs)
  (setq class-obj (cond ((object? obclass) obclass)
                        ((symbolp obclass) (symeval obclass))))
  (setq base-objs (base-objs obj))
  (or (memq class-obj base-objs)
      (dolist (base-obj base-objs)
        (if (obj-classp class-obj base-obj)
            (return t)))))

; Also missing from oblisp
(defmacro MAPCAN-ASK (objs &body body)
  `(mapcan #'(lambda (obj) (ask obj ,@body)) ,objs))


; Like symbol-value, but works for object instance vars
obj:
(defun obj-symbol-value (sym &optional (obj *object))
  (check-obj obj)
  (if (global-obj? obj)
      (symbol-value sym)
    (binding-val (binding-from-env sym (own-env obj)))))

; Opposite of PUSH, sort of
(defmacro pull (thing place)
  `(setf ,place (remove ,thing ,place)))

(defobclass display-object ())
(defclassvar (mouseable-p display-object) nil)         ;default is non-mouseable

(defobclass point (display-object)
  x y)

(defobclass window-object (display-object)
  window)

;;; Basic object management
(defobfun (exist window-object) (&rest stuff)
  (apply 'shadowed-exist stuff)
;  (format foo "~&Exist: ~A, window=~A" obj:*object window)
  (send window :add-object obj:*object))

(defobfun (disappear window-object) ()
  (erase)
  (send window :remove-object obj:*object))

(defobclass window-point (point window-object))


(defobfun (move point) (nx ny)
;  (format foo "~&Move: ~A (~D, ~D)" (current-obj) nx ny)
  (setq x nx
        y ny))

(defobfun (move-relative point) (dx dy)
  (move (+ x dx) (+ y dy)))

; A rectangle
(defobclass rect (point)
  x y width height)

(defobfun (point-in-region rect) (px py)
  (and ( x px (+ x width))
       ( y py (+ y height))))

;+++ rectangle-overlap

; A rectangle belonging to a specific window
(defobclass window-rect (rect window-object))

(defobfun (draw-border window-rect) ()
  (send window :draw-lines tv:alu-ior x y x (+ y height -1) (+ x width -1) (+ y height -1) (+ x width -1) y x y))

; +++ shading

; The default drawing method is XOR, a hairier object can redefine this
(defobfun (erase display-object) ()
  (draw))

(defobfun (remove-frippery display-object) ()
  )
