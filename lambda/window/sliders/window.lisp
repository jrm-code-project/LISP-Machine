;;; -*- Mode:LISP; Package:OBIE; Readtable:CL; Base:10 -*-

(defobclass window ()
  (mouse-char nil)
  mouse-font
  mouse-hot-x
  mouse-hot-y)

(defobclass object-managing-window (window)
  (objects nil)
  (mouseable-objects nil)
  (old-mouse-x 0)
  (old-mouse-y 0)
  (mouse-highlighted-object nil)
  )

;+++!
(defobfun (draw object-managing-window) ()
  (mapc-ask objects (draw)))

(defobfun (refresh object-managing-window) ()
  (draw))

(defobfun (obj-under-point object-managing-window) (x y &optional class non-mouseable-objects)
  (dolist (obj (if non-mouseable-objects objects mouseable-objects))
    (and (or (not class)
             (obj-classp class obj))
         (ask-funcall obj 'point-in-region x y)
         (return obj))))

(defobfun (obj-under-mouse object-managing-window) (&optional class non-mouseable-objects)
  (obj-under-point (- tv:mouse-x (send tv-window :x-offset) (tv:sheet-left-margin-size tv-window))
                   (- tv:mouse-y (send tv-window :y-offset) (tv:sheet-top-margin-size tv-window))
                   class non-mouseable-objects))

(defobfun (mouse-moves object-managing-window) (x y)
  (dolist (obj mouseable-objects)
    (when (ask-funcall obj 'point-in-region old-mouse-x old-mouse-y)
      (unless (ask-funcall obj 'point-in-region x y)
        (ask-funcall obj 'mouse-out)
        (setq mouse-highlighted-object nil)
        (return))))
  (dolist (obj mouseable-objects)
    (when (ask-funcall obj 'point-in-region x y)
      (unless (ask-funcall obj 'point-in-region old-mouse-x old-mouse-y)
        (ask-funcall obj 'mouse-in)
        (setq mouse-highlighted-object obj)
        (return))))
  (setq old-mouse-x x
        old-mouse-y y))

(defobfun (mouse-click object-managing-window) (char x y)
  (dolist (obj mouseable-objects)
    (when (ask-funcall obj 'point-in-region x y)
      (ask obj (mouse-click char x y))
      (return))))

(defobfun (add-object object-managing-window) (new-object)
  (push new-object objects)
  (if (ask new-object mouseable-p)
      (push new-object mouseable-objects)))

(defobfun (remove-object object-managing-window) (obj)
  (pull obj objects)
  (pull obj mouseable-objects))


;;; Window system interface

(defflavor obie-window
           ((oblisp-window nil))                                       ;An oblisp object that does the work
           (tv:window)
  (:init-keywords :oblisp-window-class)
  :gettable-instance-variables
  (:default-handler obie-window-handler))

(declare-flavor-instance-variables (obie-window)
(defobfun obie-window-handler (operation &rest args)
  (when oblisp-window
    (ask oblisp-window (apply (intern (symbol-name operation) 'obie) args)))))

(defmethod (obie-window :after :init) (plist)
  (setq oblisp-window (oneof (or (cadr (memq :oblisp-window-class plist)) ghost-window) 'tv-window self)))

;;; This is pointed to by a TV window and does the work for it
(defobclass ghost-window (object-managing-window)
  (tv-window nil))

(defmethod (obie-window :mouse-moves) (x y)
  (obie-window-handler :mouse-moves x y))

(defmethod (obie-window :mouse-click) (char x y)
  (obie-window-handler :mouse-click char x y))

(defmethod (obie-window :after :refresh) (&optional ignore)
  (obie-window-handler :refresh))

(defmethod (obie-window :mouse-standard-blinker) ()
  (obie-window-handler :mouse-standard-blinker))

(defobfun (mouse-standard-blinker window) ()
  (if mouse-char
      (tv:mouse-set-blinker-definition ':character mouse-hot-x mouse-hot-y ':on
                                       ':set-character mouse-char mouse-font)
    (tv:mouse-set-blinker-definition ':character 0 0 ':on
                                     ':set-character 6 'fonts:mouse)))
