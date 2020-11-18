;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL; -*-


(defflavor kbug2-pane ()
	   (tv:interaction-pane tv:process-mixin)
  (:default-init-plist
    :blinker-deselected-visibility :on
    :blinker-flavor 'tv:rectangular-blinker
    :blinker-p t
    :deexposed-typein-action :normal
    :deexposed-typeout-action :normal
    :label "KBUG2"
    :save-bits t
    :process '(kbug2-process))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)
  
(defun kbug2-process (&rest ignore)
  (let* ((window (send *k-debugger-frame* :get-pane 'kbug2))
	 (*terminal-io* window))
    (k-kbug:kbug2)))

(defflavor wimp-pane ()
	   (tv:interaction-pane tv:process-mixin)
  (:default-init-plist
    :blinker-deselected-visibility :on
    :blinker-flavor 'tv:rectangular-blinker
    :blinker-p t
    :deexposed-typein-action :normal
    :deexposed-typeout-action :normal
    :label "WIMP"
    :save-bits t
    :process '(wimp-process))
  :gettable-instance-variables
  :settable-instance-variables
  :inittable-instance-variables)

(defun wimp-process (&rest ignore)
  (let* ((window (send *k-debugger-frame* :get-pane 'wimp))
	 (*terminal-io* window))
    (k-kbug:wimp t)))

(defflavor k-debugger-frame ()
           (tv:bordered-constraint-frame tv:top-box-label-mixin)
           (:default-init-plist :panes
                                '((wimp wimp-pane)
                                  (kbug2 kbug2-pane))
                                :constraints
                                '((k-debugger (kbug2 wimp)
                                              ((kbug2 0.5s0) (wimp :even)))))
           :gettable-instance-variables
           :settable-instance-variables
           :inittable-instance-variables)

(defmethod (k-debugger-frame :selectable-windows) ()
  (list (list tv:name self)))

(defmethod (k-debugger-frame :after :init) (&rest ignore)
  (send self :set-label
	`(:string
	   ,(format nil "K Debugger")
	   :font fonts:metsi
	   :centered))
  (send self :set-selection-substitute
	     (send self :get-pane 'kbug2)))

(defvar *k-debugger-frame* nil "The frame of the K debugger")

(defmethod (k-debugger-frame :before :kill) (&rest ignore)
  (setq *k-debugger-frame* nil))

(compile-flavor-methods wimp-pane kbug2-pane k-debugger-frame)

(tv:add-system-key #\K '(wbug nil) "K Debugger" nil)

(defun wbug (&optional (select-p t) starting-address)
  (when (or (not (boundp '*k-debugger-frame*))
	    (null *k-debugger-frame*)
	    (not (typep *k-debugger-frame* 'k-debugger-frame))
	    (member (send *k-debugger-frame* :status) '(:deactivated)))
    (setq *k-debugger-frame*
	  (make-instance 'k-debugger-frame :activate-p t)))
  (when select-p
    (send *k-debugger-frame* :select))
  *k-debugger-frame*)
