;;; -*- Mode:LISP; Package:TV; Patch-File:T; Readtable:CL; Base:10 -*-

;; New file 24oct88
;; ||| Moved defmethod here from wimp-terminal to flush make-system redef warning - smh&saz 24oct88
;;  You see, this file is a PATCH-FILE.

;;; ||| Put this file in TV package and removed package prefix below. -Keith 26oct88

(defmethod (stream-mixin :rubout-handler) (options function &rest args)
  (declare (arglist rubout-handler-options function &rest args))
  (if (and (eq rubout-handler self) (not (cdr (assq :nonrecursive options))))
      (let ((rubout-handler-options (append options rubout-handler-options)))
	(apply function args))
    (let ((rubout-handler-options options))
      (if ( (rhb-fill-pointer) (rhb-scan-pointer))
	  (setf (rhb-fill-pointer) 0)
	(progn 
	  (copy-array-portion rubout-handler-buffer (rhb-scan-pointer) (rhb-fill-pointer)
			      rubout-handler-buffer 0 (array-length rubout-handler-buffer))
	  (if (numberp (rhb-typein-pointer))
	      (decf (rhb-typein-pointer) (rhb-scan-pointer)))
	  (decf (rhb-fill-pointer) (rhb-scan-pointer))))
      (setf (rhb-scan-pointer) 0 (rhb-status) :initial-entry)
      (catch 'return-from-rubout-handler
	(progv (unless (boundp 'prompt-starting-x)	;smh
		 '(prompt-starting-x prompt-starting-y
				     rubout-handler-starting-x
				     rubout-handler-starting-y))
	       (unless (boundp 'prompt-starting-x) '(nil nil nil nil))
	  (let ((rubout-handler self)
		(rubout-handler-inside self)
		(rubout-handler-re-echo-flag nil)
		(rubout-handler-activation-character nil))
	    (multiple-value (prompt-starting-x prompt-starting-y) (send self :read-cursorpos))
	    (setq rubout-handler-starting-x prompt-starting-x
		  rubout-handler-starting-y prompt-starting-y)
	    (do-forever
	      (setq rubout-handler-re-echo-flag nil)
	      (catch 'rubout-handler				;Throw here when rubbing out
		(condition-case (error)
				(return
				 (multiple-value-prog1
				   (apply function args)	;Call READ or whatever.
				   (setf (rhb-fill-pointer) (rhb-scan-pointer))
				   (and (rhb-typein-pointer)
					(> (rhb-typein-pointer) (rhb-fill-pointer))
					(setf (rhb-typein-pointer) (rhb-fill-pointer)))))
				(sys:parse-error
				 (send self :fresh-line)
				 (princ ">>ERROR: " self)
				 (send error :report self)
				 (send self :fresh-line)
				 (setq rubout-handler-re-echo-flag t)
				 (do-forever (send self :tyi)))))	;If error, force user to rub out
	      ;;Maybe return when user rubs all the way back
	      (and (zerop (rhb-fill-pointer))
		   (let ((full-rubout-option (assq :full-rubout rubout-handler-options)))
		     (when full-rubout-option
		       ;; Get rid of the prompt, if any.
		       (send self :clear-between-cursorposes
			     prompt-starting-x prompt-starting-y
			     (- cursor-x left-margin-size) (- cursor-y top-margin-size))
		       (send self :set-cursorpos prompt-starting-x prompt-starting-y)
		       (return (values nil (cadr full-rubout-option)))))))))))))
