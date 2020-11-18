;;; -*- Mode:LISP; Package:K-KBUG; Readtable:CL; Base:10 -*-

;;; This is a wimpy terminal connection to the K


(defmacro without-more-processing (the-window &body body)
  "execute body with more processing disabled"
  (let ((more-p (gentemp "morep"))
	(window (gentemp "window")))
    `(let* ((,window ,the-window)
	    (,more-p (si::send ,window :more-p)))
       (unwind-protect
	   (progn (si::send ,window :set-more-p nil)
		  ,@body)
	 (si::send ,window ':set-more-p ,more-p)))))


(defun wimp (&optional half-duplex)
  "communicate with the K stream"
  (without-more-processing *terminal-io*
    (si::*catch 'eof
      (let (p)
	(unwind-protect
	    (progn (setq p (si::process-run-function
			     "wimp input from K"
			     #'wimp-tty-characters-from-k
			     *terminal-io*
			     si::current-process))
		   (cond (half-duplex
			  (do ((line))
			      (nil)
			    (setq line (si::prompt-and-read :string ""))
			    (do ((j 0 (1+ j))
				 (n (length line)))
				((= j n))
			      (wimp-send-1-to-k (aref line j)))
			    (wimp-send-1-to-k #\return)))
			 ('else
			  (si::do-forever
			    (wimp-send-1-to-k (read-char *terminal-io*))))))
	  (and p (si::send p :kill)))))))


(defun wimp-tty-characters-from-k (to superior)
  (si::condition-case (x)
      (do ((c))
	  ((null (setq c (kbug:read-from-k-stream)))
	   (format to "~&***CONNECTION CLOSED AT REMOTE END***~%"))
	(write-char c to))
    (si::error
     (si::send x :report to)))
  (si::send superior :interrupt
	#'(lambda () (si::*throw 'eof nil)))
  (si:process-wait-forever))


(defun wimp-send-1-to-k (char)
  (write-to-k-character-stream char))

(defun wimput-proc (*terminal-io*)
  "communicate with the K stream"
  (without-more-processing *terminal-io*
    (si::*catch 'eof
      (do ((line))
	  (nil)
	(setq line (si::prompt-and-read :string ""))
	(do ((j 0 (1+ j))
	     (n (length line)))
	    ((= j n))
	  (wimp-send-1-to-k (aref line j)))
	(wimp-send-1-to-k #\return)))))

(defun wimpout-proc (*terminal-io*)
  (loop do
	(zl:catch-error-restart (sys:abort "Wimpout")
	  (si::condition-case (x)
	      (do ((c))
		  ()
		(unless (setq c (kbug:read-from-k-stream))
		  (format *terminal-io* "~&***CONNECTION CLOSED AT REMOTE END***~%")
		  (zl:process-wait "Reopen" #'(lambda ()
						(setq c (kbug:read-from-k-stream)))))
		(write-char c *terminal-io*))
	    (si::error
	      (si::send x :report *terminal-io*))))))



(zl:defflavor wimpy-falcon () (tv:process-mixin tv:pane-mixin tv:window))

(zl:defflavor falcon-food () (tv:process-mixin tv:pane-mixin tv:window))

(zl:defflavor wimp () (tv:bordered-constraint-frame-with-shared-io-buffer
			tv:select-mixin
			tv:FULL-SCREEN-HACK-MIXIN
			tv:FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
			tv:inferiors-not-in-select-menu-mixin
			tv:alias-for-inferiors-mixin
			tv:label-mixin)
  (:DEFAULT-INIT-PLIST :SAVE-BITS :DELAYED
    :panes '((falcon wimpy-falcon
		     :label nil
		     :more-p nil
		     :process (wimpout-proc))
	     (input falcon-food
		    :label "Input"
		    :more-p nil
		    :process (wimput-proc)))
    :constraints '((main . ((falcon input)
			    ((input 3 :lines))
			    ((falcon :even)))))))

(tv:add-system-key #\Greek-L 'wimp "Falcon Listener window. Do m-L in KBUG2")
