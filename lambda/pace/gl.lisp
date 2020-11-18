;;; -*- Mode:LISP; Package:GL; Base:10; Readtable:CL -*-

(defpackage "GNU"
  :use nil)

(defvar *gnu-readtable* nil)

(import '(eval
           nil
           t
           defun
           fset
           defvar
           setq
           let
           defconst
           <=
           1+
           or
           and
           fboundp
           if
           memq
           )
        "GNU")

(defvar gnu:minibuffer-local-map nil)
(defvar gnu:global-map nil)
(defvar gnu:ctl-x-map nil)
(defvar gnu:esc-map nil)
(defvar gnu:exec-directory nil)
(defvar gnu:system-type nil)
(defvar gnu:debugger nil)
(defvar gnu:completion-ignored-extensions nil)
(defvar gnu:disabled-command-hook nil)

(deff gnu:expand-file-name 'ignore)
(deff gnu:autoload 'ignore)


(defun gnu:char-to-string (char)
  (values (make-array 1 :type :art-string :initial-element char)))

(deff gnu:make-sparse-keymap 'ignore)
(deff gnu:make-keymap 'ignore)

(defun gnu:put (symbol property value)
  (putprop symbol value property))

(defun gnu:garbage-collect ()
  t)

(defun gnu:load (file)
  (load (send (fs:merge-pathname-defaults file "angel:/src/emacs/17/lisp/")
              :new-raw-type "el")))

(defun gnu:copy-sequence (seq)
  (copy-seq seq))

(defmacro gnu:while (exp &body body)
  `(do ()
       ((not ,exp))
     ,@body))

(defun gnu:define-key (&rest ignore))

(deff gnu:make-variable-buffer-local 'ignore)

(defvar gnu:blink-paren-hook nil)


(defun gnu ()
  (when (null *gnu-readtable*)
    (setq *gnu-readtable* (copy-readtable si:common-lisp-readtable)))
  (set-macro-character #\?
                       #'(lambda (stream ignore)
                           (values (char-int (si:xr-xrtyi stream nil t t))))
                       nil
                       *gnu-readtable*)
  (let ((*read-base* 10.)
        (*print-base* 10.)
        (*package* (find-package "GNU"))
        (*readtable* *gnu-readtable*)
        )
    (let (throw-flag)
      (do-forever
        (setq throw-flag t)
        (catch-error-restart ((si:abort dbg:debugger-condition) "Return to top level in ~A."
                              (or (send-if-handles *terminal-io* :name) "current process."))
          (terpri)
          (setq +++ ++ ++ + + -)                ;Save last three input forms
          (setq - (read-for-top-level terminal-io nil
                                      '((:activation char= #\end)
                                        (:prompt "Gnu-> "))))
          (let ((si:lisp-top-level-inside-eval t)
                values)
            (unwind-protect
                (setq values (multiple-value-list (si:eval-abort-trivial-errors -)))
              ;; always push SOMETHING -- NIL if evaluation is aborted.
              (push values *values*))
            (setq /// //
                  // /
                  / values)
            (setq *** **                        ;save first value, propagate old saved values
                  ** *
                  * (car /)))
          (dolist (value /)
            (terpri)
            (funcall (or prin1 #'prin1) value))
          (setq throw-flag nil))
        (when throw-flag
          ;; Inform user of return to top level.
          (format T "~&;Back to top level~@[ in ~A~]."
                  (send-if-handles *terminal-io* :name)))))))
