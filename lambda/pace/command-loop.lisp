;;; -*- Mode:LISP; Package:(SUBDEBUG USE GLOBAL); Readtable:CL; Base:10 -*-

(defvar *command-prefix*)
(defvar *get-next-command-unget* nil)

(defun unget-command (blip)
  (setq *get-next-command-unget* (append *get-next-command-unget* (list blip))))

(defconst *activation-list* '(#\/ #\tab #\space #\: #\altmode
                              #\@ #\linefeed #\^ #\= #\page #\:
                              #\. #\_ #\(
                              ))
(defun get-next-command ()
  (cond ((null *get-next-command-unget*)
         (labels ((act-fun (char)
                           (or (ldb-test %%kbd-super char)
                               (member char *activation-list*
                                       :test 'char-equal))))
           (do (command blip error-p)
               (())
             (multiple-value (command blip)
               (with-input-editing (terminal-io
                                     `((:activation ,#'act-fun)
                                       (:preemptable)
                                       (:full-rubout :full-rubout)
                                       )
                                     )
                 (do ((out-string (make-string 10. :fill-pointer 0))
                      (char (send terminal-io :any-tyi)
                            (send terminal-io :any-tyi)))
                     ((or (not (integerp char)
                               (characterp char)))
                      (values out-string char))
                   (array-push-extend out-string char))))
             (cond ((eq blip :full-rubout)
                    (return :full-rubout))
                   ((consp command)
                    (case (car command)
                      (:typeout-execute
                       (case (cadr command)
                         (lam:force-object-kbd-input
                          (funcall (cadr command) (caddr command) terminal-io))))))
                   (t
                    (multiple-value (*command-prefix* error-p)
                      (ignore-errors (read-from-string command nil)))
                    (cond (error-p
                           (format t "?? error reading "))
                          (t
                           (when (not (member (cadr blip) '(#\linefeed #\( ) :test 'char-equal))
                             (format t "~c" (cadr blip)))
                           (return blip))))))))
        (t
         (pop *get-next-command-unget*))))


(defvar *accumulator*)
(defvar *last-command*)
(defvar *update-display*)
(defvar *current-address*)

(defun sd ()
  (let ((*accumulator* nil)
        (*command-prefix* nil)
        (*update-display* t)
        (*read-base* 8)
        (*print-base* 8)
        (*last-command* nil)
        (*current-address* 0)
        )
    (command-loop)
    ))


(defsignal bad-reg-adr error ())
(defsignal sd error ())

(defun command-loop ()
  (format t "~&")
  (catch 'exit
    (do-forever
      (catch-error-restart ((sys:abort error) "Return to top level in SD")
        (condition-case (error)
            (do-forever
             (when *update-display*
               ;(update-display)
               (setq *update-display* nil))
             (let ((blip (get-next-command)))
               (cond ((eq blip :full-rubout)
                      (format t " ?? "))
                     (t
                      (let ((val (sd-eval *command-prefix*)))
                        (when (numberp val)
                          (if (null *accumulator*) (setq *accumulator* 0))
                          (incf *accumulator* val)))
                      (when (consp blip)
                        (case (car blip)
                          (:activation
                           (let* ((fun-name (format nil "SD-~@:(~:C~)" (cadr blip)))
                                  (fun (intern-soft fun-name 'subdebug)))
                             (cond ((fboundp fun)
                                    (funcall fun)
                                    (setq *last-command* fun)
                                    (when (not (memq fun '(sd-@ sd-space sd-.)))
                                      (setq *accumulator* nil)))
                                   (t
                                    (format t "?? ~s " fun-name)))))))))))
          ((bad-reg-adr sd)
           (format t "~&Error: ")
           (send error :report terminal-io)
           (format t "~&")
           (setq *accumulator* nil))))
      (format t "~&Back to SD top level.~&"))))

(defun sd-eval (exp)
  (cond ((numberp exp) exp)
        (t nil)))

(defun sd-space ()
  )
