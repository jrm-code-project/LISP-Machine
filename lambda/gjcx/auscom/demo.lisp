;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;;  DRIVER FOR THE AUSCOM MODEL 8600
;;;  MULTIBUS CHANNEL INTERFACE

;;;  6/18/86 16:32:31 - DEMO:
;;; - GEORGE CARRETTE

;;; the system 38 side of this demo:
;;;   A line is read from the terminal, then written to the channel.
;;;   then he does a READ and displays that to the terminal.


;;; My side.

(defvar *result-string* "nothing here yet.....")

(defun demo-write-data-function (string length)
  (string-translate-in-place *ebcdic->lispm* string 0 length)
  (format terminal-io "~&FROM SYSTEM-38: ~S~%" (substring string 0 length))
  (setq *result-string* (demo-evaluate string length))
  (format terminal-io "~&TO SYSTEM-38: ~S~%" *result-string*)
  (string-translate-in-place *lispm->ebcdic* *result-string*))

(defun demo-read-data-function ()
  (values *result-string* (length *result-string*)))


;;; idea, if read is chained to write, then hold off on device-end (after the write)
;;; until the read buffer is ready.


(defun demo-on ()
  (setq *chwrite-data-function* 'demo-write-data-function)
  (setq *chread-data-function* 'demo-read-data-function))


(defun demo-off ()
  (setq *chwrite-data-function* nil)
  (setq *chread-data-function* nil))



(defun read-expression ()
  (global:read))

(defun print-expression (exp)
  (prin1 exp))


(defun demo-evaluate (string &optional (length (length string)))
  (let ((*package* (find-package "USER"))
        (base 10)
        (ibase 10)
        (*readtable* (si:find-readtable-named "CL")))
    (with-output-to-string (out)
      (with-input-from-string (in string :end length)
        (let ((standard-output out)
              (error-output out)
              (standard-input in))
          (catch-error (print-expression (si:eval-special-ok (read-expression)))))))))
