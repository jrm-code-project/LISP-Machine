;;; -*- Mode:LISP; Package:MAIL; Base:10; Readtable:T; Lowercase:T -*-
;;; (c) 1986 Lisp Machine Incorporated
;;; Internet/SMTP interface to the Mailer

(si:assure-system-patch-loaded "System Revision Level" 3 170)
(si:assure-system-patch-loaded "Mailer" 2 2)

(eval-when (compile load eval)
;;; I wouldn't do this if there were shorter nicknames...
;;; Have to do this because of basic problems in the QFASL/symbol package interactions
(import (mapcar #'(lambda (name) (find-symbol name "TCP-APPLICATION"))
                '("TO" "FROM" "SMTP-REPLY" "SMTP-MAILER" "*SMTP-MAILER*" "SMTP-RCPT-VERIFIER"
                              "SMTP-REPLY-OK" "SMTP-SERVER-STATE")))
)

;;; Delivery
(defun smtp-delivery-method (addresses plist text report-stream)
  (deliver-to-sorted-hosts "Internet SMTP" #'smtp-deliver-single-host
                           addresses plist text report-stream))

(define-delivery-method smtp-mail (address)
                        (foreign-network-address-p address :internet)
                        (addresses plist text report-stream)
  (smtp-delivery-method addresses plist text report-stream))

(add-direct-mail-connected-network :internet)

(defvar *smtp-deliver-debug-p* nil)

(defun smtp-deliver-single-host (host recipients plist text report-stream &aux failed)
  (with-open-stream (u (ftp::make-smtp-user *SMTP-deliver-debug-p*))
    (send u :connect (send host :name))
    (unless (= 250 (nth-value 1
                     (send u :command
                           "MAIL FROM:<%s>" (getf plist :from))))
      (ferror "Cant accept mail from: ~S because ~A"
              (car (getf plist :from))
              (send u :last-reply)))
    (dolist (r recipients)
      (unless (= 250 (nth-value 1
                       (send u :command
                             "RCPT TO:%s" (foreign-mailer-address r))))
        (push r failed)
        (format report-stream "Can't send to ~A: ~A"
                (foreign-original-address r) (send u :last-reply))))
    (unless (= 354 (nth-value 1 (send u :command "DATA")))
      (format report-stream "Can't send the mail text data because ~A"
              (send u :last-reply))
      (return-from smtp-deliver-single-host recipients))
    (send u :funcall-on-data-stream
          #'(lambda (stream) (write-string text stream)))
    (if (= 250 (nth-value 1 (send u :end-message)))
        failed
      (format report-stream "Some problem in ending message data: ~A"
              (send u :last-reply))
      recipients)))

;;; SMTP server interface.

(defun (:property :mailer smtp-rcpt-verifier) (original-string address-string stream)
  (declare (ignore original-string))
  (multiple-value-bind (errorp result) (mailer-parse-address address-string)
      (if errorp
          (smtp-reply stream 501 "Bad address: ~A" result)
        (if (and (stringp result)
                 (not (address-existent-p result)))
            (smtp-reply stream 550 "Unknown local address")
          (push result (smtp-server-state 'to))
          (smtp-reply-ok stream)))))

(defun smtp-warn-function (type stream &rest format-args)
  (apply #'smtp-reply stream
         (case type
           (:disk-full 452)
           (:random-error 451)
           (:append-error 550)
           (otherwise 451))
         format-args))

(defun (:property :mailer smtp-mailer) (from-host data stream &aux qfile final-ok)
  (let ((*warn-function* 'smtp-warn-function))
    (ignoring-errors
      (using-resource (text text-buffer (+ 100 (* (length data) 50.)))
        (macrolet ((line-out (line)
                             `(string-nconc text ,line #\Newline)))
          (line-out (explicit-reception-line from-host "SMTP"))
          (dolist (l data) (line-out l)) ; ah, the luxuries of a large address space...
          (let ((result
                  (write-queue-file (setq qfile (qfile-name))
                                    (expand-addresses (smtp-server-state 'to))
                                    `(:failures 0
                                      :source-network-type :internet
                                      :source-protocol :smtp
                                      :from ,(second (smtp-server-state 'from))
                                      :source-host ,from-host)
                                    text stream)))
            (cond ((errorp result)
                   (setq final-ok ())
                   (if (probef qfile) (deletef qfile)))
                  (t
                   (setq final-ok :queued-ok)
                   ;; ``Error'' replies are handled by WRITE-QUEUE-FILE
                   (smtp-reply-ok stream))))))))
  (when (and (eq final-ok :queued-ok)
             (not *delay-delivery*))
    (mail-deliver-qfile qfile)))

(defun smtp-use-mailer ()
  (setq *smtp-mailer* :mailer))

(defun smtp-use-default ()
  (setq *smtp-mailer* :simple))
