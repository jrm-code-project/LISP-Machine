;;; -*- Mode:LISP; Package:MAIL; Base:10; Readtable:T; Lowercase:T -*-
;;;
;;; Chaosnet Interface to the Mailer
;;;
;;; (c) 1985 Lisp Machine Incorporated
;;;

;;; The Store and Forward server
(defun mail-server-chaos (&aux final-ok qfile)
  (let ((user-id "Mail-Server")
        (*warn-function* 'chaos-mail-warn-function))
    (ignoring-errors
      (as-mail-server (stream (chaos:open-stream nil "MAIL"))
        (send stream :add-as-server "MAIL")
        (ignoring-errors ; catch network lossage
          (with-text-buffer (text stream)
            (let ((recipients (expand-addresses (get-mail-recipients stream))))
              (with-output-to-string (s text)
                (write-line (reception-line stream) s))
              (get-mail-text stream text)
              (finish-output stream)
              (let ((result
                      (write-queue-file (setq qfile (qfile-name))
                                        recipients `(:failures 0
                                                     :source-network-type :chaos
                                                     :source-protocol :chaos-mail
                                                     :from ,(from-uname-for-delivery text)
                                                     :source-host ,(send stream :foreign-host))
                                              TEXT STREAM)))
                (cond ((errorp result)
                       (setq final-ok ())
                       (if (probef qfile) (deletef qfile)))
                      (t
                       (setq final-ok :queued-ok))))))
          (if final-ok (format stream "+Message sent successfully.~%"))
          (force-output stream)))
        (when (and (eq final-ok :queued-ok)
                   (not *delay-delivery*))
          (mail-deliver-qfile qfile)))))

(defun get-mail-recipients (stream)
  (do ((line) (address-error-p) (result)
       (recipients nil))
      (nil)
    (setq line (funcall stream :line-in))
    (and (equal line "") (return (nreverse recipients)))
    (multiple-value (address-error-p result) (mailer-parse-address line))
    (cond (address-error-p
           (format stream "-Bad address [~A]: ~A~%" line result))
          ((and (stringp result)
                (not (address-existent-p result)))
           (FORMAT STREAM "-Unknown user ~A.~%" result))
          (t
           (FORMAT STREAM "+Recipient address ~A ok.~%" result)
           (PUSH result RECIPIENTS)))
    (force-output stream)))

(add-initialization "MAIL"
                    '(process-run-function "MAIL Server" 'mail-server-chaos)
                    nil
                    'chaos:server-alist)

;;; The delivery method

(defun chaos-delivery-single-host (host plist addresses text report-stream &aux s failed)
  (declare (ignore plist))
  (format report-stream "~&  Attempting delivery to ~A (~D addresses):~%"
          host (length addresses))
  (condition-case (error)
      (unwind-protect
          (progn
            (setq s (chaos:open-stream host "MAIL"))
            (dolist (a addresses)
              (write-line (foreign-mailer-address a) s)
              (force-output s)
              (if (char= #\+ (read-char s))
                  (format report-stream "~&OK: ~A~%" (read-line s))
               (format report-stream "~&Failure for ~S: ~A" a (read-line s))
               (push a failed)))
            (terpri s)
            (write-string text s)
            ;; >> No Common Lisp way of doing this.
            (send s :eof)
            (let ((first (read-char s)))
              (if (char= #\+ first)
                  (format report-stream "~&Message queued~%")
               (format report-stream "~&~:[Permanent~;Temporary~] failure: ~A~%"
                       (char= first #\-) (read-line s))
               (setq failed addresses))))
        (when s (lisp:close s :abort t)))
    (error
     (format report-stream "Error while delivering: ~A" error)
     addresses) ; all addresses considered failed
    (:no-error failed)))

;; Copied from LAD: RELEASE-3.NETWORK.MAILER; CHAOS.LISP#8 on 3-Oct-86 17:25:07
(defun chaos-delivery-method (addresses plist text report-stream)
  (deliver-to-sorted-hosts "Chaosnet" #'chaos-delivery-single-host
                           addresses plist text report-stream))

;; Copied from LAD: RELEASE-3.NETWORK.MAILER; CHAOS.LISP#8 on 3-Oct-86 17:25:07
(define-delivery-method chaos-mail (address)
                        (foreign-network-address-p address :chaos)
                        (addresses plist text report-stream)
  (chaos-delivery-method addresses plist text report-stream))


;;; Other non-standard features
;;; Update the mailing list remotely

;; Copied from LAD: RELEASE-3.NETWORK.MAILER; CHAOS.LISP#8 on 3-Oct-86 17:25:07
(add-direct-mail-connected-network :chaos)

(defun update-mailing-list-file (&optional (host (main-lm-mail-server-host)))
  (if (eq host si:local-host) (read-mailing-list-file)
    (with-open-stream (c (chaos:open-stream host "UPDATE-MAILING-LIST" :direction :input))
      (if (char= #\+ (read-char c)) (format t "~&Everything OK.~%")
        (format t "~&Error: ")
        (stream-copy-until-eof c standard-output)))))

(defun mailing-list-update-server-chaos ()
  (catch-error
    (as-mail-server (s (chaos:open-stream () "UPDATE-MAILING-LIST" :direction :output))
      (condition-case (error) (read-mailing-list-file)
        (error
         (write-char #/- s)
         (send error :report s))
        (:no-error (format s "+~A~%" error)))
      (force-output s))
    ()))

(add-initialization "UPDATE-MAILING-LIST"
                    '(process-run-function "Mailing List Updater"
                                           'mailing-list-update-server-chaos)
                    nil
                    'chaos:server-alist)

;;; Standard CHAOSnet protocol EXPAND-MAILING-LIST
(defun expand-mailing-list-server ()
  (catch-error
    (as-mail-server (s (chaos:open-stream () "EXPAND-MAILING-LIST"))
      (loop
        (multiple-value-bind (line eofp) (read-line s)
          (let ((initial (address-existent-p line)))
            (flet ((address-out (user host)
                     (write-string user s)
                     (write-string "@" s)
                     (write-string (or host (send si:local-host :name)) s)))
              (typecase initial
                (null (write-string "-No such address" s))
                (string
                 (write-line "+Local address" s)
                 (address-out initial nil))
                (list
                 (write-line "+Mailing list" s)
                 (dolist (address (expand-addresses initial))
                   (write-line (address-as-string address) s)))
                (error
                 (write-string "-Error in parsing address" s))
                (t
                 (write-string "-Internal error" s))))
            (terpri s)
            (force-output s)
            (if eofp (return nil))))))
    nil))

(add-initialization "EXPAND-MAILING-LIST"
                    '(process-run-function "Expand Mailing List Server"
                                           'expand-mailing-list-server)
                    nil
                    'chaos:server-alist)
;; Copied from LAD: RELEASE-3.NETWORK.MAILER; CHAOS.LISP#8 on 3-Oct-86 17:25:08
(defun chaos-mail-warn-function (type stream &rest format-args)
  (flet ((gwan (string)
           (write-string string stream)
           (apply #'format stream format-args)
           (terpri stream)
           (force-output stream)))
    (case type
      (:disk-full (gwan "%"))
      (:random-error (gwan "-"))
      (:append-error (gwan "-"))
      (otherwise (gwan (format nil "-[~A]" type))))))
