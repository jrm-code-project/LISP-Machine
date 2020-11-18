;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:CL -*-


(cerror "Load it anyway" "The contents of this file have been moved to KBUG2")

;;; This file is sort of like the lambda's mini-server.
;;; It reads a KFASL file from a file server and shoves
;;; it into the K-INPUT-FASL-STREAM

(defun warm-download-byte (byte)
  (kbug-streams::kbug-stream-write-byte kbug2-common::kbug-k-input-fasl-stream byte))

(defun warm-download-string (string)
  (kbug-streams::kbug-stream-string-out kbug2-common::kbug-k-input-fasl-stream string))



(defun warm-download (file)
  (with-open-file (stream file :direction :input)
    (let ((eof-marker (cons 'EOF-MARKER nil)))
      (loop
	(let ((input (read-byte stream nil eof-marker)))
	  (if (eq input eof-marker)
	      (return-from warm-download t)
	      (warm-download-byte input)))))))



(defun k-fasl-stream (op &rest args)
  (ecase op
    (:string-out (warm-download-string (car args)))
    (:tyo        (warm-download-byte (car args)))))

(defun warm-download-cold-info (&optional (stream #'k-fasl-stream))
  (cold:fasd-cold-function-info stream))

