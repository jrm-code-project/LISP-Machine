;;; -*- Mode:LISP; Package:USER; Readtable:CL; Lowercase:T; Base:10 -*-

(defun my-time ()
  (multiple-value-bind (s m h)
      (time:get-time)
    (format nil "~s:~s:~s" h m s)))

(defun kom-to-buf ()
  (setq nc:*debug-stream* (open "ed-buffer:compiler-output" :direction :output))
  (nc:debug-on :post))

(defun kom-to-buf-off ()
  (nc:debug-off :post)
  (close nc:*debug-stream*)
  (setq nc:*debug-stream* t))
