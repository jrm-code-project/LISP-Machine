;;; -*- Mode:LISP; Package:AUSCOM; Base:10 -*-


(defun read-dl ()
  (read-io-16 #x16))

(defun read-io-16 (n)
  ;; ron said read-16 would read-16 here, via special
  ;; kludge in late-rev SDU's. not working now,
  ;; in this rev N. maybe the new sdu they are going to
  ;; send me?
  (si:%multibus-read-16 (+ #x00100000 (* N 4))))


(defun dl-t ()
  (do-forever
    (format t "~&#x~4,'0X" (read-dl))))


(defun ustep-enable ()
  (modify mc dwnld 1))


(defun ustep ()
  (do-forever
    (modify mc step 1)
    (format t "~&~4,'0X" (dpb (read download-register-msb) (byte 8 8) (read download-register-lsb)))
    (send terminal-io :tyi)))
