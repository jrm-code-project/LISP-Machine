;;; -*- Mode:LISP; Package:TCP; Base:10; Fonts:(CPTFONTB) -*-


(defun read-netload (from-file &optional (size 80000))
  (let ((a (make-array size :type 'art-8b
                       :fill-pointer 0
                       :adjustable t)))
    (with-open-file (s from-file :byte-size 16 :characters nil)
      (do ((c)
           (j 0 (1+ j)))
          ((null (setq c (send s :tyi)))
           a)
        (vector-push-extend (ldb #o1010 c) a)
        (vector-push-extend (ldb #o0010 c) a)))))

(defvar netload-dir "unix-a://u0//EXOS.sample//src//bin//netld86//")

(defun set-netload (var filename)
  (set var (read-netload (string-append netload-dir filename))))


(defun download (netload)
  (INITIALIZATIONS 'TCP-DISABLE-FORMS T)
  (RESET-SW)
  (XDLOPEN t)
  (XDLIOCDLOAD #x10000)                 ; set address on exos board for downloading
  (DOWNLOAD-1 NETLOAD))


(defun download-1 (netload)
  (DO ((N (LENGTH NETLOAD))
       (IDX 0 (+ IDX BSIZE)))
      ((>= IDX N))
    (print idx)
    (XDLWRITE NETLOAD IDX (MIN BSIZE (- N IDX)))))

;
;
;  (XDLIOCSTART #X10000)
;  (XDLCLOSE)
;  (HTONS-ME-HARDER 0)

