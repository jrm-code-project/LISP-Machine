;;; -*- Mode:LISP; Package:KBUG; Readtable:CL; Base:10 -*-



;(defun get-image (&optional (file "jb:k;image"))
;  (with-open-file (stream file :direction :output)
;    (let* ((size-byte-0 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
;	   (size-byte-1 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
;	   (size-byte-2 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
;	   (size-byte-3 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
;	   (size (dpb size-byte-2 (byte 8. 16.)
;		      (dpb size-byte-1 (byte 8. 8.)
;			   (dpb size-byte-0 (byte 8. 0.) 0)))))
;      (format t "~%Getting image of ~d (#x~x) words." size size)
;      (write-byte size-byte-0 stream)
;      (write-byte size-byte-1 stream)
;      (write-byte size-byte-2 stream)
;      (dotimes (i (* 4 size))
;	(write-byte (k2:kbug-stream-read-byte k2::kbug-k-output-stream) stream)))))

(defun read-word ()
  (let* ((byte-0 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
	 (byte-1 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
	 (byte-2 (k2:kbug-stream-read-byte k2::kbug-k-output-stream))
	 (byte-3 (k2:kbug-stream-read-byte k2::kbug-k-output-stream)))
    (dpb byte-3 (byte 8. 24.)
	 (dpb byte-2 (byte 8. 16.)
	      (dpb byte-1 (byte 8. 8.)
		   (dpb byte-0 (byte 8. 0.) 0))))))

;(defun get-load (&optional (file "jb:k;k-load"))
;  (let ((plod-pathname (merge-pathnames file ".plod"))
;	(vlod-pathname (merge-pathnames file ".vlod"))
;	(version (pathname-version file)))
;    (when (or (null version) (eq version :newest))
;      (let ((pprobe (probe-file plod-pathname))
;	    (vprobe (probe-file vlod-pathname)))
;	(setq version
;	      (1+ (max 0
;		       (if pprobe (pathname-version pprobe) 0)
;		       (if vprobe (pathname-version vprobe) 0))))
;	(setq plod-pathname ...)))))

(defun get-load (&optional (file "jb:K;k-load"))
  (get-physical-load file)
  (get-virtual-load file))

(defun get-physical-load (&optional (file "jb:k;k-load.plod"))
  (setq file (merge-pathname file ".plod"))
  (with-open-file (stream file :direction :output)
    (loop
      (when (get-block stream)
	(return)))))

(defun get-virtual-load (&optional (file "jb:K;k-load.vlod"))
  (setq file (merge-pathname file ".vlod"))
  (with-open-file (stream file :direction :output)
    (loop
      (when (get-block stream)
	(return)))))

(defun get-block (stream)
  (let ((address (ldb (byte 23. 0.) (read-word)))
	(size (ldb (byte 23. 0) (read-word))))
    (format t "~%Getting block at ~x, ~x words " address size)
    ;(hex32 (k-mem-read (ash address 2.)))
    (do ((addr (ash address 2.) (+ addr 4))
	 (i size (1- i)))
	((<= i 0))
      (let ((word (k-mem-read addr)))
	(write-byte (ldb (byte 8.  0.) word) stream)
	(write-byte (ldb (byte 8.  8.) word) stream)
	(write-byte (ldb (byte 8. 16.) word) stream)
	(write-byte (ldb (byte 8. 24.) word) stream)))
    (kbug-cmd-raw #xFF)				;ack block
    (and (zerop address) (zerop size))))



;(defun load-image (&optional (file "jb:k;image"))
;  (lam::k-reset)
;  (k-init)
;  (setq *breakpoints-installed* nil)
;  (with-open-file (stream file)
;    (let* ((size-byte-0 (read-byte stream))
;	   (size-byte-1 (read-byte stream))
;	   (size-byte-2 (read-byte stream))
;	   ;(size-byte-3 (read-byte stream))
;	   (size (dpb size-byte-2 (byte 8. 16.)
;		      (dpb size-byte-1 (byte 8. 8.)
;			   (dpb size-byte-0 (byte 8. 0.) 0)))))
;      (format t "~%Loading image of ~d (#x~x) words." size size)
;      (dotimes (i size)
;	(let ((b0 (read-byte stream))
;	      (b1 (read-byte stream))
;	      (b2 (read-byte stream))
;	      (b3 (read-byte stream)))
;						;(k-mem-write i
;	  (format t "~%~x"
;		  (dpb b3 (byte 8. 24.)
;		       (dpb b2 (byte 8. 16.)
;			    (dpb b1 (byte 8. 8.)
;				 b0))))))))

;;    (setq *code-start* instructions-start)
;;    (dolist (f cold::*cold-loaded-functions*)
;;	(setf (nc::ncompiled-function-code f) :in-memory))
;;    (setq *loaded-functions* cold::*cold-loaded-functions*)

;    nil
; )

(defun load-image (&optional (file "jb:k;klod"))
  (lam::k-reset)
  (k-init)
  (setq *breakpoints-installed* nil)
  (with-open-file (stream file)
    (dotimes (i (truncate (file-length stream) 4))
	(let ((b0 (read-byte stream))
	      (b1 (read-byte stream))
	      (b2 (read-byte stream))
	      (b3 (read-byte stream)))
	 (k-mem-write (ash i 2)
		      (dpb b3 (byte 8. 24.)
			   (dpb b2 (byte 8. 16.)
				(dpb b1 (byte 8. 8.)
				     b0))))))
   )
;    (setq *code-start* instructions-start)
;    (dolist (f cold::*cold-loaded-functions*)
;	(setf (nc::ncompiled-function-code f) :in-memory))
;    (setq *loaded-functions* cold::*cold-loaded-functions*)

    nil
 )


(defun check-ipcd ()
  (with-open-file (s "jb:k;klod")
    (dotimes (i (* #x3000 4)) (read-byte s))
    (dotimes (i 3)
      (terpri)
      (dotimes (b 4) (format t "~x " (read-byte s))))))

