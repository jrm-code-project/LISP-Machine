;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-

;;; k2:kbug-print-char k2:kbug-print-return k2:kbug-print-string

(defun kbug-print-raw-fixnum (number radix)
  (prims:dispatch %%data-type number
    ($$dtp-fixnum
      (when (minusp number)
	(k2:kbug-print-char #\-)
	(setq number (- number)))
      (multiple-value-bind (quotient remainder)
	  (truncate number radix)
	(unless (zerop quotient)
	  (print-raw-fixnum quotient radix))
	(k2:kbug-print-char (char "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" remainder))) )
    (t (li:error "~s is not a fixnum" number))))

(defun debug-print-float (float)
  (flet ((print-sign (plusp) (k2:kbug-print-char (if plusp #\+ #\-)))
	 (print-bits (ub-word start end)
	   (do ((position start (1- start)))
	       ((< position end))
	     (k2:kbug-print-char (if (zerop (hw:ldb ub-word (byte 1 position) 0)) #/0 #/1))))
	 )
    (prims:dispatch %%data-type float
      ($$dtp-short-float
	(k2:kbug-print-string "[sf ")
	(print-sign (zerop (hw:ldb float hw:%%short-float-sign 0)))
	(k2:kbug-print-string "1.")
	(print-bits float (1- (byte-size hw:%%short-float-mantissa)) (byte-position hw:%%short-float-mantissa))
	(k2:kbug-print-char #\e)
	(li:print-raw-fixnum (- (hw:ldb float hw:%%short-float-exponent 0) hw:$$short-float-excess))
	(k2:kbug-print-char #\]))
      ($$dtp-single-float
	(let ((float (array:%vm-read32 float 1)))
	  (k2:kbug-print-string "[1f ")
	  (print-sign (zerop (hw:ldb float hw:%%single-float-sign 0)))
	  (k2:kbug-print-string "1.")
	  (print-bits float (1- (byte-size hw:%%single-float-mantissa)) (byte-position hw:%%single-float-mantissa))
	  (k2:kbug-print-char #\e)
	  (li:print-raw-fixnum (- (hw:ldb float hw:%%single-float-exponent 0) hw:$$single-float-excess))
	  (k2:kbug-print-char #\]))) 
      ($$dtp-double-float
	(k2:kbug-print-string "[2f]"))
      (t (li:error "~s is not a float" float)))))

