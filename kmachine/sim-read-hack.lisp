;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:CL; Base:10 -*-

(defconstant kcl-readtable (copy-readtable))

(eval-when (eval load)
  (setf (rdtbl-names kcl-readtable) (list "KCL"))
  (setf (rdtbl-read-function-property kcl-readtable)
        (cons 'kcl-read-function (list (rdtbl-read-function-property kcl-readtable)))))

(defprop fixnum xr-read-fixnum-crock kcl-read-function)
(defun xr-read-fixnum-crock (stream string)
  (declare (ignore stream))
  (let ((b (current-read-base))
        (len (length string)))
    (multiple-value-bind (num i)
        (xr-read-fixnum-internal string 0 len b)
      (values
        `(QUOTE
          ,(sim::make-datum
           sim::$$boxed
           (sim::32-dpb
             sim::$$dtp-fixnum
             (byte 6. 26.)
             (if (= i len)
                  num
                  (let ((num2 (xr-read-fixnum-internal string (1+ i) len b)))
                    (if (= (char string i) #\_)
                        (ash num num2)
                        (* num (^ b num2))))))))
           'fixnum))))
