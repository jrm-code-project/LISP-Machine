;;; -*- Mode:LISP; Package:TCP; Readtable:CL; Base:10 -*-


(defun crypt-download-data (password)
  (let ((time (time)))
    (crypt-string (make-array (length (netload.program-data 8086-netload-data))
                              :type 'art-string
                              :displaced-to (netload.program-data 8086-netload-data))
                  (coerce password 'string))
    (crypt-string (make-array (length (netload.program-text 8086-netload-data))
                              :type 'art-string
                              :displaced-to (netload.program-text 8086-netload-data))
                  (coerce password 'string))
    (quotient (time-difference (time) time) 60.0)))

(defun crypt-string (string password)
  (check-type string string)
  (check-type password string)
  (let ((sn (length string))
        (pn (length password)))
    (dotimes (j sn)
      (setf (aref string j) (mod (+ (aref string j) j (aref password (mod (- pn j) pn))) 256)))
    (do ((j 0 (+ j pn)))
        ((not (< j sn)))
      (dotimes (k pn)
        (let ((i (+ k j)))
          (when (< i sn)
            (setf (aref string i) (logxor (aref string i) (aref password k)))))))
    (dotimes (j sn)
      (setf (aref string j) (mod (- (aref string j) j (aref password (mod (- pn j) pn))) 256))))
  (values string password))
