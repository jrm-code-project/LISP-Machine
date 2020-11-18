;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(defun foo ()
  (let ((s (make-string 10.))
        (v (make-array 10. :element-type 'string-char))
        (w (make-array 10.))
        (x (make-array 10. :type :art-q))
        (y (make-array '(3 3) :type 'mypackage:foo))
        (z (make-array 5. :type 'art-string)))
    (dotimes (i 10.) (aset 'hi x i))
    (dotimes (i 3)
      (dotimes (j 3)
        (aset 'ok y i j)))
    (dotimes (i 5) (aset #/Q z i))
    (list s v w x y z)))


(defun zool ()
  (si:simple-make-array 3 :type #+(target lambda) art-q #+(target falcon) k-array:art-q))

(defun bar (x)
  (make-array 3 :type 'art-q))

