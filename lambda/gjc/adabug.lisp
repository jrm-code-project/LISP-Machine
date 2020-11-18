;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10 -*-


(defun area-name-1 (x)
  (if (numberp x) (area-name x) x))

(advise si:fasl-op-frame :before check1 nil
  (format t "~%ENTER: ~S" (area-name-1 (AREF FASL-TABLE FASL-LIST-AREA))))

(advise si:fasl-op-frame :after check2 nil
  (format t "~%EXIT: ~S" (area-name-1 (AREF FASL-TABLE FASL-LIST-AREA)) ))


(breakon ' FASL-OP-EVAL)

(breakon ' FASL-OP-EVAL1)

(advise si:fasl-op-frame :after check3 nil
  (when (not (eq 'working-storage-area (area-name-1 (AREF FASL-TABLE FASL-LIST-AREA))))
    (format t "~&FASL-LIST-AREA bad, setting it back to good~%")
    (setf (AREF FASL-TABLE FASL-LIST-AREA) 'working-storage-area)))
