;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10 -*-



(Defun disk-16b (unit PART b)
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "reading ~A partition" PART))
    (MULTIPLE-VALUE-BIND (PART-BASE PART-SIZE NIL NIL)
        (FIND-DISK-PARTITION-FOR-READ PART NIL UNIT)
      (CHECK-ARG B (AND (NOT (< B 0)) (< B PART-SIZE)) "inside the partition")
      (WITH-DISK-RQB (RQB 1)
        (DISK-READ RQB UNIT (+ B PART-BASE))
        (LET* ((BUF (RQB-BUFFER RQB))
               (A (MAKE-ARRAY (LENGTH BUF) :TYPE 'ART-16b)))
          (fillarray a buf)
          a)))))


(defun describe-syscom (unit part)
  (WITH-DECODED-DISK-UNIT (UNIT UNIT (FORMAT NIL "reading ~A partition" PART))
    (format t "~&System communications area of: ~A~%" (partition-comment part unit))
    (do ((l SYSTEM-COMMUNICATION-AREA-QS (cdr l))
         (a (disk-16b unit part 1))
         (m (apply 'max (mapcar'flatsize SYSTEM-COMMUNICATION-AREA-QS))))
        ((null l))
      (let ((value (cond ((memq (car l) '(%SYS-COM-VALID-SIZE %SYS-COM-HIGHEST-VIRTUAL-ADDRESS))
                          (sys-com-page-number a (symeval (car l))))
                         ('else
                          (aref a (* 2 (symeval (car l))))))))
        (format t "~VS = ~11O ~10D.~%" m (car l) value value)))))
