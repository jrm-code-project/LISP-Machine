;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:10 -*-


(defun lines-in-system (x)
  (let ((l (mapcar #'lines-in-file (si:system-source-files (si:find-system-named x)))))
    (format t "~&~%Total for system ~A,~%~D code lines, ~D comment lines ~D blank lines~%"
            x
            (apply '+ (mapcar #'car l))
            (apply '+ (mapcar #'cadr l))
            (apply '+ (mapcar #'caddr l)))))


(defun lines-in-file (x)
  (format t "~&File ~A has " x)
  (let ((code 0)
        (comment 0)
        (blank 0))
    (with-open-file (s x)
      (do ((line))
          ((null (setq line (readline s nil))))
        (cond ((zerop (length line))
               (incf blank))
              ((char= #\; (aref line 0))
               (incf comment))
              ('else
               (incf code)))))
    (format t "~D code lines, ~D comment lines, ~D blank lines~%"
            code comment blank)
    (list code comment blank)))
