;;; -*- Mode:LISP; Package:(FTES LISP); Base:10; Readtable:CL -*-

;;; test
;;; delete this file

(eval-when (compile load eval)

; 'setf

)


(eval-when (compile load eval)
(shadow '(
          "SETF")
        "FTES")
)

(export '(
          setf))


(defun setf ()
  (print 'yow-setf))
