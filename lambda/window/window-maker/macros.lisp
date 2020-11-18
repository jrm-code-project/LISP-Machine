;;;-*- Mode:LISP; Package:window-maker; Base:8; Fonts:(CPTFONT) -*-
;;; Copyright C LISP MACHINE INC., 1985.
;;

(defmacro delete-element-from-list (list object)
  `(if (member ,object ,list)
       (if (zerop (find-position-in-list ,object ,list))
           (setq ,list (delq ,object ,list))
         (delq ,object ,list))))

(defmacro update-list (list object)
  `(and ,object (if ,list (nconc ,list (ncons ,object))
                  (setq ,list (ncons ,object)))))
