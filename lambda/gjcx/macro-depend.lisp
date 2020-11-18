;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-


(defun files-using-macros (in-system macros)
  (let ((result)
        (sources (si:system-source-files in-system)))
    (format t "~&Looking at ~D generic pathnames" (length sources))
    (dolist (x sources)
      (princ ".")
      (do ((l (get (send x :generic-pathname) :macros-expanded) (cdr l)))
          ((null l))
        (when (memq (if (atom (car l)) (car l) (caar l))
                    macros)
          (return (pushnew x result)))))
    result))
