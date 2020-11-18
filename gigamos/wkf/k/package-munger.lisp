;;; -*- Mode:LISP; Package:FASDUMP; Readtable:CL; Base:10 -*-

;;; $$$ The following function is used for understanding the mapping <17-Nov-88 wkf>
(defvar bar)

(defun foo ()
  (let (pr gl li re)
    (do-symbols (sym *prims-package* ())
      (when (eq (symbol-package sym) *prims-package*)
        (let ((ans (get-symbol-package-name sym)))
          (cond ((string-equal ans "PRIMITIVES")     (setq pr (cons sym pr)))
                ((string-equal ans "GLOBAL")         (setq gl (cons sym gl)))
                ((string-equal ans "LISP-INTERNALS") (setq li (cons sym li)))
                (t (setq re (cons (cons sym ans) re)))))))
    (setq bar (list (cons "PRIMITIVES"        pr)
                    (cons "GLOBAL"            gl)
                    (cons "LISP-INTERNALS"    li)
                    (cons "rest (sym . name)" re)))))
