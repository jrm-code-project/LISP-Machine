;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

;;;; Definition of package hierarchies

(defmacro def-package-hierarchy (name nicknames prefix)
  `(DEF-PACKAGE-HIERARCHY-1 ',name ',nicknames ',prefix))

(defvar *package-hierarchies* ())

;;; Because of the ridiculous way in which hierarchies are
;;; represented, each hierarchy has a package which stands
;;; for the entire hierarchy.
;;; DEF-PACKAGE-HIERARCHY-1 creates this package.
(defun def-package-hierarchy-1 (name nicknames prefix)
  (unless (find-package name)
    (let ((package (make-package name :nicknames nicknames :use 'global)))
      (setf (get package :root) package)
      (setf (get package :prefix) prefix)
      (push package *package-hierarchies*)
      (setf (si:pkg-refname-alist package)
	    (list* 
		   (cons (package-name package) package)
		   (mapcar #'(lambda (nickname)
			       (cons nickname package))
			   (package-nicknames package))))
      name)))

;(defun in-hierarchy (hierarchy &optional package)
;  (let ((current-package *package*))
;    (zl:pkg-goto hierarchy)
;    (print *package*)
;    (zl:pkg-goto (or package
;		     (cdr (assoc (si:package-primary-name current-package)
;				 (si:pkg-refname-alist *package*)))
;		     *package*))))

(defun in-hierarchy (hierarchy)
  (zl:pkg-goto hierarchy))

;;;; Here are defined the package hierarchies used in the K development system     

(def-package-hierarchy compiler-package-hierarchy ("COMP") "")

(def-package-hierarchy simulation-package-hierarchy ("SIM") "K-")
  










