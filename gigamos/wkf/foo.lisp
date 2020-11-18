;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Base:8; Readtable:ZL -*-

(defparameter *warm-symbols* (setq *warm-symbols*
                                   (cons (cons "T"
                                               (cons (cons "PRIMITIVES" 't) nil))
                                         nil)))

(defun make-package-entry (pname package)
  (let ((symbol (make-symbol pname)))
    (setf (symbol-package symbol) (find-package package))
    (cons package symbol)))

(defun warm-find-oblist-entry (pname)
  (do ((rest *warm-symbols* (cdr rest)))
         ((null rest)
          (let ((new (cons pname nil)))
            (push new *warm-symbols*)
            new))
    (when (string-equal pname (caar rest))
      (return (car rest)))))

; symlist is  '(("CAR" . CONS:CAR) ("CDR" . CONS:CDR))
;we add ("GLOBAL" . CONS:CAR) , etc to *warm-symbols*
(defun warm-fix-global-symbols (symlist)
  (dolist (pair symlist)
    (let ((entry (warm-find-oblist-entry (car pair))))
      (if (or (null (cdr entry))
              (cdr (cdr entry)))
          ;;we expect that each symbol has been interned once while fasl built our argument
          ;;therefore, each symbol should have exactly one package
          (error "warm symbols bad" *warm-symbols*))
      (setf (cdr (cdr entry)) (cons (cons "GLOBAL" (cdr pair)) nil)))))

(defun warm-intern (pname package)
  (cond ((eq *warm-symbols* t)
         (intern pname package))
         (t
           ;;*warm-symbols* is of the form '(("CAR" . (("LISP-INTERNALS" . car) ("GLOBAL" . car) ("FOO" . foo:car)))
           ;;                                   ("BAR" . (("GLOBAL" . global:bar))))
           ;; $$$ Rewrote to allow refering to the same symbol via different packages (only by explicit setup) <21-Nov-88 wkf>
           (let ((entry (warm-find-oblist-entry pname)))
             (do ((pkg-sym (cdr entry) (cdr pkg-sym)))
                    ((null pkg-sym)
                     (let ((pkg-entry (make-package-entry pname package)))
                       (setf (li:cdr entry) (li:cons pkg-entry (li:cdr entry)))
                       (cdr pkg-entry)))
               (cond ((string-equal package (car (car pkg-sym)))
                      (return (cdr (car pkg-sym))))))))))
