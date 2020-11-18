;;; -*- Mode:LISP; Package:USER; Base:10 -*-


;; what we want is some construct that
;; makes code be reentrent without going to a lot
;; of hair. e.g. INTERN and buddies.
;; Other functions like MAP-ATOMS could also do it.


(defvar *pkg* nil)

(defvar *k* 0)

(defvar done-1 nil)
(defvar done-2 nil)

(defun map-atoms-test (n q)
  (setq *pkg* (make-package (string (gensym))
                            :use nil
                            :size 10))
  (setq *k* 0)
  (setq done-1 nil)
  (setq done-2 nil)
  (let ((kr (read-meter 'si:%tv-clock-rate)))
    (unwind-protect
        (progn (write-meter 'si:%tv-clock-rate q)
               (process-run-function `(:name "test a"
                                             :quantum ,q)
                                     #'(lambda (n)
                                         (dotimes (j n)
                                           (intern (format nil "Z-~D" *k*) *pkg*))
                                         (setq done-1 t))
                                     n)
               (process-run-function `(:name "test b"
                                             :quantum ,q)
                                     #'(lambda (stream)
                                         (mapatoms #'(lambda (sym)
                                                       (print sym stream))
                                                   *pkg*)
                                         (setq done-2 t))
                                     terminal-io)
               (process-wait "hack" #'(lambda () (and done-1 done-2))))
      (write-meter 'si:%tv-clock-rate kr))))
