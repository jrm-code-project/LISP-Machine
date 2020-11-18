;;; -*- Mode:LISP; Fonts:(CPTFONT); Base:10 -*-


;;; gjc's scheme/hack for doing closures without having a data type of closure.

(defun make-f-closure (environment fef)
  (compile nil `(lambda (&rest l)
                          (apply ',fef ',environment l))))



(defun f (a b)
  (make-f-closure (list a b)
                  #'(lambda (environment)
                      (format t "~&A = ~S, B = ~S~%"
                              (car environment)
                              (cadr environment)))))

