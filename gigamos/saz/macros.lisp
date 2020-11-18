
;;;save me from having to type "(mapcar #'(lambda (x)" every time
(defmacro LIST-APPLY (fcn-body list)
  `(mapcar (lambda (x) ,(eval fcn-body))
           ,list))
