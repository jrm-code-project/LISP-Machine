;;; -*- Mode:LISP; Package:USER -*-


(defun test ()
  (dolist (flavor-name si:*all-flavor-names*)
    (let ((flavor (get flavor-name 'si:flavor)))
      (when flavor
;       (format t "~&~S:~%" flavor-name)
        (dolist (entry (si::flavor-method-table flavor))
;         (format t " ~S ~D~%" (car entry) (length (cdddr entry)))
          (let ((j 0))
            (dolist (meth (cdddr entry))
              (incf j)
              (let ((meth (fourth entry)))
                (if (getf (si:meth-plist meth) :previous-definition)
                    (format t "   (~D)~D ~S ~A~%" j
                            (length (cdddr entry))
                            (si:meth-function-spec meth)
                            (if (getf (si:meth-plist meth) :previous-definition)
                                "with previous"
                              "")))))))))))
