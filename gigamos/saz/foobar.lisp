(defun MASS-MILLIONS ()
  (let ((six-numbers
          (loop for x = (1+ (random 45))
                with y initially nil do
                (if (= (length y) 6)
                    (return y)
                  (pushnew x y)))))
    (multiple-value-bind (a b c d e f)
        (apply
          'values
          (sort six-numbers '<))
      (format t "~%Your numbers are:~%~A ~A ~A ~A ~A ~A"
              a b c d e f))))

