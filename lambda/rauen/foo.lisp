(defun foo (x)
  (if (< x 2)
      1
      (+ (foo (- x 1)) (foo (- x 2)))))
