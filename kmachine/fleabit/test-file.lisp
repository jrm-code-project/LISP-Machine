
(defun tst (x)
  (if x
      (bar x)
    (baz x)))


(defun bar (y)
  (baz y 3))

(defun baz (z &optional (y 0))
  (+ z y))

