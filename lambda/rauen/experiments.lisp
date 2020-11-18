
;;; experiments.lisp

(common-lisp t)

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))

(defun reverse-list (l)
  (cond
    ((null l) nil)
    ((atom l) l)
    ((consp l) (append (reverse-list (cdr l)) (list (car l))))
    (t (error "not a list"))))

(defstruct (ship)
  ship-x-position
  ship-y-position
  ship-x-velocity
  ship-y-velocity
  ship-mass)
