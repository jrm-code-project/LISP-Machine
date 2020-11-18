;;; -*- Mode:LISP; Package:user; Readtable:ZL; Base:8; Patch-File:T -*-



(defun foo nil
  (let ((wins 0) (losses 0)
        (a 2577772500) (b 41414040))
    (do-forever
      (cond ((not (zerop (\ a b)))
             (incf losses))
            (t (incf wins)))
      (if (si:kbd-tyi-no-hang) (return (list wins losses))))))
