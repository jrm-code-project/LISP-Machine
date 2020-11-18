(defun select-window (window)
  (send window :select))

(defun CYCLE-WINDOWS (cycles &rest window-list)
  (dotimes (i cycles)
    (dolist (each (car window-list))
      (send each :expose))))
