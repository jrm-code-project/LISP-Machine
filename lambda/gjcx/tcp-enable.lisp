

(defun tcp-license (number)
  "call this function once to enable your tcp-license"
  (flet ((bash-array (array mask)
                     (dotimes (j (length array))
                       (setf (aref array j) (logxor mask (aref array j))))))
    (bash-array (tcp:netload.program-text tcp:8086-netload-data) number)
    (bash-array (tcp:netload.program-data tcp:8086-netload-data) number)))
