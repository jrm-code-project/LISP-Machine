



(defun bar ()
  (foo #x100 0   #x10)
  (foo #x110 #xF #x10))


(defun foo (a d n)
  (dotimes (i n)
    (hw:write-vma (+ a i))
    (hw:md-start-write d)))
