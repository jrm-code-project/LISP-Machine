(DEFMETHOD (grey-screen :parse-font-descriptor) (fd)
  (get-grey-font (tv:screen-parse-font-descriptor fd 'fonts:grey-font)))


(DEFMETHOD (grey-screen :parse-font-specifier) (fd)
  (get-grey-font (tv:screen-parse-font-descriptor fd 'fonts:grey-font)))


(defun get-grey-font (some-font &aux temp)
  (cond ((eq (get (font-name some-font) 'grey-font) t)
         ;; then this IS a grey font
         some-font)
        ((setq temp (get (font-name some-font) 'fonts:grey-font))
         (symeval temp))
        ('else
         (make-grey-font some-font))))
