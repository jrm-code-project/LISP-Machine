(defun compile-my-files (my-files)
  (let ((successfully-compiled-files nil))
    (dolist (f my-files)
      (errset
        (progn (comfile f)
               (push f successfully-compiled-files))))
    (let ((lost-files (set-difference my-files
                                      successfully-compiled-files)))
      (when lost-files
        (with-open-file (loss-file "nil$disk:[nil.steve]didntcomp.fuk" 'out)
          (format loss-file "~&The following files did not compile correctly:")
          (format loss-file "~%~%~s" lost-files))
        (cons 'miscompilations lost-files)))))


(defparameter *my-files* '("nil$disk:[nil.steve]ed"
                            kills
                            edkeys
                            ))
