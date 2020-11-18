;;; -*- Mode:LISP; Package:USER; Base:10 -*-



(defun extract-clx-only ()
  (tar :verbose t
       :extract #'(lambda (p f)
                    (let ((d (send p :directory))
                          (tp (send p :type)))
                      (if (atom d) (setq d (list d)))
                      (cond ((mem #'string-equal "CLX" d)
                             (send p :new-pathname
                                   :host si:local-host
                                   :directory (cons "X11" (cdr d))
                                   :type (if (string-equal "l" tp)
                                             "LISP"
                                           tp)))
                            ('else
                             (format t "~&Skipping ~A~%" f)))))))

