;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defun map-over-all-pairs (function top1 top2)
  (labels ((dir-list (x)
                     (fs:directory-list (send (send (fs:parse-pathname x) :pathname-as-directory)
                                              :new-pathname
                                              :name :wild
                                              :type :wild
                                              :version :newest))))
    (cond ((null top1)
           (cond ((null top2)
                  nil)
                 (t
                  (let ((top2-list (dir-list top2)))
                    (dolist (top2-desc (cdr top2-list))
                      (cond ((getf (cdr top2-desc) :directory)
                             (map-over-all-pairs function nil (car top2-desc)))
                            (t
                             (funcall function nil (car top2-desc)))))))))
          (t
           (cond ((null top2)
                  (let ((top1-list (dir-list top1)))
                    (dolist (top1-desc (cdr top1-list))
                      (cond ((getf (cdr top1-desc) :directory)
                             (map-over-all-pairs function (car top1-desc) nil))
                            (t
                             (funcall function (car top1-desc) nil))))))
                 (t
                  (let ((top1-list (dir-list top1))
                        (top2-list (dir-list top2)))
                    (dolist (top1-desc (cdr top1-list))
                      (let ((top2-desc
                              (member
                                top1-desc
                                (cdr top2-list)
                                :test #'(lambda (x y)
                                          (cond ((getf (cdr x) :directory)
                                                 (cond ((getf (cdr y) :directory)
                                                        (string-equal (send (send (car x) :pathname-as-directory) :name)
                                                                      (send (send (car y) :pathname-as-directory) :name)))
                                                       (t
                                                        nil)))
                                                ((getf (cdr y) :directory)
                                                 nil)
                                                ((or (eq (send (car x) :type) :unspecific)
                                                     (eq (send (car x) :type) nil))
                                                 (cond ((or (eq (send (car y) :type) :unspecific)
                                                            (eq (send (car y) :type) nil))
                                                        t)
                                                       (t
                                                        nil)))
                                                (t
                                                 (and (string-equal (send (car x) :name)
                                                                    (send (car y) :name))
                                                      (string-equal (send (car x) :type)
                                                                    (send (car y) :type)))))))))
                        (setq top2-desc (car top2-desc))
                        (cond ((getf (cdr top1-desc) :directory)
                               (cond ((null top2-desc)
                                      (map-over-all-pairs function (car top1-desc) nil))
                                     ((getf (cdr top2-desc) :directory)
                                      (map-over-all-pairs function (car top1-desc) (car top2-desc))
                                      (setq top2-list (delq top2-desc top2-list)))
                                     (t
                                      (ferror nil "~s is a directory but ~s is not"
                                              (car top1-desc) (car top2-desc)))))
                              (t
                               (cond ((null top2-desc)
                                      (funcall function (car top1-desc) nil))
                                     ((getf (cdr top2-desc) :directory)
                                      (ferror nil "~s is a file but ~s is a directory"
                                              (car top1-desc) (car top2-desc)))
                                     (t
                                      (funcall function (car top1-desc) (car top2-desc))
                                      (setq top2-list (delq top2-desc top2-list))))))))
                    (dolist (top2-desc (cdr top2-list))
                      (cond ((getf (cdr top2-desc) :directory)
                             (map-over-all-pairs function nil (car top2-desc)))
                            (t
                             (funcall function nil (car top2-desc))))))))))))

(defun compare ()
  (map-over-all-pairs
    #'(lambda (x y)
        (format t "~&~a ~a" x y))
    "angel:/lmi3/pace/lt"
    "lam3:~;orbit"))

(defun mass-compare ()
  (map-over-all-pairs
    #'(lambda (x y)
        (cond ((null x)
               (when y
                 (format t "~&**** Unpaired file ~a" y)))
              ((null y)
               (format t "~&**** Unpaired file ~a" x))
              (t
               (let ((file1 (srccom:create-file x))
                     (file2 (srccom:create-file y)))
                 (srccom:describe-srccom-sources file1 file2 *standard-output*)
                 (srccom:source-compare-files file1 file2)))))
    "angel:/lmi3/pace/lt"
    "lam3:~;orbit"))
