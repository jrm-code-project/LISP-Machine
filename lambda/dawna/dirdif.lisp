;;; -*- Mode:LISP; Package:USER; Base:10 -*-


(defun directory-diff (path other-host &optional recursive)
  (let ((pname-1 (fs:parse-pathname path))
        (pname-2)
        (dir-1)
        (dir-2)
        (subdir-1)
        (subdir-2)
        (temp))
    (dolist (e '(:name :type :version))
      (if (memq (send pname-1 e) '(nil :unspecific))
          (setq pname-1 (send pname-1 :new-pathname e :wild))))
    (setq pname-2 (send pname-1 :new-pathname :host other-host))
    (format t "~&;Directory: ~A " pname-1)
    (multiple-value-setq (dir-1 subdir-1) (get-dirlist pname-1))
    (when (errorp dir-1)
      (return-from directory-diff (send dir-1 :report standard-output)))
    (format t "~D file~p.~%" (length dir-1) (length dir-1))
    (format t "~&;Directory: ~A " pname-2)
    (multiple-value-setq (dir-2 subdir-2) (get-dirlist pname-2))
    (when (errorp dir-2)
      (return-from directory-diff (send dir-2 :report standard-output)))
    (format t "~D file~p.~%" (length dir-2) (length dir-2))
    (multiple-value-bind (extra-1 extra-2 conflict-alist)
        (dirlist-diff dir-1 dir-2)
      (cond ((and (not extra-1) (not extra-2) (not conflict-alist))
             (format t "~&; File Contents identical~%"))
            ('else
             (when extra-1
               (format t "~&; Extra in ~A~%" pname-1)
               (dolist (e extra-1)
                 (cond ((setq temp (superceded-p e dir-2))
                        (format t "~A ; superceded by ~A~%" e temp))
                       ('else
                        (format t "~A~%" e)))))
             (when extra-2
               (format t "~&; Extra in ~A~%" pname-2)
               (dolist (e extra-2)
                 (cond ((setq temp (superceded-p e dir-1))
                        (format t "~A ; superceded by ~A~%" e temp))
                       ('else
                        (format t "~A~%" e)))))
             (when conflict-alist
               (format t "~&; Conflicting files~%")
               (dolist (e conflict-alist)
                 (format t "~A and ~A differ in ~{~A~^,~}~%"
                         (caar e) (cadar e) (mapcar #'car (cdr e))))))))
    (multiple-value-bind (extra-1 extra-2)
        (dirlist-diff (mapcar #'list subdir-1) (mapcar #'list subdir-2))
      (cond ((and (not extra-1) (not extra-2)))
            ('else
             (when extra-1
               (format t "~&; Extra subdirectories in ~A~%" pname-1)
               (dolist (e extra-1)
                 (format t "~A~%" e)))
             (when extra-2
               (format t "~&; Extra subdirectories in ~A~%" pname-2)
               (dolist (e extra-2)
                 (format t "~A~%" e))))))
    (when recursive
      (dolist (d subdir-1)
        (directory-diff d other-host recursive)))))


(defun superceded-p (path dirlist)
  (let ((name (send path :name))
        (type (send path :type))
        (version (send path :version))
        (max))
    (dolist (e dirlist)
      (cond ((not (and (equalp (send (car e) :name) name)
                       (equalp (send (car e) :type) type)
                       (> (send (car e) :version) version))))
            ((not max)
             (setq max (car e)))
            ((> (send (car e) :version) (send max :version))
             (setq max (car e)))))
    max))

(defun get-dirlist (x)
  (condition-case (e)
      (filter-dirlist x (fs:directory-list x))
    (fs:file-not-found e)
    (fs:directory-not-found e)
    (error e)))

(defun filter-dirlist (x l)
  (declare (values (files dirs)))
  (let (files dirs)
    (dolist (d l)
      (cond ((not (car d)))
            ((get d :directory)
             (push (send (send (car d) :pathname-as-directory)
                         :new-pathname
                         :name (send x :name)
                         :type (send x :type)
                         :version (send x :version))
                   dirs))
            ('else
             (push d files))))
    (values (nreverse files) (nreverse dirs))))

(defun pname-equalp (a b)
  (dolist (e '(:directory :name :type :version) t)
    (or (equalp (send a e) (send b e)) (return nil))))

(defun dirlist-diff (d1 d2)
  (declare (values extra-1 extra-2 conflict-alist))
  (let (extra-1 extra-2 conflict-alist)
    (dolist (f1 d1)
      (let ((cell (ass #'pname-equalp (car f1) d2)))
        (cond ((not cell)
               (push (car f1) extra-1))
              ('else
               (let ((conflicts))
                 (dolist (e '(:creation-date :author :length-in-bytes :byte-size :characters))
                   (or (equalp (get f1 e) (get cell e))
                       (push (list e (get f1 e) (get cell e)) conflicts)))
                 (when conflicts
                   (push (cons (list (car f1) (car cell)) conflicts) conflict-alist)))))))
    (dolist (f2 d2)
      (let ((cell (ass #'pname-equalp (car f2) d1)))
        (cond ((not cell)
               (push (car f2) extra-2)))))
    (values extra-1 extra-2 conflict-alist)))


(defun compare-machine-dirs (m1 m2 &optional (version :wild))
  "look at all dirs on m1 and compare with m2"
  (let ((alld (mapcar #'car (fs:all-directories m1))))
    (format t "~&~D directories on ~A" (length alld) m1)
    (dolist (d alld)
      (directory-diff (send d :new-pathname :version version) m2 t))))
