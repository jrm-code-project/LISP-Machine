;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-


(defun purge (directory-pathname &optional (versions-to-keep 1))
  (let ((pathname (fs:parse-pathname directory-pathname nil si:local-host)))
    (when (y-or-n-p
            "~&Ok to delete ~:[all but ~D~;all (i.e. keep ~D)~] version~p of files from ~A: ~A?"
                     (zerop versions-to-keep)
                     versions-to-keep
                     versions-to-keep
                     (send pathname :host)
                     (send pathname :directory))
      (format t "~&OK, doing filesystem purge...~%")
      (filesystem-purge :directory (send pathname :directory)
                        :host (send pathname :host)
                        :versions-to-keep versions-to-keep))))

(defun filesystem-purge (&optional &key (versions-to-keep 1) (directory :root)
                         (host si:local-host)
                         ignore-directories)
  (let ((saved-blocks 0))
    (map-all-files
      #'(lambda (files)
          (do ((extra-files nil)
               (l files (cdr l)))
              ((null l)
               (when extra-files
                 (format t "~D extra files " (length extra-files))
                 (condition-case (x)
                     (send (car extra-files)
                       :delete-multiple-files
                       t extra-files)
                     (fs:dont-delete-flag-set
                      (format t "~&;Losing because ~A~%" (send x :report-string))
                      (dolist (f extra-files)
                        (format t "File-> ~S~%" f)
                        (condition-case (x)
                            (deletef f)
                          ((fs:dont-delete-flag-set fs:open-deleted-file)
                           (format t "~&;Losing because ~A~%" (send x :report-string))))))))
               (if files
                   (let ((saved (fs:expunge-directory (send (car files)
                                                            :new-pathname
                                                            :name :wild
                                                            :type :wild
                                                            :version :wild))))
                     (format t "~D block~p saved~%" saved saved)
                     (incf saved-blocks saved))))
            (cond ((zerop versions-to-keep)
                   (push (car l) extra-files))
                  ('else
                   (do ((proto (car l))
                        (l (cdr l) (cdr l))
                        (found 1))
                       ((null l))
                     (cond ((and (string-equal (send (car l) :name)
                                               (send proto :name))
                                 (string-equal (send (car l) :type)
                                               (send proto :type)))
                            (incf found))
                           ('else
                            (return nil)))
                     (cond ((> found versions-to-keep)
                            (push proto extra-files)
                            (return nil))))))))
      (fs:make-pathname :host host
                        :directory directory)
      :before-f #'(lambda (x)
                    (format t "~&Hacking ~A " x)
                    (when (mem #'equalp (send x :directory) ignore-directories)
                      (format t "ignored~%")
                      :ignore))
      :call-on-list t)
    saved-blocks))

(defun map-all-files (f filespec &optional &key (recursive t)
                      (name :wild)
                      (type :wild)
                      (version :wild)
                      before-f
                      call-on-list
                      call-on-plist)
  (labels ((compose (x y)
                    (cond ((eq x :root) y)
                          ((atom x) (list x y))
                          (t (append x (list y)))))
           (recurse (path)
                    (cond ((and before-f (eq (funcall before-f path) :ignore))
                           nil)
                          ('else
                           (DO ((L (CDR (FS:DIRECTORY-LIST path))
                                   (CDR L))
                                (DIRS-FOUND NIL)
                                (files-found nil))
                               ((NULL L)
                                (if call-on-list
                                    (funcall f (nreverse files-found))
                                  (dolist (elem (nreverse files-found))
                                    (funcall f elem)))
                                (dolist (dir (nreverse dirs-found))
                                  (recurse (FS:MAKE-PATHNAME :HOST (SEND DIR :HOST)
                                                             :DIRECTORY (COMPOSE
                                                                          (SEND DIR :DIRECTORY)
                                                                          (SEND DIR :NAME))
                                                             :NAME name
                                                             :TYPE type
                                                             :VERSION version))))
                             (COND ((GET (CAR L) :DIRECTORY)
                                    (if recursive (PUSH (CAAR L) DIRS-FOUND)))
                                   (call-on-plist
                                    (push (car l) files-found))
                                   ('else
                                    (push (caar l) files-found))))))))
    (recurse (send (fs:parse-pathname filespec)
                   :new-pathname
                   :name name
                   :type type
                   :version version))))


(defun file-sizes (directory &key (version :newest) (recursive t))
  (let ((alist))
    (map-all-files #'(lambda (plist)
                       (incf (cadr (or (ass #'string-equal (send (car plist) :type) alist)
                                       (car (push (list (send (car plist) :type) 0) alist))))
                             (floor (* (get plist :length-in-bytes) (get plist :byte-size))
                                    8)))
                   directory :version version
                   :call-on-plist t
                   :recursive recursive)
    alist))

(defun sum-string-alist (&rest l)
  (let ((new nil))
    (dolist (b l)
      (dolist (e b)
        (let ((cell (ass #'string-equal (car e) new)))
          (cond ((null cell)
                 (push (copy-list e) new))
                ('else
                 (incf (cadr cell) (cadr e)))))))
    new))
