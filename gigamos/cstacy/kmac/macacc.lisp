;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-


;;;

;; Binding this variable to non-NIL causes auto creation of directories.
;; This is used internally to create directories, but may be bound by things
;; such as magtape restoration programs.
(defvar mac-automatically-create-directories nil)

(defvar mac-file-streams-list nil "List of all file-stream opened at some point")


;;;
;;; Global variables
;;;


(declare (special mac-file-streams-list  *current-operation* *current-operation-pathname*))


;
; Useful macros
;

(defmacro open-lookup-file ((file pathname) &body body)
  `(let ((,file nil)error)
     (unwind-protect
         (progn
           (multiple-value (error ,file)
             (mac-open-the-file (fs:pathname-raw-directory ,pathname)
                                (fs:pathname-raw-name      ,pathname)
                                (fs:pathname-raw-type      ,pathname)
                                (fs:pathname-raw-version   ,pathname)
                                :lookup nil :error nil nil nil t))
           ,@body)
       (when ,file
         (mac-close-file ,file)))))


;;;
;;; This file is almost identical to fsacc.lisp
;;;


(defflavor mac-file-access ()
           (fs:directory-list-mixin fs:basic-access))


(defmethod (mac-file-access :reset) ()
  (send self :close-all-files))


(defmethod (mac-file-access :open-streams) ()
  mac-file-streams-list)


(defmethod (mac-file-access :access-description) ()
  "Direct")


;(defmethod (mac-file-access :assure-access) ()
;  (and (not (boundp mac-disk-configuration))
;       (boot-file-system)))


(defmethod (mac-file-access :close-all-files) (&optional (mode :abort))
  (dolist (s mac-file-streams-list)
    (close s mode)))


(defmethod (mac-file-access :homedir) (&optional (user user-id))
  user
  (make-instance 'mac-pathname :host mac-host :directory user))


(defmethod (mac-file-access :change-properties) (pathname error-p &rest plist)
  (fs:identify-file-operation :change-properties
    (fs:handling-errors error-p
      (open-lookup-file (file pathname)
        (mac-change-file-properties file plist)))))


(defmethod (mac-file-access :properties) (pathname &optional error-p)
  (fs:identify-file-operation :properties
    (fs:handling-errors error-p
      (open-lookup-file (file pathname)
        (values (cons (mac-file-truename file)
                      (read-info-blk (file-info-blk file)))
                mac-unsettable-properties)))))


(defmethod (mac-file-access :COMPLETE-STRING) (pathname string options)
  (multiple-value-bind (nil dir nam typ ver)
      (send pathname :parse-namestring fs:host string)
    (multiple-value-bind (new-directory new-name new-type completion)
        (mac-complete-path (or dir (fs:pathname-raw-directory pathname) "")
                           (or nam "")
                           (or typ "")
                           (fs:pathname-raw-name pathname)
                           (fs:pathname-raw-type pathname)
                           options)
      (values (format nil "~a:~a~:[~*~;~a~]~:[~*~;.~a~]~:[~*~;#~a~]"
                      (send fs:host :name-as-file-computer)
                      (mac-print-directory new-directory)
                      new-name (mac-print-component new-name)
                      new-type (mac-print-component new-type)
                      ver (mac-print-component ver))
              completion))))


(defmethod (mac-file-access :create-directory) (pathname &optional (error t))
  (fs:identify-file-operation :create-directory
    (fs:handling-errors error
      (mac-create-directory (fs:pathname-raw-directory pathname))
      t)))


(defmethod (mac-file-access :open) (ignore pathname &rest options)
  (let ((stream (apply 'mac-open-file pathname (fs:pathname-raw-directory pathname)
                                               (fs:pathname-raw-name      pathname)
                                               (fs:pathname-raw-type      pathname)
                                               (fs:pathname-raw-version   pathname)
                                      options)))
    stream))


(defmethod (mac-file-access :directory-list) (pathname options)
  (mac-directory-list pathname fs:host
                      (fs:pathname-raw-directory pathname)
                      (fs:pathname-raw-name      pathname)
                      (fs:pathname-raw-type      pathname)
                      (fs:pathname-raw-version   pathname)
                      options))


(defmethod (mac-file-access :rename) (pathname new-name &optional (error-p t))
  (fs:identify-file-operation :rename
    (fs:handling-errors error-p
      (open-lookup-file (file pathname)
        (let ((*current-operation-pathname* new-name))
          (mac-rename-file file
                           (pathname-directory new-name)
                           (or (pathname-name new-name) "FOO")
                           (or (pathname-type new-name) :unspecific)
                           (pathname-version new-name)))))))


(defmethod (mac-file-access :delete) (pathname &optional (error-p t))
  (let ((*current-operation-pathname* pathname))
    (fs:identify-file-operation :delete
      (fs:handling-errors error-p
        (open-lookup-file (file pathname)
          (mac-delete-file file))))))


(defmethod (mac-file-access :expunge) (pathname &optional (error t))
  (fs:identify-file-operation :expunge
    (fs:handling-errors error
      (mac-expunge-directory
        (fs:pathname-raw-directory pathname)
        (fs:pathname-raw-name      pathname)
        (fs:pathname-raw-type      pathname)
        (fs:pathname-raw-version   pathname)))))


(defmethod (mac-file-access :delete-multiple-files) (error-p pathnames)
  (fs:identify-file-operation :delete
    (fs:handling-errors error-p
       (loop for pathname in pathnames
             do (open-lookup-file (file pathname)
                  (mac-delete-file file nil))))))


(defmethod (mac-file-access :all-directories) (options)
  (let ((error-p (not (memq :noerror options))))
    (fs:identify-file-operation ':all-directories
      (fs:handling-errors error-p
        (mapcar #'(lambda (dirspec)
                    (list (car dirspec)))
                (cdr (fs:directory-list "kmac:~:")))))))



;(defmethod (mac-file-access :multiple-file-plists) (pathnames options) ;a ecrire
;  "This is a hack to speed up DIRED.
;There are no currently meaningful options." options
;  (loop for pathname in pathnames
;       collect (let ((tpathname (send pathname :translated-pathname)))
;                 (mac-open-input-file (file tpathname)
;                   (if (null file)
;                       (list pathname)
;                     (list* pathname
;                            :truename (file-truename file)
;                            (mac-file-properties (file-cmd-blk file))))))))


;;;
;;; these operations are errors in the mac-file-system
;;;


(defmethod (mac-file-access :create-link) (pathname target &optional (error t))
  pathname target
  (fs:handling-errors error
    (fs:lm-signal-error 'fs:links-not-supported nil nil :create-link)))


(defmethod (mac-file-access :remote-connect) (pathname &optional (error t) ignore)
  pathname
  (fs:handling-errors error
    (fs:lm-signal-error 'fs:unknown-operation nil nil :remote-connect)))


(COMPILE-FLAVOR-METHODS mac-file-access)


(fs:define-file-access mac-file-access .80s0 (:file-system-type :kmac))        ;why

(defun define-mac-file-system ()
  (let ((host-structure (si:make-host-alist-elem :name "KMAC"
                                                 :name-list '("KMAC" "K-MAC")
                                                 :system-type-internal ':kmac
                                                 :machine-type-internal ':kmac
                                                 :site-name site-name
                                                 )))

    (setq mac-host (make-instance 'fs:mac-host :alist-elem host-structure))
    (setf (si:host-instance host-structure) mac-host)
    (setq fs:*pathname-host-list* (nconc fs:*pathname-host-list* (ncons mac-host)))))
;;; ceci devrait etre definit dans le fichier SITE;HSTTBL.LISP
;;;   (push host-structure si:host-alist)))



;;; a faire comme initialisation
;;;
(COMMENT
 (define-mac-file-system)
 (fs:set-default-pathname (fs:parse-pathname "kmac:nm3:lmi:essai:essai.text#>") )
 (fs:set-default-pathname (fs:parse-pathname "lama:renaud;essai.text#>") )
 )
