;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-
;;;


(declare (special mac-host))

(defconst  NO-ERROR       0)
(defconst  NO-MORE-FILES  #Xffffffd5)



;;;
;;;  File structure for a mac-file.
;;;


(defstruct (file :conc-name)
  directory                                  ;A string (or a list of strings)
  name                                       ;A string.
  type                                       ;A string.
  version                                    ;A number (16b).
  dev-blk                                    ;Pointer to the file device blk in BC memory
  info-blk                                   ;Pointer to the file information
  )


(defmacro with-dev-blk ((dev &optional (release t)) &body body)
  `(let* ((,dev nil))
     (unwind-protect
         (progn
           (setq ,dev  (major-k-create-device #&FILE ))
           ,@body)
       (if (and ,release ,dev)
           (major-k-delete-device #&FILE ,dev)))))


(defmacro with-path-blk ((path &optional (release t)) &body body)
  `(let* ((,path nil))
     (unwind-protect
         (progn
           (setq ,path (new-pathname-blk))
           ,@body)
       (if (and ,release ,path)
           (release-pathname-blk ,path)))))

(defmacro with-info-blk ((info &optional (release t)) &body body)
  `(let* ((,info nil))
     (unwind-protect
         (progn
           (setq ,info (new-info-blk))
           ,@body)
       (if (and ,release ,info)
           (release-info-blk ,info)))))


(defmacro send-file-cmd-wait (struct cmd narg nval &rest args)
  `(progn
     (send-cmd-wait ,struct ,cmd ,narg ,nval ,@args)
     (check-file-error   (Command-err-code ,struct))))


;;; all seem ok
(defun mac-open-file (pathname directory name type version
                      &key (error t)
                           (direction :input)
                           (characters :default)
                           (byte-size :default)
                           deleted
                           preserve-dates
                           inhibit-links
                           (element-type 'string-char element-type-p)
                           (if-exists (if (memq (pathname-version pathname)
                                                ;; :UNSPECIFIC here is to prevent lossage
                                                ;; writing ITS files with no version numbers.
                                                '(:newest :unspecific))
                                          :new-version :supersede)
                                      if-exists-p)
                           (if-does-not-exist
                             (cond ((memq direction '(:probe :probe-directory :probe-link))
                                    nil)
                                   ((and (memq direction '(:output :io))
                                         (not (memq if-exists '(:overwrite :append))))
                                    :create)
                                   ;; Note: if DIRECTION is NIL, this defaults to :ERROR
                                   ;; for compatibility with the past.
                                   ;; A Common-Lisp program would use :PROBE
                                   ;; and get NIL as the default for this.
                                   (t :error)))
                      &aux file initial-plist old-file
                           phony-characters sign-extend-bytes)
  "Implements the :OPEN message for Mac-file pathnames."
  inhibit-links file initial-plist old-file
  (fs:identify-file-operation :open
    (fs:handling-errors error
      (case direction
        (:io (ferror 'unimplemented-option "Bidirectional streams not supported."))
        ((:input :output :probe-directory))
        ((nil :probe :probe-link) (setq direction :probe))
        (t (ferror 'unimplemented-option "~S is not a valid DIRECTION argument" direction)))
      (unless (memq if-exists '(:error :new-version :rename :rename-and-delete
                                       :overwrite :append :supersede nil))
        (ferror 'unimplemented-option "~S is not a valid IF-EXISTS argument" if-exists))
      (unless (memq if-does-not-exist
                    '(:error :create nil))
        (ferror 'unimplemented-option
                "~S is not a valid IF-DOES-NOT-EXISTS argument" if-exists))
      (when element-type-p
        (setf (values characters byte-size phony-characters sign-extend-bytes)
              (fs:decode-element-type element-type byte-size)))
      (if (or phony-characters sign-extend-bytes)
          (ferror 'unimplemented-option "~S as element-type is not implemented."
                  element-type))
      (if (not (memq byte-size '(16. 8 4 2 1 :default)))
          (ferror 'fs:invalid-byte-size "~S is a invalid byte size." byte-size))
      (let (succes file)
        (multiple-value (succes file)
          (mac-open-the-file directory name type version
                             direction
                             if-exists
                             if-does-not-exist
                             characters
                             byte-size
                             preserve-dates
                             deleted))
        (if (not succes)
            (ferror "MAC: open error")
          (if (eq direction :probe-directory)
              (make-instance 'mac-probe-stream
                             :truename (send pathname :new-pathname
                                             :name nil :type nil :version nil)
                             :pathname pathname)
            (make-instance
              (selectq direction
                (:input  (if characters 'mac-character-input-stream 'mac-input-stream))
                (:output (if characters 'mac-character-output-stream 'mac-output-stream))
                (:probe 'mac-probe-stream))
              :file           file
              :pathname       pathname)))))))


(defun mac-open-the-file (directory name type version direction if-exists if-does-not-exist
                          characters byte-size preserve-dates deleted)
  (with-dev-blk (dev-blk nil)
     (selectq direction
       ((:input :output)
        (setf (dev-buffer-chain dev-blk) (get-chained-iobuffers *nb-input-iobuffer*))
        (setf (dev-current-lisp-buffer dev-blk) (dev-buffer-chain dev-blk))))
    (with-path-blk (path-blk)
      (with-info-blk (info-blk nil)
         (write-pathname-blk path-blk directory name type version)
         (send-file-cmd-wait  (dev-cmd-blk dev-blk) CMD-OPEN 9 0
                              path-blk
                              info-blk
                              (expr->num direction)
                              (expr->num characters)
                              (expr->num byte-size)
                              (expr->num if-exists)
                              (expr->num if-does-not-exist)
                              (expr->num deleted)
                              (expr->num preserve-dates))
         (values t (make-file :directory directory
                              :name      name
                              :type      type
                              :version   (num->expr (info-version info-blk))
                              :dev-blk   dev-blk
                              :info-blk info-blk))))))


(defun mac-close-file (file &optional ignore)
  (send-file-cmd-wait (dev-cmd-blk (file-dev-blk file)) CMD-CLOSE 0 0)
  (major-k-delete-device #&FILE (file-dev-blk file))
  (release-info-blk (file-info-blk file)))



(defun mac-rename-file (file new-directory new-name new-type new-version)
  (with-path-blk (path-blk)
     (write-pathname-blk path-blk new-directory new-name new-type new-version)
     (send-file-cmd-wait (dev-cmd-blk (file-dev-blk file)) CMD-RENAME 1 0
                         path-blk)
     (make-instance 'mac-pathname
                    :host      mac-host
                    :device    :unspecific
                    :directory new-directory
                    :name      new-name
                    :type      new-type
                    :version   new-version)))


(defun mac-delete-file (file &optional ignore)
  (send-file-cmd-wait (dev-cmd-blk (file-dev-blk file)) CMD-DELETE 0 0))


(defun mac-change-file-properties (file plist)
  (with-info-blk (info-blk)
     (write-info-blk info-blk plist)
     (send-file-cmd-wait  (dev-cmd-blk (file-dev-blk file)) CMD-CHANGE-PROPERTIES  1 0 info-blk)))



(defun mac-directory-list (pathname host directory name type version options)
  "Implements the :DIRECTORY-LIST message for pathnames."
  (let ((error-p          (not (memq ':noerror options)))
        (fastp            (memq ':fast options))
        (deleted?         (memq ':deleted options))
;;;     (not-backed-up?   (memq :not-backed-up options))
        (directories-only (memq ':directories-only options))
        (old-whostate     (si:process-wait-whostate current-process)))
    (setf (si:process-wait-whostate current-process) "Directory")
    (tv:who-line-process-change current-process)
    (unwind-protect
        (fs:identify-file-operation ':directory-list
          (fs:handling-errors error-p
            (cons (if (or fastp directories-only)
                      (list nil)
                    (mac-directory-list-header pathname))
                  (if directories-only
                      (mac-all-directories host error-p)
                    (mac-do-directory-list pathname directory name type version (not(null deleted?)))))))
      (progn (setf (si:process-wait-whostate current-process) old-whostate)
             (tv:who-line-process-change current-process)))))



(defun mac-do-directory-list (pathname directory name type version deleted?)
  (let ((index 0))
    (with-dev-blk (dev-blk)
     (with-path-blk (path-blk)
      (with-info-blk (info-blk)
         (write-pathname-blk path-blk directory name type version)
         (send-file-cmd-wait (dev-cmd-blk dev-blk) CMD-DIRECTORY-LIST 4 1
                             path-blk
                             index
                             (expr->num deleted?)
                             info-blk)
         (if (= (Command-args (dev-cmd-blk dev-blk) 16) NO-MORE-FILES)
             '(nil)
           (cons (cons (read-truename pathname info-blk)
                       (read-info-blk info-blk))
                 (loop do (send-file-cmd-wait (dev-cmd-blk dev-blk)CMD-DIRECTORY-LIST 4 1
                                              path-blk
                                              (incf index)
                                              (expr->num deleted?)
                                              info-blk)
                       while (//= (Command-args (dev-cmd-blk dev-blk) 16) NO-MORE-FILES)
                       collect (cons (read-truename pathname info-blk)
                                     (read-info-blk info-blk))))))))))


(defun mac-expunge-directory (directory name type version)
  (with-dev-blk (dev-blk)
   (with-path-blk (path-blk)
     (write-pathname-blk path-blk directory name type version)
     (send-file-cmd-wait (dev-cmd-blk dev-blk) CMD-EXPUNGE-DIRECTORY 1 1
                         path-blk)
     (Command-args(dev-cmd-blk dev-blk) 4))))


(defun mac-create-directory (directory)
  (with-dev-blk (dev-blk)
    (with-path-blk (path-blk)
      (write-pathname-blk path-blk directory 0 0 0)
      (send-file-cmd-wait  (dev-cmd-blk dev-blk) CMD-CREATE-DIR 1 0 path-blk))))


(defun mac-complete-path (directory name type default-name default-type options
                          &aux deleted-p write-p new-ok success)
  success default-type default-name
  (dolist (key options)
    (case key
      (:DELETED             (setq deleted-p t))
      ((:READ :IN)          (setq write-p nil))
      ((:PRINT :OUT :WRITE) (setq write-p t))
      (:OLD                 (setq new-ok nil))
      (:NEW-OK              (setq new-ok t))
      (otherwise            (ferror nil "~s is not a recognized option." key :complete-string))))
  (with-dev-blk (dev-blk)
    (with-path-blk (path-blk)
       (write-pathname-blk path-blk directory name type 0)
       (send-file-cmd-wait (dev-cmd-blk dev-blk) CMD-COMPLETE-STRING 4 1
                           path-blk
                           (expr->num deleted-p)
                           (expr->num write-p)
                           (expr->num new-ok))
       (num->expr (Command-args (dev-cmd-blk dev-blk) 16)))))



;;
;;  Utilities functions
;;
;; ok


;; change
(defun read-truename (pathname struct)
  (send pathname :new-pathname
                 :name      (num->expr (info-name    struct) struct)
                 :type      (num->expr (info-type    struct) struct)
                 :version   (num->expr (info-version struct) struct)))


;; ok
(defun mac-directory-list-header (pathname)
  `(nil :disk-space-description nil
        :settable-properties    ,mac-settable-properties
        :pathname               ,pathname))



;;;
;;; Error handler interface.
;;;

(fs:DEFINE-ERRORS
  fs:filepos-out-of-range    "filepos-out-of-range"
  fs:file-open-for-output    "file-open-for-output"
  fs:not-available           "not-available"
  fs:multiple-file-not-found "multiple-file-not-found"
  fs:wildcard-not-allowed    "wildcard-not-allowed"
  fs:file-already-exists     "file-already-exist"
  fs:unknown-property        "unknown-property")


(defun check-file-error (error)
  (when (> error #x80000000)
    (selectq error
      ((#xFFFFFFCF
        #xFFFFFFCA) (fs:lm-signal-error 'fs:file-open-for-output))
      ((#xFFFFFFDF
        #xFFFFFFDE) (fs:lm-signal-error 'fs:no-more-room))
      ((#xFFFFFFD8
        #xFFFFFFCC) (fs:lm-signal-error 'fs:filepos-out-of-range))
      ((#xFFFFFFD5
        #xFFFFD8E8) (fs:lm-signal-error 'fs:file-not-found))
      (#xFFFFFFD0   (ferror "duplicate-file-name"))
      (#xFFFFFFD3   (fs:lm-signal-error 'fs:file-locked))
      (#xFFFFD8E2   (fs:lm-signal-error 'fs:invalid-byte-size))
      (#xFFFFFFCB   (fs:lm-signal-error 'fs:not-available))
      (#xFFFFFF88   (fs:lm-signal-error 'fs:directory-not-found))
      (#xFFFFD8EE   (fs:lm-signal-error 'fs:multiple-file-not-found))
      (#xFFFFD8ED   (fs:lm-signal-error 'fs:wildcard-not-allowed))
      (#xFFFFD8EC   (fs:lm-signal-error 'fs:wrong-kind-of-file))
      (#xFFFFD8E1   (fs:lm-signal-error 'fs:file-already-exists))
      (#xFFFFD8EB   (fs:lm-signal-error 'fs:directory-not-empty))
      (#xFFFFD8EF   (fs:lm-signal-error 'fs:dont-delete-flag-set))
      (#xFFFFFFD3   (fs:lm-signal-error 'fs:rename-to-existing-file))
      (#xFFFFD8EA   (fs:lm-signal-error 'fs:rename-across-directories))
      (#xFFFFD8E5   (fs:lm-signal-error 'fs:unknown-property))
      (#xFFFFD8E9   (ferror "Name to long"))
      (#xFFFFD8E7   (ferror "Version lo long"))
      (#xFFFFD8E6   (ferror "Can't be expunged, in use")))))
