;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; DVI-USER - friendly interface to control processing by DVI system.
;;;
;;; DVI is used to process .DVI files (e.g., BoTeX output) into a
;;; format for printing, e.g. IMPRESS to Imagen printer.

;;; The interface is DVI-USER. Also, use SET-DVI-USER to establish
;;; per-user settings -- the host/working directory where .dvi file(s)
;;; will be found, the host/working directory where .impress output
;;; files should be put.

;;; Limitations:
;;;  - Assumes, for our convenience in Lowell, that output format is Impress.
;;;

(in-package 'user)

(export 'dvi-user)

;;; I need a method to step down a subdirectory.  This seems to be
;;; a generally useful thing to have, nu?

(defmethod (fs:host-pathname :down-subdirectory)(subdir)
  (send self :new-directory
        (copy-list
          (reverse
            (cons subdir
                  (reverse
                    (let((dir (send self :directory)))
                      (if (atom dir) (ncons dir) dir))))))))

(defmethod (fs:pathname-normally-lowercase-mixin :down-subdirectory)(raw-subdir)
  (send self :new-raw-directory
        (copy-list
          (reverse
            (cons raw-subdir
                  (reverse
                    (let((dir (send self :raw-directory)))
                      (if (atom dir) (ncons dir) dir))))))))

;;; Parameters

(defparameter *default-dvi-host* "CAP"
  "The host where most users' .dvi files will be")
(defparameter *default-impress-host* si:associated-machine
  "The host where most users put their .impress files")
(defparameter *default-impress-printer* si:*default-bit-array-printer*)

(defparameter *dvi-user-alist* nil
  "One element per login user; holds variables set by SET-DVI-USER")

(defun get-dvi-user(&optional (user-id fs:user-id))
  "Get DVI user info for user with USER-ID (defaults to logged-in user)"
  (ass #'string-equal user-id *dvi-user-alist*))

(defun get-dvi-dvi-doc(plist)
  "From a particular user's info, provide the name of their current .dvi file"
  (let((dvi-home (get plist :dvi-home))
       (document (get plist :document)))
    (when dvi-home
      (make-pathname :raw-name document
                     :defaults dvi-home :type "DVI"))))

(defun get-dvi-impress-printer(plist)
  "From a particular user's info, provide the spec for their current Impress printer"
  (or (get plist :impress-printer) si:*default-bit-array-printer*))


(defun get-dvi-impress-doc(plist)
  "From a particular user's info, provide the name of their current .impress file"
  (let((impress-home (get plist :impress-home))
       (document (get plist :document)))
    (when impress-home
      (make-pathname :raw-name document
                     :defaults impress-home :type "IMPRESS"))))

(defun show-dvi-user(&optional (user-id fs:user-id))
  "Show current settings for USER-ID"
  (let((user-plist (get-dvi-user user-id)))
    (format t "~%DVI     doc is ~a" (or(get-dvi-dvi-doc user-plist) "not set."))
    (format t "~%IMPRESS doc is ~a" (or(get-dvi-impress-doc user-plist) "not set."))
    (format t "~%IMPRESS printer is ~s" (or(get-dvi-impress-printer user-plist) "not set."))
    (format t "~%IMPRESS pages will ~:[not ~]be centered on the paper." (get user-plist :center))
    (not(null user-plist))))

(defun set-dvi-user(&optional (user-id fs:user-id))
  "Set DVI-USER options for USER-ID (defaults to logged-in user)"
  (when (or (null (show-dvi-user user-id))
            (y-or-n-p "Modify the current settings? "))
    (let*((delimiter '(:delimited-string-or-nil :delimiter (#\space #\end #\return)))
          (default-user-plist (or (copy-list (get-dvi-user user-id)) (ncons user-id)))
          (default-doc (get default-user-plist :document "document"))
          (subdir "doc")
          (default-dvi-home
            (make-pathname
              :defaults (or (get default-user-plist :dvi-home)
                            (send (fs:user-homedir *default-dvi-host*)
                                  :down-subdirectory subdir))
              :name :wild
              :type :wild
              :version :wild))
          (default-impress-home
            (make-pathname
              :defaults (or (get default-user-plist :impress-home)
                            (send (fs:user-homedir *default-impress-host*)
                                  :down-subdirectory subdir))
              :name :wild
              :type :wild
              :version :wild))
          ;;Get info from user
          (current-doc (or (prompt-and-read
                             delimiter
                             "~%>> Current document name~& (press ~\lozenged-character\ for default, ~a) : "
                             #\space
                             default-doc)
                           default-doc))
          (dvi-home (prompt-and-read
                      `(:pathname :defaults ,default-dvi-home)
                      "~%>> Current DVI working directory~& (press ~\lozenged-character\ for default, ~s) :"
                      #\return
                      default-dvi-home))
          (impress-home (prompt-and-read
                          `(:pathname :defaults ,default-impress-home)
                          "~%>> Current IMPRESS working directory~& (press ~\lozenged-character\ for default, ~a) : "
                          #\return
                          default-impress-home))
          (impress-printer (prompt-and-read
                             `(:eval-read-or-end :default ,(get-dvi-impress-printer default-user-plist))
                             "~%>> IMPRESS printer specification~& (press ~\lozenged-character\ for default, ~s) : "
                             #\space
                             (get-dvi-impress-printer default-user-plist)))
          (center (y-or-n-p "Center text on page?"))
          )
      ;;Modify properties
      (setf (get default-user-plist :document) current-doc)
      (setf (get default-user-plist :dvi-home) dvi-home)
      (setf (get default-user-plist :impress-home) impress-home)
      (setf (get default-user-plist :impress-printer) impress-printer)
      (setf (get default-user-plist :center) center)
      (let((*dvi-user-alist* (ncons default-user-plist)))
        (show-dvi-user user-id))
      (when (y-or-n-p "Ok? ")
        (setq *dvi-user-alist*
              (#+LMI cl:substitute
               #-LMI substitute
               default-user-plist user-id
               *dvi-user-alist*
               :key #'car :test #'string-equal))
        (unless (get-dvi-user user-id)
          (push default-user-plist *dvi-user-alist*))))))

(defun dvi-user(&optional (user-id fs:user-id))
  "Set DVI-USER options for USER-ID (defaults to logged-in user)"
  (set-dvi-user user-id)
  (let*((user-plist (get-dvi-user user-id))
        (document (get user-plist :document))
        (input (get-dvi-dvi-doc user-plist))
        (output (get-dvi-impress-doc user-plist))
        (impress-printer (get-dvi-impress-printer user-plist))
        (center (get user-plist :center)))
    (and (or input (format t "~%??? No DVI document for ~a" user-id))
         (or output (format t "~%??? No IMPRESS document for ~a" user-id))
         (let*((saveit (y-or-n-p "Backup ~a ? " document))
               (backup (and saveit
                            (make-pathname
                             :name (pathname-name input)
                             :type "BAK"
                             :version :highest
                             :defaults output)))
               (copyit (and backup
                            (y-or-n-p "...Copy from ~a to ~a <confirm> ? "
                                      input backup)))
               (showit (y-or-n-p "Show ~a ? " input))
               (processit (y-or-n-p "Process ~a ?" input))
               (printit  (y-or-n-p "Print ~a ?" output)))
           (and
             copyit
             (let((probe (probe-file backup)))
               (or (null probe)
                   (yes-or-no-p "The file ~a exists.~%Are you sure you want to supersede it ? "
                                probe)))
             (progn
               (format t "~%Backing up ~a ..." input)
               (copy-file input backup)))
           (if showit
               (dvi:show-dvi input))
           (if processit
               (dvi:process-dvi input "imagen"
                                :output-file output
                                :center center))
           (if printit
               (hardcopy-file output
                              :format :impress
                              :printer impress-printer))))))
