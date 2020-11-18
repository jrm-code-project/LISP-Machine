;;; -*- Mode:LISP; Package:ZWEI; Base:10 -*-

(defvar tv:*mouse-select-in-progress* nil)
(defvar *set-interval-internal-in-progress* nil)
(defvar *fsdebug-process* nil)
(defvar *no-remote-dbg-msg* "**** Only Local Files Can Be Debugged ****")
(defvar *dbg-line* nil)
(defvar *dbg-pathname* nil)
(defvar *dbg-file-descriptor* nil)
(defvar *old-closed-bit* nil)
(defvar *new-closed-bit* nil)
(defvar *old-deleted-bit* nil)
(defvar *new-deleted-bit* nil)
(defvar *old-open-count* nil)
(defvar *new-open-count* nil)
(defvar *old-block-status* nil)
(defvar *new-block-status* nil)


(defcom COM-LEAVE-FSDEBUG-MODE "" ()
  (when (eq (send *dired-display-frame* :configuration) 'debug-configuration)
    (without-interrupts
      (tv:delaying-screen-management
        (send *dired-display-frame* :set-configuration 'dired-configuration))
      (send *dired-display-pane* :mouse-select)
;     (fs:blast-message-to-file-servers
;      (format nil "Resuming file service from ~A."
;              (or (send si:local-host :name) "remote machine")))
;     (chaos:enable)
      ))
  dis-none)


(defcom COM-ENTER-FSDEBUG-MODE "" ()
  (when (neq (send *dired-display-frame* :configuration) 'debug-configuration)
    (refuse-to-enter-remote-debug-mode)
    (without-interrupts
;     (fs:blast-message-to-file-servers
;      (format nil "~A~A~A~%~A"
;              "Temporarily interrupting file service to repair "
;              (or (send si:local-host :name) "server's")
;              " file system."
;              "Service will continue from where it left off when repairs are done."))
;     (chaos:disable)
      (tv:delaying-screen-management
;       (send *dired-data-pane* :clear-screen)
;       (send *dired-delta-pane* :clear-screen)
        (send *dired-display-frame* :set-configuration 'debug-configuration)
        (send *dired-display-pane* :mouse-select)
        (send *dired-data-pane* :set-current-font 'fonts:tr8b t)))
    (com-fsdebug-current-file-if-needed))
  dis-none)


(defun REFUSE-TO-ENTER-REMOTE-DEBUG-MODE ()
  (let* ((dbg-line (bp-line (point)))
         (dbg-pathname (getf (line-plist dbg-line) :pathname)))
    (if (not dbg-pathname)
        t
      (unless (eq (send dbg-pathname :host) si:local-host)
        (barf "Only Local Files Can Be Debugged")))))


(defcom COM-FSDEBUG-CURRENT-FILE-IF-NEEDED "" ()
  (let* ((dbg-line (bp-line (point)))
        (dbg-pathname (getf (line-plist dbg-line) :pathname)))
    (and dbg-pathname
         (not (fs:safe-pathname-equal dbg-pathname *dbg-pathname*))
         (refuse-to-try-debugging-remote-file dbg-pathname)
         (get-file-for-debugging)
         (fsdebug-file)))
  dis-none)


(defun REFUSE-TO-TRY-DEBUGGING-REMOTE-FILE (pathname)
  (if (eq (send pathname :host) si:local-host)
      t
    (unless (processing-remote-file-p)
      (set-processing-remote-file))
    nil))


(defun PROCESSING-REMOTE-FILE-P ()
  (string-equal (sixth (send *dired-delta-pane* :label)) *no-remote-dbg-msg*))


(defun SET-PROCESSING-REMOTE-FILE ()
  (send *dired-delta-pane* :update :label (list :string *no-remote-dbg-msg* :centered))
  (send *dired-delta-pane* :update :elems (list ""))
  (send *dired-data-pane* :set-display-item nil)
  (setq *dbg-pathname* nil))


(defun GET-FILE-FOR-DEBUGGING ()
  (when (and (setq *dbg-line* (bp-line (point)))
             (setq *dbg-pathname* (getf (line-plist *dbg-line*) :pathname))
             (setq *dbg-file-descriptor* (fs:lookup-file (send *dbg-pathname* :directory)
                                                         (send *dbg-pathname* :name)
                                                         (send *dbg-pathname* :type)
                                                         (send *dbg-pathname* :version)
                                                         nil nil nil)))
    (setq *old-closed-bit* (fs:file-closed? *dbg-file-descriptor*)
          *new-closed-bit* *old-closed-bit*
          *old-deleted-bit* (fs:file-deleted? *dbg-file-descriptor*)
          *new-deleted-bit* *old-deleted-bit*
          *old-open-count* (fs:file-open-count *dbg-file-descriptor*)
          *new-open-count* *old-open-count*
          *old-block-status* (aref fs:page-usage-table (fs:map-block-location (fs:file-map *dbg-file-descriptor*) 0))
          *new-block-status* *old-block-status*)
    t))


(defun FSDEBUG-FILE ()
  (insure-fsdebug-process-ok)
  (update-delta-pane-label)
  (update-delta-pane-items)
  (update-data-pane))


(defun INSURE-FSDEBUG-PROCESS-OK ()
  (when (or (null *fsdebug-process*)
            (not (memq *fsdebug-process* si:all-processes))
            (unless (string-equal (send *fsdebug-process* :wait-whostate) "choose")
              (send *fsdebug-process* :kill) t))
    (setq *fsdebug-process*
          (process-run-function "FSEDBUG Process" 'fsdebug-choice-process))))


(defun UPDATE-DELTA-PANE-LABEL ()
  (send *dired-delta-pane* :update :label
        (list :string (send *dbg-pathname* :string-for-printing)
              :font 'fonts:cptfontb :centered)))


(defun UPDATE-DELTA-PANE-ITEMS ()
  (send *dired-delta-pane* :update
        :elems
        (list " "
              `(*new-closed-bit*   "Closed Bit Set:  " :boolean)
              `(*new-deleted-bit*  "Deleted Bit Set: " :boolean)
              `(*new-open-count*   "Open Count:      " :number)
              (if *old-block-status*
                  `(*new-block-status* "Block Status:    "
                                       :assoc (("Used " .     ,fs:put-used)
                                               ("Reserved " . ,fs:put-reserved)
                                               ("Free " .     ,fs:put-free)
                                               ("Bad" .       ,fs:put-unusable)))
                "Block Status:    (No blocks)" )
              " ")))


(defun UPDATE-DATA-PANE ()
  (send *dired-data-pane*
        :set-display-item (fs:make-scrollable-map-description *dbg-file-descriptor*)))


(defun FSDEBUG-CHOICE-PROCESS ()
  (do-forever
    (tv:choose-variable-values-process-message
      *dired-delta-pane*
      (progn
        (process-wait "Choose" *dired-delta-pane* :listen)
        (send *dired-delta-pane* :any-tyi)))
    (tv:delaying-screen-management
      (send *dired-display-pane* :mouse-select))))


(defun FSDEBUG-RESET (&rest ignore)
  (unless (processing-remote-file-p)
    (if (and (eq *new-closed-bit* *old-closed-bit*)
             (eq *new-deleted-bit* *old-deleted-bit*)
             (equal *new-open-count* *old-open-count*)
             (equal *new-block-status* *old-block-status*))
        (funcall-in-zmacs 'com-no-values-changed *dired-display-pane*)
      (setq *new-closed-bit* *old-closed-bit*
            *new-deleted-bit* *old-deleted-bit*
            *new-open-count* *old-open-count*
            *new-block-status* *old-block-status*)
      (update-delta-pane-items))))


(defun FSDEBUG-DO-IT (&rest ignore &aux write-changes)
  (unless (processing-remote-file-p)
    (cond-every
      ((neq *new-closed-bit* *old-closed-bit*)
       (setf (fs:file-closed? *dbg-file-descriptor*) *new-closed-bit*)
       (setq write-changes t))
      ((neq *new-deleted-bit* *old-deleted-bit*)
       (setf (fs:file-deleted? *dbg-file-descriptor*) *new-deleted-bit*)
       (setf (getf (line-plist *dbg-line*) :deleted) *new-deleted-bit*)
       (setq write-changes t))
      ((not (equal *new-open-count* *old-open-count*))
       (setf (fs:file-open-count *dbg-file-descriptor*) *new-open-count*)
       (setq write-changes t))
      ((not (equal *new-block-status* *old-block-status*))
       (fs:using-put (fs:set-map-disk-space (fs:file-map *dbg-file-descriptor*) *new-block-status*))
       (setq write-changes t))
      (write-changes
       (fs:write-directory-files (fs:file-directory *dbg-file-descriptor*))
       (setq *old-closed-bit* *new-closed-bit*
             *old-deleted-bit* *new-deleted-bit*
             *old-open-count* *new-open-count*
             *old-block-status* *new-block-status*)
       (funcall-in-zmacs 'com-dired-regenerate-line *dired-display-pane*)
       (update-delta-pane-items)
       (update-data-pane))
      ((not write-changes)
       (funcall-in-zmacs 'com-no-values-changed *dired-display-pane*)))
    nil))


(defcom COM-NO-VALUES-CHANGED "" ()
  (barf "No values have been changed.")
  dis-none)


(defcom COM-DIRED-REGENERATE-LINE "" ()
  (dired-regenerate-line *dbg-line*)
  (must-redisplay *window* dis-text)
  (send *window* :redisplay :point nil nil nil)
  dis-none)


(defun FUNCALL-IN-ZMACS (command zmacs-window)
  (set-comtab zwei:*zmacs-comtab* `(#/hyper-super-meta-control-top-i ,command))
  (tv:io-buffer-put (send zmacs-window :io-buffer) #/hyper-super-meta-control-top-i))


(defcom COM-STANDARD-CONFIGURATION "" ()
  (tv:delaying-screen-management
    (send *dired-constraint-frame* :set-selection-substitute
          (send *dired-display-frame* :selection-substitute))
    (send *dired-constraint-frame* :set-configuration 'standard-configuration))
  dis-none)


(defcom COM-FOUR-PANE-CONFIGURATION "" ()
  (tv:delaying-screen-management
    (send *dired-constraint-frame* :set-selection-substitute
          (send *dired-display-frame* :selection-substitute))
    (send *dired-constraint-frame* :set-configuration 'four-pane-configuration))
  dis-none)


(defun MOVE-BP-HOOK (&rest ignore)
  (and (eq (car (send tv:main-screen :inferiors)) *dired-constraint-frame*)
       (eq (send zwei:*dired-display-frame* :configuration) 'zwei:debug-configuration)
       (com-fsdebug-current-file-if-needed)))
