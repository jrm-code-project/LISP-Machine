;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10 -*-

;; This file contains the access interface to the local file system.

(DEFFLAVOR LOCAL-FILE-ACCESS () (DIRECTORY-LIST-MIXIN BASIC-ACCESS))

(DEFINE-FILE-ACCESS LOCAL-FILE-ACCESS .95S0 :LOCAL)

;;; This property is used by file servers to notify users/connections of abnormalities or
;;; losing conditions in the file system.  The return value should be a one-line string, which
;;; a file server will use for a notification, or NIL, not to notify.
(defvar *disk-space-warning-threshold* 800. "Warning threshold for Local-File system disk space")

(defun local-file-lossage-line ()
  (let ((free (aref put-usage-array put-free)))
    (if (< free *disk-space-warning-threshold*)
        (format () "Disk space low !  Only ~D blocks free." free))))

(defprop local-file-access local-file-lossage-line notification-for-server)

(DEFMETHOD (LOCAL-FILE-ACCESS :RESET) () (SEND SELF :CLOSE-ALL-FILES))

(DEFMETHOD (LOCAL-FILE-ACCESS :OPEN-STREAMS) ()
  LM-FILE-STREAMS-LIST)

(DEFMETHOD (LOCAL-FILE-ACCESS :ACCESS-DESCRIPTION) ()
  "Direct")

(DEFMETHOD (LOCAL-FILE-ACCESS :ASSURE-ACCESS) ()
  (UNLESS (BOUNDP 'DISK-CONFIGURATION)
    (si:disk-init)
    (boot-file-system)))

(DEFMETHOD (LOCAL-FILE-ACCESS :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (DOLIST (S LM-FILE-STREAMS-LIST)
    (CLOSE S MODE)))

(DEFMETHOD (LOCAL-FILE-ACCESS :HOMEDIR) (&OPTIONAL (USER USER-ID))
  USER
  (MAKE-PATHNAME :HOST SI:LOCAL-HOST :DIRECTORY USER))

(DEFMETHOD (LOCAL-FILE-ACCESS :CHANGE-PROPERTIES) (PATHNAME ERROR-P &REST PLIST)
  (IDENTIFY-FILE-OPERATION :CHANGE-PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (OPEN-INPUT-FILE-OR-DIRECTORY (FILE PATHNAME)
        (LMFS-CHANGE-FILE-PROPERTIES FILE PLIST)))))

(DEFMETHOD (LOCAL-FILE-ACCESS :PROPERTIES) (PATHNAME &OPTIONAL ERROR-P)
  (IDENTIFY-FILE-OPERATION :PROPERTIES
    (HANDLING-ERRORS ERROR-P
      (OPEN-INPUT-FILE (F PATHNAME)
        (VALUES (CONS (FILE-TRUENAME F) (LMFS-FILE-PROPERTIES F))
                LM-UNSETTABLE-PROPERTIES)))))

(DEFMETHOD (LOCAL-FILE-ACCESS :COMPLETE-STRING) (PATHNAME STRING OPTIONS)
  (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER)
      (SEND PATHNAME :PARSE-NAMESTRING HOST STRING)
    (MULTIPLE-VALUE-BIND (NEW-DIRECTORY NEW-NAME NEW-TYPE COMPLETION)
        (LMFS-COMPLETE-PATH (OR DIR (PATHNAME-RAW-DIRECTORY PATHNAME) "")
                            (OR NAM "") (OR TYP "")
                            (PATHNAME-RAW-NAME PATHNAME)
                            (PATHNAME-RAW-TYPE PATHNAME)
                            OPTIONS)
      (VALUES (LM-NAMESTRING HOST (OR DEV (PATHNAME-RAW-DEVICE PATHNAME))
                             NEW-DIRECTORY NEW-NAME NEW-TYPE VER)
              COMPLETION))))

(DEFMETHOD (LOCAL-FILE-ACCESS :CREATE-DIRECTORY) (PATHNAME &OPTIONAL (ERROR T))
  (IDENTIFY-FILE-OPERATION :CREATE-DIRECTORY
    (HANDLING-ERRORS ERROR
      (LMFS-CREATE-DIRECTORY (PATHNAME-RAW-DIRECTORY PATHNAME))
      T)))

(DEFMETHOD (LOCAL-FILE-ACCESS :CREATE-LINK) (PATHNAME TARGET &OPTIONAL (ERROR T))
  PATHNAME TARGET
  (HANDLING-ERRORS ERROR
    (LM-SIGNAL-ERROR 'LINKS-NOT-SUPPORTED NIL NIL :CREATE-LINK)))

(DEFMETHOD (LOCAL-FILE-ACCESS :REMOTE-CONNECT) (PATHNAME &OPTIONAL (ERROR T) IGNORE)
  (LET ((*CURRENT-OPERATION-PATHNAME* PATHNAME))
    (HANDLING-ERRORS ERROR
      (LM-SIGNAL-ERROR 'UNKNOWN-OPERATION NIL NIL :REMOTE-CONNECT))))

(DEFMETHOD (LOCAL-FILE-ACCESS :OPEN) (FILE *CURRENT-OPERATION-PATHNAME* &REST OPTIONS)
  (APPLY 'LMFS-OPEN-FILE *CURRENT-OPERATION-PATHNAME*
         (PATHNAME-RAW-DIRECTORY FILE)
         (PATHNAME-RAW-NAME FILE)
         (PATHNAME-RAW-TYPE FILE)
         (PATHNAME-RAW-VERSION FILE)
         OPTIONS))

(DEFMETHOD (LOCAL-FILE-ACCESS :DIRECTORY-LIST) (PATHNAME OPTIONS)
  (LET ((*CURRENT-OPERATION-PATHNAME* PATHNAME))
    (LMFS-DIRECTORY-LIST PATHNAME HOST
                         (PATHNAME-RAW-DIRECTORY PATHNAME)
                         (PATHNAME-RAW-NAME PATHNAME)
                         (PATHNAME-RAW-TYPE PATHNAME)
                         (PATHNAME-RAW-VERSION PATHNAME)
                         OPTIONS)))

(DEFMETHOD (LOCAL-FILE-ACCESS :RENAME) (PATHNAME NEW-NAME &OPTIONAL (ERROR-P T))
  (IDENTIFY-FILE-OPERATION :RENAME
    (HANDLING-ERRORS ERROR-P
      (OPEN-INPUT-FILE (FILE PATHNAME)
        (LET ((*CURRENT-OPERATION-PATHNAME* NEW-NAME))
          (LMFS-RENAME-FILE FILE
                            (PATHNAME-DIRECTORY NEW-NAME)
                            (OR (PATHNAME-NAME NEW-NAME) "FOO")
                            (OR (PATHNAME-TYPE NEW-NAME) :UNSPECIFIC)
                            (PATHNAME-VERSION NEW-NAME)))))))

(DEFMETHOD (LOCAL-FILE-ACCESS :DELETE) (PATHNAME &OPTIONAL (ERROR-P T))
  (LET ((*CURRENT-OPERATION-PATHNAME* PATHNAME))
    (IDENTIFY-FILE-OPERATION :DELETE
      (HANDLING-ERRORS ERROR-P
        (OPEN-INPUT-FILE-OR-DIRECTORY (FILE PATHNAME)
          (LMFS-DELETE-FILE FILE))))))

(DEFMETHOD (LOCAL-FILE-ACCESS :EXPUNGE) (PATHNAME &OPTIONAL (ERROR T))
  (LET ((*CURRENT-OPERATION-PATHNAME* PATHNAME))
    (IDENTIFY-FILE-OPERATION :EXPUNGE
      (HANDLING-ERRORS ERROR
        (LMFS-EXPUNGE-DIRECTORY
          (PATHNAME-RAW-DIRECTORY PATHNAME)
          (PATHNAME-RAW-NAME PATHNAME)
          (PATHNAME-RAW-TYPE PATHNAME)
          (PATHNAME-RAW-VERSION PATHNAME))))))

(DEFMETHOD (LOCAL-FILE-ACCESS :DELETE-MULTIPLE-FILES) (ERROR-P PATHNAMES)
  (IDENTIFY-FILE-OPERATION :DELETE
    (LOOP FOR PATHNAME IN PATHNAMES
          WITH FILES-OF-DIRECTORY-TO-WRITE = NIL
          collect (HANDLING-ERRORS ERROR-P
                    (OPEN-INPUT-FILE-OR-DIRECTORY (FILE PATHNAME)
                      (LMFS-DELETE-FILE FILE NIL)
                      (LOOP FOR ENTRY IN FILES-OF-DIRECTORY-TO-WRITE
                            WHEN (EQUAL (FILE-DIRECTORY FILE) (FILE-DIRECTORY ENTRY))
                            RETURN NIL
                            FINALLY (PUSH FILE FILES-OF-DIRECTORY-TO-WRITE))))
          FINALLY (handling-errors error-p
                    (DOLIST (FILE FILES-OF-DIRECTORY-TO-WRITE)
                      (WRITE-DIRECTORY-OF-FILE FILE))))))

(DEFMETHOD (LOCAL-FILE-ACCESS :ALL-DIRECTORIES) (OPTIONS)
  (LMFS-ALL-DIRECTORIES HOST (NOT (MEMQ :NOERROR OPTIONS))))

(DEFMETHOD (LOCAL-FILE-ACCESS :MULTIPLE-FILE-PLISTS) (PATHNAMES OPTIONS)
  "This is a hack to speed up DIRED.
There are no currently meaningful options." OPTIONS
  (IDENTIFY-FILE-OPERATION :PROPERTIES
    (MAPCAR #'(LAMBDA (PATHNAME)
                (LET ((TPATHNAME (SEND PATHNAME :TRANSLATED-PATHNAME)))
                  (OPEN-INPUT-FILE (FILE TPATHNAME)
                    (IF (NULL FILE)
                        (LIST PATHNAME)
                      (LIST* PATHNAME
                             :TRUENAME (FILE-TRUENAME FILE)
                             (LMFS-FILE-PROPERTIES FILE))))))
            PATHNAMES)))

(COMPILE-FLAVOR-METHODS LOCAL-FILE-ACCESS)

(defvar *previous-local-host* si:local-host)

(defun add-local-pathname-host ()
  (setq *pathname-host-list* (delq *previous-local-host* *pathname-host-list*))
  (setq *previous-local-host* si:local-host)
  (fs:add-file-computer si:local-host))

(add-initialization "Add Local Pathname Host" '(fs:add-local-pathname-host) '(:warm :now))
(add-initialization "Add Local Pathname Host" '(fs:add-local-pathname-host) :site)

;;; A special host flavor for the local file system.  The local host used to be
;;; treated specially by the the :PATHNAME-HOST-P method, which compared SELF
;;; against SI:LOCAL-HOST to determine if "LM" was a valid host.  This causes
;;; problems when SI:LOCAL-HOST is not defined, as when changing the name of
;;; a machine and updating the site files.  DEFINE-LOCAL-FILE-SYSTEM-HOST defines
;;; a valid host, connected to the local file system, without concern for any
;;; of the site definitions.  KHS 1/9/85.
;;;
;;; This thing should be added only when there are site information problems;
;;; it causes lot of pain when things are otherwise OK.  We should advertise
;;; the function DEFINE-LOCAL-FILE-SYSTEM-HOST just in case the site information
;;; (or lack of it) requires it.

(DEFFLAVOR LOCAL-FILE-HOST
  ()
  (SI:HOST-LISPM-MIXIN FILE-HOST-LISPM-MIXIN SI:HOST))

(DEFMETHOD (LOCAL-FILE-HOST :AFTER :INIT) (&REST IGNORE)
  (SETQ APPROPRIATE-ACCESS-FLAVORS '(LOCAL-FILE-ACCESS)))

(DEFMETHOD (LOCAL-FILE-HOST :FILE-SYSTEM-TYPE) () :LISPM)

(DEFMETHOD (LOCAL-FILE-HOST :DETERMINE-ACCESS) ()
  (SETQ ACCESS (MAKE-INSTANCE 'LOCAL-FILE-ACCESS :HOST SELF)))

(DEFMETHOD (LOCAL-FILE-HOST :GET-ACCESS) ()
  (OR ACCESS (SEND SELF :DETERMINE-ACCESS)))

(DEFMETHOD (LOCAL-FILE-HOST :NAME-AS-FILE-COMPUTER) () "LOCAL")

(defun undefine-local-file-system-host ()
  (SETQ *PATHNAME-HOST-LIST*
        (DEL-IF #'(LAMBDA (X) (TYPEP X 'LOCAL-FILE-HOST)) *PATHNAME-HOST-LIST*)))

;;; Remove the kludge before disk-save time
(add-initialization "Remove LOCAL host" '(undefine-local-file-system-host)
                    '(:before-cold))

(DEFUN DEFINE-LOCAL-FILE-SYSTEM-HOST ()
  (undefine-local-file-system-host)
  (LET ((HOST-STRUCTURE (SI:MAKE-HOST-ALIST-ELEM :NAME "LM"
                                                 :NAME-LIST '("LM" "LOCAL")
                                                 :SYSTEM-TYPE-INTERNAL :LISPM
                                                 :MACHINE-TYPE-INTERNAL :LISPM
                                                 :SITE-NAME SITE-NAME
                                                 :ADDRESSES NIL)))
    (LET ((HOST-OBJECT (MAKE-INSTANCE 'FS:LOCAL-FILE-HOST :ALIST-ELEM HOST-STRUCTURE)))
      (SETF (SI:HOST-INSTANCE HOST-STRUCTURE) HOST-OBJECT)
      (SETQ *PATHNAME-HOST-LIST* (NCONC *PATHNAME-HOST-LIST* (NCONS HOST-OBJECT)))
      HOST-OBJECT)))
