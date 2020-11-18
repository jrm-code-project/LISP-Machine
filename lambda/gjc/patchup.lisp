#| -*- Mode:LISP; Package:SI; Base:10; FONTS: (CPTFONTB) -*-

CODE TO IMPLEMENT THE CREATION OF PATCH TAPES FOR CUSTOMER SUPPORT.

THIS WAS NEEDED TO GET SYSTEM PATCHES FOR THE FIRST LISP TCP-IP RELEASE.
5/20/85 10:22:20 -GEORGE CARRETTE.

SHORT FORM USAGE:
 (PATCHUP-UTILITY) ... PROMPTS FOR INFORMATION.

LONG USAGE:
 When you do a major release also do
  (DUMP-LOADED-PATCH-STATUS "RELEASE-LOG;RELEASE-2P0-PATCH-STATUS")

 Then, when you want to have some patches send out, load-patches
 into your machine, and say

  (DUMP-PATCH-UPDATE-INFO "RELEASE-LOG;RELEASE-2P0-PATCH-STATUS"
                          "RELEASE-LOG;RELEASE-2P0-PATCH-UPDATE-1")

  Which will create:
     RELEASE-2P0-PATCH-UPDATE-1.LISP, a list of files,
     RELEASE-2P0-PATCH-UPDATE-1.TEXT, a report of what was changed.

 (MAKE-PATCH-UPDATE-TAPE "RELEASE-LOG;RELEASE-2P0-PATCH-UPDATE-1") will make
 a tape.

Then do
 (DUMP-LOADED-PATCH-STATUS "RELEASE-LOG;RELEASE-2P0-PATCH-STATUS-1")

 (MAKE-PATCH-UPDATE-REPORT "RELEASE-LOG;RELEASE-2P0-PATCH-STATUS"
                           "RELEASE-LOG;RELEASE-2P0-PATCH-STATUS-1"
                           "RELEASE-LOG;RELEASE-2P0-PATCH-UPDATE-1")

Which will make a line-printer style REPORT in
          "RELEASE-LOG;RELEASE-2P0-PATCH-UPDATE-1.REPORT"

|#

(DEFVAR *PATCHUP-UTILITY-DIR* NIL)
(DEFVAR *LOG-FILE-PREPEND* NIL)

(defun make-utility-filename (name &optional type)
  (send *PATCHUP-UTILITY-DIR* :new-pathname
        :name (string-append *LOG-FILE-PREPEND*
                             "-" name)
        :type type
        :version :newest))


(DEFVAR *SYSTEMS-TO-CONSIDER* T)

(DEFUN PATCHUP-UTILITY (&OPTIONAL DBASE)
  (IF DBASE (LOAD DBASE :SET-DEFAULT-PATHNAME NIL))
  (COND ((NOT (y-or-n-p "~&Load Patches?")))
        ((LISTP *SYSTEMS-TO-CONSIDER*)
         (LOAD-PATCHES :SYSTEMS *SYSTEMS-TO-CONSIDER* :NOSELECTIVE))
        ('ELSE
         (load-patches :noselective)))
  (cond ((OR (NULL *PATCHUP-UTILITY-DIR*)
             (NULL *LOG-FILE-PREPEND*)
             (Y-OR-N-P "~&Do you want to change the release log directory?"))
         (SETUP-UTILITY-VARS))
        ('else
         (list-utility-directory)))
  (LET ((UVN (OR (PROMPT-AND-READ
                   :STRING-OR-NIL
                   "~&what update number (default TEST): ")
                 "TEST")))
    (let ((previous-status-file (make-utility-filename "STATUS"))
          (update-file (make-utility-filename (string-append "UPDATE-" UVN)))
          (status-file (make-utility-filename (string-append "STATUS-" UVN))))
      (format t "~&Update info from ~S to ~S" previous-status-file
              update-file)
      (dump-patch-update-info previous-status-file update-file)
      (format t "~&Status file ~S" status-file)
      (dump-loaded-patch-status status-file)
      (format t "~&Making report")
      (make-patch-update-report previous-status-file
                                status-file
                                update-file)
      (if (y-or-n-p "~&make the tape?")
          (make-patch-update-tape update-file)))))

(DEFUN SETUP-UTILITY-VARS ()
  (DO ((F))
      (NIL)
    (SETQ *PATCHUP-UTILITY-DIR* (PROMPT-AND-READ :PATHNAME
                                                 "~&Directory with patch logs> "))
    (SETQ *LOG-FILE-PREPEND* (PROMPT-AND-READ :STRING
                                              "~&filenames start with: "))
    (SETQ F (LIST-UTILITY-DIRECTORY))
    (IF (Y-OR-N-P "Is ~A correct?" (SEND F :STRING-FOR-PRINTING)) (RETURN))))


(DEFUN LIST-UTILITY-DIRECTORY (&AUX F)
  (SETQ F (SEND *PATCHUP-UTILITY-DIR*
                :NEW-PATHNAME :NAME (STRING-APPEND *LOG-FILE-PREPEND*
                                                   "*")
                :TYPE :WILD
                :VERSION :NEWEST))
  (LISTF F)
  F)


(DEFUN GET-LOADED-PATCH-STATUS ()
  (DELQ NIL
        (MAPCAR #'(LAMBDA (SYSTEM)
                    (MULTIPLE-VALUE-BIND (MAJOR MINOR STATUS)
                        (SI:GET-SYSTEM-VERSION SYSTEM)
                      (AND MAJOR MINOR STATUS
                           (LIST (SI:SYSTEM-NAME SYSTEM)
                                 MAJOR MINOR STATUS))))
                (LOADED-PATCHABLE-SYSTEMS))))


(DEFUN DUMP-LOADED-PATCH-STATUS (FILENAME)
  (WITH-OPEN-FILE (STREAM FILENAME :OUT)
    (LET ((BASE 10)
          (PACKAGE (FIND-PACKAGE "USER")))
      (FORMAT STREAM ";;-*-MODE:LISP;PACKAGE:USER;BASE:10-*-~%")
      (FORMAT STREAM ";; Loaded patch status for ~A dumped by ~A~%"
              (send si:local-host :name)
              si:user-id)
      (format stream ";; on ~A~%" (time:print-current-date nil))
      (format stream "(SET 'SI:*SAVED-LOADED-PATCH-STATUS* '(~%")
      (dolist (x (get-loaded-patch-status))
        (format stream " ~S~%" x))
      (format stream " ))~%"))))


(DEFUN LOADED-PATCHABLE-SYSTEMS ()
  (SUBSET #'(LAMBDA (SYSTEM)
              (AND (TYPEP SYSTEM 'SYSTEM)
                   (SYSTEM-PATCHABLE-P SYSTEM)
                   (OR (EQ *SYSTEMS-TO-CONSIDER* T)
                       (MEM #'(LAMBDA (ITEM ELEMENT)
                                (EQ ITEM (SI:FIND-SYSTEM-NAMED ELEMENT NIL T)))
                            SYSTEM *SYSTEMS-TO-CONSIDER*))))
          *SYSTEMS-LIST*))


(DEFVAR *SAVED-LOADED-PATCH-STATUS* NIL)

(DEFUN LOAD-SAVED-STATUS (FILENAME)
  (LET (*SAVED-LOADED-PATCH-STATUS*)
    (LOAD FILENAME)
    *SAVED-LOADED-PATCH-STATUS*))

(DEFUN PATCH-FILES-NEEDED (saved-status)
  (let ((sys-minor-alist (incremental-need-check-consistency
                           saved-status
                           (get-loaded-patch-status))))
    (MAPCAN #'PATCH-FILES-NEEDED-FOR SYS-MINOR-ALIST)))


(DEFUN PATCH-FILES-NEEDED-FOR (SYSTEM-DESC)
  (LET ((SYSTEM (CAR SYSTEM-DESC))
        (MAJOR (CADR SYSTEM-DESC))
        (AFTER (CADDR SYSTEM-DESC)))
    (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
           (VERSION (PATCH-VERSION PATCH-SYSTEM))       ;efficiency
           (LATEST (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM)))))
      (COND ((NULL PATCH-SYSTEM)
             (FORMAT T "~&No ~A system loaded~%" SYSTEM))
            ((NOT (EQUAL MAJOR VERSION))
             (FORMAT T "~&Inconsistent usage, ~A system version is ~D should be ~D~%"
                     SYSTEM VERSION MAJOR))
            ((> AFTER LATEST)
             (FORMAT T "~&Inconsistent usage, ~A system patches not all loaded.~%"
                     SYSTEM))
            ('ELSE
             (LET (LIST)
               (PUSH (PATCH-SYSTEM-PATHNAME SYSTEM :VERSION-DIRECTORY MAJOR)
                     LIST)
               (DOLIST (V (REVERSE (PATCH-VERSION-LIST PATCH-SYSTEM)))
                 (WHEN ( AFTER (VERSION-NUMBER V))
                   (LET ((P (PATCH-SYSTEM-PATHNAME SYSTEM :PATCH-FILE
                                                MAJOR (VERSION-NUMBER V))))
                     (PUSH (SEND P :NEW-TYPE :LISP) LIST)
                     (PUSH (SEND P :NEW-TYPE :QFASL) LIST))))
               (NREVERSE LIST)))))))


(defun incremental-need-check-consistency (from to)
  "Return an alist of (SYSTEM MAJOR MINOR-NEEDED-FROM) for those systems which
have new patches to go from FROM to TO. Prints warning messages about
new systems and missing systems"
  (LET ((ALIST))
    (DOLIST (OLD FROM)
      (LET ((NEW (ASS #'STRING-EQUAL (CAR OLD) TO)))
        (COND ((NOT NEW)
               (FORMAT T "~&Warning, system ~A no longer exists~%" (CAR OLD)))
              ((NOT (EQUAL (CADR OLD) (CADR NEW)))
               (FORMAT T "~&Warning, system ~A changed MAJOR version from ~D to ~D~%"
                       (CADR OLD) (CADR NEW)))
              ((GREATERP (CADDR NEW) (CADDR OLD))
               (PUSH (LIST (CAR NEW) (CADR NEW) (1+ (CADDR OLD))) ALIST)))))
    (DOLIST (NEW TO)
      (WHEN (NOT (ASS #'STRING-EQUAL (CAR NEW) FROM))
        (FORMAT T "~&Warning, system ~A is newly loaded~%" (CAR NEW))))
    ALIST))


(defvar *files-modified* nil)
(defvar *definitions-modified* nil)

(DEFUN DUMP-PATCH-UPDATE-INFO (PREVIOUS-STATUS-FILE TO-INFO-FILE)
  "Reads the previous-status-file, then creates TO-INFO-FILE.LISP,
and TO-INFO-FILE.TEXT."
  (LET* ((REPORT-FILE (SEND (FS:PARSE-PATHNAME TO-INFO-FILE) :NEW-TYPE :TEXT))
         (INFO-FILE (SEND REPORT-FILE :NEW-TYPE :LISP)))
    (LET ((FILES (PATCH-FILES-NEEDED (LOAD-SAVED-STATUS PREVIOUS-STATUS-FILE))))
      (WITH-OPEN-FILE (REPORT-STREAM REPORT-FILE :OUT)
        (WITH-OPEN-FILE (INFO-STREAM INFO-FILE :OUT)
          (LET ((BASE 10)
                (PACKAGE (FIND-PACKAGE "USER")))
            (FORMAT INFO-STREAM ";;-*-MODE:LISP;PACKAGE:USER;BASE:10-*-~%")
            (FORMAT INFO-STREAM ";; patch update file list dumped by ~A from ~A~%"
                    si:user-id
                    (send si:local-host :name))
            (format info-stream ";; on ~A~%" (time:print-current-date nil))
            (format info-stream "(SET 'SI:*SAVED-PATCH-UPDATE-FILES* '(~%")
            (format report-stream "Report of patches to update from ~A~%"
                    previous-status-file)
            (format report-stream "Created by ~A from ~A~%on ~A~2%"
                    si:user-id
                    (send si:local-host :name)
                    (time:print-current-date nil))
            (let  ((*files-modified* nil)
                   (*definitions-modified* nil))
              (DOLIST (FILE FILES)
                (FORMAT INFO-STREAM " ~S~%" (REPORT-ON-PATCH-FILE FILE REPORT-STREAM)))
              (format report-stream
                      "~3%A list of all files modified:~%~{ ~A~%~}~%"
                      *files-modified*)
              (format report-stream
                      "~3%A list of all definitions modified:~%~{ ~A~%~}~%"
                      *definitions-modified*))
            (FORMAT INFO-STREAM " ))~%")))))))

(DEFUN REPORT-ON-PATCH-FILE (FILENAME &OPTIONAL (STREAM STANDARD-OUTPUT))
  (cond ((EQ :QFASL (SEND (FS:PARSE-PATHNAME FILENAME) :CANONICAL-TYPE))
         (LET ((TN (PROBE-FILE FILENAME)))
           (FORMAT STREAM "~&Object file for patches is ~S~%" TN)
           TN))
        ('ELSE
         (with-open-file (R filename)
           (LET ((TN (SEND R :TRUENAME)))
             (COND ((EQ :LISP (SEND TN :CANONICAL-TYPE))
                    (FORMAT STREAM "~&~%Report on source containing patches: ~S~%"
                            TN))
                   ('ELSE
                    (FORMAT STREAM "~&~%Report on directory of patches: ~S~%"
                            TN)))
             (condition-case ()
                 (do ((string (make-string 1000))
                      (n)
                      (beginingp t))
                     ((null (setq n (send r :string-line-in t string))))
                   (cond ((or (string-begins-with "; From file" string n)
                              (string-begins-with "(def" string n))
                          (send stream :line-out string 0 n)
                          (cond ((string-begins-with "; From file" string n)
                                 (let ((s (substring string 12 n)))
                                   (or (mem #'string-equal s *files-modified*)
                                       (push s *files-modified*))))
                                ('else
                                 (push (substring string 1 n)
                                       *definitions-modified*)))
                          (setq beginingp nil))
                         ((and (string-begins-with ";" string n) beginingp)
                          (send stream :line-out string 0 n))
                         ('else
                          (setq beginingp nil))))
               (sys:end-of-file
                tn)))))))

(defun string-begins-with (item string n)
  (and (>= n (string-length item))
       (string-equal item string :end2 (string-length item))))

(defvar *SAVED-PATCH-UPDATE-FILES* nil)

(DEFUN MAKE-PATCH-UPDATE-TAPE (INFO-FILENAME)
  (let ((*SAVED-PATCH-UPDATE-FILES*))
    (load INFO-FILENAME)
    (format t "~&Rewinding Tape~%")
    (fs:mt-rewind)
    (format t "~&Writing ~D files." (length *SAVED-PATCH-UPDATE-FILES*))
    (dolist (file *SAVED-PATCH-UPDATE-FILES*)
      (fs:fs-copy-file file "MT:"))
    (format t "~&Writing EOF marker~%")
    (fs:mt-write-eof)
    (format t "~&Rewinding Tape~%")
    (fs:mt-rewind)
    (format t "~&Done~%")))

(defvar *patch-report-from* "GEORGE CARRETTE")
(defvar *patch-report-to* "GEORGE COLPITTS")
(defvar *patch-report-about* "Release 2.0 Lisp Software Update Tape")

(DEFUN MAKE-PATCH-UPDATE-REPORT (BEFORE-STATUS AFTER-STATUS UPDATE-FILE)
  "Makes a printable written report of the patch activities"
  (setq update-file (FS:PARSE-PATHNAME UPDATE-FILE))
  (WITH-OPEN-FILE (STREAM (SEND update-file :NEW-TYPE "REPORT")
                          :OUT)
    (LET ((LPR (MAKE-LPR-STREAM STREAM)))
      (FORMAT LPR "~2%TO:   ~A~
                 ~%FROM: ~A~
                 ~%RE:   ~A~
                 ~%DATE: ~A~%"
              *patch-report-from*
              *patch-report-to*
              *patch-report-about*
              (time:print-current-date nil))
      (format lpr "This tape updates lisp software from the following versions:~%")
      (insert-file-into-report lpr before-status)
      (format lpr "~%to the following versions:~%")
      (insert-file-into-report lpr after-status)
      (format lpr "~%The tape contains the following files:~%")
      (insert-file-into-report lpr (send update-file :new-type :LISP))
      (format lpr "~%The following is a detailed report each patch file,~
                   ~%with a note of every FILE, FUNCTION, or VARIABLE modified:~%")
      (insert-file-into-report lpr (send update-file :new-type :text)))
    (send stream :truename)))

(defun insert-file-into-report (stream filename)
  (with-open-file (x filename)
    (stream-copy-until-eof x stream)))

(DEFUN MAKE-LPR-STREAM (OUTPUT &OPTIONAL &KEY (MARGIN 10) (WIDTH 69) (LENGTH 50)
                        &AUX STREAM (LINE-COUNT 0) (CHAR-COUNT 0) (PAGE-COUNT 1))
  "Make a stream for creating files suitable for simple line-printers"
  (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYO
                       (COND ((< ARG1 #o40))
                             ((= ARG1 #\RETURN)
                              (COND ((= LINE-COUNT LENGTH)
                                     (SEND OUTPUT :TYO #\FORM)
                                     (INCF PAGE-COUNT)
                                     (SETQ LINE-COUNT 0)
                                     (SETQ CHAR-COUNT 0)
                                     (DOTIMES (J (FLOOR WIDTH 2))
                                       (SEND OUTPUT :TYO #\SPACE))
                                     (FORMAT STREAM "Page ~D~%~%" page-count))
                                    ('ELSE
                                     (INCF LINE-COUNT)
                                     (SETQ CHAR-COUNT 0)
                                     (SEND OUTPUT :TYO #\RETURN)
                                     (DOTIMES (J MARGIN)
                                       (SEND OUTPUT :TYO #\SPACE)))))
                             ((GRAPHIC-CHAR-P ARG1)
                              (COND ((= CHAR-COUNT WIDTH)
                                     (SEND STREAM :TYO #\RETURN)
                                     (SEND STREAM :TYO ARG1))
                                    ('ELSE
                                     (INCF CHAR-COUNT)
                                     (SEND OUTPUT :TYO ARG1))))))
                     (T
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))
