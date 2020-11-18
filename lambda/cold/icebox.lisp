;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

;;; This file contains random hacks to help recompile and build new lispm
;;; worlds.  Feel free to add to it.

;;; DLA's hack to find the creation dates of all installed QFASL files.

(DEFVAR QFASL-SOURCE-FILE-PLISTS-ALIST NIL)
(DEFUN QFASL-SOURCE-FILE-PLISTS (&OPTIONAL (SYSTEM "System"))
  (AND (OR (NOT (ASS #'EQUALP SYSTEM QFASL-SOURCE-FILE-PLISTS-ALIST))
           (Y-OR-N-P "Recompute QFASL source files? "))
       (LET ((FILES (MAPCAR #'(LAMBDA (X)
                                (SEND X :NEW-PATHNAME :TYPE :QFASL :VERSION :NEWEST))
                            (SYSTEM-SOURCE-FILES SYSTEM ':ALL))))
         (PUSH (CONS SYSTEM
                     (SORT (DEL #'(LAMBDA (IGNORE X) (NULL (GET X ':CREATION-DATE)))
                                NIL
                                (FS:MULTIPLE-FILE-PROPERTY-LISTS T FILES))
                           #'(LAMBDA (X Y)
                               (< (GET X ':CREATION-DATE)
                                  (GET Y ':CREATION-DATE)))))
               QFASL-SOURCE-FILE-PLISTS-ALIST)))
  (CDR (ASS #'EQUALP SYSTEM QFASL-SOURCE-FILE-PLISTS-ALIST)))

(DEFUN LIST-QFASL-SOURCE-FILES (FILENAME &OPTIONAL (SYSTEM "System"))
  (LET ((FILES (QFASL-SOURCE-FILE-PLISTS SYSTEM)))
    (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :OUTPUT :CHARACTERS T)
      (DOLIST (P FILES)
        (FORMAT STREAM "~30A" (CAR P))
        (TIME:PRINT-UNIVERSAL-TIME (GET P ':CREATION-DATE) STREAM)
        (SEND STREAM :TYO #/NEWLINE)))))

(DEFUN RECOMPILE-FILES-AFTER-DATE (AFTER-DATE &OPTIONAL (SYSTEM "System")
                                   &AUX RECOM-FILES TEM)
  (SETQ AFTER-DATE (TIME:PARSE-UNIVERSAL-TIME AFTER-DATE 0 NIL NIL))
  (DOLIST (P (QFASL-SOURCE-FILE-PLISTS SYSTEM))
    (COND ((AND (> (GET P ':CREATION-DATE) AFTER-DATE)
                (SETQ TEM (SEND (SEND (CAR P) :GENERIC-PATHNAME)
                                :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)))
           (PUSH (SEND (MERGE-PATHNAMES TEM) :NEW-VERSION :NEWEST)
                 RECOM-FILES))))
  (DOLIST (F (NREVERSE RECOM-FILES))
    (COND ((PROBEF F)
           (FORMAT T "~%Recompiling ~A ..." F)
           (COMPILE-FILE F))
          (T (FORMAT T "~%~A does not exist.")))))

(DEFUN RECOMPILE-FILES-BEFORE-DATE (BEFORE-DATE &OPTIONAL (SYSTEM "System")
                                    &AUX RECOM-FILES TEM)
  (SETQ BEFORE-DATE (TIME:PARSE-UNIVERSAL-TIME BEFORE-DATE 0 NIL NIL))
  (DOLIST (P (QFASL-SOURCE-FILE-PLISTS SYSTEM))
    (COND ((AND (> BEFORE-DATE (GET P ':CREATION-DATE))
                (SETQ TEM (SEND (SEND (CAR P) :GENERIC-PATHNAME)
                                :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)))
           (PUSH (SEND (MERGE-PATHNAMES TEM) :NEW-VERSION :NEWEST)
                 RECOM-FILES))))
  (DOLIST (F (NREVERSE RECOM-FILES))
    (COND ((PROBEF F)
           (FORMAT T "~%Recompiling ~A ..." F)
           (COMPILE-FILE F))
          (T (FORMAT T "~%~A does not exist.")))))


;;; DLA's hack to search through the sources of QFASL files compiled after a certain
;;; date for bad strings.  Useful when a macro has expanded wrong, or the compiler
;;; broke in certain cases.

(DEFUN SOURCE-SCAN-AFTER-DATE (SUBSTRING AFTER-DATE &OPTIONAL (SYSTEM "System")
                                                              (FUNCTION 'STRING-SEARCH)
                                                    &AUX BAD-FILES FILE)
  (SETQ AFTER-DATE (TIME:PARSE-UNIVERSAL-TIME AFTER-DATE))
  (DOLIST (F (QFASL-SOURCE-FILE-PLISTS SYSTEM))
    (COND ((> (GET F ':CREATION-DATE) AFTER-DATE)
           (SETQ FILE (SEND (CAR F) :NEW-PATHNAME :TYPE :LISP :VERSION :NEWEST))
           (SEND *STANDARD-OUTPUT* :CLEAR-WINDOW)
           (FORMAT T "File ~A ..." FILE)
           (AND (SCAN-SOURCE-FILE FILE SUBSTRING FUNCTION)
                (PUSH FILE BAD-FILES)))))
  BAD-FILES)

(DEFUN SOURCE-SCAN-MULTIPLE-ALL (STRINGS &AUX ANS)
  (DOLIST (SYSTEM *SYSTEMS-LIST*)
    (SETQ ANS (NCONC ANS (SOURCE-SCAN-MULTIPLE STRINGS SYSTEM))))
  ANS)


(DEFUN SOURCE-SCAN-MULTIPLE (STRINGS &OPTIONAL (SYSTEM "System"))
  (SOURCE-SCAN (LIST STRINGS NIL) SYSTEM 'ZWEI:FSM-STRING-SEARCH))

(DEFUN SOURCE-SCAN (SUBSTRING &OPTIONAL (SYSTEM "System") (FUNCTION 'STRING-SEARCH))
  (LOOP FOR FILE IN (SYSTEM-SOURCE-FILES SYSTEM)
        DO (FORMAT T "~:|File ~A ..." FILE)
        WHEN (SCAN-SOURCE-FILE FILE SUBSTRING FUNCTION)
        COLLECT FILE))

(DEFUN SCAN-SOURCE-FILE (FILE SUBSTRING FUNCTION)
  (LET* ((BUFFER (CIRCULAR-LIST NIL NIL NIL NIL NIL NIL NIL))
         (LINE-IN-POINT (CDDDDR BUFFER)))
    (WITH-OPEN-FILE (STREAM FILE :CHARACTERS T :DIRECTION :INPUT)
      (DO () (NIL)
        (MULTIPLE-VALUE-BIND (LINE EOF)
            (SEND STREAM :LINE-IN T)
          (AND EOF (OR (NULL LINE)
                       (EQUAL LINE ""))
               (SETQ LINE ':EOF))
          (SETF (CAR LINE-IN-POINT) LINE)
          (SETQ BUFFER (CDR BUFFER)
                LINE-IN-POINT (CDR LINE-IN-POINT))
          (COND ((NULL (CAR BUFFER)))           ;Beginning of file
                ((EQ (CAR BUFFER) ':EOF)
                 (RETURN NIL))
                ((FUNCALL FUNCTION SUBSTRING (CAR BUFFER))
                 (SEND *STANDARD-OUTPUT* :SET-CURSORPOS 0 3 :CHARACTER)
                 (SEND *STANDARD-OUTPUT* :CLEAR-REST-OF-WINDOW)
                 (DO ((L (CDR LINE-IN-POINT) (CDR L)))
                     ((EQ L LINE-IN-POINT))
                   (SEND *STANDARD-OUTPUT*
                         :LINE-OUT (COND ((NULL (CAR L)) "[Beginning of file]")
                                         ((SYMBOLP (CAR L)) "[End of file]")
                                         (T (CAR L)))))
                 (FORMAT T "~% Is this OK? ")
                 (OR (Y-OR-N-P)
                     (RETURN T)))))))))

(DEFUN COMPILED-AFTER-DATE (AFTER-DATE &OPTIONAL (SYSTEM "System"))
  "Find list of files compiled after time AFTER-DATE"
  (SETQ AFTER-DATE (TIME:PARSE-UNIVERSAL-TIME AFTER-DATE))
  (LET ((LOSERS ()))
    (DOLIST (F (QFASL-SOURCE-FILE-PLISTS SYSTEM))
      (WHEN (> (GET F ':CREATION-DATE) AFTER-DATE)
        (LET ((FILE (SEND (CAR F) :NEW-PATHNAME :TYPE :LISP :VERSION :NEWEST)))
          (FORMAT T "~&~30A ~\time\" FILE (GET F :CREATION-DATE))
          (PUSH FILE LOSERS))))
    LOSERS))

(DEFUN COMPILED-BEFORE-DATE (BEFORE-DATE &OPTIONAL (SYSTEM "System"))
  "Find list of files compiled before time BEFORE-DATE"
  (SETQ BEFORE-DATE (TIME:PARSE-UNIVERSAL-TIME BEFORE-DATE))
  (LET ((LOSERS ()))
    (DOLIST (F (QFASL-SOURCE-FILE-PLISTS SYSTEM))
      (WHEN (< (GET F ':CREATION-DATE) BEFORE-DATE)
        (LET ((FILE (SEND (CAR F) :NEW-PATHNAME :TYPE :LISP :VERSION :NEWEST)))
          (FORMAT T "~&~30A ~\time\" FILE (GET F :CREATION-DATE))
          (PUSH FILE LOSERS))))
    LOSERS))

(defun assoc-symbol-safe (symbol list)
  (dolist (entry list)
    (typecase entry
      (symbol
       (when (eq symbol entry)
         (return symbol)))
      (cons
       (when (eq symbol (car entry))
         (return entry)))
      (t (error "Strange entry ~S in alist." entry)))))

(defun map-system-source-pathnames (function systems &key (types *source-file-types*))
  "Map FUNCTION (taking one argument, a pathname) over the source files of SYSTEMS.
The function is called only once for each source file.
A list of the source files is returned."
  (unless (typep systems 'list)
    (setq systems (list systems)))
  (let ((source-files-seen '()))
    (dolist (system systems)
      (dolist (file (system-source-files system types))
        (unless (lisp:member file source-files-seen)
          (funcall function file)
          (setq source-files-seen (nconc source-files-seen (ncons file))))))
    source-files-seen))

(defun file-uses-macro-p (file macro)
  "Returns either a number (a hash code), T (used, but no hash code), or NIL.
FILE must be a generic pathname; MACRO is a symbol."
  (let ((result (assoc-symbol-safe macro (get file :macros-expanded))))
    (etypecase result
      (null nil)
      (symbol t) ; just a symbol in the list of macros used
      (cons (second result)))))

(defun file-creation-date (file)
  (declare (values creation-date truename))
  (with-open-file (probe file :direction :probe)
    (values (send probe :creation-date) (truename probe))))

(defun newest-lisp-file (file)
  (send file :new-pathname :type :lisp :version :newest))

(defun compile-file-if-needed (file)
  (multiple-value-bind (creation-date truename) (file-creation-date file)
    (if (< creation-date
           (file-creation-date (send file :new-pathname :type :qfasl :version :newest)))
        (format t "~&Not compiling ~A~%" truename)
      (format t "~&Compiling ~A ..." truename)
      (compile-file file :set-default-pathname nil)
      (format t "done."))))

(defun list-older-file-users-of-macro (macro &key (systems '("System")) (use-hash-p t))
  "Return a list of LISP files in SYSTEMS that could be using obsolete versions of MACRO.
If USE-HASH-P is T, the criterion is that file has an up-to-date version of the macro
if hash code in the loaded version is different from the hash cade of the macro in core.
If USE-HASH-P is NIL, the criterion is that the source file using the macro is newer than the
source file defining the macro.

The hash method is faster because it only does two file accesses at the most; however, it
doesn't catch source files that use the macro being newer than their QFASLS.  Note that both
methods assume that the macro usage information in-core reflects the usage of the source files
``out there.''"
  (let* ((files '())
         (our-macro-id (cadr (lisp:assoc :expr-sxhash
                                         (debugging-info (macro-function macro)))))
         (generic-source-file (get macro :source-file-name))
         (loaded-file (get generic-source-file :qfasl-source-file-unique-id))
         (latest-file (truename (newest-lisp-file generic-source-file)))
         (loaded-file-creation-date (file-creation-date loaded-file))
         (change-date (if (eq loaded-file latest-file)
                          loaded-file-creation-date
                        (file-creation-date latest-file))))
    ;; Don't use the hash hack if this macro doesn't have a hash code
    (setq use-hash-p (and use-hash-p our-macro-id))
    (when (> change-date loaded-file-creation-date)
      (warn "Source file of ~S, ~A,
  (installed ~A of ~\TIME\,
      newest ~A of ~\TIME\)
  needs to be compiled and loaded."
            macro generic-source-file
            loaded-file loaded-file-creation-date latest-file change-date)
      (when use-hash-p
        (warn "Unless the macro definition of ~S has not changed in the file,
the results may be inaccurate, not catching enough changed files."
              macro)))
    (map-system-source-pathnames #'(lambda (f)
                                     (let ((gp (send f :generic-pathname)))
                                       (let ((macro-id (file-uses-macro-p gp macro)))
                                         (when macro-id
                                           (when
                                             (if use-hash-p
                                                 ;; Did this file use the same version of the macro
                                                 ;; as the one we have in core ?  T as in id means
                                                 ;; we don't know.
                                                 (not (eql macro-id our-macro-id))
                                               ;; The change date is newer than the creation date
                                               ;; of the latest source version.
                                               (> change-date
                                                  (file-creation-date (newest-lisp-file gp))))
                                             (push f files))))))
                                 systems
                                 :types '(:lisp))
    files))

(defun compile-older-file-users-of-macro (macro &rest keyword-args)
  "Compile any older files that use MACRO.
See LIST-OLDER-FILE-USERS-OF-MACRO for a discussion of the keyword arguments."
  (mapc #'compile-file-if-needed (apply #'list-older-file-users-of-macro macro keyword-args)))

;;; RG's Source compare hack.

;start dribble file if you want to save results.
(DEFUN SOURCE-COMPARE-SYSTEM-UPDATES (&OPTIONAL SYSTEMS)
  (COND ((NULL SYSTEMS) (SETQ SYSTEMS *SYSTEMS-LIST*))
        ((ATOM SYSTEMS) (SETQ SYSTEMS (LIST SYSTEMS))))
  (LET (FILES)
    (DOLIST (SYSTEM SYSTEMS)
      (SETQ FILES (APPEND FILES (SYSTEM-SOURCE-FILES SYSTEM))))
    (SETQ FILES (ELIMINATE-DUPLICATES FILES))
    (DOLIST (FILE FILES)
      (SOURCE-COMPARE-FILE-UPDATES FILE))))

(DEFUN SOURCE-COMPARE-FILE-UPDATES (FILE &AUX CURRENT-PROBE INSTALLED-PROBE)
  (UNWIND-PROTECT
    (PROG (CURRENT-VERSION INSTALLED-VERSION FILE-TO-COMPARE)
        (SETQ CURRENT-PROBE (OPEN FILE :DIRECTION :PROBE))
        (COND ((ERRORP CURRENT-PROBE)
               (FORMAT T "~% No source for ~A  (~A)" FILE CURRENT-PROBE)
               (RETURN NIL))
              (T (SETQ CURRENT-VERSION (SEND CURRENT-PROBE :TRUENAME))))
        (SETQ INSTALLED-VERSION (SEND (SEND FILE :GENERIC-PATHNAME)
                                      :GET ':QFASL-SOURCE-FILE-UNIQUE-ID))
        (COND ((NULL INSTALLED-VERSION)
               (FORMAT T "~%Installed version of ~A unrecorded, trying oldest" FILE)
               (GO OLD))
              ((STRINGP INSTALLED-VERSION)
               (SETQ INSTALLED-VERSION (FS:PARSE-PATHNAME INSTALLED-VERSION))))
        ;Get installed version from current sys host, not the one it was compiled on
        (SETQ INSTALLED-VERSION (SEND CURRENT-VERSION :NEW-VERSION
                                      (SEND INSTALLED-VERSION :VERSION)))
        (SETQ INSTALLED-PROBE (OPEN INSTALLED-VERSION ':PROBE))
        (COND ((ERRORP INSTALLED-PROBE)
               (FORMAT T "~%Installed version ~A not available (~A), trying oldest"
                         INSTALLED-VERSION INSTALLED-PROBE)
               (GO OLD))
              (T
               (SETQ FILE-TO-COMPARE INSTALLED-VERSION)
               (GO COMP)))
  OLD   (SETQ FILE-TO-COMPARE (SEND CURRENT-VERSION :NEW-VERSION :OLDEST))
        (SETQ INSTALLED-PROBE (OPEN FILE-TO-COMPARE ':PROBE))
        (COND ((ERRORP INSTALLED-PROBE)
               (FORMAT T "~% Can't get oldest version (~A), giving up." INSTALLED-PROBE)
               (RETURN NIL)))
        (SETQ FILE-TO-COMPARE (SEND INSTALLED-PROBE :TRUENAME))
  COMP  (COND ((SAME-FILE-P FILE-TO-COMPARE CURRENT-VERSION)
               (COND ((AND INSTALLED-VERSION
                           (SAME-FILE-P INSTALLED-VERSION CURRENT-VERSION))
                      (FORMAT T "~%No change to file ~A" FILE))
                     (T (FORMAT T "~%Only one version to compare ~A" FILE))))
              (T (FORMAT T "~%Comparing ~A~% and ~A~%"
                           (ZWEI::DESCRIBE-FILE-ID (SEND INSTALLED-PROBE :INFO))
                           (ZWEI::DESCRIBE-FILE-ID (SEND CURRENT-PROBE :INFO)))
                 (condition-case (condition-instance)
                     (SRCCOM:SOURCE-COMPARE FILE-TO-COMPARE CURRENT-VERSION)
                   (si:abort (format t "~&Signaled si:abort: ~s"
                                     (send condition-instance :report nil)))))))
    (AND INSTALLED-PROBE (NOT (ERRORP INSTALLED-PROBE)) (CLOSE INSTALLED-PROBE))
    (AND CURRENT-PROBE (NOT (ERRORP CURRENT-PROBE)) (CLOSE CURRENT-PROBE))))

(DEFUN SAME-FILE-P (F1 F2)
  (OR (EQ F1 F2)
      (AND (EQUALP (SEND F1 :DIRECTORY) (SEND F2 :DIRECTORY))
           (EQUALP (SEND F1 :NAME) (SEND F2 :NAME))
           (EQUALP (SEND F1 :VERSION) (SEND F2 :VERSION))
           (OR (EQUALP (SEND F1 :TYPE) (SEND F2 :TYPE))
               (AND (MEMQ (SEND F1 :TYPE) '(:UNSPECIFIC NIL))
                    (MEMQ (SEND F2 :TYPE) '(:UNSPECIFIC NIL)))))))

;;; DLA's hack to purge source files of a system.

(DEFUN UNIQUE-IDS-OF-SYSTEM (&OPTIONAL (SYSTEM "System"))
  (LET ((FILES (SYSTEM-SOURCE-FILES SYSTEM)))
    (DO ((F FILES (CDR F)))
        ((NULL F))
      (SETF (CAR F)
            (SEND (SEND (CAR F) :GENERIC-PATHNAME)
                  :GET ':QFASL-SOURCE-FILE-UNIQUE-ID))
      (AND (STRINGP (CAR F))
           (SETF (CAR F) (FS:MERGE-PATHNAME-DEFAULTS (CAR F)))))
    (DELQ NIL FILES)))

;; This function maps through the source files of a system, and deletes all files
;; which are not either installed or newest.
(DEFUN SYSTEM-SOURCE-FILE-PURGE (&OPTIONAL (SYSTEM "System") (QUERY-P T) (KEEP 1)
                                 (IGNORE-DONT-REAP T) &AUX DIR DIR-LIST)
  (LET ((FILES (UNIQUE-IDS-OF-SYSTEM SYSTEM)))
    (DO ((DIR-DELETE-P NIL NIL)) ((NULL FILES))
      (SETQ DIR (FS:PATHNAME-DIRECTORY (CAR FILES))
            DIR-LIST (FS:DIRECTORY-LIST (SEND (CAR FILES) :NEW-PATHNAME
                                              :NAME :WILD :TYPE :WILD :VERSION :WILD)))
      ;; This puts the directory in descending order.
      (SETQ DIR-LIST (SORT (CDR DIR-LIST)
                           #'(LAMBDA (X Y) (NOT (< (GET X ':CREATION-DATE)
                                                   (GET Y ':CREATION-DATE))))))
      (DOLIST (FILE FILES)
        (COND ((EQUALP (FS:PATHNAME-DIRECTORY FILE) DIR)
               (DO ((D DIR-LIST (CDR D))
                    (NEWEST KEEP)
                    (NAME (FS:PATHNAME-NAME FILE))
                    (TYPE (FS:PATHNAME-TYPE FILE))
                    (VERSION (FS:PATHNAME-VERSION FILE))
                    (RELEVANT-FILES NIL)
                    (DELETE-P NIL))
                   ((NULL D)
                    (COND (DELETE-P
                           (FORMAT T "~%~%")
                           (COND ((NOT DIR-DELETE-P)
                                  (FORMAT T "~A:~%" DIR)
                                  (SETQ DIR-DELETE-P T)))
                           (MAPC #'ZWEI::DEFAULT-LIST-ONE-FILE RELEVANT-FILES)
                           (COND ((OR (NOT QUERY-P)
                                      (Y-OR-N-P "Delete them? "))
                                  (DOLIST (F RELEVANT-FILES)
                                    (AND (GET F ':DELETED)
                                         (DELETEF (CAR F)))))))))
                 (AND (EQUALP NAME (FS:PATHNAME-NAME (CAAR D)))
                      (EQUALP TYPE (FS:PATHNAME-TYPE (CAAR D)))
                      (COND ((OR ;Don't delete if you still have to keep some files
                                 (PLUSP NEWEST)
                                 ;Or if this is an installed version
                                 (EQUALP VERSION (FS:PATHNAME-VERSION (CAAR D)))
                                 ;Or if this has a dont-reap bit, and we're checking
                                 (NOT (OR IGNORE-DONT-REAP (NOT (GET (CAR D) ':DONT-REAP)))))
                             (PUSH (CAR D) RELEVANT-FILES)
                             (SETQ NEWEST (1- NEWEST)))
                            (T (PUTPROP (CAR D) T ':DELETED)
                               (SETQ DELETE-P T)
                               (PUSH (CAR D) RELEVANT-FILES))))))))
      (SETQ FILES (DEL #'(LAMBDA (DIR FILE) (EQUALP DIR (FS:PATHNAME-DIRECTORY FILE)))
                       DIR FILES)))))

;; This function maps through the source and binary files of a system, and deletes all files
;; which are not either installed or newest.
(DEFUN SYSTEM-FILE-PURGE (&OPTIONAL (SYSTEM "System") (QUERY-P T) (KEEP 1)
                          (IGNORE-DONT-REAP T) &AUX DIR DIR-LIST)
  (LET ((FILES (UNIQUE-IDS-OF-SYSTEM SYSTEM)))
    (DO ((DIR-DELETE-P NIL NIL)) ((NULL FILES))
      (SETQ DIR (FS:PATHNAME-DIRECTORY (CAR FILES))
            DIR-LIST (FS:DIRECTORY-LIST (SEND (CAR FILES) :NEW-PATHNAME
                                              :NAME :WILD :TYPE :WILD :VERSION :WILD)))
      ;; This puts the directory in descending order.
      (SETQ DIR-LIST (stable-sort (SORT (CDR DIR-LIST)
                                        #'(LAMBDA (X Y)
                                            (>= (GET X :CREATION-DATE)
                                                (GET Y :CREATION-DATE))))
                                  #'(lambda (x y)
                                      (string< (send (car x) :type)
                                               (send (car y) :type)))
                                  ))
      (DOLIST (FILE FILES)
        (WHEN (EQUALP (FS:PATHNAME-DIRECTORY FILE) DIR)
          (DO ((D DIR-LIST (CDR D))
               (NAME (FS:PATHNAME-NAME FILE))
               (VERSION (FS:PATHNAME-VERSION FILE))
               (this-type nil)
               (last-type nil)
               (NEWEST nil)
               (RELEVANT-FILES NIL)
               (DELETE-P NIL))
              ((NULL D)
               (COND (DELETE-P
                      (setq relevant-files (nreverse relevant-files))
                      (FORMAT T "~%~%")
                      (COND ((NOT DIR-DELETE-P)
                             (FORMAT T "~A:~%" DIR)
                             (SETQ DIR-DELETE-P T)))
                      (MAPC #'ZWEI::DEFAULT-LIST-ONE-FILE RELEVANT-FILES)
                      (COND ((OR (NOT QUERY-P)
                                 (Y-OR-N-P "Delete them? "))
                             (DOLIST (F RELEVANT-FILES)
                               (AND (GET F ':DELETED)
                                    (DELETEF (CAR F)))))))))
            (setq this-type (FS:PATHNAME-TYPE (CAAR D)))
            (unless (equalp last-type this-type)
              (setq last-type this-type)
              (setq newest keep))
            (WHEN (EQUALP NAME (FS:PATHNAME-NAME (CAAR D)))
              (COND ((OR ;;Don't delete if you still have to keep some files
                       (PLUSP NEWEST)
                       ;;Or if this is an installed version
                       (EQUALP VERSION (FS:PATHNAME-VERSION (CAAR D)))
                       ;;Or if this has a dont-delete bit
                       (GET (CAR D) :DONT-DELETE)
                       ;;Or if this has a dont-reap bit, and we're checking
                       (NOT (OR IGNORE-DONT-REAP (NOT (GET (CAR D) ':DONT-REAP)))))
                     (SETQ NEWEST (1- NEWEST)))
                    (T
                     (PUTPROP (CAR D) T ':DELETED)
                     (SETQ DELETE-P T)))
              (PUSH (CAR D) RELEVANT-FILES)))))
      (SETQ FILES (DEL #'(LAMBDA (DIR FILE) (EQUALP DIR (FS:PATHNAME-DIRECTORY FILE)))
                       DIR FILES)))))

(DEFUN SET-SYSTEM-SOURCE-FILE-REAP (&OPTIONAL (SYSTEM "System") (DONT-REAP-P T))
  (LOOP FOR FILE IN (UNIQUE-IDS-OF-SYSTEM SYSTEM)
        AS RESPONSE = (FS:CHANGE-FILE-PROPERTIES FILE NIL ':DONT-REAP DONT-REAP-P)
        WHEN (ERRORP RESPONSE)
        DO (FORMAT T "~%~A -- ~A" FILE RESPONSE)))

(defun set-system-source-file-dont-delete (&optional (system "System") (dont-delete-p t))
  (loop for file in (unique-ids-of-system system)
        as response = (fs:change-file-properties file nil ':dont-delete dont-delete-p)
        when (errorp response)
        do (format t "~%~A -- ~A" file response)))

(DEFUN LIST-MISSING-INSTALLED-VERSIONS (&OPTIONAL (SYSTEMS *SYSTEMS-LIST*))
  (OR (CONSP SYSTEMS) (SETQ SYSTEMS (LIST SYSTEMS)))
  (SEND *STANDARD-OUTPUT* :FRESH-LINE)
  (LOOP FOR (FILE . PLIST)
         IN (FS:MULTIPLE-FILE-PROPERTY-LISTS NIL
              (LOOP FOR FILE IN (ELIMINATE-DUPLICATES
                                  (LOOP FOR SYSTEM IN SYSTEMS
                                        APPEND (SYSTEM-SOURCE-FILES SYSTEM)))
                    AS SOURCE = (SEND (SEND FILE :GENERIC-PATHNAME)
                                      :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)
                    AS VERSION = (AND SOURCE (SEND (FS:PARSE-PATHNAME SOURCE) :VERSION))
                    UNLESS (NULL VERSION)
                      COLLECT (SEND (SEND FILE :NEW-VERSION VERSION)
                                    :TRANSLATED-PATHNAME)))
       WHEN (NULL PLIST)
         DO (PRINC FILE) (TERPRI)))

(DEFUN LIST-COMPILATION-INFO (&OPTIONAL (S STANDARD-OUTPUT) &AUX LIST TEM)
  (MAPHASH #'(LAMBDA (IGNORE PATH)
               (AND (SEND PATH :GET ':QFASL-SOURCE-FILE-UNIQUE-ID)
                    (PUSH PATH LIST)))
           FS:*PATHNAME-HASH-TABLE*)
  (SETQ LIST (SORT (COPY-LIST LIST)
                   #'(LAMBDA (X Y)
                       (STRING-LESSP
                         (SEND X ':STRING-FOR-PRINTING)
                         (SEND Y ':STRING-FOR-PRINTING)))))
  (DOLIST (PATHNAME LIST)
    (FORMAT S "~%~A" (SEND PATHNAME :GET ':QFASL-SOURCE-FILE-UNIQUE-ID))
    (WHEN (SETQ TEM (SEND PATHNAME :GET ':COMPILE-DATA))
      (FORMAT S "~30T~D.~D~38T~A~46T~A~69T"
              (FOURTH TEM) (FIFTH TEM)
              (FIRST TEM) (SECOND TEM))
      (TIME:PRINT-UNIVERSAL-TIME (THIRD TEM)))))

(DEFUN RECOMPILE-SOURCE-FILES (SOURCE-FILE-LIST)
  (DOLIST (FILE SOURCE-FILE-LIST)
    (SETQ FILE (SEND FILE :NEW-PATHNAME :TYPE :LISP :VERSION :NEWEST))
    (FORMAT T "~&Compiling ~A" FILE)
    (COMPILE-FILE FILE)))

(DEFUN RECOMPILE-INCORRECT-COMPILE-FLAVOR-METHODS ()
  (RECOMPILE-SOURCE-FILES
    (ELIMINATE-DUPLICATES
      (LOOP FOR METH IN *FLAVOR-COMPILATIONS*
            AS SF = (GET (LOCF (FLAVOR-PLIST (GET (CADR METH) 'SI:FLAVOR)))
                         'COMPILE-FLAVOR-METHODS)
            WHEN SF COLLECT SF))))

(DEFUN RECOMPILE-ALL-COMPILE-FLAVOR-METHODS ()
  (RECOMPILE-SOURCE-FILES
    (ELIMINATE-DUPLICATES
      (LOOP FOR FLAVOR IN *ALL-FLAVOR-NAMES*
            AS SF = (GET (LOCF (FLAVOR-PLIST (GET FLAVOR 'FLAVOR)))
                         'SI:COMPILE-FLAVOR-METHODS)
            WHEN (EQ SF T)
            DO (FORMAT T "~%Flavor ~s has a compile-flavor-methods property of T" FLAVOR)
            WHEN (AND SF (NEQ SF T))
            COLLECT SF))))

;; Hack for ITS files which have nulls tacked onto their tails due to
;; cretinous software.
;(DEFUN FILE-REMOVE-NULLS (FILE &AUX BUF LOW HIGH FOUND (COUNT 0))
;  (WITH-OPEN-FILE (IN FILE '(:IN :RAW))
;    (WITH-OPEN-FILE (OUT (SEND IN ':TRUENAME) '(:OUT :RAW))
;      (FUNCALL OUT ':CHANGE-PROPERTIES T
;              ':CREATION-DATE (FUNCALL IN ':CREATION-DATE))
;      (LOOP DOING
;       (OR (MULTIPLE-VALUE (BUF LOW HIGH)
;             (FUNCALL IN ':READ-INPUT-BUFFER))
;           (RETURN NIL))
;       WHEN FOUND DO (LOOP REPEAT COUNT DOING (FUNCALL OUT ':TYO 0))
;       DO
;       (SETQ FOUND NIL COUNT 0)
;       (LOOP WHILE (AND (> HIGH LOW) (ZEROP (AREF BUF (1- HIGH))))
;             DO (SETQ FOUND T) (INCF COUNT) (DECF HIGH))
;       (FUNCALL OUT ':STRING-OUT BUF LOW HIGH)
;       (FUNCALL IN ':ADVANCE-INPUT-BUFFER))
;      (FORMAT T "~%~:[No nulls found.~;~D. nulls removed from file.~]" FOUND COUNT))))

(defun recompile-and-load-system-macros ()
  (let ((file-list system-macro-files))
    (mapc #'compile-file file-list)
    (mapc #'load file-list)))

(defun load-system-macros ()
  (mapc #'load system-macro-files))

(defun recompile-cold-load-files ()
  (let ((file-list (cdr (select-processor
                           (:cadr cold-load-file-list)
                           ((:lambda :explorer) lambda-cold-load-file-list)))))
    (mapc #'print file-list)
    (format t "~&Compiling ...~&")
    (loop for qfasl-file in file-list
          as file = (send (fs:parse-pathname qfasl-file) :new-type :LISP)
          do (print file)
          do (compile-file file))))

(defun check-cold-load-files (&optional (for-processor si:processor-type-code))
  (let ((file-list (cdr (select for-processor   ;cdr to skip cptfon.qfasl
                          (si:cadr-type-code cold-load-file-list)
                          (si:lambda-type-code lambda-cold-load-file-list)
                          (si:explorer-type-code lambda-cold-load-file-list)
                          ))))
    (dolist (f file-list)
      (format t "~&~a " f)
      (let* ((qfasl-file (fs:parse-pathname f))
             (lisp-file (send qfasl-file :new-type :LISP)))
        (let ((qfasl-probe (open qfasl-file :direction :probe))
              (lisp-probe (open lisp-file :direction :probe)))
          (cond ((> (send lisp-probe :creation-date)
                    (send qfasl-probe :creation-date))
                 (format t "compiling ...")
                 (compile-file lisp-file))))))))
