;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:CL; Base:10 -*-

;;; More winning Lisp Machine software patch facility.   DLW & BEE 10/24/80
;;; The functions in this file manage the patch files

;;; There are 3 kinds of files for each system. There is a patch description file.
;;; This file contains a version of the patch-system structure. This gives the
;;; format of the patch, and also contains the major version counter for the system
;;; In addition, it contains the format strings to generate the file names for the other
;;; files. The other files are: System directories which contain the list of patches
;;; and their descriptions for a given major version, and the patch files, which contain
;;; the actual patches

;;; Format of main patch directory
(DEFSTRUCT (PATCH-MAJOR (:TYPE LIST) (:CONC-NAME PATCH-)
                        (:ALTERANT NIL) (:CALLABLE-CONSTRUCTORS NIL))
  NAME                          ;system to be patched
  VERSION                       ;most recent version of the system
  )

;; note that the version-list of patch-systems is most recent first,
;; whereas patch-dirs are oldest first!!

;;; Internal format of each patch system
(DEFSTRUCT (PATCH-SYSTEM (:TYPE LIST) (:CONC-NAME PATCH-) (:INCLUDE PATCH-MAJOR)
                         (:ALTERANT NIL) (:CALLABLE-CONSTRUCTORS NIL))
  (STATUS NIL :DOCUMENTATION "A keyword: usually one of:
:RELEASED, :EXPERIMENTAL, :OBSOLETE, :INCONSISTENT, :BROKEN")
  (VERSION-LIST NIL :DOCUMENTATION
    "List of PATCH-VERSION structures corresponding to loaded patches. MOST RECENT FIRST.")
  (DIRECTORY-LOADED-ID NIL :DOCUMENTATION
    "Cons of truename and creation-date of the file to which we last read or wrote the
patch directory to a file, or NIL")
  (PATCH-DIR NIL :DOCUMENTATION "The patch-directory, as last read from file, or NIL.")
  )

(defun patch-system (patch-system)
  (find-system-named (patch-name patch-system)))

(defun make-patch-system-binary-pathname (patch-system pathname)
  ;patch-system can be NIL for private patch.
  (setq pathname (if patch-system
                     (make-system-binary-pathname (patch-system patch-system) pathname)
                   (send pathname :new-type (PATHNAME-DEFAULT-BINARY-FILE-TYPE pathname))))
  (when (eq (send pathname :version) :newest)
    ;;If version is >, remove it so that (compile-file) will match it to the source file
    (setq pathname (send pathname :new-version nil)))
  pathname)

;;; Format of patch directory
(DEFSTRUCT (PATCH-DIR (:TYPE LIST) (:CONC-NAME PATCH-DIR-)
                      (:ALTERANT NIL) (:CALLABLE-CONSTRUCTORS NIL))
  STATUS
  VERSION-LIST                  ;List of patches and explanations, **OLDEST** first.
  )

;;; Information for each patch.
;;; Elements of the PATCH-VERSION-LIST and the PATCH-DIR-VERSION-LIST look like this.
(DEFSTRUCT (PATCH-VERSION (:TYPE LIST) (:CONC-NAME VERSION-)
                          (:ALTERANT NIL) (:CALLABLE-CONSTRUCTORS NIL))
  (NUMBER NIL :DOCUMENTATION "Minor number of the patch")
  (EXPLANATION NIL :DOCUMENTATION "String explaining the contents of the patch")
  (AUTHOR USER-ID :DOCUMENTATION "Who dun it")
  (UNRELEASED NIL :DOCUMENTATION
    "T means that this patch is not released,
and will not normally be loaded by LOAD-PATCHES unless explicitly requested.")
  (time-stamp (get-universal-time) :documentation
    "The universal-time when this patch was started")
  )

;;; List of systems that are legal to patch, i.e. list of PATCH-SYSTEM's
(DEFVAR PATCH-SYSTEMS-LIST NIL)

(DEFVAR FROZEN-PATCH-SYSTEMS-LIST NIL)

;;; Given the name of a system, or the system itself, return the patch system
(DEFUN GET-PATCH-SYSTEM-NAMED (NAME &OPTIONAL NO-ERROR-P ONLY-IF-LOADED &AUX SYSTEM)
  "Return the object of type patch-system whose name is NAME.
NAME can be a string or symbol, or a system object.
NO-ERROR-P means return NIL rather than getting error if no such patchable system.
ONLY-IF-LOADED means return NIL if the system definition is not loaded;
 do not consider loading a file to get the definition."
  (COND ((AND (SETQ SYSTEM (FIND-SYSTEM-NAMED NAME NO-ERROR-P ONLY-IF-LOADED))
              (ASS #'STRING-EQUAL (SYSTEM-NAME SYSTEM) PATCH-SYSTEMS-LIST)))
        (NO-ERROR-P NIL)
        (T (FERROR "The system ~A is not patchable." SYSTEM))))

(DEFUN ADD-PATCH-SYSTEM (NAME &AUX PATCH-SYSTEM VERSION FIRST-VERS PATCH-DIR STATUS)
  "Add a new patchable system named NAME if there is none.
Done when the system is loaded.
Read in the patch directory files to find out the version.
Returns the major version and the system status."
  ;; Flush old patch system if there is one
  (AND (SETQ PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED NAME T))
       (SETQ PATCH-SYSTEMS-LIST (DELQ PATCH-SYSTEM PATCH-SYSTEMS-LIST)))
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME)
        PATCH-SYSTEM (MAKE-PATCH-SYSTEM :NAME NAME :VERSION VERSION)
        PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM)
        FIRST-VERS (FIRST (PATCH-DIR-VERSION-LIST PATCH-DIR)))
  (OR (EQ (VERSION-NUMBER FIRST-VERS) 0)
      (FERROR "Patch directory for ~A messed up: ~S" NAME FIRST-VERS))
  (SETF (PATCH-STATUS PATCH-SYSTEM) (SETQ STATUS (PATCH-DIR-STATUS PATCH-DIR)))
  (SETF (PATCH-VERSION-LIST PATCH-SYSTEM) (NCONS FIRST-VERS))
  (SETQ PATCH-SYSTEMS-LIST (NCONC PATCH-SYSTEMS-LIST (NCONS PATCH-SYSTEM)))
  (VALUES VERSION STATUS))

(DEFUN FREEZE-PATCH-SYSTEM (NAME)
  (PUSHNEW (GET-PATCH-SYSTEM-NAMED NAME) FROZEN-PATCH-SYSTEMS-LIST)
  NAME)

(DEFUN INCREMENT-PATCH-SYSTEM-MAJOR-VERSION (NAME STATUS &AUX VERSION PATCH-MAJOR)
  "Increment the major version of the patchable system NAME, and set status to STATUS.
This modifies the patch directory files of the system."
  (SETQ VERSION (GET-PATCH-SYSTEM-MAJOR-VERSION NAME T))
  (WHEN (NULL VERSION)
    (FORMAT T "~&No master directory for system ~A, creating one." NAME)
    (SETQ VERSION 0))
  (INCF VERSION)
  (SETQ PATCH-MAJOR (MAKE-PATCH-MAJOR :NAME NAME :VERSION VERSION))
  (WITH-OPEN-FILE (FILE (PATCH-SYSTEM-PATHNAME NAME :SYSTEM-DIRECTORY) :DIRECTION :OUTPUT)
    (FORMAT FILE
            ";;; -*- Mode:LISP; Package:USER; Base:10; Readtable:Common-Lisp; Patch-File:T -*-~%")
    (LET ((*PRINT-BASE* 10.)
          (*PACKAGE* PKG-USER-PACKAGE)
          (*READTABLE* INITIAL-COMMON-LISP-READTABLE))
      (WRITE-RESPONSIBILITY-COMMENT FILE)
      (PRINT PATCH-MAJOR FILE)))
  (LET ((FIRST-VERS (MAKE-PATCH-VERSION :NUMBER 0
                                        :EXPLANATION (FORMAT NIL "~A Loaded" NAME))))
    (WRITE-PATCH-DIRECTORY PATCH-MAJOR (MAKE-PATCH-DIR :STATUS STATUS
                                                       :VERSION-LIST (NCONS FIRST-VERS))
                           T))
  VERSION)

(DEFUN GET-PATCH-SYSTEM-MAJOR-VERSION (NAME &OPTIONAL NO-ERROR-P)
  "Return the major version of patchable system NAME, as recorded in files.
NO-ERROR-P says no error if cannot read the files."
  (LET ((PATHNAME (PATCH-SYSTEM-PATHNAME NAME :SYSTEM-DIRECTORY))
        (*READ-BASE* 10.)
        (*READTABLE* INITIAL-READTABLE) ;for old patch dirs
        (VERSION NIL))
    (CONDITION-CASE-IF NO-ERROR-P ()
        (FS:READING-FROM-FILE (FORM PATHNAME)
          (COND (VERSION        ;ie, second time around the loop.
                 (FERROR "Gubble found in major patch directory of ~A" NAME))
                ((NOT (STRING-EQUAL NAME (PATCH-NAME FORM)))
                 (FERROR "~A name does not agree with ~A the name in the patch descriptor file"
                         NAME (PATCH-NAME FORM)))
                (T
                 (SETQ VERSION (PATCH-VERSION FORM)))))
      (FS:FILE-ERROR NIL))
    VERSION))

(DEFVAR SYSTEM-STATUS-ALIST '((:EXPERIMENTAL "Experimental" "Exp" "experimental")
                              (:RELEASED "" "" "released")
                              (:OBSOLETE "Obsolete" "Obs" "obsolete")
                              (:INCONSISTENT "Inconsistent (unreleased patches loaded)"
                                             "Bad" "inconsistent (unreleased patches loaded)")
                              (:BROKEN "Broken" "Broke" "broken")))

(DEFUN READ-PATCH-DIRECTORY (PATCH-SYSTEM &OPTIONAL NOERROR &AUX DIR)
  "Read in a patch directory file, returning the list-structure representation.
PATCH-SYSTEM is an object of type PATCH-SYSTEM.
The value is described by the defstruct PATCH-DIR.
NOERROR means return NIL rather than get error if patch directory file won't open."
  (CONDITION-CASE-IF NOERROR ()
      (WITH-OPEN-FILE (PATCH-DIR (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
                                                        :VERSION-DIRECTORY
                                                        (PATCH-VERSION PATCH-SYSTEM))
                                 :CHARACTERS T)
        (LET ((*READ-BASE* 10.)
              (*PACKAGE* PKG-USER-PACKAGE)
              (*READTABLE* INITIAL-READTABLE) ;for old patch dirs
              (INFO (SEND PATCH-DIR :INFO)))
          ;; don't waste time reading it in again
          (IF (AND INFO (EQUAL INFO (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM)))
              (SETQ DIR (PATCH-PATCH-DIR PATCH-SYSTEM))
            (SI:READING-FROM-STREAM (FORM PATCH-DIR)
              (cond (DIR
                     (FERROR "Gubble found in patch directory of ~A" (PATCH-NAME PATCH-SYSTEM)))
                    (t (SETQ DIR FORM)
                       (SETF (PATCH-PATCH-DIR PATCH-SYSTEM) DIR
                             (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM) INFO))))))
        (UNLESS (ASSQ (PATCH-DIR-STATUS DIR) SYSTEM-STATUS-ALIST)
          (FERROR "Unknown patch system status ~S for ~A"
                  (PATCH-DIR-STATUS DIR) (PATCH-NAME PATCH-SYSTEM)))
        DIR)
    (FS:FILE-ERROR NIL)))

;;; Write out a patch directory file from the list-structure representation.
(DEFUN WRITE-PATCH-DIRECTORY (PATCH-SYSTEM PATCH-DIR &OPTIONAL MAJORP)
  "Write out a new patch directory file for PATCH-SYSTEM.
PATCH-DIR is a list described by the defstruct PATCH-DIR,
which is the data to write into the file."
  (LET ((*PRINT-BASE* 10.) (*PACKAGE* PKG-USER-PACKAGE)
        (*NOPOINT T) (*PRINT-RADIX* NIL)
        (*READTABLE* INITIAL-COMMON-LISP-READTABLE))
    (WITH-OPEN-FILE (STREAM (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM)
                                                   :VERSION-DIRECTORY
                                                   (PATCH-VERSION PATCH-SYSTEM))
                            :DIRECTION :OUTPUT :CHARACTERS T)
       (FORMAT STREAM
               ";;; -*- Mode:LISP; Package:USER; Base:10; Readtable:Common-Lisp; Patch-File:T -*-
;;; Patch directory for ~A version ~D~%"
               (PATCH-NAME PATCH-SYSTEM) (PATCH-VERSION PATCH-SYSTEM))
       (WRITE-RESPONSIBILITY-COMMENT STREAM)
       (FORMAT STREAM "(~S~% (~{~S~%  ~}))"
               (PATCH-DIR-STATUS PATCH-DIR)
               (PATCH-DIR-VERSION-LIST PATCH-DIR))
       (UNLESS MAJORP
         (SETF (PATCH-DIRECTORY-LOADED-ID PATCH-SYSTEM) (SEND STREAM :INFO))
         (SETF (PATCH-PATCH-DIR PATCH-SYSTEM) PATCH-DIR)))))

(DEFUN PRINT-PATCH (MAJOR-VERSION-NUMBER PATCH-VERSION-DESC)
  (FORMAT T "~&~D.~D ~8T~A~@[ (~\\time\\)~]:~:[~; (unreleased)~]~&~10T~~A~~%"
          MAJOR-VERSION-NUMBER
          (VERSION-NUMBER PATCH-VERSION-DESC)
          (VERSION-AUTHOR PATCH-VERSION-DESC)
          (VERSION-time-stamp PATCH-VERSION-DESC)
          (VERSION-UNRELEASED PATCH-VERSION-DESC)
          (VERSION-EXPLANATION PATCH-VERSION-DESC)))

(DEFUN PRINT-PATCHES (&OPTIONAL (SYSTEM "System") (AFTER 0))
  "Print the patches of the system SYSTEM after minor version AFTER."
  (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
         (VERSION (PATCH-VERSION PATCH-SYSTEM)) ;efficiency
         (LATEST (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM)))))
    (cond ((NULL PATCH-SYSTEM)
           (FORMAT T "~%No ~A system loaded~%" SYSTEM))
          (t
           (FORMAT T "~%~A ~8TModification:~%" (PATCH-NAME PATCH-SYSTEM))
           (IF (> AFTER LATEST)
               (FORMAT T "~&Most recent patch loaded is ~D." LATEST)
             (DOLIST (V (REVERSE (PATCH-VERSION-LIST PATCH-SYSTEM)))
               (WHEN ( AFTER (VERSION-NUMBER V))
                 (PRINT-PATCH VERSION V))))))))

(DEFUN PATCH-LOADED-P (MAJOR-VERSION MINOR-VERSION &OPTIONAL (SYSTEM "System"))
  "T if specified patch to patchable system SYSTEM is now loaded.
The patch specified is the one with numbers MAJOR-VERSION and MINOR-VERSION.
If the actual loaded major version is greater than MAJOR-VERSION
then the answer is T regardless of MINOR-VERSION, on the usually-true assumption
that the newer system contains everything patched into the older one.
NIL if SYSTEM is not loaded at all."
  (LET* ((PATCH-SYSTEM (GET-PATCH-SYSTEM-NAMED SYSTEM T T))
         CURRENT-MAJOR-VERSION)
    (AND PATCH-SYSTEM
         (SETQ CURRENT-MAJOR-VERSION (PATCH-VERSION PATCH-SYSTEM))
         (OR (> CURRENT-MAJOR-VERSION MAJOR-VERSION)
             (AND (= CURRENT-MAJOR-VERSION MAJOR-VERSION)
                  ( (OR (VERSION-NUMBER (CAR (PATCH-VERSION-LIST PATCH-SYSTEM))) 0)
                     MINOR-VERSION))))))

(defun assure-system-patch-loaded (system major-version minor-version)
  (unless (patch-loaded-p major-version minor-version system)
    (let* ((patch-system (get-patch-system-named system t t))
           (string (format nil "~A version ~D.~D"
                           (if patch-system (patch-name patch-system) system)
                           major-version minor-version)))
      (ferror "~~:[The rest of this ~:[~;patch ~]file requires ~A to be~;~*~A not~] loaded.~
~:[~%Note that a patchable version of the ~A system does not appear to be loaded at all.~;~]~"
                  (not fdefine-file-pathname) fs:this-is-a-patch-file
                  string patch-system system))))

(defun load-patch-file (filename verbose-p)
  ;; I wish I could put the following hack in when the patch is being written.
  ;; Anyway, too many times, people make patches that reload files,
  ;; then the files get recompiled and do bizzare things.  For example,
  ;; my 110 band is now useless now that 115 has incompatable qfasl's.
  ;; Therefore, I institute this policy of not allowing random qfasl's
  ;; to be loaded by a patch.  If you want to do that, Add Patch the
  ;; whole file.  This will also break the technique of using System patchs to load
  ;; other systems.   - Pace
  ;;;The following technique had the horrible side-effect: upon warm-booting,
  ;;;the new "temporary" function value of LOAD became permanent. -Keith
    ;;(let ((real-load #'load))
    ;;(letf (((symbol-function 'load) #'(lambda (&rest ignore)
    ;;(ferror nil "The use of ~S in a patch file is not supported." 'load))))
  (load filename :verbose verbose-p :set-default-pathname nil))

(defun compile-patch-file (patch-system pathname)
  (compile-file pathname
                :output-file (make-patch-system-binary-pathname patch-system pathname)))

(defun load-patches-for-systems (system-names selective-p verbose-p unreleased-p force-through-unfinished-patches-p)
  (LET ((FIRST-SYSTEM T) (something-changed nil))  ; This is the first system being patched.
    (DOLIST (PATCH SYSTEM-NAMES)
      (CATCH-ERROR-RESTART (ERROR "Give up on patches for ~A." (CAR PATCH))
        (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH T))
               (NEW-VERS (PATCH-DIR-VERSION-LIST PATCH-DIR))
               (MAJOR (PATCH-VERSION PATCH))
               (binary-file-type (system-default-binary-file-type (patch-system patch)))
               PATCHES-NOT-LOADED
               (CHANGE-STATUS T)                ;Ok to change the system status
               (UNRELEASED-CONSIDERED NIL)      ;T if considering unreleased patches.
               (PATCH-SKIPPED NIL)      ;T if considering patches after skipping one.
               (PROCEED-FLAG (NOT SELECTIVE-P))) ; Has the user said to proceed?
          (IF (AND (NULL PATCH-DIR) VERBOSE-P)
              (FORMAT T "~&Skipping system ~A, whose patch directory cannot be accessed.~%"
                      (CAR PATCH))
            (progn
              ;; Get list of patches of this system not already loaded.
              (SETQ PATCHES-NOT-LOADED
                    (CDR (MEMASSQ (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
                                  NEW-VERS)))
              ;; Maybe announce the system.
              (WHEN (AND PATCHES-NOT-LOADED VERBOSE-P)  ;verbose and silent is nonsense
                (FRESH-LINE *STANDARD-OUTPUT*)
                (UNLESS FIRST-SYSTEM (TERPRI))
                (FORMAT T "~&Patches for ~A (Current version is ~D.~D):"
                        (PATCH-NAME PATCH) MAJOR (CAAR (LAST NEW-VERS))))
              (DOLIST (VERSION PATCHES-NOT-LOADED)
                (LET* ((FILENAME (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH) :PATCH-FILE
                                                        (PATCH-VERSION PATCH)
                                                        (VERSION-NUMBER VERSION)
                                                        binary-file-type)))
                  ;; NIL is used to mark patches that are reserved, but not finished.
                  ;; We can't load any more patches without this one, in order to
                  ;; make sure that any two systems claiming to be version xx.yy
                  ;; always have exactly the same set of patches loaded.  Punt.
                  ;; If someone forgets to finish a patch, we assume a hacker will
                  ;; eventually see what is happening and fix the directory to unstick
                  ;; things.  We might at least say the patches are unfinished.
                  (UNLESS (VERSION-EXPLANATION VERSION)
                    (WHEN VERBOSE-P
                      (FORMAT T "~&There are unfinished patches in ~A."
                              (PATCH-NAME PATCH)))
                    (UNLESS FORCE-THROUGH-UNFINISHED-PATCHES-P
                      (RETURN)))
                  (WHEN (VERSION-UNRELEASED VERSION)
                    (WHEN VERBOSE-P
                      (FORMAT T "~&There are unreleased patches in ~A."
                              (PATCH-NAME PATCH)))
                    (OR
                      FORCE-THROUGH-UNFINISHED-PATCHES-P
                      UNRELEASED-P
                      UNRELEASED-CONSIDERED
                      (EQ (PATCH-STATUS PATCH) :INCONSISTENT)
                      (AND SELECTIVE-P
                           (WITH-TIMEOUT ((* 5 60. 60.)
                                          (FORMAT T " -- timed out, No.")
                                          NIL)
                             (FORMAT T "~&Such patches are subject to change; therefore,
  you should not load them if you are going to dump a band.
  If you are not going to dump a band, it is reasonable
  to load these patches to benefit from the improvements in them.")
                             (SETQ PROCEED-FLAG NIL)
                             (Y-OR-N-P "Consider the unreleased patches? (Automatic No after 5 minutes) ")))
                      (RETURN))
                    (SETQ UNRELEASED-CONSIDERED T))
                  (WHEN VERBOSE-P
                    (PRINT-PATCH (PATCH-VERSION PATCH) VERSION))
                  (SELECTQ-EVERY
                    (COND (PROCEED-FLAG)
                          (T (WITH-TIMEOUT ((* 5 60. 60.)
                                            (FORMAT T " -- timed out, Proceed.")
                                            'PROCEED)
                               (FQUERY '(:CHOICES (((T "Yes.") #\Y #\SP #\T #\HAND-UP)
                                                   ((NIL "No.") #\N #\RUBOUT #\HAND-DOWN)
                                                   ((PROCEED "Proceed.") #\P)))
                                       "Load? (Automatic Proceed after 5 minutes) "))))
                    (NIL
                     ;; "No", don't load any more for this system.
                     ;; Also don't change the status.
                     ;; Except, if we are considering unreleased patches,
                     ;; loading out of order is no worse than loading unreleased
                     ;; patches in the first place, so keep on offering.
                     (SETQ CHANGE-STATUS NIL)
                     (UNLESS (OR FORCE-THROUGH-UNFINISHED-PATCHES-P
                                 UNRELEASED-CONSIDERED)
                       (RETURN NIL))
                     (WHEN (EQ VERSION (CAR (LAST PATCHES-NOT-LOADED)))
                       ;; Don't give a spiel about following patches
                       ;; if there are none.
                       (RETURN NIL))
                     (UNLESS (OR PATCH-SKIPPED
                                 (EQ (PATCH-STATUS PATCH) ':INCONSISTENT))
                       (FORMAT T "~&If you load any following patches for this system,
  they will be out of sequence, so you must not dump a band.")
                       (SETQ PATCH-SKIPPED T)))
                    (PROCEED
                     ;; "Proceed" with the rest for this system.
                     (SETQ PROCEED-FLAG T))
                    ((T PROCEED)
                     ;; "Yes" or "Proceed", do this one.
                     (SETQ SOMETHING-CHANGED T)
                     ;; Unfinished, unreleased or out of sequence =>
                     ;;  mark system as inconsistent.
                     (WHEN (OR PATCH-SKIPPED
                               (NULL (VERSION-EXPLANATION VERSION))
                               (VERSION-UNRELEASED VERSION))
                       (UNLESS (EQ (PATCH-STATUS PATCH) ':INCONSISTENT)
                         (SETF (PATCH-STATUS PATCH) ':INCONSISTENT)
                         (FORMAT T "~&~A is now inconsistent; do not dump a band."
                                 (PATCH-NAME PATCH))))
                     ;; Avoid error if non ex file, if patch is known to be unfinished.
                     (CONDITION-CASE-IF (NULL (VERSION-EXPLANATION VERSION)) ()
                         (load-patch-file filename verbose-p)
                       (FS:FILE-NOT-FOUND
                        (WHEN VERBOSE-P
                          (FORMAT T "~&File ~A does not exist, ignoring this patch."
                                  FILENAME))))
                     (PUSH VERSION (PATCH-VERSION-LIST PATCH))))))
              (AND CHANGE-STATUS
                   (NEQ (PATCH-STATUS PATCH) ':INCONSISTENT)
                   (LET ((NEW-STATUS (PATCH-DIR-STATUS PATCH-DIR)))
                     (COND ((NEQ (PATCH-STATUS PATCH) NEW-STATUS)
                            (SETQ SOMETHING-CHANGED T)
                            (WHEN VERBOSE-P
                              (FORMAT T "~&~A (version ~D) is now ~A."
                                      (PATCH-NAME PATCH)
                                      major
                                      (FOURTH (ASSQ NEW-STATUS
                                                    SYSTEM-STATUS-ALIST))))
                            ;; Update the status.
                            (SETF (PATCH-STATUS PATCH) NEW-STATUS)))))))))
      (SETQ FIRST-SYSTEM NIL))
    something-changed))

(DEFUN LOAD-PATCHES (&REST OPTIONS &AUX TEM SOMETHING-CHANGED override-root-query-p)
  "Load any new patches for one or more systems.
Options can include these symbols:
 :NOSELECTIVE - don't ask about each patch.
 :SILENT or :NOWARN - don't print out any information on loading patches
   (and also don't ask).
 :VERBOSE - says to print out information about loading each patch.
   This is the default and is only turned off by :silent and :nowarn.
 :UNRELEASED - says to load or consider unreleased patches.
   Once unreleased patches have been loaded, a band may not be dumped.
 :FORCE-UNFINISHED - load all patches that have not been finished yet,
   if they have QFASL files.  This is good for testing patches.
 :NOOP - do nothing
 :SITE - load latest site configuration info.
 :NOSITE - do not load latest site configuration info.
   :SITE is the default unless systems to load patches for are specified.

Options can also include :SYSTEMS followed by a list of systems to load patches for.
One or more names of systems are also allowed.

LOAD-PATCHES returns T if any patches were loaded, otherwise NIL."
  (CATCH-ERROR-RESTART (SYS:REMOTE-NETWORK-ERROR
                         "Give up on trying to load patches.")
    (LET ((SYSTEM-NAMES NIL)                    ;A-list of systems to load patches for.
          (SELECTIVE-P T)                       ;Ask the user.
          (VERBOSE-P T)                         ;Tell the user what's going on.
          (UNRELEASED-P NIL)
          (SITE-SPECIFIED-P NIL)
          (SITE-P T)
          (FORCE-THROUGH-UNFINISHED-PATCHES-P NIL))
      (DO ((OPTS OPTIONS (CDR OPTS)))
          ((NULL OPTS))
        (SELECTQ (CAR OPTS)
          (:SYSTEMS
           (SETQ OPTS (CDR OPTS))
           (SETQ SYSTEM-NAMES
                 (IF (CONSP (CAR OPTS))
                     (MAPCAR #'GET-PATCH-SYSTEM-NAMED (CAR OPTS))
                   (LIST (GET-PATCH-SYSTEM-NAMED (CAR OPTS)))))
           (UNLESS SITE-SPECIFIED-P
             (SETQ SITE-P NIL)))
          ((:SILENT :NOWARN) (SETQ VERBOSE-P NIL SELECTIVE-P NIL))
          (:VERBOSE (SETQ VERBOSE-P T))
          (:SELECTIVE (SETQ SELECTIVE-P T))
          (:SITE (SETQ SITE-P T SITE-SPECIFIED-P T))
          (:NOOP NIL)
          (:NOSITE (SETQ SITE-P NIL SITE-SPECIFIED-P T))
          (:UNRELEASED (SETQ UNRELEASED-P T))
          (:NOSELECTIVE (SETQ SELECTIVE-P NIL))
          (:FORCE-UNFINISHED (SETQ FORCE-THROUGH-UNFINISHED-PATCHES-P T))
          (OTHERWISE
            (COND ((AND (OR (SYMBOLP (CAR OPTS)) (STRINGP (CAR OPTS)))
                        (SETQ TEM (GET-PATCH-SYSTEM-NAMED (CAR OPTS) T)))
                   (PUSH TEM SYSTEM-NAMES)
                   (UNLESS SITE-SPECIFIED-P
                     (SETQ SITE-P NIL)))
                  (T (FERROR "~S is neither a ~S option nor a system name."
                                 (CAR OPTS) 'LOAD-PATCHES))))))
      ;;;Check that we're in global package root:
      (let ((root (si:pkg-root *package*)))
        (when (and root (null override-root-query-p))
          (cerror "Proceed to load patches within ~A package root"
                  "The current package root is ~A.  ~&~
                   Loading patches made for a different package root will probably not work.~&~
                   Proceed with caution, only if you know this will work." root)
          (setq override-root-query-p t)))
      (WITH-SYS-HOST-ACCESSIBLE
        (LET-IF VERBOSE-P ((TV:MORE-PROCESSING-GLOBAL-ENABLE NIL))
          (WHEN SITE-P
            (WHEN VERBOSE-P
              (FORMAT T "~%Checking whether site configuration has changed..."))
            (IF (IF SELECTIVE-P
                    (MAKE-SYSTEM "SITE" :NO-RELOAD-SYSTEM-DECLARATION)
                  (IF VERBOSE-P
                      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION)
                      (MAKE-SYSTEM "SITE" :NOCONFIRM :NO-RELOAD-SYSTEM-DECLARATION :SILENT)))
                (SETQ SOMETHING-CHANGED T)
              (WHEN VERBOSE-P (FORMAT T "  it hasn't.")))
            (LOAD-PATCHES-FOR-LOGICAL-PATHNAME-HOSTS))
          (OR SYSTEM-NAMES (SETQ SYSTEM-NAMES (SUBSET #'(LAMBDA (X)
                                                          (NOT (MEMQ X FROZEN-PATCH-SYSTEMS-LIST)))
                                                      PATCH-SYSTEMS-LIST)))
          (DOLIST (X SYSTEM-NAMES)
            (WHEN (MEMQ X FROZEN-PATCH-SYSTEMS-LIST)
              (FORMAT T "~&Note: Patch System ~S is frozen. There should be no need to load patches from it.~%"
                      (PATCH-NAME X))))
          (setq something-changed (or (load-patches-for-systems system-names selective-p verbose-p unreleased-p
                                                                force-through-unfinished-patches-p)
                                      something-changed))))))
  SOMETHING-CHANGED)

(defun load-patches-for-logical-pathname-hosts ()
  (dolist (host fs::*logical-pathname-host-list*)
    (if (send host :get 'fs:make-logical-pathname-host)
        (fs:make-logical-pathname-host host :warn-about-redefinition nil
                                       ;use the same as last time, if any.
                                       :host-name-for-translations (send host :get :host-name-for-translations)
                                       ))))

(DEFUN LOAD-AND-SAVE-PATCHES (&OPTIONAL BAND &REST KEYWORD-ARGS)
  "Load all patches and save a new Lisp world in a disk partition.
KEYWORD-ARGS are passed to LOAD-PATCHES.
BAND is the name or number of a LOD band to save in."
  (CHECK-TYPE BAND (OR NUMBER STRING NULL) "A specifier for a band")
  (IF (OR (MEMQ ':FORCE-UNFINISHED KEYWORD-ARGS)
          (MEMQ ':UNRELEASED KEYWORD-ARGS))
      (FERROR "~S and ~S are not reasonable arguments here."
              :FORCE-UNFINISHED :UNRELEASED))
  (DOLIST (PATCH-SYSTEM PATCH-SYSTEMS-LIST)
    (WHEN (EQ (PATCH-STATUS PATCH-SYSTEM) :INCONSISTENT)
      (BEEP)
      (FORMAT *QUERY-IO* "~&You have loaded patches out of sequence,
 or loaded unreleased patches, in ~A.
As a result, the environment is probably inconsistent with the
current patches and will remain so despite attempts to update it.
Unless you understand these problems well and know how to
be sure whether they are occurring, or how to clean them up,
you should not save this environment."
              (PATCH-NAME PATCH-SYSTEM))
      (SEND *QUERY-IO* :CLEAR-INPUT)
      (UNLESS (YES-OR-NO-P "Dump anyway? ")
        (RETURN-FROM LOAD-AND-SAVE-PATCHES NIL))))
  (DO ((BAND1 BAND (PROMPT-AND-READ :STRING "~&Save into which band? "))
       (COUNT 0 (1+ COUNT)))
      (())
    (WHEN BAND1
      (COND ((NUMBERP BAND1)
             (SETQ BAND1 (FORMAT NIL "LOD~D" BAND1)))
            ((PARSE-NUMBER BAND1 0 NIL NIL T)
             (SETQ BAND1 (STRING-APPEND "LOD" BAND1))))
      (COND ((NOT (STRING-EQUAL BAND1 "LOD" :END1 3))
             (FORMAT *QUERY-IO* "~&You must save into a LOD partition."))
            ((NOT (FIND-DISK-PARTITION BAND1))
             (FORMAT *QUERY-IO* "~&No such band: ~A." BAND1))
            ((FIND-DISK-PARTITION-FOR-WRITE BAND1)
             ;; Non-NIL means user gave confirmation.
             (SETQ BAND BAND1)
             (RETURN))))
    (IF (ZEROP COUNT) (PRINT-DISK-LABEL)))
  (WITH-SYS-HOST-ACCESSIBLE
    (COND ((APPLY #'LOAD-PATCHES :NOSELECTIVE KEYWORD-ARGS)
           (DISK-SAVE BAND T))
          (T (FORMAT *QUERY-IO* "~&No patches have been made.")))))

;;;Say who did it: which hardware, which firmware, which software, and which meatware.
;;;Written, e.g., into patch files.

(DEFUN WRITE-RESPONSIBILITY-COMMENT (STREAM &OPTIONAL (COMMENT-CHARACTER #\;))
  (FLET ((BEGIN-COMMENT-LINE ()
           (FRESH-LINE STREAM)
           (DOTIMES (I 3) (WRITE-CHAR COMMENT-CHARACTER STREAM))
           (WRITE-CHAR #\Space STREAM)))
    (BEGIN-COMMENT-LINE)
    (WRITE-STRING "Written " STREAM)
    (TIME:PRINT-CURRENT-TIME STREAM :DD-MMM-YY)
    (FORMAT STREAM " by ~A ~:[~*~;(~A) ~]at site ~A"
            USER-ID
            (NOT (ZEROP (LENGTH FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST)))
            FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST
            (SHORT-SITE-NAME))
    (BEGIN-COMMENT-LINE)
    (FORMAT STREAM "while running on ~A from band ~C"
            LOCAL-PRETTY-HOST-NAME (LDB (BYTE 8. 16.) CURRENT-LOADED-BAND))
    (BEGIN-COMMENT-LINE)
    (FORMAT STREAM "with ~A.~2%" (SYSTEM-VERSION-INFO))))

(DEFUN RESERVE-PATCH (PATCH-SYSTEM &OPTIONAL (WARNING-STREAM *STANDARD-OUTPUT*))
  "Allocate a minor system number for patchable system PATCH.
Mark it in the directory with a NIL.
PATCH should be an object of type PATCH-SYSTEM.
Returns the allocated minor version number."
  (IF (MEMQ PATCH-SYSTEM FROZEN-PATCH-SYSTEMS-LIST)
      (cerror "Do it anyway.  ** Be sure you know what you're doing! **"
              "System ~S is frozen and should not be patched." (PATCH-NAME PATCH-SYSTEM)))
  (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM))
         (PATCHES (PATCH-DIR-VERSION-LIST PATCH-DIR))
         (LAST-PATCH (LAST PATCHES))
         (NEW-VERSION (1+ (VERSION-NUMBER (FIRST LAST-PATCH)))))
    (DOLIST (P PATCHES)
      (WHEN (VERSION-EXPLANATION P)
        (UNLESS (ASSQ (VERSION-NUMBER P)
                      (PATCH-VERSION-LIST PATCH-SYSTEM))
          (FORMAT WARNING-STREAM "~&Note: Patch ~D.~D is not loaded yet."
                  (PATCH-VERSION PATCH-SYSTEM)
                  (VERSION-NUMBER P))
          (RETURN))))                           ;Only mention the first one.
    (DOLIST (P PATCHES)
      (OR (VERSION-EXPLANATION P)
          (RETURN (FORMAT WARNING-STREAM "~&Note: Patch ~D.~D is not finished yet."
                          (PATCH-VERSION PATCH-SYSTEM)
                          (VERSION-NUMBER P)))))
    (SETF (CDR LAST-PATCH)
          (NCONS (MAKE-PATCH-VERSION :NUMBER NEW-VERSION :EXPLANATION NIL)))
    (WRITE-PATCH-DIRECTORY PATCH-SYSTEM PATCH-DIR)
    NEW-VERSION))

(DEFUN CONSUMMATE-PATCH (PATCH-SYSTEM NUMBER MESSAGE &OPTIONAL (RELEASE-FLAG T) NO-RECOMPILE)
  "Finish up making the patch for the specified minor system version number.
MESSAGE is the message to be displayed to the user in PRINT-PATCHES.
This replaces the NILs left by RESERVE-PATCH with the message.
If RELEASE-FLAG is NIL, the patch is not released, so it will
be loaded only by users who say to load unreleased patches.
To release the patch, call CONSUMMATE-PATCH again
 with NIL (or an updated message) for MESSAGE and T for RELEASE-FLAG.
NO-RECOMPILE says do not compile the patch file source;
 this would normally be used only with releasing an already finished patch."
  (UNLESS NO-RECOMPILE
    (COMPILE-patch-FILE patch-system (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM) :PATCH-FILE
                                                            (PATCH-VERSION PATCH-SYSTEM) NUMBER
                                                            :LISP)))
  (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM))
         (PATCHES (PATCH-DIR-VERSION-LIST PATCH-DIR))
         (VERSION (ASSQ NUMBER PATCHES)))
    (IF MESSAGE (SETF (VERSION-EXPLANATION VERSION) MESSAGE))
    (SETF (VERSION-UNRELEASED VERSION) (NOT RELEASE-FLAG))
    (WRITE-PATCH-DIRECTORY PATCH-SYSTEM PATCH-DIR)
    NIL))       ;Despite the comment above, this function doesn't seem to detect any
                ; meaningful errors and returns randomness.  So return NIL for success.

(DEFUN ABORT-PATCH (PATCH-SYSTEM NUMBER)
  "Remove all record of patch number NUMBER in PATCH-SYSTEM.
PATCH-SYSTEM is an object of type PATCH-SYSTEM, which specifies
the system name and major version number."
  (LET* ((PATCH-DIR (READ-PATCH-DIRECTORY PATCH-SYSTEM))
         (PATCHES (PATCH-DIR-VERSION-LIST PATCH-DIR)))
    (SETF (PATCH-DIR-VERSION-LIST PATCH-DIR)
          (DELQ (ASSQ NUMBER PATCHES) (PATCH-DIR-VERSION-LIST PATCH-DIR)))
    (WRITE-PATCH-DIRECTORY PATCH-SYSTEM PATCH-DIR)
    NIL))

(DEFUN VIEW-UNFINISHED-PATCHES (&OPTIONAL (SYSTEM "System")
                                (STREAM *STANDARD-OUTPUT*) (LOADED-ONLY T))
  "Displays the contents of all files that represent unfinished patches in SYSTEM.
Unreleased patches are also described."
  (FRESH-LINE STREAM)
  (LET* ((SYS-NAME (AND (FIND-SYSTEM-NAMED SYSTEM T LOADED-ONLY) SYSTEM))
         (PATCH-SYSTEM (AND SYS-NAME
                            (ASS #'STRING-EQUAL SYS-NAME PATCH-SYSTEMS-LIST))))
    (IF (NULL PATCH-SYSTEM)
        (FORMAT STREAM "There seems to be no patchable ~A system .~%" SYSTEM)
      (DOLIST (ELEM (PATCH-DIR-VERSION-LIST (READ-PATCH-DIRECTORY PATCH-SYSTEM)))
        (WHEN (OR (NOT (VERSION-EXPLANATION ELEM))
                  (VERSION-UNRELEASED ELEM))
          (FORMAT STREAM "~&  Found a patch started by ~A " (THIRD ELEM))
          (WITH-OPEN-FILE-CASE (F (PATCH-SYSTEM-PATHNAME (PATCH-NAME PATCH-SYSTEM) :PATCH-FILE
                                                         (PATCH-VERSION PATCH-SYSTEM) (FIRST ELEM)
                                                         :LISP))
            (FS:FILE-NOT-FOUND
             (FORMAT STREAM
                     "of minor version ~D, which hasn't yet been written out."
                     (FIRST ELEM)))
            (:NO-ERROR
             (FORMAT STREAM "in the file ~A~%" (SEND F :TRUENAME))
             (STREAM-COPY-UNTIL-EOF F STREAM))))))))


;;;; Utilities for system versions

(DEFVAR SYSTEM-ADDITIONAL-INFO ""
  "Additional info is printed after the version when the system is booted.")

;;; This function updates the system version, asking the user.  If this is a fresh
;;; cold-load, the major-version stored on the file system is incremented.
;;; Returns string to go in the disk label.
;;; The user is allowed to add additional commentary.  If the whole string
;;; won't fit in the silly small disk-label comment field, the user
;;; is asked to retype it in an abbreviated form.
(DEFUN GET-NEW-SYSTEM-VERSION (&OPTIONAL (MAXIMUM-LENGTH 16.) &KEY INCREMENTAL query)
  (setq maximum-length (or maximum-length 16.))
  (when (not (plusp maximum-length))
    (warn "No characters allowed for new system version label \(maximum-length = ~D\)"
          maximum-length)
    (return-from get-new-system-version ""))
  (FORMAT T "~&The system is now running:")
  (DESCRIBE-SYSTEM-VERSIONS)
  (FRESH-LINE)
  (SETQ SYSTEM-ADDITIONAL-INFO
        (READLINE-TRIM *QUERY-IO* ""
                       `((:PROMPT "Additional comment for herald: ")
                         (:INITIAL-INPUT ,SYSTEM-ADDITIONAL-INFO)
                         (:INITIAL-INPUT-POINTER ,(LENGTH SYSTEM-ADDITIONAL-INFO)))))
  (LET ((VERS (SYSTEM-VERSION-INFO T)))
    (IF INCREMENTAL (SETQ VERS (STRING-APPEND "Inc " VERS)))
    ;; Offer user chance to change it
    (when query
      (setq vers
            (string-trim
              #\return
              (readline-trim *query-io* ""
                             `((:prompt ,(if (stringp query)
                                             (format nil query vers maximum-length)
                                           (format nil "~%Edit the system version label \(as shown\); ~
up to ~D character~:P allowed.~&Press RETURN when done.~2%> " maximum-length)))
                               (:initial-input ,vers)
                               (:initial-output-pointer ,(string-length vers))
                               (:initial-input-pointer 0.))))))
    ;; If short version doesn't fit, allow user to edit it (e.g. abbreviate system names)
    (DO (SHORT)
        (( (LENGTH VERS) MAXIMUM-LENGTH))
      (SETQ SHORT (SUBSTRING VERS 0 MAXIMUM-LENGTH))
      (beep)
      (SETQ VERS
            (READLINE-TRIM *QUERY-IO* ""
                           `((:PROMPT ,(FORMAT NIL "~%~S will not fit in disk label.~@
                                                    ~%Please abbreviate the label to ~D character~:P: "
                                               VERS MAXIMUM-LENGTH))
                             (:INITIAL-INPUT ,VERS)
                             (:INITIAL-INPUT-POINTER ,MAXIMUM-LENGTH)))))
    VERS))

(DEFUN SYSTEM-VERSION-INFO (&OPTIONAL (BRIEF-P NIL) &AUX (FIRST T) TEM)
  "Return a one-line string giving the versions of all patchable systems.
Also gives the microcode version, and the loaded band's disk label comment.
With BRIEF-P, return stuff suitable for disk label comment."
  (WITH-OUTPUT-TO-STRING (S)
    (UNLESS (AND BRIEF-P (EQ (PATCH-STATUS (GET-PATCH-SYSTEM-NAMED "System")) :INCONSISTENT))
      ;; If some system is inconsistent but System is not,
      ;; make sure "Bad" appears at the front.
      (DOLIST (SYS PATCH-SYSTEMS-LIST)
        (WHEN (EQ (PATCH-STATUS SYS) ':INCONSISTENT)
          (FORMAT S (IF BRIEF-P "Bad " "Don't-dump-a-band! "))
          (RETURN))))
    (DOLIST (SYS PATCH-SYSTEMS-LIST)
      (COND ((NOT (AND BRIEF-P (OR (SYSTEM-SHOULD-NOT-APPEAR-IN-DISK-LABEL (PATCH-NAME SYS))
                                   (MEMQ SYS FROZEN-PATCH-SYSTEMS-LIST))))
             (IF (NOT FIRST)
                 (SEND S :STRING-OUT (IF BRIEF-P " " ", ")))
             (SETQ FIRST NIL)
             (COND ((NULL (SETQ TEM (ASSQ (PATCH-STATUS SYS) SYSTEM-STATUS-ALIST)))
                    (SETQ TEM (STRING (PATCH-STATUS SYS))))
                   (BRIEF-P
                    (SETQ TEM (THIRD TEM)))
                   (T
                    (SETQ TEM (SECOND TEM))))
             (UNLESS (EQUAL TEM "")
               (SEND S :STRING-OUT TEM)
               (WRITE-CHAR #\SPACE S))
             (IF (NOT (AND BRIEF-P (EQUALP (PATCH-NAME SYS) "System")))
                 (FORMAT S "~A " (IF (NOT BRIEF-P) (PATCH-NAME SYS)
                                     (SYSTEM-SHORT-NAME (PATCH-NAME SYS)))))
             (FORMAT S "~D.~D"
                     (PATCH-VERSION SYS) (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST SYS)))))))
    (unless brief-p
      #+(target lambda cadr explorer)
      (format s ", Microcode ~D" %MICROCODE-VERSION-NUMBER)
      (describe-external-system s))
    ;;;Always print SYSTEM-ADDITIONAL-INFO, but:
    ;;; 1) Newlines, spaces on ends get trimmed,
    ;;; 2) Newlines inside become spaces.
    (let ((sys-info
            (substitute #\space #\return
                        (string-trim '(#\return #\space) system-additional-info))))
      (AND (PLUSP (STRING-LENGTH sys-info))
           (FORMAT S ", ~A" sys-info)))))

(DEFUN DESCRIBE-SYSTEM-VERSIONS (&OPTIONAL (S *STANDARD-OUTPUT*) IGNORE-FROZEN
                                 &AUX (MAX 9) NAME-LIST STATUS UL)
  "Print the version numbers of all patchable systems, one per line, on stream S.
The microcode version number and some other suitable information is also included."
  (SETQ UL (IF IGNORE-FROZEN
              (SUBSET #'(LAMBDA (X) (NOT (MEMQ X FROZEN-PATCH-SYSTEMS-LIST)))
                      PATCH-SYSTEMS-LIST)
             PATCH-SYSTEMS-LIST))
  (SETQ NAME-LIST (MAKE-LIST (LENGTH UL)))
  (DO ((SYS UL (CDR SYS))
       (NAM NAME-LIST (CDR NAM)))
      ((NULL SYS))
    (SETQ STATUS (SECOND (ASSQ (PATCH-STATUS (CAR SYS)) SYSTEM-STATUS-ALIST)))
    (SETF (CAR NAM)
          (WITH-OUTPUT-TO-STRING (STREAM)
            (WHEN (PLUSP (LENGTH STATUS))
              (SEND STREAM :STRING-OUT STATUS)
              (WRITE-CHAR #\SPACE STREAM))
            (SEND STREAM :STRING-OUT (PATCH-NAME (CAR SYS)))))
    (SETQ MAX (MAX (LENGTH (CAR NAM)) MAX)))
  (DO ((SYS UL (CDR SYS))
       (NAM NAME-LIST (CDR NAM)))
      ((NULL SYS))
    (FORMAT S "~& ~A" (CAR NAM))
    (DOTIMES (I (- MAX (LENGTH (CAR NAM))))
      (WRITE-CHAR #\SPACE S))
    (FORMAT S " ~3D.~D"
            (PATCH-VERSION (CAR SYS))
            (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST (CAR SYS))))))
  #+(target lambda cadr explorer)
  (progn
    (FORMAT S "~& Microcode")
    (DOTIMES (I (- MAX 9. 1))
      (WRITE-CHAR #\SPACE S))
    (FORMAT S " ~4D" %MICROCODE-VERSION-NUMBER))
  #+(target lambda)
  ;print SDU boot tape version if set up
  (let ((boot-tape-version (get-boot-tape-version-number)))
    (when boot-tape-version
      (format s "~& SDU")
      (dotimes (i (- max 3.))
        (write-char #\space s))
      (format s " ~3D.~D"
              (ldb (BYTE 16. 16.) boot-tape-version)
              (ldb (BYTE 16. 0.) boot-tape-version))))
  )

; Moved to SYS: IO; DISK on 10/9/86  (which is loaded before this file; avoid problem
; with Site initializations while building cold load).

;(defun get-boot-tape-version-number ()
;  "return boot tape version number, or nil if not available
;   version is 32-bits, major.minor"
;  (select-processor
;    (:lambda
;      (when (and (> (%system-configuration-size *sys-conf*)
;                   %system-configuration-newboot-version-number)
;                (neq 0 (ldb #o2020 (%system-configuration-newboot-version-number *sys-conf*))))
;       (%system-configuration-newboot-version-number *sys-conf*)))
;    ((:explorer :cadr))))

(defun get-sdu-rom-version-number ()
  "return sdu rom version number, or nil if not available"
  (select-processor
    (:lambda
      (when (and (> (%system-configuration-size *sys-conf*)
                    %system-configuration-sdu-rom-version-number)
                 (neq 0 (%system-configuration-sdu-rom-version-number *sys-conf*)))
        (%system-configuration-sdu-rom-version-number *sys-conf*)))
    ;;N/A:
    ((:explorer :cadr :falcon))))

(defun describe-external-system (&optional (s *standard-output*))
  #+(target lambda)
  ;;Include SDU Boot Tape and ROM version numbers if set up
  (let ((boot-tape-version (get-boot-tape-version-number))
        (rom-version (get-sdu-rom-version-number)))
    (when boot-tape-version
      (format s ", SDU Boot Tape ~D.~D"
              (ldb (BYTE 16. 16.) boot-tape-version)
              (ldb (BYTE 16. 0.)  boot-tape-version)))
    (when rom-version
      (format s ", SDU ROM ~D" rom-version)))
  ;;;|||KMC: Hold this thought. 10/18/88
  #+(target falcon)
  (format s ",K/Mac Interface ??"))

(DEFUN PRINT-SYSTEM-MODIFICATIONS (&REST SYSTEM-NAMES)
  "Print descriptions of all loaded patches of the systems in SYSTEM-NAMES, or all systems."
  (IF (NULL SYSTEM-NAMES)
      (DOLIST (PATCH PATCH-SYSTEMS-LIST)
        (PRINT-PATCHES (PATCH-NAME PATCH)))
    (DOLIST (PAT-NAME SYSTEM-NAMES)
      (PRINT-PATCHES PAT-NAME))))

(DEFUN GET-SYSTEM-VERSION (&OPTIONAL (SYSTEM "System"))
  "Returns the major and minor version numbers and status of the system named SYSTEM.
This describes what is currently loaded, not the most recent ones on disk.
Returns NIL if no such patchable system exists."
  (DECLARE (VALUES MAJOR MINOR STATUS))
  (LET ((PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM T T)))
    (IF PATCH
        (VALUES (PATCH-VERSION PATCH)
                (VERSION-NUMBER (FIRST (PATCH-VERSION-LIST PATCH)))
                (PATCH-STATUS PATCH)))))

(DEFUN SET-SYSTEM-STATUS (SYSTEM NEW-STATUS &OPTIONAL MAJOR-VERSION &AUX PATCH PATCH-DIR)
  "Change the status of the system named SYSTEM to NEW-STATUS.
NEW-STATUS should be :EXPERIMENTAL, :BROKEN, :RELEASED or :OBSOLETE.
If MAJOR-VERSION is specified, the status of that major version is set.
Otherwise the status of the currently loaded major version is set.
This modifies the patch directory files."
  (UNLESS (ASSQ NEW-STATUS SYSTEM-STATUS-ALIST)
    (FERROR "~S is not a defined system status." NEW-STATUS))
  (SETQ PATCH (GET-PATCH-SYSTEM-NAMED SYSTEM))
  (IF (AND MAJOR-VERSION ( MAJOR-VERSION (PATCH-VERSION PATCH)))
      (SETQ PATCH (MAKE-PATCH-SYSTEM :NAME SYSTEM :VERSION MAJOR-VERSION :STATUS NEW-STATUS))
    ;; Also change in core copy
    (SETF (PATCH-STATUS PATCH) NEW-STATUS))
  (SETQ PATCH-DIR (READ-PATCH-DIRECTORY PATCH))
  (SETF (PATCH-DIR-STATUS PATCH-DIR) NEW-STATUS)
  (WRITE-PATCH-DIRECTORY PATCH PATCH-DIR))

(ADD-INITIALIZATION 'RECORD-SYSTEM-VERSION
                    '(SETF (AREF #'SYSTEM-COMMUNICATION-AREA %SYS-COM-MAJOR-VERSION)
                           (GET-SYSTEM-VERSION))
                    :BEFORE-COLD)
