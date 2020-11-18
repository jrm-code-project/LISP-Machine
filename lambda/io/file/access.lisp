;;; -*- Mode:LISP; Package:FILE-SYSTEM; Readtable:ZL; Base:10 -*-
;;; (c) 1984 Massachusetts Institute of Technology
;;;
;;; This is SYS: IO; FILE; ACCESS
;;;
;;; Flavor definitions for file access.  Also includes useful functions and variables that
;;; are needed by various kinds of access.
;;; Definitions include the FILE-HOST-MIXIN, basic access objects, and useful flavors of
;;; host units.  Aids for error handling/signalling, password guessing (your own, silly !).
;;; ``Data Connection'' defstruct.  Host flavors for LispM-supported file systems.
;;; Dormant file service connection cleanup process.  For examples of using these functions
;;; and flavors, look at SYS: NETWORK; CHAOS; QFILE or SYS: FILE; FSACC

;;>> BARF!
(defmacro file-operation-retry (&body body)
  "Like ERROR-RESTART for condition FILE-ERROR, but uses proceed-type :RETRY-FILE-OPERATION."
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (catch ',tag
             ;; BARF!
             (with-stack-list* (eh:condition-resume-handlers
                                 '(file-error :retry-file-operation t
                                              ("Retry the operation on the same file.")
                                              dbg::catch-error-restart-throw ,tag)
                                 eh:condition-resume-handlers)
               (return-from ,tag (progn . ,body))))
           (go ,tag)))))

(defmacro handling-file-errors ((error-p) &body body)
  (let ((tag (gensym))
        (error (gensym)))
    `(condition-case-if (not ,error-p) (,error)
         (condition-resume-if ,error-p '((file-error remote-network-error)
                                         :retry-file-operation t
                                         ("Retry the operation on the same file.")
                                         (lambda (ignore) (throw ',tag nil)))
           (block ,tag
             (loop
               (catch ',tag
                 (return-from ,tag
                   (progn ,@body))))))
       (file-error ,error))))

;;; This is the ``library'' function that should be used by protocol interfaces
;;; CONDITION-NAME is a condition name, and ERROR-STRING is a string decribing just the
;;; that error, not any files involved.  The first of MAKE-CONDITION-ARGS is the operation
;;; name; extra arguments are dependent on what CONDITION-NAME provides.  If
;;; CONDITION-NAME is NIL, then the condition FS:FILE-OPERATION-FAILURE-1 is used.
;;; PROCEEDABLE can be a list of proceed types, NIL, or an non-NIL atom (usually T) meaning
;;; to use the proceed type :RETRY-FILE-OPERATION.
(DEFPROP FILE-PROCESS-ERROR T :ERROR-REPORTER)
(DEFUN FILE-PROCESS-ERROR (CONDITION-NAME ERROR-STRING &OPTIONAL PATHNAME-OR-STREAM
                           PROCEEDABLE NOERROR &REST MAKE-CONDITION-ARGS &AUX WHO-FOR)
  (COND ((TYPEP PATHNAME-OR-STREAM 'PATHNAME)
         (SETQ WHO-FOR PATHNAME-OR-STREAM))
        ((TYPEP PATHNAME-OR-STREAM 'SI:FILE-STREAM-MIXIN)
         (SETQ WHO-FOR (SEND PATHNAME-OR-STREAM :PATHNAME)))
        (T (SETQ WHO-FOR PATHNAME-OR-STREAM)))
  (AND WHO-FOR
       (SETQ ERROR-STRING (STRING-APPEND ERROR-STRING " for " (STRING WHO-FOR))))
  (LET ((CONDITION (APPLY 'MAKE-CONDITION (OR CONDITION-NAME 'FILE-OPERATION-FAILURE-1)
                          "~A"  WHO-FOR MAKE-CONDITION-ARGS)))
    (SEND CONDITION :SET-FORMAT-ARGS (LIST ERROR-STRING))
    (IF NOERROR CONDITION
      (SIGNAL CONDITION :PROCEED-TYPES
              (COND ((CONSP PROCEEDABLE) PROCEEDABLE)
                    (PROCEEDABLE '(:RETRY-FILE-OPERATION)))))))

;;; This is the flavor for a host that can have pathnames.
;;; ++SAMPLE-PATHNAME instance variable not patched in 109.  Per-host pathname hash table
;;; support not yet implemented.
(defflavor pathname-host-mixin ((pathname-hash-table nil) (sample-pathname nil)) ()
  (:required-flavors si:basic-host)
  (:required-methods :pathname-flavor))

(defmethod (pathname-host-mixin :SAMPLE-PATHNAME) ()
  (or sample-pathname
      (setq sample-pathname (make-pathname-internal self nil nil nil nil nil))))

(defmethod (pathname-host-mixin :reset-sample-pathname) ()
  (setq sample-pathname nil))

(defvar *session-properties* '(connected-directory accessed-directory
                               capabilities-alist)
  "A list of host properties that are used to record the state of a file session.
User-ID and password properties are kept in other places for now.")

(DEFFLAVOR FILE-HOST-MIXIN
           ((ACCESS NIL)                        ;Contains HOST-UNITs
            (APPROPRIATE-ACCESS-FLAVORS NIL))   ;In case of failure
        (pathname-host-mixin)
  (:REQUIRED-METHODS :HSNAME-INFORMATION :SYSTEM-TYPE) ; for QFILE.
  (:REQUIRED-INSTANCE-VARIABLES SI:PROPERTY-LIST)
  (:REQUIRED-FLAVORS SI:BASIC-HOST SI:PROPERTY-LIST-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES))

(defmethod (file-host-mixin :clear-file-session-data) ()
  (dolist (p *session-properties*)
    (remf si:property-list p)))

;;; This method may be overidden by other flavors, or by site information to distinguish
;;; slightly differing versions of file systems.
(defmethod (file-host-mixin :file-system-type) ()
  (getf si:property-list 'file-system-type (send self :system-type)))

(defmethod (file-host-mixin :set-file-system-type) (type)
  (let ((otype (send self :file-system-type)))
    (setf (getf si:property-list 'file-system-type) type)
    (unless (eq type otype)
      (send-if-handles self :reset-sample-pathname))))

(defmethod (file-host-mixin :reset-file-system-type) ()
  (let ((otype (send self :file-system-type)))
    (remf si:property-list 'file-system-type)
    (unless (eq (send self :system-type) otype)
      (send-if-handles self :reset-sample-pathname))))

(DEFMETHOD (FILE-HOST-MIXIN :FILE-HOST-P) () T)
(DEFMETHOD (FILE-HOST-MIXIN :MAX-DATA-CONNECTIONS) () 20.)

(DEFMETHOD (FILE-HOST-MIXIN :GET-ACCESS) ()
  (OR ACCESS (SETQ ACCESS (SEND SELF :DETERMINE-ACCESS))))

(defmethod (file-host-mixin :set-access) (flavor)
  (setq appropriate-access-flavors (determine-file-access-flavors self))
  (when (member flavor appropriate-access-flavors)
    (when access                                ;forget old flavor
      (send self :send-if-handles :reset :clear-session))
    (setq access (make-instance flavor :host self))))

(DEFMETHOD (FILE-HOST-MIXIN :ACCESS-OPERATION) (OP &REST ARGS)
  (LEXPR-SEND (SEND SELF :GET-ACCESS) OP ARGS))

(DEFMETHOD (FILE-HOST-MIXIN :DETERMINE-ACCESS) ()
  (SETQ APPROPRIATE-ACCESS-FLAVORS (DETERMINE-FILE-ACCESS-FLAVORS SELF))
  (IF APPROPRIATE-ACCESS-FLAVORS
      (SETQ ACCESS (MAKE-INSTANCE (FIRST APPROPRIATE-ACCESS-FLAVORS) :HOST SELF))
    (FERROR () "No file access path to ~A available" SELF)))

(DEFMETHOD (FILE-HOST-MIXIN :RETRY-ACCESS) ()
  (DOLIST (F APPROPRIATE-ACCESS-FLAVORS)
    (UNLESS (EQ (TYPE-OF ACCESS) F)
      (RETURN (SETQ ACCESS (MAKE-INSTANCE F :HOST SELF))))))

(DEFMETHOD (FILE-HOST-MIXIN :ASSURE-ACCESS) ()
  (SEND SELF :ACCESS-OPERATION :ASSURE-ACCESS))

(DEFMETHOD (FILE-HOST-MIXIN :PEEK-FILE-SYSTEM) ()
  (AND ACCESS (SEND ACCESS :PEEK-FILE-SYSTEM)))

(DEFMETHOD (FILE-HOST-MIXIN :PEEK-FILE-SYSTEM-HEADER) ()
  (AND ACCESS (SEND ACCESS :PEEK-FILE-SYSTEM-HEADER)))

(DEFVAR *FILE-ACCESS-PATHS* () "An alist of (ACCESS-FLAVOR DESIRABILITY . CONDITIONS)")

(DEFMACRO DEFINE-FILE-ACCESS (&WHOLE ENTRY ACCESS-FLAVOR DESIRABILITY &BODY CONDITIONS)
  "Define a file access path, to be possibly used by pathnames.
ACCESS-FLAVOR should be a flavor of file access (see FS:BASIC-ACCESS).  It should
accept :HOST as an init keyword.  The desirability is expressed as a flonum in the range
/(0, 1].  CONDITIONS can be any of the following:
 :LOCAL                 The host must be local.  Usually the desirability of local access
        should be 1.0.
 (:FILE-SYSTEM-TYPE . types) The file system flavor must be one of the listed types
 (:NETWORK . network-types) The host must be on one of the networks of the listed types
 (:PROTOCOL . protocols) The host must support one of the listed protocols."
  CONDITIONS
  (CHECK-TYPE DESIRABILITY (FLOAT (0) 1))
  `(PROGN
     (SETQ *FILE-ACCESS-PATHS* (DELQ (ASSQ ',ACCESS-FLAVOR *FILE-ACCESS-PATHS*)
                                     *FILE-ACCESS-PATHS*))
     ;; Don't use PUSH here - this is used before PUSH is loaded.
     (SETQ *FILE-ACCESS-PATHS* (CONS ',(CDR ENTRY) *FILE-ACCESS-PATHS*))))

(DEFUN PERMISSIBLE-ACCESS-PATH-FLAVOR-P (HOST ACCESS-D)
  "Return NIL or a list (FITNESS FLAVOR)"
  (DO ((L (CDDR ACCESS-D) (CDR L)))
      ((NULL L)
       (LIST (CADR ACCESS-D)
             (CAR ACCESS-D)))
    (OR (FUNCALL (GET (IF (ATOM (CAR L)) (CAR L) (CAAR L))
                      'FILE-ACCESS-CONDITION
                      'DEFAULT-FILE-ACCESS-CONDITION-CHECK)
                 HOST
                 (CAR L))
        (RETURN NIL))))


(DEFUN DETERMINE-FILE-ACCESS-FLAVORS (HOST)
  (MAPCAR #'CADR
          (SORTCAR (REMQ NIL (MAPCAR #'(LAMBDA (ACCESS)
                                         (PERMISSIBLE-ACCESS-PATH-FLAVOR-P HOST ACCESS))
                                     *FILE-ACCESS-PATHS*))
                   #'>)))

(DEFUN DEFAULT-FILE-ACCESS-CONDITION-CHECK (HOST ITEM)
  ;; THIS IS FOR AS-YET-UNIMPLEMENTED CONDITIONS
  HOST ITEM
  T)

(DEFUN (:LOCAL FILE-ACCESS-CONDITION) (HOST IGNORE)
  (OR (EQ SI:LOCAL-HOST HOST)
      (LET ((CHAOS (SEND HOST :SEND-IF-HANDLES :CHAOS-ADDRESS))) ; KLUDGE!
        (AND CHAOS
             (IF SI:LOCAL-HOST
                 (= CHAOS (SEND SI:LOCAL-HOST :CHAOS-ADDRESS))
               NIL)))))

(DEFUN (:FILE-SYSTEM-TYPE FILE-ACCESS-CONDITION) (HOST ITEM)
  (MEMQ (SEND HOST :FILE-SYSTEM-TYPE) (CDR ITEM)))


(DEFUN (:NETWORK FILE-ACCESS-CONDITION) (HOST ITEM)
  (DOLIST (NETWORK (CDR ITEM))
    (IF (NET:NETWORK-PATH-AVAILABLE NETWORK HOST)
        (RETURN T))))

(DEFMETHOD (FILE-HOST-MIXIN :HOST-UNITS) ()
  (AND ACCESS (SEND ACCESS :SEND-IF-HANDLES :HOST-UNITS)))

;;; If the optional argument is :CLEAR-SESSION, then things like capabilities
;;; are forgotten too.
(DEFMETHOD (FILE-HOST-MIXIN :RESET) (&OPTIONAL FORGET-ACCESS-P)
  (WHEN ACCESS
    (SEND ACCESS :RESET)
    (AND FORGET-ACCESS-P (SETQ ACCESS NIL
                               APPROPRIATE-ACCESS-FLAVORS NIL)))
  (when (eq forget-access-p :clear-session)
    (send self :clear-file-session-data)))

;;; This also frees up any slots marked as open
(DEFMETHOD (FILE-HOST-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT))
  (AND ACCESS (SEND ACCESS :CLOSE-ALL-FILES MODE)))

(DEFMETHOD (FILE-HOST-MIXIN :OPEN-STREAMS) ()
  (AND ACCESS (SEND ACCESS :OPEN-STREAMS)))

(DEFMETHOD (FILE-HOST-MIXIN :RESET-DORMANT-HOST-UNITS) ()
  (AND ACCESS (SEND ACCESS :SEND-IF-HANDLES :RESET-DORMANT-HOST-UNITS)))

(DEFMETHOD (FILE-HOST-MIXIN :ANY-DORMANT-HOST-UNITS-P) ()
  (AND ACCESS (SEND ACCESS :SEND-IF-HANDLES :ANY-DORMANT-HOST-UNITS-P)))

;;; This method is currently QFILE specific
(DEFMETHOD (FILE-HOST-MIXIN :HSNAME-INFORMATION) (UNIT STR IDX)
  (LET* ((HSNAME (SUBSTRING STR (SETQ IDX (1+ IDX))
                            (SETQ IDX (STRING-SEARCH-CHAR #/NEWLINE STR IDX))))
         (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA)
         (HSNAME-PATHNAME (SEND SELF :HSNAME-PATHNAME HSNAME (SEND UNIT :HOST)))
         (PERSONAL-NAME (SUBSTRING STR (SETQ IDX (1+ IDX))
                                   (SETQ IDX (STRING-SEARCH-CHAR #/NEWLINE STR IDX))))
         (GROUP-AFFILIATION (IF (OR (NULL IDX) (= IDX (1- (STRING-LENGTH STR))))
                                #/SP
                              (char STR (1+ IDX)))))
    (SETQ IDX (STRING-SEARCH ", " PERSONAL-NAME)
          STR (NSUBSTRING PERSONAL-NAME 0 IDX))
    (AND IDX (SETQ STR (STRING-APPEND (NSUBSTRING PERSONAL-NAME (+ IDX 2)) #/SP STR)))
    (VALUES HSNAME-PATHNAME PERSONAL-NAME GROUP-AFFILIATION STR)))

(DEFMETHOD (FILE-HOST-MIXIN :LOGIN-UNIT) (UNIT LOGIN-P)
  ;; Connection is used up when logging out
  (AND (SEND UNIT :VALID-CONTROL-CONNECTION-P)
       (IF LOGIN-P
           (SEND UNIT :LOGIN LOGIN-P SELF)
         (SEND UNIT :CLOSE-CONTROL-CONNECTION)))
  T)

;;; Fascist caste system support. Boo Hiss etc
(DEFFLAVOR CASTE-FILE-HOST-MIXIN
           ()
           ()
  (:REQUIRED-FLAVORS FILE-HOST-MIXIN)
  (:REQUIRED-METHODS :DEFAULT-CAPABILITIES))

(DEFMETHOD (CASTE-FILE-HOST-MIXIN :ENABLE-CAPABILITIES) (&REST CAPABILITIES)
  (SEND SELF :ACCESS-OPERATION :ENABLE-CAPABILITIES
        (OR CAPABILITIES (SEND SELF :DEFAULT-CAPABILITES))))

(DEFMETHOD (CASTE-FILE-HOST-MIXIN :DISABLE-CAPABILITIES) (&REST CAPABILITIES)
  (SEND SELF :ACCESS-OPERATION :DISABLE-CAPABILITIES CAPABILITIES))

(DEFUN FORGET-PASSWORD (NEW-USER-ID HOST)
  (LET ((ALIST-ELEMENT (ASS 'EQUALP (LIST NEW-USER-ID (SEND HOST :NAME))
                            USER-HOST-PASSWORD-ALIST)))
    (IF ALIST-ELEMENT
        (SETQ USER-HOST-PASSWORD-ALIST
              (DELQ ALIST-ELEMENT USER-HOST-PASSWORD-ALIST)))))

(DEFUN GUESS-PASSWORD-MAYBE (NEW-USER-ID HOST &AUX PASSWORD)
  (OR (CADR (ASS 'EQUALP
                 (LIST NEW-USER-ID (SEND HOST :NAME))
                 USER-HOST-PASSWORD-ALIST))
      ;; None remembered => guess, except on Multics
      ;; since multics would hassle the guy if it's wrong.
      (IF (EQ (SEND HOST :SYSTEM-TYPE) :MULTICS) "")
      ;; Try guessing password same as on some other host.
      (CADR (CAR USER-HOST-PASSWORD-ALIST))
      ;; Try guessing password same as uname or last part of it.
      (PROG1
        (SETQ PASSWORD
              (SUBSTRING NEW-USER-ID
                         (1+ (OR (STRING-REVERSE-SEARCH #/. NEW-USER-ID)
                                 -1))))
        (PUSH (LIST (LIST NEW-USER-ID (SEND HOST :NAME))
                    PASSWORD)
              USER-HOST-PASSWORD-ALIST))))

;;; This function should be useful for the :LOGIN methods of host units.  UNAME-HOST and
;;; HOST are usually the same, unless UNAME-HOST is the symbol FS:ITS
(DEFUN DETERMINE-USER-ID-AND-PASSWORD (UNAME-HOST HOST NEED-PASSWORD
                                       &AUX PASSWORD NEW-USER-ID ENABLE-CAPABILITIES)
  (DECLARE (VALUES NEW-USER-ID PASSWORD ENABLE-CAPABILITIES))
  (SETQ NEW-USER-ID (CDR (ASSQ UNAME-HOST USER-UNAMES)))
  (COND ((EQ UNAME-HOST 'ITS)
         (UNLESS NEW-USER-ID
           ;; This is an ITS; ask for the user name for all ITSes.
           (FORMAT *QUERY-IO* "~&ITS uname (default ~A): " USER-ID)
           (LET ((NID (READLINE-TRIM *QUERY-IO*)))
             (SETQ NEW-USER-ID (IF (EQUAL NID "") USER-ID NID)))))
        ;; Not an ITS: if we don't know user id or if password failed,
        ;; ask for one or both.
        ((OR NEED-PASSWORD
             ;(NULL NEW-USER-ID)
             )
         (MULTIPLE-VALUE (NEW-USER-ID PASSWORD ENABLE-CAPABILITIES)
           (FILE-GET-PASSWORD USER-ID UNAME-HOST)))
        ;; We know the user id; use remembered password if any.
        ((NULL PASSWORD)
         (SETQ PASSWORD (GUESS-PASSWORD-MAYBE (OR NEW-USER-ID USER-ID) HOST))))
  (OR NEW-USER-ID (SETQ NEW-USER-ID USER-ID))
  (FILE-HOST-USER-ID NEW-USER-ID HOST)
  (VALUES NEW-USER-ID PASSWORD ENABLE-CAPABILITIES))

(DEFUN SET-LOCAL-VARIABLES-FROM-HOST-INFO
       (HOST NEW-USER-ID HSNAME-PATHNAME PERSONAL-NAME GROUP PERSONAL-NAME-1 &AUX ITEM)
  ;; Record info about this user
  ;; only if host login name equals name given to LOGIN.
  (AND (EQUAL USER-ID NEW-USER-ID)
       (EQUAL USER-PERSONAL-NAME "")
       (SETQ USER-PERSONAL-NAME PERSONAL-NAME
             USER-GROUP-AFFILIATION GROUP
             USER-PERSONAL-NAME-FIRST-NAME-FIRST PERSONAL-NAME-1))
  (SETQ CHAOS:GIVE-FINGER-SAVED-USER-ID T)      ;Clear cache
  ;; If this is the user's login host but the host user id is not the one specified in LOGIN,
  ;; do not accept the file server's suggested home dir
  ;; since it is based on the file server login id.
  (AND (EQ HOST USER-LOGIN-MACHINE)
       (NOT (EQUAL USER-ID NEW-USER-ID))
       (SETQ HSNAME-PATHNAME (QUIET-USER-HOMEDIR HOST)))
  ;; Record homedir for this host.
  (IF (SETQ ITEM (ASSQ HOST USER-HOMEDIRS))
      (setf (cdr ITEM) HSNAME-PATHNAME)
      (PUSH (CONS HOST HSNAME-PATHNAME) USER-HOMEDIRS))
  (UNLESS (AND (CONSP *DEFAULT-PATHNAME-DEFAULTS*)
               (ASSQ HOST *DEFAULT-PATHNAME-DEFAULTS*))
    (PUSH (CONS HOST HSNAME-PATHNAME) *DEFAULT-PATHNAME-DEFAULTS*)))

(DEFFLAVOR BASIC-ACCESS
           (HOST)
           ()
  (:INITABLE-INSTANCE-VARIABLES HOST)
  (:GETTABLE-INSTANCE-VARIABLES HOST)
  (:REQUIRED-METHODS
    :RESET              ; () Close all streams, reset host units
    :OPEN-STREAMS       ; () List of open streams
    :CLOSE-ALL-FILES    ; ()
    :OPEN               ; (FILE PATHNAME &REST OPTIONS) The PATHNAME argument is for the
                        ; the PATHNAME which was originally requested; the usual ``different''
                        ; thing for this to be is a logical pathname.
                        ; FILE seems to be ignored, but it's being kept in for now
    :RENAME             ; (FILE NEW-PATHNAME ERROR-P)
    :DELETE             ; (FILE ERROR-P)
    :COMPLETE-STRING    ; (FILE STRING OPTIONS) FILE is mostly for defaulting
    :CHANGE-PROPERTIES  ; (FILE ERROR-P &REST PROPERTIES)
;Is :HOMEDIR really used??
    :HOMEDIR            ; (USER) Ignored most of the time
    :CREATE-LINK        ; (FILE LINK-TO ERROR)
    :EXPUNGE            ; (FILE ERROR)
    :REMOTE-CONNECT     ; (FILE ERROR ACCESS-MODE &OPTIONAL UNIT) Connect to the directory
                        ; FILE.  If ACCESS-MODE is T, then do TOPS-20 access.  If UNIT
                        ; is given, connect for just that unit.  (This argument should be
                        ; be ignored if it does not make sense the access object.)
    :CREATE-DIRECTORY   ; (FILE ERROR)
    :DIRECTORY-LIST     ; (FILE OPTIONS)
    :DIRECTORY-LIST-STREAM ; (FILE OPTIONS)
    :ACCESS-DESCRIPTION ; () Returns a string describing the access method (for :PRINT-SELF)
    ))

;;; An operation which is defaultly supplied (doing nothing) is :ASSURE-ACCESS, which tries
;;; to make the file access possible.  This is currently only needed by the error-table
;;; loading code, which would be screwed if the local file system were not mounted and the
;;; SYS host was the local host.
(DEFMETHOD (BASIC-ACCESS :ASSURE-ACCESS) () NIL)

;;; Two other operations that an access can handle are :ENABLE/DISABLE-CAPABILITIES.
;;; The arglist is (CAPABILITIES &OPTIONAL UNIT).  If UNIT is given, the capabilities
;;; are changed only on that unit.  The UNIT argument should be ignored by access objects
;;; for which it does not make sense.
(DEFMETHOD (BASIC-ACCESS :PRINT-SELF) (STREAM PRINDEPTH ESCAPE-P)
  PRINDEPTH
  (IF ESCAPE-P
      (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPEP)
        (FORMAT STREAM "~A access to ~A" (SEND SELF :ACCESS-DESCRIPTION) HOST))
    (FORMAT STREAM "~A access to ~A" (SEND SELF :ACCESS-DESCRIPTION) HOST)))

;;; These two methods do the basics.  However, you can make hairier methods if you wish
;;; to show access internals.  DC-ACCESS-MIXIN has a hairier version of :PEEK-FILE-SYSTEM
(DEFMETHOD (BASIC-ACCESS :PEEK-FILE-SYSTEM) ()
  (LET ((this-access self)) ; must close lambda over lexical variable, not SELF
    (LIST ()
          (TV:SCROLL-MAINTAIN-LIST
            #'(LAMBDA () (send this-access :open-streams))
            #'(LAMBDA (STREAM) (SEND STREAM :PEEK-FILE-SYSTEM 2))))))

(DEFMETHOD (BASIC-ACCESS :PEEK-FILE-SYSTEM-HEADER) ()
  (TV:SCROLL-PARSE-ITEM `(:STRING ,(FORMAT NIL "~A" SELF))))

;;; Some access methods may implement this as a separate subprotocol
(DEFMETHOD (BASIC-ACCESS :PROPERTIES) (PATHNAME ERROR-P)
  (HANDLING-FILE-ERRORS (ERROR-P)
    (LET ((DIR (SEND SELF :DIRECTORY-LIST PATHNAME (IF ERROR-P '(:DELETED)
                                                     '(:NOERROR :DELETED)))))
      (IF (CONSP DIR)
          (IF (CADR DIR)
              (VALUES (CADR DIR) (GET (CAR DIR) :SETTABLE-PROPERTIES))
            ;; It is possible for a nonexistent file to give no error
            ;; but just return an empty directory.
            (LET ((CONDITION (MAKE-CONDITION 'FILE-NOT-FOUND "~A" PATHNAME :PROPERTIES)))
              (SEND CONDITION :SET-FORMAT-ARGS
                    (LIST (FORMAT NIL "File not found for ~A" PATHNAME)))
              (IF ERROR-P (SIGNAL-CONDITION :PROCEED-TYPES ())
                CONDITION)))
        DIR))))

;;; These two operations are used to speed up Dired.
(DEFMETHOD (BASIC-ACCESS :DELETE-MULTIPLE-FILES) (ERROR-P FILES)
  (loop for file in files
        collect (send file :delete error-p)))

(defmethod (basic-access :undelete-multiple-files) (error-p files)
  (loop for file in files
        collect (send file :undelete error-p)))

;; Copied from LAD: RELEASE-3.IO.FILE; ACCESS.LISP#35 on 2-Oct-86 05:49:55
(defmethod (basic-access :multiple-file-plists) (files options)
  ;; also a speed-up you can shadow. used by MAKE-SYSTEM
  (rem #'(lambda (ignore x) (null (car x)))
       nil
       (apply #'append (mapcar #'(lambda (file) (send self :directory-list file options))
                               files))))


;;; This is currently here to abstract the usual file access protocol maintenance issues.
;;; It is not neccessary to use this, but it will be easier in many cases.
(DEFFLAVOR HOST-UNIT-ACCESS-MIXIN ; usually for remote protocols
           ((HOST-UNITS NIL))
           ()
  (:GETTABLE-INSTANCE-VARIABLES HOST-UNITS)
  (:REQUIRED-FLAVORS BASIC-ACCESS)
  (:REQUIRED-METHODS
    :HOST-UNIT-FLAVOR   ; () Returns a flavor name, accepts init keywords for the instance
                        ; variables ACCESS and HOST
    ))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :RESET) ()
  (DOLIST (HOST-UNIT HOST-UNITS)
    (SEND HOST-UNIT :RESET)))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :ANY-DORMANT-HOST-UNITS-P) ()
  (DOLIST (HOST-UNIT HOST-UNITS)
    (WHEN (SEND HOST-UNIT :DORMANT-P)
      (RETURN T))))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :RESET-DORMANT-HOST-UNITS) ()
  (DOLIST (HOST-UNIT HOST-UNITS)
    (SEND HOST-UNIT :DORMANT-RESET)))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT) &AUX CLOSED)
  (DOLIST (UNIT HOST-UNITS)
    (SETQ CLOSED (NCONC CLOSED (SEND UNIT :CLOSE-ALL-FILES MODE))))
  CLOSED)

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :NEW-HOST-UNIT) (&OPTIONAL (NOERROR-P NIL) &AUX UNIT
                                          (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  (SETQ UNIT (MAKE-INSTANCE (SEND SELF :HOST-UNIT-FLAVOR)
                            :ACCESS SELF :HOST HOST))
  (SETQ HOST-UNITS (NCONC HOST-UNITS (NCONS UNIT)))
  (AND (SEND UNIT :VALIDATE-CONTROL-CONNECTION NOERROR-P)
       UNIT))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :OPEN-STREAMS) (&AUX STREAMS)
  (DOLIST (UNIT HOST-UNITS)
    (SETQ STREAMS (NCONC STREAMS (SEND UNIT :OPEN-STREAMS))))
  STREAMS)

;;; Return a valid host unit.  If no units, make one.  If any unit is still open, use it.
;;; Errors if fails to connect unless noerror-p is T.
(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :GET-HOST-UNIT) (&OPTIONAL NOERROR-P)
  (COND ((NULL HOST-UNITS)
         (SEND SELF :NEW-HOST-UNIT NOERROR-P))
        ((LOOP FOR UNIT IN HOST-UNITS
               WHEN (SEND UNIT :VALIDATE-CONTROL-CONNECTION T)
               RETURN UNIT))
        ((NOT NOERROR-P)
         (LET ((UNIT (CAR HOST-UNITS)))
           (SEND UNIT :VALIDATE-CONTROL-CONNECTION)
           UNIT))))

(DEFMETHOD (HOST-UNIT-ACCESS-MIXIN :LOGIN-UNIT) (-UNIT- LOGIN-P UNAME-HOST)
  (SEND -UNIT- :LOGIN LOGIN-P UNAME-HOST))

(DEFCONST HOST-UNIT-LIFETIME 214500)   ;20 MINUTES.

(DEFCONST DATA-CONNECTION-LIFETIME 3600.)

;;; This flavor should be used with CHAOS FILE, and perhaps FTP.  It assumes that the
;;; the protcol uses data-connections internally, and that the DATA-CONNECTION
;;; DEFSTRUCT is used.  The flavor of host unit to be used with this should incorporate
;;; DATA-CONNECTION-MIXIN.
(DEFFLAVOR DC-ACCESS-MIXIN
           ()
  (HOST-UNIT-ACCESS-MIXIN))

;;; This can be redefined
(DEFMETHOD (DC-ACCESS-MIXIN :ACCESS-SPECIFIC-DATA-CONN-LIMIT) () #O37)

;;; Get a DATA-CONNECTION for use in DIRECTION.
;;; Make two passes over existing units, first trying open ones.
(DEFMETHOD (DC-ACCESS-MIXIN :GET-DATA-CONNECTION) (DIRECTION &OPTIONAL NOERROR-P)
  (DO-NAMED TOP ((ERROR-P NIL T)) (NIL)
    (DO ((UNITS HOST-UNITS (CDR UNITS))
         (UNIT) (DATA-CONN))
        ((NULL UNITS))
      (SETQ UNIT (CAR UNITS))
      (AND (SEND UNIT :VALIDATE-CONTROL-CONNECTION (OR NOERROR-P (NOT ERROR-P)))
           (SETQ DATA-CONN (SEND UNIT :GET-DATA-CONNECTION DIRECTION))
           (RETURN-FROM TOP (values DATA-CONN UNIT))))
    (AND NOERROR-P
         (NOT (SEND SELF :GET-HOST-UNIT T))     ;If you can't get a valid connection,
         (RETURN-FROM TOP (values NIL NIL)))            ;then you have a loosing host.
    (AND ERROR-P
         (LET* ((UNIT (SEND SELF :NEW-HOST-UNIT))
                (DATA-CONN (SEND UNIT :GET-DATA-CONNECTION DIRECTION)))
           (OR DATA-CONN
               (FERROR NIL "New unit failed to allocate data connection"))
           (RETURN-FROM TOP (values DATA-CONN UNIT))))))

(DEFMETHOD (DC-ACCESS-MIXIN :PEEK-FILE-SYSTEM) ()
  (DC-ACCESS-PEEK-FILE-SYSTEM))

;;; For things such as QFILE-ACCESS which access directories through a :DIRECTORY-STREAM
(DEFFLAVOR DIRECTORY-STREAM-ACCESS-MIXIN () ()
  (:REQUIRED-FLAVORS BASIC-ACCESS)
  (:REQUIRED-METHODS
    :DIRECTORY-STREAM-DEFAULT-PARSER ; () Returns function that takes args like SUBSTRING
    :DIRECTORY-STREAM           ; (PATHNAME OPTIONS) Stream be parsed
    :READ-DIRECTORY-STREAM-ENTRY ; (STREAM PATHNAME) PATHNAME defaults any fns encountered
    ))

(DEFMETHOD (DIRECTORY-STREAM-ACCESS-MIXIN :DIRECTORY-LIST) (PATHNAME OPTIONS &AUX DIR-LIST)
  (HANDLING-FILE-ERRORS ((NOT (MEMQ :NOERROR OPTIONS)))
    (WITH-OPEN-STREAM (STREAM (SEND SELF :DIRECTORY-STREAM PATHNAME (REMQ ':SORTED OPTIONS)))
      (IF (ERRORP STREAM) STREAM
        (SETQ DIR-LIST
              (LET ((PATHNAME (SEND STREAM :PATHNAME)))
                (LOOP AS ENTRY = (SEND SELF :READ-DIRECTORY-STREAM-ENTRY STREAM PATHNAME options)
                      UNTIL (NULL ENTRY)
                      COLLECTING ENTRY)))
        (IF (MEMQ :SORTED OPTIONS)
            (LET ((NULL-ELEM (ASSQ NIL DIR-LIST)))
              (AND NULL-ELEM (SETQ DIR-LIST (DELQ NULL-ELEM DIR-LIST)))
              (SETQ DIR-LIST (SORTCAR DIR-LIST #'PATHNAME-LESSP))
              (AND NULL-ELEM (PUSH NULL-ELEM DIR-LIST))))
        DIR-LIST))))

;; This tells READ-DIRECTORY-STREAM-ENTRY how to parse most lines of directory stream data.
(DEFMETHOD (DIRECTORY-STREAM-ACCESS-MIXIN :DEFAULT-DIRECTORY-STREAM-PARSER) ()
  'SUBSTRING)

(DEFMETHOD (DIRECTORY-STREAM-ACCESS-MIXIN :DIRECTORY-LIST-STREAM) (PATHNAME OPTIONS)
  (LET ((STREAM (SEND SELF :DIRECTORY-STREAM PATHNAME OPTIONS)))
    (IF (ERRORP STREAM)
        STREAM
      (LET-CLOSED ((INTERNAL-STREAM STREAM) (DEFAULTING-PATHNAME (SEND STREAM ':PATHNAME))
                   (ACCESS SELF))
        #'DIRECTORY-STREAM-DIRECTORY-LIST-STREAM))))

(LOCAL-DECLARE ((SPECIAL INTERNAL-STREAM DEFAULTING-PATHNAME ACCESS))
(DEFSELECT DIRECTORY-STREAM-DIRECTORY-LIST-STREAM
  (:ENTRY () (SEND ACCESS :READ-DIRECTORY-STREAM-ENTRY INTERNAL-STREAM DEFAULTING-PATHNAME))
  (:CLOSE (&OPTIONAL MODE) (SEND INTERNAL-STREAM :CLOSE MODE))) )

;;; This is for access methods that get the directory information all at once, with
;;; a :DIRECTORY-LIST option
(DEFFLAVOR DIRECTORY-LIST-MIXIN () ()
  (:REQUIRED-FLAVORS BASIC-ACCESS)
  (:REQUIRED-METHODS :DIRECTORY-LIST)) ; (PATHNAME OPTIONS) Defines :DIRECTORY-LIST-STREAM

(DEFMETHOD (DIRECTORY-LIST-MIXIN :DIRECTORY-LIST-STREAM)
           (PATHNAME OPTIONS &AUX DIRECTORY-LIST)
  (DECLARE (SPECIAL DIRECTORY-LIST))
  (IF (ERRORP (SETQ DIRECTORY-LIST (SEND SELF :DIRECTORY-LIST PATHNAME OPTIONS)))
      DIRECTORY-LIST
    (CLOSURE '(DIRECTORY-LIST) #'DEFAULT-DIRECTORY-LIST-STREAM)))

(DEFSELECT DEFAULT-DIRECTORY-LIST-STREAM
  (:ENTRY ()
    (DECLARE (SPECIAL DIRECTORY-LIST))
    (POP DIRECTORY-LIST))
  (:CLOSE . IGNORE))

(DEFFLAVOR BASIC-HOST-UNIT
        (HOST                           ;Host object
         ACCESS                         ;Access object
         CONTROL-CONNECTION             ;May be the only connection if things are multiplexed
         (LOCK NIL)                     ;Lock to insure no timing screws
         (LAST-USE-TIME (TIME)))
        ()
  (:INITABLE-INSTANCE-VARIABLES HOST ACCESS)
  (:GETTABLE-INSTANCE-VARIABLES HOST ACCESS CONTROL-CONNECTION LAST-USE-TIME)
  (:REQUIRED-METHODS
    :RESET      ; (&optional dont-unlock-lock-p) close all data streams in abort mode,
                ; close control connection
    :DORMANT-RESET ; () Reset self if dormant
    :DORMANT-P  ; () Return T if dormant.
    :CLOSE-ALL-FILES ; () close all files in this host unit, reporting to *ERROR-OUTPUT*
                ; and returning a list of closed streams
    :VALIDATE-CONTROL-CONNECTION ; (&optional no-error-p) Check that connection hasn't
                ; gone away, making a new one if necessary.  Return NIL when failure
                ; and NO-ERROR-P equal to T, otherwise barf on failure
    :OPEN-STREAMS ; () Return a list of open streams
    :OPEN-CONTROL-CONNECTION-P ; () Returns T if the connection is in an open state
    :CLOSE-CONTROL-CONNECTION ; () Close connection, for logging out
    )
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES LOCK LAST-USE-TIME))

(DEFMETHOD (BASIC-HOST-UNIT :PRINT-SELF) (STREAM PRINDEPTH ESCAPE-P)
  PRINDEPTH ESCAPE-P
  (SI:PRINTING-RANDOM-OBJECT (SELF STREAM :TYPEP)
    (FORMAT STREAM "for ~A" ACCESS)))

;;; Restore connected/accessed directories, capabilities of a unit.  If ENABLE-CAPABILITIES
;;; is T, enable capabilities for the host as a whole
(DEFMETHOD (BASIC-HOST-UNIT :RESTORE-SERVER-STATE) (ENABLE-CAPABILITIES &AUX TEM)
  (IF (SETQ TEM (GET HOST 'CONNECTED-DIRECTORY))
      (SEND ACCESS :REMOTE-CONNECT TEM T NIL SELF))
  (IF (SETQ TEM (GET HOST 'ACCESSED-DIRECTORIES))
      (DOLIST (X TEM)
        (SEND ACCESS :REMOTE-CONNECT X T T SELF)))
  (IF (SETQ TEM (GET HOST 'CAPABILITIES-ALIST))
      (DOLIST (X TEM)
        (WITH-STACK-LIST (CAP (CAR X))
          (SEND ACCESS (IF (CDR X) :ENABLE-CAPABILITIES :DISABLE-CAPABILITIES) CAP))))
  (IF ENABLE-CAPABILITIES (SEND HOST :ENABLE-CAPABILITIES)))

;;; Lock a host unit around BODY
(DEFMACRO LOCK-HOST-UNIT ((HOST-UNIT) &BODY BODY)
  (LET ((LOCK (GENSYM)) (LOCKED-P (GENSYM)) (HU (GENSYM)))
    `(LET* ((,HU ,HOST-UNIT)
            (,LOCK (LOCF (BASIC-HOST-UNIT-LOCK ,HU)))
            (,LOCKED-P NIL))
       (UNWIND-PROTECT
         (PROGN
           (COND ((NEQ (CAR ,LOCK) CURRENT-PROCESS)
                  (PROCESS-LOCK ,LOCK)
                  (SETQ ,LOCKED-P T)))
           . ,BODY)
         (SETF (BASIC-HOST-UNIT-LAST-USE-TIME ,HU) (TIME))
         (AND ,LOCKED-P (PROCESS-UNLOCK ,LOCK))))))

(DEFMETHOD (BASIC-HOST-UNIT :VALID-CONTROL-CONNECTION-P) ()
  (AND CONTROL-CONNECTION
       (SEND SELF :OPEN-CONTROL-CONNECTION-P)))

(DEFMETHOD (BASIC-HOST-UNIT :PEEK-FILE-SYSTEM) () ())

;;; A DATA-CONNECTION is associated with each data connection implementing a file stream.
;;; The two directions in the connection itself are used independently.
;;; This structure should be useful for file protocols other than CHAOS FILE
;;; You must maintain the LAST-USE-TIME so that the data connection won't be considered
;;; dormant.
(DEFSTRUCT (DATA-CONNECTION :LIST*
                            (:CONC-NAME DATA-)
                            (:ALTERANT NIL)
                            (:CONSTRUCTOR MAKE-DATA-CONNECTION
                                          (CONNECTION INPUT-HANDLE OUTPUT-HANDLE)))
  CONNECTION                                    ;The chaos connection
  INPUT-HANDLE
  OUTPUT-HANDLE
  (LAST-USE-TIME (TIME))
  (STREAM-LIST (LIST ':INPUT NIL ':OUTPUT NIL))
  )

(DEFSUBST DATA-HANDLE (DATA-CONNECTION DIRECTION)
  (SELECTQ DIRECTION
    (:INPUT (DATA-INPUT-HANDLE DATA-CONNECTION))
    (:OUTPUT (DATA-OUTPUT-HANDLE DATA-CONNECTION))))

(DEFSUBST DATA-STREAM (DATA-CONNECTION DIRECTION)
  (CADR (MEMQ DIRECTION (DATA-STREAM-LIST DATA-CONNECTION))))

(DEFUN DATA-CONNECTION-DORMANT (DATA-CONNECTION)
  (AND (NULL (DATA-STREAM DATA-CONNECTION ':INPUT))
       (NULL (DATA-STREAM DATA-CONNECTION ':OUTPUT))
       (> (TIME-DIFFERENCE (TIME) (DATA-LAST-USE-TIME DATA-CONNECTION))
          DATA-CONNECTION-LIFETIME)))

(DEFFLAVOR DATA-CONNECTION-MIXIN
           (MAX-DATA-CONNECTIONS
            DATA-CONNECTIONS)
           ()
  :GETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS BASIC-HOST-UNIT)
  (:REQUIRED-METHODS
    :FREE-DATA-CONNECTION ; (DATA-CONNECTION DIRECTION) Called when done with a
                ; DATA-CONNECTION for DIRECTION
    :CLOSE-DORMANT-DATA-CONNECTIONS ; ()
    :GET-DATA-CONNECTION ; (DIRECTION) Return a free data conn.  See the `directional' mixins
    ))

(DEFMETHOD (DATA-CONNECTION-MIXIN :INIT) (IGNORE)
  (SETQ MAX-DATA-CONNECTIONS (SEND HOST :MAX-DATA-CONNECTIONS)))

;;; Called when done with a DATA-CONNECTION for DIRECTION.
;;; If free in both directions for long enough, it is flushed for being dormant.
(DEFMETHOD (DATA-CONNECTION-MIXIN :FREE-DATA-CONNECTION) (DATA-CONNECTION DIRECTION)
  (SETF (DATA-STREAM DATA-CONNECTION DIRECTION) NIL)
  (SETF (DATA-LAST-USE-TIME DATA-CONNECTION) (TIME))
  (SETQ LAST-USE-TIME (TIME)))

(DEFMETHOD (DATA-CONNECTION-MIXIN :DORMANT-P) ()
  (AND (NOT LOCK)
       ;; Don't kill a host unit if it has more than one data connection still,
       (NULL (CDR DATA-CONNECTIONS))
       ;; or if that data connection is doing anything.
       (LET ((DATA-CONNECTION (CAR DATA-CONNECTIONS)))
         (OR (NULL DATA-CONNECTION)
             (AND (NULL (DATA-STREAM DATA-CONNECTION :INPUT))
                  (NULL (DATA-STREAM DATA-CONNECTION :OUTPUT)))))
       (> (TIME-DIFFERENCE (TIME) LAST-USE-TIME)
          HOST-UNIT-LIFETIME)))

(DEFMETHOD (DATA-CONNECTION-MIXIN :DORMANT-RESET) ()
  ;; If host unit has extra data connections, close them.
  (IF (CDR DATA-CONNECTIONS)
      (SEND SELF :CLOSE-DORMANT-DATA-CONNECTIONS))
  (LET (DO-IT)
    (WITHOUT-INTERRUPTS
      (AND (SEND SELF :CONTROL-CONNECTION)      ; Don't bother, if already reset.
           (SEND SELF :DORMANT-P)       ; Don't reset if being used or used recently.
           (SETQ DO-IT T)
           (SETF LOCK 'LOCKED-FOR-SUICIDE)))
    (IF DO-IT
        (SEND SELF :RESET))))

;;; This also frees up any slots marked as open
(DEFMETHOD (DATA-CONNECTION-MIXIN :CLOSE-ALL-FILES) (&OPTIONAL (MODE :ABORT) &AUX CLOSED)
  (DOLIST (DATA-CONN DATA-CONNECTIONS)
    (DO ((LIST (DATA-STREAM-LIST DATA-CONN) (CDDR LIST)))
        ((NULL LIST))
      (LET ((STREAM (CADR LIST)))
        (COND ((NULL STREAM))
              ((symbolp STREAM)         ;was just T.
               (SETF (CADR LIST) NIL))
              (T
               (FORMAT *ERROR-OUTPUT* "~%Closing ~S" STREAM)
               (PUSH STREAM CLOSED)
               (SEND STREAM :CLOSE MODE))))))
  CLOSED)

(DEFMETHOD (DATA-CONNECTION-MIXIN :OPEN-STREAMS) (&AUX STREAMS)
  (DOLIST (DATA-CONN DATA-CONNECTIONS)
    (DO LIST (DATA-STREAM-LIST DATA-CONN) (CDDR LIST) (NULL LIST)
        (LET ((STREAM (CADR LIST)))
          (OR (SYMBOLP STREAM)
              (PUSH STREAM STREAMS)))))
  STREAMS)

;;; This is for winning protocols like CHAOS FILE (affectionately known as QFILE)
;;; A file stream can be placed on the data connection if the desired direction on a
;;; particular data connection is free.
(DEFFLAVOR BIDIRECTIONAL-DATA-CONNECTION-MIXIN ()
           (DATA-CONNECTION-MIXIN)
  )

;;; Get a data connection for this unit.  Makes a new one if there is room in within the
;;; maximum number.  We are assumed to have recently been checked for validity.
(DEFMETHOD (BIDIRECTIONAL-DATA-CONNECTION-MIXIN :GET-DATA-CONNECTION) (DIRECTION)
  (LOCK-HOST-UNIT (SELF)
     (DO ((DATA-CONNS DATA-CONNECTIONS (CDR DATA-CONNS))
         (DATA-CONN))
        (NIL)
      (SETQ DATA-CONN (COND (DATA-CONNS (CAR DATA-CONNS))
                            ((= (LENGTH DATA-CONNECTIONS)
                                (MIN MAX-DATA-CONNECTIONS
                                     (SEND ACCESS :ACCESS-SPECIFIC-DATA-CONN-LIMIT)))
                             (RETURN NIL))
                            (T (SEND SELF :NEW-DATA-CONNECTION))))
      (COND ((NULL (DATA-STREAM DATA-CONN DIRECTION))
             (SETF (DATA-STREAM DATA-CONN DIRECTION) T) ;Mark as allocated
             (RETURN DATA-CONN))))))

;;; This is for lusing protocols like TCP FTP
;;; A file stream can be placed on the data connection if the data connection is unused in
;;; both directions
(DEFFLAVOR UNIDIRECTIONAL-DATA-CONNECTION-MIXIN ()
           (DATA-CONNECTION-MIXIN)
  )

;;; Get a data connection for this unit.  Makes a new one if there is room in within the
;;; maximum number.  We are assumed to have recently been checked for validity.  Since
;;; this is the unidirectional mixin, both directions must be free.
(DEFMETHOD (UNIDIRECTIONAL-DATA-CONNECTION-MIXIN :GET-DATA-CONNECTION) (DIRECTION)
  (LOCK-HOST-UNIT (SELF)
     (DO ((DATA-CONNS DATA-CONNECTIONS (CDR DATA-CONNS))
         (DATA-CONN))
        (NIL)
      (SETQ DATA-CONN (COND (DATA-CONNS (CAR DATA-CONNS))
                            ((= (LENGTH DATA-CONNECTIONS)
                                (MIN MAX-DATA-CONNECTIONS
                                     (SEND ACCESS :ACCESS-SPECIFIC-DATA-CONN-LIMIT)))
                             (RETURN NIL))
                            (T (SEND SELF :NEW-DATA-CONNECTION))))
      (COND ((AND (NULL (DATA-STREAM DATA-CONN :INPUT))
                  (NULL (DATA-STREAM DATA-CONN :OUTPUT)))
             (SETF (DATA-STREAM DATA-CONN DIRECTION) T) ;Mark as allocated
             (RETURN DATA-CONN))))))

;;; The following takes care of the process to do the above stuff every minute.
;;; These functions take care of closing connections that have not been used for a while.
(DEFUN RESET-DORMANT-HOST-UNITS ()
  (ERRSET
    (DOLIST (HOST *PATHNAME-HOST-LIST*)
      (SEND HOST :SEND-IF-HANDLES :RESET-DORMANT-HOST-UNITS))
    NIL))

(DEFVAR DORMANT-HOST-CONNECTION-GC-WAIT-TIME 3600.)

(DEFUN WAIT-UNTIL-TIME (TIME DELAY)
  (> (TIME-DIFFERENCE (TIME) TIME) DELAY))

(DEFUN ANY-DORMANT-HOST-UNITS ()
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (WHEN (SEND HOST :SEND-IF-HANDLES :ANY-DORMANT-HOST-UNITS-P)
      (RETURN T))))

(DEFVAR DORMANT-HOST-GC-PROCESS)

(DEFUN DORMANT-HOST-CONNECTION-GC-TOP-LEVEL ()
  (DO-FOREVER
    (IF (ANY-DORMANT-HOST-UNITS)
        (PROCESS-RUN-FUNCTION "Flush file connections" 'RESET-DORMANT-HOST-UNITS))
    (process-sleep dormant-host-connection-gc-wait-time)))

(DEFUN INIT-DORMANT-HOST-GC-PROCESS ()
  (OR (BOUNDP 'DORMANT-HOST-GC-PROCESS)
      (SETQ DORMANT-HOST-GC-PROCESS (MAKE-PROCESS "Dormant FILE connection GC")))
  (SEND DORMANT-HOST-GC-PROCESS :PRESET 'DORMANT-HOST-CONNECTION-GC-TOP-LEVEL)
  (PROCESS-RESET-AND-ENABLE DORMANT-HOST-GC-PROCESS))

(ADD-INITIALIZATION 'DORMANT-HOST-GC
                    `(INIT-DORMANT-HOST-GC-PROCESS)
                    '(:NORMAL))

(DEFUN FILE-LOGIN (LOGINP &AUX TEM)
  "Log all open host units in or out.  LOGINP = NIL means log out, otherwise log in."
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (DOLIST (PROP '(QFILE-CONNECTED-DIRECTORY QFILE-ACCESSED-DIRECTORY CAPABILITIES-ALIST))
      (AND (SETQ TEM (GET-LOCATION-OR-NIL HOST PROP)) (SETF (CONTENTS TEM) NIL)))
    (DOLIST (UNIT (SEND HOST :SEND-IF-HANDLES :HOST-UNITS))
      (SEND HOST :LOGIN-UNIT UNIT LOGINP))))

(ADD-INITIALIZATION "File Login" '(FILE-LOGIN T) '(LOGIN))
(ADD-INITIALIZATION "File Logout" '(FILE-LOGIN NIL) '(LOGOUT))

;;; Operating system particular host flavors
(DEFFLAVOR FILE-HOST-ITS-MIXIN () (FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-ITS-MIXIN :PATHNAME-FLAVOR) () 'ITS-PATHNAME)

(DEFMETHOD (FILE-HOST-ITS-MIXIN :MAX-DATA-CONNECTIONS) () 3)

(DEFMETHOD (FILE-HOST-ITS-MIXIN :LOGIN-UNIT) (UNIT LOGIN-P)
  (SEND UNIT :LOGIN LOGIN-P 'ITS))

(DEFMETHOD (FILE-HOST-ITS-MIXIN :HSNAME-INFORMATION) (UNIT STR IDX)
  (LET* ((HOST (SEND UNIT :HOST))
         (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA)
         (HSNAME (SUBSTRING STR (SETQ IDX (1+ IDX))
                            (SETQ IDX (STRING-SEARCH-CHAR #/NEWLINE STR IDX))))
         (HSNAME-PATHNAME (MAKE-PATHNAME :HOST HOST :DEVICE "DSK" :DIRECTORY HSNAME))
         (PERSONAL-NAME (SUBSTRING STR (SETQ IDX (1+ IDX))
                                   (SETQ IDX (STRING-SEARCH-CHAR #/NEWLINE STR IDX))))
         (GROUP-AFFILIATION (AREF STR (1+ IDX))))
    (SETQ IDX (STRING-SEARCH ", " PERSONAL-NAME)
          STR (NSUBSTRING PERSONAL-NAME 0 IDX))
    (AND IDX (SETQ STR (STRING-APPEND (NSUBSTRING PERSONAL-NAME (+ IDX 2)) #/SP STR)))
    (VALUES HSNAME-PATHNAME PERSONAL-NAME GROUP-AFFILIATION STR)))

(DEFMETHOD (FILE-HOST-ITS-MIXIN :LOGICALLY-BACKTRANSLATE-HOST-DEV-DIR)
           (PHYSICAL-HOST PHYSICAL-DEVICE PHYSICAL-DIRECTORY)
  (IF (AND (EQ PHYSICAL-HOST SELF)
           (MEMQ PHYSICAL-DEVICE '(NIL :UNSPECIFIC)))
      (VALUES SELF "DSK" PHYSICAL-DIRECTORY)))

(DEFMETHOD (FILE-HOST-ITS-MIXIN :GENERIC-BASE-TYPE) (FILE-TYPE)
  (IF (ASSOC FILE-TYPE *GENERIC-BASE-TYPE-ALIST*)
      :UNSPECIFIC  ;on ITS, we cant distinguish base types, since name2 is frequently
    FILE-TYPE))     ; used as a version number.

(DEFFLAVOR FILE-HOST-TOPS20-MIXIN () (CASTE-FILE-HOST-MIXIN FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-TOPS20-MIXIN :DEFAULT-CAPABILITIES) () '("OPERATOR" "WHEEL"))

(DEFMETHOD (FILE-HOST-TOPS20-MIXIN :PATHNAME-FLAVOR) () 'TOPS20-PATHNAME)

(DEFMETHOD (FILE-HOST-TOPS20-MIXIN :MAX-DATA-CONNECTIONS) () 8)

(DEFFLAVOR FILE-HOST-TENEX-MIXIN () (FILE-HOST-TOPS20-MIXIN))

(DEFMETHOD (FILE-HOST-TENEX-MIXIN :PATHNAME-FLAVOR) () 'TENEX-PATHNAME)

(DEFFLAVOR FILE-HOST-VMS-MIXIN () (CASTE-FILE-HOST-MIXIN FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-VMS-MIXIN :PATHNAME-FLAVOR) () 'VMS-PATHNAME)

(DEFMETHOD (FILE-HOST-VMS-MIXIN :DEFAULT-CAPABILITIES) () '("SYSPRV"))

;;; FOO, This could be any number at all, depending on the quota assigned.....
(DEFMETHOD (FILE-HOST-VMS-MIXIN :MAX-DATA-CONNECTIONS) () 10.)

(DEFFLAVOR FILE-HOST-UNIX-MIXIN () (FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-UNIX-MIXIN :PATHNAME-FLAVOR) () 'UNIX-PATHNAME)

(DEFFLAVOR FILE-HOST-MULTICS-MIXIN () (FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-MULTICS-MIXIN :PATHNAME-FLAVOR) () 'MULTICS-PATHNAME)

;;; Mixin for hosts that knows how to name itself.
(DEFFLAVOR FILE-HOST-LISPM-MIXIN () (FILE-HOST-MIXIN))

(DEFMETHOD (FILE-HOST-LISPM-MIXIN :PATHNAME-HOST-NAMEP) (STRING)
  (AND (EQ SELF SI:LOCAL-HOST)
       (STRING-EQUAL STRING "LM")))

(DEFMETHOD (FILE-HOST-LISPM-MIXIN :PRIMARY-DEVICE) () "DSK")

;;;; FILE protocol support.
(DEFMETHOD (FILE-HOST-LISPM-MIXIN :MAX-DATA-CONNECTIONS) () #O37)

(DEFMETHOD (FILE-HOST-LISPM-MIXIN :LOGIN-UNIT)
           (UNIT LOGIN-P &AUX TEM (DEFAULT-CONS-AREA SYS:BACKGROUND-CONS-AREA))
  ;; Don't confuse the user by asking for UNAME and PASSWORD if he's logged in elsewhere.
  (AND (SETQ TEM (COND ((NOT (EQUAL USER-ID "")) USER-ID)
                       ((CDR (ASSQ 'ITS USER-UNAMES)))
                       ((CDAR USER-UNAMES))))
       (PUSH (CONS SELF TEM) USER-UNAMES))
  ;; Connection is used up when logging out
  (AND (SEND UNIT :VALID-CONTROL-CONNECTION-P)
       (IF LOGIN-P
           (SEND UNIT :LOGIN LOGIN-P SELF)
         (SEND UNIT :CLOSE-CONTROL-CONNECTION)))
  T)

;;; The Fileserver doesn't supply the user name information, so might as well use
;;; whatever's hanging around.
(DEFMETHOD (FILE-HOST-LISPM-MIXIN :HSNAME-INFORMATION) (IGNORE STR IDX)
  (VALUES (PARSE-PATHNAME (SUBSTRING STR (1+ (SETQ IDX (1+ IDX)))
                                             (STRING-SEARCH-CHAR #/NEWLINE STR (1+ IDX)))
                          ;; better parse with-respect-to SELF
                          SELF)
          USER-PERSONAL-NAME USER-GROUP-AFFILIATION
          USER-PERSONAL-NAME-FIRST-NAME-FIRST))

(DEFMETHOD (FILE-HOST-LISPM-MIXIN :PATHNAME-FLAVOR) ()
  (GET (SEND SELF :FILE-SYSTEM-TYPE) 'LISPM-PATHNAME-FLAVOR))


;;;; Predefined host flavors
(DEFFLAVOR ITS-HOST () (SI:HOST-ITS-MIXIN FILE-HOST-ITS-MIXIN SI:HOST))
(DEFPROP :ITS ITS-HOST SI:HOST-FLAVOR)

(DEFFLAVOR TOPS20-HOST () (SI:HOST-TOPS20-MIXIN FILE-HOST-TOPS20-MIXIN SI:HOST))
(DEFPROP :TOPS-20 TOPS20-HOST SI:HOST-FLAVOR)
(DEFPROP :TOPS20 TOPS20-HOST SI:HOST-FLAVOR)

(DEFFLAVOR TENEX-HOST () (SI:HOST-TENEX-MIXIN FILE-HOST-TENEX-MIXIN SI:HOST))
(DEFPROP :TENEX TENEX-HOST SI:HOST-FLAVOR)

(DEFFLAVOR VMS-HOST () (SI:HOST-VMS-MIXIN FILE-HOST-VMS-MIXIN SI:HOST))
(DEFPROP :VMS VMS-HOST SI:HOST-FLAVOR)

(DEFFLAVOR UNIX-HOST () (SI:HOST-UNIX-MIXIN FILE-HOST-UNIX-MIXIN SI:HOST))
(DEFPROP :UNIX UNIX-HOST SI:HOST-FLAVOR)

(DEFFLAVOR MULTICS-HOST () (SI:HOST-MULTICS-MIXIN FILE-HOST-MULTICS-MIXIN SI:HOST))
(DEFPROP :MULTICS MULTICS-HOST SI:HOST-FLAVOR)

(DEFFLAVOR LISPM-HOST () (SI:HOST-LISPM-MIXIN FILE-HOST-LISPM-MIXIN SI:HOST))
(DEFPROP :LISPM LISPM-HOST SI:HOST-FLAVOR)
(DEFPROP :LISPM LISPM-HOST FILE-SYSTEM-HOST-FLAVOR)

(COMPILE-FLAVOR-METHODS ITS-HOST TOPS20-HOST TENEX-HOST VMS-HOST
                        UNIX-HOST MULTICS-HOST LISPM-HOST)

(defun site-pathname-initialize ()
  ;; Flush all old hosts
  (setq *pathname-host-list* (del-if #'(lambda (x)
                                         (send x :send-if-handles :file-host-p))
                                     *pathname-host-list*))
  ;;; Handle special frobs:
  ;; Adjust the file system type, maybe.  Usually LispMs to :LMFS
  (let ((fs-frobbed-hosts '())
        (pd-frobbed-hosts '()))
    (dolist (elt (get-site-option :special-file-hosts))
      (let ((fstype (car elt)))
        (dolist (hname (cdr elt))
          (let ((host (si:parse-host hname t nil)))
            (when (and host (operation-handled-p host :set-file-system-type))
              (send host :set-file-system-type fstype)
              (push host fs-frobbed-hosts))))))
    (dolist (elt (get-site-option :host-default-device-alist))
      (when (cdr elt)
        (let ((host (si:parse-host (car elt) t nil)))
          (when (and host (operation-handled-p host :set-primary-device))
            (send host :set-primary-device (cdr elt))
            (push host pd-frobbed-hosts)))))
    ;; Don't forget to remove the frobbed properties from hosts that
    ;; don't have one anymore.
    (si::do-all-hosts (h)
      (unless (memq h fs-frobbed-hosts)
        (send-if-handles h :reset-file-system-type))
      (unless (memq h pd-frobbed-hosts)
        (send-if-handles h :reset-primary-device))))
  ;; Add LMFILE hosts.
  (add-lmfile-hosts))

(DEFUN ADD-FILE-COMPUTER (HOST)
  "Add HOST to the list of hosts that can act as file servers.
HOST can be either a host name or a list (<host-name> <file-system-type>)."
  (LET ((H (FS:GET-PATHNAME-HOST (IF (CONSP HOST) (CAR HOST) HOST))))
    (IF (CONSP HOST)
        (send-if-handles h :set-file-system-type (CADR HOST))
      ;; needed because this may depend on :special-file-hosts which may change.
      (SEND H :SEND-IF-HANDLES :RESET-SAMPLE-PATHNAME)
      H)))

;;; Forget about the previous ways of getting to a file...

(defun reset-file-access (&optional (hosts nil hosts-specified-p) flavor)
  (when (variable-boundp *pathname-host-list*)  ;Hosts not set up?
    (typecase hosts
      (null (setq hosts *pathname-host-list*))
      (atom (setq hosts (list hosts))))
    (dolist (h hosts)
      (let ((host (fs:get-pathname-host h t nil)))
        (cond
          ((and (null host) hosts-specified-p)
           (warn "~S is not a valid pathname host" h))
          ((null host))
          (flavor (send host :set-access flavor))
          (t (send host :send-if-handles :reset :clear-session)))))))

(ADD-INITIALIZATION "Forget old file access" '(RESET-FILE-ACCESS) '(SYSTEM))


;;;; Interface to the LMFILE file system.
;;; The host used for access to a remote machine's LMFILE is NOT a host table host.
;;; Note that this flavor is only needed if the machine runs both the older Local-File
;;; system and the new one.  (Currently, Lisp Machines are assumed to run Local-File.)
;;; Otherwise, use an entry in the :SPECIAL-FILE-HOSTS site option to mark the host as
;;; using LMFILE.
(DEFFLAVOR LMFILE-HOST
           (HOST-NAME ; preferred host name
            associated-host)
           (FILE-HOST-MIXIN SI:BASIC-HOST SI:PROPERTY-LIST-MIXIN)
  (:GETTABLE-INSTANCE-VARIABLES HOST-NAME associated-host)
  (:INITABLE-INSTANCE-VARIABLES HOST-NAME associated-host))

(DEFPROP :LMFILE LMFILE-HOST FILE-SYSTEM-HOST-FLAVOR)

(defmethod (lmfile-host :network-typep) (network-type)
  (send associated-host :network-typep network-type))
(defmethod (lmfile-host :network-address) (network-type)
  (send associated-host :network-address network-type))

(DEFMETHOD (LMFILE-HOST :NAME) () HOST-NAME)
(DEFMETHOD (LMFILE-HOST :PATHNAME-FLAVOR) () 'LMFILE-PATHNAME)
(DEFMETHOD (LMFILE-HOST :SYSTEM-TYPE) () :LISPM)
(DEFMETHOD (LMFILE-HOST :FILE-SYSTEM-TYPE) () :LMFILE)

(DEFMETHOD (LMFILE-HOST :MAX-DATA-CONNECTIONS) () 8)

;;; The Fileserver doesn't supply the user name information, so might as well use
;;; whatever's hanging around.  Also, use this host as the host in the homedir,
;;; since the fileserver may use its own machine's hostname.
(DEFMETHOD (LMFILE-HOST :HSNAME-INFORMATION) (IGNORE STR IDX)
  (VALUES (SEND (PARSE-PATHNAME (SUBSTRING STR (SETQ IDX (1+ IDX))
                                           (STRING-SEARCH-CHAR #/NEWLINE STR IDX)))
                :NEW-PATHNAME :HOST SELF)
          USER-PERSONAL-NAME USER-GROUP-AFFILIATION
          USER-PERSONAL-NAME-FIRST-NAME-FIRST))

(DEFINE-SITE-VARIABLE LMFILE-SERVER-HOSTS :LMFILE-SERVER-HOSTS)

(DEFUN ADD-LMFILE-HOST (NAME real-host)
  (DOLIST (OHOST *PATHNAME-HOST-LIST*
                 (LET ((HOST (MAKE-INSTANCE 'LMFILE-HOST :HOST-NAME NAME
                                            :associated-host real-host)))
                   (PUSH HOST *PATHNAME-HOST-LIST*)
                   HOST))
    (WHEN (AND (TYPEP OHOST 'LMFILE-HOST)
               (EQ (SEND OHOST :HOST-NAME) NAME)
               (EQ (SEND OHOST :REMOTE-HOST-NAME) real-host))
      (SETQ *PATHNAME-HOST-LIST*
            (CONS OHOST (DELQ OHOST *PATHNAME-HOST-LIST*)))
      (RETURN OHOST))))

;;; Add a host to reach each machine that should have one.
;;; The name used should be present in the host table as a nickname
;;; but should not be that host's primary name.
;;; Also deletes any such hosts that no longer belong.
;;; It is assumed that this is called AFTER the ordinary chaos hosts are put on
;;; so that this host will override the ordinary chaos host for this host's name.
;;; Now this is called directly by SITE-CHAOS-PATHNAME-INITIALIZE.
(DEFUN ADD-LMFILE-HOSTS (&AUX HOSTS-WANTED)
  (DOLIST (HN LMFILE-SERVER-HOSTS)
    (LET ((HOST (SI:PARSE-HOST HN T)))
      (WHEN HOST
        ;; Took out -R gubbage
        (PUSH (ADD-LMFILE-HOST HN host)
              HOSTS-WANTED))))
  ;; Discard any hosts of this type that aren't supposed to be there.
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (AND (TYPEP HOST 'LMFILE-HOST)
         (NOT (MEMQ HOST HOSTS-WANTED))
         (SETQ *PATHNAME-HOST-LIST* (DELQ HOST *PATHNAME-HOST-LIST*)))))

(COMPILE-FLAVOR-METHODS LMFILE-HOST)
