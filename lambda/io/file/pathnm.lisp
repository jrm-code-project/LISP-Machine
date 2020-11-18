;;; Pathnames -*- Mode:LISP; Package:FS; Base:8; Readtable:ZL -*-

;;;  ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Advertised function interfaces:
;;; PARSE-PATHNAME THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
;;;  Parses a string (or whatever) into a pathname.
;;; DEFAULT-PATHNAME &OPTIONAL DEFAULTS HOST DEFAULT-TYPE DEFAULT-VERSION
;;;  Returns the default for the given HOST from DEFAULTS.
;;; SET-DEFAULT-PATHNAME PATHNAME &OPTIONAL DEFAULTS
;;;  Sets the default for either the host of the pathname or the NIL default.
;;; MAKE-PATHNAME-DEFAULTS
;;;  Returns an alist that you can pass to the functions below that take a set of defaults.
;;;  Most things that take a set of defaults will also take a single pathname.
;;; MERGE-PATHNAME-DEFAULTS PATHNAME &OPTIONAL DEFAULTS DEFAULT-TYPE DEFAULT-VERSION
;;;  Fill in slots in PATHNAME from program defaults.  This is what most
;;;  programs interface to.
;;; MERGE-AND-SET-PATHNAME-DEFAULTS PATHNAME &OPTIONAL DEFAULTS DEFAULT-TYPE DEFAULT-VERSION
;;;  Does parse, merge, and updating of defaults.
;;; DESCRIBE-PATHNAME PATHNAME
;;;  Describes all files that look like pathname.  Also useful when you cannot remember what
;;;  directory a file is in.
;;; PATHNAME-PLIST PATHNAME

;;; Advertised messages on pathnames:
;;; :GET INDICATOR         --- see below for a discourse on pathname properties
;;; :PUTPROP PROP INDICATOR
;;; :REMPROP INDICATOR
;;; :DEVICE, :DIRECTORY, :NAME, :TYPE, :VERSION
;;; :NEW-DEVICE, :NEW-DIRECTORY, :NEW-NAME, :NEW-TYPE, :NEW-VERSION
;;; :NEW-PATHNAME &REST OPTIONS
;;; :DEFAULT-NAMESTRING STRING
;;; :GENERIC-PATHNAME
;;; :STRING-FOR-HOST, :STRING-FOR-PRINTING, :STRING-FOR-WHOLINE, :STRING-FOR-EDITOR
;;; :STRING-FOR-DIRED
;;; :INIT-FILE PROGRAM-NAME

;;; Advertised special variables:
;;; *KNOWN-TYPES* - list of types unimportant for the sake of generic pathnames.
;;; *DEFAULTS-ARE-PER-HOST* - user option.  If NIL, pathnames defaults are maintained all
;;;  together for all hosts.
;;; *ITS-UNINTERESTING-TYPES* - types that do not deserve the FN2 slot.
;;; *ALWAYS-MERGE-TYPE-AND-VERSION* - user option.  If T, gives TENEX style defaulting
;;;  of pathnames.  Default is NIL.

;;; Other system types (pathname syntaxes) must implement at least the following messages:
;;; They can then be mixed with CHAOS-PATHNAME for use with the QFILE chaosnet file
;;; job protocol.
;;;  :STRING-FOR-HOST - returns a string that can be sent to the file computer that
;;;  specifying the file in question.
;;;  :PARSE-NAMESTRING - takes a string and returns multiple values for various components
;;;  present in the string.
;;; See ITS-PATHNAME-MIXIN and/or TOPS20-PATHNAME-MIXIN for additional details.

;;; To add another protocol, implement the messages of CHAOS-PATHNAME for generic file
;;; manipulation.  That flavor is defined in QFILE.

;;; Interaction with host objects:
;;; The HOST instance variable of a pathname is a host object, as
;;; outlined in SYS: SYS2; HOST
;;; *PATHNAME-HOST-LIST* is the set of all logical pathname hosts.
;;; *PATHNAME-HOST-LIST* is the set of all physical pathname hosts.
;;; When parsing a string into a pathname, the specified host
;;; (the part of the string before the colon) is sent in the :PATHNAME-HOST-NAMEP
;;; message to each host in this list.  When that returns T, that host is used.
;;; The host is sent a :PATHNAME-FLAVOR message to determine the flavor of the
;;; pathname to instantiate.  (If the reply to :PATHNAME-FLAVOR returns multiple
;;; values, the second is an addition for the INIT-PLIST to use when instantiating.)
;;; Normally, when printing the host portion of a pathname, the host is
;;; sent a :NAME-AS-FILE-COMPUTER message.

;;;TRUENAMEs  refer exactly to a single instance of a single file on a single filecomputer.

;;;LOGICAL-HOSTS are provided in an attempt to improve portability of source
;;; file systems to various file computers.  The general idea is we gain by
;;; refering to SYS: SYS; instead of AI: LISPM; .  The mapping between logical hosts
;;; and physical hosts is usally controlled by site options; however that need not
;;; necessarily be true.  However, if this mapping is changed in a running system,
;;; it must be realized that the consequence is that already loaded files will, in
;;; some sense, be assumed to have "come" from the new place.  Being more abstract
;;; objects than physical hosts, logical hosts are preferred when contructing
;;; generic pathnames (see below).

;;;LOGICAL-PATHNAMES are pathnames whose host is a LOGICAL-HOST.

;;;GENERIC-PATHNAMES
;;;  A generic-pathname is a single pathname common to a logical group of files,
;;;  where a logical group consists of all versions and forms (LISP, QFASL, etc.) of a file.
;;;  Generic-pathnames are used for
;;;  storing properties about the logical group.  For example, the mode-line properties
;;;  and information about what packages the file has been loaded into are stored on
;;;  the generic pathname.  The generic pathname is obtained
;;;  by sending a :GENERIC-PATHNAME message to a PATHNAME.
;;;  The following properties are held on GENERIC-PATHNAMES:
;;;   :FILE-ID-PACKAGE-ALIST  Alist keyed on package.  Remembers which forms of
;;;      this file have been loaded into which packages.  Association is (currently)
;;;      a two list (<file-id> <access-pathname>).
;;;         <file-id> is a dotted pair (<TRUENAME> . <creation-time>)
;;;         <access-pathname>  is the acceess pathname used for the load.  It can be
;;;                     SYS: SYS; MLAP QFASL > or AI: LISPM; MLAP QFASL, etc.

;;;    A generic pathname normally has a type of unspecific, but not always.  Consider
;;;  FOO.LISP, FOO.QFASL, FOO.DOC and FOO.PRESS.  Being as this is the lisp machine
;;;  (and we have to worry about ITS), the generic pathname for FOO.LISP and FOO.QFASL
;;;  is defined to be FOO.unspecific.  However, we also provide a mechanism to deal with
;;;  cases where certain types of files on certain hosts represent separate logical
;;;  "groups" (.DOC and .PRESS for example).  This consists of sending the host a
;;;  a :GENERIC-BASE-TYPE <type> message when computing a generic pathname.  So we
;;;  might get back "DOC" in the case of .DOC and .PRESS rather than the usual UNSPECIFIC.
;;;  The default :GENERIC-BASE-TYPE method of BASIC-HOST looks at *GENERIC-BASE-TYPE-ALIST*
;;;  for a few types which are assumed to map into non :UNSPECIFIC generic base types
;;;  if not otherwise specified by the host.

;;;  Hosts of generic pathnames.
;;;   Generic pathnames are defined to be BACKTRANSLATED with respect to logical hosts.
;;;  Backtranslating means translating from physical (host device directory) to (currently)
;;;  equivalent logical ones, if possible.
;;;  Thus, one obtains SYS: SYS; from AI: LISPM;.  This is consistant with the
;;;  general idea of generic pathnames, which is to refer to "the object" with as
;;;  high an abstraction as possible.  When moving bands to different sites, this
;;;  causes the right thing to happen as much as in any other scheme.  A consequence
;;;  of making generic pathnames be backtranslated is that ALL files on the
;;;  translated from directories will have logical hosts in the generic pathnames.
;;;  If random miscellaneous files are also stored in directories which are logically
;;;  mapped, questionably intended results could be obtained in some cases.
;;;  However, no great disasters will occur, and it should be kept in mind that
;;;  relatively "clean" bands are shipped between sites, which should have only
;;;  referenced system files.

;;;  When computing a generic pathname, the :GENERIC-BASE-TYPE message is first
;;;  sent to the actual host (if that happens to be available).  Then the
;;;  (host, directory) pair is backtranslated (possibly obtaining a logical host).
;;;  Then, if the BASE-TYPE is still :UNSPECIFIC, another :GENERIC-BASE-TYPE message
;;;  is tried.

;;;  Names of generic pathnames.
;;;   No conversion of NAME is ever done on generic pathnames.  If you are using logical
;;;  hosts (to attempt to improve portability) you should avoid complex file names
;;;  for the same reason.

(DEFUN PATHNAME (OBJECT)
  "Convert OBJECT to a pathname.
If it's a pathname, it is unchanged.
If it's a stream, the :PATHNAME operation is invoked.
If it's a string or symbol, it is parsed."
  (ETYPECASE OBJECT
    (PATHNAME OBJECT)
    (STREAM (SEND OBJECT :PATHNAME))
    ((OR SYMBOL STRING) (PARSE-PATHNAME OBJECT))))

(DEFSUBST PATHNAMEP (OBJECT)
  "T if OBJECT is a pathname."
  (TYPEP OBJECT 'PATHNAME))

(DEFUN TRUENAME (OBJECT)
  "Convert OBJECT to a pathname, then return the truename of the file it refers to."
  (IF (STREAMP OBJECT)
      (SEND OBJECT :TRUENAME)
    (SEND (PATHNAME OBJECT) :TRUENAME)))

(DEFUN NAMESTRING (OBJECT)
  "Convert OBJECT to a pathname and then that into a namestring."
  (SEND (PATHNAME OBJECT) :STRING-FOR-PRINTING))

(DEFUN FILE-NAMESTRING (OBJECT)
  "Convert OBJECT to a pathname; return a namestring specifying just name, type and version."
  (SEND (PATHNAME OBJECT) :STRING-FOR-DIRED))

(DEFUN DIRECTORY-NAMESTRING (OBJECT)
  "Convert OBJECT to a pathname; return a namestring specifying just device and directory."
  (SEND (PATHNAME OBJECT) :STRING-FOR-DIRECTORY))

(DEFUN HOST-NAMESTRING (OBJECT)
  "Convert OBJECT to a pathname; return a namestring with just OBJECT's host name and a colon."
  (STRING-APPEND (SEND (PATHNAME OBJECT) :HOST) ":"))

(DEFUN ENOUGH-NAMESTRING (OBJECT &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  "Return enough namestring to produce whatever OBJECT produced
when merged with DEFAULTS using MERGE-PATHNAMES.
OBJECT is converted to a pathname, and that is made into a string
from which components may be omitted if their values are the same as
what would result from defaulting whatever is left with the specified defaults."
  (LET* ((PATHNAME (PATHNAME OBJECT))
         (DEFHOST (DEFAULT-HOST DEFAULTS))
         (DP (DEFAULT-PATHNAME DEFAULTS (PATHNAME-HOST PATHNAME)))
         (NEED-NAME (NOT (EQUAL (PATHNAME-RAW-NAME PATHNAME) (PATHNAME-RAW-NAME DP))))
         (NEED-TYPE (NOT (EQUAL (PATHNAME-RAW-TYPE PATHNAME) (PATHNAME-RAW-TYPE DP))))
         (NEED-VERSION (NEQ (PATHNAME-RAW-VERSION PATHNAME) (PATHNAME-RAW-VERSION DP)))
         (STRING
           (SEND
             (SEND PATHNAME :NEW-PATHNAME
                   (IF (EQUAL (PATHNAME-RAW-DIRECTORY PATHNAME) (PATHNAME-RAW-DIRECTORY DP))
                       :DIRECTORY)
                   NIL
                   (IF (EQUAL (PATHNAME-RAW-DEVICE PATHNAME) (PATHNAME-RAW-DEVICE DP))
                       :DEVICE)
                   NIL
                   (IF (EQ (PATHNAME-VERSION PATHNAME) :NEWEST)
                       :VERSION)
                   :NEWEST)
             (IF (OR NEED-NAME NEED-TYPE NEED-VERSION)
                 :STRING-FOR-PRINTING
               :STRING-FOR-DIRECTORY))))
    (IF (OR NEED-NAME NEED-TYPE NEED-VERSION)
        (IF (EQ (PATHNAME-HOST PATHNAME) DEFHOST)
            (STRING-LEFT-TRIM #/SP (SUBSTRING-AFTER-CHAR #/: STRING))
          STRING)
      (IF (EQ (PATHNAME-HOST PATHNAME) DEFHOST)
          STRING
        (STRING-APPEND (SEND (PATHNAME-HOST PATHNAME) :NAME-AS-FILE-COMPUTER)
                       ": " STRING)))))

(DEFFLAVOR PATHNAME-ERROR () (ERROR))

(DEFMETHOD (PATHNAME-ERROR :CASE :PROCEED-ASKING-USER :NEW-PATHNAME)
           (CONTINUATION READ-OBJECT-FUNCTION)
  (FUNCALL CONTINUATION :NEW-PATHNAME
           (FUNCALL READ-OBJECT-FUNCTION
                    `(:PATHNAME :DEFAULTS (()))
                    "Pathname to use instead: ")))

(COMPILE-FLAVOR-METHODS PATHNAME-ERROR)

;; The pathname hash table, pathnames, and cached strings are stored here to improve locality.
;; Individual :STRING-FOR-mumble methods can save a string copy by explicitly
;; consing the string in PATHNAME-AREA, but this is not necessary.
(DEFVAR PATHNAME-AREA (MAKE-AREA :NAME 'PATHNAME-AREA :REGION-SIZE #O100000 :VOLATILITY 1))

(DEFVAR *PATHNAME-HOST-LIST* NIL
  "List of physical hosts that can serve in pathnames.")
(DEFVAR *LOGICAL-PATHNAME-HOST-LIST* NIL
  "List of hosts for logical pathnames.
Note that the names of some of the logical hosts in this list may shadow the names
of physical hosts in FS:*PATHNAME-HOST-LIST*")


;;; This is the actual base flavor
(DEFFLAVOR PATHNAME
        (HOST
         (DEVICE NIL)
         (DIRECTORY NIL)
         (NAME NIL)
         (TYPE NIL)
         (VERSION NIL)
         (SI:PROPERTY-LIST NIL)                 ;Where files properties are stored
         (STRING-FOR-PRINTING NIL)
         )
        (SI:PROPERTY-LIST-MIXIN)
  :ORDERED-INSTANCE-VARIABLES
  (:ACCESSOR-PREFIX PATHNAME-RAW-)
  (:OUTSIDE-ACCESSIBLE-INSTANCE-VARIABLES HOST DEVICE DIRECTORY NAME TYPE VERSION
                                          SI:PROPERTY-LIST)
  (:GETTABLE-INSTANCE-VARIABLES HOST DEVICE DIRECTORY NAME TYPE VERSION SI:PROPERTY-LIST)
  (:INITABLE-INSTANCE-VARIABLES HOST DEVICE DIRECTORY NAME TYPE VERSION SI:PROPERTY-LIST)
  (:METHOD-COMBINATION (:OR :BASE-FLAVOR-LAST :WILD-P))
  (:REQUIRED-METHODS :PARSE-NAMESTRING))

;;; PRINC of a pathname is just like PRINC of the :STRING-FOR-PRINTING
;;; PRIN1 prints inside a # ...  so it can be read back.
(DEFMETHOD (PATHNAME :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (COND (SLASHIFY-P
         (PRINC "#" STREAM)
         (PRIN1 (TYPE-OF SELF) STREAM)
         (TYO #/SP STREAM)
         (PRIN1 (SEND SELF :STRING-FOR-PRINTING) STREAM)
         (TYO #/ STREAM))
        (T (SEND STREAM :STRING-OUT (SEND SELF :STRING-FOR-PRINTING)))))

;And this is what is called to read back a PRIN1'd pathname.
(DEFMETHOD (PATHNAME :READ-INSTANCE) (IGNORE STREAM)
  (PARSE-PATHNAME (CLI:READ STREAM T NIL T)))

(DEFMETHOD (PATHNAME :PLIST) ()
  SI:PROPERTY-LIST)

;For bootstrapping.
(DEFMETHOD (PATHNAME :PARSE-TYPE-SPEC) PATHNAME-PASS-THROUGH-SPEC)

(DEFVAR CANONICAL-TYPES NIL
  "Value is alternating list of canonical types (keywords) and their definitions.
Each definition is an alist of elements of the form
  (system-type surface-types...)
where NIL as a system type applies to any system type not specifically mentioned.")

;Here for bootstrapping.
(DEFMETHOD (PATHNAME :CANONICAL-TYPE) ()
  (LET ((-TYPE- (SEND SELF :TYPE))
        (SYSTEM-TYPE (or (send host :send-if-handles :file-system-type)
                         (send host :system-type)))     ;this is for logical hosts
        (ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
    (LOOP FOR (CANONICAL-TYPE DEFN) ON CANONICAL-TYPES BY 'CDDR
          AS PER-SYSTEM = (OR (ASSQ SYSTEM-TYPE DEFN) (ASSQ NIL DEFN))
          DO
          (IF (SYS:MEMBER-EQUAL -TYPE- (CDR PER-SYSTEM))
              (RETURN
                (values
                  CANONICAL-TYPE
                  (UNLESS (EQUAL -TYPE- (CADR PER-SYSTEM)) -TYPE-))))
          FINALLY (RETURN -TYPE-))))

(DEFMETHOD (PATHNAME :RAW-DEVICE) () DEVICE)
(DEFMETHOD (PATHNAME :RAW-DIRECTORY) () DIRECTORY)
(DEFMETHOD (PATHNAME :RAW-NAME) () NAME)
(DEFMETHOD (PATHNAME :RAW-TYPE) () TYPE)

;;; The pathname component accessors, which were around before Common Lisp, didn't convert their
;;; arguments to pathnames.  Now the Common Lisp functions do so.
(DEFUN PATHNAME-HOST (P)
  "Returns the host P is on."
  (PATHNAME-RAW-HOST (PATHNAME P)))
(DEFSUBST PATHNAME-DEVICE (P)
  "Returns the device component of P."
  (SEND (PATHNAME P) :DEVICE))
(DEFSUBST PATHNAME-DIRECTORY (P)
  "Returns the directory component of P."
  (SEND (PATHNAME P) :DIRECTORY))
(DEFSUBST PATHNAME-NAME (P)
  "Returns the name of P."
  (SEND (PATHNAME P) :NAME))
(DEFSUBST PATHNAME-TYPE (P)
  "Returns the type of P."
  (SEND (PATHNAME P) :TYPE))
(DEFSUBST PATHNAME-VERSION (P)
  "Returns the version of P."
  (SEND (PATHNAME P) :VERSION))
;;; > Why are these different ?
(DEFSUBST PATHNAME-PROPERTY-LIST (P)
  "Returns the list of properties of P, which must be a pathname object."
  (SYMEVAL-IN-INSTANCE P 'SI:PROPERTY-LIST))
(DEFSUBST PATHNAME-PLIST (P)
  "Returns the list of properties of P, which must be a pathname object"
  (PATHNAME-PROPERTY-LIST P))

(DEFUN DESCRIBE-PATHNAME (PATHNAME)
  (IF (TYPEP PATHNAME 'PATHNAME)
      (DESCRIBE-PATHNAME-1 PATHNAME)
    (SETQ PATHNAME (PARSE-PATHNAME PATHNAME))
    (LET ((HOST (PATHNAME-HOST PATHNAME))
          (DEVICE (PATHNAME-DEVICE PATHNAME))
          (DIRECTORY (PATHNAME-DIRECTORY PATHNAME))
          (NAME (PATHNAME-NAME PATHNAME))
          (TYPE (PATHNAME-TYPE PATHNAME))
          (VERSION (PATHNAME-VERSION PATHNAME)))
      (MAPHASH #'(LAMBDA (KEY VAL)
                   (AND (OR (NULL HOST) (EQ HOST (FIRST KEY)))
                        (OR (NULL DEVICE) (EQUAL DEVICE (SECOND KEY)))
                        (OR (NULL DIRECTORY) (EQUAL DIRECTORY (THIRD KEY)))
                        (OR (NULL NAME) (EQUAL NAME (FOURTH KEY)))
                        (OR (NULL TYPE) (EQUAL TYPE (FIFTH KEY)))
                        (OR (NULL VERSION) (EQUAL VERSION (SIXTH KEY)))
                        (DESCRIBE-PATHNAME-1 VAL)))
               *PATHNAME-HASH-TABLE*))))

(DEFUN DESCRIBE-PATHNAME-1 (PATHNAME &AUX PLIST)
  (AND (SETQ PLIST (PATHNAME-PROPERTY-LIST PATHNAME))
       (LET ((LOADED-IDS (GET (LOCF PLIST) :FILE-ID-PACKAGE-ALIST)))
         (AND LOADED-IDS
              (DO ((LOADED-IDS LOADED-IDS (CDR LOADED-IDS))
                   (FIRST-P T NIL)
                   (INFO) (TRUENAME) (CREATION-DATE))
                  ((NULL LOADED-IDS))
                (SETQ INFO (CADAR LOADED-IDS)
                      TRUENAME (CAR INFO)
                      CREATION-DATE (CDR INFO))
                (FORMAT T "~&The version ~:[~*~;of ~A ~]in package ~A ~:[is ~A, and ~;~*~]~
                             was created ~\TIME\~%"
                        FIRST-P PATHNAME (CAAR LOADED-IDS) (EQ PATHNAME TRUENAME) TRUENAME
                        CREATION-DATE)))
         (DO ((PLIST PLIST (CDDR PLIST))
              (FLAG NIL)
              (IND) (PROP))
             ((NULL PLIST))
           (SETQ IND (CAR PLIST)
                 PROP (CADR PLIST))
           (COND ((NEQ IND :FILE-ID-PACKAGE-ALIST)
                  (COND ((NULL FLAG)
                         (FORMAT T "~&~A has the following ~:[other ~]properties:~%"
                                 PATHNAME (NULL LOADED-IDS))
                         (SETQ FLAG T)))
                  (FORMAT T "~&~7@T~S:~27T~S~%" IND PROP)))))))

(DEFMETHOD (PATHNAME :INIT) (IGNORE)
  (OR (VARIABLE-BOUNDP HOST)
      (FERROR NIL "Host must be specified when initializing a pathname")))

;;; Caching of strings
(DEFMACRO CACHE-IN-VARIABLE (VARIABLE . BODY)
  `(OR ,VARIABLE
       (SETQ ,VARIABLE (PROGN . ,BODY)
             ,VARIABLE (COPY-INTO-PATHNAME-AREA ,VARIABLE))))

(DEFWRAPPER (PATHNAME :STRING-FOR-PRINTING) (IGNORE . BODY)
  `(CACHE-IN-VARIABLE STRING-FOR-PRINTING . ,BODY))

(DEFMETHOD (PATHNAME :STRING-FOR-WHOLINE) (&OPTIONAL IGNORE)
  (SEND SELF :STRING-FOR-PRINTING))

(DEFMETHOD (PATHNAME :STRING-FOR-DIRED) ()
  (SEND SELF :STRING-FOR-PRINTING))

(DEFMETHOD (PATHNAME :STRING-FOR-MINI) ()
  (SEND SELF :STRING-FOR-PRINTING))

(DEFMETHOD (PATHNAME :STRING-FOR-DIRECTORY) ()
  (SEND SELF :STRING-FOR-PRINTING))

(DEFMETHOD (PATHNAME :WILD-P) ()
  (OR (EQ DEVICE :WILD) (EQ DIRECTORY :WILD) (EQ NAME :WILD)
      (EQ TYPE :WILD) (EQ VERSION :WILD)))

(DEFMETHOD (PATHNAME :DEVICE-WILD-P) ()
  (EQ DEVICE :WILD))

(DEFMETHOD (PATHNAME :DIRECTORY-WILD-P) ()
  (EQ DIRECTORY :WILD))

(DEFMETHOD (PATHNAME :NAME-WILD-P) ()
  (EQ NAME :WILD))

(DEFMETHOD (PATHNAME :TYPE-WILD-P) ()
  (EQ TYPE :WILD))

(DEFMETHOD (PATHNAME :VERSION-WILD-P) ()
  (EQ VERSION :WILD))

(DEFMETHOD (PATHNAME :WILDCARD-MAP) (OPERATION PLISTP DIR-LIST-OPTIONS &REST ARGS)
  (IF (SEND SELF :WILD-P)
      (LET ((DLIST (APPLY 'DIRECTORY-LIST SELF DIR-LIST-OPTIONS))
            (RESULTS '()))
        (IF (ERRORP DLIST)
            DLIST
            (OR (DOLIST (DLIST-ENTRY (CDR DLIST))
                  (IF (NOT (GET DLIST-ENTRY :DIRECTORY))        ;Subdirectories aren't files!
                      (LET ((RESULT (APPLY OPERATION
                                           (IF PLISTP DLIST-ENTRY (CAR DLIST-ENTRY))
                                           ARGS)))
                        (IF (ERRORP RESULT)
                            (RETURN RESULT)
                            (PUSH RESULT RESULTS)))))
                (NREVERSE RESULTS))))
      (LET ((RESULT
              (IF (AND PLISTP (NEQ PLISTP ':MAYBE))
                  (LET ((DLIST-ENTRY
                          (FILE-PROPERTIES SELF
                                           (NOT (MEMQ ':NOERROR DIR-LIST-OPTIONS)))))
                    (IF (ERRORP DLIST-ENTRY)
                        DLIST-ENTRY
                        (APPLY OPERATION DLIST-ENTRY ARGS)))
                  (APPLY OPERATION SELF ARGS))))
        (IF (ERRORP RESULT)
            RESULT
            (LIST RESULT)))))

(DEFMETHOD (PATHNAME :SUPPRESSED-DEVICE-NAMES) () '(NIL :UNSPECIFIC))

(DEFMETHOD (PATHNAME :PRIMARY-DEVICE) () (SEND HOST :PRIMARY-DEVICE))

;T if files of this host are can be undeleted.
(DEFMETHOD (PATHNAME :UNDELETABLE-P) () NIL)

;T if a namestring with no file type, on this system, reads in as :UNSPECIFIC.
;This implies that LOAD should treat :UNSPECIFIC in the type
;as possibly a request for a default, or possibly a specification
;of a file whose name includes no type.
(DEFMETHOD (PATHNAME :UNSPECIFIC-TYPE-IS-DEFAULT) () NIL)

;;; This is the flavor that interfaces to the acess stuff
(DEFFLAVOR HOST-PATHNAME
        ((STRING-FOR-EDITOR NIL)
         (STRING-FOR-DIRED NIL)
         (STRING-FOR-HOST NIL)
         (STRING-FOR-DIRECTORY NIL))
        (PATHNAME)
  (:REQUIRED-METHODS :STRING-FOR-HOST))

(DEFWRAPPER (HOST-PATHNAME :STRING-FOR-EDITOR) (IGNORE . BODY)
  `(CACHE-IN-VARIABLE STRING-FOR-EDITOR . ,BODY))

(DEFWRAPPER (HOST-PATHNAME :STRING-FOR-DIRED) (IGNORE . BODY)
  `(CACHE-IN-VARIABLE STRING-FOR-DIRED . ,BODY))

(DEFWRAPPER (HOST-PATHNAME :STRING-FOR-DIRECTORY) (IGNORE . BODY)
  `(CACHE-IN-VARIABLE STRING-FOR-DIRECTORY . ,BODY))

(DEFMETHOD (HOST-PATHNAME :STRING-FOR-PRINTING) ()
  (STRING-APPEND (SEND HOST :NAME-AS-FILE-COMPUTER) ": "
                 (SEND SELF :STRING-FOR-HOST)))

(DEFMETHOD (HOST-PATHNAME :STRING-FOR-EDITOR) ()
  (STRING-APPEND (SEND SELF :STRING-FOR-HOST) #/SP
                 (SEND HOST :NAME-AS-FILE-COMPUTER) #/:))

(DEFWRAPPER (HOST-PATHNAME :STRING-FOR-HOST) (IGNORE . BODY)
  `(CACHE-IN-VARIABLE STRING-FOR-HOST . ,BODY))

(DEFMETHOD (HOST-PATHNAME :STRING-FOR-MINI) ()
  (SEND SELF :STRING-FOR-HOST))

(DEFMETHOD (HOST-PATHNAME :STRING-FOR-DIRECTORY) ()
  (SEND SELF :STRING-FOR-HOST))

(DEFMETHOD (HOST-PATHNAME :DIRECTORY-STREAM-DEFAULT-PARSER) ()
  (SEND HOST :ACCESS-OPERATION :DEFAULT-DIRECTORY-STREAM-PARSER))

(DEFMETHOD (HOST-PATHNAME :OPEN) (PATHNAME &REST OPTIONS)
  (LEXPR-SEND HOST :ACCESS-OPERATION :OPEN SELF PATHNAME OPTIONS))

(DEFMETHOD (HOST-PATHNAME :RENAME) (NEW-PATHNAME &OPTIONAL (ERROR-P T))
  (SEND HOST :ACCESS-OPERATION :RENAME SELF NEW-PATHNAME ERROR-P))

(DEFMETHOD (HOST-PATHNAME :DELETE) (&OPTIONAL (ERROR-P T))
  (SEND HOST :ACCESS-OPERATION :DELETE SELF ERROR-P))

(DEFMETHOD (HOST-PATHNAME :DELETE-MULTIPLE-FILES) (ERROR-P FILES)
  (SEND HOST :ACCESS-OPERATION :DELETE-MULTIPLE-FILES ERROR-P FILES))

(DEFMETHOD (HOST-PATHNAME :UNDELETE-MULTIPLE-FILES) (ERROR-P FILES)
  (SEND HOST :ACCESS-OPERATION :UNDELETE-MULTIPLE-FILES ERROR-P FILES))

(DEFMETHOD (HOST-PATHNAME :COMPLETE-STRING) (STRING OPTIONS)
  (SEND HOST :ACCESS-OPERATION :COMPLETE-STRING SELF STRING OPTIONS))

(DEFMETHOD (HOST-PATHNAME :CHANGE-PROPERTIES) (ERROR-P &REST PROPERTIES)
  (LEXPR-SEND HOST :ACCESS-OPERATION :CHANGE-PROPERTIES SELF ERROR-P PROPERTIES))

(DEFMETHOD (HOST-PATHNAME :DIRECTORY-STREAM) (OPTIONS)
  (SEND HOST :ACCESS-OPERATION :DIRECTORY-STREAM SELF OPTIONS))

(DEFMETHOD (HOST-PATHNAME :DIRECTORY-LIST) (OPTIONS)
  (SEND HOST :ACCESS-OPERATION :DIRECTORY-LIST SELF OPTIONS))

(DEFMETHOD (HOST-PATHNAME :DIRECTORY-LIST-STREAM) (OPTIONS)
  (SEND HOST :ACCESS-OPERATION :DIRECTORY-LIST-STREAM SELF OPTIONS))

(DEFMETHOD (HOST-PATHNAME :HOMEDIR) (&OPTIONAL USER)
  (SEND HOST :ACCESS-OPERATION :HOMEDIR USER))

(DEFMETHOD (HOST-PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (SEND HOST :ACCESS-OPERATION :CREATE-LINK SELF LINK-TO ERROR))

(DEFMETHOD (HOST-PATHNAME :EXPUNGE) (&KEY &OPTIONAL (ERROR T))
  (SEND HOST :ACCESS-OPERATION :EXPUNGE SELF ERROR))

(DEFMETHOD (HOST-PATHNAME :REMOTE-CONNECT) (&OPTIONAL &KEY (ERROR T) ACCESS-MODE)
  (SEND HOST :ACCESS-OPERATION :REMOTE-CONNECT SELF ERROR ACCESS-MODE))

(DEFMETHOD (HOST-PATHNAME :CREATE-DIRECTORY) (&KEY &OPTIONAL (ERROR T))
  (SEND HOST :ACCESS-OPERATION :CREATE-DIRECTORY SELF ERROR))

(DEFMETHOD (HOST-PATHNAME :MULTIPLE-FILE-PLISTS) (FILES OPTIONS)
  (SEND HOST :ACCESS-OPERATION :MULTIPLE-FILE-PLISTS FILES OPTIONS))

;;; Perhaps this would be a reasonable default for the way all hosts should work?
(DEFMETHOD (HOST-PATHNAME :ALL-DIRECTORIES) (OPTIONS)
  (LET ((DIRS (SEND SELF :DIRECTORY-LIST (CONS :DIRECTORIES-ONLY OPTIONS))))
    (IF (ERRORP DIRS) DIRS
      (SETQ DIRS (CDR DIRS))
      (DOLIST (X DIRS)
        (RPLACA X (SEND (CAR X) :NEW-PATHNAME :NAME :UNSPECIFIC :TYPE :UNSPECIFIC
                        :VERSION :UNSPECIFIC)))
      DIRS)))


;;; By default, a directory is stored as a file in the superior directory whose name gives
;;; the name of the component at this level.
(DEFMETHOD (PATHNAME :PATHNAME-AS-DIRECTORY) ()
  (SEND SELF :NEW-PATHNAME
             :RAW-DIRECTORY (cond ((null name) directory)  ;TOPS-20 sometimes sends back a directory-form
                ;pathname where a file-form pathname was expected, causing this.
                ;Also, this cause c-u m-x dired ai:*; to win.  In any case, appending a
                ;NIL into the directory list isnt going to get us anywhere.
                                  ((EQ DIRECTORY :ROOT)
                                   NAME)
                                  (t (APPEND (IF (CONSP DIRECTORY) DIRECTORY
                                               (NCONS DIRECTORY))
                                             (NCONS NAME))))
             :NAME :UNSPECIFIC
             :TYPE :UNSPECIFIC
             :VERSION :UNSPECIFIC))

(DEFMETHOD (PATHNAME :GENERIC-PATHNAME) ()
  (LET* ((TYP (SEND SELF :CANONICAL-TYPE))
         (NEW-TYPE (SEND HOST :GENERIC-BASE-TYPE TYP))
         (DEV (SEND SELF :DEVICE))
         DEV1)
    (AND (MEMQ DEV (SEND SELF :SUPPRESSED-DEVICE-NAMES))
         (SETQ DEV1 (SEND HOST :PRIMARY-DEVICE)))
    (LET ((STAGE1
            (SEND SELF :NEW-PATHNAME :DEVICE (OR DEV1 DEV)
                                     :DIRECTORY (SEND SELF :DIRECTORY)
                                     :NAME (SEND SELF :NAME)
                                     :TYPE NEW-TYPE
                                     :VERSION :UNSPECIFIC)))
      ;; Now try backtranslating into a logical pathname.
      ;; If there is no suitable one, we return STAGE1.
      (DOLIST (H *LOGICAL-PATHNAME-HOST-LIST*
                 STAGE1)
        (WHEN (AND (TYPEP H 'LOGICAL-HOST) (EQ HOST (SEND H :PHYSICAL-HOST)))
          (LET ((BTPN (SEND (SEND H :SAMPLE-PATHNAME) :BACK-TRANSLATED-PATHNAME STAGE1)))
            (WHEN BTPN
              (IF (EQ NEW-TYPE :UNSPECIFIC)
                  (RETURN (SEND BTPN :NEW-PATHNAME :TYPE (SEND H :GENERIC-BASE-TYPE
                                                               (SEND BTPN :CANONICAL-TYPE))))
                (RETURN BTPN)))))))))

(DEFMETHOD (PATHNAME :SOURCE-PATHNAME) ()
  (GENERIC-PATHNAME-SOURCE-PATHNAME (SEND SELF :GENERIC-PATHNAME)))

;;; $$$ New losing workaround <23-Nov-88 smh>
(defvar *known-non-source-file-types*
        '(:QFASL :FBIN :FDEF :UNSPECIFIED
                 :PRESS :WIDTHS :KST :IMPRESS :DVI :FASL :MCR :QBIN :EXE :BIN)
  "A list of canonical types which should never be considered source file types,
no matter how confused the pathname system becomes.  These types are always
replaced by :LISP inside GENERIC-PATHNAME-SOURCE-PATHNAME.")

(DEFUN GENERIC-PATHNAME-SOURCE-PATHNAME (PATHNAME)
  "Given the generic pathname PATHNAME, return a pathname for the source file.
We use the actual source file name as recorded, if possible."
  (LET ((QFASL-SOURCE (SEND PATHNAME :GET :QFASL-SOURCE-FILE-UNIQUE-ID))
        (LOADED-FILE (CAADAR (SEND PATHNAME :GET :FILE-ID-PACKAGE-ALIST))))
    (MULTIPLE-VALUE-BIND (CTYPE OTYPE)
        (COND (QFASL-SOURCE
               (IF (CONSP QFASL-SOURCE)         ;dont bomb if list frobs somehow
                   ;left from cold load.
                   (SETQ QFASL-SOURCE
                         (PATHNAME-FROM-COLD-LOAD-PATHLIST QFASL-SOURCE)))
               (SEND QFASL-SOURCE :CANONICAL-TYPE))
              ((AND LOADED-FILE
                    (NOT (EQUAL (SEND LOADED-FILE :TYPE) "QFASL")))
               (SEND LOADED-FILE :CANONICAL-TYPE))
              (T (SEND (SEND PATHNAME :HOST) :GENERIC-SOURCE-TYPE
                       (SEND PATHNAME :CANONICAL-TYPE))))
      (SEND PATHNAME :NEW-PATHNAME :VERSION :NEWEST
            ;; The replacement of :UNSPECIFIC by :LISP
            ;; is for files that were last compiled on ITS or FC.
            ;; $$$ Replaced by a list of unacceptable answers to keep developers
            ;; from going mad.  The pathname system should be diked out with rusty
            ;; pinking shears. <23-Nov-88 smh>
            :CANONICAL-TYPE (IF (memq CTYPE *known-non-source-file-types*)
                                :LISP CTYPE)
            :ORIGINAL-TYPE OTYPE))))

(DEFMETHOD (PATHNAME :TRANSLATED-PATHNAME) () SELF)

(DEFMETHOD (PATHNAME :BACK-TRANSLATED-PATHNAME) (PATHNAME) PATHNAME)

;;; This is used to parse a string which may not have the host in it
(DEFMETHOD (PATHNAME :PARSE-TRUENAME) (STRING)
  (PARSE-PATHNAME STRING HOST))

(DEFUN DECODE-CANONICAL-TYPE (CANONICAL-TYPE SYSTEM-TYPE)
  (LET ((PROP (GETF CANONICAL-TYPES CANONICAL-TYPE)))
    (IF (NULL PROP)
        CANONICAL-TYPE
      (LET ((PER-SYSTEM (OR (ASSQ SYSTEM-TYPE PROP) (ASSQ NIL PROP))))
        (VALUES (CADR PER-SYSTEM) (CDR PER-SYSTEM))))))

(DEFMACRO DEFINE-CANONICAL-TYPE (CANONICAL-TYPE DEFAULT-SURFACE-TYPE &BODY SYSTEM-SURFACE-TYPES)
  "Defines a keyword CANONICAL-TYPE as a canonical type.
DEFAULT-SURFACE-TYPE is the string that it corresponds to.
SYSTEM-SURFACE-TYPES overrides that default for specific kinds of file systems.
Each element of it is a list whose CAR is a file-system keyword
/(:ITS, :UNIX, etc). and whose remaining elements are surface strings,
all of which correspond to this canonical type.
The first surface string in each list is the preferred one for that system."
  `(DEFINE-CANONICAL-TYPE-1 ',CANONICAL-TYPE ',DEFAULT-SURFACE-TYPE ',SYSTEM-SURFACE-TYPES))

(DEFUN DEFINE-CANONICAL-TYPE-1 (CTYPE DSTY SSTYS)
  (LET ((ALIST (LIST (LIST NIL DSTY))))
    (DOLIST (ELT SSTYS)
      (IF (SYMBOLP (CAR ELT))
          (PUSH ELT ALIST)
        (DOLIST (SYSTEM (CAR ELT))
          (PUSH (CONS SYSTEM (CDR ELT)) ALIST))))
    (IF (GET (LOCF CANONICAL-TYPES) CTYPE)
        (PUTPROP (LOCF CANONICAL-TYPES) ALIST CTYPE)
      (SETF (CDR (OR (LAST CANONICAL-TYPES)
                     (LOCF CANONICAL-TYPES)))
            (LIST* CTYPE ALIST NIL)))))

(DEFINE-CANONICAL-TYPE :LISP "LISP"
  ((:TOPS-20 :TENEX) "LISP" "LSP")
  (:UNIX "L" "LISP")
  (:VMS "LSP"))

(DEFINE-CANONICAL-TYPE :QFASL "QFASL"
  (:UNIX "QF")
  (:VMS "QFS"))

;;; $$$ Removed the dog meat comment and added KENV files <09-Nov-88 JIM>
(DEFINE-CANONICAL-TYPE :KFASL "KFASL")  ;fleabit compiled files for FALCON.
(DEFINE-CANONICAL-TYPE :KENV "KENV")    ;environment for fleabit compiled files for FALCON.

(DEFINE-CANONICAL-TYPE :FBIN "FBIN")    ;compiled files for the FALCON.

(DEFINE-CANONICAL-TYPE :FDEF "FDEF")    ;cross-compilation environments for the FALCON.

(DEFINE-CANONICAL-TYPE :MIDAS "MIDAS"
  ((:TOPS-20 :TENEX) "MID" "MIDAS")
  (:UNIX "MD")
  (:VMS "MID"))

(DEFINE-CANONICAL-TYPE :MAC "MAC")
(DEFINE-CANONICAL-TYPE :TASM "TASM")

(DEFINE-CANONICAL-TYPE :PALX "PALX")

(DEFINE-CANONICAL-TYPE :TEXT "TEXT"
  ((:TOPS-20 :TENEX) "TEXT" "TXT")
  (:UNIX "TX")
  (:VMS "TXT"))

(define-canonical-type :botex "BOTEX"
  (:vms "TEX"))

(DEFINE-CANONICAL-TYPE :DOC "DOC")
(DEFINE-CANONICAL-TYPE :MSS "MSS")
(DEFINE-CANONICAL-TYPE :TEX "TEX")

(DEFINE-CANONICAL-TYPE :PRESS "PRESS"
  (:UNIX "PR")
  (:VMS "PRS"))

(DEFINE-CANONICAL-TYPE :IMPRESS "IMPRESS")
(DEFINE-CANONICAL-TYPE :DVI "DVI")

(DEFINE-CANONICAL-TYPE :PATCH-DIRECTORY "PATCH-DIRECTORY"
  (:ITS "(PDIR)")
  (:UNIX "PD")
  (:VMS "PDR")
  (:LMFS "PDIR"))

(DEFINE-CANONICAL-TYPE :LOGICAL-PATHNAME-TRANSLATIONS "TRANSLATIONS"
  (:ITS "LOGTRN")
  (:UNIX "LT" "LOGTRAN")
  (:VMS "LTR"))

(DEFINE-CANONICAL-TYPE :QWABL "QWABL"
  (:UNIX "QW")
  (:VMS "QWB"))

(DEFINE-CANONICAL-TYPE :BABYL "BABYL"
  (:UNIX "BB")
  (:VMS "BAB"))

(DEFINE-CANONICAL-TYPE :XMAIL "XMAIL"
  (:UNIX "XM")
  (:VMS "XML"))

(DEFINE-CANONICAL-TYPE :MAIL "MAIL"
  (:UNIX "MA")
  (:VMS "MAI"))

(DEFINE-CANONICAL-TYPE :INIT "INIT"
  (:UNIX "IN")
  (:VMS "INI"))

(DEFINE-CANONICAL-TYPE :UNFASL "UNFASL"
  (:UNIX "UF")
  (:VMS "UNF"))

(DEFINE-CANONICAL-TYPE :OUTPUT "OUTPUT"
  (:UNIX "OT")
  (:VMS "OUT"))

(DEFINE-CANONICAL-TYPE :WIDTHS "WIDTHS"
  (:UNIX "WD")
  (:VMS "WID"))

(DEFINE-CANONICAL-TYPE :PL1 "PL1")
(DEFINE-CANONICAL-TYPE :CLU "CLU")
(DEFINE-CANONICAL-TYPE :C "C")

(define-canonical-type :scheme "SCHEME"
  (:unix "SCM")
  (:vms "SCM"))

(define-canonical-type :t "T")

(DEFMETHOD (PATHNAME :SYSTEM-TYPE) ()
  (SEND HOST :SYSTEM-TYPE))

(DEFUN FIND-FILE-WITH-TYPE (FILE CANONICAL-TYPE)
  "Try to open a file with some type that matches CANONICAL-TYPE.
All other components come from FILE, a pathname or string, or are defaulted.
Return the file's truename if successful, NIL if file-not-found.
Any other error condition is not handled."
  (CONDITION-CASE (STREAM)
      (SEND (MERGE-PATHNAME-DEFAULTS FILE) :OPEN-CANONICAL-TYPE CANONICAL-TYPE NIL
            :DIRECTION NIL)
    (FILE-NOT-FOUND NIL)
    (:NO-ERROR (PROG1 (SEND STREAM :TRUENAME) (CLOSE STREAM)))))

(DEFMETHOD (PATHNAME :TYPES-FOR-CANONICAL-TYPE) (CANONICAL-TYPE)
  (MULTIPLE-VALUE-BIND (NIL TEM)
      (DECODE-CANONICAL-TYPE CANONICAL-TYPE (SEND HOST :SYSTEM-TYPE))
    TEM))

(DEFMETHOD (PATHNAME :OPEN-CANONICAL-DEFAULT-TYPE) (CANONICAL-TYPE &REST ARGS)
  (IF TYPE
      (LEXPR-SEND SELF :OPEN SELF ARGS)
    (LEXPR-SEND SELF :OPEN-CANONICAL-TYPE CANONICAL-TYPE SELF ARGS)))

(DEFMETHOD (PATHNAME :OPEN-CANONICAL-TYPE)
           (CANONICAL-TYPE PRETRANSLATED-PATHNAME &REST ARGS
            &KEY (ERROR T) &ALLOW-OTHER-KEYS)
  (LET ((SURFACE-TYPES (SEND SELF :TYPES-FOR-CANONICAL-TYPE CANONICAL-TYPE)))
    (DO ((TYPES SURFACE-TYPES (CDR TYPES)))
        ((NULL TYPES)
         (LET ((CONDITION
                 ;; If no file found, signal with the preferred surface type.
                 (MAKE-CONDITION 'FILE-NOT-FOUND "File not found for ~A."
                                 (SEND SELF :NEW-TYPE CANONICAL-TYPE)
                                 :OPEN)))
           (IF ERROR (SIGNAL-CONDITION CONDITION) CONDITION)))
      (CONDITION-CASE (RESULT)
          (LEXPR-SEND (SEND SELF :NEW-PATHNAME :TYPE (CAR TYPES)) :OPEN
                      (SEND (OR PRETRANSLATED-PATHNAME SELF) :NEW-PATHNAME
                            :TYPE (IF (EQ TYPES SURFACE-TYPES)
                                       CANONICAL-TYPE
                                     (CAR TYPES)))
                      :ERROR T
                      ARGS)
        (FILE-NOT-FOUND NIL)
        (:NO-ERROR (RETURN RESULT))))))

;;; Creation of pathnames, normal user interface.

;;These two methods are right for file systems where upper case is normally used.
(DEFMETHOD (PATHNAME :NEW-SUGGESTED-NAME) (NEW-NAME)
  (SEND SELF :NEW-PATHNAME :NAME (STRING-UPCASE NEW-NAME)))

(DEFMETHOD (PATHNAME :NEW-SUGGESTED-DIRECTORY) (NEW-DIRECTORY)
  (SEND SELF :NEW-PATHNAME :DIRECTORY (STRING-UPCASE NEW-DIRECTORY)))

(DEFMETHOD (PATHNAME :NEW-DEVICE) (NEW-DEVICE)
  (SEND SELF :NEW-PATHNAME :DEVICE NEW-DEVICE))

(DEFMETHOD (PATHNAME :NEW-DIRECTORY) (NEW-DIRECTORY)
  (SEND SELF :NEW-PATHNAME :DIRECTORY NEW-DIRECTORY))

(DEFMETHOD (PATHNAME :NEW-NAME) (NEW-NAME)
  (SEND SELF :NEW-PATHNAME :NAME NEW-NAME))

(DEFMETHOD (PATHNAME :NEW-TYPE) (NEW-TYPE)
  (SEND SELF :NEW-PATHNAME :TYPE NEW-TYPE))

(DEFMETHOD (PATHNAME :NEW-VERSION) (NEW-VERSION)
  (SEND SELF :NEW-PATHNAME :VERSION NEW-VERSION))

(DEFMETHOD (PATHNAME :NEW-RAW-DEVICE) (NEW-DEVICE)
  (SEND SELF :NEW-PATHNAME :RAW-DEVICE NEW-DEVICE))

(DEFMETHOD (PATHNAME :NEW-RAW-DIRECTORY) (NEW-DIRECTORY)
  (SEND SELF :NEW-PATHNAME :RAW-DIRECTORY NEW-DIRECTORY))

(DEFMETHOD (PATHNAME :NEW-RAW-NAME) (NEW-NAME)
  (SEND SELF :NEW-PATHNAME :RAW-NAME NEW-NAME))

(DEFMETHOD (PATHNAME :NEW-RAW-TYPE) (NEW-TYPE)
  (SEND SELF :NEW-PATHNAME :RAW-TYPE NEW-TYPE))

(DEFMETHOD (PATHNAME :NEW-CANONICAL-TYPE) (CANONICAL-TYPE &OPTIONAL ORIGINAL-TYPE)
  (SEND SELF :NEW-PATHNAME :CANONICAL-TYPE CANONICAL-TYPE :ORIGINAL-TYPE ORIGINAL-TYPE))

;;; These exist for the sake of ITS
(DEFMETHOD (PATHNAME :NEW-TYPE-AND-VERSION) (NEW-TYPE NEW-VERSION)
  (SEND SELF :NEW-PATHNAME :TYPE NEW-TYPE :VERSION NEW-VERSION))

(DEFMETHOD (PATHNAME :TYPE-AND-VERSION) ()
  (VALUES TYPE VERSION))

(DEFMETHOD (PATHNAME :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP &REST ARGS)
  (LET ((PATOM (STRING-UPCASE (IF SAME-DIRECTORY-P PATOM NAM))))
    (SELECTQ TYP
      (:SYSTEM-DIRECTORY
       (SEND SELF :NEW-PATHNAME :NAME PATOM
                  :TYPE (IF SAME-DIRECTORY-P "DIRECTORY" :PATCH-DIRECTORY)
                  :VERSION :NEWEST))
      (:VERSION-DIRECTORY
       (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~A-~D" PATOM (CAR ARGS))
                  :TYPE (IF SAME-DIRECTORY-P "DIRECTORY" :PATCH-DIRECTORY)
                  :VERSION :NEWEST))
      (:PATCH-FILE
       (SEND SELF :NEW-PATHNAME
                  :NAME (FORMAT NIL "~A-~D-~D" PATOM (CAR ARGS) (CADR ARGS))
                  :TYPE (CADDR ARGS)
                  :VERSION :NEWEST)))))

(DEFUN INIT-FILE-PATHNAME (PROGRAM-NAME &OPTIONAL (HOST USER-LOGIN-MACHINE))
  "Return the pathname for PROGRAM-NAME's init file, on host HOST.
FORCE-P means don't get an error if HOST cannot be contacted; guess instead."
  (SEND (USER-HOMEDIR HOST NIL USER-ID) :INIT-FILE (STRING PROGRAM-NAME)))

(DEFMETHOD (HOST-PATHNAME :INIT-FILE) (PROGRAM-NAME)
  (SEND SELF :NEW-PATHNAME :NAME PROGRAM-NAME
                           :TYPE :INIT
                           :VERSION :NEWEST))

(DEFUN COMPUTE-HOMEDIR-FROM-USER-ID (USER HOST)
  "Return the best homedir name for user-id as we can, without a file server."
  ;; This might want to depend on the host's system type;
  ;; perhaps using an operation defined by host type mixins.
  (IGNORE HOST)
  USER)

(DEFMETHOD (PATHNAME :QUIET-HOMEDIR) (&OPTIONAL (USER USER-ID))
  (SEND (MAKE-PATHNAME :HOST HOST
                       :DEVICE (SEND SELF :PRIMARY-DEVICE))
        :NEW-SUGGESTED-DIRECTORY (COMPUTE-HOMEDIR-FROM-USER-ID USER HOST)))

;;; Make a guess for the home directory if we don't have it.  Don't ask the host.
;(DEFMETHOD (PATHNAME :QUIET-HOMEDIR) ()
;  (SEND (MAKE-PATHNAME :HOST HOST :DEVICE (SEND SELF :PRIMARY-DEVICE))
;       :NEW-SUGGESTED-DIRECTORY USER-ID))

(DEFUN MAKE-PATHNAME (&REST OPTIONS
                      &KEY (DEFAULTS T)
                           (HOST (IF (EQ DEFAULTS T)
                                     (DEFAULT-HOST *DEFAULT-PATHNAME-DEFAULTS*)
                                   (DEFAULT-HOST DEFAULTS)))
                      &ALLOW-OTHER-KEYS)
  "Create a pathname, specifying components as keyword arguments.
If DEFAULTS is a pathname or a defaults list, the pathname is defaulted from it.
If DEFAULTS is T (the default), the host is defaulted from
*DEFAULT-PATHNAME-DEFAULTS* and the other components are not defaulted at all."
  (DECLARE (ARGLIST &KEY (DEFAULTS T)
                         HOST DEVICE RAW-DEVICE DIRECTORY RAW-DIRECTORY
                         NAME RAW-NAME TYPE RAW-TYPE VERSION
                         CANONICAL-TYPE ORIGINAL-TYPE))
  (IF (NOT (SYMBOLP DEFAULTS))
      (MERGE-PATHNAME-DEFAULTS
        (LEXPR-SEND (SAMPLE-PATHNAME HOST) :NEW-PATHNAME OPTIONS)
        DEFAULTS)
    (LEXPR-SEND (SAMPLE-PATHNAME HOST) :NEW-PATHNAME OPTIONS)))

(DEFUN SAMPLE-PATHNAME (HOST)
  "Return a pathname for HOST with all other components NIL."
  (send (get-pathname-host host) :sample-pathname))

;;; Because some pathname flavors can change, we've got to uncache the old sample pathname if
;;; it has changed.
(DEFUN FIX-SAMPLE-PATHNAMES-SITE ()
  (DOLIST (HOST *PATHNAME-HOST-LIST*)
    (WHEN (GET (SEND HOST :PATHNAME-FLAVOR) 'PATHNAME-FLAVOR-CHANGES)
      (send host :send-if-handles :reset-sample-pathname))))

(ADD-INITIALIZATION "Reset Sample Pathnames" '(FIX-SAMPLE-PATHNAMES-SITE) '(:SITE-OPTION))

;;; Make sure that a :NEW-PATHNAME which specifies a new host
;;; is processed by the flavor of pathname for that host.
(DEFWRAPPER (PATHNAME :NEW-PATHNAME) (OPTIONS . BODY)
  `(LET ((NEW-HOST (GETF OPTIONS :HOST))
         (NEW-PATHNAME-HOST NIL))
     (IF (AND NEW-HOST
              (SETQ NEW-PATHNAME-HOST (GET-PATHNAME-HOST NEW-HOST))
              (NEQ HOST NEW-PATHNAME-HOST))
         (LEXPR-SEND (SAMPLE-PATHNAME NEW-PATHNAME-HOST)
                     :NEW-PATHNAME
                     :STARTING-PATHNAME (OR (GET (LOCF OPTIONS) :STARTING-PATHNAME)
                                             SELF)
                     OPTIONS)
         . ,BODY)))

;;; This is the fundamental way of altering some of the components of a pathname.
;;; Specify an alternating list of components and values.
;;; Components allowed are :HOST, :DEVICE, :DIRECTORY, :NAME, :TYPE and :VERSION;
;;; All the :NEW-x operations call this one (by default),
;;; and all the :PARSE-x operations (except :PARSE-NAMESTRING)
;;; are called only by MAKE-PATHNAME-1, which is called only from here.

;;; STARTING-PATHNAME is specified if we are doing the work on behalf of that pathname
;;; because it was told to change hosts, and we are a pathname of the correct new host.
(DEFMETHOD (PATHNAME :NEW-PATHNAME) (&REST OPTIONS
                                     &KEY STARTING-PATHNAME
                                     &ALLOW-OTHER-KEYS)
  (APPLY #'MAKE-PATHNAME-1
         :STARTING-PATHNAME (OR STARTING-PATHNAME SELF)
         :PARSING-PATHNAME SELF
         OPTIONS))

;;; MAKE-PATHNAME is equivalent to this if the standard :NEW-PATHNAME method is in use.
;;; MAKE-PATHNAME could do this directly, except that would take away
;;; the ability for some pathname flavor to replace this entirely.
(DEFUN MAKE-PATHNAME-1 (&REST OPTIONS &KEY &ALLOW-OTHER-KEYS
                        STARTING-PATHNAME &OPTIONAL (PARSING-PATHNAME STARTING-PATHNAME)
                        &OPTIONAL (HOST NIL HOST-P) (VERSION NIL VERSION-P)
                        (ORIGINAL-TYPE NIL ORIGINAL-TYPE-P)
                        &AUX DEVICE-P DIRECTORY-P NAME-P TYPE-P
                        DEVICE DIRECTORY NAME TYPE CANONICAL-TYPE)
  (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
        DO
        (SELECTQ KEYWORD
          (:NAME (UNLESS NAME-P (SETQ NAME VALUE NAME-P T)))
          (:RAW-NAME (UNLESS NAME-P (SETQ NAME VALUE NAME-P :RAW)))
          (:DIRECTORY (UNLESS DIRECTORY-P (SETQ DIRECTORY VALUE DIRECTORY-P T)))
          (:RAW-DIRECTORY (UNLESS DIRECTORY-P (SETQ DIRECTORY VALUE DIRECTORY-P :RAW)))
          (:DEVICE (UNLESS DEVICE-P (SETQ DEVICE VALUE DEVICE-P T)))
          (:RAW-DEVICE (UNLESS DEVICE-P (SETQ DEVICE VALUE DEVICE-P :RAW)))
          (:TYPE (UNLESS TYPE-P
                   (IF (AND (SYMBOLP VALUE) (NOT (MEMQ VALUE '(NIL :UNSPECIFIC))))
                       (SETQ CANONICAL-TYPE VALUE TYPE-P :CANONICAL)
                     (SETQ TYPE VALUE TYPE-P T))))
          (:RAW-TYPE (UNLESS TYPE-P (SETQ TYPE VALUE TYPE-P :RAW)))
          (:CANONICAL-TYPE
           (UNLESS TYPE-P (SETQ CANONICAL-TYPE VALUE TYPE-P :CANONICAL)))
          ;; All keywords that do NOT require special decoding must go here.
          ((:HOST :VERSION :STARTING-PATHNAME :PARSING-PATHNAME
            :ORIGINAL-TYPE :DEFAULTS NIL)
           NIL)
          (T (FERROR NIL "Unknown keyword ~S to MAKE-PATHNAME or :NEW-PATHNAME." KEYWORD))))
  (UNLESS HOST-P
    (SETQ HOST (PATHNAME-HOST STARTING-PATHNAME)))
  (SETQ HOST (GET-PATHNAME-HOST HOST))
  ;; Turn a specified canonical type into a string (in standard case).
  (WHEN (EQ TYPE-P :CANONICAL)
    (MULTIPLE-VALUE-BIND (PREFERRED ALL)
        (DECODE-CANONICAL-TYPE CANONICAL-TYPE (SEND HOST :SYSTEM-TYPE))
      (UNLESS ORIGINAL-TYPE-P
        (SETQ ORIGINAL-TYPE (PATHNAME-TYPE STARTING-PATHNAME)))
      (SETQ TYPE (IF (SYS:MEMBER-EQUAL ORIGINAL-TYPE ALL)
                     ORIGINAL-TYPE
                     PREFERRED))))
  (COND ((EQ (PATHNAME-HOST STARTING-PATHNAME) HOST)
         (UNLESS DEVICE-P
           (SETQ DEVICE (PATHNAME-RAW-DEVICE STARTING-PATHNAME) DEVICE-P :RAW))
         (UNLESS DIRECTORY-P
           (SETQ DIRECTORY (PATHNAME-RAW-DIRECTORY STARTING-PATHNAME) DIRECTORY-P :RAW))
         (UNLESS NAME-P
           (SETQ NAME (PATHNAME-RAW-NAME STARTING-PATHNAME) NAME-P :RAW))
         (UNLESS TYPE-P
           (SETQ TYPE (PATHNAME-RAW-TYPE STARTING-PATHNAME) TYPE-P :RAW)))
        ;; Hosts don't match; must convert to standard syntax and reparse.
        (T
         (UNLESS DEVICE-P
           (SETQ DEVICE (PATHNAME-DEVICE STARTING-PATHNAME)))
         (UNLESS DIRECTORY-P
           (SETQ DIRECTORY (PATHNAME-DIRECTORY STARTING-PATHNAME)))
         (UNLESS NAME-P
           (SETQ NAME (PATHNAME-NAME STARTING-PATHNAME)))
         (UNLESS TYPE-P
           (SETQ TYPE (PATHNAME-TYPE STARTING-PATHNAME)))))
  (UNLESS VERSION-P
    (SETQ VERSION (PATHNAME-RAW-VERSION STARTING-PATHNAME)))
  ;; The new fields are parsed only once to save time, consing, and possible errors
  ;; due to incompatible fields in different types of pathnames.
  (WHEN (NEQ DEVICE-P :RAW)
    (SETQ DEVICE (SEND PARSING-PATHNAME :PARSE-DEVICE-SPEC DEVICE)))
  (WHEN (NEQ DIRECTORY-P :RAW)
    (SETQ DIRECTORY (SEND PARSING-PATHNAME :PARSE-DIRECTORY-SPEC DIRECTORY)))
  (WHEN (NEQ NAME-P :RAW)
    (SETQ NAME (SEND PARSING-PATHNAME :PARSE-NAME-SPEC NAME)))
  (WHEN (NEQ TYPE-P :RAW)
    (SETQ TYPE (SEND PARSING-PATHNAME :PARSE-TYPE-SPEC TYPE)))
  (SETQ VERSION (SEND PARSING-PATHNAME :PARSE-VERSION-SPEC VERSION))
  (MAKE-PATHNAME-INTERNAL HOST DEVICE DIRECTORY NAME TYPE VERSION))

(DEFSUBST FAST-NEW-PATHNAME (PATHNAME NEW-DEVICE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION)
  "Modify those fields of PATHNAME that are supplied as non-NIL arguments."
  (SEND PATHNAME :NEW-PATHNAME
        (AND NEW-DEVICE :DEVICE) NEW-DEVICE
        (AND NEW-DIRECTORY :DIRECTORY) NEW-DIRECTORY
        (AND NEW-NAME :NAME) NEW-NAME
        (AND NEW-TYPE :TYPE) NEW-TYPE
        (AND NEW-VERSION :VERSION) NEW-VERSION))

(DEFUN PATHNAME-PASS-THROUGH-SPEC (IGNORE SPEC)
  (SEND SELF :PARSE-COMPONENT-SPEC SPEC))

;;; Default is to leave the string alone
(DEFMETHOD (PATHNAME :PARSE-COMPONENT-SPEC) (SPEC)
  SPEC)

;;; These operations should in general convert an interchange component to a raw one
;;; and also turn any invalid component into something valid.

(DEFMETHOD (PATHNAME :PARSE-DEVICE-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DEVICE (QUIET-USER-HOMEDIR HOST)))))

(DEFMETHOD (PATHNAME :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

(DEFMETHOD (PATHNAME :PARSE-NAME-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T "FOO")))

(DEFMETHOD (PATHNAME :PARSE-TYPE-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (DECODE-CANONICAL-TYPE :LISP (SEND HOST :SYSTEM-TYPE)))))

;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:42
;;; Since there are no "interchange" versions,
;;; this is only to convert invalid ones to valid ones.
(DEFMETHOD (PATHNAME :PARSE-VERSION-SPEC) (SPEC)
  (IF (OR (AND (FIXNUMP SPEC) (> SPEC 0))
          (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
      SPEC
      :NEWEST))

(DEFFLAVOR PATHNAME-NORMALLY-LOWERCASE-MIXIN () ()
  (:REQUIRED-FLAVORS PATHNAME))

(DEFUN CONVERT-SOLID-CASE (OBJECT)
  (COND ((STRINGP OBJECT)
         (LET (SOME-LC SOME-UC)
           (DOTIMES (I (LENGTH OBJECT))
             (LET ((CH (AREF OBJECT I)))
               (IF (UPPER-CASE-P CH) (SETQ SOME-UC T))
               (IF (LOWER-CASE-P CH) (SETQ SOME-LC T))))
           (IF (NEQ SOME-UC SOME-LC)
               (IF SOME-UC (STRING-DOWNCASE OBJECT) (STRING-UPCASE OBJECT))
             OBJECT)))
        ((CONSP OBJECT)
         (MAPCAR #'CONVERT-SOLID-CASE OBJECT))
        (T OBJECT)))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :TRANSLATION-CASE-CONVERTER) ()
  'CONVERT-SOLID-CASE)

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (CONVERT-SOLID-CASE SPEC))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :NAME) ()
  (CONVERT-SOLID-CASE NAME))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :DEVICE) ()
  (CONVERT-SOLID-CASE DEVICE))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :DIRECTORY) ()
  (CONVERT-SOLID-CASE DIRECTORY))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :TYPE) ()
  (CONVERT-SOLID-CASE TYPE))

;;; These two are right for systems where mixed case is normally used.
(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :NEW-SUGGESTED-NAME) (NEW-NAME)
  (SEND SELF :NEW-PATHNAME :NAME NEW-NAME))

(DEFMETHOD (PATHNAME-NORMALLY-LOWERCASE-MIXIN :NEW-SUGGESTED-DIRECTORY) (NEW-DIRECTORY)
  (SEND SELF :NEW-PATHNAME :DIRECTORY NEW-DIRECTORY))

;;;; Creation of pathnames, low level.

(DEFVAR *PATHNAME-HASH-TABLE* :UNBOUND
  "This is the EQUAL-hash-table used for uniquizing pathnames.")

;;; *PATHNAME-HASH-TABLE* is not address-dependent anymore.  KHS 850805.
;(ADD-INITIALIZATION 'REHASH-PATHNAME-HASH-TABLE
;                   '(GETHASH NIL *PATHNAME-HASH-TABLE*)
;                   '(AFTER-FULL-GC))
;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:43
(defun purge-pathname-hash-table (&aux (count 0))
  (maphash #'(lambda (key &rest ignore)
               (unless (typecase (car key)
                         (logical-pathname (memq (car key) *logical-pathname-host-list*))
                         (t (memq (car key) *pathname-host-list*)))
                 (incf count)
                 (format t "~&Removing a pathname for host /"~a/".")
                 (remhash key *pathname-hash-table*)))
           *pathname-hash-table*))


(DEFUN MAKE-PATHNAME-INTERNAL (&REST REST &AUX PATHNAME FLAVOR-NAME OPTIONS)
  "Create a pathname from components specified positionally, with no defaulting.
All components are raw."
  (DECLARE (ARGLIST HOST DEVICE DIRECTORY NAME TYPE VERSION)
           (VALUES PATHNAME FOUND-IN-HASH-TABLE-P))
  (LET ((ALPHABETIC-CASE-AFFECTS-STRING-COMPARISON T))
    (SETQ PATHNAME
          (GETHASH REST *PATHNAME-HASH-TABLE*)))
  (IF (GET (TYPE-OF PATHNAME) 'PATHNAME-FLAVOR-CHANGES)
      (MULTIPLE-VALUE (FLAVOR-NAME OPTIONS)
        (SEND (CAR REST) :PATHNAME-FLAVOR)))
  (IF (AND PATHNAME (OR (NULL FLAVOR-NAME) (EQ (TYPE-OF PATHNAME) FLAVOR-NAME)))
      (VALUES PATHNAME T)
    (LET ((OPATHNAME PATHNAME))
      (SETQ REST (COPY-INTO-PATHNAME-AREA REST))
      (OR FLAVOR-NAME
          (MULTIPLE-VALUE (FLAVOR-NAME OPTIONS)
            (SEND (CAR REST) :PATHNAME-FLAVOR)))
      (SETQ PATHNAME (APPLY 'MAKE-PATHNAME-INSTANCE
                            FLAVOR-NAME
                            :HOST (FIRST REST)
                            :DEVICE (SECOND REST)
                            :DIRECTORY (THIRD REST)
                            :NAME (FOURTH REST)
                            :TYPE (FIFTH REST)
                            :VERSION (SIXTH REST)
                            OPTIONS))
      (IF OPATHNAME (SEND PATHNAME :SETPLIST (SEND OPATHNAME :PLIST)))
      (PUTHASH REST PATHNAME *PATHNAME-HASH-TABLE*)
      (VALUES PATHNAME NIL))))

(DEFUN MAKE-PATHNAME-INSTANCE (FLAVOR-NAME &REST OPTIONS)
  (INSTANTIATE-FLAVOR FLAVOR-NAME (LOCF OPTIONS) T NIL PATHNAME-AREA))

(DEFUN COPY-INTO-PATHNAME-AREA (OBJ)
  "Return a copy of OBJ in PATHNAME-AREA.
All levels of lists and string in OBJ are copied
unless they are already in PATHNAME-AREA."
  (IF (OR (FIXNUMP OBJ) (TYPEP OBJ 'SHORT-FLOAT) (= (%AREA-NUMBER OBJ) PATHNAME-AREA))
      OBJ
    (COND ((CONSP OBJ)
           (SETQ OBJ (COPYLIST OBJ PATHNAME-AREA))
           (DO ((O OBJ (CDR O))) (NIL)
             (SETF (CAR O) (COPY-INTO-PATHNAME-AREA (CAR O)))
             (WHEN (ATOM (CDR O))
               (AND (CDR O)
                    (SETF (CDR O) (COPY-INTO-PATHNAME-AREA (CDR O))))
               (RETURN OBJ))))
          ((STRINGP OBJ)
           (LET ((DEFAULT-CONS-AREA PATHNAME-AREA))
             (STRING-APPEND OBJ)))
          (T OBJ))))

(DEFMETHOD (PATHNAME :FASD-FORM) ()
  `(MAKE-FASLOAD-PATHNAME ',(SEND HOST :NAME-AS-FILE-COMPUTER)
                          ',DEVICE ',DIRECTORY ',NAME ',TYPE ',VERSION))

(DEFUN MAKE-FASLOAD-PATHNAME (HOST DEVICE DIRECTORY NAME TYPE VERSION &AUX PATH-HOST PATH)
  ;; Don't bomb out if the file computer that compiled this file doesn't exist any more.
  ;; Just take the one the file is being loaded from.
  (AND (SETQ PATH-HOST (GET-PATHNAME-HOST HOST T))
       (SETQ HOST PATH-HOST))
  (OR PATH-HOST (SETQ PATH-HOST (IF SI:FDEFINE-FILE-PATHNAME
                                    (PATHNAME-HOST SI:FDEFINE-FILE-PATHNAME)
                                    USER-LOGIN-MACHINE)))
  (SETQ PATH (MAKE-PATHNAME-INTERNAL PATH-HOST DEVICE DIRECTORY NAME TYPE VERSION))
  ;; Record the actual host for possible debugging.
  (AND (NEQ HOST PATH-HOST)
       (SEND PATH :PUTPROP HOST :FASLOAD-HOST))
  PATH)

;;;; Comparison of pathnames.

(DEFUN PATHNAME-EQUAL (PATHNAME1 PATHNAME2)
  "T if the two pathnames match by components.
The same as EQ for most flavors of pathname, but not for all.
In this normal case, we must swap in the first arg but not the second."
  (SEND PATHNAME1 :EQUAL PATHNAME2))

(DEFMETHOD (PATHNAME :EQUAL) (OTHER-PATHNAME)
  (EQ OTHER-PATHNAME SELF))

(DEFUN PATHNAME-LESSP (PATHNAME-1 PATHNAME-2)
  "Standard comparison of pathnames, for sorting directory listings."
  (SEND PATHNAME-1 :SORT-LESSP PATHNAME-2))

;;; Redefine this if your standard components and raw ones
;;; fail to match in a way that affects sorting (not just case)
(DEFMETHOD (PATHNAME :SORT-COMPONENTS) ()
  (VALUES HOST DEVICE DIRECTORY NAME TYPE VERSION))

;;;; Redefine this too, to use your standard components.
(DEFMETHOD (PATHNAME :SORT-LESSP) (OTHER-PATHNAME &AUX TEM)
  (MULTIPLE-VALUE-BIND (OHOST ODEVICE ODIRECTORY ONAME OTYPE OVERSION)
      (SEND OTHER-PATHNAME :SORT-COMPONENTS)
    (AND (ZEROP (SETQ TEM (PATHNAME-COMPONENT-COMPARE HOST OHOST)))
         (ZEROP (SETQ TEM (PATHNAME-COMPONENT-COMPARE DEVICE ODEVICE)))
         (ZEROP (SETQ TEM (PATHNAME-COMPONENT-COMPARE DIRECTORY ODIRECTORY)))
         (ZEROP (SETQ TEM (PATHNAME-COMPONENT-COMPARE NAME ONAME)))
         (ZEROP (SETQ TEM (PATHNAME-COMPONENT-COMPARE TYPE OTYPE)))
         (SETQ TEM (PATHNAME-COMPONENT-COMPARE VERSION OVERSION)))
    (MINUSP TEM)))

(DEFUN PATHNAME-COMPONENT-COMPARE (X Y)
  (COND ((EQUAL X Y) 0)
        ((SYMBOLP X)
         (IF (SYMBOLP Y)
             (PATHNAME-KEYWORD-COMPARE X Y)
             -1))
        ((SYMBOLP Y) 1)
        ((STRINGP X)
         (IF (STRINGP Y)
             (STRING-COMPARE X Y)
             -1))
        ((STRINGP Y) 1)
        ((NUMBERP X)
         (IF (NUMBERP Y)
             (- X Y)
             -1))
        ((NUMBERP Y) 1)
        ((ALPHALESSP X Y) -1)
        (T 1)))

(DEFUN PATHNAME-KEYWORD-COMPARE (X Y)
  (COND ((EQ X :NEWEST)
         (COND ((EQ Y :NEWEST) 0)
               (T 1)))
        ((EQ Y :NEWEST)
         (COND ((EQ X :NEWEST) 0)
               (T -1)))
        (T (STRING-COMPARE X Y))))

(DEFUN DEFAULT-HOST (DEFAULTS &AUX ELEM)
  "Return the default host to use from defaults-list or pathname DEFAULTS."
  (OR DEFAULTS (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((AND DEFAULTS (ATOM DEFAULTS))
         (PATHNAME-RAW-HOST (PARSE-PATHNAME DEFAULTS)))
        (T
         (SETQ ELEM (COND ((NOT *DEFAULTS-ARE-PER-HOST*) (ASSQ NIL DEFAULTS))
                          (T (DOLIST (DEFAULT DEFAULTS) ;Last host mentioned
                               (AND (CDR DEFAULT) (RETURN DEFAULT))))))
         ;; If none better found, take the one for the login machine
         (OR (CDR ELEM)
             (SETQ ELEM (OR (ASSQ USER-LOGIN-MACHINE DEFAULTS)
                            (NCONS USER-LOGIN-MACHINE))))
         ;; If there isn't one already, build a pathname from the host of this one
         (OR (CAR ELEM) (PATHNAME-HOST (CDR ELEM))))))

;;; Returns the default for the given host from defaults.
;;; INTERNAL-P means this function is being called from inside the parsing function and
;;; cannot do any parsing itself, but must just return something to accept messages.
;;; DEFAULTS can also be an atom, which is used as a default.
(DEFUN DEFAULT-PATHNAME (&OPTIONAL DEFAULTS HOST DEFAULT-TYPE DEFAULT-VERSION INTERNAL-P
                         &AUX ELEM PATHNAME HOST-TO-USE CTYPE OTYPE)
  (AND HOST (SETQ HOST (GET-PATHNAME-HOST HOST)))
  ;; Defaults '(NIL) '((NIL)) have been seen prior to login.
  (WHEN (OR (NULL DEFAULTS) (EQUAL DEFAULTS '(NIL)) (EQUAL DEFAULTS '((NIL))))
    (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((AND DEFAULTS (ATOM DEFAULTS))
         (SETQ PATHNAME (PARSE-PATHNAME DEFAULTS)))
        (T
         (SETQ ELEM (COND ((NOT *DEFAULTS-ARE-PER-HOST*) (ASSQ NIL DEFAULTS))
                          (HOST (ASSQ HOST DEFAULTS))
                          (T (DOLIST (DEFAULT DEFAULTS) ;Last host mentioned
                               (AND (CDR DEFAULT) (RETURN DEFAULT))))))
         ;; If none better found, take the one for the login machine
         (OR (CDR ELEM)
             (SETQ ELEM (OR (ASSQ USER-LOGIN-MACHINE DEFAULTS)
                            (IF (NULL USER-LOGIN-MACHINE)
                                (NCONS SI:ASSOCIATED-MACHINE)
                              (NCONS USER-LOGIN-MACHINE)))))
         ;; If there isn't one already, build a pathname from the host of this one
         (SETQ HOST-TO-USE (OR HOST (CAR ELEM) (PATHNAME-HOST (CDR ELEM))))
         (COND ((SETQ PATHNAME (CDR ELEM)))
               (INTERNAL-P
                (SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST-TO-USE NIL NIL NIL NIL NIL)))
               (T
                (SETQ PATHNAME (SEND (USER-HOMEDIR HOST-TO-USE) :NEW-PATHNAME
                                     :NAME "FOO" :TYPE *NAME-SPECIFIED-DEFAULT-TYPE*
                                     :VERSION :NEWEST))
                (SETF (CDR ELEM) PATHNAME)))))
  ;; If default-type or default-version was given, or the host has changed,
  ;; merge those in.
  (AND (OR (AND HOST (NEQ HOST (PATHNAME-HOST PATHNAME))) DEFAULT-TYPE DEFAULT-VERSION)
       (SETQ HOST (OR HOST (PATHNAME-HOST PATHNAME)))
       (IF INTERNAL-P
           (AND HOST (SETQ PATHNAME (MAKE-PATHNAME-INTERNAL HOST NIL NIL NIL NIL NIL)))
         (SETF (VALUES CTYPE OTYPE) (SEND PATHNAME :CANONICAL-TYPE))
         (SETQ PATHNAME (SEND (MAKE-PATHNAME :HOST HOST :DEFAULTS NIL)
                              :NEW-PATHNAME
                              :DIRECTORY (PATHNAME-DIRECTORY PATHNAME)
                              :DEVICE (PATHNAME-DEVICE PATHNAME)
                              :HOST (OR HOST (PATHNAME-HOST PATHNAME))
                              :NAME (PATHNAME-NAME PATHNAME)
                              :CANONICAL-TYPE CTYPE
                              :ORIGINAL-TYPE OTYPE
                              :VERSION (OR DEFAULT-VERSION (PATHNAME-VERSION PATHNAME))
                              ))))
  PATHNAME)

(DEFMETHOD (PATHNAME :NEW-DEFAULT-PATHNAME) (&REST OPTIONS)
  (LEXPR-SEND SELF :NEW-PATHNAME OPTIONS))

;(COMMENT
;;; Generate a default filename.  Most just merge in the name, etc.
;;; Used only from the preceding function.
;(DEFMETHOD (PATHNAME :NEW-DEFAULT-PATHNAME) (&REST OPTIONS)
;  (LET* ((COPY (APPEND OPTIONS NIL))
;        (PLIST (LOCF COPY))
;        (DEVNAME (GET PLIST :DEVICE))
;        (DIRNAME (GET PLIST :DIRECTORY))
;        (FILENAME (GET PLIST :NAME))
;        (FILETYPE (GET PLIST :TYPE))
;        (FILEVERSION (GET PLIST :VERSION)))
;    (PUTPROP PLIST (SEND SELF :VALID-DEVICE DEVNAME) :DEVICE)
;    (PUTPROP PLIST (SEND SELF :VALID-DIRECTORY DIRNAME) :DIRECTORY)
;    (PUTPROP PLIST (SEND SELF :VALID-NAME FILENAME) :NAME)
;    (PUTPROP PLIST (SEND SELF :VALID-TYPE FILETYPE) :TYPE)
;    (PUTPROP PLIST (SEND SELF :VALID-VERSION FILEVERSION) :VERSION)
;    (LEXPR-SEND SELF :NEW-PATHNAME COPY)))

;;; The following are used only from
;;; :NEW-DEFAULT-PATHNAME's old definition (no longer from BALDIR).

;(DEFMETHOD (PATHNAME :VALID-DEVICE) (DEVNAME)
;  (COND ((SEND SELF :VALID-DEVICE-P DEVNAME)
;        DEVNAME)
;       ((AND (CONSP DEVNAME) (NULL (CDR DEVNAME)))
;        (SEND SELF :VALID-DEVICE (CAR DEVNAME)))
;       (T
;        (PATHNAME-DEVICE (QUIET-USER-HOMEDIR HOST)))))

;(DEFMETHOD (PATHNAME :VALID-DEVICE-P) (DEVNAME)
;  (OR (STRINGP DEVNAME)
;      (MEMQ DEVNAME '(NIL :WILD :UNSPECIFIC))))

;(DEFMETHOD (PATHNAME :VALID-DIRECTORY) (DIRNAME)
;  (COND ((SEND SELF :VALID-DIRECTORY-P DIRNAME)
;        DIRNAME)
;       ((AND (CONSP DIRNAME) (NULL (CDR DIRNAME)))
;        (SEND SELF :VALID-DIRECTORY (CAR DIRNAME)))
;       (T
;        (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

;(DEFMETHOD (PATHNAME :VALID-DIRECTORY-P) (DIRNAME)
;  (OR (STRINGP DIRNAME) (MEMQ DIRNAME '(NIL :WILD :UNSPECIFIC))))

;(DEFMETHOD (PATHNAME :VALID-NAME) (NAM)
;  (COND ((SEND SELF :VALID-NAME-P NAM)
;        NAM)
;       ((AND (CONSP NAM) (NULL (CDR NAM)))
;        (SEND SELF :VALID-NAME (CAR NAM)))
;       (T "FOO")))

;(DEFMETHOD (PATHNAME :VALID-NAME-P) (NAM)
;  (OR (STRINGP NAM) (MEMQ NAM '(NIL :WILD :UNSPECIFIC))))

;(DEFMETHOD (PATHNAME :VALID-TYPE) (TYP)
;  (IF (SEND SELF :VALID-TYPE-P TYP)
;      TYP
;    :LISP))

;(DEFMETHOD (PATHNAME :VALID-TYPE-P) (TYP)
;  (OR (STRINGP TYP) (MEMQ TYP '(NIL :WILD :UNSPECIFIC))))

;(DEFMETHOD (PATHNAME :VALID-VERSION-P) (VRS)
;  (OR (AND (FIXNUMP VRS) (> VRS 0))
;      (MEMQ VRS '(NIL :UNSPECIFIC :WILD :NEWEST))))

;(DEFMETHOD (PATHNAME :VALID-VERSION) (VRS)
;  (IF (SEND SELF :VALID-VERSION-P VRS) VRS :NEWEST))

;) ;end comment

(DEFVAR PARSE-PATHNAME-FLAG NIL)

(DEFSIGNAL PATHNAME-PARSE-ERROR (PATHNAME-ERROR PATHNAME-PARSE-ERROR)
           (PARSE-END-INDEX REPORT-STRING REPORT-ARGS)
  "Any error that makes it impossible to parse a string into a pathname.")

(DEFPROP PATHNAME-ERROR T :ERROR-REPORTER)
(DEFUN PATHNAME-ERROR (INDEX LOSING-STRING REPORT-STRING &REST ARGS)
  (IF PARSE-PATHNAME-FLAG
      (THROW 'PARSE-PATHNAME INDEX)
    (FERROR 'PATHNAME-PARSE-ERROR
            "~?~%~VT~%   /"~A/"~%"
            REPORT-STRING ARGS
            (- INDEX
               1
               (OR (STRING-REVERSE-SEARCH-CHAR #/NEWLINE LOSING-STRING INDEX) -4))
            LOSING-STRING)))

(DEFUN PARSE-NAMESTRING (THING &OPTIONAL WITH-RESPECT-TO
                                         (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
                                         &KEY (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
The same as FS:PARSE-PATHNAME except that that function's args are all positional."
  (PARSE-PATHNAME THING WITH-RESPECT-TO DEFAULTS START END JUNK-ALLOWED))

(DEFUN PARSE-PATHNAME (THING &OPTIONAL WITH-RESPECT-TO (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
                       (START 0) END JUNK-ALLOWED)
  "Parse THING into a pathname and return it.
THING can be a pathname already (it is just passed back),
 a string or symbol, or a Maclisp-style namelist.
WITH-RESPECT-TO can be NIL or a host or host-name;
 if it is not NIL, the pathname is parsed for that host
 and it is an error if the pathname specifies a different host.
If WITH-RESPECT-TO is NIL, then DEFAULTS is used to get the host
 if none is specified.  DEFAULTS may be a host object in this case.
START and END are indices specifying a substring of THING to be parsed.
 They default to 0 for START and NIL (meaning end of THING) for END.
If JUNK-ALLOWED is non-NIL, parsing stops without error if
 the syntax is invalid, and this function returns NIL.
The second value is the index in THING at which parsing stopped.
 If JUNK-ALLOWED is T and there was invalid syntax,
 this is the index of the invalid character."
  (DECLARE (VALUES PARSED-PATHNAME PARSE-END-INDEX))
  (AND WITH-RESPECT-TO
       (SETQ WITH-RESPECT-TO (GET-PATHNAME-HOST WITH-RESPECT-TO)))
  (CONDITION-RESUME '((PATHNAME-ERROR) :NEW-PATHNAME T ("Proceed, supplying a new pathname.")
                      PARSE-PATHNAME-THROW-NEW-PATHNAME)
    (LET ((PARSE-PATHNAME-FLAG JUNK-ALLOWED))
      (CATCH-CONTINUATION 'PARSE-PATHNAME
          #'(LAMBDA (INDEX-OR-PATHNAME)
              (IF (NUMBERP INDEX-OR-PATHNAME)
                  (VALUES NIL (MIN (OR END (LENGTH THING)) INDEX-OR-PATHNAME))
                (VALUES INDEX-OR-PATHNAME START)))
          NIL
        (COND ((TYPEP THING 'PATHNAME)
               (AND WITH-RESPECT-TO (NEQ WITH-RESPECT-TO (PATHNAME-HOST THING))
                    (FERROR 'PATHNAME-PARSE-ERROR
                            "Host ~A in ~A does not match ~A"
                            (PATHNAME-HOST THING) THING WITH-RESPECT-TO))
               (VALUES THING START))
              ((CONSP THING)
               (SETQ THING (CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST THING))
               (LET (DEVICE DIRECTORY NAME TYPE VERSION HOST)
                 (COND ((CONSP (CAR THING))
                        (SETF `((,DEVICE ,DIRECTORY) ,NAME ,TYPE ,VERSION) THING))
                       ((NUMBERP (THIRD THING))
                        (SETF `(,NAME ,TYPE ,VERSION ,DEVICE ,DIRECTORY) THING))
                       (T
                        (SETF `(,NAME ,TYPE ,DEVICE ,DIRECTORY ,VERSION) THING)))
                 (SETQ HOST (COND ((GET-PATHNAME-HOST DEVICE T))
                                  (WITH-RESPECT-TO)
                                  ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
                                  (T (DEFAULT-HOST DEFAULTS))))
                 (AND WITH-RESPECT-TO
                      (NEQ WITH-RESPECT-TO HOST)
                      (FERROR 'PATHNAME-PARSE-ERROR
                              "Host ~A in ~A does not match ~A" HOST THING WITH-RESPECT-TO))
                 (VALUES (MAKE-PATHNAME :HOST HOST
                                        :DEVICE DEVICE :DIRECTORY DIRECTORY :NAME NAME
                                        :TYPE TYPE :VERSION VERSION)
                         START)))
              (T
               (SETQ THING (STRING THING))
               (MULTIPLE-VALUE-BIND (HOST-SPECIFIED START END)
                   (PARSE-PATHNAME-FIND-COLON THING START END)
                 ;; If the thing before the colon is really a host,
                 ;; and WITH-RESPECT-TO was specified, then they had better match
                 (AND WITH-RESPECT-TO
                      HOST-SPECIFIED
                      (NEQ WITH-RESPECT-TO HOST-SPECIFIED)
                      ;; Otherwise treat it as a device name
                      (SETQ HOST-SPECIFIED NIL START 0 END NIL))
                 (LET* ((HOST
                          (COND ((AND HOST-SPECIFIED (GET-PATHNAME-HOST HOST-SPECIFIED T)))
                                (WITH-RESPECT-TO)
                                ((TYPEP DEFAULTS 'SI:BASIC-HOST) DEFAULTS)
                                (T (DEFAULT-HOST DEFAULTS)))))
                   (MULTIPLE-VALUE-BIND (DEVICE DIRECTORY NAME TYPE VERSION PARSE-END)
                       (SEND (SAMPLE-PATHNAME HOST) :PARSE-NAMESTRING
                             (NOT (NULL HOST-SPECIFIED)) THING START END)
                     (VALUES
                       ;; If device is :NO-INTERN then immeditely return 2nd value, DIRECTORY.
                       ;; this provides a way to bypass as much of this lossage as possible
                       ;; in cases where it doesnt make sense.
                       (COND ((EQ DEVICE :NO-INTERN)
                              DIRECTORY)
                             (T
                              ;; Otherwise we assume we got the raw forms of everything.
                              (MAKE-PATHNAME-INTERNAL
                                HOST DEVICE DIRECTORY NAME TYPE VERSION)))
                       PARSE-END))))))))))

(DEFUN PARSE-PATHNAME-THROW-NEW-PATHNAME (IGNORE PATHNAME)
  (*THROW 'PARSE-PATHNAME PATHNAME))

(DEFUN CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST (X)
  (COND ((OR (NULL X) (NUMBERP X)) X)
        ((CONSP X) (MAPCAR #'CANONICALIZE-KLUDGEY-MACLISP-PATHNAME-STRING-LIST X))
        (T (STRING X))))

;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:46
(DEFUN PARSE-PATHNAME-FIND-COLON (STRING &OPTIONAL (ORIGINAL-START 0) END
                                  &AUX HOST-SPECIFIED (START ORIGINAL-START))
  (DECLARE (VALUES HOST-SPECIFIED START END))
  (UNLESS END (SETQ END (LENGTH STRING)))
  (DO ((IDX START (1+ IDX))
       (HOST-START START)
       (ONLY-WHITESPACE-P T)
       (CHAR))
      (( IDX END))
    (COND ((= (SETQ CHAR (AREF STRING IDX)) #/:)
           ;; The first atom ends with a colon, take the host from that, and
           ;; parse from the end of that.
           (SETQ HOST-SPECIFIED (SUBSTRING STRING HOST-START IDX)
                 START (1+ IDX))
           (RETURN))
          ((AND (= CHAR #/SP) ONLY-WHITESPACE-P)       ;Skip leading spaces
           (SETQ HOST-START (1+ IDX)))
          (T
           (SETQ ONLY-WHITESPACE-P NIL)
           (OR (ALPHANUMERICP CHAR)
               (= CHAR #/.)
               (= CHAR #/-)
               ;; If we get to non-alphabetic or -numeric,
               ;; then no interesting colon
               (RETURN NIL)))))
  (COND ((AND HOST-SPECIFIED
              (< START END)
              (= (AREF STRING START) #/:))
         ;; TWO COLONS IN A ROW, E.G. FOOBAR::, WHICH IS THE DECNET
         ;; UNAMBIGUOUS HOST SPECIFICATION.
         (GET-PATHNAME-HOST HOST-SPECIFIED)
         (INCF START))
        ('ELSE
         (AND (NULL HOST-SPECIFIED)
              (PLUSP END) (= (AREF STRING (1- END)) #/:)
              (SETQ HOST-SPECIFIED (STRING-REVERSE-SEARCH-CHAR #/SP STRING (1- END)))
              ;; The last character is a colon, take the host from the last atom, and
              ;; parse from the beginning to the space before that.
              (PSETQ HOST-SPECIFIED (SUBSTRING STRING (1+ HOST-SPECIFIED) (1- END))
                     END HOST-SPECIFIED))))
  ;; If it's just a colon with only whitespace before it,
  ;; believe there is no host name, but don't count the colon as part of the
  ;; per-host pathname.
  (AND (EQUAL HOST-SPECIFIED "")
       (SETQ HOST-SPECIFIED NIL))
  ;; If what looked like the host really wasn't, forget it and reset the indices
  (AND HOST-SPECIFIED
       (NULL (SETQ HOST-SPECIFIED (GET-PATHNAME-HOST HOST-SPECIFIED T)))
       (SETQ START ORIGINAL-START
             END NIL))                  ;This will be interpreted correctly
  (VALUES HOST-SPECIFIED START END))

(DEFSIGNAL UNKNOWN-PATHNAME-HOST (PATHNAME-ERROR UNKNOWN-PATHNAME-HOST)
           (NAME)
  "Used when GET-PATHNAME-HOST does not recognize the host name.")

(DEFUN GET-PATHNAME-HOST (HOST-NAME &OPTIONAL NO-ERROR-P
                          (UNKNOWN-OK (VARIABLE-BOUNDP CHAOS:MY-ADDRESS)))
  "Parse a host for use in a pathname.
HOST-NAME can be a host object or a host name.
If NO-ERROR-P is non-NIL, we return NIL if given an undefined host name."
  (FLET ((GET-HOST-FROM-LIST (LIST)
           (IF (MEMQ HOST-NAME LIST)
               HOST-NAME
             (LET ((HOST NIL))
               (DOLIST (X LIST HOST)
                 ;; We prefer an exact match: This is a hack for LMFILE to share LM27 (FC/FS)
                 (WHEN (SEND X :PATHNAME-HOST-NAMEP HOST-NAME)
                   (IF (STRING-EQUAL HOST-NAME (SEND X :NAME-AS-FILE-COMPUTER))
                       (RETURN X)
                     (SETQ HOST X)))))))) ; Non-exact match
    ;; And said MLY unto the Lusers ``Let logical hosts shadow physical hosts.''
    (COND ((GET-HOST-FROM-LIST *LOGICAL-PATHNAME-HOST-LIST*))
          ((GET-HOST-FROM-LIST *PATHNAME-HOST-LIST*))
          ;; Don't let SI:PARSE-HOST check for an unknown host here when making SYS.
          ((LET ((HOST (SI:PARSE-HOST HOST-NAME T UNKNOWN-OK)))
             (WHEN (AND HOST (SEND HOST :SEND-IF-HANDLES :FILE-HOST-P))
               (PUSHNEW HOST *PATHNAME-HOST-LIST* :TEST 'EQ)
               HOST)))
          (NO-ERROR-P NIL)
          (T (FERROR 'UNKNOWN-PATHNAME-HOST
                     "~S is not the name of a known file host" HOST-NAME)))))

;;;; Defaults alists

(DEFVAR *DEFAULT-PATHNAME-DEFAULTS* :UNBOUND
  "These are the defaults MERGE-PATHNAME-DEFAULTS uses if none are specified.")

(DEFVAR CLI:*DEFAULT-PATHNAME-DEFAULTS* :UNBOUND
  "These are the defaults pathname defaults as far as Common Lisp programs know them.
The value of this variable is a pathname.
The value cell is kludgily shared with a cell of the alist
stored in GLOBAL:*DEFAULT-PATHNAME-DEFAULTS*.")

(DEFCONST *DEFAULTS-ARE-PER-HOST* NIL
  "Non-NIL means each default-list should keep a separate default file name for each host.
NIL means defaults are independent of host.")

;;; Returns an alist that you can pass to the functions below that take a set of defaults.
(DEFUN MAKE-PATHNAME-DEFAULTS (&AUX LIST HOSTS)
  "Create an empty defaults-list for use with MERGE-PATHNAME-DEFAULTS."
  (SETQ HOSTS (APPEND *LOGICAL-PATHNAME-HOST-LIST* *PATHNAME-HOST-LIST*))
  (SETQ LIST (MAKE-LIST (1+ (LENGTH HOSTS))))
  (DO ((L2 LIST (CDR L2))
       (L1 HOSTS (CDR L1)))
      ((NULL L2))
    (SETF (CAR L2) (NCONS (CAR L1))))
  LIST)

(DEFUN COPY-PATHNAME-DEFAULTS (DEFAULTS)
  "Copy a defaults-list, returning a new defaults-list."
  (COPYALIST DEFAULTS))

(DEFUN SET-DEFAULT-PATHNAME (PATHNAME &OPTIONAL DEFAULTS &AUX ELEM)
  "Alter the defaults in the defaults-list DEFAULTS from PATHNAME.
DEFAULTS defaults to *DEFAULT-PATHNAME-DEFAULTS*."
  (SETQ DEFAULTS (OR DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (SETQ PATHNAME (PARSE-PATHNAME PATHNAME NIL DEFAULTS))
  (OR (SETQ ELEM (ASSQ (PATHNAME-HOST PATHNAME) DEFAULTS))
      (SETQ ELEM (NCONS (PATHNAME-HOST PATHNAME))))
  (SETF (CDR ELEM) PATHNAME)
  (PULL ELEM DEFAULTS)                          ;This is the default host
  (AND (NOT *DEFAULTS-ARE-PER-HOST*)
       (SETQ ELEM (ASSQ NIL DEFAULTS))
       (SETF (CDR ELEM) PATHNAME))
  PATHNAME)

;;; Move ITEM to the front of LIST destructively
(DEFUN PULL (ITEM LIST)
  (DO ((LS LIST (CDR LS))
       (IT ITEM))
      ((NULL LS)
       (SETQ LIST (NCONC LIST (NCONS IT))))
    (SETF (CAR LS) (PROG1 IT (SETQ IT (CAR LS))))
    (AND (EQ ITEM IT) (RETURN)))
  LIST)

;;;; Merging of defaults

;;; Setting this to T gives TENEX style pathname defaulting.
(DEFCONST *ALWAYS-MERGE-TYPE-AND-VERSION* NIL
  "T means that merging pathnames should use the default type or version
if the specified pathname does not contain one
even if the specified pathname does contain a name component.")

(DEFCONST *NAME-SPECIFIED-DEFAULT-TYPE* :LISP
  "This is the default type component to use in MERGE-PATHNAME-DEFAULTS
if the specified pathname contains a name but no type.")

;(DEFVAR HOST-WORKING-DIRECTORY-ALIST NIL
;  "Alist of elements (host-object working-directory-pathname).")

;; this is totally worthless
(DEFUN SET-HOST-WORKING-DIRECTORY (HOST PATHNAME)
  "Set the working device//directory for HOST to that in PATHNAME.
When a pathname containing device component DSK is defaulted,
its device is replaced by the working device, and its directory
defaulted (if not explicitly specified) to the working directory."
  (LET* ((HOST1 (GET-PATHNAME-HOST HOST))
         (DIR (PARSE-PATHNAME PATHNAME HOST1)))
    (SEND HOST1 :SET :GET 'WORKING-DIRECTORY DIR)))

(defun merge-pathnames (pathname &optional defaults (default-version :newest))
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If PATHNAME specifies a name component, the DEFAULT-VERSION is used in place of
 the default version derived from DEFAULTS.
Otherwise, the version is defaulted from the corresponding component from DEFAULTS."
  (setq pathname (parse-pathname pathname nil defaults))
  (if (pathname-name pathname)
      (merge-pathname-components pathname defaults :default-version default-version)
    (merge-pathname-components pathname defaults)))

(defun merge-pathname-components
       (pathname &optional defaults
                 &key (default-version nil default-version-specified-p)
                      (default-type nil default-type-specified-p)
                      (default-name nil default-name-specified-p)
                      always-merge-name always-merge-type always-merge-version
                 &aux default new-device new-directory new-name new-type new-version
                      new-otype merge-name-p merge-type-p merge-version-p)
  "Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
If supplied, DEFAULT-NAME, DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
their components if those components are not supplied by PATHNAME.
Otherwise, these components are defaulted from DEFAULTS in the usual manner.
ALWAYS-MERGE-xxx mean that the the xxx components should *always* be merged in
/(from either DEFAULT-xxx or from DEFAULTS) even if the relevant component is already
specified by PATHNAME."
  (setq pathname (parse-pathname pathname nil defaults))
  (if (null defaults) (setq defaults *default-pathname-defaults*))
  (if (not (typep pathname 'pathname))
      pathname                                  ;Some funny thing.  No defaulting possible.
    (setq default (if (atom defaults)
                      (parse-pathname defaults nil pathname)
                      (default-pathname defaults (pathname-host pathname) nil nil t)))
    ;; Merge the and device and directory in vanilla fashion
    (when (null (pathname-device pathname))
      (setq new-device (pathname-device default)))
    (let ((pdir (pathname-directory pathname))
          (ddir (pathname-directory default)))
      (cond ((null pdir)
             (setq new-directory ddir))
            ((eq (car-safe pdir) :relative)
             (setq new-directory
                   (merge-relative-directory pdir ddir)))))
    ;; merge name, type and version hirsutely
    (when (or (null (pathname-name pathname))
              always-merge-name)
      (setq new-name (if default-name-specified-p
                         default-name
                         (pathname-name default))
            merge-name-p t))
    (when (or (null (pathname-type pathname))
              always-merge-type)
      (setq merge-type-p t)
      (if default-type-specified-p
          (setq new-type default-type)
          (multiple-value-setq (new-type new-otype) (send default :canonical-type))))
    (when (or (null (pathname-version pathname))
              always-merge-version)
      (setq new-version (if default-version-specified-p
                            default-version
                            (pathname-version default))
            merge-version-p t))
    (send pathname :new-pathname
                   (if new-device :device) new-device
                   (if new-directory :directory) new-directory
                   (if merge-name-p :name) new-name
                   (if merge-type-p :type) new-type
                   (if new-otype :original-type) new-otype
                   (if merge-version-p :version) new-version)))

;;; What a crock.
;;; Fill in slots in PATHNAME from program defaults.  This is what most
;;; programs interface to.
(DEFUN MERGE-PATHNAME-DEFAULTS (PATHNAME
                                &OPTIONAL DEFAULTS
                                          (DEFAULT-TYPE *NAME-SPECIFIED-DEFAULT-TYPE*)
                                          (DEFAULT-VERSION :NEWEST)
                                          ALWAYS-MERGE-TYPE
                                &AUX HOST DEFAULT SECONDARY-DEFAULT
                                     NEW-DEVICE NEW-DIRECTORY NEW-NAME NEW-TYPE NEW-VERSION
                                     NEW-OTYPE)
  "If I were you I wouldn't use this function:
  Try MERGE-PATHNAMES and FS:MERGE-PATHNAME-COMPONENTS instead.

Default components that are NIL in PATHNAME, and return the defaulted pathname.
DEFAULTS is a pathname or a defaults-list to get defaults from.
DEFAULT-TYPE and DEFAULT-VERSION are used as the defaults for
the type and version components, iff a name was specified
and FS:*ALWAYS-MERGE-TYPE-AND-VERSION* is NIL.
Otherwise, the type and version are obtained from DEFAULTS,
and DEFAULT-TYPE and DEFAULT-VERSION are not used.
If ALWAYS-MERGE-TYPE is non-NIL, that forces the type component
to be merged like the name, directory, etc. but has no effect on the version."
  (SETQ PATHNAME (PARSE-PATHNAME PATHNAME NIL DEFAULTS))
  (IF (NULL DEFAULTS)
      (SETQ DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*))
  (COND ((NOT (TYPEP PATHNAME 'PATHNAME))
         PATHNAME)                              ;Some funny thing.  No defaulting possible.
        (T
         ;; Host always comes from pathname
         (SETQ HOST (PATHNAME-HOST PATHNAME))
         ;; Setup default pathnames.  If a pathname is supplied as the defaults,
         ;; then two levels of defaulting are needed, otherwise only one.
         (IF (ATOM DEFAULTS)                    ;if not defaults.
             (SETQ DEFAULT (PARSE-PATHNAME DEFAULTS NIL PATHNAME)
                   DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*
                   SECONDARY-DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST)
                   )
             (SETQ DEFAULT (DEFAULT-PATHNAME DEFAULTS HOST)
                   SECONDARY-DEFAULT NIL)
                   )
         ;; Device name DSK means the working directory and associated device if any.
         (COND ((EQUAL (PATHNAME-DEVICE PATHNAME) "DSK")
                (LET ((WDIR (OR (GET HOST 'WORKING-DIRECTORY) (USER-HOMEDIR HOST))))
                  (SETQ NEW-DEVICE
                        (OR (SEND WDIR :DEVICE)
                            (SEND HOST :PRIMARY-DEVICE)))
                  (IF (AND (NULL (PATHNAME-DIRECTORY PATHNAME))
                           ;; Don't do this when explicit directory supplied.
                           (NULL (PATHNAME-DIRECTORY DEFAULT))
                           (OR (NULL SECONDARY-DEFAULT)
                               (NULL (PATHNAME-DIRECTORY SECONDARY-DEFAULT))))
                      (SETQ NEW-DIRECTORY
                            (SEND WDIR :DIRECTORY))))))
         ;; Merge the device, directory, and name
         (IF (NULL (PATHNAME-DEVICE PATHNAME))
             (SETQ NEW-DEVICE
                   (OR (PATHNAME-DEVICE DEFAULT)
                       (AND (NOT (NULL SECONDARY-DEFAULT))
                            (PATHNAME-DEVICE SECONDARY-DEFAULT))
                       )))
         (UNLESS NEW-DIRECTORY
           (LET ((PDIR (PATHNAME-DIRECTORY PATHNAME))
                 (DDIR (OR (PATHNAME-DIRECTORY DEFAULT)
                           (AND (NOT (NULL SECONDARY-DEFAULT))
                                (PATHNAME-DIRECTORY SECONDARY-DEFAULT))
                           )))
             (COND ((NULL PDIR)
                    (SETQ NEW-DIRECTORY DDIR))
                   ((EQ (CAR-SAFE PDIR) :RELATIVE)
                    (SETQ NEW-DIRECTORY
                          (MERGE-RELATIVE-DIRECTORY PDIR DDIR))))))
         (IF (NULL (PATHNAME-NAME PATHNAME))
             (SETQ NEW-NAME
                   (OR (PATHNAME-NAME DEFAULT)
                       (AND (NOT (NULL SECONDARY-DEFAULT))
                            (PATHNAME-NAME SECONDARY-DEFAULT))
                       ;; Never let the name of the resulting pathname be NIL.
                       "FOO")))

         ;; Merge the type and version if the name was NIL before the above merge,
         ;; or if the user says to always do so.
         (IF (NULL (PATHNAME-TYPE PATHNAME))
             (IF (OR (NULL (PATHNAME-NAME PATHNAME))
                     ALWAYS-MERGE-TYPE
                     *ALWAYS-MERGE-TYPE-AND-VERSION*)
                 (PROGN
                   (SETF (VALUES NEW-TYPE NEW-OTYPE)
                         (SEND DEFAULT :CANONICAL-TYPE))
                   (UNLESS NEW-TYPE
                     (SETQ NEW-TYPE
                           (OR (AND (NOT (NULL SECONDARY-DEFAULT))
                                    (PATHNAME-TYPE SECONDARY-DEFAULT))
                               ;; Never let the type of the resulting pathname be NIL.
                               DEFAULT-TYPE)))
               )
               (SETQ NEW-TYPE DEFAULT-TYPE)))
         (IF (NULL (PATHNAME-VERSION PATHNAME))
             (IF (OR (NULL (PATHNAME-NAME PATHNAME))
                     *ALWAYS-MERGE-TYPE-AND-VERSION*)
                 (SETQ NEW-VERSION
                       (OR (PATHNAME-VERSION DEFAULT)
                           (AND (NOT (NULL SECONDARY-DEFAULT))
                                (PATHNAME-VERSION SECONDARY-DEFAULT))
                           ;; Never let the version of the resulting pathname be NIL.
                           DEFAULT-VERSION))
               (SETQ NEW-VERSION DEFAULT-VERSION)))
         (SEND PATHNAME :NEW-PATHNAME
                        (IF NEW-DEVICE :DEVICE) NEW-DEVICE
                        (IF NEW-DIRECTORY :DIRECTORY) NEW-DIRECTORY
                        (IF NEW-NAME :NAME) NEW-NAME
                        (IF NEW-TYPE :TYPE) NEW-TYPE
                        (IF NEW-OTYPE :ORIGINAL-TYPE) NEW-OTYPE
                        (IF NEW-VERSION :VERSION) NEW-VERSION))))

;;; A relative directory is one whose CAR is :RELATIVE and whose CDR is a a list of
;;; strings and special symbols.  The symbol :UP means step up in the hierarchy.
;;; Strings are just added onto the end of the default.
;;; E.g. (:relative "foo") ("usr" "lispm") => ("usr" "lispm" "foo")
;;;      (:relative :up "bar") ("usr" "lispm" "foo") => ("usr" "lispm" "bar")
(DEFUN MERGE-RELATIVE-DIRECTORY (RELATIVE DEFAULT &AUX DIRECTORY)
  (SETQ DIRECTORY (COND ((OR (NULL DEFAULT) (EQ DEFAULT :ROOT)) NIL)
                        ((ATOM DEFAULT) (NCONS DEFAULT))
                        (T (COPYLIST DEFAULT))))
  (DOLIST (REL (CDR RELATIVE))
    (IF (EQ REL :UP)
        (IF (NULL DIRECTORY)
            (FERROR 'PATHNAME-PARSE-ERROR "There is no superior to the root")
            (DO ((L DIRECTORY (CDR L))
                 (OL (LOCF DIRECTORY) L))
                ((NULL (CDR L)) (RPLACD OL NIL))))
        (SETQ DIRECTORY (NCONC DIRECTORY (NCONS REL)))))
  (AND (NULL (CDR DIRECTORY))
       (SETQ DIRECTORY (CAR DIRECTORY)))
  DIRECTORY)

;;; Another crock.
;;; Another handy user interface, fills in from defaults and updates them.  Useful when
;;; not prompting.
(DEFUN MERGE-AND-SET-PATHNAME-DEFAULTS (PATHNAME
                                        &OPTIONAL (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*)
                                                  (DEFAULT-TYPE *NAME-SPECIFIED-DEFAULT-TYPE*)
                                                  (DEFAULT-VERSION :NEWEST))
  "Default PATHNAME like MERGE-PATHNAME-DEFAULTS, but then set the defaults.
If DEFAULTS is a defaults-list (rather than a pathname), the specified
pathname sets the defaults."
  (SETQ PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME DEFAULTS DEFAULT-TYPE DEFAULT-VERSION))
  (AND (CONSP DEFAULTS) (SET-DEFAULT-PATHNAME PATHNAME DEFAULTS))
  PATHNAME)

;;; Used only from zwei:com-old-list-files
(DEFMETHOD (PATHNAME :DEFAULT-NAMESTRING) (NAMESTRING &OPTIONAL (DEFAULT-TYPE :UNSPECIFIC)
                                                                (DEFAULT-VERSION :NEWEST))
  (MERGE-PATHNAME-DEFAULTS NAMESTRING SELF DEFAULT-TYPE DEFAULT-VERSION))

;;;; Wildcard mapping operations.
;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:50
(DEFVAR *DEFAULT-PATHNAME-COMPONENT-MATCH* #'STRING=)


;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:51
(DEFUN PATHNAME-COMPONENT-MATCH (PATTERN SAMPLE WILD-ANY WILD-ONE
                                 &OPTIONAL RETURN-SPECS-FLAG (STRING= *DEFAULT-PATHNAME-COMPONENT-MATCH*)
                                 &AUX SPECS)
  ;; If RETURN-SPECS-FLAG, we return a list of the chars or strings
  ;; that matched the wildcards, in the order they appeared,
  ;; or T if no wildcards but the pattern does match.
  (IF (AND (CONSP PATTERN) (NULL (CDR PATTERN))) (SETQ PATTERN (CAR PATTERN)))
  (IF (AND (CONSP SAMPLE) (NULL (CDR SAMPLE))) (SETQ SAMPLE (CAR SAMPLE)))
  (COND ((AND (EQ PATTERN :WILD)
              (ATOM SAMPLE))
         (IF RETURN-SPECS-FLAG
             (IF (CONSP SAMPLE) SAMPLE (LIST SAMPLE))
           T))
        ((SYMBOLP PATTERN) (EQ PATTERN SAMPLE))
        ((NUMBERP PATTERN) (EQ PATTERN SAMPLE))
        ((CHARACTERP PATTERN) (= PATTERN SAMPLE))
        ((CONSP PATTERN)
         (AND (CONSP SAMPLE)
              (= (LENGTH PATTERN) (LENGTH SAMPLE))
              (LOOP FOR P IN PATTERN
                    FOR S IN SAMPLE
                    DO
                    (LET ((TEM
                            (PATHNAME-COMPONENT-MATCH P S WILD-ANY WILD-ONE
                                                      RETURN-SPECS-FLAG STRING=)))
                      (IF (NULL TEM) (RETURN NIL))
                      (UNLESS (EQ TEM T)
                        (SETQ SPECS (APPEND SPECS TEM))))
                    FINALLY (RETURN (OR SPECS T)))))
        ((NOT (STRINGP SAMPLE)) NIL)
        (T
         (DO ((P-PTR 0)
              (P-NEXT)
              (P-CHAR WILD-ONE)
              (S-PTR -1)
              (SET (LIST WILD-ANY WILD-ONE)))
             (())
           (SETQ P-NEXT (STRING-SEARCH-SET SET PATTERN P-PTR NIL T))
           (COND ((= P-CHAR WILD-ONE)
                  (AND RETURN-SPECS-FLAG ( S-PTR 0)
                       (PUSH (AREF SAMPLE S-PTR) SPECS))
                  (SETQ S-PTR
                        (AND (FUNCALL STRING= SAMPLE PATTERN
                                      :START1 (1+ S-PTR)
                                      :START2 P-PTR
                                      :END1 (+ 1 S-PTR
                                                 (- (OR P-NEXT (LENGTH PATTERN)) P-PTR))
                                      :END2 P-NEXT)
                             (1+ S-PTR))))
                 ((NULL P-NEXT)
                  ;; Stuff at end following a star =>
                  ;;  win if tail of rest of string matches that stuff.
                  (LET ((OLD-S-PTR S-PTR))
                    (SETQ S-PTR (STRING-REVERSE-SEARCH PATTERN SAMPLE NIL S-PTR P-PTR NIL T))
                    (WHEN RETURN-SPECS-FLAG
                      (PUSH (SUBSTRING SAMPLE OLD-S-PTR S-PTR) SPECS))))
                 (T
                  (LET ((OLD-S-PTR S-PTR))
                    (SETQ S-PTR (STRING-SEARCH PATTERN SAMPLE S-PTR NIL P-PTR P-NEXT T))
                    (WHEN RETURN-SPECS-FLAG
                      (PUSH (SUBSTRING SAMPLE OLD-S-PTR S-PTR) SPECS)))))
           (UNLESS S-PTR (RETURN NIL))
           (INCF S-PTR (- (OR P-NEXT (LENGTH PATTERN)) P-PTR))
           (UNLESS P-NEXT (RETURN (AND (= S-PTR (LENGTH SAMPLE)) (OR (NREVERSE SPECS) T))))
           (SETQ P-CHAR (AREF PATTERN P-NEXT))
           (SETQ P-PTR (1+ P-NEXT))))))

(DEFMETHOD (PATHNAME :PATHNAME-MATCH) (PATHNAME &OPTIONAL (MATCH-HOST T))
  (MULTIPLE-VALUE-BIND (W* W1)
      (SEND SELF :INTERNAL-WILD-CHARACTERS)
    (AND (OR (NOT MATCH-HOST)
             (EQ HOST (PATHNAME-HOST PATHNAME)))
         (PATHNAME-COMPONENT-MATCH DEVICE (PATHNAME-DEVICE PATHNAME) W* W1)
         (PATHNAME-COMPONENT-MATCH DIRECTORY (PATHNAME-DIRECTORY PATHNAME) W* W1)
         (PATHNAME-COMPONENT-MATCH NAME (PATHNAME-NAME PATHNAME) W* W1)
         (PATHNAME-COMPONENT-MATCH TYPE (PATHNAME-TYPE PATHNAME) W* W1)
         (PATHNAME-COMPONENT-MATCH VERSION (PATHNAME-VERSION PATHNAME) W* W1))))

(DEFMETHOD (PATHNAME :PATHNAME-MATCH-SPECS) (PATHNAME)
  (MULTIPLE-VALUE-BIND (W* W1)
      (SEND SELF :INTERNAL-WILD-CHARACTERS)
    (VALUES
      (PATHNAME-COMPONENT-MATCH DEVICE (PATHNAME-DEVICE PATHNAME) W* W1 T)
      (PATHNAME-COMPONENT-MATCH DIRECTORY (PATHNAME-DIRECTORY PATHNAME) W* W1 T)
      (PATHNAME-COMPONENT-MATCH NAME (PATHNAME-NAME PATHNAME) W* W1 T)
      (PATHNAME-COMPONENT-MATCH TYPE (PATHNAME-TYPE PATHNAME) W* W1 T))))

;;; Wildcard translation

;; Copied from LAD: RELEASE-3.IO.FILE; PATHNM.LISP#570 on 2-Oct-86 05:41:52
;;; Return a pathname component made from TARGET-PATTERN
;;; by replacing each wildcard with an element of SPECS
(DEFUN PATHNAME-TRANSLATE-WILD-COMPONENT
       (TARGET-PATTERN DATA SPECS WILD-ANY WILD-ONE &OPTIONAL REVERSIBLE-P)
  (COND ((EQ TARGET-PATTERN :WILD)
         (IF (AND (CONSP SPECS) REVERSIBLE-P)
             (CAR SPECS)
           DATA))
        ((OR (NUMBERP TARGET-PATTERN)
             (SYMBOLP TARGET-PATTERN)
             (EQ SPECS T))
         TARGET-PATTERN)
        ((CONSP TARGET-PATTERN)
         (LOOP FOR ELT IN TARGET-PATTERN
               COLLECT
               (IF (EQ ELT :WILD)
                   (POP SPECS)
                 (MULTIPLE-VALUE-BIND (NEW-ELT SPECS-LEFT)
                     (PATHNAME-TRANSLATE-COMPONENT-FROM-SPECS
                       ELT SPECS WILD-ANY WILD-ONE)
                   (SETQ SPECS SPECS-LEFT)
                   NEW-ELT))))
        (T (PATHNAME-TRANSLATE-COMPONENT-FROM-SPECS
             TARGET-PATTERN SPECS WILD-ANY WILD-ONE))))

(DEFUN PATHNAME-TRANSLATE-COMPONENT-FROM-SPECS (PATTERN SPECS WILD-ANY WILD-ONE)
  (DECLARE (VALUES TRANSLATED-COMPONENT SPECS-LEFT))
  (IF (EQ PATTERN :WILD) (SETQ PATTERN (STRING WILD-ANY)))
  (LET ((TARGET-INDICES (PATHNAME-WILD-CHAR-INDICES PATTERN WILD-ANY WILD-ONE)))
    (DO ((TIS TARGET-INDICES (CDR TIS))
         (RESULT (MAKE-STRING 24. :FILL-POINTER 0))
         (SPECS-LEFT SPECS)
         TI
         (PREV-TI -1 TI))
        (())
      (SETQ TI (CAR TIS))
      (UNLESS (MINUSP PREV-TI)
        (STRING-NCONC RESULT
                      (OR (POP SPECS-LEFT) "")))
      (STRING-NCONC RESULT
                    (SUBSTRING PATTERN (1+ PREV-TI) TI))
      (UNLESS TI (RETURN (values RESULT SPECS-LEFT))))))

(DEFUN PATHNAME-WILD-CHAR-INDICES (STRING &REST SET)
  (IF (NOT (STRINGP STRING)) NIL
    (DO ((I (LENGTH STRING)) VALUES)
        (())
      (SETQ I (STRING-REVERSE-SEARCH-SET SET STRING I))
      (UNLESS I (RETURN VALUES))
      (PUSH I VALUES))))

(DEFMETHOD (PATHNAME :TRANSLATE-WILD-PATHNAME)
           (TARGET-PATTERN DATA-PATHNAME &OPTIONAL REVERSIBLE-P)
  (SEND TARGET-PATTERN :TARGET-TRANSLATE-WILD-PATHNAME SELF DATA-PATHNAME REVERSIBLE-P))

(DEFMETHOD (PATHNAME :TARGET-TRANSLATE-WILD-PATHNAME)
           (SOURCE-PATTERN DATA-PATHNAME &OPTIONAL REVERSIBLE-P)
  (MULTIPLE-VALUE-BIND (W* W1)
      (SEND SELF :INTERNAL-WILD-CHARACTERS)
    (LET ((CASE-CONVERTER
            (SEND SELF :TRANSLATION-CASE-CONVERTER)))
      (MULTIPLE-VALUE-BIND (DEV-SPECS DIR-SPECS NAME-SPECS TYPE-SPECS)
          (SEND SOURCE-PATTERN :PATHNAME-MATCH-SPECS DATA-PATHNAME)
        (MAKE-PATHNAME :HOST HOST
                       :RAW-DEVICE (PATHNAME-TRANSLATE-WILD-COMPONENT
                                      DEVICE
                                      (FUNCALL CASE-CONVERTER
                                               (PATHNAME-DEVICE DATA-PATHNAME))
                                      (FUNCALL CASE-CONVERTER DEV-SPECS)
                                      W* W1 REVERSIBLE-P)
                       :RAW-DIRECTORY (PATHNAME-TRANSLATE-WILD-COMPONENT
                                         DIRECTORY
                                         (FUNCALL CASE-CONVERTER
                                                  (PATHNAME-DIRECTORY DATA-PATHNAME))
                                         (FUNCALL CASE-CONVERTER DIR-SPECS)
                                         W* W1 REVERSIBLE-P)
                       :RAW-NAME (PATHNAME-TRANSLATE-WILD-COMPONENT
                                    NAME
                                    (FUNCALL CASE-CONVERTER
                                             (PATHNAME-NAME DATA-PATHNAME))
                                    (FUNCALL CASE-CONVERTER NAME-SPECS)
                                    W* W1 REVERSIBLE-P)
                       (IF (AND (EQ TYPE :WILD)
                                (OR (NOT REVERSIBLE-P)
                                    (EQ (PATHNAME-TYPE SOURCE-PATTERN) :WILD)))
                           :TYPE :RAW-TYPE)
                                  (IF (AND (EQ TYPE :WILD)
                                           (OR (NOT REVERSIBLE-P)
                                               (EQ (PATHNAME-TYPE SOURCE-PATTERN) :WILD)))
                                      (SEND DATA-PATHNAME :CANONICAL-TYPE)
                                    (PATHNAME-TRANSLATE-WILD-COMPONENT
                                      TYPE
                                      (FUNCALL CASE-CONVERTER
                                               (PATHNAME-TYPE DATA-PATHNAME))
                                      (FUNCALL CASE-CONVERTER TYPE-SPECS)
                                      W* W1 REVERSIBLE-P))
                       :VERSION (IF (EQ VERSION :WILD) (PATHNAME-VERSION DATA-PATHNAME)
                         VERSION))))))

;;; Returns function that converts interchange case into this flavor's raw case.
(DEFMETHOD (PATHNAME :TRANSLATION-CASE-CONVERTER) ()
  #'(LAMBDA (X) X))

;;; Returns two values, the wild-any char for this flavor and the wild-one char.
(DEFMETHOD (PATHNAME :INTERNAL-WILD-CHARACTERS) ()
  (VALUES #/* -1))

;;;; Operations that refer to the file.

;;; The default is not to have completion at all
(DEFMETHOD (PATHNAME :COMPLETE-STRING) (STRING IGNORE)
  (VALUES (STRING-APPEND (SEND HOST :NAME-AS-FILE-COMPUTER) ": " STRING)
          NIL))

(DEFMETHOD (PATHNAME :MULTIPLE-FILE-PLISTS) (FILES OPTIONS &AUX (CHARACTERS T))
  (LOOP FOR (IND OPT) ON OPTIONS BY 'CDDR
        DO (SELECTQ IND
             (:CHARACTERS (SETQ CHARACTERS OPT))
             (OTHERWISE (FERROR NIL "~S is not a known MULTIPLE-FILE-PLISTS option" IND))))
  (LOOP FOR FILE IN FILES
        AS STREAM = (OPEN FILE :DIRECTION NIL :ERROR NIL :CHARACTERS CHARACTERS)
        COLLECT (CONS FILE (AND (NOT (ERRORP STREAM))
                                (LET* ((LIST (SEND STREAM :PLIST))
                                       (PLIST (LOCF LIST)))
                                  (OR (GET PLIST :TRUENAME)
                                      (PUTPROP PLIST (SEND STREAM :TRUENAME) :TRUENAME))
                                  LIST)))))

(DEFMETHOD (PATHNAME :UNDELETE) (&OPTIONAL (ERROR-P T))
  (CHANGE-FILE-PROPERTIES SELF ERROR-P :DELETED NIL))

(DEFMETHOD (PATHNAME :TRUENAME) (&OPTIONAL (ERROR-P T))
  (WITH-OPEN-FILE (STREAM SELF :ERROR ERROR-P)
    (IF (ERRORP STREAM) STREAM
      (SEND STREAM :TRUENAME))))

;;; This isn't implemented as a separate file subprotocol, just use the directory
(DEFMETHOD (PATHNAME :PROPERTIES) (&OPTIONAL (ERROR-P T))
  (FILE-OPERATION-RETRY
    (LET ((DIR (SEND SELF :DIRECTORY-LIST
                          (IF ERROR-P '(:DELETED)
                            '(:NOERROR :DELETED)))))
      (COND ((CONSP DIR)
             (IF (CADR DIR)
                 (VALUES (CADR DIR) (GET (CAR DIR) :SETTABLE-PROPERTIES))
               ;; It is possible for a nonexistent file to give no error
               ;; but just return an empty directory.
               (FILE-PROCESS-ERROR 'FILE-NOT-FOUND "File not found" SELF
                                   NIL (NOT ERROR-P) :PROPERTIES)))
            (T DIR)))))

;;; Should these flavors go away completely?
;;; Or is there something that Unix, Multics, T(w)enex and Lispm file systems
;;; have in common which could reasonably be put here?

(DEFFLAVOR HIERARCHICAL-DIRECTORY-MIXIN () ()
  (:REQUIRED-FLAVORS PATHNAME))

;(comment
;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :VALID-DIRECTORY) (DIRNAME)
;  (IF (ATOM DIRNAME)
;      (COND ((MEMQ DIRNAME '(NIL :WILD :UNSPECIFIC))
;            DIRNAME)
;           ((STRINGP DIRNAME)
;            DIRNAME)
;           (T
;            (SEND SELF :DEFAULT-DIRECTORY)))
;    (SEND SELF :CHECK-SUBDIRECTORIES DIRNAME 0)))

;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :VALID-DIRECTORY-P) (DIRNAME)
;  (OR (NULL DIRNAME)
;      (STRINGP DIRNAME)
;      (AND (CONSP DIRNAME)
;          (SEND SELF :VALID-SUBDIRECTORY-P DIRNAME 0))))

;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :VALID-SUBDIRECTORY-P) (DIRNAME-LIST LEVEL)
;  (OR (NULL DIRNAME-LIST)
;      (AND (CONSP DIRNAME-LIST)
;          (SEND SELF :VALID-DIRECTORY-COMPONENT-P (CAR DIRNAME-LIST) LEVEL)
;          (SEND SELF :VALID-SUBDIRECTORY-P (CDR DIRNAME-LIST) (1+ LEVEL)))))

;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :VALID-DIRECTORY-COMPONENT-P) (DIRNAME IGNORE)
;  (OR (STRINGP DIRNAME) (MEMQ DIRNAME '(:WILD :UNSPECIFIC))))

;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :DEFAULT-DIRECTORY) ()
;  (SEND (QUIET-USER-HOMEDIR HOST) :DIRECTORY))

;(DEFMETHOD (HIERARCHICAL-DIRECTORY-MIXIN :CHECK-SUBDIRECTORIES) (DIRNAMES LEVEL)
;  (IF (NULL DIRNAMES) NIL
;      (CONS (IF (SEND SELF :VALID-DIRECTORY-COMPONENT-P (CAR DIRNAMES) LEVEL)
;               (CAR DIRNAMES))
;           (SEND SELF :CHECK-SUBDIRECTORIES (CDR DIRNAMES) (1+ LEVEL)))))
;);end comment

(DEFFLAVOR MEANINGFUL-ROOT-MIXIN () ()
  (:REQUIRED-FLAVORS PATHNAME)
  (:REQUIRED-METHODS :DIRECTORY-PATHNAME-AS-FILE)
  (:METHOD-COMBINATION (:DAEMON-WITH-OR :BASE-FLAVOR-LAST :VALID-DIRECTORY-COMPONENT-P))
  (:DOCUMENTATION :MIXIN "For use with file systems where the root directory is treated
as an ordinary directory."))

;(comment
;(DEFMETHOD (MEANINGFUL-ROOT-MIXIN :OR :VALID-DIRECTORY-COMPONENT-P) (DIRNAME IGNORE)
;  (MEMQ DIRNAME '(:ROOT :RELATIVE :UP)))
;);end comment

;;; brand S
(DEFF DESCRIBE-PHYSICAL-HOST 'SI:DESCRIBE-HOST)


;;;; Pathname system initialization

(DEFUN PATHNAME-INITIALIZE ()
  (SETQ *PATHNAME-HASH-TABLE* (MAKE-EQUAL-HASH-TABLE :SIZE 3000. :AREA PATHNAME-AREA))
  (SETQ *DEFAULT-PATHNAME-DEFAULTS* (MAKE-PATHNAME-DEFAULTS))
  (SETQ CLI:*DEFAULT-PATHNAME-DEFAULTS*
        (SI:CDR-LOCATION-FORCE (ASSQ NIL *DEFAULT-PATHNAME-DEFAULTS*)))
  (%P-STORE-DATA-TYPE (LOCF CLI:*DEFAULT-PATHNAME-DEFAULTS*) DTP-EXTERNAL-VALUE-CELL-POINTER)
  (SETQ LOAD-PATHNAME-DEFAULTS (MAKE-PATHNAME-DEFAULTS)))

(ADD-INITIALIZATION "PATHNAME-INITIALIZE" '(PATHNAME-INITIALIZE) '(ONCE))

;(DEFVAR PATHNAME-PLISTS-LINEARIZED-ONCE NIL)

;(DEFUN LINEARIZE-PATHNAME-PLISTS ()
;  (IF PATHNAME-PLISTS-LINEARIZED-ONCE
;      ;; If already been recopied, just reference all of them so they are
;      ;; all copied into newspace together.
;      (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
;                  (REFERENCE-ALL (SEND FILE :PLIST)))
;              FS:*PATHNAME-HASH-TABLE*)
;    (SETQ PATHNAME-PLISTS-LINEARIZED-ONCE T)
;    (MAPHASH #'(LAMBDA (IGNORE FILE &REST IGNORE)
;                (SEND FILE :SET-PROPERTY-LIST (COPYTREE (SEND FILE :PLIST))))
;            FS:*PATHNAME-HASH-TABLE*)))

;(DEFUN REFERENCE-ALL (OBJECT)
;  (UNLESS (ATOM OBJECT)
;    (DO ((TAIL OBJECT (CDR TAIL)))
;       ((ATOM TAIL))
;      (UNLESS (ATOM (CAR TAIL))
;       (REFERENCE-ALL (CAR TAIL))))))

;(ADD-INITIALIZATION "Pathname plists" '(LINEARIZE-PATHNAME-PLISTS) '(AFTER-FULL-GC))
