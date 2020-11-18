;-*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL -*-
;;;; Global interface functions, and random file stuff

;;; First define the error flavors and signal names used by all file errors.

(DEFFLAVOR FILE-ERROR (PATHNAME OPERATION) (NO-ACTION-MIXIN ERROR)
  :ABSTRACT-FLAVOR
  :GETTABLE-INSTANCE-VARIABLES
  :INITTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (FILE-ERROR :AFTER :INIT) (IGNORE)
  (SETQ PATHNAME (GETF SI:PROPERTY-LIST ':PATHNAME))
  (OR (VARIABLE-BOUNDP OPERATION)
      (SETQ OPERATION (GETF SI:PROPERTY-LIST ':OPERATION))))

(DEFMETHOD (FILE-ERROR :SET-FORMAT-ARGS) (NEW-ARGS)
  (SETQ EH:FORMAT-ARGS NEW-ARGS))

(DEFMETHOD (FILE-ERROR :CASE :PROCEED-ASKING-USER :RETRY-FILE-OPERATION)
           (CONTINUATION IGNORE)
  "Proceeds, trying the file operation again."
  (FUNCALL CONTINUATION ':RETRY-FILE-OPERATION))

(DEFMETHOD (FILE-ERROR :CASE :PROCEED-ASKING-USER :NEW-PATHNAME)
           (CONTINUATION READ-OBJECT-FUNCTION)
  "Proceeds, reading a new filename and using that instead."
  (FUNCALL CONTINUATION ':NEW-PATHNAME
           (FUNCALL READ-OBJECT-FUNCTION
                    `(:PATHNAME :DEFAULTS ,PATHNAME)
                    "Pathname to use instead: (default ~A) "
                    PATHNAME)))

(DEFFLAVOR FILE-REQUEST-FAILURE () (FILE-ERROR))

(DEFPROP DAT DATA-ERROR FILE-ERROR)
(DEFPROP DATA-ERROR DAT FILE-ERROR)
(DEFSIGNAL DATA-ERROR FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "Inconsistent data found in the file system.")

(DEFPROP HNA HOST-NOT-AVAILABLE FILE-ERROR)
(DEFPROP HOST-NOT-AVAILABLE HNA FILE-ERROR)
(DEFSIGNAL HOST-NOT-AVAILABLE FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "A file system is refusing to listen to requests.")

(DEFPROP NFS NO-FILE-SYSTEM FILE-ERROR)
(DEFPROP NO-FILE-SYSTEM NFS FILE-ERROR)
(DEFSIGNAL NO-FILE-SYSTEM FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "The file system does not seem to exist.")

(DEFSIGNAL NETWORK-LOSSAGE FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "Misc. network problems during file accessing.
In particular, failure to open data connection.")

(DEFPROP NER NOT-ENOUGH-RESOURCES FILE-ERROR)
(DEFPROP NOT-ENOUGH-RESOURCES NER FILE-ERROR)
(DEFSIGNAL NOT-ENOUGH-RESOURCES FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "Shortage of resources (network or file server) to transmit operation.
This is if the file server itself says it hasn't got enough resources.")

(DEFPROP UOP UNKNOWN-OPERATION FILE-ERROR)
(DEFPROP UNKNOWN-OPERATION UOP FILE-ERROR)
(DEFPROP UKC UNKNOWN-OPERATION FILE-ERROR)
(DEFSIGNAL UNKNOWN-OPERATION FILE-REQUEST-FAILURE (PATHNAME OPERATION)
  "OPERATION is not supported on the particular file system used.")

(DEFPROP LIP LOGIN-PROBLEMS FILE-ERROR)
(DEFPROP LOGIN-PROBLEMS LIP FILE-ERROR)
(DEFSIGNAL LOGIN-PROBLEMS FILE-REQUEST-FAILURE ()
           "Some sort of failure to log in.")

(DEFPROP UNK UNKNOWN-USER FILE-ERROR)
(DEFPROP UNKNOWN-USER UNK FILE-ERROR)
(DEFSIGNAL UNKNOWN-USER
           (FILE-REQUEST-FAILURE LOGIN-PROBLEMS CORRECTABLE-LOGIN-PROBLEMS UNKNOWN-USER)
  ()
  "USER is not a known login-name at HOST.")

(DEFPROP IP? INVALID-PASSWORD FILE-ERROR)
(DEFPROP INVALID-PASSWORD IP? FILE-ERROR)
(DEFSIGNAL INVALID-PASSWORD (FILE-REQUEST-FAILURE LOGIN-PROBLEMS
                                                  CORRECTABLE-LOGIN-PROBLEMS INVALID-PASSWORD)
  ()
  "PASSWORD is not USER's password at HOST.")

(DEFPROP NLI NOT-LOGGED-IN FILE-ERROR)
(DEFPROP NOT-LOGGED-IN NLI FILE-ERROR)
(DEFSIGNAL NOT-LOGGED-IN (FILE-REQUEST-FAILURE NOT-LOGGED-IN)
  (PATHNAME OPERATION)
  "The file server said you were not logged in when the Lispm thought you should be.")

(DEFFLAVOR FILE-OPERATION-FAILURE () (FILE-ERROR)
  (:DOCUMENTATION "The file system understood an operation but decided it was erroneous."))

(DEFMETHOD (FILE-OPERATION-FAILURE :CASE :PROCEED-ASKING-USER :DIRED) (&REST IGNORE)
  "Runs DIRED, then returns to the debugger when you type Control-Z."
  (DIRED (SEND (SEND SELF :PATHNAME) :NEW-PATHNAME
               :NAME :WILD :TYPE :WILD :VERSION :WILD))
  NIL)

(DEFMETHOD (FILE-OPERATION-FAILURE :CASE :PROCEED-ASKING-USER :CLEAN-DIRECTORY) (&REST IGNORE)
  "Calls (ZWEI:CLEAN-DIRECTORY), then returns to debugger."
  (ZWEI:CLEAN-DIRECTORY (SEND (SEND SELF :PATHNAME) :NEW-PATHNAME
                              :NAME :WILD :TYPE :WILD :VERSION :WILD))
  NIL)

(DEFMETHOD (FILE-OPERATION-FAILURE :CASE :PROCEED-ASKING-USER :EXPUNGE-DIRECTORY) (&REST IGNORE)
  "Expunges the directory, then returns to debugger."
  (EXPUNGE-DIRECTORY (SEND (SEND SELF ':PATHNAME) ':NEW-PATHNAME
                           ':NAME ':WILD ':TYPE ':WILD ':VERSION ':WILD))
  NIL)

(DEFSIGNAL FILE-OPERATION-FAILURE-1 FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "Unrecognized file error codes signal this.")

(DEFSIGNAL FILE-OPEN-FOR-OUTPUT FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "How is this different from FILE-LOCKED?")

(DEFPROP LCK FILE-LOCKED FILE-ERROR)
(DEFPROP FILE-LOCKED LCK FILE-ERROR)
(DEFSIGNAL FILE-LOCKED FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "File cannot be accessed because someone else is using it.")

(DEFPROP CIR CIRCULAR-LINK FILE-ERROR)
(DEFPROP CIRCULAR-LINK CIR FILE-ERROR)
(DEFSIGNAL CIRCULAR-LINK FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "A link pointed to a link which pointed to a link ... too deeply.")

;;;Unimplemented-option

(DEFPROP UOO UNIMPLEMENTED-OPTION FILE-ERROR)
(DEFPROP UNIMPLEMENTED-OPTION UOO FILE-ERROR)
(DEFSIGNAL UNIMPLEMENTED-OPTION FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "Specified some option this file system doesn't understand.")

(DEFPROP IBS INVALID-BYTE-SIZE FILE-ERROR)
(DEFPROP INVALID-BYTE-SIZE IBS FILE-ERROR)
(DEFSIGNAL INVALID-BYTE-SIZE (FILE-OPERATION-FAILURE UNIMPLEMENTED-OPTION INVALID-BYTE-SIZE)
           (PATHNAME OPERATION)
  "Specified a byte size that the file system cannot handle.")

(DEFPROP ICO INCONSISTENT-OPTIONS FILE-ERROR)
(DEFPROP INCONSISTENT-OPTIONS ICO FILE-ERROR)
(DEFSIGNAL INCONSISTENT-OPTIONS FILE-OPERATION-FAILURE
           (PATHNAME OPERATION)
  "Specified a byte size that the file system cannot handle.")

(DEFFLAVOR NO-MORE-ROOM-ERROR () (FILE-OPERATION-FAILURE))

(DEFMETHOD (NO-MORE-ROOM-ERROR :USER-PROCEED-TYPES) (REAL-PROCEED-TYPES)
  (APPEND (INTERSECTION '(:NO-ACTION :RETRY-FILE-OPERATION) REAL-PROCEED-TYPES)
          '(:DIRED :CLEAN-DIRECTORY :expunge-directory)
          (REM-IF #'(LAMBDA (ELT)
                      (MEMQ ELT '(:NO-ACTION :RETRY-FILE-OPERATION :DIRED :CLEAN-DIRECTORY :expunge-directory)))
                  REAL-PROCEED-TYPES)))

(DEFPROP NMR NO-MORE-ROOM FILE-ERROR)
(DEFPROP NO-MORE-ROOM NMR FILE-ERROR)
(DEFSIGNAL NO-MORE-ROOM NO-MORE-ROOM-ERROR (PATHNAME OPERATION)
  "Out of resources within the file system.")

(DEFPROP FOR FILEPOS-OUT-OF-RANGE FILE-ERROR)
(DEFPROP FILEPOS-OUT-OF-RANGE FOR FILE-ERROR)
(DEFSIGNAL FILEPOS-OUT-OF-RANGE FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "Access pointer set to bad value, outside 0 to length of file.")

(DEFPROP NAV NOT-AVAILABLE FILE-ERROR)
(DEFPROP NOT-AVAILABLE NAV FILE-ERROR)
(DEFSIGNAL NOT-AVAILABLE FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "File, pack etc. exists but is currently down.")

(DEFSIGNAL INVALID-FILE-ATTRIBUTE ERROR (PATHNAME ATTRIBUTE VALUE)
           "An attribute in the file attribute list had a bad value.
This is detected in the Lisp machine, not in the file server.")

;;; Subclasses of FILE-OPERATION-FAILURE.

(DEFFLAVOR FILE-LOOKUP-ERROR () (FILE-OPERATION-FAILURE))

(DEFPROP FNF FILE-NOT-FOUND FILE-ERROR)
(DEFPROP FILE-NOT-FOUND FNF FILE-ERROR)
(DEFSIGNAL FILE-NOT-FOUND FILE-LOOKUP-ERROR (PATHNAME OPERATION)
  "The file was not found in the containing directory.")

(DEFSIGNAL MULTIPLE-FILE-NOT-FOUND FILE-LOOKUP-ERROR (OPERATION PATHNAME PATHNAMES)
  "None of the files was found in the containing directory.")

(DEFFLAVOR DIRECTORY-NOT-FOUND-ERROR () (FILE-LOOKUP-ERROR))

(DEFMETHOD (DIRECTORY-NOT-FOUND-ERROR :USER-PROCEED-TYPES) (REAL-PROCEED-TYPES)
  (APPEND (INTERSECTION '(:NO-ACTION :RETRY-FILE-OPERATION) REAL-PROCEED-TYPES)
          '(:CREATE-DIRECTORY-AND-RETRY)
          (REM-IF #'(LAMBDA (ELT) (MEMQ ELT '(:NO-ACTION :RETRY-FILE-OPERATION
                                                         :CREATE-DIRECTORY-AND-RETRY)))
                  REAL-PROCEED-TYPES)))

(DEFMETHOD (DIRECTORY-NOT-FOUND-ERROR :CASE :PROCEED-ASKING-USER :CREATE-DIRECTORY-AND-RETRY)
           (CONTINUATION IGNORE)
  "Creates the directory and tries again."
  (CREATE-DIRECTORY (SEND SELF ':PATHNAME) ':RECURSIVE T)
  (FUNCALL CONTINUATION ':RETRY-FILE-OPERATION))

(DEFPROP DNF DIRECTORY-NOT-FOUND FILE-ERROR)
(DEFPROP DIRECTORY-NOT-FOUND DNF FILE-ERROR)
(DEFSIGNAL DIRECTORY-NOT-FOUND DIRECTORY-NOT-FOUND-ERROR (PATHNAME OPERATION)
           "Containing directory is not found.")

(DEFPROP DEV DEVICE-NOT-FOUND FILE-ERROR)
(DEFPROP DEVICE-NOT-FOUND DEV FILE-ERROR)
(DEFSIGNAL DEVICE-NOT-FOUND FILE-LOOKUP-ERROR (PATHNAME OPERATION)
  "Device specified in pathname not found.")

(DEFPROP LNF LINK-TARGET-NOT-FOUND FILE-ERROR)
(DEFPROP LINK-TARGET-NOT-FOUND LNF FILE-ERROR)
(DEFSIGNAL LINK-TARGET-NOT-FOUND FILE-LOOKUP-ERROR (PATHNAME OPERATION)
  "Failure in looking up the target of a link that was found.")

;;; ACCESS-ERROR means some kind of protection failure.
(DEFPROP ACC ACCESS-ERROR FILE-ERROR)
(DEFPROP ACCESS-ERROR ACC FILE-ERROR)
(DEFSIGNAL ACCESS-ERROR FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "Some sort of protection screwed you.")

(DEFPROP ATF INCORRECT-ACCESS-TO-FILE FILE-ERROR)
(DEFPROP INCORRECT-ACCESS-TO-FILE ATF FILE-ERROR)
(DEFSIGNAL INCORRECT-ACCESS-TO-FILE
           (FILE-OPERATION-FAILURE ACCESS-ERROR INCORRECT-ACCESS-TO-FILE)
  (PATHNAME OPERATION)
  "File protection screwed you.")

(DEFPROP ATD INCORRECT-ACCESS-TO-DIRECTORY FILE-ERROR)
(DEFPROP INCORRECT-ACCESS-TO-DIRECTORY ATD FILE-ERROR)
(DEFSIGNAL INCORRECT-ACCESS-TO-DIRECTORY
           (FILE-OPERATION-FAILURE ACCESS-ERROR INCORRECT-ACCESS-TO-DIRECTORY)
  (PATHNAME OPERATION)
  "Directory protection screwed you.")

;;;INVALID-PATHNAME-SYNTAX

(DEFPROP IPS INVALID-PATHNAME-SYNTAX FILE-ERROR)
(DEFPROP INVALID-PATHNAME-SYNTAX IPS FILE-ERROR)
(DEFSIGNAL INVALID-PATHNAME-SYNTAX FILE-OPERATION-FAILURE (PATHNAME OPERATION)
  "Some weird syntax got through pathname parsing but file system didn't like it.")

(DEFPROP IWC INVALID-WILDCARD FILE-ERROR)
(DEFPROP INVALID-WILDCARD IWC FILE-ERROR)
(DEFSIGNAL INVALID-WILDCARD (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX INVALID-WILDCARD)
           (PATHNAME OPERATION)
  "Wildcard that got through pathname parsing but file system didn't like it.")

(DEFPROP WNA WILDCARD-NOT-ALLOWED FILE-ERROR)
(DEFPROP WILDCARD-NOT-ALLOWED WNA FILE-ERROR)
(DEFSIGNAL WILDCARD-NOT-ALLOWED
           (FILE-OPERATION-FAILURE INVALID-PATHNAME-SYNTAX WILDCARD-NOT-ALLOWED)
           (PATHNAME OPERATION)
  "Wildcard is ok but you did something to it that doesn't like wildcards.")

;;; WRONG-KIND-OF-FILE

(DEFPROP WKF WRONG-KIND-OF-FILE FILE-ERROR)
(DEFPROP WRONG-KIND-OF-FILE WKF FILE-ERROR)
(DEFSIGNAL WRONG-KIND-OF-FILE FILE-OPERATION-FAILURE (PATHNAME OPERATION))

(DEFSIGNAL INVALID-OPERATION-FOR-LINK
           (FILE-OPERATION-FAILURE WRONG-KIND-OF-FILE INVALID-OPERATION-FOR-LINK)
           (PATHNAME OPERATION))

(DEFPROP IOD INVALID-OPERATION-FOR-DIRECTORY FILE-ERROR)
(DEFPROP INVALID-OPERATION-FOR-DIRECTORY IOD FILE-ERROR)
(DEFSIGNAL INVALID-OPERATION-FOR-DIRECTORY
           (FILE-OPERATION-FAILURE WRONG-KIND-OF-FILE INVALID-OPERATION-FOR-DIRECTORY)
           (PATHNAME OPERATION))

;;; CREATION-FAILUREs

(DEFPROP FAE FILE-ALREADY-EXISTS FILE-ERROR)
(DEFPROP FILE-ALREADY-EXISTS FAE FILE-ERROR)
(DEFSIGNAL FILE-ALREADY-EXISTS (FILE-OPERATION-FAILURE CREATION-FAILURE FILE-ALREADY-EXISTS)
           (PATHNAME OPERATION)
  "A file of this name already exists.")

(DEFPROP SND SUPERIOR-NOT-DIRECTORY FILE-ERROR)
(DEFPROP SUPERIOR-NOT-DIRECTORY SND FILE-ERROR)
(DEFSIGNAL SUPERIOR-NOT-DIRECTORY
           (FILE-OPERATION-FAILURE WRONG-KIND-OF-FILE CREATION-FAILURE SUPERIOR-NOT-DIRECTORY)
           (PATHNAME OPERATION)
  "The /"directory/" was not a directory.")

(DEFPROP CCD CREATE-DIRECTORY-FAILURE FILE-ERROR)
(DEFPROP CREATE-DIRECTORY-FAILURE CCD FILE-ERROR)
(DEFSIGNAL CREATE-DIRECTORY-FAILURE
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-DIRECTORY-FAILURE)
           (PATHNAME OPERATION)
  "")

(DEFPROP DAE DIRECTORY-ALREADY-EXISTS FILE-ERROR)
(DEFPROP DIRECTORY-ALREADY-EXISTS DAE FILE-ERROR)
(DEFSIGNAL DIRECTORY-ALREADY-EXISTS
           (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-DIRECTORY-FAILURE
                                   FILE-ALREADY-EXISTS DIRECTORY-ALREADY-EXISTS)
  (PATHNAME OPERATION)
  "A directory of this name already exists.")

(DEFPROP CCL CREATE-LINK-FAILURE FILE-ERROR)
(DEFPROP CREATE-LINK-FAILURE CCL FILE-ERROR)
(DEFSIGNAL CREATE-LINK-FAILURE (FILE-OPERATION-FAILURE CREATION-FAILURE CREATE-LINK-FAILURE)
           (PATHNAME OPERATION)
  "Some problem creating a link.")

(DEFFLAVOR RENAME-FAILURE () (FILE-OPERATION-FAILURE)
  (:DEFAULT-INIT-PLIST :OPERATION ':RENAME))
;Should have property :NEW-PATHNAME.

(DEFPROP REF RENAME-TO-EXISTING-FILE FILE-ERROR)
(DEFPROP RENAME-TO-EXISTING-FILE REF FILE-ERROR)
(DEFSIGNAL RENAME-TO-EXISTING-FILE RENAME-FAILURE (PATHNAME NEW-PATHNAME)
           "NEW-PATHNAME already exists.")

(DEFPROP RAD RENAME-ACROSS-DIRECTORIES FILE-ERROR)
(DEFPROP RENAME-ACROSS-DIRECTORIES RAD FILE-ERROR)
(DEFSIGNAL RENAME-ACROSS-DIRECTORIES RENAME-FAILURE (PATHNAME NEW-PATHNAME)
           "PATHNAME and NEW-PATHNAME are on different directories.")

(DEFFLAVOR CHANGE-PROPERTY-FAILURE () (FILE-OPERATION-FAILURE)
  (:DEFAULT-INIT-PLIST :OPERATION ':CHANGE-PROPERTIES))

(DEFPROP UKP UNKNOWN-PROPERTY FILE-ERROR)
(DEFPROP UNKNOWN-PROPERTY UKP FILE-ERROR)
(DEFSIGNAL UNKNOWN-PROPERTY CHANGE-PROPERTY-FAILURE (PATHNAME PROPERTY)
  "PROPERTY isn't one of the properties this file system handles.")

(DEFPROP IPV INVALID-PROPERTY-VALUE FILE-ERROR)
(DEFPROP INVALID-PROPERTY-VALUE IPV FILE-ERROR)
(DEFSIGNAL INVALID-PROPERTY-VALUE CHANGE-PROPERTY-FAILURE (PATHNAME PROPERTY VALUE)
  "VALUE is not a legal value for PROPERTY.")

(DEFPROP IPN INVALID-PROPERTY-NAME FILE-ERROR)
(DEFPROP INVALID-PROPERTY-NAME IPN FILE-ERROR)
(DEFSIGNAL INVALID-PROPERTY-NAME CHANGE-PROPERTY-FAILURE (PATHNAME PROPERTY)
  "Property name is syntactically bad.")

(DEFFLAVOR DELETE-FAILURE () (FILE-OPERATION-FAILURE)
  (:DEFAULT-INIT-PLIST :OPERATION ':DELETE))

(DEFPROP DNE DIRECTORY-NOT-EMPTY FILE-ERROR)
(DEFPROP DIRECTORY-NOT-EMPTY DNE FILE-ERROR)
(DEFSIGNAL DIRECTORY-NOT-EMPTY DELETE-FAILURE (PATHNAME)
  "Deleting a directory that is not empty.")

(DEFPROP DND DONT-DELETE-FLAG-SET FILE-ERROR)
(DEFPROP DONT-DELETE-FLAG-SET DND FILE-ERROR)
(DEFSIGNAL DONT-DELETE-FLAG-SET DELETE-FAILURE (PATHNAME)
  "File's dont-delete flag was set.")

(COMPILE-FLAVOR-METHODS
  FILE-ERROR
  FILE-REQUEST-FAILURE
  FILE-OPERATION-FAILURE
  FILE-LOOKUP-ERROR
  NO-MORE-ROOM-ERROR
  DIRECTORY-NOT-FOUND-ERROR
  RENAME-FAILURE
  CHANGE-PROPERTY-FAILURE
  DELETE-FAILURE)

;;; This variable is to make DIRED have a guess at what is losing, it should work better
;;; somehow.
(DEFVAR LAST-FILE-OPENED NIL
  "This is the last filename that was opened, successfully or not.")

;;; For Maclisp compatibility, the OPEN function accepts an option name
;;; or a list of options (a single arg), as an alternative to
;;; keywords and values.  These option names can be in any package.

;;; Possible keywords and values include the following:
;;; Keyword          Possible Values    Default         Comment
;;; :DIRECTION          :INPUT          :INPUT
;;;                     :OUTPUT
;;;                     NIL                             This is a probe opening,
;;;                                                     no data is transfered.
;;; :CHARACTERS         boolean         T               T if file is textual data.
;;;                     :DEFAULT
;;; :ERROR              boolean         T               An error is signaled if T.
;;; :BYTE-SIZE          NIL             NIL             16 for binary files.
;;;                                                     System-dependent fixed value for
;;;                                                     text files.
;;;                     :DEFAULT                        Whatever size the file says it is.
;;;                     fixnum
;;; :INHIBIT-LINKS      boolean         NIL
;;; :DELETED            boolean         NIL
;;; :TEMPORARY          boolean         NIL
;;; :PRESERVE-DATES     boolean         NIL             Do not update reference or
;;;                                                     modification dates.
;;; :FLAVOR             NIL             NIL             Normal file
;;;                     :DIRECTORY                      Directory file
;;;                     :LINK
;;; :LINK-TO            pathname                        Creates a link when used with
;;;                                                     :LINK flavor.
;;; :ESTIMATED-SIZE     NIL             NIL
;;;                     fixnum (number of bytes)
;;; :NEW-FILE           boolean         T iff output    T means ok to create new file.
;;; :NEW-VERSION        boolean         NEW-FILE        NIL says version = NEWEST
;;;                                                     finds newest existing version.
;;; :OLD-FILE           :ERROR          (NOT NEW-FILE)  Generate an error when overwriting.
;;;                     T or :REWRITE                   Use the old file.
;;;                     :APPEND                         Use it, but append if output.
;;;                     NIL or :REPLACE                 Overwrite the file when closed.
;;;                     :RENAME                         Rename old file
;;;                     :RENAME-AND-DELETE              Same, but delete when closed.
;;;                     :NEW-VERSION                    If version is a number and there
;;;                                                     is an old file, create new version.
;;; :PHYSICAL-VOLUME    NIL             NIL
;;;                     string                          Where to put file.
;;; :LOGICAL-VOLUME     NIL             NIL             In some systems the pathname can
;;;                     string                          specify this.
;;; :INCREMENTAL-UPDATE boolean         NIL             Attempt to save recoverable data
;;;                                                     more often.

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:46:47
(DEFUN OPEN (FILENAME &REST KEYWORD-ARGS)
  "Open a file and return a stream.  FILENAME is a pathname or a string.
DIRECTION is :INPUT, :OUTPUT, :PROBE, :PROBE-LINK :PROBE-DIRECTORY
ELEMENT-TYPE specifies how the data of the stream file are to be interpreted.
  Possible values include :DEFAULT CHARACTER (UNSIGNED-BYTE n) (SIGNED-BYTE n) (MOD n) BIT
One may also specify the type using the following two options:
  CHARACTERS may be T, NIL or :DEFAULT.
  BYTE-SIZE specifies byte size to use for non-character files.
IF-EXISTS specifies what to do if FILENAME already exists when opening it for output.
  NIL means return NIL from OPEN if file already exists
  :ERROR Signal an error (FS:FILE-ALREADY-EXISTS) See the ERROR option
  :NEW-VERSION Default for when FILENAME's version is :NEWEST. Create a higher-version file
  :SUPERSEDE Create a new file which, when closed, replaces the old one
  :OVERWRITE Writes over the data of the old file.
    Sets file length to length of the data written during this open when the file is closed.
  :TRUNCATE Like :OVERWRITE, but sets length to 0 immediately upon open
  :APPEND Append new data to the end of the existing file
  :RENAME Rename the existing file to something and then create and use a new one
  :RENAME-AND-DELETE Like :RENAME, only the old (renamed) file is deleted when we close
IF-DOES-NOT-EXIST is one of :CREATE (default for most output opens, except if otherwise
  specified by IF-EXISTS), :ERROR (default for input opens, and the other output opens)
  means signal FS:FILE-NOT-FOUND, or NIL (default for :PROBE-mumble opens) meaning return NIL
ERROR specifies what to do if an error is signaled in the process of opening the file
  T (the default) means that nothing special is done; handlers are invoked if they exist
   or else the debugger is entered.
  NIL means to return the condition object itself as the value of OPEN
  :REPROMPT means to ask the user for a different filename to use instead, and retries.
   See also WITH-OPEN-FILE-RETRY and FILE-RETRY-NEW-PATHNAME, which may be The Right Thing
PRESERVE-DATES means not to alter the files reference or modification dates
ESTIMATED-SIZE informs the remote file system what thew estimated final file size will be
RAW, SUPER-IMAGE disable character set translation from ascii servers
DELETED, TEMPORARY mean to allow opening of deleted or temporary files respectively,
  on systems which support those concepts.
SUBMIT means to submit the file as a batch job on the remote host when the file is closed.

Other system-specific keywords may be supported for some file systems."
  (DECLARE (ARGLIST FILENAME &KEY (DIRECTION :INPUT) (ERROR T) (ELEMENT-TYPE :DEFAULT)
                                  CHARACTERS BYTE-SIZE IF-EXISTS IF-DOES-NOT-EXIST ERROR
                                  PRESERVE-DATES DELETED TEMPORARY SUBMIT PRESERVE-DATES
                                  RAW SUPER-IMAGE INHIBIT-LINKS
                             &ALLOW-OTHER-KEYS))
  (FORCE-USER-TO-LOGIN)
  (IF (STREAMP FILENAME)
      (SETQ FILENAME (SEND FILENAME :PATHNAME)))
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
  (SETQ LAST-FILE-OPENED FILENAME)
  (IF (OR (NULL KEYWORD-ARGS)                   ;No args is good args
          (NOT (NULL (CDR KEYWORD-ARGS))))
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ (GETF KEYWORD-ARGS ':ERROR) '(:RETRY :REPROMPT))
                                  (FILENAME FILE-ERROR)
        (LEXPR-SEND FILENAME :OPEN FILENAME KEYWORD-ARGS))
    ;; Old Syntax.
    (DO ((KEYL (IF (AND (CAR KEYWORD-ARGS) (SYMBOLP (CAR KEYWORD-ARGS)))
                   (LIST (CAR KEYWORD-ARGS))
                 (CAR KEYWORD-ARGS))
               (CDR KEYL))
         (KEY)
         (CHARACTERS T)
         (DIRECTION :INPUT)
         (BYTE-SIZE NIL)
         (ERRORP T)
         (ERRORP-SPECD NIL)
         (DELETED-P NIL)
         (TEMPORARY-P NIL)
         ;; These two are really only useful for machines that do not natively store
         ;; 8-bit characters.
         (RAW-P NIL)
         (SUPER-IMAGE-P NIL)
         )
        ((NULL KEYL)
         (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERRORP '(:RETRY :REPROMPT))
                                     (FILENAME FILE-ERROR)
           ;; Because we don't want to send meaningless keywords to file systems
           ;; which don't support them, and we don't want to cons...
           (%ASSURE-PDL-ROOM 19.)                       ;Worst case
           (%OPEN-CALL-BLOCK FILENAME 0 4)      ;D-RETURN
           (%PUSH :OPEN)        (%PUSH FILENAME)
           (%PUSH :CHARACTERS)  (%PUSH CHARACTERS)
           (%PUSH :DIRECTION)   (%PUSH DIRECTION)
           (COND (BYTE-SIZE     (%PUSH :BYTE-SIZE)      (%PUSH BYTE-SIZE)))
           (COND (ERRORP-SPECD  (%PUSH :ERROR)          (%PUSH ERRORP)))
           (COND (DELETED-P     (%PUSH :DELETED)                (%PUSH DELETED-P)))
           (COND (TEMPORARY-P   (%PUSH :TEMPORARY)      (%PUSH TEMPORARY-P)))
           (COND (SUPER-IMAGE-P (%PUSH :SUPER-IMAGE)    (%PUSH SUPER-IMAGE-P)))
           (COND (RAW-P         (%PUSH :RAW)            (%PUSH RAW-P)))
           (%ACTIVATE-OPEN-CALL-BLOCK)))
      (SETQ KEY (CAR KEYL))
      (SELECTOR KEY STRING-EQUAL
        ((:IN :READ) (SETQ DIRECTION :INPUT))
        ((:OUT :WRITE :PRINT) (SETQ DIRECTION :OUTPUT))
        ((:BINARY :FIXNUM) (SETQ CHARACTERS NIL))
        ((:CHARACTER :ASCII) (SETQ CHARACTERS T))
        ((:BYTE-SIZE) (SETQ KEYL (CDR KEYL)
                             BYTE-SIZE (CAR KEYL)))
        ((:PROBE) (SETQ DIRECTION NIL
                         CHARACTERS NIL
                         ERRORP (IF (NOT ERRORP-SPECD) NIL ERRORP)
                         ERRORP-SPECD T))
        ((:NOERROR) (SETQ ERRORP NIL ERRORP-SPECD T))
        ((:ERROR) (SETQ ERRORP T ERRORP-SPECD T))
        ((:RAW) (SETQ RAW-P T))
        ((:SUPER-IMAGE) (SETQ SUPER-IMAGE-P T))
        ((:DELETED) (SETQ DELETED-P T))
        ((:TEMPORARY) (SETQ TEMPORARY-P T))
        ((:BLOCK :SINGLE))                      ;Ignored for compatility with Maclisp
        (OTHERWISE (FERROR NIL "~S is not a known OPEN option" KEY))))))

(DEFUN OPEN-FILE-SEARCH (BASE-PATHNAME TYPE-LIST DEFAULTS FOR-FUNCTION &REST OPEN-OPTIONS)
  (COND ((NULL (PATHNAME-RAW-TYPE BASE-PATHNAME)))
        ((AND (EQ (PATHNAME-RAW-TYPE BASE-PATHNAME) :UNSPECIFIC)
              (SEND BASE-PATHNAME :UNSPECIFIC-TYPE-IS-DEFAULT))
         ;; If type is really insignificant, replace it with NIL
         ;; so we will get the same behavior as if it were already NIL.
         (SETQ BASE-PATHNAME (SEND BASE-PATHNAME ':NEW-TYPE NIL)))
        ;; Otherwise, will use only the specified type,
        ;; so the elements of TYPE-LIST matter only in how many they are,
        ;; and we might as well have only one to avoid wasting time on duplicate opens.
        (T (SETQ TYPE-LIST '(NIL))))
  (DOLIST (TYPE TYPE-LIST
                (FERROR 'FS:MULTIPLE-FILE-NOT-FOUND
                        "~S could not find any file related to ~A."
                        FOR-FUNCTION BASE-PATHNAME
                        (MAPCAR #'(LAMBDA (TYPE)
                                    (FS:MERGE-PATHNAME-DEFAULTS
                                      BASE-PATHNAME DEFAULTS TYPE))
                                TYPE-LIST)))
    (CONDITION-CASE (OPEN-VALUE)
        (APPLY 'OPEN (FS:MERGE-PATHNAME-DEFAULTS
                       BASE-PATHNAME DEFAULTS TYPE)
               OPEN-OPTIONS)
      (FILE-NOT-FOUND)
      (:NO-ERROR (RETURN OPEN-VALUE)))))

(DEFUN CLOSE (STREAM &OPTIONAL ABORTP)
  "Close STREAM.  ABORTP says discard file, if output."
  (SEND STREAM ':CLOSE ABORTP))

(DEFUN CLI:CLOSE (STREAM &KEY ABORT)
  "Close STREAM.  ABORT non-NIL says discard file, if output."
  (SEND STREAM ':CLOSE (IF ABORT ':ABORT)))

(DEFUN WILDCARDED-FILE-OPERATION (STRING-OR-STREAM
                                  HELPER-FUNCTION
                                  DIR-LIST-OPTIONS
                                  &REST ARGS)
  "Call HELPER-FUNCTION for each file which STRING-OR-STREAM refers to.
If STRING-OR-STREAM is a string (or a pathname) then it can refer to
more than one file by containing wildcards.
The arguments passed to HELPER-FUNCTION each time are
1) the truename of a file,
2) the possibly wildcarded pathname being mapped over
 followed by ARGS.
DIR-LIST-OPTIONS are passed to FS:DIRECTORY-LIST when finding out
what files exist to be processed."
  (FORCE-USER-TO-LOGIN)
  (IF (OR (STRINGP STRING-OR-STREAM)
          (TYPEP STRING-OR-STREAM 'PATHNAME))   ;Not a stream
      (LET ((SPECIFIED-PATHNAME (MERGE-PATHNAME-DEFAULTS STRING-OR-STREAM)))
        (LEXPR-SEND SPECIFIED-PATHNAME ':WILDCARD-MAP HELPER-FUNCTION
                    NIL DIR-LIST-OPTIONS SPECIFIED-PATHNAME ARGS))
    (LET ((SPECIFIED-PATHNAME (SEND STRING-OR-STREAM ':TRUENAME)))
      (LIST (APPLY HELPER-FUNCTION
                   SPECIFIED-PATHNAME
                   SPECIFIED-PATHNAME
                   ARGS)))))

;;; Handle special file query stuff.  Each of the file operations expects
;;; the query optional argument to be a format style function.  The result
;;; of calling the query function is a boolean, and by using this particular
;;; function with *FILE-QUERY-FLAG* closed over it, it is possible to proceed too.
;;;
;;; The function returns 4 possible values:
;;;
;;; NIL, T == asked the user, got Y or N response.
;;; :PROCEED == asked the user, got P response.
;;; :NEVER-ASKED == didn't ask the user.

(DEFVAR *FILE-QUERY-FLAG* NIL)
(DEFVAR *FILE-QUERY-OPTIONS*
        `(:CHOICES (,@FORMAT:Y-OR-N-P-CHOICES
                    ((:PROCEED "Proceed.") #/P #/HAND-RIGHT))))

(DEFUN FILE-QUERY-FUNCTION (FORMAT-STRING &REST ARGS)
  (IF (NULL *FILE-QUERY-FLAG*)
      ':NEVER-ASKED
      (LET ((QRESULT (APPLY 'FQUERY *FILE-QUERY-OPTIONS* FORMAT-STRING ARGS)))
        (IF (EQ QRESULT ':PROCEED)
            (SETQ *FILE-QUERY-FLAG* NIL))
        QRESULT)))

(DEFUN MAKE-FILE-QUERY-FUNCTION (QUERY?)
  "Return a suitable query-function to pass to PRIMITIVE-DELETE-FILE, etc.
This query function, a closure, accepts a format-string and format-arguments,
queries the user and returns T or NIL.
If the user types P instead of Y, the query function returns ':PROCEED
and also returns ':NEVER-ASKED on all successive calls without asking any more.
If QUERY? is NIL, the query function always returns ':NEVER-ASKED without asking."
  (LET-CLOSED ((*FILE-QUERY-FLAG* QUERY?)) 'FILE-QUERY-FUNCTION))

(DEFF COPYF 'COPY-FILE) ;a logical assumption

(DEFUN COPY-FILE (PATHNAME-OR-STREAM NEW-NAME
                  &REST OPTIONS
                  &KEY (ERROR T)
                  &ALLOW-OTHER-KEYS)
  "Copy a file, specified as a pathname, string or I//O stream.
CHARACTERS can be T, NIL, meaning the same as in OPEN.
 or it can be :ASK, meaning always ask the user,
 or :MAYBE-ASK meaning ask the user unless the answer is clear,
 or :DEFAULT meaning guess as well as possible but never ask.
Specify BYTE-SIZE to force copying in a certain byte size.
 BYTE-SIZE affects only binary mode copying.
REPORT-STREAM is a stream to output messages to as files are copied.
 If it is NIL, no messages are output.
COPY-CREATION-DATE if NIL means don't copy the file creation date;
 make now be the new file's creation date.
COPY-AUTHOR if NIL means don't copy the author; make you the new file's author.
CREATE-DIRECTORIES says whether to create a directory to be copied into.
 Values are T, NIL and :QUERY (meaning ask the user if the situation comes up).
Values returned:
1) the first value is normally the defaulted pathname to copy to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
4) the fourth value is a mode of copying, or a list of such.
 A mode of copying is a type specifier such as STRING-CHAR or (UNSIGNED-BYTE 8).
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (ARGLIST PATHNAME-OR-STREAM NEW-NAME
                    &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
                    REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
                    (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT))
           (VALUES TARGET-PATHNAME TARGET-TRUENAME RESULT-PATHNAME COPY-MODE))
  (FORCE-USER-TO-LOGIN)
  (LET ((RESULT
          (IF (OR (STRINGP PATHNAME-OR-STREAM)
                  (TYPEP PATHNAME-OR-STREAM 'PATHNAME)) ;Not a stream
              (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
                                          (PATHNAME-OR-STREAM FILE-ERROR)
                (LET ((MERGED-PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME-OR-STREAM)))
                  (APPLY MERGED-PATHNAME
                         ':WILDCARD-MAP #'PRIMITIVE-COPY-FILE
                         ':MAYBE NIL
                         MERGED-PATHNAME (PARSE-PATHNAME NEW-NAME NIL MERGED-PATHNAME) OPTIONS)))
            (LET ((TRUENAME (SEND PATHNAME-OR-STREAM ':TRUENAME)))
              (LIST (APPLY 'PRIMITIVE-COPY-FILE
                           (FILE-PROPERTIES TRUENAME)
                           TRUENAME (PARSE-PATHNAME NEW-NAME NIL TRUENAME) OPTIONS))))))
    (IF (EQ (CAAR RESULT) (CADAR RESULT))
        (VALUES (THIRD (CAR RESULT))
                (FOURTH (CAR RESULT))
                (FIFTH (CAR RESULT))
                (SIXTH (CAR RESULT)))
      (VALUES (MAPCAR 'THIRD RESULT)
              (MAPCAR 'FOURTH RESULT)
              (MAPCAR 'FIFTH RESULT)
              (MAPCAR 'SIXTH RESULT)))))

(DEFCONST *COPY-FILE-KNOWN-TEXT-TYPES* '(:LISP :TEXT :MIDAS :PALX :PATCH-DIRECTORY :C
                                               :INIT :UNFASL :BABYL :XMAIL :MAIL :QWABL :DOC :BOTEX
                                               "LPT" "XGP" "ULOAD")
  "Files whose names have these canonical types are normally copied as characters.")
(DEFCONST *COPY-FILE-KNOWN-BINARY-TYPES* '(:QFASL :PRESS :WIDTHS :KST
                                                  :impress :dvi
                                                  "FASL" "MCR" "QBIN" "EXE" "BIN")
  "Files whose names have these canonical types are normally copied as binary.")

(DEFUN PRIMITIVE-COPY-FILE (INPUT-PLIST-OR-PATHNAME MAPPED-PATHNAME OUTPUT-SPEC
                            &KEY (ERROR T) (COPY-CREATION-DATE T) (COPY-AUTHOR T)
                            REPORT-STREAM (CREATE-DIRECTORIES ':QUERY)
                            (CHARACTERS ':DEFAULT) (BYTE-SIZE ':DEFAULT)
                            &AUX INTYPE INPUT-PLIST INPUT-PATHNAME INPUT-TRUENAME)
  (IF (NLISTP INPUT-PLIST-OR-PATHNAME)
      (SETQ INPUT-PATHNAME INPUT-PLIST-OR-PATHNAME
            INPUT-PLIST NIL)
    (SETQ INPUT-PATHNAME (CAR INPUT-PLIST-OR-PATHNAME)
          INPUT-PLIST INPUT-PLIST-OR-PATHNAME))
  ;; Decide whether to copy as binary file.
  ;; Either do as told, guess from file byte size or type, or ask the user.
  (LET ((CHARACTERS?
          (SELECTQ CHARACTERS
            ((T) CHARACTERS)
            (:ASK (FQUERY NIL "~&Is ~A a text file? " INPUT-PATHNAME))
            (OTHERWISE
             ;; At this point we really need to refer to the file's property list,
             ;; so get it if we were not given it as the first arg.
             (IF (NULL INPUT-PLIST)
                 (SETQ INPUT-PLIST (FILE-PROPERTIES INPUT-PATHNAME)
                       INPUT-PATHNAME (CAR INPUT-PLIST)))
             (LET ((BYTE-SIZE (GET INPUT-PLIST ':BYTE-SIZE)))
               (COND ((NULL CHARACTERS) NIL)
                     ((MEMQ BYTE-SIZE '(7 8)) T)
                     ((EQ BYTE-SIZE 16.) NIL)
                     ((MEMBER (SETQ INTYPE (SEND INPUT-PATHNAME ':CANONICAL-TYPE))
                              *COPY-FILE-KNOWN-TEXT-TYPES*)
                      T)
                     ((MEMBER INTYPE *COPY-FILE-KNOWN-BINARY-TYPES*) NIL)
                     ((EQ CHARACTERS ':DEFAULT) ':DEFAULT)
                     (T (FQUERY '(:BEEP T) "~&Is ~A a text file? " INPUT-PATHNAME))))))))
    (IF (EQ BYTE-SIZE ':DEFAULT)
        (SETQ BYTE-SIZE (OR (GET INPUT-PLIST ':BYTE-SIZE) ':DEFAULT)))
    (IF (EQ BYTE-SIZE 36.)
        (SETQ BYTE-SIZE 12.))
    (CONDITION-CASE-IF (NOT ERROR) (ERROR)
        (WITH-OPEN-FILE (INSTREAM INPUT-PATHNAME
                                  ':DIRECTION ':INPUT
                                  ':CHARACTERS CHARACTERS?
                                  ':BYTE-SIZE BYTE-SIZE)
          (SETQ INPUT-TRUENAME (SEND INSTREAM ':TRUENAME))
          (LET ((DEFAULTED-NEW-NAME
                  (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                    (MERGE-PATHNAME-DEFAULTS
                      (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME
                            OUTPUT-SPEC INPUT-TRUENAME)
                      INPUT-TRUENAME))))
            (CONDITION-BIND ((DIRECTORY-NOT-FOUND
                               #'(LAMBDA (ERROR)
                                   (WHEN (IF (EQ CREATE-DIRECTORIES ':QUERY)
                                             (PROGN
                                               (SEND *QUERY-IO* ':FRESH-LINE)
                                               (SEND ERROR ':REPORT *QUERY-IO*)
                                               (Y-OR-N-P "Create the directory? "))
                                           CREATE-DIRECTORIES)
                                     (CREATE-DIRECTORY defaulted-new-name ':RECURSIVE T)
                                     ':RETRY-FILE-OPERATION))))
              (WITH-OPEN-FILE (OUTSTREAM DEFAULTED-NEW-NAME
                                         ':DIRECTION ':OUTPUT
                                         ':CHARACTERS CHARACTERS?
                                         ':BYTE-SIZE (IF CHARACTERS? ':DEFAULT BYTE-SIZE))
                (IF COPY-AUTHOR
                    (IF COPY-CREATION-DATE
                        (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
                              ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE)
                              ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
                                           (GET INPUT-PLIST ':AUTHOR)))
                      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
                            ':AUTHOR (OR (SEND INSTREAM ':GET ':AUTHOR)
                                         (GET INPUT-PLIST ':AUTHOR))))
                  (IF COPY-CREATION-DATE
                      (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
                            ':CREATION-DATE (SEND INSTREAM ':CREATION-DATE))))
                (STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM)
                (CLOSE OUTSTREAM)
                (WHEN REPORT-STREAM
                  (FORMAT REPORT-STREAM "~&Copied ~A to ~A "
                          INPUT-TRUENAME (SEND OUTSTREAM ':TRUENAME))
                  (IF CHARACTERS?
                      (FORMAT REPORT-STREAM "in character mode.~%")
                    (FORMAT REPORT-STREAM "in byte size ~D.~%"
                            BYTE-SIZE)))
                (LIST MAPPED-PATHNAME INPUT-PATHNAME DEFAULTED-NEW-NAME
                      INPUT-TRUENAME (SEND OUTSTREAM ':TRUENAME)
                      (STREAM-ELEMENT-TYPE INSTREAM))))))
      ((FILE-ERROR SYS:REMOTE-NETWORK-ERROR)
       (LIST MAPPED-PATHNAME INPUT-PATHNAME
             (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
               (MERGE-PATHNAME-DEFAULTS
                 (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME
                       OUTPUT-SPEC (OR INPUT-TRUENAME INPUT-PATHNAME))
                 (OR INPUT-TRUENAME INPUT-PATHNAME)))
             INPUT-PATHNAME ERROR)))))

(DEFUN RENAME-FILE (STRING-OR-STREAM NEW-NAME &KEY (ERROR T) QUERY)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR is NIL."
  (DECLARE (VALUES OLD-NAME OLD-TRUENAME NEW-TRUENAME))
  (RENAMEF STRING-OR-STREAM NEW-NAME ERROR QUERY))

(DEFUN RENAMEF (STRING-OR-STREAM NEW-NAME &OPTIONAL (ERROR-P T) QUERY?)
  "Rename a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means ask about each file before renaming it.
Values returned:
1) the first value is normally the defaulted pathname to rename to,
 or a list of such if multiple files were considered.
2) the second value is the old truename of the file considered,
 or a list of old truenames of the files considered.
3) the third value is the outcome, or a list of outcomes.
 An outcome is either a truename if the file was renamed,
 an error object if it failed to be renamed,
 or NIL if the user was asked and said no.
Error objects can appear in the values only if ERROR-P is NIL."
  (DECLARE (VALUES OLD-NAME OLD-TRUENAME NEW-TRUENAME))
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
                                       (TYPEP STRING-OR-STREAM 'PATHNAME))
                                   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
                              (STRING-OR-STREAM FILE-ERROR)
    (LET* ((FROM-PATHNAME (PATHNAME STRING-OR-STREAM))
           (RESULT (WILDCARDED-FILE-OPERATION
                     STRING-OR-STREAM
                     #'PRIMITIVE-RENAME-FILE
                     NIL
                     (PARSE-PATHNAME NEW-NAME NIL FROM-PATHNAME) ERROR-P
                     (MAKE-FILE-QUERY-FUNCTION QUERY?))))
      (IF (EQ (CAAR RESULT) (CADAR RESULT))
          (VALUES (THIRD (CAR RESULT))
                  (FOURTH (CAR RESULT))
                  (FIFTH (CAR RESULT)))
        (VALUES (MAPCAR 'THIRD RESULT)
                (MAPCAR 'FOURTH RESULT)
                (MAPCAR 'FIFTH RESULT))))))

(DEFUN PRIMITIVE-RENAME-FILE (OLD-NAME MAPPED-PATHNAME NEW-NAME &OPTIONAL (ERROR-P T) QUERYF)
  (LET ((TRUENAME (IF (or (EQ OLD-NAME MAPPED-PATHNAME)
                          (typep old-name 'logical-pathname))
                      (SEND OLD-NAME ':TRUENAME ERROR-P)
                    OLD-NAME))
        (newname (if (typep new-name 'logical-pathname)
                     (translated-pathname (merge-pathname-defaults new-name mapped-pathname))
                   new-name)))
    (IF (ERRORP TRUENAME)
        (LIST MAPPED-PATHNAME OLD-NAME NIL OLD-NAME TRUENAME)
      (LET* ((DEFAULTED-NEW-NAME
               (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                 (MERGE-PATHNAME-DEFAULTS
                   (SEND MAPPED-PATHNAME ':TRANSLATE-WILD-PATHNAME newname TRUENAME)
                   TRUENAME)))
             (RENAMED? (FUNCALL QUERYF "~&Rename ~A to ~A? "
                                TRUENAME DEFAULTED-NEW-NAME))
             (RESULT (AND RENAMED? (SEND TRUENAME ':RENAME
                                         DEFAULTED-NEW-NAME ERROR-P))))
        (LIST MAPPED-PATHNAME OLD-NAME DEFAULTED-NEW-NAME TRUENAME RESULT)))))

(DEFUN CREATE-LINK (LINK LINK-TO &KEY (ERROR T))
  "Create a link, which is specified as a pathname or string, to the file LINK-TO."
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
                              (LINK FILE-ERROR)
    (LET ((PATHNAME (MERGE-PATHNAME-DEFAULTS LINK)))
      (SEND PATHNAME ':CREATE-LINK (LET ((*ALWAYS-MERGE-TYPE-AND-VERSION* T))
                                     (MERGE-PATHNAME-DEFAULTS LINK-TO PATHNAME))
            ':ERROR ERROR))))

(DEFUN DELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Delete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY, if true, means to ask the user before deleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't delete this one,
 or another non-NIL object if the file was deleted.
 OUTCOME can be an error object only if ERROR is NIL.
ERROR does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (DELETEF STRING-OR-STREAM ERROR QUERY))

(DEFUN DELETEF (STRING-OR-STREAM &OPTIONAL (ERROR-P T) QUERY?)
  "Delete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.
QUERY?, if true, means to ask the user before deleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't delete this one,
 or another non-NIL object if the file was deleted.
 OUTCOME can be an error object only if ERROR-P is NIL.
ERROR-P does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
                                       (TYPEP STRING-OR-STREAM 'PATHNAME))
                                   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
                              (STRING-OR-STREAM FILE-ERROR)
    (WILDCARDED-FILE-OPERATION STRING-OR-STREAM
                               #'PRIMITIVE-DELETE-FILE NIL
                               ERROR-P
                               (MAKE-FILE-QUERY-FUNCTION QUERY?))))

(DEFUN PRIMITIVE-DELETE-FILE (PATHNAME MAPPED-PATHNAME &OPTIONAL (ERROR-P T) QUERYF)
  "QUERYF should be a function that takes a format-string and a pathname
and returns T or NIL saying whether to delete that file.
If you don't want any querying, pass FILE-QUERY-TRUE as QUERYF."
  (LET ((TRUENAME (cond
                    ((EQ PATHNAME MAPPED-PATHNAME) (SEND PATHNAME ':TRUENAME ERROR-P))
                    ((typep pathname 'logical-pathname) (translated-pathname pathname))
                    (t PATHNAME))))
    (IF (ERRORP TRUENAME)
        (LIST PATHNAME TRUENAME)
      (LET* ((DELETE? (FUNCALL QUERYF "~&Delete ~A? " TRUENAME))
             (RESULT (AND DELETE? (SEND TRUENAME ':DELETE ERROR-P))))
        (LIST TRUENAME (IF (ERRORP RESULT) RESULT DELETE?))))))

(DEFUN UNDELETE-FILE (STRING-OR-STREAM &KEY (ERROR T) QUERY)
  "Undelete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.  Not all file servers support undeletion.
QUERY, if true, means to ask the user before undeleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't undelete this one,
 or another non-NIL object if the file was undeleted.
 OUTCOME can be an error object only if ERROR is NIL.
ERROR does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (UNDELETEF STRING-OR-STREAM ERROR QUERY))

(DEFUN UNDELETEF (STRING-OR-STREAM &OPTIONAL (ERROR-P T) QUERY?)
  "Undelete a file, specified as a pathname, string or I//O stream.
Wildcards are allowed.  Not all file servers support undeletion.
QUERY?, if true, means to ask the user before undeleting each file.
The value is a list containing one element for each file we considered;
 the element looks like (TRUENAME OUTCOME), where OUTCOME
 is either an error object, NIL if the user said don't undelete this one,
 or another non-NIL object if the file was undeleted.
 OUTCOME can be an error object only if ERROR-P is NIL.
ERROR-P does not affect errors that happen in determining
 what files match a wildcarded pathname."
  (FILE-RETRY-NEW-PATHNAME-IF (AND (OR (STRINGP STRING-OR-STREAM)
                                       (TYPEP STRING-OR-STREAM 'PATHNAME))
                                   (MEMQ ERROR-P '(:RETRY :REPROMPT)))
                              (STRING-OR-STREAM FILE-ERROR)
    (WILDCARDED-FILE-OPERATION STRING-OR-STREAM
                               #'PRIMITIVE-UNDELETE-FILE '(:DELETED)
                               ERROR-P
                               (MAKE-FILE-QUERY-FUNCTION QUERY?))))

(DEFUN PRIMITIVE-UNDELETE-FILE (PATHNAME MAPPED-PATHNAME &OPTIONAL (ERROR-P T) QUERYF)
  "QUERYF should be a function that takes a format-string and a pathname
and returns T or NIL saying whether to delete that file.
If you don't want any querying, pass FILE-QUERY-TRUE as QUERYF."
  (LET ((TRUENAME (IF (EQ PATHNAME MAPPED-PATHNAME)
                      (WITH-OPEN-FILE (STREAM PATHNAME ':ERROR ERROR-P ':DELETED T
                                              ':DIRECTION NIL)
                        (IF (ERRORP STREAM) STREAM
                          (SEND STREAM ':TRUENAME)))
                    PATHNAME)))
    (IF (ERRORP TRUENAME)
        (LIST PATHNAME TRUENAME)
      (LET* ((UNDELETE? (FUNCALL QUERYF "~&Undelete ~A? " TRUENAME))
             (RESULT (AND UNDELETE? (SEND TRUENAME ':UNDELETE ERROR-P))))
        (LIST TRUENAME (IF (ERRORP RESULT) RESULT UNDELETE?))))))

(DEFF PROBE-FILE 'PROBEF)
(DEFUN PROBEF (FILE)
  "Non-NIL if FILE exists.
Return the file's truename if successful, NIL if file-not-found.
Any other error condition is not handled."
  (IF (STREAMP FILE)
      (SEND FILE ':TRUENAME)
    (CONDITION-CASE (STREAM)
        (OPEN FILE ':DIRECTION NIL)
      (FILE-NOT-FOUND NIL)
      (:NO-ERROR (PROG1 (SEND STREAM ':TRUENAME) (CLOSE STREAM))))))

(DEFUN FILE-WRITE-DATE (FILENAME-OR-STREAM)
  "Return file's creation or last write date.  Specify pathname, namestring or file stream."
  (IF (OR (STRINGP FILENAME-OR-STREAM)
          (SYMBOLP FILENAME-OR-STREAM)
          (TYPEP FILENAME-OR-STREAM 'PATHNAME))
      (WITH-OPEN-FILE (STREAM FILENAME-OR-STREAM ':DIRECTION NIL)
        (SEND STREAM ':CREATION-DATE))
    (SEND FILENAME-OR-STREAM ':CREATION-DATE)))

(DEFUN FILE-AUTHOR (FILENAME-OR-STREAM)
  "Return file's author's name (a string).  Specify pathname, namestring or file stream."
  (IF (OR (STRINGP FILENAME-OR-STREAM)
          (SYMBOLP FILENAME-OR-STREAM)
          (TYPEP FILENAME-OR-STREAM 'PATHNAME))
      (WITH-OPEN-FILE (STREAM FILENAME-OR-STREAM ':DIRECTION NIL)
        (SEND STREAM ':GET ':AUTHOR))
    (SEND FILENAME-OR-STREAM ':GET ':AUTHOR)))

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:46:52
(DEFUN FILE-POSITION (FILE-STREAM &OPTIONAL NEW-POSITION)
  "Return or set the stream pointer of FILE-STREAM.
With one argument, return the stream pointer of FILE-STREAM, or NIL if not known.
With two arguments, try to set the stream pointer to NEW-POSITION
 and return T if it was possible to do so."
  (COND (NEW-POSITION
         (SEND-IF-HANDLES FILE-STREAM :SET-POINTER NEW-POSITION))
        (T
         (SEND-IF-HANDLES FILE-STREAM :READ-POINTER))))

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:46:53
(DEFUN FILE-LENGTH (FILE-STREAM)
  "Return the length of the file that is open, or NIL if not known."
  (SEND FILE-STREAM :LENGTH))

(DEFUN VIEWF (FILE &OPTIONAL (OUTPUT-STREAM *STANDARD-OUTPUT*) LEADER)
  "Print the contents of a file on the output stream.
LEADER is passed to the :LINE-IN operation."
  (WITH-OPEN-FILE (FILE-STREAM FILE ':ERROR ':REPROMPT)
    (IF (ERRORP FILE-STREAM)
        FILE-STREAM
      (SEND OUTPUT-STREAM ':FRESH-LINE)
      (STREAM-COPY-UNTIL-EOF FILE-STREAM OUTPUT-STREAM LEADER))))

;;this isn't the right place for this, but its not clear where it should go
(DEFUN FS:WORKING-DIRECTORY () ;&optional defaults
  (LET ((PATHNAME (FS:DEFAULT-PATHNAME)))
    (FS:MAKE-PATHNAME ':HOST (SEND PATHNAME ':HOST) ':DIRECTORY (SEND PATHNAME ':DIRECTORY))))

(DEFUN LISTF (&OPTIONAL (DIRECTORY (FS:WORKING-DIRECTORY))
              (OUTPUT-STREAM *STANDARD-OUTPUT*) LEADER)
  "Print a listing of DIRECTORY on OUTPUT-STREAM.
LEADER is passed to the :LINE-IN operation."
  (SETQ DIRECTORY (FS:PARSE-PATHNAME DIRECTORY))
  (OR (SEND DIRECTORY ':NAME)
      (SETQ DIRECTORY (SEND DIRECTORY ':NEW-NAME ':WILD)))
  (SETQ DIRECTORY (FS:MERGE-PATHNAME-DEFAULTS DIRECTORY NIL ':WILD ':WILD))
  (SEND OUTPUT-STREAM ':FRESH-LINE)
  (FILE-RETRY-NEW-PATHNAME (DIRECTORY FS:FILE-ERROR)
    (WITH-OPEN-STREAM (STREAM (ZWEI:DIRECTORY-INPUT-STREAM DIRECTORY))
      (STREAM-COPY-UNTIL-EOF STREAM OUTPUT-STREAM LEADER)))
  DIRECTORY)


(DEFVAR USER-HOMEDIRS NIL
  "Alist mapping host objects to pathnames.  Gives the home device//directory for each host.")
(DEFVAR USER-PERSONAL-NAME ""
  "The user's full name, last name first.  Obtained from the FS:USER-LOGIN-MACHINE.")
(DEFVAR USER-PERSONAL-NAME-FIRST-NAME-FIRST ""
  "The user's full name, first name first.  Obtained from the FS:USER-LOGIN-MACHINE.")
(DEFVAR USER-GROUP-AFFILIATION #/-
  "The user's /"group affiliation/", a character.  Obtained from the FS:USER-LOGIN-MACHINE.")
(DEFVAR USER-LOGIN-MACHINE SI:ASSOCIATED-MACHINE
  "The host the user logged in on.")

(DEFF USER-HOMEDIR-PATHNAME 'USER-HOMEDIR)
(DEFUN USER-HOMEDIR (&OPTIONAL (HOST USER-LOGIN-MACHINE) RESET-P (USER USER-ID))
  "Return a pathname describing the home directory for USER on host HOST.
This is used as a default, sometimes.  RESET-P says make this our login host."
  (SETQ HOST (GET-PATHNAME-HOST HOST))
  (AND (TYPEP HOST 'LOGICAL-HOST) (SETQ HOST (SEND HOST ':PHYSICAL-HOST)))      ;Just in case
  (AND RESET-P (SETQ USER-LOGIN-MACHINE HOST))
  (CONDITION-CASE ()
      (SEND (SAMPLE-PATHNAME HOST) ':HOMEDIR USER)
    (FILE-ERROR
     (QUIET-USER-HOMEDIR HOST))))

(DEFUN QUIET-USER-HOMEDIR (HOST)
  (OR (CDR (ASSQ HOST USER-HOMEDIRS))
      (SEND (SAMPLE-PATHNAME HOST) ':QUIET-HOMEDIR)))

(DEFUN FORCE-USER-TO-LOGIN (&OPTIONAL (HOST SI:ASSOCIATED-MACHINE)
                            &AUX INPUT USER DONT-READ-INIT IDX IDX2)
  "Require the user to log in.  HOST is a default for logging in."
  (WHEN (OR (NULL USER-ID) (STRING-EQUAL USER-ID ""))
    (SEND *QUERY-IO* ':BEEP)
    (FORMAT *QUERY-IO*
            "~&Please log in.  ~
~<~%~:;Type username or username@host ~<~%~:;(host defaults to ~A)~>~>
To avoid loading your init file, ~<~%~:;follow by <space>T : ~>"
            HOST)
    (SETQ INPUT (STRING-TRIM '(#/SPACE #/TAB) (READLINE *QUERY-IO*)))
    (AND (SETQ IDX (STRING-SEARCH-CHAR #/@ INPUT))
         (SETQ USER (SUBSTRING INPUT 0 IDX)))
    (AND (SETQ IDX2 (STRING-SEARCH-SET '(#/SPACE #/TAB) INPUT (OR IDX 0)))
         (SETQ DONT-READ-INIT (READ-FROM-STRING INPUT T IDX2)))
    (IF IDX
        (SETQ HOST (SUBSTRING INPUT (1+ IDX) IDX2))
      (SETQ USER (SUBSTRING INPUT 0 IDX2)))
    (OR (STRING-EQUAL USER "")
        (LOGIN USER HOST DONT-READ-INIT))))

(DEFVAR USER-UNAMES NIL "Alist mapping host objects into usernames.")
(DEFUN FILE-HOST-USER-ID (UID HOST)
  "Specify the user-id UID for use on host HOST."
  (AND (MEMQ (SEND HOST ':SYSTEM-TYPE) '(:ITS :LMFILE))
       ;; All ITS's have the same set of unames, so record as ITS rather than the host.
       (SETQ HOST 'ITS
             UID (SUBSTRING UID 0 (MIN (STRING-LENGTH UID) 6))))
  (LET ((AE (ASSQ HOST USER-UNAMES)))
       (IF AE
           (RPLACD AE UID)
           (PUSH (CONS HOST UID) USER-UNAMES))))

(DEFUN UNAME-ON-HOST (HOST)                     ; Must be a host object
  (CDR (ASSQ (IF (EQ (SEND HOST ':SYSTEM-TYPE) ':ITS) 'ITS HOST) USER-UNAMES)))

(ADD-INITIALIZATION "File Host User ID" '(FILE-HOST-USER-ID USER-ID USER-LOGIN-MACHINE)
                    '(LOGIN))
(ADD-INITIALIZATION "Reset File Host User ID" '(SETQ USER-UNAMES NIL) '(LOGOUT))

;;; Alist of elements ((username host) password-string)
(DEFVAR USER-HOST-PASSWORD-ALIST NIL
  "Alist of elements ((USERNAME HOST-NAME) PASSWORD).
The three data in each element are all strings.")

(DEFVAR RECORD-PASSWORDS-FLAG T
  "T => record passwords when the user types them, in case they are useful again.")
; brand s name
(DEFVAR *REMEMBER-PASSWORDS* :UNBOUND
  "T => record passwords when the user types them, in case they are useful again.")
(FORWARD-VALUE-CELL '*REMEMBER-PASSWORDS* 'RECORD-PASSWORDS-FLAG)

(ADD-INITIALIZATION "Forget Passwords" '(SETQ USER-HOST-PASSWORD-ALIST NIL) '(BEFORE-COLD))

(DEFUN FILE-GET-PASSWORD (UID HOST &OPTIONAL DIRECTORY-FLAG)
  "Given a username and host, ask for either a password or a username and password.
If DIRECTORY-FLAG is set, we are using directory names, not passwords"
  (DECLARE (VALUES NEW-USER-ID PASSWORD ENABLE-CAPABILITIES))
  (LET* ((LINE (MAKE-STRING 30 ':FILL-POINTER 0))
         ENABLE-CAPABILITIES CHAR UID-P
         (HACK (AND (SEND *QUERY-IO* ':OPERATION-HANDLED-P ':CLEAR-BETWEEN-CURSORPOSES)
                    (SEND *QUERY-IO* ':OPERATION-HANDLED-P ':COMPUTE-MOTION)))
         START-X START-Y)
    (UNLESS DIRECTORY-FLAG
      (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
    (LET ((PW (CADR (ASSOC-EQUALP (LIST UID (SEND HOST ':NAME))
                                  USER-HOST-PASSWORD-ALIST))))
      (WHEN PW (RETURN-FROM FILE-GET-PASSWORD (values UID PW))))
    (TAGBODY
        (WHEN HACK (SETQ HACK (MAKE-STRING 30. ':INITIAL-VALUE #/X ':FILL-POINTER 0)))
     RESTART
        (UNLESS DIRECTORY-FLAG
          (SETQ UID (OR (CDR (ASSQ HOST USER-UNAMES)) UID)))
        (LET ((PW (CADR (ASSOC-EQUALP (LIST UID (SEND HOST ':NAME))
                                      USER-HOST-PASSWORD-ALIST))))
          (WHEN PW (RETURN-FROM FILE-GET-PASSWORD (values UID PW))))
        (FORMAT *QUERY-IO*
                (IF DIRECTORY-FLAG "~&Type the password for directory ~A on host ~A,
or a directory and password.  /"Directory/" here includes devices as well: "
                  "~&Current login name is ~A ~<~%~:;for host ~A.~>
Type either password or ~<~%~:;loginname<space>password: ~>")
                UID HOST)
        (WHEN HACK (MULTIPLE-VALUE (START-X START-Y) (SEND *QUERY-IO* ':READ-CURSORPOS)))
     L  (SETQ CHAR (SEND *QUERY-IO* ':TYI))
        (COND ((= CHAR #/C-Q)                   ;quoting character.
               (VECTOR-PUSH-EXTEND (SEND *QUERY-IO* ':TYI) LINE)
               (WHEN HACK
                 (VECTOR-PUSH-EXTEND #/X HACK)
                 (SEND *QUERY-IO* ':TYO #/X)))
              ((= CHAR #/RUBOUT)
               (WHEN (ZEROP (FILL-POINTER LINE))
                 (GO FLUSH))
               (VECTOR-POP LINE)
               (WHEN HACK
                 (VECTOR-POP HACK)
                 (MULTIPLE-VALUE-BIND (X Y)
                     (SEND *QUERY-IO* ':COMPUTE-MOTION HACK 0 NIL
                                      START-X START-Y)
                   (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
                     (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES X Y CX CY))
                   (SEND *QUERY-IO* ':SET-CURSORPOS X Y))))
              ((= CHAR #/CLEAR-INPUT)
               (GO FLUSH))
              ((AND (= CHAR #/SPACE)
                    (NOT UID-P))                ;allow spaces in passwords
               (WHEN ENABLE-CAPABILITIES
                 (VECTOR-PUSH-EXTEND #/* LINE 1);make sure we have room for extra element
                 (DOTIMES (I (1- (FILL-POINTER LINE)))
                   (SETF (AREF LINE (1+ I)) (AREF LINE (1- I))))
                 (SETF (CHAR LINE 0) #/*)
                 (SETQ ENABLE-CAPABILITIES NIL))
               (SETQ UID-P T
                     UID LINE
                     LINE (MAKE-STRING 30. :FILL-POINTER 0))
               (WHEN HACK
                 (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
                   (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
                 (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y))
               (FORMAT *QUERY-IO* "~A " UID)
               (WHEN HACK (MULTIPLE-VALUE (START-X START-Y)
                            (SEND *QUERY-IO* ':READ-CURSORPOS))))
              ((= CHAR #/RETURN)
               (OR DIRECTORY-FLAG (FILE-HOST-USER-ID UID HOST))
               (IF RECORD-PASSWORDS-FLAG
                   (PUSH `((,UID ,(SEND HOST ':NAME)) ,LINE) USER-HOST-PASSWORD-ALIST))
               (WHEN HACK
                 (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
                   (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
                 (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y))
               (FRESH-LINE *QUERY-IO*)
               (SEND *QUERY-IO* ':SEND-IF-HANDLES ':MAKE-COMPLETE)
               (RETURN-FROM FILE-GET-PASSWORD (values UID LINE ENABLE-CAPABILITIES)))
              ((AND (= CHAR #/*)
                    (= (FILL-POINTER LINE) 0))
               (SETQ ENABLE-CAPABILITIES T))
              (( 0 (CHAR-BITS CHAR)) (BEEP))
              (T (WHEN HACK
                   (VECTOR-PUSH-EXTEND #/X HACK)
                   (SEND *QUERY-IO* ':TYO #/X))
                 (VECTOR-PUSH-EXTEND CHAR LINE)))
        (GO L)
     FLUSH
        (WHEN HACK
          (MULTIPLE-VALUE-BIND (CX CY) (SEND *QUERY-IO* ':READ-CURSORPOS)
            (SEND *QUERY-IO* ':CLEAR-BETWEEN-CURSORPOSES START-X START-Y CX CY))
          (SEND *QUERY-IO* ':SET-CURSORPOS START-X START-Y)
          (SETF (FILL-POINTER HACK) 0))
        (WHEN UID-P (RETURN-ARRAY (PROG1 LINE (SETQ LINE UID UID NIL))))
        (FORMAT *QUERY-IO* "Flushed.~&")
        (SETF (FILL-POINTER LINE) 0 UID-P NIL)
        (GO RESTART))))

;;; Used by MAKE-SYSTEM for fast INFO access
(DEFUN MULTIPLE-FILE-PLISTS (FILENAMES &REST OPTIONS &AUX HOST-FILE-LIST)
  "Given a list of pathnames or namestrings, return a list of property lists.
Each element of the value looks like (pathname . properties)."
  (FORCE-USER-TO-LOGIN)
  (DOLIST (FILENAME FILENAMES)
    (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME))
    (DO ((HOST (SEND FILENAME ':HOST))
         (LIST HOST-FILE-LIST (CDR LIST)))
        ((NULL LIST)
         (PUSH (NCONS FILENAME) HOST-FILE-LIST))
      (COND ((EQ HOST (SEND (CAAR HOST-FILE-LIST) ':HOST))
             (PUSH FILENAME (CAR LIST))
             (RETURN)))))
  (LOOP FOR LIST IN (NREVERSE HOST-FILE-LIST)
        NCONC (SEND (CAR LIST) ':MULTIPLE-FILE-PLISTS (NREVERSE LIST) OPTIONS)))

;;; Old name for compatibility
(DEFUN MULTIPLE-FILE-PROPERTY-LISTS (BINARY-P FILENAMES)
  (MULTIPLE-FILE-PLISTS FILENAMES ':CHARACTERS (NOT BINARY-P)))

(DEFUN CLOSE-ALL-FILES (&OPTIONAL (MODE :ABORT))
  "Close all file streams that are open."
  (NCONC (AND (BOUNDP 'TV::WHO-LINE-FILE-STATE-SHEET)
              TV::WHO-LINE-FILE-STATE-SHEET
              (DO ((F (SEND TV::WHO-LINE-FILE-STATE-SHEET :OPEN-STREAMS)
                      (CDR F))
                   (THINGS-CLOSED NIL))
                  ((NULL F)
                   (SEND TV::WHO-LINE-FILE-STATE-SHEET :DELETE-ALL-STREAMS)
                   (NREVERSE THINGS-CLOSED))
                (FORMAT *ERROR-OUTPUT* "~%Closing ~S" (CAR F))
                (PUSH (CAR F) THINGS-CLOSED)
                (SEND (CAR F) :CLOSE MODE)))
         (LOOP FOR HOST IN *PATHNAME-HOST-LIST*
               NCONC (SEND HOST :CLOSE-ALL-FILES MODE))))

(DEFUN ALL-OPEN-FILES ()
  "Return a list of all file streams that are open."
  (SI:ELIMINATE-DUPLICATES (APPEND (SEND TV:WHO-LINE-FILE-STATE-SHEET ':OPEN-STREAMS)
                                   (LOOP FOR HOST IN *PATHNAME-HOST-LIST*
                                         APPEND (SEND HOST ':OPEN-STREAMS)))))

;;;; Directory stuff

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:46:55
(DEFUN DIRECTORY (PATHNAME)
  "Common Lisp function to get the list of files in a directory.
The value is just a list of truenames.
You will get much more useful information by using FS:DIRECTORY-LIST."
  (let ((path (pathname pathname)))
    (unless (send path :name)
      (setq path (send path :new-name :wild)))
    (MAPCAR 'CAR (CDR (DIRECTORY-LIST path :fast)))))

;;; This is the primary user interface to the directory listing
;;; stuff.  It returns a list of lists, one for each file.  The format
;;; of these lists is (PATHNAME . PLIST).  The currently defined indicators
;;; for PLIST are:
;;; :ACCOUNT <string>
;;; :AUTHOR <string>
;;; :BLOCK-SIZE <number>
;;; :BYTE-SIZE <number>
;;; :CREATION-DATE <universal-date>
;;; :DELETED <boolean>
;;; :DONT-DELETE <boolean>
;;; :DONT-DUMP <boolean>
;;; :DONT-REAP <boolean>
;;; :DUMPED <boolean>
;;; :GENERATION-RETENTION-COUNT <number>
;;; :LENGTH-IN-BLOCKS <number>
;;; :LENGTH-IN-BYTES <number>
;;; :LINK-TO <string>
;;; :OFFLINE <boolean>
;;; :PHYSICAL-VOLUME <string>
;;; :PROTECTION <string>
;;; :READER <string>
;;; :REFERENCE-DATE <universal-date>
;;; :TEMPORARY <boolean>

;;; A pathname of NIL is treated specially and gives properties for all
;;; the files listed.  The indicators for this "pathname" are:
;;; :SETTABLE-PROPERTIES <list-of-indicators, or just T>
;;; :BLOCK-SIZE  <number>
;;; :PHYSICAL-VOLUME-FREE-BLOCKS alist of (<string> . <number>)
;;; :DISK-SPACE-DESCRIPTION <string>

;;; The currently defined OPTIONS are:
;;; :NOERROR - as with OPEN.
;;; :DELETED - also (rather than exclusively) list deleted files.
;;; :NO-EXTRA-INFO - only include enough information for listing directory as in DIRED.
;;; :SORTED - we want the directory sorted at least so that
;;;   multiple versions of a file are consecutive in increasing version order.
(DEFUN DIRECTORY-LIST (FILENAME &REST OPTIONS)
  "Return a listing of the directory specified in FILENAME, a pathname or string.
OPTIONS can include :NOERROR, :DELETED (mention deleted files),
 :SORTED and :NO-EXTRA-INFO.
The value is an alist of elements (pathname . properties).
There is an element whose car is NIL.  It describes the directory as a whole.
One of its properties is :PATHNAME, whose value is the directory's pathname."
  (FORCE-USER-TO-LOGIN)
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME NIL ':WILD ':WILD))
  (SEND FILENAME ':DIRECTORY-LIST OPTIONS))

(DEFUN DIRECTORY-LIST-STREAM (FILENAME &REST OPTIONS)
  "Return a stream which returns elements of a directory-list one by one.
The stream's operations are :ENTRY, to return the next element, and :CLOSE."
  (FORCE-USER-TO-LOGIN)
  (SETQ FILENAME (MERGE-PATHNAME-DEFAULTS FILENAME NIL ':WILD ':WILD))
  (SEND FILENAME ':DIRECTORY-LIST-STREAM OPTIONS))

;;; These are the understood indicators
;;; Format is ((PARSER-FROM-STRING PRINTER TYPE-FOR-CHOOSE-VARIABLE-VALUES) . INDICATORS)
(DEFVAR *KNOWN-DIRECTORY-PROPERTIES*
  '(((PARSE-DIRECTORY-BOOLEAN-PROPERTY PRIN1 :BOOLEAN)
     . (:DELETED :DONT-DELETE :DONT-DUMP :DONT-REAP :DELETE-PROTECT :SUPERSEDE-PROTECT
        :NOT-BACKED-UP :OFFLINE :TEMPORARY :CHARACTERS :DUMPED :DIRECTORY
        ;; Supported by LM
        :QFASLP :PDP10 :MAY-BE-REAPED))
    ((SUBSTRING PRINC :STRING) . (:ACCOUNT :AUTHOR :LINK-TO :PHYSICAL-VOLUME :PROTECTION
                                  :VOLUME-NAME :PACK-NUMBER :READER :DISK-SPACE-DESCRIPTION
                                  :INCREMENTAL-DUMP-TAPE :COMPLETE-DUMP-TAPE))
    ((ZWEI:PARSE-NUMBER PRINT-DECIMAL-PROPERTY :NUMBER)
     . (:BLOCK-SIZE :BYTE-SIZE :GENERATION-RETENTION-COUNT :LENGTH-IN-BLOCKS
        :LENGTH-IN-BYTES :DEFAULT-GENERATION-RETENTION-COUNT))
    ((PARSE-DIRECTORY-DATE-PROPERTY PRINT-DIRECTORY-DATE-PROPERTY :DATE)
     . (:CREATION-DATE :MODIFICATION-DATE))
    ((PARSE-DIRECTORY-DATE-PROPERTY PRINT-UNIVERSAL-TIME-OR-NEVER-FOR-DIRLIST :DATE-OR-NEVER)
     . ( :REFERENCE-DATE :INCREMENTAL-DUMP-DATE :COMPLETE-DUMP-DATE :DATE-LAST-EXPUNGED
         :EXPIRATION-DATE))
    ((PARSE-SETTABLE-PROPERTIES PRINT-SETTABLE-PROPERTIES)
     . (:SETTABLE-PROPERTIES :LINK-TRANSPARENCIES :DEFAULT-LINK-TRANSPARENCIES))
    ((PARSE-DIRECTORY-FREE-SPACE PRINT-DIRECTORY-FREE-SPACE) . (:PHYSICAL-VOLUME-FREE-BLOCKS))
    ((TIME:PARSE-INTERVAL-OR-NEVER TIME:PRINT-INTERVAL-OR-NEVER :TIME-INTERVAL-OR-NEVER)
         . (:AUTO-EXPUNGE-INTERVAL))
    ))

(DEFVAR *TRANSFORMED-DIRECTORY-PROPERTIES* NIL
  "Fast access to directory properties for READ-DIRECTORY-STREAM-ENTRY.
Each element is a list (first-char . alist)
where alist's elements look like (propstring propsymbol parser printer cvv-type).")

(DEFUN TRANSFORM-DIRECTORY-PROPERTIES ()
  (SETQ *TRANSFORMED-DIRECTORY-PROPERTIES* NIL)
  (DOLIST (ELT *KNOWN-DIRECTORY-PROPERTIES*)
    (DOLIST (PROP (CDR ELT))
      (LET* ((HASH (AREF (GET-PNAME PROP) 0))
             (HASHELT
              (OR (ASSQ HASH *TRANSFORMED-DIRECTORY-PROPERTIES*)
                  (CAR (PUSH (CONS HASH NIL) *TRANSFORMED-DIRECTORY-PROPERTIES*)))))
        (PUSH (LIST* (GET-PNAME PROP) PROP (CAR ELT)) (CDR HASHELT))))))

(TRANSFORM-DIRECTORY-PROPERTIES)

;Read the text describing one element of a directory-list from STREAM.
;Default the pathname in it using DEFAULTING-PATHNAME, which should
;be the pathname of the directory being listed.
;If this is the entry for NIL, which describes the whole directory,
;then we append the directory pathname as the :PATHNAME property to this entry.
(DEFUN READ-DIRECTORY-STREAM-ENTRY (STREAM DEFAULTING-PATHNAME options &AUX PATH EOF IND FUN
                                    (DEFAULT-FUN (SEND DEFAULTING-PATHNAME
                                                       ':DIRECTORY-STREAM-DEFAULT-PARSER)))
     ;options is options to DIRECTORY-LIST.  See below.
  (MULTIPLE-VALUE (PATH EOF)
    (SEND STREAM ':LINE-IN))
  (IF EOF NIL
    (IF (ZEROP (ARRAY-ACTIVE-LENGTH PATH))
        (SETQ PATH NIL)
      (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER)
          (SEND DEFAULTING-PATHNAME ':PARSE-NAMESTRING NIL PATH)
        (SETQ PATH (MAKE-PATHNAME-INTERNAL
                     (PATHNAME-HOST DEFAULTING-PATHNAME)
                     (OR DEV (PATHNAME-DEVICE DEFAULTING-PATHNAME))
                     (OR DIR (PATHNAME-DIRECTORY DEFAULTING-PATHNAME))
                     NAM (OR TYP ':UNSPECIFIC) VER))))
    ;; This is a little hairy to try to avoid page faults when interning.
    (LOOP AS LINE = (SEND STREAM ':LINE-IN)
          AS LEN = (ARRAY-ACTIVE-LENGTH LINE)
          UNTIL (ZEROP LEN)
          AS I = (%STRING-SEARCH-CHAR #/SPACE LINE 0 LEN)
          DO (LOOP NAMED FOO
                   FOR X IN (CDR (ASSQ (AREF LINE 0) *TRANSFORMED-DIRECTORY-PROPERTIES*))
                   WHEN (%STRING-EQUAL LINE 0 (CAR X) 0 I)
                   DO (RETURN (SETQ IND (CADR X) FUN (CADDR X)))
                   FINALLY (SETQ IND (INTERN (SUBSTRING LINE 0 I) SI:PKG-KEYWORD-PACKAGE)
                                 FUN DEFAULT-FUN))
          NCONC (LIST* IND (OR (NULL I) (SEND FUN LINE (1+ I))) NIL) INTO PLIST
          FINALLY (progn (if (and (memq :directories-only options)      ;this allows c-u m-x dired on "ai:*;" to work!
                                  (not (get-from-alternating-list plist :directory)))
                             (setq plist (list* :directory t plist)))
                         (RETURN (CONS PATH (IF PATH PLIST
                                              (LIST* ':PATHNAME DEFAULTING-PATHNAME PLIST))))))))

;;; Nifty, handy function for adding new ones
(DEFUN PUSH-DIRECTORY-PROPERTY-ON-TYPE (TYPE PROP)
  (LET ((X (OR (DOLIST (E *KNOWN-DIRECTORY-PROPERTIES*)
                 (IF (EQ (CADDAR E) TYPE) (RETURN E)))
               (FERROR NIL "Unknown property type: ~A" TYPE))))
    (OR (MEMQ TYPE (CDR X))
        (PUSH PROP (CDR X))))
  (TRANSFORM-DIRECTORY-PROPERTIES))

(DEFUN PARSE-RANDOM-SEXP (STRING START)
  (LET ((*READ-BASE* 10.)
        (*READTABLE* SI:INITIAL-READTABLE)
        (*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
    (READ-FROM-STRING STRING NIL START)))

(DEFUN PRINT-RANDOM-SEXP (SEXP &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (LET ((*PRINT-BASE* 10.)
        (*NOPOINT T) (*PRINT-RADIX* NIL)
        (*READTABLE* SI:INITIAL-READTABLE)
        (*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
    (PRIN1 SEXP STREAM)))

;;; Fast date parser for simple case of MM/DD/YY HH:MM:SS
(DEFUN PARSE-DIRECTORY-DATE-PROPERTY (STRING START &OPTIONAL END &AUX FLAG)
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (COND  ;;addition to "protocol" of 10/3/85: string consisting entirely of digits is
         ;; directly universal time in decimal, so read it in.
        ((NULL (STRING-SEARCH-NOT-SET "0123456789" STRING START END))
         (LET ((*read-base* 10.)
               (*print-base* 10.))
           (READ-FROM-STRING STRING NIL START END)))
        ((NOT (FBOUNDP 'ENCODE-UNIVERSAL-TIME))
         ;; in cold load --- filesystem initialization fixes this up later.
         NIL)
        ((AND (OR (= END (+ START 8))
                  (SETQ FLAG (= END (+ START 17.))))
              (EQL (CHAR STRING (+ START 2)) #//)
              (EQL (CHAR STRING (+ START 5)) #//)
              (OR (NULL FLAG)
                  (AND (EQL (CHAR STRING (+ START 8)) #/SPACE)
                       (EQL (CHAR STRING (+ START 11.)) #/:)
                       (EQL (CHAR STRING (+ START 14.)) #/:))))
         (FLET ((GET-TWO-DIGITS (START)
                  (+ (* (DIGIT-CHAR-P (CHAR STRING START)) 10.)
                     (DIGIT-CHAR-P (CHAR STRING (1+ START))))))
           (LET* ((MONTH (GET-TWO-DIGITS START))
                  (DAY (GET-TWO-DIGITS (+ START 3)))
                  (YEAR (GET-TWO-DIGITS (+ START 6)))
                  (HOURS (IF FLAG (GET-TWO-DIGITS (+ START 9.)) 0))
                  (MINUTES (IF FLAG (GET-TWO-DIGITS (+ START 12.)) 0))
                  (SECONDS (IF FLAG (GET-TWO-DIGITS (+ START 15.)) 0)))
             ;; The file job is wont to give dates of the form 00/00/00 for things made by
             ;; ITS DSKDMP, e.g..  Avoid errors later.
             (AND (PLUSP MONTH)
                  (ENCODE-UNIVERSAL-TIME SECONDS MINUTES HOURS DAY MONTH YEAR)))))
        (T
         ;;Not in simple format, escape to full parser
         ;;>> This should signal a qfile-protocol-violation error!
         (CONDITION-CASE ()
             (TIME:PARSE-UNIVERSAL-TIME STRING START END)
           (ERROR NIL)))))


(DEFUN PRINT-UNIVERSAL-TIME-OR-NEVER-FOR-DIRLIST (TIME STREAM)
  (IF (NULL TIME) (PRINC "never" STREAM)
    (TIME:PRINT-UNIVERSAL-TIME TIME STREAM NIL ':mm//dd//yy)))


;;; Printer which always prints MM/DD/YY HH:MM:SS
;;; ***This is a needed bug fix.  Strict time protocol says that the year must be given as two digits.
;;; What happens in year 2000?

(DEFUN PRINT-DIRECTORY-DATE-PROPERTY (UT STREAM)
  (if (numberp ut)      ;"defensive"
      (MULTIPLE-VALUE-BIND (SEC MIN HR DAY MON YR)
          (TIME:DECODE-UNIVERSAL-TIME UT)
        (FORMAT STREAM "~2,'0D//~2,'0D//~2,'0D ~2,'0D:~2,'0D:~2,'0D"
                MON DAY (MOD YR 100.) HR MIN SEC))))

(DEFUN PARSE-DIRECTORY-BOOLEAN-PROPERTY (STRING START)
  (LET ((TEM (READ-FROM-STRING STRING NIL START)))
    (IF (EQ TEM ':NIL) NIL TEM)))

(DEFUN PARSE-SETTABLE-PROPERTIES (STRING START)
  (IF (SETQ START (STRING-SEARCH-NOT-CHAR #/SPACE STRING START))
      (DO ((I START (1+ J))
           (J)
           (LIST NIL))
          (NIL)
        (SETQ J (STRING-SEARCH-CHAR #/SPACE STRING I))
        (PUSH (INTERN (STRING-UPCASE (SUBSTRING STRING I J)) "") LIST)
        (OR J (RETURN (NREVERSE LIST))))
      T))                                       ;Treat like blank line

(DEFUN PRINT-SETTABLE-PROPERTIES (PROPERTIES &OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*))
  (AND (LISTP PROPERTIES)
       (DO ((TAIL PROPERTIES (CDR TAIL))) ((NULL TAIL))
         (PRINC (CAR TAIL))
         (IF (CDR TAIL) (TYO #/SPACE)))))

(DEFUN PARSE-DIRECTORY-FREE-SPACE (STRING START &AUX LIST)
  (DO ((I START (1+ I))
       (J)
       (VOL))
      (NIL)
    (OR (SETQ J (STRING-SEARCH-CHAR #/: STRING I))
        (RETURN))
    (SETQ VOL (SUBSTRING STRING I J))
    (SETQ I (STRING-SEARCH-CHAR #/, STRING (SETQ J (1+ J))))
    (PUSH (CONS VOL (ZWEI:PARSE-NUMBER STRING J I)) LIST)
    (OR I (RETURN)))
  (NREVERSE LIST))

(DEFUN PRINT-DIRECTORY-FREE-SPACE (ALIST &OPTIONAL (*STANDARD-OUTPUT* *STANDARD-OUTPUT*))
  (DO ((TAIL ALIST (CDR TAIL)))
      ((NULL TAIL))
    (PRINC (CAAR TAIL))
    (PRINC ":")
    (PRINC (CDAR TAIL))
    (IF (CDDR TAIL) (PRINC ","))))

(DEFUN PRINT-DECIMAL-PROPERTY (PROP STREAM)
  (LET ((*PRINT-BASE* 10.)
        (*NOPOINT T)
        (*PRINT-RADIX* NIL)
        (*READTABLE* SI:INITIAL-READTABLE))
    (PRIN1 PROP STREAM)))

;;; List all directories w.r.t. the pathname.  The only option currently defined
;;; is :NOERROR, which causes the function to return a string rather than an error.
;;; A successful return returns a plist, as in :DIRECTORY-LIST, of pathnames with
;;; one for each directory.  Currently the only non-nil fields in these pathnames
;;; are host, directory, and device, but this may be changed later on by some options.
;;; Also, there are no properties defined yet.

;;; First argument may be a host name for convenience
(DEFUN ALL-DIRECTORIES (&OPTIONAL (PATHNAME USER-LOGIN-MACHINE) &REST OPTIONS &AUX TEM)
  "Return a list of pathnames describing all directories on a specified host.
The argument is either a host, a hostname, or a pathname or namestring
whose host is used.  The only option is :NOERROR."
  (FORCE-USER-TO-LOGIN)
  (IF (AND (TYPEP PATHNAME '(OR STRING SI:HOST))
           (SETQ TEM (GET-PATHNAME-HOST PATHNAME T)))
      (SETQ PATHNAME (SEND (SAMPLE-PATHNAME TEM) ':NEW-PATHNAME
                           ':DEVICE ':WILD ':DIRECTORY ':WILD))
    (SETQ PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME)))
  (SEND PATHNAME ':ALL-DIRECTORIES OPTIONS))

;;; Default is to complain that it can't be done.
(DEFMETHOD (PATHNAME :ALL-DIRECTORIES) (OPTIONS)
  (LET ((ERROR (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't list all directories on file system of ~A."
                               SELF ':ALL-DIRECTORIES)))
    (IF (MEMQ ':NOERROR OPTIONS) ERROR (SIGNAL ERROR))))

(DEFMETHOD (MEANINGFUL-ROOT-MIXIN :ALL-DIRECTORIES) (OPTIONS)
  (LOOP FOR FILE IN (CDR (APPLY #'DIRECTORY-LIST
                                (SEND SELF ':NEW-PATHNAME
                                           ':DIRECTORY ':ROOT
                                           ':NAME ':WILD
                                           ':TYPE ':WILD
                                           ':VERSION ':WILD)
                                OPTIONS))
        WHEN (GET FILE ':DIRECTORY)
        COLLECT (NCONS (SEND (CAR FILE) ':PATHNAME-AS-DIRECTORY))))

(DEFUN COMPLETE-PATHNAME (DEFAULTS STRING TYPE VERSION &REST OPTIONS &AUX PATHNAME)
  "Attempt to complete the filename STRING, returning the results.
DEFAULTS, TYPE and VERSION are as in MERGE-PATHNAME-DEFAULTS.
OPTIONS are :DELETED, :READ (file is for input), :WRITE (it's for output),
  :OLD (only existing files allowed), :NEW-OK (new files are allowed too).
There are two values: a string which is the completion as far as possible,
and SUCCESS, which can be :OLD, :NEW or NIL.
:OLD says that the returned string names an existing file,
:NEW says that the returned string is no file but some completion was done,
NIL says that no completion was possible."
  (DECLARE (VALUES STRING SUCCESS))
  (FORCE-USER-TO-LOGIN)
  (MULTIPLE-VALUE-BIND (HOST START END)
      (PARSE-PATHNAME-FIND-COLON STRING)
    (AND HOST (SETQ START (OR (STRING-SEARCH-NOT-CHAR #/SPACE STRING START END) END)
                    STRING (SUBSTRING STRING START END)))
    (SETQ PATHNAME (DEFAULT-PATHNAME DEFAULTS HOST TYPE VERSION T)))
  (SEND PATHNAME ':COMPLETE-STRING STRING OPTIONS))

(defun pathname-completion-list (defaults string type version &rest options
                                 &AUX pathname whole-directory directory-pathname
                                 directory-list)
  "Returns a list of possible completions of string."
  type version
  (declare (values string success))
  (force-user-to-login)
  (setq pathname (fs:merge-pathname-defaults string defaults ':WILD ':WILD))
  ;; now get a pathname with just the first word of the name and a magic
  ;; character that signifies wild card match at end of name.
  (let ((name (send pathname ':NAME)))
    (if (or (null name) (equalp name "")
            (eq ':WILD name))
        (setq whole-directory T
              directory-pathname
              (send pathname ':NEW-PATHNAME ':NAME ':WILD ':VERSION ':WILD))
      ;; otherwise clip off whatever is necessary from the name
      (let ((position (string-search-set '(#/SPACE #/. #/-) name)))
        (if position (setq name (substring name 0 position))))
      (setq name (string-append name "*"))
      (let ((temp-pathname (fs:parse-pathname name (send pathname ':HOST))))
        (setq directory-pathname (send pathname
                                     ':NEW-PATHNAME
                                     ':NAME (send temp-pathname ':NAME) ':VERSION ':WILD)))))
  ;; now get directory-list
  (setq directory-list (send directory-pathname ':DIRECTORY-LIST options))
  (if (errorp directory-list)
      directory-list
    (setq directory-list (cdr directory-list))
    ;; maybe do more completion
    (if (null whole-directory)
        (multiple-value-bind (NIL matching-subset)
            (zwei:complete-string (send pathname ':NAME)
                                  (LOOP FOR entry IN directory-list
                                        AS pathname = (car entry)
                                        AS name = (send pathname ':NAME)
                                        COLLECT (list name pathname))
                                  '(#/. #/- #/SPACE))
          (setq directory-list
                (LOOP FOR (pathname-name pathname) IN matching-subset
                      COLLECT pathname)))
      (setq directory-list
            (LOOP FOR entry IN directory-list
                  COLLECT (car entry))))
    directory-list))

;;; Alter properties as returned by DIRECTORY-LIST.  PROPERTIES is a
;;; PLIST with the same indicators as returned by that.
(DEFUN CHANGE-FILE-PROPERTIES (PATHNAME ERROR-P &REST PROPERTIES)
  "Change some file properties of a file, specified as pathname or namestring.
The file properties are those that are returned by FILE-PROPERTIES.
PROPERTIES are the alternating properties and new values."
  (FORCE-USER-TO-LOGIN)
  (SETQ PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME))
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR-P '(:RETRY :REPROMPT))
                              (PATHNAME FILE-ERROR)
    (LEXPR-SEND PATHNAME ':CHANGE-PROPERTIES ERROR-P PROPERTIES)))
(DEFF CHANGE-PATHNAME-PROPERTIES 'CHANGE-FILE-PROPERTIES)       ;Obsolete old name

;;; Find the properties, like those returned by DIRECTORY-LIST, of a single file.
;;; Returns a plist whose car is the truename and whose cdr is the properties.
(DEFUN FILE-PROPERTIES (PATHNAME &OPTIONAL (ERROR-P T))
  "Return the property list of a file, specified as a pathname or namestring.
These properties are the same ones that appear in a directory-list.
The car of the value is the truename.
The second value is a list of properties whose values can be changed."
  (DECLARE (VALUES PROPERTIES SETTABLE-PROPERTIES))
  (FORCE-USER-TO-LOGIN)
  (SETQ PATHNAME (MERGE-PATHNAME-DEFAULTS PATHNAME))
  (MULTIPLE-VALUE-BIND (PLIST SETTABLE-PROPERTIES)
      (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR-P '(:RETRY :REPROMPT))
                                  (PATHNAME FILE-ERROR)
        (SEND PATHNAME ':PROPERTIES ERROR-P))
    (OR SETTABLE-PROPERTIES
        (IF (SEND PATHNAME ':OPERATION-HANDLED-P ':PROPERTY-SETTABLE-P)
            (SETQ SETTABLE-PROPERTIES
                  (UNION (SEND PATHNAME ':DEFAULT-SETTABLE-PROPERTIES)
                         (LOOP FOR IND IN (CDR PLIST) BY 'CDDR
                               WHEN (SEND PATHNAME ':PROPERTY-SETTABLE-P IND)
                                  COLLECT IND)))))
    (VALUES PLIST SETTABLE-PROPERTIES)))

(DEFUN EXPUNGE-DIRECTORY (PATHNAME &REST OPTIONS &KEY (ERROR T))
  "Expunge all deleted files in the directory specified in PATHNAME.
PATHNAME can be a pathname or a namestring."
  (DECLARE (VALUES BLOCKS-FREED))
  (FORCE-USER-TO-LOGIN)
  ;; avoid merge-pathname-defaults braindeath
  (OR (SEND (SETQ PATHNAME (FS:PARSE-PATHNAME PATHNAME)) :NAME)
      (SETQ PATHNAME (SEND PATHNAME :NEW-NAME :WILD)))
  (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS PATHNAME NIL :WILD :WILD))
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
                              (PATHNAME FILE-ERROR)
    (LEXPR-SEND PATHNAME :EXPUNGE OPTIONS)))

(DEFMETHOD (PATHNAME :EXPUNGE) (&REST ARGS)
  (LET ((ERROR (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't expunge or undelete on file system of ~A."
                               SELF ':EXPUNGE))
        (ERRORP (COND ((NULL ARGS) T)
                      ((NULL (CDR ARGS))
                       (CAR ARGS))
                      (T (GET (LOCF ARGS) ':ERROR)))))
    (IF ERRORP (SIGNAL ERROR) ERROR)))

(DEFUN REMOTE-CONNECT (PATHNAME &REST OPTIONS &KEY (ERROR T) ACCESS)
  "Tell file servers to connect or access to a directory.
PATHNAME, either a pathname or a namestring, specifies both the host
and the device and directory to connect or access to."
  ERROR ACCESS
  (FORCE-USER-TO-LOGIN)
  (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
                              (PATHNAME FILE-ERROR)
    (LEXPR-SEND (MERGE-PATHNAME-DEFAULTS PATHNAME NIL) ':REMOTE-CONNECT OPTIONS)))

(DEFMETHOD (PATHNAME :REMOTE-CONNECT) (&KEY ERROR ACCESS)
  ACCESS
  (LET ((CONDITION
          (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't do remote connect or access on file system of ~A."
                          SELF ':REMOTE-CONNECT)))
    (IF ERROR (SIGNAL CONDITION) CONDITION)))

(DEFUN ENABLE-CAPABILITIES (HOST &REST CAPABILITIES)
  "Tell file servers on HOST to enable some capabilities.
Defaults are according to operating system."
  (FORCE-USER-TO-LOGIN)
  (LEXPR-SEND (GET-PATHNAME-HOST HOST) ':ENABLE-CAPABILITIES CAPABILITIES))

(DEFUN DISABLE-CAPABILITIES (HOST &REST CAPABILITIES)
  "Tell file servers on HOST to disable some capabilities.
Defaults are according to operating system."
  (FORCE-USER-TO-LOGIN)
  (LEXPR-SEND (GET-PATHNAME-HOST HOST) ':DISABLE-CAPABILITIES CAPABILITIES))

(DEFUN CREATE-DIRECTORY (PATHNAME &KEY (ERROR T) RECURSIVE)
  "Create a directory specified in PATHNAME, either a pathname or a namestring.
RECURSIVE non-NIL says if this directory is supposed to be a subdirectory
of another directory which also fails to exist, create that one too, etc."
  (FORCE-USER-TO-LOGIN)
  (LET ((PN (MERGE-PATHNAME-DEFAULTS PATHNAME NIL)))
    (CONDITION-CASE-IF RECURSIVE ()
        (FILE-RETRY-NEW-PATHNAME-IF (MEMQ ERROR '(:RETRY :REPROMPT))
                                    (PATHNAME FILE-ERROR)
          (SEND PN :CREATE-DIRECTORY :ERROR ERROR))
      (DIRECTORY-NOT-FOUND
       (CREATE-DIRECTORY (SEND PN :DIRECTORY-PATHNAME-AS-FILE) :RECURSIVE T :error error)
       (CREATE-DIRECTORY PN :ERROR ERROR)))))

(DEFMETHOD (PATHNAME :CREATE-LINK) (LINK-TO &KEY (ERROR T))
  (DECLARE (IGNORE LINK-TO))
  (LET ((CONDITION
          (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't create links on file system of ~A."
                          SELF ':CREATE-LINK)))
    (IF ERROR (SIGNAL CONDITION) CONDITION)))


(DEFMETHOD (PATHNAME :CREATE-DIRECTORY) (&KEY (ERROR T))
  (LET ((CONDITION
          (MAKE-CONDITION 'UNKNOWN-OPERATION "Can't create directory on file system of ~A."
                          SELF :CREATE-DIRECTORY)))
    (IF ERROR (SIGNAL CONDITION) CONDITION)))


;;;; Handle File attribute lists

;;; If no :MODE property is in the file's -*- line, and the file type is on this
;;; list, then the corresponding :MODE property is put on the file's plist.
;;; This helps losers who don't have -*- lines get the right mode on TWENEX, etc.
(DEFCONST *FILE-TYPE-MODE-ALIST*
          '((:LISP . :LISP)
            (:TEXT . :TEXT)
            (:MIDAS . :MIDAS)
            (:DOC . :TEXT)
            (:MSS . :SCRIBE)
            (:PALX . :TEXT)
            (:MAC . :MIDAS)
            (:TASM . :MIDAS)
            (:C . :PL1)
            (:CLU . :PL1)
            (:PL1 . :PL1)
            (:scheme . :scheme))
  "Alist mapping standard pathname type component strings into editor major mode name keywords.")

;;; New faster parser, uses :READ-INPUT-BUFFER, returns the new property list.
;;; Works for multiple-line plists.
;;; Beware of making streams do :LINE-INs on files which aren't really ASCII.
;;; :LINE-IN can lose rather badly on such files.
;;; PATHNAME may be NIL if you don't want any properties put on any pathname.
;;; But it is better to call FILE-EXTRACT-ATTRIBUTE-LIST if you want that.
(DEFF READ-SYNTAX-PLIST 'READ-ATTRIBUTE-LIST)
(DEFF FILE-READ-PROPERTY-LIST 'READ-ATTRIBUTE-LIST)
(DEFF FILE-READ-ATTRIBUTE-LIST 'READ-ATTRIBUTE-LIST)
(DEFUN READ-ATTRIBUTE-LIST (PATHNAME STREAM &AUX PLIST)
  "Return the attribute list read from STREAM, and put properties on PATHNAME.
STREAM can be reading either a text file or a QFASL file.
PATHNAME should be the generic pathname that was opened."
  (SETQ PLIST (FILE-EXTRACT-ATTRIBUTE-LIST STREAM))
  ;; First remove any properties that we put on the last time we parsed
  ;; this file's plist, so that we update correctly if some property has been deleted.
  (AND PATHNAME
       (LET ((OLD-PLIST (SEND PATHNAME ':GET 'LAST-FILE-PLIST)))
         (DO ((L OLD-PLIST (CDDR L))) ((NULL L))
           (SEND PATHNAME ':REMPROP (CAR L)))))
  ;; Now put on the properties desired this time.
  (AND PATHNAME
       (DO ((L PLIST (CDDR L)))
           ((NULL L))
         (SEND PATHNAME ':PUTPROP (SECOND L) (FIRST L))))
  ;; Record the entire plist, so we can update properly next time.
  (AND PATHNAME
       (SEND PATHNAME ':PUTPROP PLIST 'LAST-FILE-PLIST))
  PLIST)

;Return the property list from a stream, but don't alter any pathname's plist.
(DEFF FILE-EXTRACT-PROPERTY-LIST 'EXTRACT-ATTRIBUTE-LIST)
(DEFF EXTRACT-PROPERTY-LIST 'EXTRACT-ATTRIBUTE-LIST)
(DEFF FILE-EXTRACT-ATTRIBUTE-LIST 'EXTRACT-ATTRIBUTE-LIST)
(DEFUN EXTRACT-ATTRIBUTE-LIST (STREAM &AUX WO PLIST PATH MODE ERROR)
  "Return the attribute list read from STREAM.
STREAM can be reading either a text file or a QFASL file."
  (declare (values plist error))
  (SETQ WO (SEND STREAM ':WHICH-OPERATIONS))
  (COND ((MEMQ ':SYNTAX-PLIST WO)
         (SETQ PLIST (SEND STREAM ':SYNTAX-PLIST)))
        ((NOT (SEND STREAM ':CHARACTERS))
         (SETQ PLIST (SI:QFASL-STREAM-PROPERTY-LIST STREAM)))
        ;; If the file supports :READ-INPUT-BUFFER, check for absence of a plist
        ;; without risk that :LINE-IN will read the whole file
        ;; if the file contains no Return characters.
        ((AND (MEMQ ':READ-INPUT-BUFFER WO)
              (MULTIPLE-VALUE-BIND (BUFFER START END)
                  (SEND STREAM ':READ-INPUT-BUFFER)
                (AND BUFFER
                     (NOT (STRING-SEARCH "-*-" BUFFER START END)))))
         NIL)
        ;; If stream does not support :SET-POINTER, there is no hope
        ;; of parsing a plist, so give up on it.
        ((NOT (MEMQ ':SET-POINTER WO))
         NIL)
        (T (DO ((LINE) (EOF)) (NIL)
             (MULTIPLE-VALUE (LINE EOF) (SEND STREAM ':LINE-IN NIL))
             (COND ((NULL LINE)
                    (SEND STREAM ':SET-POINTER 0)
                    (RETURN NIL))
                   ((STRING-SEARCH "-*-" LINE)
                    (SETQ LINE (FILE-GRAB-WHOLE-PROPERTY-LIST LINE STREAM))
                    (SEND STREAM ':SET-POINTER 0)
                    (SETF (VALUES PLIST ERROR) (FILE-PARSE-PROPERTY-LIST LINE))
                    (RETURN NIL))
                   ((OR EOF (STRING-SEARCH-NOT-SET '(#/SPACE #/TAB) LINE))
                    (SEND STREAM ':SET-POINTER 0)
                    (RETURN NIL))))))
  ;;
  ;;From here on, infer properties where possible.
  ;;
  ;;Infer: Iff no MODE, try to get from pathname type
  (AND (NOT (GETF PLIST ':MODE))
       (MEMQ ':PATHNAME WO)
       (SETQ PATH (SEND STREAM ':PATHNAME))
       (SETQ MODE (CDR (ASSOC (SEND PATH ':TYPE) *FILE-TYPE-MODE-ALIST*)))
       (PUTPROP (LOCF PLIST) MODE ':MODE))
  ;;Infer: Iff MODE or SYNTAX is CommonLISP or equivalent, set READTABLE
  ;;  (this assumes ZL is still "traditional" and default!)
  (when (and (null (getf plist :readtable))
             (or (member (getf plist :mode) '(:commonlisp :common-lisp))
                 (eq (getf plist :syntax) :CL)))
    (putprop (locf plist) :CL :readtable))
  ;;
  ;;Finally return PLIST and any error from along the way.
  ;;
  (VALUES PLIST ERROR))

;Given a line on which a file's property list starts,
;read through the file appending onto the line until we get to
;the end of the property list.
;So we return a string that contains the whole thing.
(DEFUN FILE-GRAB-WHOLE-PROPERTY-LIST (STARTING-LINE STREAM)
  (DO ((START (+ 3 (STRING-SEARCH "-*-" STARTING-LINE)) 0)
       (STRING STARTING-LINE (SEND STREAM ':LINE-IN))
       (COUNT 0 (1+ COUNT))
       (ACCUM (MAKE-ARRAY 0 ':TYPE ART-STRING ':FILL-POINTER 0)))
      ((= COUNT 100.)
       (FORMAT *QUERY-IO* "~&The file ~A has an unterminated property list line."
               (SEND STREAM ':PATHNAME))
       "")
    (SETQ ACCUM (STRING-NCONC ACCUM #/NEWLINE STRING))
    (IF (STRING-SEARCH "-*-" STRING START)
        (RETURN ACCUM))))

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:47:00
;;; This takes a string which probably has a property list in it, and returns the plist.
;;; If it has any trouble parsing, returns whatever plist it could find.
(DEFUN FILE-PARSE-PROPERTY-LIST (STRING &OPTIONAL (START 0) (END (LENGTH STRING))
                                 &AUX PLIST ERROR
                                 (*READ-BASE* 10.)
                                 (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
                                 (*READTABLE* SI:INITIAL-COMMON-LISP-READTABLE))

  (AND STRING
       (ARRAYP STRING)
       (= (ARRAY-ELEMENT-SIZE STRING) 8)
       ;; Narrow down to the stuff between the -*-'s
       (SETQ START (STRING-SEARCH "-*-" STRING START END))
       (SETQ END (STRING-SEARCH "-*-" STRING (SETQ START (+ START 3)) END))
       ;; Now parse it.
       (IF (NOT (%STRING-SEARCH-CHAR #/: STRING START END))
           (SETQ PLIST (LIST ':MODE (READ-FROM-SUBSTRING STRING START END)))
         (DO ((S START (1+ SEMI-IDX))
              (COLON-IDX) (SEMI-IDX) (SYM) (ELEMENT NIL NIL) (DONE)
              (WIN-THIS-TIME NIL NIL))
             (NIL)
           (OR (SETQ SEMI-IDX (%STRING-SEARCH-CHAR #/; STRING S END))
               (SETQ DONE T SEMI-IDX END))
           (OR (SETQ COLON-IDX (%STRING-SEARCH-CHAR #/: STRING S SEMI-IDX))
               (RETURN NIL))
           (IGNORE-ERRORS
             (OR (SETQ SYM (READ-FROM-SUBSTRING STRING S COLON-IDX))
                 (RETURN NIL))
             (IGNORE-ERRORS
               (IF (%STRING-SEARCH-CHAR #/, STRING (SETQ S (1+ COLON-IDX)) SEMI-IDX)
                   (DO ((COMMA-IDX) (ELEMENT-DONE))
                       (NIL)
                     (OR (SETQ COMMA-IDX (%STRING-SEARCH-CHAR #/, STRING S SEMI-IDX))
                         (SETQ ELEMENT-DONE T COMMA-IDX SEMI-IDX))
                     (SETQ ELEMENT
                           (NCONC ELEMENT
                                  (NCONS (LET ((TEM (READ-FROM-SUBSTRING STRING S COMMA-IDX)))
                                           (setq tem (nsubst nil :nil tem))))))
                     (AND ELEMENT-DONE (RETURN NIL))
                     (SETQ S (1+ COMMA-IDX)))
                 (SETQ ELEMENT (LET ((TEM (READ-FROM-SUBSTRING STRING S SEMI-IDX)))
                                 (setq tem (nsubst nil :nil tem)))))
               (SETQ WIN-THIS-TIME T))
             (SETQ PLIST (NCONC PLIST (LIST* SYM ELEMENT NIL))))        ;Nicer CDR-CODEs
           (UNLESS WIN-THIS-TIME
             (SETQ ERROR T))
           (AND DONE (RETURN NIL)))))
  (VALUES PLIST ERROR))

(DEFUN READ-FROM-SUBSTRING (STRING &OPTIONAL (START 0) (END (LENGTH STRING)))
  (READ-FROM-STRING STRING NIL START END))

(DEFF FILE-PROPERTY-BINDINGS 'FILE-ATTRIBUTE-BINDINGS)

(DEFUN (:SYNTAX FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (NCONS '*READTABLE*) (NCONS (SI:FIND-READTABLE-NAMED VAL :ERROR))))

(DEFUN FILE-ATTRIBUTE-BINDINGS (PATHNAME)
  "Return bindings to be made according to the attribute list of PATHNAME.
READ-ATTRIBUTE-LIST should have been done previously on that pathname.
Returns two values, a list of special variables and a list of values to bind them to.
Use the two values in a PROGV if you READ expressions from the file."
  (ATTRIBUTE-BINDINGS-FROM-LIST
    (IF (LOCATIVEP PATHNAME) (CONTENTS PATHNAME) (SEND PATHNAME :PROPERTY-LIST))
    PATHNAME))

(DEFUN ATTRIBUTE-BINDINGS-FROM-LIST (ATTLIST PATHNAME)
  (DO ((ATTLIST ATTLIST (CDDR ATTLIST))
       (VARS NIL)
       (VALS NIL)
       (BINDING-FUNCTION))
      ((NULL ATTLIST)
       (VALUES VARS VALS))
    (AND (SETQ BINDING-FUNCTION (GET (CAR ATTLIST) 'FILE-ATTRIBUTE-BINDINGS))
         (MULTIPLE-VALUE-BIND (VARS1 VALS1)
             (FUNCALL BINDING-FUNCTION PATHNAME (CAR ATTLIST) (CADR ATTLIST))
           (SETQ VARS (NCONC VARS1 VARS)
                 VALS (NCONC VALS1 VALS))))))

(DEFUN EXTRACT-ATTRIBUTE-BINDINGS (STREAM)
  "Return a list of variables and values corresponding to STREAM's attribute list.
Useful for arguments to the PROGV special form."
  (ATTRIBUTE-BINDINGS-FROM-LIST
    (EXTRACT-ATTRIBUTE-LIST STREAM)
    (OR (SEND STREAM ':SEND-IF-HANDLES ':TRUENAME) ':NO-PATHNAME))) ; Best one can do

(DEFUN (:PACKAGE FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE PKG)
  (VALUES (NCONS '*PACKAGE*) (NCONS (PKG-FIND-PACKAGE PKG :ERROR *package*))))

(defun (:compile-in-roots file-attribute-bindings) (ignore ignore roots-to-compile-in)
  (values (ncons 'si:roots-to-compile-in)
          (ncons  roots-to-compile-in)))

(DEFUN (:BASE FILE-ATTRIBUTE-BINDINGS) (FILE IGNORE BSE)
  (UNLESS (TYPEP BSE '(INTEGER 1 36.))
    (FERROR 'INVALID-FILE-ATTRIBUTE "File ~A has an illegal -*- BASE:~*~S -*-"
            FILE ':BASE BSE))
  (VALUES (LIST* '*READ-BASE* '*PRINT-BASE* NIL) (LIST* BSE BSE NIL)))

(DEFUN (:COLD-LOAD FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE FLAG)
  (VALUES (NCONS 'SI:FILE-IN-COLD-LOAD) (NCONS FLAG)))

;;; So that functions can tell if they are being loaded out of, or compiled in, a patch file
(DEFVAR-RESETTABLE THIS-IS-A-PATCH-FILE NIL NIL
  "Non-NIL while loading a patch file.")
(DEFUN (:PATCH-FILE FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (NCONS 'THIS-IS-A-PATCH-FILE) (NCONS VAL)))

;(DEFUN (:COMMON-LISP FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
;  (VALUES (LIST* '*READTABLE* 'SI:*READER-SYMBOL-SUBSTITUTIONS*
;                'SI:INTERPRETER-FUNCTION-ENVIRONMENT '*NOPOINT
;                NIL)
;         (LIST* (IF VAL SI:COMMON-LISP-READTABLE SI:STANDARD-READTABLE)
;                (IF VAL SI:*COMMON-LISP-SYMBOL-SUBSTITUTIONS* NIL)
;                NIL T
;                NIL)))

(DEFUN (:READTABLE FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE VAL)
  (VALUES (NCONS '*READTABLE*) (NCONS (SI:FIND-READTABLE-NAMED VAL :ERROR))))

(DEFUN (:FONTS FILE-ATTRIBUTE-BINDINGS) (IGNORE IGNORE IGNORE)
  (VALUES (NCONS 'SI:READ-DISCARD-FONT-CHANGES) (NCONS T)))

(DEFVAR *NAMED-READER-ALIST* '((:T ZL:READ)(NIL ZL:READ))
  "An alist of (:keyword <function>) for names of readers in the file mode line
Note these take args like ZL:READ")

(DEFUN (:READ FILE-ATTRIBUTE-BINDINGS) (FILE IGNORE VAL)
  (VALUES (NCONS 'SI:*READFILE-READ-FUNCTION*)
          (NCONS (OR (CADR (ASSQ VAL *NAMED-READER-ALIST*))
                     (FERROR 'INVALID-FILE-ATTRIBUTE "File ~A has an illegal -*- READ:~*~S -*-"
                             FILE :READ VAL)))))

;;; This returns the -*- properties for a ascii file, the qfasl properties for a qfasl file
(DEFF PATHNAME-SYNTAX-PLIST 'FILE-ATTRIBUTE-LIST)
(DEFF PATHNAME-ATTRIBUTE-LIST 'FILE-ATTRIBUTE-LIST)
(DEFF FILE-PROPERTY-LIST 'FILE-ATTRIBUTE-LIST)
(DEFUN FILE-ATTRIBUTE-LIST (PATHNAME)
  "Return the attribute list for the file specified by PATHNAME, a pathname or namestring."
  (WITH-OPEN-FILE (STREAM PATHNAME ':CHARACTERS ':DEFAULT)
    (COND ((SEND STREAM ':SEND-IF-HANDLES ':FILE-PLIST))
          ((SEND STREAM ':CHARACTERS)
           (EXTRACT-ATTRIBUTE-LIST STREAM))
          (T
           (SI:QFASL-STREAM-PROPERTY-LIST STREAM)))))

(DEFVAR LOAD-PATHNAME-DEFAULTS :UNBOUND
  "Now the same as *default-pathname-defaults*
Used to be used as the pathname-defaults list for LOAD, COMPILE-FILE, etc.")
(FORWARD-VALUE-CELL 'LOAD-PATHNAME-DEFAULTS '*DEFAULT-PATHNAME-DEFAULTS*)

(DEFVAR *LOAD-VERBOSE* T
  "Non-NIL means LOAD can print a message saying what it is loading.
Can be overridden by the :VERBOSE keyword when LOAD is called.")

(DEFVAR *LOAD-SET-DEFAULT-PATHNAME* T
  "Non-NIL means LOAD sets the default pathname to the name of the file loaded.
Can be overridden by the :SET-DEFAULT-PATHNAME keyword when LOAD is called.")

(PROCLAIM '(SPECIAL SI::PRINT-LOADED-FORMS))    ;in sys; qfasl

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:47:02
(DEFUN LOAD (FILE &REST KEY-OR-POSITIONAL-ARGS)
  "Load the specified text file or QFASL file or input stream.
If the specified filename has no type field, we try QFASL and then LISP and then INIT.
Regardless of the filename type, we can tell QFASL files from text files.
PACKAGE specifies the package to load into; if missing or NIL,
 the package specified by the file's attribute list is used.
VERBOSE non-NIL says it's ok to print a message saying what is being loaded.
 Default comes from *LOAD-VERBOSE*, normally T.
SET-DEFAULT-PATHNAME non-NIL says set the default pathname for LOAD
 to the name of this file.  Default from *LOAD-SET-DEFAULT-PATHNAME*, normally T.
IF-DOES-NOT-EXIST NIL says just return NIL for file-not-found.  Default T.
 In all other cases the value is the truename of the loaded file, or T.
PRINT non-NIL says print all forms loaded."
  (DECLARE (ARGLIST FILE &KEY PACKAGE
                              (VERBOSE *LOAD-VERBOSE*)
                              (SET-DEFAULT-PATHNAME *LOAD-SET-DEFAULT-PATHNAME*)
                              (IF-DOES-NOT-EXIST T)
                              PRINT))
  ;;This used to be enforced by SI:LOAD-PATCH-FILE in a horrible kludge.
  ;;Now, it is merely a recommendation.
  (when fs:this-is-a-patch-file
    (cerror "Proceed to LOAD ~S anyway."
            "The use of LOAD in a patch file is not recommended."
            file))
  (IF (AND (CAR KEY-OR-POSITIONAL-ARGS)
           (MEMQ (CAR KEY-OR-POSITIONAL-ARGS)
                 '(:PACKAGE :PRINT :IF-DOES-NOT-EXIST :SET-DEFAULT-PATHNAME :VERBOSE)))
      (LET ((SI::PRINT-LOADED-FORMS (GETF KEY-OR-POSITIONAL-ARGS ':PRINT)))
        (LOAD-1 FILE
                (GETF KEY-OR-POSITIONAL-ARGS ':PACKAGE)
                (NOT (GETF KEY-OR-POSITIONAL-ARGS ':IF-DOES-NOT-EXIST T))
                (NOT (GETF KEY-OR-POSITIONAL-ARGS ':SET-DEFAULT-PATHNAME
                           *LOAD-SET-DEFAULT-PATHNAME*))
                (NOT (GETF KEY-OR-POSITIONAL-ARGS ':VERBOSE *LOAD-VERBOSE*))))
    (APPLY #'LOAD-1 FILE KEY-OR-POSITIONAL-ARGS)))

;; Copied from LAD: RELEASE-3.IO.FILE; OPEN.LISP#208 on 2-Oct-86 05:47:03
(DEFUN LOAD-1 (FILE &OPTIONAL PKG NONEXISTENT-OK-FLAG DONT-SET-DEFAULT-P NO-MSG-P)
  (IF (STREAMP FILE)
      (PROGN
        ;; Set the defaults from the pathname we finally opened
        (OR DONT-SET-DEFAULT-P
            (SET-DEFAULT-PATHNAME (SEND FILE :PATHNAME) LOAD-PATHNAME-DEFAULTS))
        (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on loading ~A."
                                                    (SEND FILE :PATHNAME))
          (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry loading ~A." (SEND FILE :PATHNAME))
            ;; If the file was a character file, read it, else try to fasload it.
            (FUNCALL (IF (SEND FILE :CHARACTERS)
                         #'SI::READFILE-INTERNAL #'SI::FASLOAD-INTERNAL)
                     FILE PKG NO-MSG-P)
            (OR (SEND FILE :SEND-IF-HANDLES :TRUENAME) T))))
    (LET ((PATHNAME (PARSE-PATHNAME FILE)))
      (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on loading ~A." PATHNAME)
        (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry loading ~A." PATHNAME)
          ;; Broken off from due to hairy macrology exceeding frame size!
          (FLET ((KLUDGE ()
                   (CONDITION-CASE-IF NONEXISTENT-OK-FLAG ()
                       (WITH-OPEN-FILE-SEARCH
                                (STREAM ('LOAD LOAD-PATHNAME-DEFAULTS
                                         (NOT NONEXISTENT-OK-FLAG))
                                        (VALUES
                                          (LIST (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE
                                                  PATHNAME)
                                                :LISP :INIT)
                                          PATHNAME)
                                        :CHARACTERS :DEFAULT)
                         ;; Set the defaults from the pathname we finally opened
                         (OR DONT-SET-DEFAULT-P
                             (SET-DEFAULT-PATHNAME (SEND STREAM :PATHNAME)
                                                   LOAD-PATHNAME-DEFAULTS))
                         ;; If the file was a character file, read it, else try to fasload it.
                         (FUNCALL (IF (SEND STREAM :CHARACTERS)
                                      #'SI::READFILE-INTERNAL #'SI::FASLOAD-INTERNAL)
                                  STREAM PKG NO-MSG-P)
                         (SEND STREAM :TRUENAME))
                     (MULTIPLE-FILE-NOT-FOUND
                      NIL))))
            (KLUDGE)))))))

;; Avoid lossage in LOAD-1 before this function is loaded from MAKSYS.
(IF (NOT (FBOUNDP 'SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE))
    (FSET 'SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE
          '(LAMBDA (IGNORE) ':QFASL)))

(DEFUN READFILE (FILE-NAME &OPTIONAL PKG NO-MSG-P)
  "Read and evaluate the expressions from a text file.
PKG specifies the package to read in; if it is NIL,
 the file's attribute list specifies the package.
NO-MSG-P suppresses the message saying that a file is being loaded."
  (WITH-OPEN-STREAM (STREAM (SEND (MERGE-PATHNAME-DEFAULTS
                                    FILE-NAME LOAD-PATHNAME-DEFAULTS NIL)
                                  :OPEN-CANONICAL-DEFAULT-TYPE :LISP
                                  :ERROR :REPROMPT))
    (SET-DEFAULT-PATHNAME (SEND STREAM :PATHNAME) LOAD-PATHNAME-DEFAULTS)
    (SI::READFILE-INTERNAL STREAM PKG NO-MSG-P)))
