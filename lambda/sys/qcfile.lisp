;;; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-
;;; This SYS: SYS; QCFILE
;;;
;;;     ** (c) Copyright 1980, 1984 Massachusetts Institute of Technology **

;;; Compile a LISP source file into a QFASL file.

(DEFVAR QC-FILE-LOAD-FLAG :UNBOUND
  "Holds an arg to QC-FILE which enables a feature that has bugs.
Make the arg NIL.")

(DEFVAR QC-FILE-IN-CORE-FLAG :UNBOUND
  "Holds an argument to QC-FILE which, if non-NIL, causes fasl-updating instead of compilation.")

(DEFVAR-RESETTABLE QC-FILE-IN-PROGRESS NIL NIL
  "T while inside COMPILE-STREAM.")

(DEFVAR-RESETTABLE QC-FILE-READ-IN-PROGRESS NIL NIL
  "T while inside READ within COMPILE-STREAM.")

(DEFPARAMETER QC-FILE-WHACK-THRESHOLD #o40000
  "Generate a new whack in the output QFASL file when fasl table gets this big.")

(DEFVAR QC-FILE-REL-FORMAT NIL
  "T means COMPILE-STREAM writes a REL file.
If QC-FILE-REL-FORMAT-OVERRIDE is NIL (as it initially is),
 the file's attribute list can override this variable.
If the :FASL file attribute's value is :REL, a REL file is made.
If the value is :FASL, a QFASL file is made.
If there is no :FASL attribute, the global value of this variable decides.")

(DEFVAR QC-FILE-RECORD-MACROS-EXPANDED NIL
  "T if within QC-FILE; tells compiler to record macros expanded on QC-FILE-MACROS-EXPANDED.")

(DEFVAR QC-FILE-MACROS-EXPANDED :UNBOUND
  "Within QC-FILE, a list of all macros expanded.
The elements are macro names or lists (macro-name sxhash).")

(DEFCONST QC-FILE-CHECK-INDENTATION T
  "T => check the indentation of input expressions to detect missing closeparens.")

(DEFVAR QC-FILE-REL-FORMAT-OVERRIDE NIL
  "T means ignore the :FASL attribute in the file's attribute list.
The global value of QC-FILE-REL-FORMAT controls the output file format.")

(DEFVAR QC-FILE-FILE-GROUP-SYMBOL :UNBOUND
  "Within COMPILE-STREAM, holds the generic-pathname of the input file.")

(DEFUN QC-FILE-LOAD (&REST QC-FILE-ARGS)
  "Compile a file and then load the binary file."
  (LOAD (APPLY #'QC-FILE QC-FILE-ARGS)))


;;; Compile a source file, producing a QFASL file in the binary format.
;;; If QC-FILE-LOAD-FLAG is T, the stuff in the source file is left defined
;;; as well as written into the QFASL file.  If QC-FILE-IN-CORE-FLAG is T,
;;; then rather than recompiling anything, the definitions currently in core
;;; are written out into the QFASL file.

;;; While a QC-FILE is in progress the default CONS area is sometimes QCOMPILE-TEMPORARY-AREA,
;;; which will be flushed by the end of the next FASL-WHACK.  Note this can happen
;;; in the middle of single QC-FILE, (between function boundaries).
;;; **no** by the end of the QC-FILE or by the start of another.
;;; Therefore, if a breakpoint occurs during a QC-FILE, you must call (QC-FILE-RESET).
;;; This also clears out QC-FILE-IN-PROGRESS, which is T while a QC-FILE is being done.

;;; Note that macros and specials are put on LOCAL-DECLARATIONS to make them temporary.
;;; They are also sent over into the QFASL file.

(DEFUN FASD-UPDATE-FILE (INFILE &OPTIONAL OUTFILE)
  (QC-FILE INFILE OUTFILE NIL T))

;;; This function does all the "outer loop" of the compiler.  It is called
;;; by the editor as well as the compiler.
;;; INPUT-STREAM is what to compile.  GENERIC-PATHNAME is for the corresponding file.
;;; FASD-FLAG is NIL if not making a QFASL file.
;;; PROCESS-FN is called on each form.
;;; QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
;;; FILE-LOCAL-DECLARATIONS is normally initialized to NIL,
;;; but you can optionally pass in an initializations for it.
;;; READ-THEN-PROCESS-FLAG means do all reading first, thus minimizing thrashing.

;*target-computer* is the major switch that allows the whole thing
; to be redirected.
(DEFUN COMPILE-STREAM (INPUT-STREAM GENERIC-PATHNAME FASD-FLAG PROCESS-FN
                       QC-FILE-LOAD-FLAG QC-FILE-IN-CORE-FLAG PACKAGE-SPEC
                       &OPTIONAL ignore IGNORE
                                 COMPILING-WHOLE-FILE-P
                                 (*target-computer* 'lambda-interface))
  "This function does all the /"outer loop/" of the compiler, for file and editor compilation.
 to be compiled are read from INPUT-STREAM.
The caller is responsible for handling any file attributes.
GENERIC-PATHNAME is the file to record information for and use the attributes of.
 It may be NIL if compiling to core.
FASD-FLAG is NIL if not making a QFASL file.
PROCESS-FN is called on each form.
QC-FILE-LOAD-FLAG, QC-FILE-IN-CORE-FLAG, and PACKAGE-SPEC are options.
COMPILING-WHOLE-FILE-P should be T if you are processing all of the file."
  (LET ((*PACKAGE* *PACKAGE*)
        (*READ-BASE* *READ-BASE*)
        (*PRINT-BASE* *PRINT-BASE*)
        FDEFINE-FILE-PATHNAME
        (READ-FUNCTION (IF QC-FILE-CHECK-INDENTATION 'READ-CHECK-INDENTATION 'ZL:READ)))
    (FILE-OPERATION-WITH-WARNINGS (GENERIC-PATHNAME ':COMPILE COMPILING-WHOLE-FILE-P)
      (COMPILER-WARNINGS-CONTEXT-BIND
        ;; Override the package if required.  It has been bound in any case.
        (AND PACKAGE-SPEC (SETQ *PACKAGE* (PKG-FIND-PACKAGE PACKAGE-SPEC)))
        ;; Override the generic pathname
        (SETQ FDEFINE-FILE-PATHNAME
              (LET ((PATHNAME (SEND INPUT-STREAM :SEND-IF-HANDLES :PATHNAME)))
                (AND PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))))
        ;; Having bound the variables, process the file.
        (LET ((QC-FILE-IN-PROGRESS T)
              (UNDO-DECLARATIONS-FLAG (or                       ; ||| force if cross compiling - smh 30sep88
                                        (NOT QC-FILE-LOAD-FLAG)
                                        (not (eq *target-computer* *host-computer*))))
              (LOCAL-DECLARATIONS NIL)
              (OPEN-CODE-MAP-SWITCH OPEN-CODE-MAP-SWITCH)
              ;(RUN-IN-MACLISP-SWITCH RUN-IN-MACLISP-SWITCH)
              ;(OBSOLETE-FUNCTION-WARNING-SWITCH OBSOLETE-FUNCTION-WARNING-SWITCH)
              (ALL-SPECIAL-SWITCH ALL-SPECIAL-SWITCH)
              (SOURCE-FILE-UNIQUE-ID)
              (FASD-PACKAGE NIL))
          (WHEN FASD-FLAG
            ;; Copy all suitable file properties into the fasl file
            ;; Suitable means those that are lambda-bound when you read in a file.
            (LET ((PLIST (COPY-LIST (SEND GENERIC-PATHNAME :PROPERTY-LIST))))
              ;; Remove unsuitable properties
              (DO ((L (LOCF PLIST)))
                  ((NULL (CDR L)))
                (IF (NOT (NULL (GET (CADR L) 'FS:FILE-ATTRIBUTE-BINDINGS)))
                    (SETQ L (CDDR L))
                  (SETF (CDR L) (CDDDR L))))
              ;; Make sure the package property is really the package compiled in
              ;; Must load QFASL file into same package compiled in
              ;; On the other hand, if we did not override it
              ;; and the attribute list has a list for the package, write that list.
              (unless (and (consp (getf plist ':package))
                           (null package-spec))
                (setf (getf plist ':package)
                      (intern (package-name *package*) si:pkg-keyword-package)))
              (AND INPUT-STREAM
                   (SETQ SOURCE-FILE-UNIQUE-ID (SEND INPUT-STREAM :SEND-IF-HANDLES :TRUENAME))
                   (SETF (GETF PLIST ':QFASL-SOURCE-FILE-UNIQUE-ID)
                         SOURCE-FILE-UNIQUE-ID))
              ;; If a file is being compiled across directories, remember where the
              ;; source really came from.
              (AND FDEFINE-FILE-PATHNAME FASD-STREAM
                   (LET ((OUTFILE (SEND FASD-STREAM :SEND-IF-HANDLES :PATHNAME)))
                     (WHEN OUTFILE
                       (SETQ OUTFILE (SEND OUTFILE :GENERIC-PATHNAME))
                       (AND (NEQ OUTFILE FDEFINE-FILE-PATHNAME)
                            (SETF (GETF PLIST ':SOURCE-FILE-GENERIC-PATHNAME)
                                  FDEFINE-FILE-PATHNAME)))))
              (MULTIPLE-VALUE-BIND (MAJOR MINOR)
                  (SI:GET-SYSTEM-VERSION "System")
                (SETF (GETF PLIST ':COMPILE-DATA)
                      `(,USER-ID
                        ,SI:LOCAL-PRETTY-HOST-NAME
                        ,(TIME:GET-UNIVERSAL-TIME)
                        ,MAJOR ,MINOR
                        ;; flush this next major release
                        ;;  --- fasload shouldn't even try to load qfasls this old
                        (NEW-DESTINATIONS T     ; NOT :new-destinations!!
                                                ;install this when we want to change FASD-FEF-Q
                                                ;        new-cdr-codes ,(zerop sys:cdr-next)
                         :SITE ,SI:SITE-NAME))))
              ;; First thing in QFASL file must be property list
              ;; These properties wind up on the GENERIC-PATHNAME.
              (COND (QC-FILE-REL-FORMAT
                     (FUNCALL (INTERN (STRING 'DUMP-FILE-PROPERTY-LIST) 'QFASL-REL)
                              GENERIC-PATHNAME
                              PLIST))
                    (T (FASD-FILE-PROPERTY-LIST PLIST)))))
          (QC-PROCESS-INITIALIZE)
          (DO ((EOF (NCONS NIL))
               (FORM))
              (())
            ;; Detect EOF by peeking ahead, and also get an error now
            ;; if the stream is wedged.  We really want to get an error
            ;; in that case, not make a warning.
            (LET ((CH (SEND INPUT-STREAM :TYI)))
              (OR CH (RETURN nil))
              (SEND INPUT-STREAM :UNTYI CH))
            (setq si:premature-warnings
                  (append si:premature-warnings si:premature-warnings-this-object))
            (let ((si:premature-warnings nil))
              (SETQ FORM
                    (LET ((READ-AREA (IF QC-FILE-LOAD-FLAG
                                         DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA))
                          (WARN-ON-ERRORS-STREAM INPUT-STREAM)
                          (QC-FILE-READ-IN-PROGRESS FASD-FLAG)) ;looked at by XR-#,-MACRO
                      (WARN-ON-ERRORS ('READ-ERROR "Error in reading")
                        (FUNCALL (OR SI:*READFILE-READ-FUNCTION* READ-FUNCTION)
                                 INPUT-STREAM EOF))))
              (setq si:premature-warnings-this-object si:premature-warnings))
            (AND (EQ FORM EOF) (RETURN nil))
            ;; Start a new whack if FASD-TABLE is getting too big.
            (AND FASD-FLAG
                 ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
                 (FASD-END-WHACK))
            (WHEN (AND (ATOM FORM) FASD-FLAG)
              (WARN 'ATOM-AT-TOP-LEVEL :IMPLAUSIBLE
                    "The atom ~S appeared at top level; this would do nothing at FASLOAD time."
                    FORM))
            (FUNCALL PROCESS-FN FORM)))))))

(DEFUN PRINT-FUNCTIONS-REFERENCED-BUT-NOT-DEFINED ()
  "Record and print warnings about any functions referenced in compilation but not defined."
  ;; Discard any functions that have since become defined.
  (SETQ FUNCTIONS-REFERENCED
        (DEL-IF (LAMBDA (X) (COMPILATION-DEFINEDP (CAR X))) FUNCTIONS-REFERENCED))
  ;; Record warnings about the callers, saying that they called an undefined function.
  (DOLIST (FREF FUNCTIONS-REFERENCED)
    (DOLIST (CALLER (CDR FREF))
      (OBJECT-OPERATION-WITH-WARNINGS (CALLER NIL T)
        (RECORD-WARNING 'UNDEFINED-FUNCTION-USED :PROBABLE-ERROR NIL
                        "The undefined function ~S was called"
                        (CAR FREF)))))
  ;; Now print messages describing the undefined functions used.
  (WHEN FUNCTIONS-REFERENCED
    (FORMAT *ERROR-OUTPUT*
            "~&The following functions were referenced but don't seem defined:")
    (IF (OPERATION-HANDLED-P *ERROR-OUTPUT* :ITEM)
        (DOLIST (X FUNCTIONS-REFERENCED)
          (FORMAT *ERROR-OUTPUT* "~& ~S referenced by " (CAR X))
          (DO ((L (CDR X) (CDR L))
               (LINEL (OR (SEND-IF-HANDLES *ERROR-OUTPUT* :SIZE-IN-CHARACTERS)
                          72.)))
              ((NULL L))
            (IF (> (+ (SEND *ERROR-OUTPUT* :READ-CURSORPOS :CHARACTER)
                      (FLATSIZE (CAR L))
                      3)
                   LINEL)
                (FORMAT *ERROR-OUTPUT* "~%  "))
            (SEND *ERROR-OUTPUT* :ITEM 'ZWEI::FUNCTION-NAME (CAR L) "~S" (CAR L))
            (AND (CDR L) (PRINC ", " *ERROR-OUTPUT*)))
          (FORMAT *ERROR-OUTPUT* "~&"))
      (DOLIST (X FUNCTIONS-REFERENCED)
        (FORMAT *ERROR-OUTPUT* "~& ~S referenced by " (CAR X))
        (FORMAT:PRINT-LIST *ERROR-OUTPUT* "~S" (CDR X))
        (FRESH-LINE *ERROR-OUTPUT*)))))


(DEFUN COMPILE-FILE (INPUT-FILE
                     &rest args
                     &KEY OUTPUT-FILE
                          (SET-DEFAULT-PATHNAME T)
                          LOAD
                          ((:PACKAGE PACKAGE-SPEC))
                          explicit-compilation-environment)
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the file after compiling it."
  #+(TARGET FALCON)
  (apply #'compile-file-for-falcon input-file args)
  #+(TARGET LAMBDA)
  (LET* ((FILE (FS:MERGE-PATHNAME-DEFAULTS (OR INPUT-FILE "") *DEFAULT-PATHNAME-DEFAULTS*))
         (RESULT (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on compiling ~A." FILE)
                   (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry compiling ~A." FILE)
                     (compile-file-driver file
                                          :output-file output-file
                                          :package-spec package-spec
                                          :explicit-compilation-environment explicit-compilation-environment
                                          :dont-set-default-p (not set-default-pathname))))))
    (AND RESULT LOAD (LOAD RESULT :SET-DEFAULT-PATHNAME NIL))
    RESULT))


(defvar *falcon-environment* (make-compilation-environment :target 'falcon))

(DEFUN COMPILE-FILE-for-falcon (INPUT-FILE &KEY
                                OUTPUT-FILE
                                (SET-DEFAULT-PATHNAME T)
                                LOAD
                                ((:PACKAGE PACKAGE-SPEC))
                                (explicit-compilation-environment nil explicit-c-e-p)
                                (environment-pathname nil env-supplied-p))
  "Compile file INPUT-FILE to a QFASL file named OUTPUT-FILE.
OUTPUT-FILE defaults based on INPUT-FILE, which defaults using the standard defaults.
SET-DEFAULT-PATHNAME if NIL means do not set the defaults.
PACKAGE if non-NIL is the package to compile in.
LOAD means to load the file after compiling it."
  (LET* ((*compilation-environment* (if explicit-c-e-p
                                        explicit-compilation-environment
                                      *falcon-environment*))
         ;; Add this binding to allow c-m from the debugger to do the correct thing.
         ;; ||| JIM 10/19/88
         (eh:*bug-report-recipient-system* "FALCON-COMPILER")
         (env (if explicit-c-e-p
                  *compilation-environment*
                (make-compilation-environment :target 'falcon)))
         (FILE (FS:MERGE-PATHNAME-DEFAULTS (OR INPUT-FILE "") *DEFAULT-PATHNAME-DEFAULTS*))
         (RESULT (CATCH-ERROR-RESTART (EH:DEBUGGER-CONDITION "Give up on compiling ~A." FILE)
                   (ERROR-RESTART (EH:DEBUGGER-CONDITION "Retry compiling ~A." FILE)
                     (compile-file-driver file
                                          :output-file output-file
                                          :package-spec package-spec
                                          :explicit-compilation-environment env
                                          :target-computer 'k
                                          :target-features si:*falcon-features*
                                          :environment-pathname
                                          (if env-supplied-p
                                              environment-pathname
                                            (let ((pathname-source (if output-file      ;not exactly right
                                                                       (FS:MERGE-PATHNAME-DEFAULTS output-file file)
                                                                     file)))
                                              (send pathname-source :new-pathname
                                                    :type :fdef
                                                    :version (IF *QC-FILE-OUTPUT-SAME-VERSION*
                                                                 (SEND (SEND pathname-source :TRUENAME) :VERSION)
                                                               :NEWEST))))
                                          :byte-size 8.
                                          :dont-set-default-p (NOT SET-DEFAULT-PATHNAME))))))
    (AND RESULT LOAD (LOAD RESULT :SET-DEFAULT-PATHNAME NIL))
    RESULT))

;;; New function ||| 28sep88 -smh

(defun LOAD-FDEF-FILE (input-file &key (*compilation-environment* *falcon-environment*))
  (let* ((FILE (FS:MERGE-PATHNAME-DEFAULTS (OR INPUT-FILE "") *DEFAULT-PATHNAME-DEFAULTS*))
         (pathname (send file :new-pathname :type :fdef :version :NEWEST)))
    (load pathname)))

;; Note: Some file servers just cant hack :IF-EXISTS :SUPERSEDE without first deleting the old
;;       QFASL. Therefore if compilation bombs you lose your old qfasl file forever.
;;       Also MAKE-SYSTEM looks at creation dates, not version numbers in any case.
;; -gjc

(DEFVAR *QC-FILE-OUTPUT-SAME-VERSION* T "If T, usual behavior, FOO.LISP#3 => FOO.QFASL#3 if NIL then uses :NEWEST")

(DEFVAR *QC-FILE-OUTPUT-DRIBBLE-TYPE* NIL "If non-nil create a dribble file of this type for the file being compiled")

(DEFUN QC-FILE (INFILE &OPTIONAL OUTFILE LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
                                 explicit-compilation-environment
                                 DONT-SET-DEFAULT-P
                                 READ-THEN-PROCESS-FLAG)
  (compile-file-driver infile
                       :output-file outfile
                       :load-flag load-flag
                       :in-core-flag in-core-flag
                       :package-spec package-spec
                       :explicit-compilation-environment explicit-compilation-environment
                       :dont-set-default-p dont-set-default-p
                       :read-then-process-flag read-then-process-flag))

(defun compile-file-driver (input-file &key output-file load-flag in-core-flag package-spec
                            explicit-compilation-environment dont-set-default-p read-then-process-flag
                            ((:target-computer *target-computer*) 'lambda-interface)
                            ((:target-features si:*target-features*))
                            (environment-pathname)
                            (byte-size 16.)
                       &AUX GENERIC-PATHNAME
                            QC-FILE-MACROS-EXPANDED
                            (QC-FILE-RECORD-MACROS-EXPANDED T)
                            (QC-FILE-REL-FORMAT QC-FILE-REL-FORMAT))
  "Compile Lisp source file INPUT-FILE, producing a binary file called OUTPUT-FILE.
PACKAGE-SPEC specifies which package in which to read the source.
Usually the file's attribute list provides the right default.
LOAD-FLAG and IN-CORE-FLAG are semi-losing features; leave them NIL."
  ;; READ-THEN-PROCESS-FLAG says read the entire file before compiling (less thrashing).
  ;; Default the specified input and output file names.  Open files.
  (SETQ INPUT-FILE (FS:MERGE-PATHNAME-DEFAULTS INPUT-FILE FS:LOAD-PATHNAME-DEFAULTS NIL))
  (WITH-OPEN-STREAM (INPUT-STREAM
                      (FILE-RETRY-NEW-PATHNAME (INPUT-FILE FS:FILE-ERROR)
                        (SEND INPUT-FILE :OPEN-CANONICAL-DEFAULT-TYPE :LISP)))
    ;; The input pathname might have been changed by the user in response to an error.
    ;; Also, find out what type field was actually found.
    (SETQ INPUT-FILE (SEND INPUT-STREAM :PATHNAME))
    (OR DONT-SET-DEFAULT-P (FS:SET-DEFAULT-PATHNAME INPUT-FILE FS:LOAD-PATHNAME-DEFAULTS))
    (SETQ GENERIC-PATHNAME (SEND INPUT-FILE :GENERIC-PATHNAME))
    (SETQ OUTPUT-FILE
          (COND ((TYPEP OUTPUT-FILE 'PATHNAME)
                 (IF (SEND OUTPUT-FILE :VERSION)
                     OUTPUT-FILE
                   (SEND OUTPUT-FILE :NEW-PATHNAME
                                 :VERSION (IF *QC-FILE-OUTPUT-SAME-VERSION*
                                              (SEND (SEND INPUT-STREAM :TRUENAME) :VERSION)
                                            :NEWEST))))
                (OUTPUT-FILE
                 (FS:MERGE-PATHNAME-DEFAULTS
                   OUTPUT-FILE INPUT-FILE
                   (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE GENERIC-PATHNAME)
                   (IF *QC-FILE-OUTPUT-SAME-VERSION*
                       (SEND (SEND INPUT-STREAM :TRUENAME) :VERSION)
                     :NEWEST)))
                (T
                 (SEND INPUT-FILE :NEW-PATHNAME
                              :TYPE (SI:PATHNAME-DEFAULT-BINARY-FILE-TYPE GENERIC-PATHNAME)
                              :VERSION (IF *QC-FILE-OUTPUT-SAME-VERSION*
                                           (SEND (SEND INPUT-STREAM :TRUENAME) :VERSION)
                                         :NEWEST)))))
    ;; Get the file property list again, in case we don't have it already or it changed
    (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME INPUT-STREAM)
    (let ((compile-in-roots-prop (get generic-pathname :compile-in-roots)))
      (cond ((and compile-in-roots-prop
                  (not (cl:member (si:package-root-name (if package-spec package-spec *package*))
                                  compile-in-roots-prop
                                  :test 'string-equal)))
             (ferror "This file is supposed to be compiled only in ~s hierarchies, not ~s"
                     compile-in-roots-prop
                     (si:package-root-name (if package-spec package-spec *package*))))))
    (OR QC-FILE-REL-FORMAT-OVERRIDE
        (CASE (SEND GENERIC-PATHNAME :GET ':FASL)
          (:REL (SETQ QC-FILE-REL-FORMAT T))
          (:FASL (SETQ QC-FILE-REL-FORMAT NIL))
          ((NIL))
          (T (FERROR "File property FASL value not FASL or REL in file ~A"
                     GENERIC-PATHNAME))))
    ;; Bind all the variables required by the file property list.
    (MULTIPLE-VALUE-BIND (VARIABLES VALS) (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME)
      (PROGV VARIABLES VALS
        ;; File compilation always gets its's own environment, which is discarded after the
        ;; compilation.  If there is non-null environment, the new environment established
        ;; here is `inside' that environment, and captures everything in it.
       (let ((*compilation-environment*
               (or explicit-compilation-environment
                   (make-compilation-environment :target *target-computer*))))  ;make a resource?
        (COND (QC-FILE-REL-FORMAT
               (LET ((FASD-STREAM NIL)) ;REL compiling doesn't work the same way
                 (LOCKING-RESOURCES
                   (FUNCALL (INTERN (STRING 'DUMP-START) 'QFASL-REL))
                   (COMPILE-STREAM INPUT-STREAM GENERIC-PATHNAME FASD-STREAM 'QC-FILE-WORK-COMPILE
                                   LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
                                   nil          ; was FILE-LOCAL-DECLARATIONS -smh
                                   READ-THEN-PROCESS-FLAG
                                   *target-computer*)
                   ;; Output a record of the macros expanded and their current sxhashes.
                   (WHEN QC-FILE-MACROS-EXPANDED
                     (FUNCALL (INTERN (STRING 'DUMP-FORM) 'QFASL-REL)
                              `(SI:FASL-RECORD-FILE-MACROS-EXPANDED
                                 ',QC-FILE-MACROS-EXPANDED)))
                   (LET ((*PACKAGE* (IF PACKAGE-SPEC (PKG-FIND-PACKAGE PACKAGE-SPEC) *PACKAGE*)))
                     (FUNCALL (INTERN (STRING 'WRITE-REL-FILE) 'QFASL-REL) OUTPUT-FILE)))))
              (T
               (WITH-OPEN-STREAM (FASD-STREAM (IF *QC-FILE-OUTPUT-SAME-VERSION*
                                                  (OPEN OUTPUT-FILE
                                                        :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE byte-size
                                                        :IF-EXISTS :SUPERSEDE)
                                                (OPEN OUTPUT-FILE
                                                      :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE byte-size)))
                 (with-fasd-indirect-array (fasd-stream)
                   (FLET ((DOIT ()
                                (LOCKING-RESOURCES
                                  (SETQ OUTPUT-FILE (SEND FASD-STREAM :PATHNAME))
                                  (FASD-INITIALIZE)
                                  (FASD-START-FILE)
                                  (COMPILE-STREAM INPUT-STREAM GENERIC-PATHNAME FASD-STREAM 'QC-FILE-WORK-COMPILE
                                                  LOAD-FLAG IN-CORE-FLAG PACKAGE-SPEC
                                                  nil   ; was FILE-LOCAL-DECLARATIONS -smh
                                                  READ-THEN-PROCESS-FLAG
                                                  T
                                                  *target-computer*)
                                  ;; Output a record of the macros expanded and their current sxhashes.
                                  (WHEN QC-FILE-MACROS-EXPANDED
                                    (FASD-FORM
                                      `(SI::FASL-RECORD-FILE-MACROS-EXPANDED ',QC-FILE-MACROS-EXPANDED)))
                                  (FASD-END-WHACK)
                                  (FASD-END-FILE))))
                     (COND (*QC-FILE-OUTPUT-DRIBBLE-TYPE*
                            (WITH-OPEN-STREAM (DRIBBLE-FILE (IF *QC-FILE-OUTPUT-SAME-VERSION*
                                                                (OPEN (SEND OUTPUT-FILE :NEW-TYPE *QC-FILE-OUTPUT-DRIBBLE-TYPE*)
                                                                      :DIRECTION :OUTPUT :CHARACTERS T
                                                                      :IF-EXISTS :SUPERSEDE)
                                                              (OPEN (SEND OUTPUT-FILE :NEW-TYPE *QC-FILE-OUTPUT-DRIBBLE-TYPE*)
                                                                    :DIRECTION :OUTPUT :CHARACTERS T)))
                              (FORMAT DRIBBLE-FILE
                                      "Compilation log started at ~\time\ by ~S for~% INPUT: ~S~% OUTPUT: ~S~2%"
                                      (TIME:GET-UNIVERSAL-TIME) SI:USER-ID
                                      (SEND INPUT-STREAM :TRUENAME)
                                      (SEND FASD-STREAM :TRUENAME))
                              (LET ((DRIBBLE-STREAM (SI:MAKE-DRIBBLE-STREAM *TERMINAL-IO* DRIBBLE-FILE)))
                                (LET ((*STANDARD-INPUT* DRIBBLE-STREAM)
                                      (*STANDARD-OUTPUT* DRIBBLE-STREAM)
                                      (*QUERY-IO* DRIBBLE-STREAM)
                                      (*ERROR-OUTPUT* DRIBBLE-STREAM)
                                      (*TRACE-OUTPUT* DRIBBLE-STREAM)
                                      (TIME (TIME))
                                      (DW (SI:READ-METER 'SI:%DISK-WAIT-TIME)))
                                  (DOIT)
                                  (FORMAT DRIBBLE-FILE
                                          "~&~3%Compilation complete at ~\time\~
                                       ~%~\scientific\seconds realtime ~\scientific\seconds disk wait~%"
                                          (TIME:GET-UNIVERSAL-TIME)
                                          (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0)
                                          (QUOTIENT (- (SI:READ-METER 'SI:%DISK-WAIT-TIME) DW) 1.0E6))
                                  (GC:STATUS DRIBBLE-FILE)
                                  (GC:PRINT-STATISTICS DRIBBLE-FILE)))))
                           ('ELSE
                            (DOIT))))))))
        (when environment-pathname
          (write-compilation-environment *compilation-environment* environment-pathname))
        )))
  OUTPUT-FILE))

;;; COMPILE-STREAM when called by QC-FILE calls this on each form in the file
(DEFUN QC-FILE-WORK-COMPILE (FORM)
  ;; Maybe macroexpand in temp area.
  (LET-IF (NOT QC-FILE-LOAD-FLAG) ((DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA))
    ;; Macro-expand and output this form in the appropriate way.
    (COMPILE-DRIVER FORM 'QC-FILE-COMMON NIL)))

;;; Common processing of each form, for both QC-FILE and FASD-UPDATE-FILE.
(DEFUN QC-FILE-COMMON (FORM TYPE)
  (COND ((MEMQ TYPE '(SPECIAL DECLARE MACRO))
         ;; While evaluating the thing, turn off the temporary area, and
         ;; if this is an EVAL-WHEN (COMPILE), turn off the undo-declarations
         ;; flag so that macro definitions will really happen.
         ;;  NO, don't it screws DEFSTRUCT, which uses EVAL-WHEN (COMPILE LOAD EVAL).
         ;; YES, do it, since DEFSTRUCT no longer uses EVAL-WHEN.
         (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
               (UNDO-DECLARATIONS-FLAG (AND UNDO-DECLARATIONS-FLAG
                                            (NEQ TYPE 'DECLARE))))
           (OR QC-FILE-IN-CORE-FLAG (EVAL (COPY-TREE FORM))))
         ;; If supposed to compile or fasdump as well as eval, do so.
         (COND ((EQ TYPE 'SPECIAL) (QC-FILE-FASD-FORM FORM NIL))
               ((EQ TYPE 'MACRO)
                (QC-TRANSLATE-FUNCTION (CADR FORM)
                                       (DECLARED-DEFINITION (CADR FORM))
                                       'MACRO-COMPILE
                                       (IF QC-FILE-REL-FORMAT 'REL 'QFASL)))))
        (QC-FILE-IN-CORE-FLAG (QC-FILE-FASD-FORM FORM T))
        (T (QC-FILE-FORM FORM))))

;;; Enable microcompilation (when it is requested).  NIL turns it off always.
(DEFVAR *MICROCOMPILE-SWITCH* T)

;;; Handle one form from the source file, in a QC-FILE which is actually recompiling.
;;; Only DEFUNs and random forms to be evaluated come here.
;;; We assume that DEFUNs have already been munged into the standard
;;; (DEFUN fn args . body) form.
(DEFUN QC-FILE-FORM (FORM)
  (PROG (TEM FV)
    (COND ((ATOM FORM))
          ((EQ (CAR FORM) 'COMMENT))            ;Delete comments entirely
          ((EQ (CAR FORM) 'DEFUN)
           (SETQ TEM (CADR FORM))
           (SETQ FV (SI:PROCESS-DEFUN-BODY TEM (CDDR FORM)))
           (COND (QC-FILE-LOAD-FLAG
                     (RPLACA (FUNCTION-CELL-LOCATION TEM) FV)   ;In case used interpreted
                     (COMPILE-1 TEM FV)
                     (RETURN (QC-FILE-FASD-FORM FORM T))))
           (QC-TRANSLATE-FUNCTION TEM FV
                                  'MACRO-COMPILE
                                  (IF QC-FILE-REL-FORMAT 'REL 'QFASL))
           (IF (AND *MICROCOMPILE-SWITCH*
                    (GETDECL TEM 'MICROCOMPILE))
               (QC-TRANSLATE-FUNCTION
                 TEM FV                         ;Once more, with feeling
                 'MICRO-COMPILE
                 (COND (QC-FILE-REL-FORMAT 'REL)
                       (T 'QFASL))))
           (RETURN NIL))
          ((eq (car form) 'defafun)             ; $$$ added <22-Nov-88 smh>
           (return (compiler-target-switch (defafun form 'qfasl))))
          (QC-FILE-LOAD-FLAG (EVAL FORM)))
    (RETURN (QC-FILE-FASD-FORM FORM T))))

;;; Dump out a form to be evaluated at load time.
;;; Method of dumping depends on format of file being written.
(DEFUN QC-FILE-FASD-FORM (FORM &OPTIONAL OPTIMIZE)
  (UNLESS (OR (NUMBERP FORM) (STRINGP FORM) (MEMQ FORM '(NIL T))
              (AND (CONSP FORM) (EQ (CAR FORM) 'QUOTE)))
    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
      (IF QC-FILE-REL-FORMAT
          (FUNCALL (INTERN "DUMP-FORM" 'QFASL-REL) FORM OPTIMIZE)
        (FASD-FORM FORM OPTIMIZE)))))

;;; This is the heart of the M-X Fasl Update command.
;;; Reads from INPUT-STREAM using READ-FUNCTION (called with arguments like READ's)
;;; INFILE should be the name of the input file that INPUT-STREAM is reading from.
;;; OUTFILE is a pathname used to open an output file.
(DEFUN FASL-UPDATE-STREAM (INFILE OUTFILE INPUT-STREAM READ-FUNCTION
                           &AUX QC-FILE-LOAD-FLAG (QC-FILE-IN-CORE-FLAG T)
                           (DEFAULT-CONS-AREA DEFAULT-CONS-AREA))
  (DECLARE (IGNORE INFILE))
  (LET ((QC-FILE-IN-PROGRESS T)
        (LOCAL-DECLARATIONS NIL)
        (FASD-PACKAGE NIL))
    (LOCKING-RESOURCES
      (WITH-OPEN-FILE (FASD-STREAM OUTFILE :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE 16.)
        (FASD-INITIALIZE)
        (FASD-START-FILE)
        ;; First thing in QFASL file must be property list
        ;; Only property supported just now is PACKAGE property
        (FASD-ATTRIBUTES-LIST
          (LIST ':PACKAGE (INTERN (PACKAGE-NAME *PACKAGE*) SI:PKG-KEYWORD-PACKAGE)))
        (QC-PROCESS-INITIALIZE)
        (DO ((EOF '(()))
             FORM)
            (NIL)
          ;; Start a new whack if FASD-TABLE is getting too big.
          (AND ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
               (FASD-END-WHACK))
          ;; Read and macroexpand in temp area.
          (SETQ DEFAULT-CONS-AREA QCOMPILE-TEMPORARY-AREA)
          (LET ((QC-FILE-READ-IN-PROGRESS T))
            (SETQ FORM (FUNCALL READ-FUNCTION INPUT-STREAM EOF)))
          (AND (EQ EOF FORM)
               (RETURN NIL))
          (SETQ FORM (MACROEXPAND FORM))
          (SETQ DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
          ;; Output this form in the appropriate way.
          (COMPILE-DRIVER FORM 'FASL-UPDATE-FORM NIL))
        (FASD-END-WHACK)
        (FASD-END-FILE)))))

;;; Process one form, for COMPILE-DRIVER.
(DEFUN FASL-UPDATE-FORM (FORM TYPE)
  (CASE TYPE
    (SPECIAL (FASD-FORM FORM NIL))
    (DECLARE)                           ;Ignore DECLAREs -- this may not always be right!
    ((DEFUN MACRO)                      ;Don't compile -- send over whatever is already compiled
     (OR (FDEFINEDP (CADR FORM))
         (FERROR "You forgot to define ~S" (CADR FORM)))
     (LET ((TEM (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC (CADR FORM)))))
       (AND (CONSP TEM) (EQ (CAR TEM) 'MACRO) (SETQ TEM (CDR TEM)))
       (COND ((AND (CONSP TEM) (FUNCTIONP TEM T))
              (FORMAT *ERROR-OUTPUT* "~&Compiling ~S~%" (CADR FORM))
              (COMPILE (CADR FORM))))
       ;; This works on this bodiless DEFUN by virtue of the fact that FASD-FORM in
       ;; Optimize mode calls FDEFINITION rather than looking at the form.
       (FASD-FORM FORM T)))
    (OTHERWISE (FASD-FORM FORM T))))

;;; (COMPILE-DRIVER form processing-function override-fn) should be used by anyone
;;; trying to do compilation of forms from source files, or any similar operation.
;;; It knows how to decipher DECLAREs, EVAL-WHENs, DEFUNs, macro calls, etc.
;;; It doesn't actually compile or evaluate anything,
;;; but instead calls the processing-function with two args:
;;;  a form to process, and a flag which is one of these atoms:
;;;   SPECIAL  -  QC-FILE should eval this and put it in the FASL file.
;;;             UNDO-DECLARATIONS-FLAG, if on, should stay on for this.
;;;   DECLARE  -  QC-FILE should eval this.
;;;   PROCLAIM -  Hair.
;;;               More Recent Historical Note:
;;;                  As of system 124, PROCLAIM forms were only being done at
;;;                  compile-time.  COMPILE-PROCLAIM now calls the compiler user's
;;;                  processing function.  For compile-file, this means that a
;;;                  form gets in the fastload-file to evaluate the PROCLAIM form.
;;;                  We also taught ZWEI:COMPILE-BUFFER-FORM to handle PROCLAIM
;;;                  like SPECIAL.  *** Keith and PLD, 6/24/88
;;;   DEFUN    -  QC-FILE should compile this and put the result in the FASL file.
;;;   MACRO    -  This defines a macro.  QC-FILE should record a declaration
;;;                and compile it into the FASL file.
;;;   RANDOM   -  QC-FILE should just put this in the FASL file to be evalled.
;;; Of course, operations other than QC-FILE will want to do different things
;;; in each case, but they will probably want to distinguish the same cases.
;;; That's why COMPILE-DRIVER will be useful to them.

;;; override-fn gets to look at each form just after macro expansion.
;;; If it returns T, nothing more is done to the form.  If it returns NIL,
;;; the form is processed as usual (given to process-fn, etc.).
;;; override-fn may be NIL.

(DEFUN COMPILE-DRIVER (FORM PROCESS-FN OVERRIDE-FN &OPTIONAL COMPILE-TIME-TOO (TOP-LEVEL-P T)
                       &AUX FN (OFORM FORM))
  ;; The following loop is essentially MACROEXPAND,
  ;; but for each expansion, we create an appropriate warn-on-errors message
  ;; containing the name of the macro about to be (perhaps) expanded this time.
  (DO ((NFORM))
      (())
    (IF (AND OVERRIDE-FN
             (FUNCALL OVERRIDE-FN FORM))
        (RETURN-FROM COMPILE-DRIVER NIL))
    (IF (ATOM FORM) (RETURN NIL))
    (maybe-invoke-style-checker form)
;    ;; Don't expand LOCALLY into PROGN here!
;    ;; This way, we protect DECLAREs inside the LOCALLY
;    ;; from being treated as top-level DECLARE, which would be erroneous.
;    ;; The LOCALLY form will just be executed as a random form.
;    (IF (EQ (CAR FORM) 'LOCALLY)
;        (RETURN))
    (SETQ NFORM
          (WARN-ON-ERRORS ('MACRO-EXPANSION-ERROR "Error expanding macro ~S at top level"
                           (CAR FORM))
            (MACROEXPAND-1 FORM)))
    (IF (EQ FORM NFORM) (RETURN)
      (SETQ FORM NFORM)))
  ;; If this was a top-level macro, supply a good guess
  ;; for the function-parent for any DEFUNs inside the expansion.
  (LET ((LOCAL-DECLARATIONS LOCAL-DECLARATIONS))
    (COND ((ATOM FORM))
          ((AND (NEQ FORM OFORM) (SYMBOLP (CADR OFORM)))
           (PUSH `(FUNCTION-PARENT ,(CADR OFORM)) LOCAL-DECLARATIONS))
          ((EQ (CAR OFORM) 'DEFSTRUCT)
           (PUSH `(FUNCTION-PARENT ,(IF (SYMBOLP (CADR OFORM)) (CADR OFORM) (CAADR OFORM)))
                 LOCAL-DECLARATIONS)))
    (AND (CONSP FORM)
         (NEQ (CAR FORM) 'EVAL-WHEN)
         COMPILE-TIME-TOO
         (FUNCALL PROCESS-FN FORM 'DECLARE))
    (COND ((ATOM FORM)
           (FUNCALL PROCESS-FN FORM 'RANDOM))
          ((EQ (CAR FORM) 'EVAL-WHEN)
           (OR (AND (CL:LISTP (CADR FORM))
                    (LOOP FOR TIME IN (CADR FORM)
                          ALWAYS (MEMQ TIME '(EVAL LOAD COMPILE))))
               (FERROR "~S invalid ~S times;~%  must be a list of ~S, ~S, and//or ~S."
                           (CADR FORM) 'EVAL-WHEN 'EVAL 'LOAD 'COMPILE))
           (LET* ((COMPILE (MEMQ 'COMPILE (CADR FORM)))
                  (LOAD (MEMQ 'LOAD (CADR FORM)))
                  (EVAL (MEMQ 'EVAL (CADR FORM)))
                  (EVAL-NOW (OR COMPILE (AND COMPILE-TIME-TOO EVAL))))
             (DOLIST (FORM1 (CDDR FORM))
               (IF LOAD
                   (IF EVAL-NOW
                       (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN T NIL)
                     (COMPILE-DRIVER FORM1 PROCESS-FN OVERRIDE-FN NIL NIL))
                 (IF EVAL-NOW
                     (FUNCALL PROCESS-FN FORM1 'DECLARE))))))
          ((EQ (SETQ FN (CAR FORM)) 'DEFF)
           (COMPILATION-DEFINE (CADR FORM))
           (FUNCALL PROCESS-FN FORM 'RANDOM))
          ((EQ FN 'DEF)
           (COMPILATION-DEFINE (CADR FORM))
           (MAPC (LAMBDA (FORM)
                   (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
                 (CDDR FORM)))
          ((EQ FN 'WITH-SELF-ACCESSIBLE)
           (MAPC (LAMBDA (FORM)
                   (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL))
                 (CDDR FORM)))
          ((EQ FN 'PROGN)
           (MAPC (LAMBDA (FORM)
                   (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
                 (CDR FORM)))
          ((MEMQ FN '(MACRO DEFSUBST))
           (FUNCALL PROCESS-FN FORM 'MACRO))
          ((AND TOP-LEVEL-P
                (MEMQ FN '(SPECIAL UNSPECIAL MAKE-PACKAGE IN-PACKAGE SHADOW SHADOWING-IMPORT
                           EXPORT UNEXPORT USE-PACKAGE UNUSE-PACKAGE IMPORT DEFF-MACRO
                           REQUIRE)))
           (FUNCALL PROCESS-FN FORM 'SPECIAL))
          ((EQ FN 'DECLARE)
           (COMPILE-DECLARE (CDR FORM) PROCESS-FN))
          ((EQ FN 'PROCLAIM)
           (COMPILE-PROCLAIM (CDR FORM) PROCESS-FN))
          ((EQ FN 'COMMENT) NIL)
          ((EQ FN 'PATCH-SOURCE-FILE)
           (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
                              (SETQ SI:PATCH-SOURCE-FILE-NAMESTRING ,(CADR FORM)))
                           PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)
           (MAPC (LAMBDA (FORM)
                   (COMPILE-DRIVER FORM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
                 (CDDR FORM))
           (COMPILE-DRIVER `(EVAL-WHEN (LOAD EVAL)
                              (SETQ SI:PATCH-SOURCE-FILE-NAMESTRING NIL))
                           PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T))
          ((EQ FN 'COMPILER-LET)
           (PROGW (CADR FORM)
             (COMPILE-DRIVER `(PROGN . ,(CDDR FORM))
                             PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO T)))
          ((EQ FN 'DEFUN)
           (LET (TEM)
             (WARN-ON-ERRORS ('MALFORMED-DEFUN "Malformed DEFUN")
               (SETQ TEM (DEFUN-COMPATIBILITY (CDR FORM))))
             (COND ((EQ (CDR TEM) (CDR FORM))
                    (IF (NULL (CDDR TEM))
                        (WARN 'MALFORMED-DEFUN :IMPOSSIBLE "Malformed defun ~S" FORM)
                      (FUNCALL PROCESS-FN FORM 'DEFUN)))
                   (T (COMPILE-DRIVER TEM PROCESS-FN OVERRIDE-FN COMPILE-TIME-TOO NIL)))))
          ((eq fn 'defafun)                     ; $$$ added <22-Nov-88 smh>
           (funcall process-fn form 'defafun))
          (T (FUNCALL PROCESS-FN FORM 'RANDOM)))))

(DEFUN COMPILE-DECLARE (DECL-LIST PROCESS-FN)
  (MAPC (LAMBDA (DECL)
          (FUNCALL PROCESS-FN DECL
                   (IF (MEMQ (CAR DECL) '(SPECIAL UNSPECIAL))
                       'SPECIAL
                       'DECLARE)))
        DECL-LIST))

(DEFUN COMPILE-PROCLAIM (DECL-LIST PROCESS-FN)
  (apply #'proclaim (mapcar #'eval decl-list))
  (funcall process-fn (cons 'proclaim decl-list) 'proclaim)
  decl-list)

;;; Does this do anything at all differently?
;;; NOP NOP NOP
;  (MAPC (LAMBDA (DECL &AUX X)
;         (CONDITION-CASE (ERROR)
;             ;; this randomness brought to you though the miracle of Design by Committee
;             (SETQ X (EVAL DECL))
;           (ERROR (WARN 'BAD-DECLARATION :IMPOSSIBLE
;                        "Error evaluating argument ~S to ~S~&~A"
;                        DECL 'PROCLAIM ERROR))
;           (:NO-ERROR
;            (IF (NOT (SYMBOLP (CAR X)))
;                (WARN 'BAD-DECLARATION :IMPOSSIBLE
;                      "An argument of ~S evaluated to ~S, which is not a valid declaration"
;                      'PROCLAIM DECL)
;              (LET ((S (CAR X)))
;                (COND ((eq s 'special)   (mapcar #'proclaim-special   (cdr x)))
;                      ((eq s 'unspecial) (mapcar #'proclaim-unspecial (cdr x)))
;                      ((EQ S 'INLINE)
;                       )
;                      ((EQ S 'NOTINLINE)
;                       )
;                      ((EQ S 'DECLARATION)
;                       )
;                      (T
;                       ;;ignore unknown declarations for now
;                       )))))))
;       DECL-LIST))

(DEFPROP PATCH-SOURCE-FILE T SI:MAY-SURROUND-DEFUN)
(DEFUN PATCH-SOURCE-FILE (&QUOTE SI:PATCH-SOURCE-FILE-NAMESTRING &REST BODY)
  (MAPC #'EVAL BODY))

(defun write-compilation-environment (compilation-environment pathname &aux pvals mvals)
  ;;(print-comp-env compilation-environment)
  (maphash #'(lambda (k v) (push (cons k v) pvals))
           (compilation-environment-plist-hashtab compilation-environment))
  (maphash #'(lambda (k v) (push (cons k v) mvals))
           (compilation-environment-macro-hashtab compilation-environment))
  ;; rebind target while writing FENV so it gets written for the right machine.  ||| 28sep88 smh
  (let ((*target-computer* 'lambda-interface))
    (zl:dump-forms-to-file
      (fs:merge-pathname-components pathname nil :default-type :FDEF)
      `((load-to-compilation-environment-internal
          ',(compilation-environment-target compilation-environment)
          ',pvals
          ',mvals))
      `(:package ,(package-name *package*)
                 :readtable ,(car (si:rdtbl-names *readtable*))))))

;; Fixed hash key botch. ||| 28sep88 smh
(defun load-to-compilation-environment-internal (target plists macros)
  (unless (compilation-environment-p *compilation-environment*)
    (error "Attempting to load a compilation-environment without binding *COMPILATION-ENVIRONMENT*
to a COMPILATION-ENVIRONMENT object."))
  (unless (equal (compilation-environment-target *compilation-environment*) target)
    (error "Attepting to load a compilation-environment for target ~S but *COMPILATION-ENVIRONMENT*
has a target of ~S." target (compilation-environment-target *compilation-environment*)))
  (let ((pht (compilation-environment-plist-hashtab *compilation-environment*)))
    (dolist (key-pvals plists)
      (let ((plist (gethash (car key-pvals) pht)))
        (do ((p (cdr key-pvals) (cddr p)))
            ((null p))
          (setf (getf plist (car p)) (cadr p)))
        (setf (gethash (car key-pvals) pht) plist))))
  (let ((mht (compilation-environment-macro-hashtab *compilation-environment*)))
    (dolist (key-macro macros)
      (setf (gethash (car key-macro) mht) (cdr key-macro)))))

;;;; Barf all over SPECIAL and UNSPECIAL "declarations."

;;; When not compiling a file, etc., or in Maclisp,
;;;  we simply put on or remove a SPECIAL property.
;;; When compiling a file (COMPILE-NO-LOAD-FLAG is T)
;;;  we just use FILE-LOCAL-DECLARATIONS to make the change.
;;; SPECIAL just pushes one big entry on FILE-LOCAL-DECLARATIONS, to save consing.
;;; UNSPECIAL, for each symbol, tries to avoid lossage in the case where a symbol
;;; is repeatedly made special and then unspecial again, by removing any existing
;;; unshadowed SPECIALs from FILE-LOCAL-DECLARATIONS, and then putting on an UNSPECIAL
;;; only if there isn't already one.  This way, FILE-LOCAL-DECLARATIONS doesn't keep growing.

;;; SPECIAL-1 and UNSPECIAL-1 can be used to make a computed symbol special or unspecial.

(record-source-file-name 'special 'defun t)
(record-source-file-name 'unspecial 'defun t)
(DEFUN SPECIAL (&REST &QUOTE SYMBOLS)
  "Make all the SYMBOLS be marked as special
ie make the scope of their value bindings be dynamic."
  (MAPC #'SPECIAL-1 SYMBOLS)
  T)

(DEFUN SPECIAL-1 (SYMBOL)
  "Make SYMBOL be marked special as special."
  (COND (UNDO-DECLARATIONS-FLAG
         (putdecl symbol 'special 't))
        (T (PUTPROP SYMBOL (OR FDEFINE-FILE-PATHNAME T) 'SPECIAL))))

(DEFUN UNSPECIAL (&REST &QUOTE SYMBOLS)
  "Make all the SYMBOLS not be marked special
ie make the scope of their value bindings be lexical."
  (MAPC #'UNSPECIAL-1 SYMBOLS)
  T)

(DEFUN UNSPECIAL-1 (SYMBOL)
  "Make SYMBOL not be marked as special."
  (COND (UNDO-DECLARATIONS-FLAG
         (putdecl SYMBOL 'special 'nil))
        (T (REMPROP SYMBOL 'SPECIAL)
           (REMPROP SYMBOL 'SYSTEM-CONSTANT))))

(DEFUN DEFUN-COMPATIBILITY (EXP)
  "Process the cdr of a DEFUN-form, converting old Maclisp formats into modern Lispm ones.
This must be done before the name of the function can be determined with certainty.
The value is an entire form, starting with DEFUN or MACRO.
If no change has been made, the cdr of the value will be EQ to the argument.
Also gets rid of old Lisp Machine &QUOTE"
  (DEFUN-COMPATIBILITY-OLD-LISPM
    (COND ((AND (CONSP (CAR EXP))
             (SYMBOLP (CAAR EXP))
             (GET (CAAR EXP) 'FUNCTION-SPEC-HANDLER))
           (CONS 'DEFUN EXP))
          ('ELSE
           (DEFUN-COMPATIBILITY-MACLISP EXP)))))

(DEFUN DEFUN-COMPATIBILITY-OLD-LISPM (EXP &OPTIONAL NOWARN)
  (COND ((NOT (MEMQ '&QUOTE (CADDR EXP)))
         EXP)
        ('ELSE
         (LET ((NAME (CADR EXP))
               (ARGLIST (CADDR EXP))
               (LET*-ARGLIST)
               (ARG-STATE NIL)
               (ARG-EVAL 'EVAL1)
               (FORM (GENTEMP "form"))
               (ARGS (GENTEMP "args"))
               (NEW-BODY NIL)
               (DECLS NIL)
               (NDECL '(DECLARE)))
           ;; This has to be an error, not a warning, since we can't support it.
           (unless (symbolp name)
             (ferror "Attempt to define the special form with the name ~S, which is not a symbol."
                     name))
           (OR NOWARN
               (WARN 'FUNCTION-NOT-VALID :OBSOLETE
                     "(DEFUN ~S ...) has &QUOTE in argument list. New special forms should be macros"
                     NAME))
           (DOLIST (ARG ARGLIST)
             (WHEN (AND (MEMQ ARG LAMBDA-LIST-KEYWORDS)
                        (NOT (MEMQ ARG '(&QUOTE &EVAL &REST &AUX &OPTIONAL))))
               (WARN 'FUNCTION-NOT-VALID
                     "Unhandled lambda list keyword: ~S" ARG)))
           (SETQ LET*-ARGLIST `((,ARGS (CDR ,FORM))))
           (DOLIST (ARG ARGLIST)
             (COND ((MEMQ ARG '(&REST &AUX &OPTIONAL))
                    (SETQ ARG-STATE ARG))
                   ((MEMQ ARG '(&QUOTE &EVAL))
                    (SETQ ARG-EVAL (IF (EQ ARG '&EVAL) 'EVAL1 'PROGN)))
                   ((EQ ARG-STATE '&OPTIONAL)
                    (LET ((ARG-NAME (IF (ATOM ARG) ARG (CAR ARG)))
                          (ARG-DEFAULT (IF (ATOM ARG) NIL (CADR ARG)))
                          (ARG-GIVEN-P (IF (ATOM ARG) NIL (CADDR ARG))))
                      (SETQ LET*-ARGLIST (APPEND LET*-ARGLIST
                                                 (COND ((NOT ARG-GIVEN-P)
                                                        `((,ARG-NAME (IF ,ARGS
                                                                         (,ARG-EVAL (POP ,ARGS))
                                                                       ,ARG-DEFAULT))))
                                                       ('ELSE
                                                        `((,ARG-GIVEN-P (IF ,ARGS T NIL))
                                                          (,ARG-NAME (IF ,ARG-GIVEN-P
                                                                         (,ARG-EVAL (POP ,ARGS))
                                                                       ,ARG-DEFAULT)))))))))

                   ((EQ ARG-STATE '&REST)
                    (SETQ LET*-ARGLIST (APPEND LET*-ARGLIST
                                                 `((,ARG ,(IF (EQ ARG-EVAL 'PROGN) ARGS
                                                            `(MAPCAR #'EVAL1 ,ARGS)))))))
                   ((EQ ARG-STATE '&AUX)
                    (SETQ LET*-ARGLIST (APPEND LET*-ARGLIST (LIST ARG))))
                   ('ELSE
                    (SETQ LET*-ARGLIST (APPEND LET*-ARGLIST
                                               `((,ARG (IF ,ARGS
                                                           (,ARG-EVAL (POP ,ARGS))
                                                         (CERROR "use NIL instead"
                                                                 "ran out of args")))))))))
           (DO ((L (CDDDR EXP) (CDR L)))
               ((OR (NULL L)
                    (NOT (OR (STRINGP (CAR L))
                             (AND (CONSP (CAR L)) (EQ (CAAR L) 'DECLARE)))))
                (SETQ NEW-BODY L))
             (SETQ DECLS (APPEND DECLS (LIST (CAR L)))))
           ;; DO A LITTLE NORMALIZATION ON THE DECLARATIONS FOUND.
           (DO ((L DECLS (CDR L))
                (FOUND NIL))
               ((NULL L)
                (OR FOUND
                    (SETQ DECLS (APPEND DECLS `((DECLARE (ARGLIST ,@ARGLIST)))))))
             (WHEN (AND (NOT (ATOM (CAR L))) (EQ (CAAR L) 'DECLARE))
               (DOLIST (D (CDAR L))
                 (COND ((ATOM D))
                       ((EQ (CAR D) 'ARGLIST)
                        (SETQ FOUND T))
                       ((EQ (CAR D) 'IGNORE)
                        (SETQ NDECL (APPEND NDECL (LIST D))))))))
           `(DEFUN (:SPECIAL-FORM ,NAME) (,FORM)
              ,@DECLS
              (LET* ,LET*-ARGLIST
                ,NDECL
                ,(IF (NOT (MEMQ '&REST ARGLIST))
                     `(IF ,ARGS (CERROR "forget the rest" "too many arguments")))
                ,@NEW-BODY))))))

(DEFUN DEFUN-COMPATIBILITY-MACLISP (EXP)
  (PROG (FCTN-NAME LL BODY TYPE)
        (SETQ TYPE 'EXPR)
        (SETQ FCTN-NAME (CAR EXP))
        (COND ((NOT (ATOM FCTN-NAME))           ;Convert list function specs
               (COND ((AND (= (LENGTH FCTN-NAME) 2)     ;(DEFUN (FOO MACRO) ...)
                           (EQ (SECOND FCTN-NAME) 'MACRO))
                      (SETQ TYPE 'MACRO FCTN-NAME (CAR FCTN-NAME)))
                     ((EQ FCTN-NAME (SETQ FCTN-NAME (STANDARDIZE-FUNCTION-SPEC FCTN-NAME)))
                      (RETURN (CONS 'DEFUN EXP)))))     ;Return if no conversion required
              ((OR (NOT (ATOM (CADR EXP))) (NULL (CADR EXP))) ;Detect a valid DEFUN.
               (RETURN (CONS 'DEFUN EXP)))
              ((MEMQ (CADR EXP) '(FEXPR EXPR MACRO))
               (SETQ TYPE (CADR EXP) EXP (CDR EXP)))
              ((MEMQ FCTN-NAME '(FEXPR EXPR MACRO))
               (SETQ TYPE FCTN-NAME FCTN-NAME (CADR EXP) EXP (CDR EXP))))
        ;; Here if a new DEFUN has to be constructed
        (SETQ LL (CADR EXP))
        (SETQ BODY (CDDR EXP))
;Weird conversion hack to unconvert interlisp nlambdas that were previously converted
; By holloway's random hacker to kludgy fexpr's
        (COND ((AND (EQ TYPE 'FEXPR)
                    (EQUAL LL '(*ARGS*)))
                (SETQ TYPE 'EXPR)
                (SETQ LL (CONS '&QUOTE (CADAAR BODY)))  ;Lambda list of internal lambda
                (SETQ BODY (CDDAAR BODY)) ))    ;Body of internal lambda
; **END OF THAT HACK**
        (COND ((EQ TYPE 'FEXPR)
               (SETQ LL (CONS '&QUOTE (CONS '&REST LL))))
              ((EQ TYPE 'MACRO)
               (RETURN (CONS 'MACRO (CONS FCTN-NAME (CONS LL BODY)))))
              ((AND LL (ATOM LL))
                (SETQ TYPE 'LEXPR
                     LL `(&EVAL &REST *LEXPR-ARGLIST* &AUX (,LL (LENGTH *LEXPR-ARGLIST*))))))
        (RETURN (CONS 'DEFUN (CONS FCTN-NAME (CONS LL BODY))))))

;;; This is the modern way for the compiler to issue a warning.

(DEFUN COMPILER:WARN (TYPE SEVERITY FORMAT-STRING &REST ARGS)
  "Record and print a compiler warning.
TYPE describes the particular kind of problem, such as FUNCTION-NOT-VALID.
SEVERITY is a symbol in the keyword package giving a broader classification;
  see the source for a list of possible severities.
FORMAT-STRING and ARGS are used to print the warning.
/
Severities include /(though are not limited to/):
/
 :IMPLAUSIBLE - something that is not intrinsically wrong but is probably due
        to a mistake of some sort.
 :IMPOSSIBLE - something that cannot have a meaning.
 :MISSING-DECLARATION - free variable not declared special, usually.
 :PROBABLE-ERROR - something that is an error unless you have changed something else.
 :OBSOLETE - something that you shouldn't use any more.
 :VERY-OBSOLETE - similar to :OBSOLETE, only more so.
 :NOT-PORTABLE - code that may not be portable between LISP implementations.
 :ERROR - there was an error in reading or macro expansion.
 :FATAL - there was an error that prevents further compilation."
  (declare (zwei:indentation (2 1)))
  (values
    (APPLY #'SI:RECORD-AND-PRINT-WARNING TYPE SEVERITY NIL FORMAT-STRING
           ;;;>>>Don't need to worry about temp areas with Lambda GC.
           ;;    (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
           ;;      ;; Copy temp area data only; note that ARGS lives in PDL-AREA.
           ;;      ;; on error for nonexistent package refname.
           ;;      (MAPCAR (LAMBDA (ARG) (SI:COPY-OBJECT-TREE ARG T 12.)) ARGS))
           ;;;Other systems don't put &rest args on stack, but args might get bashed
           ;;;during RECORD-WARNING's list-munging.  So, 'tho I don't know if this is needed:
           ;;;  -Keith
           (copy-tree args))
    'warn))

;;;why???

(DEFUN MEMQL (A B)
  "Returns elements of A that are EQL to elements of B."
  (PROG ()
     L  (COND ((NULL A) (RETURN NIL))
              ((MEMQ (CAR A) B) (RETURN A)))
        (SETQ A (CDR A))
        (GO L)))

;;; World-load (as opposed to cold-load) version of DEFMIC.
;;; Store into MICRO-CODE-ENTRY-ARGLIST-AREA
;;; Put on QLVAL and QINTCMP properties

(defmacro defmic (name opcode arglist lisp-function-p &optional no-qintcmp)
  "Tell LISP about a function definition that is microcoded.  Used only in SYS: COLD; DEFMIC."
  `(defmic-1 ',name ,opcode ',arglist ',lisp-function-p ,no-qintcmp))

(defun defmic-1 (name opcode arglist lisp-function-p no-qintcmp
                 &aux function-name instruction-name micro-code-entry-index)
  (if (symbolp name)
      (setq function-name name instruction-name name)
    (setq function-name (car name) instruction-name (cdr name)))
  (cond ((and lisp-function-p
              (fboundp function-name) ;In case DEFMIC file edited after cold-load made
              (= (%data-type (symbol-function function-name)) dtp-u-entry))
         (setq micro-code-entry-index (%pointer (symbol-function function-name)))
         ;; there is no slot if it is not a LISP function.
         (setf (aref (symbol-function 'micro-code-entry-arglist-area)
                     micro-code-entry-index)
               arglist))
        (t
         (putprop function-name arglist 'arglist)))
  (unless no-qintcmp
    (putprop instruction-name (length arglist) 'qintcmp)
    (unless (eq function-name instruction-name)
      (putprop function-name instruction-name 'misc-insn)
      (putprop function-name (length arglist) 'qintcmp)))
  (putprop instruction-name opcode 'qlval))
