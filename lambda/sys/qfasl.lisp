;;; -*- Mode:Lisp; Readtable:Zl; Package:si; Base:8.; Cold-load: T -*-
;;; This is SYS: SYS; QFASL, a cold load file.
;;;
;;; LOAD, READFILE, and FASLOAD for the Lisp Machine
;;; ** (c) Copyright 1980, 1984 Massachusetts Institute of Technology **

;;; All external symbols relating to this file either are in QDEFS, QCOM or deal with the file system...
;;; i.e. this should be a relatively portable fasloader.

;;; This gets defined early so that the defsubst in the old version of the file doesn't get used.
;;; The value is saved
;;; away in the FASL-TABLE for later use, and the index is returned (as the
;;; result of FASL-GROUP).


(DEFUN ENTER-FASL-TABLE (V)
  (OR (VECTOR-PUSH V FASL-TABLE)
      (VECTOR-PUSH-EXTEND V FASL-TABLE)))

(DEFVAR FASL-GROUP-DISPATCH :UNBOUND
  "Array of functions to handle fasl ops, indexed by fasl op code.")

(DEFCONST FASL-OPS '(
  FASL-OP-ERR
  FASL-OP-NOOP
  FASL-OP-INDEX
  FASL-OP-SYMBOL
  FASL-OP-LIST
  FASL-OP-TEMP-LIST
  FASL-OP-FIXED
  FASL-OP-FLOAT
  FASL-OP-ARRAY
  FASL-OP-EVAL
  FASL-OP-MOVE
  FASL-OP-FRAME
  FASL-OP-LIST-COMPONENT
  FASL-OP-ARRAY-PUSH
  FASL-OP-STOREIN-SYMBOL-VALUE
  FASL-OP-STOREIN-FUNCTION-CELL
  FASL-OP-STOREIN-PROPERTY-CELL
  FASL-OP-FETCH-SYMBOL-VALUE
  FASL-OP-FETCH-FUNCTION-CELL
  FASL-OP-FETCH-PROPERTY-CELL
  FASL-OP-APPLY
  FASL-OP-END-OF-WHACK
  FASL-OP-END-OF-FILE
  FASL-OP-SOAK
  FASL-OP-FUNCTION-HEADER
  FASL-OP-FUNCTION-END
  FASL-OP-NULL-ARRAY-ELEMENT
  FASL-OP-NEW-FLOAT
  FASL-OP-UNUSED10
  FASL-OP-UNUSED11
  FASL-OP-UNUSED12
  FASL-OP-QUOTE-POINTER
  FASL-OP-S-V-CELL
  FASL-OP-FUNCELL
  FASL-OP-CONST-PAGE
  FASL-OP-SET-PARAMETER
  FASL-OP-INITIALIZE-ARRAY
  FASL-OP-CHARACTER
  FASL-OP-UNUSED1
  FASL-OP-UNUSED2
  FASL-OP-UNUSED3
  FASL-OP-UNUSED4
  FASL-OP-UNUSED5
  FASL-OP-UNUSED6
  FASL-OP-STRING
  FASL-OP-STOREIN-ARRAY-LEADER
  FASL-OP-INITIALIZE-NUMERIC-ARRAY
  FASL-OP-REMOTE-VARIABLE
  FASL-OP-PACKAGE-SYMBOL
  FASL-OP-EVAL1
  FASL-OP-FILE-PROPERTY-LIST
  FASL-OP-REL-FILE
  FASL-OP-RATIONAL
  FASL-OP-COMPLEX
  FASL-OP-LARGE-INDEX
  FASL-OP-STOREIN-SYMBOL-CELL
  FASL-OP-VERSION-INFO
  fasl-op-k-compiled-function
  fasl-op-UNUSED13
  fasl-op-UNUSED14
  fasl-op-k-local-refs
  fasl-op-k-refs
  fasl-op-k-entry-points
  fasl-op-UNUSED15
  ;; No more FASL ops; this is enough to completely fill the field, sigh.
  ))

(DEFUN FASL-RESTART ()
  (SETQ LAST-FASL-FILE-FORMS NIL)
  (let ((fasl-op-limit (lsh 1 (byte-size %%fasl-group-type)))
        (fasl-op-count (length fasl-ops)))
    (when (> fasl-op-count fasl-op-limit)
      (error "Too many FASL ops defined.  Limit is ~D, but ~D are defined."
             fasl-op-limit fasl-op-count))
    ;; Initialize the fasl table if necessary
    (SETQ FASL-GROUP-DISPATCH (MAKE-ARRAY (LENGTH FASL-OPS) :AREA CONTROL-TABLES))
    (DO ((I 0 (1+ I))
         (L FASL-OPS (CDR L))
         (N (LENGTH FASL-OPS)))
        (( I N))
      (SETF (AREF FASL-GROUP-DISPATCH I) (CAR L)))))

#+(target falcon)
(eval-when (compile)
  (defmacro import-lambda-macro (name)
    `(setf (nlisp:macro-function ,name) (lisp:macro-function ,name))))

#+(target falcon)
(eval-when (compile)
  (import-lambda-macro 'loop)
  (import-lambda-macro 'ecase)
  (import-lambda-macro 'with-open-file)
  (import-lambda-macro 'dolist)
  (import-lambda-macro 'with-timeout))

(defvar *fasl-nibble-8bit* :unbound "If T, using 8-bit bytes.")

(defvar *fasl-nibble-peek* :unbound "Holds a PEEKED fasl-nibble-from-8bit")

(defvar *fasl-byte-size* 16. "Default byte size for new FASL files")

(DEFVAR FASL-TABLE)

;;; The stream which we are fasloading off of.
(DEFVAR FASL-STREAM)

(DEFVAR-RESETTABLE *READFILE-READ-FUNCTION* NIL NIL
  "If non-nil a function to use instead of READ")

(DEFVAR-RESETTABLE ROOTS-TO-COMPILE-IN NIL
 "If non-nil, a LIST of package names.  FASLOAD only loads if (PACKAGE-ROOT-STRING)
is one of these, otherwise, it forces a READFILE.")

;;; T if the stream supports :GET-INPUT-BUFFER (and therefore FASLOAD should use it)
(DEFVAR FASL-STREAM-BYPASS-P)

;;; The three values returned by the :GET-INPUT-BUFFER stream operation
;;; are put in these three values; the index and count are updated as the
;;; elements are read from the array.
(DEFVAR *FASL-STREAM-ARRAY*)
(DEFVAR *FASL-STREAM-INDEX*)
(DEFVAR *FASL-STREAM-COUNT*)


;;; Bound to the object to send PUTPROP messages to, for file properties, etc.
;;; Can be a generic pathname, can be an instance of PROPERTY-LIST-MIXIN,
;;; or in MINI it is a random function which accepts appropriate args.
(DEFVAR FASL-GENERIC-PLIST-RECEIVER NIL)

;;; Bound by FASL-GROUP to the length of the group being processed.
(DEFVAR FASL-GROUP-LENGTH)
;;; Bound by FASL-GROUP to the flag bit of the nibble starting the group.
(DEFVAR FASL-GROUP-FLAG)

;;; Bound by FASL-WHACK; set by a group to cause FASL-WHACK to return.
(DEFVAR FASL-RETURN-FLAG)

;;; String reused as buffer by FASL-OP-SYMBOL.
(DEFVAR FASL-OP-SYMBOL-TEMP-STRING NIL)

(DEFVAR LAST-FASL-FILE-PACKAGE :UNBOUND
  "After FASLOAD returns, holds the package the file was loaded into.")

(DEFVAR FASL-PACKAGE-SPECIFIED :UNBOUND
  "Holds the PKG argument to FASLOAD.")

(DEFVAR FASLOAD-FILE-PROPERTY-LIST-FLAG :UNBOUND
  "T within FASLOAD-INTERNAL means exit after loading the file attribute list.")

(DEFVAR FASL-FILE-PLIST :UNBOUND
  "Within FASLOAD, holds attribute list of this QFASL file.")

(DEFVAR DONT-CONVERT-DESTINATIONS :UNBOUND
  "Within FASLOAD, T if destination fields in fefs in this QFASL file are already converted.")

;; CDR-CODES permuted by Pace and KHS, 2/20/85.
(defvar dont-convert-cdr-codes :unbound
  "Within FASLOAD, T if cdr codes in fefs in this QFASL file are already converted.")

(defun map-to-new-cdr-code (x)
  ;; This is, conveniently, a no-op if you run it in a system with the old values.
  (symbol-value (nth (logand x 3) '(cdr-normal cdr-error cdr-nil cdr-next))))

(DEFVAR DEBUG-INFO-AREA (MAKE-AREA :NAME 'DEBUG-INFO-AREA
                                   :REGION-SIZE #o1000000
                                   :READ-ONLY T
                                   :VOLATILITY 0)
  "Debugging info and documentation of FEFs goes in this area.")


(DEFVAR-RESETTABLE PRINT-LOADED-FORMS NIL NIL
  "Set by :PRINT argument to LOAD.  Non-NIL means print the forms loaded.")

(DEFVAR-RESETTABLE ACCUMULATE-FASL-FORMS NIL NIL
  "Non-NIL means FASLOAD should compute LAST-FASL-FILE-FORMS.")

(DEFVAR LAST-FASL-FILE-FORMS :UNBOUND
  "FASLOAD sets this to a list of forms describing the file.
Only if ACCUMULATE-FASL-FORMS is non-NIL, this variable is set to a list of forms
which are equivalent to what was done by loading the file.")

;;; In this we accumulate a list of all forms evaluated at load time.
;;; Ordinary function defining is not included, nor is anything that is
;;; expected to record its action as a "definition" of any sort.
;;; This list is always created, and goes on the :RANDOM-FORMS property
;;; of the generic pathname.
(DEFVAR FASL-FILE-EVALUATIONS)

(DEFVAR MACRO-MISMATCH-FUNCTIONS NIL
  "List of functions fasloaded which had been compiled with different macro definitions.
Each element of this list looks like (USING-FUNCTION-NAME MACRO-NAME GENERIC-PATHNAME).")

(DEFVAR FASLOADED-FILE-TRUENAMES NIL
  "List of truenames of all fasl files loaded.
Files loaded by MINI are represented by strings.")

(defvar *fasl-table-free-list* nil)

(defun allocate-fasl-table ()
  (without-interrupts
    (when (null *fasl-table-free-list*)
      (push (make-array length-of-fasl-table
                        :area fasl-table-area
                        :type 'art-q-list
                        :fill-pointer fasl-table-working-offset)
            *fasl-table-free-list*))
    (setq fasl-table (car *fasl-table-free-list*))
    (setf (fill-pointer fasl-table) fasl-table-working-offset)
    (setq *fasl-table-free-list* (cdr *fasl-table-free-list*))))

(defun return-fasl-table ()
  (without-interrupts
    (when (typep fasl-table 'array)
      (push fasl-table *fasl-table-free-list*)
      (setq fasl-table nil))))

;; T => trace nibbles, only if bypassing.
;; (so we don't trace what is done through mini)
(DEFVAR FASL-TRACE-LOSSAGE NIL)

(defsubst fasl-nibble ()
  (if *fasl-nibble-8bit*
      (fasl-nibble-from-8bit)
    (fasl-nibble-from-16bit)))

(defsubst fasl-nibble-peek ()
  (if *fasl-nibble-8bit*
      (fasl-nibble-from-8bit-peek)
    (fasl-nibble-from-16bit-peek)))

(defvar test 0)
(defun get-fasl-buffer ()
  (when *fasl-stream-array*
    (send fasl-stream :advance-input-buffer))
  (multiple-value-setq (*fasl-stream-array* *fasl-stream-index* *fasl-stream-count*)
    (send fasl-stream :get-input-buffer)))

(defsubst next-fasl-byte ()
  (unless (plusp *fasl-stream-count*)
    (get-fasl-buffer))
  (prog1 (aref *fasl-stream-array* *fasl-stream-index*)
         (incf *fasl-stream-index*)
         (decf *fasl-stream-count*)))

(DEFUN FASL-NIBBLE-FROM-8BIT-SLOW ()
  (COND (FASL-STREAM-BYPASS-P
         (let ((nib (dpb (next-fasl-byte) (byte 8. 0.) 0)))
           (setq nib (dpb (next-fasl-byte) (byte 8. 8.)
                          nib))
           nib))
         (T (incf test 2)
            (let ((num1 (tyi fasl-stream)))
              (let ((nib (dpb num1 (byte 8. 0.) 0)))
                (let ((num2 (tyi fasl-stream)))
                  (dpb num2 (byte 8. 8.) nib)))))))

;;; Look ahead at the next nibble without discarding it.
;(DEFUN FASL-NIBBLE-FROM-8BIT-PEEK ()
;  (IF FASL-STREAM-BYPASS-P
;      (PROG1 (FASL-NIBBLE-FROM-8BIT)
;            (SETQ *FASL-STREAM-COUNT* (+ 2 *FASL-STREAM-COUNT*))
;            (SETQ *FASL-STREAM-INDEX* (- *FASL-STREAM-INDEX*) 2))
;    (LET ((TEM (SEND FASL-STREAM :TYI)))
;      (SEND FASL-STREAM :UNTYI TEM)
;      TEM)))

;;; This is the function which gets a 16-bit "nibble" from the fasl stream.
;; This is the fastest way to do this.
(defun fasl-nibble-from-8bit ()
  (cond (*fasl-nibble-peek*
         (prog1 *fasl-nibble-peek*
                (setq *fasl-nibble-peek* ())))
        ((> *fasl-stream-count* 1)              ;Can we gobble two bytes at once?
         (let* ((ary *fasl-stream-array*)
                (idx *fasl-stream-index*))
           (prog1 (dpb (AREF ary (1+ idx)) (byte 8. 8.) (AREF ary idx))
                  (incf *fasl-stream-index* 2)
                  (DECF *FASL-STREAM-COUNT* 2))))
        (t (fasl-nibble-from-8bit-slow))))

;Previous definition of above, hacked for speed.
;(defun FASL-NIBBLE-FROM-8BIT ()
;  (IF (PLUSP *FASL-STREAM-COUNT*)
;      (PROG1 (AREF *FASL-STREAM-ARRAY* *FASL-STREAM-INDEX*)
;            (WHEN FASL-TRACE-LOSSAGE
;              (PRINT *FASL-STREAM-INDEX*)
;              (PRIN1 (AREF *FASL-STREAM-ARRAY* *FASL-STREAM-INDEX*)))
;            (INCF *FASL-STREAM-INDEX*)
;            (DECF *FASL-STREAM-COUNT*))
;    (FASL-NIBBLE-FROM-8BIT-SLOW)))

(defun fasl-nibble-from-8bit-peek ()
 (or *fasl-nibble-peek*
     (setq *fasl-nibble-peek* (fasl-nibble-from-8bit))))

(defun FASL-NIBBLE-from-16bit ()
  (if *fasl-nibble-peek*
      (prog1 *fasl-nibble-peek*
             (setq *fasl-nibble-peek* nil))
    (IF (NOT (MINUSP (DECF *FASL-STREAM-COUNT*)))
        (AREF *FASL-STREAM-ARRAY* (1- (INCF *FASL-STREAM-INDEX*)))
      (INCF *FASL-STREAM-COUNT*)                ;Prematurely decremented above.
      (FASL-NIBBLE-SLOW-from-16bit))))

(DEFUN FASL-NIBBLE-SLOW-from-16bit ()
  (COND (*fasl-nibble-peek*
         (prog1 *fasl-nibble-peek*
                (setq *fasl-nibble-peek* nil)))
        (FASL-STREAM-BYPASS-P
         (next-fasl-byte))
        (T (SEND FASL-STREAM :TYI))))

(DEFUN FASL-NIBBLE-from-16bit-PEEK ()
  (IF FASL-STREAM-BYPASS-P
      (or *fasl-nibble-peek*
          (setq *fasl-nibble-peek* (fasl-nibble)))
    (LET ((TEM (SEND FASL-STREAM :TYI)))
      (SEND FASL-STREAM :UNTYI TEM)
      TEM)))


(DEFUN READFILE-INTERNAL (*STANDARD-INPUT* PKG NO-MSG-P)
  (LET* ((FILE-ID (SEND *STANDARD-INPUT* :INFO))
         (PATHNAME (SEND *STANDARD-INPUT* :PATHNAME))
         (GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
         (*PACKAGE* *PACKAGE*)
         (FDEFINE-FILE-DEFINITIONS)
         (FDEFINE-FILE-PATHNAME GENERIC-PATHNAME))
    (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME *STANDARD-INPUT*)
    ;; Enter appropriate environment for the file
    (MULTIPLE-VALUE-BIND (VARS VALS)
        (FS:FILE-ATTRIBUTE-BINDINGS
          (IF PKG
              ;; If package is specified, don't look up the file's package
              ;; since that might ask the user a spurious question.
              (LET ((PLIST (COPY-LIST (SEND GENERIC-PATHNAME :PROPERTY-LIST))))
                (REMF PLIST ':PACKAGE)
                (LOCF PLIST))
            GENERIC-PATHNAME))
      (PROGV VARS VALS
        ;; If package overridden, do so.  *PACKAGE* is bound in any case.
        (COND (PKG (SETQ *PACKAGE* (PKG-FIND-PACKAGE PKG () *package*))) ;added () and *package*
              (NO-MSG-P)                        ;And tell user what it was unless told not to
              (T (FORMAT *QUERY-IO* "~&Loading ~A into package ~A~%" PATHNAME *PACKAGE*)))
        (DO ((EOF '(()))
             ;; If the file contains a SETQ, don't alter what package we recorded loading in
             (*PACKAGE* *PACKAGE*)
             (FORM))
    ;Unfortunately, we have to use ZL:READ here, because the analogous thing in compile file
    ; might call READ-CHECK-INDENTATION which takes args the old way.  There should be a
    ; way to check indentation here too.
            ((EQ (SETQ FORM (FUNCALL (OR *READFILE-READ-FUNCTION* #'ZL:READ)
                                     *STANDARD-INPUT* EOF)) EOF))
          (IF PRINT-LOADED-FORMS
              (PRINT (eval FORM))
            (EVAL FORM)))
        (SET-FILE-LOADED-ID PATHNAME FILE-ID *PACKAGE*)
        (RECORD-FILE-DEFINITIONS PATHNAME (NREVERSE FDEFINE-FILE-DEFINITIONS))
        PATHNAME))))

;;; This is the function which provides entry to fasload.
;;; NOTE WELL: If you change this, change MINI-FASLOAD too!
;;; This is the function which provides entry to fasload.
;;; NOTE WELL: If you change this, change MINI-FASLOAD too!
(DEFUN FASLOAD (FILE-NAME &OPTIONAL PKG NO-MSG-P)
  "Load a binary file.  PKG specifies package to load in.
NO-MSG-P inhibits the message announcing that the loading is taking place."
  (LET* ((DEFAULTED-NAME (FS:MERGE-PATHNAME-DEFAULTS FILE-NAME FS:LOAD-PATHNAME-DEFAULTS NIL))
         (DEFAULT-BINARY-FILE-TYPE (PATHNAME-DEFAULT-BINARY-FILE-TYPE DEFAULTED-NAME)))
    (WITH-OPEN-FILE (STREAM (FS:MERGE-AND-SET-PATHNAME-DEFAULTS FILE-NAME
                                                                FS:LOAD-PATHNAME-DEFAULTS
                                                                DEFAULT-BINARY-FILE-TYPE)
                            :DIRECTION :INPUT
                            :CHARACTERS NIL
                            :BYTE-SIZE *fasl-byte-size*)
      (FASLOAD-INTERNAL STREAM PKG NO-MSG-P))))


(DEFUN FASLOAD-INTERNAL (FASL-STREAM PKG NO-MSG-P)
  (LET* ((PATHNAME (SEND FASL-STREAM :PATHNAME))
         (size (or (send-if-handles fasl-stream :byte-size)
                   (send fasl-stream :get :byte-size)))
         *fasl-nibble-peek*
         (*fasl-nibble-8bit* (ecase size
                               (8. t)
                               (16. nil)))
         (FDEFINE-FILE-PATHNAME
           (IF (STRINGP PATHNAME) PATHNAME
             (SEND PATHNAME :GENERIC-PATHNAME)))
         (PATCH-SOURCE-FILE-NAMESTRING)
         (FDEFINE-FILE-DEFINITIONS)
         (FASL-GENERIC-PLIST-RECEIVER (SEND FASL-STREAM :GENERIC-PATHNAME))
         (FILE-ID (SEND FASL-STREAM :INFO))
         (FASL-STREAM-BYPASS-P (OPERATION-HANDLED-P FASL-STREAM :GET-INPUT-BUFFER))
         *FASL-STREAM-ARRAY* *FASL-STREAM-INDEX* (*FASL-STREAM-COUNT* 0)
         (FASLOAD-FILE-PROPERTY-LIST-FLAG NIL)
         (FASL-PACKAGE-SPECIFIED PKG)
         ;(last-fasl-file-forms nil)
         ;last-fasl-file-package
         FASL-FILE-EVALUATIONS
         FASL-FILE-PLIST
         fasl-internal-version
         DONT-CONVERT-DESTINATIONS
         dont-convert-cdr-codes
         (FASL-TABLE NIL))
    ;; Set up the environment
    (FASL-START)
    (PUSH (CAR (SEND FASL-STREAM :INFO)) FASLOADED-FILE-TRUENAMES)
    ;; Start by making sure the file type in the first word is really SIXBIT/QFASL/.
    (flet ((version-token ()  (or (if *cold-booting* (send fasl-stream :tyi) (fasl-nibble))
                                  (ferror "Premature EOF on FASL stream for ~A" pathname))))
      (LET ((W1 (version-token)) (W2 (version-token)))
        (OR (AND (= W1 #o143150) (= W2 #o71660))
            ;;;+++Debugging
            (format *error-output* "~&>>> Tokens = #o~O (~:*~@C) , #o~O (~:*~@C)" w1 w2)
            (FERROR "~A is not a QFASL file" PATHNAME))))
    (WHEN (= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-version-info)
      (check-version-info)
      (save-version-info))
    (SEND FASL-GENERIC-PLIST-RECEIVER :REMPROP :MACROS-EXPANDED)
    ;; Read in the file property list before choosing a package.
    (WHEN (= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-FILE-PROPERTY-LIST)
      (FASL-FILE-PROPERTY-LIST)
      (let ((compile-in-roots-prop (get (locf fasl-file-plist) :compile-in-roots)))
        (cond ((and compile-in-roots-prop
                    (not (cl:member (si:package-root-name (if pkg pkg *package*))
                                    compile-in-roots-prop
                                    :test 'string-equal)))
       ;gee, we're not supposed to load compiled code into this heirarchy.  Force READFILE.
               (close fasl-stream)
               (return-from fasload-internal
                 (readfile (fs:merge-pathname-defaults (send pathname :new-type :unspecified)
                                                       nil :LISP) pkg no-msg-p)))
              (t
               (let* ((fasd-data (OR (GET (LOCF FASL-FILE-PLIST) :FASD-DATA)
                                     (GET (LOCF FASL-FILE-PLIST) :COMPILE-DATA)))
                      (fasd-data-plist (sixth fasd-data)))
                 (let ((version (fourth fasd-data)))
                   (and (not (null version))
                        (< version 98.)
                        (cerror "Try to load it anyway"
                                "~This QFASL file was written from system version ~D~%~
                            and may be dangerous to load into this system.~"
                                version))
                   (when fasd-data-plist
                     (setq dont-convert-destinations
                           (if (> version 98.) t
                             (getf fasd-data-plist 'compiler::new-destinations)))
                     (setq dont-convert-cdr-codes
                           (getf fasd-data-plist 'compiler::new-cdr-codes)))))))))
    ;; Enter appropriate environment defined by file property list
    (MULTIPLE-VALUE-BIND (VARS VALS)
        (IF (NOT (STRINGP PATHNAME))
            (FS:FILE-ATTRIBUTE-BINDINGS
              (IF PKG
                  ;; If package is specified, don't look up the file's package
                  ;; since that might ask the user a spurious question.
                  (LET ((PLIST (COPY-LIST (SEND FDEFINE-FILE-PATHNAME :PROPERTY-LIST))))
                    (REMF PLIST ':PACKAGE)
                    (LOCF PLIST))
                FDEFINE-FILE-PATHNAME)))
      (PROGV VARS VALS
        (LET ((*PACKAGE* (PKG-FIND-PACKAGE (OR PKG *PACKAGE*) :ASK)))
          (LET ((*PACKAGE* *PACKAGE*))
            (OR PKG
                ;; Don't want this message for a REL file
                ;; since we don't actually know its package yet
                ;; and it might have parts in several packages.
                ;; (WHEN (= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-version-info)
                ;;   (check-version-info))
                ;; The above lines have been added in other places where files are loaded.
                ;; I don't know if they are needed here or not, since I don't think that this
                ;; code works anyway -- Jim
                (= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-REL-FILE)
                NO-MSG-P
                (FORMAT *QUERY-IO* "~&Loading ~A into package ~A~%" PATHNAME *PACKAGE*))
            (SETQ LAST-FASL-FILE-PACKAGE *PACKAGE*)
            (FASL-TOP-LEVEL))                   ;load it.
          (SEND FASL-GENERIC-PLIST-RECEIVER :PUTPROP FASL-FILE-EVALUATIONS ':RANDOM-FORMS)
          (RECORD-FILE-DEFINITIONS PATHNAME (NREVERSE FDEFINE-FILE-DEFINITIONS)
                                   T FASL-GENERIC-PLIST-RECEIVER)
          (SET-FILE-LOADED-ID PATHNAME FILE-ID *PACKAGE*))))
    (SETQ *FASL-STREAM-ARRAY* NIL)
    (SETQ LAST-FASL-FILE-FORMS (NREVERSE LAST-FASL-FILE-FORMS))
    PATHNAME))

(DEFUN QFASL-FILE-PLIST (FILE)
  "Return the attribute list of a compiled file."
  (WITH-OPEN-FILE (STREAM FILE :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE *fasl-byte-size*)
    (QFASL-STREAM-PROPERTY-LIST STREAM)))

(DEFUN QFASL-STREAM-PROPERTY-LIST (FASL-STREAM)
  (LET ((FASL-GENERIC-PLIST-RECEIVER (MAKE-INSTANCE 'SI:PROPERTY-LIST-MIXIN))
        (FASL-STREAM-BYPASS-P (OPERATION-HANDLED-P FASL-STREAM :GET-INPUT-BUFFER))
        *FASL-STREAM-ARRAY* *FASL-STREAM-INDEX* (*FASL-STREAM-COUNT* 0)
        ;; smh 12aug88
        (*fasl-nibble-peek* ())
        (*fasl-nibble-8bit* (ecase (or (send-if-handles fasl-stream :byte-size)
                                       (send fasl-stream :get :byte-size))
                              (8. t)
                              (16. nil)))
        (FASLOAD-FILE-PROPERTY-LIST-FLAG NIL)
        (FASL-TABLE NIL))
    ;; Set up the environment
    (FASL-START)
    ;; Start by making sure the file type in the first word is really SIXBIT/QFASL/.
    (LET ((W1 (FASL-NIBBLE))
          (W2 (FASL-NIBBLE)))
      (OR (AND (= W1 #o143150) (= W2 #o71660))
          (FERROR "~A is not a QFASL file" (SEND FASL-STREAM :PATHNAME))))
    (WHEN (= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-version-info)
      (check-version-info)
      (save-version-info))
    ;; Read in the file property list before choosing a package.
    (COND ((= (LOGAND (FASL-NIBBLE-PEEK) %FASL-GROUP-TYPE) FASL-OP-FILE-PROPERTY-LIST)
           (FASL-FILE-PROPERTY-LIST)))
    (return-fasl-table)
;   (AND FASL-TABLE (RETURN-ARRAY (PROG1 FASL-TABLE (SETQ FASL-TABLE NIL))))
    (SEND FASL-GENERIC-PLIST-RECEIVER :PROPERTY-LIST)))


(DEFUN RECORD-FILE-DEFINITIONS (ACCESS-PATHNAME DEFINITIONS &OPTIONAL (WHOLE-FILE T)
                                GENERIC-PATHNAME
                                &AUX (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  "Update the list of functions defined in the file ACCESS-PATHNAME.
DEFINITIONS is a list of new definitions.  WHOLE-FILE-P says flush any old ones.
If any methods used to be defined in that file but aren't any longer,
offer to undefine them.  You can specify GENERIC-PATHNAME to save time,
or let it be computed from ACCESS-PATHNAME.

The elements of DEFINITIONS look like (OBJECT-DEFINED . DEFINITION-TYPE).
Usually DEFINITION-TYPE is DEFUN and OBJECT-DEFINED is a function spec."
  (UNLESS GENERIC-PATHNAME
    (SETQ GENERIC-PATHNAME
          (IF (TYPEP ACCESS-PATHNAME 'INSTANCE)
              (SEND ACCESS-PATHNAME :GENERIC-PATHNAME)
            'MINI-PLIST-RECEIVER)))             ;In MINI, and flavors not in use yet.
  (LET* ((ALIST-ELEM (ASSQ *PACKAGE* (SEND GENERIC-PATHNAME :GET ':DEFINITIONS)))
         (OLD-DEFINITIONS (CDR ALIST-ELEM))
         OLD-FUN)
;    (LOCALLY
      (DECLARE (SPECIAL *UNANALYZED-FILES*))
      (IF (NOT (VARIABLE-BOUNDP *UNANALYZED-FILES*))
          (SETQ *UNANALYZED-FILES* NIL))
      (IF (NOT (MEMQ GENERIC-PATHNAME *UNANALYZED-FILES*))
          ;;I'm not sure what sort of locality improvement is expected here.
          (SETQ *UNANALYZED-FILES* (COPY-LIST (CONS GENERIC-PATHNAME *UNANALYZED-FILES*))))
;)
    (IF (NOT WHOLE-FILE)
        (SETQ DEFINITIONS (NUNION-EQUAL OLD-DEFINITIONS DEFINITIONS))
      ;; Make the data structure compact for paging efficiency.
      (SETQ DEFINITIONS (COPY-TREE DEFINITIONS)))
    (IF ALIST-ELEM
        (SETF (CDR ALIST-ELEM) DEFINITIONS)
        (SEND GENERIC-PATHNAME :PUSH-PROPERTY (CONS *PACKAGE* DEFINITIONS) :DEFINITIONS))
    (IF (NOT WHOLE-FILE)
        NIL
      ;; If we are doing the whole file, offer to undefine any methods deleted from the file.
      (DO ((DEFS DEFINITIONS (CDR DEFS))) ((NULL DEFS))
        (SETF (CAR DEFS) (COPY-LIST (CAR DEFS))))
      (UNLESS (SEND GENERIC-PATHNAME :GET :PATCH-FILE)
        (DOLIST (OLD-DEF OLD-DEFINITIONS)
          (AND (CONSP OLD-DEF)
               (EQ (CDR OLD-DEF) 'DEFUN)
               (SETQ OLD-FUN (CAR OLD-DEF))
               (CONSP OLD-FUN)
               (EQ (CAR OLD-FUN) :METHOD)
               ;; Leave out combined methods, which may have been present
               ;; due to COMPILE-FLAVOR-METHODS.  They are handled automatically.
               (OR (= (LENGTH OLD-FUN) 3)
                   (NOT (MEMQ (CADDR OLD-FUN) '(:COMBINED FASLOAD-COMBINED))))
               (NOT (MEMBER-EQUAL OLD-DEF DEFINITIONS))
               (FDEFINEDP OLD-FUN)
               ;; Detect automatic methods defined by a DEFFLAVOR that is still present.
               (MULTIPLE-VALUE-BIND (NAME TYPE)
                   (SI:FUNCTION-PARENT OLD-FUN)
                 (NOT (MEMBER-EQUAL (CONS NAME TYPE) DEFINITIONS)))
               (LET* ((FILES (CDR (ASSQ 'DEFUN (GET-ALL-SOURCE-FILE-NAMES OLD-FUN))))
                      (FILES-1 FILES))
                 (DO () ((NOT (AND FILES-1 (SEND (CAR FILES-1) :GET :PATCH-FILE))))
                   (POP FILES-1))
                 (AND (EQ (CAR FILES-1) GENERIC-PATHNAME)
                      (PROGN
                        (IF (EQ FILES FILES-1)
                            (FORMAT *QUERY-IO*
                                    "~&File ~A no longer contains a definition of ~S.~%"
                                    ACCESS-PATHNAME OLD-FUN)
                          (FORMAT *QUERY-IO*
                                  "~&File ~A no longer contains a definition of ~S.
It was more recently redefined by patch file ~A, but no other non-patch file.~%"
                                  ACCESS-PATHNAME OLD-FUN
                                  (SEND (CAR FILES) :SOURCE-PATHNAME)))
                        (PROG1 (WITH-TIMEOUT ((* 60. 60.) (FORMAT *QUERY-IO* " ... Yes by timeout.") T)
                                 (Y-OR-N-P "Undefine it? (60 sec timeout for Yes) "))
                               (TERPRI *QUERY-IO*)))))
               (FUNDEFINE OLD-FUN)))
        (and (variable-boundp *analyze-files-when-loaded*)
             *analyze-files-when-loaded*
             (fboundp 'analyze-file)
             (analyze-file access-pathname))))))


(DEFUN FASL-START ()
  (SETQ LAST-FASL-FILE-FORMS NIL)
  ;; Initialize the fasl table if necessary
  (WHEN (NOT (VARIABLE-BOUNDP FASL-GROUP-DISPATCH))
    (SETQ FASL-GROUP-DISPATCH (MAKE-ARRAY (LENGTH FASL-OPS) :AREA CONTROL-TABLES))
    (DO ((I 0 (1+ I))
         (L FASL-OPS (CDR L))
         (N (LENGTH FASL-OPS)))
        (( I N))
      (SETF (AREF FASL-GROUP-DISPATCH I) (CAR L)))))

;(DEFUN FASL-OP-REL-FILE ()
;  (MULTIPLE-VALUE (*FASL-STREAM-ARRAY* *FASL-STREAM-INDEX* *FASL-STREAM-COUNT*)
;    (QFASL-REL:REL-LOAD-STREAM FASL-STREAM
;                              *FASL-STREAM-ARRAY*
;                              *FASL-STREAM-INDEX*
;                              *FASL-STREAM-COUNT*
;                              FASL-PACKAGE-SPECIFIED)))

;;; FASL-GENERIC-PATHNAME-PLIST, FASL-STREAM, FASL-SOURCE-GENERIC-PATHNAME implicit arguments
(DEFUN FASL-FILE-PROPERTY-LIST ()
  ;; File property lists are all FASDed and FASLed in the keyword package, so
  ;; that what you FASD is what you FASL!
  (LET ((*PACKAGE* PKG-KEYWORD-PACKAGE)
        (FASLOAD-FILE-PROPERTY-LIST-FLAG T))
    (FASL-WHACK-SAVE-FASL-TABLE)))

(DEFUN FASL-OP-FILE-PROPERTY-LIST ()
  (LET ((PLIST (FASL-NEXT-VALUE)))
    (SETQ FASL-FILE-PLIST PLIST)
    ;; Make the source file really correspond to where things were compiled from.
    (AND FDEFINE-FILE-PATHNAME
         (LET ((SOURCE-PATHNAME (GETF PLIST :SOURCE-FILE-GENERIC-PATHNAME)))
           (COND ((AND SOURCE-PATHNAME (NOT (STRINGP FDEFINE-FILE-PATHNAME)))
                  ;; If opened via a logical host, should record with that host in, even if
                  ;; not compiled that way.
                  (SETQ SOURCE-PATHNAME (SEND FDEFINE-FILE-PATHNAME
                                              :BACK-TRANSLATED-PATHNAME SOURCE-PATHNAME))
                  (SETQ FDEFINE-FILE-PATHNAME (SEND SOURCE-PATHNAME :GENERIC-PATHNAME))
                  (SETQ FASL-GENERIC-PLIST-RECEIVER FDEFINE-FILE-PATHNAME)))))
    (DO ((PLIST PLIST (CDDR PLIST)))
        ((NULL PLIST))
      (SEND FASL-GENERIC-PLIST-RECEIVER :PUTPROP (CADR PLIST) (CAR PLIST))
;      (WHEN PRINT-LOADED-FORMS
;       (PRINT `(SEND ',FASL-GENERIC-PLIST-RECEIVER :PUTPROP
;                     ',(CADR PLIST) ',(CAR PLIST))))
      (WHEN ACCUMULATE-FASL-FORMS
        (PUSH `(SEND ',FASL-GENERIC-PLIST-RECEIVER :PUTPROP
                     ',(CADR PLIST) ',(CAR PLIST))
              LAST-FASL-FILE-FORMS))))
  (AND FASLOAD-FILE-PROPERTY-LIST-FLAG (SETQ FASL-RETURN-FLAG T))) ;Cause FASL-WHACK to return

;;; A call to this function is written at the end of each QFASL file by the compiler.
(DEFUN FASL-RECORD-FILE-MACROS-EXPANDED (FILE-MACROS-EXPANDED)
  ;; For files in cold load, this will be called at cold-load startup time.
  ;; For now, do nothing, just avoid bombing out.
  (WHEN FASL-GENERIC-PLIST-RECEIVER
    (SEND FASL-GENERIC-PLIST-RECEIVER :PUTPROP
          FILE-MACROS-EXPANDED :MACROS-EXPANDED)
    (CHECK-MACROS-EXPANDED FILE-MACROS-EXPANDED NIL)))

;;; Since the :MACROS-EXPANDED property tells us what happened.
(defprop fasl-record-file-macros-expanded t qfasl-dont-record)

(DEFVAR INHIBIT-MACRO-MISMATCH-WARNINGS 'BUILD-SYSTEM
  "Non-NIL inhibits warnings about loading functions compiled with different versions of macros.")

;;; The above variable should be off during initial system loadup.
(ADD-INITIALIZATION 'SET-INHIBIT-MACRO-MISMATCH-WARNINGS
                    '(AND (EQ INHIBIT-MACRO-MISMATCH-WARNINGS 'BUILD-SYSTEM)
                          (NEQ *TERMINAL-IO* COLD-LOAD-STREAM)
                          (SETQ INHIBIT-MACRO-MISMATCH-WARNINGS NIL
                                #|*analyze-files-when-loaded* t|#))
                    '(:BEFORE-COLD :NORMAL))

(DEFUN CHECK-MACROS-EXPANDED (MACRO-RECORD-LIST FUNCTION)
  "Look at a list of macros and sxhashes; report any whose sxhashes don't match."
  (LET ((DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
    (DOLIST (MACRO MACRO-RECORD-LIST)
      (AND (CONSP MACRO)
           (FDEFINEDP (CAR MACRO))
           (FBOUNDP 'COMPILER::EXPR-SXHASH)
           (LET ((CURRENT-SXHASH (COMPILER::EXPR-SXHASH (CAR MACRO))))
             (AND CURRENT-SXHASH (CADR MACRO)
                  (NEQ (CADR MACRO) CURRENT-SXHASH)))
           (PUSH (LIST FUNCTION (CAR MACRO) (SEND FASL-STREAM :TRUENAME))
                 MACRO-MISMATCH-FUNCTIONS)
           (NOT INHIBIT-MACRO-MISMATCH-WARNINGS)
           (FORMAT:OUTPUT *ERROR-OUTPUT*
             (FRESH-LINE)
             "Warning: "
             (IF FUNCTION (PRIN1 FUNCTION) (PRINC (SEND FASL-STREAM :TRUENAME)))
             " was compiled with a different version of macro "
             (PRIN1 (CAR MACRO))
             (TERPRI))))))

;;; The :FILE-ID-PACKAGE-ALIST property of a file-symbol is an a-list
;;; of packages and FILE-ID's for the version of that file loaded into
;;; that package.  The FILE-ID is in the CADR rather the CDR, for expansibility.

;;; Record the fact that a file has been loaded (in a certain package)
(DEFUN SET-FILE-LOADED-ID (ACCESS-PATHNAME FILE-ID PKG &AUX GENERIC-PATHNAME TEM)
  (SETQ GENERIC-PATHNAME
        (IF (TYPEP ACCESS-PATHNAME 'INSTANCE)
            (SEND ACCESS-PATHNAME :GENERIC-PATHNAME)
          'MINI-PLIST-RECEIVER))                ;In MINI, and flavors not in use yet.
  (COND ((SETQ TEM (ASSQ PKG (SEND GENERIC-PATHNAME :GET :FILE-ID-PACKAGE-ALIST)))
         (SETF (CADR TEM) FILE-ID)
         (SETF (CADDR TEM) ACCESS-PATHNAME))
        (T
         (SEND GENERIC-PATHNAME :PUSH-PROPERTY
                                (LIST PKG FILE-ID ACCESS-PATHNAME)
                                :FILE-ID-PACKAGE-ALIST))))

;;; Get the version of a file that was loaded into a particular package, NIL if never loaded.
;;; If the package is given as NIL, the file's :PACKAGE property is used.
(DEFUN GET-FILE-LOADED-ID (ACCESS-PATHNAME PKG &AUX GENERIC-PATHNAME)
  (SETQ GENERIC-PATHNAME
        (IF (TYPEP ACCESS-PATHNAME 'INSTANCE)
            (SEND ACCESS-PATHNAME :GENERIC-PATHNAME)
            'MINI-PLIST-RECEIVER))      ;In MINI, and flavors not in use yet.
  (AND (NULL PKG)
       (SETQ PKG (SEND GENERIC-PATHNAME :GET ':PACKAGE)))
  (CADR (LET ((PROP (SEND GENERIC-PATHNAME :GET ':FILE-ID-PACKAGE-ALIST)))
          (IF PKG (ASSQ (PKG-FIND-PACKAGE PKG nil *package*) PROP) (CAR PROP)))))

;;; This is the top-level loop of fasload, a separate function so
;;; that the file-opening and closing are separated out.
;;; The special variable FASL-STREAM is an implicit argument.
(DEFUN FASL-TOP-LEVEL ()
  (IF FASL-TABLE
      (INITIALIZE-FASL-TABLE))
  (DO ()
      ((EQ (FASL-WHACK) 'EOF)))
  T)

;;; This function processes one "whack" (independent section) of a fasl file.
(DEFUN FASL-WHACK ()
  (PROG1 (FASL-WHACK-SAVE-FASL-TABLE)
         (unless (null fasl-table) (return-fasl-table))))
;        (AND FASL-TABLE (RETURN-ARRAY (PROG1 FASL-TABLE (SETQ FASL-TABLE NIL))))))

(DEFUN FASL-WHACK-SAVE-FASL-TABLE (&AUX FASL-RETURN-FLAG)
; (RESET-TEMPORARY-AREA FASL-TABLE-AREA)
  (COND ((NULL FASL-TABLE)
         (allocate-fasl-table)
;        (SETQ FASL-TABLE (MAKE-ARRAY LENGTH-OF-FASL-TABLE
;                                     :AREA FASL-TABLE-AREA
;                                     :TYPE 'ART-Q-LIST
;                                     :FILL-POINTER FASL-TABLE-WORKING-OFFSET))
         (INITIALIZE-FASL-TABLE)))
; (FASL-SET-MESA-EXIT-BASE)
  (DO () (FASL-RETURN-FLAG)
    (FASL-GROUP))
  FASL-RETURN-FLAG)

(DEFUN INITIALIZE-FASL-TABLE ()
  (SETF (AREF FASL-TABLE FASL-SYMBOL-HEAD-AREA) NR-SYM)
  (SETF (AREF FASL-TABLE FASL-SYMBOL-STRING-AREA) P-N-STRING)
; (SETF (AREF FASL-TABLE FASL-OBARRAY-POINTER) OBARRAY)
  (SETF (AREF FASL-TABLE FASL-ARRAY-AREA) WORKING-STORAGE-AREA)
  (SETF (AREF FASL-TABLE FASL-FRAME-AREA) MACRO-COMPILED-PROGRAM)
  (SETF (AREF FASL-TABLE FASL-LIST-AREA) WORKING-STORAGE-AREA)
  (SETF (AREF FASL-TABLE FASL-TEMP-LIST-AREA) FASL-TEMP-AREA))

;;; Process one "group" (a single operation)
(DEFUN FASL-GROUP ()
  (LET (FASL-GROUP-FLAG
        FASL-GROUP-BITS
        FASL-GROUP-TYPE
        FASL-GROUP-LENGTH)
    (WHEN FASL-TRACE-LOSSAGE (PRINT 'GROUP))
    (SETQ FASL-GROUP-BITS (FASL-NIBBLE))
    (WHEN (ZEROP (LOGAND FASL-GROUP-BITS %FASL-GROUP-CHECK))
      (FERROR "Invalid QFASL file: first nibble of group is missing the check bit."))
    (SETQ FASL-GROUP-FLAG (NOT (ZEROP (LOGAND FASL-GROUP-BITS %FASL-GROUP-FLAG))))
    (SETQ FASL-GROUP-LENGTH (LDB %%FASL-GROUP-LENGTH FASL-GROUP-BITS))
    (WHEN (= FASL-GROUP-LENGTH #o377)
      (SETQ FASL-GROUP-LENGTH (FASL-NIBBLE)))
    (SETQ FASL-GROUP-TYPE (LOGAND FASL-GROUP-BITS %FASL-GROUP-TYPE))
    (FUNCALL (AREF FASL-GROUP-DISPATCH FASL-GROUP-TYPE))))

;;; Get next nibble out of current group
(DEFUN FASL-NEXT-NIBBLE ()
  (IF (not (MINUSP (SETQ FASL-GROUP-LENGTH (1- FASL-GROUP-LENGTH))))
      (FASL-NIBBLE)
    (FERROR "Invalid QFASL file: not enough nibbles in this group.")))

;;; Get next value for current group.  Works by recursively evaluating a group.
(DEFUN FASL-NEXT-VALUE ()
  (AREF FASL-TABLE (FASL-GROUP)))

(DEFUN FASL-STORE-EVALED-VALUE (V)
  (ASET V FASL-TABLE FASL-EVALED-VALUE)
  FASL-EVALED-VALUE)

;;;; FASL ops

(DEFUN FASL-OP-ERR ()
  (FERROR "Invalid QFASL file: group code 0 encountered."))

(DEFUN FASL-OP-NOOP ()
  0)

(DEFUN FASL-OP-INDEX ()
  (FASL-NEXT-NIBBLE))

(DEFUN FASL-OP-LARGE-INDEX ()
 (DPB (FASL-NEXT-NIBBLE) (BYTE 8. 16.) (FASL-NEXT-NIBBLE)))

(DEFUN FASL-OP-STRING ()
  (FASL-OP-SYMBOL T))

(DEFMACRO FASL-OP-SYMBOL-GET-STRING ()
  '(OR
     (DO (OLD)
         ((%STORE-CONDITIONAL (LOCF FASL-OP-SYMBOL-TEMP-STRING)
                              (SETQ OLD FASL-OP-SYMBOL-TEMP-STRING)
                              NIL)
          OLD))
     (MAKE-STRING #o1000 :FILL-POINTER 0)))

(DEFUN FASL-OP-SYMBOL (&OPTIONAL STRING-FLAG &AUX STRING)
  ;; Get reusable string to accumulate data in.
  (SETQ STRING (FASL-OP-SYMBOL-GET-STRING))
  ;; Make sure it's long enough, though.
  (UNLESS ( (ARRAY-LENGTH STRING) (* 2 FASL-GROUP-LENGTH))
    (SETQ STRING (MAKE-STRING (MAX (* 2 FASL-GROUP-LENGTH)
                                   (* 2 (ARRAY-LENGTH STRING)))
                              :FILL-POINTER 0)))
  (SETF (FILL-POINTER STRING) 0)
  ;; Read in the contents.
  (DO ((NIB))
      ((ZEROP FASL-GROUP-LENGTH))
    (SETQ NIB (FASL-NEXT-NIBBLE))               ;Two characters, packed.
    (VECTOR-PUSH NIB STRING)
    (UNLESS (= (LSH NIB -8) #o200)
      (VECTOR-PUSH (LSH NIB -8) STRING)))
  ;; Construct and record the desired object.
  (PROG1 (ENTER-FASL-TABLE (COND (STRING-FLAG (STRING-APPEND STRING))
                                 ((NOT FASL-GROUP-FLAG)
                                  (INTERN STRING))
                                 (T (MAKE-SYMBOL (STRING-APPEND STRING)))))
         ;; Arrange for reuse of the string.
         (SETQ FASL-OP-SYMBOL-TEMP-STRING STRING)))

(DEFUN FASL-OP-PACKAGE-SYMBOL (&AUX (LEN FASL-GROUP-LENGTH)
                               STR PKG DOUBLE-COLON)
  (DECLARE (SPECIAL STR PKG))
  (IF ( LEN 1)
      (FORMAT *ERROR-OUTPUT* "This file is in the old format -- recompile the source.~%")
    (SETQ LEN (FASL-NEXT-NIBBLE)))
  ;; This kludge is so that we can win without the package feature loaded.
  ;; Values of LEN that are meaningful nowadays are:
  ;; 402 - one prefix, double colon (ignore local package nicknames).
  ;; 2 -- one prefix, single colon.
  ;; 3 -- two prefixes, single colon (no longer produced by QFASD).
  ;; 4 -- three ....
  ;; FASL-GROUP-FLAG is non-NIL to allow internal symbols and creation of symbols.
  (AND (= LEN 402)
       (SETQ DOUBLE-COLON T LEN 2))
  (SETQ STR (FASL-NEXT-VALUE))
  (IF (AND FASL-GROUP-FLAG (EQUAL STR ""))
      ;; Prefix is just #: -- make uninterned symbol.
      (ENTER-FASL-TABLE (MAKE-SYMBOL STR))
    ;; We want an interned symbol in some package.
    ;; Decode the first package prefix.
    (SETQ PKG (OR (AND (NOT DOUBLE-COLON)
                       (FIND-PACKAGE STR *PACKAGE*))
                  (PKG-FIND-PACKAGE STR :ASK)))
    ;; Handle case of multiple prefixes (obsolete).
    (DO ((I (- LEN 2) (1- I)))
        (( I 0))
      (SETQ STR (FASL-NEXT-VALUE))
      (SETQ PKG (OR (FIND-PACKAGE STR PKG)
                    (PKG-FIND-PACKAGE STR :ASK))))
    ;; Read in the pname.
    (SETQ STR (FASL-NEXT-VALUE))
    (MULTIPLE-VALUE-BIND (SYM FLAG PKG-IN)
        ;; Get the symbol.
        (INTERN STR PKG)
      FLAG PKG-IN
;     (WHEN (AND (MEMQ FLAG '(NIL :INTERNAL))
;                (NEQ PKG-IN PKG-KEYWORD-PACKAGE)
;                (NOT (PACKAGE-AUTO-EXPORT-P PKG-IN))
;                (NOT (MEMQ SYM FASL-INTERNAL-DONT-RECORD)))
;       (PUSH (LIST SYM FDEFINE-FILE-PATHNAME)
;             FASL-INTERNAL-SYMBOL-HISTORY))
      ;; Ok, record the symbol we got.
      (ENTER-FASL-TABLE SYM))))

;;; Generate a FIXNUM (or BIGNUM) value.
(DEFUN FASL-OP-FIXED ()
  (DO ((POS (LSH (1- FASL-GROUP-LENGTH) 4) (- POS 16.))
       (C FASL-GROUP-LENGTH (1- C))
       (ANS 0))
      ((ZEROP C) (COND (FASL-GROUP-FLAG (SETQ ANS (MINUS ANS))))
                 (ENTER-FASL-TABLE ANS))
    (SETQ ANS (DPB (FASL-NEXT-NIBBLE) (+ (LSH POS 6) 16.) ANS))))

;;; Generate a CHARACTER value.
(DEFUN FASL-OP-CHARACTER ()
  (DO ((POS (LSH (1- FASL-GROUP-LENGTH) 4) (- POS 16.))
       (C FASL-GROUP-LENGTH (1- C))
       (ANS 0))
      ((ZEROP C)
       (COND (FASL-GROUP-FLAG (SETQ ANS (MINUS ANS))))
       (SETQ ANS (%MAKE-POINTER DTP-CHARACTER ANS))
       (ENTER-FASL-TABLE ANS))
    (SETQ ANS (DPB (FASL-NEXT-NIBBLE) (+ (LSH POS 6) 16.) ANS))))

(DEFUN FASL-OP-FLOAT ()
  (IF FASL-GROUP-FLAG
      (FASL-OP-FLOAT-SMALL-FLOAT)
    (FASL-OP-FLOAT-FLOAT)))

(DEFUN FASL-OP-FLOAT-SMALL-FLOAT NIL
  (LET ((AS-FIXNUM (%LOGDPB (FASL-NEXT-NIBBLE) (BYTE 8. 16.) (FASL-NEXT-NIBBLE))))
    ;; Change exponent from excess #o100 to excess #o200.
    (SETQ AS-FIXNUM (IF (ZEROP AS-FIXNUM) 0 (%POINTER-PLUS AS-FIXNUM #o40000000)))
    (ENTER-FASL-TABLE (%MAKE-POINTER DTP-SMALL-FLONUM AS-FIXNUM))))

(DEFUN FASL-OP-FLOAT-FLOAT ()
  (LET ((ANS (FLOAT 0))
        (TEM))
    (%P-DPB-OFFSET (FASL-NEXT-NIBBLE) (BYTE 11. 8.) ANS 0)
    (SETQ TEM (FASL-NEXT-NIBBLE))
    (%P-DPB-OFFSET (LDB (BYTE 8. 8.) TEM) (BYTE 8. 0.) ANS 0)
    (%P-DPB-OFFSET (%LOGDPB TEM (BYTE 8. 16.) (FASL-NEXT-NIBBLE)) (BYTE 24. 0.) ANS 1)
    (ENTER-FASL-TABLE ANS)))

;;; hair squared
(defun fasl-op-new-float ()
  (let ((sign (if fasl-group-flag -1 1))
        (exponent-length (fasl-next-nibble))
        (exponent 0)
        mantissa-length
        (mantissa 0)
        result)
    (cond ((< exponent-length 9.)               ;small float
           (setq exponent (fasl-next-nibble))
           (setq mantissa-length (fasl-next-nibble))
           (cond ((< mantissa-length 18.)
                  (do ((i 0 (1+ i))
                       (scale 1 (* scale (^ 2 16.))))
                      ((= i (ceiling mantissa-length 16.)))
                    (setq mantissa (+ mantissa (* scale (fasl-next-nibble)))))
                  (setq mantissa (logand mantissa (if (plusp sign)
                                                      ;; nuke leading 1
                                                      (1- (^ 2 17.))
                                                      ;; nuke sign bit and leading 1
                                                      (1- (^ 2 16.)))))
                  (setq result (%make-pointer dtp-small-flonum
                                              (%logdpb exponent
                                                       (byte 8. 17.)
                                                       mantissa)))
                  ;;>> broken setf in 107
                  ;(setq result (%make-pointer dtp-small-flonum mantissa))
                  ;(setf (%short-float-exponent result) exponent))
                  )
                 (t (ferror "Fasl-op-new-float: Exponent length ~D, Mantissa length ~D"
                            exponent-length mantissa-length))))
          ((< exponent-length 12.)              ;single float
           (setq exponent (fasl-next-nibble))
           (if ( exponent (^ 2 11.))
               (setq exponent (- (logand (1- (^ 2 12.)) (^ 2 12.)))))
           (setq mantissa-length (fasl-next-nibble))
           (cond ((< mantissa-length 32.)
                  (do ((i 0 (1+ i))
                       (scale 1 (* scale (^ 2 16.))))
                      ((= i (ceiling mantissa-length 16.)))
                    (setq mantissa (+ mantissa (* scale (fasl-next-nibble)))))
                  (let ((number-cons-area working-storage-area))
                    (setq result (%float-double 0 1)))
                  (setf (%single-float-mantissa result) (* sign mantissa)
                        (%single-float-exponent result) exponent))
                 (t (ferror "Fasl-op-new-float: Exponent length ~D, Mantissa length ~D"
                            exponent-length mantissa-length))))
          (t
           (ferror "Fasl-op-new-float: Exponent length ~D" exponent-length)))
    (enter-fasl-table result)))


(DEFUN FASL-OP-RATIONAL ()
  (LET ((RAT (CL:// (FASL-NEXT-VALUE) (FASL-NEXT-VALUE))))
    (ENTER-FASL-TABLE RAT)))

(DEFUN FASL-OP-COMPLEX ()
  (LET ((COMP (COMPLEX (FASL-NEXT-VALUE) (FASL-NEXT-VALUE))))
    (ENTER-FASL-TABLE COMP)))

;the NEWDRAW system redefines this ... if you change it please let LMI know
(DEFUN FASL-OP-LIST (&OPTIONAL AREA COMPONENT-FLAG
                     &AUX (LIST-LENGTH (FASL-NEXT-NIBBLE)) LST)
  (IF (NULL AREA) (SETQ AREA (AREF FASL-TABLE FASL-LIST-AREA)))
  (SETQ LST (MAKE-LIST LIST-LENGTH :AREA AREA)) ;Make the list
  (DO ((P LST (CDR P))                          ;Store the contents
       (N LIST-LENGTH (1- N)))
      ((ZEROP N))
    (SETF (CAR P) (FASL-NEXT-VALUE)))
  (COND (FASL-GROUP-FLAG (DOTIFY LST)))         ;Flag means "last pair is dotted"
  (IF (NULL COMPONENT-FLAG)
      (ENTER-FASL-TABLE LST)
    (FASL-STORE-EVALED-VALUE LST)))

(DEFUN FASL-OP-TEMP-LIST () (FASL-OP-LIST (AREF FASL-TABLE FASL-TEMP-LIST-AREA)))

;;; This one leaves the value in FASL-EVALED-VALUE instead of adding it to FASL-TABLE,
;;;  thus avoiding bloatage.
(DEFUN FASL-OP-LIST-COMPONENT () (FASL-OP-LIST NIL T))

;;; The argument must be a linear list.
;;; Note (hope) that the GC cannot unlinearize a linear list.
;;; The CAR of LAST of it becomes the CDR of LAST.
(DEFUN DOTIFY (ARG)
  (DO ((LST ARG (CDR LST)))                     ;Find the 2nd to last CONS of it
      ((NULL (CDDR LST))
       (OR (= (%P-CDR-CODE LST) CDR-NEXT)       ;Make sure someone didn't screw up
           (FERROR "~S is not a linear list" ARG))
       (%P-STORE-CDR-CODE LST CDR-NORMAL)       ;Change last 2 single-Q nodes to one double-Q node
       (%P-DPB-OFFSET CDR-ERROR %%Q-CDR-CODE LST 1)     ;Fix 2nd cdr code for error checking
       ARG)))

;;;; Array stuff

;;; FASL-OP-ARRAY arguments are
;;;  <value>  Area
;;;  <value>  Type symbol
;;;  <value>  The dimension or dimension list (use temp-list)
;;;  <value>  Displace pointer (NIL if none)
;;;  <value>  Leader (NIL, number, or list) (use temp-list)
;;;  <value>  Index offset (NIL if none)
;;;  [<value>]named-structure-p, if flag set, else this value not supplied
(DEFUN FASL-OP-ARRAY ()
  (LET ((AREA (FASL-NEXT-VALUE))                ;Area
        ;; With luck, this LET* does whatever the &AUX above was intended to do
        ;; with less disasterous byproducts.
        (TYPE (LET ((*PACKAGE* PKG-GLOBAL-PACKAGE))
                (FASL-NEXT-VALUE)))             ;Type symbol
        (DIMS (FASL-NEXT-VALUE))                ;Dimensions
        (DISP (FASL-NEXT-VALUE))                ;Displaced-p
        (LEAD (FASL-NEXT-VALUE))                ;Leader
        (IOFF (FASL-NEXT-VALUE))                ;Index-offset
        (NSP (IF FASL-GROUP-FLAG                ;Named-structure-p
                  (FASL-NEXT-VALUE)
                NIL)))
    (ENTER-FASL-TABLE (MAKE-ARRAY DIMS
                                  :TYPE TYPE
                                  :AREA AREA
                                  :DISPLACED-TO DISP
                                  :DISPLACED-INDEX-OFFSET IOFF
                                  :LEADER-LENGTH (IF (CONSP LEAD)
                                                     (LENGTH LEAD)
                                                     LEAD)
                                  :LEADER-LIST (IF (CONSP LEAD) (REVERSE LEAD))
                                  :NAMED-STRUCTURE-SYMBOL NSP))))

;;; Get values and store them into an array.
(DEFUN FASL-OP-INITIALIZE-ARRAY (&OPTIONAL LOAD-16BIT-MODE
                                 &AUX ARRAY NUM TEM-ARRAY HACK)
  (SETQ HACK (FASL-GROUP))
  (SETQ ARRAY (AREF FASL-TABLE HACK))
  (CHECK-TYPE ARRAY ARRAY)
  (SETQ NUM (FASL-NEXT-VALUE))                  ;Number of values to initialize with
  (SETQ TEM-ARRAY                               ;Indirect array used to store into it
        (MAKE-ARRAY NUM :AREA FASL-TABLE-AREA
                        :TYPE (IF (NOT LOAD-16BIT-MODE)
                                  (%P-MASK-FIELD %%ARRAY-TYPE-FIELD ARRAY)
                                  'ART-16B)
                        :DISPLACED-TO ARRAY
                        :FILL-POINTER 0))
  (DO ((N NUM (1- N))) ((ZEROP N))              ;Initialize specified num of vals
      (LET ((N (FASL-NIBBLE-PEEK)))
        (IF (= (LOGAND %FASL-GROUP-TYPE N) FASL-OP-NULL-ARRAY-ELEMENT)
            (PROGN
              (FASL-NIBBLE)
              (VECTOR-PUSH NIL TEM-ARRAY)
              (%P-STORE-DATA-TYPE (ALOC ARRAY (1- (FILL-POINTER TEM-ARRAY)))
                                  DTP-NULL))
          (VECTOR-PUSH (FASL-NEXT-VALUE) TEM-ARRAY))))
 ;(RETURN-ARRAY (PROG1 TEM-ARRAY (SETQ TEM-ARRAY NIL)))
  (IF (TYPEP ARRAY 'NAMED-STRUCTURE)
      (WHEN (MEMQ :FASLOAD-FIXUP (NAMED-STRUCTURE-INVOKE :WHICH-OPERATIONS ARRAY))
        (NAMED-STRUCTURE-INVOKE :FASLOAD-FIXUP ARRAY)))
  HACK)

;;; Get nibbles and store them into 16-bit hunks of an array.
(DEFUN FASL-OP-INITIALIZE-NUMERIC-ARRAY (&AUX ARRAY NUM TEM-ARRAY HACK)
  (SETQ HACK (FASL-GROUP))
  (SETQ ARRAY (AREF FASL-TABLE HACK))
  (CHECK-TYPE ARRAY ARRAY)
  (SETQ NUM (FASL-NEXT-VALUE))                  ;# of vals to initialize
  (SETQ TEM-ARRAY (MAKE-ARRAY NUM :AREA FASL-TABLE-AREA
                                  :ELEMENT-TYPE '(UNSIGNED-BYTE 16.)
                                  :DISPLACED-TO ARRAY
                                  :FILL-POINTER 0))
  (DO ((N NUM (1- N))) ((ZEROP N))
    (VECTOR-PUSH (FASL-NIBBLE) TEM-ARRAY))
 ;(RETURN-ARRAY (PROG1 TEM-ARRAY (SETQ TEM-ARRAY NIL)))
  HACK)

(DEFUN FASL-OP-ARRAY-PUSH ()
  (LET ((VECTOR (FASL-NEXT-VALUE)))
    (VECTOR-PUSH (FASL-NEXT-VALUE) VECTOR))
  0)


(DEFUN FASL-OP-EVAL ()
  (ferror "Obsolete QFASL file."))
;  (LET ((FORM (AREF FASL-TABLE (FASL-NEXT-NIBBLE))))
;    (WHEN (OR (ATOM FORM) (NEQ (CAR FORM) 'FUNCTION))
;      (WHEN PRINT-LOADED-FORMS (PRINT FORM))
;      (WHEN ACCUMULATE-FASL-FORMS
;       (PUSH FORM LAST-FASL-FILE-FORMS))
;      (PUSH FORM FASL-FILE-EVALUATIONS))
;    (FASL-STORE-EVALED-VALUE (EVAL FORM)))
;  NIL)

(defprop qfasl-dont-record t debug-info)

;;; Calls to these functions should not be recorded.
(DEFPROP DEFVAR-1 T QFASL-DONT-RECORD)
(DEFPROP DEFCONST-1 T QFASL-DONT-RECORD)
(DEFPROP DEFVAR-RESETTABLE-1 T QFASL-DONT-RECORD)
(DEFPROP DEFSELECT-INTERNAL T QFASL-DONT-RECORD)
(DEFPROP FUNCTION-SPEC-PUTPROP T QFASL-DONT-RECORD)
(DEFPROP FDEFINITION-LOCATION T QFASL-DONT-RECORD)
(DEFPROP RECORD-SOURCE-FILE-NAME T QFASL-DONT-RECORD)
(DEFPROP FS::MAKE-FASLOAD-PATHNAME T QFASL-DONT-RECORD)

;;; These properties should not be recorded when they are DEFPROPed.
(DEFPROP DEFSTRUCT-SLOT T QFASL-DONT-RECORD)
(DEFPROP DEFSTRUCT-DESCRIPTION T QFASL-DONT-RECORD)
(DEFPROP DEFSTRUCT-NAME T QFASL-DONT-RECORD)

(DEFUN FASL-OP-EVAL1 ()
  (LET ((FORM (FASL-NEXT-VALUE)))
    (WHEN (OR (ATOM FORM) (NEQ (CAR FORM) 'FUNCTION))
;     (WHEN PRINT-LOADED-FORMS (PRINT FORM))
      (WHEN ACCUMULATE-FASL-FORMS
        (PUSH FORM LAST-FASL-FILE-FORMS))
      (IF (NOT (AND (CONSP FORM)
                    (OR (GET (CAR FORM) 'QFASL-DONT-RECORD)
                        (ignore-errors (assq 'qfasl-dont-record (debugging-info (car form))))
                        (AND (EQ (CAR FORM) 'FDEFINE)
                             (EQ (FOURTH FORM) T))
                        (AND (EQ (CAR FORM) 'DEFPROP)
                             (GET (FOURTH FORM) 'QFASL-DONT-RECORD)))))
          (PUSH FORM FASL-FILE-EVALUATIONS)))
    (ENTER-FASL-TABLE (EVAL FORM))))

(DEFUN FASL-OP-MOVE ()
  (LET ((FROM (FASL-NEXT-NIBBLE))
        (TO (FASL-NEXT-NIBBLE)))
    (IF (= TO #o177777)
        (ENTER-FASL-TABLE (AREF FASL-TABLE FROM))
      (SETF (AREF FASL-TABLE TO) (AREF FASL-TABLE FROM))
      TO)))

(DEFVAR *SNAP-INDEXED-FORWARDS* NIL)

#+(target lambda)
(DEFUN FASL-OP-FRAME ()
  (LET ((Q-COUNT (FASL-NEXT-NIBBLE))            ;Number of boxed Qs
        (UNBOXED-COUNT (FASL-NEXT-NIBBLE))      ;Number of unboxed Qs (half num instructions)
        (SIZE NIL)                              ;Total number of Qs
        (FEF NIL)                               ;The FEF being created
        (OBJ NIL)
        (TEM NIL)
        (OFFSET NIL)
        (%INHIBIT-READ-ONLY T)
        )
     (SETQ FASL-GROUP-LENGTH (FASL-NEXT-NIBBLE))        ;Amount of stuff that follows
     (SETQ FEF (%make-structure dtp-fef-pointer
                                dtp-header
                                (fasl-next-value)
                                (setq size (+ q-count unboxed-count))
                                (aref fasl-table fasl-frame-area)
                                size
                                q-count))
     (FASL-NEXT-NIBBLE)                         ;Skip modifier nibble for header Q
     (LETF (((AREF FASL-TABLE FASL-LIST-AREA) MACRO-COMPILED-PROGRAM)
            ((AREF FASL-TABLE FASL-ARRAY-AREA) MACRO-COMPILED-PROGRAM))
       (DO ((I 1 (1+ I)))                               ;Fill in boxed Qs
           (( I Q-COUNT))
         ;; OBJ gets the object to be stored.
         (COND ((AND (= I (1- Q-COUNT))
                     (> I %FEFHI-MISC)
                     (fef-debugging-info-present-p fef))
                ;; If this constant is the fef's debugging info alist,
                ;; read it in in a special area.
                (LETF (((AREF FASL-TABLE FASL-LIST-AREA) DEBUG-INFO-AREA))
                  (SETQ OBJ (FASL-NEXT-VALUE)))
                ;; See if any macros that were used in this fef
                ;; have changed their sxhashes since the fef was compiled.
                (LET ((TEM (ASSQ :MACROS-EXPANDED OBJ)))
                  (IF TEM
                      (CHECK-MACROS-EXPANDED (CADR TEM) (FUNCTION-NAME FEF)))))
               ;; Read everything except the debugging info alist.
               (T
                (SETQ OBJ (FASL-NEXT-VALUE))))
         (SETQ TEM (FASL-NEXT-NIBBLE))                  ;Get ultra-kludgey modifier
         (OR (ZEROP (SETQ OFFSET (LOGAND #o17 TEM)))    ;Add offset if necessary
             (SETQ OBJ (%MAKE-POINTER-OFFSET DTP-LOCATIVE OBJ OFFSET)))
         (%P-STORE-CONTENTS-OFFSET OBJ FEF I)           ;Store it
         (%P-DPB-OFFSET (map-to-new-cdr-code (LSH TEM -6)) %%Q-CDR-CODE FEF I)  ;Mung cdr code
         (AND (BIT-TEST #o20 TEM)                       ;Make into one-q-forward.
              (%P-DPB-OFFSET DTP-ONE-Q-FORWARD
                             %%Q-DATA-TYPE FEF I))
;        (AND (BIT-TEST #o20 TEM)                       ;Make into external value cell pointer
;             (%P-DPB-OFFSET DTP-EXTERNAL-VALUE-CELL-POINTER
;                            %%Q-DATA-TYPE FEF I))
         (AND (BIT-TEST #o400 TEM)                      ;Make into locative
              (%P-DPB-OFFSET DTP-LOCATIVE %%Q-DATA-TYPE FEF I))
         (AND (BIT-TEST #o1000 TEM)
              (%P-DPB-OFFSET DTP-SELF-REF-POINTER %%Q-DATA-TYPE FEF I))
         (WHEN (AND *SNAP-INDEXED-FORWARDS*
                    (= DTP-ONE-Q-FORWARD (%P-LDB-OFFSET %%Q-DATA-TYPE FEF I)))
           (LET* ((LOC (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF I))
                  (PTR (%FIND-STRUCTURE-HEADER LOC)))
             (WHEN (AND (SYMBOLP PTR)
                        (= DTP-INDEXED-FORWARD (%P-DATA-TYPE LOC)))
               (%P-STORE-TAG-AND-POINTER (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF I)
                                         DTP-INDEXED-FORWARD
                                         (%P-POINTER LOC))))))
       (DO ((I Q-COUNT (1+ I)))                         ;Now store unboxed Qs
           (( I SIZE))
         (%P-DPB-OFFSET (FASL-NEXT-NIBBLE)              ;Store low-order halfword
                        %%Q-LOW-HALF FEF I)
         (%P-DPB-OFFSET (FASL-NEXT-NIBBLE)              ;Then high-order halfword
                        %%Q-HIGH-HALF FEF I))
       ;; Convert old destination codes to new ones.
       (UNLESS DONT-CONVERT-DESTINATIONS
         (FEF-CONVERT-DESTINATIONS FEF))
       (maybe-change-fef-type fef)
       (ENTER-FASL-TABLE FEF))))

;;; Used to be called DISASSEMBLE-FETCH and EH:FEF-INSTRUCTION.
(DEFSUBST FEF-INSTRUCTION (FEF PC)
  "Given a FEF and a PC, returns the corresponding 16-bit macro instruction.
There is -no- error checking."
  (%P-LDB-OFFSET (IF (EVENP PC) %%Q-LOW-HALF %%Q-HIGH-HALF)
                 FEF
                 (TRUNCATE PC 2)))

;;; Convert old codes for D-PDL and D-LAST into new ones.
;;; This must be run on all fefs fasloaded or compiled in core.
;;; When new values for D-PDL, etc. are installed it will not
;;; be needed on fefs created in core.
;;; It will be needed on fefs fasloaded for a long time.
(DEFUN FEF-CONVERT-DESTINATIONS (FEF &AUX ILEN LIM-PC)
  (SETQ LIM-PC (FEF-LIMIT-PC FEF))
  (DO ((PC (FEF-INITIAL-PC FEF) (+ PC ILEN)))
      (( PC LIM-PC))
    (LET* ((INSN (FEF-INSTRUCTION FEF PC))
           (OP (LDB (BYTE 4. 9.) INSN)))
      (IF (OR (< OP 11) (= OP #o15))
          (LET ((DEST (LDB (BYTE 3. 13.) INSN)))
            (IF (< 0 DEST 4)
                (SETF (FEF-INSTRUCTION FEF PC)
                      (DPB (* 2 DEST) (BYTE 3. 13.) INSN))))))
    (SETQ ILEN (FEF-INSTRUCTION-LENGTH FEF PC))))

;;; Used to be called DISASSEMBLE-INSTRUCTION-LENGTH
(DEFUN FEF-INSTRUCTION-LENGTH (FEF PC)
  "Return the length in halfwords of the instruction at PC in FEF."
  (LET* ((WD (FEF-INSTRUCTION FEF PC))
         (OP (LDB (BYTE 4. 9.) WD))
         (DISP (LDB (BYTE 9. 0.) WD)))
    (COND ((AND (= OP #o14) (= DISP #o777)) 2)
          ((AND (< OP #o14) (= DISP #o776)) 2)
          (T 1))))

(DEFUN FEF-LIMIT-PC (FEF)
  "Return the pc value of the end of the code of the fef."
  (LET ((LIM-PC (* 2 (FEF-LENGTH FEF))))
    (IF (ZEROP (FEF-INSTRUCTION FEF (1- LIM-PC)))
        (1- LIM-PC)
        LIM-PC)))

(DEFUN FASL-OP-FUNCTION-HEADER ()
  (LET ((FCTN (FASL-NEXT-VALUE))
        (F-SXH (FASL-NEXT-VALUE)))
    (DECLARE (IGNORE FCTN F-SXH)))
  0)

(DEFUN FASL-OP-FUNCTION-END ()
  0)

(DEFUN FASL-OP-STOREIN-SYMBOL-CELL ()
  (LET ((CELL (FASL-NEXT-NIBBLE))
        (DATA (FASL-NEXT-VALUE))
        (SYM (FASL-NEXT-VALUE)))
    (ECASE CELL
      (1 (SET SYM DATA)
         (WHEN PRINT-LOADED-FORMS
           (PRINT `(SETQ ,SYM ',DATA)))
         (WHEN ACCUMULATE-FASL-FORMS
           (PUSH `(SETQ ,SYM ',DATA) LAST-FASL-FILE-FORMS)))
      (2 (FSET SYM DATA)
         (WHEN PRINT-LOADED-FORMS
           (PRINT `(FSET ',SYM ',DATA)))
         (WHEN ACCUMULATE-FASL-FORMS
           (PUSH `(FSET ',SYM ',DATA) LAST-FASL-FILE-FORMS)))
      (3 (SETF (SYMBOL-PLIST SYM) DATA)
         (WHEN PRINT-LOADED-FORMS
           (PRINT `(SETF (SYMBOL-PLIST ,SYM) ',DATA)))
         (WHEN ACCUMULATE-FASL-FORMS
           (PUSH `(SETF (SYMBOL-PLIST ',SYM) ',DATA) LAST-FASL-FILE-FORMS)))))
    0)

(DEFUN FASL-OP-STOREIN-SYMBOL-VALUE ()
  (LET ((DATA (AREF FASL-TABLE (FASL-NEXT-NIBBLE)))
        (SYM (FASL-NEXT-VALUE)))
    (SET SYM DATA)
    (PUSH `(SETQ ,SYM ',DATA) FASL-FILE-EVALUATIONS)
    (WHEN PRINT-LOADED-FORMS
      (PRINT (CAR FASL-FILE-EVALUATIONS)))
    (WHEN ACCUMULATE-FASL-FORMS
      (PUSH (CAR FASL-FILE-EVALUATIONS)
            LAST-FASL-FILE-FORMS)))
    0)

(DEFUN FASL-OP-STOREIN-FUNCTION-CELL ()
  (LET ((DATA (AREF FASL-TABLE (FASL-NEXT-NIBBLE)))
        (SYM (FASL-NEXT-VALUE)))
    (FSET-CAREFULLY SYM DATA)
    (WHEN PRINT-LOADED-FORMS
      (PRINT `(SETF (SYMBOL-FUNCTION ',SYM) ',DATA)))
    (WHEN ACCUMULATE-FASL-FORMS
      (PUSH `(SETF (SYMBOL-FUNCTION ',SYM) ',DATA)
            LAST-FASL-FILE-FORMS)))
  0)

(DEFUN FASL-OP-STOREIN-PROPERTY-CELL ()
  (LET ((DATA (AREF FASL-TABLE (FASL-NEXT-NIBBLE)))
        (SYM (FASL-NEXT-VALUE)))
    (SETF (SYMBOL-PLIST SYM) DATA)
    (PUSH `(SETF (SYMBOL-PLIST ',SYM) ',DATA) FASL-FILE-EVALUATIONS)
    (WHEN PRINT-LOADED-FORMS
      (PRINT (CAR FASL-FILE-EVALUATIONS)))
    (WHEN ACCUMULATE-FASL-FORMS
      (PUSH (CAR FASL-FILE-EVALUATIONS)
            LAST-FASL-FILE-FORMS)))
  0)

(DEFUN FASL-OP-STOREIN-ARRAY-LEADER ()
  (LET ((ARRAY (AREF FASL-TABLE (FASL-NEXT-NIBBLE)))
        (SUBSCR (AREF FASL-TABLE (FASL-NEXT-NIBBLE)))
        (VALUE (AREF FASL-TABLE (FASL-NEXT-NIBBLE))))
    (SETF (ARRAY-LEADER ARRAY SUBSCR) VALUE))
  0)

(DEFUN FASL-OP-FETCH-SYMBOL-VALUE ()
  (ENTER-FASL-TABLE (SYMBOL-VALUE (FASL-NEXT-VALUE))))

(DEFUN FASL-OP-FETCH-FUNCTION-CELL ()
  (ENTER-FASL-TABLE (CONTENTS (FUNCTION-CELL-LOCATION (FASL-NEXT-VALUE)))))

(DEFUN FASL-OP-FETCH-PROPERTY-CELL ()
  (ENTER-FASL-TABLE (CONTENTS (PROPERTY-CELL-LOCATION (FASL-NEXT-VALUE)))))

(DEFUN FASL-OP-APPLY ()
  (LET* ((COUNT (FASL-NEXT-NIBBLE))
         (FCTN (FASL-NEXT-VALUE))
         V
         (P (VALUE-CELL-LOCATION V)))
    (DOTIMES (I COUNT)
      (SETF (CDR P)
            (SETQ P (NCONS-IN-AREA (FASL-NEXT-VALUE)
                                   (AREF FASL-TABLE FASL-TEMP-LIST-AREA)))))
    (WHEN ACCUMULATE-FASL-FORMS
      (PUSH `(APPLY ',FCTN ',V)
            LAST-FASL-FILE-FORMS))
;   (WHEN PRINT-LOADED-FORMS
;   (PRINT `(APPLY ',FCTN ',V)))
    (PUSH `(,FCTN) FASL-FILE-EVALUATIONS)
    (FASL-STORE-EVALED-VALUE (APPLY FCTN V))))

(DEFUN FASL-OP-END-OF-WHACK ()
  (SETQ FASL-RETURN-FLAG 'END-OF-WHACK)
  0)

(DEFUN FASL-OP-END-OF-FILE ()
  (SETQ FASL-RETURN-FLAG 'EOF)
  0)

(DEFUN FASL-OP-SOAK ()
  (LET ((COUNT (FASL-NEXT-NIBBLE)))
    (DOTIMES (I COUNT)
      (FASL-NEXT-VALUE)))
  (FASL-GROUP))

(DEFUN FASL-OP-SET-PARAMETER ()
  (LET ((TO (FASL-NEXT-VALUE))
        (FROM (FASL-GROUP)))
    (SETF (AREF FASL-TABLE (EVAL TO)) (AREF FASL-TABLE FROM)))
  0)

(DEFUN FASL-APPEND (OUTFILE &REST INFILES)
  "Concatenate the contents of QFASL files INFILES into one QFASL file named OUTFILE."
  (WITH-OPEN-FILE (OSTREAM
                    (FS:MERGE-PATHNAME-DEFAULTS OUTFILE FS:LOAD-PATHNAME-DEFAULTS
                                                :QFASL)
                    :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE *fasl-byte-size*)
    (DO ((FILES INFILES (CDR FILES)))
        ((NULL FILES))
      (WITH-OPEN-FILE (ISTREAM (FS:MERGE-PATHNAME-DEFAULTS
                                 (CAR FILES) FS:LOAD-PATHNAME-DEFAULTS :QFASL)
                               :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE *fasl-byte-size*)
        ;; Skip first two nibbles of all but the first file.
        (UNLESS (EQ FILES INFILES)
          (SEND ISTREAM :TYI)
          (SEND ISTREAM :TYI))
        (DO ((NIBBLE (SEND ISTREAM :TYI))
             (NEXT1 (SEND ISTREAM :TYI))
             (NEXT2))
            ((NULL NIBBLE))
          (SETQ NEXT2 (SEND ISTREAM :TYI))
          (AND (OR NEXT2
                   (AND NEXT1 (NOT (ZEROP NEXT1)))
                   (AND (NULL (CDR FILES))      ;Skip the last nonzero nibble
                        (NOT (ZEROP NIBBLE))))  ;of all files except the last.
               (SEND OSTREAM :TYO NIBBLE))
          (SETQ NIBBLE NEXT1
                NEXT1 NEXT2))))
    OUTFILE))

#+(target falcon)
(defun fasl-make-vector (size)
  (array:make-vector size))

#+(target falcon)
(defun fasl-op-k-compiled-function ()
  (let ((name (fasl-next-value))
        (local-refs (fasl-next-value))
        (refs (fasl-next-value))
        (entry-points (fasl-next-value))
        (code (fasl-next-value))
        (immediates (fasl-next-value))
        (function ()))
    (setq function
          (cons:allocate-structure compiled-function-structure-size 0
                                   k2:$$dtp-compiled-function
                                   (cons:make-header k2:$$dtp-compiled-function-header length)))
    (setf (k2:%compiled-function-name      function)  name)
    (setf (k2:%compiled-function-entry-points function) (cadr entry-points))
    (setf (k2:%compiled-function-local-refs   function) (cadr local-refs))
    (setf (k2:%compiled-function-refs         function) (cadr refs)
    (setf (k2:%compiled-function-length       function) (car code))
    (let ((address (cons:allocate-code-space (car code) function gr:*default-code-area*)))
      (let ((code-addr (hw:24+ 2 address)))
        ;;  (setf (%compiled-function-starting-address function) code-addr)
        (setf (k2:%compiled-function-code function) (addr->pc code-addr))
        (do ((i 0 (1+ i))
             (addr code-addr (hw:24+ 2 addr)))
            ((>= i length))
          (fasl-write-instruction addr
                                  (svref (cadr code) (* i 4))
                                  (svref (cadr code) (+ 1 (* i 4)))
                                  (svref (cadr code) (+ 2 (* i 4)))
                                  (svref (cadr code) (+ 3 (* i 4)))))
        (fasl-read-and-link-immediates code-addr immeds)
        (fasl-link-function function code-addr)))
    ;; Don't do this here; do this by explicit command in the file to store into a function cell.
    ;; We may be putting this into a list (as for (MACRO #<DTP-CODE ...>)) or otherwise doing
    ;; something special with it.
    #+Ignore
    (when (li:symbolp name)
      (setf (symbol:symbol-function name) function))
    (enter-fasl-table function))))

#+(target falcon)
(defun fasl-write-instruction (address 1st 2nd 3rd 4th)
    (map-fault:call-while-allowing-write-in-read-only
      #'(lambda ()
          (hw:write-md-unboxed
            (hw:dpb-unboxed 2nd (byte 16. 16.) 1st))
          (hw:vma-start-write-no-gc-trap-unboxed address)
          ;; Write the high half
          (hw:write-md-unboxed
            (hw:dpb-unboxed 4th (byte 16. 16.) 3rd))
          (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ 1 address))
          nil)))

#+(target falcon)
(defun fasl-read-and-link-immediates (code-adr immeds)
  (dotimes (i (car immeds))
    (write-boxed-immediate
      (hw:24+ (ash (svref (cadr immeds) (* i 2)) 1.) code-adr)
      (svref (cadr immeds) (+ 1 (* i 2))))))


#+(target falcon)
(defun fasl-link-function (function address)
  (k2:relocate-local-refs function address)
  (k2:link-refs function address))

;;; avoid consing, read into an array 4 times bigger thane the number of insts.
#+(target falcon)
(defun fasl-op-k-instructions ()
  (let ((num-insts (fasl-next-value)))
    (let ((inst-array (fasl-make-vector (* 4 num-insts))))
      (do ((i 0 (1+ i)))
          ((>= i (* 4 num-insts)) (enter-fasl-table `(,num-insts ,inst-array)))
        (setf (aref inst-array i) (fasl-next-nibble))))))

#+(target falcon)
(defun fasl-op-k-immediates ()
  (let ((immeds (fasl-next-value)))
    (do ((i 0 (+ i 1))
         (imms (fasl-make-vector (* 2 immeds))))
        ((>= i (* 2 immeds))
         (enter-fasl-table `(,immeds ,imms)))
      (setf (aref imms i) (fasl-next-value)))))

;;; Set the load-time-eval immediate values for a particular function.
#+(target falcon)
(defun fasl-op-k-load-time-evals ()
  (loop with code-adr = (fasl-next-value)
        repeat (fasl-next-nibble)
        for idx = (fasl-next-nibble)
        for form = (fasl-next-value)
        do (write-boxed-immediate
             (hw:24+ (ash idx 1.) code-adr)
             (eval form))))


#+(target falcon)
(defun fasl-op-k-local-refs ()
  (let ((locals (fasl-next-value)))
    (do ((i 0 (+ i 1))
         (locs (fasl-make-vector (* 2 locals))))
        ((>= i (* 2 locals))
         (enter-fasl-table `(,locals ,locs)))
      (setf (aref locs i) (fasl-next-value)))))

#+(target falcon)
(defun fasl-op-k-refs ()
  (let ((k-refs (fasl-next-value)))
      (do ((i 0 (+ i 1))
           (refs (fasl-make-vector (* 3 k-refs))))
          ((>= i (* 3 k-refs)) (enter-fasl-table `(,k-refs ,refs)))
        (setf (aref refs i) (fasl-next-value)))))

#+(target falcon)
(defun fasl-op-k-entry-points ()
  (let ((entries (fasl-next-value)))
    (do ((i 0 (+ i 1))
         (ents (fasl-make-vector (* 3 entries))))
        ((>= i (* 2 entries))
         (enter-fasl-table `(,entries ,ents)))
      (setf (aref ents i) (fasl-next-value)))))

(defvar *machine-fasling-on* :unbound
  "Machine on which FASL input is being loaded")

(defvar *fasl-version* 2
  "Version number of internal FASL format currently supported")

(defvar fasl-internal-version 2
  "Version number of current FASL")

(defun check-version-info ()
  (fasl-whack)
  t)

(defun fasl-op-version-info ()
  (let ((machine (fasl-next-value))
        (version (fasl-next-value)))
    (unless (eql machine *machine-fasling-on*)
      (ferror "File was compiled for ~A and is being loaded on ~A." machine *machine-fasling-on*))
    (cond ((eql version *fasl-version*))
          ;; Versions 1 and 2 are compatible on the Lambda.
          ((and (eql version 1)
                (eql *fasl-version* 2)
                (eql *machine-fasling-on* :lambda)))
          (t (ferror "File was compiled with version ~A FASL format; version ~A was expected." version *fasl-version*)))
    (setq fasl-internal-version version)
    (enter-fasl-table ())))

(defun save-version-info ()
  (send fasl-generic-plist-receiver :putprop fasl-internal-version :fasl-version))

;;;Avoid cold load lossage:

(defun fasl-cold-boot ()
  "Performs QFASL//Fasload initializations  for cold boot"
  (setq *machine-fasling-on* '#.(compiler:target-processor-symbol))
  (assign-values fasl-ops 0))

(defun fasl-warm-boot ()
  "Performs QFASL//Fasload initializations for warm restart"
  (fasl-restart))

(add-initialization "Load//Fasload cold boot" '(fasl-cold-boot) '(:now :system))

(add-initialization "Load//Fasload warm boot" '(fasl-warm-boot) '(:now :warm))
