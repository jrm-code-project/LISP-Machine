;;; -*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL -*-

;;;; ITS support

;;; An FN2 can be either a version, if it is all numbers, or a type otherwise.
(DEFFLAVOR ITS-PATHNAME-MIXIN () ()
  (:REQUIRED-FLAVORS PATHNAME))

;;; These messages are careful to cons only the actual string in PATHNAME-AREA.
;;; See comment by PATHNAME-AREA.

(DEFMETHOD (ITS-PATHNAME-MIXIN :STRING-FOR-HOST) ()
  (LET ((DIR (STRING-OR-WILD DIRECTORY))
        (XDEV (ITS-DEVICE-STRING T))
        (FN1 (ITS-FN1-STRING))
        (FN2 (ITS-FN2-STRING))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A: ~A; ~A ~A" XDEV DIR FN1 FN2)))

(DEFMETHOD (ITS-PATHNAME-MIXIN :STRING-FOR-PRINTING) ()
  (LET ((DEV (ITS-DEVICE-STRING))
        (DIR (STRING-OR-WILD DIRECTORY))
        (FN1 (ITS-FN1-STRING))
        (FN2 (ITS-FN2-STRING))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A: ~:[~A;~:[ ~]~;~2*~]~@[~A~]~@[ ~A~]"
            DEV (MEMQ DIRECTORY '(NIL :UNSPECIFIC))
            DIR (AND (MEMQ NAME '(NIL :UNSPECIFIC))
                     (MEMQ TYPE '(NIL :UNSPECIFIC))
                     (MEMQ VERSION '(NIL :UNSPECIFIC)))
            FN1 FN2)))

(DEFMETHOD (ITS-PATHNAME-MIXIN :STRING-FOR-EDITOR) ()
  (LET ((FN1 (ITS-FN1-STRING))
        (FN2 (ITS-FN2-STRING))
        (DIR (STRING-OR-WILD DIRECTORY))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A ~:[~A ~;~*~]~:[~A: ~;~*~]~A; ~A:"
            FN1
            ;; Do we really have a significant type or version worth printing?
            (NOT (OR (AND TYPE (NEQ TYPE :UNSPECIFIC))
                     (AND VERSION (NEQ VERSION :UNSPECIFIC))))
            FN2
            (OR (SI:MEMBER-EQUAL DEVICE '("DSK" NIL :UNSPECIFIC))
                (EQUAL DEVICE (SEND HOST :NAME-AS-FILE-COMPUTER)))
            DEVICE DIR
            (SEND HOST :NAME-AS-FILE-COMPUTER))))

(DEFMETHOD (ITS-PATHNAME-MIXIN :STRING-FOR-DIRECTORY) ()
  (LET ((DIR (STRING-OR-WILD DIRECTORY))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~:[~A: ~;~*~]~A;"
            (SI:MEMBER-EQUAL DEVICE '("DSK" NIL :UNSPECIFIC))
            DEVICE DIR)))

(DEFMETHOD (ITS-PATHNAME-MIXIN :STRING-FOR-DIRED) ()
  (LET ((FN1 (ITS-FN1-STRING T))
        (FN2 (ITS-FN2-STRING T))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~6A ~A" FN1 FN2)))

(DEFMETHOD (ITS-PATHNAME-MIXIN :FN1) ()
  (ITS-FN1-STRING T T))

(DEFMETHOD (ITS-PATHNAME-MIXIN :FN2) ()
  (ITS-FN2-STRING T T))

;;; If the device is DSK, avoid printing it.
(DEFUN ITS-DEVICE-STRING (&OPTIONAL OMIT-HOST &AUX HOSTS PRINT-HOSTS)
  (DECLARE (:SELF-FLAVOR ITS-PATHNAME-MIXIN))
  (SETQ HOSTS (SEND HOST :NAME-AS-FILE-COMPUTER))
  (SETQ PRINT-HOSTS (IF OMIT-HOST "DSK" HOSTS))
  (IF (OR (SI:MEMBER-EQUAL DEVICE '("DSK" NIL :UNSPECIFIC))
          (EQUAL DEVICE HOSTS))
      PRINT-HOSTS
      (STRING-APPEND PRINT-HOSTS ": " DEVICE)))

;;; If name is a list, its first component is the FN1 and second is FN2
;;; If only FN2 is present, FN1 is placeholder ""
(DEFUN ITS-FN1-STRING (&OPTIONAL NO-QUOTE-P NO-PLACEHOLDER)
  (DECLARE (:SELF-FLAVOR PATHNAME))
  (COND ((NULL NAME)
         (IF NO-PLACEHOLDER
             NIL ""))
        ((EQ NAME :UNSPECIFIC) NIL)  ;Should this ever happen?
        ((CONSP NAME) (IF NO-QUOTE-P (CAR NAME)
                          (QUOTE-COMPONENT-STRING (CAR NAME))))
        (T (STRING-OR-WILD NAME NO-QUOTE-P))))

(DEFUN ITS-FN2-STRING (&OPTIONAL NO-QUOTE-P NO-PLACEHOLDER)
  (DECLARE (:SELF-FLAVOR ITS-PATHNAME-MIXIN))
  (COND ((AND (CONSP NAME) (CDR NAME))
         (IF NO-QUOTE-P (CADR NAME) (QUOTE-COMPONENT-STRING (CADR NAME))))
        ((AND (NULL TYPE) (NULL VERSION))
         (IF NO-PLACEHOLDER NIL ""))
        ((AND (MEMQ TYPE '(NIL :UNSPECIFIC))
              (MEMQ VERSION '(NIL :UNSPECIFIC)))
         NIL)
        ((EQ VERSION :OLDEST) "<")
        ((EQ VERSION :WILD) "*")
        ((OR (EQ VERSION :UNSPECIFIC) (NOT (SI:MEMBER-EQUAL TYPE *ITS-UNINTERESTING-TYPES*)))
         (STRING-OR-WILD TYPE NO-QUOTE-P))
        ((NOT (MEMQ VERSION '(NIL :UNSPECIFIC :NEWEST))) (FORMAT NIL "~D" VERSION))
        (T ">")))

(DEFUN STRING-OR-WILD (FIELD &OPTIONAL NO-QUOTE-P SPECIALS REPLACED-BY)
  "Convert FIELD, a pathname component, to a string to appear in a printed representation.
NO-QUOTE-P inhibits insertion of quoting characters;
otherwise, quote characters are inserted and some characters translated:
SPECIALS is a list of characters to be translated,
and REPLACED-BY is an equally-long list of characters to translate them to."
  (COND ((EQ FIELD :WILD) "*")
        ((MEMQ FIELD '(NIL :UNSPECIFIC)) NIL)
        (NO-QUOTE-P (STRING FIELD))
        ((QUOTE-COMPONENT-STRING FIELD SPECIALS REPLACED-BY))))

(DEFUN QUOTE-COMPONENT-STRING (STRING &OPTIONAL SPECIALS REPLACED-BY &AUX LENGTH)
  "Put a quote character before each character of STRING that needs one.
Copies STRING if it has to be changed.  Any instance of a char in SPECIALS
is replaced by the corresponding char in REPLACED-BY, not quoted."
  (SETQ STRING (STRING STRING)
        LENGTH (STRING-LENGTH STRING))
  (DO ((NSTRING NIL)
       (QUOTE-IDX 0 NQUOTE-IDX)
       (NQUOTE-IDX -1))
      (NIL)
    (SETQ NQUOTE-IDX
          (DO ((I (1+ NQUOTE-IDX) (1+ I)))
              (( I LENGTH))
            (AND (NOT (MEMQ (AREF STRING I) SPECIALS))
                 (SEND SELF :CHARACTER-NEEDS-QUOTING-P (AREF STRING I))
                 (RETURN I))))
    (AND (OR NQUOTE-IDX NSTRING)
         (SETQ NSTRING (IF NSTRING
                           (STRING-APPEND NSTRING
                                          (SEND SELF :QUOTE-CHARACTER)
                                          (SUBSTRING STRING QUOTE-IDX NQUOTE-IDX))
                           (SUBSTRING STRING QUOTE-IDX NQUOTE-IDX))))
    (OR NQUOTE-IDX
        (PROGN
          (DO-FOREVER
            (LET ((I (STRING-SEARCH-SET SPECIALS (OR NSTRING STRING))))
              (OR I (RETURN))
              (OR NSTRING
                  (SETQ NSTRING (STRING-APPEND STRING)))
              (SETF (AREF NSTRING I)
                    (NTH (FIND-POSITION-IN-LIST (AREF NSTRING I) SPECIALS) REPLACED-BY))))
          (RETURN (OR NSTRING STRING))))))

(DEFMETHOD (ITS-PATHNAME-MIXIN :QUOTE-CHARACTER) () #/)

(DEFMETHOD (ITS-PATHNAME-MIXIN :CHARACTER-NEEDS-QUOTING-P) (CH)
  (MEMQ CH '(#/; #/: #/SP)))

(DEFVAR *ITS-UNINTERESTING-TYPES* '("LISP" "TEXT" NIL :UNSPECIFIC))

;; Differs from the default method in that if both type and version are specified
;; we clobber one of them to :UNSPECIFIC.
;; If only one is specified, we clobber the other to :UNSPECIFIC.
;; Exception: if either of them is :WILD, both of them are :WILD.
(DEFMETHOD (ITS-PATHNAME-MIXIN :NEW-PATHNAME)
           (&REST OPTIONS
            &KEY STARTING-PATHNAME
                 ((:VERSION -VERSION-) NIL VERSION-P)
            &ALLOW-OTHER-KEYS
            &AUX -TYPE- TYPE-P)
  (SETQ -TYPE- (CADR (GETL (LOCF OPTIONS) '(:TYPE :CANONICAL-TYPE))))
  (COND ((EQ -TYPE- :WILD)
         (SETQ -VERSION- :WILD VERSION-P T))
        ((EQ -VERSION- :WILD)
         (SETQ -TYPE- :WILD TYPE-P T))
        ((AND (NOT (MEMQ -TYPE- '(NIL :UNSPECIFIC)))
              (NOT (MEMQ -VERSION- '(NIL :UNSPECIFIC))))
         (IF (MEM 'STRING-EQUAL -TYPE- *ITS-UNINTERESTING-TYPES*)
             (SETQ -TYPE- :UNSPECIFIC TYPE-P T)
           (SETQ -VERSION- :UNSPECIFIC VERSION-P T)))
        ((NOT (MEMQ -TYPE- '(NIL :UNSPECIFIC)))
         (SETQ -VERSION- :UNSPECIFIC VERSION-P T))
        ((NOT (MEMQ -VERSION- '(NIL :UNSPECIFIC)))
         (SETQ -TYPE- :UNSPECIFIC TYPE-P T)))
  (APPLY #'MAKE-PATHNAME-1
         (IF TYPE-P :TYPE) -TYPE-
         (IF VERSION-P :VERSION) -VERSION-
         :STARTING-PATHNAME (OR STARTING-PATHNAME SELF)
         :PARSING-PATHNAME SELF
         OPTIONS))

(DEFMETHOD (ITS-PATHNAME-MIXIN :AFTER :INIT) (IGNORE)
  (AND (NOT (MEMQ TYPE '(NIL :UNSPECIFIC)))
       (NOT (MEMQ VERSION '(NIL :UNSPECIFIC)))
       (NOT (AND (EQ TYPE :WILD) (EQ VERSION :WILD)))
       (FERROR NIL "ITS-PATHNAME created with type ~S and version ~S." TYPE VERSION)))

;;; For most components, just upcase the string
(DEFMETHOD (ITS-PATHNAME-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (COND ((CONSP SPEC) (MAPCAR (LAMBDA (X)
                                (SEND SELF :PARSE-COMPONENT-SPEC X))
                              SPEC))
        ((STRINGP SPEC) (SIX-SIXBIT-CHARACTERS SPEC))
        (T SPEC)))

(DEFMETHOD (ITS-PATHNAME-MIXIN :PARSE-NAME-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (CONSP (CDR SPEC))
              (STRINGP (CADR SPEC))
              (NULL (CDDR SPEC)))
         ;; Allow list of length two as name component.
         (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T "FOO")))

(DEFMETHOD (ITS-PATHNAME-MIXIN :PARSE-VERSION-SPEC) (SPEC)
  (IF (OR (AND (FIXNUMP SPEC) (> SPEC 0))
          (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
      SPEC :NEWEST))

;;; Parse an its pathname string.   and  are quoting characters.
(DEFMETHOD (ITS-PATHNAME-MIXIN :PARSE-NAMESTRING) (HOST-SPECIFIED NAMESTRING
                                             &OPTIONAL (START 0) END)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (DO ((I START)
       (J START (1+ J))
       (CH) (TEM)
       (DEV (AND HOST-SPECIFIED "DSK"))
       (DIR) (FN1) (FN1P) (FN2)
       (TYP) (VERS))
      ((> J END)
       (COND ((NULL FN2))
             ((SETQ TEM (NUMERIC-P FN2))
              (SETQ VERS TEM TYP :UNSPECIFIC))
             ((EQUAL FN2 ">")
              (SETQ VERS :NEWEST TYP :UNSPECIFIC))
             ((EQUAL FN2 "<")
              (SETQ VERS :OLDEST TYP :UNSPECIFIC))
             ((EQUAL FN2 "*")
              (SETQ TYP :WILD VERS :WILD))
;            ((SI:MEMBER-EQUAL FN2 *ITS-UNINTERESTING-TYPES*)
;             (SETQ TYP FN2 VERS :UNSPECIFIC))
             (T
              ;; Used to use :NEWEST here.
              (SETQ TYP FN2 VERS :UNSPECIFIC)))
       (VALUES DEV DIR FN1 TYP VERS))
    (SETQ CH (IF (= J END) #/SP (AREF NAMESTRING J)))
    (COND ((MEMQ CH '(#/ #/))
           (SETQ J (1+ J)))
          ((MEMQ CH '(#/: #/; #/ #/SP #/TAB))
           (COND (( I J)
                  (SETQ TEM (cond ((and (= (1+ i) j)
                                        (= (aref namestring i) #/))
                                   :unspecific)
                                  (t
                                   (SIX-SIXBIT-CHARACTERS NAMESTRING T I J))))
                  (SELECTQ CH
                    (#/: (SETQ DEV TEM))
                    (#/; (SETQ DIR (COND ((EQUAL TEM "*") :WILD)
                                         (T TEM))))
                    (OTHERWISE
                     (COND (FN2)
                           (FN1P (SETQ FN2 TEM))
                           (T (SETQ FN1 TEM FN1P T)))))))
           (IF (EQ CH #/) (SETQ FN1P T))
           (SETQ I (1+ J))))))

;;; Truncate to six characters
(DEFUN SIX-SIXBIT-CHARACTERS (STRING &OPTIONAL QUOTE-P (START 0) (END (STRING-LENGTH STRING)))
  "Truncate STRING to six characters and make sure they are all SIXBIT characters.
START and END specify the part of STRING to use (default is all).
QUOTE-P non-NIL says insert quoting characters where appropriate."
  (DO ((I START (1+ I))
       (NCH 0) (CH)
       (NEED-COPY NIL))
      ((OR ( I END) (= NCH 6))
       (COND ((AND (= START 0) (= I (STRING-LENGTH STRING)) (NOT NEED-COPY))
              STRING)                           ;To avoid consing
             ((NOT NEED-COPY)
              (SUBSTRING STRING START I))
             (T
              (DO ((NSTRING (MAKE-ARRAY NCH :TYPE 'ART-STRING))
                   (J 0)
                   (K START (1+ K))
                   (CH))
                  (( K I) NSTRING)
                (SETQ CH (AREF STRING K))
                (COND ((NOT (AND QUOTE-P (MEMQ CH '(#/ #/))))
                       (SETQ CH (COND ((< CH #o40) (+ CH #o40))
                                      ((< CH #o140) CH)
                                      (T (- CH #o40))))
                       (SETF (AREF NSTRING J) CH)
                       (INCF J)))))))
    (SETQ CH (AREF STRING I))
    (IF (AND QUOTE-P (MEMQ CH '(#/ #/)))
        (SETQ NEED-COPY T)
        (SETQ NCH (1+ NCH)))
    (OR (AND ( CH #/SP) ( CH #/_))            ;Already legal SIXBIT
        (SETQ NEED-COPY T))))

(DEFUN NUMERIC-P (STRING &OPTIONAL PARTIAL-OK SIGN-OK)
  "If STRING is a printed representation of a number, return the number, else NIL.
PARTIAL-OK non-NIL says, if the number is not the whole of STRING,
 still return the number that there is (normally, NIL is returned)
 with a second value which is the index of the character after the number.
SIGN-OK non-NIL says a sign at the front is allowed."
  (AND (STRINGP STRING)
       (DO ((I 0 (1+ I))
            (LEN (STRING-LENGTH STRING))
            (NUM NIL)
            (SIGN 1)
            (CH))
           (( I LEN)
            (AND NUM (* NUM SIGN)))
         (SETQ CH (AREF STRING I))
         (COND ((AND SIGN-OK
                     (ZEROP I)
                     (MEMQ CH '(#/+ #/-)))
                (IF (EQ CH #/+)
                    (SETQ SIGN 1)
                  (SETQ SIGN -1)))
               ((AND ( #/9 CH)
                     ( CH #/0))
                (SETQ NUM (+ (- CH #/0) (IF NUM (* NUM 10.) 0))))
               (PARTIAL-OK
                (RETURN (values (AND NUM (* NUM SIGN)) I)))
               (T (RETURN NIL))))))

(DEFMETHOD (ITS-PATHNAME-MIXIN :INIT-FILE) (PROGRAM-NAME)
  (SEND SELF :NEW-PATHNAME :NAME USER-ID
                           :TYPE PROGRAM-NAME))

;;; These are for things like the microcode files that need to compact both a name and a type
;;; into one pathname.
(DEFMETHOD (ITS-PATHNAME-MIXIN :NEW-TYPE-AND-VERSION) (NEW-TYPE NEW-VERSION)
  (SEND SELF :NEW-PATHNAME :TYPE (SIX-SIXBIT-CHARACTERS (FORMAT NIL "~D~A"
                                                                     (\ NEW-VERSION 1000.)
                                                                     NEW-TYPE))))

(DEFMETHOD (ITS-PATHNAME-MIXIN :TYPE-AND-VERSION) (&AUX TYP VERS I)
  (COND ((STRINGP TYPE)
         (MULTIPLE-VALUE (VERS I) (NUMERIC-P TYPE T))
         (AND I (SETQ TYP (SUBSTRING TYPE I)))
         (VALUES TYP VERS))
        (T (VALUES TYPE TYPE))))

;;; Patch system interface, more kludges for only six character filenames
(DEFMETHOD (ITS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
                                                      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (IF SAME-DIRECTORY-P PATOM
                                        (IF SAME-DIRECTORY-P PATOM
                                          (IF (< (STRING-LENGTH NAM) 7.)
                                              NAM
                                            (SI:SYSTEM-SHORT-NAME NAM))))
                              :TYPE "(PDIR)"))
    (:VERSION-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (WITH-OUTPUT-TO-STRING (STREAM)
                                        (LET ((SNAME (IF SAME-DIRECTORY-P PATOM
                                                       (SI:SYSTEM-SHORT-NAME NAM))))
                                          (DOTIMES (I (MIN (STRING-LENGTH SNAME) 3))
                                            (SEND STREAM :TYO (AREF SNAME I))))
                                        (LET ((*PRINT-BASE* 10.) (*NOPOINT T))
                                          (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
                               :TYPE "(PDIR)"))
    (:PATCH-FILE
     (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~:[~*~;~C~]~D.~D"
                                              SAME-DIRECTORY-P PATOM
                                              (\ (CAR ARGS) 100.)
                                              (\ (CADR ARGS)
                                                 (IF SAME-DIRECTORY-P 100. 1000.)))
                               :TYPE (CADDR ARGS)))))

;;;;  Various TENEX-family pathnames:  TOPS20, TENEX, VMS

(DEFFLAVOR TENEX-FAMILY-PATHNAME-MIXIN () ()
  (:REQUIRED-FLAVORS PATHNAME)
  (:REQUIRED-METHODS :SUPPRESSED-DEVICE-NAMES
   :QUOTE-CHARACTER))

(DEFCONST *TENEX-WILD-CHARACTERS* '(#/* #/%))
(DEFCONST *TENEX-INTERNAL-WILD-CHARACTERS* '(#/BREAK #/RESUME))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :INTERNAL-WILD-CHARACTERS) ()
  (VALUES-LIST *TENEX-INTERNAL-WILD-CHARACTERS*))

;;; These messages are careful about consing in PATHNAME-AREA
;;; See comment by PATHNAME-AREA

;;; Changed 7/19/83 so that NIL as type and NIL as version produce no output.
;;; This is for the sake of completion of logical pathnames
;;; that translate into these.
(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-HOST) ()
  (LET ((DIR-DELIM (CAR (SEND SELF :DIRECTORY-DELIMITERS)))
        (DEV (STRING-OR-WILD DEVICE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (DIR (TENEX-FAMILY-DIRECTORY-NAME))
        (NAM (STRING-OR-WILD NAME NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (TYP (STRING-OR-WILD TYPE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (VER (IF VERSION (OR (TENEX-FAMILY-VERSION-STRING) "")))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~A:~C~A~C~@[~A~]~:[~;.~]~@[~A~]~@[~A~]"
            DEV (CAR DIR-DELIM) DIR (CDR DIR-DELIM)
            NAM (OR TYP VER) TYP VER)))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-PRINTING) ()
  (LET ((DIR-DELIM (CAR (SEND SELF :DIRECTORY-DELIMITERS)))
        (DEV (STRING-OR-WILD DEVICE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (DIR (TENEX-FAMILY-DIRECTORY-NAME))
        (NAM (STRING-OR-WILD NAME NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (TYP (STRING-OR-WILD TYPE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (VER (TENEX-FAMILY-VERSION-STRING))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT:OUTPUT NIL
      (PRINC (SEND HOST :NAME-AS-FILE-COMPUTER))
      ":"
      ;; Maybe print the device and a colon.
      (UNLESS (SI:MEMBER-EQUAL DEVICE (SEND SELF :SUPPRESSED-DEVICE-NAMES))
        (PRINC DEV) (TYO #/:))
      ;; Maybe print the directory in delimiters.
      (COND (DIR
             (TYO (CAR DIR-DELIM))
             (PRINC DIR)
             (TYO (CDR DIR-DELIM))))
      ;; Maybe print the filename.
      (WHEN (OR NAME TYPE VERSION)
        (CASE NAME
          ((NIL)
           (PRINC ""))
          (:UNSPECIFIC NIL)
          (T (PRINC NAM)))
        ;; Print "." and the type.
        (COND ((OR TYPE VER)
               (TYO #/.)
               (CASE TYPE
                 ((NIL) (PRINC ""))
                 (:UNSPECIFIC NIL)
                 (T (PRINC TYP)))))
        (IF VER (PRINC VER))))))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-WHOLINE) (&OPTIONAL (LENGTH 10000))
  (LET* ((DIR-DELIM (CAR (SEND SELF :DIRECTORY-DELIMITERS)))
         (HOST-NAME (SEND HOST :NAME-AS-FILE-COMPUTER))
         (DEV (STRING-OR-WILD DEVICE NIL
                              *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
         (DIR (TENEX-FAMILY-DIRECTORY-NAME))
         (NAM (STRING-OR-WILD NAME NIL
                              *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
         (TYP (STRING-OR-WILD TYPE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
         (VER (TENEX-FAMILY-VERSION-STRING))
;        (DEFAULT-CONS-AREA PATHNAME-AREA)
         (DEV-LENGTH (IF (SI:MEMBER-EQUAL DEVICE (SEND SELF :SUPPRESSED-DEVICE-NAMES))
                         0
                       (1+ (LENGTH DEV))))
         (TYPE-VER-LENGTH (+ (COND ((NULL TYPE) 0)
                                   ((EQ TYPE :WILD) 2)
                                   ((EQ TYPE :UNSPECIFIC) 1)
                                   (T (1+ (LENGTH TYP))))
                             (LENGTH VER))))
    (FORMAT:OUTPUT NIL
      (PRINC HOST-NAME)
      ":"
      ;; Maybe print the device and a colon.
      (UNLESS (SI:MEMBER-EQUAL DEVICE (SEND SELF :SUPPRESSED-DEVICE-NAMES))
        (PRINC DEV) (TYO #/:))
      ;; Maybe print the directory in delimiters.
      (COND (DIR
             (TYO (CAR DIR-DELIM))
             (COND ((>= LENGTH (+ (LENGTH HOST-NAME) 1
                                  DEV-LENGTH (LENGTH DIR) 2
                                  (COND ((MEMQ NAME '(NIL :UNSPECIFIC)) 0)
                                        (T (LENGTH NAM)))
                                  TYPE-VER-LENGTH))
                    (PRINC DIR))
                   (T (SEND STANDARD-OUTPUT :STRING-OUT
                            DIR 0 (MAX 0
                                       (- LENGTH
                                          (+ (LENGTH HOST-NAME) 1
                                             DEV-LENGTH 3
                                             (COND ((MEMQ NAME '(NIL :UNSPECIFIC)) 0)
                                                   (T (LENGTH NAM)))
                                             TYPE-VER-LENGTH))))
                      (TYO #/ )))
             (TYO (CDR DIR-DELIM))))
      ;; Maybe print the filename.
      (OR (MEMQ NAME '(NIL :UNSPECIFIC))
          (PRINC NAM))
      ;; Print "." and the type.
      (WHEN TYPE
        (TYO #/.)
        (UNLESS (EQ TYPE :UNSPECIFIC)
          (PRINC TYP)))
      (IF VER (PRINC VER)))))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-EDITOR) ()
  (LET ((DIR-DELIM (CAR (SEND SELF :DIRECTORY-DELIMITERS)))
        (DEV (STRING-OR-WILD DEVICE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (DIR (TENEX-FAMILY-DIRECTORY-NAME))
        (NAM (STRING-OR-WILD NAME NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (TYP (STRING-OR-WILD TYPE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (VER (TENEX-FAMILY-VERSION-STRING))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~:[~A~;~*~].~:[~A~;~*~]~@[~A~] ~:[~A~;~*~]:~@[~C~A~C~] ~A:"
            (MEMQ NAME '(NIL :UNSPECIFIC)) NAM
            (MEMQ TYPE '(NIL :UNSPECIFIC)) TYP
            VER
            (MEMQ DEVICE '(NIL :UNSPECIFIC)) DEV
            (CAR DIR-DELIM) DIR (CDR DIR-DELIM)
            (SEND HOST :NAME-AS-FILE-COMPUTER))))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-DIRED) ()
  (LET ((NAM (STRING-OR-WILD NAME NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (TYP (STRING-OR-WILD TYPE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (VER (TENEX-FAMILY-VERSION-STRING))
        (DEFAULT-CONS-AREA PATHNAME-AREA))
    (FORMAT NIL "~:[~A~;~*~].~:[~A~;~*~]~@[~A~]"
            (MEMQ NAME '(NIL :UNSPECIFIC)) NAM
            (MEMQ TYPE '(NIL :UNSPECIFIC)) TYP
            VER)))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :STRING-FOR-DIRECTORY) ()
  (LET ((DIR-DELIM (CAR (SEND SELF :DIRECTORY-DELIMITERS)))
        (DEV (STRING-OR-WILD DEVICE NIL
                             *TENEX-INTERNAL-WILD-CHARACTERS* *TENEX-WILD-CHARACTERS*))
        (DEFAULT-CONS-AREA PATHNAME-AREA)
        (DIR (TENEX-FAMILY-DIRECTORY-NAME)))
    (FORMAT NIL "~:[~A:~;~*~]~@[~C~A~C~]"
          (SI:MEMBER-EQUAL DEVICE (SEND SELF :SUPPRESSED-DEVICE-NAMES)) DEV
          (CAR DIR-DELIM) DIR (CDR DIR-DELIM))))

(DEFUN TENEX-FAMILY-DIRECTORY-NAME ()
  (DECLARE (:SELF-FLAVOR TENEX-FAMILY-PATHNAME-MIXIN))
  (COND ((MEMQ DIRECTORY '(NIL :UNSPECIFIC)) NIL)
        ((ATOM DIRECTORY) (STRING-OR-WILD DIRECTORY NIL
                                          *TENEX-INTERNAL-WILD-CHARACTERS*
                                          *TENEX-WILD-CHARACTERS*))
        (T (FORMAT NIL "~{~A~^.~}"
                   (LOOP FOR ELT IN DIRECTORY
                         COLLECT
                         (STRING-OR-WILD ELT NIL
                                         *TENEX-INTERNAL-WILD-CHARACTERS*
                                         *TENEX-WILD-CHARACTERS*))))))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :WILD-P) ()
  ;; Note: method-combination type is :OR.
  (IF (OR (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* NAME)
          (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* TYPE)
          (IF (STRINGP DIRECTORY)
              (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* DIRECTORY)
            (IF (CONSP DIRECTORY)
                (DOLIST (DC DIRECTORY)
                  (IF (OR (EQ DC :WILD)
                          (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* DC))
                      (RETURN T))))))
      T))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :DIRECTORY-WILD-P) ()
  (OR (EQ DIRECTORY :WILD)
      (IF (STRINGP DIRECTORY)
          (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* DIRECTORY)
        (IF (CONSP DIRECTORY)
            (DOLIST (DC DIRECTORY)
              (IF (OR (EQ DC :WILD)
                      (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* DC))
                  (RETURN T)))))))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :NAME-WILD-P) ()
  (OR (EQ NAME :WILD)
      (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* NAME)))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :TYPE-WILD-P) ()
  (OR (EQ TYPE :WILD)
      (STRING-SEARCH-SET *TENEX-INTERNAL-WILD-CHARACTERS* TYPE)))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :VERSION-DELIMITER) () #/.)
(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :DIRECTORY-DELIMITERS) ()
  '((#/< . #/>) (#/[ . #/])))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :INIT-FILE) (PROGRAM-NAME)
  (SEND SELF :NEW-PATHNAME :NAME (STRING-UPCASE PROGRAM-NAME)
                           :TYPE "INIT"
                           :VERSION :NEWEST))

(DEFUN TENEX-FAMILY-VERSION-STRING ()
  (DECLARE (:SELF-FLAVOR TENEX-FAMILY-PATHNAME-MIXIN))
  (SELECTQ VERSION
    (:NEWEST NIL)
    ((NIL) ".")
    (:UNSPECIFIC
     (STRING (SEND SELF :VERSION-DELIMITER)))
    (T
     (FORMAT NIL "~C~D"
             (SEND SELF :VERSION-DELIMITER)
             (SELECTQ VERSION
               (:OLDEST -2)
               (:WILD "*")
               (OTHERWISE VERSION))))))

;;I think it's a no-op, let's see (3/30/83)
;(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
;  (COND ((CONSP SPEC) (MAPCAR (LAMBDA (X)
;                                (SEND SELF :PARSE-COMPONENT-SPEC X))
;                             SPEC))
;       ((STRINGP SPEC)
;;       ;; Convert wild characters to their internal form.
;;This doesn't do that!
;        (TENEX-FAMILY-STRING-UNTIL-DELIM SPEC NIL 0 NIL T))
;       (T SPEC)))

;(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-VERSION-SPEC) (SPEC)
;  (IF (OR (AND (FIXNUMP SPEC) ( SPEC 0))
;         (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
;      SPEC :NEWEST))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
;       ;; Canonicalize list of length 1 into a single string.
;       ((AND (CONSP SPEC)
;             (STRINGP (CAR SPEC))
;             (NULL (CDR SPEC)))
;        (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((CONSP SPEC)
         (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

;;; Parse a TENEX-family pathname string.
(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :PARSE-NAMESTRING)
           (HOST-SPECIFIED-P NAMESTRING &OPTIONAL (START 0) END
            &AUX (WILD-STRINGS '(#.(STRING #/BREAK))))
  (DECLARE (VALUES DEVICE DIRECTORY NAME TYPE VERSION
                   DEVICE-SPECIFIED-P DIRECTORY-SPECIFIED-P NAME-SPECIFIED-P TYPE-SPECIFIED-P
                   VERSION-SPECIFIED-P))
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (LET* ((DIR-DELIM-ALIST (SEND SELF :DIRECTORY-DELIMITERS))
         (ALL-DELIMS (NCONC (MAPCAR #'CAR DIR-DELIM-ALIST) '(#/: #/. #/; #/SP))))
    (DO ((IDX (OR (STRING-SEARCH-NOT-CHAR #/SP NAMESTRING START END) END))
         (TEM) (TEM1) (DELIM)
         (DIR-DELIM)
         (DEV)
         (DIR) (NAM) (TYP) (VER)
         DEV-SPECIFIED-P NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P)
        (( IDX END)
         (IF (EQUAL TYP "") (SETQ TYP :UNSPECIFIC))
         (IF (EQUAL NAM "") (SETQ NAM NIL))
         (SETQ DEV (OR DEV (IF HOST-SPECIFIED-P (SEND SELF :PRIMARY-DEVICE))))
         (VALUES DEV DIR NAM TYP VER
                 DEV-SPECIFIED-P DIR NAM-SPECIFIED-P TYP-SPECIFIED-P VER-SPECIFIED-P))
      (COND ((SETQ DIR-DELIM (CDR (ASSQ (AREF NAMESTRING IDX) DIR-DELIM-ALIST)))
             (AND DIR
                  (PATHNAME-ERROR IDX NAMESTRING "Directory occurs twice"))
             (INCF IDX)
             (DO-FOREVER
               (MULTIPLE-VALUE (TEM IDX DELIM)
                 (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING
                                                  (LIST #/. DIR-DELIM) IDX END NIL T))
               (IF (SI:MEMBER-EQUAL TEM WILD-STRINGS) (SETQ TEM :WILD))
               (SETQ DIR (IF (AND (= DELIM DIR-DELIM) (NULL DIR))
                             (LIST TEM)
                           (NCONC DIR (NCONS TEM))))
               (AND (= DELIM DIR-DELIM) (RETURN))))
            (T
             (MULTIPLE-VALUE (TEM IDX DELIM)
               (TENEX-FAMILY-STRING-UNTIL-DELIM NAMESTRING ALL-DELIMS IDX END T T))
             (COND ((ASSQ DELIM DIR-DELIM-ALIST)
                    (SETQ IDX (1- IDX)))
                   ((AND (= DELIM #/;) VER)     ;Protect against twenex attribute usage
                    (SETQ IDX END)))
             (IF (SI:MEMBER-EQUAL TEM WILD-STRINGS) (SETQ TEM :WILD))
             (COND ((= DELIM #/:)
                    (AND DEV
                         (PATHNAME-ERROR IDX NAMESTRING "Device occurs twice" NAMESTRING))
                    (SETQ DEV TEM DEV-SPECIFIED-P (1- IDX)))
                   ((= DELIM #/;)
                    (COND ((NULL NAM-SPECIFIED-P)
                           (SETQ NAM TEM TYP ""
                                 NAM-SPECIFIED-P (1- IDX) TYP-SPECIFIED-P (1- IDX)))
                          ((NULL TYP-SPECIFIED-P)
                           (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX)))))
                   ((NULL NAM-SPECIFIED-P)
                    (SETQ NAM TEM NAM-SPECIFIED-P (1- IDX))
                    (IF (= DELIM #/.) (SETQ TYP :UNSPECIFIC)))
                   ((NULL TYP-SPECIFIED-P)
                    (SETQ TYP TEM TYP-SPECIFIED-P (1- IDX))
                    (IF (EQ DELIM #/.) (SETQ VER :UNSPECIFIC)))
                   ((NULL VER-SPECIFIED-P)
                    (SETQ VER-SPECIFIED-P (1- IDX))
                    (COND ((NULL TEM)
                           (SETQ VER NIL))
                          ((EQUAL TEM "")
                           (SETQ VER :UNSPECIFIC))
                          ((SETQ TEM1 (NUMERIC-P TEM))
                           (SETQ VER TEM1))
                          ((EQ TEM :WILD)
                           (SETQ VER :WILD))
                          ((SEND SELF :OLDEST-CHECK TEM)
                           (SETQ VER :OLDEST))
                          (T (PATHNAME-ERROR IDX NAMESTRING "Version must be numeric"))))))))))

;;; Internal parsing function, read the next atom from string to one of delims
;;; EOS-OK means it is alright to fall off the end of the string, that is treated
;;; as a delimiter and -1 is returned for the third value.
;;; QUOTE-P says look for quoting characters (and remove them in the result).
(DEFUN TENEX-FAMILY-STRING-UNTIL-DELIM (STRING DELIMS &OPTIONAL (START 0) END EOS-OK QUOTE-P
                                        &AUX IDX DELIM (NCH 0) (NEED-COPY NIL))
  (DECLARE (VALUES SUBSTRING END DELIM))
  (OR END (SETQ END (STRING-LENGTH STRING)))
  (DO ((I START (1+ I))
       (CHAR))
      (( I END)
       (OR EOS-OK (PATHNAME-ERROR I STRING "Illegal end of string"))
       (SETQ IDX END DELIM -1))
    (SETQ CHAR (AREF STRING I))
    (COND (( #/A CHAR #/Z))
          ((AND QUOTE-P (= CHAR #/))
           ;; TOPS-20 quoting character
           (AND ( (SETQ I (1+ I)) END)
                (PATHNAME-ERROR I STRING "End of string after quoting character ~C" #/))
           (SETQ NEED-COPY T
                 NCH (1+ NCH)))
          ((MEMQ CHAR DELIMS)
           (SETQ IDX I DELIM CHAR)
           (RETURN))
          (QUOTE-P
           (IF (MEMQ CHAR '(#/* #/%))
               (SETQ NEED-COPY T))
           (AND ( CHAR #/a) ( CHAR #/z)
                (SETQ NEED-COPY T)))))
  ;; NCH is number of characters that we will discard.
  ;; NEED-COPY is T if either we will discard some chars or we must upcase some.
  (VALUES (COND ((AND QUOTE-P (= IDX (1+ START))
                      (STRING-EQUAL STRING "" :start1 START :start2 0 :end1 IDX :end2 1))
                 NIL)
                ((AND (= START 0) (= IDX (STRING-LENGTH STRING)) (NOT NEED-COPY))
                 STRING)                        ;Avoid consing
                ((NOT NEED-COPY)
                 (SUBSTRING STRING START IDX))
                (T
                 (DO ((SUBSTRING (MAKE-ARRAY (- IDX START NCH) :TYPE 'ART-STRING))
                      (I 0)
                      (J START (1+ J))
                      (QUOTE-P NIL)
                      (CHAR))
                     (( J IDX)
                      SUBSTRING)
                   (SETQ CHAR (LOGAND #o177 (AREF STRING J)))
                   (IF (AND (NOT QUOTE-P)
                            (MEMQ CHAR '(#/% #/*)))
                       (SETQ CHAR (IF (= CHAR #/%) #/RESUME #/BREAK)))
                   (IF (AND (NOT QUOTE-P) (= CHAR #/))
                       (SETQ QUOTE-P T)
                     (IF QUOTE-P
                         (SETQ QUOTE-P NIL)
                       (SETQ CHAR (CHAR-UPCASE CHAR)))
                     (SETF (AREF SUBSTRING I) CHAR)
                     (INCF I)))))
          (1+ IDX) DELIM))

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :DIRECTORY-FILE-TYPE) ()
  "DIRECTORY")

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :DIRECTORY-PATHNAME-AS-FILE)
           DEFAULT-DIRECTORY-PATHNAME-AS-FILE)

(DEFMETHOD (TENEX-FAMILY-PATHNAME-MIXIN :UNDELETABLE-P) () T)

;;;; TOPS-20 support
(DEFFLAVOR TOPS20-PATHNAME-MIXIN () (HIERARCHICAL-DIRECTORY-MIXIN TENEX-FAMILY-PATHNAME-MIXIN)
  (:REQUIRED-FLAVORS PATHNAME))

;;; Differs from the standard one in that we replace "DSK" with "PS".
(DEFMETHOD (TOPS20-PATHNAME-MIXIN :NEW-PATHNAME) (&REST OPTIONS
                                                  &KEY STARTING-PATHNAME
                                                       ((:DEVICE DEV))
                                                  &ALLOW-OTHER-KEYS)
  (IF (EQ DEV :UNSPECIFIC)
      (APPLY #'MAKE-PATHNAME-1
             :STARTING-PATHNAME (OR STARTING-PATHNAME SELF)
             :PARSING-PATHNAME SELF
             :DEVICE (SEND HOST :PRIMARY-DEVICE)
             OPTIONS)
      (APPLY #'MAKE-PATHNAME-1
             :STARTING-PATHNAME (OR STARTING-PATHNAME SELF)
             :PARSING-PATHNAME SELF
             OPTIONS)))

(DEFMETHOD (TOPS20-PATHNAME-MIXIN :SUPPRESSED-DEVICE-NAMES) () '(NIL :UNSPECIFIC))

(DEFMETHOD (TOPS20-PATHNAME-MIXIN :QUOTE-CHARACTER) () #/)

(DEFMETHOD (TOPS20-PATHNAME-MIXIN :CHARACTER-NEEDS-QUOTING-P) (CH)
  (NOT (OR (AND ( CH #/A) ( CH #/Z))
           (AND ( CH #/0) ( CH #/9))
           (MEMQ CH '(#/# #/$ #/_ #/-)))))

(DEFMETHOD (TOPS20-PATHNAME-MIXIN :OLDEST-CHECK) (STRING)
  (STRING-EQUAL STRING "-2"))

(DEFUN DEFAULT-DIRECTORY-PATHNAME-AS-FILE (IGNORE &AUX DIR NAM)
  (DECLARE (:SELF-FLAVOR PATHNAME))
  (COND ((EQ DIRECTORY :ROOT)
         (FERROR 'PATHNAME-PARSE-ERROR "There is no pathname for the root as a file"))
        ((OR (ATOM DIRECTORY) (NULL (CDR DIRECTORY)))
         (SETQ DIR :ROOT
               NAM (IF (CONSP DIRECTORY) (CAR DIRECTORY) DIRECTORY)))
        (T
         (LET ((LAST (LAST DIRECTORY)))
           (SETQ DIR (LDIFF DIRECTORY LAST)
                 NAM (CAR LAST)))))
  (SEND SELF :NEW-PATHNAME :RAW-DIRECTORY DIR
                           :RAW-NAME NAM
                           :TYPE (SEND SELF :DIRECTORY-FILE-TYPE)))

(DEFMETHOD (TOPS20-PATHNAME-MIXIN :PARSE-TRUENAME) (STRING)
  ;Since pathname is definitely for this host, avoid probing "structure" as host.
  ;easiest way to do this is to prefix real host name before string if there is only one
  ;apparent host in string.
  (parse-pathname (tops20-local-pathstring string host) host))

(defun tops20-local-pathstring (string host)
 "Parse a tops20 pathstring which may not contain the <host>:
without risk of interpreting the structure as a host"
  (cond ((stringp string)
         (multiple-value-bind (host1 idx1)
             (tops20-find-host string 0)
           (cond ((and host1
                       (null (tops20-find-host string idx1)))
                  (string-append (send host :name-as-file-computer)
                                 ":"
                                 string))
                 (t string))))
        (t string)))

(defun tops20-find-host (string start)
  (DO ((IDX start (1+ IDX))
       (HOST-START start)
       (ONLY-WHITESPACE-P T)
       (CHAR))
      (( IDX (string-length string)))
    (COND ((= (SETQ CHAR (AREF STRING IDX)) #/:)
           ;; The first atom ends with a colon, take the host from that, and
           ;; parse from the end of that.
           (RETURN (values (SUBSTRING STRING HOST-START IDX) (1+ IDX))))
          ((AND (= CHAR #/SP) ONLY-WHITESPACE-P)       ;Skip leading spaces
           (SETQ HOST-START (1+ IDX)))
          (T
           (SETQ ONLY-WHITESPACE-P NIL)
           (OR (ALPHANUMERICP CHAR)
               (= CHAR #/.)
               (= CHAR #/-)
               ;; If we get to non-alphabetic or -numeric,
               ;; then no interesting colon
               (RETURN NIL))))))

;;;; Tenex support
(DEFFLAVOR TENEX-PATHNAME-MIXIN () (TENEX-FAMILY-PATHNAME-MIXIN))

(DEFMETHOD (TENEX-PATHNAME-MIXIN :VERSION-DELIMITER) () #/;)

(DEFMETHOD (TENEX-PATHNAME-MIXIN :SUPPRESSED-DEVICE-NAMES) () '(NIL :UNSPECIFIC "DSK"))

(DEFMETHOD (TENEX-PATHNAME-MIXIN :CHARACTER-NEEDS-QUOTING-P) (CH)
  (NOT (OR ( #/A CH #/Z)
           ( #/0 CH #/9)
           ( #/! CH #/$)
           ( #/& CH #/))
           (MEMQ CH '(#/+ #// #/= #/-))
           ( #/[ CH #/^))))

(DEFMETHOD (TENEX-PATHNAME-MIXIN :OLDEST-CHECK) (STRING)
  (STRING-EQUAL STRING "-2"))

(DEFMETHOD (TENEX-PATHNAME-MIXIN :QUOTE-CHARACTER) ()
  #/)

;;;; VMS support
(DEFFLAVOR VMS-PATHNAME-MIXIN () (MEANINGFUL-ROOT-MIXIN HIERARCHICAL-DIRECTORY-MIXIN
                                  TENEX-FAMILY-PATHNAME-MIXIN)
  (:REQUIRED-FLAVORS PATHNAME))

(DEFMETHOD (VMS-PATHNAME-MIXIN :OLDEST-CHECK) (IGNORE) NIL)     ;Maybe make the server simulate?
(DEFMETHOD (VMS-PATHNAME-MIXIN :VERSION-DELIMITER) () #/;)

(DEFMETHOD (VMS-PATHNAME-MIXIN :SUPPRESSED-DEVICE-NAMES) () '(NIL :UNSPECIFIC))

;;; Let the TOPS-20 parser do the work.  Then take the
;;; result, check it against the more limited VMS constraints.

(DEFWRAPPER (VMS-PATHNAME-MIXIN :PARSE-NAMESTRING) (ARGLIST . BODY)
  `(LET ((NAMESTRING (SECOND ARGLIST)))
     (IF (STRING-SEARCH-CHAR #/ (NTH 1 ARGLIST))
         (FERROR 'PATHNAME-PARSE-ERROR "Illegal chararacter in ~A" (NTH 1 ARGLIST)))
     (MULTIPLE-VALUE-BIND (DEV DIR NAM TYP VER NIL NIL
                           NAM-SPECIFIED-P TYP-SPECIFIED-P)
         (PROGN ,@BODY)
       (AND TYP-SPECIFIED-P
            (MEMQ VER '(NIL :UNSPECIFIC))
            (< TYP-SPECIFIED-P (LENGTH NAMESTRING))
            (EQ (AREF NAMESTRING TYP-SPECIFIED-P) #/;)
            (SETQ VER :NEWEST))
       (AND NAM-SPECIFIED-P
            (EQ NAM NIL)
            ( (AREF NAMESTRING (1- NAM-SPECIFIED-P)) #/)
            (SETQ NAM :UNSPECIFIC))
       (AND (EQUAL DIR '("000000"))
            (SETQ DIR :ROOT))
       (VALUES DEV DIR NAM TYP VER))))

;;; There are no quoting characters possible, sigh.
(DEFMETHOD (VMS-PATHNAME-MIXIN :QUOTE-CHARACTER) () ())
(DEFMETHOD (VMS-PATHNAME-MIXIN :CHARACTER-NEEDS-QUOTING-P) (IGNORE) ())
(DEFMETHOD (VMS-PATHNAME-MIXIN :DIRECTORY-DELIMITERS) ()
  '((#/[ . #/]) (#/< . #/>)))

;;; The VMS character set is absurdly limited.  devname:<dir.dir.dir>name.typ,
;;; where devname can be alphanumerics plus $ and _, and the rest can be alphanumerics.

(DEFVAR VMS-DEVNAME-CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$_")
;; Break and Resume included since they are internal representatives of wildcards.
(DEFVAR VMS-FILENAME-CHARSET "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(DEFMETHOD (VMS-PATHNAME-MIXIN :DIRECTORY-FILE-TYPE) ()
  "DIR")

(DEFMETHOD (VMS-PATHNAME-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (IF (STRINGP SPEC) (STRING-UPCASE SPEC) SPEC))

(defun vms-suggested-component (original charset limit &aux (new ""))
  (dotimes (i (string-length original) new)
    (when (string-search (char original i) charset)
      (setq new (string-append new (char-upcase (char original i))))
      (decf limit))
    (when (zerop limit)
      (return new))))

(DEFMACRO VMS-PARSE-PATHNAME-COMPONENT (OPERATION COMPONENT SIZE CHARSET DEFAULT)
  `(COND ((MEMQ ,COMPONENT '(NIL :UNSPECIFIC :WILD)) ,COMPONENT)
         ((STRINGP ,COMPONENT)
          (LET ((SPEC1 (IF (< (LENGTH ,COMPONENT) ,SIZE)
                           ,COMPONENT
                         (SUBSTRING ,COMPONENT 0 ,SIZE))))
            (IF (STRING-SEARCH-NOT-SET ,CHARSET SPEC1)
                ,DEFAULT
              SPEC1)))
        ((AND (CONSP ,COMPONENT)
              (STRINGP (CAR ,COMPONENT))
              (NULL (CDR ,COMPONENT)))
         (SEND SELF ',OPERATION (CAR ,COMPONENT)))
        (T ,DEFAULT)))

(DEFMETHOD (VMS-PATHNAME-MIXIN :PARSE-DEVICE-SPEC) (SPEC)
  (VMS-PARSE-PATHNAME-COMPONENT :PARSE-DEVICE-SPEC SPEC 64. VMS-DEVNAME-CHARSET
                                (PATHNAME-DEVICE (QUIET-USER-HOMEDIR HOST))))

(DEFMETHOD (VMS-PATHNAME-MIXIN :PARSE-NAME-SPEC) (SPEC)
  (VMS-PARSE-PATHNAME-COMPONENT :PARSE-NAME-SPEC SPEC 9 vms-filename-charset
                                (vms-suggested-component spec vms-filename-charset 9)))

(DEFMETHOD (VMS-PATHNAME-MIXIN :PARSE-TYPE-SPEC) (SPEC)
  (VMS-PARSE-PATHNAME-COMPONENT :PARSE-NAME-SPEC SPEC 3 VMS-FILENAME-CHARSET "LSP"))

(DEFMETHOD (VMS-PATHNAME-MIXIN :AROUND :PARSE-DIRECTORY-SPEC) (CONT MT ARGS IGNORE)
  (LET ((PARSED-SPEC (AROUND-METHOD-CONTINUE CONT MT ARGS)))
    (COND ((STRINGP PARSED-SPEC)
           (VMS-PARSE-PATHNAME-COMPONENT NIL PARSED-SPEC 9. VMS-FILENAME-CHARSET
                                         (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST))))
          ((CONSP PARSED-SPEC)
           (SETQ PARSED-SPEC
                 (LOOP FOR ELT IN PARSED-SPEC
                       COLLECT
                       (VMS-PARSE-PATHNAME-COMPONENT NIL ELT 9. VMS-FILENAME-CHARSET
                                                     T)))
           (IF (MEMQ T PARSED-SPEC)
               (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST))
             PARSED-SPEC))
          (T PARSED-SPEC))))

;(DEFMETHOD (VMS-PATHNAME-MIXIN :VALID-NAME-P) (NAM)
;  (VMS-DEFAULT-PATHNAME-COMPONENT-TEST NAM 9.))

;(DEFUN VMS-DEFAULT-PATHNAME-COMPONENT-TEST (COMPONENT SIZE)
;  (OR (MEMQ COMPONENT '(NIL :UNSPECIFIC :WILD))
;      (AND (STRINGP COMPONENT)
;          (NOT (STRING-SEARCH-NOT-SET VMS-FILENAME-CHARSET COMPONENT))
;          (<= (STRING-LENGTH COMPONENT) SIZE))))

;(DEFMETHOD (VMS-PATHNAME-MIXIN :VALID-TYPE) (TYP)
;  (IF (VMS-DEFAULT-PATHNAME-COMPONENT-TEST TYP 3.)
;      TYP "TXT"))

;(DEFMETHOD (VMS-PATHNAME-MIXIN :VALID-TYPE-P) (TYP)
;  (VMS-DEFAULT-PATHNAME-COMPONENT-TEST TYP 3))

;(DEFMETHOD (VMS-PATHNAME-MIXIN :VALID-VERSION-P) (VERS)
;  (OR (MEMQ VERSION '(:WILD :NEWEST))
;      (NULL VERS)
;      (AND (FIXNUMP VERS) (< 0 VERS 65536))))

;(DEFMETHOD (VMS-PATHNAME-MIXIN :VALID-VERSION) (VERS)
;  (IF (SEND SELF :VALID-VERSION-P VERS) VERS
;    :NEWEST))


;;; Patch system interface, more kludges for only 9 character VMS filenames
(DEFMETHOD (VMS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
                                                      &REST ARGS)
  (SELECTQ TYP
    (:SYSTEM-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (IF SAME-DIRECTORY-P PATOM
                                           (IF (< (STRING-LENGTH NAM) 10.)
                                               NAM
                                              (SI:SYSTEM-SHORT-NAME NAM)))
                              :TYPE :PATCH-DIRECTORY
                              :VERSION :NEWEST))
    (:VERSION-DIRECTORY
     (SEND SELF :NEW-PATHNAME :NAME (WITH-OUTPUT-TO-STRING (STREAM)
                                           (LET ((SNAME (IF SAME-DIRECTORY-P PATOM
                                                          (SI:SYSTEM-SHORT-NAME NAM))))
                                             (DOTIMES (I (MIN (STRING-LENGTH SNAME) 6))
                                               (SEND STREAM :TYO (AREF SNAME I))))
                                           (LET ((*PRINT-BASE* 10.) (*NOPOINT T))
                                             (PRIN1 (\ (CAR ARGS) 1000.) STREAM)))
                              :TYPE :PATCH-DIRECTORY :VERSION :NEWEST))
    (:PATCH-FILE
     (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~:[~*~;~C~]~DP~D"
                                              SAME-DIRECTORY-P PATOM
                                              (\ (CAR ARGS) 100.)
                                              (\ (CADR ARGS)
                                                 (IF SAME-DIRECTORY-P 100. 1000.)))
                              :TYPE (CADDR ARGS) :VERSION :NEWEST))))

(DEFMETHOD (VMS-PATHNAME-MIXIN :UNDELETABLE-P) () NIL)

;;;; Unix and Multics support

(DEFFLAVOR UNIX-PATHNAME-MIXIN () (HIERARCHICAL-DIRECTORY-MIXIN MEANINGFUL-ROOT-MIXIN
                                   PATHNAME-NORMALLY-LOWERCASE-MIXIN)
  (:REQUIRED-FLAVORS PATHNAME))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :CHARACTER-NEEDS-QUOTING-P) (IGNORE) ())

(DEFMETHOD (UNIX-PATHNAME-MIXIN :UNSPECIFIC-TYPE-IS-DEFAULT) () T)

(DEFMETHOD (UNIX-PATHNAME-MIXIN :DIRECTORY-PATHNAME-AS-FILE)
           DEFAULT-DIRECTORY-PATHNAME-AS-FILE)

(DEFMETHOD (UNIX-PATHNAME-MIXIN :DIRECTORY-FILE-TYPE) ()
  :UNSPECIFIC)

(DEFMETHOD (UNIX-PATHNAME-MIXIN :DIRECTORY-DELIMITER-CHARACTER) () #//)

(DEFMETHOD (UNIX-PATHNAME-MIXIN :DIRECTORY-UP-DELIMITER) () "..")
(DEFUN UNIX-FILENAME (NAME TYPE &AUX (NEW-TYPE (IF TYPE TYPE "'")))
  (IF (EQ NAME :UNSPECIFIC) (SETQ NAME ""))
  (IF (EQ NAME :WILD)
      (IF (MEMQ TYPE '(:WILD :UNSPECIFIC)) "*"  ;Both wild, just *
        (STRING-APPEND "*." NEW-TYPE))
    (IF (AND (NULL NAME) (MEMQ TYPE '(NIL :UNSPECIFIC)))
        ""
      (OR NAME (SETQ NAME "'"))
      (COND ((EQ TYPE :WILD)
             (FORMAT NIL "~A.*" NAME))
            ((EQ TYPE :UNSPECIFIC)
             NAME)
            (T
             (STRING-APPEND NAME "." NEW-TYPE))))))

;;; was going to change above form to this.  maybe later --dg
;(defun unix-filename (name type)
;  (string-append (case name
;                  ((:unspecific nil) "")
;                  (:wild "*")
;                  (t name))
;                (if (and (stringp type)
;                         (zerop (string-length type)))
;                    ""
;                  (case type
;                    ((:unspecific nil) "")
;                    (:wild (if (eq name :wild) "" ".*"))
;                    ("" "")
;                    (t (format nil ".~A" type))))))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :STRING-FOR-HOST) ()
  (FORMAT NIL "~@[~A~]~A" (UNIX-DIRECTORY-STRING) (UNIX-FILENAME NAME TYPE)))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :STRING-FOR-EDITOR) ()
  (FORMAT NIL "~A ~A ~A:"
          (UNIX-FILENAME NAME TYPE) (UNIX-DIRECTORY-STRING)
          (SEND HOST :NAME-AS-FILE-COMPUTER)))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :STRING-FOR-DIRED) ()
  (UNIX-FILENAME NAME TYPE))

(defmethod (unix-pathname-mixin :directory-string) ()
  (unix-directory-string))

(DEFUN UNIX-DIRECTORY-STRING ()
  (DECLARE (:SELF-FLAVOR UNIX-PATHNAME-MIXIN))
  (IF (MEMQ DIRECTORY '(NIL :UNSPECIFIC)) NIL
      (LET ((DIRECT DIRECTORY)
            (SUPPRESS-DELIM NIL))
        (STRING-APPEND (COND ((EQ DIRECT :ROOT) "")
                             ((AND (EQ (CAR-SAFE DIRECT) :RELATIVE))
                              (POP DIRECT)
                              "")
                             (T
                              (SEND SELF :DIRECTORY-DELIMITER-CHARACTER)))
                       (COND ((EQ DIRECT :ROOT) "")
                             ((ATOM DIRECT) (UNIX-DIRECTORY-COMPONENT DIRECT))
                             ((NULL (CDR DIRECT))
                              (LET (STRING)
                                (MULTIPLE-VALUE (STRING SUPPRESS-DELIM)
                                  (UNIX-DIRECTORY-COMPONENT (CAR DIRECT)))
                                STRING))
                             (T (LOOP FOR SUBDIR IN DIRECT
                                      WITH STRING = (MAKE-STRING 20 :FILL-POINTER 0)
                                      AS DELIM-P = NIL THEN T
                                      DO (AND DELIM-P (NOT SUPPRESS-DELIM)
                                              (ARRAY-PUSH-EXTEND
                                                STRING
                                                (SEND SELF
                                                  :DIRECTORY-DELIMITER-CHARACTER)))
                                         (LET (SUBSTR)
                                           (MULTIPLE-VALUE (SUBSTR SUPPRESS-DELIM)
                                             (UNIX-DIRECTORY-COMPONENT SUBDIR))
                                           (SETQ STRING (STRING-NCONC STRING SUBSTR)))
                                      FINALLY (RETURN STRING))))
                     (COND (SUPPRESS-DELIM "")
                           (T (SEND SELF :DIRECTORY-DELIMITER-CHARACTER)))))))

(DEFUN UNIX-DIRECTORY-COMPONENT (STRING)
  (SELECTQ STRING
    (:WILD "*")
    (:UP (LET ((DELIM (SEND SELF :DIRECTORY-UP-DELIMITER)))
           (IF (STRINGP DELIM) DELIM (VALUES (STRING DELIM) T))))
    (OTHERWISE (STRING STRING))))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :PARSE-NAMESTRING) (IGNORE NAMESTRING
                                                      &OPTIONAL (START 0) END
                                                      &AUX DIR NAM TYP (VER :UNSPECIFIC)
                                                      DELIM-CHAR DIRSTART DIREND)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (SETQ START (OR (STRING-SEARCH-NOT-CHAR #/SP NAMESTRING START END) END))
  (SETQ END (1+ (OR (STRING-REVERSE-SEARCH-NOT-CHAR #/SP NAMESTRING END START) (1- START))))
  (SETQ DELIM-CHAR (SEND SELF :DIRECTORY-DELIMITER-CHARACTER))
  (LET (I)
    (IF (AND (SETQ I (STRING-SEARCH-CHAR #/SP NAMESTRING START END))
             (CHAR-EQUAL DELIM-CHAR (AREF NAMESTRING (1- END)))
             (NOT (STRING-SEARCH-CHAR DELIM-CHAR NAMESTRING START I)))
        (SETQ DIRSTART (STRING-SEARCH-NOT-CHAR #/SP NAMESTRING I END)
              DIREND END
              END I)
      (SETQ DIRSTART START
            DIREND (STRING-REVERSE-SEARCH-CHAR DELIM-CHAR NAMESTRING END START)
            START (IF DIREND (1+ DIREND) START))))
  ;; Now START..END are the indices around the name and type,
  ;; and DIRSTART..DIREND are the indices around the directory.
  (WHEN DIREND
    (SETQ DIR (LET ((RELATIVE-P T)
                    (DIRIDX DIRSTART)
                    (UP (SEND SELF :DIRECTORY-UP-DELIMITER))
                    (NUP NIL)
                    (STRS NIL))
                (COND ((= (AREF NAMESTRING DIRIDX) DELIM-CHAR)
                       (SETQ RELATIVE-P NIL)
                       (SETQ DIRIDX (STRING-SEARCH-NOT-CHAR
                                      DELIM-CHAR NAMESTRING DIRIDX))))
                (AND DIRIDX (> DIREND DIRIDX)
                     (SETQ STRS (LOOP FOR IDX = DIRIDX THEN JDX
                                      AS JDX = (STRING-SEARCH-CHAR
                                                 DELIM-CHAR NAMESTRING IDX DIREND)
                                      COLLECT (SUBSTRING NAMESTRING IDX (OR JDX DIREND))
                                      WHILE
                                      (AND JDX
                                           (SETQ JDX
                                                 (STRING-SEARCH-NOT-CHAR
                                                   DELIM-CHAR NAMESTRING JDX DIREND))))))
                (AND (STRINGP UP)
                     (DO L STRS (CDR L) (NULL L)
                         (AND (STRING-EQUAL (CAR L) UP)
                              (RPLACA L :UP))))
                (AND NUP (SETQ STRS (NCONC NUP STRS)))
                (COND (RELATIVE-P (CONS :RELATIVE STRS))
                      ((NULL STRS) :ROOT)
                      ((NULL (CDR STRS)) (CAR STRS))
                      (T STRS)))))
  (SETQ TYP (STRING-REVERSE-SEARCH-CHAR #/. NAMESTRING END START))
  (IF (EQ TYP START) (SETQ TYP NIL))            ;Initial . is part of NAM
  (IF TYP (PSETQ END TYP
                 TYP (SUBSTRING NAMESTRING (1+ TYP) END)))
  (SETQ NAM (AND ( START END) (SUBSTRING NAMESTRING START END)))
  (COND ((EQUAL NAM "'") (SETQ NAM NIL))
        ((EQUAL NAM "*") (SETQ NAM :WILD)))
  (COND ((NULL TYP) (SETQ TYP (AND NAM :UNSPECIFIC)))
        ((EQUAL TYP "'") (SETQ TYP NIL))
        ((EQUAL TYP "*") (SETQ TYP :WILD VER :WILD)))
  ;; VER is :UNSPECIFIC unless TYP is :WILD, in which case VER is also :WILD.
  (VALUES :UNSPECIFIC DIR NAM TYP VER))

;; Differs from the default method in that if the type is :WILD
;; we clobber the version to :WILD; otherwise we clobber the version to :UNSPECIFIC.
(DEFMETHOD (UNIX-PATHNAME-MIXIN :NEW-PATHNAME)
           (&REST OPTIONS
            &KEY (STARTING-PATHNAME SELF)
            ((:TYPE -TYPE-) (PATHNAME-TYPE STARTING-PATHNAME))
            &ALLOW-OTHER-KEYS)
  (APPLY #'MAKE-PATHNAME-1
         :VERSION (IF (EQ -TYPE- :WILD) :WILD :UNSPECIFIC)
         :STARTING-PATHNAME STARTING-PATHNAME
         :PARSING-PATHNAME SELF
         OPTIONS))

(DEFMETHOD (UNIX-PATHNAME-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (LIST (SEND SELF :PARSE-COMPONENT-SPEC SPEC)))
        ((AND (CONSP SPEC)
              (LOOP FOR ELT IN SPEC
                    ALWAYS (OR (MEMQ ELT '(:UP :WILD :RELATIVE))
                               (STRINGP ELT)))
              (NOT (MEMQ :RELATIVE (CDR SPEC))))
         (LOOP FOR ELT IN SPEC
               COLLECT (IF (SYMBOLP ELT) ELT
                         (SEND SELF :PARSE-COMPONENT-SPEC ELT))))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD :root)) SPEC)
        (T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

(defmethod (unix-pathname-mixin :canonicalize-directories) (&optional with-respect-to)
  "Get rid of :UP tokens in the directory of a UNIX style pathname.
If the arument WITH-RESPECT-TO is given, and the pathname is relative,
then also make it absolute."
  (let ((directories (send self :raw-directory)))
    ;:UPs at the root directory do nothing
    (loop until (not (eq (car-safe directories) :UP))
          do (pop directories))
    (cond ((not (null with-respect-to))
           (cond ((null directories)
                  (setq directories (send with-respect-to :raw-directory)))
                 ((eq (car-safe directories) :RELATIVE)
                  (let ((other-directories (send with-respect-to :raw-directory)))
                    (setq directories
                          (append (if (consp other-directories)
                                      other-directories
                                    (list other-directories)) directories)))))))

    (if (not (consp directories))
        (setq directories (list directories))) ;maybe makes (:ROOT)

    ;; now we have something like ("usr" "pace" "src" :RELATIVE "path" :UP :UP "lisp-eval")
    ;; or (:ROOT)
    ;; or ("foo" "bar")
    (let ((new-directories nil))
      (do ((d-list (reverse directories) (cdr d-list))
           (up-counter 0))
          ((null d-list)
           (dotimes (i up-counter)
             (push :UP new-directories)))
        (cond ((eq (car d-list) :UP)
               (incf up-counter))
              ((eq (car d-list) :RELATIVE))
              ((eq (car d-list) :ROOT))
              ((not (zerop up-counter))
               (decf up-counter))
              (t
               (push (car d-list) new-directories))))
      (send self :new-raw-directory
            (cond ((null new-directories)
                   :ROOT)
                  ((= (length new-directories) 1)
                   (car new-directories))
                  ((stringp (car new-directories))
                   new-directories)
                  (t
                   (cons :RELATIVE new-directories)))))))

;;; Patch system interface, more kludges for only 14 character UNIX filenames
(DEFMETHOD (UNIX-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP
                                              &REST ARGS)
   (LET ((SNAME (IF SAME-DIRECTORY-P PATOM (SI:SYSTEM-SHORT-NAME NAM))))
     (IF (> (STRING-LENGTH SNAME) 6)
         (SETQ SNAME (SUBSTRING SNAME 0 6)))
     (SELECTQ TYP
       (:SYSTEM-DIRECTORY
        (SEND SELF :NEW-PATHNAME :NAME SNAME :TYPE :PATCH-DIRECTORY :VERSION :NEWEST))
       (:VERSION-DIRECTORY
        (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~A-~D" SNAME (\ (CAR ARGS) 10000.))
                                 :TYPE :PATCH-DIRECTORY :VERSION :NEWEST))
       (:PATCH-FILE
        (SEND SELF :NEW-PATHNAME
                   :NAME (FORMAT NIL "~A-~D-~D"
                                 (SUBSTRING SNAME 0 (MIN 3 (STRING-LENGTH SNAME)))
                                 (\ (CAR ARGS) 1000.)
                                 (\ (CADR ARGS) 1000.))
                   :TYPE (CADDR ARGS) :VERSION :NEWEST)))))

(defmethod (unix-pathname-mixin :pathname-as-directory) ()
  (send self :new-pathname
             :raw-directory (if (eq directory :root)
                                 name
                               (append (if (consp directory) directory
                                         (ncons directory))
                                       (ncons (unix-filename name type))))
             :name :unspecific
             :type :unspecific
             :version :unspecific))



(DEFFLAVOR MULTICS-PATHNAME-MIXIN () (UNIX-PATHNAME-MIXIN))

(DEFMETHOD (MULTICS-PATHNAME-MIXIN :DIRECTORY-DELIMITER-CHARACTER) () #/>)

(DEFMETHOD (MULTICS-PATHNAME-MIXIN :DIRECTORY-UP-DELIMITER) () #/<)

;(DEFMETHOD (MULTICS-PATHNAME-MIXIN :INIT-FILE) (PROGRAM-NAME)
;  (SEND SELF :NEW-PATHNAME :NAME PROGRAM-NAME
;                           :TYPE "INIT"))

(DEFFLAVOR LMFS-PATHNAME-MIXIN () (UNIX-PATHNAME-MIXIN))

(DEFMETHOD (LMFS-PATHNAME-MIXIN :DIRECTORY-DELIMITER-CHARACTER) () #/>)

(DEFMETHOD (LMFS-PATHNAME-MIXIN :DIRECTORY-UP-DELIMITER) () #/<)

(DEFMETHOD (LMFS-PATHNAME-MIXIN :CONVERT-TYPE-FOR-HOST) (-TYPE-)
  (STRING-OR-WILD -TYPE-))

(DEFMETHOD (LMFS-PATHNAME-MIXIN :UNSPECIFIC-TYPE-IS-DEFAULT) () NIL)

(DEFMETHOD (LMFS-PATHNAME-MIXIN :UNDELETABLE-P) () T)

(DEFMETHOD (LMFS-PATHNAME-MIXIN :NEW-PATHNAME)
           (&REST OPTIONS
            &KEY (STARTING-PATHNAME SELF)
            &ALLOW-OTHER-KEYS)
  (APPLY #'MAKE-PATHNAME-1
         :VERSION (GETF OPTIONS :VERSION VERSION)
         :STARTING-PATHNAME STARTING-PATHNAME
         :PARSING-PATHNAME SELF
         OPTIONS))

(DEFMETHOD (LMFS-PATHNAME-MIXIN :PARSE-NAMESTRING) (IGNORE NAMESTRING
                                                      &OPTIONAL (START 0) END
                                                      &AUX DIR NAM TYP DELIM-CHAR ver)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (SETQ START (OR (STRING-SEARCH-NOT-CHAR #\SP NAMESTRING START END) END))
  (SETQ DELIM-CHAR (FUNCALL-SELF ':DIRECTORY-DELIMITER-CHARACTER)
        DIR (STRING-REVERSE-SEARCH-CHAR DELIM-CHAR NAMESTRING END START)
        TYP (STRING-REVERSE-SEARCH-CHAR #/. NAMESTRING END (OR DIR START)))
  (COND (DIR
         (PSETQ START (1+ DIR)
                DIR (LET ((RELATIVE-P T)
                          (DIR-START START)
                          (UP (FUNCALL-SELF ':DIRECTORY-UP-DELIMITER))
                          (NUP NIL)
                          (STRS NIL))
                      (COND ((= (AREF NAMESTRING DIR-START) DELIM-CHAR)
                             (SETQ RELATIVE-P NIL)
                             (SETQ DIR-START (STRING-SEARCH-NOT-CHAR
                                               DELIM-CHAR NAMESTRING DIR-START))))
                      (AND DIR-START (> DIR DIR-START)
                           (SETQ STRS (LOOP FOR IDX = DIR-START THEN JDX
                                            AS JDX = (STRING-SEARCH-CHAR
                                                       DELIM-CHAR NAMESTRING IDX DIR)
                                            COLLECT (SUBSTRING NAMESTRING IDX (OR JDX DIR))
                                            WHILE
                                              (AND JDX
                                                   (SETQ JDX
                                                         (STRING-SEARCH-NOT-CHAR
                                                           DELIM-CHAR NAMESTRING JDX DIR))))))
                      (AND (STRINGP UP)
                           (DO L STRS (CDR L) (NULL L)
                             (AND (STRING-EQUAL (CAR L) UP)
                                  (RPLACA L ':UP))))
                      (AND NUP (SETQ STRS (NCONC NUP STRS)))
                      (COND (RELATIVE-P (CONS ':RELATIVE STRS))
                            ((NULL STRS) ':ROOT)
                            ((NULL (CDR STRS)) (CAR STRS))
                            (T STRS))))))
  (IF TYP (PSETQ END TYP
                 TYP (SUBSTRING NAMESTRING (1+ TYP) END)))
  (SETQ NAM (AND ( START END) (SUBSTRING NAMESTRING START END)))
  (when nam
    (let ((idx (string-search #/. nam)))
      (when idx
        (setq ver
              (and typ
                   (cond ((string-equal typ "newest") ':newest)
                         ((string-equal typ "oldest") ':oldest)
                         ((string-equal typ "*") ':wild)
                         ((string-equal typ "") ':unspecific)
                         ((= (string-length typ) 0) nil)
                         (t (or (parse-number typ 0 nil 10. t)
                                (ferror 'pathname-parse-error "Bad version spec (~A)" typ))))))
        (psetq nam (substring nam 0 idx)
               typ (substring nam (1+ idx))))))
  (COND ((EQUAL NAM "") (SETQ NAM NIL))
        ((EQUAL NAM "*") (SETQ NAM :WILD))
        ((EQUAL NAM "") (SETQ NAM :UNSPECIFIC)))
  (COND ((EQUAL TYP "") (SETQ TYP NIL))
        ((EQUAL TYP "*") (SETQ TYP :WILD))
        ((EQUAL TYP "") (SETQ TYP :UNSPECIFIC)))
  (VALUES :UNSPECIFIC DIR NAM TYP ver))

(defun slmfs-name-string (name type version &optional for-editor)
  (format nil "~A.~A~A"
          (slmfs-text-string name)
          (slmfs-text-string type)
          (cond ((numberp version) (format () ".~D" version))
                ((null version) "")
                (t (selectq version
                     (:newest (if for-editor "" ".newest"))
                     (:oldest ".oldest")
                     (:unspecific ".")
                     (:wild ".*")
                     (t ".!ERROR!"))))))

(defun slmfs-text-string (x)
  (cond ((stringp x) x)
        ((null x) "")
        (t (selectq x
             (:wild "*")
             (:unspecific "")
             (t "!ERROR!")))))

(defmethod (lmfs-pathname-mixin :string-for-host) ()
  (format nil "~@[~a~]~a" (unix-directory-string) (slmfs-name-string name type version)))

(defmethod (lmfs-pathname-mixin :string-for-editor) ()
  (format nil "~a ~a ~a:"
          (slmfs-name-string name type version t) (unix-directory-string)
          (send host ':name-as-file-computer)))

(defmethod (lmfs-pathname-mixin :string-for-dired) () (slmfs-name-string name type version))


;;;; Logical pathnames
;; Copied from LAD: RELEASE-3.IO.FILE; PATHST.LISP#209 on 2-Oct-86 05:36:43
(DEFMETHOD (LMFS-PATHNAME-MIXIN :PATHNAME-AS-DIRECTORY) ()
  ;; *horror*
  ;; this shadows the unix definition with the regular one.
  (SEND SELF :NEW-PATHNAME
             :RAW-DIRECTORY (IF (EQ DIRECTORY :ROOT)
                                 NAME
                               (APPEND (IF (CONSP DIRECTORY) DIRECTORY
                                         (NCONS DIRECTORY))
                                       (NCONS NAME)))
             :NAME :UNSPECIFIC
             :TYPE :UNSPECIFIC
             :VERSION :UNSPECIFIC))

;; Copied from LAD: RELEASE-3.IO.FILE; PATHST.LISP#209 on 2-Oct-86 05:36:44
(DEFMETHOD (LMFS-PATHNAME-MIXIN :PATHNAME-MATCH-SPECS) (PATHNAME)
  ;; *horror*
  (MULTIPLE-VALUE-BIND (W* W1)
      (SEND SELF :INTERNAL-WILD-CHARACTERS)
    (VALUES
      (PATHNAME-COMPONENT-MATCH DEVICE (PATHNAME-DEVICE PATHNAME) W* W1 T #'STRING-EQUAL)
      (PATHNAME-COMPONENT-MATCH DIRECTORY (PATHNAME-DIRECTORY PATHNAME) W* W1 T #'STRING-EQUAL)
      (PATHNAME-COMPONENT-MATCH NAME (PATHNAME-NAME PATHNAME) W* W1 T #'STRING-EQUAL)
      (PATHNAME-COMPONENT-MATCH TYPE (PATHNAME-TYPE PATHNAME) W* W1 T #'STRING-EQUAL))))

;; Copied from LAD: RELEASE-3.IO.FILE; PATHST.LISP#209 on 2-Oct-86 05:36:45
(DEFMETHOD (LMFS-PATHNAME-MIXIN :PATHNAME-MATCH) (PATHNAME &OPTIONAL (MATCH-HOST T))
  ;; *horror*
  (MULTIPLE-VALUE-BIND (W* W1)
      (SEND SELF :INTERNAL-WILD-CHARACTERS)
    (AND (OR (NOT MATCH-HOST)
             (EQ HOST (PATHNAME-HOST PATHNAME)))
         ;; punt the device.
;;;      (PATHNAME-COMPONENT-MATCH DEVICE (PATHNAME-DEVICE PATHNAME) W* W1 NIL #'STRING-EQUAL)
         (PATHNAME-COMPONENT-MATCH DIRECTORY (PATHNAME-DIRECTORY PATHNAME) W* W1 NIL #'STRING-EQUAL)
         (PATHNAME-COMPONENT-MATCH NAME (PATHNAME-NAME PATHNAME) W* W1  NIL #'STRING-EQUAL)
         (PATHNAME-COMPONENT-MATCH TYPE (PATHNAME-TYPE PATHNAME) W* W1  NIL #'STRING-EQUAL)
         (PATHNAME-COMPONENT-MATCH VERSION (PATHNAME-VERSION PATHNAME) W* W1 NIL #'STRING-EQUAL))))

;; Copied from LAD: RELEASE-3.IO.FILE; PATHST.LISP#209 on 2-Oct-86 05:36:45
(DEFMETHOD (LMFS-PATHNAME-MIXIN :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP &REST ARGS)
  ;; *horror*
  ;; The horror here is that logical pathnames cant be used for this sort of thing.
  ;; The default extention PATCH-DIRECTORY is too long for Symbolics file system. Amazing but true.
  ;; In any case the unix-pathname-mixin would screw us so we do our best by this definition.
  (LET ((PATOM (STRING-UPCASE (IF SAME-DIRECTORY-P PATOM NAM))))
    (SELECTQ TYP
      (:SYSTEM-DIRECTORY
       (SEND SELF :NEW-PATHNAME :NAME PATOM
                  :TYPE (IF SAME-DIRECTORY-P "PDIR" "PDIR")
                  :VERSION :NEWEST))
      (:VERSION-DIRECTORY
       (SEND SELF :NEW-PATHNAME :NAME (FORMAT NIL "~A-~D" PATOM (CAR ARGS))
                  :TYPE (IF SAME-DIRECTORY-P "PDIR" "PDIR")
                  :VERSION :NEWEST))
      (:PATCH-FILE
       (SEND SELF :NEW-PATHNAME
                  :NAME (FORMAT NIL "~A-~D-~D" PATOM (CAR ARGS) (CADR ARGS))
                  :TYPE (CADDR ARGS)
                  :VERSION :NEWEST)))))


(DEFUN TRANSLATED-PATHNAME (PATHNAME)
  "Return translated pathname made from PATHNAME.
If PATHNAME refers to a logical host, the result will refer to the
corresponding physical host."
  (SEND (PATHNAME PATHNAME) :TRANSLATED-PATHNAME))

(DEFUN BACK-TRANSLATED-PATHNAME (LOGICAL-PATHNAME ACTUAL-PATHNAME)
  "Try to untranslate ACTUAL-PATHNAME for the host of LOGICAL-PATHNAME.
If LOGICAL-PATHNAME indeed refers to a logical host, and ACTUAL-PATHNAME
is a pathname that could be produced by translation of some logical pathname
on that host, then said logical pathname is returned.
Otherwise, ACTUAL-PATHNAME is returned."
  (SEND (PATHNAME LOGICAL-PATHNAME) :BACK-TRANSLATED-PATHNAME (PATHNAME ACTUAL-PATHNAME)))

(DEFFLAVOR LOGICAL-PATHNAME () (PATHNAME))

(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-PRINTING) ()
  (LET* ((DEFAULT-CONS-AREA PATHNAME-AREA)
         (DIR (IF (ATOM DIRECTORY) (LIST (STRING-OR-WILD DIRECTORY))
                (MAPCAR 'STRING-OR-WILD DIRECTORY)))
         (DEV (LOGICAL-DEVICE-STRING))
         (NAM (LOGICAL-NAME-STRING))
         (TYP (LOGICAL-TYPE-STRING))
         (VER (LOGICAL-VERSION-STRING)))        ; can actually be a number
        (FORMAT NIL "~A: ~@[~A: ~]~:[~{~A; ~}~;~*~]~@[~A~]~@[ ~A~]~@[ ~A~]"
            (SEND HOST :NAME-AS-FILE-COMPUTER)
            DEV
            (SI:MEMBER-EQUAL DIRECTORY '(NIL (:UNSPECIFIC)))
            DIR NAM TYP (IF (NUMBERP VER) (FORMAT NIL "~D" VER) VER))))

(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-DIRECTORY) ()
  (LET ((DIR (IF (ATOM DIRECTORY) (LIST (STRING-OR-WILD DIRECTORY))
               (MAPCAR 'STRING-OR-WILD DIRECTORY)))
        (DEV (LOGICAL-DEVICE-STRING)))
    (FORMAT NIL "~@[~A: ~]~:[~{~A;~^ ~}~;~]"
            DEV
            (SI:MEMBER-EQUAL DIRECTORY '(NIL (:UNSPECIFIC)))
            DIR)))

(DEFF LOGICAL-NAME-STRING 'ITS-FN1-STRING)

(DEFUN LOGICAL-DEVICE-STRING ()
  (DECLARE (:SELF-FLAVOR LOGICAL-PATHNAME))
  (IF (MEMQ DEVICE '(NIL :UNSPECIFIC))
      NIL
    (STRING-OR-WILD DEVICE)))

(DEFUN LOGICAL-TYPE-STRING (&OPTIONAL NO-PLACEHOLDER)
  (DECLARE (:SELF-FLAVOR LOGICAL-PATHNAME))
  (COND ((EQ TYPE :UNSPECIFIC) "")
        ((NULL TYPE)
         (AND (NOT NO-PLACEHOLDER)
              VERSION
              ""))
        (T
         (STRING-OR-WILD TYPE))))

;;; Contrary to its name, this can also return NIL or numbers as well as strings
(DEFUN LOGICAL-VERSION-STRING ()
  (DECLARE (:SELF-FLAVOR LOGICAL-PATHNAME))
  (SELECTQ VERSION
    (:UNSPECIFIC "")
    (NIL NIL)
    (:NEWEST ">")
    (:OLDEST "<")
    (:WILD "*")
    (OTHERWISE VERSION)))

;;;As of system 124, logical pathnames support LISPM directory delimiter
;;;style in addition to the standard style.  I.e., "sys:hardcopy.tiger;"
;;;can be used instead of "sys:hardcopy;tiger;" - whichever is preferred.
;;;This might make some programs work more easily. -Jim/KmC

(DEFMETHOD (LOGICAL-PATHNAME :PARSE-NAMESTRING) (IGNORE NAMESTRING &OPTIONAL (START 0) END
                                                 &aux semicolon-p found-semicolon)
  (OR END (SETQ END (STRING-LENGTH NAMESTRING)))
  (setq semicolon-p (string-search #/; namestring start end))
  (DO ((I START)
       (J START (1+ J))
       CH TEM Q
       DIR NAM NAMP TYP TYPP VERS)
      ((> J END)
       (SETQ DIR (NREVERSE DIR))
       (VALUES :UNSPECIFIC DIR NAM TYP VERS))
    (SETQ CH (IF (= J END) #/SP (AREF NAMESTRING J)))
    (COND ((= CH '#/)
           (SETQ J (1+ J)))
          ((MEMQ CH '(#/; #/: #/ #/ #/SP #/TAB #/. #/#))
           (COND ((OR ( I J) (= CH #/) (= CH #/))
                  (AND (MEM #'= CH '(#/ #/))
                       (OR ( I J)
                           (AND ( (1+ J) END)
                                ( (AREF NAMESTRING (1+ J)) #/SP)))
                       (PATHNAME-ERROR (1+ J) NAMESTRING
                                       "An unquoted ~C must be a component unto itself." CH))
                  (MULTIPLE-VALUE (TEM Q)
                    (SELECTQ CH
                      (#/ (VALUES :UNSPECIFIC NIL))
                      (#/ (VALUES NIL NIL))
                      (T (UNQUOTE-LOGICAL-STRING NAMESTRING I J))))
                  (IF (AND (NOT Q) (STRING= TEM "*"))
                      (SETQ TEM :WILD))
                  (cond ((eq ch #/:) nil)
                        ((or (eq #/; ch)
                             (and (eq #/. ch)
                                  semicolon-p
                                  (not found-semicolon)))
                         (if (eq ch #/;)
                             (setq found-semicolon t))
                         (push tem dir))
                        ('OTHERWISE
                         (COND (VERS)
                               (TYPP (SETQ VERS (COND ((MEMQ TEM '(:UNSPECIFIC :WILD)) TEM)
                                                      ((AND (NOT Q)
                                                            (COND ((STRING= TEM ">") :NEWEST)
                                                                  ((STRING= TEM "<") :OLDEST)
                                                                  ((NUMERIC-P TEM)))))
                                                      (T (PATHNAME-ERROR J NAMESTRING
                                                                         "Version not numeric")))))
                               (NAMP (SETQ TYP TEM TYPP T))
                               (T (SETQ NAM TEM NAMP T)))))))
           (SETQ I (1+ J))))))

(DEFMETHOD (LOGICAL-PATHNAME :QUOTE-CHARACTER) () #/)

(DEFMETHOD (LOGICAL-PATHNAME :CHARACTER-NEEDS-QUOTING-P) (CH)
  (OR ( #/a CH #/z)
      (MEM #'= CH '(#/: #/; #/ #/ #/. #/SP #/TAB #/#))))

(DEFUN UNQUOTE-LOGICAL-STRING (STRING &OPTIONAL (START 0) (END (STRING-LENGTH STRING)))
  (DO ((I START (1+ I))
       (NCH 0) (CH)
       (NEED-COPY NIL))
      (( I END)
       (COND ((AND (= START 0) (= I (STRING-LENGTH STRING)) (NOT NEED-COPY))
              STRING)                           ;To avoid consing
             ((NOT NEED-COPY)
              (SUBSTRING STRING START I))
             (T
              (DO ((NSTRING (MAKE-STRING NCH))
                   (J 0)
                   (K START (1+ K))
                   CHAR-QUOTED
                   (CH))
                  (( K I) (VALUES NSTRING T))
                (SETQ CH (AREF STRING K))
                (COND (( CH #/)
                       (SETF (AREF NSTRING J) (IF CHAR-QUOTED CH (CHAR-UPCASE CH)))
                       (SETQ CHAR-QUOTED NIL)
                       (INCF J))
                      (T (SETQ CHAR-QUOTED T)))))))
    (SETQ CH (AREF STRING I))
    (IF (= CH #/)
        (SETQ NEED-COPY T)
      (IF ( #/a CH #/z) (SETQ NEED-COPY T))
      (INCF NCH))))

(DEFMETHOD (LOGICAL-PATHNAME :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((CONSP SPEC)
         (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))


(DEFMETHOD (LOGICAL-PATHNAME :COMPLETE-STRING) (STRING OPTIONS &AUX STRING1 FOO SUCCESS)
  (LET ((TRANSLATED (let ((temp (PARSE-PATHNAME STRING HOST)))
                      (send (if (send temp :directory)
                                temp
                              (send temp :new-directory directory))
                            :translated-pathname))))
    (SETQ STRING1 (SEND TRANSLATED :STRING-FOR-HOST))
;    ;; This used to be just :STRING-FOR-PRINTING,
;    ;; but we want to get rid of the 's that NIL components would make.
;    (SETQ STRING1
;         (IF (NOT (MEMQ (SEND TRANSLATED :VERSION) '(NIL :UNSPECIFIC)))
;             (SEND TRANSLATED :STRING-FOR-PRINTING)
;           (IF (SEND TRANSLATED :TYPE)
;               (SEND (SEND TRANSLATED :NEW-VERSION :NEWEST) :STRING-FOR-PRINTING)
;             (STRING-APPEND (SEND (SEND TRANSLATED :NEW-PATHNAME
;                                        :NAME NIL :TYPE NIL)
;                                  :STRING-FOR-PRINTING)
;                            (SEND TRANSLATED :NAME)))))
;    (SETQ STRING1 (SUBSTRING STRING1 (1+ (STRING-SEARCH-CHAR #/: STRING1))))
    )
  ;; What STRING1 is will match the :STRING-FOR-HOST for many kinds of pathname,
  ;; but not for all.
  (LET (BASE-PATHNAME)
    (CONDITION-CASE ()
        (SETQ BASE-PATHNAME (SEND SELF :TRANSLATED-PATHNAME))
      (UNKNOWN-LOGICAL-PATHNAME-TRANSLATION
       (SETQ BASE-PATHNAME
             (SEND (SEND (SEND SELF :NEW-DIRECTORY NIL) :TRANSLATED-PATHNAME)
                   :NEW-DIRECTORY (SEND SELF :DIRECTORY)))))
    (VALUES
      (SEND
        (SEND SELF :BACK-TRANSLATED-PATHNAME (PARSE-PATHNAME (MULTIPLE-VALUE (FOO SUCCESS)
                                                               (SEND BASE-PATHNAME
                                                                     :COMPLETE-STRING
                                                                     STRING1
                                                                     OPTIONS))))
        :STRING-FOR-PRINTING)
      SUCCESS)))

(DEFUN LOGICAL-PATHNAME-PASS-ON (&REST REST)
  (LEXPR-SEND (SEND SELF :TRANSLATED-PATHNAME) REST))

(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-HOST) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-WHOLINE) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-EDITOR) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :CHANGE-PROPERTIES) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :EXPUNGE) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :OPEN) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :OPEN-CANONICAL-TYPE) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :DELETE) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :UNDELETE) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :RENAME) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :UNDELETABLE-P) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :HOMEDIR) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :MULTIPLE-FILE-PLISTS) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :PARSE-TRUENAME) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :DIRECTORY-LIST) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :DIRECTORY-LIST-STREAM) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :ALL-DIRECTORIES) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :STRING-FOR-DIRED) LOGICAL-PATHNAME-PASS-ON)

(DEFMETHOD (LOGICAL-PATHNAME :MAIL-FILE-FORMAT-COMPUTER) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :INBOX-BUFFER-FLAVOR) LOGICAL-PATHNAME-PASS-ON)

;;; These need to be passed on since otherwise the information on how they were created
;;; will be lost.
(DEFMETHOD (LOGICAL-PATHNAME :PATCH-FILE-PATHNAME) LOGICAL-PATHNAME-PASS-ON)
(DEFMETHOD (LOGICAL-PATHNAME :NEW-TYPE-AND-VERSION) LOGICAL-PATHNAME-PASS-ON)

(DEFFLAVOR LOGICAL-HOST
        (NAME                                   ;Logical device name
         PHYSICAL-HOST                          ;Host that turns into
         TRANSLATIONS)                          ;The actual translations
        ;; NON-DEFAULT-DEVICE-LIST  would record devices other than default-device for
        ;; which GENERIC-PATHNAMES have been created for this host.  See discussion
        ;; in PATHNM. Not implemented.  For now, if it would need this it just bombs
        ;; in (method pathname :generic-pathname).
        (pathname-host-mixin SI:BASIC-HOST)
  (:SETTABLE-INSTANCE-VARIABLES))

(DEFUN LOGICAL-HOST-PASS-ON (&REST REST)
  (DECLARE (:SELF-FLAVOR LOGICAL-HOST))
  (LEXPR-SEND PHYSICAL-HOST REST))

(DEFMETHOD (LOGICAL-HOST :PATHNAME-FLAVOR) () 'LOGICAL-PATHNAME)

(DEFMETHOD (LOGICAL-HOST :SYSTEM-TYPE) ()
  :LOGICAL)

;;; old name [need MAKE-OBSOLETE for messages]
(defmethod (logical-host :host) () physical-host)

;;; These characteristics of a logical host are the same as those of the physical host
(DEFMETHOD (LOGICAL-HOST :NETWORK-TYPE) LOGICAL-HOST-PASS-ON)
(DEFMETHOD (LOGICAL-HOST :NETWORK-TYPEP) LOGICAL-HOST-PASS-ON)
(DEFMETHOD (LOGICAL-HOST :ENABLE-CAPABILITIES) LOGICAL-HOST-PASS-ON)
(DEFMETHOD (LOGICAL-HOST :DISABLE-CAPABILITIES) LOGICAL-HOST-PASS-ON)

(DEFMETHOD (LOGICAL-HOST :GMSGS-PATHNAME) LOGICAL-HOST-PASS-ON)

;;;EZ SleZ Nicknames for logical hosts:

(defmethod (logical-host :nicknames) ()
  (send self :get 'nicknames))

(defmethod (logical-host :host-names) ()
  (append (send self :nicknames)
          (ncons (send self :name))))

(defmethod (logical-host :set-nicknames) (nicknames)
  (send self :putprop (mapcar #'string nicknames) 'nicknames))

(defmethod (logical-host :pathname-host-namep) (testname)
  (car (mem #'string-equal testname (send self :host-names))))

;;;; Actual installation and use of translations.
(defstruct (logical-pathname-translation :list (:conc-name translation-) (:alterant nil))
  logical-pattern
  physical-pattern)

(defmethod (logical-pathname :translated-pathname) ()
  (block done
    (do-forever
      (dolist (trans (send host :translations))
        (when (send (translation-logical-pattern trans) :pathname-match self t)
          (return-from done
            (send (translation-logical-pattern trans) :translate-wild-pathname
                  (translation-physical-pattern trans) self t))))
      (unless (or directory name type version)
        (return (send self :new-pathname :host (send host :physical-host) :device nil)))
      (signal-proceed-case ((newdir)
                            'unknown-logical-pathname-translation
                            "No translation for ~A." self)
        (:define-directory
         (add-logical-host-translation
           host (send self :new-pathname :name :wild :type :wild :version :wild)
           newdir))))))

(defmethod (logical-pathname :back-translated-pathname) (pathname)
  (if (eq (type-of pathname) 'logical-pathname)
      pathname
    (dolist (trans (send host :translations))
      (when (send (translation-physical-pattern trans) :pathname-match pathname t)
        (return
          (send (translation-physical-pattern trans) :translate-wild-pathname
                (translation-logical-pattern trans) pathname t))))))

(deff change-logical-pathname-directory 'add-logical-host-translation)

(defun add-logical-host-translation (logical-host from-pattern to-pattern)
  "Add or modify translation of FROM-PATTERN in existing logical host LOGICAL-HOST.
It will translate to TO-PATTERN.
LOGICAL-HOST may be a host or host name.
FROM-PATTERN and TO-PATTERN may be pathnames or namestrings."
  (if (stringp logical-host)
      (setq logical-host (get-pathname-host logical-host)))
  (send logical-host :set-translations
        (delete-duplicates
          (append (ncons (decode-translation logical-host from-pattern to-pattern))
                  (send logical-host :translations))
          :test #'equalp
          :key #'(lambda (x) (send (car x) :string-for-printing))
          ;; New translations added at beginning now that we allow wild cards.
          :from-end t)))

(defun decode-translation (logical-host from-pattern to-pattern)
  (let ((parsed-from-pattern (parse-pathname from-pattern logical-host)))
    ;; Detect missing semicolon after directory name.
    (if (and (null (send parsed-from-pattern :directory))
             (null (send parsed-from-pattern :type))
             (null (send parsed-from-pattern :version))
             (send parsed-from-pattern :name))
        (setq parsed-from-pattern
              (send parsed-from-pattern :new-pathname
                    :directory (send parsed-from-pattern :name)
                    :name nil)))
    (make-logical-pathname-translation
      :logical-pattern (wildify parsed-from-pattern)
      :physical-pattern (wildify (parse-pathname to-pattern
                                                 (send logical-host :physical-host))))))

;;; Default the dir, name, type and version to wild.
;;; Default the device to the host's primary device.
(defun wildify (pathname)
  (make-pathname :host (pathname-host pathname)
                 :device (or (pathname-device pathname)
                             (send (pathname-host pathname) :primary-device))
                 :directory (or (pathname-directory pathname) :wild)
                 :name (or (pathname-name pathname) :wild)
                 :type (or (pathname-type pathname) :wild)
                 :version (or (pathname-version pathname) :wild)))

(defun set-logical-pathname-host (logical-host &key physical-host translations nicknames)
  "Define a logical host named LOGICAL-HOST, which translates to PHYSICAL-HOST.
NICKNAMES is a list of strings, nicknames for LOGICAL-HOST.
TRANSLATIONS is a list of translations to use: each element looks like
 (logical-pattern physical-pattern),
 where each pattern is a file namestring with wildcards.
 Omitted components default to * (:WILD)."
  (declare (zwei:indentation 1 1))
  (let (log phys)
    (tagbody
        retry
           (setq log (get-pathname-host logical-host t nil))
           (unless (typep log '(or null logical-host))
             (multiple-cerror () () ("~Creating the logical host with name /"~A/"
will override that name for the physical host ~S,
making /"~:2*~A/" unacceptable as a name for the physical host~" logical-host log)
               ("Create the logical host anyway"
                (setq log nil))
               ("Don't create the logical host"
                (return-from set-logical-pathname-host nil))
               ("Supply a new name for the logical host"
                (let ((*query-io* *debug-io*))
                  (setq logical-host
                        (string-upcase (prompt-and-read :string-trim
                                                        "New name for logical host: "))))
                (go retry))))
           ;;Changed (get-pathname-host ... NIL) - Don't want "unknown" physical hosts
           ;;getting added for logical hosts!!! Causes bad brains. -KmC
           (unless (setq phys (or (get-pathname-host physical-host nil)
                               (si:parse-host physical-host)))
                ;;Also continue only if we have physical host -- don't add to list
                ;;if it isn't real. -KmC
                (error "Cannot define logical host without corresponding physical host"))
           (unless log (setq log (make-instance 'logical-host :name logical-host)))
           ;;The new logical host must be accessible via GET-PATHNAME-HOST now
           (pushnew log *logical-pathname-host-list* :test 'eq))
    ;; Here is a bit of a kludge for SI:SET-SITE.  If the physical host is not defined yet,
    ;; add it now.
    ;;(unless (typep phys 'logical-host)
    ;;  (pushnew phys *pathname-host-list* :test 'eq))
    ;;**** NO! Also wrong, and extremely dangerous.   All I know, this is part of
    ;;what caused a si:set-sys-host to a previously unknown host to lay a bomb:
    ;;you can end up with non-EQ phys's (one on si:host-alist and another
    ;;one on *pathname-host-list*).  This occurred if you don't happen to type in
    ;;the exact (case-matching) real name of the sys host.  After doing
    ;;(update-site-configuration-info), the world went mad -- can't get at the
    ;;physical host anymore.  A fix was also needed in set-sys-host.
    ;;-KmC 1/6/88
    ;;
    ;;;Add physical host
    (send log :set-physical-host phys)
    ;;;Add nicknames to existing host
    (dolist (name nicknames)
      (let ((test (get-pathname-host name t t)))
        (when (and test (neq test log))
          (warn "Not using existing host name ~A as nickname for ~A" name logical-host)
          (setq nicknames (lisp:delete name nicknames :test #'string-equal)))))
    (when nicknames
      (send log :set-nicknames
            (mapcar #'string
                    (lisp:union (send log :nicknames) nicknames :test #'string-equal))))
    ;;;Add translations
    (if translations
        (send log :set-translations
              (loop for trans in translations
                    collect (decode-translation log (car trans) (cadr trans)))))
    log))

(defun add-logical-pathname-host (logical-host physical-host translations)
  "Define a logical host named LOGICAL-HOST, which translates to PHYSICAL-HOST.
TRANSLATIONS is a list of translations to use: each element looks like
 (logical-pattern physical-pattern),
 where each pattern is a file namestring with wildcards.
 Omitted components default to * (:WILD)."
  (set-logical-pathname-host logical-host
                             :physical-host physical-host
                             :translations translations))

(defun make-logical-pathname-host (host-name &key (warn-about-redefinition t) host-name-for-translations)
  "Defines HOST-NAME to be the name of a logical host.
If this conflicts the name or nickname of any physical host,
then and error is signalled, and the new logical host may be allowed to
override that name of the physical host.
This function loads the file SYS: SITE; host-name TRANSLATIONS, (or host-name-for-translations,
if specified), which should contain
a call to FS:SET-LOGICAL-PATHNAME-HOST to set up the translations for the host."
  (setq host-name (string-upcase (string host-name)))
  (if host-name-for-translations (setq host-name-for-translations (string-upcase (string host-name-for-translations))))
  (let ((old (get-pathname-host host-name t))
        new file-id loaded-id)
    (catch-error-restart ((fs:remote-network-error fs:file-not-found)
                          "Give up loading logical pathname translations for ~A" host-name)
      (when (typep old 'logical-host)
        (setq loaded-id (send old :get 'make-logical-pathname-host))
        ;; if previously defined by hand, don't load translations and clobber it
        (cond ((not loaded-id)
               (return-from make-logical-pathname-host old))
              (warn-about-redefinition
               (format *error-output* "~&Warning: The logical host ~A is being redefined"
                       old))))
      ;; no need to give error if redefining physical host, as set-logical-pathname-host errs
      ;;can't make the sys pathname below if we are here to create the SYS host itself !!
      ;;also, this means, for now, that any information in the SITE;SYS.TRANSLATIONS file must
      ;;also be in SITE;SITE
      (cond ((get-pathname-host "SYS" t)
             (let ((pathname (make-pathname :host "SYS"
                                            :device :unspecific
                                            :directory '("SITE")
                                            :name (if host-name-for-translations host-name-for-translations host-name)
                                            :canonical-type :logical-pathname-translations
                                            :version :newest)))
               (setq file-id (with-open-file (stream pathname :direction :probe
                                                     :if-does-not-exist :error)
                               (send stream :info)))
               (unless (equal loaded-id file-id)
                 (load pathname :verbose nil :package (symbol-package 'foo)))))))
    (cond ((typep (setq new (get-pathname-host host-name nil)) 'logical-host)
           (send new :set :get 'make-logical-pathname-host file-id)
           (if host-name-for-translations
               (send new :set :get :host-name-for-translations host-name-for-translations))
           new)
          (t (format *error-output*
                     "~&Warning: The logical host ~S was not defined by ~S."
                     host-name 'make-logical-pathname-host)
             nil))))

;;;; Errors from untranslatable pathnames.

(DEFFLAVOR UNKNOWN-LOGICAL-PATHNAME-TRANSLATION () (PATHNAME-ERROR))

(DEFSIGNAL UNKNOWN-LOGICAL-PATHNAME-TRANSLATION UNKNOWN-LOGICAL-PATHNAME-TRANSLATION
           (LOGICAL-PATHNAME)
  "Used when a logical pathname's directory is not recognized for that host.")

(DEFMETHOD (UNKNOWN-LOGICAL-PATHNAME-TRANSLATION :CASE :PROCEED-ASKING-USER
                                                 :DEFINE-DIRECTORY)
           (PROCEED-FUN READ-OBJECT-FUN)
  "Proceed, reading a physical pathname translate this directory to, permanently."
  (FUNCALL PROCEED-FUN :DEFINE-DIRECTORY
           (FUNCALL READ-OBJECT-FUN :STRING
                    "Filename to translate ~A to, permanently: "
                    (SEND (SEND SELF :LOGICAL-PATHNAME)
                          :NEW-PATHNAME :NAME :WILD :TYPE :WILD :VERSION :WILD))))

(COMPILE-FLAVOR-METHODS UNKNOWN-LOGICAL-PATHNAME-TRANSLATION)

;;;; Kludges for bootstrapping from a world without flavors loaded.
(DEFUN CANONICALIZE-COLD-LOAD-PATHNAMES (&AUX SYS-PATHNAME PHYS-PATHNAME)
  (DECLARE (SPECIAL SYS-PATHNAME PHYS-PATHNAME))
  ;; Get someone who can do the translations
  (SETQ SYS-PATHNAME (SAMPLE-PATHNAME "SYS")
        PHYS-PATHNAME (SEND SYS-PATHNAME :TRANSLATED-PATHNAME))
  ;; Make pathnames for all files initially loaded, and setup their properties
  (DOLIST (ELEM SI::*COLD-LOADED-FILE-PROPERTY-LISTS*)
    (LET* ((RECORDED-PATHNAME (MERGE-PATHNAME-DEFAULTS (CAR ELEM) PHYS-PATHNAME))
           (PATHNAME (OR (SEND SYS-PATHNAME :BACK-TRANSLATED-PATHNAME RECORDED-PATHNAME)
                         RECORDED-PATHNAME))
           (GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME)))
      (DO ((L (CDR ELEM) (CDDR L)))
          ((NULL L))
        (LET ((PROP (INTERN (SYMBOL-NAME (CAR L)) SI:PKG-KEYWORD-PACKAGE))
              (VAL (CADR L)))
          ;; Cold load generator does not know how to put in instances, it makes
          ;; strings instead.  Also, during MINI loading, calls to MAKE-PATHNAME-INTERNAL
          ;; are saved just as lists.  Note: we do not back translate this pathname, so
          ;; that we really remember the machine it was compiled on.
          (CASE PROP
            (:QFASL-SOURCE-FILE-UNIQUE-ID
             ;; val should be a pathname
             (ETYPECASE VAL
               (PATHNAME)
               (NULL)
               (STRING
                (SETQ VAL (MERGE-PATHNAMES VAL PHYS-PATHNAME)))
               (CONS
                ;; Don't bomb out if host isn't defined.
                (SETF (CAR VAL)
                      (OR (GET-PATHNAME-HOST (CAR VAL) T)
                          (SEND SYS-PATHNAME :HOST)))
                ;; Symbols like UNSPECIFIC may be in the wrong package
                (DO ((L (CDR VAL) (CDR L)))
                    ((NULL L))
                  (AND (SYMBOLP (CAR L))
                       (SETF (CAR L) (INTERN (SYMBOL-NAME (CAR L)) SI:PKG-KEYWORD-PACKAGE))))
                (SETQ VAL (APPLY #'MAKE-PATHNAME-INTERNAL VAL)))))
            (:FILE-ID-PACKAGE-ALIST
             ;; val should be ((,package (,truename ,creation-time)))
             ;; Kludge, built before there are packages
             (SETF (CAAR VAL) (PKG-FIND-PACKAGE
                                (OR (CAAR VAL)
                                    SI::PKG-SYSTEM-INTERNALS-PACKAGE)))
             ;; And before there are truenames
             (LET ((INFO (CADAR VAL)))
               (ETYPECASE (CAR INFO)
                 (PATHNAME)
                 (NULL)
                 (STRING (SETF (CAR INFO) (MERGE-PATHNAMES (CAR INFO) PHYS-PATHNAME))))))
            (:DEFINITIONS
             (COND ((OR (NULL VAL)
                        (SYMBOLP (CAR VAL)))
                    ;; The cold load maker doesn't put anything in saying what package,
                    ;; so just cons on SI.
                    (SETQ VAL (LIST (CONS SI::PKG-SYSTEM-INTERNALS-PACKAGE (CADR L)))))
                   (T
                    ;; Kludge, built before there are packages
                    (SETF (CAAR VAL)
                          (PKG-FIND-PACKAGE (OR (CAAR VAL)
                                                SI::PKG-SYSTEM-INTERNALS-PACKAGE)))))))
          (SEND GENERIC-PATHNAME :PUTPROP VAL PROP)))))
  (setq si::*cold-loaded-file-property-lists* nil)
  ;; Replace all strings saved on symbols with pathnames
  (LET (PATHNAME-MAP-ALIST)
    (DECLARE (SPECIAL PATHNAME-MAP-ALIST))
    (SI::MAPATOMS-NR-SYM
      (LAMBDA (SYMBOL &AUX NAME)
        (AND (SETQ NAME (GET SYMBOL :SOURCE-FILE-NAME))
             (NOT (TYPEP NAME 'PATHNAME))
             (SETF (GET SYMBOL ':SOURCE-FILE-NAME)
                   (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY NAME)))
        (AND (SETQ NAME (GET SYMBOL 'SPECIAL))
             (STRINGP NAME)
             (SETF (GET SYMBOL 'SPECIAL)
                   (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1 NAME)))
        (AND (GET SYMBOL 'SI::INITIALIZATION-LIST)
             (DOLIST (INIT (SYMBOL-VALUE SYMBOL))
               (AND (SI::INIT-SOURCE-FILE INIT)
                    (SETF (SI::INIT-SOURCE-FILE INIT)
                          (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1
                            (SI::INIT-SOURCE-FILE INIT))))))))
    ;; Store source file names from the cold load
    (SETQ SI::FUNCTION-SPEC-HASH-TABLE (MAKE-EQUAL-HASH-TABLE :SIZE 6000.))
    (DOLIST (ELEM SI::COLD-LOAD-FUNCTION-PROPERTY-LISTS)
      (unless (eq (second elem) :previous-definition)
        (SI:FUNCTION-SPEC-PUTPROP (FIRST ELEM)
                                  (IF (EQ (SECOND ELEM) :SOURCE-FILE-NAME)
                                      (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY (THIRD ELEM))
                                    (THIRD ELEM))
                                  (SECOND ELEM))))
    (setq si::cold-load-function-property-lists nil)
    (DOLIST (FLAVOR SI:*ALL-FLAVOR-NAMES*)
      (LET ((FL (GET FLAVOR 'SI:FLAVOR)))
        (LET ((LOC (SI::FLAVOR-GET-LOCATION FL ':COMPILE-FLAVOR-METHODS)))
          (AND LOC
               (STRINGP (CONTENTS LOC))
               (SETF (CONTENTS LOC)
                     (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1 (CONTENTS LOC)))))))
    (canonicalize-cold-loaded-times)))

(DEFUN PATHNAME-FROM-COLD-LOAD-PATHLIST (PATHLIST)
  ;; Don't bomb out if host isn't defined.
  (SETF (CAR PATHLIST)
        (OR (GET-PATHNAME-HOST (CAR PATHLIST) T)
            (SEND (SAMPLE-PATHNAME "SYS") :PHYSICAL-HOST)))
  ;; Symbols like UNSPECIFIC may be in the wrong package
  (DO ((L (CDR PATHLIST) (CDR L))) ((NULL L))
    (AND (SYMBOLP (CAR L))
         (SETF (CAR L) (INTERN (GET-PNAME (CAR L)) ""))))
  (APPLY #'MAKE-PATHNAME-INTERNAL PATHLIST))


(DEFUN CANONICALIZE-SOURCE-FILE-NAME-PROPERTY (PROPERTY)
  (IF (ATOM PROPERTY)
      (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1 PROPERTY)
    (DOLIST (TYPE PROPERTY)
      (DO ((L (CDR TYPE) (CDR L))) ((NULL L))
        (SETF (CAR L) (CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1 (CAR L)))))
    PROPERTY))

(DEFUN CANONICALIZE-SOURCE-FILE-NAME-PROPERTY-1 (NAME)
  (DECLARE (SPECIAL SYS-PATHNAME PHYS-PATHNAME PATHNAME-MAP-ALIST))
  (LET ((TEM (SI:ASSOC-EQUAL NAME PATHNAME-MAP-ALIST)))
    (IF TEM
        (CDR TEM)
      (LET ((RECORDED-PATHNAME (MERGE-PATHNAME-DEFAULTS NAME PHYS-PATHNAME)))
        (SETQ TEM
              (SEND (OR (SEND SYS-PATHNAME :BACK-TRANSLATED-PATHNAME
                              RECORDED-PATHNAME)
                        RECORDED-PATHNAME)
                    :GENERIC-PATHNAME))
        (PUSH (CONS NAME TEM) PATHNAME-MAP-ALIST)
        TEM))))

(DEFUN CANONICALIZE-COLD-LOADED-TIMES ()
  (MAPHASH (LAMBDA (IGNORE VAL &AUX ALIST)
             (WHEN (SETQ ALIST (GETF (PATHNAME-PROPERTY-LIST VAL) ':FILE-ID-PACKAGE-ALIST))
               (DOLIST (ID ALIST)
                 (LET ((INFO (CADR ID)))
                   (WHEN (STRINGP (CDR INFO))
                     (LET ((ANS (PARSE-DIRECTORY-DATE-PROPERTY (CDR INFO) 0)))
                       (IF ANS
                           (SETF (CDR INFO) ANS)
                         (FORMAT T "~&Foo! Somebody violated time protocol for ~A: ~S
  Nuke him till he glows and then shoot him with a gun!~%"
                                 VAL (CDR INFO)))))))))
           *PATHNAME-HASH-TABLE*))

;;;; Initializations

(COMPILE-FLAVOR-METHODS LOGICAL-PATHNAME LOGICAL-HOST)

;;; Here are the pathnames that we support

(DEFFLAVOR ITS-PATHNAME () (FS:ITS-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR TOPS20-PATHNAME () (FS:TOPS20-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR TENEX-PATHNAME () (FS:TENEX-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR VMS-PATHNAME () (FS:VMS-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR UNIX-PATHNAME () (FS:UNIX-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR MULTICS-PATHNAME () (FS:MULTICS-PATHNAME-MIXIN HOST-PATHNAME))

(DEFFLAVOR LMFS-PATHNAME () (FS:LMFS-PATHNAME-MIXIN HOST-PATHNAME))
(DEFPROP :LMFS LMFS-PATHNAME LISPM-PATHNAME-FLAVOR)
(DEFPROP LMFS-PATHNAME T PATHNAME-FLAVOR-CHANGES)

(COMPILE-FLAVOR-METHODS ITS-PATHNAME TOPS20-PATHNAME TENEX-PATHNAME
                        VMS-PATHNAME UNIX-PATHNAME MULTICS-PATHNAME
                        LMFS-PATHNAME)


;;; Partially special-case hack for converting all the :file-id-package-alist pathnames
;;; when the system directories are moved.

;(defvar fix-loaded-pathnames-old-translations)

;(defvar fix-loaded-pathnames-host)

;(defun fix-loaded-pathnames (fix-loaded-pathnames-host old-translation-alist)
;  (setq fix-loaded-pathnames-host (get-pathname-host fix-loaded-pathnames-host))
;  (setq fix-loaded-pathnames-old-translations
;       (mapcar #'(lambda (transl) (cons (car transl)
;                                        (substring (cadr transl) 1
;                                                   (1- (string-length (cadr transl))))))
;               old-translation-alist))
;  (maphash 'fix-loaded-pathnames-1 *pathname-hash-table*))

;(defun fix-loaded-pathnames-1 (ignore pathname)
;  (let ((prop (send pathname :get :file-id-package-alist)))
;    (dolist (elt prop)
;      (if (eq (send (caadr elt) :host) fix-loaded-pathnames-host)
;         (let ((new-dir (si:rassoc-equal (send (caadr elt) :directory)
;                                fix-loaded-pathnames-old-translations)))
;           (if new-dir
;               (setf (caadr elt)
;                     (send (caadr elt) :new-pathname
;                           :device "OZ"
;                           :directory
;                           (list "L" (car new-dir))))))))))
