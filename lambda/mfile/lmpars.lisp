;; -*- Mode: Lisp; Package: Moby-File-System; Base: 8 -*-

;;; Pathname parsing support for LM systems.

(DEFUN MLM-PARSE-NAMESTRING (STRING &OPTIONAL (START 0) END
                            &AUX CHAR STATE TEM TEM1 FIELD-START
                            (DEVICE "DSK") DIRECTORY NAME TYPE VERSION)
  ;; STATE can be T, DOTTED, VERSION, DIRECTORY or NIL.
  ;; NIL is the initial state, and means anything is allowed and nothing is in progress.
  ;; T means that we are in the middle of a name, but nothing else special.
  ;; DOTTED means we have encountered a single period.  TEM is what preceded it.
  ;; DOUBLE-DOTTED means we have encountered "name.name."
  ;;  TEM is the first name and TEM1 is the second.
  ;; DIRECTORY means we have encountered "name . name . name"
  ;;  which can only be the beginning of a directory name,
  ;;  or else that we have encountered a "<".
  ;; VERSION means reading a version number (following a #).
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (DO ((INDEX START (1+ INDEX))) (NIL)
    (IF ( INDEX END)
        (SETQ CHAR 'DONE)
        (SETQ CHAR (AREF STRING INDEX)))
    (COND ((AND (NOT (MEMQ CHAR '(#/SP #/TAB #/. #/: #/; #/# DONE)))
                (OR (NOT (MEMQ CHAR '(#/< #/>)))
                    (MEMQ STATE '(VERSION DOUBLE-DOTTED))))
           (AND (%STORE-CONDITIONAL (LOCF STATE) NIL T)
                (SETQ FIELD-START INDEX))
           (COND ((OR (EQ CHAR #//) (EQ CHAR #/))
                  (SETQ INDEX (1+ INDEX))
                  (OR (< INDEX END)
                      (MLM-CHAR-ERROR STRING 'DONE))
                  (SETQ CHAR (AREF STRING INDEX))
                  (AND ( CHAR #o200)
                       ( CHAR #/TAB)
                       (MLM-CHAR-ERROR STRING CHAR)))))
          ((EQ CHAR #/<)
           (COND ((NULL STATE))                 ;Extraneous whitespace.
                 ((EQ STATE T)
                  (SETQ NAME (MLM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (MLM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (AND TEM1 (SETQ TYPE TEM1))
                  (SETQ VERSION (MLM-FIELD STRING FIELD-START INDEX T)))
                 (T (MLM-CHAR-ERROR STRING CHAR)))
           (SETQ STATE 'DIRECTORY DIRECTORY NIL)
           (GO NEW-FIELD))
          ((MEMQ CHAR '(#/SP #/TAB DONE))
           (COND ((NULL STATE))                 ;Extraneous whitespace.
                 ((EQ STATE T)
                  (SETQ NAME (MLM-FIELD STRING FIELD-START INDEX) STATE NIL))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (MLM-FIELD STRING FIELD-START INDEX) STATE NIL))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (AND TEM1 (SETQ TYPE TEM1))
                  (SETQ VERSION (MLM-FIELD STRING FIELD-START INDEX T)
                        STATE NIL)
                  (COND ((EQ VERSION 0) (SETQ VERSION :NEWEST))
                        ((EQ VERSION -2) (SETQ VERSION :OLDEST))))
                 ((EQ STATE 'VERSION)
                  (SETQ VERSION (MLM-FIELD STRING FIELD-START INDEX T) STATE NIL))
                 (T (MLM-CHAR-ERROR STRING CHAR))))
          ((EQ CHAR #/.)
           (COND ((NULL STATE)                  ;Could only be :UNSPECIFIC name
                  (SETQ TEM NIL STATE 'DOTTED))
                 ((EQ STATE T)                  ;Could either be directory or name
                  (SETQ STATE 'DOTTED TEM (MLM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOTTED)
                  (OR TEM (MLM-CHAR-ERROR STRING #/.))
                  (SETQ TEM1 (MLM-FIELD STRING FIELD-START INDEX)
                        STATE 'DOUBLE-DOTTED))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (OR TEM (MLM-CHAR-ERROR STRING #/.))
                  (SETQ STATE 'DIRECTORY
                        DIRECTORY (LIST* TEM TEM1 (MLM-FIELD STRING FIELD-START INDEX) NIL)))
                 ((EQ STATE 'DIRECTORY)
                  (SETQ DIRECTORY
                        (NCONC DIRECTORY (NCONS (MLM-FIELD STRING FIELD-START INDEX)))))
                 ((EQ STATE 'VERSION)
                  (SETQ VERSION (MLM-FIELD STRING FIELD-START INDEX T)
                        STATE 'DOTTED))
                 (T (MLM-CHAR-ERROR STRING CHAR)))
           (GO NEW-FIELD))
          ((EQ CHAR #/#)
           (COND ((NULL STATE)
                  (SETQ STATE 'VERSION))
                 ((EQ STATE T)
                  (SETQ NAME (MLM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (MLM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
                 (T (MLM-CHAR-ERROR STRING CHAR)))
           (GO NEW-FIELD))
          ((OR (EQ CHAR #/;) (EQ CHAR #/>))
           (COND ((EQ STATE T)
                  (SETQ DIRECTORY (MLM-FIELD STRING FIELD-START INDEX))
                  (IF (STRING-EQUAL DIRECTORY "~")
                      (SETQ DIRECTORY :ROOT)))
                 ((EQ STATE 'DOTTED)
                  (OR TEM (MLM-CHAR-ERROR STRING CHAR))
                  (SETQ DIRECTORY (LIST TEM (MLM-FIELD STRING FIELD-START INDEX))))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (OR (AND TEM TEM1) (MLM-CHAR-ERROR STRING CHAR))
                  (SETQ DIRECTORY (LIST TEM TEM1 (MLM-FIELD STRING FIELD-START INDEX))))
                 ((EQ STATE 'DIRECTORY)
                  (LET ((FIELD (MLM-FIELD STRING FIELD-START INDEX)))
                    (IF (AND (NULL DIRECTORY)
                             (EQ FIELD :UNSPECIFIC))
                        (SETQ DIRECTORY :ROOT)
                        (SETQ DIRECTORY
                              (NCONC DIRECTORY (LIST FIELD))))))
                 (T (MLM-CHAR-ERROR STRING CHAR)))
           (SETQ STATE NIL))
          ((EQ STATE T)
           (SETQ DEVICE (SEND SELF :PARSE-DEVICE-SPEC (MLM-FIELD STRING FIELD-START INDEX))
                 STATE NIL))
          (T (MLM-CHAR-ERROR STRING CHAR)))
    (GO SKIP)
  NEW-FIELD
    (SETQ FIELD-START (1+ INDEX))
  SKIP
    (AND (EQ CHAR 'DONE)
         (RETURN (values DEVICE DIRECTORY NAME TYPE VERSION)))))

(DEFPROP MLM-CHAR-ERROR T :ERROR-REPORTER)
(DEFUN MLM-CHAR-ERROR (STRING CHAR)
  (IF (EQ CHAR 'DONE)
      (FERROR :PATHNAME-PARSE-ERROR "Unexpected end of string while parsing ~S." STRING)
      (FERROR :PATHNAME-PARSE-ERROR
              "Unexpected character (~:C) while parsing ~S." CHAR STRING)))

(DEFUN MLM-FIELD (STRING &OPTIONAL (START 0) END VERSION-P DEVICE-P
                        &AUX SIZE ARR CHAR)
  DEVICE-P
  (OR END (SETQ END (ARRAY-ACTIVE-LENGTH STRING)))
  (SETQ SIZE (- END START))
  (IF (ZEROP SIZE)
      :UNSPECIFIC
      (SETQ ARR (MAKE-ARRAY SIZE :TYPE 'ART-STRING))
      (DO ((I START (1+ I))
           (SI 0))
          (( I END)
           (OR (= SI SIZE)
               (SETQ ARR (ADJUST-ARRAY-SIZE ARR SI))))
        (COND ((NOT (MEMQ (SETQ CHAR (AREF STRING I)) '(#// #/)))
               (AND ( CHAR 200)
                    ( CHAR #/TAB)
                    (MLM-CHAR-ERROR STRING CHAR))
               (ASET (CHAR-UPCASE CHAR) ARR SI)
               (SETQ SI (1+ SI)))))
      (COND ((STRING-EQUAL ARR "*") :WILD)
            ((NOT VERSION-P) ARR)
            ((fs:NUMERIC-P ARR NIL T))
            ((CDR (ASSOC ARR '((">" . :NEWEST) ("<" . :OLDEST)))))
            (T (FERROR :PATHNAME-PARSE-ERROR "Invalid version spec ~S in ~S" ARR STRING)))))

;;; Like MLM-FIELD, but doesn't "unquotify", or know about versions.
(DEFUN MLM-SPEC (SPEC &AUX LENGTH UPCASE-FLAG CHAR)
  (COND ((STRINGP SPEC)
         (DOTIMES (I (SETQ LENGTH (ARRAY-ACTIVE-LENGTH SPEC)))
           (AND (> (SETQ CHAR (AREF SPEC I)) 177)
                ( CHAR #/TAB)
                (MLM-CHAR-ERROR SPEC CHAR))
           (AND ( CHAR #/a)
                ( CHAR #/z)
                (SETQ UPCASE-FLAG T)))
         (COND ((ZEROP LENGTH) :UNSPECIFIC)
               ((STRING-EQUAL SPEC "*") :WILD)
               (UPCASE-FLAG (STRING-UPCASE SPEC))
               (T SPEC)))
        (T SPEC)))

(DEFVAR *MBFS-USE-TWENEX-SYNTAX* NIL
  "When non-NIL, print out LMFS pathnames in Twenex syntax.")

(DEFUN MLM-NAMESTRING (HOST DEVICE DIRECTORY NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (MLM-PRINT-NAME NAME TYPE VERSION S
                   (MLM-PRINT-DIRECTORY DEVICE DIRECTORY S
                                       (MLM-PRINT-HOST HOST S)))))

(DEFUN MLM-NAMESTRING-FOR-DIRECTORY (DEVICE DIRECTORY)
  (WITH-OUTPUT-TO-STRING (S)
    (MLM-PRINT-DIRECTORY DEVICE DIRECTORY S NIL)))

(DEFUN MLM-NAMESTRING-FOR-EDITOR (HOST DEVICE DIRECTORY NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (MLM-PRINT-NAME NAME TYPE VERSION S NIL)
    (SEND S :TYO #/SP)
    (MLM-PRINT-DIRECTORY DEVICE DIRECTORY S NIL)
    (SEND S :TYO #/SP)
    (MLM-PRINT-HOST HOST S)))

(DEFUN MLM-NAMESTRING-FOR-DIRED (NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (UNLESS (MEMQ NAME '(NIL :UNSPECIFIC))
      (MLM-PRINT-COMPONENT NAME S))
    (WHEN TYPE
      (SEND S :TYO #/.)
      (OR (EQ TYPE :UNSPECIFIC)
          (MLM-PRINT-COMPONENT TYPE S)))
    (IF (NOT *MBFS-USE-TWENEX-SYNTAX*) (FORMAT S "~15T"))
    (WHEN VERSION
      (SEND S :TYO (IF *MBFS-USE-TWENEX-SYNTAX* #/. #/#))
      (UNLESS (EQ VERSION :UNSPECIFIC)
        (MLM-PRINT-COMPONENT VERSION S T)))))

(DEFUN MLM-PRINT-HOST (HOST S)
  (COND ((NOT (NULL HOST))
         (MLM-PRINT-COMPONENT (SEND HOST :NAME-AS-FILE-COMPUTER) S)
         (SEND S :TYO #/:)
         T)
        (T NIL)))

(DEFUN MLM-PRINT-DIRECTORY (DEVICE DIRECTORY S SPACE)
  (COND ((NOT (EQ DIRECTORY :UNSPECIFIC))
         (IF *MBFS-USE-TWENEX-SYNTAX*
             (SEND S :TYO #/<)
             (IF (NULL SPACE)
                 (SETQ SPACE T)
                 (SEND S :TYO #/SP)))
         (COND ((MEMQ DIRECTORY '(NIL :ROOT))
                (IF (NOT *MBFS-USE-TWENEX-SYNTAX*)
                    (SEND S :TYO #/~)))
               ((LISTP DIRECTORY)
                (DO ((D DIRECTORY (CDR D)))
                    ((NULL D))
                  (MLM-PRINT-COMPONENT (CAR D) S)
                  (IF (NOT (NULL (CDR D)))
                      (SEND S :TYO #/.))))
               (T (MLM-PRINT-COMPONENT DIRECTORY S)))
         (SEND S :TYO (IF *MBFS-USE-TWENEX-SYNTAX* #/> #/;))))
  (COND ((AND (NOT (MEMQ DEVICE '(NIL :UNSPECIFIC)))
              (NOT (EQUAL DEVICE "DSK")))
         (IF (NOT *MBFS-USE-TWENEX-SYNTAX*)
             (IF (NULL SPACE)
                 (SETQ SPACE T)
                 (SEND S :TYO #/SP)))
         (MLM-PRINT-COMPONENT DEVICE S)
         (SEND S :TYO #/:)))
  SPACE)

(DEFUN MLM-PRINT-NAME (NAME TYPE VERSION S SPACE)
  (UNLESS (MEMQ NAME '(NIL :UNSPECIFIC))
    (WHEN SPACE
      (IF (NOT *MBFS-USE-TWENEX-SYNTAX*)
          (SEND S :TYO #/SP))
      (SETQ SPACE NIL))
    (MLM-PRINT-COMPONENT NAME S))
  (UNLESS (NULL TYPE)
    (WHEN SPACE
      (IF (NOT *MBFS-USE-TWENEX-SYNTAX*)
          (SEND S :TYO #/SP))
      (SETQ SPACE NIL))
    (SEND S :TYO #/.)
    (IF (NOT (EQ TYPE :UNSPECIFIC))
        (MLM-PRINT-COMPONENT TYPE S)))
  (IF *MBFS-USE-TWENEX-SYNTAX*
      (UNLESS (MEMQ VERSION '(NIL :NEWEST))
        (SEND S :TYO #/.)
        (IF (NOT (EQ VERSION :UNSPECIFIC))
            (MLM-PRINT-COMPONENT VERSION S T))))
  (UNLESS (NULL VERSION)
    (WHEN SPACE
      (SEND S :TYO #/SP)
      (SETQ SPACE NIL))
    (SEND S :TYO #/#)
    (IF (NOT (EQ VERSION :UNSPECIFIC))
        (MLM-PRINT-COMPONENT VERSION S T)))
  SPACE)

(DEFUN MLM-PRINT-COMPONENT (SPEC STREAM &OPTIONAL VERSION-P &AUX TEM)
  (COND ((EQ SPEC :WILD) (SEND STREAM :TYO #/*))
        ((NUMBERP SPEC)
         (LET ((*PRINT-BASE* 10.)
               (*NOPOINT T))
           (SI:PRINT-FIXNUM SPEC STREAM)))
        (VERSION-P
         (COND ((SETQ TEM (CDR (ASSQ SPEC '((:NEWEST . #/>) (:OLDEST . #/<)))))
                (SEND STREAM :TYO TEM))
               (T (FERROR "Attempt to print ~S, which is not a valid version." SPEC))))
        ((STRINGP SPEC)
         (DOTIMES (I (ARRAY-ACTIVE-LENGTH SPEC))
           (AND (MEMQ (SETQ TEM (AREF SPEC I)) '(#/SP #/TAB #/. #/: #/; #/# #// #/> #/<))
                (SEND STREAM :TYO #//))
           (SEND STREAM :TYO TEM)))
        (T (FERROR NIL "Attempt to print ~S, which is not a valid component." SPEC))))


(DEFFLAVOR MLM-PARSING-MIXIN () ()
  (:REQUIRED-FLAVORS fs:HOST-PATHNAME))

(DEFMETHOD (MLM-PARSING-MIXIN :PARSE-NAMESTRING) (HOST-SPECIFIED NAMESTRING
                                                 &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (MLM-PARSE-NAMESTRING NAMESTRING START END))

(DEFMETHOD (MLM-PARSING-MIXIN :STRING-FOR-PRINTING) ()
  (MLM-NAMESTRING fs:HOST fs:DEVICE fs:DIRECTORY fs:NAME fs:TYPE fs:VERSION))

(DEFMETHOD (MLM-PARSING-MIXIN :STRING-FOR-HOST) ()
  (MLM-NAMESTRING NIL fs:DEVICE fs:DIRECTORY fs:NAME fs:TYPE fs:VERSION))

(DEFMETHOD (MLM-PARSING-MIXIN :STRING-FOR-DIRED) ()
  (MLM-NAMESTRING-FOR-DIRED fs:NAME fs:TYPE fs:VERSION))

(DEFMETHOD (MLM-PARSING-MIXIN :STRING-FOR-EDITOR) ()
  (MLM-NAMESTRING-FOR-EDITOR fs:HOST fs:DEVICE fs:DIRECTORY fs:NAME fs:TYPE fs:VERSION))

(DEFMETHOD (MLM-PARSING-MIXIN :STRING-FOR-DIRECTORY) ()
  (MLM-NAMESTRING-FOR-DIRECTORY fs:DEVICE fs:DIRECTORY))

(DEFMETHOD (MLM-PARSING-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (COND ((STRINGP SPEC)
         (MLM-SPEC SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T "FOO")))

(DEFMETHOD (MLM-PARSING-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ;; Canonicalize list of length 1 into a single string.
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ;; A list of strings is also a structured directory.
        ((AND (CONSP SPEC)
              (LOOP FOR ELT IN SPEC
                    ALWAYS (OR (STRINGP ELT) (MEMQ ELT '(NIL :ROOT :UNSPECIFIC :WILD)))))
         (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
        ((MEMQ SPEC '(NIL :ROOT :UNSPECIFIC :WILD)) SPEC)
        (T (fs:PATHNAME-DIRECTORY (fs:QUIET-USER-HOMEDIR fs:HOST)))))

(DEFMETHOD (MLM-PARSING-MIXIN :PARSE-VERSION-SPEC) (SPEC)
  (IF (OR (FIXNUMP SPEC)
          (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
      SPEC :NEWEST))

(DEFMETHOD (MLM-PARSING-MIXIN :UNDELETABLE-P) () T)

(DEFMETHOD (MLM-PARSING-MIXIN :PRIMARY-DEVICE) () "DSK")

(DEFMETHOD (MLM-PARSING-MIXIN :DIRECTORY-FILE-TYPE) () "DIRECTORY")
(DEFMETHOD (MLM-PARSING-MIXIN :DIRECTORY-PATHNAME-AS-FILE) DEFAULT-DIRECTORY-PATHNAME-AS-FILE)

(DEFFLAVOR MLM-PATHNAME () (MLM-PARSING-MIXIN fs:HOST-PATHNAME))
(DEFPROP :MOBY-LISPM MLM-PATHNAME LISPM-PATHNAME-FLAVOR)
(COMPILE-FLAVOR-METHODS MLM-PATHNAME)
