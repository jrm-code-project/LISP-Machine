;; -*- Mode:LISP; Package:FS; Base:8; Readtable:ZL -*-

;;; Pathname parsing support for LM systems.

(DEFUN LM-PARSE-NAMESTRING (STRING &OPTIONAL (START 0) END
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
                      (LM-CHAR-ERROR STRING 'DONE))
                  (SETQ CHAR (AREF STRING INDEX))
                  (AND ( CHAR #o200)
                       ( CHAR #/TAB)
                       (LM-CHAR-ERROR STRING CHAR)))))
          ((EQ CHAR #/<)
           (COND ((NULL STATE))                 ;Extraneous whitespace.
                 ((EQ STATE T)
                  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (AND TEM1 (SETQ TYPE TEM1))
                  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)))
                 (T (LM-CHAR-ERROR STRING CHAR)))
           (SETQ STATE 'DIRECTORY DIRECTORY NIL)
           (GO NEW-FIELD))
          ((MEMQ CHAR '(#/SP #/TAB DONE))
           (COND ((NULL STATE))                 ;Extraneous whitespace.
                 ((EQ STATE T)
                  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX) STATE NIL))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX) STATE NIL))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (AND TEM1 (SETQ TYPE TEM1))
                  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)
                        STATE NIL)
                  (COND ((EQ VERSION 0) (SETQ VERSION :NEWEST))
                        ((EQ VERSION -2) (SETQ VERSION :OLDEST))))
                 ((EQ STATE 'VERSION)
                  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T) STATE NIL))
                 (T (LM-CHAR-ERROR STRING CHAR))))
          ((EQ CHAR #/.)
           (COND ((NULL STATE)                  ;Could only be :UNSPECIFIC name
                  (SETQ TEM NIL STATE 'DOTTED))
                 ((EQ STATE T)                  ;Could either be directory or name
                  (SETQ STATE 'DOTTED TEM (LM-FIELD STRING FIELD-START INDEX)))
                 ((EQ STATE 'DOTTED)
                  (OR TEM (LM-CHAR-ERROR STRING #/.))
                  (SETQ TEM1 (LM-FIELD STRING FIELD-START INDEX)
                        STATE 'DOUBLE-DOTTED))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (OR TEM (LM-CHAR-ERROR STRING #/.))
                  (SETQ STATE 'DIRECTORY
                        DIRECTORY (LIST* TEM TEM1 (LM-FIELD STRING FIELD-START INDEX) NIL)))
                 ((EQ STATE 'DIRECTORY)
                  (SETQ DIRECTORY
                        (NCONC DIRECTORY (NCONS (LM-FIELD STRING FIELD-START INDEX)))))
                 ((EQ STATE 'VERSION)
                  (SETQ VERSION (LM-FIELD STRING FIELD-START INDEX T)
                        STATE 'DOTTED))
                 (T (LM-CHAR-ERROR STRING CHAR)))
           (GO NEW-FIELD))
          ((EQ CHAR #/#)
           (COND ((NULL STATE)
                  (SETQ STATE 'VERSION))
                 ((EQ STATE T)
                  (SETQ NAME (LM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
                 ((EQ STATE 'DOTTED)
                  (AND TEM (SETQ NAME TEM))
                  (SETQ TYPE (LM-FIELD STRING FIELD-START INDEX) STATE 'VERSION))
                 (T (LM-CHAR-ERROR STRING CHAR)))
           (GO NEW-FIELD))
          ((OR (EQ CHAR #/;) (EQ CHAR #/>))
           (COND ((EQ STATE T)
                  (SETQ DIRECTORY (LM-FIELD STRING FIELD-START INDEX))
                  (IF (STRING-EQUAL DIRECTORY "~")
                      (SETQ DIRECTORY :ROOT)))
                 ((EQ STATE 'DOTTED)
                  (OR TEM (LM-CHAR-ERROR STRING CHAR))
                  (SETQ DIRECTORY (LIST TEM (LM-FIELD STRING FIELD-START INDEX))))
                 ((EQ STATE 'DOUBLE-DOTTED)
                  (OR (AND TEM TEM1) (LM-CHAR-ERROR STRING CHAR))
                  (SETQ DIRECTORY (LIST TEM TEM1 (LM-FIELD STRING FIELD-START INDEX))))
                 ((EQ STATE 'DIRECTORY)
                  (LET ((FIELD (LM-FIELD STRING FIELD-START INDEX)))
                    (IF (AND (NULL DIRECTORY)
                             (EQ FIELD :UNSPECIFIC))
                        (SETQ DIRECTORY :ROOT)
                        (SETQ DIRECTORY
                              (NCONC DIRECTORY (LIST FIELD))))))
                 (T (LM-CHAR-ERROR STRING CHAR)))
           (SETQ STATE NIL))
          ((EQ STATE T)
           (SETQ DEVICE (SEND SELF :PARSE-DEVICE-SPEC (LM-FIELD STRING FIELD-START INDEX))
                 STATE NIL))
          (T (LM-CHAR-ERROR STRING CHAR)))
    (GO SKIP)
  NEW-FIELD
    (SETQ FIELD-START (1+ INDEX))
  SKIP
    (AND (EQ CHAR 'DONE)
         (RETURN (values DEVICE DIRECTORY NAME TYPE VERSION)))))

(DEFPROP LM-CHAR-ERROR T :ERROR-REPORTER)
(DEFUN LM-CHAR-ERROR (STRING CHAR)
  (IF (EQ CHAR 'DONE)
      (FERROR 'PATHNAME-PARSE-ERROR "Unexpected end of string while parsing ~S." STRING)
      (FERROR 'PATHNAME-PARSE-ERROR
              "Unexpected character (~:C) while parsing ~S." CHAR STRING)))

(DEFUN LM-FIELD (STRING &OPTIONAL (START 0) END VERSION-P DEVICE-P
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
                    (LM-CHAR-ERROR STRING CHAR))
               (ASET (CHAR-UPCASE CHAR) ARR SI)
               (SETQ SI (1+ SI)))))
      (COND ((STRING-EQUAL ARR "*") :WILD)
            ((NOT VERSION-P) ARR)
            ((NUMERIC-P ARR NIL T))
            ((CDR (ASSOC ARR '((">" . :NEWEST) ("<" . :OLDEST)))))
            (T (FERROR 'PATHNAME-PARSE-ERROR "Invalid version spec ~S in ~S" ARR STRING)))))

;;; Like LM-FIELD, but doesn't "unquotify", or know about versions.
(DEFUN LM-SPEC (SPEC &AUX LENGTH UPCASE-FLAG CHAR)
  (COND ((STRINGP SPEC)
         (DOTIMES (I (SETQ LENGTH (ARRAY-ACTIVE-LENGTH SPEC)))
           (AND (> (SETQ CHAR (AREF SPEC I)) 177)
                ( CHAR #/TAB)
                (LM-CHAR-ERROR SPEC CHAR))
           (AND ( CHAR #/a)
                ( CHAR #/z)
                (SETQ UPCASE-FLAG T)))
         (COND ((ZEROP LENGTH) :UNSPECIFIC)
               ((STRING-EQUAL SPEC "*") :WILD)
               (UPCASE-FLAG (STRING-UPCASE SPEC))
               (T SPEC)))
        (T SPEC)))

(DEFVAR *LMFS-USE-TWENEX-SYNTAX* NIL
  "When non-NIL, print out LMFS pathnames in Twenex syntax.")

(DEFUN LM-NAMESTRING (HOST DEVICE DIRECTORY NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (LM-PRINT-NAME NAME TYPE VERSION S
                   (LM-PRINT-DIRECTORY DEVICE DIRECTORY S
                                       (LM-PRINT-HOST HOST S)))))

(DEFUN LM-NAMESTRING-FOR-DIRECTORY (DEVICE DIRECTORY)
  (WITH-OUTPUT-TO-STRING (S)
    (LM-PRINT-DIRECTORY DEVICE DIRECTORY S NIL)))

(DEFUN LM-NAMESTRING-FOR-EDITOR (HOST DEVICE DIRECTORY NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (LM-PRINT-NAME NAME TYPE VERSION S NIL)
    (SEND S :TYO #/SP)
    (LM-PRINT-DIRECTORY DEVICE DIRECTORY S NIL)
    (SEND S :TYO #/SP)
    (LM-PRINT-HOST HOST S)))

(DEFUN LM-NAMESTRING-FOR-DIRED (NAME TYPE VERSION)
  (WITH-OUTPUT-TO-STRING (S)
    (UNLESS (MEMQ NAME '(NIL :UNSPECIFIC))
      (LM-PRINT-COMPONENT NAME S))
    (WHEN TYPE
      (SEND S :TYO #/.)
      (OR (EQ TYPE :UNSPECIFIC)
          (LM-PRINT-COMPONENT TYPE S)))
    (IF (NOT *LMFS-USE-TWENEX-SYNTAX*) (FORMAT S "~15T"))
    (WHEN VERSION
      (SEND S :TYO (IF *LMFS-USE-TWENEX-SYNTAX* #/. #/#))
      (UNLESS (EQ VERSION :UNSPECIFIC)
        (LM-PRINT-COMPONENT VERSION S T)))))

(DEFUN LM-PRINT-HOST (HOST S)
  (COND ((NOT (NULL HOST))
         (LM-PRINT-COMPONENT (SEND HOST :NAME-AS-FILE-COMPUTER) S)
         (SEND S :TYO #/:)
         T)
        (T NIL)))

(DEFUN LM-PRINT-DIRECTORY (DEVICE DIRECTORY S SPACE)
  (COND ((NOT (MEMQ DIRECTORY '(NIL :UNSPECIFIC)))
         (IF *LMFS-USE-TWENEX-SYNTAX*
             (SEND S :TYO #/<)
             (IF (NULL SPACE)
                 (SETQ SPACE T)
                 (SEND S :TYO #/SP)))
         (COND ((eq DIRECTORY :ROOT)
                (IF (NOT *LMFS-USE-TWENEX-SYNTAX*)
                    (SEND S :TYO #/~)))
               ((LISTP DIRECTORY)
                (DO ((D DIRECTORY (CDR D)))
                    ((NULL D))
                  (LM-PRINT-COMPONENT (CAR D) S)
                  (IF (NOT (NULL (CDR D)))
                      (SEND S :TYO #/.))))
               (T (LM-PRINT-COMPONENT DIRECTORY S)))
         (SEND S :TYO (IF *LMFS-USE-TWENEX-SYNTAX* #/> #/;))))
  (COND ((AND (NOT (MEMQ DEVICE '(NIL :UNSPECIFIC)))
              (NOT (EQUAL DEVICE "DSK")))
         (IF (NOT *LMFS-USE-TWENEX-SYNTAX*)
             (IF (NULL SPACE)
                 (SETQ SPACE T)
                 (SEND S :TYO #/SP)))
         (LM-PRINT-COMPONENT DEVICE S)
         (SEND S :TYO #/:)))
  SPACE)

(DEFUN LM-PRINT-NAME (NAME TYPE VERSION S SPACE)
  (UNLESS (MEMQ NAME '(NIL :UNSPECIFIC))
    (WHEN SPACE
      (IF (NOT *LMFS-USE-TWENEX-SYNTAX*)
          (SEND S :TYO #/SP))
      (SETQ SPACE NIL))
    (LM-PRINT-COMPONENT NAME S))
  (UNLESS (NULL TYPE)
    (WHEN SPACE
      (IF (NOT *LMFS-USE-TWENEX-SYNTAX*)
          (SEND S :TYO #/SP))
      (SETQ SPACE NIL))
    (SEND S :TYO #/.)
    (IF (NOT (EQ TYPE :UNSPECIFIC))
        (LM-PRINT-COMPONENT TYPE S)))
  (IF *LMFS-USE-TWENEX-SYNTAX*
      (UNLESS (MEMQ VERSION '(NIL :NEWEST))
        (SEND S :TYO #/.)
        (IF (NOT (EQ VERSION :UNSPECIFIC))
            (LM-PRINT-COMPONENT VERSION S T)))
    (UNLESS (NULL VERSION)
      (WHEN SPACE
        (SEND S :TYO #/SP)
        (SETQ SPACE NIL))
      (SEND S :TYO #/#)
      (IF (NOT (EQ VERSION :UNSPECIFIC))
          (LM-PRINT-COMPONENT VERSION S T))))
  SPACE)

(DEFUN LM-PRINT-COMPONENT (SPEC STREAM &OPTIONAL VERSION-P &AUX TEM)
  (COND ((EQ SPEC :WILD) (SEND STREAM :TYO #/*))
        ((NUMBERP SPEC)
         (LET ((*PRINT-BASE* 10.)
               (*NOPOINT T)
               (*PRINT-RADIX* NIL))
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
        (T (FERROR "Attempt to print ~S, which is not a valid component." SPEC))))


(DEFFLAVOR LM-PARSING-MIXIN () ()
  (:REQUIRED-FLAVORS HOST-PATHNAME))

(DEFMETHOD (LM-PARSING-MIXIN :PARSE-NAMESTRING) (HOST-SPECIFIED NAMESTRING
                                                 &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (LM-PARSE-NAMESTRING NAMESTRING START END))

(DEFMETHOD (LM-PARSING-MIXIN :STRING-FOR-PRINTING) ()
  (LM-NAMESTRING HOST DEVICE DIRECTORY NAME TYPE VERSION))

(DEFMETHOD (LM-PARSING-MIXIN :STRING-FOR-HOST) ()
  (LM-NAMESTRING NIL DEVICE DIRECTORY NAME TYPE VERSION))

(DEFMETHOD (LM-PARSING-MIXIN :STRING-FOR-DIRED) ()
  (LM-NAMESTRING-FOR-DIRED NAME TYPE VERSION))

(DEFMETHOD (LM-PARSING-MIXIN :STRING-FOR-EDITOR) ()
  (LM-NAMESTRING-FOR-EDITOR HOST DEVICE DIRECTORY NAME TYPE VERSION))

(DEFMETHOD (LM-PARSING-MIXIN :STRING-FOR-DIRECTORY) ()
  (LM-NAMESTRING-FOR-DIRECTORY DEVICE DIRECTORY))

(DEFMETHOD (LM-PARSING-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (COND ((STRINGP SPEC)
         (LM-SPEC SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T "FOO")))

(DEFMETHOD (LM-PARSING-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ;; Canonicalize list of length 1 into a single string.
        ((and (consp spec)
              (eq (car spec) :relative))
         (cons (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST))
               (cdr spec)))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ;; A list of strings is also a structured directory.
        ((AND (CONSP SPEC)
              (LOOP FOR ELT IN SPEC
                    ALWAYS (OR (STRINGP ELT) (MEMQ ELT '(NIL :ROOT :UNSPECIFIC :WILD :RELATIVE)))))
         (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
        ((MEMQ SPEC '(NIL :ROOT :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DIRECTORY (QUIET-USER-HOMEDIR HOST)))))

(DEFMETHOD (LM-PARSING-MIXIN :PARSE-VERSION-SPEC) (SPEC)
  (IF (OR (FIXNUMP SPEC)
          (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
      SPEC :NEWEST))

(DEFMETHOD (LM-PARSING-MIXIN :UNDELETABLE-P) () T)

(DEFMETHOD (LM-PARSING-MIXIN :PRIMARY-DEVICE) () "DSK")

(DEFMETHOD (LM-PARSING-MIXIN :DIRECTORY-FILE-TYPE) () "DIRECTORY")
(DEFMETHOD (LM-PARSING-MIXIN :DIRECTORY-PATHNAME-AS-FILE) DEFAULT-DIRECTORY-PATHNAME-AS-FILE)

(DEFFLAVOR LM-PATHNAME () (FS:LM-PARSING-MIXIN HOST-PATHNAME))
(DEFPROP :LISPM LM-PATHNAME LISPM-PATHNAME-FLAVOR)
(DEFPROP LM-PATHNAME T PATHNAME-FLAVOR-CHANGES)
(COMPILE-FLAVOR-METHODS LM-PATHNAME)
