;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-

;;; Un exemple de ce que doit contenir le fichier MAC-PATHNAME


;;; Question or thing to see???!!!

;;; canonical-type a declarer dans pathnm.lisp

;;; undeletable-p avec le mac on peut toujours jeter des fichiers

;;; these two operations are not supported
;;; :create-link
;;; :remote-connect

;;; wildcard operations (mapping translation)

;;; :default-directory-stream-parser WHY???
;;; :directory-stream
;;; :directory-list-stream



(DEFCONST COMPONENT-MAX-LENGTH 31)
(DEFCONST FIRST-COMPONENT-MAX-LENGTH 27)



(DEFFLAVOR MAC-PARSING-MIXIN ()
           ()
  (:REQUIRED-FLAVORS FS:HOST-PATHNAME))



(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-NAMESTRING) (HOST-SPECIFIED NAMESTRING
                                                 &OPTIONAL (START 0) END)
  HOST-SPECIFIED
  (MAC-PARSE-NAMESTRING NAMESTRING START END))

;;;
;;; Pathname parsing support for Mac-File-System.
;;;

(DEFUN MAC-PARSE-NAMESTRING (NAMESTRING &OPTIONAL (START 0) END
                             &AUX (DEVICE :UNSPECIFIC) (DIRECTORY NIL) NAME TYPE VERSION)
" Directory-name are separate by #/:
  the first one cannot exceed 27 characters the others can have 32 characters
  The others component can have 31 characters for all three together
  something like  << name . type # version >>
      must have   <<   31 characters  MAX  >>
  Two characters cannot appear in a component #/X  All the fields are positionnal
  You cannot quote characters on the MacIntosh side
  On the MacIntosh side we can use the 254. others characters
  on the Lisp Machine side the characters greater than 127 are precedent by #/X  and the following is (- character 128)
  The version is the string following the last #/#, it's converted in number.
  The type is the string following the last #/. but before the version if it is present."
  (SETQ NAMESTRING (SUBSTRING NAMESTRING START END))
  (LET ((FIRST T)(SUB-DIRECTORY T) IDX TEM)
    (LOOP WHILE (SETQ IDX (STRING-SEARCH-CHAR #/: NAMESTRING))
          DO (SETQ SUB-DIRECTORY (MAC-FIELD (SUBSTRING NAMESTRING 0 IDX) FIRST))
          (IF (SETQ TEM (CDR (ASSOC SUB-DIRECTORY '(("~" . :ROOT)("" . :UNSPECIFIC) ("*" . :WILD)))))
              (SETQ SUB-DIRECTORY TEM))
          (SETQ FIRST NIL
                NAMESTRING (SUBSTRING NAMESTRING (1+ IDX))
                DIRECTORY (CONS SUB-DIRECTORY DIRECTORY)))
    (SETQ DIRECTORY
          (COND ((NULL (CDR DIRECTORY))(CAR DIRECTORY))
                (T (REVERSE DIRECTORY))))

    (SETQ NAMESTRING (MAC-FIELD NAMESTRING))
    (AND (SETQ IDX (STRING-REVERSE-SEARCH-CHAR #/# NAMESTRING))
         (SETQ VERSION (SUBSTRING NAMESTRING (1+ IDX)))
         (SETQ VERSION (COND ((CDR (ASSOC VERSION
                                         '(("" . :UNSPECIFIC)("*" . :WILD)(">" . :NEWEST)("<" . :OLDEST)))))
                             ((FS:NUMERIC-P VERSION nil t)))))

    (SETQ NAMESTRING (SUBSTRING NAMESTRING 0 IDX))
    (AND (SETQ IDX  (STRING-REVERSE-SEARCH-CHAR #/. NAMESTRING))
         (SETQ TYPE (SUBSTRING NAMESTRING (1+ IDX)))
         (SETQ TEM  (CDR (ASSOC TYPE '(("" . :UNSPECIFIC) ("*" . :WILD)))))
         (SETQ TYPE TEM))

    (SETQ NAMESTRING (SUBSTRING NAMESTRING 0 IDX))
    (SETQ NAME (COND ((ZEROP (ARRAY-ACTIVE-LENGTH NAMESTRING)) NIL)
                     ((STRING-EQUAL NAMESTRING "*") :WILD)
                     (T NAMESTRING))))
  (VALUES DEVICE DIRECTORY NAME TYPE VERSION))


;;; for the moment (25 May 1988)
;;; The LispMachine dont have a print representation for characters above 127
(DEFUN MAC-FIELD (STRING &OPTIONAL FIRST
                         &AUX CHAR (LENGTH (ARRAY-ACTIVE-LENGTH STRING)))
  (IF (OR (AND FIRST (> LENGTH FIRST-COMPONENT-MAX-LENGTH))
          (> LENGTH COMPONENT-MAX-LENGTH))
      (FERROR 'PATHNAME-PARSE-ERROR "~A is a too long string for a pathname spec" STRING)
    (DOTIMES (I LENGTH)
      (IF (> (SETQ CHAR (AREF STRING I)) 127)
          (FERROR 'PATHNAME-PARSE-ERROR
                  "Unexpected character (~:C) while parsing ~S." CHAR STRING)))
    STRING))


;;; Default is to leave the string alone
;;; These operations should in general convert an interchange component to a raw one
;;; and also turn any invalid component into something valid.
;;;
(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-COMPONENT-SPEC) (SPEC)
  (COND ((CONSP SPEC) (MAPCAR (LAMBDA (X)
                                (SEND SELF :PARSE-COMPONENT-SPEC X))
                              SPEC))
        ((STRINGP SPEC) (MAC-FIELD SPEC))
        ((MEMQ SPEC '(NIL :ROOT :UNSPECIFIC :WILD :NEWEST :OLDEST)) SPEC)
        (T "FOO")))

;;; They are no device on the MacIntosh system
;;; the default is :UNSPECIFIC
;;;
(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-DEVICE-SPEC) (IGNORE)
  :UNSPECIFIC)


;;; A list of strings is also a structured directory.
;;;
(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-DIRECTORY-SPEC) (SPEC)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (LOOP FOR ELT IN SPEC
                    ALWAYS (OR (STRINGP ELT) (MEMQ ELT '(NIL :ROOT :UNSPECIFIC :WILD)))))
         (MAPCAR SELF (CIRCULAR-LIST :PARSE-COMPONENT-SPEC) SPEC))
        ((MEMQ SPEC '(NIL :ROOT :UNSPECIFIC :WILD)) SPEC)
        (T (PATHNAME-DIRECTORY (FS:QUIET-USER-HOMEDIR FS:HOST)))))

;;;
;;; WE needs to rewrite :PARSE-NAME-SPEC :PARSE-TYPE-SPEC :PARSE-VERSION-SPEC
;;; Because the length of the 3 component together should not be more than 31 characters
;;;
(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-NAME-SPEC) (SPEC)
  (SEND SELF :LENGTH-NAME-TYPE-VERSION)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((AND (CONSP SPEC)
              (STRINGP (CAR SPEC))
              (NULL (CDR SPEC)))
         (SEND SELF :PARSE-COMPONENT-SPEC (CAR SPEC)))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T "FOO")))


(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-TYPE-SPEC) (SPEC)
  (SEND SELF :LENGTH-NAME-TYPE-VERSION)
  (COND ((STRINGP SPEC) (SEND SELF :PARSE-COMPONENT-SPEC SPEC))
        ((MEMQ SPEC '(NIL :UNSPECIFIC :WILD)) SPEC)
        (T (FS:DECODE-CANONICAL-TYPE :LISP (SEND FS:HOST :SYSTEM-TYPE)))))


(DEFMETHOD (MAC-PARSING-MIXIN :PARSE-VERSION-SPEC) (SPEC)
  (SEND SELF :LENGTH-NAME-TYPE-VERSION)
  (IF (OR (FIXNUMP SPEC)
          (MEMQ SPEC '(NIL :UNSPECIFIC :WILD :NEWEST :OLDEST)))
      SPEC :NEWEST))

;;; The two following are use to count the length of (name . type # version)
;;;
(DEFUN SPEC-LENGTH (SPEC)
  (COND ((STRINGP SPEC)(ARRAY-ACTIVE-LENGTH SPEC))
        ((NUMBERP SPEC)(ARRAY-ACTIVE-LENGTH (STRING SPEC)))
        (T 1)))                                 ;FOR the single character '(#/< #/> #/# #/.)

(DEFMETHOD (MAC-PARSING-MIXIN :LENGTH-NAME-TYPE-VERSION) ()
  (OR (> COMPONENT-MAX-LENGTH
         (+ (SPEC-LENGTH (SEND SELF :NAME))
            (SPEC-LENGTH (SEND SELF :TYPE))
            (SPEC-LENGTH (SEND SELF :VERSION))))
      (FERROR 'PATHNAME-PARSE-ERROR "Component too long in pathname: ~a." self)))

;;;

(DEFMETHOD (MAC-PARSING-MIXIN :DIRECTORY-FILE-TYPE) ()  :UNSPECIFIC)

(DEFMETHOD (MAC-PARSING-MIXIN :DIRECTORY-DELIMITER-CHARACTER) () #/:)

(DEFMETHOD (MAC-PARSING-MIXIN :QUOTE-CHARACTER) () #//)

(DEFMETHOD (MAC-PARSING-MIXIN :CHARACTER-NEEDS-QUOTING-P) (CHARACTER)
  (MEMQ CHARACTER '(#/; #/SP)))


(DEFMETHOD (MAC-PARSING-MIXIN :UNDELETABLE-P) () T)

(DEFMETHOD (MAC-PARSING-MIXIN :VERSION-DELIMITER) () #/#)

(DEFMETHOD (MAC-PARSING-MIXIN :SUPPRESSED-DEVICE-NAMES) () '(NIL :UNSPECIFIC))


;;;
;;;   string stuff
;;;

(DEFSUBST MAC-PRINT-DIRECTORY (SPEC)
    (IF (LISTP SPEC)
        (FORMAT NIL "~{~A:~}" (MAPCAR #'MAC-PRINT-COMPONENT SPEC))
      (FORMAT NIL "~:[~*~;~A:~]" SPEC (MAC-PRINT-COMPONENT SPEC))))


(DEFUN MAC-PRINT-COMPONENT (SPEC)
  (COND ((NULL SPEC) NIL)
        ((STRINGP SPEC) (MAC-OUTPUT-STRING SPEC))
        ((NUMBERP SPEC) (FORMAT NIL "~A" SPEC))
        ((CDR (ASSQ SPEC '((:ROOT . "~") (:NEWEST . ">")(:OLDEST . "<")(:WILD . "*") (:UNSPECIFIC . "")))))
        (T (FERROR "Attempt to print ~S, which is not a valid component." SPEC))))


(DEFUN MAC-OUTPUT-STRING (SPEC &AUX CHAR)
  (WITH-OUTPUT-TO-STRING (STREAM)
    (DOTIMES (I (ARRAY-ACTIVE-LENGTH SPEC))
      (AND (MEMQ (SETQ CHAR (AREF SPEC I)) '(#/SP #/TAB #/. #/: #/; #/# #// #/> #/<))
                (SEND STREAM :TYO #//))
           (SEND STREAM :TYO CHAR))))


(DEFMETHOD (MAC-PARSING-MIXIN :STRING-FOR-EDITOR) ()
  (FORMAT NIL "~:[~*~;~A~]~:[~*~;.~A~]~:[~*~;#~A~] ~A ~:[~*~;~A:~]"
          FS:NAME (MAC-PRINT-COMPONENT FS:NAME)
          TYPE (MAC-PRINT-COMPONENT TYPE)
          FS:VERSION (MAC-PRINT-COMPONENT FS:VERSION)
          (MAC-PRINT-DIRECTORY DIRECTORY)
          FS:HOST (SEND FS:HOST :NAME-AS-FILE-COMPUTER)))


(DEFMETHOD (MAC-PARSING-MIXIN :STRING-FOR-DIRED) ()
  (FORMAT NIL "~:[~*~;~A~]~:[~*~;.~A~]~15T~:[~*~;#~A~]"
          FS:NAME (MAC-PRINT-COMPONENT FS:NAME)
          TYPE (MAC-PRINT-COMPONENT TYPE)
          FS:VERSION (MAC-PRINT-COMPONENT FS:VERSION)))


(DEFMETHOD (MAC-PARSING-MIXIN :STRING-FOR-DIRECTORY) ()
  (MAC-PRINT-DIRECTORY DIRECTORY))


(DEFMETHOD (MAC-PARSING-MIXIN :STRING-FOR-PRINTING) ()
  (FORMAT NIL "~:[~*~;~A:~]~A~:[~*~;~A~]~:[~*~;.~A~]~:[~*~;#~A~]"
          FS:HOST (SEND FS:HOST :NAME-AS-FILE-COMPUTER)
          (MAC-PRINT-DIRECTORY DIRECTORY)
          FS:NAME (MAC-PRINT-COMPONENT FS:NAME)
          TYPE (MAC-PRINT-COMPONENT TYPE)
          FS:VERSION (MAC-PRINT-COMPONENT FS:VERSION)))


(DEFMETHOD (MAC-PARSING-MIXIN :STRING-FOR-HOST) ()
  (FORMAT NIL "~A~:[~*~;~A~]~:[~*~;.~A~]~:[~*~;#~A~]"
          (MAC-PRINT-DIRECTORY DIRECTORY)
          FS:NAME (MAC-PRINT-COMPONENT FS:NAME)
          TYPE (MAC-PRINT-COMPONENT TYPE)
          FS:VERSION (MAC-PRINT-COMPONENT FS:VERSION)))


;;;
;;;  :INIT-FILE
;;;
; Il n'y a pas de changement pour le MACintosh

;(DEFMETHOD (HOST-PATHNAME :INIT-FILE) (PROGRAM-NAME)
;  (SEND SELF :NEW-PATHNAME :NAME PROGRAM-NAME
;                           :TYPE :INIT
;                           :VERSION :NEWEST))


;;;
;;; PATCH-FILE-PATHNAME
;;;
; Il n'y a pas de changement pour le MACintosh
; Peut-etre a ecrire?????
; pour le moment copie identique de PATHNAME

;(DEFMETHOD (PATHNAME :PATCH-FILE-PATHNAME) (NAM SAME-DIRECTORY-P PATOM TYP &REST ARGS)
;  (LET ((PATOM (STRING-UPCASE (IF SAME-DIRECTORY-P PATOM NAM))))
;    (SELECTQ TYP
;      (:SYSTEM-DIRECTORY
;       (SEND SELF :NEW-PATHNAME
;                 :NAME PATOM
;                 :TYPE (IF SAME-DIRECTORY-P "DIRECTORY" :PATCH-DIRECTORY)
;                 :VERSION :NEWEST))
;      (:VERSION-DIRECTORY
;       (SEND SELF :NEW-PATHNAME
;                  :NAME (FORMAT NIL "~A-~D" PATOM (CAR ARGS))
;                 :TYPE (IF SAME-DIRECTORY-P "DIRECTORY" :PATCH-DIRECTORY)
;                 :VERSION :NEWEST))
;      (:PATCH-FILE
;       (SEND SELF :NEW-PATHNAME
;                 :NAME (FORMAT NIL "~A-~D-~D" PATOM (CAR ARGS) (CADR ARGS))
;                 :TYPE (CADDR ARGS)
;                 :VERSION :NEWEST)))))


(DEFFLAVOR MAC-PATHNAME ((FS:DEVICE :UNSPECIFIC))
           (MAC-PARSING-MIXIN FS:HOST-PATHNAME))

(DEFPROP :KMAC MAC-PATHNAME LISPM-PATHNAME-FLAVOR)


(COMPILE-FLAVOR-METHODS MAC-PATHNAME)
