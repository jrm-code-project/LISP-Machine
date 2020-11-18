;;; -*- Mode: LISP; Package: FILE-SYSTEM; Base: 10; Patch-File: Yes;-*-


;;; (1) Compile and load this patch on your Symbolics machine.
;;; (2) using tv:edit-namespace-object add a USER PROPERTY
;;;     called :LMI-LAMBDA-P on all your LMI machines.
;;;
;;; Or, if you want, modify the definition of LMI-LAMBDA-P to use
;;; the naming heuristic, which will work on default customer lambda sites
;;; because machines are named "LAMBDA-A" "LAMBDA-B" etc...

(DEFUN LMI-LAMBDA-NAME-P (STRING)
  (STRING-SEARCH "LAMBDA" STRING))

;;; NOTES: MOST OF THIS CODE IS LIFTED DIRECTLY FROM THE LMI-LAMBDA SYSTEM
;;; CODE FOR PARSING ITS OWN PATHNAMES. AS SUCH, THE FOLLOWING IS TECHICALLY
;;; APPLICABLE:
;;; Copyright LISP Machine, Inc. 1984
;;;   See filename "Copyright.Text" for
;;; licensing and release information.
;;;
;;; Inquiries concerning copyright release or licensing
;;;  should be directed to Mr. Damon Lawrence at (213) 642-1116.
;;;  It is LMI's practice to grant royalty-free licenses on
;;;  certain software to appropriate non-profit organizations who
;;;  agree to not further distribute the software without prior
;;;  written LMI permission, and who agree to provide LMI with
;;;  all enhancements made to the software.
;;; NOT THAT ANYONE WOULD WANT TO USE THIS CODE FOR ANYTHING OTHER THAN
;;; ALLOWING SYMBOLICS AND LMI MACHINES TO TALK TOGETHER...


(DEFMETHOD (lispm-host :pathname-flavor) ()
  (COND ((SEND SELF :USER-GET :LMI-LAMBDA-P)
         'LMI-LAMBDA-PATHNAME)
        ((LMI-LAMBDA-NAME-P (STRING (SEND SELF :NAME)))
         'LMI-LAMBDA-PATHNAME)
        ('ELSE
         'LMFS-PATHNAME)))

(SETQ *USE-FULL-PARSER* t) ; NEEDED BECAUSE LMI-LAMBDA QFILE-ACCESS SENDS 1985 INSTEAD OF 85.

(DEFFLAVOR LMI-lambda-pathname ()
           (lmfs-pathname))

(DEFMETHOD (LMI-lambda-pathname :valid-directory-p) (dirname)
  (TYPECASE dirname
    (:string t)
    (:symbol (MEMQ dirname '(nil :wild :root)))
    (:list (AND (OR (NEQ (CAR dirname) :wild-inferiors)
                    (NOT (SEND self :root-always-explicit-p)))
                (SEND self :valid-subdirectory-p dirname 0)))))



(DEFUN-METHOD LMI-lambda-type-string LMI-lambda-pathname ()
  (string-or-wild type))


(DEFUN-METHOD LMI-lambda-version-string LMI-lambda-pathname ()
  (SELECTQ version
    (nil nil)
    (:newest ">")
    (:oldest "<")
    (:wild "*")
    (otherwise version)))


(DEFUN-METHOD LMI-lambda-directory-string LMI-lambda-pathname ()
  (COND ((LISTP directory)
         (LET ((temp-directory (REMQ :relative directory)))
           (FORMAT nil "~a~{.~a~}" (FIRST temp-directory) (CDR temp-directory))))
        (t (string-or-wild directory))))

(DEFMETHOD (LMI-lambda-pathname :character-needs-quoting-p) (ch)
  ch
  nil)



(DEFMETHOD (LMI-lambda-pathname :string-for-directory) ()
  (LET ((default-cons-area pathname-area))
    (FORMAT nil "~A;" (LMI-lambda-directory-string))))


(DEFMETHOD (LMI-lambda-pathname :string-for-dired) ()
  (LET ((nam (string-or-wild name))
        (typ (LMI-lambda-type-string))
        (ver (LMI-lambda-version-string))
        (default-cons-area pathname-area))
    (FORMAT nil "~:[~A~;~*~]~:[.~A~;~*~]~:[#~D~;~*~]"
            (NULL nam) nam (NULL typ) typ (NULL ver) ver)))


(DEFMETHOD (LMI-lambda-pathname :string-for-editor) ()
  (LET ((dir (LMI-lambda-directory-string))
        (nam (string-or-wild name))
        (typ (LMI-lambda-type-string))
        (ver (LMI-lambda-version-string))
        (default-cons-area pathname-area))
    (FORMAT nil "~:[~A~;~*~]~:[.~A~;~*~]~:[#~D~;~*~] ~:[~A;~;~*~] ~a:"
             (NULL nam) nam (NULL typ) typ (NULL ver) ver (NULL directory) dir
             (FUNCALL host :name-as-file-computer))))



(DEFMETHOD (LMI-lambda-pathname :string-for-host) ()
  (LET ((dir (IF (MEMQ directory '(:wild :root))
                 "~"
                 (LMI-lambda-directory-string)))
        (nam (string-or-wild name))
        (typ (LMI-lambda-type-string))
        (ver (LMI-lambda-version-string))
        (default-cons-area pathname-area))
    (FORMAT nil "~A:~:[~A;~;~*~]~:[~A~;~*~]~:[.~A~;~*~]~:[/#~D~;~*~]"
            (FUNCALL host :name-as-file-computer)
            (NULL directory) dir (NULL nam) nam (NULL typ) typ (NULL ver) ver)))

(DEFMETHOD (LMI-lambda-pathname :string-for-printing) ()
  (LET ((dir (LMI-lambda-directory-string))
        (nam (string-or-wild name))
        (typ (LMI-lambda-type-string))
        (ver (LMI-lambda-version-string))
        (default-cons-area pathname-area))
    (FORMAT nil "~A:~:[~A;~;~*~]~:[~A~;~*~]~:[.~A~;~*~]~:[/#~D~;~*~]"
            (FUNCALL host :name-as-file-computer)
            (NULL directory) dir (NULL nam) nam (NULL typ) typ (NULL ver) ver)))



(DEFMETHOD (LMI-lambda-pathname :parse-namestring) (IGNORE namestring &optional (start 0) end)
  (LMI-lambda-parse-namestring namestring start end))



(DEFWHOPPER (LMI-lambda-pathname :new-pathname) (&rest args)
  (LET* ((new-pathname (LEXPR-CONTINUE-WHOPPER args))
         (directory-list (SEND new-pathname :directory)))
    (WHEN (AND (LISTP directory-list)
               (MEMQ :relative directory-list))
      (SETQ new-pathname (SEND new-pathname :new-pathname :directory
                               (REMQ :relative directory-list))))
    new-pathname))

(COMPILE-FLAVOR-METHODS LMI-lambda-pathname)


(DEFUN LMI-lambda-parse-namestring (STRING &optional (start 0) end
                            &aux char state tem tem1 field-start
                            (device "dsk") directory name type version)
  (OR end (SETQ end (ARRAY-ACTIVE-LENGTH string)))
  (DO ((index start (1+ index))) (nil)
    (IF (>= index end)
        (SETQ char 'done)
        (SETQ char (AREF string index)))
    (COND ((AND (NOT (MEMQ char '(#\sp #\tab #/. #/: #/; #/# done)))
                (OR (NOT (MEMQ char '(#/< #/>)))
                    (MEMQ state '(version double-dotted))))
           (AND (%STORE-CONDITIONAL (LOCF state) nil t)
                (SETQ field-start index))
           (COND ((OR (EQ char #//) (EQ char #\circle-plus))
                  (SETQ index (1+ index))
                  (OR (< index end)
                      (LMI-lambda-char-error string 'done))
                  (SETQ char (AREF string index))
                  (AND (>= char #o200)
                       (NOT (= char #\tab))
                       (LMI-lambda-char-error string char)))))
          ((EQ char #\<)
           (COND ((NULL state))                 ;Extraneous whitespace.
                 ((EQ state t)
                  (SETQ name (LMI-lambda-field string field-start index)))
                 ((EQ state 'dotted)
                  (AND tem (SETQ name tem))
                  (SETQ type (LMI-lambda-field string field-start index)))
                 ((EQ state 'double-dotted)
                  (AND tem (SETQ name tem))
                  (AND tem1 (SETQ type tem1))
                  (SETQ version (LMI-lambda-field string field-start index t)))
                 (t (LMI-lambda-char-error string char)))
           (SETQ state 'DIRECTORY directory nil)
           (GO new-field))
          ((MEMQ char '(#\sp #\tab done))
           (COND ((NULL state))                 ;Extraneous whitespace.
                 ((EQ state t)
                  (SETQ name (LMI-lambda-field string field-start index) state nil))
                 ((EQ state 'dotted)
                  (AND tem (SETQ name tem))
                  (SETQ type (LMI-lambda-field string field-start index) state nil))
                 ((EQ state 'double-dotted)
                  (AND tem (SETQ name tem))
                  (AND tem1 (SETQ type tem1))
                  (SETQ version (LMI-lambda-field string field-start index t)
                        state nil)
                  (COND ((EQ version 0) (SETQ version :newest))
                        ((EQ version -2) (SETQ version :oldest))))
                 ((EQ state 'version)
                  (SETQ version (LMI-lambda-field string field-start index t) state nil))
                 (t (LMI-lambda-char-error string char))))
          ((EQ char #/.)
           (COND ((NULL state)                  ;Could only be :UNSPECIFIC name
                  (SETQ tem nil state 'dotted))
                 ((EQ state t)                  ;Could either be directory or name
                  (SETQ state 'dotted tem (LMI-lambda-field string field-start index)))
                 ((EQ state 'dotted)
                  (OR tem (LMI-lambda-char-error string #/.))
                  (SETQ tem1 (LMI-lambda-field string field-start index)
                        state 'double-dotted))
                 ((EQ state 'double-dotted)
                  (OR tem (LMI-lambda-char-error string #/.))
                  (SETQ state 'DIRECTORY
                        directory (LIST* tem tem1 (LMI-lambda-field string field-start index) nil)))
                 ((EQ state 'DIRECTORY)
                  (SETQ directory
                        (NCONC directory (NCONS (LMI-lambda-field string field-start index)))))
                 ((EQ state 'version)
                  (SETQ version (LMI-lambda-field string field-start index t)
                        state 'dotted))
                 (t (LMI-lambda-char-error string char)))
           (GO new-field))
          ((EQ char #/#)
           (COND ((NULL state)
                  (SETQ state 'version))
                 ((EQ state t)
                  (SETQ name (LMI-lambda-field string field-start index) state 'version))
                 ((EQ state 'dotted)
                  (AND tem (SETQ name tem))
                  (SETQ type (LMI-lambda-field string field-start index) state 'version))
                 (t (LMI-lambda-char-error string char)))
           (GO new-field))
          ((OR (EQ char #/;) (EQ char #/>))
           (COND ((EQ state t)
                  (SETQ directory (LMI-lambda-field string field-start index))
                  (IF (STRING-EQUAL directory "~")
                      (SETQ directory :root)))
                 ((EQ state 'dotted)
                  (OR tem (LMI-lambda-char-error string char))
                  (SETQ directory (LIST tem (LMI-lambda-field string field-start index))))
                 ((EQ state 'double-dotted)
                  (OR (AND tem tem1) (LMI-lambda-char-error string char))
                  (SETQ directory (LIST tem tem1 (LMI-lambda-field string field-start index))))
                 ((EQ state 'DIRECTORY)
                  (LET ((field (LMI-lambda-field string field-start index)))
                    (IF (AND (NULL directory)
                             (EQ field :unspecific))
                        (SETQ directory :root)
                        (SETQ directory
                              (NCONC directory (LIST field))))))
                 (t (LMI-lambda-char-error string char)))
           (SETQ state nil))
          ((EQ state t)
           (SETQ device ;(FUNCALL-SELF :PARSE-DEVICE-SPEC
                          ;            (LMI-LAMBDA-FIELD STRING FIELD-START INDEX))
                 :unspecific
                 state nil))
          (t (LMI-lambda-char-error string char)))
    (GO skip)
  new-field
    (SETQ field-start (1+ index))
  skip
    (AND (EQ char 'done)
         (RETURN (VALUES device directory name type version)))))

(DEFPROP LMI-lambda-char-error t :error-reporter)

(DEFUN LMI-lambda-char-error (STRING char)
  (IF (EQ char 'done)
      (FERROR :pathname-parse-error "Unexpected end of string while parsing ~s." string)
      (FERROR :pathname-parse-error
              "Unexpected character (~:C) while parsing ~s." char string)))

(DEFUN LMI-lambda-field (STRING &optional (start 0) end version-p device-p
                        &aux size arr char)
  device-p
  (OR end (SETQ end (ARRAY-ACTIVE-LENGTH string)))
  (SETQ size (- end start))
  (IF (ZEROP size)
      :unspecific
      (SETQ arr (MAKE-ARRAY size :type 'art-string))
      (DO ((i start (1+ i))
           (si 0))
          ((>= i end)
           (OR (= si size)
               (SETQ arr (ADJUST-ARRAY-SIZE arr si))))
        (COND ((NOT (MEMQ (SETQ char (AREF string i)) '(#// #\circle-plus)))
               (AND (>= char #o200)
                    (NOT (= char #\tab))
                    (LMI-lambda-char-error string char))
               (ASET (CHAR-UPCASE char) arr si)
               (SETQ si (1+ si)))))
      (COND ((STRING-EQUAL arr "*") :wild)
            ((NOT version-p) arr)
            ((numeric-p arr nil t))
            ((CDR (ASSOC arr '((">" . :newest) ("<" . :oldest)))))
            (t (FERROR :pathname-parse-error "Invalid version spec ~S in ~s" arr string)))))
