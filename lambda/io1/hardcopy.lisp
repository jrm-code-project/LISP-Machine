;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;; The Hardcopy System
;;; It used to be in QMISC, but then again, your mother used to be in QMISC.

(DEFINE-SITE-VARIABLE *DEFAULT-PRINTER* :DEFAULT-PRINTER
  "Default for :PRINTER argument to hardcopy functions.
Either a keyword for a type of printer,
or a list of such a keyword and arguments saying which printer of that type.")

(DEFINE-SITE-VARIABLE *DEFAULT-BIT-ARRAY-PRINTER* :DEFAULT-BIT-ARRAY-PRINTER
  "Default for :PRINTER argument to HARDCOPY-SCREEN-ARRAY.
Either a keyword for a type of printer,
or a list of such a keyword and arguments saying which printer of that type.")

(DEFINE-SITE-VARIABLE *PRINTER-NAMES* :PRINTER-NAMES
  "List of all printer which can be refered to by a string. This list
has the form (((\"NAME1\" \"NAME2\" ...) (:TYPE \"LOCATION\")) ...)
where NAMEn is a string which the printer is known by, and :TYPE and
LOCATION are as per the variables above")

(DEFMACRO DECODE-PRINTER-ARGUMENT (PRINTER PROP)
  `(DO-FOREVER
     (LET ((PRINTER-TYPE (IF (CONSP ,PRINTER) (CAR ,PRINTER) ,PRINTER)))
       (IF (GET PRINTER-TYPE ,PROP) (RETURN (GET PRINTER-TYPE ,PROP)))
       (SETQ ,PRINTER (CERROR T NIL NIL
                              "The printer ~S cannot do ~A" ,PRINTER ,PROP)))))

(DEFUN SET-PRINTER-DEFAULT-OPTION (PRINTER-TYPE OPTION VALUE)
  "For all printers of PRINTER-TYPE, default OPTION to VALUE"
  (SETF (GET PRINTER-TYPE OPTION) VALUE))

(DEFVAR *PRINTER-OPTIONS* '(:FONT :FONT-LIST :HEADING-FONT :PAGE-HEADINGS :VSP
                            :COPIES :SPOOL)
  "A list of keyword options that can be defaulted on a per-printer-type basis.
This is looked at by SI:GET-PRINTER-DEFAULT-OPERATIONS.")

;;; Any HARDCOPY-function that actually does printing should call this to let the
;;; have more control over the printing.
(DEFUN GET-PRINTER-DEFAULT-OPTIONS (PRINTER SUPPLIED-OPTIONS &AUX V)
  "Returns a list of default options with the SUPPLIED-OPTIONS.
A default for a printer type is set with SET-PRINTER-DEAFULT-OPTION.
Defined options to default are in the list SI:*PRINTER-OPTIONS*; your printer
type may want to PUSHNEW a symbol on this."
  (IF (CONSP PRINTER) (SETQ PRINTER (CAR PRINTER)))
  (DOLIST (O *PRINTER-OPTIONS*)
    (UNLESS ;; supplied-p
      (DO ((SUPPLIED SUPPLIED-OPTIONS (CDDR SUPPLIED)))
          ((NULL SUPPLIED) ())
        (IF (EQ O (CAR SUPPLIED)) (RETURN T)))
      (AND (SETQ V (GET PRINTER O))
           (SETQ SUPPLIED-OPTIONS (CONS O (CONS V SUPPLIED-OPTIONS))))))
  SUPPLIED-OPTIONS)

(defun expand-printer-name (printer)
  (typecase printer
    (string
     (let ((elt (cl:assoc printer *printer-names*
                          :test #'(lambda (name names)
                                    (cl:member name names :test #'string-equal)))))
       (or (second elt)
           (ferror "No such printer named ~A" printer))))
    (t printer)))

(DEFUN HARDCOPY-FILE (FILE-NAME &REST OPTIONS
                      &KEY &OPTIONAL FORMAT (PRINTER *DEFAULT-PRINTER*)
                      &ALLOW-OTHER-KEYS
                      &AUX (PATHNAME (FS:MERGE-PATHNAME-DEFAULTS FILE-NAME)))
  "Print the file FILE-NAME on a printer.
:FORMAT specifies the file format; options are :TEXT, :PRESS, :XGP or :SUDS-PLOT.
:PRINTER specifies the printer to use; default is SI:*DEFAULT-PRINTER*.
:FONT specifies the font to use; or, :FONT-LIST a list of fonts to use.
:HEADING-FONT specifies the font for page headings.
:PAGE-HEADINGS NIL inhibits generation of page headings.
:VSP is the space to leave between lines, in micas [only for printers that use press]
:COPIES specifies the number of copies to be printed.
:SPOOL T says spool the file, if the selected printer has optional spooling."
  (DECLARE (ARGLIST FILE-NAME
                    &KEY &OPTIONAL FORMAT
                    (FONT "LPT8") FONT-LIST HEADING-FONT
                    (PAGE-HEADINGS T) VSP (COPIES 1) SPOOL))
  (OR FORMAT
      (SETQ FORMAT
            (SELECTOR (SEND PATHNAME :TYPE) EQUALP
              (("PRESS" "PRE") :PRESS)
              ("XGP" :XGP)
              ("PLT" :SUDS-PLOT)
              (T :TEXT))))
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (APPLY (DECODE-PRINTER-ARGUMENT PRINTER 'PRINT-FILE)
         PRINTER FILE-NAME :FORMAT FORMAT (GET-PRINTER-DEFAULT-OPTIONS PRINTER OPTIONS)))

(DEFUN HARDCOPY-STREAM (STREAM &REST OPTIONS
                        &KEY &OPTIONAL (PRINTER *DEFAULT-PRINTER*)
                        &ALLOW-OTHER-KEYS)
  "Print the text read from STREAM on a printer.
:PRINTER specifies the printer to use; default is SI:*DEFAULT-PRINTER*.
:FILE-NAME is an arbitrary string to use as the \"filename\" on the listing.
:FONT specifies the font to use; or, :FONT-LIST a list of fonts to use.
:HEADING-FONT specifies the font for page headings.
:PAGE-HEADINGS NIL inhibits generation of page headings.
:VSP is the space to leave between lines, in micas [only for printers that use press]
:COPIES specifies the number of copies to be printed.
:SPOOL T says spool the file, if the selected printer has optional spooling."
  (DECLARE (ARGLIST STREAM
                    &KEY &OPTIONAL FILE-NAME
                    (FONT "LPT8") FONT-LIST HEADING-FONT
                    (PAGE-HEADINGS T) VSP (COPIES 1) SPOOL))
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (APPLY (DECODE-PRINTER-ARGUMENT PRINTER 'PRINT-STREAM)
         PRINTER STREAM (GET-PRINTER-DEFAULT-OPTIONS PRINTER OPTIONS)))

(DEFUN HARDCOPY-BIT-ARRAY (ARRAY LEFT TOP RIGHT BOTTOM &REST OPTIONS
                           &KEY &OPTIONAL
                           (PRINTER (OR *DEFAULT-BIT-ARRAY-PRINTER* *DEFAULT-PRINTER*))
                           &ALLOW-OTHER-KEYS)
  "Print all or part of the bits in ARRAY on the printer PRINTER.
PRINTER defaults to SI:*DEFAULT-BIT-ARRAY-PRINTER*,
 or to SI:*DEFAULT-PRINTER* if the former is NIL.
LEFT, TOP, RIGHT, BOTTOM specify the part of the array to print,
 measuring from the top left corner."
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (APPLY (DECODE-PRINTER-ARGUMENT PRINTER 'PRINT-BIT-ARRAY)
         PRINTER ARRAY LEFT TOP RIGHT BOTTOM
         (GET-PRINTER-DEFAULT-OPTIONS PRINTER OPTIONS)))

(DEFUN HARDCOPY-STATUS (&OPTIONAL (PRINTER *DEFAULT-PRINTER*)
                        (STREAM *STANDARD-OUTPUT*))
  "Print the status of printer PRINTER on STREAM."
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (FUNCALL (DECODE-PRINTER-ARGUMENT PRINTER 'PRINT-STATUS)
           PRINTER STREAM))

;;; At the moment, there's only one "LPT" per host.  Maybe the FILE protocol can
;;; accomodate us...  Also, there might be a way of simulating fonts with a DEC
;;; (or a Paper Tiger, etc).  :LPT doesn't assume anything like that for now.
(DEFUN (:PROPERTY :LPT PRINT-FILE) (PRINTER FILE-NAME
                                    &OPTIONAL &KEY (FORMAT :TEXT) &ALLOW-OTHER-KEYS)
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (IF (EQ FORMAT :TEXT)
      (WITH-OPEN-FILE-CASE (FILE-STREAM FILE-NAME :DIRECTION :INPUT)
        (FS:FILE-ERROR
         (SEND *ERROR-OUTPUT* :FRESH-LINE)
         (SEND *ERROR-OUTPUT* :STRING-OUT "LPT error in opening file: ")
         (SEND FILE-STREAM :REPORT *ERROR-OUTPUT*))
        (:NO-ERROR (LPT-PRINT-STREAM PRINTER FILE-STREAM)))
    (FORMAT *ERROR-OUTPUT* "~&LPT's only know about the TEXT format, not ~S." FORMAT)))

(DEFUN (:PROPERTY :LPT PRINT-STREAM) (PRINTER STREAM
                                      &OPTIONAL &KEY (FORMAT :TEXT) &ALLOW-OTHER-KEYS)
  (IF (STRINGP PRINTER)
      (SETQ PRINTER (EXPAND-PRINTER-NAME PRINTER)))
  (IF (EQ FORMAT :TEXT)
      (LPT-PRINT-STREAM PRINTER STREAM)
    (FORMAT *ERROR-OUTPUT* "~&LPT's only know about the TEXT format, not ~S." FORMAT)))

;;; By this point, the FORMAT had darn well better be :TEXT...
(DEFUN LPT-PRINT-STREAM (PRINTER FROM-STREAM &AUX (HOST (IF (CONSP PRINTER)
                                                            (SI:PARSE-HOST (SECOND PRINTER))
                                                          SI:ASSOCIATED-MACHINE)))
  (WITH-OPEN-FILE-CASE
    (STREAM (FS:MAKE-PATHNAME :HOST HOST
                              :DEVICE (SELECTQ (SEND HOST :SYSTEM-TYPE)
                                         (:ITS "TPL")
                                         (:UNIX :UNSPECIFIC)
                                         (OTHERWISE "LPT"))
                              :DIRECTORY (IF (EQ (SEND HOST :SYSTEM-TYPE) :UNIX) '("lpr")))
            :DIRECTION :OUTPUT)
    (FS:FILE-ERROR
     (SEND *ERROR-OUTPUT* :FRESH-LINE)
     (SEND *ERROR-OUTPUT* :STRING-OUT "LPT error in opening file: ")
     (SEND STREAM :REPORT *ERROR-OUTPUT*))
    (:NO-ERROR (STREAM-COPY-UNTIL-EOF FROM-STREAM STREAM))))
