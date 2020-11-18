;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:ZL -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;; This file provides the section specific code for ZMACS
;;; It uses the utility file stuff in ZWEI; FILES.
;;; The simple stuff is in ZWEI; ZMACS

(DEFCOM COM-EDIT-CALLERS "Edit functions that call the specified one.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (FUN CALLERS)
      (WHO-CALLS-INTERNAL "Edit")
    (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" FUN T CALLERS)))

(DEFCOM COM-LIST-CALLERS "List functions that use the specified function.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (FUN CALLERS)
      (WHO-CALLS-INTERNAL "List")
    (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" FUN NIL CALLERS))
  DIS-NONE)

(DEFCOM COM-MULTIPLE-EDIT-CALLERS "Edit functions that use the specified functions.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's.
This is the same as Edit Callers except it keeps asking for callees until
you type just a carriage return." ()
  (MULTIPLE-VALUE-BIND (FUN CALLERS)
      (MULTIPLE-WHO-CALLS-INTERNAL "Edit")
    (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" FUN T CALLERS)))

(DEFCOM COM-MULTIPLE-LIST-CALLERS "List functions that use the specified functions.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's.
This is the same as List Callers except it keeps asking for callees until
you type just a carriage return." ()
  (MULTIPLE-VALUE-BIND (FUN CALLERS)
      (MULTIPLE-WHO-CALLS-INTERNAL "List")
    (LIST-ZMACS-CALLERS-TO-BE-EDITED "Callers of" FUN NIL CALLERS))
  DIS-NONE)

;;; Get a package and the name of it to go in the prompt string,
;;; based on numeric argument.
(DEFUN GET-PACKAGE-TO-SEARCH ()
  (LET ((PKG (COND ((< *NUMERIC-ARG* 4) *PACKAGE*)
                   ((< *NUMERIC-ARG* 16.) PKG-GLOBAL-PACKAGE)
                   (T (LET ((X (TYPEIN-LINE-READLINE "Package to search (default ~A):"
                                                     *PACKAGE*)))
                        (PKG-FIND-PACKAGE (IF (EQUAL X "") *PACKAGE* (STRING-UPCASE X))))))))
    (VALUES PKG (IF (= *NUMERIC-ARG* 4) "all packages" (FORMAT NIL "package ~A" PKG)))))

(DEFUN WHO-CALLS-INTERNAL (PROMPT &OPTIONAL MUST-BE-DEFINED-FLAG)
  "Read a function name and find all functions that call it.
First value is function looked for, second value is list of callers' names."
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (LET ((FUNCTION (READ-FUNCTION-NAME
                      (FORMAT NIL "~A callers in ~A of" PROMPT PKG-NAME)
                      (RELEVANT-FUNCTION-NAME (POINT))
                      MUST-BE-DEFINED-FLAG
                      'ALWAYS-READ)))
      (FORMAT *QUERY-IO* "~&~Aing callers in ~A of ~S" PROMPT PKG-NAME FUNCTION)
      (VALUES FUNCTION
              (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-CALLERS FUNCTION PKG))))))

(DEFUN MULTIPLE-WHO-CALLS-INTERNAL (PROMPT)
  "Read several function names and find all functions that call any of them.
First value is functions looked for,
second value is list of their callers' names."
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (DO ((FUNCTIONS NIL) (FUNCTION)  (END "Stop")) (NIL)
      (SETQ FUNCTION (READ-FUNCTION-NAME
                       (FORMAT NIL "~A callers in ~A of" PROMPT PKG-NAME)
                       (IF (NULL FUNCTIONS) (RELEVANT-FUNCTION-NAME (POINT)) END)
                       NIL
                       'ALWAYS-READ))
      (COND ((NEQ FUNCTION END) (PUSH FUNCTION FUNCTIONS))
            (T (SETQ FUNCTIONS (NREVERSE FUNCTIONS))
               (RETURN (VALUES
                         FUNCTIONS
                         (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-CALLERS FUNCTIONS PKG)))))))))

(DEFUN SETUP-ZMACS-CALLERS-TO-BE-EDITED (CALLERS)
  "Sort a list of function specs by pathname of source file.
Also discard any function specs that are for :PREVIOUS-DEFINITION properties."
  (SETQ CALLERS (DEL-IF (LAMBDA (X) (AND (EQ (CAR-SAFE X) ':PROPERTY)
                                         (EQ (CADDR X) ':PREVIOUS-DEFINITION)))
                        CALLERS))
  (SORT CALLERS (LAMBDA (X Y)
                  (LET ((XNAME (CAR (SOURCE-FILE-NAMES X NIL)))
                        (YNAME (CAR (SOURCE-FILE-NAMES Y NIL))))
                    (AND (EQ XNAME YNAME) (SETQ XNAME X YNAME Y))
                    (OR (SYMBOLP XNAME) (TYPEP XNAME 'FS:PATHNAME)
                        (SETQ XNAME ""))
                    (OR (SYMBOLP YNAME) (TYPEP YNAME 'FS:PATHNAME)
                        (SETQ YNAME ""))
                    (STRING-LESSP XNAME YNAME)))))

(DEFUN LIST-ZMACS-CALLERS-TO-BE-EDITED (TYPE FUNCTION JUST-EDIT CALLERS)
  (COMMAND-STORE 'COM-GO-TO-NEXT-TOP-LEVEL-POSSIBILITY #/C-. *ZMACS-COMTAB*)
  (FUNCALL (IF JUST-EDIT
               'EDIT-FUNCTIONS-NO-DISPLAY
             'EDIT-FUNCTIONS-DISPLAY)
           ;; We want the symbols to show with their proper package prefixes
           (STABLE-SORTCAR (MAPCAR (LAMBDA (X)
                                     (CONS (FORMAT NIL "~S" X) X))
                                   CALLERS)
                           'STRING-LESSP)
           "~A~@[ ~S~]:"
           "~&No ~A~@[ ~S~] found."
           TYPE FUNCTION))

(DEFCOM COM-FUNCTION-APROPOS "List functions containing the given substring.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (MULTIPLE-VALUE-BIND (FUNCTION KEY STR)
        (GET-EXTENDED-SEARCH-STRINGS
          (FORMAT NIL "List functions in ~A containing substring:" PKG-NAME))
      (LIST-ZMACS-CALLERS-TO-BE-EDITED
        "Functions matching" STR NIL
        (SETUP-ZMACS-CALLERS-TO-BE-EDITED
          (LET ((L NIL))
            (FUNCALL (IF (EQ PKG PKG-GLOBAL-PACKAGE) #'MAPATOMS-ALL #'MAPATOMS)
                     (LAMBDA (SYM)
                       (DECLARE (SYS:DOWNWARD-FUNCTION))
                       (AND (FUNCALL FUNCTION KEY (STRING SYM))
                            (FBOUNDP SYM)
                            (PUSH SYM L)))
                     PKG)
            L)))))
  DIS-NONE)

(DEFCOM COM-LIST-MATCHING-SYMBOLS "List symbols satisfying the given predicate.
Searches the current package, or all packages with control-U, or asks for
a package with two control-U's." ()
  (MULTIPLE-VALUE-BIND (PKG PKG-NAME) (GET-PACKAGE-TO-SEARCH)
    (LET ((FUNCTION (READ-EVALUATED-MINI-BUFFER
                      "'(LAMBDA (SYMBOL) )" 18.
                      "List functions in ~A satisfying: (end with ~C)" PKG-NAME #/END))
          (SYMBOL (GENSYM)))
      (FSET SYMBOL FUNCTION)
      (COMPILE SYMBOL)
      (LET ((*PRINT-LENGTH* 3) (*PRINT-LEVEL* 3))
        (LIST-ZMACS-CALLERS-TO-BE-EDITED
          "Symbols satisfying" FUNCTION NIL
          (SETUP-ZMACS-CALLERS-TO-BE-EDITED (LIST-MATCHING-SYMBOLS SYMBOL PKG))))))
  DIS-NONE)

(DEFUN READ-EVALUATED-MINI-BUFFER (&OPTIONAL INITIAL-CONTENTS INITIAL-CHAR-POS
                                   FORMAT-STRING &REST FORMAT-ARGS &AUX INTERVAL PROMPT)
  "Read and evaluate an expression in the mini buffer, returning the value.
INITIAL-CONTENTS is a string to start off with, and INITIAL-CHAR-POS if non-NIL
 is where to put the cursor in that string.
FORMAT-STRING and FORMAT-ARGS are used for prompting."
  (SETQ PROMPT (IF (NULL FORMAT-ARGS) FORMAT-STRING
                   (APPLY #'FORMAT NIL FORMAT-STRING FORMAT-ARGS)))
  (MULTIPLE-VALUE-SETQ (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB* INITIAL-CONTENTS INITIAL-CHAR-POS
                         (AND PROMPT (NCONS PROMPT))))
  (LET ((FORM-STRING (STRING-INTERVAL INTERVAL))
        (FORM)
        (EOF '(())))
    (SETQ FORM (CL:READ-FROM-STRING FORM-STRING NIL EOF :START 0))
    (AND (EQ FORM EOF) (BARF "Unbalanced parentheses."))
    (EVAL FORM)))

(DEFUN STRING-FROM-SPEC (SPEC)
  "Return a string for how function spec SPEC would look in a definition a source file.
For example, a (:PROPERTY x y) function spec looks like (x y)."
  (COND ((STRINGP SPEC) SPEC)
        ((SYMBOLP SPEC) (GET-PNAME SPEC))
        ((ATOM SPEC) (FORMAT NIL "~S" SPEC))
        ((MEMQ (CAR SPEC) '(:PROPERTY :METHOD :MAYBE-METHOD :HANDLER))
         (DEFINITION-NAME-AS-STRING (CAR SPEC)
                                    (IF (EQ (AND (CDDDR SPEC) (THIRD SPEC)) ':WRAPPER)
                                        (LIST (SECOND SPEC) (FOURTH SPEC))
                                      (CDR SPEC))))
        ((MEMQ (CAR SPEC) '(:DEFSTRUCT :DEFSELECT))
         (DEFINITION-NAME-AS-STRING (CAR SPEC) (CADR SPEC)))
        ((EQ (CAR SPEC) ':PROPERTY)
         (DEFINITION-NAME-AS-STRING ':PROPERTY (CDR SPEC)))
        ((EQ (CAR SPEC) ':INTERNAL)
         (STRING-FROM-SPEC (CADR SPEC)))
        ;; For random function specs, use as given.
        (T (DEFINITION-NAME-AS-STRING NIL SPEC))))

(DEFUN DEFINITION-NAME-AS-STRING (TYPE SPEC)
  "Like printing SPEC into a string, but faster, with a few quirks.
The quirk is that if TYPE is non-NIL the printing is done
in the package which the car of SPEC belongs to, if that is possible."
  (LET-IF (AND TYPE (TYPEP (CAR-SAFE SPEC) '(AND SYMBOL (NOT NULL)))
               (LET ((TEM (SYMBOL-PACKAGE (CAR SPEC))))
                 (AND TEM (NEQ TEM SI:PKG-KEYWORD-PACKAGE)
                      (NEQ TEM SI:PKG-GLOBAL-PACKAGE)
                      (NEQ TEM SI:PKG-SYSTEM-PACKAGE))))
          ((*PACKAGE* (SYMBOL-PACKAGE (CAR SPEC))))
    (COND ((AND (SYMBOLP SPEC)
                (LOOP WITH PNAME = (SYMBOL-NAME SPEC)
                      FOR I FROM 0 BELOW (LENGTH PNAME)
                      AS CH = (CHAR PNAME I)
                      ALWAYS (OR (CHAR #/A CH #/Z) (EQ CH #/-))))
           (SYMBOL-NAME SPEC))
          ((AND (CONSP SPEC)
                (LOOP FOR ELT IN SPEC
                      ALWAYS
                      (AND (SYMBOLP ELT)
                           (LET ((PNAME (SYMBOL-NAME ELT)))
                             (LOOP FOR I FROM 0 BELOW (LENGTH PNAME)
                                   AS CH = (CHAR PNAME I)
                                   ALWAYS (OR (CHAR #/A CH #/Z) (EQ CH #/-)))))))
           (LET ((STRING (MAKE-STRING 40. :FILL-POINTER 0)))
             (VECTOR-PUSH #/( STRING)
             (LOOP FOR X IN SPEC
                   FOR POS FROM 0 BY 1
                   AS P = (SYMBOL-PACKAGE X)
                   AS PNAME = (SYMBOL-NAME X)
                DO (OR (ZEROP POS) (VECTOR-PUSH #/SP STRING))
                   (IF (AND (NEQ P *PACKAGE*)
                            (NOT (MEMQ P (PACKAGE-USE-LIST *PACKAGE*))))
                       (STRING-NCONC STRING
                                     (IF (EQ P SI:PKG-KEYWORD-PACKAGE)
                                         ""
                                       (SI:PKG-SHORTEST-NAME P SI:PKG-GLOBAL-PACKAGE))
                                     #/: PNAME)
                     (STRING-NCONC STRING PNAME)))
             (VECTOR-PUSH #/) STRING)
             STRING))
          (T
           ;; Not all symbols, stay on the safe side
           (PRIN1-TO-STRING SPEC)))))

(DEFUN SYMBOL-FROM-STRING (STR &OPTIONAL LINE OK-TO-ASK SYM &AUX ERROR-P error)
  "Given a string STR as found after DEF..., return the name of the object being defined.
LINE is the line that the string was found in.  It is used for
finding the particular defining construct used; this affects the result
since (DEFUN (FOO BAR) defines (:PROPERTY FOO BAR)
while (DEFMETHOD (FOO BAR) defines (:METHOD FOO BAR).
OK-TO-ASK means in certain circumstances
where things are not clear, ask the user.  Otherwise we guess.

The arg can also be an object; then its printed representation is used as the string.

The second value is a canonicalized string for the object
 (maybe the same string specified, maybe not).
The third value is T if there was a problem
 in parsing the string (such as unbalanced parens).
 In this case the fourth value may be an error object,
 in which case it is the error object indicating the read parsing problem.

You can pass the read-in form of the object as the fourth arg
if you already know it."
  (DECLARE (VALUES SYM STR ERROR-P error))
  (IF (ARRAYP STR)
      (UNLESS SYM
        (CONDITION-CASE (err)
                        (SETQ SYM (CL:READ-FROM-STRING STR))
          (SYS:PARSE-ERROR (SETQ ERROR-P T)
                           (setq error err))))
    (SETQ SYM STR
          STR (FORMAT NIL "~S" STR)))
  (COND (ERROR-P
         (VALUES NIL NIL ERROR-P error))
        ((SYMBOLP SYM)
         (VALUES SYM (SYMBOL-NAME SYM)))
        ((ATOM SYM)
         (VALUES NIL NIL T))
        (T
         ;; Here SYM is a list.  Certain types of function specs have two ways to
         ;; type them, with and without the leading type keyword.  Also certain types
         ;; of functions and other definitions do not follow the standard form
         ;; of (DEFxxx name options...).  What we do here is to recognize and
         ;; standardize those cases.  The variables are:
         ;;     TYPE - the type of function spec or non-function definition
         ;;     SYM - the function spec or definition name
         ;;     SPEC - the variant of SYM which appears in the source code
         ;;     STR - SPEC converted to a string
         ;; :HANDLER doesn't appear in source files, but gets translated into
         ;; an appropriate :METHOD here, by analyzing the combined method.
         ;; :INTERNAL doesn't appear in source files, but might be given as the argument
         ;; to M-X Disassemble.  The code here just tries not to destroy it.
         (LET ((TYPE (CAR SYM))
               DELIM-IDX SPEC)
           (IF (GET TYPE 'SI:FUNCTION-SPEC-HANDLER)
               (SETQ SPEC (CDR SYM)
                     STR (DEFINITION-NAME-AS-STRING TYPE SPEC))
             (SETQ SPEC SYM
                   DELIM-IDX (AND LINE (STRING-SEARCH-SET "( " LINE 1))
                   TYPE (let ((start-idx (definition-function-start line)))
                          (COND ((NULL LINE)
                                 :MAYBE-METHOD)
                                ((%STRING-EQUAL LINE start-idx "DEFMETHOD" 0 9.)
                                 :ALWAYS-METHOD)
                                ((%STRING-EQUAL LINE start-idx "DEFWRAPPER" 0 10.)
                                 (SETQ SPEC (LIST (CAR SPEC) :WRAPPER (SECOND SPEC)))
                                 :ALWAYS-METHOD)
                                ((%STRING-EQUAL LINE start-idx "DEFSTRUCT" 0 9.)
                                 :DEFSTRUCT)
                                ((%STRING-EQUAL LINE start-idx "DEFSELECT" 0 9.)
                                 :DEFSELECT)
                                (T :PROPERTY)))))
           (OR (SELECTQ TYPE
                 (:INSTANCE-METHOD
                  (AND (BOUNDP (CAR SPEC))
                       (SETQ SYM (FUNCALL (SI:CLASS (SYMBOL-VALUE (CAR SPEC)))
                                          :METHOD-FOR (CADR SPEC)))))
                 (:ALWAYS-METHOD
                  (SETQ SYM (CONS ':METHOD SPEC)))
                 ((:METHOD :HANDLER :MAYBE-METHOD)
                  (LET ((FLAVOR (CAR SPEC))
                        (MESSAGE (IF (CDDR SPEC) (CADDR SPEC) (CADR SPEC)))
                        FL)
                    (COND ((SETQ FL (GET FLAVOR 'SI:FLAVOR)))
                          ((AND (VALIDATE-2-LONG-LIST SPEC) (SI:CLASS-SYMBOLP FLAVOR))
                           (SETQ SYM (FUNCALL (SYMBOL-VALUE FLAVOR) :METHOD-FOR (CADR SPEC))
                                 FL T))
                          (OK-TO-ASK
                           (DOLIST (SYMBOL (PACKAGE-LOOKALIKE-SYMBOLS FLAVOR
                                              NIL '(SI:FLAVOR)))
                             (IF (FQUERY () "Do you mean ~S? "
                                         `(:METHOD ,SYMBOL . ,(CDR SPEC)))
                                 (RETURN (SETQ FLAVOR SYMBOL
                                               SPEC (CONS FLAVOR (CDR SPEC))
                                               FL (GET FLAVOR 'SI:FLAVOR)))))))
                    (COND ((SYMBOLP FL)         ;T or NIL
                           (AND (EQ TYPE ':MAYBE-METHOD)
                                (VALIDATE-2-LONG-LIST SPEC)
                                (SETQ SYM (CONS ':PROPERTY SPEC))))
                          ((FDEFINEDP `(:METHOD . ,SPEC))
                           (SETQ SYM `(:METHOD . ,SPEC)))
                          (OK-TO-ASK
                           (DOLIST (SYMBOL (OR (FIND-COMBINED-METHODS FLAVOR MESSAGE NIL)
                                               (SI:FLAVOR-ALL-INHERITABLE-METHODS
                                                 FLAVOR MESSAGE)))
                             (IF (FQUERY () "Do you mean ~S? " SYMBOL)
                                 (RETURN (SETQ SYM SYMBOL))))))))
                 ((:DEFSTRUCT :SPECIAL-FORM)
                  (SETQ SYM (CAR SPEC)
                        STR (GET-PNAME SYM)))
                 (:DEFSELECT
                  (SETQ SYM (CAR SPEC))
                  (IF (SYMBOLP SYM)
                      (SETQ STR (GET-PNAME SYM))
                      (MULTIPLE-VALUE-SETQ (SYM STR)
                        (SYMBOL-FROM-STRING SYM))))
                 (:PROPERTY
                  (AND (VALIDATE-2-LONG-LIST SPEC)
                       (SETQ SYM (CONS TYPE SPEC))))
                 (:INTERNAL (SETQ SYM (CONS TYPE SPEC))
                            (SETQ STR (DEFINITION-NAME-AS-STRING NIL (CAR SPEC)))))
               ;; Something we don't understand, make a bogus symbol to use as a property
               ;; list to remember the location of this definition
               (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))))
         (IF (NOT (SYS:VALIDATE-FUNCTION-SPEC SYM))
             (VALUES NIL NIL T)
             (VALUES SYM STR)))))

(DEFUN VALIDATE-2-LONG-LIST (L)
  "T if L is a list of exactly two symbols."
  (AND (CONSP L)
       (SYMBOLP (CAR L))
       (CONSP (CDR L))
       (SYMBOLP (CADR L))
       (NULL (CDDR L))))

(defun call-editing-type-function (mode property default &rest args)
  (let ((func (get (get mode 'editing-type) property)))
    (cond ((null func)
           (when default
             (apply default args)))
          (t
           (apply func args)))))

(defun (:property :lisp lisp-syntax-p) ()
  t)

(defun (:property :lisp mark-thing-function) ()
  'lisp-mark-thing)

(defun (:property :text mark-thing-function) ()
  'text-mark-thing)

;;; The properties this uses are defined in MODES.
;;; The mode's property is probably :LISP, :TEXT or NIL.
;;; Note that some things open-code this so they can do the GETs only once.
(DEFUN GET-SECTION-NAME (MODE LINE BP)
  "Determine whether LINE is a definition line, and if so return the name defined.
Determination is made according to MODE (which might be, eg, LISP-MODE).
BP should be a temporary BP that can be modified by this function.
The first value is the symbol or function spec defined, or NIL.
The second value is the string which appears in the line to specify that symbol or spec.
The third value is T if there is no definition on the line."
  (DECLARE (VALUES SYM STR ERROR-P))
  (FUNCALL (GET (GET MODE 'EDITING-TYPE) 'GET-SECTION-NAME)
           LINE BP))

;;; And here are the functions that actually implement GET-SECTION-NAME
;;; for various well-known modes.

(DEFVAR *SECTION-COUNT* 0)

;; Copied from LAD: RELEASE-3.ZWEI; SECTIO.LISP#283 on 26-Mar-87 18:41:32
(DEFUN (:PROPERTY :LISP GET-SECTION-NAME) (LINE BP &AUX STR SYM ERROR-P
                                           IDX start-idx END-IDX (EOF "") NON-FONT-LINE)
  (IF (NOT (AND (> (LENGTH LINE) 1) (CHAR-EQUAL (CHAR LINE 0) #/()))
      (VALUES NIL NIL T)
    (SETQ ERROR-P T)
    (WHEN (AND (setq start-idx (definition-function-start line))
               (%STRING-EQUAL LINE start-idx "DEF" 0 3)
               (NOT (%STRING-EQUAL LINE start-idx "DEFPROP " 0 8))
               (SETQ IDX (STRING-SEARCH-SET *WHITESPACE-CHARS* LINE))
               (SETQ IDX (STRING-SEARCH-NOT-SET *WHITESPACE-CHARS* LINE IDX)))
     (SETQ NON-FONT-LINE (STRING-REMOVE-FONTS LINE))
     (dotimes (i 2)
       ;;Kludge -- (defstruct (abc :opt1 xx :opt2 xxx)  parses fine
       ;;          (defstruct (abc :opt1 xx
       ;;                          :opt2 xxx)           doesn't
       ;;If we run out of line on first try and start idx is "(", try again skipping the "("
       (SETQ ERROR-P NIL)
       (CONDITION-CASE ()
          (MULTIPLE-VALUE-SETQ (SYM END-IDX)
            (CL:READ-FROM-STRING NON-FONT-LINE NIL EOF :START IDX))
        (:NO-ERROR
         (cond ((EQ SYM EOF)
                (SETQ ERROR-P T)
                (if (eq (char non-font-line idx) #/()
                    (incf idx)
                  (return)))
               (t
                (SETQ STR (SUBSTRING NON-FONT-LINE IDX (MIN (LENGTH LINE) END-IDX))))))
        (SYS:PARSE-ERROR
         (SETQ STR (GET-DEFUN-NAME (MOVE-BP BP LINE 0)))))
       (UNLESS ERROR-P
         (MULTIPLE-VALUE-SETQ (SYM NIL ERROR-P)
           (SYMBOL-FROM-STRING STR NON-FONT-LINE NIL SYM))
         ;;No error, all is cool
         (return))))
    (WHEN ERROR-P
      (SETQ SYM (CONCATENATE 'STRING
                             (LET ((BUFFER (NODE-TOP-LEVEL-NODE (LINE-NODE LINE))))
                               (IF (safe-get-zwei-buffer-instance-variable BUFFER 'PATHNAME)
                                   (LET ((NAME
                                           (PATHNAME-NAME (BUFFER-PATHNAME BUFFER))))
                                     (IF (CONSP NAME)
                                         (APPLY #'STRING-APPEND
                                                (MAPCAR (LAMBDA (NAME-ELT)
                                                          (IF (CONSP NAME-ELT)
                                                              (CAR NAME-ELT) NAME-ELT))
                                                        NAME))
                                       (STRING NAME)))
                                 (or (safe-get-zwei-buffer-instance-variable BUFFER 'name)
                                     "SECTION")))
                             "-"
                             (LET ((START-INDEX (STRING-SEARCH-NOT-CHAR #/( LINE)))
                               (SUBSTRING LINE START-INDEX
                                          (AND START-INDEX
                                               (STRING-SEARCH-SET *WHITESPACE-CHARS*
                                                                  LINE START-INDEX))))
                             "-"
                             (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))
            STR SYM))
    (VALUES SYM STR NIL)))

(defun definition-function-start (line &aux idx end-idx)
  (when line
    (if (not (char-equal (char line 0) #/())
        0
      (setq end-idx (string-search-set *whitespace-chars* line 1))
      (if (setq idx (string-search-char #/: line 1 end-idx))
          (do ()
              ((char-not-equal (char line idx) #/:) idx)
            (incf idx))
        1))))

(defun (:property :scheme lisp-syntax-p) ()
  t)

(defun (:property :scheme mark-thing-function) ()
  'lisp-mark-thing)

(DEFUN (:PROPERTY :scheme GET-SECTION-NAME) (buffer-LINE BP)
  bp
  (block nil
    (labels ((generate-name-and-return ()
               (let ((str (CONCATENATE 'STRING
                                       (LET ((BUFFER (NODE-TOP-LEVEL-NODE (LINE-NODE buffer-LINE))))
                                         (IF (BUFFER-PATHNAME BUFFER)
                                             (LET ((NAME
                                                     (PATHNAME-NAME (BUFFER-PATHNAME BUFFER))))
                                               (IF (CONSP NAME)
                                                   (APPLY #'STRING-APPEND
                                                          (MAPCAR (LAMBDA (NAME-ELT)
                                                                    (IF (CONSP NAME-ELT)
                                                                        (CAR NAME-ELT) NAME-ELT))
                                                                  NAME))
                                                 (STRING NAME)))
                                           (BUFFER-NAME BUFFER)))
                                       "-"
                                       (LET ((START-INDEX (STRING-SEARCH-NOT-CHAR #/( buffer-LINE)))
                                         (SUBSTRING buffer-LINE START-INDEX
                                                    (AND START-INDEX
                                                         (STRING-SEARCH-SET *WHITESPACE-CHARS*
                                                                            buffer-LINE START-INDEX))))
                                       "-"
                                       (PRIN1-TO-STRING (INCF *SECTION-COUNT*)))))
                 (return (values str str nil)))))
      (let ((line (STRING-REMOVE-FONTS buffer-LINE)))
        (cond ((<= (length line) 1)
               (values nil nil t))
              ((not (char-equal (char line 0) #/( ))
               (values nil nil t))
              ((not (string-equal line "(DEFINE" :end1 7))
               (generate-name-and-return))
              (t
               (let (index end-index sym)
                 (setq index (string-search-set '(#/space #/tab) line))
                 (setq index (string-search-not-set '(#/space #/tab #/( ) line (or index 0)))
                 (when (null index)
                   (generate-name-and-return))
                 (condition-case ()
                     (multiple-value (sym end-index)
                       (cl:read-from-string line t nil :start index))
                   (si:parse-error
                    (generate-name-and-return)))
                 (values sym
                         (substring line
                                    index
                                    (max 0 (min (1- end-index) (string-length line))))
                         nil))))))))

(DEFUN GET-DEFUN-NAME (BP &AUX BP1)
  "Return the function spec defined by the defun starting at BP."
  (AND (SETQ BP (FORWARD-ATOM BP))
       (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* BP))
       (SETQ BP1 (FORWARD-SEXP BP))
       (STRING-REMOVE-FONTS (STRING-INTERVAL BP BP1))))

(DEFUN (:TEXT GET-SECTION-NAME) (LINE BP &AUX STR SYM ERROR-P)
  (IF (AND (%STRING-EQUAL LINE 0 ".DEF" 0 4)
           (SETQ STR (GET-TEXT-DEFUN-NAME (MOVE-BP BP LINE 0))))
      (SETQ SYM (INTERN STR *UTILITY-PACKAGE*))
    (SETQ ERROR-P T))
  (VALUES SYM STR ERROR-P))

(DEFUN GET-TEXT-DEFUN-NAME (BP &AUX BP1)
  (ATOM-WORD-SYNTAX-BIND
    ;; Now get the second word after BP.
    (AND (SETQ BP (FORWARD-WORD BP))
         (SETQ BP (FORWARD-OVER *BLANKS* BP))
         (SETQ BP1 (FORWARD-WORD BP))
         (STRING-REMOVE-FONTS (STRING-INTERVAL BP BP1)))))


; c functions look like
;    foo ()
;    {
;         ...
;    }
;
;    /*
;     * this is the bar function
;     */
;
; => struct xyz *
;    bar (a, b, c)
;    {
;         ...
;    }
; =>
;    /*
;     * last function
;     */
; -> lastfunc ()
;    {
;       ...
;    }
; ->

; the { may be on the same line as the function name
;
; we want the section to cover the marked region


           ;;this line contains an open paren.
           ;;first of all, it must start with a letter
           ;;if the previous line has a letter in column 0,
           ;;then it should be the beginning of the section

           ;;this line doesn't contain an open paren
           ;;this might be a type declaration, and the next line has
           ;;the function name, and arg list

(defun (:property :c get-section-name) (line bp)
  (declare (values section-name string error-p))
  (block nil
    (cond ((string-search-char #/( line)
           ;;this is a line containing an open paren
           (cond ((not (eq (word-syntax (aref line 0)) word-alphabetic))
                  ;;the first letter must be a letter
                  (return (VALUES nil nil t)))
                 ((null (line-previous line))
                  ;;it's ok if there are no preceeding lines
                  )
                 ((zerop (string-length (line-previous line)))
                  ;;also OK if the preceeding line is blank
                  )
                 ((eq (word-syntax (aref (line-previous line) 0)) word-alphabetic)
                  ;;but if it has anything on it, it better not be a letter in the first column
                  (return (VALUES nil nil t)))
                 ))
          (t
           ;;this line doesn't have an open paren in column 0
           ;;that's ok if the next line has the function name itself
           (cond ((zerop (string-length line))
                  (return (VALUES nil nil t)))
                 ((not (eq (word-syntax (aref line 0)) word-alphabetic))
                  ;;this line has to have a letter in column 0
                  (return (VALUES nil nil t)))
                 )
           ;;the previous line better not have a letter in column 0
           (cond ((null (line-previous line))
                  ;;OK if no previous
                  )
                 ((zerop (string-length (line-previous line)))
                  ;;ok if blank
                  )
                 ((eq (word-syntax (aref (line-previous line) 0)) word-alphabetic)
                  ;;previous line starts in column 0, so this isn't start of section
                  (return (VALUES nil nil t)))
                 )
           ;;the next line must have the function on it
           (cond ((null (line-next line))
                  ;;must have another line
                  (return (VALUES nil nil t)))
                 ((zerop (string-length (line-next line)))
                  ;;and it has to have stuff on it
                  (return (VALUES nil nil t)))
                 ((not (eq (word-syntax (aref (line-next line) 0)) word-alphabetic))
                  ;;must start with letter
                  (return (VALUES nil nil t)))
                 ((not (string-search-char #/( (line-next line)))
                  ;;and must have an arglist
                  (return (VALUES nil nil t)))
                 (t
                  (setq line (line-next line))))))

    (let ((func (get-c-function-name (move-bp bp line 0))))
      (cond ((null func)
             (return (VALUES nil nil t)))
            (t
             (return (VALUES (intern func *utility-package*) func nil)))))))

;bp points to beginning of a line like:
;
;foo (a, b, c)
;
(defun get-c-function-name (bp &aux bp1)
  (and (setq bp1 (forward-word bp))
       (string-remove-fonts (string-interval bp bp1))))

(DEFUN (:property NIL GET-SECTION-NAME) (LINE BP)
  LINE BP
  (VALUES NIL NIL T))

;;; The SECTION-P is a function which takes a line as arg
;;; and returns T if the line looks likely to be the start of a section.
;;; You could use the GET-SECTION-NAME function, but that will not be
;;; guaranteed to be right if the following line(s) are not there,
;;; and if it is wrong it will return "no section".
;;; These functions err by returning T if in doubt;
;;; then you can use the GET-SECTION-NAME function after you prepare for it.
(DEFUN (:PROPERTY :LISP SECTION-P) (LINE)
  (AND (PLUSP (LENGTH LINE))
       (CHAR-EQUAL (CHAR LINE 0) #/()))

(DEFUN (:PROPERTY :scheme SECTION-P) (LINE)
  (AND (PLUSP (LENGTH LINE))
       (CHAR-EQUAL (CHAR LINE 0) #/()))

(DEFUN (:PROPERTY :TEXT SECTION-P) (LINE)
  (%STRING-EQUAL LINE 0 ".DEF" 0 4))

(defun (:property :c section-p) (line)
  (block nil
    (cond ((zerop (string-length line))
           ;;not blank lines
           nil)
          ((not (eq (word-syntax (aref line 0)) word-alphabetic))
           ;;must start with letter
           nil)
          ((null (line-previous line))
           ;;ok if no prev line
           t)
          ((zerop (string-length (line-previous line)))
           ;;ok if prev line blank
           t)
          ((eq (word-syntax (aref (line-previous line) 0)) word-alphabetic)
           ;;not ok if prev line starts with letter
           nil)
          (t
           ;;otherwise - maybe ok
           t))))

(DEFUN (:PROPERTY NIL SECTION-P) (IGNORE)
  NIL)

(DEFUN DEFINITION-LIKELY-POSITION (BP1 &OPTIONAL BP2 IN-ORDER-P SYMBOL
                                       &AUX DEFUN-LINE NON-DEFUN-LINE COMMENT-LINE BUFFER)
  "Return a BP within specified interval to a line that might relate to defining SYMBOL.
SYMBOL is actually a function spec or a name being defined in any way.
The line is found by a textual search for SYMBOL.
However, lines starting with open parens get first priority.
Comment lines have lower priority than non-comment lines.
If nothing is found, the beginning of the interval is returned."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (FROM-INDEX (BP-INDEX BP1) 0)
       (END-LINE (BP-LINE BP2))
       (KEY (STRING-FROM-SPEC SYMBOL))
       (INDEX) (TEM))
      (NIL)
    (AND (SETQ INDEX (STRING-SEARCH KEY LINE FROM-INDEX
                                    (AND (EQ LINE END-LINE) (BP-INDEX BP2))))
         (COND ((CHAR-EQUAL #/( (CHAR LINE 0))
                (SETQ DEFUN-LINE LINE)
                (RETURN T))
               ((AND (NULL COMMENT-LINE) (SETQ TEM (FIND-COMMENT-START LINE)) (< TEM INDEX))
                (SETQ COMMENT-LINE LINE))
               ((NULL NON-DEFUN-LINE)
                (SETQ NON-DEFUN-LINE LINE))))
    (AND (EQ LINE END-LINE)
         (RETURN NIL)))
  (SETQ BUFFER (BP-TOP-LEVEL-NODE BP1))
  (VALUES (CREATE-BP (OR DEFUN-LINE NON-DEFUN-LINE COMMENT-LINE (BP-LINE BP1))
                     0)
          BUFFER))

(DEFUN PACKAGE-LOOKALIKE-SYMBOLS (PNAME
                                  &OPTIONAL IGNORE
                                            (PROPERTIES '(:SOURCE-FILE-NAME ZMACS-BUFFERS))
                                            &AUX LIST)
  "Return a list of symbols with pname PNAME in various packages.
Only symbols which possess one of the properties in PROPERTIES are considered."
  (DOLIST (PKG *ALL-PACKAGES*)
    (SETQ LIST (PACKAGE-LOOKALIKE-SYMBOLS-1 (STRING PNAME) PKG LIST PROPERTIES)))
  LIST)

(DEFUN PACKAGE-LOOKALIKE-SYMBOLS-1 (PNAME PKG LIST PROPERTIES &AUX TEM)
  (AND (SETQ TEM (INTERN-LOCAL-SOFT PNAME PKG))
       (NOT (MEMQ TEM LIST))
       ;; Used to be GETL, but that was fooled by a property of NIL
       (LOOP FOR PROP IN PROPERTIES
             THEREIS (GET TEM PROP))
       (PUSH TEM LIST))
  LIST)

(defun definition-text-location-1 (spec)
  "Return a BP to SPEC's definition in ZMACS, if it has a unique definition.
If SPEC has no definition or more than one, return NIL.
Will read in a source file if there is a unique source file.
Does not select the buffer, and can be called from outside the editor.
If there is more than one definition, the user should be advised to
enter ZMACS, give C-M-X Make Correspondence on the definition he prefers,
and retry your operation."
  (declare (return-list bp buffer))
  (let ((chosen-def (si:function-spec-get spec 'zmacs-chosen-definition))
        (zbp (si:function-spec-get spec 'zmacs-buffers)))
    (cond (chosen-def
           (values (create-bp (cdr chosen-def) 0) (car chosen-def)))
          (zbp
           (and (null (cdr zbp))
                (not (buffer-is-not-only-source-file-p (caar zbp) spec))
                (if (definition-still-real-p-new (caar zbp) (cdar zbp) spec)
                    (values (create-bp (cdar zbp) 0) (caar zbp))
                  (let ((*package* *package*))
                    (compute-buffer-package (caar zbp))
                    (sectionize-buffer (caar zbp))
                    (definition-text-location-1 spec)))))
          (t
           (let ((sources (subset-not #'find-buffer-named (source-file-names spec))))
             (if (and sources (null (cdr sources)))
                 (progn (find-file (car sources))
                        (definition-text-location-1 spec))))))))

;;;; Tag table stuff

(DEFCOM COM-VISIT-TAG-TABLE "Read in the specified tag table file.
Go through the tag table, and mark the name of each tag as being
a possible section of its file.  Later, the Edit Definition command
will see these marks and figure out which file to use.
Get the name of the file from the mini-buffer." ()
  (READ-TAG-TABLE (READ-DEFAULTED-PATHNAME "Tag Table:" (PATHNAME-DEFAULTS) "TAGS"))
  DIS-NONE)

(DEFUN READ-TAG-TABLE (FILE &AUX (ADDED-COMPLETIONS (MAKE-ARRAY 500. :TYPE 'ART-Q-LIST
                                                                     :LEADER-LENGTH 2)))
  "Read in tag table file named FILE, recording source files of functions in it."
  (STORE-ARRAY-LEADER 0 ADDED-COMPLETIONS 0)
  (WITH-OPEN-FILE (STREAM FILE :DIRECTION :INPUT :SUPER-IMAGE T :CHARACTERS T)
    (DO ((FILE-LIST) (PATHNAME) (MODE))
        (NIL)
      (MULTIPLE-VALUE-BIND (LINE EOF)
          (SEND STREAM :LINE-IN)
        (COND (EOF
               (SEND FILE :PUTPROP (NREVERSE FILE-LIST) 'ZMACS-TAG-TABLE-FILE-SYMBOLS)
               (OR (RASSQ FILE *ZMACS-TAG-TABLE-ALIST*)
                   (PUSH (CONS (STRING FILE) FILE) *ZMACS-TAG-TABLE-ALIST*))
               (RETURN)))
        (SETQ PATHNAME (FS:MERGE-PATHNAME-DEFAULTS LINE *PATHNAME-DEFAULTS*))
        (PUSH PATHNAME FILE-LIST)
        (SETQ LINE (SEND STREAM :LINE-IN))      ;Length,Mode
        (SETQ MODE (GET-FILE-MAJOR-MODE
                     (INTERN (SUBSTRING LINE (1+ (STRING-SEARCH-CHAR #/, LINE))) "USER")))
        (DO ((*PACKAGE* (PKG-FIND-PACKAGE (OR (SEND (SEND PATHNAME :GENERIC-PATHNAME)
                                                    :GET ':PACKAGE)
                                              *PACKAGE*)))
             (SPACE-POS) (RUBOUT-POS)
             (STR) (SNAME))
            ((EQ (CHAR (SETQ LINE (SEND STREAM :LINE-IN)) 0) #/))
          (COND ((SETQ SPACE-POS (STRING-SEARCH-SET '(#/SP #/TAB) LINE))
                 (SETQ SPACE-POS (1+ SPACE-POS)
                       RUBOUT-POS (COND ((STRING-SEARCH-CHAR (int-char #o177) LINE SPACE-POS))
                                        (T (SEND STREAM :LINE-IN)
                                           (1+ (STRING-LENGTH LINE))))
                       STR (SUBSTRING LINE SPACE-POS (1- RUBOUT-POS)))
                 (COND ((CASE (GET MODE 'EDITING-TYPE)
                          (:LISP
                           (AND (%STRING-EQUAL LINE 0 "(DEF" 0 4)
                                (NOT (%STRING-EQUAL LINE 0 "(DEFPROP " 0 9))
                                (SETQ SNAME (SYMBOL-FROM-STRING STR LINE))))
                          (:TEXT
                           (AND (%STRING-EQUAL LINE 0 ".DEF" 0 4)
                                (SETQ SNAME (INTERN STR *UTILITY-PACKAGE*))))
                          (OTHERWISE NIL))
                        (SECTION-COMPLETION SNAME STR ADDED-COMPLETIONS 1000)
                        (SECTION-COMPLETION SNAME (DEFINITION-NAME-AS-STRING NIL SNAME)
                                            ADDED-COMPLETIONS 1000)
                        (OR (GET SNAME ':SOURCE-FILE-NAME)
                            (SETF (GET SNAME ':SOURCE-FILE-NAME) PATHNAME))
                        (PUSH* PATHNAME (GET SNAME 'ZMACS-TAG-FILE-SYMBOLS))))))))))
  (SORT-COMPLETION-AARRAY ADDED-COMPLETIONS)
  (MERGE-COMPLETION-AARRAY *ZMACS-COMPLETION-AARRAY* ADDED-COMPLETIONS))

(DEFCOM COM-LIST-TAG-TABLES "List the names of tag table files (buffers)." ()
  (let ((output nil))
    (DOLIST (TAG-TABLE *ZMACS-TAG-TABLE-ALIST*)
      (setq output *standard-output*)
      (FORMAT output "~&~4TFiles in tag table ~A~@[ (current tag table)~]:~%"
              (CAR TAG-TABLE)
              (eq (cdr tag-table) *zmacs-current-tag-table*))
      (SEND output :ITEM-LIST 'FILE
            (SEND (CDR TAG-TABLE) :GET 'ZMACS-TAG-TABLE-FILE-SYMBOLS))
      (format output "~%"))
    (if output
        (format output "~&Done.~%")
      (format *query-io* "~&No tag table files read in.")))
  DIS-NONE)

(defun zmacs-current-tag-table ()
  (or *zmacs-current-tag-table*
      (cdar *zmacs-tag-table-alist*)))

(defcom com-current-tag-table "Display the name of the current tag table." ()
  (if (setq *zmacs-current-tag-table* (zmacs-current-tag-table))
      (format *query-io* "~A" (send *zmacs-current-tag-table* :name))
    (format *query-io* "No current tag table"))
  dis-none)

(DEFCOM COM-NEXT-FILE "Move to the next file in the tags table.
With numeric argument, start at the beginning of the tags table
/(move to first file in it)." ()
  (NEXT-FILE *NUMERIC-ARG-P*)
  DIS-TEXT)

(DEFVAR *ZMACS-LAST-TAGS-FILE-LIST* NIL
  "List of pathnames of files for NEXT-FILE to go through.")

(DEFUN NEXT-FILE (RESTART &AUX PATHNAME BUFFER)
  "Select the next file in the selected tag table.
RESTART non-NIL means select the first file in the tag table
and reset the list of files to be gone through."
  (AND RESTART
       (SETQ *ZMACS-LAST-TAGS-FILE-LIST* (SEND (SELECT-TAG-TABLE) :GET
                                               'ZMACS-TAG-TABLE-FILE-SYMBOLS)))
  (OR *ZMACS-LAST-TAGS-FILE-LIST* (BARF "No more files"))
  (POP *ZMACS-LAST-TAGS-FILE-LIST* PATHNAME)
  (COND ((SETQ BUFFER (FIND-FILE-BUFFER PATHNAME))
         (FORMAT *QUERY-IO* "~&~A~%" PATHNAME)
         (MAKE-BUFFER-CURRENT BUFFER)
         (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))
        (T
         (FIND-FILE PATHNAME))))

(defun set-tag-table-pathname-and-bp (table pathname bp)
  (when table
    (send table :putprop pathname 'current-pathname)
    (let ((old-bp (send table :get 'current-point)))
      (cond ((and old-bp bp)
             (move-bp old-bp bp))
            ((and old-bp (null bp))
             (flush-bp old-bp)
             (send table :remprop 'current-point))
            (bp
             (send table :putprop (copy-bp bp :normal) 'current-point))))))

(DEFUN NEXT-FILE-BP (RESTART &AUX PATHNAME BUFFER)
  "Return BP to start of the next file in the selected tag table.
RESTART non-NIL means start again at first file in tag table."
  (AND RESTART
       (SETQ *ZMACS-LAST-TAGS-FILE-LIST* (SEND (SELECT-TAG-TABLE) :GET
                                               'ZMACS-TAG-TABLE-FILE-SYMBOLS)))
  (unless *ZMACS-LAST-TAGS-FILE-LIST*
    (set-tag-table-pathname-and-bp *zmacs-current-tag-table* nil nil)
    (BARF "No more files"))
  (POP *ZMACS-LAST-TAGS-FILE-LIST* PATHNAME)
  (let ((bp (COND ((SETQ BUFFER (FIND-FILE-BUFFER PATHNAME))
                   (FORMAT *QUERY-IO* "~&~A~%" PATHNAME)
                   (INTERVAL-FIRST-BP BUFFER))
                  (T
                   (INTERVAL-FIRST-BP (FIND-FILE PATHNAME NIL))))))
    (set-tag-table-pathname-and-bp *zmacs-current-tag-table* (bp-top-level-node bp) bp)
    bp))

(DEFUN TAG-TABLE-BUFFERS (READ-IN-ALL-FILES &optional (default-p t)
                          &AUX BUFFER-LIST FILE-LIST tag-table)
  "Return a list of all buffers in the selected tag table.
Also return the tag table that was selected.
READ-IN-ALL-FILES means visit all the files;
otherwise, we return only the buffers for files already read in.
When DEFAULT-P is NIL, offer to select a tag table."
  (declare (values buffer-list tag-table))
  (SETQ FILE-LIST (SEND (or (setq tag-table (SELECT-TAG-TABLE default-p))
                            (return-from tag-table-buffers))
                        :GET 'ZMACS-TAG-TABLE-FILE-SYMBOLS))
  (DOLIST (FILE FILE-LIST)
    (LET ((BUFFER (FIND-FILE-BUFFER FILE)))
      (IF BUFFER (PUSH BUFFER BUFFER-LIST))
      (IF READ-IN-ALL-FILES (PUSH (FIND-FILE FILE NIL) BUFFER-LIST))))
  (values (NREVERSE BUFFER-LIST) tag-table))

(defcom com-list-tag-buffers "Display buffers and search context associated with a tag table.
  If there is numeric (or universal) arg, use current tag table.
  Else, ask which tag table to display buffers for.
  Displays item-list of buffer names you can select." ()
  (let ((output *query-io*)
        tag-table tag-table-name
        buffers current-buffer
        search-string search-key-p)
    (multiple-value (buffers tag-table) (tag-table-buffers nil *numeric-arg-p*))
    (if (null buffers) (BARF "No buffers."))
    (setq output *standard-output*)
    (setq tag-table-name (or (car (rassq tag-table *zmacs-tag-table-alist*))
                             (format nil "~S" tag-table)))
    ;;This is the buffer, if any, in path of current search
    (setq current-buffer (send tag-table :get 'current-pathname))
    ;;We may have a search string associated with the tag table.
    (setq search-string  (zmacs-tags-search-key-string))
    ;;We <may> also have a search context, i.e. we're not at end of buffers with search
    (setq search-key-p   (and current-buffer search-string))
    (format output "~&Tag table~@[ ~A~]~@[ ~*(the current tag table)~]~@[ - ~*search context is established.~]~2&"
            tag-table-name
            (eq tag-table *zmacs-current-tag-table*)
            search-key-p)
    (format output "~&Buffers read in:")
    (let* ((current-buffer-mark-char #/+)
           (max-size (- (send output :size-in-characters) 6.))
           (max-buffer-string-len (- max-size 6))
           (max-buffer-name-len (find-maximum-buffer-name-length max-buffer-string-len buffers)))
      (format output "~%   ~a" (make-string max-buffer-name-len :initial-element #\-))
      (when search-key-p
        (format output "  Search string:"))
      (dolist (buffer buffers)
        (let*((currentp  (eq buffer current-buffer))
              (buffer-mark-char (if currentp current-buffer-mark-char #/space))
              (buffer-string (name-for-display buffer max-buffer-name-len)))
          (format output "~& ~c " buffer-mark-char)
          (send output :item 'zmacs-buffer buffer "~A" buffer-string)
          (when (and search-key-p currentp)
            (let*((extra (+ 2 (- max-buffer-name-len (string-length buffer-string))))
                  (leftover (- max-size max-buffer-name-len extra))
                  (search-string (and (plusp leftover)
                                      (zmacs-tags-search-key-string-for-display leftover))))
              (when search-string (format output "~V,@T{~A}" extra search-string))))
          (terpri output)))
      (terpri output)
      (when current-buffer
        (format output "~& ~c  means current search buffer." current-buffer-mark-char))
      (cond
        (search-key-p (format output "~&{ } shows current search string."))
        (search-string (format output "~&{~A} was last search string." search-string)))
      (terpri output)))
  dis-none)

;;;Tag Searching
;;
;;;These vars control the search context for tag search.  In a simple
;;;search situation, both are strings (and they really <ought> to be the
;;;same).  If search expressions are employed, the search key may be a
;;;list of strings and search information, and then the search key string
;;is a concatenation of the search strings with separators.

(DEFVAR *ZMACS-TAGS-SEARCH-KEY-STRING* "FOO")
(DEFVAR *ZMACS-TAGS-SEARCH-KEY*)
(DEFVAR *ZMACS-TAGS-SEARCH-FUNCTION*)

(defun zmacs-tags-search-key-string (&optional firstonly)
  ;;Because the search key can be a list, use this to snarf out
  ;;the string or just the first part of it.
  (if firstonly
      (or (car-safe *zmacs-tags-search-key*) *zmacs-tags-search-key-string*)
    (or *zmacs-tags-search-key-string*
        (car-safe *zmacs-tags-search-key*)
        *zmacs-tags-search-key*)))

(defun zmacs-tags-search-key-string-for-display (max-size &optional firstonly)
  (declare (values search-key substring))
  (let ((search-key (zmacs-tags-search-key-string firstonly)))
    (if (<= (string-length search-key) max-size)
        (values search-key nil)
      (values (string-append (substring search-key 0 (max (- max-size 2) 0)) " ") t))))

(DEFCOM COM-TAGS-SEARCH "Search for the specified string within files of the tags table.
  By default, start the search from the beginning of the tags table files sequence.
  With a numeric argument (other than 1), start this search from the current search point. " ()
  (LET ((*MINI-BUFFER-DEFAULT-STRING* *ZMACS-TAGS-SEARCH-KEY-STRING*))
    (MULTIPLE-VALUE-SETQ (*ZMACS-TAGS-SEARCH-FUNCTION* *ZMACS-TAGS-SEARCH-KEY*)
      (read-function-name-or-string  NIL "Tags search:" *SEARCH-MINI-BUFFER-COMTAB*)))
  (SETQ *ZMACS-TAGS-SEARCH-KEY-STRING*
        (STRING-INTERVAL (WINDOW-INTERVAL (GET-SEARCH-MINI-BUFFER-WINDOW))))
  (COMMAND-STORE 'COM-TAGS-SEARCH-NEXT-OCCURRENCE #/c-. *ZMACS-COMTAB*)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (TAGS-SEARCH-NEXT-OCCURRENCE (equal *numeric-arg* 1)))

(DEFUN TAGS-SEARCH-ALTERNATIVE-STRINGS (&REST STRINGS)
  "Begin a tags search looking for any of STRINGS."
  (IF (= (LENGTH STRINGS) 1)
      (SETQ *ZMACS-TAGS-SEARCH-KEY-STRING* (CAR STRINGS)
            *ZMACS-TAGS-SEARCH-KEY* (CAR STRINGS)
            *ZMACS-TAGS-SEARCH-FUNCTION* 'ZWEI-SEARCH)
    (SETQ *ZMACS-TAGS-SEARCH-KEY* (COPYLIST STRINGS))
    (SETQ *ZMACS-TAGS-SEARCH-FUNCTION* 'FSM-SEARCH)
    (SETQ *ZMACS-TAGS-SEARCH-KEY-STRING*
          (MAKE-ARRAY #o20 :TYPE ART-FAT-STRING :FILL-POINTER 0))
    (DO ((STRINGS STRINGS (CDR STRINGS)))
        ((NULL STRINGS))
      (STRING-NCONC *ZMACS-TAGS-SEARCH-KEY-STRING*
                    (CAR STRINGS))
      (OR (NULL (CDR STRINGS))
          (STRING-NCONC *ZMACS-TAGS-SEARCH-KEY-STRING*
                        #o402))))
  (COMMAND-STORE 'COM-TAGS-SEARCH-NEXT-OCCURRENCE #/c-. *ZMACS-COMTAB*)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (TAGS-SEARCH-NEXT-OCCURRENCE T))

(DEFCOM COM-TAGS-SEARCH-NEXT-OCCURRENCE "Search for the next occurrence of search string." ()
  (TAGS-SEARCH-NEXT-OCCURRENCE NIL))

(DEFUN TAGS-SEARCH-NEXT-OCCURRENCE (RESTART)
  (DO ((BP)
       (PT (IF RESTART (NEXT-FILE-BP T) (POINT))))
      (NIL)
    (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE PT)))
      (SETQ BP (FUNCALL *ZMACS-TAGS-SEARCH-FUNCTION* PT *ZMACS-TAGS-SEARCH-KEY*)))
    (COND (BP
           (POINT-PDL-PUSH (POINT) *WINDOW*)
           (MAKE-BUFFER-CURRENT (BP-TOP-LEVEL-NODE BP))
           (MOVE-BP (POINT) BP)
           (set-tag-table-pathname-and-bp *zmacs-current-tag-table* (bp-top-level-node bp) bp)
           (RETURN DIS-TEXT))
          (T
           (SETQ PT (NEXT-FILE-BP NIL))
           (MUST-REDISPLAY *WINDOW* DIS-TEXT)))))

(DEFVAR *TAGS-QUERY-REPLACE-FROM*)
(DEFVAR *TAGS-QUERY-REPLACE-TO*)
(DEFVAR *TAGS-QUERY-REPLACE-DELIMITED*)

(DEFPROP COM-TAGS-QUERY-REPLACE KIND-OF-QUERY-REPLACE-DOCUMENTATION DOCUMENTATION-FUNCTION)
(DEFPROP COM-TAGS-QUERY-REPLACE T CONTROL-PERIOD)

(DEFCOM COM-TAGS-QUERY-REPLACE "Perform a Query Replace within the tags table files.
  Does Query Replace over all the files in the selected tags table, one file at a time." ()
  (MULTIPLE-VALUE-SETQ (*TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*)
    (QUERY-REPLACE-STRINGS NIL))
  (SETQ *TAGS-QUERY-REPLACE-DELIMITED* (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))
  (COMMAND-STORE 'COM-CONTINUE-TAGS-QUERY-REPLACE #/c-. *ZMACS-COMTAB*)
  (CONTINUE-TAGS-QUERY-REPLACE T))

(DEFCOM COM-CONTINUE-TAGS-QUERY-REPLACE "Continue the last Tags Query Replace." ()
  (CONTINUE-TAGS-QUERY-REPLACE NIL))

(DEFUN CONTINUE-TAGS-QUERY-REPLACE (RESTART)
  (DO ((BEGINNING RESTART)
       (VAL))
      (NIL)
    ;; Find the next buffer in the list which has an occurrence, select it.
    (DO (BP FOUND-AT) (())
      (SETQ BP (NEXT-FILE-BP BEGINNING))
      (SETQ BEGINNING NIL)
      (WHEN (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
              (SETQ FOUND-AT (ZWEI-SEARCH BP *TAGS-QUERY-REPLACE-FROM*)))
        (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
        (MAKE-BUFFER-CURRENT (BP-TOP-LEVEL-NODE BP))
        ;; Move bp to just before the occurrence, so we avoid
        ;; re-scanning the part of the buffer already searched over.
        (MOVE-BP (POINT) (FORWARD-CHAR FOUND-AT (- (LENGTH *TAGS-QUERY-REPLACE-FROM*))))
        (RETURN)))
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)
    ;; Query replace thru that buffer.
    (SETQ VAL (QUERY-REPLACE (POINT) (INTERVAL-LAST-BP *INTERVAL*)
                             *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
                             *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))))

(DEFPROP COM-TAGS-MULTIPLE-QUERY-REPLACE KIND-OF-QUERY-REPLACE-DOCUMENTATION
         DOCUMENTATION-FUNCTION)
(DEFPROP COM-TAGS-MULTIPLE-QUERY-REPLACE T CONTROL-PERIOD)

(DEFCOM COM-TAGS-MULTIPLE-QUERY-REPLACE "Perform a Query Replace within the tags table files."
        ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (MULTIPLE-QUERY-REPLACE-STRINGS NIL)
    (TAGS-MULTIPLE-QUERY-REPLACE FROM-LIST TO-LIST (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))))

(DEFPROP COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER KIND-OF-QUERY-REPLACE-DOCUMENTATION
         DOCUMENTATION-FUNCTION)
(DEFPROP COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER T CONTROL-PERIOD)

(DEFCOM COM-TAGS-MULTIPLE-QUERY-REPLACE-FROM-BUFFER
        "Perform a Multiple Query Replace from the contents of the specified buffer." ()
  (MULTIPLE-VALUE-BIND (FROM-LIST TO-LIST)
      (PARSE-BUFFER-REPLACE-PAIRS *INTERVAL*)
    (TAGS-MULTIPLE-QUERY-REPLACE FROM-LIST TO-LIST (AND *NUMERIC-ARG-P* *NUMERIC-ARG*))))

(DEFPROP COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER KIND-OF-QUERY-REPLACE-DOCUMENTATION
         DOCUMENTATION-FUNCTION)

(DEFCOM COM-MULTIPLE-QUERY-REPLACE-FROM-BUFFER
        "Perform a Multiple Query Replace from the contents of the specified buffer." ()
  (WITH-REGION-OR-WHOLE-INTERVAL (REGION-P)
    (LET ((*MODE-WORD-SYNTAX-TABLE* (IF (AND *NUMERIC-ARG-P* (MINUSP *NUMERIC-ARG*))
                                        *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
          FROM-LIST TO-LIST)
      (MULTIPLE-VALUE-SETQ (FROM-LIST TO-LIST)
        (PARSE-BUFFER-REPLACE-PAIRS T))
      (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
                          FROM-LIST TO-LIST *NUMERIC-ARG-P*)))
  DIS-TEXT)

(DEFUN TAGS-MULTIPLE-QUERY-REPLACE (FROM-LIST TO-LIST ARG)
  (SETQ *TAGS-QUERY-REPLACE-FROM* FROM-LIST
        *TAGS-QUERY-REPLACE-TO* TO-LIST
        *TAGS-QUERY-REPLACE-DELIMITED* ARG)
  (COMMAND-STORE 'COM-CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE #/c-. *ZMACS-COMTAB*)
  (CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE T))

(DEFCOM COM-CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE "Continue the last Tags Query Replace." ()
  (CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE NIL))

(DEFUN CONTINUE-TAGS-MULTIPLE-QUERY-REPLACE (RESTART)
  (DO ((BEGINNING RESTART)
       (*MODE-WORD-SYNTAX-TABLE* (IF (AND *TAGS-QUERY-REPLACE-DELIMITED*
                                          (MINUSP *TAGS-QUERY-REPLACE-DELIMITED*))
                                *ATOM-WORD-SYNTAX-TABLE* *MODE-WORD-SYNTAX-TABLE*))
       (VAL))
      (NIL)
    ;; Find and select the next buffer that has an occurrence of any of the strings.
    (DO (BP) (())
      (SETQ BP (NEXT-FILE-BP BEGINNING))
      (SETQ BEGINNING NIL)
      (WHEN (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
              (DOLIST (STR *TAGS-QUERY-REPLACE-FROM*)
                (IF (ZWEI-SEARCH BP STR) (RETURN T))))
        (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
        (MAKE-BUFFER-CURRENT (BP-TOP-LEVEL-NODE BP))
        (MOVE-BP (POINT) BP)
        (RETURN)))
    (MUST-REDISPLAY *WINDOW* DIS-TEXT)
    ;; Query replace thru that buffer.
    (SETQ VAL (QUERY-REPLACE-LIST (POINT) (INTERVAL-LAST-BP *INTERVAL*)
                                  *TAGS-QUERY-REPLACE-FROM* *TAGS-QUERY-REPLACE-TO*
                                  *TAGS-QUERY-REPLACE-DELIMITED*))
    (AND (EQ VAL 'ABORTED) (RETURN DIS-TEXT))))

(DEFCOM COM-SELECT-TAG-TABLE "Make a tag table current for commands like tags search." ()
  (make-tag-table-current (select-tag-table nil))
  DIS-NONE)

(DEFCOM COM-SELECT-SYSTEM-AS-TAG-TABLE "Make the files in a system behave like a tags file."
        ()
  (LET ((SYSTEM-NAME (READ-SYSTEM-NAME "System to select as tag table:")))
    (SELECT-FILE-LIST-AS-TAG-TABLE
      (SI:SYSTEM-SOURCE-FILES SYSTEM-NAME SI:*SOURCE-FILE-TYPES* NIL T)  ;include subsystems.
      SYSTEM-NAME))
  DIS-NONE)

(DEFUN SYSTEM-OF-PATHNAME (PATHNAME &OPTIONAL ALREADY-GENERIC)
  "Return the SI::SYSTEM structure for the system PATHNAME is in.
If none can be determined, return the one for the /"System/" system.
ALREADY-GENERIC non-NIL says assume PATHNAME is already a generic pathname."
  ;;if there wasn't a pathname, return the SYSTEM system
  (IF (NULL PATHNAME) (SI:FIND-SYSTEM-NAMED "System")
      (LET* ((GENERIC-PATHNAME
               (IF ALREADY-GENERIC PATHNAME
                 (SEND PATHNAME :GENERIC-PATHNAME)))
             (SYSTEMS (SEND GENERIC-PATHNAME :GET ':SYSTEMS)))
        ;;if it wasn't defined as part of a system, use the SYSTEM system
        (IF (NULL SYSTEMS) (SI:FIND-SYSTEM-NAMED "System")
          (SI:FIND-SYSTEM-NAMED (CAR SYSTEMS))))))

;;; $$$ New function. <17-Nov-88 smh>
(DEFUN SYSTEMs-OF-PATHNAME (PATHNAME &OPTIONAL ALREADY-GENERIC)
  "Returns a list of SI::SYSTEM structures for each system PATHNAME is in.
ALREADY-GENERIC non-NIL says assume PATHNAME is already a generic pathname."
  (IF (NULL PATHNAME) '()
    (LET* ((GENERIC-PATHNAME
             (IF ALREADY-GENERIC PATHNAME
               (SEND PATHNAME :GENERIC-PATHNAME)))
           (SYSTEMS (SEND GENERIC-PATHNAME :GET ':SYSTEMS)))
      ;;if it wasn't defined as part of a system, use the SYSTEM system
      (IF (NULL SYSTEMS) '()
        (mapcar #'SI:FIND-SYSTEM-NAMED SYSTEMS)))))

(DEFUN READ-SYSTEM-NAME (PROMPT
                         &OPTIONAL (DEFAULT (SYSTEM-OF-PATHNAME
                                              (BUFFER-GENERIC-PATHNAME *INTERVAL*) T)))
  "Read a system name in the mini buffer, defaulting to DEFAULT.
Prompts with PROMPT (which should end with a colon and not mention the default).
DEFAULT defaults to a guess based on the current buffer."
  (LET* ((default-name (if default (si:system-name default)))
         (*mini-buffer-default-string* default-name)
         (SYSTEM-NAME (COMPLETING-READ-FROM-MINI-BUFFER
                        (IF DEFAULT
                            (FORMAT NIL "~A (Default ~A)" PROMPT default-name)
                          PROMPT)
                        (SI:ALL-SYSTEMS-NAME-ALIST)
                        T)))
    (COND ((CONSP SYSTEM-NAME)
           (SETQ SYSTEM-NAME (CAR SYSTEM-NAME)))
          ((STRING-EQUAL SYSTEM-NAME "")
           (OR (SETQ SYSTEM-NAME (si:system-name DEFAULT)) (BARF "Unable to determine the default system for command.")))
          ((STRINGP SYSTEM-NAME)
           (CONDITION-CASE (SYSTEM) (SI:FIND-SYSTEM-NAMED SYSTEM-NAME NIL NIL)
             (ERROR (BARF "~A" SYSTEM))
             (:NO-ERROR (SETQ SYSTEM-NAME (SI:SYSTEM-NAME SYSTEM))))))
    SYSTEM-NAME))


(DEFCOM COM-SELECT-ALL-BUFFERS-AS-TAG-TABLE
  "Select all files currently read in as a tag table.
Numeric arg means read a string and consider only buffers whose names contain it.
This causes commands such as Tags Search, Tags Query Replace, and
Tags Compile Changed Sections to look through all files now visited." ()
  (SELECT-FILE-LIST-AS-TAG-TABLE
    (mapcar #'buffer-pathname
            (specify-list-of-buffers
              :only-non-special-buffers t
              :only-file-buffers t
              :substring (if *numeric-arg-p*
                             (typein-line-readline "Substring to check for buffers containing: "))))
     (TYPEIN-LINE-READLINE-WITH-DEFAULT "All buffers visiting files" "Name for this tag table"))
  DIS-NONE)

(DEFCOM COM-SELECT-SOME-BUFFERS-AS-TAG-TABLE
  "Select some of the files currently read in as a tag table.
For each file-visiting buffer, you are asked to say whether to include it.
Numeric arg means read a string and consider only buffers whose names contain it.
This causes commands such as Tags Search, Tags Query Replace, and
Tags Compile Changed Sections to look through all files you specify." ()
  (SELECT-FILE-LIST-AS-TAG-TABLE
     (mapcar #'buffer-pathname
            (specify-list-of-buffers
              :only-non-special-buffers t
              :only-file-buffers t
              :substring (if *numeric-arg-p*
                             (typein-line-readline "Substring to check for buffers containing: "))
              :query-string "Include file ~*~A in the tag table? "))
     (TYPEIN-LINE-READLINE-WITH-DEFAULT "Some buffers visiting files" "Name for this tag table"))
  DIS-NONE)

(DEFFLAVOR TAG-TABLE-DUMMY-FILE (NAME) (SI:PROPERTY-LIST-MIXIN)
  :INITTABLE-INSTANCE-VARIABLES
  :GETTABLE-INSTANCE-VARIABLES)

(DEFMETHOD (TAG-TABLE-DUMMY-FILE :PRINT-SELF) (STREAM IGNORE SLASHIFY-P)
  (IF SLASHIFY-P
      (SI:PRINTING-RANDOM-OBJECT (SELF STREAM) (PRINC NAME STREAM))
    (PRINC NAME STREAM)))

(compile-flavor-methods tag-table-dummy-file)

(DEFUN SELECT-FILE-LIST-AS-TAG-TABLE (FILE-LIST NAME)
  "Select a tag table named NAME consisting of the files in FILE-LIST.
This can be used to control commands such as Tags Search."
  (setq name (string name))
  (let ((elt (cdr (ass #'string-equal name *zmacs-tag-table-alist*))))
    (unless elt
      (setq elt (MAKE-INSTANCE 'TAG-TABLE-DUMMY-FILE :NAME NAME) )
      (PUSH (CONS NAME elt) *ZMACS-TAG-TABLE-ALIST*))
    (SEND elt :PUTPROP (MAPCAR (LAMBDA (X) (FS:MERGE-PATHNAMES X *PATHNAME-DEFAULTS*)) FILE-LIST)
                       'ZMACS-TAG-TABLE-FILE-SYMBOLS)
    (make-tag-table-current elt)))

(defun make-tag-table-current (name-or-tag-table)
  (let ((elt (typecase name-or-tag-table
               (tag-table-dummy-file
                name-or-tag-table)
               (string
                 (cdr (ass #'string-equal name-or-tag-table *zmacs-tag-table-alist*)))
               (otherwise
                nil))))
    (when elt
      (when *zmacs-current-tag-table*
        ;;Save current search values....
        (send *zmacs-current-tag-table* :putprop *zmacs-tags-search-key-string* 'search-key-string)
        (send *zmacs-current-tag-table* :putprop *zmacs-last-tags-file-list* 'tags-file-list)
        (if (boundp '*zmacs-tags-search-function*)
            (send *zmacs-current-tag-table* :putprop *zmacs-tags-search-function* 'search-function)
          (send *zmacs-current-tag-table* :remprop 'search-function))
        (if (boundp '*zmacs-tags-search-key*)
            (send *zmacs-current-tag-table* :putprop *zmacs-tags-search-key* 'search-key)
          (send *zmacs-current-tag-table* :remprop 'search-key)))
      (setq *zmacs-current-tag-table* elt)
      ;;Restore saved search values
      (setq *zmacs-last-tags-file-list* (send *zmacs-current-tag-table* :get 'tags-file-list))
      (let ((buffer (send *zmacs-current-tag-table* :get 'current-pathname)))
        (when buffer
          (point-pdl-push (point) *window* nil t)
          (make-buffer-current buffer)
          (move-bp (point) (send *zmacs-current-tag-table* :get 'current-point))))
      (let ((function (send *zmacs-current-tag-table* :get 'search-function)))
        (when function
          (setq *zmacs-tags-search-function* function)))
      (let ((key (send *zmacs-current-tag-table* :get 'search-key)))
        (when key
          (setq *zmacs-tags-search-key* key))))))

(DEFUN SELECT-TAG-TABLE (&OPTIONAL (DEFAULT-P T))
  "Read a tag table name and return that tag table.
DEFAULT-P non-NIL (as it is if omitted) means if there is an
obvious default than just return it without asking the user at all."
  (COND ((NULL *ZMACS-TAG-TABLE-ALIST*)
         (format *query-io* "~&No tag tables have been read in.")
         (beep nil *query-io*)
         (if (null (y-or-n-p "~&Do you want to read an external /"tag table file/"?"))
             (barf)
           ;;This is support for ITS tag-table files:
           (LET ((PATHNAME (READ-DEFAULTED-PATHNAME "Tag table pathname:"
                                                    (PATHNAME-DEFAULTS) "TAGS")))
             (READ-TAG-TABLE PATHNAME)
             PATHNAME)))
        ((AND DEFAULT-P *ZMACS-CURRENT-TAG-TABLE*)
         *ZMACS-CURRENT-TAG-TABLE*)
        ((AND DEFAULT-P (NULL (CDR *ZMACS-TAG-TABLE-ALIST*)))
         (CDAR *ZMACS-TAG-TABLE-ALIST*))
        (T
         (setq *zmacs-current-tag-table* (zmacs-current-tag-table))
         (LET ((TABLE (let ((*mini-buffer-default-string*
                              (and *zmacs-current-tag-table* (send *zmacs-current-tag-table* :name))))
                        (COMPLETING-READ-FROM-MINI-BUFFER
                          (format nil "Tag table~@[ (default is ~a)~]:" *mini-buffer-default-string*)
                          *ZMACS-TAG-TABLE-ALIST*))))
           (COND ((and (stringp table)
                       (EQUAL (string-trim *whitespace-chars* TABLE) ""))
                  (COND (*ZMACS-CURRENT-TAG-TABLE* *ZMACS-CURRENT-TAG-TABLE*)
                        (T (BARF))))
                 ((CDR TABLE))
                 (t (barf)))))))

(defun forget-tag-table()
  (when *zmacs-current-tag-table*
    (setq *zmacs-tag-table-alist*
          (lisp:remove *zmacs-current-tag-table* *zmacs-tag-table-alist* :key #'cdr))
    (setq *zmacs-current-tag-table* nil))
  (when (setq *zmacs-current-tag-table*
              (zmacs-current-tag-table))
    (make-tag-table-current *zmacs-current-tag-table*))
  *zmacs-current-tag-table*)

(defcom com-forget-tag-table "Remove the current tag table from list of known tag tables." ()
  (forget-tag-table)
  dis-none)


(DEFUN PARSE-BUFFER-REPLACE-PAIRS (DEFAULT &AUX *INTERVAL*)
  "Parse a buffer as a list of string replacements.
Reads a buffer name from the user and parses that buffer.
Returns two values, a list of strings to replace and a list of replacement strings."
  (SETQ *INTERVAL* (READ-BUFFER-NAME "Use replacements in buffer:" DEFAULT))
  (DO ((BP (INTERVAL-FIRST-BP *INTERVAL*))
       (END-BP (INTERVAL-LAST-BP *INTERVAL*))
       (FROM-LIST) (TO-LIST) (TEM))
      (NIL)
    (SETQ BP (FORWARD-OVER *WHITESPACE-CHARS* BP))
    (AND (BP-= BP END-BP) (RETURN (VALUES (NREVERSE FROM-LIST) (NREVERSE TO-LIST))))
    (IF (EQL (BP-CH-CHARACTER BP) #/;)
        (SETQ BP (BEG-LINE BP 1))
      (MULTIPLE-VALUE-SETQ (TEM BP)
        (PARSE-BUFFER-REPLACE-PAIRS-1 BP))
      (PUSH TEM FROM-LIST)
      (SETQ BP (FORWARD-OVER *BLANKS* BP))
      (AND (END-LINE-P BP) (BARF "Only one item on line ~S" (BP-LINE BP)))
      (MULTIPLE-VALUE-SETQ (TEM BP)
        (PARSE-BUFFER-REPLACE-PAIRS-1 BP))
      (PUSH TEM TO-LIST))))

(DEFUN PARSE-BUFFER-REPLACE-PAIRS-1 (BP &AUX BP1 STR)
  (OR (SETQ BP1 (FORWARD-SEXP BP)) (BARF "Premature EOF on line ~S???" (BP-LINE BP)))
  (SETQ STR (STRING-INTERVAL BP BP1 T))
  (AND (EQL (CHAR STR 0) #/")
       (SETQ STR (CL:READ-FROM-STRING STR)))
  (VALUES STR BP1))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Arglist"
                          TYPEOUT-MENU-ARGLIST NIL
                          "Print arglist for this function.")

(DEFUN TYPEOUT-MENU-ARGLIST (FUNCTION)
  (FORMAT *QUERY-IO* "~&~S: ~A" FUNCTION (ARGLIST FUNCTION))
  T)                                            ;Leave the typeout window there

;;;TYPEOUT-YES-OR-NO-P moved to ZWEI;DISPLA -Keith

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* BP "Move" MOVE-TO-BP T
                          "Move to this line.")

(DEFUN MOVE-TO-BP (BP &AUX INTERVAL)
  "Move point to BP, selecting its buffer if necessary."
  (AND (SETQ INTERVAL (BP-TOP-LEVEL-NODE BP))
       (NEQ INTERVAL *INTERVAL*)
       (PROGN (TYPEOUT-ABORT-MINI-BUFFER)
              (MAKE-BUFFER-CURRENT INTERVAL)))
  (MOVE-BP (POINT) BP)
  NIL)

(DEFUN DEFAULTED-TYPEOUT-FILE-NAME (STRING &OPTIONAL HACK-NEWEST &AUX PATHNAME VERSION)
  (TYPEOUT-ABORT-MINI-BUFFER)
  (SETQ PATHNAME (MAKE-DEFAULTED-PATHNAME (STRING STRING) (PATHNAME-DEFAULTS)))
  (WHEN HACK-NEWEST
    ;;It we get a specific file, see if that was the newest and if so, use that instead
    (AND (NOT (MEMQ (SETQ VERSION (SEND PATHNAME :VERSION)) '(:NEWEST :UNSPECIFIC)))
         (= VERSION (SEND (SEND (SEND PATHNAME :NEW-VERSION :NEWEST) :TRUENAME)
                          :VERSION))
         (SETQ PATHNAME (SEND PATHNAME :NEW-VERSION :NEWEST))))
  PATHNAME)


(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "Find"
                          (LAMBDA (STRING)
                            (FIND-FILE (DEFAULTED-TYPEOUT-FILE-NAME STRING) T)) T
                          "Find file this file.")
(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "View"
                          (LAMBDA (STRING)
                            (VIEW-FILE (DEFAULTED-TYPEOUT-FILE-NAME STRING))) NIL
                          "View this file.")
(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FILE "Load"
                          (LAMBDA (STRING)
                            (LOAD (DEFAULTED-TYPEOUT-FILE-NAME STRING) :VERBOSE T)) NIL
                          "LOAD this file.")

(DEFCOM COM-EDIT-METHODS "Edit all methods for specified message." ()
  (MULTIPLE-VALUE-BIND (CLASSES-AND-FUNCTION-SYMBOLS MESSAGE)
      (LIST-METHODS-INTERNAL
        (READ-OPERATION-NAME "Edit flavors with methods for"))
    (EDIT-FUNCTIONS-NO-DISPLAY ;; The printed-representation of each item is the flavor name,
                            ;; including package prefix if necessary, and the method type
                            ;; in parentheses.  The cdr (function-name)
                            ;; of each item is the mumble-class-mumble-method symbol.
                            (MAPCAR (LAMBDA (X)
                                      (CONS (FORMAT NIL "~S~@[ (:~A)~]" (CAR X) (CADR X))
                                            (CADDR X)))
                                    CLASSES-AND-FUNCTION-SYMBOLS)
                            "Flavors with ~S methods:"
                            "No methods for ~S found."
                            MESSAGE)))

(DEFCOM COM-LIST-METHODS "List all flavors with methods for specified message." ()
  (MULTIPLE-VALUE-BIND (CLASSES-AND-FUNCTION-SYMBOLS MESSAGE)
      (LIST-METHODS-INTERNAL
        (READ-OPERATION-NAME "List classes and flavors with methods for"))
    (EDIT-FUNCTIONS-DISPLAY ;; The printed-representation of each item is the flavor name,
                            ;; including package prefix if necessary, and the method type
                            ;; in parentheses.  The cdr (function-name)
                            ;; of each item is the mumble-class-mumble-method symbol.
                            (MAPCAR (LAMBDA (X)
                                      (CONS (FORMAT NIL "~S~@[ (:~A)~]" (CAR X) (CADR X))
                                            (CADDR X)))
                                    CLASSES-AND-FUNCTION-SYMBOLS)
                            "Flavors with ~S methods:"
                            "No methods for ~S found."
                            MESSAGE))
  DIS-NONE)

(DEFUN LIST-METHODS-INTERNAL (OPERATION &AUX FL TEM)
  (LET ((CLASSES-AND-FUNCTION-SYMBOLS
          (DO ((L (CONS SI:OBJECT-CLASS (SI:ALL-SUBCLASSES-OF-CLASS SI:OBJECT-CLASS)) (CDR L))
               (R NIL)
               (SYM))
              ((NULL L) R)
            (COND ((SETQ SYM (SEND (CAR L) :METHOD-FOR OPERATION NIL))
                   (PUSH (LIST (SEND (CAR L) :CLASS-SYMBOL) NIL SYM) R))))))
    (DOLIST (FLAVOR SI:*ALL-FLAVOR-NAMES*)
      (AND (SETQ FL (GET FLAVOR 'SI:FLAVOR))
           (SETQ TEM (ASSQ OPERATION (SI:FLAVOR-METHOD-TABLE FL)))
           (DOLIST (METH (CDDDR TEM))
             (OR (EQ (SI:METH-METHOD-TYPE METH) :COMBINED)
                 (AND (SI:METH-DEFINEDP METH)
                      (PUSH (LIST FLAVOR (SI:METH-METHOD-TYPE METH) (SI:METH-FUNCTION-SPEC METH))
                            CLASSES-AND-FUNCTION-SYMBOLS))))))
    (VALUES CLASSES-AND-FUNCTION-SYMBOLS OPERATION)))

(DEFF GET-MESSAGE-NAME 'READ-OPERATION-NAME)

(DEFUN READ-OPERATION-NAME (PROMPT)
  "Read an operation name in the minibuffer, prompting with PROMPT.
PROMPT should be a string ending in a colon, if you want a colon.
If point is inside a FUNCALL or similar function, a default
may be derived from it."
  (PKG-BIND SI:PKG-KEYWORD-PACKAGE      ;So the colon can be omitted
    (MULTIPLE-VALUE-BIND (SYM STR)
        (READ-FUNCTION-NAME PROMPT
          (LET ((FUN (RELEVANT-FUNCTION-NAME (POINT))))
            (AND (MEMQ FUN '(SEND LEXPR-SEND FUNCALL-SELF LEXPR-FUNCALL-SELF))
                 (RELEVANT-METHOD-NAME (POINT)
                                       (IF (MEMQ FUN '(FUNCALL-SELF LEXPR-FUNCALL-SELF)) 1 2))))
          NIL T)
      ;; Kludge around to not get screwed by completions to funny symbols
      ;; while still working if user points with the mouse
      (IF STR (CL:READ-FROM-STRING STR) SYM))))

(DEFCOM COM-EDIT-COMBINED-METHODS
  "Edit all methods used for specified operation on specified flavor." ()
  (LIST-COMBINED-METHODS-INTERNAL "Edit" T)
  DIS-NONE)

(DEFCOM COM-LIST-COMBINED-METHODS
  "List all methods used for specified operation on specified flavor." ()
  (LIST-COMBINED-METHODS-INTERNAL "List" NIL)
  DIS-NONE)

(DEFUN LIST-COMBINED-METHODS-INTERNAL (OP JUST-EDIT &AUX MESSAGE FLAVOR METHODS)
  (SETQ MESSAGE (READ-OPERATION-NAME (FORMAT NIL "~A combined methods for operation" OP)))
  (SETQ FLAVOR (READ-FLAVOR-NAME (FORMAT NIL "~A combined methods for operation ~S on flavor"
                                               OP MESSAGE)
                        "You are typing the name of a flavor, to see its combined methods"))

  ;Duplicates code from SETUP-ZMACS-CALLERS-TO-BE-EDITED in order to
  ;put the methods in execution order rather than alphabetical order
  (SETQ METHODS (FIND-COMBINED-METHODS FLAVOR MESSAGE))
  (COMMAND-STORE 'COM-GO-TO-NEXT-TOP-LEVEL-POSSIBILITY #/C-. *ZMACS-COMTAB*)
  ;Duplicates code from LIST-ZMACS-CALLERS-TO-BE-EDITED in order to
  ;put the methods in execution order rather than alphabetical order
  (FUNCALL (IF JUST-EDIT #'EDIT-FUNCTIONS-NO-DISPLAY #'EDIT-FUNCTIONS-DISPLAY)
           (MAPCAR (LAMBDA (X) (CONS (FORMAT NIL "~S" X) X))
                   METHODS)
           "Methods combined for operation ~S on flavor ~S:"
           "No methods for operation ~S on flavor ~S."
           MESSAGE FLAVOR))

(DEFUN FIND-COMBINED-METHODS (FLAVOR MESSAGE &OPTIONAL (ERROR T) &AUX FL SM METHOD)
  "Return a list of the non-combined methods involved in handling MESSAGE to FLAVOR"
  (BLOCK NIL
    (OR (SETQ FL (GET FLAVOR 'SI:FLAVOR))
        (IF ERROR (BARF "~S not DEFFLAVOR'ed" FLAVOR)
          (RETURN NIL)))
    (OR (SI:FLAVOR-METHOD-HASH-ARRAY FL)
        (IF (FQUERY NIL "~S's combined methods are not composed.  Compose them? "
                    FLAVOR)
            (SI:RECOMPILE-FLAVOR FLAVOR NIL T NIL)
          (IF ERROR (BARF "~S's methods are not composed" FLAVOR)
            (RETURN NIL))))
    (SETQ METHOD
          (IF (SI:FLAVOR-GET FL ':ABSTRACT-FLAVOR)
              (AND (FDEFINEDP `(:METHOD ,FLAVOR :COMBINED ,MESSAGE))
                   (FDEFINITION `(:METHOD ,FLAVOR :COMBINED ,MESSAGE)))
            (IGNORE-ERRORS
              (SI:GET-FLAVOR-HANDLER-FOR FLAVOR MESSAGE))))
    (OR METHOD (IF ERROR
                   (BARF "Flavor ~S does not handle message ~S" FLAVOR MESSAGE)
                 (RETURN NIL)))
    (SETQ METHOD (FUNCTION-NAME METHOD))
    (IF (SETQ SM (CDDDR (OR (CADR (ASSQ 'SI:COMBINED-METHOD-DERIVATION
                                        (DEBUGGING-INFO
                                          (SI:UNENCAPSULATE-FUNCTION-SPEC METHOD))))
                            (SI:FUNCTION-SPEC-GET METHOD
                                                  'SI:COMBINED-METHOD-DERIVATION))))
        (NCONC (REVERSE (CDR (ASSQ ':WRAPPER SM)))      ;Try to approximate the order
               (REVERSE (CDR (ASSQ ':BEFORE SM)))       ;in which they're called
               (REVERSE (CDR (ASSQ NIL SM)))
               (COPY-LIST (CDR (ASSQ ':AFTER SM)))
               (MAPCAN (LAMBDA (X)
                         (AND (NOT (MEMQ (CAR X) '(:WRAPPER :BEFORE NIL :AFTER)))
                              (REVERSE (CDR X))))
                       SM))
      (LIST METHOD))))
