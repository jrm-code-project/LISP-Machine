;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:T -*-
;;; Buffer attribute list commands

(defcom com-set-package-root "Change the package root associated with buffer or file.
If a package with the same name as the current package exists in that hierarchy,
select it, otherwise, offer to create it.  Then you will be asked whether to
resectionize the buffer.  If you say yes, all function definitions in the buffer
will be recorded under symbols in the new package." ()
  (let (root-alist)
    (dolist (pkg *all-packages*)
      (let ((root (get pkg ':root)))
        (unless (or (null root)
                    (cl:member root root-alist :key 'cdr))
          (push (cons (package-name root) root) root-alist)
          (dolist (nick (package-nicknames root))
            (push (cons nick root) root-alist)))))
  ; (push '("" . nil) root-alist)
    (push `("GLOBAL" . ,(find-package 'global)) root-alist)
    (let ((new-root (car (completing-read-from-mini-buffer
                           "Set new root package: " root-alist ))))
      (let ((pkg (find-package (si:package-primary-name *package*) (find-package new-root)))
            (pkg-name (package-name *package*))
            (pkg-nicknames (package-nicknames *package*)))
        (if pkg
            (pkg-goto pkg () new-root)
          (IF (YES-OR-NO-P (FORMAT NIL "Package ~A does not exist.  Create? " PKG))
              (progn
                (if new-root
                    (pkg-goto new-root)
                  (pkg-goto 'user)
                (pkg-goto (make-package pkg-name :nicknames pkg-nicknames)))
            (BARF)))))))
  (SETF (BUFFER-PACKAGE *INTERVAL*) *PACKAGE*)
  (WHEN (FQUERY NIL "Resectionize the buffer? ")
    (SEND *INTERVAL* ':REMPROP ':DONT-SECTIONIZE)
    (SECTIONIZE-BUFFER *INTERVAL*))
  DIS-NONE)

(DEFCOM COM-SET-PACKAGE "Change the package associated with buffer or file.
Specify a new package to be used when interning symbols read from this buffer;
for example, evaluating or compiling parts of the buffer.
This much does not affect operations on the file itself, only this ZMACS buffer.

To specify a package that doesn't already exist, you must exit the first
prompt with Control-Return, or type Return twice.  Then you must confirm
with /"Yes/".

If you answer the first prompt with just Return, you will be asked
whether to use a package chosen by default based on the current
environment.  Then you must confirm whether to use the default.

You will also be asked whether to change the attribute list in the text.
If you answer yes, the buffer's first line is modified to say that
it belongs in the new package.  This will affect all operations on the
file, once you save the buffer.

Then you will be asked whether to resectionize the buffer.
If you say yes, all the functions definitions in the buffer
will be recorded under symbols in the new package." ()
  (LET ((ALIST nil)
        (default-package (or *DEFAULT-PACKAGE* *package* (find-package "USER"))))
    (DOLIST (PKG *ALL-PACKAGES*)
      (PUSH (CONS (PACKAGE-NAME PKG) PKG) ALIST)
      (DOLIST (N (PACKAGE-NICKNAMES PKG))
        (PUSH (CONS N PKG) ALIST)))
    (LET ((PKG
            (let((*mini-buffer-default-string* (and default-package
                                                    (package-name default-package))))
              (COMPLETING-READ-FROM-MINI-BUFFER "Set package:" ALIST 'MAYBE))))
      (OR (STRINGP PKG) (SETQ PKG (CAR PKG)))
      (cond
        ((not (EQUAL PKG "")))
        ((and (SETQ PKG default-package)
              (or (YES-OR-NO-P (format nil "Use package ~A by default? " pkg))
                  (barf "You must specify a package or take the default."))))
        (t (barf "There is no default package; cannot Set Package in this context.")))
      (IF (FIND-PACKAGE PKG)
          (PKG-GOTO PKG)
        (progn
          (SETQ PKG (STRING-UPCASE PKG))
          (IF (YES-OR-NO-P (FORMAT NIL "Package ~A does not exist.  Create? " PKG))
              (PKG-GOTO (MAKE-PACKAGE PKG))
            (BARF))))))
  (SETF (BUFFER-PACKAGE *INTERVAL*) *PACKAGE*)
  (SEND *INTERVAL* ':SET-ATTRIBUTE ':PACKAGE (PACKAGE-NAME *PACKAGE*) ':QUERY)
  (WHEN (FQUERY NIL "Resectionize the buffer? ")
    (SEND *INTERVAL* ':REMPROP ':DONT-SECTIONIZE)
    (SECTIONIZE-BUFFER *INTERVAL*))
  DIS-NONE)

;; Merged from LAD: RELEASE-3.ZWEI; ZMNEW.LISP#43 on 2-Oct-86 02:15:49
(DEFUN COMPUTE-BUFFER-PACKAGE (BUFFER)
  "Set *PACKAGE*, *READ-BASE*, *PRINT-BASE* and *READTABLE*
to the right values for BUFFER."
  (SETQ *PRINT-BASE* (SETQ *READ-BASE* (OR (SEND BUFFER ':GET-ATTRIBUTE ':BASE)
                                           *DEFAULT-BASE* *READ-BASE* 8)))
  (SETQ *PACKAGE* (OR (SEND BUFFER ':SEND-IF-HANDLES ':SAVED-PACKAGE)
                      (PKG-FIND-PACKAGE (OR *DEFAULT-PACKAGE* *PACKAGE*))))
  (SETQ *READTABLE* (SI:FIND-READTABLE-NAMED (OR (OR (SEND BUFFER ':GET-ATTRIBUTE ':READTABLE)
                                                     ;; :SYNTAX is brand-S compatibility.
                                                     (SEND BUFFER ':GET-ATTRIBUTE ':SYNTAX))
                                                 *DEFAULT-READTABLE*
                                                 *READTABLE*
                                                 SI:STANDARD-READTABLE)))
  NIL)

(DEFUN INITIALIZE-BUFFER-PACKAGE (BUFFER)
  "Initialize the BUFFER-PACKAGE of BUFFER from its :PACKAGE attribute."
  (OR (BUFFER-PACKAGE BUFFER)
      (PKG-BIND (cond (*DEFAULT-PACKAGE*)
                      ((and *PACKAGE*
                            (null (get *package* :root)))
                       *package*)  ;preserve old (fairly random) behavior if package-root
                                   ;feature not in use
                      (t si:pkg-user-package))  ;make sure we dont get a non-global-root
                                ;package unless the user explicitly says he wants that.
        (CONDITION-BIND ((SYS:PACKAGE-NOT-FOUND 'INITIALIZE-BUFFER-PACKAGE-HANDLER))
          (SETF (BUFFER-PACKAGE BUFFER)
                (MULTIPLE-VALUE-BIND (VARS VALS)
                    (SEND BUFFER ':ATTRIBUTE-BINDINGS)
                  (PROGV VARS VALS *PACKAGE*)))))))

(DEFUN INITIALIZE-BUFFER-PACKAGE-HANDLER (CONDITION)
  (WHEN (YES-OR-NO-P "~&Package ~A does not exist.  Create it? "
                     (SEND CONDITION ':PACKAGE-NAME))
    ':CREATE-PACKAGE))

(DEFCOM COM-SET-BASE "Change the input radix associated with this buffer or file.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer.  The default is ZWEI:*DEFAULT-BASE*."
  ()
  (SETQ *READ-BASE* (IF *NUMERIC-ARG-P* *NUMERIC-ARG*
                      (LET ((*READ-BASE* 10.) (*PRINT-BASE* 10.)
                            (*mini-buffer-default-string* "10.")
                            (*typein-line-eof-value* 10.))
                        (TYPEIN-LINE-READ "Set base (default is 10.):" *typein-line-eof-value*)))
        *PRINT-BASE* *READ-BASE*)
  (format *query-io* "Setting base to ~10r -" *read-base*)
  (SEND *INTERVAL* ':SET-ATTRIBUTE ':BASE *READ-BASE* ':QUERY)
  DIS-NONE)

(DEFUN SET-ATTRIBUTE (KEYWORD NAMESTRING &OPTIONAL DEFAULT)
  (LET ((INPUT (STRING-TRIM *BLANKS* (TYPEIN-LINE-READLINE "Set ~A:" NAMESTRING)))
        (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
        (*PRINT-BASE* 10.)
        (*READ-BASE* 10.))
    (SEND *INTERVAL* ':SET-ATTRIBUTE KEYWORD
          (IF (EQUAL INPUT "")
              DEFAULT
            (LET ((TEM (READ-FROM-STRING INPUT)))
              (IF (EQ TEM ':NIL) NIL TEM)))
          ':QUERY))
  DIS-NONE)

(DEFCOM COM-SET-COMMON-LISP
  "Change whether the contents of this buffer are to be regarded as having common-lisp syntax.
Applies only to this buffer, and overrides what the attribute list says,
and is done by changing the readtable in effect for this buffer. See also m-x Set Readtable
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer."
  ()
  (LET ((INPUT (STRING-TRIM *BLANKS* (TYPEIN-LINE-READLINE "Set Common Lisp:")))
        (*PACKAGE* SI:PKG-KEYWORD-PACKAGE))
    (SETQ *READTABLE* (IF (EQUAL INPUT "")
                          *READTABLE*
                        (IF (LET ((TEM (READ-FROM-STRING INPUT)))
                              (IF (EQ TEM :NIL) NIL TEM))
                            SI:COMMON-LISP-READTABLE
                            SI:STANDARD-READTABLE)))
    (SEND *INTERVAL* :SET-ATTRIBUTE :READTABLE
                                    (DONT-OPTIMIZE (SI:RDTBL-SHORT-NAME *READTABLE*))
                                    :QUERY))
  (WHEN (EQ *MAJOR-MODE* 'LISP-MODE)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
                         LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
                         LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))
  DIS-NONE)

(DEFCOM COM-SET-READTABLE
  "Change the readtable associated with buffer or file.
Specify a new readtable to be used when reading from this buffer;
for example, evaluating or compiling parts of the buffer, or when
parsing the list syntax of the buffer in ZMACS.
This much does not affect operations on the file itself, only this ZMACS buffer.

To specify a readtable that doesn't already exist, you must exit with
Control-Return, or type Return twice.  Then you must confirm with /"Yes/".

You will also be asked whether to change the attribute list in the text.
If you answer yes, the buffer's first line is modified to say that
it should be read using the new readtable.  This will affect all operations on the
file, once you save the buffer.

Then you will be asked whether to resectionize the buffer.
If you say yes, all the functions definitions in the buffer
will be recorded under symbols in the new package." ()
  (LET (ALIST)
    (DOLIST (RDTBL SI:*ALL-READTABLES*)
      (DOLIST (N (DONT-OPTIMIZE (SI:RDTBL-NAMES RDTBL)))
        (PUSH (CONS N RDTBL) ALIST)))
    (LET* ((default-str (si:rdtbl-short-name *readtable*))
           (*mini-buffer-default-string* default-str)
           (RDTBL (COMPLETING-READ-FROM-MINI-BUFFER
                    (format nil "Set readtable (default is ~a) :" default-str)
                    ALIST 'MAYBE)))
      (UNLESS (STRINGP RDTBL) (SETQ RDTBL (CAR RDTBL)))
      (IF (EQUAL RDTBL "")
          (SETQ RDTBL *READTABLE*)
        (IF (SI:FIND-READTABLE-NAMED RDTBL :FIND)
            (SETQ *READTABLE* (SI:FIND-READTABLE-NAMED RDTBL))
          (SETQ RDTBL (STRING-CAPITALIZE-WORDS RDTBL NIL NIL))
          (IF (NOT (YES-OR-NO-P (FORMAT NIL "Readtable ~A does not exist.  Create? " RDTBL)))
              (BARF)
            (SETQ *READTABLE* (COPY-READTABLE *READTABLE*))
            (SETF (DONT-OPTIMIZE (SI:RDTBL-NAMES *READTABLE*)) (LIST RDTBL)))))))
  (SEND *INTERVAL* ':SET-ATTRIBUTE :READTABLE
                                   (DONT-OPTIMIZE (SI:RDTBL-SHORT-NAME *READTABLE*))
                                   :QUERY)
  (WHEN (EQ *MAJOR-MODE* 'LISP-MODE)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
                         LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
    (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
                         LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))
  DIS-NONE)

(DEFCOM COM-SET-BACKSPACE "Change the Backspace attribute of this buffer.
Anything non-NIL causes backspace characters to actually overprint on display.
The new value is read in the minibuffer.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well."
  ()
  (SET-ATTRIBUTE ':BACKSPACE "attribute Backspace")
  (REDEFINE-WINDOW-OVERPRINTING-FLAG *WINDOW* (SEND *INTERVAL* ':GET-ATTRIBUTE ':BACKSPACE))
  DIS-NONE)

(DEFCOM COM-SET-LOWERCASE "Change the Lowercase attribute of this buffer.
Anything non-NIL identifies this file as containing lowercase or mixed-case data.
 So Electric Shift-Lock mode will not be used as a default.
The new value is read in the minibuffer.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well."
  ()
  (SET-ATTRIBUTE ':LOWERCASE "attribute Lowercase"))

(DEFCOM COM-SET-NOFILL "Set the Nofill attribute of this buffer.
Anything non-NIL prevents use of Auto Fill mode without an explicit user command.
The new value is read in the minibuffer.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well."
  ()
  (SET-ATTRIBUTE ':NOFILL "attribute Nofill"))

(DEFCOM COM-SET-PATCH-FILE "Set the Patch-File attribute of this buffer.
Anything non-NIL identifies this file as a patch file.
The new value is read in the minibuffer.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well."
  ()
  (SET-ATTRIBUTE ':PATCH-FILE "attribute Patch-File")
  DIS-NONE)

(DEFCOM COM-SET-TAB-WIDTH "Set the displayed width of Tab characters for this buffer.
This is the separation of tab stops, measured in space characters.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer.  The default is 8."
  ()
  (IF *NUMERIC-ARG-P*
      (SEND *INTERVAL* ':SET-ATTRIBUTE ':TAB-WIDTH *NUMERIC-ARG* ':QUERY)
    (SET-ATTRIBUTE ':TAB-WIDTH "attribute Tab-Width" 8))
  (REDEFINE-WINDOW-TAB-NCHARS *WINDOW*
                              (SEND *INTERVAL* ':GET-ATTRIBUTE ':TAB-WIDTH))
  DIS-NONE)

(DEFCONST *VSP* 2)     ;global default value, also gotten by (:VSP default-attribute-value)

(DEFCOM COM-SET-VSP "Set the vertical interline spacing for this buffer.
This is the number of blank rows of pixels between lines of text.
Applies only to this buffer, and overrides what the attribute list says.
Queries you for whether to change the attribute list in the text as well.
The numeric argument is used as the new value,
or else one is read in the minibuffer.  The default is 2." ()
  (IF *NUMERIC-ARG-P*
      (SEND *INTERVAL* ':SET-ATTRIBUTE *NUMERIC-ARG* ':QUERY)
    (SET-ATTRIBUTE ':VSP "attribute VSP" *VSP*))
  (REDEFINE-FONTS *WINDOW* (WINDOW-FONT-ALIST *WINDOW*)
                  (SEND *INTERVAL* ':GET-ATTRIBUTE ':VSP))
  DIS-NONE)

(DEFCOM COM-REPARSE-ATTRIBUTE-LIST "Look at the -*- line again; obey any changes." ()
  (REPARSE-BUFFER-attribute-list *INTERVAL*)
  DIS-NONE)

(DEFUN REPARSE-BUFFER-attribute-list (BUFFER)
  "Reparse BUFFER's attribute list line and set the buffer's recorded attributes."
  (FS:READ-ATTRIBUTE-LIST BUFFER (INTERVAL-STREAM BUFFER))
  ;; If there is any Font attribute, enforce it to be a list
  (let ((fonts (get buffer :fonts)))
    (when (and fonts (atom fonts))
      (setf (get buffer :fonts) (list fonts))))
  ;; Forget (and thereby override) any Set Package previously done.
  (SETF (BUFFER-PACKAGE BUFFER) NIL)
  (INITIALIZE-BUFFER-PACKAGE BUFFER)
  (SEND BUFFER ':SET-MAJOR-MODE
        (OR (GET-FILE-MAJOR-MODE (OR (FUNCALL BUFFER ':GET-ATTRIBUTE ':MODE)
                                     *DEFAULT-MAJOR-MODE*))
            'FUNDAMENTAL-MODE))
  (LET* (FONTS (*INTERVAL* BUFFER)) ;Must not be bound around the :SET-MAJOR-MODE!
    (SETQ FONTS (SET-BUFFER-FONTS BUFFER))
    (DOLIST (W (SEND BUFFER ':WINDOWS))
      (REDEFINE-FONTS W FONTS (SEND BUFFER ':GET-ATTRIBUTE ':VSP))
      (REDEFINE-WINDOW-OVERPRINTING-FLAG W (SEND BUFFER ':GET-ATTRIBUTE ':BACKSPACE))
      (REDEFINE-WINDOW-TAB-NCHARS W (SEND BUFFER ':GET-ATTRIBUTE ':TAB-WIDTH)))
    (IF (AND *WINDOW* (EQ BUFFER (WINDOW-INTERVAL *WINDOW*)))
        (COMPUTE-BUFFER-PACKAGE BUFFER))))

(DEFCOM COM-UPDATE-ATTRIBUTE-LIST "Update the -*- line from current settings.
Update the attribute list in the text from current settings of buffer."
  ()
  (update-buffer-attribute-list *interval*)
  DIS-TEXT)

(defun update-buffer-attribute-list (interval)
  "Update INTERVAL's -*- line to agree with its attributes (on its property list)."
  ;; Give PLIST the current values of all of those attributes.
  (let ((plist (get interval 'fs::last-file-plist)))
    (do ((ps plist (cddr ps)))
        ((null ps))
      (setf (cadr ps)
            (send interval :get (car ps))))
    (store-attribute-list interval plist)))

(DEFCONST *UPDATE-PLIST-ON-WRITE-OK* T
  "Non-NIL enables adding Base and other important attributes to files missing them.")

(DEFUN CHECK-PLIST-FOR-IMPORTANT-ATTRIBUTES (PLIST BUFFER)
  (MULTIPLE-VALUE-BIND (NIL PARSING-ERROR)
      (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM BUFFER))
    (IF PARSING-ERROR
        (PROGN
          (FORMAT *QUERY-IO* "~&Invalid syntax in the -*- line of buffer ~A." BUFFER)
          NIL)
      (WHEN (AND *UPDATE-PLIST-ON-WRITE-OK*
                 (EQ ':LISP (GET (LOCF PLIST) ':MODE))
                 (BUFFER-PATHNAME BUFFER)
                 ;; Don't mess with init files, since they may SETQ BASE.
                 (NEQ (SEND (BUFFER-PATHNAME BUFFER) ':CANONICAL-TYPE) ':INIT))
        (WHEN (OR
                ;; insert a base attribute.
                (WHEN (AND (NOT (SEND BUFFER ':GET-ATTRIBUTE ':NO-BASE-ATTRIBUTE))
                           (NULL (GET (LOCF PLIST) ':BASE)))
                  (FORMAT *QUERY-IO* "~&Updating Base attribute of ~A to ~D.~&"
                          BUFFER *READ-BASE*)
                  (PUSH *READ-BASE* PLIST)
                  (PUSH ':BASE PLIST))
                ;; insert a readtable attribute.
                (WHEN (AND (NOT (SEND BUFFER ':GET-ATTRIBUTE ':NO-READTABLE-ATTRIBUTE))
                           (NOT (OR (GET (LOCF PLIST) :READTABLE)
                                    (GET (LOCF PLIST) :SYNTAX))))
                  (LET ((NAME (DONT-OPTIMIZE (SI:RDTBL-SHORT-NAME *READTABLE*))))
                    (FORMAT *QUERY-IO* "~&Updating readtable attribute of ~A to ~A.~&"
                            BUFFER name)
                    (PUSH NAME PLIST)
                    (PUSH :READTABLE PLIST))))
          (STORE-ATTRIBUTE-LIST BUFFER PLIST)
          T)))))                                ;return T if modified

(DEFUN STORE-ATTRIBUTE-LIST (BUFFER PLIST)
  "Modify the attribute list in BUFFER's text to correspond to PLIST.
However, the buffer's current major mode is always recorded
rather than anything PLIST says."
  (LET ((START-BP (INTERVAL-FIRST-BP BUFFER))
        OLD-ATTRIBUTES PARSING-ERROR
        OLD-ATTRIBUTE-NAMES
        (*INTERVAL* BUFFER)
        LINE ALIST)
    (DECLARE (SPECIAL OLD-ATTRIBUTE-NAMES))
    (SETF (VALUES OLD-ATTRIBUTES PARSING-ERROR)
          (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM BUFFER)))
    (WHEN PARSING-ERROR
      (FERROR NIL "Invalid syntax in the -*- line in buffer ~A." BUFFER))
    ;; Turn the plist into an alist with elements (propname-string value-string)
    (DO ((PS PLIST (CDDR PS))
         (*PRINT-BASE* 10.)
         (*NOPOINT T) (*PRINT-RADIX* NIL)
         (*PACKAGE* SI:PKG-KEYWORD-PACKAGE)
         (*READTABLE* SI:INITIAL-COMMON-LISP-READTABLE)
         (*PRINT-ESCAPE* NIL)
         (*PRINT-CASE* ':UPCASE))
        ((NULL PS))
      (PUSH (LIST (GET-PNAME (CAR PS))
                  (FORMAT:OUTPUT NIL (PRINC (CADR PS))))
            ALIST))
    ;; Get a list of names of attributes in order they appear in the -*- line now.
    (DO ((PS OLD-ATTRIBUTES (CDDR PS))) ((NULL PS))
      (PUSH (GET-PNAME (CAR PS)) OLD-ATTRIBUTE-NAMES))
    (SETQ OLD-ATTRIBUTE-NAMES (NREVERSE OLD-ATTRIBUTE-NAMES))
    ;; Sort the new ones into the same order.
    ;; All new ones come after all old; new ones are alphabetized.
    (SORTCAR ALIST #'(LAMBDA (AT1 AT2)
                       (LET ((TEM1 (SYS:MEMBER-EQUALP AT1 OLD-ATTRIBUTE-NAMES))
                             (TEM2 (SYS:MEMBER-EQUALP AT2 OLD-ATTRIBUTE-NAMES)))
                         (IF (AND TEM1 TEM2)
                             (SYS:MEMBER-EQUALP AT1 TEM1)
                           (OR TEM1
                               (AND (NOT TEM2)
                                    (STRING-LESSP AT1 AT2)))))))
    (LET (TEM)
      ;; Put the package near the front.
      (IF (SETQ TEM (ASS 'EQUALP "PACKAGE" ALIST))
          (SETQ ALIST (CONS TEM (DELQ TEM ALIST))))
      ;; Put Common-Lisp next to the mode.
      (IF (SETQ TEM (ASS 'EQUALP "COMMON-LISP" ALIST))
          (SETQ ALIST (CONS TEM (DELQ TEM ALIST))))
      ;; Ignore what PLIST says for the :MODE.
      (IF (SETQ TEM (ASS 'EQUALP "MODE" ALIST))
          (SETQ ALIST (DELQ TEM ALIST)))
      ;; Put the buffer's actual mode on, at the very front.
      (PUSH (LIST "Mode" (SYMEVAL (SEND BUFFER ':MAJOR-MODE))) ALIST))
    (SETQ LINE (BP-LINE START-BP))
    (LET (IDX END-BP)
      (COND ((SETQ IDX (STRING-SEARCH "-*-" LINE))
             ;; Put on a comment starter if there isn't one already.
             (WHEN *COMMENT-START*
               (LET ((START-START (FIND-COMMENT-START LINE)))
                 (UNLESS (AND START-START (< START-START IDX))
                   (SETQ IDX
                         (BP-INDEX
                           (INSERT-MOVING (BACKWARD-OVER *BLANKS*
                                                         (CREATE-BP LINE IDX))
                                          (IF ;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
                                            (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                                              ";;; "
                                            *COMMENT-BEGIN*))))
                   (WHEN (AND *COMMENT-END* (PLUSP (LENGTH *COMMENT-END*)))
                     (INSERT (END-OF-LINE LINE) *COMMENT-END*)))))
             (SETQ START-BP (CREATE-BP LINE (SETQ IDX (+ IDX 3))))
             (SETQ IDX (STRING-SEARCH "-*-" LINE IDX))
             (IF IDX
                 (SETQ END-BP (CREATE-BP LINE IDX))
               (SETQ END-BP (END-LINE start-bp))
               (INSERT END-BP " -*-"))
             (DELETE-INTERVAL START-BP END-BP)
             (SETQ START-BP (INSERT START-BP " "))
             (INSERT START-BP " "))
            (T
             ;;special kludge for tex, since start of line 1 is sacred.
             (COND ((EQ *MAJOR-MODE* 'TEX-MODE)
                    (SETQ START-BP (INSERT (CREATE-BP LINE (LINE-LENGTH LINE))
                                           "  % ")))
                   (;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) ':LISP)
                    (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                    (SETQ START-BP (INSERT START-BP ";;; ")))
                   (*COMMENT-START* (SETQ START-BP (INSERT START-BP *COMMENT-BEGIN*))))
             (SETQ START-BP (INSERT START-BP "-*- "))
             (SETQ END-BP (INSERT START-BP " -*-"))
             (COND ((NOT (MEMBER *COMMENT-END* '(NIL "")))
                    (INSERT-MOVING END-BP #/SPACE)
                    (INSERT-MOVING END-BP *COMMENT-END*)))
             (OR (EQ *MAJOR-MODE* 'TEX-MODE) (INSERT END-BP #/RETURN)))))
    (DO ((LIST ALIST (CDR LIST)))
        ((NULL LIST))
      (INSERT-MOVING START-BP (STRING-CAPITALIZE-WORDS (CAAR LIST) T NIL))
      (INSERT-MOVING START-BP #/:)
      (INSERT-MOVING START-BP (CADAR LIST))
      (AND (OR (CDR LIST))              ;If more to come or some there already
           (INSERT-MOVING START-BP "; ")))))

(DEFUN PREVIOUS-BUFFER (&REST NOT-THESE-BUFFERS)
  "Return most recently selected buffer except for selected buffer and NOT-THESE-BUFFERS."
  (DOLIST (B (buffer-list-as-selected))
    (AND (NEQ B *INTERVAL*)
         (NOT (MEMQ B NOT-THESE-BUFFERS))
         (RETURN B))))
