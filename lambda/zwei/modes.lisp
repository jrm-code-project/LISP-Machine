;;; -*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:8 -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Functions to implement major and minor modes.

;;; NOTE: self-doc in EMACS mode, and recursive control-R for wordab still need
;;; to be done.  NOTE: The worthless TECO mode command have not yet been converted.

;;; Two specials, bound at every ZWEI invocation, are used to keep track of
;;; modes.  *MODE-LIST* is a list, each of whose elements speaks of a mode that
;;; is in effect.  The most recent mode entered is at the head of the list, and
;;; the least recent at the end.  Modes are turned on and off in a stack-like
;;; fashion, to prevent a bug in which keys end up redefined even after all
;;; modes are turned off.

;;; Each mode is known by a symbol, such as AUTO-FILL-MODE.  The symbol has a
;;; MODE property, the value of which is a list of Lisp forms.  These forms are
;;; very restricted; they must be SETQ, ASET, SET-COMTAB, PUSH, SET-CHAR-SYNTAX,
;;; SET-SYNTAX-TABLE-INDIRECTION or COMMAND-HOOK forms.  They are interpreted
;;; by ZWEI rather than Lisp, because ZWEI figures out how to undo them, and
;;; generates a list of forms to undo their effects.  An element of the
;;; *MODE-LIST* consists of a 2-list of the mode symbol, and the ZWEI-generated
;;; list of undoing forms.

;;; The symbol *MAJOR-MODE* is the symbol for the major mode currently in
;;; effect, if any (e.g. LISP-MODE).  When a new major mode is entered, the old
;;; one is exited automatically.

;;; The value of a mode symbol is a string used to display that mode, and it
;;; has a MAJOR-MODE-P property which is T if it is a major-mode symbol.

;;; Modes are defined with DEFMAJOR or DEFMINOR.
;;; The syntax is the nearly the same:

;;; (DEFMAJOR <command-name> <mode-symbol> <mode-name> <command-documentation>
;;; <command-options> . <body>)

;;; (DEFMINOR <command-name> <mode-symbol> <mode-name> <mode-line-position>
;;; <command-documentation> <command-options> . <body>)

;;; The <mode-name> is the value of the <mode-symbol>.  The
;;; <mode-line-position> is a fixnum that determines where the name goes on
;;; the mode line, it is stored on the MODE-LINE-POSITION property of the
;;; symbol.  The other three things are used to form a DEFCOM for a command
;;; to enter or exit the mode (exiting happens when the argument is zero).
;;; The reason that the <command-name> comes first, and isn't next to the
;;; <command-documentation>, etc., is so that M-. can find the command.
;;; The <body> should consist of only the restricted set of forms mentioned
;;; above.  If you need to use other forms, change the function below to
;;; know how to produce an undo-form.

(DEFCONST *MAJOR-MODE-TRANSLATIONS* '((:SCRIBE . :TEXT)
                                      ;;Added 5/88, compatible with other LMs- KmC
                                      (:COMMONLISP  . :LISP)
                                      (:COMMON-LISP . :LISP))
  "Alist of mode name translations.
The element (:SCRIBE . :TEXT) in this list /"translates/" SCRIBE mode to TEXT mode.
It makes an attempt to turn on SCRIBE mode turn on TEXT mode instead.")

(DEFUN TURN-OFF-MODE (MODE-SYMBOL)
  "Turn off mode MODE-SYMBOL.  Mode symbol is, for example, LISP-MODE.
Does nothing if the mode is not on."
  (LET ((MODE-ELEMENT (ASSQ MODE-SYMBOL *MODE-LIST*)))
    (COND ((NOT (NULL MODE-ELEMENT))
           ;; We must first turn off all more-recently added modes,
           ;; then turn off this mode, and then turn the more-recently added
           ;; modes on in reverse order.  To accomplish the last, backward step
           ;; more elegantly, we use a recursive subfunction.
           (TURN-OFF-MODE-UNDERSCORE *MODE-LIST* MODE-ELEMENT))))
  (SETQ *MODE-NAME-LIST* (DELQ MODE-SYMBOL *MODE-NAME-LIST*)))

;;; Internal recursive subfunction of TURN-OFF-MODE.
(DEFUN TURN-OFF-MODE-UNDERSCORE (LIST ELEMENT)
  (COND ((EQ (CAR LIST) ELEMENT)
         (MAPC #'EVAL (SECOND ELEMENT))
         (SETQ *MODE-LIST* (DELQ ELEMENT *MODE-LIST*)))
        (T
         (MAPC #'EVAL (SECOND (CAR LIST)))
         (TURN-OFF-MODE-UNDERSCORE (CDR LIST) ELEMENT)
         (SETF (SECOND (CAR LIST)) (EVALUATE-FORMING-UNDO-LIST (GET (CAAR LIST) 'MODE))))))

(DEFUN TURN-ON-MODE (MODE-SYMBOL)
  "Turn on mode MODE-SYMBOL.  Mode symbol is, for example, LISP-MODE.
Does nothing if the mode is already on."
  (COND ((NULL (ASSQ MODE-SYMBOL *MODE-LIST*))
         (COND ((GET MODE-SYMBOL 'MAJOR-MODE-P)
                (SETQ *MAJOR-MODE* MODE-SYMBOL)))
         (PUSH (LIST MODE-SYMBOL (EVALUATE-FORMING-UNDO-LIST (GET MODE-SYMBOL 'MODE)))
               *MODE-LIST*)))
  (COND ((NULL (MEMQ MODE-SYMBOL *MODE-NAME-LIST*))
         (PUSH MODE-SYMBOL *MODE-NAME-LIST*)))
  (COND ((GET MODE-SYMBOL 'MAJOR-MODE-P)
         (DOLIST (MINOR *INITIAL-MINOR-MODES*)
           (TURN-ON-MODE MINOR))))
  (LET ((HOOK (GET MODE-SYMBOL 'MODE-HOOK-SYMBOL)))
    (AND HOOK (BOUNDP HOOK) (FUNCALL (SYMEVAL HOOK))))
  (SORT *MODE-NAME-LIST* #'(LAMBDA (X Y) (< (GET X 'MODE-LINE-POSITION)
                                            (GET Y 'MODE-LINE-POSITION)))))

;;; Take a list of forms, evaluate them, and produce a list of forms which,
;;; when evaluated, will undo the effects of the evaluatation of the original list.
;;; These forms are very restricted; they must be SETQ, ASET, SET-COMTAB,
;;; PUSH, SET-CHAR-SYNTAX, SET-SYNTAX-TABLE-INDIRECTION or COMMAND-HOOK forms.
(DEFUN EVALUATE-FORMING-UNDO-LIST (FORM-LIST)
  (LET ((RESULT NIL))
    (DOLIST (FORM FORM-LIST)
      (SELECTQ (CAR FORM)
        (SETQ
         (OR (GET (SECOND FORM) 'MODE-SETTABLE-P)
             (FERROR NIL "Illegal to SETQ ~A inside a mode macro" (SECOND FORM)))
         (PUSH `(SETQ ,(SECOND FORM) ',(SYMBOL-VALUE (SECOND FORM))) RESULT)
         (EVAL FORM))
        (ASET
         (PUSH `(ASET ',(EVAL `(AREF . ,(CDDR FORM))) . ,(CDDR FORM)) RESULT)
         (EVAL FORM))
        (SET-COMTAB
         ;; Knowledge of how to reverse SET-COMTAB is kept in the COMTAB > file.
         (PUSH (MAKE-SET-COMTAB-UNDO-LIST FORM) RESULT)
         (EVAL FORM))
        (PUSH
         (LET ((THING (EVAL (SECOND FORM))))
           (PUSH `(SETF ,(THIRD FORM)
                        (DELQ ',THING ,(THIRD FORM)))
                 RESULT)
           (EVAL `(PUSH ',THING ,(THIRD FORM)))))
        (COMMAND-HOOK
         (LET ((THING (EVAL (SECOND FORM))))
           (PUSH `(SETF ,(THIRD FORM)
                        (DELQ ',THING ,(THIRD FORM)))
                 RESULT)
           (COMMAND-HOOK THING (THIRD FORM))))
        (SET-CHAR-SYNTAX
         (LET ((SYNTAX-TABLE (SYMEVAL (THIRD FORM)))
               (CHAR (FOURTH FORM)))
           (PUSH `(SET-CHAR-SYNTAX ,(CHAR-SYNTAX CHAR SYNTAX-TABLE) ',SYNTAX-TABLE ,CHAR)
                 RESULT))
         (EVAL FORM))
        (SET-MODE-LINE-LIST
         (PUSH `(SET-MODE-LINE-LIST ',*MODE-LINE-LIST*) RESULT)
         (EVAL FORM))
        (SET-SYNTAX-TABLE-INDIRECTION
         (LET ((OF (SYMEVAL (SECOND FORM)))
               (TO (SYMEVAL (THIRD FORM))))
           (PUSH `(RPLACA ',OF ',(CAR OF)) RESULT)
           (RPLACA OF TO)))
        (PROGN
         (EVAL FORM))
        (OTHERWISE
         (FERROR NIL "The form ~S cannot be used in a mode, because I can't invert it."
                 FORM))))
    RESULT))

;;; Turn off all modes.  For ZMACS.
(DEFUN UN-SET-MODES ()
  "Turns off all modes that are on."
  (DOLIST (V *LOCAL-BOUND-VARIABLES*)
    (EH:DELETE-BINDING-FROM-CLOSURE *EDITOR* V))
  (DOLIST (V *LOCAL-VARIABLES*)
    (UNLESS (MEMQ V *LOCAL-BOUND-VARIABLES*)
      (SET V (SI:SYMEVAL-GLOBALLY V))))
  (DOLIST (L *MODE-LIST*)
    (MAPC #'EVAL (SECOND L)))
  (SETQ *MODE-NAME-LIST* NIL
        *MODE-LIST* NIL)
  (SETQ *LOCAL-VARIABLES* NIL)
  (SETQ *LOCAL-BOUND-VARIABLES* NIL))

;;; Turn on a saved set of modes.  For ZMACS.
(DEFUN SET-MODES (MODE-LIST MAJOR-MODE &OPTIONAL LOCAL-VARIABLES)
  "Turn on the modes in MODE-LIST, and major mode MAJOR-MODE.
MAJOR-MODE, and the elements of MODE-LIST, are mode symbols, like LISP-MODE."
  (SET-MODES-UNDERSCORE MODE-LIST)
  (SETQ *MAJOR-MODE* MAJOR-MODE)
  (TURN-ON-MODE *MAJOR-MODE*)
  (DOLIST (V LOCAL-VARIABLES)
    (MAKE-LOCAL-VARIABLE (CAR V) NIL (CDR V))
    (SET (CAR V) (CADR V))))

(DEFUN SET-MODES-UNDERSCORE (LIST)
  (COND ((NULL LIST) NIL)
        (T
         (SET-MODES-UNDERSCORE (CDR LIST))
         (PUSH (LIST (CAAR LIST) (EVALUATE-FORMING-UNDO-LIST (GET (CAAR LIST) 'MODE)))
               *MODE-LIST*)
         (SETQ *MODE-NAME-LIST* (NCONC *MODE-NAME-LIST* (NCONS (CAAR LIST)))))))

(DEFUN MAKE-LOCAL-VARIABLE (VARIABLE &OPTIONAL (VALUE NIL VALUEP) XVCELL)
  "Make VARIABLE be bound locally in the editor that is running.
If XVCELL is non-NIL, we use it as the closure value cell for the variable
 if the variable does not already have one.
If XVCELL is NIL, then if VALUE was specified, that becomes the
 new value of the variable; otherwise, the value does not change."
  ;; Make sure that our editor closure includes *LOCAL-VARIABLES*
  ;; This is a kludge to get around the fact that in system 98
  ;; all the closure variable lists omit *LOCAL-VARIABLES*.
  ;; The following four lines are not needed after system 98.
;  (UNLESS *LOCAL-VARIABLES*
;    (EH:INSERT-BINDING-IN-CLOSURE *EDITOR* '*LOCAL-VARIABLES* '*EDITOR*))
;  (UNLESS *LOCAL-BOUND-VARIABLES*
;    (EH:INSERT-BINDING-IN-CLOSURE *EDITOR* '*LOCAL-BOUND-VARIABLES* '*EDITOR*))
  (UNLESS (MEMQ VARIABLE *LOCAL-VARIABLES*)
    (PUSH VARIABLE *LOCAL-VARIABLES*)
    (IF (SYS:INSERT-BINDING-IN-CLOSURE *EDITOR* VARIABLE '*EDITOR* XVCELL)
        (PUSH VARIABLE *LOCAL-BOUND-VARIABLES*)
      (WHEN XVCELL
        (SET VARIABLE (CAR XVCELL)))))
  (UNLESS XVCELL
    (WHEN VALUEP
      (SET VARIABLE VALUE)))
  (WHEN (TYPEP *INTERVAL* 'ZMACS-BUFFER)
    ;; Put this variable, with correct vcell or value,
    ;; on the buffer's local variable list.  Don't wait for a buffer switch.
    (LET ((AELT (ASSQ VARIABLE (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*))))
      (UNLESS AELT
        (SETQ AELT (CONS VARIABLE NIL))
        (PUSH AELT (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)))
      (IF (MEMQ VARIABLE *LOCAL-BOUND-VARIABLES*)
          (SETF (CDR AELT) (%P-CONTENTS-AS-LOCATIVE (VALUE-CELL-LOCATION VARIABLE)))
        (SETF (CDR AELT) (LIST (SYMEVAL VARIABLE)))))))

(DEFUN KILL-LOCAL-VARIABLE (VARIABLE)
  "Make VARIABLE no longer be bound locally in the editor that is running.
Its value reverts to the global value."
  (WHEN (TYPEP *INTERVAL* 'ZMACS-BUFFER)
    (SETF (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*)
          (CLI:DELETE VARIABLE (BUFFER-SAVED-LOCAL-VARIABLES *INTERVAL*) :KEY 'CAR)))
  (WHEN (MEMQ VARIABLE *LOCAL-BOUND-VARIABLES*)
    (SYS:DELETE-BINDING-FROM-CLOSURE *EDITOR* VARIABLE)
    (SETQ *LOCAL-BOUND-VARIABLES* (DELQ VARIABLE *LOCAL-BOUND-VARIABLES*)))
  (SETQ *LOCAL-VARIABLES* (DELQ VARIABLE *LOCAL-VARIABLES*)))

;;; For the mode line list.
(DEFUN NAME-OF-MAJOR-MODE ()
  "Return the pretty name of the current major mode."
  (SYMBOL-VALUE *MAJOR-MODE*))

(DEFUN GET-FILE-MAJOR-MODE (MODE-PROP &AUX MODE)
  "Given a keyword such as :LISP, return the major mode symbol such as LISP-MODE."
  (AND MODE-PROP
       (SYMBOLP MODE-PROP)
       (PROGN
         (SETQ MODE-PROP (OR (CDR (ASSQ MODE-PROP *MAJOR-MODE-TRANSLATIONS*)) MODE-PROP))
         (AND (SETQ MODE (INTERN-SOFT (STRING-APPEND (GET-PNAME MODE-PROP) "-MODE")
                                      (SYMBOL-PACKAGE 'FOO)))
              (BOUNDP MODE)
              (STRING-EQUAL (SYMEVAL MODE) (SYMBOL-NAME MODE-PROP))
              MODE))))

(DEFUN STICKY-MODE-LIST ()
  "Return a list of minor modes currently on that should be sticky.
Sticky carry forward to a new buffer from the current buffer."
  (AND (BOUNDP '*MODE-LIST*)                    ;Somewhat of a kludge
       (DO ((MODES *MODE-LIST* (CDR MODES))
            (NMODES NIL))
           ((NULL MODES)
            (NREVERSE NMODES))
         (AND (MEMQ (CAAR MODES) *STICKY-MINOR-MODES*)
              (PUSH (CAR MODES) NMODES)))))

(DEFUN TURN-ON-EMACS-MODE ()
  (OR (MEMQ 'EMACS-MODE *INITIAL-MINOR-MODES*)
      (LOGIN-SETQ *INITIAL-MINOR-MODES* (CONS 'EMACS-MODE *INITIAL-MINOR-MODES*))))

(DEFUN TURN-OFF-EMACS-MODE ()
  (LOGIN-SETQ *INITIAL-MINOR-MODES* (REMQ 'EMACS-MODE *INITIAL-MINOR-MODES*)))

;;;; Major modes.

;;; lets modes frob this variable
(DEFPROP *READTABLE* T MODE-SETTABLE-P)
(DEFPROP *WORD-SYNTAX-TABLE* T MODE-SETTABLE-P)
(DEFPROP *ATOM-WORD-SYNTAX-TABLE* T MODE-SETTABLE-P)
(DEFPROP *LIST-SYNTAX-TABLE* T MODE-SETTABLE-P)

;;Function for pre-setting a buffer's readtable and syntax attributes
;;from the mode actually present on the attribute line.
;;This is used to pre-determine CommonLISP-ishness.
(defun set-lisp-attributes-from-mode-with-translations (&optional (buffer *interval*) &aux realmode)
  (declare(values set-attributes error-flag))
  (check-type buffer node "a ZWEI buffer")
  (cond
    ;;If no real mode determined, punt
    ((null (setq realmode (send buffer :get-attribute :mode))) (values nil :no-mode-attribute))
    ;;Must be a LISP mode buffer
    ((not (eq (send buffer :major-mode) 'lisp-mode)) (values nil :not-lisp-mode))
    ;;If mode says LISP, default as usual
    ((string-equal realmode "LISP") (values nil :must-be-zetalisp))
    ;;If mode is not a valid LISP mode translation, punt.
    ;;See the global *major-mode-translations* for OK modes.
    ((not (string-equal (cdr (assoc realmode *major-mode-translations*)) "LISP"))
     (values nil :not-translated-to-lisp))
    ;;If mode says COMMON-LISP or COMMONLISP, set stuff.
    ((mem #'string-equal realmode '(:COMMON-LISP :COMMONLISP))
     (or (send buffer :get-attribute :readtable)
         (send buffer :set-attribute :readtable :CL))
     (or (send buffer :get-attribute :syntax)
         (send buffer :set-attribute :syntax :CL))
     t)
    ;;Apparently the mode is some other translation that may not
    ;;be Common-LISP.
    (t nil)))

(DEFMAJOR COM-LISP-MODE LISP-MODE "LISP"
  "Sets things up for editing Lisp code.
Puts Indent-For-Lisp on Tab, sets readtable,
sets escape character handling, etc."
  ()
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-INDENT-FOR-LISP
                              #/RUBOUT COM-TAB-HACKING-RUBOUT
                              #/C-RUBOUT COM-RUBOUT
                              #/M-Z COM-COMPILE-AND-EXIT
                              #/C-M-Z COM-EVALUATE-AND-EXIT))
  ;;Pre-set readtable, syntax from real mode [e.g, Mode:COMMON-LISP]. -KmC
  (progn(set-lisp-attributes-from-mode-with-translations))
  (SETQ *READTABLE* (OR (SI:FIND-READTABLE-NAMED
                          (SEND *INTERVAL* :GET-ATTRIBUTE :READTABLE)
                          :FIND)
                        *READTABLE*
                        si:standard-readtable))
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))

(DEFPROP LISP-MODE T ALL-UPPERCASE)

(DEFPROP LISP-MODE :LISP EDITING-TYPE)

;;;Common-LISP is not really a "mode", just a friendly command.
;;;This is not the same as SET-COMMON-LISP, which sets to T or NIL,
;;;and doesn't affect attributes besides readtable. -KmC

(defcom com-common-lisp-mode
        "Set things up for editing CommonLISP code.
Sets LISP mode and CL readtable.  Also (re)computes base and package
from the buffer attributes. Note: not a true mode.  Updating the
attribute list again will set the mode line to LISP mode, which is
compatible."
        ()
  (com-lisp-mode)
  (setq *readtable* (or si:common-lisp-readtable
                        (si:find-readtable-named "CL")))
  (SEND *INTERVAL* :SET-ATTRIBUTE :READTABLE
        (DONT-OPTIMIZE (SI:RDTBL-SHORT-NAME *READTABLE*))
        :QUERY)
  ;;For Brand S compatibility, do:
  (send *interval* :set-attribute :syntax "CL")
  ;;Reparse other attributes
  (compute-buffer-package *interval*)
  dis-none)

(DEFMAJOR COM-SCHEME-MODE SCHEME-MODE "SCHEME"
  "Sets things up for editing Scheme code.
Puts Indent-For-Lisp on Tab."
  ()
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-INDENT-FOR-LISP
                              #/RUBOUT COM-TAB-HACKING-RUBOUT
                              #/C-RUBOUT COM-RUBOUT
                              #/break com-scheme-break
                              #/m-. com-scheme-edit-definition
                              ))
  (SETQ *READTABLE* (or (and (send *interval* :get-attribute :readtable)
                             (si:find-readtable-named
                               (send *interval* :get-attribute :readtable)
                               :find))
                        (and (find-package "CLSCH")
                             (boundp (intern "SCHEME-READTABLE" "CLSCH"))
                             (symbol-value (intern "SCHEME-READTABLE" "CLSCH")))
                        *READTABLE*
                        SI:STANDARD-READTABLE
                        ))

  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))

(defvar scheme-mode-indent-alist
        '(("IF" 2 3)
          ("RECEIVE" 1 3 2 1)))

(defun add-scheme-indentation ()
  (dolist (indentation scheme-mode-indent-alist)
    (pushnew (cons (intern (first indentation) "SCHEME") (rest indentation))
             *lisp-indent-offset-alist*
             :test #'equal)))

(defun scheme-mode-hook-function ()
  (cond ((null (find-package "CLSCH"))
         (format t "~&No CLSCH Package - you will lose."))
        ((null (find-package "SCHEME"))
         (format t "~&No SCHEME Package - you will lose."))
        (t
         (add-scheme-indentation)
         (let ((evaluator (intern "SCHEME-EVAL-FOR-ZWEI" "CLSCH"))
               (compiler (intern "SCHEME-COMPILE" "CLSCH")))
           (when (null (send *interval* :get-attribute :evaluator))
             (send *interval* :putprop evaluator :evaluator))
           (when (null (send *interval* :get-attribute :compiler))
             (send *interval* :putprop compiler :compiler))
           (when (not (eq (find-package (send *interval* :get-attribute :package))
                          (find-package "SCHEME")))
             (format *query-io* "~&Changing package to SCHEME.")
             (let ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM *interval*)))
                   (scheme-package (find-package "SCHEME")))
               (setf (buffer-package *interval*) scheme-package)
               (send *interval* :putprop scheme-package :package)
               (SETF (GETF ATTRIBUTES :package) scheme-package)
               (let ((move-to-next-line-p (and (null (line-previous (bp-line (point))))
                                               (null (line-next (bp-line (point)))))))
                 (STORE-ATTRIBUTE-LIST *interval* attributes)
                 (when move-to-next-line-p
                   (MOVE-BP (point) (line-next (bp-line (INTERVAL-FIRST-BP *INTERVAL*))) 0)))
               (compute-buffer-package *interval*)))
           (when (not (eq (si:find-readtable-named (send *interval* :get-attribute :readtable))
                          (si:find-readtable-named "SCHEME")))
             (format *query-io* "~&Changing readtable to SCHEME.")
             (let ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM *interval*)))
                   (scheme-readtable (si:find-readtable-named "SCHEME")))
               (send *interval* :putprop :SCHEME :readtable)
               (SETF (GETF ATTRIBUTES :readtable) :SCHEME)
               (let ((move-to-next-line-p (and (null (line-previous (bp-line (point))))
                                               (null (line-next (bp-line (point)))))))
                 (STORE-ATTRIBUTE-LIST *interval* attributes)
                 (when move-to-next-line-p
                   (MOVE-BP (point) (line-next (bp-line (INTERVAL-FIRST-BP *INTERVAL*))) 0)))
               (setq *readtable* scheme-readtable))))
         (must-redisplay *window* dis-text)
         )))

(setq scheme-mode-hook 'scheme-mode-hook-function)

(DEFPROP SCHEME-MODE T ALL-UPPERCASE)

(DEFPROP SCHEME-MODE :scheme EDITING-TYPE)

(DEFCOM COM-scheme-BREAK "Enter a SCHEME break loop" ()
  (LET ((*INSIDE-BREAK* T))
    (funcall (intern "SCHEME" "CLSCH")))
  (SEND *STANDARD-OUTPUT* ':MAKE-COMPLETE)
  DIS-NONE)

(defcom com-scheme-edit-definition
        "Just like META-., but fixes readtable first."
        ()
  (let ((*readtable* (si:find-readtable-named "CL")))
    (com-edit-definition)))

(DEFMAJOR COM-T-MODE T-MODE "T"
          "Sets things up for editing T style Scheme code."
          ()
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* 'LISP-FIND-COMMENT-START-AND-END)
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-INDENT-FOR-LISP
                              #/RUBOUT COM-TAB-HACKING-RUBOUT
                              #/C-RUBOUT COM-RUBOUT
                              #/m-. com-scheme-edit-definition
                              ))
  (SETQ *READTABLE* (or (and (send *interval* :get-attribute :readtable)
                             (si:find-readtable-named
                               (send *interval* :get-attribute :readtable)
                               :find))
                        (and (find-package "CLSCH")
                             (boundp (intern "SCHEME-READTABLE" "CLSCH"))
                             (symbol-value (intern "SCHEME-READTABLE" "CLSCH")))
                        *READTABLE*
                        SI:STANDARD-READTABLE
                        ))

  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #//)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #//)
  (SET-CHAR-SYNTAX (IF (= (SI:PTTBL-SLASH *READTABLE*) #/\)
                       LIST-SLASH LIST-ALPHABETIC) *MODE-LIST-SYNTAX-TABLE* #/\))

(defun t-mode-hook-function ()
  (cond ((null (find-package "CLSCH"))
         (format t "~&No CLSCH Package - you will lose."))
        ((null (find-package "T"))
         (format t "~&No T Package - you will lose."))
        (t
         (add-scheme-indentation)
         (let ((evaluator (intern "SCHEME-EVAL-FOR-ZWEI" "CLSCH"))
               (compiler (intern "SCHEME-COMPILE" "CLSCH")))
           (when (null (send *interval* :get-attribute :evaluator))
             (send *interval* :putprop evaluator :evaluator))
           (when (null (send *interval* :get-attribute :compiler))
             (send *interval* :putprop compiler :compiler))
           (when (not (eq (find-package (send *interval* :get-attribute :package))
                          (find-package "T")))
             (let ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM *interval*)))
                   (t-package (find-package "T")))
               (setf (buffer-package *interval*) t-package)
               (send *interval* :putprop t-package :package)
               (SETF (GETF ATTRIBUTES :package) t-package)
               (compute-buffer-package *interval*)))
           (when (not (eq (si:find-readtable-named (send *interval* :get-attribute :readtable))
                          (si:find-readtable-named "SCHEME")))
             (let ((ATTRIBUTES (FS:EXTRACT-ATTRIBUTE-LIST (INTERVAL-STREAM *interval*)))
                   (scheme-readtable (si:find-readtable-named "SCHEME")))
               (send *interval* :putprop :SCHEME :readtable)
               (SETF (GETF ATTRIBUTES :readtable) :SCHEME)
               (setq *readtable* scheme-readtable))))
         (must-redisplay *window* dis-text)
         )))

(setq t-mode-hook 't-mode-hook-function)

(DEFPROP t-MODE T ALL-UPPERCASE)

(DEFPROP t-MODE :scheme EDITING-TYPE)

(DEFMAJOR COM-MIDAS-MODE MIDAS-MODE "MIDAS"
  "Sets things up for editing assembly language code."
  ()
  (SETQ *COMMENT-COLUMN* 256.)
  (SETQ *COMMENT-START* ";")
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/.)
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-INSERT-TAB
                              #/C-M-A COM-GO-TO-AC-FIELD
                              #/C-M-E COM-GO-TO-ADDRESS-FIELD
                              #/C-M-D COM-KILL-TERMINATED-WORD
                              #/C-M-N COM-GO-TO-NEXT-LABEL
                              #/C-M-P COM-GO-TO-PREVIOUS-LABEL)))

(DEFPROP MIDAS-MODE T ALL-UPPERCASE)

(DEFCOM COM-KILL-TERMINATED-WORD "Kill a word and the following character.
If the word is followed by a CRLF, the CRLF is not killed." ()
  (LET ((BP (OR (FORWARD-WORD (POINT)) (BARF))))
    (OR (= (BP-CH-CHAR BP) #/CR) (SETQ BP (FORWARD-CHAR BP 1 T)))
    (KILL-INTERVAL-ARG (POINT) BP 1))
  (SETQ *CURRENT-COMMAND-TYPE* 'KILL)
  DIS-TEXT)

(DEFCOM COM-GO-TO-PREVIOUS-LABEL "Put point after last label.
With an argument, moves after the argth previous label." (KM)
  (LET ((*NUMERIC-ARG* (- *NUMERIC-ARG*)))
    (COM-GO-TO-NEXT-LABEL)))

(DEFCOM COM-GO-TO-NEXT-LABEL "Put point after the next label.
With an argument, moves after the argth label." (KM)
  (LET ((ARG (ABS *NUMERIC-ARG*))
        (SIGN (IF (MINUSP *NUMERIC-ARG*) -1 1))
        (POINT (POINT)))
    (DO ((I 0 (1+ I))
         (BP (BEG-LINE POINT)))
        (NIL)
      (DO NIL (NIL)
        (OR (MEMQ (BP-CH-CHAR BP) '(#/* #/SP #/TAB #/CR))
            (STRING-EQUAL (BP-LINE BP) *COMMENT-START* :START1 0 :START2 0
                          :END1 (STRING-LENGTH *COMMENT-START*))
            (RETURN NIL))
        (OR (SETQ BP (BEG-LINE BP SIGN)) (BARF)))
      (COND (( I ARG)
             (LET ((LINE (BP-LINE BP)))
               (MOVE-BP BP LINE
                        (OR (STRING-SEARCH-SET *BLANKS* LINE) (LINE-LENGTH LINE))))
             (COND ((IF (MINUSP SIGN) (BP-< BP POINT) (BP-< POINT BP))
                    (MOVE-BP POINT BP)
                    (RETURN NIL)))))))
  DIS-BPS)

(DEFCOM COM-GO-TO-ADDRESS-FIELD "Put point before the address field." (KM)
  (GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL T))

(DEFCOM COM-GO-TO-AC-FIELD "Put point before the accumulator field." (KM)
  (GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL NIL))

(DEFUN GO-TO-ADDRESS-OR-AC-FIELD-INTERNAL (ADDRESS-P &AUX LINE BP)
  (SETQ LINE (BP-LINE (POINT))
        BP (OR (FORWARD-WORD (BEG-LINE (POINT))) (BARF)))
  (OR (MEMQ (BP-CH-CHAR BP) '(#/: #/= #/_))
      (SETQ BP (BEG-LINE BP)))
  (SETQ BP (OR (FORWARD-TO-WORD BP) (BARF)))
  (MOVE-BP BP LINE (OR (STRING-SEARCH-SET *BLANKS* LINE (BP-INDEX BP))
                       (LINE-LENGTH LINE)))
  (LET ((BP1 (FORWARD-OVER *BLANKS* BP)))
    (OR (= (BP-CH-CHAR BP1) #/;)
        (SETQ BP BP1)))
  (COND ((MEMQ (CHAR-CODE (BP-CHAR-BEFORE BP)) *BLANKS*)
         (AND ADDRESS-P
              (LET ((I (STRING-SEARCH-SET '(#/SP #/, #/; #//) LINE (BP-INDEX BP))))
                (AND I (CHAR-EQUAL (AREF LINE I) #/,)
                     (MOVE-BP BP LINE (1+ I)))))
         (MOVE-BP (POINT) BP)
         DIS-BPS)
        (T
         (MOVE-BP (POINT) (INSERT BP " "))
         DIS-TEXT)))

(DEFMAJOR COM-TEXT-MODE TEXT-MODE "Text"
          "Sets things up for editing English text.
Puts Tab-To-Tab-Stop on Tab." ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-TAB-TO-TAB-STOP)))

(DEFPROP TEXT-MODE :TEXT EDITING-TYPE)

(DEFMAJOR COM-BOLIO-MODE BOLIO-MODE "Bolio"
          "Sets things up for editing Bolio source files.
Like Text mode, but also makes c-m-digit and c-m-: and c-m-* do font stuff,
and makes word-abbrevs for znil and zt." ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-TAB-TO-TAB-STOP
                              ;;Next line gets an error, so do it manually
                              ;;(#/C-M-0 10.) COM-BOLIO-INTO-FONT
                              #/C-M-0 COM-BOLIO-INTO-FONT
                              #/C-M-1 COM-BOLIO-INTO-FONT
                              #/C-M-2 COM-BOLIO-INTO-FONT
                              #/C-M-3 COM-BOLIO-INTO-FONT
                              #/C-M-4 COM-BOLIO-INTO-FONT
                              #/C-M-5 COM-BOLIO-INTO-FONT
                              #/C-M-6 COM-BOLIO-INTO-FONT
                              #/C-M-7 COM-BOLIO-INTO-FONT
                              #/C-M-8 COM-BOLIO-INTO-FONT
                              #/C-M-9 COM-BOLIO-INTO-FONT
                              #/C-M-/: COM-BOLIO-OUTOF-FONT
                              #/C-M-* COM-BOLIO-OUTOF-FONT
                              #/C-M-SP COM-EXPAND-ONLY))
  (SETQ *COMMENT-START* ".c ")
  (SETQ *COMMENT-BEGIN* ".c ")
  (SETQ *COMMENT-COLUMN* 0)
  (SETQ *PARAGRAPH-DELIMITER-LIST* (CONS #/' *PARAGRAPH-DELIMITER-LIST*))
  (PROGN (TURN-ON-MODE 'WORD-ABBREV-MODE)
         ;; Set up BOLIO-mode-dependent word abbrevs
         (PUTPROP (INTERN "ZNIL" *UTILITY-PACKAGE*)     ;This stuff loses at top level since
                  "3nil*"                     ;*UTILITY-PACKAGE* not set up at readin time.
                  '|Bolio-ABBREV|)
         (PUTPROP (INTERN "ZT" *UTILITY-PACKAGE*)
                  "3t*"
                  '|Bolio-ABBREV|)))

(DEFPROP BOLIO-MODE :TEXT EDITING-TYPE)

(DEFCOM COM-BOLIO-INTO-FONT "Insert font-change sequence" (NM)
  (LET ((CHAR (CHAR-CODE *LAST-COMMAND-CHAR*))
        (POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
      (INSERT-MOVING POINT #/)
      (INSERT-MOVING POINT CHAR)
      (VALUES DIS-LINE LINE INDEX))))

(DEFCOM COM-BOLIO-OUTOF-FONT "Insert font-change sequence" (NM)
  (LET ((POINT (POINT)))
    (LET ((LINE (BP-LINE POINT)) (INDEX (BP-INDEX POINT)))
      (INSERT-MOVING POINT #/)
      (INSERT-MOVING POINT #/*)
      (VALUES DIS-LINE LINE INDEX))))

(DEFMAJOR COM-FUNDAMENTAL-MODE FUNDAMENTAL-MODE "Fundamental"
  "Return to ZWEI's fundamental mode." ())

(DEFMAJOR COM-PL1-MODE PL1-MODE "PL1"
          "Set things up for editing PL1 programs.
Makes comment delimiters //* and *//, Tab is Indent-For-PL1,
Control-Meta-H is Roll-Back-PL1-Indentation, and Control- (Top-D)
is PL1dcl.  Underscore is made alphabetic for word commands." ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-COMTAB *MODE-COMTAB*
              '(#/TAB COM-INDENT-FOR-PL1
                #/C-M-H COM-ROLL-BACK-PL1-INDENTATION
                #/C-/ COM-PL1DCL
                ))
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* "//*")
  (SETQ *COMMENT-BEGIN* "//* ")
  (SETQ *COMMENT-END* "*//")
  (SETQ *COMMENT-COLUMN* (* 60. 6)))

(DEFMAJOR COM-ELECTRIC-PL1-MODE ELECTRIC-PL1-MODE "Electric PL1!!"
  "REALLY set things up for editing PL1 programs!
Does everything PL1-Mode does:
Makes comment delimiters //* and *//, Tab is Indent-For-PL1,
Control-Meta-H is Roll-Back-PL1-Indentation, and Control- (Top-D)
is PL1dcl.  Underscore is made alphabetic for word commands.
In addition, ; is PL1-Electric-Semicolon, : is PL1-Electric-Colon,
# is Rubout, @ is Clear, \ is Quoted Insert." ()
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (PROGN (OR (BOUNDP 'PL1DCL) (READ-PL1DCL)))
  (SET-COMTAB *MODE-COMTAB*
              '(#/TAB COM-INDENT-FOR-PL1
                #/C-M-H COM-ROLL-BACK-PL1-INDENTATION
                #/C-/ COM-PL1DCL
                #/; COM-PL1-ELECTRIC-SEMICOLON
                #/: COM-PL1-ELECTRIC-COLON
                #/# COM-RUBOUT
                #/@ COM-CLEAR
                #/\ COM-VARIOUS-QUANTITIES
                ))
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* "//*")
  (SETQ *COMMENT-BEGIN* "//* ")
  (SETQ *COMMENT-COLUMN* (* 60. 6))
  (SETQ *COMMENT-END* "*//"))

;;; c mode

(DEFMAJOR COM-C-MODE C-MODE "C"
          "Sets things up for editing C code."
          ()
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* "//*")
  (SETQ *COMMENT-BEGIN* "//* ")
  (SETQ *COMMENT-END* "*//")
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-COMTAB *MODE-COMTAB*
              '(#/hyper-. com-edit-c-definition)
              '(("Read C Tags File" . com-read-c-tags-file)))
  )

(DEFPROP C-MODE :C EDITING-TYPE)

(defcom com-edit-c-definition
  "Edit the definition of a C function."
  ()
  (let* ((func-name (completing-read-from-mini-buffer "Name of a C function:" nil t))
         (func-atom (intern-soft func-name *utility-package*)))
    (if func-atom
        (edit-definition-1 func-atom t)
      (barf)))
  dis-text)

;a ctags file looks like:
; func name      file
;  ASSERT       ../h/param.h    ...
;  BASEPRI      ../h/param.h    ...
;  CHFCONN      ../chunix/chaos.c       ...
;  CHHANGDEV    /usr/include/chaos/dev.h        ...

(defcom com-read-c-tags-file
  "Read a tag file created by the unix program 'ctags'." ()
  (let ((filename (fs:parse-pathname
                    (completing-read-from-mini-buffer "Name of tags file:" nil t))))
    (with-open-file (s filename)
      (do (line
           func source-file
           index
           (host (send filename :host))
           eofp
           )
          (())
        (multiple-value (line eofp)
          (send s :line-in))
        (if eofp (return))
        (cond ((char-equal (aref line 0) #/;))
              (t
               (setq index (string-search-set '(#\space #\tab) line))
               (setq func (intern (substring line 0 index) *utility-package*))
               (setq index (string-search-not-set '(#\space #\tab) line index))
               (setq source-file
                     (send (fs:parse-pathname
                             (substring line index
                                        (string-search-set '(#\space #\tab) line index))
                             host)
                           :canonicalize-directories
                           filename))
               (putprop func (fs:parse-pathname source-file) :source-file-name))))))
  dis-none)

(defun canonicalize-unix-file-name (filename sister-file)
  (let ((whole-pathname (fs:parse-pathname
                          (string-append (send (send sister-file :host) :name)
                                         ":"
                                         (send sister-file :directory-string)
                                         filename))))
    (do ((dir (reverse (send whole-pathname :directory)))
         (newdir nil))
        ((null dir)
         (send whole-pathname :new-directory newdir))
      (cond ((stringp (car dir))
             (push (car dir) newdir)
             (setq dir (cdr dir)))
            ((eq (car dir) :up)
             (setq dir (cddr dir)))
            (t
             (ferror nil "??"))))))



(DEFMAJOR COM-TECO-MODE TECO-MODE "TECO"
  "Set things up for editing (ugh) TECO.
Makes comment delimiters be !* and !. Tab is Indent-Nested,
Meta-' is Forward-Teco-Conditional, and Meta-/" is Backward-Teco-Conditional." ()
  (SET-COMTAB *MODE-COMTAB*
              '(#/TAB COM-INDENT-NESTED
                #/M-/' COM-FORWARD-TECO-CONDITIONAL
                #/M-/" COM-BACKWARD-TECO-CONDITIONAL
                ))
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-START* "!*")
  (SETQ *COMMENT-BEGIN* "!* ")
  (SETQ *COMMENT-END* "!"))

(DEFVAR *MACSYMA-LIST-SYNTAX-TABLE*)
(DEFVAR *MACSYMA-LIST-SYNTAX-LIST*)

(DEFMAJOR COM-MACSYMA-MODE MACSYMA-MODE "MACSYMA"
  "Enter a mode for editing Macsyma code.
Modifies the delimiter dispatch tables appropriately for Macsyma syntax,
makes comment delimiters //* and *//.  Tab is Indent Nested." ()
  (SET-COMTAB *MODE-COMTAB*
              '(#/TAB COM-INDENT-NESTED))
  ;; Tab hacking rubout.
  (SETQ *SPACE-INDENT-FLAG* T)
  (SETQ *PARAGRAPH-DELIMITER-LIST* NIL)
  (SETQ *COMMENT-COLUMN* (* 40. 6))
  (SETQ *COMMENT-START* "//*")
  (SETQ *COMMENT-BEGIN* "//* ")
  (SETQ *COMMENT-END* "*//")
  (PROGN
    (OR (BOUNDP '*MACSYMA-LIST-SYNTAX-TABLE*)
        (SETQ *MACSYMA-LIST-SYNTAX-TABLE* (MAKE-SYNTAX-TABLE *MACSYMA-LIST-SYNTAX-LIST*))))
  (SETQ *LIST-SYNTAX-TABLE* *MACSYMA-LIST-SYNTAX-TABLE*)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/?)
  ;; Also does something like make right bracket point at right paren?
  )

(SETQ *MACSYMA-LIST-SYNTAX-LIST*
    '(
      (#o40 LIST-ALPHABETIC)

      LIST-DELIMITER            ;040 space
      LIST-DELIMITER            ;041 ! ***
      LIST-DOUBLE-QUOTE         ;042 "
      LIST-DELIMITER            ;043 # ***
      LIST-DELIMITER            ;044 $ ***
      LIST-ALPHABETIC           ;045 %
      LIST-DELIMITER            ;046 & ***
      LIST-SINGLE-QUOTE         ;047 '
      LIST-OPEN                 ;050 (
      LIST-CLOSE                ;051 )
      LIST-DELIMITER            ;052 * ***
      LIST-DELIMITER            ;053 + ***
      LIST-DELIMITER            ;054 , ***
      LIST-DELIMITER            ;055 - ***
      LIST-DELIMITER            ;056 . ***
      LIST-DELIMITER            ;057 / ***
      (10. LIST-ALPHABETIC)                     ;DIGITS
      LIST-DELIMITER            ;072 : ***
      LIST-DELIMITER            ;073 ; ***
      LIST-DELIMITER            ;074 < ***
      LIST-DELIMITER            ;075 = ***
      LIST-DELIMITER            ;076 > ***
      LIST-ALPHABETIC           ;077 ?
      LIST-DELIMITER            ;100 @ ***
      (26. LIST-ALPHABETIC)                     ;LETTERS
      LIST-OPEN                 ;133 [ ***
      LIST-SLASH                ;134 \ ***
      LIST-CLOSE                ;135 ] ***
      LIST-DELIMITER            ;136 ^ ***
      LIST-DELIMITER            ;137 _ ***
      LIST-DELIMITER            ;140 ` ***
      (26. LIST-ALPHABETIC)                     ;MORE LETTERS
      LIST-OPEN                 ;173 { ***
      LIST-DELIMITER            ;174 | ***        |
      LIST-CLOSE                ;175 } ***
      LIST-DELIMITER            ;176 ~ ***
      LIST-ALPHABETIC           ;177 integral ???

      LIST-ALPHABETIC           ;200 null character
      LIST-DELIMITER            ;201 break
      LIST-DELIMITER            ;202 clear
      LIST-DELIMITER            ;203 call
      LIST-DELIMITER            ;204 escape (NOT altmode!)
      LIST-DELIMITER            ;205 backnext
      LIST-DELIMITER            ;206 help
      LIST-DELIMITER            ;207 rubout
      LIST-ALPHABETIC           ;210 bs
      LIST-DELIMITER            ;211 tab
      LIST-DELIMITER            ;212 line
      LIST-DELIMITER            ;213 vt
      LIST-DELIMITER            ;214 form = newpage
      LIST-DELIMITER            ;215 return = newline
      (#o162 LIST-ALPHABETIC)))

(DEFMAJOR COM-TEX-MODE TEX-MODE "TeX"
  "Set up things for editing TeX files
Gives paren syntax to {[]}, makes \ escape, % comment, tab like text." ()
  (SET-CHAR-SYNTAX LIST-COMMENT    *MODE-LIST-SYNTAX-TABLE* #/%)
  (SET-CHAR-SYNTAX LIST-DELIMITER  *MODE-LIST-SYNTAX-TABLE* #//)
  (SET-CHAR-SYNTAX LIST-DELIMITER  *MODE-LIST-SYNTAX-TABLE* #/;)
  (SET-CHAR-SYNTAX LIST-OPEN       *MODE-LIST-SYNTAX-TABLE* #/[)
  (SET-CHAR-SYNTAX LIST-SLASH      *MODE-LIST-SYNTAX-TABLE* #/\)
  (SET-CHAR-SYNTAX WORD-DELIMITER  *MODE-WORD-SYNTAX-TABLE* #/\)
  (SET-CHAR-SYNTAX LIST-CLOSE      *MODE-LIST-SYNTAX-TABLE* #/])
  (SET-CHAR-SYNTAX LIST-OPEN       *MODE-LIST-SYNTAX-TABLE* #/{)
  (SET-CHAR-SYNTAX WORD-DELIMITER  *MODE-WORD-SYNTAX-TABLE* #/{)
  (SET-CHAR-SYNTAX LIST-DELIMITER  *MODE-LIST-SYNTAX-TABLE* #/|)
  (SET-CHAR-SYNTAX LIST-CLOSE      *MODE-LIST-SYNTAX-TABLE* #/})
  (SET-CHAR-SYNTAX WORD-DELIMITER  *MODE-WORD-SYNTAX-TABLE* #/})
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/_)
  (SET-CHAR-SYNTAX WORD-ALPHABETIC *MODE-WORD-SYNTAX-TABLE* #/')
  (SET-COMTAB *MODE-COMTAB* '(#/TAB COM-STUPID-TAB))
  (SETQ *PARAGRAPH-DELIMITER-LIST* (CONS #/\ *PARAGRAPH-DELIMITER-LIST*))
  (SETQ *COMMENT-START* "%")
  (SETQ *COMMENT-BEGIN* "% ")
  (SETQ *COMMENT-END* ""))

(DEFPROP TEX-MODE :TEXT EDITING-TYPE)

;;;; Minor modes.

(DEFMINOR COM-RETURN-INDENTS-MODE RETURN-INDENTS-MODE "Return indents" 1
  "Minor mode in which Return indents and Line does not.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
          ()
  (SETQ *INDENT-NEW-LINE-NEW-LINE-FUNCTION* 'COM-INSERT-CRS)
  (SET-COMTAB *MODE-COMTAB* '(#/RETURN COM-INDENT-NEW-LINE
                              #/LINE COM-INSERT-CRS)))

(DEFMINOR COM-ATOM-WORD-MODE ATOM-WORD-MODE "" 1
  "Minor mode in which all word commands act on Lisp atoms.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
          ()
  (SET-SYNTAX-TABLE-INDIRECTION *MODE-WORD-SYNTAX-TABLE* *ATOM-WORD-SYNTAX-TABLE*))

(DEFMINOR COM-EMACS-MODE EMACS-MODE "Emacs" 1
  "Minor mode to provide commands for EMACS users.
This is for people who have used EMACS from non-TV keyboards for a long
time and are not yet adjusted to the more winning commands.  It puts
bit prefix commands on Altmode, Control-^ and Control-C.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
          ()
  (SET-COMTAB *MODE-COMTAB* '(#/C-/^ COM-PREFIX-CONTROL
                              #/ COM-PREFIX-META
                              #/C-C COM-PREFIX-CONTROL-META
                              #/C-I (0 #/TAB)
                              #/C-H (0 #/BS)
                              #/C-/] (0 #/ABORT))))

;;; Gets a single character from the user.  If HIGHBITSP is true, does not
;;; strip the control and meta bis.
(DEFUN GET-ECHO-CHAR (PROMPT HIGHBITSP &AUX CHAR)
  (DISCARD-LAST-PROMPT)
  (ADD-PROMPT PROMPT)
  (SETQ CHAR (INPUT-WITH-PROMPTS STANDARD-INPUT :TYI))
  (OR HIGHBITSP (SETQ CHAR (CHAR-CODE CHAR)))
  CHAR)

(DEFCOM COM-PREFIX-CONTROL DOCUMENT-PREFIX-CHAR ()
  (PROCESS-PREFIX-COMMAND-CHAR (DPB 1 %%KBD-CONTROL (GET-ECHO-CHAR "Control-" NIL))))

(DEFCOM COM-PREFIX-META DOCUMENT-PREFIX-CHAR ()
  (PROCESS-PREFIX-COMMAND-CHAR (LOGIOR (DPB 1 %%KBD-META 0)
                                       (GET-ECHO-CHAR "Meta-" (EQ *LAST-COMMAND-CHAR* #/)))))

(DEFCOM COM-PREFIX-CONTROL-META DOCUMENT-PREFIX-CHAR ()
  (PROCESS-PREFIX-COMMAND-CHAR
    (DPB 1 %%KBD-CONTROL (DPB 1 %%KBD-META (GET-ECHO-CHAR "Control-Meta-" NIL)))))

(DEFUN PROCESS-PREFIX-COMMAND-CHAR (KEY &AUX VALUE)
  (SETQ VALUE (PROCESS-COMMAND-CHAR KEY))
  (IF (EQ VALUE :ARGUMENT) VALUE DIS-NONE))

(DEFUN DOCUMENT-PREFIX-CHAR (COMMAND IGNORE OP &AUX COLNUM)
  (SETQ COLNUM (CDR (ASSQ COMMAND '((COM-PREFIX-CONTROL . 1)
                                    (COM-PREFIX-META . 2)
                                    (COM-PREFIX-CONTROL-META . 3)))))
  (SELECTQ OP
    (:NAME (GET COMMAND 'COMMAND-NAME))
    (:SHORT (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix." (1- COLNUM)))
    (:FULL (FORMAT T "Set the ~[Control~;Meta~;Control-Meta~] prefix.
Make the next character act as if it were typed with ~[CTRL~;META~;CTRL and META~]
held down, just as if you were on a losing terminal that doesn't
support all of the wonderful keys that we cleverly provide on these marvelous keyboards.
Type a subcommand to document (or /"*/" for all): " (1- COLNUM) (1- COLNUM))
           (LET ((CHAR (SEND STANDARD-INPUT :TYI)))
             (COND ((= CHAR #/*)
                    (FORMAT T "~2%The following ~[Control~;Meta~;Control-Meta~]- commands are availible:~%" (1- COLNUM))
                    (LET ((N (%LOGDPB COLNUM %%KBD-CONTROL-META 0)))
                      (DO ((I N (1+ I))
                           (LIM (+ N 220)))
                          (( I LIM))
                        (PRINT-SHORT-DOC-FOR-TABLE I *COMTAB* 3))))
                   (T (SETQ CHAR (%LOGDPB COLNUM %%KBD-CONTROL-META CHAR))
                      (FORMAT T "~:C~2%" CHAR)
                      (DOCUMENT-KEY CHAR *COMTAB*)))))))

(DEFCOM COM-UNIVERSAL-ARGUMENT "Sets argument or multiplies it by four.
Followed by digits, uses them to specify the
argument for the command after the digits.
Not followed by digits, multiplies the argument by four." ()
  (SETQ *NUMERIC-ARG-P* :CONTROL-U)
  (DO ((FIRSTP T NIL)
       (MINUSP NIL)
       (DIGITP NIL)
       (NUM 1)
       (CHAR)
       )
      (NIL)
    (UNLESS (SEND STANDARD-INPUT :LISTEN)
      (REDISPLAY-ALL-WINDOWS))
    (SETQ CHAR (INPUT-WITH-PROMPTS STANDARD-INPUT :TYI))
    (COND ((AND FIRSTP (= CHAR #/-))
           (SETQ MINUSP T
                 *NUMERIC-ARG-P* :SIGN))
          (( #/0 CHAR #/9)
           (COND (DIGITP (SETQ NUM (+ (- CHAR #/0) (* NUM 10.))))
                 (T (SETQ NUM (- CHAR 60)
                          *NUMERIC-ARG-P* :DIGITS
                          DIGITP T))))
          (T
           (COND ((OR MINUSP DIGITP)
                  (SETQ *NUMERIC-ARG* (IF MINUSP (MINUS NUM) NUM)))
                 (T (SETQ *NUMERIC-ARG* (* 4 *NUMERIC-ARG*))))
           (SEND STANDARD-INPUT :UNTYI CHAR)
           (DISCARD-LAST-PROMPT)
           (RETURN :ARGUMENT)))))

(DEFMINOR COM-ANY-BRACKET-MODE ANY-BRACKET-MODE "{[()]}" 4
  "Minor mode in which List commands treat [{}] like parens.
This makes them easier to balance, etc.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
  ()
  (SET-CHAR-SYNTAX LIST-OPEN  *MODE-LIST-SYNTAX-TABLE* #/[)
  (SET-CHAR-SYNTAX LIST-OPEN  *MODE-LIST-SYNTAX-TABLE* #/{)
  (SET-CHAR-SYNTAX LIST-CLOSE *MODE-LIST-SYNTAX-TABLE* #/})
  (SET-CHAR-SYNTAX LIST-CLOSE *MODE-LIST-SYNTAX-TABLE* #/]))

(DEFMINOR COM-AUTO-FILL-MODE AUTO-FILL-MODE "Fill" 2
  "Minor mode in which insertion fills text.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
          ()
  (COMMAND-HOOK 'AUTO-FILL-HOOK *POST-COMMAND-HOOK*))

(DEFPROP AUTO-FILL-HOOK 20 COMMAND-HOOK-PRIORITY)
(DEFUN AUTO-FILL-HOOK (CHAR &AUX BP)
  (SETQ CHAR (CHAR-INT CHAR))
  (AND (MEMQ CHAR *AUTO-FILL-ACTIVATION-CHARACTERS*)
       (NOT *NUMERIC-ARG-P*)
       (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
       (LET ((LINE (BP-LINE (POINT)))
             (FILL-COLUMN *FILL-COLUMN*))
         (AND (= CHAR #/CR) (SETQ LINE (LINE-PREVIOUS LINE)))
;        (AND (PLUSP (STRING-LENGTH *FILL-PREFIX*))
;             (SETQ FILL-COLUMN (- FILL-COLUMN (STRING-WIDTH *FILL-PREFIX*))))
         (SETQ BP (DO ((SHEET (WINDOW-SHEET *WINDOW*))
                       (LEN (1+ (OR (STRING-REVERSE-SEARCH-NOT-CHAR #/SP LINE) -1)))
                       (POS 0)
                       (CHAR-POS 0 CP)
                       (CP))
                      ((= CHAR-POS LEN) NIL)
                    (SETQ CP (OR (STRING-SEARCH-CHAR #/SP LINE (1+ CHAR-POS)) LEN)
                          POS (TV:SHEET-STRING-LENGTH SHEET LINE CHAR-POS CP NIL NIL POS))
                    (AND (> POS FILL-COLUMN) (> CHAR-POS 0)
                         (RETURN (CREATE-BP LINE CHAR-POS))))))
       (WITH-BP (PT (POINT) :MOVES)             ;Save point
         (MOVE-BP (POINT) BP)
         (LET ((LINE (BP-LINE BP))
               (LINE2 NIL))
           (COND ((OR (END-LINE-P (FORWARD-OVER *BLANKS* BP))
                      (EQ LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                      (> (+ (STRING-WIDTH LINE (BP-INDEX BP))
                            (STRING-WIDTH
                              (SETQ LINE2 (LINE-NEXT LINE))))
                         *FILL-COLUMN*)
                      ;; If in Lispish mode and not inside a comment.
                      (AND (call-editing-type-function *major-mode* 'lisp-syntax-p nil)
                           ;(EQ (GET *MAJOR-MODE* 'EDITING-TYPE) :LISP)
                           (> (OR (FIND-COMMENT-START LINE) (1+ (line-length line)))
                              (BP-INDEX BP))))
                  (DELETE-INTERVAL BP (FORWARD-CHAR BP))
                  (LET ((AT-POINT-P (BP-= PT BP)))
                    (MUST-REDISPLAY *WINDOW* (COM-INDENT-NEW-COMMENT-LINE))
                    (AND AT-POINT-P (MOVE-BP PT (POINT)))))
                 (T
                  (MULTIPLE-VALUE-BIND (COMMENT-START COMMENT-END)
                      (FIND-COMMENT-START LINE)
                    ;; Move some text from the end of the line onto the next line.
                    ;; If the next line starts a paragraph,
                    ;; or is a solitary blank line,
                    ;; make a new line to move the text onto.
                    (COND ((AND COMMENT-START
                                (< COMMENT-START (BP-INDEX BP)))
                           ;; If we are breaking inside a comment, make a new line
                           ;; unless the next one is also a comment line
                           ;; and the comment starts at the same hpos
                           ;; and with the same comment-starting string.
                           (MULTIPLE-VALUE-BIND (LINE2-COMMENT-START LINE2-COMMENT-END)
                               (AND LINE2 (FIND-COMMENT-START LINE2))
                             (WHEN (OR (NOT LINE2-COMMENT-START)
                                       ( (BP-VIRTUAL-INDENTATION
                                            (CREATE-BP LINE2 LINE2-COMMENT-START)
                                            (WINDOW-SHEET *WINDOW*))
                                          (BP-VIRTUAL-INDENTATION
                                            (CREATE-BP LINE COMMENT-START)
                                            (WINDOW-SHEET *WINDOW*)))
                                       ( (- COMMENT-END COMMENT-START)
                                          (- LINE2-COMMENT-END LINE2-COMMENT-START))
                                       (NOT (STRING-EQUAL LINE LINE2
                                                          :START1 COMMENT-START
                                                          :START2 LINE2-COMMENT-START
                                                          :END1 COMMENT-END
                                                          :END2 LINE2-COMMENT-END)))
                               (INSERT (CREATE-BP LINE2 0) #/CR)
                               (SETQ LINE2 (LINE-PREVIOUS LINE2)))))
                          ((IF (LINE-BLANK-P LINE2)
                               (NOT (OR (EQ LINE2 (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
                                        (LINE-BLANK-OR-DIAGRAM-P (LINE-NEXT LINE2))))
                             (BP-AT-PARAGRAPH-TERMINATOR (CREATE-BP LINE2 0)))
                           (unless (char-equal char #/CR) (INSERT (CREATE-BP LINE2 0) #/CR))
                           (SETQ LINE2 (LINE-PREVIOUS LINE2))))
                    ;; Don't include leading blanks in the stuff to move to the next line.
                    (MOVE-BP BP (FORWARD-OVER *BLANKS* BP))
                    ;; IDX is beginning of text to move to next line.
                    (LET ((IDX (BP-INDEX BP))
                          (START-BP (CREATE-BP LINE2 0)))
                      (COND ((AND COMMENT-START
                                  (< COMMENT-START (BP-INDEX BP)))
                             ;; If stuff to move down is inside a comment,
                             ;; must move it to inside a comment.
                             (MULTIPLE-VALUE-BIND (ST END)
                                 (FIND-COMMENT-START LINE2)
                               (IF END (MOVE-BP START-BP LINE2 END)
                                 (INDENT-TO START-BP
                                            (BP-INDENTATION (CREATE-BP LINE COMMENT-START)))
                                 (MOVE-BP START-BP LINE2 (STRING-LENGTH LINE2))
                                 (SETF (VALUES ST END) (FIND-COMMENT-START LINE))
                                 (INSERT-MOVING START-BP (NSUBSTRING LINE ST END)))))
                            (T
                             ;; Make sure the following line starts with the fill prefix
                             ;; (in case we just created it and it is empty.)
                             (OR (LOOKING-AT START-BP *FILL-PREFIX*)
                                 (INSERT (CREATE-BP LINE2 0) *FILL-PREFIX*))
                             (MOVE-BP START-BP (FORWARD-OVER *BLANKS* START-BP))))
                      ;; Insert the stuff from the previous line.
                      (LET* ((AFTER-BP
                               (INSERT START-BP (NSUBSTRING LINE IDX)))
                             (BACK-ONE-BP (FORWARD-CHAR AFTER-BP -1)))
                        ;; Make sure there is a space between the new stuff and
                        ;; what was already on the following line.
                        (OR (BP-LOOKING-AT-LIST AFTER-BP *BLANKS*)
                            (BP-LOOKING-AT-LIST BACK-ONE-BP *BLANKS*)
                            (END-LINE-P AFTER-BP)
                            (INSERT AFTER-BP " "))
                        ;; If it ends a sentence, want two spaces after it.
                        (AND (BP-LOOKING-AT-LIST BACK-ONE-BP
                                                 *FILL-EXTRA-SPACE-LIST*)
                             (NOT (LOOKING-AT AFTER-BP "  "))
                             (NOT (END-LINE-P AFTER-BP))
                             (INSERT AFTER-BP " ")))
                      ;; If Point was within the text moved, keep it with that text.
                      (AND (EQ (BP-LINE PT) (BP-LINE BP))
                           ( (SETQ IDX (- (BP-INDEX PT) IDX)) 0)
                           (MOVE-BP PT LINE2 (+ (BP-INDEX START-BP) IDX))))
                    ;; Delete what we moved from the previous line, plus any blanks before it.
                    (DELETE-INTERVAL (BACKWARD-OVER *BLANKS* BP) (END-OF-LINE LINE))
                    ;; If the text cut left us on the same line we started on, and we were inserting
                    ;; a space, reinsert the trailing space deleted above.
                    (and (char-equal char #/Space)
                         (neq (bp-line pt) line2)
                         (= (bp-index pt) (line-length line))
                         (insert pt #/Space))
                    (MUST-REDISPLAY *WINDOW* DIS-TEXT)))))
         (MOVE-BP (POINT) PT))))


(DEFPROP AUTO-FILL-HOOK DOCUMENT-AUTO-FILL-HOOK HOOK-DOCUMENTATION-FUNCTION)

(DEFUN DOCUMENT-AUTO-FILL-HOOK (IGNORE CHAR)
  (AND (MEMQ (CHAR-INT CHAR) *AUTO-FILL-ACTIVATION-CHARACTERS*)
       (PRINC "With no numeric argument, auto fill line if needed.
")))

(DEFMINOR COM-OVERWRITE-MODE OVERWRITE-MODE "Overwrite" 4
  "Minor mode in which normal typing replaces existing text.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
  ()
  (SETQ *STANDARD-COMMAND* 'COM-SELF-OVERWRITE))

(DEFCOM COM-SELF-OVERWRITE "Replace the character at point with the character typed.
At the end of a line, inserts instead of replacing the newline." ()
  (LET ((PT (POINT)))
    (OR (= (BP-INDEX PT) (LINE-LENGTH (BP-LINE PT)))
        (DELETE-INTERVAL PT (FORWARD-CHAR PT 1 T)))
    (INSERT-MOVING PT *LAST-COMMAND-CHAR*)
    (VALUES DIS-LINE (BP-LINE PT) (1- (BP-INDEX PT)))))

;;;; Word abbrev mode

(DEFUN INITIALIZE-WORD-ABBREV-TABLE ()
  (LET ((INIT  " ~@#;$%^&*()-_=+[]\/|:'`/"{},<.>//?!
212"))
    (SETQ *WORD-ABBREV-TABLE* (MAKE-ARRAY 400 :TYPE 'ART-1B))
    (DO ((I 0 (1+ I))
         (LIM (STRING-LENGTH INIT)))
        (( I LIM))
      (ASET 1 *WORD-ABBREV-TABLE* (AREF INIT I)))))

(DEFCOM COM-EXPAND-ONLY "Expand last word, but insert nothing after it.
If given an argument, beep unless expanded." ()
  (AND (NULL (EXPAND-ABBREV)) *NUMERIC-ARG-P*
       (BARF))
  DIS-TEXT)

(DEFPROP EXPAND-ABBREV-HOOK 10 COMMAND-HOOK-PRIORITY)
(DEFUN EXPAND-ABBREV-HOOK (IGNORE)
  (AND (EXPAND-P *LAST-COMMAND-CHAR*)
       (NOT *NUMERIC-ARG-P*)
       (EXPAND-ABBREV)
       (MUST-REDISPLAY *WINDOW* DIS-TEXT)))

(DEFPROP EXPAND-ABBREV-HOOK DOCUMENT-EXPAND-ABBREV-ITEM HOOK-DOCUMENTATION-FUNCTION)
(DEFUN DOCUMENT-EXPAND-ABBREV-ITEM (IGNORE CHAR)
  (AND (EXPAND-P CHAR)
       (PRINC "With no numeric argument, expand preceeding word abbrev if any.
")))

;;; Does this character try to expand preceeding abbrev?
(DEFUN EXPAND-P (CHAR)
  "T if CHAR is a command that should expand word abbrevs."
  (AND (ZEROP (LDB %%KBD-CONTROL-META CHAR))
       (NOT (ZEROP (AREF *WORD-ABBREV-TABLE* (LDB %%KBD-CHAR CHAR))))))

(DEFUN EXPAND-ABBREV (&AUX BP STRING SYM TEM PROP)
  "Expand the word abbrev before point, if there is one."
  (AND (NOT (DELIMCHAR-P (BP-CHAR-BEFORE (POINT))))
       (MULTIPLE-VALUE (STRING BP)
         (BOUND-WORD (POINT))))
  (COND ((AND STRING
              (SETQ SYM (INTERN-SOFT (STRING-UPCASE STRING) *UTILITY-PACKAGE*))
              (SETQ TEM (OR (GET SYM (SETQ PROP (GET-ABBREV-MODE-NAME)))
                            (GET SYM (SETQ PROP '*-ABBREV)))))
         (WITH-UNDO-SAVE ("Abbrev expansion" BP (POINT) T)
           (COND (*LAST-EXPANSION-BP*
                  (MOVE-BP *LAST-EXPANSION-BP* BP))
                 (T
                  (SETQ *LAST-EXPANSION-BP* (COPY-BP BP :NORMAL))))
           (COND ((AND (CHAR-EQUAL (BP-CHAR-BEFORE BP) #/-)
                       (BP-= (MOVE-BP BP (FORWARD-CHAR BP -1))
                             *WORD-ABBREV-PREFIX-MARK*))
                  (SETQ STRING (STRING-APPEND "-" STRING))
                  (DELETE-INTERVAL BP (FORWARD-CHAR BP) T))
                 (T
                  (SETQ STRING (STRING-APPEND STRING))))
           (SETQ *LAST-EXPANDED* STRING
                 *LAST-EXPANSION* TEM
                 *LAST-EXPANSION-SYMBOL* SYM
                 *LAST-EXPANSION-USAGE-PROP* (GET-ABBREV-USAGE-NAME PROP))
           (LET ((V (GET SYM *LAST-EXPANSION-USAGE-PROP*)))
             (PUTPROP SYM (IF V (1+ V) 1) *LAST-EXPANSION-USAGE-PROP*))
           (MOVE-BP (POINT) (CASE-REPLACE *LAST-EXPANSION-BP* (POINT) TEM))))))

(DEFCOM COM-UNEXPAND-LAST-WORD "Undo last expansion, leaving the abbrev." ()
  (LET (BP TEM)
    (OR *LAST-EXPANSION* (BARF "No last expansion"))
    (SETQ BP (FORWARD-CHAR *LAST-EXPANSION-BP* (ARRAY-ACTIVE-LENGTH *LAST-EXPANSION*)))
    (OR (STRING-EQUAL (STRING-INTERVAL *LAST-EXPANSION-BP* BP T) *LAST-EXPANSION*)
        (BARF "No last expansion"))
    (SETQ TEM (BP-= BP (POINT))
          BP (INSERT (DELETE-INTERVAL *LAST-EXPANSION-BP* BP)
                     *LAST-EXPANDED*))
    (PUTPROP *LAST-EXPANSION-SYMBOL*
             (1- (GET *LAST-EXPANSION-SYMBOL*
                      *LAST-EXPANSION-USAGE-PROP*))
             *LAST-EXPANSION-USAGE-PROP*)
    (AND TEM (MOVE-BP (POINT) BP)))
  DIS-TEXT)

(DEFMINOR COM-WORD-ABBREV-MODE WORD-ABBREV-MODE "Abbrev" 3
  "Mode for expanding word abbrevs.
No arg or non-zero arg sets the mode, 0 arg clears it." ()
; (SET-COMTAB *MODE-COMTAB* '(#/C-M-SP COM-EXPAND-ONLY))
  (SET-COMTAB *STANDARD-CONTROL-X-COMTAB*
              '(#/U COM-UNEXPAND-LAST-WORD
                #/C-A COM-ADD-MODE-WORD-ABBREV
                #/M-/' COM-WORD-ABBREV-PREFIX-MARK
                #/+ COM-ADD-GLOBAL-WORD-ABBREV))
  (COMMAND-HOOK 'EXPAND-ABBREV-HOOK *COMMAND-HOOK*)
  (SETQ *LAST-EXPANSION-BP* NIL)
  (SETQ *LAST-EXPANDED* NIL)
  (SETQ *LAST-EXPANSION* NIL)
  (SETQ *WORD-ABBREV-PREFIX-MARK* NIL))

(DEFCOM COM-MAKE-WORD-ABBREV "Prompt for and make a new word abbrev.
An argument means make global abbrev, else local for this mode." ()
  (MAKE-WORD-ABBREV
    *NUMERIC-ARG-P*
    (TYPEIN-LINE-READLINE "Define ~:[~A mode~;global~*~] abbrev for: "
                          *NUMERIC-ARG-P* (NAME-OF-MAJOR-MODE))))

(DEFCOM COM-ADD-MODE-WORD-ABBREV "Define mode-specific abbrev to expand into word before point.
The abbrev is read with the minibuffer.
Positive argument means use that many words before point as the expansion.
If there is a region, it is used as the expansion.
A negative arg means delete the mode-specific definition of an abbrev.
/(If specified abbrev has no mode-specific definition, offer to kill the global definition.)" ()
  (COND ((MINUSP *NUMERIC-ARG*)
         (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
           (COM-KILL-MODE-WORD-ABBREV)))
        (T
         (MAKE-WORD-ABBREV NIL))))

(DEFCOM COM-ADD-GLOBAL-WORD-ABBREV "Define global abbrev to expand into word before point.
The abbrev is read with the minibuffer.
Positive argument means use that many words before point as the expansion.
If there is a region, it is used as the expansion.
A negative arg means delete the global definition of an abbrev.
/(If specified abbrev has no global definition, offer to kill the mode-specific definition.)" ()
  (COND ((MINUSP *NUMERIC-ARG*)
         (LET ((*NUMERIC-ARG* (MINUS *NUMERIC-ARG*)))
           (COM-KILL-GLOBAL-WORD-ABBREV)))
        (T
         (MAKE-WORD-ABBREV T))))

(DEFUN MAKE-WORD-ABBREV (GLOBAL-P &OPTIONAL STRING &AUX ABBREV)
  (OR STRING
      (SETQ STRING (COND ((WINDOW-MARK-P *WINDOW*)
                          (STRING-INTERVAL (MARK) (POINT)))
                         (T
                          (OR (BOUND-WORD (POINT) (ABS *NUMERIC-ARG*))
                              (BARF))))))
  (OR (STRINGP STRING)
      (SETQ STRING (STRING-APPEND "" STRING)))
  (SETQ ABBREV (LET ((*MINI-BUFFER-DEFAULT-STRING* STRING)
                     (*COMMAND-HOOK* NIL))      ;Don't expand abbrevs within
                 (TYPEIN-LINE-READLINE
                   (FORMAT NIL "~:[~A mode~;Global~*~] abbrev for /"~A/": "
                           GLOBAL-P (NAME-OF-MAJOR-MODE) STRING))))
  (PUTPROP (INTERN (STRING-UPCASE ABBREV) *UTILITY-PACKAGE*) (STRING-APPEND STRING)
           (COND (GLOBAL-P '*-ABBREV) (T (GET-ABBREV-MODE-NAME))))
  (SETQ *WORD-ABBREV-TICK* (TICK))
  DIS-NONE)

(DEFCOM COM-KILL-MODE-WORD-ABBREV "Kill mode-specific definition of specified word abbrev." ()
  (KILL-ABBREV NIL))

(DEFCOM COM-KILL-GLOBAL-WORD-ABBREV "Kill global definition of specified word abbrev." ()
  (KILL-ABBREV T))

(DEFUN KILL-ABBREV (GLOBAL-P &AUX STRING SYM MODE-NAME PROP)
  (SETQ STRING (LET ((*COMMAND-HOOK* NIL))      ;Don't expand abbrevs within
                 (TYPEIN-LINE-READLINE "Kill ~:[~A mode~;global~*~] abbrev: "
                                       GLOBAL-P (NAME-OF-MAJOR-MODE))))
  (OR (SETQ SYM (INTERN-SOFT (STRING-UPCASE STRING) *UTILITY-PACKAGE*))
      (BARF "No such abbrev defined"))
  (COND (GLOBAL-P
         (OR (GET SYM '*-ABBREV) (BARF "No such global abbrev defined."))
         (REMPROP SYM '*-ABBREV)
         (REMPROP SYM '*-ABBREV-USAGE))
        ((GET SYM (SETQ MODE-NAME (SETQ PROP (GET-ABBREV-MODE-NAME))))
         (REMPROP SYM MODE-NAME)
         (REMPROP SYM (GET-ABBREV-USAGE-NAME PROP)))
        ((NOT (GET SYM '*-ABBREV))
         (BARF "No such abbrev defined."))
        ((FQUERY '(:SELECT T) "~A is not a ~A mode abbrev, but is a global one, kill it? "
                 STRING (NAME-OF-MAJOR-MODE))
         (REMPROP SYM '*-ABBREV)
         (REMPROP SYM '*-ABBREV-USAGE))
        (T
         (PROMPT-LINE "~&Not killed.")))
  (SETQ *WORD-ABBREV-TICK* (TICK))
  DIS-NONE)

(DEFCOM COM-KILL-ALL-WORD-ABBREVS "No word abbrevs are defined after this." ()
  (MAPATOMS #'KILL-ALL-ABBREVS-1 *UTILITY-PACKAGE* NIL)
  DIS-NONE)

(DEFUN KILL-ALL-ABBREVS-1 (SYM)
  (DO ((L (PLIST SYM) (CDDR L))
       (IND)
       (IND-NAME)
       (LEN))
      ((NULL L))
    (SETQ IND (CAR L)
          IND-NAME (GET-PNAME IND)
          LEN (STRING-LENGTH IND-NAME))
    (AND (> LEN 7)
         (STRING-EQUAL (NSUBSTRING IND-NAME (- (STRING-LENGTH IND-NAME) 7))
                       "-ABBREV")
         (REMPROP SYM IND)
         (REMPROP SYM (GET-ABBREV-USAGE-NAME IND-NAME)))))

(DEFUN BOUND-WORD (BP &OPTIONAL (TIMES 1) &AUX BP1 STRING)
  (AND (SETQ BP (FORWARD-TO-WORD BP -1))
       (SETQ BP1 (FORWARD-WORD BP (- TIMES)))
       (SETQ STRING (STRING-INTERVAL BP BP1)))
  (VALUES STRING BP1))

(DEFCOM COM-WORD-ABBREV-PREFIX-MARK "Mark point as end of a prefix" ()
  (EXPAND-ABBREV)
  (COND (*WORD-ABBREV-PREFIX-MARK*
         (MOVE-BP *WORD-ABBREV-PREFIX-MARK* (POINT)))
        (T
         (SETQ *WORD-ABBREV-PREFIX-MARK* (COPY-BP (POINT) :NORMAL))))
  (INSERT-MOVING (POINT) "-")
  DIS-TEXT)

(DEFCOM COM-LIST-WORD-ABBREVS "List all abbrevs and their expansions." ()
  (FORMAT T "~%Abbrev:   (Mode)             Count:     /"Expansion/"~3%")
  (LIST-WORD-ABBREV-1 STANDARD-OUTPUT)
  DIS-NONE)

(DEFCOM COM-INSERT-WORD-ABBREVS "Insert all abbrevs and their expansions into the buffer."
        (X)
  (LIST-WORD-ABBREV-1 (INTERVAL-STREAM-INTO-BP (POINT)))
  DIS-TEXT)

(DEFUN LIST-WORD-ABBREV-1 (STREAM)
  (MAPATOMS
    #'(LAMBDA (SYM)
        (DO ((L (PLIST SYM) (CDDR L))
             (IND)
             (IND-NAME)
             (USAGE)
             (LEN)
             (STRING ""))
            ((NULL L)
             (FORMAT STREAM STRING))
          (SETQ IND (CAR L)
                IND-NAME (GET-PNAME IND)
                LEN (STRING-LENGTH IND-NAME)
                USAGE (OR (GET SYM (GET-ABBREV-USAGE-NAME IND-NAME)) 0))
          (AND (> LEN 7)
               (STRING-EQUAL (NSUBSTRING IND-NAME (- LEN 7))
                             "-ABBREV")
               (SETQ STRING (FORMAT NIL
                                    "~A~10,4,2A~:[~15,4,2A~;~15@T~*~] ~6D~6@T /"~A/"~%"
                                    STRING
                                    (STRING-APPEND SYM ":")
                                    (EQ IND '*-ABBREV)
                                    (STRING-APPEND "("
                                                   (NSUBSTRING IND-NAME 0 (- LEN 7))
                                                   ")")
                                    USAGE
                                    (CADR L))))))
    *UTILITY-PACKAGE*))

(DEFCOM COM-DEFINE-WORD-ABBREVS "Define word abbrevs from buffer" ()
  (DO ((BP1 (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*)))
       (BP2)
       (MODE "*" "*")
       (USAGE)
       (SYM)
       (TEM))
      (())
    (OR (SETQ BP2 (ZWEI-SEARCH BP1 #/:)) (RETURN NIL))
    (SETQ TEM (STRING-UPCASE (STRING-INTERVAL BP1 (FORWARD-CHAR BP2 -1))))
    (SETQ SYM (INTERN TEM *UTILITY-PACKAGE*))
    (SETQ BP2 (FORWARD-OVER *BLANKS* (FORWARD-CHAR BP2)))
    (COND ((CHAR-EQUAL (BP-CHAR BP2) #/()
           (OR (SETQ BP1 (ZWEI-SEARCH (SETQ BP2 (FORWARD-CHAR BP2)) #/)))
               (BARF "Unmatched paren ~A" (BP-LINE BP2)))
           (SETQ MODE (STRING-INTERVAL BP2 (FORWARD-CHAR BP1 -1)))
           (SETQ BP2 (PROG1 (FORWARD-OVER *BLANKS* (FORWARD-CHAR BP1))
                            (SETQ BP1 BP2)))))
    (MULTIPLE-VALUE (USAGE TEM)
      (PARSE-NUMBER (BP-LINE BP2) (BP-INDEX BP2) NIL 10.))
    (AND (= TEM (BP-INDEX BP2)) (BARF "No usage count ~A" (BP-LINE BP2)))
    (SETF (BP-INDEX BP2) TEM)
    (SETQ BP2 (FORWARD-OVER *BLANKS* BP2))
    (OR (CHAR-EQUAL (BP-CHAR BP2) #/")
        (BARF "No expansion ~A" (BP-LINE BP2)))
    (OR (SETQ BP1 (ZWEI-SEARCH (SETQ BP2 (FORWARD-CHAR BP2)) #/"))
        (BARF "Unmatched quote ~A" (BP-LINE BP2)))
    (PUTPROP SYM (STRING-APPEND (STRING-INTERVAL BP2 (FORWARD-CHAR BP1 -1)))
             (GET-ABBREV-MODE-NAME MODE))
    (AND (EQ (BP-LINE BP1) (BP-LINE (INTERVAL-LAST-BP *INTERVAL*)))
         (RETURN NIL))
    (MOVE-BP BP1 (BEG-LINE BP1 1)))
  DIS-NONE)

(DEFUN GET-ABBREV-MODE-NAME (&OPTIONAL (MODE (STRING (NAME-OF-MAJOR-MODE))))
  (INTERN (STRING-APPEND MODE "-ABBREV") (SYMBOL-PACKAGE 'FOO)))

;;; Given the name of a X-ABBREV prop, return the usage count property.
(DEFUN GET-ABBREV-USAGE-NAME (STR)
  (INTERN (STRING-APPEND STR "-USAGE") (SYMBOL-PACKAGE 'FOO)))

(DEFCOM COM-EDIT-WORD-ABBREVS "Enter recursive edit on the abbrev definitions." ()
  (LET ((INTERVAL (CREATE-INTERVAL NIL NIL T)))
    (SETF (NODE-TICK INTERVAL) (TICK))
    (LET ((*INTERVAL* INTERVAL)
          (*BATCH-UNDO-SAVE* T))
      (COM-INSERT-WORD-ABBREVS))
    (RECURSIVE-EDIT INTERVAL "Edit Word Abbrevs")
    (COM-KILL-ALL-WORD-ABBREVS)
    (LET ((*INTERVAL* INTERVAL))
      (COM-DEFINE-WORD-ABBREVS)))
  DIS-NONE)

(DEFCOM COM-RECURSIVE-EDIT-BEEP "Exit from recursive edit without updating." ()
  (BEEP)
  (MUST-REDISPLAY *WINDOW* DIS-TEXT)
  (COND (*NUMERIC-ARG-P* DIS-NONE)
        ((WINDOW-MARK-P *WINDOW*)
         (SETQ *MARK-STAYS* NIL)
         DIS-NONE)
        (T
         (*THROW 'TOP-LEVEL T))))

(DEFVAR APROPOS-KEY)
(DEFVAR APROPOS-SEARCH-FUNCTION)

(DEFCOM COM-LIST-SOME-WORD-ABBREVS "List abbreviations or expansions with the given string" ()
  (MULTIPLE-VALUE-BIND (APROPOS-SEARCH-FUNCTION APROPOS-KEY)
      (GET-EXTENDED-SEARCH-STRINGS "Word abbrev apropos (substring:)")
    (MAPATOMS #'WORD-ABBREV-APROPOS-INTERNAL *UTILITY-PACKAGE* NIL))
  (FORMAT T "Done.~%")
  DIS-NONE)

(DEFUN WORD-ABBREV-APROPOS-INTERNAL (SYM)
  (DO ((L (PLIST SYM) (CDDR L))
       (IND)
       (IND-NAME)
       (LEN))
      ((NULL L))
    (SETQ IND (CAR L)
          IND-NAME (GET-PNAME IND)
          LEN (STRING-LENGTH IND-NAME))
    (AND (> LEN 7)
         (STRING-EQUAL (NSUBSTRING IND-NAME (- LEN 7))
                       "-ABBREV")
         (OR (FUNCALL APROPOS-SEARCH-FUNCTION APROPOS-KEY (GET-PNAME SYM))
             (FUNCALL APROPOS-SEARCH-FUNCTION APROPOS-KEY (CADR L)))
         (FORMAT T "~A: ~:[(~A)~;~*~]   /"~A/"~%"
                 SYM (EQ IND '*-ABBREV)
                 (NSUBSTRING IND-NAME 0 (- LEN 7)) (CADR L)))))

(DEFCOM COM-READ-WORD-ABBREV-FILE "Load up new format word abbrev file." ()
  (AND *WORD-ABBREV-FILE-NAME* (FS:SET-DEFAULT-PATHNAME *WORD-ABBREV-FILE-NAME*
                                                        *AUX-PATHNAME-DEFAULTS*))
  (LET ((FNAME (READ-DEFAULTED-AUX-PATHNAME "Load QWABL file:" :QWABL)))
    (WITH-OPEN-FILE (STREAM FNAME '(IN))
      (LOAD-QWABL STREAM))
    (SETQ *WORD-ABBREV-FILE-NAME* FNAME
          *WORD-ABBREV-FILE-TICK* (TICK)))
  DIS-NONE)

(DEFUN PRELOAD-WORD-ABBREVS (FILE)
  "Load FILE's word abbrevs into ZMACS.  Loses under canonical file operations."
  (WITH-OPEN-FILE (STREAM FILE)
    (LOAD-QWABL STREAM)))

(DEFUN LOAD-QWABL (STREAM)
  (SEND STREAM :LINE-IN)                        ;Flush some TECO macros
  (SEND STREAM :LINE-IN)
  (DO ((SYM)
       (USAGE)
       (MODE)
       (STR)
       (EOFP)
       (TEM)
       (TEM1))
      (())
    (MULTIPLE-VALUE (STR EOFP)
      (SEND STREAM :LINE-IN T))
    (AND EOFP (RETURN NIL))
    (OR (SETQ TEM (STRING-SEARCH-CHAR #/SP STR))
        (BARF "No abbrev ~S" STR))
    (SETQ TEM (1+ TEM))
    (OR (SETQ TEM1 (STRING-SEARCH-CHAR #/SP STR TEM))
        (BARF "No mode ~S" STR))
    (SETQ SYM (STRING-UPCASE (NSUBSTRING STR TEM TEM1))
          TEM1 (1+ TEM1))
    (OR (SETQ TEM (STRING-SEARCH-CHAR #/SP STR TEM1))
        (BARF "No end of mode ~S" STR))
    (SETQ MODE (NSUBSTRING STR TEM1 TEM)
          TEM (1+ TEM))
    (SETQ MODE (GET-ABBREV-MODE-NAME MODE))
    (OR (SETQ TEM1 (STRING-SEARCH-CHAR #/ STR TEM))
        (BARF "No expansion ~S" STR))
    (SETQ STR (NSUBSTRING STR (1+ TEM1)))
    (SETQ STR (DO ((EXPANSION "" (STRING-APPEND EXPANSION STR))
                   (STR STR (SEND STREAM :LINE-IN))
                   (POS))
                  (())
                (COND ((SETQ POS (STRING-SEARCH-CHAR #/ STR))
                       (SETQ USAGE (PARSE-NUMBER STR (1+ POS) NIL 10.))
                       (SETQ STR (NSUBSTRING STR 0 POS))
                       (RETURN (STRING-APPEND EXPANSION STR))))))
    (SETQ SYM (INTERN SYM *UTILITY-PACKAGE*))
    (PUTPROP SYM STR MODE)
    (PUTPROP SYM USAGE (GET-ABBREV-USAGE-NAME MODE))
    ))

(DEFUN WRITE-QWABL (STREAM &AUX L)
  (SEND STREAM :LINE-OUT "m.m& Make Usage Abbrev Variable[V")
  (SEND STREAM :LINE-OUT "q..q[..o")
  (MAPATOMS #'(LAMBDA (SYM) (PUSH SYM L)) *UTILITY-PACKAGE*)
  (SETQ L (SORT L #'STRING-LESSP))
  (DO ((SL L (CDR SL))
       (SYM))
      ((NULL SL))
    (SETQ SYM (CAR SL))
    (DO ((LIST (PLIST SYM) (CDDR LIST))
         (PROPNAME)
         (LPROPNAME))
        ((NULL LIST))
      (SETQ PROPNAME (CAR LIST) LPROPNAME (STRING-LENGTH PROPNAME))
      (COND ((STRING-EQUAL "-ABBREV" PROPNAME :START1 0 :START2 (- LPROPNAME 7))
             (FORMAT STREAM "MVX ~A ~A Abbrev~A~D~%"
                     (STRING-DOWNCASE SYM)
                     (NSUBSTRING PROPNAME 0 (- LPROPNAME 7))
                     (CADR LIST)
                     (OR (GET SYM (GET-ABBREV-USAGE-NAME PROPNAME)) 0)
                     ))))))

(DEFCOM COM-WRITE-WORD-ABBREV-FILE "Write out all word abbrevs in QWABL format." ()
  (AND *WORD-ABBREV-FILE-NAME* (FS:SET-DEFAULT-PATHNAME *WORD-ABBREV-FILE-NAME*
                                                        *AUX-PATHNAME-DEFAULTS*))
  (LET ((FN (READ-DEFAULTED-AUX-PATHNAME "Write word abbrevs to:" :QWABL NIL :WRITE)))
    (COM-WRITE-WORD-ABBREV-FILE-INTERNAL FN))
  DIS-NONE)

(DEFCOM COM-SAVE-WORD-ABBREV-FILE "Write out word abbrevs if changed." ()
  (COM-SAVE-WORD-ABBREV-FILE-INTERNAL :EXPLICIT)
  DIS-NONE)

(DEFUN COM-SAVE-WORD-ABBREV-FILE-INTERNAL (WHEN)
  (COND (( *WORD-ABBREV-TICK* *WORD-ABBREV-FILE-TICK*)
         (AND (EQ WHEN :EXPLICIT)
              (FORMAT *QUERY-IO* "~&(Word abbrevs do not need to be saved)")))
        ((AND (EQ WHEN :ASK)
              (NOT (FQUERY () "Save word abbrevs~@[on file ~A~]? "
                           *WORD-ABBREV-FILE-NAME*))))
        (T
         (OR *WORD-ABBREV-FILE-NAME*
             (SETQ *WORD-ABBREV-FILE-NAME* (READ-DEFAULTED-AUX-PATHNAME
                                             "Save word abbrevs to:" :QWABL NIL :WRITE)))
         (COM-WRITE-WORD-ABBREV-FILE-INTERNAL *WORD-ABBREV-FILE-NAME*))))

;;; Return T if caller should save the word abbrevs.
;;; Any questions which need to be asked are asked now.
;;; The caller can then simply call COM-WRITE-WORD-ABBREV-FILE-INTERNAL.
(DEFUN WORD-ABBREVS-NEED-SAVING-P (NO-QUERY)
  (AND (> *WORD-ABBREV-TICK* *WORD-ABBREV-FILE-TICK*)
       (OR NO-QUERY
           (FQUERY () "Save word abbrevs~@[ on file ~A~]? "
                   *WORD-ABBREV-FILE-NAME*))
       (PROGN
         (OR *WORD-ABBREV-FILE-NAME*
             (SETQ *WORD-ABBREV-FILE-NAME* (READ-DEFAULTED-AUX-PATHNAME
                                             "Save word abbrevs to:" :QWABL NIL :WRITE)))
         T)))

(DEFUN COM-WRITE-WORD-ABBREV-FILE-INTERNAL (&OPTIONAL (FN *WORD-ABBREV-FILE-NAME*))
  (WITH-OPEN-FILE (STREAM FN '(WRITE))
    (WRITE-QWABL STREAM)
    (CLOSE STREAM)
    (FORMAT *QUERY-IO* "~&Written: ~A" (SEND STREAM :TRUENAME)))
  (SETQ *WORD-ABBREV-FILE-NAME* FN
        *WORD-ABBREV-FILE-TICK* (TICK)))

;;;; Shift and font lock modes

(DEFMINOR COM-ELECTRIC-SHIFT-LOCK-MODE ELECTRIC-SHIFT-LOCK-MODE "Electric Shift-lock" 5
  "Minor mode upcasing all but comments and strings.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
  ()
  (COMMAND-HOOK 'SHIFT-LOCK-HOOK *COMMAND-HOOK*))

(DEFMINOR COM-ELECTRIC-FONT-LOCK-MODE ELECTRIC-FONT-LOCK-MODE "Electric Font-lock" 5
          "Minor mode to put comments in font B.
A positive argument turns the mode on, zero turns it off;
no argument toggles."
  ()
  (COMMAND-HOOK 'FONT-LOCK-HOOK *COMMAND-HOOK*))

(DEFVAR *SHIFT-LOCK-HOOK-LAST-LINE* NIL)
(DEFVAR *SHIFT-LOCK-HOOK-DEFUN-BEGINNING* NIL)

(DEFPROP SHIFT-LOCK-HOOK 10 COMMAND-HOOK-PRIORITY)
(DEFUN SHIFT-LOCK-HOOK (CHAR &AUX STRING SLASH COMMENT
                                  (POINT (POINT)) (*LISP-PARSE-PREPARSED-FLAG* T))
  (WHEN (AND (OR (AND ( CHAR #/A) ( CHAR #/Z))
                 (AND ( CHAR #/a) ( CHAR #/z)))
             (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*)))
    (OR (AND (EQ *LAST-COMMAND-TYPE* 'SELF-INSERT)
             (EQ (BP-LINE POINT) *SHIFT-LOCK-HOOK-LAST-LINE*))
        (SETQ *SHIFT-LOCK-HOOK-DEFUN-BEGINNING* (FORWARD-DEFUN POINT -1 T)
              *SHIFT-LOCK-HOOK-LAST-LINE* (BP-LINE POINT)
              *LISP-PARSE-PREPARSED-FLAG* NIL))
    (MULTIPLE-VALUE (STRING SLASH COMMENT)
      (LISP-BP-SYNTACTIC-CONTEXT POINT *SHIFT-LOCK-HOOK-DEFUN-BEGINNING*))
    (OR STRING SLASH COMMENT
        (SETQ *LAST-COMMAND-CHAR* (IF *ELECTRIC-SHIFT-LOCK-XORS*
                                      (LOGXOR CHAR #o40)
                                    (BOOLE 4 CHAR #o40))))))

(DEFPROP FONT-LOCK-HOOK 10 COMMAND-HOOK-PRIORITY)
(DEFUN FONT-LOCK-HOOK (IGNORE &AUX COMMENT (POINT (POINT)) (*LISP-PARSE-PREPARSED-FLAG* T))
  (WHEN (NEQ *INTERVAL* (WINDOW-INTERVAL *MINI-BUFFER-WINDOW*))
    (OR (AND (EQ *LAST-COMMAND-TYPE* 'SELF-INSERT)
             (EQ (BP-LINE POINT) *SHIFT-LOCK-HOOK-LAST-LINE*))
        (SETQ *SHIFT-LOCK-HOOK-DEFUN-BEGINNING* (FORWARD-DEFUN POINT -1 T)
              *SHIFT-LOCK-HOOK-LAST-LINE* (BP-LINE POINT)
              *LISP-PARSE-PREPARSED-FLAG* NIL))
    (MULTIPLE-VALUE (NIL NIL COMMENT)
      (LISP-BP-SYNTACTIC-CONTEXT POINT *SHIFT-LOCK-HOOK-DEFUN-BEGINNING*))
    (LET ((NEW-FONT (IF COMMENT 1 0)))
      (WHEN ( *FONT* NEW-FONT)
        (SETQ *FONT* NEW-FONT)
        (UPDATE-FONT-NAME)))))

;;; It is useful to setq LISP-MODE-HOOK to this
(DEFUN ELECTRIC-SHIFT-LOCK-IF-APPROPRIATE ()
  "Turn on Electric Shift Lock Mode unless the LOWERCASE attribute is T."
  (IF (SEND *INTERVAL* :GET-ATTRIBUTE :LOWERCASE)
      (TURN-OFF-MODE 'ELECTRIC-SHIFT-LOCK-MODE)
    (TURN-ON-MODE 'ELECTRIC-SHIFT-LOCK-MODE)))

;;; It is useful to setq LISP-MODE-HOOK to this
(DEFUN ELECTRIC-FONT-LOCK-IF-APPROPRIATE ()
  "Turn on Electric Font Lock Mode if there's more than one font in the attribute list."
  (IF (LET ((FONTS (SEND *INTERVAL* :GET-ATTRIBUTE :FONTS)))
        (AND (CONSP FONTS)
             (> (LENGTH FONTS) 1)))
      (TURN-ON-MODE 'ELECTRIC-FONT-LOCK-MODE)
    (TURN-OFF-MODE 'ELECTRIC-FONT-LOCK-MODE)))

;;; It is useful to setq TEXT-MODE-HOOK to this
(DEFUN AUTO-FILL-IF-APPROPRIATE ()
  (IF (SEND *INTERVAL* :GET-ATTRIBUTE :NOFILL)
      (TURN-OFF-MODE 'AUTO-FILL-MODE)
    (TURN-ON-MODE 'AUTO-FILL-MODE)))
