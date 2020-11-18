;-*- mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) May 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

;
;Variables used for customizing the editor.
;Notice that because of M-X EDIT-OPTIONS the values of all of these
;variables must be PRINT/READable.
;

;This is a list of variable definitions.
;Each element is the list (variable-name documentation)
;
(defparameter *editor-variables* nil)

(defmacro define-editor-variable (var default documentation &rest doc-args)
  ;;Documentation is NOT optional.
  `(progn (defvar ,var ,default ,documentation)
          (setq *editor-variables*
                (nconc
                 (delq (assq ',var *editor-variables*) *editor-variables*)
                 (list
                  (list ',var ,documentation ,@(mapcar #'(lambda (x)
                                                           (list 'quote x))
                                                       doc-args)))))
          ',var))

;First some generally usefull variables, which will be needed
;to initiailize other variables.

(defvar *tty-height*)
(defvar *tty-width*)

;These two variables are the root nodes of the entire buffer and its
;content
(defvar *editor-cursor*)                ;Use these variables in programs.
(defvar *editor-buffer*)                ;Don't use the short names below.
(defvar *.*)                            ;Copy of *editor-cursor* for typin.
(defvar *b*)                            ;Copy of *editor-buffer* for typin.

(defvar *major-mode* "Fundamental")

;The variables.

(defvar *editor-current-key*)           ;Like #\control-F
(defvar *editor-last-command*)          ;like 'self-insert
(defvar *editor-command-count*)

(defvar *editor-running* nil
        "Bound to T inside the editor")

(define-editor-variable *editor-device-mode* t
        "Controls whether editor is in PASSALL mode")

;Some people may want the buffer name reader to complete when a RETURN
;is typed. Others may not like the idea of making it hard to create
;buffers whose names are a prefix of an existing buffer name.
;Hence there is a switch to control this behavior. If nil, return
;does not cause completion.
(define-editor-variable *buffer-name-completion-on-return* nil
  "Controls whether <CR> causes buffer name completion")

;Should two or one space be required after a period (.) to end
;a sentance. I favor one.
(define-editor-variable *single-space-sentence-delimiter* t
  "When T only one space is required to end a sentance.
When NIL a sentance must end with two spaces.")

;For text a paragraph begins with a line having a whitespace character
;in the first column. For non-text a blank line delimits a paragraph.
(define-editor-variable *paragraph-text-style* t
  "Paragraphs begin with a line starting with a space or tab (text style)
or a blank line (non-text style, for programs.)")

;In lisp-listener mode the final paren of a top-level form causes it
;to be evaluated. The value of the form is inserted into the buffer on the
;line after the form.
(define-editor-variable *lisp-listener* nil
  "When T the editor acts like a top-level lisp interpreter.")


;Standard EMACS overwrite mode. (But not fully compatible.)
(define-editor-variable *overwrite-mode* nil
  "Causes self-insert to overwrite existing characters.")

;Same as the EMACS variable Display Matching Paren
(define-editor-variable *display-matching-paren* 1.0
  "Controls the length of time to put the cursor on the matching paren.
Set to zero the feature is disabled completely. When negative the
cursor does not leave the current screen to match.")

;The time spent finding the paren to match is included in the time above.
;When positive, allowing scrolling to display a matching paren, the
;time spent finding the paren may use up the available time. This variable
;is used to prevent this effect, without wasting time when the parens are
;close together.
(define-editor-variable minimum-display-time 0.23
  "This is a hack. The time spent finding the paren to match is included
in the time spent on it. For big s-exprs this can use up the whole second
so this variable is the minimum time spent anyway, after finding it.")

(define-editor-variable *display-prefixes* t
  "Controls whether Meta and Control-X prefixes are displayed in the echo
area. When this variable is non-null they will be displayed.")

;When T the meta-. command will look at the SUBR of a defined function
;to find its source file, and find-file that file. If this variable is
;nil, or if the argument is not the name of a function
;then meta-. will search all buffers for the defun.
(define-editor-variable *function-search* nil
  "Meta-. will try to find the function source code through the SUBR
and MODULE if this variable is not NIL")

;Causes the screen to begin on a page boundary when near it, and
;causes it to end when a page boundary is reached.
(define-editor-variable page-mode nil
  "Probably doesn't work. I was trying to implement INFO with this.
Perhaps it should be debugged, it might be usefull")

;Comment customizations.

(define-editor-variable comment-start ";"
  "A comment must begins with this string.
Also see COMMENT-BEGIN (now ~s) COMMENT-END (~s) COMMENT-COLUMN (~s)
and COMMENT-MULTI-LINE (~s)" comment-begin comment-end
                             comment-multi-line comment-column)

(define-editor-variable comment-begin ";"
  "When the editor inserts a comment it begins with this string.
Often this is COMMENT-START with a space after it. See COMMENT-START (now ~s)"
  comment-start)

(define-editor-variable comment-end ""
  "A comment ends with this string. The null string means end-of-line.
This must match COMMENT-START (~s) and COMMENT-BEGIN (~s)"
  comment-start comment-begin)

(define-editor-variable comment-column 40
  "When the editor adds or adjusts comments, they go in this column")

(define-editor-variable comment-multi-line 0
  "Not sure if this is used right.")

;See auto-push-mark in Aux.lsp.
(define-editor-variable auto-push-point-notification "Mark Set"
  "The string to type out when automatically setting a mark")

;Major Modes.

;Default major mode is set by file type.
;see the file NIL$DISK:[NIL.STEVE]MODES.LSP for its value.
(define-editor-variable default-major-mode-alist nil
  "Default major modes are set according to the file type of the
visited file, according to this variable.")

;This affects C-X =, and M-= etc.
(define-editor-variable *line-separator-length* 1
  "Functions which count the number of characters in a file or region
use this as the length of the line-separator. It is usefull for figuring
out how big a file would be on a pdp-10")

(define-editor-variable *file-versions-kept* 2
  "Controls how many file versions are kept by DIRED")

(define-editor-variable *temp-file-fn2-list* nil
  "Any file type which appears in this list is considered to be
a temporary file by DIRED")

(define-editor-variable *buffer-creation-hook* nil
  "A one-argument function to FUNCALL on every newly created buffer.
SETQ this to NIL to turn off the feature.")

(define-editor-variable *incremental-compiling-function*
  '(("LISP" COMPILE)
    )
  "The function used by C-M-C to compile a single function.
Currently this is FUNCALLed on the FORM to be compiled, which must
be READable. This could change in the future.")

(define-editor-variable *compiling-function*
  '(("LISP" compile-file)
    ("LSB" lsbcl))
  "The function used by M-X COMPILE to compile a file.
The function will be FUNCALLed on the pathname of the file to be
compiled.")
