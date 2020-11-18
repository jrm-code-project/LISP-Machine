;;; -*- Mode:LISP; Package:ZWEI; Base:8; Readtable:T -*-
;;; Zwei compiler commands, see ZWEI;COMA for comments
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFCOM COM-EVALUATE-MINI-BUFFER "Evaluate a form from the mini-buffer." (KM)
  (EVALUATE-MINI-BUFFER))

(DEFUN EVALUATE-MINI-BUFFER (&OPTIONAL INITIAL-CONTENTS INITIAL-CHAR-POS &AUX INTERVAL)
  "Read an expression with a mini buffer, and evaluate it.
INITIAL-CONTENTS is a string to initialize the contents from,
and INITIAL-CHAR-POS is where to put the cursor, as a number of
characters from the beginning."
  (MULTIPLE-VALUE (NIL NIL INTERVAL)
    (EDIT-IN-MINI-BUFFER *MINI-BUFFER-MULTI-LINE-COMTAB* INITIAL-CONTENTS INITIAL-CHAR-POS
                         '("Forms to evaluate (end with End)")))
  (LET ((FORM-STRING (STRING-INTERVAL INTERVAL)))
    (DO ((I 0)
         (FORM)
         (*READTABLE* *READTABLE*)
         (*READ-BASE* *PRINT-BASE*)
         (*PRINT-BASE* *PRINT-BASE*)
         (EOF '(())))
        (NIL)
      (CONDITION-CASE (ERROR)
          (MULTIPLE-VALUE (FORM I)
            (CLI:READ-FROM-STRING FORM-STRING NIL EOF :START I))
        (SYS:READ-ERROR
         (BARF (SEND ERROR :REPORT-STRING))
         (RETURN NIL)))
      (WHEN (EQ FORM EOF)
        (RETURN NIL))
      (DO ((VALS (LET ((*STANDARD-INPUT* SI:SYN-TERMINAL-IO))
                   (MULTIPLE-VALUE-LIST (SI:EVAL-ABORT-TRIVIAL-ERRORS FORM)))
                 (CDR VALS))
           (FLAG T NIL))
          ((NULL VALS))
        (UNLESS FLAG (SEND *QUERY-IO* :FRESH-LINE))
        (FORMAT *QUERY-IO* "~:[, ~]~S" FLAG (CAR VALS)))))
  DIS-TEXT)     ;DIS-TEXT in case user manually alters the buffer with Lisp code

(DEFCOM COM-EVALUATE-INTO-BUFFER
        "Evaluate a form from the mini-buffer and insert the results into the buffer.
If there are multiple values, each value is printed into the buffer,
with a Return before each one.
A numeric argument means output printed by the evaluation also goes in the buffer." (KM)
  (LET ((FORM (TYPEIN-LINE-MULTI-LINE-READ "Lisp form: (end with ~C)" #/END))
        (STREAM (INTERVAL-STREAM-INTO-BP (POINT))))
    (LET ((VALUES
            (MULTIPLE-VALUE-LIST
              (LET ((*STANDARD-OUTPUT* (IF *NUMERIC-ARG-P* STREAM *STANDARD-OUTPUT*)))
                (SI:EVAL-SPECIAL-OK FORM)))))
      (DOLIST (V VALUES)
        (TERPRI STREAM)
        (FUNCALL (OR PRIN1 #'PRIN1)
                 V STREAM)))
    (MOVE-BP (POINT) (SEND STREAM :READ-BP)))
  DIS-TEXT)

(DEFUN READ-OR-BARF (STREAM)
  (CONDITION-CASE (ERROR)
      (READ-PRESERVING-WHITESPACE STREAM)
    (SYS:READ-ERROR (BARF (STRING ERROR)))))

(DEFCOM COM-EVALUATE-AND-REPLACE-INTO-BUFFER
        "Evaluate the next s-expression and replace the result into the buffer.
The original expression is deleted and the value, printed out, replaces it."
        ()
  (LET* ((POINT (POINT)) (MARK (MARK))
         (STREAM (REST-OF-INTERVAL-STREAM POINT))
         (FORM (READ-OR-BARF STREAM))
         (VALUE (SI:EVAL-SPECIAL-OK FORM)))
    (MOVE-BP MARK (SEND STREAM :READ-BP))
    (WITH-UNDO-SAVE ("replacement" POINT MARK T)
      (FUNCALL (OR PRIN1 'PRIN1) VALUE STREAM)
      (WITH-BP (END (SEND STREAM :READ-BP) :NORMAL)
        (DELETE-INTERVAL POINT MARK T)
        (MOVE-BP POINT END))))
  DIS-TEXT)

(DEFCOM COM-EVALUATE-AND-PRINT-INTO-BUFFER
        "Evaluate the next s-expression and print standard output into the buffer.
The original expression is deleted and the output replaces it."
        ()
  (LET* ((POINT (POINT)) (MARK (MARK))
         (STREAM (REST-OF-INTERVAL-STREAM POINT))
         (FORM (READ-OR-BARF STREAM))
         ;(VALUE (SI:EVAL-SPECIAL-OK FORM))
         )
    (MOVE-BP MARK (SEND STREAM :READ-BP))
    (WITH-UNDO-SAVE ("replacement" POINT MARK T)
      ;(FUNCALL (OR PRIN1 'PRIN1) VALUE STREAM)
      (LET ((*STANDARD-OUTPUT* STREAM))
        (SI:EVAL-SPECIAL-OK FORM))
      (WITH-BP (END (SEND STREAM :READ-BP) :NORMAL)
        (DELETE-INTERVAL POINT MARK T)
        (MOVE-BP POINT END))))
  DIS-TEXT)

(DEFCOM COM-MACRO-EXPAND-EXPRESSION "Print macroexpansion of next s-expression.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL (MACROEXPAND FORM))))
  DIS-NONE)

(DEFCOM COM-MACRO-EXPAND-EXPRESSION-ALL "Print macroexpansion of next s-expression to all levels.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL (COMPILER:MACROEXPAND-ALL FORM))))
  DIS-NONE)

(DEFCOM COM-MACRO-EXPAND-EXPRESSION-INTO-BUFFER
        "Print macroexpansion of next s-expression at end of the current buffer.
If there are multiple values, each value is printed into the buffer,
with a Return before each one.
A numeric argument means output printed by the evaluation also goes in the buffer." (KM)
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL
        (MACROEXPAND FORM)
        (send *standard-output* :size-in-characters)
        (interval-stream-into-bp (INTERVAL-LAST-BP *INTERVAL*)))))
  DIS-TEXT)

(DEFCOM COM-MACRO-EXPAND-EXPRESSION-ALL-INTO-BUFFER
        "Print macroexpansion of next s-expression to all levels at end of current buffer."
        ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL
        (COMPILER:MACROEXPAND-ALL FORM)
        (send *standard-output* :size-in-characters)
        (interval-stream-into-bp (INTERVAL-LAST-BP *INTERVAL*)))))
  DIS-TEXT)

(DEFCOM COM-COMPILER-OPTIMIZE "Print compiler optimization of next s-expression.
The result is printed on the screen with GRIND-TOP-LEVEL." ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL (COMPILER:COMPILER-OPTIMIZE-EXTERNAL FORM))))
  DIS-NONE)

(DEFCOM COM-COMPILE-REGION "Compile the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled.
In that case, DEFVARs reset the variable even if already bound." ()
  (COMPILE-DEFUN-INTERNAL (or (get-buffer-compiler *interval*) T) "Compiling" "compiled.")
  DIS-NONE)

(DEFCOM COM-CAREFUL-COMPILE-REGION "Compile the current region or defun,
with optimizers and the peephole optimizer turnned off.  Try this if
you suspect a compiler bug.  If there is a region, it is compiled.
Otherwise, the current or next defun is compiled." ()
  (COMPILE-DEFUN-INTERNAL (or (get-buffer-compiler *interval*) T)
                          "Careful-compiling" "Careful-compiled."
                            NIL ;USE-TYPEOUT
                            NIL ;DEFVAR-HACK
                            '(:MODE COMPILER:MACRO-COMPILE :CAREFUL-MODE T))
  DIS-NONE)

(DEFCOM COM-MICROCOMPILE-REGION "Microcompile the current region or defun.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled." ()
  (COMPILE-DEFUN-INTERNAL (or (get-buffer-compiler *interval*) T)
                          "Microcompiling" "microcompiled."
                            NIL ;USE-TYPEOUT
                            NIL ;DEFVAR-HACK
                            '(:MODE COMPILER:MICRO-COMPILE))
  DIS-NONE)

(DEFCOM COM-CAREFUL-MICROCOMPILE-REGION "Microcompile the current region or defun,
with optimizers turnned off.  Try this if you suspect a microcompiler bug.
If there is a region, it is compiled.
Otherwise, the current or next defun is compiled." ()
  (COMPILE-DEFUN-INTERNAL (or (get-buffer-compiler *interval*) T)
                          "Careful-microcompiling" "Careful-microcompiled."
                            NIL ;USE-TYPEOUT
                            NIL ;DEFVAR-HACK
                            '(:MODE COMPILER:MICRO-COMPILE :CAREFUL-MODE T))
  DIS-NONE)

(DEFCOM COM-EVALUATE-REGION "Evaluate the current region or defun.
Result is typed out in the echo area.
If there is a region, it is evaluated.
Otherwise, the current or next defun is evaluated.
In that case, DEFVARs reset the variable even if already bound." ()
  (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
                           "Evaluating"
                           "evaluated."
                           :PROMPT)
  DIS-NONE)

(DEFCOM COM-EVALUATE-REGION-VERBOSE "Evaluate the current region or defun.
Result is typed out in the typeout window.
If there is a region, it is evaluated.
Otherwise, the current or next defun is evaluated.
In that case, DEFVARs reset the variable even if already bound." ()
  (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
                           "Evaluating"
                           "evaluated."
                           T)
  DIS-NONE)

(DEFCOM COM-EVALUATE-REGION-HACK "Evaluate the current region or defun.
DEFVARs reset the variable even if already bound.
If there is a region, it is evaluated.
Otherwise, the current or next defun is evaluated." ()
  (COMPILE-DEFUN-INTERNAL  (GET-BUFFER-EVALUATOR *INTERVAL*)
                           "Evaluating"
                           "evaluated."
                           :PROMPT T)
  DIS-NONE)

(DEFUN COMPILE-DEFUN-INTERNAL (COMPILE-P MODE-NAME ECHO-NAME
                               &OPTIONAL USE-TYPEOUT DEFVAR-HACK
                               (COMPILER-PROCESSING-MODE
                                 '(:MODE COMPILER:MACRO-COMPILE))
                               (*target-computer* 'compiler:lambda-interface)
                               (compilation-environment nil compilation-environment-p)  ;||| 14oct smh
                               &AUX BP1 BP2 DEFUN-NAME)
  "Compile or evaluate a part of the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
If there is a region, it is used; otherwise the current or following defun is used.
USE-TYPEOUT is passed to COMPILE-PRINT-INTERVAL and controls where information is printed.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (COND ((WINDOW-MARK-P *WINDOW*)
         (SETQ BP1 (MARK) BP2 (POINT))
         (OR (BP-< BP1 BP2) (PSETQ BP1 BP2 BP2 BP1))
         (SETQ DEFUN-NAME "Region"))
        ((SETQ BP1 (DEFUN-INTERVAL (BEG-LINE (POINT)) 1 NIL NIL))
         (SETQ BP2 (INTERVAL-LAST-BP BP1) BP1 (INTERVAL-FIRST-BP BP1))
         (SETQ DEFVAR-HACK T))
        (T
         (BARF "Cannot find a defun near point.")))
  (progv (and compilation-environment-p '(compiler:*compilation-environment*))  ;||| 14oct smh
         (list compilation-environment)
    ;; Add this binding to allow c-m from the debugger to do the correct thing.
    ;; ||| JIM 10/19/88
    (let ((eh:*bug-report-recipient-system* (if (eq *target-computer* 'compiler:k)
                                             "FALCON-COMPILER"
                                           "LISPM")))
      (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P
                              DEFUN-NAME MODE-NAME ECHO-NAME USE-TYPEOUT DEFVAR-HACK
                              COMPILER-PROCESSING-MODE
                              nil                       ;already-resectionized-flag
                              *target-computer*))))

(DEFCOM COM-EVALUATE-BUFFER "Evaluate the entire buffer." ()
  (COMPILE-BUFFER (GET-BUFFER-EVALUATOR *INTERVAL*) "Evaluating" "evaluated."))

(DEFCOM COM-COMPILE-BUFFER "Compile the entire buffer." ()
  (COMPILE-BUFFER (or (get-buffer-compiler *interval*) T) "Compiling" "compiled."))

(DEFCOM COM-MICROCOMPILE-BUFFER "Microcompile the entire buffer." ()
  (COMPILE-BUFFER (or (get-buffer-compiler *interval*) T) "Microcompiling" "microcompiled."
                  '(:MODE COMPILER:MICRO-COMPILE)))

(DEFUN COMPILE-BUFFER (COMPILE-P MODE-NAME ECHO-NAME
                       &OPTIONAL (COMPILER-PROCESSING-MODE
                                   '(:MODE COMPILER:MACRO-COMPILE))
                                 (*target-computer* 'compiler:lambda-interface)
                       &AUX BP1 BP2 NAME)
  "Compile or evaluate the current buffer.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
COMPILE-PROCESSING-MODE is a keyword list.  The :MODE component should be either
 COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowecase past participle and period (/"compiled./")."
  (IF *NUMERIC-ARG-P*
      (SETQ BP1 (POINT) BP2 (INTERVAL-LAST-BP *INTERVAL*) NAME "Rest of buffer")
      (SETQ BP1 *INTERVAL* NAME "Buffer"))
  (COMPILE-PRINT-INTERVAL BP1 BP2 T COMPILE-P NAME MODE-NAME ECHO-NAME
                          NIL                   ;USE-TYPEOUT
                          NIL                   ;DEFVAR-HACK
                          COMPILER-PROCESSING-MODE
                          nil                   ;already-resectionized-flag
                          *target-computer*)
  DIS-NONE)

(DEFUN GET-BUFFER-EVALUATOR (BUFFER)
  "Return the evaluate-and-print function for BUFFER, or NIL (the default)."
  (SEND BUFFER :GET-ATTRIBUTE :EVALUATOR))

(defun get-buffer-compiler (buffer)
  "Return the compile-and-print function for BUFFER, or NIL (the default)."
  (send buffer :get-attribute :compiler))

;; Copied from LAD: RELEASE-3.ZWEI; COMC.LISP#216 on 26-Mar-87 17:35:18
(DEFUN COMPILE-PRINT-INTERVAL (BP1 BP2 IN-ORDER-P COMPILE-P REGION-NAME MODE-NAME ECHO-NAME
                               &OPTIONAL USE-TYPEOUT DEFVAR-HACK
                               COMPILER-PROCESSING-MODE
                               ALREADY-RESECTIONIZED-FLAG
                               &optional (*target-computer* 'compiler:lambda-interface)
                               &AUX FORMAT-FUNCTION SUCCESS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
REGION-NAME is a string to print as the name of this whole object,
 or NIL to mention each object's name.
USE-TYPEOUT can be T, NIL, :TYPEOUT or :PROMPT.
  T prints form values and names of objects in typeout window.
 Otherwise, form values appear in the echo area, and
  :TYPEOUT prints names of objects in typeout window.
  :PROMPT prints names of objects in prompt line.
  NIL prints names of objects in the echo area.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is a keyword list selecting compiler options.  For compatibility,
 a symbol such as COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE is converted to
 (:mode <symbol>)
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization.
MODE-NAME is a string containing a capitalized present participle, such as /"Compiling/".
ECHO-NAME is a string containing a lowercase past participle and period (/"compiled./")."
  (cond ((null compiler-processing-mode)
         (setq compiler-processing-mode '(:mode compiler:macro-compile)))
        ((symbolp compiler-processing-mode)
         (setq compiler-processing-mode `(:mode ,compiler-processing-mode))))
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (UNLESS ALREADY-RESECTIONIZED-FLAG
    (CHECK-INTERVAL-SECTIONS BP1 BP2 T))
  (UNDO-SAVE-CURRENT-RANGE)
  (SETQ FORMAT-FUNCTION (CASE USE-TYPEOUT
                          ((T :TYPEOUT) #'(LAMBDA (STRING &REST ARGS)
                                            (APPLY 'FORMAT T STRING ARGS)))
                          (:PROMPT #'PROMPT-LINE-MORE)
                          (OTHERWISE #'(LAMBDA (STRING &REST ARGS)
                                        (APPLY 'FORMAT *QUERY-IO* STRING ARGS)))))
  (IF REGION-NAME
      (FUNCALL FORMAT-FUNCTION "~&~A ~A" MODE-NAME REGION-NAME)
    (FUNCALL FORMAT-FUNCTION "~&~A ~S" MODE-NAME (function-name-from-bp BP1)))
  (UNWIND-PROTECT
    (PROGN
      (COMPILE-INTERVAL COMPILE-P
                        (CASE USE-TYPEOUT
                          ((T) T)
                          (T *QUERY-IO*))
                        DEFVAR-HACK BP1 BP2 T
                        COMPILER-PROCESSING-MODE
                        *target-computer*)
      (SETQ SUCCESS T))
    (OR SUCCESS
        (FUNCALL FORMAT-FUNCTION " -- aborted.")))
  (FUNCALL FORMAT-FUNCTION " -- ~A" ECHO-NAME)
  (UPDATE-INTERVAL-COMPILE-TICK BP1 BP2 T))

;; Copied from LAD: RELEASE-3.ZWEI; COMC.LISP#216 on 26-Mar-87 17:35:19
(defun function-name-from-bp (bp)
  (let ((node (bp-node bp)))
    (if (typep node 'section-node)
        (section-node-name node)
      (funcall (get :lisp 'get-section-name) (car bp) bp))))

;Careful!  When you get around to changing this, leave
; COMPILE-P and COMPILE-PROCESSING-MODE as specials with the same meaning
; as now.  Prolog uses them while expanding its macros.  3/17/85
(DEFUN COMPILE-INTERVAL (COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK
                         BP1 BP2 IN-ORDER-P
                         COMPILE-PROCESSING-MODE
                         &optional (*target-computer* 'compiler:lambda-interface)
                         &AUX GENERIC-PATHNAME STREAM
                              WHOLE-FILE   ;T if processing the entire file.
                              SI:FDEFINE-FILE-DEFINITIONS)
  "Compile or evaluate the interval specified by BP1, BP2, IN-ORDER-P.
Does not print any sort of message saying what is being compiled,
does not know about sectionization.
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
PRINT-RESULTS-STREAM is a stream for printing the results of evaluation, or NIL not to print.
DEFVAR-HACK says always re-set variables if DEFVARs are evaluated.
 Normally this is only done if there is no region.
COMPILE-PROCESSING-MODE is a keyword list.  The :MODE item should be either
 COMPILER:MACRO-COMPILE or COMPILER:MICRO-COMPILE.
ALREADY-RESECTIONIZED-FLAG should be T to inhibit resectionization."
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM DEFVAR-HACK COMPILE-PROCESSING-MODE))
  (SETQ GENERIC-PATHNAME (SEND *INTERVAL* :GENERIC-PATHNAME))
  ;; Does not reparse the mode line; we should let the user decide whether to do that.!
  ;; Should not override the user's Set Package if he has done one.
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  ;; Decide whether the entire file is being processed or just a part.
  ;; If the whole file, we want to notice if any function present in the file previously
  ;; is now missing.  If just a part, anything we don't notice now we must assume
  ;; is elsewhere in the file.
  (SETQ WHOLE-FILE
        (AND (BP-= BP1 (INTERVAL-FIRST-BP *INTERVAL*))
             (BP-= BP2 (INTERVAL-LAST-BP *INTERVAL*))))
  (SETQ STREAM (INTERVAL-STREAM BP1 BP2 T))
  ;; Arrange for first read-error's location to be saved in q-reg ".".
  (REMPROP (MAKE-REGISTER-NAME #/.) 'POINT)
  (LET ((SI:*ALL-FREE-INTERPRETER-VARIABLE-REFERENCES-SPECIAL* T))
    (MULTIPLE-VALUE-BIND (VARS VALS) (SEND *INTERVAL* :ATTRIBUTE-BINDINGS)
      (PROGV VARS VALS
        (let ((compile-in-roots-prop (get *interval* :compile-in-roots)))
          (cond ((and (eq compile-p t)
                      compile-in-roots-prop
                      (not (cl:member (si:package-root-name *package*)
                                      compile-in-roots-prop
                                      :test 'string-equal)))
                 (cond ((not (= 1 (length compile-in-roots-prop)))
                        (fsignal "The current heirarchy ~S is not among those acceptable ~s."
                                 (si:package-root-name *package*)
                                 compile-in-roots-prop))
                       (t (format print-results-stream
                                  "  Transferring to hierarchy ~s" (car compile-in-roots-prop))
                          (pkg-goto (si:pkg-name *package*) nil
                                    (pkg-find-package (car compile-in-roots-prop))))))))
        (WHEN FS:THIS-IS-A-PATCH-FILE
          ;; If compiling out of the editor buffer of a patch file,
          ;; make sure the file itself is marked
          ;; so that Meta-. will behave right.
          (PUTPROP GENERIC-PATHNAME T :PATCH-FILE))
        ;; Bind off this flag -- our stream is not generating font changes
        ;; so READ should not try to remove any.
        (LET ((SI:READ-DISCARD-FONT-CHANGES NIL))
          (FLET ((DO-IT ()
                        (COMPILER:COMPILE-STREAM
                          STREAM
                          GENERIC-PATHNAME
                          NIL                   ;FASD-FLAG
                          (IF (AND COMPILE-P (NOT (EQ COMPILE-P T)))
                ;if using user supplied evaluator, avoid any possible macro-expanding, etc
                ; in COMPILE-DRIVER.
                              'SIMPLE-COMPILE-INTERVAL-PROCESS-FN
                            'COMPILE-INTERVAL-PROCESS-FN)
                          T                     ;QC-FILE-LOAD-FLAG
                          NIL                   ;QC-FILE-IN-CORE-FLAG
                          *PACKAGE*
                          NIL                   ;FILE-LOCAL-DECLARATIONS
                          NIL                   ;Unused
                          WHOLE-FILE
                          *target-computer*)))
            (IF COMPILE-P
                (COMPILER:LOCKING-RESOURCES-NO-QFASL (DO-IT))
              (DO-IT)))))))
  (OR (NULL GENERIC-PATHNAME)
      (SI:RECORD-FILE-DEFINITIONS GENERIC-PATHNAME SI:FDEFINE-FILE-DEFINITIONS WHOLE-FILE)))

(DEFUN COMPILE-INTERVAL-PROCESS-FN (FORM)
  (let ((compiler:*just-once-for-style-checkers-per-inner-form-alist* nil))
    (COMPILER:COMPILE-DRIVER FORM 'COMPILE-INTERVAL-PROCESS-BASIC-FORM
                             'COMPILE-INTERVAL-PREPROCESS-FN)))

;;;COMPILE-DRIVER does all sorts of macro-expand stuff these days, regardless of process-fn
;;;  and OVERRIDE-FN, so we bypass it if this buffer doesnt contain LISP anyway.
(DEFUN SIMPLE-COMPILE-INTERVAL-PROCESS-FN (FORM)
  (COMPILE-INTERVAL-PROCESS-BASIC-FORM FORM 'RANDOM))

;;; Record the name of what we are compiling, if this form makes it clear.
;;; Turn DEFVAR into SETQ if appropriate.
;;; If we are "evaluating", look for EVAL rather than COMPILE and LOAD in any EVAL-WHEN.
;;; Do not affect the processing of anything but EVAL-WHENs.
(DEFUN COMPILE-INTERVAL-PREPROCESS-FN (FORM)
  (DECLARE (SPECIAL COMPILE-P DEFVAR-HACK))
  ;; If appropriate, turn a DEFVAR into a SETQ.
  (WHEN (AND DEFVAR-HACK
             (CONSP FORM)
             (> (LENGTH FORM) 2)
             (MEMQ (CAR FORM) '(DEFVAR DEFVAR-RESETTABLE))
             (NEQ (CADDR FORM) ':UNBOUND))
    (UNLESS (SYMBOLP (CADR FORM))
      (FERROR NIL "~S not a recognized form" FORM))
    (PUTPROP (CADR FORM) T 'SPECIAL)            ;Declare it
    (WHEN (> (LENGTH FORM) 3)                   ;in case there is a documentation string.
      (SETF (DOCUMENTATION (SECOND FORM) 'VARIABLE) (SI:EVAL1 (FOURTH FORM)))
      (SETQ FORM (NBUTLAST FORM)))              ;remove documentation so that
                                                ;hack into SETQ works properly.
    (SETF (CAR FORM) 'SETQ))                    ;then always SETQ
  (WHEN (AND (NOT COMPILE-P) (EQ (CAR-SAFE FORM) 'EVAL-WHEN))
    (WHEN (MEMQ 'EVAL (CADR FORM))
      (MAPC #'COMPILE-INTERVAL-PROCESS-FN (CDDR FORM)))
    T))

;;; We get here when COMPILER:COMPILE-DRIVER finds something it doesn't handle specially.
(DEFUN COMPILE-INTERVAL-PROCESS-BASIC-FORM (FORM TYPE)
  (DECLARE (SPECIAL COMPILE-P PRINT-RESULTS-STREAM))
  ;; Really eval or compile the thing.
  (COND ((EQ COMPILE-P T)
         (COMPILE-BUFFER-FORM FORM TYPE))
        (COMPILE-P
         (FUNCALL COMPILE-P FORM))
        (T
         (EVAL-PRINT FORM PRINT-RESULTS-STREAM))))

(DEFUN EVAL-PRINT (OBJECT PRINT-RESULTS-STREAM)
  (LET ((LIST (MULTIPLE-VALUE-LIST (SYS:EVAL1 OBJECT))))
    (DOLIST (VAL LIST)
      (IF PRINT-RESULTS-STREAM
          (LET-IF (EQ PRINT-RESULTS-STREAM *QUERY-IO*)
                  ((*PRINT-LENGTH* 5) (*PRINT-LEVEL* 2))
            (FORMAT PRINT-RESULTS-STREAM "~&~S" VAL))))
    (VALUES (CAR LIST) OBJECT)))

;;; Functional to be passed to COMPILE-DRIVER.
;;; @@@ TYPE descrimination should *NOT* be in the bloody editor!!! <22-Nov-88 smh>
(DEFUN COMPILE-BUFFER-FORM (FORM TYPE)
  (DECLARE (SPECIAL COMPILE-PROCESSING-MODE))
  ;; $$$ Cosmetic changes, and added DEFAFUN <22-Nov-88 smh>
  (cond ((MEMQ TYPE '(DECLARE RANDOM SPECIAL proclaim))
         (EVAL FORM))
        ((eq type 'compiler::defafun)
         (compiler::compile-defafun form))
        (t (let-if (get-from-alternating-list compile-processing-mode :CAREFUL-MODE)
                   ((compiler:peep-enable nil)
                    (compiler:*inhibit-optimizers* t))
             (COMPILER:COMPILE-1 (CADR FORM)
                                 (APPEND
                                   (CASE (CAR FORM)
                                     (DEFSUBST '(NAMED-SUBST))
                                     (MACRO '(MACRO NAMED-LAMBDA))
                                     (DEFUN '(NAMED-LAMBDA)))
                                   (CDR (SI:PROCESS-DEFUN-BODY (CADR FORM) (CDDR FORM))))
                                 (IF (MEMQ (CAR FORM) '(MACRO DEFSUBST))
                                     'COMPILER:MACRO-COMPILE
                                   (GET-FROM-ALTERNATING-LIST COMPILE-PROCESSING-MODE :MODE)))))))

;;;; Correspondences

(DEFSIGNAL DEFINITION-NOT-UNIQUE FERROR (FUNCTION-SPEC)
  "Signaled by FUNCTION-CORRESPONDENCE when ZWEI can't tell which
text definition to make the correspondence with.")

(DEFUN FUNCTION-CORRESPONDENCE (FUNCTION-SPEC)
  "Given a function spec, make a correspondence from its definition to its text.
The function spec better have an interpreted definition.
If there is not a unique definition section for the function spec,
an error is signaled with condition name ZWEI:DEFINITION-NOT-UNIQUE.
The command M-X Make Correspondence can be used to tell ZWEI which
definition to prefer."
  (MULTIPLE-VALUE-BIND (BP BUFFER)
      (DEFINITION-TEXT-LOCATION-1 FUNCTION-SPEC)
    (IF BP
        (FUNCTION-CORRESPONDENCE-1 FUNCTION-SPEC BP BUFFER)
      (FERROR 'DEFINITION-NOT-UNIQUE "There is more than one definition of ~S." FUNCTION-SPEC))))

(DEFCOM COM-MAKE-CORRESPONDENCE "Make a correspondence table for this function.
The correspondence table maps between links in the list structure
of the function and positions in the text in the buffer." ()
  (CONDITION-CASE (ERROR)
      (FUNCTION-CORRESPONDENCE-1 (SECTION-NODE-NAME (LINE-NODE (BP-LINE (POINT))))
                                 (POINT) *INTERVAL*)
    (DEFINITION-NOT-SEXP
      (BARF ERROR)))
  DIS-NONE)

(DEFUN FUNCTION-CORRESPONDENCE-1 (FUNCTION BP BUFFER)
  (LET* ((LINE (CAR BP))
         (INT (DEFUN-INTERVAL (CREATE-BP LINE 0) 1 NIL NIL))
         (DEFINITION (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC FUNCTION)))
         NEWSEXP TEM
         (CORRESPONDENCE (SI:FUNCTION-SPEC-GET FUNCTION 'ZMACS-CORRESPONDENCE)))
    (COND ((OR (ATOM DEFINITION)
               (AND (EQ (CAR DEFINITION) 'MACRO)
                    (ATOM (CDR DEFINITION))))
           (FERROR 'DEFINITION-NOT-SEXP "The definition of ~S is not an s-expression."
                   FUNCTION)))
    (SI:FUNCTION-SPEC-PUTPROP FUNCTION
                              (CONS BUFFER
                                    (SECTION-NODE-DEFUN-LINE
                                      (LINE-NODE
                                        (BP-LINE (INTERVAL-FIRST-BP INT)))))
                              'ZMACS-CHOSEN-DEFINITION)
    (COND ((OR (NULL CORRESPONDENCE)
               (NEQ (CAR CORRESPONDENCE) DEFINITION)
               (> (INTERVAL-REAL-TICK INT) (CADDR CORRESPONDENCE)))
           ;; Read in the text.  Get a new sexp for the function,
           ;; together with a correspondence between it and the text.
           (MULTIPLE-VALUE (NEWSEXP CORRESPONDENCE)
             (ESTABLISH-CORRESPONDENCE DEFINITION BUFFER INT))
           (SETQ TEM (MEMQ NEWSEXP CORRESPONDENCE))
           (AND TEM (RPLACA TEM DEFINITION))
           (SETQ NEWSEXP (CDDR NEWSEXP))        ;Flush DEFUN or DEFMETHOD, and fn name.
           (SETQ DEFINITION (SI:LAMBDA-EXP-ARGS-AND-BODY DEFINITION))
           ;; Now the new sexp should look like the definition.
           ;; Move the correspondence to the definition.
           (TRANSFER-CORRESPONDENCE FUNCTION CORRESPONDENCE NEWSEXP DEFINITION)
           (SI:FUNCTION-SPEC-PUTPROP FUNCTION CORRESPONDENCE 'ZMACS-CORRESPONDENCE)))
    CORRESPONDENCE))

(DEFUN ESTABLISH-CORRESPONDENCE (DEFINITION BUFFER BP1 &OPTIONAL BP2 IN-ORDER-P)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (LET ((STREAM (INTERVAL-STREAM BP1 BP2 T))
        (SI:XR-CORRESPONDENCE-FLAG T)
        SI:XR-CORRESPONDENCE)
    (VALUES (READ STREAM)
            `(,DEFINITION ,BUFFER ,(NODE-TICK BUFFER)
              ,BP1 ,BP2 . ,SI:XR-CORRESPONDENCE))))

(DEFUN INTERVAL-REAL-TICK (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Return the latest tick at which any line in an interval was modified.
Pass either an interval or a pair of BPs."
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((LINE (BP-LINE BP1) (LINE-NEXT LINE))
       (FIRST-LINE (BP-LINE BP1))
       (MAX-TICK 0)
       (LIMIT (BP-LINE BP2)))
      (NIL)
    (SETQ MAX-TICK
          (MAX MAX-TICK
               (LINE-TICK LINE)
               (OR (AND (NEQ LINE FIRST-LINE)
                        (GET (LOCF (LINE-PLIST LINE)) 'PRECEDING-LINES-DELETED-TICK))
                   0)))
    (IF (EQ LINE LIMIT)
        (RETURN MAX-TICK))))

;;; Given a correspondence from the sexp TEMPDEF, matches up TEMPDEF
;;; and REALDEF and clobbers the correspondence to be from REALDEF instead.
;;; FUNCTION is just for error messages.
;;; We throw to TRANSFER-CORRESPONDENCE-LOSSAGE if the two sexps don't match.
(DEFUN TRANSFER-CORRESPONDENCE (FUNCTION CORRESPONDENCE TEMPDEF REALDEF)
  (LET ((TEM (MEMQ TEMPDEF CORRESPONDENCE)))
    (AND TEM (RPLACA TEM REALDEF)))
  ;; In the real definition, some displacing macros may have gone off.
  (AND (EQ (CAR REALDEF) 'SI:DISPLACED)
       (SETQ REALDEF (CADR REALDEF)))
  (OR (= (LENGTH TEMPDEF) (LENGTH REALDEF))
      (*THROW 'TRANSFER-CORRESPONDENCE-LOSSAGE NIL))
  (DO ((TD TEMPDEF (CDR TD))
       (RD REALDEF (CDR RD)))
      ((NULL TD))
    (AND (COND ((ATOM (CAR TD)) (NOT (EQUAL (CAR TD) (CAR RD))))
               (T (ATOM (CAR RD))))
         (*THROW 'TRANSFER-CORRESPONDENCE-LOSSAGE NIL))
    (OR (ATOM (CAR TD))
        (TRANSFER-CORRESPONDENCE FUNCTION CORRESPONDENCE (CAR TD) (CAR RD)))))

;;;; These functions know about zmacs buffers and sections.

(DEFUN UPDATE-INTERVAL-COMPILE-TICK (BP1 &OPTIONAL BP2 IN-ORDER-P)
  "Update the tick-of-last-compilation for all sections in an interval.
Pass either an interval or a pair of BPs."
  (TICK)
  (GET-INTERVAL BP1 BP2 IN-ORDER-P)
  (DO ((NODE (BP-NODE BP1) (NODE-NEXT NODE))
       (FIRST T NIL)
       TEM)
      ((OR (NULL NODE)
           (NOT (OR FIRST (BP-< (INTERVAL-FIRST-BP NODE) BP2)))))
    (WHEN (OR (NOT FIRST)
              ;; If compiled or evaluated only part of the text in a node,
              ;; don't set its compile tick.
              ;; Now that there is only one form per section,
              ;; we can be confident that if the compiled code
              ;; started at the beginning of the form,
              ;; it must have reached the end,
              ;; unless either the compilation bombed out from unmatched parens
              ;; or the section contains unmatched parens.
              (EQ (BP-LINE BP1) (BP-LINE (INTERVAL-FIRST-BP NODE)))
              (AND (SETQ TEM (SEND NODE :SEND-IF-HANDLES :DEFUN-LINE))
                   (BP-< BP1 (CREATE-BP TEM 1))))
      (SEND NODE :UPDATE-COMPILE-TICK))))

(DEFCOM COM-COMPILE-BUFFER-CHANGED-SECTIONS
        "Compile any sections in this buffer which have been edited.
Only sections that contain definitions will be compiled.
A numeric arg means ask about each section individually."
        ()
  (SI:FILE-OPERATION-WITH-WARNINGS
    ((AND (BUFFER-FILE-ID *INTERVAL*)
          (SEND (SEND *INTERVAL* :GENERIC-PATHNAME) :GENERIC-PATHNAME))
     :COMPILE NIL)
    (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
      (COMPILE-BUFFER-CHANGED-FUNCTIONS *INTERVAL* *NUMERIC-ARG-P*)))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFMACRO DO-CHANGED-LISP-BUFFERS ((BUFFER BUFFER-LIST) &BODY BODY)
  "This is careful to not use random buffers."
  `(DOLIST (,BUFFER ,BUFFER-LIST)
     (WHEN (AND (EQ (IF (EQ ,BUFFER *INTERVAL*)
                        *MAJOR-MODE*
                      (BUFFER-SAVED-MAJOR-MODE ,BUFFER))
                    'LISP-MODE)
                (NOT (GET ,BUFFER 'SPECIAL-PURPOSE))
                ;; Don't consider buffers never modified.
                (> (NODE-TICK ,BUFFER)
                   (BUFFER-FILE-READ-TICK BUFFER)))
       ,@BODY)))

(DEFCOM COM-COMPILE-CHANGED-SECTIONS "Compile any sections which have been edited.
Only sections that contain definitions will be compiled.
A numeric arg means ask about each section individually."
        ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER *ZMACS-BUFFER-LIST*)
    (SI:FILE-OPERATION-WITH-WARNINGS
      ((AND (BUFFER-FILE-ID BUFFER)
            (SEND (SEND BUFFER :GENERIC-PATHNAME) :GENERIC-PATHNAME))
       :COMPILE NIL)
      (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
        (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-TAGS-COMPILE-CHANGED-SECTIONS "Compile any sections in files in tag table which have been edited.
Only sections that contain definitions will be compiled.
A numeric arg means ask about each section individually." ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER (TAG-TABLE-BUFFERS NIL))
    (SI:FILE-OPERATION-WITH-WARNINGS
      ((AND (BUFFER-FILE-ID BUFFER)
            (SEND (SEND BUFFER :GENERIC-PATHNAME) :GENERIC-PATHNAME))
       :COMPILE NIL)
      (COMPILER:COMPILER-WARNINGS-CONTEXT-BIND
        (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*))))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-EVALUATE-BUFFER-CHANGED-SECTIONS
        "Evaluate any sections in this buffer which have been edited.
Only sections that contain definitions will be evaluated.
A numeric arg means ask about each section individually." ()
  (COMPILE-BUFFER-CHANGED-FUNCTIONS *INTERVAL* *NUMERIC-ARG-P* NIL
                                    '("Evaluate" "Evaluating" "evaluated."))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-EVALUATE-CHANGED-SECTIONS "Evaluate any sections which have been edited.
Only sections that contain definitions will be evaluated.
A numeric arg means ask about each section individually."
        ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER *ZMACS-BUFFER-LIST*)
    (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*
                                      NIL '("Evaluate" "Evaluating" "evaluated.")))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFCOM COM-TAGS-EVALUATE-CHANGED-SECTIONS
  "Evaluate any sections in files in tag table which have been edited.
Only sections that contain definitions will be evaluated.
A numeric arg means ask about each section individually."
  ()
  (DO-CHANGED-LISP-BUFFERS (BUFFER (TAG-TABLE-BUFFERS NIL))
    (COMPILE-BUFFER-CHANGED-FUNCTIONS BUFFER *NUMERIC-ARG-P*
                                      NIL '("Evaluate" "Evaluating" "evaluated.")))
  (FORMAT T "~&Done.~%")
  DIS-NONE)

(DEFUN COMPILE-BUFFER-CHANGED-FUNCTIONS (BUFFER ASK-P
                                         &OPTIONAL (COMPILE-P T)
                                         (NAMES '("Compile" "Compiling" "compiled."))
                                         (*target-computer* 'compiler:lambda-interface)
                                         &AUX (*QUERY-IO* *STANDARD-OUTPUT*))
  "Recompile or evaluate all changed sections in BUFFER (that contain definitions).
COMPILE-P is T to compile, NIL to eval, or else a function to evaluate and print a form.
ASK-P if non-NIL means query user for each section to be processed.
NAMES has three elements, that are like (/"Compile/" /"Compiling/" /"compiled./")."
  (LET ((*INTERVAL* BUFFER))
    (RESECTIONIZE-BUFFER *INTERVAL*)
    (DOLIST (SECTION (NODE-INFERIORS *INTERVAL*))
      (IF (AND (TYPEP SECTION 'SECTION-NODE)
               (NOT (STRINGP (SECTION-NODE-NAME SECTION)))
               (NOT (BP-= (INTERVAL-FIRST-BP SECTION) (INTERVAL-LAST-BP SECTION)))
               (> (NODE-TICK SECTION)
                  (SECTION-NODE-COMPILE-TICK SECTION))
               (OR (NOT ASK-P)
                   (FQUERY () "~A ~A? " (FIRST NAMES)
                           (SECTION-NODE-NAME SECTION))))
          (COMPILE-PRINT-INTERVAL SECTION NIL T COMPILE-P
                                  NIL                   ;region-name
                                  (SECOND NAMES)        ;mode-name
                                  (THIRD NAMES)         ;echo-name
                                  T                     ;use-typeout
                                  T                     ;defvar-hack
                                  NIL                   ;compiler-processing-mode
                                  T                     ;already-resectionized-flag
                                  *target-computer*
                                  )))))
