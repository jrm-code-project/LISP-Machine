;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Readtable:CL; Base:10 -*-

;;; Lisp read-eval-print loop.  Used to be in SYS; LTOP

;; who uses this loser? --- answer: (:method tv:lisp-listener :lisp-listener-p).  Blorge.
(DEFCONST LISP-TOP-LEVEL-INSIDE-EVAL nil
  "Bound to T while within EVAL inside the top-level loop.")

;; these are resettable in case they are something obscene as a result of warm-boot,
;;  thus making the rep loops fail.  In any case, their values are saved in *values*
(DEFVAR-RESETTABLE * NIL NIL
  "Value of last expression evaluated by read-eval-print loop.")
(DEFVAR-RESETTABLE ** NIL NIL
  "Value of next-to-last expression evaluated by read-eval-print loop.")
(DEFVAR-RESETTABLE *** NIL NIL
  "Value of third-to-last expression evaluated by read-eval-print loop.")

(DEFVAR-RESETTABLE + NIL NIL
  "Last expression evaluated by read-eval-print loop.")
(DEFVAR-RESETTABLE ++ NIL NIL
  "Next-to-last expression evaluated by read-eval-print loop.")
(DEFVAR-RESETTABLE +++ NIL NIL
  "Third-to-last expression evaluated by read-eval-print loop.")

(DEFVAR-RESETTABLE CL:/ NIL NIL
  "All values of last expression evaluated by read-eval-print loop.")
(DEFVAR ZL:/ NIL "All values of last expression evaluated by read-eval-print loop.")
(FORWARD-VALUE-CELL 'ZL:/ 'CL:/)
(DEFVAR-RESETTABLE // NIL NIL
  "All values of next-to-last expression evaluated by read-eval-print loop.")
(DEFVAR-RESETTABLE /// NIL NIL
  "All values of third-to-last expression evaluated by read-eval-print loop.")
; what is the use of this?
(DEFVAR-RESETTABLE - NIL NIL
  "Expression currently being evaluated by read-eval-print loop.")
(DEFVAR *VALUES* NIL
  "List of all lists-of-values produced by the expressions evaluated in this listen loop.
Most recent evaluations come first on the list.")

;; this really doesn't belong here.
(defun set-in-process (process var val)
  (check-type process si:process)
  (unless (si::process-simple-p process)
    (condition-case ()
        (setf (symeval-in-stack-group var (process-stack-group process))
              val)
      (error))))

(add-initialization "Nuke *VALUES*"
                    ;; i think this is an extremely dubious way to
                    ;; clean up lisp listeners. -gjc
                    '(dolist (p active-processes)
                       (and (car p) (set-in-process (car p) '*values* nil)))
                    :before-cold)


;;; Simple version of FERROR to be used in the cold load environment.
(DEFUN FERROR-COLD-LOAD (&REST ARGS)
  (SETQ * ARGS)
  (TERPRI) (DOTIMES (I 70.) (WRITE-CHAR #\-))
  (PRINT (CAR ARGS))
  (REP-COLD-LOAD 'FERROR))

(DEFUN CERROR-COLD-LOAD (&REST ARGS)
  (SETQ * ARGS)
  (TERPRI) (DOTIMES (I 70.) (WRITE-CHAR #\-))
  (PRINT (CAR ARGS))
  (REP-COLD-LOAD "CERROR:  Throw to SI::REP-COLD-LOAD to continue."))

(DEFUN REP-COLD-LOAD (&OPTIONAL STRING (*TERMINAL-IO* COLD-LOAD-STREAM))
  (CATCH 'REP-COLD-LOAD
    (TERPRI) (PRINC "Cold load REP: ") (PRINC STRING) (TERPRI)
    (ERROR-RESTART-LOOP ((SYS:ABORT DBG:DEBUGGER-CONDITION) "Return to REP-COLD-LOAD.")
      (TERPRI)
      (SETQ +++ ++ ++ + + -)
      (SETQ - (READ-FOR-TOP-LEVEL))
      (LET (VALUES)
        (UNWIND-PROTECT
            (SETQ VALUES (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
          ;; Always push SOMETHING -- NIL if evaluation is aborted.
          (PUSH VALUES *VALUES*))
        (SHIFTF /// // / VALUES)
        (SHIFTF *** ** * (CAR /)))
      (DOLIST (VALUE /)
        (TERPRI)
        (PRIN1 VALUE)))))


(DEFUN LISP-TOP-LEVEL1 (*TERMINAL-IO* &OPTIONAL (TOP-LEVEL-P T) &AUX OLD-PACKAGE W-PKG)
  "Read-eval-print loop used by lisp listeners.
*TERMINAL-IO* is the stream with which to read and print."
  (LET-IF (VARIABLE-BOUNDP *PACKAGE*) ((*PACKAGE* *PACKAGE*))
    (WHEN (FBOUNDP 'FORMAT)
      (FORMAT T "~&;Reading~:[~; at top level~]~@[ in ~A~]."
              TOP-LEVEL-P (SEND-IF-HANDLES *TERMINAL-IO* :NAME)))
    (PUSH NIL *VALUES*)
    (DO ((*READTABLE* (if (fboundp 'symbol-value-globally)
                          (SYMBOL-VALUE-GLOBALLY '*READTABLE*)
                        *readtable*))
         (LAST-TIME-READTABLE NIL)
         THROW-FLAG)    ;Gets non-NIL if throw to COMMAND-LEVEL (e.g. quitting from an error)
        (NIL)           ;Do forever
      ;; If *PACKAGE* has changed, set OLD-PACKAGE and tell our window.
      ;; Conversely, if the window's package has changed, change ours.
      ;; The first iteration, we always copy from the window.
      (COND ((NOT (VARIABLE-BOUNDP *PACKAGE*)))
            ((EQ *TERMINAL-IO* COLD-LOAD-STREAM))
            ;; User set the package during previous iteration of DO
            ;; => tell the window about it.
            ((AND OLD-PACKAGE (NEQ *PACKAGE* OLD-PACKAGE))
             (SEND-IF-HANDLES *TERMINAL-IO* :SET-PACKAGE *PACKAGE*)
             (SETQ OLD-PACKAGE *PACKAGE*))
            ;; Window's package has been changed, or first iteration through DO,
            ;; => set our package to the window's -- if the window has one.
            ((SETQ W-PKG (SEND-IF-HANDLES *TERMINAL-IO* :PACKAGE))
             (AND (NEQ W-PKG *PACKAGE*)
                  (SETQ *PACKAGE* W-PKG))
             (SETQ OLD-PACKAGE *PACKAGE*))
            ;; First time ever for this window => set window's package
            ;; to the global value of *PACKAGE*.
            ((NULL OLD-PACKAGE)
             (SETQ OLD-PACKAGE *PACKAGE*)
             (SEND-IF-HANDLES *TERMINAL-IO* :SET-PACKAGE *PACKAGE*)))
      (CHECK-FOR-READTABLE-CHANGE LAST-TIME-READTABLE)
      (SETQ LAST-TIME-READTABLE *READTABLE*)
      (SETQ THROW-FLAG T)
      (CATCH-ERROR-RESTART ((SYS:ABORT DBG:DEBUGGER-CONDITION) "Return to top level in ~A."
                            (OR (SEND-IF-HANDLES *TERMINAL-IO* :NAME) "current process."))
        (TERPRI)
        (SETQ +++ ++ ++ + + -)                  ;Save last three input forms
        (SETQ - (READ-FOR-TOP-LEVEL))
        (LET ((LISP-TOP-LEVEL-INSIDE-EVAL T)
              VALUES)
          (UNWIND-PROTECT
              (SETQ VALUES (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
            ;; Always push SOMETHING -- NIL if evaluation is aborted.
            (PUSH VALUES *VALUES*))
          (SETQ /// //
                // /
                / VALUES)
          (SETQ *** **                          ;Save first value, propagate old saved values
                ** *
                * (CAR /)))
        (DOLIST (VALUE /)
          (TERPRI)
          (FUNCALL (OR PRIN1 #'PRIN1) VALUE))
        (SETQ THROW-FLAG NIL))
      (WHEN THROW-FLAG
        ;; Inform user of return to top level.
        (FORMAT T "~&;Back to top level~@[ in ~A~]."
                (SEND-IF-HANDLES *TERMINAL-IO* :NAME))))))

(defun check-for-readtable-change (last-time-readtable)
  "Says something about the readtable if (neq *readtable* last-time-readtable)"
  (cond ((eq *readtable* last-time-readtable)
         nil)
        (t
         (when (fboundp 'format)
           (format t "~&;Reading in base ~D in package ~A with ~A.~&"
                   *read-base* *package* *readtable*))
         t)))

(defun common-lisp (flag &optional globally-p &aux (old-rdtbl *readtable*))
  "Makes the default syntax be either Common Lisp (if FLAG is non-NIL)
or Traditional Zetalisp (if FLAG is NIL)"
  (setq *readtable* (if flag common-lisp-readtable standard-readtable))
  (setq zwei:*default-readtable* *readtable*)
  (when globally-p
    (setq-globally *readtable* *readtable*)
    (setq-globally zwei:*default-readtable* *readtable*))
  (if (eq *readtable* old-rdtbl) flag (values)))

(DEFVAR *BREAK-BINDINGS*
        '((RUBOUT-HANDLER NIL)                  ;Start new level of rubout catch
          (READ-PRESERVE-DELIMITERS NIL)        ;For normal Lisp syntax
          (READ-CHECK-INDENTATION NIL)
          (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
          (*STANDARD-INPUT* SYN-TERMINAL-IO)    ;Rebind streams to terminal
          (*STANDARD-OUTPUT* SYN-TERMINAL-IO)
          (*QUERY-IO* SYN-TERMINAL-IO)
          (EH:CONDITION-HANDLERS NIL)           ;Condition wall for conditions
          (EH:CONDITION-DEFAULT-HANDLERS NIL)
          (LOCAL-DECLARATIONS NIL)
          (SELF-FLAVOR-DECLARATION NIL)
          ;; must use FUNCALL in the line below as the cold-load cannot hack the macro "SEND"
          (*READTABLE* (IF (EQ (FUNCALL *READTABLE* :GET :SYNTAX) ':COMMON-LISP)
                           COMMON-LISP-READTABLE
                           STANDARD-READTABLE))
          ;Changed 3/3/80 by Moon not to bind *, +, and -.
          )
  "Bindings to be made by the function BREAK.
Each element is a list (VARNAME VALUE-FORM) describing one binding.
Bindings are made sequentially.")

;;; Note that BREAK binds RUBOUT-HANDLER to NIL so that a new level of catch
;;; will be established.  Before returning it restores the old rubout handler's buffer.
(DEFUN BREAK (&OPTIONAL FORMAT-STRING &REST FORMAT-ARGS)
  "Read-eval-print loop for use as subroutine.  Args are passed to FORMAT.
Many variables are rebound, as specified in SI::*BREAK-BINDINGS*."
  (SETQ FORMAT-STRING (STRING FORMAT-STRING))
  (when (not qld-mini-done)     ;*in-cold-load-p*
    (terpri) (princ "BREAK called.")
    (return-from break (apply #'cerror-cold-load format-string format-args)))
  (UNLESS (OR (EQUAL FORMAT-STRING "")
              (MEMQ (CHAR FORMAT-STRING (1- (LENGTH FORMAT-STRING))) '(#\. #\? #\!)))
    (SETQ FORMAT-STRING (STRING-APPEND FORMAT-STRING #\.)))
  (let ((*package* (if ;; kludge alert -- am I a STANDARD VALUE yet?  Or am I suffering in SAFEWAY
                       (or (not (variable-boundp *package*))
                           (null *package*)
                           (not (or (memq pkg-global-package (pkg-use-list *package*))
                                    (memq pkg-lisp-package (pkg-use-list *package*))))
                           (pkg-read-lock-p *package*)
                           (pkg-auto-export-p *package*))
                       pkg-user-package
                     *package*))
        (OLD-STANDARD-INPUT *STANDARD-INPUT*)
        (OLD-QUERY-IO *QUERY-IO*))
    (declare (unspecial old-query-io old-standard-input);so we can compile this file correctly in 104
             (ignore old-query-io))             ;so luser can find it in stack frame
    (PROGW *BREAK-BINDINGS*
      ;; Deal with keyboard multiplexing in a way similar to the error-handler.
      ;; If we break in the scheduler, set CURRENT-PROCESS to NIL.
      ;; If this is not the scheduler process, make sure it has a run reason
      ;; in case we broke in the middle of code manipulating process data.
      ;; If INHIBIT-SCHEDULING-FLAG is set, turn it off and print a warning.
      (WHEN (AND (BOUNDP 'SCHEDULER-STACK-GROUP)
                 (EQ %CURRENT-STACK-GROUP SCHEDULER-STACK-GROUP))
        (SETQ CURRENT-PROCESS NIL))
      (AND (NOT (NULL CURRENT-PROCESS))
           (NULL (SEND CURRENT-PROCESS :RUN-REASONS))
           (SEND CURRENT-PROCESS :RUN-REASON 'BREAK))
      (WHEN INHIBIT-SCHEDULING-FLAG
        (FORMAT T "~%---> Turning off INHIBIT-SCHEDULING-FLAG, you may lose. <---~%")
        (SETQ INHIBIT-SCHEDULING-FLAG NIL))
      (MULTIPLE-VALUE-BIND (SAVED-BUFFER SAVED-BUFFER-POSITION)
          (SEND-IF-HANDLES OLD-STANDARD-INPUT :SAVE-RUBOUT-HANDLER-BUFFER)
        (FORMAT T "~&;Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
                FORMAT-STRING FORMAT-ARGS #\RESUME #\ABORT)
        (LET* ((LAST-TIME-READTABLE NIL)
               VALUE)
          (DO-FOREVER
            (CHECK-FOR-READTABLE-CHANGE LAST-TIME-READTABLE)
            (SETQ LAST-TIME-READTABLE *READTABLE*)
            (TERPRI)
           LOOK-FOR-SPECIAL-KEYS
            (LET ((CHAR (SEND *STANDARD-INPUT* :TYI)))
              ;; Intercept characters even if otherwise disabled in program broken out of.
              (COND ((AND (BOUNDP 'TV::KBD-STANDARD-INTERCEPTED-CHARACTERS)
                          (ASSQ CHAR TV::KBD-STANDARD-INTERCEPTED-CHARACTERS))
                     (FUNCALL (CADR (ASSQ CHAR TV:KBD-STANDARD-INTERCEPTED-CHARACTERS))
                              CHAR))
                    ((= CHAR (CHAR-INT #\RESUME))
                     (SEND *STANDARD-OUTPUT* :STRING-OUT "[Resume]")
                     (TERPRI)
                     (RETURN NIL))
                    (T (SEND *STANDARD-INPUT* :UNTYI CHAR))))
            ;; Hide earlier dynamuc resume handlers
            (LET ((EH::CONDITION-RESUME-HANDLERS (CONS T EH::CONDITION-RESUME-HANDLERS))
                  (THROW-FLAG T))
              (CATCH-ERROR-RESTART ((SYS:ABORT DBG:DEBUGGER-CONDITION) "Return to BREAK ~?"
                                    FORMAT-STRING FORMAT-ARGS)
                (MULTIPLE-VALUE-BIND (TEM1 TEM)
                    (WITH-INPUT-EDITING (*STANDARD-INPUT* '((:FULL-RUBOUT :FULL-RUBOUT)
                                                            (:ACTIVATION CHAR= #\END)))
                      (READ-FOR-TOP-LEVEL))
                  (IF (EQ TEM ':FULL-RUBOUT)
                      (GO LOOK-FOR-SPECIAL-KEYS))
                  (SHIFTF +++ ++ + - TEM1))
                (WHEN (EQ (CAR-SAFE -) 'RETURN) ;(RETURN form) proceeds
                  (SETQ VALUE (EVAL-ABORT-TRIVIAL-ERRORS (CADR -)))
                  (RETURN))
                (LET (VALUES)
                  (UNWIND-PROTECT
                      (SETQ VALUES
                            (MULTIPLE-VALUE-LIST (EVAL-ABORT-TRIVIAL-ERRORS -)))
                    ;; Always push SOMETHING for each form evaluated.
                    (PUSH VALUES *VALUES*))
                  (SHIFTF /// // / VALUES)
                  (SHIFTF *** ** * (CAR /))
                  (DOLIST (VALUE /)
                    (TERPRI)
                    (FUNCALL (OR PRIN1 #'PRIN1) VALUE))
                  (SETQ THROW-FLAG NIL)))
              (WHEN THROW-FLAG
                (FORMAT T "~&;Back to Breakpoint ~?  ~:@C to continue, ~:@C to quit.~%"
                        FORMAT-STRING FORMAT-ARGS #\RESUME #\ABORT))))
          ;; Before returning, restore and redisplay rubout handler's buffer so user
          ;; gets what he sees, if we broke out of reading through the rubout handler.
          ;; If we weren't inside there, the rubout handler buffer is now empty because
          ;; we read from it, so leave it alone.  (Used to :CLEAR-INPUT).
          (WHEN SAVED-BUFFER
            (SEND OLD-STANDARD-INPUT :RESTORE-RUBOUT-HANDLER-BUFFER
                  SAVED-BUFFER SAVED-BUFFER-POSITION))
          VALUE)))))
