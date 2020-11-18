;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;; --- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Readtable:CL; Base:10 ---

(DEFUN LISP-TOP-LEVEL1 (*TERMINAL-IO* &OPTIONAL (TOP-LEVEL-P T) &AUX OLD-PACKAGE W-PKG)
  "Read-eval-print loop used by lisp listeners.
*TERMINAL-IO* is the stream with which to read and print."
  (LET-IF (VARIABLE-BOUNDP *PACKAGE*) ((*PACKAGE* *PACKAGE*))
    (WHEN (FBOUNDP 'FORMAT)
      (FORMAT T "~&;Reading~:[~; at top level~]~@[ in ~A~]."
              TOP-LEVEL-P (SEND-IF-HANDLES *TERMINAL-IO* :NAME)))
    (PUSH NIL *VALUES*)
    (DO ((*READTABLE* *READTABLE*)
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
