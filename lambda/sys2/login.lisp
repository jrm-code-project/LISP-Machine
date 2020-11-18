;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-
;;; LISP Machine Package for Logging In and Out.                DLW 11/13/77
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; List of forms to be evaluated on logout
;;; to undo the things done at login.
;;; The various LOGIN-MUMBLE functions push undo forms on this list.
(DEFVAR LOGOUT-LIST NIL "List of forms to evaluate on logout, to undo effects of init file.")

;;; History so we can tell who has had their little paws into a saved band.
(DEFVAR LOGIN-HISTORY NIL
  "Each element is (user-name host-object-logged-into local-pretty-host-name date-as-string)")

(DEFUN PRINT-LOGIN-HISTORY (&OPTIONAL (STREAM STANDARD-OUTPUT) (HISTORY LOGIN-HISTORY))
  "Print out information on who has used this machine onto STREAM."
  (FORMAT STREAM "~&Who~15TLogin host~30TPhysical Lisp Machine~60TDate  and  time ")
  (FORMAT STREAM "~&---~15T----------~30T---------------------~60T----------------")
  (DOLIST (ENTRY HISTORY)
    ;;"~%~A at ~A~@[~28T~A~]~@[~46T~\TIME\~]
    (FORMAT STREAM "~&~A~15T~A~30T~A~60T~A"
            (FIRST ENTRY)                       ;who
            (SECOND ENTRY)                      ;login host
            (THIRD ENTRY)                       ;machine
            (OR (FOURTH ENTRY) "     unknown")));time
  (VALUES))

(DEFUN LOGIN (USER-NAME &OPTIONAL (HOST ASSOCIATED-MACHINE) INHIBIT-INIT-FILE-P)
  "Log in, specifying user name and host.
This identifies you, for the sake of other users running FINGER.
You cannot access files until you have logged in.
You can log in on any host that files can be read from, including
the local file system if one is loaded."
  ;; Do this so LOGIN init list has the correct enviroment.
  (DECLARE (SPECIAL USER-ID HOST))
  (LET ((WIN-P NIL)
        (LOAD-INIT-FILE-P (NOT INHIBIT-INIT-FILE-P)))
    (DECLARE (SPECIAL LOAD-INIT-FILE-P))
    (UNWIND-PROTECT
      (PROGN
        (LOGOUT)
        (AND (EQ HOST T)                        ;For compatibility
             (SETQ HOST ASSOCIATED-MACHINE LOAD-INIT-FILE-P NIL))
        (SETQ USER-ID (STRING-TRIM '(#/SP) (STRING USER-NAME)))
        (SETQ HOST (FS:GET-PATHNAME-HOST HOST))
        (SETQ FS:USER-LOGIN-MACHINE HOST)
        (INITIALIZATIONS 'LOGIN-INITIALIZATION-LIST)
        (RESET-INITIALIZATIONS 'LOGOUT-INITIALIZATION-LIST)
        (PUSH (LIST USER-ID HOST
                    (AND (BOUNDP 'LOCAL-PRETTY-HOST-NAME) LOCAL-PRETTY-HOST-NAME)
                    (AND (FBOUNDP 'TIME:PRINT-CURRENT-TIME) (TIME:PRINT-CURRENT-TIME NIL)))
              LOGIN-HISTORY)
        (PUSH (CONS HOST USER-ID) FS:USER-UNAMES)
        (FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:*DEFAULT-PATHNAME-DEFAULTS*)
        ;;(FS:SET-DEFAULT-PATHNAME (FS:USER-HOMEDIR) FS:LOAD-PATHNAME-DEFAULTS)
        (SETQ WIN-P T)
        (WHEN LOAD-INIT-FILE-P
          (CONDITION-CASE ()
              (LOAD (FS:INIT-FILE-PATHNAME "LISPM" HOST)
                    :PACKAGE "USER"
                    :IF-DOES-NOT-EXIST NIL
                    :SET-DEFAULT-PATHNAME NIL) ; already done explicity above
            ((FS:DIRECTORY-NOT-FOUND
                 (FORMAT *TERMINAL-IO*
                         "~&There does not seem to be directory for you on ~A." HOST))))))
      (UNLESS WIN-P
        ;; If user aborts during login, particularly if he types Abort when
        ;; being asked for his password, log him out so he can try again.  But
        ;; if he aborts about of loading the init file, leave him logged in.
        (LOGOUT))))
  T)

;;; LOG1 is an alternate form of LOGIN, which has two additional features.
;;; The proper form is:
;;; (LOG1 user :keyword1 argument1 :keyword2 argument2...)
;;; There are two predefined keywords, HOST and INIT. HOST sets the
;;; associated machine, INIT is a boolean which loads the user's init-file
;;; when true. All other keywords are placed into SI:USER-INIT-OPTIONS,
;;; along with their arguments. This allows users to have their own login
;;; options which their init-file processes.

(DEFVAR USER-INIT-OPTIONS NIL
  "While executing the init file, this holds the options given to LOG1.")

(DEFUN LOG1 (USER-NAME &REST USER-INIT-OPTIONS
             &KEY (HOST ASSOCIATED-MACHINE) (INIT T)
             &ALLOW-OTHER-KEYS)
  "Log in, specifying user name and other options.
This identifies you, for the sake of other users running FINGER.
You cannot access files until you have logged in.
You can log in on any host that files can be read from, including
the local file system if one is loaded.
The options :HOST and :INIT say what host to log in on
and whether to run your init file.  Other options may be found
in the variable USER-INIT-OPTIONS by your init file, which can
use them to decide what to do."
  (LOGIN USER-NAME HOST INIT))

(DEFUN LOGOUT ()
  "Log out.  Undoes certain things done by logging in, or by your init file.
It is not usually useful to log out, since cold-booting the machine
is usually preferable."
  (MAPC 'EVAL LOGOUT-LIST)
  (INITIALIZATIONS 'LOGOUT-INITIALIZATION-LIST)
  (RESET-INITIALIZATIONS 'LOGIN-INITIALIZATION-LIST)
  ;; Do this last so that the initializations won't ask you to login.
  (SETQ USER-ID ""
        FS:USER-HOMEDIRS NIL
        FS:USER-PERSONAL-NAME ""
        FS:USER-PERSONAL-NAME-FIRST-NAME-FIRST ""
        FS:USER-GROUP-AFFILIATION #/-
        FS:USER-LOGIN-MACHINE ASSOCIATED-MACHINE)
  (SETQ LOGOUT-LIST NIL)
  T)

(DEFMACRO LOGIN-FORMS (&BODY FORMS)
  "Execute FORMS, arranging to undo them at logout."
  `(UNDOABLE-FORMS-1 'LOGOUT-LIST ',FORMS "at logout"))

(DEFUN UNDOABLE-FORMS-1 (UNDO-LIST-NAME FORMS &OPTIONAL (COMPLAINT-STRING ""))
  (DOLIST (FORM FORMS)
    (IF (EQ (CAR FORM) 'PROGN)
        (UNDOABLE-FORMS-1 UNDO-LIST-NAME (CDR FORM) COMPLAINT-STRING)
      (LET ((U (UNDOABLE-EVAL FORM)))
        (IF (EQ U T)
            (FORMAT *ERROR-OUTPUT*
                    "~&[A ~S form is supposed to be undone ~A, but this is not implemented.
The form's effects will be permanent.]~%"
                    (CAR FORM)
                    COMPLAINT-STRING)
          (WHEN U (PUSH U (SYMEVAL UNDO-LIST-NAME))))))))

(DEFUN UNDOABLE-EVAL (FORM)
  (IF (ATOM FORM)
      (PROGN (EVAL FORM) NIL)
    (LET ((UNDOER (GET (CAR FORM) :UNDO-FUNCTION))
          TEM)
      (IF (AND (NOT UNDOER)
               (NEQ FORM (SETQ TEM (MACROEXPAND-1 FORM))))
          (UNDOABLE-EVAL TEM)
        (PROG1 (IF UNDOER (FUNCALL UNDOER FORM) T)
               (EVAL FORM))))))

(DEFUN (SETQ :UNDO-FUNCTION) (FORM &AUX RESULTS)
  (DO ((L (CDR FORM) (CDDR L)))
      ((NULL L))
    (IF (BOUNDP (CAR L))
        (PUSH `(SETQ ,(CAR L) ',(SYMEVAL (CAR L))) RESULTS)
        (PUSH `(MAKUNBOUND ',(CAR L)) RESULTS)))
  `(PROGN . ,RESULTS))

(defun (quote :undo-function) (ignore))

(DEFPROP DEFF UNDO-DEFINITION :UNDO-FUNCTION)
(DEFPROP MACRO UNDO-DEFINITION :UNDO-FUNCTION)
(DEFPROP DEFUN UNDO-DEFINITION :UNDO-FUNCTION)
(DEFPROP DEFSUBST UNDO-DEFINITION :UNDO-FUNCTION)

(DEFUN UNDO-DEFINITION (FORM)
  (LET ((FUNCTION-NAME (CADR FORM)))
    (IF (FDEFINEDP FUNCTION-NAME)
        `(FDEFINE ',FUNCTION-NAME ',(FDEFINITION FUNCTION-NAME))
      `(FUNDEFINE ',FUNCTION-NAME))))

(DEFUN (ADVISE :UNDO-FUNCTION) (FORM)
  `(UNADVISE ,(SECOND FORM) ,(THIRD FORM) ,(FOURTH FORM)))

(DEFUN LOGIN-EVAL (FORM)        ;Value returned by such a form is how to undo it
  "Arrange to undo the effects of FORM when (LOGOUT) is done.
The value produced by FORM is assumed to be another form which will undo it.
That value is pushed on LOGOUT-LIST so the effects of FORM will be
undone when you call LOGOUT."
  (PUSH FORM LOGOUT-LIST))

(DEFUN LOGIN-SETQ (&QUOTE &REST L)              ;Undoing SETQ
  "Like SETQ, but the changes are undone by logging out."
  (DO ((L L (CDDR L)))
      ((NULL L))
    (IF (BOUNDP (CAR L))
        (PUSH `(SETQ ,(CAR L) ',(SYMBOL-VALUE (CAR L))) LOGOUT-LIST)
        (PUSH `(MAKUNBOUND ',(CAR L)) LOGOUT-LIST))
    (SET (CAR L) (EVAL1 (CADR L)))))

(DEFUN LOGIN-FDEFINE (FUNCTION-NAME DEFINITION) ;Undoing FDEFINE
  "Like FDEFINE, but the changes are undone by logging out."
  (PUSH (IF (FDEFINEDP FUNCTION-NAME)
            `(FDEFINE ',FUNCTION-NAME ',(FDEFINITION FUNCTION-NAME))
          `(FUNDEFINE ',FUNCTION-NAME))
        LOGOUT-LIST)
  (FDEFINE FUNCTION-NAME DEFINITION))

zwei:
(defun (command-define :undo-function) (form)
  (let* ((command (cadr (cadr form)))           ;(car (cadr form)) is QUOTE
         (name (make-command-name command))
         (aentry (ass 'equalp name *command-alist*))
         )
    `(progn (setf (get ',command 'documentation) ',(get command 'documentation))
            (setf (get ',command 'documentation-function) ',(get command 'documentation-function))
            (setf (get ',command 'command-name) ',(get command 'command-name))
            ,(if aentry
                 `(let ((aentry (ass 'equalp ,name *command-alist*)))
                    (if aentry
                        (setf (cdr aentry) ',(cdr aentry))
                      (push (cons ,name ,(cdr aentry)) *command-list*)))
               `(setq *command-alist* (del-if #'(lambda (foo) (equalp (car foo) ',name))
                                              zwei:*command-alist*))))))

(defun (pkg-goto :undo-function) (form)
  `(progn
     ,(when (caddr form)                        ;globally flag
        (if (boundp-globally '*package*)
            `(setq-globally *package* ',(symbol-value-globally '*package*))
            `(setq-globally *package* pkg-user-package)))
     ,(if (boundp '*package*)
          `(setq *package* ',*package*)
          `(setq *package* pkg-user-package))))

(defun (pkg-goto-globally :undo-function) (ignore)
  (if (boundp-globally '*package*)
      `(setq-globally *package* ',(symbol-value-globally '*package*))
      `(setq-globally *package* pkg-user-package)))

(defun (add-initialization :undo-function) (form)
  (let ((name (eval (cadr form)))
        (keywords (eval (cadddr form)))
        (list-name (eval (car (cddddr form))))
        tem)
    (dolist (l (if (cli:listp keywords) keywords (list keywords)))
      (if (setq tem (ass 'string-equal (symbol-name l) initialization-keywords))
          (setq list-name (cadr tem))))
    (dolist (l (symeval list-name)
               `(delete-initialization ',name nil ',list-name))
      (when (string-equal (init-name l) name)
        (return `(let ((foo (cli:assoc ',name ,list-name :test 'string-equal))
                       (bar (make-init-list-entry  ',name ',(init-form l) ',(init-flag l)
                                                   ',(init-source-file l))))
                   (if foo
                       (setf (cdr foo) (cdr bar))
                     (setq ,list-name (nconc ,list-name (list bar))))))))))

(DEFMACRO SETQ-GLOBALLY (&REST VARIABLES-AND-FORMS)
  "Like SETQ but sets the global bindings of the variables, not the current bindings.
It works by doing the SETQ in another process."
  `(PROGN . ,(LOOP FOR (VAR FORM) ON VARIABLES-AND-FORMS BY 'CDDR
                   COLLECT `(SET-GLOBALLY ',VAR ,FORM))))

(DEFUN PROCESS-RUN-FUNCTION-WAIT (NAME FUNCTION &REST ARGS)
  (PROCESS-WAIT (IF (CONSP NAME) (CAR NAME) NAME)
                #'(LAMBDA (PROCESS) (NOT (SEND PROCESS :RUNNABLE-P)))
                (APPLY 'PROCESS-RUN-FUNCTION NAME FUNCTION ARGS)))

(DEFUN (SETQ-GLOBALLY :UNDO-FUNCTION) (FORM &AUX RESULTS)
  (DO ((L (CDR FORM) (CDDR L)))
      ((NULL L))
    (IF (BOUNDP-GLOBALLY (CAR L))
        (PUSH `(SET-GLOBALLY ',(CAR L) ',(SYMEVAL-GLOBALLY (CAR L))) RESULTS)
      (PUSH `(MAKUNBOUND-GLOBALLY ',(CAR L)) RESULTS)))
  `(PROGN . ,RESULTS))

(DEFUN BOUNDP-GLOBALLY (SYMBOL)
  "T if the global binding of SYMBOL is not unbound.
This is the binding that is in effect outside of rebindings made in this stack group;
the binding seen in any other stack group that does not rebind SYMBOL."
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)
    (LOCATION-BOUNDP LOCATION)))

(DEFUN MAKUNBOUND-GLOBALLY (SYMBOL)
  "Make the global binding of SYMBOL be unbound.
This is the binding that is in effect outside of rebindings made in this stack group;
the binding seen in any other stack group that does not rebind SYMBOL."
  (MULTIPLE-VALUE-BIND (NIL NIL LOCATION)
      (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)
    (LOCATION-MAKUNBOUND LOCATION SYMBOL))
  SYMBOL)

(DEFUN SET-GLOBALLY (SYMBOL VALUE)
  "Set the global binding of SYMBOL to VALUE.
This is the binding that is in effect outside of rebindings made in this stack group;
the value seen in any other stack group that does not rebind SYMBOL."
  (EH:SET-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP VALUE 0))

(DEFUN SYMBOL-VALUE-GLOBALLY (SYMBOL)
  "Return the global binding of SYMBOL.
This is the value that is in effect outside of rebindings made in this stack group;
the value seen in any other stack group that does not rebind SYMBOL."
  (VALUES (SYMEVAL-IN-STACK-GROUP SYMBOL CURRENT-STACK-GROUP 0)))
(DEFF SYMEVAL-GLOBALLY 'SYMBOL-VALUE-GLOBALLY)

;;;; Support for WITH-SYS-HOST-ACCESSIBLE macro.

;;; Make sure we can access files from the sys host.
;;; If not logged in, log in.
;;; If logged in, make sure we know something to log in our file server on on that host.
;;; Also make sure we know the password for the sys login on that host.
;;; Returns a form to evaluate to undo what we did.
(DEFUN MAYBE-SYS-LOGIN (&AUX (HOST (SEND (FS:GET-PATHNAME-HOST "SYS") :HOST))
                        (UNAME (GET-SITE-OPTION :SYS-LOGIN-NAME))
                        PWD)
  "Make sure it is possible to read system files, by logging in if necessary.
The site configuration file specifies the user-name and password to use,
as well as what the system file host is."
  (UNLESS (ASSOC-EQUAL `(,UNAME ,(SEND HOST :NAME)) FS:USER-HOST-PASSWORD-ALIST)
    (SETQ PWD `((,UNAME ,(SEND HOST :NAME)) ,(GET-SITE-OPTION :SYS-LOGIN-PASSWORD)))
    (PUSH PWD FS:USER-HOST-PASSWORD-ALIST))
  (COND ((MEMBER-EQUAL USER-ID '(NIL ""))
         (PUSH (CONS HOST (GET-SITE-OPTION :SYS-LOGIN-NAME)) FS:USER-UNAMES)
         (LOGIN UNAME HOST T)
         `(PROGN (LOGOUT)
                 (SETQ FS:USER-HOST-PASSWORD-ALIST
                       (DELQ ',PWD FS:USER-HOST-PASSWORD-ALIST))))
        ((NULL (ASSQ HOST FS:USER-UNAMES))
         (PUSH (CONS HOST (GET-SITE-OPTION :SYS-LOGIN-NAME)) FS:USER-UNAMES)
         `(PROGN (FLUSH-UNAME ',HOST)
                 (SETQ FS:USER-HOST-PASSWORD-ALIST
                       (DELQ ',PWD FS:USER-HOST-PASSWORD-ALIST))))
        (T `(SETQ FS:USER-HOST-PASSWORD-ALIST
                  (DELQ ',PWD FS:USER-HOST-PASSWORD-ALIST)))))

(DEFUN FLUSH-UNAME (HOST)
  (SETQ FS:USER-UNAMES
        (DELQ (ASSQ HOST FS:USER-UNAMES)
              FS:USER-UNAMES)))
