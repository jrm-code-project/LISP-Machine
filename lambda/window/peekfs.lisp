;;; -*- Mode:LISP; Package:FS; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;; Chaosnet FILE JOB peek functions

(DEFUN DC-ACCESS-PEEK-FILE-SYSTEM ()
  (TV:SCROLL-MAINTAIN-LIST
    `(LAMBDA () ',(SEND SELF :HOST-UNITS))
    #'PEEK-FILE-SYSTEM-HOST-UNIT))

(DEFUN PEEK-FILE-SYSTEM-HOST-UNIT (UNIT &OPTIONAL (INDENT 2))
  "Generate a scroll item describing a host unit"
  (LIST ()
    (TV:SCROLL-PARSE-ITEM ':MOUSE
                          `(NIL :MENU-CHOOSE
                                ("Host-unit operations"
                                 ("Reset" :EVAL (FUNCALL THISUNIT ':RESET)
                                  :DOCUMENTATION
                                  "Click left to close this connection")
                                 ("Inspect" :EVAL
                                  (LET ((TERMINAL-IO TYPWIN))
                                    (INSPECT THISUNIT))
                                  :DOCUMENTATION
                                  "Click left to INSPECT this host-unit.")
                                 ("Describe" :EVAL
                                  (LET ((TERMINAL-IO TYPWIN))
                                    (DESCRIBE THISUNIT))
                                  :DOCUMENTATION
                                  "Click left to DESCRIBE this host-unit."))
                                :DOCUMENTATION
                                "Menu of things to do to this host-unit."
                                :BINDINGS
                                ((THISUNIT ',UNIT)
                                 (TYPWIN ',(FUNCALL SELF ':TYPEOUT-WINDOW))))
                          (FORMAT NIL "~V@THost unit for ~A, control connection in "
                                  INDENT (SEND UNIT :HOST))
                          `(:FUNCTION ,#'(LAMBDA (UNIT)
                                           (LET ((CONN (SEND UNIT :CONTROL-CONNECTION)))
                                             (IF CONN (GET-PNAME (CHAOS:STATE CONN))
                                                 "NONEXISTENT-STATE")))
                                      (,UNIT)))
    (TV:SCROLL-MAINTAIN-LIST `(LAMBDA () (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-STREAM
                                           (NCONS (SEND ',UNIT :DATA-CONNECTIONS)) T))
                             `(LAMBDA (STREAM)
                                (FUNCALL STREAM ':PEEK-FILE-SYSTEM (+ 2 ,INDENT)))
                             NIL
                             #'(LAMBDA (STATE &AUX NS STREAM)
                                 (MULTIPLE-VALUE (NS STREAM)
                                   (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-STREAM STATE))
                                 (VALUES STREAM NS
                                         (NULL (PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-STREAM
                                                 NS T)))))))

(DEFUN PEEK-FILE-SYSTEM-HOST-UNIT-NEXT-STREAM (STATE &OPTIONAL DONT-STEP &AUX STREAM FLAG NS)
  "Returns new state and next stream.  If DONT-STEP is specified, returns the current
state if there is a stream available, else NIL"
  (SETQ FLAG (CDR STATE))
  (DO ((S (CAR STATE) (CDR S)))
      ((NULL S) (SETQ NS NIL))
    (SETQ NS S)
    (AND (NULL FLAG) (SETQ STREAM (DATA-STREAM (CAR S) ':INPUT))
         (NEQ STREAM T) (RETURN (SETQ FLAG T)))
    (SETQ FLAG NIL)
    (AND (SETQ STREAM (DATA-STREAM (CAR S) ':OUTPUT))
         (NEQ STREAM T) (RETURN (SETQ NS (CDR NS)))))
  (AND (NOT (SYMBOLP STREAM))
       (VALUES (IF DONT-STEP
                   STATE
                   (RPLACA STATE NS)
                   (RPLACD STATE FLAG))
               STREAM)))

(defmethod (ftp-access :peek-file-system) ()
  (tv:scroll-maintain-list
    `(lambda () ',(send self :host-units))
    #'peek-ftp-host-unit))

(defun peek-ftp-host-unit (unit &optional (indent 2))
  "Generate a scroll item describing a host unit"
  (list ()
    (tv:scroll-parse-item ':mouse
                          `(nil :menu-choose
                                ("Host-unit operations"
                                 ("Reset" :eval (funcall thisunit ':reset)
                                  :documentation
                                  "Click left to close this connection")
                                 ("History" :eval (send tv:selected-window :force-kbd-input `(:eval (ftp-history ,thisunit)))
                                  :documentation
                                  "Show command history for this unit")
                                 ("Inspect" :eval (send tv:selected-window :force-kbd-input `(inspect ,thisunit))
                                  :documentation
                                  "Click left to INSPECT this host-unit.")
                                 ("Describe" :eval (send tv:selected-window :force-kbd-input `(describe ,thisunit))
                                  :documentation
                                  "Click left to DESCRIBE this host-unit."))
                                :documentation
                                "Menu of things to do to this host-unit."
                                :bindings
                                ((thisunit ',unit)
                                 (typwin ',(funcall self ':typeout-window))))
                          (format nil "~V@THost unit for ~A"
                                  indent (send unit :host))
                          `(:FUNCTION ,#'(LAMBDA (UNIT)
                                           (let* ((control (symeval-in-instance unit 'ftp:*control*))
                                                  (control-socket (and control (send control :socket)))
                                                  (data (symeval-in-instance unit 'ftp:*data*))
                                                  (data-socket (and data (send data :socket))))
                                             (format nil
                                                   " with ~A control connection and ~A data connection"
                                                   (if control-socket
                                                       (tcp:tcp-user-state control-socket)
                                                     :nonexistent)
                                                   (if data-socket
                                                       (tcp:tcp-user-state data-socket)
                                                     :nonexistent)
                                                   )))
                                      (,UNIT)))
    (tv:scroll-maintain-list `(lambda () (send ',unit :open-streams))
                             `(lambda (stream)
                                (funcall stream ':peek-file-system (+ 2 ,indent))))
    ))
