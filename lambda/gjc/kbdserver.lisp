;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:ZL -*-

;;; use this server if the keyboard on a machine is broken.
;;; to load it, use TELNET, or the function LOAD-THIS-FILE (uses the EVAL server)
;;; which are defaulty loaded/enabled.

(DEFUN LOAD-THIS-FILE (ON-HOST &OPTIONAL (OF-FUNCTION 'LOAD-THIS-FILE))
  (LET ((PATHNAME (SEND (SEND (SI:GET-SOURCE-FILE-NAME OF-FUNCTION)
                              :new-pathname :type nil :version nil)
                        :translated-pathname)))
    (format t "~&; Connecting to EVAL server on ~S~%; to load ~S"
            (si:parse-host on-host)
            pathname)
    (WITH-OPEN-STREAM (X (CHAOS:OPEN-STREAM ON-HOST "EVAL"))
      ;; note: does ascii->lispm translation, so we make sure no funny
      ;; characters are used here.
      (format x " (LET ((SI:USER-ID ~S)) (LOAD ~S :SET-DEFAULT-PATHNAME NIL))  CHAOS:QUIT  "
              ;; bind si:user-id so that the LOAD doesnt force user login.
              "LUSER"
              (send pathname :string-for-printing))
      (send x :force-output)
      (do ((c))
          ((null (setq c (send x :tyi))))
        (setq c (fs:ascii-to-lispm c))
        (and c (send standard-output :tyo c))))))

(DEFUN VANILLA-CHAOS-SERVER (NAME FUNCTION)
  (ADD-INITIALIZATION NAME
                      `(PROCESS-RUN-FUNCTION ,(FORMAT NIL "~A SERVER" NAME)
                                             'VANILLA-CHAOS-SERVER-FUNCTION
                                             ',FUNCTION
                                             ',NAME)
                      NIL
                      'CHAOS:SERVER-ALIST))


(DEFUN VANILLA-CHAOS-SERVER-FUNCTION (F NAME)
  (CONDITION-CASE ()
      (with-open-stream (x (chaos:open-stream NIL NAME))
        (chaos:accept (send x :connection))
        (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER (send x :connection) NAME)
        (FUNCALL F X))
    (SI:REMOTE-NETWORK-ERROR)))

(VANILLA-CHAOS-SERVER "KBD-INPUT" 'KBD-INPUT-SERVER)

(defun kbd-input-server (X)
  (DO ((C)
       (R (SI:FIND-READTABLE-NAMED "CL"))
       (P (FIND-PACKAGE "USER")))
      ((NULL (SETQ C (LET ((*READTABLE* R)
                           (*PACKAGE* P)
                           (BASE 10)
                           (IBASE 10))
                       (READ X NIL)))))
    (SEND TV:SELECTED-WINDOW :FORCE-KBD-INPUT C)))


(DEFUN KBD-CONNECT (HOST)
  (WITH-OPEN-STREAM (X (CHAOS:OPEN-STREAM HOST "KBD-INPUT"))
    (DO ((R (SI:FIND-READTABLE-NAMED "CL"))
         (P (FIND-PACKAGE "USER"))
         (C))
        ((NULL (SETQ C (SEND STANDARD-INPUT :ANY-TYI))))
      (IF (EQUAL C #\HAND-DOWN) (SETQ C #\ABORT))
      (LET ((*PACKAGE* P)
            (*READTABLE* R)
            (IBASE 10)
            (BASE 10))
        (PRINT C X))
      (SEND X :FORCE-OUTPUT))))

(VANILLA-CHAOS-SERVER "CECHO" 'CHAOS-CHARACTER-ECHO)

(DEFUN CHAOS-CHARACTER-ECHO (STREAM)
  (DO ((C))
      ((NULL (SETQ C (SEND STREAM :TYI))))
    (SEND STREAM :TYO C)
    (SEND STREAM :FORCE-OUTPUT)))


(DEFUN CECHO-TEST (HOST)
  (WITH-OPEN-STREAM (STREAM (CHAOS:OPEN-STREAM HOST "CECHO"))
    (format t "~&Connected to ~A~%" (send (send stream :connection) :foreign-host))
    (DO ((C)
         (TB)
         (TA))
        ((OR (NOT (SETQ C (SEND STANDARD-INPUT :TYI)))
             (EQ C #\END)))
      (SETQ TB (TIME))
      (SEND STREAM :TYO C)
      (SEND STREAM :FORCE-OUTPUT)
      (SETQ C (SEND STREAM :TYI))
      (SETQ TA (TIME))
      (FORMAT T "~D milliseconds~%"
              (* 1000 (quotient (time-difference ta tb) 60.0))))))
