;;; -*- Mode:LISP; Package:CHAOS; Base:10 -*-


(DEFUN VANILLA-CHAOS-SERVER-FUNCTION (CONTACT FUNCTION REJECTP)
  (WITH-OPEN-STREAM (STREAM (CHAOS:OPEN-STREAM NIL CONTACT))
    (LET ((CONN (SEND STREAM :CONNECTION)))
      (LET ((ST (SUBSTRING (CHAOS:PKT-STRING (CHAOS:CONN-READ-PKTS CONN))
                           (STRING-LENGTH CONTACT))))
        (LET ((REJECT? (AND REJECTP (FUNCALL REJECTP ST))))
          (IF REJECT?
              (RETURN-FROM VANILLA-CHAOS-SERVER-FUNCTION
                (CHAOS:REJECT CONN REJECT?))))
        (CHAOS:ACCEPT CONN)
        (SEND TV:WHO-LINE-FILE-STATE-SHEET :ADD-SERVER CONN CONTACT)
        (CONDITION-CASE ()
            (FUNCALL FUNCTION STREAM ST)
          (SYS:REMOTE-NETWORK-ERROR NIL))))))


(DEFUN ADD-VANILLA-CHAOS-SERVER (CONTACT FUNCTION &KEY REJECTP)
  "Contant is CONTACT name, function is called on stream and contact argument as long
as REJECTP predicate returns NIL."
  (check-type contact string)
  (ADD-INITIALIZATION CONTACT
                      `(PROCESS-RUN-FUNCTION ,(FORMAT NIL
                                                      "~A server"
                                                      contact)
                                             'vanilla-chaos-server-function
                                             ,contact
                                             ',function
                                             ',REJECTP)
                      nil
                      'CHAOS:SERVER-ALIST))

(DEFUN SDU-SERIAL-STREAM-IN-USE-P (ARG)
  (COND ((NOT (FIXP (catch-error (PARSE-INTEGER (STRING-TRIM " " ARG)) nil)))
         (FORMAT NIL "bad baud rate argument: ~S" arg))
        ('else
         (LET* ((DEVICE (SEND (FS:PARSE-PATHNAME "SDU-SERIAL-B:") :HOST))
                (LOCK (CDR (SEND DEVICE :LOCK))))
           (COND ((NOT (SEND DEVICE :ALLOCATE-IF-EASY))
                  "device not available")
                 (LOCK
                  (FORMAT NIL "device in use by ~A" LOCK)))))))

(DEFUN SDU-SERIAL-STREAM-SERVER (NETWORK-STREAM ARG)
  (WITH-OPEN-FILE (SERIAL-STREAM "SDU-SERIAL-B:")
    (SEND SERIAL-STREAM :SET-BAUD-RATE (PARSE-INTEGER (STRING-TRIM " " ARG)))
    (CATCH 'DEATH
      (LET ((PROC))
        (UNWIND-PROTECT
            (PROGN (SETQ PROC (PROCESS-RUN-FUNCTION
                                "SDU SERIAL STREAM => NETWORK"
                                #'(LAMBDA (FROM-STREAM TO-STREAM SUPERIOR)
                                    (CONDITION-CASE ()
                                        (do ((buf) (offset) (limit))
                                            (())
                                          (multiple-value (buf offset limit)
                                            (send from-stream :read-input-buffer))
                                          (cond ((null buf) (return nil)))
                                          (send to-stream :string-out buf offset limit)
                                          (SEND TO-STREAM :FORCE-OUTPUT)
                                          (send from-stream :advance-input-buffer))
                                      (SYS:REMOTE-NETWORK-ERROR))
                                    (SEND SUPERIOR :INTERRUPT #'(LAMBDA () (THROW 'DEATH NIL)))
                                    (SI:PROCESS-WAIT-FOREVER))
                                SERIAL-STREAM NETWORK-STREAM CURRENT-PROCESS))
                   (STREAM-COPY-UNTIL-EOF NETWORK-STREAM SERIAL-STREAM))
          (AND PROC (SEND PROC :KILL)))))))


(ADD-VANILLA-CHAOS-SERVER "SDU-SERIAL-B" 'SDU-SERIAL-STREAM-SERVER
                          :REJECTP 'SDU-SERIAL-STREAM-IN-USE-P)


(DEFUN OPEN-REMOTE-SDU-SERIAL-STREAM (HOST &OPTIONAL &KEY (BAUD-RATE 1200.))
  (OPEN-STREAM HOST (FORMAT NIL "SDU-SERIAL-B ~D" BAUD-RATE)))

