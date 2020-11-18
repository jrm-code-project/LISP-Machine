;;; -*- Mode:LISP; Package:KERMIT; Base:8; Readtable:ZL -*-



;;Copyright LISP Machine, Inc. 1984, 1985, 1986
;;   See filename "Copyright" for
;;licensing and release information.
;;



;;; A KERMIT server is a KERMIT program running remotely with no "user
;;; interface". All commands to the server arrive in packets from the
;;; local KERMIT....

;;; Between transactions, a KERMIT server waits for packets containing
;;; server commands. The packet sequence number is always set back to 0
;;; after a transaction. A KERMIT server in command wait should be
;;; looking for packet 0. Certain server commands will result in the
;;; exchange of multiple packets. Those operations proceed exactly like
;;; file transfer.

;;; Server operation must be implemented in two places: in the server
;;; itself, and in any KERMIT program that will be communicating with a
;;; server. The server must have code to read the server commands from
;;; packets and respond to them. the user KERMIT must have code to parse
;;; commands to send requests to servers, to form the server command
;;; packets, and to handle the responses to those server commands....

;;; Server commands are as follows:
;;; S  Send Initiate (exchange parameters, server waits for a file).
;;; R  Receive Initiate (ask the server to send the specified files).
;;; I  Initialize (exchange parameters)....
;;; G  Generic KERMIT Command.  Single character in data field (possibly
;;;    followed by operands, shown in {braces}, optional fields in
;;;    [brackets]) specifies the command:
;;;
;;;    ...
;;;    L  Logout, Bye
;;;    F  Finish (Shut down the server, but don't logout).
;;;    ...

;;; Between transactions, when the server has no tasks pending, it may
;;; send out periodic NAKs (always with type 1 checksums) to prevent a
;;; deadlock in case a command was sent to it but was lost.  These NAKs
;;; can pile up in the local "user" KERMIT's unput buffer (if it has
;;; one), so the user KERMIT should be prepared to clear its input
;;; buffer before sending a command to a server.



(declare (special kstate)                       ;in calls.lisp
         )

(defconst *timint-for-server-wait* 45 "Amount of time to wait before timeout when in server mode")


(defun kermit-remote-server (tty &optional working-directory)
  (send kstate ':remote-server tty working-directory))


(defun receive-file-header (packet num &aux ourfilename)
  num
  (multiple-value-bind (ignore num ignore data) (rpack)
    data
    (cond ((not (= num *packet-number*))
           #\A)
          (t (setq ourfilename (string-for-kermit-outfile packet))
             (cond ((setq *fp* (open-file-out-or-not ourfilename))
                    (format interaction-pane "~&Receiving ~A as ~A"
                            packet
                            ourfilename)
                    (or *remote* (update-status-label ourfilename nil))
                    (spack #\Y *packet-number* 0 nil)
                    (setq *oldtry* *numtry*)
                    (setq *numtry* 0)
                    (bump-packet-number)
                    #\D)
                   (t (format interaction-pane "~&Cannot create ~S" packet)
                                                ;experimental error packet sending--mhd
                      (spack #\E *packet-number* 45     ;
                             "Kermit-Q: Error in file header.")
                      #\A))))))






(DEFUN SERVER-COMMAND-WAIT ()

  (CONDITION-CASE ()                            ;; in case of a sys:abort condition
                                                ;; just return nil; thus they just
                                                ;; abort out of kermit server, not
                                                ;; the login server too.


                                                ;; PS-terminal doesn't die then!!

  (LOOP INITIALLY (AND *DEBUG* (FORMAT T "~&Entering Kermit Server Command Wait...~%"))
        WITH *TIMINT* = *TIMINT-FOR-SERVER-WAIT*
        WITH *REMOTE* = T
        WITH *STATE* = #\W                      ;my own name: WAIT
        FOR *BYTECOUNT* = NIL
        FOR *NUMTRY* = 0 AND *PACKET-NUMBER* = 0 AND *OLDTRY* = 0

        DOING
        (FLUSHINPUT)
        (MULTIPLE-VALUE-BIND (TYPE NUM LEN DATA) (RPACK) LEN
          (SELECT TYPE
            (#\S (COND ((EQ NUM 0)              ;you do the job of Rinit and Rfile
                        (RPAR DATA)             ;here, then jump into Recsw at Rdata
                        (SETQ DATA (SPAR DATA))
                        (SPACK #\Y *PACKET-NUMBER* 6 DATA)
                        (SETQ *OLDTRY* *NUMTRY*)
                        (SETQ *NUMTRY* 0)
                        (BUMP-PACKET-NUMBER)
                        (RECEIVE-FILE-HEADER DATA NUM)
                        (SETQ DATA-XFER-START-TIME (TIME) *BYTECOUNT* 0)
                        (RECSW #\D *PACKET-NUMBER* *NUMTRY*))))
            (#\R (COND ((NOT (= *PACKET-NUMBER* NUM)))
                       (T
                          (COND ((SETQ *FILELIST* (KERMIT-FILELIST DATA)
                                       *FILNAM* (CAR *FILELIST*))
                                 (IF *DEBUG* (FORMAT INTERACTION-PANE
                                                     "Files to send:~A" *FILELIST*))
                                 (BUMP-PACKET-NUMBER)
                                 (SENDSW #\S *PACKET-NUMBER*))
                                (T (SPACK #\E *PACKET-NUMBER*
                                          25 "Error: File Not Found"))))))
            (#\G (COND ((EQ LEN 1)
                        (COND ((EQ (AREF DATA 0) #\L)   ;generic logout
                               (SPACK #\Y *PACKET-NUMBER* 0 NIL)
                               (AND *DEBUG* (FORMAT T "...logout on ~A"
                                                    (time:print-current-date nil)))
                               (RETURN ':LOGOUT))
                              ((EQ (AREF DATA 0) #\F)   ;generic finish
                               (SPACK #\Y *PACKET-NUMBER* 0 NIL)
                               (AND *DEBUG* (FORMAT T "...finishing on ~A"
                                                    (time:print-current-date nil)))
                               (RETURN NIL))))))

            (*FALSE* (SPACK #\A *PACKET-NUMBER* 0 NIL))

            (OTHERWISE
             (SPACK #\E *PACKET-NUMBER* 60
                    "unimplemented server command                               "))))

  )
    (SYS:ABORT NIL)))
