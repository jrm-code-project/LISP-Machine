;;; -*- Mode:LISP; Package:FILE-SYSTEM; Patch-File:T; Base:10 -*-

(setq CATCH-LOGIN-PROBLEMS-P nil)

(defvar *lmfs-parse-for-server-device-kludge* nil
  "If T turn non-dsk devices into host specifications")

(defun lmfs-parse-for-server (string)
  (let ((obj (condition-case (result)
                 (fs:merge-pathname-defaults
                  string
                  local-host-pathname
                  ':unspecific ':newest)
               (:pathname-parse-error
                result))))
    (cond ((errorp obj)
           obj)
          ((not *lmfs-parse-for-server-device-kludge*)
           obj)
          ((string-equal "DSK" (send obj :device))
           obj)
          ((si:parse-host (send obj :device) t nil)
           (setq obj
                 (send obj :new-pathname
                       :host (send obj :device)
                       :device (PATHNAME-DEVICE (QUIET-USER-HOMEDIR (send obj :device)))))
           obj)

          ('else
           obj))))

(defvar *file-server-user-logged-in* nil)

(defun file-server-login (rest)
  (let ((uname (car rest)))
    (cond ((null uname)
           (format conn-stream "~A  ERROR LIP F Invalid Login syntax" tid))
          ((and *lmfs-parse-for-server-device-kludge* (file-server-login-device-kludge rest)))
          ('else
           (setq *file-server-user-logged-in* t)
           (format conn-stream "~A  ~A ~A ~%~A~%" tid "LOGIN" uname
                   (send local-host-pathname
                         ':new-pathname ':device ':unspecific
                         ':directory (string uname)
                         ':name nil ':type nil ':version nil))
           (string uname)))))

(defvar *file-server-user-password-table* nil)

(defun encrypt-password (string)
  "encrypt the string using the same algorithm as Unix(TM) uses"
  (check-type string string)
  string)

(defun file-server-login-device-kludge (l)
  (let ((uname-host (string (car l)))
        (password (string (cadr l)))
        (uname)(temp)(host))
    (cond ((and (setq temp (string-search "@" uname-host))
                (setq host (si:parse-host (substring uname-host (1+ temp)) t nil))
                (progn (setq uname (substring uname-host 0 temp))
                       (not (eq host si:local-host))))
           (push (list (list uname (send host :name)) password)
                 user-host-password-alist)
           (let ((homedir (let ((CATCH-LOGIN-PROBLEMS-P NIL))
                            (CONDITION-CASE (E)
                                (SEND (SAMPLE-PATHNAME HOST) :HOMEDIR uname)
                              (login-problems
                               e)))))
             (cond ((errorp homedir)
                    (format conn-stream "~A LOGIN ERROR LIP F ~A"
                            tid (send homedir :report-string))
                    nil)
                   ('else
                    (setq *file-server-user-logged-in* t)
                    (format conn-stream "~A  ~A ~A ~%~A~%" tid "LOGIN" uname
                            (send homedir :new-pathname
                                  :host si:local-host
                                  :device (send host :name)))
                    uname))))
          ((not *file-server-user-password-table*)
           nil)
          ((not (setq temp (gethash *file-server-user-password-table* uname)))
           (format conn-stream "~A  ERROR UNK C user not known: ~A" tid uname)
           nil)
          ((not (string-equal (encrypt-password password) temp))
           (format conn-stream "~A  ERROR IP? C wrong password given for ~A" tid uname)
           nil)
          ('else
           uname))))

(defun rfile-server ()
  (if (and server-login
           (or (null user-id)
               (zerop (string-length user-id))))
      (trap-lossage (error "Server Login")
          (progn
            (login server-login-id si:local-host)
            (print-server-login-exegesis))))
  (let (tid conn-stream conn alldatas server-openings
        (server-instance (gensym))              ;bind em all local....
        (user-id server-login-id)
        (server-protocol-version server-protocol-version)
        (fs:*local-server-via-net* nil)
        (CATCH-LOGIN-PROBLEMS-P nil)
        (*file-server-user-logged-in* nil))
    (unwind-protect
      (trap-lossage (error "Server Top Level")
          (*catch 'server-chaos-disappear
            (setq conn (chaos:listen "FILE"))
            (when (chaos:symbolics-connection-p conn t)
              (chaos:symbolics-reject (prog1 conn (setq conn nil)))
              (ferror nil "Symbolics tried to come in."))
            (when *lmfs-server-dont-answer-logins*
              (chaos:reject (prog1 conn (setq conn nil))
                            *lmfs-server-dont-answer-logins*)
              (ferror nil *lmfs-server-dont-answer-logins*))
            (let* ((pkt (chaos:read-pkts conn)) ;s/b rfc
                   (result (server-parse-rfc pkt)))
              (cond ((fixp result)
                     (setq server-protocol-version result))
                    (t (chaos:reject (prog1 conn (setq conn nil)) result)
                       (ferror nil result))))
            (chaos:accept conn)
            (send tv:who-line-file-state-sheet
                  ':add-server conn "FILE" si:current-process
                  'lmfs-peek-server (process-stack-group si:current-process))
            (setq conn-stream (chaos:make-stream conn))
            (if *server-shutdown-message* (send-single-shutdown-message conn))
            (error-restart-loop ((sys:abort error) "Return to server command-reading loop.")
              (let (pkt op)
                (setq pkt (trap-lossage (error "Server Reading packets")
                              (condition-bind
                                ((sys:host-stopped-responding
                                   #'(lambda (&rest ignore)
                                       (*throw 'server-chaos-disappear nil)))
                                 (sys:connection-lost
                                   #'(lambda (&rest ignore)
                                       (*throw 'server-chaos-disappear nil))))
                                (chaos:get-next-pkt conn))
                            (ferror 'server-control-conn-network-lossage
                                    "Control connection lost")))
                (setq op (chaos:pkt-opcode pkt))
                (cond ((or (= op chaos:eof-op)
                           (= op chaos:cls-op))
                       (send conn-stream ':force-output)
                       (chaos:return-pkt pkt)
                       (*throw 'server-chaos-disappear nil))
                      ((not (= op chaos:dat-op))
                       (ferror nil "Unrecognized packet opcode: ~S" op)))
                (let* ((string (chaos:pkt-string pkt))
                       (strings (get-strings-from-pktstring string)))   ;nl-delimited strings

                  (if trace-server-enabled
                      (without-interrupts (push (string-append string) server-traces)))
                  (destructuring-bind (tid fh cmd . rest) (parse-cmd-string string)
                    (cond (*lmfs-server-dont-answer-logins*
                           (format conn-stream "~A ~A ERROR HNA F Host not available - ~A "
                                   tid (or fh "")
                                   *lmfs-server-dont-answer-logins*))
                          ((and (not *file-server-user-logged-in*)
                                (not (eq cmd :login)))
                           (format conn-stream "~A ~A ERROR NLI C login first before ~A"
                                   tid (or fh "") cmd))
                          ('else
                           (selectq cmd
                             (:login (setq user-id (file-server-login rest)))
                             (:open   (file-server-open fh rest (car strings)))
                             (:open-for-lispm
                              (apply 'file-server-open-for-lispm
                                     fh (car strings)
                                     (let ((*read-base* 10.) (*print-base* 10.)
                                           (*package* si:pkg-user-package)
                                           (*readtable* si:initial-readtable))
                                       (read-from-string
                                         string nil
                                         (+ (string-search-char #/cr string)
                                            (length (car strings))
                                            2)))))
                             (:extended-command
                              (apply 'file-server-extended-command
                                     fh (car rest) (car strings)
                                     (let ((*read-base* 10.) (*print-base* 10.)
                                           (*package* si:pkg-user-package)
                                           (*readtable* si:initial-readtable))
                                       (read-from-string
                                         string nil
                                         (+ (string-search-char #/cr string)
                                            (length (car strings))
                                            2)))))
                             (:data-connection (file-server-data-connection fh rest))
                             (:undata-connection (file-server-undata-connection fh))
                             (:close (file-server-close-connection fh))
                             (:filepos (file-server-filepos fh rest))
                             (:delete (file-server-delete fh strings))
                             (:rename (file-server-rename fh strings))
                             (:expunge (file-server-expunge fh strings))
                             (:complete (file-server-complete fh rest strings))
                             (:continue (file-server-continue fh))
                             (:directory (file-server-directory fh rest strings))
                             (:change-properties (file-server-change-props fh strings))
                             (:create-directory (file-server-create-directory fh strings))
                             (:create-link (file-server-create-link fh strings))
                             (otherwise (format conn-stream
                                                "~A ~A ERROR UKC F Unknown command: ~A"
                                                tid (or fh "") cmd))))))
                  (send conn-stream ':force-output)
                  (chaos:return-pkt pkt))))))
      (when conn
        (send tv:who-line-file-state-sheet ':delete-server conn)
        (trap-lossage (error "Server Top Level close")
                      (chaos:close-conn conn
                                        (or *lmfs-server-dont-answer-logins*
                                            "Server error")))
        (chaos:remove-conn conn))
      (if server-openings
          (trap-lossage (error "Server finish closing remaining openings")
              (dolist (opening server-openings)
                (send opening ':close ':abort))))
      (trap-lossage (error "Closeout undata")
                    (dolist (data alldatas)
                      (rplaca (server-dataproc-comm-cell
                                (get (server-dataproc-comm-sibling data) server-instance))
                              'undata)
                      (rplaca (server-dataproc-comm-cell data) 'undata)
                      (chaos:remove-conn (server-dataproc-comm-conn data)))))))
