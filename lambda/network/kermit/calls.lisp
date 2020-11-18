;;; -*- Mode:LISP; Package:KERMIT; Base:8; Readtable:ZL -*-


;; Copyright LISP Machine, Inc. 1984, 1985, 1986
;;   See filename "Copyright.text" for
;; licensing and release information.







;;; this code is designed to unify the protocol and
;;; perform the basic protol in which globals are safely
;;; bound to their proper values. This also makes "cold
;;; boots" of the system easier.


;;; all these instance variables are declared special
;;; in elsewhere in the sources (mostly in the kermit-protocol
;;; file).


;;; kstate should be a special instance variable of the kermit
;;; frame for this to really work for it.




(defvar kstate)

(defflavor kstate
           (

            ;; main user settables
            (*soh* 1)
            (*mytime* #o12)
            (*myquote* #\#)
            (*myeol* #o15)
            (*mypad* 0)
            (*mypchar* 0)
            (*filnamcnv* :generic)
            (*8-bit-lispm* t)                   ;to do lispm-ascii translation right
            (*image* nil)
            (*debug* nil)
            (*checksum-type* 1)

            (ascii-extra-safe-filter?
              '(lambda (char)
                 (if (< char #\space) #\space char)))

            (kermit-default-pathname (string (fs:user-homedir)))

            (*rpsiz* 0)
            (*spsiz* 0)
            (*pad* 0)
            (*timint* 0)

            (*remote* nil)
            (*filecount* 0)
            (*size* 0)
            (*packet-number* 0)
            (*numtry* 0)
            (*oldtry* 0)

            (*state* 0)

            (*padchar* 0)
            (*quote* 0)
            (*eol* #o15)
            (*escchr* 0)
            (*eof* 0)

            (bufemp-ignore-line-feed nil)

            (*recpkt* (make-array *maxpacksiz* :type 'art-string :fill-pointer 0))
            (*packet* (make-array *maxpacksiz* :type 'art-string :fill-pointer 0))
            (*string-array-buffer* (make-array (* 2 *maxpacksiz*)
                                               ;; should be enough for padding
                                               ;; soh, eol, type, num, len, and data
                                               :type 'art-string :fill-pointer 0))



            (*filnam* nil)
            (*filelist* ())

            (*ttyfd* nil)
            (*fp* nil)
            (*kermit-beginning-time* nil)
            (*packcount-wraparound* 0))

           ()
  (:settable-instance-variables
    kermit-default-pathname)
  :special-instance-variables)


(defmethod (kstate :string-for-kermit)
           (filename)                           ;*filnamcnv* is specially bound by method
  (string-for-kermit filename))

(defmethod (kstate :filelist)
           (filename)
  (kermit-filelist filename))

(defmethod (kstate :simple-receive)
           (stream)
  (si:%bind (variable-location *ttyfd*) stream)
  (recsw))







;;;..............................



(defconst kermit-max-delay-before-transaction 500.
  "Maximum time Kermit will delay before doing a file send or receive.")





(defvar kermit-delay-before-transaction 0
  "Time to delay before starting a send transaction.")


(SPECIAL *FILNAM* *FILELIST*)

(defmethod (kstate :simple-send) (stream filelist)
  (si:%bind (variable-location *filnam*)
            (if (consp (car filelist))
                (first (car filelist))
              (car filelist)))

  (si:%bind (variable-location *as-filnam*)
            (if (consp (car filelist))
                (second (car filelist))))

  (si:%bind (variable-location *filelist*) (cdr filelist))
  (si:%bind (variable-location *ttyfd*) stream)

  (sendsw))



(defmethod (kstate :server-receive)
           (stream filename as-filename)
  (si:%bind (variable-location *filnam*) filename)
  (si:%bind (variable-location *ttyfd*) stream)
  (si:%bind (variable-location kermit-default-pathname) as-filename)
  (flushinput)
  (spack #/R 0 (length *filnam*) *filnam*)
  (recsw))


(defmethod (kstate :remote-server) (stream
                                    &optional
                                    working-directory?)
  (if working-directory?
      (si:%bind (variable-location kermit-default-pathname) working-directory?))
  (si:%bind (variable-location *ttyfd*) stream)
  (si:%bind (variable-location *remote*) t)
  (server-command-wait))


(defmethod (kstate :bye-server)   (stream)
  (si:%bind (variable-location *ttyfd*) stream)
  (flushinput)
  (spack #\G *packet-number* 1 "L")
  (selectq (rpack)
    (#\Y (format interaction-pane "~% ...BYE~%"))
    (#\N (format interaction-pane "~% ...unable to say BYE~%"))
    (t (format interaction-pane "~% ...error saying BYE~%"))))


(defmethod (kstate :finish-server) (stream)
  (si:%bind (variable-location *ttyfd*) stream)
  (flushinput)
  (spack #\G *packet-number* 1 "F")
  (selectq (rpack)
    (#\Y (format interaction-pane "~% ...Finished~%"))
    (#\N (format interaction-pane "~% ...unable to finish~%"))
    (t (format interaction-pane "~% ...error finishing~%"))))


;;;Let's not cons this up every time inside :SET-PARAMS

(defconst *invalid-image-opt-msg*
          (let((s (make-array 0. :type 'art-fat-string :fill-pointer 0.)))
            (format s "~5&You cannot set both image mode and ASCII translation!")
            (format s "~3&Press any key to continue: ")
            (dotimes(i (fill-pointer s))
              (setf (char-font (char s i)) 0))
            s))

(defmethod (kstate :set-params) ()
  (declare (special kermit-frame serial-stream-open-form))
  (let ((oldx tv:mouse-x) (oldy tv:mouse-y)
        (menux (tv:sheet-inside-right kermit-frame))
        (menuy (tv:sheet-inside-bottom kermit-frame))
        ;;
        ;; APPEND NEW SYMBOLS TO THESE TWO LISTS!
        ;;
        (vars '(kermit-default-pathname
                 serial-stream-open-form
                 *sdu-serial-xon-xoff-p*
                 ;;; *sdu-serial-ascii-p* ;;Don't want this now - Kermit translates Ascii
                 *file-closing-disposition* *filnamcnv* *8-bit-lispm* *image*
                 ascii-extra-safe-filter?
                 *soh* *mytime* *myquote* *mypad* *mypchar* *image* *debug*
                 *checksum-type*
                  ))
        (old-vals (list kermit-default-pathname
                        serial-stream-open-form
                        *sdu-serial-xon-xoff-p*
                        ;;*sdu-serial-ascii-p* ;;See above
                        *file-closing-disposition* *filnamcnv* *8-bit-lispm* *image*
                        ascii-extra-safe-filter?
                        *soh* *mytime* *myquote* *mypad* *mypchar* *image* *debug*
                        *checksum-type*
                        )))
    ;;
    (tv:mouse-warp (- menux 50.) (- menuy 50.)) ;try to put the mouse around the ctr of menu

    (multiple-value-bind (nil abort-p)
        (*catch 'legal-abortion
          (tv:choose-variable-values
            `("Mouse-click on the desired option, type a new value, and press <RETURN>.        "
              "When you are done modifying parameters, click in the margin on /"Execute:/"     "
              "================================================================================"
              "The follow parameters determine the form of login or Kermit I//O connection.    "
              "Note that they take effect only after the current connection, if any, is closed."
              "                                                                                "
              (serial-stream-open-form
                :documentation "The serial stream//device for login or Kermit connections."
                :menu-alist
                (
                 ;;Map over fs:*pathname-host-list* to get serial-port open forms:
                 ;; ("Serial Port A" (sdu-serial-open "SDU-SERIAL-A:"))
                 ;; ("Serial Port B" (sdu-serial-open "SDU-SERIAL-B:"))
                 ,@(gather-sdu-serial-devices T)        ;Let baud rate default dynamically

                 ,@(IF (FIND-PACKAGE "TCP") '(("TCP TELNET" (OPEN-TCP-TELNET-SERIAL-STREAM))))
                 ("CHAOS TELNET" (OPEN-CHAOS-TELNET-STREAM))

                 ;; SUPDUP doesnt work because we are not sending the right negotiations.
                 ;; ("CHAOS SUPDUP" (OPEN-CHAOS-TELNET-STREAM "SUPDUP"))

                 ("REMOTE SERIAL STREAM" (OPEN-REMOTE-SERIAL-STREAM))

                 ;;This was pace's own hack, and doesn't work anywhere anymore:
                 ;("REMOTE UNIX SERIAL" (OPEN-REMOTE-UNIX-SERIAL-STREAM))

                 ("Prompt User" (prompt-and-read :eval-read "~&Form to EVAL and return a stream: "))

                 ;;Unix share ttys
                 ;; One should make sure the pathname exists; otherwise, you'll
                 ;; open an 'i//o stream' to some random file probably.
                 . ,(loop for share-tty in (and (boundp 'unix:*share-ttys*)
                                                unix:*share-ttys*)
                          as port-number from 0
                          collect
                           (list
                             (format nil "Unix Port ~D (//dev//ttyl~D)"
                                     port-number port-number)
                             `(open
                                ,(format nil "UNIX-STREAM-~D:"
                                         port-number))))))

              ;;We may want this in the future, but for now Kermit controls Ascii translation
              ;;(*sdu-serial-ascii-p*
              ;; :documentation "For serial ports, Yes to perform ASCII//LISPM character conversion."
              ;; :boolean)

              ;;We usually <want> software flow control, but only the other host knows!
              (*sdu-serial-xon-xoff-p*
                :documentation "For serial ports, Yes to use software Xon//Xoff flow control."
                :boolean)

              "--------------------------------------------------------------------------------"
              "The remaining parameters affect the ongoing behavior of Kermit transfers.       "
              "Note that they take effect after the current connection, if any, is closed.     "
              "                                                                                "

              (kermit-default-pathname
                :documentation "Default local pathname/directory for Kermit file transfers"
                :pathname kermit-default-pathname)

              (*filnamcnv* :documentation "Specify mode of filename conversion, if any."
                           :menu-alist ,(cons '("Raw - no conversion" :raw)
                                              (cons '("Unknown - generic" :generic)
                                                    (mapcar #'(lambda (x)
                                                                (list (car x) (car x)))
                                                            (get (locf fs:canonical-types)
                                                                 :lisp)))))

              (ascii-extra-safe-filter?
                :documentation
                "Either NIL, or a LISP function that filters unwanted control characters.")

              (*8-bit-lispm* :documentation
               "Yes if you can send 8-bit characters, and want LISPM//ASCII character translation."
               :boolean)

              (*image* :documentation
                         "Yes, if you want 8-bit, binary mode (no character translation)."
                       :boolean)

              (*file-closing-disposition*
                :documentation
                  "Decide whether files only partially written due to interrupt should be saved."
                :menu-alist (("delete-if-abort" :abort)
                             ("dont-delete" nil)))

              (*debug* :documentation
                         "Yes, if you want verbose debugging information during transfer."
                       :boolean)
              (*terminal-debug-mode* :documentation "Yes for debugging the terminal emulator"
                                     :boolean)

              "--------------------------------------------------------------------------------"
              "Some less commonly changed, Kermit packet level parameters - for expert users   "
              "with knowledge of the remote Kermit Protocol and//or operating system           "
              "and their features//problems:"

              (*soh* :documentation
                       "mark for start of packet (a non-printing character)"
                     :number)
              (*mytime* :documentation
                          "max time to wait for packet"
                        :number)
              (*myquote* :documentation "Character to use to quote non-printing chars."
                         :number)
              (*myeol* :documentation "mark for end of packet"
                       :number)
              (*mypad* :documentation
                         "Number of padding characters to use in packet (usually 0)"
                       :number)
              (*mypchar* :documentation
                           "Padding character to use in packet (usually NUL (0))"
                         :number)
              (*checksum-type* :documentation
                               "[Only one character checksums are supported at this time]"
                               :menu-alist (("Normal-one-character" 1)))
              "      ")

            :label "Review//Modify Kermit Parameters"
            :near-mode `(:point ,menux ,menuy)
            :superior kermit-frame
            :margin-choices '("Execute:" ("Abort:" (*throw 'legal-abortion nil)))
            :function #'(lambda(window var old new)
                          (if (and (member var '(*sdu-serial-ascii-p* *image*))
                                   new
                                   *sdu-serial-ascii-p* *image*)
                              (progn
                                (beep)
                                (send window :clear-window)
                                (send window :fat-string-out *invalid-image-opt-msg*)
                                (send window :tyi)
                                (send window :refresh)
                                (set var old))
                            nil))
            ))
      (and abort-p
           (loop for var in vars and old-val in old-vals doing (set var old-val)))
      nil)

    (tv:mouse-warp oldx oldy)))

(defun open-tcp-telnet-serial-stream (&optional auto-force-output)
  (declare (ignore auto-force-output))
  (enable-telnet-iac)
  (let (host stream)
    (do-forever
      (setq host (prompt-and-read :string-or-nil "~&Telnet to host: "))
      (if (catch-error (ip:parse-internet-address host))
        (return nil)
        (warn "~s is not an Internet host" host)))
    (format t "~&Connecting to ~S" host)
    (setq stream (open (format nil "TCP-HOST:~A#TELNET" host)
                       :keyword "Kermit User Telnet"
                       :auto-force-output t
                       :coroutine-input t))
    (send-initial-telnet-frobs stream)
    stream))

(defun send-initial-telnet-frobs (stream)
  (send stream :set-force-output-p nil)
  (send stream :tyo telnet:iac)
  (send stream :tyo telnet:do)
  (send stream :tyo telnet:telopt_echo)
  (send stream :tyo telnet:iac)
  (send stream :tyo telnet:do)
  (send stream :tyo telnet:telopt_sga)
  (send stream :tyo telnet:iac)
  (send stream :tyo telnet:will)
  (send stream :tyo telnet:telopt_sga)
  (send stream :set-force-output-p t)
  (force-output stream))

(defun open-chaos-telnet-stream (&OPTIONAL (CONTACT "TELNET"))
  (enable-telnet-iac)
  (let (host)
    (do-forever
      (setq host (prompt-and-read :string-or-nil "~&Telnet to host: "))
      (if (catch-error (si:parse-host host))
          (return nil)))
    (format t "~&Connecting to ~S" host)
    (make-input-force-output-stream (chaos:open-stream host CONTACT))))


(defun open-REMOTE-SERIAL-STREAM ()
  (format t "~%Connect to remote Lambda serial port B server.")
  (let (host BAUD)
    (do-forever
      (setq host (prompt-and-read :string-or-nil "~&Lambda host: "))
      (if (catch-error (si:parse-host host))
          (return nil)))
    (SETQ BAUD (PROMPT-AND-READ :NUMBER "~&Baud rate: "))
    (format t "~&Connecting to ~S" host)
    (make-input-force-output-stream (CHAOS:OPEN-STREAM HOST (FORMAT NIL "SDU-SERIAL-B ~D" BAUD)))))

;;;How can people be so crude???
;(defun open-remote-unix-serial-stream ()
;  (let (host ;BAUD
;       )
;    (do-forever
;      (setq host (prompt-and-read :string-or-nil "~&Use serial port on host: "))
;      (if (catch-error (si:parse-host host))
;         (return nil)))
;    ;(SETQ BAUD (PROMPT-AND-READ :NUMBER "~&Baud rate: "))
;    (format t "~&Connecting to ~S" host)
;    (make-input-force-output-stream (CHAOS:OPEN-STREAM HOST
;                                                      "EVAL //lmi//pace//xtip//tip -p vadic"))))


(defun make-input-force-output-stream (substream &aux stream need-forcep)
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     ((:tyi :listen)
                       (when need-forcep
                         (send substream :force-output)
                         (setq need-forcep nil))
                       (send substream op))
                     (:tyo
                       (setq need-forcep t)
                       (send substream :tyo arg1))
                     (:close
                       (send substream :close))
                     (t
                       (stream-default-handler stream op arg1 args))))))




(defconst kstate ()                             ;should be bound during program
  "The flavor instance of kstate which calls Kermit programs and bind globals.")


(defun telnet-h19 (&optional to-host)
  (let ((status-pane *terminal-io*)
        (command-pane *terminal-io*)
        (interaction-pane *terminal-io*)
        (terminal-pane *terminal-io*)
        (kterm-state (make-instance 'kterm-state))
        (kermit-frame nil))
    (declare (special status-pane command-pane interaction-pane terminal-pane kterm-state kermit-frame))
    (enable-telnet-iac)
    (let ((host to-host))
      (loop
        (setq host (or host (prompt-and-read :string-or-nil "~&Telnet to host: ")))
        (when (catch-error (ip:parse-internet-address host))
          (return nil))
        (setq host nil))
      (format t "~&Connecting to ~S" host)
      (with-open-file (stream (format nil "TCP-HOST:~A#TELNET" host)
                              :keyword "Kermit User Telnet"
                              :auto-force-output t)
        (send-initial-telnet-frobs stream)
        (send kterm-state
              :make-connection
              stream
              terminal-pane)))))

(compile-flavor-methods kstate)
