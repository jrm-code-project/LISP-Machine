;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-


(DEFFLAVOR TEST-DISK-UNIT
           ((UNIT 0))
           ()
  :initable-instance-variables)

(DEFMETHOD (TEST-DISK-UNIT :READ) (RQB ADDRESS)
  (FORMAT T "~&;;;; DISK READ ON UNIT ~D, ~D PAGE~P AT BLOCK ~D~%"
          UNIT
          (RQB-NPAGES RQB)
          (RQB-NPAGES RQB)
          ADDRESS)
  (DISK-READ RQB UNIT ADDRESS))

(DEFMETHOD (TEST-DISK-UNIT :WRITE) (RQB ADDRESS)
  (FORMAT T "~&;;;; DISK WRITE ON UNIT ~D, ~D PAGE~P AT BLOCK ~D~%"
          UNIT
          (RQB-NPAGES RQB)
          (RQB-NPAGES RQB)
          ADDRESS)
  (DISK-WRITE RQB UNIT ADDRESS))

(defmethod (test-disk-unit :dispose) ()
  (format t "~&;;;; DISPOSE OF DISK UNIT~%"))

(defmethod (test-disk-unit :unit-number) ()
  (format t "~&;;;; UNIT NUMBER IS ~D~%" UNIT)
  unit)

(defmethod (test-disk-unit :machine-name) ()
  (format t "~&;;;; MACHINE NAME IS ~D~%" "FOO")
  "FOO")

(tcp-application:define-tcp-server tcp-application:DISK "Read and write disk blocks"
  :toplevel-function 'serial-stream-disk-server
  :listen-port 101
  :STREAM-GENERIC-P t)

(defun open-tcp-disk-unit (machine &optional (why "randomness") (unit 0))
  (check-type machine string)
  (let ((u (make-instance 'serial-stream-disk-unit
                          :stream (open (string-append "TCP-HOST:" machine "#101"))
                          :unit-number unit
                          :machine-name machine)))
    (send u :notify (format nil "Disk being hacked remotely by ~A@~A -- ~A"
                            si:user-id (send si:local-host :name)
                            why))
    u))

(defflavor serial-stream-disk-unit
           ((unit-number 0) (stream nil)(machine-name nil))
           ()
  :initable-instance-variables
  :gettable-instance-variables)

(defun transmit-32b (word stream)
  (do ((j 0 (+ j 8)))
      ((= j 32))
    (send stream :tyo (ldb (byte 8 j) word))))

(defun receive-32b (stream)
  (do ((j 0 (+ j 8))
       (word 0 (dpb (send stream :tyi) (byte 8 j) word)))
      ((= j 32) word)))

(defun transmit-string (string stream)
  (transmit-32b (length string) stream)
  (send stream :string-out string))

(defun receive-string (stream &optional to-string)
  (let* ((length (receive-32b stream))
         (string (or to-string (make-string length))))
    (send stream :string-in nil string)
    string))

(defmethod (serial-stream-disk-unit :notify) (string)
  (send stream :tyo #\N)
  (transmit-string string stream)
  (send stream :force-output)
  (let ((r (send stream :tyi)))
    (cond ((eq r #\R))
          ('else
           (send self :remote-error r)))))

(defmethod (serial-stream-disk-unit :read) (rqb address)
  (send stream :tyo #\R)
  (transmit-32b unit-number stream)
  (transmit-32b (rqb-npages rqb) stream)
  (transmit-32b address stream)
  (send stream :force-output)
  (let ((r (send stream :tyi)))
    (cond ((eq r #\R)
           (receive-string stream (rqb-8-bit-buffer rqb)))
          ('else
           (send self :remote-error r))))
  rqb)

(defmethod (serial-stream-disk-unit :write) (rqb address)
  (send stream :tyo #\W)
  (transmit-32b unit-number stream)
  (transmit-32b (rqb-npages rqb) stream)
  (transmit-32b address stream)
  (transmit-string (rqb-8-bit-buffer rqb) stream)
  (send stream :force-output)
  (let ((r (send stream :tyi)))
    (cond ((eq r #\R))
          ('else
           (send self :remote-error r))))
  rqb)

(defmethod (serial-stream-disk-unit :remote-error) (code)
  (cond ((null code)
         (ferror nil "serial disk unit end of file"))
        ((eq code #\E)
         (ferror nil "serial disk unit error: ~A"
                 (receive-string stream)))
        ('else
         (ferror nil "internal bug, unknown error code serial disk unit: ~D" code))))

(defmethod (serial-stream-disk-unit :dispose) ()
  (close stream))

(defmethod (serial-stream-disk-unit :tyi) ()
  (send stream :tyi))

(defun serial-stream-disk-server (stream &aux rqb)
  ;; this should work over any reliable 8-bit byte stream.
  (unwind-protect
      (do ((opcode))
          ((null (setq opcode (send stream :tyi))))
        (selectq opcode
          ((#\R #\W)
           (let ((unit (receive-32b stream))
                 (n-pages (receive-32b stream))
                 (address (receive-32b stream)))
             (cond ((not rqb)
                    (setq rqb (get-disk-rqb n-pages)))
                   ((not (= n-pages (rqb-npages rqb)))
                    (return-disk-rqb (prog1 rqb (setq rqb nil)))
                    (setq rqb (get-disk-rqb n-pages))))
             (selectq opcode
               (#\R
                (disk-read rqb unit address)
                (send stream :tyo #\R)
                (transmit-string (rqb-8-bit-buffer rqb)
                                 stream))
               (#\W
                (receive-string stream (rqb-8-bit-buffer rqb))
                (disk-write rqb unit address)
                (send stream :tyo #\R)))))
          (#\N
           (tv:notify nil "~A" (receive-string stream))
           (send stream :tyo #\R))
          (#\B
           (let ((quantum 17.)
                 part-name part-base part-size part-comment sub-start sub-n top)
             (let

          (t
           (ferror nil "unknown opcode in serial disk stream")))
        (send stream :force-output))
    (and rqb (return-disk-rqb (prog1 rqb (setq rqb nil))))))


          (SETQ STR (CHAOS:PKT-STRING (CHAOS:READ-PKTS CONN)))  ;Look at the RFC
          (LET ((*READ-BASE* 10.))      ;RFC is BAND-TRANSFER READ/WRITE band subset size comment
                                ;subset is NIL or list of rel start and n-blocks
            (SETQ TEM (READ-FROM-STRING (STRING-APPEND "(" STR ")"))))
          (AND (NULL BAND-TRANSFER-SERVER-ON)
               (NOT (MEMBER USER-ID '(NIL "")))
               (RETURN (CHAOS:REJECT CONN (FORMAT NIL "This machine is in use by ~A" USER-ID))))
          (MULTIPLE-VALUE (PART-BASE PART-SIZE NIL PART-NAME)
            (SYS:FIND-DISK-PARTITION (THIRD TEM)))
          (OR PART-BASE
              (RETURN (CHAOS:REJECT CONN (FORMAT NIL "No /"~A/" partition here." PART-NAME))))
          (AND (FOURTH TEM) (SETQ SUB-START (FIRST (FOURTH TEM)) SUB-N (SECOND (FOURTH TEM))))
          (COND ((STRING-EQUAL (SECOND TEM) "READ")
                 (SETQ WRITE-P NIL)
                 (SETQ PART-COMMENT (PARTITION-COMMENT PART-NAME 0)))
                ((STRING-EQUAL (SECOND TEM) "WRITE")
                 (SETQ WRITE-P T)
                 (OR ( (FIFTH TEM) PART-SIZE)
                     (RETURN (CHAOS:REJECT CONN (FORMAT NIL "Partition too small, ~D>~D"
                                                        (FIFTH TEM) PART-SIZE))))
                 (SETQ PART-SIZE (FIFTH TEM))
                 (SETQ PART-COMMENT (STRING (SIXTH TEM))))      ;Comment to store later
                (T (RETURN (CHAOS:REJECT CONN "Illegal operation, must be READ or WRITE"))))
          (AND SUB-START (OR (MINUSP SUB-START) (MINUSP SUB-N) (> (+ SUB-START SUB-N) PART-SIZE))
               (RETURN (CHAOS:REJECT CONN "Subset outside of partition")))
          (CHAOS:ACCEPT CONN)
          (AND (EQ BAND-TRANSFER-SERVER-ON ':NOTIFY)
               (PROCESS-RUN-FUNCTION "Notify" 'TV:NOTIFY NIL
                                     "BAND-TRANSFER-SERVER: ~:[READ~;WRITE~] of ~A partition by ~A"
                                     WRITE-P PART-NAME
                                     (CHAOS:HOST-DATA (CHAOS:FOREIGN-ADDRESS CONN))))
          (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':ADD-SERVER CONN "BAND-TRANSFER")
          (COND ((NOT WRITE-P)                  ;Send packet containing size, comment
                 (SETQ PART-SIZE (MEASURED-SIZE-OF-PARTITION PART-NAME))
                 (SETQ PKT (CHAOS:GET-PKT))
                 (CHAOS:SET-PKT-STRING PKT (FORMAT NIL "~D ~S" PART-SIZE PART-COMMENT))
                 (CHAOS:SEND-PKT CONN PKT)))
          (AND SUB-START (SETQ PART-BASE (+ PART-BASE SUB-START)
                               PART-SIZE SUB-N))
          (COND (WRITE-P (UPDATE-PARTITION-COMMENT PART-NAME "Incomplete Copy" 0)))
          (SETQ RQB (SYS:GET-DISK-RQB QUANTUM)
                BUF (SYS:RQB-BUFFER RQB))
          (SETQ DISK-ERROR-RETRY-COUNT 20.)     ;Try to bypass hardware overrun problem
          (WIRE-DISK-RQB RQB)
          (SETQ TOP (+ PART-BASE PART-SIZE))
          (DO ((BLOCK PART-BASE (+ BLOCK QUANTUM)))
              (( BLOCK TOP))
            (AND (< (SETQ NB (- TOP BLOCK)) QUANTUM)
                 (WIRE-DISK-RQB RQB (SETQ QUANTUM NB)))
            (COND ((NOT WRITE-P)                ;This can modify pages without setting
                   (DISK-READ-WIRED RQB 0 BLOCK)        ; the modified bits, but as long as
                                                ; we dont depend on data after its unwired,
                                                ; it wont hurt.
                   (ARRAY-TO-NET BUF CONN (* QUANTUM PAGE-SIZE 2)))
                  (T (ARRAY-FROM-NET BUF CONN (* QUANTUM PAGE-SIZE 2))
                     (DISK-WRITE-WIRED RQB 0 BLOCK))))
          (CHAOS:FINISH-CONN CONN)
          (CHAOS:CLOSE-CONN CONN "Done")
          (AND WRITE-P (UPDATE-PARTITION-COMMENT PART-NAME PART-COMMENT 0))))
      (AND RQB (SYS:RETURN-DISK-RQB RQB))
      (AND CONN (PROGN (FUNCALL TV:WHO-LINE-FILE-STATE-SHEET ':DELETE-SERVER CONN)
                       (CHAOS:REMOVE-CONN CONN))))
    NIL))
