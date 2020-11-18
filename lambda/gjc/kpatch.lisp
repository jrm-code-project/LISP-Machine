;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-

;;; Patch file that combines patch file for System version 102.174
;;; and the patch file for KERMIT version 26.22

;;; Usage: Load this file (compiled version that is, KPATCH.QFASL) then use the
;;; KERMIT mouse selected command "Set Baud Rate" but instead of choosing a
;;; numerical quantity choose the item on the top of the menu list,
;;; called "Extended". When you do this you get another menu, to choose
;;; serial characteristics. You may use the mouse to select character data
;;; length of 5,6,7 or 8 bits (8 is the default), and stop bits of 1, 1.5, or 2 bits,
;;; (2 is the default), parity of EVEN, ODD or NIL (default is NIL or no parity),
;;; and a baud rate which is an arbitrary number you can type in.
;;; Once you have made whatever changes you choose, mouse the [EXIT] square.
;;; 11/24/85 10:48:35 -George Carrette.

;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for System version 102.174
;;; Reason:
;;;  Now supporting :set-parity :even, :odd, or NIL to the sdu-serial-stream.
;;; Written 23-Nov-85 14:40:00 by GJC,
;;; while running on Lambda Six from band 1
;;; with System 102.173, Local-File 56.12, FILE-Server 13.2, Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12, KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, TCP-Kernel 30.5, TCP-User 57.8, TCP-Server 33.1, microcode 778, 2.0 Process Systems 5/2/85 Fork..



; From file SDU-SERIAL.LISP#> QL.IO1; LAD: (20)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SDU-SERIAL  "

(let ((x (locf (symeval-in-instance (send (fs:parse-pathname "SDU-SERIAL-B:") :host)
                                    'last-instance))))
  (when (car x)
    (close (car x))
    (setf (car x) nil)))

))

; From file SDU-SERIAL.LISP#> QL.IO1; LAD: (20)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SDU-SERIAL  "

(defflavor sdu-serial-stream-mixin
        (input-unibus-channel
         output-unibus-channel
         (input-buffer-size page-size)
         (output-buffer-size page-size)
         (force-output-p t)
         (serial-unrchf nil)
         (baud-rate 9600.)
         (pci-csr #x1c15c)
         (pci-data #x1c158)
         (pit-mode #x1c17c)
         (pit-counter #x1c174)
         (pit-channel #x40)
         (xmit-interrupt-number 113)
         (rcv-interrupt-number 112)
         (char-length :8bits)
         (stop-bits :2bits)
         shared-device
         (parity nil)
         )
        (si:line-output-stream-mixin)
  (:required-flavors si:bidirectional-stream si:character-stream
                     si:unbuffered-line-input-stream
                     si:basic-buffered-output-stream)
  (:initable-instance-variables)
  (:settable-instance-variables))

))

; From file SDU-SERIAL.LISP#> QL.IO1; LAD: (20)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SDU-SERIAL  "

(defmethod (sdu-serial-stream-mixin :reset) (&optional (abort-output :abort) (reset-chip t))
  abort-output
  (cond ((or (> input-buffer-size %array-max-short-index-length)
             (> output-buffer-size %array-max-short-index-length))
         (ferror nil "buffer sizes must be <= ~s" %array-max-short-index-length)))
  (let ((new-mode-word (assemble-word-from-description
                         *8251-mode-word-description*
                         'baud-rate-factor :16x
                         'char-length char-length
                         'parity-enable (not (null parity))
                         'generate-parity parity
                         'stop-bits stop-bits))
        (new-command-word   (assemble-word-from-description
                              *8251-command-word-description*
                              'transmit-enable t
                              'dtr t
                              'receive-enable t
                              'reset-errors t
                              'rts t)))
    (send self :remove-unibus-channels)         ;Get rid of unibus channels if any
    (cond (reset-chip
           (%multibus-write-8 pci-csr 0)
           (%multibus-write-8 pci-csr 0)
           (%multibus-write-8 pci-csr 0)
           (%multibus-write-8 pci-csr #x40)     ; reset chip
           (%multibus-write-8 pci-csr new-mode-word)
           (%multibus-write-8 pci-csr new-command-word)))
                                                ; now do the baud rate
    (%multibus-write-8 pit-mode (logior pit-channel #x36))
    (let ((counter-value (// 76800. baud-rate)))
      (%multibus-write-8 pit-counter counter-value)
      (%multibus-write-8 pit-counter (ash counter-value -8)))

    (setq input-unibus-channel
          (get-unibus-channel rcv-interrupt-number
                              0 0 0 1 (ceiling input-buffer-size page-size))
          output-unibus-channel
          (get-unibus-channel xmit-interrupt-number
                              0 0 0 1 (ceiling output-buffer-size page-size)))
    ))

))

; From file SDU-SERIAL.LISP#> QL.IO1; LAD: (20)
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: IO1; SDU-SERIAL  "

(defmethod (sdu-serial-stream-mixin :set-parity) (type)
  (symeval-in-field-context type 'generate-parity *8251-mode-word-description*)
  (setq parity type)
  (send self :reset))



(defmethod (sdu-serial-stream-mixin :set-char-length) (length)
  (symeval-in-field-context length 'char-length *8251-mode-word-description*)
  (setq char-length length)
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :set-stop-bits) (length)
  (symeval-in-field-context length 'stop-bits *8251-mode-word-description*)
  (setq stop-bits length)
  (send self :reset))



))


;;; -*- Mode:Lisp; Readtable:T; Package:USER; Base:8; Patch-File:T -*-
;;; Patch file for KERMIT version 26.22
;;; Reason:
;;;  SET-BAUD-RATE choice of EXTENDED gives menu to choose Char Length, Parity, etc.
;;; Written 23-Nov-85 16:13:05 by GJC,
;;; while running on Lambda Six from band 1
;;; with System 102.174, Local-File 56.12, FILE-Server 13.2, Unix-Interface 5.6, MagTape 40.23, ZMail 57.10, Tiger 20.12, KERMIT 26.21, MEDIUM-RESOLUTION-COLOR 17.4, TCP-Kernel 30.5, TCP-User 57.8, TCP-Server 33.1, microcode 778, 2.0 Process Systems 5/2/85 Fork..



; From file WINDOW.LISP#> QL.KERMIT; LAD: (67)
#8R KERMIT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "KERMIT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: KERMIT; WINDOW  "

(defconst all-baud-choices-items-alist
          '((" Extended " 0)
            (" 50.      " 50.)
            (" 75.      " 75.)
            (" 110.     " 110.)
            (" 134.     " 134.)
            (" 150.     " 150.)
            (" 300.     " 300.)
            (" 600.     " 600.)
            (" 1200.    " 1200.)
            (" 1800.    " 1800.)
            (" 2000.    " 2000.)
            (" 2400.    " 2400.)
            (" 3600.    " 3600.)
            (" 4800.    " 4800.)
            (" 7200.    " 7200.)
            (" 9600.    " 9600.)
            (" 19200.   " 19200.)))

))

; From file WINDOW.LISP#> QL.KERMIT; LAD: (67)
#8R KERMIT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "KERMIT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: KERMIT; WINDOW  "

(defun set-baud-rate ()
  (let ((base 10.) (*nopoint nil))              ;just for printing
    (cond
      (kermit-serial-stream
       (let ((old-baud (current-baud-rate)))
         (with-status ("Change Baud~%Old Baud Rate: ~S" old-baud)
           (let ((new-baud
                   (tv:menu-choose
                     all-baud-choices-items-alist
                     "Choose the Baud Rate:"
                     '(:mouse)
                     nil
                     terminal-pane)))
             (cond ((and new-baud (zerop new-baud))
                    (extended-set-baud-rate))
                   ((and new-baud               ; nil if they move out of the window
                         (not (= old-baud new-baud)))   ;really have to change it
                    (set-current-baud-rate new-baud)
                    (format t "~&New Baud Rate: ~S~%" new-baud)))))))
      (t (ferror nil "kermit-serial-stream is NIL.")))))

))

; From file WINDOW.LISP#> QL.KERMIT; LAD: (67)
#8R KERMIT#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "KERMIT")))
  (COMPILER#:PATCH-SOURCE-FILE "SYS: KERMIT; WINDOW  "

(defun extended-set-baud-rate ()
  (select-processor
    (:lambda
      (when (typep kermit-serial-stream 'si:sdu-serial-stream)
        (let ((old-char-length (symeval-in-instance kermit-serial-stream 'si:char-length))
              (old-stop-bits (symeval-in-instance kermit-serial-stream 'si:stop-bits))
              (old-parity (symeval-in-instance kermit-serial-stream 'si:parity))
              (old-baud-rate (symeval-in-instance kermit-serial-stream 'si:baud-rate)))
          (let ((char-length old-char-length)
                (stop-bits old-stop-bits)
                (parity old-parity)
                (baud-rate old-baud-rate))
            (tv:choose-variable-values
              `((,(locf char-length) "Character Length" :choose (:5bits :6bits :7bits :8bits))
                (,(locf stop-bits)   "Stop Bits       " :choose (:1bit :1.5bits :2bits))
                (,(locf parity)      "Parity or None  " :choose (:even :odd NIL))
                (,(locf baud-rate)   "Baud Rate       " :number))
              :label "Extended Choice of Serial Characteristics")
            (or (equal old-char-length char-length)
                (send kermit-serial-stream :set-char-length char-length))
            (or (equal old-stop-bits stop-bits)
                (send kermit-serial-stream :set-stop-bits stop-bits))
            (or (equal old-parity parity)
                (send kermit-serial-stream :Set-parity parity))
            (or (equal old-baud-rate baud-rate)
                (send kermit-serial-stream :set-baud-rate (fix baud-rate)))))))))

))
