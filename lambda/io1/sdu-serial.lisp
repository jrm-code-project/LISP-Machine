;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:8; Readtable:ZL -*-
;;; (c) Copyright 1984,1985,1986 Lisp Machine, Inc.

; sdu serial i/o code for lambdas


(defconst sdu-lambda-serial-status-meanings
          '((0001 "xmit buffer empty" "xmit buffer not empty")
            (0101 "rcv char avail" "no rcv char avail")
            (0201 "the other xmit buffer empty"
                  "the other xmit buffer not empty")
            (0301 "parity error" "no parity error")
            (0401 "overrun error" "no overrun error")
            (0501 "framing error" "no framing error")
            (0601 "break detect (sync mode)"
                  "no break detect (sync mode)")
            (0701 "data set ready" "not data set ready")))

(defun sdu-serial-status ()
  (SEND (OR (SEND (SEND (FS:PARSE-PATHNAME "SDU-SERIAL-B:") :HOST) :LAST-INSTANCE)
            (FERROR NIL "SDU-SERIAL-B not open, cant describe status"))
        :describe-status))


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
         (parity nil)
         shared-device
         last-index-given-away
         )
        (si:buffered-stream)
  (:required-flavors si:bidirectional-stream si:character-stream
                     si:unbuffered-line-input-stream
                     si:basic-buffered-output-stream)
  (:initable-instance-variables)
  (:settable-instance-variables))


(defmethod (sdu-serial-stream-mixin :print-self) (stream flag depth)
  flag depth
  (printing-random-object (self stream :type)
    (prin1 (send (send self :shared-device) :host))))

(defflavor sdu-serial-stream () (sdu-serial-stream-mixin
                             si:bidirectional-stream
                             si:character-stream
                             si:unbuffered-line-input-stream
                             si:buffered-output-stream)
  (:documentation :combination "RS232 Serial I//O Stream, no character-set translation"))

;;; Housekeeping methods

(defvar *sdu-serial-stream-csr-alist* '(("SDU-SERIAL-B"
                                         (:pci-csr #x1c15c)
                                         (:pci-data #x1c158)
                                         (:pit-mode #x1c17c)
                                         (:pit-counter #x1c174)
                                         (:pit-channel #x40)
                                         (:xmit-interrupt-number 113)
                                         (:rcv-interrupt-number 112))
                                        ("SDU-SERIAL-A"
                                         (:pci-csr #x1c154)
                                         (:pci-data #x1c150)
                                         (:pit-mode #x1c17c)
                                         (:pit-counter #x1c170)
                                         (:pit-channel 0)
                                         (:xmit-interrupt-number 111)
                                         (:rcv-interrupt-number 110))))

(defmethod (sdu-serial-stream-mixin :after :init) (init-plist)
  init-plist
  (DOLIST (REG (CDR (OR (ASS #'STRING-EQUAL
                             (SEND (SEND (SEND SELF :SHARED-DEVICE) :HOST) :NAME)
                             *sdu-serial-stream-csr-alist*)
                        (FERROR NIL "internal error, no CSR's known for this device"))))
    ;; Yes. This overrides the default values *and* the init plist.
    (SEND SELF :SET (CAR REG) (CADR REG)))
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :after :set-baud-rate) (baud)
  baud
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :set-input-buffer-size) (size)
  (check-type size (fixnum 1))
  (setq input-buffer-size size)
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :set-output-buffer-size) (size)
  (check-type size (fixnum 1))
  (setq output-buffer-size size)
  (send self :reset))


(defmethod (sdu-serial-stream-mixin :close) (&optional abort-p)
  (and (not abort-p) (variable-boundp output-unibus-channel) output-unibus-channel
       (send self :finish))
  (send self :remove-unibus-channels)
  (send shared-device :close abort-p))

(defmethod (sdu-serial-stream-mixin :remove-unibus-channels) ()
  (and (variable-boundp input-unibus-channel) input-unibus-channel
       (return-unibus-channel (prog1 input-unibus-channel
                                     (setq input-unibus-channel nil))))
  (and (variable-boundp output-unibus-channel) output-unibus-channel
       (return-unibus-channel (prog1 output-unibus-channel
                                     (setq output-unibus-channel nil)))))


(defmethod (sdu-serial-stream-mixin :finish) ()
  (process-wait "Serial Finish"
                #'(lambda (chan) (not (unibus-channel-not-empty chan)))
                output-unibus-channel))

(defmethod (sdu-serial-stream-mixin :after :clear-input) ()
  (funcall-self :reset nil nil))

(defmethod (sdu-serial-stream-mixin :describe-status) ()
  (let ((status (%multibus-read-8 pci-csr)))
    (format t "~&status = ~O" status)
    (dolist (l sdu-lambda-serial-status-meanings)
      (cond ((zerop (ldb (car l) status))
             (format t "~&~A" (caddr l)))
            (t
             (format t "~&~A" (cadr l)))))))

;mode instruction bits for the INTEL 8251 chip.
; bits 0&1   baud rate factor  0=sync mode; 1=1x; 2=16x; 3=64x
; bits 2&3   char length 0=5bits; 1=6bits; 2=7bits; 3=8bits
; bit 4      parity enable
; bit 5      generate parity  1=even; 0=odd
; bits 6&7   number of stop bits  0=illegal; 1=1 bit; 2=1.5 bits; 3=2 bits
; we use 16x, 8bit, no parity, 2 stop bits = 316

(defconst *8251-mode-word-description*
          '((baud-rate-factor #o0002 (:sync-mode . 0) (:1x . 1) (:16x . 2) (:64x . 3))
            (char-length      #o0202 (:5bits . 0)(:6bits . 1)(:7bits . 2)(:8bits . 3))
            (parity-enable    #o0401 (nil . 0) (t . 1))
            (generate-parity  #o0501 (:even . 1)(:odd . 0)(nil . 0))
            (stop-bits        #o0602 (:1bit . 1)(:1.5bits . 2)(:2bits . 3))))

;command instruction bits
; bit 0      transmit enable
; bit 1      "data terminal ready" output
; bit 2      receive enable
; bit 3      send break
; bit 4      reset error flags
; bit 5      "request to send" output
; bit 6      master reset
; bit 7      enter hunt mode (sync mode)
; we use transmit enable; DTR; receive; reset errors; RTS = 67

(defconst *8251-command-word-description*
          '((transmit-enable #o0001 (nil . 0) (t . 1))
            (dtr             #o0101 (nil . 0) (t . 1))
            (receive-enable  #o0201 (nil . 0) (t . 1))
            (send-break      #o0301 (nil . 0) (t . 1))
            (reset-errors    #o0401 (nil . 0) (t . 1))
            (rts             #o0501 (nil . 0) (t . 1))
            (master-reset    #o0601 (nil . 0) (t . 1))
            (sync-mode       #o0701 (nil . 0) (t . 1))))

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

(defmethod (sdu-serial-stream-mixin :set-char-length) (length)
  (symeval-in-field-context length 'char-length *8251-mode-word-description*)
  (setq char-length length)
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :set-stop-bits) (length)
  (symeval-in-field-context length 'stop-bits *8251-mode-word-description*)
  (setq stop-bits length)
  (send self :reset))

(defmethod (sdu-serial-stream-mixin :set-parity) (type)
  (symeval-in-field-context type 'generate-parity *8251-mode-word-description*)
  (setq parity type)
  (send self :reset))


;; because Pace changed the protocol here to use the SI:BUFFERED-INPUT-STREAM
;; we need to kludge (indications of which is the usage of dynamic variables)
;; a bit to implement the useful :TYI-BUSY-WAIT and :TYI-WITH-TIMEOUT functions.
;; The right way to fix this is to add these to SI:BUFFERED-INPUT-STREAM and
;; to generalize the protocol of its :NEXT-INPUT-BUFFER-MESSAGE.

(defun sdu-serial-stream-mixin-default-input-wait (unibus-channel)
  (process-wait "Serial Stream Input" #'unibus-channel-not-empty unibus-channel)
  t)

(defun sdu-serial-stream-mixin-busy-input-wait (unibus-channel)
  (do ()
      ((unibus-channel-not-empty unibus-channel)
       t)))

(defvar *sdu-serial-stream-mixin-input-wait* 'sdu-serial-stream-mixin-default-input-wait)

(defmethod (sdu-serial-stream-mixin :tyi-busy-wait) (&optional ignore)
  ;; this was added by pace in doing the explorer-serial-debug-interface.
  (let ((*sdu-serial-stream-mixin-input-wait* 'sdu-serial-stream-mixin-busy-input-wait))
    (send self :tyi)))

(defvar *sdu-serial-stream-mixin-timeout-input-wait*)

(defun sdu-serial-stream-mixin-timeout-input-wait (unibus-channel)
  (PROCESS-WAIT "Serial input or timeout"
                #'(LAMBDA (NOW TIMEOUT UNIBUS-CHANNEL)
                    (OR (UNIBUS-CHANNEL-NOT-EMPTY UNIBUS-CHANNEL)
                        (> (TIME-DIFFERENCE (TIME) NOW) TIMEOUT)))
                (TIME)
                *sdu-serial-stream-mixin-timeout-input-wait*
                UNIBUS-CHANNEL)
  (UNIBUS-CHANNEL-NOT-EMPTY UNIBUS-CHANNEL))

(DEFMETHOD (SDU-SERIAL-STREAM-MIXIN :TYI-WITH-TIMEOUT) (TIMEOUT)
  ;;; TYI-WITH-TIMEOUT -- added by MHD @ LMI, 4/12/84
  ;;; (used by KERMIT which could better use :READ-INPUT-BUFFER now)
  (let ((*sdu-serial-stream-mixin-input-wait* 'sdu-serial-stream-mixin-timeout-input-wait)
        (*sdu-serial-stream-mixin-timeout-input-wait* timeout))
    (send self :tyi)))

;;; new buffered input protocol

;;; values are array, first index that has data, and the first index that doesn't have data
(defmethod (sdu-serial-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (block no-hang
    (let ((initial-data-available (unibus-channel-not-empty input-unibus-channel)))
      (cond ((and (null initial-data-available) no-hang-p)
             nil)
            (t
             (when (null initial-data-available)
               (when (not (funcall *sdu-serial-stream-mixin-input-wait* input-unibus-channel))
                 (return-from no-hang nil)))
             (let ((start (%pointer-difference
                            (%p-ldb-offset %%q-pointer input-unibus-channel
                                           %unibus-channel-buffer-out-ptr)
                            input-unibus-channel))
                   (end (%pointer-difference
                          (%p-ldb-offset %%q-pointer input-unibus-channel
                                         %unibus-channel-buffer-in-ptr)
                          input-unibus-channel)))
               (cond ((> start end)
                      (setq end (%pointer-difference
                                  (%p-ldb-offset %%q-pointer input-unibus-channel
                                                 %unibus-channel-buffer-end)
                                  input-unibus-channel))))
               (setq last-index-given-away end)
               (decf start (array-data-offset input-unibus-channel))
               (decf end (array-data-offset input-unibus-channel))
               (do ((j start (1+ j))
                    (array input-unibus-channel)
                    (n end))
                   ((= j n))
                 ;; get rid of the high bits, which are a timestamp.
                 (setf (aref array j) (logand #xFF (aref array j))))
               (values input-unibus-channel start end)))))))


(defmethod (sdu-serial-stream-mixin :discard-input-buffer) (ignore)
  (let ((new-out-ptr (%pointer-plus last-index-given-away
                                    input-unibus-channel)))
    (cond ((>= (%pointer-difference new-out-ptr
                                    (%p-ldb-offset %%q-pointer input-unibus-channel
                                                   %unibus-channel-buffer-end))
               0)
           (setq new-out-ptr (%p-ldb-offset %%q-pointer input-unibus-channel
                                            %unibus-channel-buffer-start))))
    (%p-dpb-offset new-out-ptr
                   %%q-pointer
                   input-unibus-channel
                   %unibus-channel-buffer-out-ptr)))


;;; Next three methods implement buffered output stream protocol
(defmethod (sdu-serial-stream-mixin :new-output-buffer) ()
  (process-wait "Serial TYO" #'unibus-channel-not-full output-unibus-channel)
  (multiple-value-bind (start end) (unibus-channel-space-available output-unibus-channel)
    (values output-unibus-channel start end)))

(defmethod (sdu-serial-stream-mixin :send-output-buffer) (ignore new-index)
  (unibus-channel-advance output-unibus-channel new-index)
  (%nubus-write rg-quad-slot
                (* (+ 400
                      (%p-ldb-offset %%q-pointer
                                     output-unibus-channel
                                     %unibus-channel-vector-address))
                   4)
                1))


(defmethod (sdu-serial-stream-mixin :make-interrupt) ()
  (%nubus-write rg-quad-slot
                (* (+ 400
                      (%p-ldb-offset %%q-pointer
                                     output-unibus-channel
                                     %unibus-channel-vector-address))
                   4)
                1))


(defmethod (sdu-serial-stream-mixin :discard-output-buffer) (ignore)
   nil)

;The inherited method for this (from BUFFERED-OUTPUT-STREAM) does not work
;because we have a single circular buffer, so just punt.
;This is used by :FRESH-LINE.
(defmethod (sdu-serial-stream-mixin :last-char-output) () nil)

;For compatibility with the old unbuffered stream, we provide a mode where
;you don't have to do :FORCE-OUTPUT manually, and even make it the default.
;This should be a mixin, but it was too inconvenient to have so many flavors
;for such a trivial thing.
(defmethod (sdu-serial-stream-mixin :after :tyo) (ignore)
  (and force-output-p (funcall-self :force-output)))

(defmethod (sdu-serial-stream-mixin :after :string-out) (&rest ignore)
  (and force-output-p (funcall-self :force-output)))


(defflavor sdu-serial-ascii-stream ()
           (si:ascii-translating-output-stream-mixin si:ascii-translating-input-stream-mixin
            sdu-serial-stream-mixin
            si:bidirectional-stream si:character-stream
            si:unbuffered-line-input-stream
            si:basic-buffered-output-stream)
  (:documentation :combination "RS232 Serial I//O Stream, Ascii character translation"))

(defflavor sdu-serial-xon-xoff-mixin ((buffering-capacity 10.)) ()
  (:initable-instance-variables buffering-capacity)
  (:required-flavors sdu-serial-stream-mixin)
  (:documentation :mixin "Serial output controlled by XON and XOFF characters.
   BUFFERING-CAPACITY is one half the allowed number of characters between XOFF checks."))

(defflavor sdu-serial-xon-xoff-stream () (sdu-serial-xon-xoff-mixin sdu-serial-stream)
  (:documentation :combination
                  "RS232 Serial I//O Stream, XON//XOFF, no character translation"))

(defflavor sdu-serial-ascii-xon-xoff-stream () (sdu-serial-xon-xoff-mixin sdu-serial-ascii-stream)
  (:documentation :combination
                  "RS232 Serial I//O Stream, XON//XOFF, ascii character translation"))

(defmethod (sdu-serial-xon-xoff-mixin :new-output-buffer) ()
  (process-wait "Serial TYO"
                #'unibus-channel-not-full output-unibus-channel buffering-capacity)
  (let ((ch (funcall-self :tyi-no-hang)))
    (selectq ch
      (23       ;XOFF (^S)
        (loop do (process-wait "Serial XON" #'unibus-channel-not-empty input-unibus-channel)
              until (= (funcall-self :tyi) 21)))        ;XON (^Q)
      (nil )
      (otherwise nil)))
  (multiple-value-bind (start end) (unibus-channel-space-available output-unibus-channel)
    (if (> (- end start) buffering-capacity)
        (setq end (+ start buffering-capacity)))
    (values output-unibus-channel start end)))


(defmethod (sdu-serial-stream-mixin :after :describe) ()
  (send self :describe-status)
  (cond ((and (variable-boundp input-unibus-channel) input-unibus-channel)
         (format t "~2&Input input unibus channel:")
         (describe-unibus-channel input-unibus-channel)))
  (cond ((and (variable-boundp output-unibus-channel) output-unibus-channel)
         (format t "~2&Output input unibus channel:")
         (describe-unibus-channel output-unibus-channel))))


(defun describe-unibus-channel (u)
  (let ((pointer (%pointer u))
        (vector (%p-ldb-offset %%q-pointer u %unibus-channel-vector-address)))
    (format t "address ~O; vector ~O" pointer vector)
    (dolist (q unibus-channel-qs)
      (format t "~&~8t~S ~O (offset = ~O)"
              q
              (%p-ldb-offset %%q-pointer u (symeval q))
              (- (%p-ldb-offset %%q-pointer u (symeval q)) (%pointer u))))
    (format t "~&  CSR bit values:")
    (dolist (b unibus-csr-bits)
      (format t "~&~8t~S ~O" b
              (ldb (symeval b) (%p-ldb-offset %%q-pointer u %unibus-channel-csr-bits))))
    (without-interrupts
      (do ((x (%pointer (system-communication-area %sys-com-unibus-interrupt-list))
              (%p-ldb %%q-pointer (+ x %unibus-channel-link))))
          ((zerop x)
           (format t "~&Not on %sys-com-unibus-interrupt-list"))
        (cond ((eq (%p-ldb-offset %%q-pointer x %unibus-channel-vector-address) vector)
               (cond ((eq x pointer)
                      (format t "~&This unibus-channel is the first on ")
                      (format t "the interrupt list with its vector"))
                     (t
                      (format t "~&The unibus-channel at ~O has the same vector, " x)
                      (format t "but comes before this one on the interrupt list")))
               (return nil)))))))


(defun unibus-channel-sp (chan)
  "Describe the state of the ring buffer part of the channel"
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET ((IN-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR))
        (OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
        (START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
        (END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)))
    ;; CONCEPTUALLY: LET START BE EQUAL TO ZERO.
    (SETQ END (- END START))
    (SETQ IN-PTR (- IN-PTR START))
    (SETQ OUT-PTR (- OUT-PTR START))
    (format t "~%IN = ~3D, OUT = ~3D, DIM = ~3D AVAIL = ~3D"
            IN-PTR OUT-PTR END
            (- (+ IN-PTR (IF (< IN-PTR OUT-PTR) END 0))
               OUT-PTR))))


(defun unibus-channel-data-available (chan)
  "Non zero means same as UNIBUS-CHANNEL-NOT-EMPTY"
  (DECLARE (VALUES NUMBER-OF-DATA-AVAILABLE MAXIMUM-NUMBER-OF-DATA))
  (LET ((IN-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR))
        (OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
        (START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
        (END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)))
    ;; CONCEPTUALLY: LET START BE EQUAL TO ZERO.
    (SETQ END (- END START))
    (SETQ IN-PTR (- IN-PTR START))
    (SETQ OUT-PTR (- OUT-PTR START))
    (VALUES (- (+ IN-PTR (IF (< IN-PTR OUT-PTR) END 0))
               OUT-PTR)
            END)))



(defun assemble-word-from-description (context &rest description)
  ;; this shouldn't be in here.
  "Description is a list of <keyword> <value> ...
The <keyword>s are names of fields, and the <value>s are arithmetic expressions
possibly containing some symbols with arithmetic values defined in the context.
The context is an alist of elements (<keyword> <ppss> . <symbol-alist>)
Symbols in the <value> expression are looked up in the <symbol-alist>"
  (do ((word 0)
       (l description (cddr l)))
      ((null l) word)
    (let ((key-context (lookup-bit-field-key (car l) context)))
      (setf (ldb (car key-context) word)
            (eval-arithmetic-expression (cadr l) (cdr key-context))))))

(defun lookup-bit-field-key (key context)
  (cdr (or (assq key context)
           (ferror nil "unknown bit field: ~S" key))))

(defun disassemble-field-value (context key value)
  (car (or (rassoc value (cdr (lookup-bit-field-key key context)))
           (ferror nil "~S is an unknown value for ~S field" value key))))

(defun symeval-in-field-context (symbol key context)
  (cdr (or (assq symbol (cdr (lookup-bit-field-key key context)))
           (ferror nil "~S is an undefined symbol for ~S field" symbol key))))

(defun eval-arithmetic-expression (exp alist)
  (cond ((symbolp exp)
         (cdr (or (assq exp alist)
                  (ferror nil "undefined symbol: ~S" exp))))
        ((numberp exp) exp)
        ((atom exp)
         (ferror nil "unknown type of expression: ~S" exp))
        ('else
         (apply (car exp) (mapcar #'(lambda (e) (eval-arithmetic-expression e alist))
                                  (cdr exp))))))

(defun print-unibus-channel-buffer (u)
  (do ((i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-start) (1+ i)))
      ((>= i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-end)))
    (tyo (%p-ldb (byte 7 0) i))))

(defun make-string-from-unibus-channel-buffer (u)
  (with-output-to-string (s)
    (do ((i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-start) (1+ i)))
        ((>= i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-end)))
      (let ((c (%p-ldb (byte 8 0) i)))
        (if (not (zerop c))
            (send s :tyo c))))))

(defun clear-unibus-channel-buffer (u)
  (do ((i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-start) (1+ i)))
      ((>= i (%p-ldb-offset %%q-pointer u %unibus-channel-buffer-end)))
    (%p-dpb 0 (byte 8 0) i)))

(defmethod (sdu-serial-stream-mixin :print-buffers) ()
  (format t "~&Input~&")
  (print-unibus-channel-buffer input-unibus-channel)
  (format t "~&Output~&")
  (print-unibus-channel-buffer output-unibus-channel))

(compile-flavor-methods sdu-serial-stream sdu-serial-ascii-stream
                        sdu-serial-xon-xoff-stream sdu-serial-ascii-xon-xoff-stream)


;;;A function to generate the correct serial stream flavor symbol from
;;;keyword arguments:

(defun combined-sdu-serial-stream-flavor(&key ascii xon-xoff)
  "Returns the symbol name of the appropriate serial port stream flavor,
based on the keyword argument values.

If ASCII is non-NIL, the returned flavor name supports ASCII character translation.

If XON-XOFF is non-NIL, the returned flavor name supports software flow control."
  ;;
  (declare (values symbol string))
  (let((name
         (format nil "SDU-SERIAL-~@[~*ASCII-~]~@[~*XON-XOFF-~]STREAM" ascii xon-xoff)))
    (values (intern-soft name 'system-internals) name)))

;;;Use this as a type predicate if you object to testing whether an
;;;object is of SDU-SERIAL-STREAM-MIXIN.

(defun sdu-serial-stream-p(stream)
  (typecase stream
    (si:sdu-serial-stream t)
    (si:sdu-serial-ascii-stream t)))
