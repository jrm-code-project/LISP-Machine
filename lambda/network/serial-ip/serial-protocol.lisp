;;; -*- Mode:LISP; Package:SERIAL-PROTO; Base:10; Readtable:CL -*-

#|

  Copyright GigaMos Systems, Inc. 1988
   See filename "Copyright.Text" for
  licensing and release information.

This is part of the implementation of the "Serial IP" interface for the
new network system.  This code implements serial connection types, the
serial packet protocol, and low-level serial I/O.  The protocol is
described below.

The key interface routines are as follows:

(MAKE-SERIAL-PROTO-STREAM :TYPE <type>) - connection type determines open
and connection methods to be used.  No other args strictly required, but
these are the ones that can/should be externally settable:

  :TYPE - defaults to :HARDWIRED (default connection, returns T)
  :STREAM - if you default this, send stream ":OPEN T" to OPEN protocol stream
    without calling open method on serial stream
  :OPEN-FCN - a function for :OPEN method to call to open the serial stream;
    defaults to DEFAULT-SERIAL-OPEN-METHOD for all standard connection types.
    Can take any arguments you like; Serial-IP passes it the site's OPEN-ARGS.
  :CONNECT-FCN  - a function for :CONNECT method to call to connect with remote
     port; defaults are appropriate to connect types.
  :CLOSE-FCN - a function for :CLOSE method to call to close the serial stream;
     should take one arg, ABORT-P.
  :TYI-ARGS - a list of arguments to APPLY to the serial stream object.
     For example, on the Lambda:
      (:TYI)            - means wait for a character
      (:TYI-NO-HANG)    - return immediately unless there's a character
      (:TYI-WITH-TIMEOUT <number>) - timeout after <number> * 1/60th second.

(NEW-SERIAL-PROTO-PACKET) - returns a handy packet string; or use your own.

(SERIAL-PROTO-RECEIVE) - asynchronous receive; grab any available input.

(SERIAL-PROTO-SEND) - send a data packet out on serial stream.

(WITH-SERIAL-PORT-ALLOCATED <device>) - allocate and keep the device;
  steal it if necessary.

|#

(in-package "SERIAL-PROTO")

(use-package '("NETWORK" "LISP"))

(export '(
          serial-port-listen
          with-serial-port-allocated
          *serial-open-args-alist*
          *serial-proto-max-packet-size*
          *default-serial-device*
          *default-baud-rate*
          *default-tyi-with-timeout*
          make-serial-proto-stream
          new-serial-proto-packet
          serial-proto-receive
          serial-proto-send))

(defparameter *default-serial-device*
          #+LMI "SDU-SERIAL-B")


;;;Serial port utility functions

(defun serial-port-open-p(stream)
  (and stream
       #+LMI
       (and (funcall stream :input-unibus-channel)
            (funcall stream :output-unibus-channel))))

(defun serial-port-owned-by-me-p(stream &aux path dev)
  (and stream
       #+LMI
       (and (setq path (send stream :shared-device))
            (setq dev (send path :host))
            (eq (car (send dev :lock)) si:current-process))))

(defun serial-port-listen(stream)
  #+LMI
  (send stream :listen))

(defun serial-port-locate(&optional (name *default-serial-device*))
  (declare(values device))
  (find name si:all-shared-devices
                  :key  #'(lambda(dev)(send dev :name))
                  :test #'string-equal))

(defun serial-port-allocate(&optional (name *default-serial-device*))
  (let((dev (serial-port-locate name)))
    (cond
      ((null dev)
       (error "No such device ~a" name))
      ((not(typep dev 'si:sdu-serial-b-shared-device))
       (error "Device ~s is not an SDU Serial device?"))
      ((null
         (progn
           (send dev :allocate)
           (send dev :allocate-if-easy)))
       nil)
      ((eq (car (send dev :lock)) si:current-process)
       ;;It's ours, or was
         dev)
      (t
       (or
         (null (car (send dev :lock)))
         (cerror "Free the device and hope the other user doesn't use it"
                 "The serial device ~s~% is locked by ~s"
                 dev (car(send dev :lock))))
       (send dev :steal-lock)
       dev))))

(defun serial-port-deallocate(dev)
  (send dev :free-lock)
  (send dev :deallocate))

(defmacro with-serial-port-allocated(device-name &body body)
  `(let(dev locker)
     (unwind-protect
         (progn
           (setq dev (serial-port-allocate ,device-name))
           (setq locker (car (send dev :lock)))
           ,@body)
       (and dev
            (eq locker (car(send dev :lock)))
                (serial-port-deallocate dev)))))


#|

SERIAL PORT CONNECTION METHODS

A connection method type is denoted by a keyword, which is associated with
open, connect, and close methods. This keyword must have the following
symbol properties:

1. SERIAL-PROTO:OPEN-METHOD - a function to call to open up the serial port.

2. SERIAL-PROTO:CONNECT-METHOD - a function to perform any further processing
over the serial port, e.g. dialing the telephone, modem handshakes, etc.

Any arguments to these functions get passed through the protocol stream's
OPEN or CONNECT method. The stream's :CLOSE method calls the stream's
assigned CLOSE-FCN; this is not associated by keyword, but can be set when
initializing the protocol stream.

Certain standard connection types are defined:

1) :DIAL-UP - arguments tell how to auto-dial the remote site; they are
<TELEPHONE-NUMBER> and <MODEM-TYPE>. The default connection method
(someday????) uses these to auto-dial the modem.

2) :HARDWIRED - hosts are hardwired together.

Implementors may have to define their own site types and/or methods; for
example, if it is necessary to pop up a menu or otherwise prompt for the
operator to specify anything relevant.

In the standard OPEN method, the variable *SERIAL-OPEN-ARGS-ALIST* is an
A-list that, when flattened, specifies any other serial port initialization
that may be required (e.g. parity, stop-bits).

|#

;;Getting the methods:

(defmacro serial-open-method(type)
  `(get ,type 'open-method))

(defmacro serial-connect-method(type)
  `(get ,type 'connect-method))

(defun serial-device-pathname(&optional (device *default-serial-device*))
  (string-append
    (string-right-trim ":" device)
    ":"))

(defparameter *default-baud-rate* 1200.)

(defvar *serial-open-args-alist*
        '((:parity :even)
          (:char-length :8bits)
          (:baud-rate 1200.)
          #+LMI (:force-output-p nil)
          #+LMI (:input-buffer-size 1023.)
          #+LMI (:output-buffer-size 1023.)
          )
  "A-list of additional arguments to be supplied to open the serial device")

;; DEFAULT-SERIAL-OPEN-METHOD must be defined when porting this code!!!

#-LMI
(eval-when(compile)
  (cerror "Compile anyway with error-signalling definition"
          "The function DEFAULT-SERIAL-OPEN-METHOD must be defined")
  (defun default-serial-open-method(&rest ignore)
    (error "I wasn't kidding; define DEFAULT-SERIAL-OPEN-METHOD!")))

#+LMI
(defun default-serial-open-method(&optional
                                  (device *default-serial-device*)
                                  &rest args
                                  &key (baud-rate *default-baud-rate*)
                                  &allow-other-keys
                                  &aux sdu-serial-stream
                                  (all-args nil)
                                  (open-args nil))
  (declare(values sdu-serial-stream))
  (setq all-args (append (copy-list args)
                          (list :baud-rate baud-rate)
                          (loop for pair in (copy-list *serial-open-args-alist*)
                                append pair)))
  (do*((pair all-args (cddr pair))
       (key (car pair) (car pair))
       (val (cadr pair) (cadr pair)))
      ((null pair))
    (pushnew (list key val) open-args :key #'car))
  (setq sdu-serial-stream
        (open (serial-device-pathname device)
              :flavor-and-init-options
              `(si:sdu-serial-stream
                 ,@(loop for pair in open-args append pair))))
  (send sdu-serial-stream :clear-input)
  (send sdu-serial-stream :clear-output)
  (send sdu-serial-stream :reset)
  sdu-serial-stream)

(defun default-serial-null-connect-method(&rest ignore)
  t)

(defun default-serial-close-method(stream &optional abortp &rest ignore)
  (and (serial-port-open-p stream)
       (serial-port-owned-by-me-p stream)
       (close stream :abort abortp)))

(defun dialup-serial-connect-method(telephone modem-type &rest ignore)
  telephone
  (error "I don't know to dial-up a site yet")
  (case modem-type
    (t (error "I don't know to dial up on a ~a" modem-type))))

;;;Defined connection types:

;; :HARDWIRED

(setf (serial-open-method :hardwired)
      #'default-serial-open-method)

(setf (serial-connect-method :hardwired)
      #'default-serial-null-connect-method)

;; :DIAL-UP

(setf (serial-open-method :dial-up)
      #'default-serial-open-method)

(setf (serial-connect-method :dial-up)
      #'dialup-serial-connect-method)

#|
Protocol I/O Stream

A SERIAL-SPROTO-STREAM is defined by DEFSTREAM (implemented by the new
Network software), which creates a DEFSTRUCT that acts much like a
Flavors object; it defines a function which dispatches on a message.

The protocol is taken from a "Serial Line Interface" protocol by
"rick@seismo.ARPA", which does not packetize I/O per se.  Rather, a
special character, SP-FRAME-END, indicates the end-of-packet.  Another
character, SP-FRAME-ESCAPE, is used to precede and "escape" special
characters.  However, the SP-FRAME-END and SP-FRAME-ESCAPE characters
themselves are not send following an escape; translation characters,
SP-FRAME-XEND and SP-FRAME-XESCAPE, are sent.  Thus an SP-FRAME-ESCAPE
or SP-FRAME-END received in the data stream always mean what they say.

A packet is expected to be an array of 8-byte words/characters whose
FILL-POINTER indicates the number of characters already in the packet,
and thus it is the index with which the next character should be read
or written.

Any data or synchronization problem will not be detected; rather, the
user of these packets must figure out whether they got the right stuff.
In the worst case, an escape or end character could be dropped; on the
other hand, eventually an end character will be found, and the packets
will be "in synch" again.  This is OK for Internet.

Input is asynchronous and packetized; output is done all at once.  This
is because we have control over our own output, but may stall for input.

|#

(defvar *default-tyi-with-timeout* 6. "1/10th second, timeout on :TYI-WITH-TIMEOUT")

(defstream serial-proto-stream
           ()
           "SP-"
  (type      :hardwired)                        ;Connection type
  (stream    nil)                               ;Associated stream
  (status    nil)                               ;STREAM status
  (closed    :not-opened-yet)                   ;Closed flag
  (packet    nil)                               ;Current packet
  (open-fcn  (or (serial-open-method type)
                 #'default-serial-open-method)) ;Function to open STREAM
  (connect-fcn (serial-connect-method type))    ;Function to connect further
  (close-fcn #'default-serial-close-method)     ;Function to close STREAM
  ;;Assuming STREAM is something arguments can be APPLY'd to, the args are:
  (tyi-args (list :tyi-with-timeout             ;TYI with Timeout when no input,
                  *default-tyi-with-timeout*))  ;  input timeout in 1/60th second
  (escaping  nil)                               ;Escaping flag
  )

;;;Open/close stream methods:

(defop (serial-proto-stream :open) (&optional (stream-or-pathname *default-serial-device*)
                                              &rest args)
  (when
    (cond
      ((and (sp-closed self)                    ;Can't reopen
            (not (eq (sp-closed self) :not-opened-yet)))
       nil)
      ((eq stream-or-pathname t) t)             ;Call with T if stream already init'd
      ((streamp stream-or-pathname)             ;Call with a stream
       (setf (sp-stream self) stream-or-pathname))
      ((sp-open-fcn self)                       ;Call open method
       (streamp
         (setf (sp-stream self) (apply  (sp-open-fcn self)  stream-or-pathname args)))))
    (setf (sp-closed self) nil)                 ;Set status
    (setf (sp-status self) :open)))

(defop (serial-proto-stream :connect) (&rest args)
    (when (sp-connect-fcn self)                 ;Call connect method
      (apply (sp-connect-fcn self) (sp-stream self) args)))

(defop (serial-proto-stream :close) (&optional abortp)
  (setf (sp-closed self) :closed)
  (setf (sp-status self) :closed)
  ;;This sends 3 null packets to tell the other side to shut down.
  (dotimes(p 3)
    (send self :finish-output))
  (when (sp-close-fcn self)
    (funcall (sp-close-fcn self) (sp-stream self) abortp)))

;;;Pass args along to STREAM variable:

(defop (serial-proto-stream :pass) (stream-method &rest args)
  (apply (sp-stream self) stream-method args))

;;;Packet special characters:

(defconstant *serial-proto-max-packet-size* 1036.)      ;???
(defconstant *sp-frame-end*     #o300)
(defconstant *sp-frame-escape*  #o333)
(defconstant *sp-frame-xend*    #o334)
(defconstant *sp-frame-xescape* #o335)

;;;Protocol utility functions:

;;Callers can use this to make a packet, or use their own arrays

(defun new-serial-proto-packet()
  (make-array *serial-proto-max-packet-size*
              :element-type 'string-char
              :initial-element 0.
              :fill-pointer 0.))

(defun full-packet(packet)
 (>= (fill-pointer packet) (array-length packet)))


;;;Input methods

(defop (serial-proto-stream :full-packet) ()
  (when (full-packet (sp-packet self))
    (setf (sp-status self) :full)))

(defop (serial-proto-stream :done-input-p) ()
  (or (send self :full-packet)
      (member (sp-status self) '(:done :full))))

(defop (serial-proto-stream :new-packet) (&optional new-packet)
  (unless (sp-closed self)
    (prog1
      (setf (sp-packet self) (or new-packet (new-serial-proto-packet)))
      (setf (sp-status self) :open))))

(defop (serial-proto-stream :listen) ()
  (funcall (sp-stream self) :listen))

(defop (serial-proto-stream :tyi) (&optional (tyi-args (sp-tyi-args self)))
  (cond
    ((sp-closed self) (values nil (sp-closed self)))
    ((funcall self :done-input-p) (values nil (sp-status self)))
    (t
     (prog(ch)
          (unless (setq ch (apply (sp-stream self) tyi-args))
            (return (values nil :eof)))
          (setf (aref (sp-packet self) (fill-pointer (sp-packet self)))
                (cond
                  ((char= ch *sp-frame-end*)
                   (setf (sp-status self) :done)
                   (return (values nil :done)))
                  ((not (sp-escaping self))
                   (cond
                     ((char= ch *sp-frame-escape*)
                      (setf (sp-escaping self) t)
                      (return (values nil :escaping)))
                     (t ch)))
                  (t                            ;Escaping
                   (setf (sp-escaping self) nil)
                   (cond
                     ((char= ch *sp-frame-xend*)
                      *sp-frame-end*)
                     ((char= ch *sp-frame-xescape*)
                      *sp-frame-escape*)
                     (t ch)))))
          (incf (fill-pointer (sp-packet self)))
          (return ch)))))


;;;;Output methods

(defop (serial-proto-stream :tyo) (ch)
  (unless (sp-closed self)
    (let((stream (sp-stream self)))
      (cond
        ((char= ch *sp-frame-escape*)
         (funcall stream :tyo *sp-frame-escape*)
         (funcall stream :tyo *sp-frame-xescape*))
        ((char= ch *sp-frame-end*)
         (funcall stream :tyo *sp-frame-escape*)
         (funcall stream :tyo *sp-frame-xend*))
        (t
         (funcall stream :tyo ch))))))

;;;This doesn't follow the packet protocol:

(defop (serial-proto-stream :string-out) (&rest args)
  (apply (sp-stream self) :string-out args))

;;;This does follow the packet protocol:

(defop (serial-proto-stream :send-packet) (new-packet)
  (unless (sp-closed self)
    (unwind-protect
        (dotimes(i (fill-pointer new-packet))
          (send self :tyo (aref new-packet i)))
      (send self :finish-output))))

(defop (serial-proto-stream :finish-output) ()
  (let((stream (sp-stream self)))
    (unless (sp-closed self)
      (funcall stream :tyo *sp-frame-end*)
      ;;For the Lambda's sake we do :FORCE-OUTPUT to the serial stream when
      ;;we're done outputting a packet.  This yields a minor improvement over
      ;;the default, where characters are forced out by :TYO and :STRING-OUT.
      ;;
      ;;You better set the fill-pointer to 0 yourself!!!
      (funcall stream :force-output))))


;;;External interfaces

;;;Input:
;;;
;;;This routine packs characters into a PACKET if they are available.
;;;Initially the packet should have its FILL-POINTER == 0.  This probably
;;;must be called several times before the packet will be filled. By
;;;default we always returns whenever a :TYI-WITH-TIMEOUT returns NIL,
;;;meaning there's no input after TYI-TIMEOUT.  This behaviour means more
;;;passes through, but nobody need ever hang waiting for input, and no
;;;required time-out computation.  This can be changed by setting the
;;;:tyi-method on the stream.
;;;
;;;The TYI-TIMEOUT variable of the SERIAL-PROTO-STREAM is, on the Lambda,
;;;expressed as a number of clock intervals (1=1/60th of a second).
;;;
;;;We return non-NIL eventually when the packet is full or the
;;;end-of-packet character is received.  After using the packet, the caller
;;;is responsible for resetting the fill-pointer to 0 before it can be used
;;;again.
;;;

(defun serial-proto-receive(stream)
  (do((result (funcall stream :tyi)
              (funcall stream :tyi)))
     ((null result)
      (send stream :done-input-p))))

;;;Output:
;;;
;;;Send PACKET immediately on STREAM.
;;;

(defun serial-proto-send(stream packet)
  (funcall stream :send-packet packet))


#|
;;;Test code

(defvar port)
(defvar str)

(defun init()
  (setq str (make-serial-proto-stream :type :hardwired))
  (send str :open)
  (send str :new-packet)
  (setq port (sp-stream str)))

(defun echo ()
  (do ((ch (send str :tyi) (send str :tyi)))
      ((null ch))
    (format t "~c" ch) (sleep 1)))

(defun show()
  (do ((st (serial-proto-receive str)  (serial-proto-receive str)))
      (st (print 'done))
      (format t "~%~a" (sp-packet str))))

|#
