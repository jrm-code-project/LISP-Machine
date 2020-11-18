;;; -*- Mode:LISP; Package:KERMIT; Base:8; Readtable:T -*-

;;Copyright LISP Machine, Inc. 1984, 1985 ,1986
;;   See filename "Copyright" for
;;licensing and release information.
;;

;;; KERMIT in LISP by Mark David at LMI






;;; this implementation is based on and closely resembles
;;; kermit for unix, written in c by columbia university.






;;; this file encodes the basic protocol for sending
;;; and receiving files to/from any other kermit.
;;; the two highest level functions, which are not
;;; however user functions, that are in this file are:
;;; SENDSW -- the send state table switcher and dispatcher
;;; RECSW -- the receive state table switcher and dispatcher







;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; SOME POINTERS TO "KERMIT-" OTHER FILES:
;;;________________________________________
;;;



;;; @@@ main: toplevel
;;;=
;;; main routine - parse command and options, set up the tty lines,
;;; and dispatch to the appropriate routine.
;;;=
;;; ;; (...
;;;      ;; (make-serial-stream ...)
;;;      ;;... )
;;;
;;;
;;; FOR THE MAIN TOPLEVEL INTERFACE ROUTINES SEE THE FILE:
;;;     "sys: kermit; window" and "sys: kermit; calls"
;;;
;;;  the window file runs the window interface to kermit. upon
;;;  selection of a routine on the command menu, a call is made
;;;  to a top level function defined in calls.  the calls
;;;  file contains the top level calls as methods of the flavor
;;;  kstate. a kstate instance has special instance variables
;;;  corresponding to most of the specials declared here.
;;;
;;;  there is a special variable called KSTATE bound to the current
;;;  instance of kstate. Thus (funcall 'kstate ':send-files) is the
;;;  form called when you mouse "Send" on the kermit command menu.


;;;  thus, you must change the instance variables of a kstate
;;;  flavor instance to affect the binding of the specials during
;;;  execution of its methods.
;;;
;;;  thus "reinitializing" is just evaluating the form
;;;     (setq kstate (make-instance 'kstate))



;;;



;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; @@@ CONNECT
;;;
;;; connect with another kermit over an assigned tty line.
;;; some degree of terminal emulation is attempted.
;;;
;;; FOR THE Connect FUNCTION:
;;;
;;; SEE THE FILE:  "sys:kermit; terminal"



;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; @@@ SERVER (talk to one)
;;;
;;; Defined in calls, basically.




;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; @@@ SERVER (be one)
;;;
;;; a login server interface is in "SYS: KERMIT; PS-TERMINAL"
;;; a KERMIT server is coded in "SYS: KERMIT; SERVER"







;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;;; @@@ OPENNING FILE STUFF
;;;
;;; FOR THE OPENNING FILE STUFF, SEE THE FILE:
;;;
;;;     "sys:kermit;open.lisp"
;;;
;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;basic KERMIT protocol:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(DECLARE (SPECIAL INTERACTION-PANE STATUS-PANE))


;;; @@@ SYMBOL DEFINITIONS ["'constants'"]

;;; actually should be initialized by making an instance of kstate (in the file
;;; calls) which calls the functions herein with these (mostly) as special
;;; instance variables.




(DEFCONST *MAXPACKSIZ* #O136 "maximum packet size")


(DEFCONST *SOH* 1 "start of header")


(DEFCONST *CR* #O15 "ascii carriage return")


(DEFCONST *DEL* #O177 "ascii delete (rubout)")


(DEFCONST *ESCCHR* #\NETWORK "default escape char for Connect")



(DEFCONST *MAXTRY* #O12 "times to retry a packet")



(DEFCONST *MYQUOTE* #\# "Quote character I want to use to quote /"control characters/"")



(DEFCONST *MYPAD* 0 "number of padding characters I require")



(DEFCONST *MYPCHAR* 0 "char I will use as a padding char")



(DEFCONST *MYEOL* #O15 "my kind of return char")        ; LM's 215, which won't fit in 7 bits...?



(DEFCONST *MYTIME* #O12 "seconds after which i should be timed out")



(DEFCONST *MAXTIM* #O74 "maximum timeout interval in seconds")



(DEFCONST *MINTIM* 2 "minimum timeout interval in seconds")




(DEFCONST *CHECKSUM-TYPE* 1 "1 for one character checksum, 2 for 2-character. 3 not available.")




(DEFCONST *TRUE* -1 "-1 = boolean constant true")




(DEFCONST *FALSE* 0 "0 = boolean constant false")





;;; @@@global variables


;;; integers:






(DEFVAR *RPSIZ*  0 "maximum receive packet size")



(DEFVAR *SPSIZ*  0 "maximum send packet size")



(DEFVAR *PAD* 0 "how much padding to send")



(DEFVAR *TIMINT* 0 "timeout for foreign host on sends")








(DEFVAR *REMOTE* NIL "t means we're a remote kermit")



(DEFVAR *FILECOUNT* 0 "number of files left to send")




(DEFVAR *SIZE* 0 "size of present data")




(DEFVAR *PACKET-NUMBER* 0 "the packet number")



(DEFVAR *NUMTRY* 0 "times this packet retried")



(DEFVAR *OLDTRY* 0 "times previous packet retried")





;;; @@@ CHARACTERS:

(DEFVAR *QUOTE* 0 "quote character in incoming data")



(DEFVAR *STATE* 0 "present state of the machine")



(DEFVAR *PADCHAR* 0  "padding character to send")



(DEFVAR *EOL* #O15 "end-of-line character to send")



(DEFVAR *ESCCHR* 0 "quote character in incoming data")



(DEFVAR *EOF* 0 "character marking end of file")



;;; other data types:


(DEFVAR BUFEMP-IGNORE-LINE-FEED NIL
  "Initially nil for each file, this tells bufemp whether
  to ignore the line feed or not at this point in the
  file over an entire file transfer.")





(DEFVAR *FILNAMCNV* ':GENERIC
  ":GENERIC means do filename conversions to generic standards.
        ...others will be here some day...")





(DEFVAR DATA-XFER-START-TIME :UNBOUND "Start time of this xfer")

(DEFVAR *BYTECOUNT* :UNBOUND "Bytes sent during this xfer, roughly")

(DEFVAR *FILE-CLOSING-DISPOSITION* ':ABORT "How to handle partially finished files, delete
        or just close?")


(DEFVAR *SUCCESSFUL-TRANSACTIONS* ()
  "a list of lists:(<true-pathname> <time> <:send or :receive>")


(defvar current-file-props-list ()
  "Holds a list of properties to put on the currently transfered file if non-nil")
;; especially useful, since no one has implemented attributes protocol yet.




(defvar ascii-extra-safe-char)

(defvar ascii-extra-safe-filter?)


(defvar *8-bit-lispm* t
  ;; see bufill, bufemp for the affect of this flag
  "Mode with fullest translation of lispm into//from ascii.
 -formatting chars (like RETURN) are stripped of their 8th bit,
 which makes them look like their corresponding ascii values.
 -greek chars (like lambda) and #o177 are quoted by DEL (ascii #o177).")



(DEFVAR *IMAGE* NIL "t means 8-bit mode, no ascii//lispm translations")





(DEFVAR *DEBUG* NIL "t means supply debugging info as you run")





(DEFVAR *FILELIST* () "list of files to be sent")


(DEFVAR *FILNAM* NIL "current file name")

(defvar *as-filnam* nil "If non-nil, a string naming a filename to receive/send AS")


(defvar *string-array-buffer*
        (make-array (* 2 *maxpacksiz*) ':type ':art-string
                    ':fill-pointer 0)
  "Used as buffer for outgoing packets by spack")




(defvar *recpkt*
        (make-array *maxpacksiz*
                    ':type ':art-string
                    ':fill-pointer 0)
  "receive packet buffer")





(defvar *packet*
        (make-array *maxpacksiz*
                    ':type 'art-string
                    ':fill-pointer 0)
  "packet buffer")





(defvar *ttyfd* nil "file descriptor of tty for i/o, 0 if remote")




(defvar *fp*  nil "file pointer for current disk file")




(defconst abort-transfer-flag nil "set true when current transfer is being aborted.")


(defvar *kermit-beginning-time* nil "the universal time beginning of the current session.")

(defvar *packcount-wraparound* 0
  "The number of times the packet count has /"wrapped
        around/", i.e. went like ..., 98, 99, 0, 1,...). Updated by bump-packet-count.")



(defun update-status-label (filename sending?)
  (and (boundp 'status-pane)
       status-pane
       (send status-pane ':set-cursorpos
             0 (- (send status-pane ':line-height) 2)
             ':character)
       (format status-pane "~a ~a"
               (if sending? "Sending:" "Receiving:")
               (fs:parse-pathname filename))))



;;; this draws state and packet info in the little
;;; status pane in the upper left hand corner of
;;; the kermit frame. only reports numtry, if its higher
;;; than zero.


;;; some day make this a method of the status pane.
;;; and let the status pane keep field info to only erase
;;; changed parts.


(defvar bps 0. "bytes per second this transfer")

(defun give-state-info (state n ntries)

  (send status-pane ':home-cursor)
  (send status-pane ':clear-eol)

  (format status-pane ": ~14A  :  ~3D  :  ~D"
          (selectq state
            (#\D "Data")
            (#\S "Send Init")
            (#\R "Receive Init")
            (#\F "File Header")
            (#\A "Abort")
            (#\Z "Eof")
            (#\B "Eot")
            (#\C "Complete")
            (0 "Unknown")
            (t "unknown"))
          (+ (* #o100 *packcount-wraparound*) n)
          ntries)

  (and *bytecount*                              ;if so, assume data-xfer-start-time is ok too.
       (let ((old-bps bps))
         (cond ((< (floor old-bps)
                   (setq bps
                         (// *bytecount*
                             (// (time-difference (time) data-xfer-start-time)
                                 #.(small-float 60.))))
                   (ceiling old-bps)))
               (t (terpri status-pane)
                  (send status-pane ':clear-eol)
                  (format status-pane "~%Bytes Per Second: ~D" (fix bps))))))
  )

;;; @@@ MACRO DEFINITIONS




;;; @@@ WARN-USER

(defmacro warn-user (format-string . format-args)
  `(cond (*debug*
          (format interaction-pane "~% Warning: ")
          (format interaction-pane ,format-string . ,format-args))))








;;; @@@ TOCHAR, UNCHAR, CTL
;;; Tochar: converts a control character to a printable one by adding a space.
;;;
;;; Unchar: undoes tochar.
;;;
;;; Ctl:    converts between control characters and printable characters by
;;;         toggling the control bit (i.e. ^a becomes a and a becomes ^a).



(DEFSUBST TOCHAR (CH)
  (+ CH #\SPACE))






(DEFSUBST UNCHAR (CH)
  (- CH #\SPACE))






(DEFSUBST CTL (CH)
  (LOGXOR CH #o100))








;;; Syncf decrements the value of num
;;; by 1 or sets it back to 77 when it reaches
;;; 0.

(DEFMACRO SYNCF (NUM)
  `(SETQ ,NUM (IF (< (DECF ,NUM) 0) 77 ,NUM)))







;;; Bump-packet-number adds one to *PACKET-NUMBER* modulo 100
;;; (octal). *packet-number* is the global packet count, which
;;; must be agreed upon by the two interacting KERMITs.

;;; *packet-wraparound* is maintained for the sake of
;;; statistics hacks, so total packet count can be kept.

(defsubst bump-packet-number ()
  (cond ((not (< (setq *packet-number* (1+ *packet-number*)) #o100))
         (incf *packcount-wraparound*)
         (setq *packet-number* 0))))







(defsubst compute-final-checksum (chksum)
  (logand (+ (ash (logand chksum #o0300) -6) chksum) #o077))





;;;; 2-char-checksum
;;; This will be in the code soon as an optional feature.
;;; Its not considered necessary or advisable to use this,
;;; but that's up to the user. Here's what: we allocate one
;;; cons. Its car is the fixnum representing the 1st char
;;; and its cdr is the fixnum representing the 2nd char.
;;; This check sum is based on the low order 12 bits of the
;;; checksum. The first character is bits 6-11, and the second
;;; character is bits 0-5.



(DEFCONST 2-CHAR-CHECKSUM-CONS '(0 . 0) "The cons whose car and cdr hold a 2 char cksum")

(DEFUN COMPUTE-2-CHAR-CHECKSUM (CHECKSUM)
  (SETF (CAR 2-CHAR-CHECKSUM-CONS) (LSH (LOGAND CHECKSUM 7700) -6))
  (SETF (CDR 2-CHAR-CHECKSUM-CONS) (LOGAND CHECKSUM 77))
  2-CHAR-CHECKSUM-CONS)





;;; @@@ ACKP, NACKP, ERRP, FAILP

;;; predicate macros applied to the type of response from rpack:
;;; an ACK (Y), a NACK (N), an ERRORMESSAGE (E), or a failed
;;; packet transmission (type = *FALSE*).

;;; we use eq instead of = because its supposed to be
;;; faster on lisp machines (for fixnums).


(DEFMACRO ACKP (TYPE)
  `(EQ ,TYPE #\Y))





(DEFMACRO NACKP (TYPE)
  `(EQ ,TYPE #\N))





(DEFMACRO ERRP (TYPE)
  `(EQ ,TYPE #\E))





(DEFMACRO FAILP (TYPE)
  `(EQ ,TYPE 0))                                ;0 = BOOLEAN FALSE









;;; CONCERNING SENDING LISPM FILES TO ASCII COMPUTERS:

;;; from the greenual:
;;; "...In the currently implemented ASCII file servers, the following encoding is used.
;;; All printing characters and any characters not mentioned explicitly
;;; here are represented as themselves. Codes 010 (lambda), 011 (gamma)
;;; 012 (delta), 014 (plus-minus), 015 (circle-plus), 177 (integral),
;;; 200 through 207 inclusinve, 213 (delete), and 216 and anything
;;; higher are preceeded by a 177; that is, 177 is used as a "quoting
;;; character" for these codes. Codes 210 (overstrike) 211 (tab), 212
;;; (line), and 214 (page), are converted to their ascii cognates,
;;; namely 010 (backspace), 011 (horizontal tab), 012 (line feed), and
;;; 0145 (form feed) respectively. code 215 (return) is converted into
;;; 015 (carriage return) followed by 012 (line feed).
;;; Code 377 is ignored completely, and so cannot be stored in files."

;;; *** someday, think about using this, but note that, e.g. 11 [       ] would
;;; expand into ##     , a quadruple expansion! That's because 177 [] is
;;; ascii rubout, which must be control quoted by kermit.









(COMMENT
(DEFUN ASCII-TO-LISPM (CH)
  ;; note! it is not presently the case that CH = (LISPM-TO-ASCII (ASCII-TO-LISPM CH)) FOR
  ;; ANY CH. Not too good. This is not actually used right now.!!!
  "Converts ascii to lispm as well as possible, which sometimes
        means returning >8bit numbers, in which case we usually punt with ()."
  (IF (EQ CH 177)
      #\RUBOUT
    (IF (MEMQ CH '(#O10 #O11 #O12 #O14 #O15))
        (+ CH #O200)
      (IF (< CH #\SPACE)
          NIL
        CH))))
)







(COMMENT
(DEFUN LISPM-TO-ASCII (CH)
  "May return nil in case of high bit numbers; also, in case of
        greek characters {,    ,
, , } [10, 11, 12, 14, and 15
        octal], it will return nil, since these are the translations
        for PAGE, TAB, RETURN, OVERSTRIKE, and LINEFEED, and so would
        cause conflict. This may change but we have to devise a better
        lispm-ascii translation convention than in greenual, p. 134"
  (SELECTQ CH
    (#\RUBOUT #O177)
    ((#\PAGE #\TAB #\RETURN #\OVERSTRIKE #\LINEFEED) (- CH #O200))
    ((#O10 #O11 #O12 #O14 #O15) NIL)
    (OTHERWISE (COND ((> CH #O177) NIL)
                      (T CH)))))
)







;;; @@@  BUFEMP

(defun bufemp (buffer len)
  "Put data from an incoming packet into a local disk file."
  (let ((temp-outbuf *string-array-buffer*))
    (loop initially (setf (fill-pointer temp-outbuf) 0)
          with i fixnum
          until (>= i len)
          as ch fixnum = (aref buffer i)

          doing

          (cond ((eq ch *myquote*)
                 (setq ch (aref buffer (setq i (1+ i))))
                 (unless (eq  (logand ch 177) *myquote*)
                   (setq ch (ctl ch)))))
          (cond (*image*
                 (array-push temp-outbuf ch))
                (*8-bit-lispm*
                   (cond ((eq ch #o177)         ;lispm quoted
                          (setq ch (aref buffer (setq i (+ 2 i))))
                          (unless (eq ch #o177) (setq ch (ctl ch))))    ;get one after
                         ((memq ch '(#o10 #o11 #o14))
                          (setq ch (+ ch 200)))
                         ((eq ch #o12) (setq ch (cond (bufemp-ignore-line-feed
                                                       (setq bufemp-ignore-line-feed nil))
                                                      (t #\return))))
                         ((eq ch #o15) (setq bufemp-ignore-line-feed t ch #\return)))
                   (and ch (array-push temp-outbuf ch)))

                (t (cond ((setq ch (selectq (setq ch (logand ch 177))
                                     (#o10 #\overstrike)
                                     (#o11 #\tab)
                                     (#o12 (cond (bufemp-ignore-line-feed
                                                  (setq bufemp-ignore-line-feed nil))
                                                 (t #\return)))
                                     (#o14 #\page)
                                     (#o15 (setq bufemp-ignore-line-feed t)
                                           #\return)
                                     (#o177 #\delete)
                                     (otherwise ch)))
                          (array-push temp-outbuf ch)))))
          (incf i)
          finally (send *fp* ':string-out temp-outbuf)))
  buffer)
















;;; @@@ bufill

;;;; bufill (buffer)
;;; There are four ways to fill a buffer:
;;; 1. kermit default: 7-bit, quote all control characters, map newlines
;;; and tabs and any other funny characters into ascii.
;;;
;;; 2. lisp machine default: (*8-bit-lispm*)
;;;   as described in Chineual and honoring kermit as well by quoting
;;;   control characters.
;;; 3. *image*
;;;   send everything thru with no conversion, except for quoting the
;;;   quote character.
;;; 4. ascii-extra-safe-filter?
;;;   like 1. but filter out any characters less than #\space
;;;   which are commonly used on lisp machines (such as greek characters
;;;   and less-than-or-equal-sign.) The value should be a filtering
;;;   function that looks for wierd characters. [Note: it should not
;;;   need filter out formatting chars like RETURN and TAB. These
;;;   are already handled.
;;;
;;; Right now these may interfere in wierd ways. Fix this up alot.
;;; To do: repeat count prefixing!
;;;  optional huffman encoding?


(defun bufill (buffer)
  "fill buffer with the outgoing data from the file *FP* points to.
   only control quoting is done; 8-bit and
    repeat count prefixes are not handled."
  (let ((fullsize (- *spsiz* 6)))
    (loop initially (setf (fill-pointer buffer) 0)
          until  (>= (fill-pointer buffer) fullsize)
          for c fixnum = (send *fp* ':tyi nil)
          when (null c) do (loop-finish)

          doing
          (cond ((not (and (>= c #\sp) (< c #o177)))
                 (cond (ascii-extra-safe-filter?
                        (setq c
                              (funcall ascii-extra-safe-filter? c))))))
          (cond ((and (>= c #\sp) (< c #o177)
                      (not (eq c *quote*)))     ;regular character
                 (array-push buffer c))
                ((eq c *quote*)                 ;control quote character
                 (array-push buffer *quote*)
                 (array-push buffer *quote*))
                ((not *image*)                  ;do lispm -> ascii mapping if not image mode.
                 (cond ((eq c #o215)                    ;carriage return
                        (array-push buffer *quote*)
                        (array-push buffer (ctl #o15))
                        (array-push buffer *quote*)
                        (array-push buffer (ctl #o12)))
                       ((memq c '(#\overstrike #\tab #\line #\page))    ;lispm control characters
                        (setq c (logand c #o177))
                        (array-push buffer *quote*)
                        (array-push buffer (ctl c)))
                       ((and *8-bit-lispm*
                             (or (memq c '(#\delete #o177))
                                 (> c #o177)))
                        (array-push buffer *quote*)
                        (array-push buffer #o177)
                        (if (eq c #o177) (array-push buffer *quote*))
                        (array-push buffer c))
                       ((or (memq c '(#o10 #o11 #o12 #o14 #o15))
                            (>= c 177))         ;losing lispm characters
                        (cond (*8-bit-lispm*
                               (cond ((< c #o177)
                                      (array-push buffer *quote*)
                                      (array-push buffer #o177)
                                      (array-push buffer *quote*)
                                      (array-push buffer (ctl c)))
                                     (t (array-push buffer *quote*)
                                        (array-push buffer #o177)
                                        (array-push buffer c))))

                              (t (warn-user     ;wierd char don't send anything for it.
                                   "~&The character ~C [~O octal] could not~A"
                                   c c " be translated to ASCII."))))
                       (t (array-push buffer *quote*)   ;normal case to *quote*
                          (array-push buffer c))))
                (t (array-push buffer *quote*)
                   (array-push buffer c)))
          finally
          (return (cond ((zerop (fill-pointer buffer))
                         *eof*)
                        (t (fill-pointer buffer)))))))

(defun bufill (buffer)
  "fill buffer with the outgoing data from the file *FP* points to.
   only control quoting is done; 8-bit and
    repeat count prefixes are not handled."
  (let ((fullsize (- *spsiz* 6)))
    (loop with index = 0
          until  (>= index fullsize)
          for c fixnum = (send *fp* ':tyi nil)
          doing
          (cond ((null c) (loop-finish)))
          (cond ((not (and (>= c #\sp) (< c #o177)))
                 (cond (ascii-extra-safe-filter?
                        (setq c (funcall ascii-extra-safe-filter? c))))))
          (cond ((and (>= c #\sp) (< c #o177)
                      (not (eq c *quote*)))     ;regular character
                 (setf (aref buffer index) c)
                 (incf index))
                ((eq c *quote*)                 ;control quote character
                 (setf (aref buffer index) *quote*)
                 (incf index)
                 (setf (aref buffer index) *quote*)
                 (incf index))
                ((not *image*)                  ;do lispm -> ascii mapping if not image mode.
                 (cond ((eq c 215)                      ;carriage return
                        (setf (aref buffer index) *quote*)
                        (incf index)
                        (setf (aref buffer index) (ctl #o12))
                        (incf index)
                        (setf (aref buffer index) *quote*)
                        (incf index)
                        (setf (aref buffer index) (ctl #o15))
                        (incf index))
                       ((memq c '(#\overstrike #\tab #\line #\page))    ;lispm control characters
                        (setq c (logand c #o177))
                        (setf (aref buffer index) *quote*)
                        (incf index)
                        (setf (aref buffer index) (ctl c))
                        (incf index))
                       ((and *8-bit-lispm* (>= c #o177))
                        (setf (aref buffer index) *quote*)
                        (incf index)
                        (setf (aref buffer index) #o177)
                        (incf index)
                        (cond ((eq c #o177)
                               (setf (aref buffer index) *quote*)
                               (incf index)))
                        (setf (aref buffer index) c)
                        (incf index))
                       ((or (memq c '(#o10 #o11 #o12 #o14 #o15))
                            (>= c 177))         ;losing lispm characters
                        (cond (*8-bit-lispm*
                               (cond ((< c #o177)
                                      (setf (aref buffer index) *quote*)
                                      (incf index)
                                      (setf (aref buffer index) #o177)
                                      (incf index)
                                      (setf (aref buffer index) *quote*)
                                      (incf index)
                                      (setf (aref buffer index) (ctl c))
                                      (incf index))
                                     (t (setf (aref buffer index) *quote*)
                                        (incf index)
                                        (setf (aref buffer index) #o177)
                                        (incf index)
                                        (setf (aref buffer index) c)
                                        (incf index))))

                              (t (warn-user     ;wierd char don't send anything for it.
                                   "~&The character ~C [~O octal] could not~A"
                                   c c " be translated to ASCII."))))
                       (t (setf (aref buffer index) *quote*)    ;normal case to *quote*
                          (incf index)
                          (setf (aref buffer index) c)
                          (incf index))))
                (t (setf (aref buffer index) *quote*)
                   (incf index)
                   (setf (aref buffer index) c)
                   (incf index)))

          finally
          (return (cond ((zerop index)
                         *eof*)
                        (t index))))))




(defselect (debugger-tell-user ignore)
  (:gnxtfl (filelist)
    (format t " ~&gnxtfl     next file is: ~A.~% k: ~D files remain"
            (car filelist) (1- (length filelist))))
  (:sendsw ()
    (format t "~2%sendsw     state:    ~C      ~% " *state*))
  (:recsw  ()
    (format t "~2%recsw      state:    ~C      ~% " *state*))
  (:rpack  (type num len data) data
    (format t "~&rpack     TYPE>>~3C    NUM>>~3D    LEN>>~3D"
            type num len))
  (:spack  (type num len data) data
    (format t "~&spack     TYPE>>~3C    NUM>>~3D    LEN>>~3D"
            type num len))
  (:spack-line (string)
    (format t "~&send-packet>> ~S" string)))


;;; @@@ PRERRPKT
;;;  Print error packet to the local user that came from the remote
;;; KERMIT in an E packet.

(DEFUN PRERRPKT (MSG)
  "print contents of error packet received from remote host."
  (FORMAT INTERACTION-PANE
          "~&KERMIT aborting with following error from remote host:~%   ~S~%"
          MSG))



















(DEFUN FLUSHINPUT ()
  (SEND *TTYFD* ':CLEAR-INPUT))





(DEFUN ERROR-MESSAGE (FORMAT-STRING &REST FORMAT-ARGS)
  ;;; THIS WILL DO FOR NOW...
  (APPLY #'FORMAT `(,INTERACTION-PANE ,FORMAT-STRING . ,FORMAT-ARGS)))












;;; toplevel sender/receiver:











;;;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;;;; @@@ SENDSW
;;;
;;;
;;; This is the state table switcher for sending files to
;;; a another KERMIT.

;;; now checks out terminal-io steam for ABORT character:
;; the abort character is CONTROL-Z.



(DEFUN SENDSW (&OPTIONAL (*STATE* #\S) (*PACKET-NUMBER* 0) (*NUMTRY* 0))
  ;; calls: sinit, sfile, sdata, seof, sbreak
  "sendsw is the state table switcher for sending files. it loops until
   either it finishes, or an error is encountered. the routines called
   by sendsw are responsible for changing the state."
  (UNWIND-PROTECT
      (LET (;(*STATE* #\S)                      ;INIT STATE,PACKET-NUMBER,NUMTRY
            ;(*PACKET-NUMBER* 0)
            ;(*NUMTRY* 0)
            (*PACKCOUNT-WRAPAROUND* 0)
            (ABORT-TRANSFER-FLAG NIL)
            (*bytecount* (if (eq *state* #\S) nil 0))
            (bps 0.))
        (LOOP                                   ; DO AS LONG AS NECESSARY
          WHEN *DEBUG* DO
          (DEBUGGER-TELL-USER ':SENDSW)
          DO
          (OR *REMOTE* (GIVE-STATE-INFO *STATE* *PACKET-NUMBER* *NUMTRY*))
          (COND ((EQ (SEND TERMINAL-IO ':TYI-NO-HANG) #\CONTROL-Z)
                 (COND ((MEMQ *STATE* '(#\D #\F #\Z))
                        (SETQ *STATE* #\Z ABORT-TRANSFER-FLAG T))
                       (T (SETQ *STATE* #\A)))
                 (FORMAT INTERACTION-PANE "...~C~%Aborting file transfer!" #\CONTROL-Z)))
          (SELECTQ *STATE*
            (#\S (SETQ *STATE* (SINIT)))        ; S SEND INIT
            (#\F (SETQ *STATE* (SFILE)))        ; F SEND FILE HEADER
            (#\D (SETQ *STATE* (SDATA)))        ; D SEND DATA
            (#\Z (SETQ *STATE* (SEOF)))         ; Z SEND EOF - CTRL Z
            (#\B (SETQ *STATE* (SBREAK)))       ; B SEND BREAK (EOT)
            (#\C (RETURN *TRUE*))               ; C DONE COMPLETE
            (#\A (RETURN *FALSE*))              ; A DONE ABORT
            (OTHERWISE (RETURN *FALSE*)))))     ; T DONE FAIL
    (COND (*FP* (SEND *FP* ':CLOSE)
                (SETQ *FP* NIL)))               ;MAKE SURE NO FILES ARE HANGING OPEN
    ))
















;;; @@@ SINIT
;;;
;;; the fields of send initiate:
;;; 0. maxl 1. time 2. npad 3. padc 4. eol
;;; 5. qctl 6. qbin 7. chkt 8. rept 9. capas ...
;;;
;;; but we only concern ourselves with eol and quote
;;; at this point



(DEFUN SINIT ()
  "send initiate: send this host's parameters and get other side's back."
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (SETQ *PACKET* (SPAR *PACKET*))
           (FLUSHINPUT)
           (SPACK #\S *PACKET-NUMBER* 6 *PACKET*)
           (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE RECPKT) (RPACK)
             (COND ((NACKP REPLY) *STATE*)
                   ((ACKP REPLY)
                    (COND ((NOT (= *PACKET-NUMBER* NUM))
                           *STATE*)
                          (T (SETQ *EOL* 0 *QUOTE* 0) ;INITIALIZE QUESTIONABLE PARAMS.
                             (RPAR RECPKT)            ;CHECK AND SET DEFAULTS
                             (AND (ZEROP *EOL*) (SETQ *EOL* *MYEOL*))
                             (AND (ZEROP *QUOTE*) (SETQ *QUOTE* *MYQUOTE*))
                             (SETQ *NUMTRY* 0)
                             (BUMP-PACKET-NUMBER)
                             #\F)))
                   ((ERRP REPLY) (PRERRPKT RECPKT) #\A)
                   ((FAILP REPLY) *STATE*)
                   (T #\A))))))


(DEFUN SFILE ()
  "open file, then send file header to other kermit, see if its accepted.
Then get first buffer full of data from the file if its ok to send."
  (PROG (NEW-FILE-NAME NEWLENGTH)

    (COND ((> *NUMTRY* *MAXTRY*) (RETURN #/A))
          (T (INCF *NUMTRY*)
                                                ;this will try to send an error message
                                                ;packet if trouble openning now.  -mhd
             (UNLESS *FP*
               (AND *DEBUG* (DEBUGGER-TELL-USER ':SFILE *FILNAM*))
               (COND ((NOT (SETQ *FP* (OPEN-FILE-IN-OR-NOT *FILNAM*)))
                      (SPACK #\E *PACKET-NUMBER* 45
                             "kermit-q: Error in sending file header")
                      (ERROR-MESSAGE "~&Cannot open file ~A ~%" *FILNAM*)
                      (RETURN *FALSE*))))

                                                ;ok, got a file open; let's rip!
                                                ;first do file name conversions

             (SETQ NEW-FILE-NAME (or (prog1 *as-filnam*
                                            (setq *as-filnam* nil))
                                     (STRING-FOR-KERMIT *FILNAM*)))
             (SETQ NEWLENGTH (STRING-LENGTH NEW-FILE-NAME))
             (FORMAT INTERACTION-PANE "~& K: Sending ~A as ~A" *FILNAM* NEW-FILE-NAME)
             (OR *REMOTE* (UPDATE-STATUS-LABEL *FILNAM* T))

                                                ;now send file header to other kermit

             (SPACK #\F *PACKET-NUMBER* NEWLENGTH NEW-FILE-NAME)

                                                ;what was the reply?

             (RETURN (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE RECPKT) (RPACK)
                       (COND
                         ((NACKP REPLY)
                          (SETQ NUM (IF (< (DECF NUM) 0) 63 NUM))
                          (COND ((NOT (= NUM *PACKET-NUMBER*))
                                 *STATE*)
                                (T #\A)))

                         ((ACKP REPLY)
                          (COND ((NOT (= NUM *PACKET-NUMBER*)) *STATE*)
                                (T (SETQ *NUMTRY* 0)
                                   (BUMP-PACKET-NUMBER)
                                   (SETQ DATA-XFER-START-TIME (TIME))   ;for status check/display
                                   (SETQ *SIZE* (BUFILL *PACKET*))
                                   (SETQ *BYTECOUNT* *SIZE*)    ;for status check/display info
                                   #\D)))

                         ((ERRP REPLY) (PRERRPKT RECPKT) #\A)

                         ((FAILP REPLY) *STATE*)

                         (T #\A))))))))



;;; @@@ sdata


(DEFUN SDATA ()
  "send file data."
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (SPACK #\D *PACKET-NUMBER* *SIZE* *PACKET*)
           (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE RECPKT) (RPACK)
             (COND ((NACKP REPLY)
                    (SYNCF NUM)
                    (COND ((NOT (= NUM *PACKET-NUMBER*)) *STATE*)
                          (T #\A)))
                   ((ACKP REPLY)
                    (COND ((NOT (= *PACKET-NUMBER* NUM)) *STATE*)
                          (T (SETQ *NUMTRY* 0)
                             (BUMP-PACKET-NUMBER)
                             (IF (= (SETQ *SIZE* (BUFILL *PACKET*)) *EOF*)
                                 #\Z
                               (PROG1 #\D (SETQ *BYTECOUNT* (+ *BYTECOUNT* *SIZE*)))))))
                   ((ERRP REPLY) (PRERRPKT RECPKT) #\A)
                   ((FAILP REPLY) *STATE*)
                   (T #\A))))))






;;; @@@ SEOF

(DEFUN SEOF ()
  "send end-of-file"
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (COND (ABORT-TRANSFER-FLAG
                  (SPACK #\Z *PACKET-NUMBER* 1 "D"))    ;send a Discard if abortp
                 (T (SPACK #\Z *PACKET-NUMBER* 0 NIL)))
           (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE DATA) (RPACK)
             (COND ((NACKP REPLY)
                    (SYNCF NUM)
                    (COND ((NOT (= NUM *PACKET-NUMBER*)) *STATE*)
                          (T #\A)))
                   ((ACKP REPLY)
                    (COND ((NOT (= NUM *PACKET-NUMBER*))
                           *STATE*)
                          (T (SETQ *NUMTRY* 0)
                             (BUMP-PACKET-NUMBER)
                             (FORMAT INTERACTION-PANE
                                     "~%File sent successfully: ~A~%"
                                     (SEND *FP* ':TRUENAME))
                             (PUSH (LIST (SEND *FP* ':TRUENAME) (TIME) ':SEND)
                                   *SUCCESSFUL-TRANSACTIONS*)
                             (AND *DEBUG* (DEBUGGER-TELL-USER ':SEOF-CLOSE *FILNAM*))   ;
                             (SEND *FP* ':CLOSE)
                             (SETQ *FP* NIL)
                             (AND *DEBUG* (DEBUGGER-TELL-USER ':SEOF-LOOKING))
                             (COND ((NOT (GNXTFL)) #\B) ;exercise in obscurity;
                                                        ;hint: see file open.lisp
                                   (T (AND *DEBUG* (DEBUGGER-TELL-USER ':SEOF-FOUND *FILNAM*))
                                      #\F)))))
                   ((ERRP REPLY) (PRERRPKT DATA) #\A)
                   ((FAILP REPLY) *STATE*)
                   (T #\A))))))







;;; @@@ SBREAK

(DEFUN SBREAK ()
  "send break (eot)."
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (SPACK #\B *PACKET-NUMBER* 0 NIL)
           (MULTIPLE-VALUE-BIND (REPLY NUM IGNORE RECPKT) (RPACK)
             (COND ((NACKP REPLY)
                    (SYNCF NUM)
                    (COND ((NOT (= *PACKET-NUMBER* NUM))
                           *STATE*)
                          (T #\A)))
                   ((ACKP REPLY)
                    (COND ((NOT (= *PACKET-NUMBER* NUM))
                           *STATE*)
                          (T (SETQ *NUMTRY* 0)
                             (BUMP-PACKET-NUMBER)
                             #\C)))
                   ((ERRP REPLY) (PRERRPKT RECPKT) #\A)
                   ((FAILP REPLY) *STATE*)
                   (T #\A))))))











;;; @@@ RECSW
;;;
;;; This is the state table switcher and main dispatcher for
;;; receiving a file from another KERMIT.

;;; add abort key: CONTROL-Z

(DEFUN RECSW (&OPTIONAL (*STATE* #\R)(*PACKET-NUMBER* 0)(*NUMTRY* 0))
  ;;; use these functions: rinit, rfile, rdata.
  "this is the state table switcher for receiving files."
  (UNWIND-PROTECT
      (LET (;(*STATE* #\R)                      ; INIT STATE, PACKET-NUMBER, NUMTRY
            ;(*PACKET-NUMBER* 0)
            ;(*NUMTRY* 0)
            (BUFEMP-IGNORE-LINE-FEED NIL)       ; KLUDGE, SO CR/LF TRANSLATES RIGHT.
            (*PACKCOUNT-WRAPAROUND* 0)
            (*bytecount* (if (eq *state* #\R) nil 0))
            (bps 0.))
        (LOOP                                   ; DO AS LONG AS NECESSARY
          WHEN *DEBUG* DO
          (DEBUGGER-TELL-USER ':RECSW)
          DO

          (OR *REMOTE* (GIVE-STATE-INFO *STATE* *PACKET-NUMBER* *NUMTRY*))
          (COND ((EQ (SEND TERMINAL-IO ':TYI-NO-HANG) #\CONTROL-Z)
                 (SETQ *STATE* #\A)
                 (FORMAT INTERACTION-PANE "...~C~% Aborting file transfer." #\CONTROL-Z)))
          (SELECTQ *STATE*
            (#\R (SETQ *STATE* (RINIT)))        ;  R RECEIVE INIT
            (#\F (SETQ *STATE* (RFILE)))        ;  F RECEIVE FILE HEADER
            (#\D (SETQ *STATE* (RDATA)))        ;  D RECEIVE DATA
            (#\C (RETURN *TRUE*))               ;  C DONE, COMPLETE
            (#\A (RETURN *FALSE*))              ;  A ABORT
            (OTHERWISE (RETURN *FALSE*)))));  T DONE FAIL

    (COND (*FP* (SEND *FP* ':CLOSE *FILE-CLOSING-DISPOSITION*)
                (SETQ *FP* NIL))))              ; MAKE SURE NO FILES ARE HANGING OPEN
  )











;;; @@@ RINIT

(DEFUN RINIT ()
  "receive initialization."
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (MULTIPLE-VALUE-BIND (TYPE NUM LEN DATA) (RPACK)
             NUM LEN; COMPILER
             (COND ((= TYPE #\S)
                    (RPAR DATA)
                    (SETQ DATA (SPAR DATA))
                    (SPACK #\Y *PACKET-NUMBER* 6 DATA)
                    (SETQ *OLDTRY* *NUMTRY*)
                    (SETQ *NUMTRY* 0)
                    (BUMP-PACKET-NUMBER)
                    #\F)
                   ((ERRP TYPE) (PRERRPKT DATA) #\A)
                   ((FAILP TYPE)
                    (WARN-USER "sinit failed: sending a NAK.")
                    (SPACK #\N *PACKET-NUMBER* 0 NIL)
                    *STATE*)
                   (T #\A))))))







;;; @@@ RFILE



(DEFUN RFILE (&AUX OURFILENAME)
  (COND ((> *NUMTRY* *MAXTRY*) #/A)
        (T (INCF *NUMTRY*)
           (MULTIPLE-VALUE-BIND (TYPE NUM LEN PACKET) (RPACK)
             LEN                                ; COMPILER
             (COND ((= TYPE #\S)
                    ;;; SEND INIT
                    (COND ((> *OLDTRY* *MAXTRY*) #\A)
                          (T (INCF *OLDTRY*)
                             (COND ((= NUM (IF (= *PACKET-NUMBER* 0) #o77 (1- *PACKET-NUMBER*)))
                                    (SETQ PACKET (SPAR PACKET))
                                    (SPACK #\Y NUM 6 PACKET)
                                    (SETQ *NUMTRY* 0)
                                    *STATE*)
                                   (T #\A)))))
                   ((= TYPE #\Z)
                    ;;; END OF FILE
                    (COND ((> *OLDTRY* *MAXTRY*) #\A)
                          (T (INCF *OLDTRY*)
                             (COND ((= NUM (IF (= *PACKET-NUMBER* 0) #o77 (1- *PACKET-NUMBER*)))
                                    (SPACK #\Y NUM 0 NIL)
                                    (SETQ *NUMTRY* 0)
                                    *STATE*)
                                   (T #\A)))))
                   ((= TYPE #\F)
                    ;;; FILE HEADER
                    (COND ((NOT (= NUM *PACKET-NUMBER*))
                           #\A)
                          (T (SETQ OURFILENAME (or (prog1 *as-filnam*
                                                          (setq *as-filnam* nil))
                                                   (STRING-FOR-KERMIT-OUTFILE PACKET)))
                             (COND ((SETQ *FP* (OPEN-FILE-OUT-OR-NOT OURFILENAME))
                                    (FORMAT INTERACTION-PANE "~&Receiving ~A as ~A"
                                            PACKET
                                            OURFILENAME)
                                    (OR *REMOTE* (UPDATE-STATUS-LABEL OURFILENAME NIL))
                                    (SPACK #\Y *PACKET-NUMBER* 0 NIL)
                                    (SETQ *OLDTRY* *NUMTRY*)
                                    (SETQ *NUMTRY* 0)
                                    (BUMP-PACKET-NUMBER)
                                    (SETQ DATA-XFER-START-TIME (TIME)
                                          *BYTECOUNT* 0)
                                    #\D)
                                   (T (FORMAT INTERACTION-PANE "~&Cannot create ~S" PACKET)
                                                ;experimental error packet sending--mhd
                                      (SPACK #\E *PACKET-NUMBER* 45     ;
                                             "kermit-q: Error in receiving file header.")
                                      #\A)))))
                   ((= TYPE #\B)
                    ;;; END OF TRANSMISSION
                    (COND ((NOT (= NUM *PACKET-NUMBER*)) #\A)
                          (T (SPACK #\Y *PACKET-NUMBER* 0 NIL) #\C)))
                   ((ERRP TYPE)
                    ;;; ERROR
                    (PRERRPKT PACKET)
                    #\A)
                   ((FAILP TYPE)
                    ;;; FAILURE
                    (SPACK #\N *PACKET-NUMBER* 0 NIL)
                    *STATE*)
                   (T #\A))))))





;;; @@@ rdata

(DEFUN RDATA ()
  (COND ((> *NUMTRY* *MAXTRY*) #\A)
        (T (INCF *NUMTRY*)
           (MULTIPLE-VALUE-BIND (TYPE NUM LEN DATA) (RPACK)
             (COND ((= TYPE #\D)
                    (COND ((NOT (= NUM *PACKET-NUMBER*))

                           (COND ((> *OLDTRY* *MAXTRY*) #\A)
                                 (T (INCF *OLDTRY*)
                                    (COND
                                      ((= NUM (IF (= *PACKET-NUMBER* 0)
                                                  77 (1- *PACKET-NUMBER*)))
                                       (SPACK #\Y NUM 6 DATA)
                                       (SETQ *NUMTRY* 0)
                                       *STATE*)
                                      (T #\A)))))
                          (T  ;; OK, GOT DATA WITH RIGHT PACKET NUMBER.

                           (BUFEMP DATA LEN)
                           (SETQ *BYTECOUNT* (+ LEN *BYTECOUNT*))
                           (SPACK #\Y *PACKET-NUMBER* 0 NIL)
                           (SETQ *OLDTRY* *NUMTRY*)
                           (SETQ *NUMTRY* 0)
                           (BUMP-PACKET-NUMBER)
                           #\D )))
                   ((= TYPE #\F)
                    (COND ((> *OLDTRY* *MAXTRY*) #\A)
                          (T (INCF *OLDTRY*)
                             (COND ((= NUM (IF (= *PACKET-NUMBER* 0)
                                               77 (1- *PACKET-NUMBER*)))
                                    (SPACK #\Y NUM 0 NIL)
                                    (SETQ *NUMTRY* 0)
                                    *STATE*)
                                   (T #\A)))))
                   ((= TYPE #\Z)
                    (COND ((NOT (= NUM *PACKET-NUMBER*))
                           #\A)
                          (T (SPACK #\Y *PACKET-NUMBER* 0 NIL)
                             (FORMAT INTERACTION-PANE
                                     "~% File received successfully: ~A~%"
                                     (SEND *FP* ':TRUENAME))
                             (and current-file-props-list       ;temp hacks
                                  (lexpr-funcall #'fs:change-file-properties
                                                 (send *fp* ':truename)
                                                  'error-yes current-file-props-list))

                             (PUSH (LIST (SEND *FP* ':TRUENAME) (TIME) ':RECEIVE)
                                   *SUCCESSFUL-TRANSACTIONS*)
                             (SEND *FP* ':CLOSE)
                             (BUMP-PACKET-NUMBER)
                             #\F)))
                   ((ERRP TYPE)
                    (PRERRPKT DATA)
                    #/A)
                   ((FAILP TYPE)
                    (SPACK #\N *PACKET-NUMBER* 0 NIL)
                    *STATE*)
                   (T #\A))))))










;;; @@@ SPACK
  ;; TYPE -- a number, the type of packet this is.
  ;; NUM  -- a number, the the packet-number of this packet.
  ;; LEN  -- a number, the length of the packet.
  ;; DATA -- a string, i.e. an art-string type of array, the data of this pkt.

(defun spack (type num len data)
  "send a packet..."

  (let ((chksum 0)
        (buffer *string-array-buffer*)
        (index 0)
        (temp nil))

    (and *debug* (debugger-tell-user ':spack type num len data))

    ;; issue any padding:
    (loop for i from 0 below *pad*
          do
           (setf (aref buffer index) *padchar*)
           (incf index))

    ;; issue packet marker (ascii 1, soh):
    (setf (aref buffer index) *soh*)
    (incf index)

    ;; issue char count & update checksum:
    (setf (aref buffer index) (setq temp (tochar (+ len 3))))
    (incf index)
    (incf chksum temp)

    ;; issue packet-number & update checksum:
    (setf (aref buffer index) (setq temp (tochar num)))
    (incf index)
    (incf chksum temp)

    ;; issue the packet type & update checksum:
    (setf (aref buffer index) type)
    (incf index)
    (incf chksum type)

    ;; issue all the data & update checksum (as we go):
    (and data (loop for i from 0 below len
                    as ch = (aref data i)
                    doing
                     (setf (aref buffer index) ch)
                     (incf index)
                     (incf chksum ch)))

    ;; compute & issue the final checksum:
    (setf (aref buffer index) (tochar (compute-final-checksum chksum)))
    (incf index)

    ;; issue an extra-packet line terminator:
    (setf (aref buffer index) *eol*)
    (incf index)
    (setf (fill-pointer buffer) index)

    ;;; packet is alive and well and living in buffer;
    ;;; so release it now:
    (and *debug* (debugger-tell-user ':spack-line buffer))
    (send *ttyfd* ':string-out buffer 0 index)
    nil))












;;; @@@ RPACK
    ;;; values returned are in order:
    ;;; TYPE, NUM, LEN, DATA
    ;;; type -- a character (fixnum), in {#\A, #\S, ...}, for ex., which means "abort".
    ;;; num  -- a number, the packet-number of this packet.
    ;;; len  -- a number, the number of characters in this packet.
    ;;; data -- a string, the data of this packet, which is as many
    ;;;         characters as appropriate/desired for this type of packet.
;;;  many callers need only one (usually the type) value.

(defun rpack ()
  "receive other kermit's packet, which should be a string
of xxxnxxx to xxxn+mxxx characters. each character means.."

  ;; Still need: 2-char checksum handling to be added in other parts.

  (prog (ch type rchksum len num stage
         (data *recpkt*)
         (max-time-interval
           (* 60. (if (< *timint* *mintim*) *mintim* *timint*)))
         (cchksum 0)
         (tyi-function
           (cond ((memq ':tyi-with-timeout (send *ttyfd* ':which-operations))
                  #'(lambda (stream max-time)
                      (send stream ':tyi-with-timeout max-time)))
                 (t #'(lambda (stream max-time)
                        (cond
                          ((or (send stream ':listen)
                               (progn
                                 (process-wait-with-timeout
                                   "tyi"
                                   max-time
                                   #'(lambda (stream) (send stream ':listen))
                                   stream)
                                 (send stream ':listen)))
                           (send stream ':tyi))))))))
     continue
     (loop for ch = (funcall tyi-function *ttyfd* max-time-interval)
           if (not ch) do (and (setq stage 'soh) (go timeout))
           until (= (logand ch 0177) *soh*))    ; WAIT FOR SOH.

     (setq ch (funcall tyi-function *ttyfd* max-time-interval))
     (if (not ch) (and (setq stage 'len) (go timeout)))
     (if (not *image*) (setq ch (logand ch 0177)))
     (if (= ch *soh*) (go continue))
     (setq cchksum ch)                          ;OK, START CHECKSUM
     (setq len (- (unchar ch) 3))               ;GET CHARACTER COUNT

     (cond ((or (< len 0) (> len (- *maxpacksiz* 3)))
            (go fatal-error)))                  ;bad error, happens alot, when other kermit
                                                ;is at command level instead of waiting for
                                                ;packets.

     (setq ch (funcall tyi-function *ttyfd* max-time-interval))
     (if (not ch) (and (setq stage 'num) (go timeout)))
     (if (not *image*) (setq ch (logand ch 0177)))
     (if (= ch *soh*) (go continue))
     (incf cchksum ch)                          ;OK, UPDATE CHECKSUM
     (setq num (unchar ch))                     ;GET PACKET NUMBER


     (setq ch (funcall tyi-function *ttyfd* max-time-interval))
     (if (not ch) (and (setq stage 'type) (go timeout)))
     (if  (not *image*) (setq ch (logand ch 0177)))
     (if (= ch *soh*) (go continue))
     (incf cchksum ch)                          ;OK, UPDATE CHECKSUM
     (setq type ch)                             ;GET PACKET TYPE

     (loop for i from 0 below len
           doing (progn (setq ch (funcall tyi-function *ttyfd* max-time-interval))
                        (if (not ch) (and (setq stage 'data) (go timeout)))
                        (if (not *image*) (setq ch (logand ch 0177)))
                        (if (= ch *soh*) (go continue))
                        (incf cchksum ch)       ;OK, UPDATE CHECKSUM
                        (setf (aref data i) ch))        ;GET DATA CHARACTER

           finally (progn (setf (aref data len) 0)      ;MARK THE END OF THE DATA
                          (setf (fill-pointer data) len)))

     (setq ch (funcall tyi-function *ttyfd* max-time-interval))
     (if (not ch) (and (setq stage 'rchksum) (go timeout)))
     (setq rchksum (unchar ch))                 ;OK, GET LAST CHARACTER (CHECKSUM)
     (cond  ((eq *checksum-type* 2)
             (setq ch (funcall tyi-function *ttyfd* max-time-interval))
             (if (not ch) (and (setq stage 'rchksum) (go timeout)))
             (setq rchksum (cons rchksum (unchar ch)))) ;ok, make a two ch checksum maybe.
            ((eq *checksum-type* 3) (ferror nil "Only 1 or 2 character checksums are supported."))
            ((not (memq *checksum-type* '(1 2)))
             (ferror nil "Bad value for *checksum-type*: ~A is not a legal type;~
                          ~%value can only be 1 or 2."
                     *checksum-type*)))

     (setq ch (funcall tyi-function *ttyfd* max-time-interval))
     (if (not ch) (and (setq stage 'eol) (go timeout)))
     (if (not *image*) (setq ch (logand ch 0177)))
     (if (= ch *soh*) (go continue))            ;OK, GET EOL CHAR AND TOSS IT
                                                ;SAFE!
     (and *debug* (debugger-tell-user ':rpack type num len data))

     (setq cchksum (selectq *checksum-type*
                     (1 (compute-final-checksum cchksum))
                     (2 (compute-2-char-checksum cchksum))))

     (if (not (equal cchksum rchksum))
         (progn (warn-user "RPACK received bad checksum [~A//~A]"
                           rchksum cchksum)
                ;; corruption, oh no!
                (return (values *false* num len data)))
       ;; else checksum ok, 'uncorrupted'.
       (return (values type num len data)))
   timeout
     (warn-user "RPACK timed out waiting for ~A character." stage)
     (return *false*)
   fatal-error
     ;; should send error packet, when that can be done.
     (warn-user "RPACK received illegal packet length spec: ~D" len)
     (return *false*)))










;;; @@@ spar packet

(DEFUN SPAR (PACKET)
  "Fill the data array with my send-init parameters."
  (SETF (FILL-POINTER PACKET) 6)
  (SETF (AREF PACKET 0) (TOCHAR *MAXPACKSIZ*))
  (SETF (AREF PACKET 1) (TOCHAR *MYTIME*))
  (SETF (AREF PACKET 2) (TOCHAR *MYPAD*))
  (SETF (AREF PACKET 3) (CTL *MYPCHAR*))
  (SETF (AREF PACKET 4) (TOCHAR *MYEOL*))
  (SETF (AREF PACKET 5) *MYQUOTE*)
  PACKET)








;;; @@@ rpar

(DEFUN RPAR (DATA)
  "Get the other hosts send-init parameters."
  (SETF (FILL-POINTER DATA) 6)
  (SETQ *SPSIZ* (UNCHAR (AREF DATA 0)))
  (SETQ *TIMINT* (UNCHAR (AREF DATA 1)))
  (SETQ *PAD* (UNCHAR (AREF DATA 2)))
  (SETQ *PADCHAR* (CTL (AREF DATA 3)))
  (SETQ *EOL* (UNCHAR (AREF DATA 4)))
  (SETQ *QUOTE* (AREF DATA 5))
  DATA)
