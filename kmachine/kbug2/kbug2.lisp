;;; -*- Mode:LISP; Package:K-KBUG; Compile-In-Roots:(K-GLOBAL); Base:10; Readtable:CL -*-

;;; Definitions in "COMMON-DEFINITIONS"

(defvar *falcon-error-clobbered* nil)

(defun kbug2 (&optional starting-address)
  (let ((*package* (find-package "K-KBUG")))
    (when starting-address
      (kbug-write-pc starting-address))
    (when (not (kbug-stopped?))
      (k-mem-write (ash k2:kbug-status-addr 2) k2:kbug-status-busy)
      (k-mem-write (ash k2:kbug-command-addr 2) k2:kbug-command-continue)
      (format t "~%K is running - waiting for it to stop~%")
      (kbug-wait-until-stopped))
    (deinstall-breakpoints)
    (do ()
	(())
      (kbug-wait-until-stopped)
      (kbug2-print-instruction)
      (catch 'kbug-cmd-error
	(let ((input (peek-char)))
	  (kbug-stop)
	  (case input
	    ((#\space #\c-n)        (read-char) (kbug-step 1 nil))
	    (#\c-p                  (read-char) (kbug-proceed)
				                (kbug-wait-until-stopped-and-no-pause)
						(deinstall-breakpoints))
	    (#\c-f                  (read-char) (show-user-frame))
	    (#\c-m-f                (read-char) (show-user-frame-with-datatypes))
	    (#\c-z                  (read-char) (kbug-stop))
  	    (#\q                    (read-char) (return-from kbug2))
	    ((#\c-l #\clear-screen) (read-char) (zl::send *terminal-io* :clear-window))
	    (#\m-f                  (read-char) (kbug2-flush-call-stack))
	    (#\m-l                  (read-char) (kbug2-run-listener))
	    (#\m-s                  (read-char) (show-call-stack))
	    (#\m-e                  (read-char) (format t "~%") (show-error))
	    ;; $$$ Added meta-r as in KBUG. <08-Nov-88 wkf>
	    (#\m-r		    (read-char) (global:do-forever
						  (cond ((listen *terminal-io*) (return t))
							;; $$$ Added halt check. <16-Nov-88 wkf>
							((k-halted-p)
							 (format t "~%The falcon has halted.")
							 (return t))
							(t (kbug-step 1 nil)
							   (kbug2-print-instruction)))))
	    (#\help                 (read-char) (kbug2-debugger-help))
	    (otherwise              (kbug-evaluator "Return to KBUG2."))))))))

(defun kbug2-debugger-help ()
  (format t "~%You are typing at the K debugger MARK-II. (KBUG2)
 to exit.
SPACE, C-N to single-step.
C-P        to proceed K, remaning in debugger.
C-F        to view register-frames.
C-M-F      to view register-frames with datatypes and values.
C-L        to clear-screen.
C-Z        to stop machine.
M-S        to display the K call stack
M-F        to flush the K call stack
M-L        to run mini-lisp-listener on the K
M-E        to display an error message
M-R        to auto-single-step the falcon until user types at keyboard
HELP       to get this message
Q          to quit/return from KBUG2
Otherwise  to type a form to the evaluator"))	;

(defun kbug2-print-instruction ()
  (let* ((addr (logand #xFFFFFF (kbug-read-pc)))
	 (sym-addr (get-warm-symbolic-address addr)))
    (if sym-addr
	(format t "~&~30a  " sym-addr)
      (format t "~&~6,'0x               " addr))
    (print-instruction (kbug-read-inst addr) addr)))

(defun kbug-evaluator (abort-string)
  (zl:catch-error-restart ((sys:abort) abort-string)
    (multiple-value-bind (sexp flag)
	(zl:with-input-editing (*terminal-io* '((:full-rubout :full-rubout)
						(:activation char= #\end)
						(:prompt prompt-for-kbug)))
	  (read))
      (unless (eq flag :full-rubout)
	(terpri)
	(prin1 (prog1 (eval sexp) (terpri)))))))

(defmacro define-k-function-invoker (name k-function &rest body)
  "Define a function named NAME which executes the BODY forms, then
causes K-FUNCTION to be run until it completes or the K stops running 
for some reason (like a breakpoint)."
  `(DEFUN ,name ()
     ,@body
     (KBUG-GOTO ',k-function)
     (SLEEP 0.1)
     (KBUG-PROCEED)
     (KBUG-WAIT-UNTIL-STOPPED-AND-NO-PAUSE)
     (DEINSTALL-BREAKPOINTS)))

(define-k-function-invoker kbug2-flush-call-stack li::flush-call-stack
  (unless (y-or-n-p "~&Flush the call stack and streams? ")
    (return-from kbug2-flush-call-stack NIL))
  (format t "~&Flushing the K call stack and streams...")
  (flush-streams))

;(define-k-function-invoker kbug2-run-rep li::rep
;  (format t "~&Running read-eval-print on the K..."))

(define-k-function-invoker kbug2-run-listener li::listener
  (cond ((null *falcon-error-clobbered*)
	 (format t "~&Warning!  Running mini-lisp-listener smashes LI:ERROR on the K.")
	 (format t "~&Are you sure you want to run it? ")
	 (unless (y-or-n-p) (return-from kbug2-run-listener NIL))))
  (setq *falcon-error-clobbered* t)
  (format t "~&Running mini-lisp-listener on the K..."))

(defun prompt-for-kbug (stream ignore)
  (format stream "~&Eval> "))

(defun kbug-set-single-step-flag ()
  (lam:k-mem-write (ash k2:kbug-flag-addr 2)
		   (dpb 1 k2:%%kbug-trace-flag (lam:k-mem-read (ash k2:kbug-flag-addr 2)))))

(defun kbug-clear-single-step-flag ()
  (lam:k-mem-write (ash k2:kbug-flag-addr 2)
		   (dpb 0 k2:%%kbug-trace-flag (lam:k-mem-read (ash k2:kbug-flag-addr 2)))))

(defmacro define-kbug-register (name location)
  `(DEFUN ,name ()
     (LAM:K-MEM-READ (ASH ,location 2))))

(defun kbug-status ()
  (logand #xffffff (lam:k-mem-read (ash k2:kbug-status-addr 2))))

(defun kbug-command ()
  (logand #xffffff (lam:k-mem-read (ash k2:kbug-command-addr 2))))

(defun kbug-read-pc ()
  (logand #xffffff (lam:k-mem-read (ash k2:kbug-pc-addr 2))))

(defun kbug-write-pc (pc)
  (lam:k-mem-write (ash k2:kbug-pc-addr 2) pc))

(defun kbug-left ()
  (lam:k-mem-read (ash k2:kbug-left-addr 2)))

(defun kbug-right ()
  (lam:k-mem-read (ash k2:kbug-right-addr 2)))

(defun kbug-left-boxed ()
  (lam:k-mem-read (ash k2:kbug-left-boxed-addr 2)))

(defun kbug-right-boxed ()
  (lam:k-mem-read (ash k2:kbug-right-boxed-addr 2)))

(defun kbug-read-inst (pc)
  (kbug-cmd k2:kbug-command-read-memory (logior #x02000000 (ash pc 1)) 2)
  (logior (kbug-data 0) (ash (kbug-data 1) 32.)))

;start PC space address.
(defun kbug-disassemble (start how-much &optional (stream t) print-bits)
  (dotimes (i how-much)
    (let* ((adr (+ i start))
	   (inst (kbug-generic-read-inst-via-processor adr)))  ;uses the physical hardware to do virtual-to-physical translation.
      (format stream "~%~6x ~x" adr		 ;  map must be set up to win.
	      (print-instruction inst adr nil))	; $$$ Made use common routine. <08-Nov-88 wkf>
      (if print-bits (lam:print-bits inst)))))

(defun kbug-dis (function-symbol &optional (stream t) print-bits)
  (multiple-value-bind (start length)
      (get-address function-symbol)
    (if (null length) (setq length #x100))
    (when start
      (kbug-disassemble start (logand #o77777777 length) stream print-bits))))	;vinc:%%pointer

(defun kbug-disassemble-function (symbol &optional (stream t) print-bits)
  (kbug-dis symbol stream print-bits))

(defun kbug-read-vma ()
  (lam:k-mem-read (ash k2:kbug-vma-addr 2)))

(defun show-memory-status ()
  (let ((memstat (lam:k-mem-read (ash k2:kbug-mstat-addr 2))))
    (format t "~%Memory board has ~[double-~;single-~]sided ram sips.~
               ~%Autoboot jumper      ~[off~;on~]~
               ~%Parity error         ~[on~;off~]~
               ~[~:;~%*Undefined bits are on*~]~
               ~%Transport trap       ~[armed~;unarmed~]~
               ~%Read fault           ~[armed~;unarmed~]~
               ~%VMA boxed bit        ~[boxed~;unboxed~]~
               ~%MD boxed bit         ~[boxed~;unboxed~]~
               ~%Last cycle type      ~[write~*~
               ~%GC trap              ~[enabled~;disabled~]~;~
                                             read~
               ~%Transport type       ~[write~;visible-evcp~;transport~;no-transport~]~*~]~
               ~%MD written lately    ~[yes~;no~]~
               ~%Nubus bootstrap mode ~[RESET~;~Short reset~:;*undefined*~]~
               ~%ECO jumpers          #b~4,'0b~
               ~%Nubus slot           #x~1x"
	    (ldb hw:%%memory-status-16meg                   memstat)
	    (ldb hw:%%memory-status-autoboot-jumper-bit	    memstat)
	    (ldb hw:%%memory-status-parity-error	    memstat)
	    (ldb hw:%%memory-status-spare-19-20		    memstat)
	    (ldb hw:%%memory-status-read-md-will-trans-trap memstat)
	    (ldb hw:%%memory-status-read-md-will-fault	    memstat)
	    (ldb hw:%%memory-status-vma-not-boxed-bit       memstat)
	    (ldb hw:%%memory-status-md-not-boxed-bit        memstat)
	    (ldb hw:%%memory-status-cycle-type              memstat)
	    (ldb hw:%%memory-status-transport-type          memstat)
	    (ldb hw:%%memory-status-gc-trap-enable          memstat)
	    (ldb hw:%%memory-status-md-written-lately       memstat)
	    (ldb hw:%%memory-status-nubus-bootstrap-mode    (lognot memstat))
	    (ldb hw:%%memory-status-eco-jumper-number       memstat)
	    (ldb hw:%%memory-status-nubus-slot-id           memstat))))


(define-kbug-register k2:kbug-read-vma-boxed k2:kbug-vma-boxed-addr)
(define-kbug-register k2:kbug-read-md-boxed  k2:kbug-md-boxed-addr)

(defun kbug-read-md ()
  (lam:k-mem-read (ash k2:kbug-md-addr 2)))

(defun kbug-stopped? ()
  (if (= (kbug-command) k2:kbug-command-idle)
      t nil))
  
(defvar *k-pausing* nil)	;if T, K is momentarily stopped by a process that will restart it.
				; the main K-BUG2 loop should not intervene.

(defun kbug-wait-until-stopped-and-no-pause ()
  (global:do-forever
    (zl:process-wait "KBUG2" (let ((stream *terminal-io*))
			       (make-fancy-wait-function
				 #'(lambda () (and (null *k-pausing*)
						   (listen stream)))
				 #'(lambda ()
				     (and (null *k-pausing*)
					  (or (kbug-stopped?)
					      (k-halted-p)))))))
    (if (null *k-pausing*) (return t)))
  (if (k-halted-p)
      (progn (format t "~%HALTED:  ") (why)
	     (k-stop)
	     (format t "~%*** Entering KBUG ***")
	     (kbug))
    (kbug-stop)))

(defun kbug-wait-until-stopped ()
  (zl:process-wait "KBUG2" (let ((stream *terminal-io*))
			       (make-fancy-wait-function
				 #'(lambda () (listen stream))
				 #'(lambda ()
				     (or (kbug-stopped?)
					 (k-halted-p))))))
  (if (k-halted-p)
      (progn (format t "~%HALTED:  ") (why)
	     (k-stop)
	     (format t "~%*** Falcon stopped unexpectedly; will enter KBUG ***")
	     (kbug))				; $$$ Removed history saving, now done in KBUG. <08-Nov-88 wkf>
    (kbug-stop)))

(defun kbug-stop ()
  (when (or
	  (= (kbug-status) k2:kbug-status-busy))
;	  (not (= (kbug-command) k2:kbug-command-idle)))
    (lam::k-write-int 7 1))
  (if (= (kbug-status) k2:kbug-status-busy)
    (format t "~%Sorry, it won't stop!")
    (kbug-read-pc)))

(defun kbug-force-stop ()
  (lam::k-write-int 7 1))

(defun kbug-cmd-raw (cmd &rest parms)
  "Sends a KBUG2 command with its arguments.  Caller is responsible for checking completion and status."
  (do* ((count 0 (1+ count))
	(params parms (cdr params))
	(parm (car params) (car params)))
       ((null params))
    (lam:k-mem-write (ash (+ k2:kbug-parameter-addr count) 2) parm))
  (lam:k-mem-write (ash k2:kbug-command-addr 2) cmd))

(defun kbug-cmd (cmd &rest parms)
  (do* ((count 0 (1+ count))
	(params parms (cdr params))
	(parm (car params) (car params)))
       ((null params))
    (lam:k-mem-write (ash (+ k2:kbug-parameter-addr count) 2) parm))
  (lam:k-mem-write (ash k2:kbug-command-addr 2) cmd)
  (dotimes (i 1500.)
    (let ((command (kbug-command))
	  (status  (kbug-status)))
      (when (= command k2:kbug-command-idle)
	(if (not (= status k2:kbug-status-done))
	    (format t "~%**** Bad command status ~D ******~%" status))
	(return-from kbug-cmd status))))
  (return-from kbug-cmd -1))

(defun kbug-cmd-confirm (cmd &rest params)
  (let ((status (apply #'kbug-cmd cmd params)))
    (cond ((= -1 status)
	   (Format t "~%No wedge response to ~x command" cmd)
	   (throw 'kbug-cmd-error nil))
	  (t status))))
      
(defun kbug-cmd-safe (cmd &rest params)
  (if (= -1 (apply #'kbug-cmd cmd params))
      -1
    (throw 'kbug-cmd-error nil)))

(defun kbug-data (index)
  "Read data transfer area.  Index is a word offset in the area."
  (lam:k-mem-read (ash (+ k2:kbug-data-transfer-area-addr index) 2)))	;NuBus is byte addressed

(defun kbug-set-data (index data)
  "Write to data transfer area.  Index is a word offset in the area."
  (lam:k-mem-write (ash (+ k2:kbug-data-transfer-area-addr index) 2) data))

;(defun kbug-get-comm-word (location)
;  "Read from anywhere in the KBUG communication area.
;LOCATION is the index into the area of the word to be read."
;  (if (<= 0 location (1- #x400))
;      (lam:k-mem-read (ash (+ location k2:kbug-base-addr)
;			   2))
;    nil))

;(defun kbug-set-comm-word (location value)
;  "write to anywhere in the KBUG communication.
;LOCATION is the index into the area of the wrod to write VALUE to."
;  (if (<= 0 location (1- #x400))
;      (lam:k-mem-write (ash (+ location k2:kbug-base-addr)
;			    2)
;		       value)
;    nil))

; User-frame-OA
;  User-frame-R
;   Multiple-value-return-frame-1
;    Multiple-value-return-frame-2
;     Kbug-trap-handler-1 (trap-save-frame)
;      Kbug-trap-handler-2
;       Kbug-command-handler (In OAR now)

(defconstant depth-to-user-OA-frame           5) ;user open & active
(defconstant depth-to-user-R-frame            4) ;user R frame in OA slots
(defconstant depth-to-handler-trap-save-frame 1) 
(defconstant depth-to-handler-2-frame         0)


;;; An interesting hack.
;;; Like hell it is.  This is the ugliest piece of SHIT I can remember encountering,
;;; and I'm going to forget it as fast as I can.  Unfortunately, the people who did
;;; this will never find out my opinion of it.  No way should this have *EVER* been
;;; put in production code of any sort.  --RWK

(defconstant running-format   "~{~@?~}")
(defconstant combining-format "~{~a~}")
(defconstant format-argument  "~@?")
(defconstant boxed-32 "~:[ ~;*~]~8,'0x")
(defconstant hex-2    "~2,'0x")
(defconstant newline "~&")

(defun new-format (stream &rest args)
  (format stream running-format args))

(defun combine-format (&rest args)
  (format nil combining-format args))

(defconstant labeled-hex-2
	     (combine-format
	       format-argument hex-2 format-argument))

(defconstant labeled-boxed-32
	     (combine-format
	       format-argument boxed-32))

;(defun foo (a b)
;  (new-format t
;	      labeled-hex-2 "Foo " a
;	      labeled-hex-2 "Bar " b
;	      newline
;	      "Here is some text."
;	      labeled-boxed-32 "Test " t 2374298.
;	      newline))

;;; Using lisp.

;(defun labeled-hex-2 (label num)
;  (format:ostring label)
;  (hex-2 num))

;(defun hex-2 (num)
;  (format:onum num 16. 2 :pad-char #\0))

;(defun newline ()
;  (terpri))

;(defun labeled-boxed-32 (label boxed-p num)
;  (format:ostring label)
;  (boxed-32 boxed-p num))

;(defun boxed-32 (boxed-p num)
;  (format:ostring
;    (if boxed-p
;	" "
;	"*"))
;  (format:onum num 16. 8. :pad-char #\0))

;(defun foo (a b)
;  (format:output t
;    (labeled-hex-2 "Foo " a)
;    (labeled-hex-2 "Bar " b)
;    (newline)
;    (format t "Here is ~a some text." 'really)
;    (labeled-boxed-32 "Test " t 23742323.)
;    (newline)))

;;; Using your brains and good taste!
;(defun foo (a b)
;  (format t labeled-hex-2 "Foo " a)
;  (format t labeled-hex-2 "Bar " b
;  (fresh-line)
;  (write-string "Here is some text.")
;  (format t labeled-boxed-32 "Test " t 2374298.)
;  (fresh-line))


(defun read-A-frame-number ()
  (kbug-cmd-confirm k2:kbug-command-read-call-stack)
  (ldb (byte 8. 0.) (kbug-data (+ 2 (* 2 depth-to-user-OA-frame)))))

(defun read-A-frame ()
  (read-frame-as-list (read-A-frame-number)))

(defun read-A-reg (n)
  (declare (values (value boxed)))
  (let ((reg (nth n (read-A-frame))))
    (values (cdr reg) (car reg))))

(defun show-mem-word (word-address &optional (stream t))
  (let ((contents (lam:k-mem-read-word-address word-address)))
    (fresh-line)
    (show-data-type-and-object contents stream)))

;;; For tracking down NIL cloberages:

;(defun tell-nil (where)
;  (let ((nil-p t))
;    (labels ((show-word (word-address)
;			(let ((contents (lam:k-mem-read-word-address word-address)))
;			  (show-data-type-and-object contents *standard-output*)
;			  (unless (zerop contents)
;			    (setq nil-p nil)))))
;      (format *standard-output* "~&At ~A, and NIL is:~%  Car: ~vQ  CDR:  ~vQ~%"
;	      where
;	      0 #'show-word
;	      1 #'show-word)
;      (unless nil-p
;	(loop repeat 5 (tv:beep))
;	(cerror "Fix it" "NIL is broken.")
;	(fix-nil)))))

;;; If it ever gets bashed, you can fix it with this.
;;; "It" is NIL.
;;; Its lucky that real page 0 is virtual page 0.
;(defun fix-nil ()
;  (lam:k-mem-write-word-address 0 0)
;  (lam:k-mem-write-word-address 1 0)
;  (show-mem-word 0)
;  (show-mem-word 1))		

(defun show-user-frame (&optional (stream t))
  (kbug-cmd-confirm k2:kbug-command-read-call-stack)
  (let* ((user-open-frame
	   (ldb (byte 8. 8.) (kbug-data (+ 2 (* 2 depth-to-user-OA-frame)))))
	 (user-active-frame
	   (ldb (byte 8. 0.) (kbug-data (+ 2 (* 2 depth-to-user-OA-frame)))))
	 (user-return-frame
	   (ldb (byte 8. 0.) (kbug-data (+ 2 (* 2 depth-to-user-R-frame)))))
	 (O-list (read-frame-as-list user-open-frame))
	 (a-list (read-frame-as-list user-active-frame))
	 (r-list (read-frame-as-list user-return-frame))
	 (left (kbug-left))
	 (right (kbug-right))
	 (left-boxed (kbug-left-boxed))
	 (right-boxed (kbug-right-boxed)))
    (new-format stream
	    newline
	    labeled-boxed-32 "Left "  (ldb-test (byte 1. 0.) left-boxed) left
	    labeled-boxed-32 " Right " (ldb-test (byte 1. 0.) right-boxed) right
	    labeled-hex-2    " Open #x"   user-open-frame
	    labeled-hex-2    " Active #x" user-active-frame
	    labeled-hex-2    " Return #x" user-return-frame)
    (do* ((reg 0 (1+ reg))
	  (o o-list (cdr o))
	  (a a-list (cdr a))
	  (r r-list (cdr r)))
	 ((= reg 16.))
;      (format
;	t "~%  O~2D ~:[ ~;*~]~8,'0X    A~2D ~:[ ~;*~]~8,'0X    R~2D ~:[ ~;*~]~8,'0X"
;	reg (caar o) (cdar o) reg (caar a) (cdar a) reg (caar r) (cdar r))))
      (new-format stream
		  newline
		  "  O~2D" reg
		  boxed-32 (caar o) (cdar o)
		  "   A~2D" reg
		  boxed-32 (caar a) (cdar a)
		  "   R~2d" reg
		  boxed-32 (caar r) (cdar r))))
  (format stream "~%"))

(defun show-user-frame-with-datatypes (&optional (stream t))
  (kbug-cmd-confirm k2:kbug-command-read-call-stack)
  (let* ((user-open-frame          (ldb (byte 8. 8.) (kbug-data (+ 2 (* 2 depth-to-user-OA-frame)))))
	 (user-active-frame        (ldb (byte 8. 0.) (kbug-data (+ 2 (* 2 depth-to-user-OA-frame)))))
	 (user-return-frame        (ldb (byte 8. 0.) (kbug-data (+ 2 (* 2 depth-to-user-R-frame)))))
	 (o-list                   (read-frame-as-list user-open-frame))
	 (a-list                   (read-frame-as-list user-active-frame))
	 (r-list                   (read-frame-as-list user-return-frame))
	 (left                     (kbug-left))
	 (right                    (kbug-right))
	 (left-boxed               (kbug-left-boxed))
	 (right-boxed              (kbug-right-boxed))
	 (*show-object-max-depth*  10)
	 (*show-object-max-length* 20))  ;;These two used by SHOW-OBJECT
    (new-format stream
	    newline
	    labeled-boxed-32 "Left "  (ldb-test (byte 1. 0.) left-boxed) left
	    labeled-boxed-32 " Right " (ldb-test (byte 1. 0.) right-boxed) right
	    labeled-hex-2    " Open #x"   user-open-frame
	    labeled-hex-2    " Active #x" user-active-frame
	    labeled-hex-2    " Return #x" user-return-frame
	    newline)
    (dolist (list (if (= user-open-frame user-active-frame)
		      `(,(cons 'a o-list) ,(cons 'r r-list))
		    `(,(cons 'o o-list) ,(cons 'a a-list) ,(cons 'r r-list))))
      (do* ((reg 0 (1+ reg))
	    (type (car list))
	    (o (cdr list) (cdr o))
	    (pointer (cdar o) (cdar o)))
	   ((= reg 16.))
	(new-format stream
		    "~%  ~a~2D" type reg
		    boxed-32 (caar o) pointer " ")
	(when (caar o) (show-data-type-and-object pointer stream)))
      (format stream "~%"))))

 
(defun k-data-type-symbol (dt)
  (dolist (e vinc:k-data-type-names-alist dt)
    (cond ((= dt (cadr e))
	   (return (car e))))))
  
(defun read-frame-as-list (frame)
  (kbug-cmd-confirm k2:kbug-command-read-register-frame frame)
  (do* ((r 15. (1- r))
	(rlist nil))
       ((minusp r) rlist)
    (setq rlist (cons (box-reg-cons r) rlist))))

(defun kbug2-read-register (frame offset)
  (kbug-cmd-confirm k2:kbug-command-read-register-frame frame)
  (values (kbug-data offset)
	  (= 1 (ldb (byte 1 offset) (kbug-data 16.)))))

  
(defun write-frame-from-list (frame list)
  (kbug-cmd-confirm k2:kbug-command-read-register-frame frame)  ;read current contents.
  (do* ((r 0 (1+ r))
	(rlist list (cdr rlist)))
       ((null rlist))
    (let* ((cell (car rlist))
	   (boxed (car cell))
	   (data (cdr cell)))
      (kbug-set-data 16. (dpb (if boxed 1 0) (byte 1 r) (kbug-data 16.)))
      (kbug-set-data r data)))
  (kbug-cmd-confirm k2:kbug-command-write-register-frame frame)  ;write back update thing.
  t)	;truth!

(defun box-reg-cons (reg)
  (cons (= 1 (ldb (byte 1 reg) (kbug-data 16.)))
	(kbug-data reg)))

(defun read-frame-address (address)
  (let ((frame (ldb (byte 8 4) address))
	(reg (ldb (byte 4 0) address)))
    (kbug-cmd-confirm k2:kbug-command-read-register-frame frame)
    (logior (kbug-data reg) (ash (ldb (byte 1 reg) (kbug-data 16.)) 32.))))

(defun show-full-stack (&optional (name "ed-buffer:kbug-stack"))
  (with-open-file (stream name :direction :output)
    (format stream "____________________________________________________________________________________________________")
    (show-call-stack :stream stream)		; $$$ Show a summary first. <08-Nov-88 wkf>
    (show-call-stack :with-frames '(a o) :stream stream)))

(defun show-call-stack (&key with-frames (stream t))
  ;with frames should be a list or NIL.  useful things are 'A for active frame, 'O for open frame.
  (let ((k-halted-p (k-halted-p)))
    (format stream "~%CALL stack, ~s, machine is ~:[running~;halted~]" with-frames k-halted-p))
  (let ((cs (kbug-generic-read-call-stack-as-list)))
    (show-cs-list cs t :with-frames with-frames :stream stream)))

(defun show-cs-list (cs symbolic-addresses &key with-frames (stream t))
  (do* ((cse       (car cs)      next-cse)
	(cs-rest   cs            next-rest)
	(next-rest (cdr cs)      (cdr next-rest))
	(next-cse  (car cs-rest) (car cs-rest)))
       ((null cs-rest))
    (let* ((d0  (car  cse))
	   (d1  (cadr cse))
	   (rpc (ldb (byte 24. 0.) d0))
	   (rdf (ldb (byte 3. 28.) d0))
	   (rdr (ldb (byte 4. 24.) d0))
	   (o   (ldb (byte 8. 8.) d1))
	   (a   (ldb (byte 8. 0.) d1))
	   (sym-adr (if symbolic-addresses (kbug-symbolic-address rpc))))
      (format stream "~%  Ret-Dest ~[ O~; A~; R~; G~;NO~;NO~;NT~;NT~]~2,'0D   Open ~2,'0X   Active ~2,'0X" rdf rdr o a)
      ;; $$$ Added better check for non active frames. <15-Nov-88 wkf>
      (if (and (not (= o a)) next-cse (let ((next-active (ldb (byte 8 0) (cadr next-cse))))
					(= a next-active)))
	  (format stream "     rpc #x~6,'0X" rpc)
	(format stream "   rpc #x~6,'0X" rpc))
      (when sym-adr
	(cond ((not (consp sym-adr))
	       (format stream " ~x" sym-adr))
	      (t
	       (format stream " ~x #x~x" (car sym-adr) (cadr sym-adr)))))
      (cond ((global:memq 'a with-frames)
	     (format stream "~%Active frame ~X:" a)
	     (show-frame-with-datatypes a stream)))
      (cond ((and (global:memq 'o with-frames)
		  (or (not (global:memq 'a with-frames))
		      (not (= o a))))
	     (format stream "~%Open frame ~X:" o)
	     (show-frame-with-datatypes o stream))))))

;;; $$$ Removed obsolete version of SHOW-CALL-STACK. <08-Nov-88 wkf>

(defun read-call-stack-as-list ()
  (let ((ans nil))
    (kbug-cmd-confirm k2:kbug-command-read-call-stack)
    (let ((max (ash (logand #xff (kbug-data 0)) 1)))
      (do ((index 1 (+ 2 index)))
	  ((>= index max))
	(let* ((d0 (kbug-data index))
	       (d1 (kbug-data (1+ index))))
	  (setq ans (nconc ans (list (list d0 d1)))))))
    ans))

(defun kbg-read-call-stack-as-array ()
  "read the entire contents of the call stack into an array.  This uses the spy path."
  (let ((saved-pc (lam:k-read-spy-pc))
	(saved-hp-csp (lam:k-read-hp-sp))
	(saved-oar (lam:k-read-oar))
	(call-stack (make-array 256 :initial-element nil :element-type t)))
    (do ((i 0 (1+ i)))
	((>= i 256)
	 (lam:k-write-hp-csp saved-hp-csp)	;restore csp first.
	 (lam:k-write-oar saved-oar)
	 (lam:k-set-pc saved-pc)
	 call-stack)
      (k-write-hp-csp (dpb i (byte 8. 0.) 0.))
      (let ((retpc-rdest (k-read-retpc-rdest)))
						;return first.  The toplevel OAR goes with the PC,
						;which we tried to save above.
	(lam:k-execute lam:KIH-RETURN 0.)	;to cause OAR to restore from stack.
	(let ((oar (k-read-oar)))
	  (global:aset `((o ,(ldb (byte 8 16.) oar))
		  (a ,(ldb (byte 8 8) oar))
		  (return-pc ,(ldb (byte 24. 0.) retpc-rdest))
		  (return-frame ,(ldb (byte 3. 28.) retpc-rdest))
		  (return-register ,(ldb (byte 4. 24.) retpc-rdest)))
		call-stack i))))))

;;; $$$ Simplified. <08-Nov-88 wkf>
(defun get-warm-symbolic-address (pc &optional show-hex)
  "Returns a string which represents the pc as a function plus offset."
  (let* ((result      (kbug-symbolic-address pc))
	 (list        (if (consp result) result (cons result nil)))
	 (symbol-name (first list))
	 (offset      (second list)))
    (format nil "~a ~:[~*~;(~x)~]"
	    (if offset (format nil "~a+#x~x" symbol-name offset) symbol-name)
	    show-hex pc)))

(defun kbug-symbolic-address (pc)
  "Returns NIL if no info, a symbol if exact match, or a list of a symbol and an offset."
  (setq pc (ldb vinc:%%fixnum-field pc))
  ;; $$$ Moved next form out of conditional so that cold symbols are always printed as [pack]:cold-symbol <16-Nov-88 wkf>
  (multiple-value-bind (name offset)
      (get-symbolic-address-and-offset pc)
    (when name
      (return-from kbug-symbolic-address
	;; $$$ The following cruft is to print cold symbols nicely. <08-Nov-88 wkf>
	(let* ((sym-name  (symbol-name name))	
	       (sym-pack  (symbol-package name))
	       (pack-nics (package-nicknames sym-pack))
	       (pack      (if pack-nics (first pack-nics) (package-name sym-pack)))
	       (local-symbol (format nil "[~a]:~a" (subseq pack 2) sym-name)))
	  (if (zerop offset)
	      local-symbol
	    (list local-symbol offset))))))
  (let ((k-halted-p (k-halted-p)))
    (if (not k-halted-p) (kbug-cmd-confirm k2:kbug-command-pc-to-function pc))
    (let* ((function (kbug-data 0))
	   (symbol   (kbug-data 1)))
      (if (or k-halted-p (zerop function))
	  ;; $$$ Moved out case for cold symbols to be always done (needed for pretty printing) see above. <16-Nov-88 wkf>
	  (let* ((result (kbg-symbolic-address pc))
		 (name   (first result))
		 (offset (second result)))
	    (if offset (list name offset) name))
	(progn ;; Otherwise ask the K
	  (kbug-cmd k2:kbug-command-read-memory function 10)
	  (let ((entry-pc (ldb vinc:%%fixnum-field (kbug-data 6))))	; $$$ Use a constant not a literal. <09-Nov-88 wkf>
	    (if (not (= vinc:$$dtp-symbol (ldb vinc:%%data-type symbol)))
		nil
	      (let ((local-symbol (read-symbol-name symbol)))
		(if (= pc entry-pc)
		    local-symbol
		  (list local-symbol (- pc entry-pc)))))))))))

(defun test-read-misc (n)
  (kbug-cmd-confirm k2:kbug-command-read-misc 0 n)
  (let ((ans (kbug-data 0)))
    (format t "~%ans= ~o" ans)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Memory map functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The parenthesized map values are marginal.
;;; They have the same effect, but should never
;;; be used.

(defvar map-status-values
	#("read-mar"
	  "(unused 1)"
	  "read-mar-aged"
	  "(unused 3)"
	  "(unused 4)"
	  "read-only"
	  "read-only-aged"
	  "(unused 7)"
	  "swapped-out"
	  "(unused 9)"
	  "(unused 10)"
	  "(unused 11)"
	  "(unused 12)"
	  "direct-mapped"
	  "normal-aged"
	  "normal"))

(defun get-map-status-strings ()
  map-status-values)

(defun map-status->string (map-status)
  (aref (get-map-status-strings) map-status))

(defun decode-map-entry (value stream)
  (format stream "Protection ~x, ~:[NUBUS ~5,48X~*~;LOCAL  ~*~4,48x~] ~
               Volatility ~D ~
               ~15a ~@[~*Fresh~]"
	  (logand #xF value)
	  (map::map-local-memory?      value)
	  (map::map-off-board-address  value)
	  (map::map-on-board-address   value)
	  (map::map-cluster-volatility value)
	  (map-status->string (map::extract-map-status value))
	  (map::cluster-is-fresh? value)))
  
(defun kbug2-dump-map-for-cluster (cluster)	;actually this randomly retrieves 256. entries, 16. clusters
  (kbug2-read-map-cluster cluster)
  (dotimes (i 256.)
    (format t "~%#x~4,'0x  " i)
    (decode-map-entry (kbug-data i) t)))

(defun kbug2-read-map-cluster (cluster)
  (kbug-cmd-confirm k2:kbug-command-read-map cluster)
  (kbug-data 0))

(defun kbug-goto (where &optional (offset 0))
  (let ((pc (kbug2-get-starting-address where)))
    (if pc
	(kbug-write-pc (+ pc offset))
      (cerror "Forget it and continue" "Symbol ~S not found" where))))

(defun kbug2-get-starting-address (fcn)
  (if (numberp fcn)
      fcn
    (let ((pc (get-address fcn)))
      (if pc
	  pc
	(let ((f (nc::get-ncompiled-function fcn)))
	  (and f
	       (nc::ncompiled-function-starting-address f)))))))


(defvar *breakpoints* nil)
(defvar *breakpoints-installed* nil)

(defstruct (breakpoint)
  name
  (offset 0)
  addr
  (instr 0))

(defun set-breakpoint-region (fname from to)
  (let ((function (kbug2-get-starting-address fname)))
    (do ((offset from (1+ offset)))
	((> offset to))
      (push (make-breakpoint :name fname :offset offset
			     :addr (+ offset function))
	    *breakpoints*))))

(defun set-breakpoint (fname &rest offsets)
  (unless offsets (setq offsets '(0)))
  (dolist (offset offsets)
    (if (numberp fname)
	(push (make-breakpoint :name nil :offset offset :addr (+ fname offset))
	      *breakpoints*)
      (let ((function (kbug2-get-starting-address fname)))
	(if (null function)
	    (format t "~% *** Function ~S is undefined. ***" fname)
	  (push (make-breakpoint :name fname :offset offset
				 :addr (+ offset function))
		*breakpoints*)))))
  nil)

(defun clear-breakpoint (&optional fname)
  (if fname
      (setq *breakpoints* 
	    (delete-if #'(lambda (bp)
			   (eq fname (breakpoint-name bp)))
		       *breakpoints*))
    (setq *breakpoints* nil))
  nil)

(defun show-breakpoints ()
  (dolist (bp *breakpoints*)
    (format t "~%   ~A+~D" (breakpoint-name bp) (breakpoint-offset bp))))

(defvar *show-breakpoint-installs* t "When non-nil prints messages about breakpoint install/deinstall.")

(defun install-breakpoints ()
  (when (not *breakpoints-installed*)
    (setq *breakpoints-installed* t)
    (dolist (bp *breakpoints*)
      (let* ((pc (breakpoint-addr bp)))
	(when *show-breakpoint-installs* (format t "~%Installing breakpoint at virtual #x~X" pc))
	(setf (breakpoint-instr bp) (kbug-generic-read-inst-via-processor pc))
	(kbug-generic-write-inst-via-processor pc
	 #.(nc#:assemble-inst'K#:(JUMP 32. (NOP GR:*ZERO*) DT-RIGHT-ARRAY-AND-LEFT-STRUCTURE)))))))

(defun kbug-install-breakpoints ()
 ;use this when hacking breakpoints before KBUG2 mode comes up.  Installs a halt instruction.
  (when (not *breakpoints-installed*)
    (setq *breakpoints-installed* t)
    (dolist (bp *breakpoints*)
      (let* ((pc (breakpoint-addr bp)))
	(format t "~%Installing breakpoint at virtual #x~X" pc)
	(setf (breakpoint-instr bp) (kbug-generic-read-inst-via-processor pc))
	(kbug-generic-write-inst-via-processor pc
	 #.(nc#:assemble-inst'K#:(JUMP trap::illop (NOP GR:*ZERO*))))))))

(defun deinstall-breakpoints ()
  (when *breakpoints-installed*
    (setq *breakpoints-installed* nil)
    (dolist (bp *breakpoints*)
      (let* ((pc (breakpoint-addr bp)))
	(when *show-breakpoint-installs* (format t "~%Deinstalling breakpoint at virtual #x~X" pc))
	(kbug-generic-write-inst-via-processor pc (breakpoint-instr bp))
	))))


(defun kbug-set-stat-bit (fname &optional (bit-value 1))
  (let ((pc (if (numberp fname) fname
	       (kbug2-get-starting-address fname))))
    (let ((old-inst (kbug-generic-read-inst-via-processor pc)))
      (kbug-generic-write-inst-via-processor pc (dpb bit-value (byte 1 63.) old-inst)))
    ))

(defun kbug-step (&optional (count 1) (stop-list '(li:error)))
  (let ((stop-addrs (mapcar #'kbug-step-spec stop-list)))
    (dotimes (i count)
      (let ((pc (ldb (byte 24. 0) (kbug-read-pc))))
	(when (member pc stop-addrs :test #'(lambda (pc item) (<= (car item) pc (cdr item))))
	  (format t "Stopping step at label")
	  (return-from kbug-step)))
      (kbug-set-single-step-flag)
      (kbug-cmd-confirm k2:kbug-command-continue)
      )))

(defun kbug-step-spec (z)
  (cond
    ((symbolp z)
     (multiple-value-bind (pc len)
	 (get-address z)
       (setq pc  (ldb (byte 24. 0) pc))
       (setq len (ldb (byte 24. 0) len))
       (cons pc (+ pc len -1))))
    ((consp z)
     (let ((pc (get-address (first z))))
       (setq pc (+ (ldb (byte 24. 0) pc) (second z)))
       (cons pc pc)))
    (t (error "Bad step spec"))))



(defun kbug-proceed ()
  (deinstall-breakpoints)
  (kbug-set-single-step-flag)
  (kbug-cmd-raw k2:kbug-command-continue)	;kbug-cmd-confirm loses due to protocol defect
  (install-breakpoints)
  (kbug-clear-single-step-flag)
  (kbug-cmd-raw k2:kbug-command-continue))	;kbug-cmd-confirm loses due to protocol defect

(defun flush-breakpoints ()
  (setq *breakpoints* nil)
  (setq *breakpoints-installed* nil))

(defun kbug-make-string (string)
  (let ((length (zl:string-length string))
	status)
    (dotimes (i length)
      (kbug-set-data i (aref string i)))
    (setq status (kbug-cmd-confirm k2:kbug-command-make-string length))
    (values (kbug-data 0) status)))

(defun kbug-intern (symbol package)
  (let (k-symbol-name k-package-name status)
    (multiple-value-setq (k-package-name status)
      (kbug-make-string (string package)))
    (multiple-value-setq (k-symbol-name status)
      (kbug-make-string (string symbol)))
    (kbug-cmd-confirm k2:kbug-command-intern k-symbol-name k-package-name)
    (kbug-data 0)))

(defun write-to-k-character-stream (char)
  (k2::kbug-stream-write-character k2::kbug-k-input-character-stream char))

(defun read-from-k-stream ()
  (int-char
    (ldb (byte 8 0) (k2::kbug-stream-read-character k2::kbug-k-output-stream))))

(defun show-global (global-name)
  (do ((framelist *global-frame-table* (rest framelist))
       (framecount 0                   (1+ framecount)))
      ((null framelist) (error  "Couldn't find global register ~s" global-name))
    (do ((registers (cdr (first framelist)) (rest registers))
	 (offset    0                       (1+ offset)))
	((null registers))
      (when (eq global-name (car registers))
	(let ((register-contents (nth offset (kbug-generic-read-frame-as-list framecount))))
	  (format t "~%~@?" boxed-32 (car register-contents) (cdr register-contents)))
	(return-from show-global (values))))))

(defun read-global-register (global-name)
  (let* ((locn-list (get global-name :register))
	 (frame (second locn-list))
	 (offset (third locn-list)))
    (kbug-generic-read-register-with-boxed (dpb frame (byte 8 4) offset))))

;(defun read-global-register (global-name)
;  (do ((framelist *global-frame-table* (rest framelist))
;       (framecount 0                   (1+ framecount)))
;      ((null framelist) (error  "Couldn't find global register ~s" global-name))
;    (do ((registers (cdr (first framelist)) (rest registers))
;	 (offset    0                       (1+ offset)))
;	((null registers))
;      (when (eq global-name (car registers))
;	(kbug-generic-read-register-with-boxed (dpb framecount (byte 8 4) offset))
; ;	(let ((register-contents (nth offset (kbug-generic-read-frame-as-list framecount))))
; ;	  (return-from read-global-register (cdr register-contents)))
;	))))

;;;; Errors

;;; Error comes back with string in A0
(defun show-error ()
  (show-object (read-A-reg 0) t))

(defun show-illop ()
  ;;Illop code is in active register 0.  --wkf
  (why))

;;; this should hack entry points
(defun get-address (symbol)
  (cond ((numberp symbol) symbol)
	((k-halted-p)
	 (let ((f (nc::get-ncompiled-function symbol)))
	   (if f
	       (values (nc::ncompiled-function-starting-address f)
		       (nc::ncompiled-function-length f)))))
	(t
	 (let* ((sym (kbug-intern (symbol-name symbol)
				  (si:package-primary-name (symbol-package symbol))))
		(fcn (kbug-generic-read-memory (+ sym symbol:*symbol-function*))))
	   (if (= vinc:$$dtp-compiled-function (ldb vinc:%%data-type fcn))
	       ;; there should be some way to call %compiled-function-code
	       (values
		 (kbug-generic-read-memory (+ fcn 6))	; %compiled-function-code
		 (kbug-generic-read-memory (+ fcn 5)))	; %compiled-function-length
	     nil)))))


(defun flush-streams ()
  (k2::kbug-stream-initialize
    k2:kbug-k-output-stream
    (hw:dpb k2:$$kbug-stream-flags-direction-from-k k2:%%kbug-stream-flags-direction 0)
    k2:kbug-output-stream-base
    (+ k2:kbug-output-stream-base k2:kbug-stream-buffer-size))
  (k2::kbug-stream-initialize
    k2:kbug-k-input-fasl-stream
    (hw:dpb k2:$$kbug-stream-flags-direction-to-k k2:%%kbug-stream-flags-direction 0)
    k2:kbug-input-fasl-stream-base
    (+ k2:kbug-input-fasl-stream-base k2:kbug-stream-buffer-size))
  (k2::kbug-stream-initialize
    k2:kbug-k-input-character-stream
    (hw:dpb k2:$$kbug-stream-flags-direction-to-k k2:%%kbug-stream-flags-direction 0)
    k2:kbug-input-character-stream-base
    (+ k2:kbug-input-character-stream-base k2:kbug-stream-buffer-size)))

(defun kbug-load-cold-info (&key (warning-stream t))
  (kbug-cmd-raw k2:kbug-command-load-cold-info)
  (warm-download-cold-info *kfasl-stream* :warning-stream warning-stream))

(defun kbug-download-binary (filename file-type)
  (kbug-cmd-raw (ecase file-type
		  (:KFASL k2:kbug-command-fasl-stream)
		  (:FBIN  k2:kbug-command-load-fbin)))
  (warm-download filename :file-type file-type))

;;; $$$ Removed KBUG-FASL and KBUG-DOWNLOAD-FBIN replaced both with KBUG-DOWNLOAD-BINARY. <08-Nov-88 wkf>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Here is some stuff to make a FLAVORful output stream
;;; that tries to minimize cycles over the debug port.
;;; The lambda side maintains the real copy of the stream in-pointer
;;; and uses a possibly out of date value of the out-pointer
;;; for the limit.  When it either runs up against the limit
;;; or a :FORCE-OUTPUT occurs, you write the  in-pointer
;;; back into the K's memory and read the new out-pointer.

(zl:defflavor out-to-k-stream
	 (kbug-stream				;these three are constant
	  buffer-base				;properties of the stream.
	  buffer-limit

	  real-in-pointer			;these represent the lambda;s
	  stale-out-pointer			;idea of what the pointers are.
	  )
	 (si:output-stream)
  (:initable-instance-variables kbug-stream)
  :gettable-instance-variables)

(zl:defmethod (out-to-k-stream :reload-info) ()
  (setq real-in-pointer		(k2:kbug-stream-in-pointer kbug-stream)
	stale-out-pointer	(k2:kbug-stream-out-pointer kbug-stream)
	buffer-base		(k2:kbug-stream-base kbug-stream)
	buffer-limit		(k2:kbug-stream-end kbug-stream)))

(zl:defmethod (out-to-k-stream :update-pointers) ()
  (setf (k2:kbug-stream-in-pointer kbug-stream) real-in-pointer)
  (setq stale-out-pointer (k2:kbug-stream-out-pointer kbug-stream)))

(zl:defmethod (out-to-k-stream :force-output) ()
  (zl:send zl:self :update-pointers))

(defun wait-for-space ()
  (declare (:self-flavor out-to-k-stream))
  (do ((n 0 (1+ n))
       (wait-time 1000))
      ((not (or (= (1+ real-in-pointer) stale-out-pointer)
		(and (= stale-out-pointer buffer-base)
		     (>= (1+ real-in-pointer) buffer-limit)))))
    ;; $$$ Added time out check to detect errors on the K. <28-Oct-88 wkf>
    (when (= n wait-time)
      (cerror "Simply proceed." "The K appears to have stopped reading the download stream.
An error has probably occured.
Type ~c to keep trying with a longer time out." #\resume)
      (setq wait-time (* 2 wait-time)))
    (zl:send zl:self :update-pointers)))

(zl:defmethod (out-to-k-stream :tyo) (char)
  (wait-for-space)
  (k2::kbug-set-comm-word real-in-pointer char)
  (incf real-in-pointer)
  (when (>= real-in-pointer buffer-limit)
    (setq real-in-pointer buffer-base)))

(zl:defmethod (out-to-k-stream :string-out) (string &optional (start 0) (end (length string)))
  (unless start
    (setq start 0))
  (unless end
    (setq end (length string)))
  (do ((string-index start)
       (chars-to-send (- end start)))
      ((= string-index end))
    (wait-for-space)
    (let* ((space (if (>= real-in-pointer stale-out-pointer)
		      (1- (+ (- buffer-limit real-in-pointer)
			     (- stale-out-pointer buffer-base)))
		    (- stale-out-pointer real-in-pointer 1)))
	   (chars (min chars-to-send space)))
      (dotimes (i chars)
	(k2:kbug-set-comm-word real-in-pointer (char string string-index))
	(incf string-index)
	(incf real-in-pointer)
	(when (>= real-in-pointer buffer-limit)
	  (setq real-in-pointer buffer-base)))
      (decf chars-to-send chars))))

(zl:compile-flavor-methods out-to-k-stream)

(defvar *kfasl-stream* (zl:make-instance 'out-to-k-stream
				      :kbug-stream k2:kbug-k-input-fasl-stream))

;;; Warm Loading

;;; This is sort of like the lambda's mini-server.
;;; It reads a KFASL file from a file server and shoves
;;; it into the K-INPUT-FASL-STREAM

(defvar *fasl-buf* (make-array 4096. :element-type 'string-char :fill-pointer t))

(defun warm-download (file &key (file-type :KFASL))
  (zl:send *kfasl-stream* :reload-info)
  (let ((path (find-newest-version-of-file-type file file-type)))
    (format *query-io* "~&Downloading ~A ~%" (truename path))

    ;; MEGA-BOOT clears *DOWNLOADED-FILES*
    ;; we will keep track of multiple versions of the same file
    ;; if they are downloaded in the lifetime of the same MEGA-BOOT
    (push (truename path) *downloaded-files*)
    
    (with-open-file (stream path :direction :input)
      (do ((ibuf *fasl-buf*)
	   (len  nil)
	   (eof  nil))
	  (eof)
	(setf (fill-pointer ibuf) 0)
	(multiple-value-setq (len eof) (zl:send stream :string-in nil ibuf))
	;(dotimes (i len) (zl:send *kfasl-stream* :tyo (char ibuf i)))
	(zl:send *kfasl-stream* :string-out ibuf 0 len)
	))
    (zl:send *kfasl-stream* :force-output)
    ))

;;; $$$ Removed LOAD-KENV-FILES defintion which was unused. <08-Nov-88 wkf>

;;; $$$ Added <08-Nov-88 wkf>
(defun find-newest-version-of-file-type (file type)
  (fs:merge-pathname-components file nil
				;; $$$ I'm not sure why the string is required below. <09-Nov-88 JIM>
				;; But KENV's and FDEF's weren't getting defaulted correctly <09-Nov-88 JIM>

				:default-type         type
				:always-merge-type    t
				:default-version      :newest
				:always-merge-version nil))

;;; $$$ Wrote one function for FDEF and KENV file loading.  Removed LOAD-KENV-FILE and LOAD-FDEF-FILE. <08-Nov-88 wkf>
(defun load-environment-file (file binary-type) "Binary-type is :KFASL or :FBIN"
  (let* ((*package*        (zl:find-package 'USER))
	 (*readtable*      (si:find-readtable-named "CL"))
	 ;; $$$ changed type to binary-type <09-Nov-88 JIM>
	 (environment-type (ecase binary-type
			     (:KFASL :KENV)
			     (:FBIN  :FDEF)))
	 (pathname         (find-newest-version-of-file-type file environment-type)))
    (load pathname :verbose nil))) ;;Don't print name of environment file being loaded.

(defun warm-download-cold-info (stream &key warning-stream)
  (zl:send stream :reload-info)
  (cold:fasd-cold-function-info stream :warning-stream warning-stream)
  (zl:send stream :force-output)
  )

;;; $$$ Command defs moved to zwei-coms <04-Nov-88 smh>



