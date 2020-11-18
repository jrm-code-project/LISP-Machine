;;;-*- Mode:LISP; Package:LAMBDA; Base:16; Readtable:ZL -*-


(export '(debug-write-word debug-write-byte
	  debug-read-word debug-read-byte))
  
;switch to allow diags to run with K resident on local NUBUS with minimum
; perturbation to routines with have DEFSUBSTs, etc
 
(defvar *local-debugging* 'foo "Non nil K is really on local bus, not using debug board at all")
;;RG now may be T for local bus, COUPLER for nubus-coupler mode, or NIL for HOULT debug board mode.
;;wkf 5/11/88 changed initial value to 'foo so we can tell that it has been set to T or nil.

(defvar *local-mapped-falcon-slot-base-address* 0)	;In nubus-coupler mode, add this to all addresses.
	;(it is necessary because bus coupler map operates in quanta of two-board-slot-spaces).
	;it is sometimes added anyway, so set it to 0 in other modes.

(defun find-a-debug-board ()
  (let ((this-lambda-processor-slot (si::%processor-conf-slot-number si::*my-proc-conf*))
	(first-debug-board          (si::%system-configuration-lmi-debug-board-owner si::*sys-conf*))
	(second-debug-board         (si::%system-configuration-second-lmi-debug-board-owner si::*sys-conf*)))
    (let ((first-board-exists?  (not (= (ldb (byte 8. 0.) first-debug-board)  (ldb (byte 8. 0.) -1.))))
	  (second-board-exists? (not (= (ldb (byte 8. 0.) second-debug-board) (ldb (byte 8. 0.) -1.)))))
      (if (not first-board-exists?)
	  (ferror nil "I can't find a debug board, get one and run CONFIG.")
	  ;; We have at least one board, do we own it?
	  (cond ((= this-lambda-processor-slot first-debug-board)
		 (si::%system-configuration-lmi-debug-board-slot si::*sys-conf*))
		((= this-lambda-processor-slot second-debug-board)
		 (si::%system-configuration-second-lmi-debug-board-slot si::*sys-conf*))
		;; I don't own either, offer to take one.
		(t (cerror "Take one." "I don't own a debug board, but there is at least one here.~
                                        You can take it.")
		   (if second-board-exists?
		       (fquery
			 (list ':choices
			       `(((,(si::%system-configuration-lmi-debug-board-slot
				      si::*sys-conf*) "First board")  #/1)
				 (( ,(si::%system-configuration-second-lmi-debug-board-slot
				       si::*sys-conf*) "Second board") #/2))
			       ':fresh-line 't)
			 "Take which board?")
		 (si::%system-configuration-lmi-debug-board-slot si::*sys-conf*)
		     )))))))

(defvar khh-coupler-slot nil)
(defvar debug-addr nil)
(defvar debug-test-reg-addr nil)

(defun assure-debug-board ()
  (when (null khh-coupler-slot)
    (let ((slot (find-a-debug-board)))
      (setq khh-coupler-slot (logior #xF0 slot))
      (setq debug-addr (ash khh-coupler-slot 24.))
      (setq debug-test-reg-addr (logior #xF0FFF7C4 debug-addr)))))

(defun manual-debug-slot-setup (slot)
  ;use this if you have to set debug board manually because debug board is not known to CONFIG.
  (setq khh-coupler-slot slot)
  (setq debug-addr (ash khh-coupler-slot 24.))
  (setq debug-test-reg-addr (logior #xF0FFF7C4 debug-addr)))

;(defparameter khh-coupler-slot #xfa)
;(defparameter debug-addr (ash khh-coupler-slot 24.))
;(defparameter debug-test-reg-addr (logior #xf0fff7c4 debug-addr))

(defun test-debug-board (slot)
  (setq khh-coupler-slot (logior (logand #x0f slot) #xf0))
  (setq debug-addr (ash khh-coupler-slot 24.))
  (setq debug-test-reg-addr (logior #xf0fff7c4 debug-addr))
  (format t "~%LMI Debug board diagnostics starting.~%")
  (format t "~%Basic register R/W test -----------~%")
  (debug-rw-test)
  (format t "~%Serial loopback test (no DMA) -----~%")
  (debug-serial-loop-test)
  (format t "~%Serial loopback test (with Mastership) ----~%")
  (debug-iloop-test)
  (format t "~%Nubus analyzer RAM test ---------~%")
  (test-debug-analyzer-ram)
  (format t "~%LMI Debug board diagnostics complete.~%"))

;;;****************************************************************

(defun print-all-config-proms-via-debug-board ()
  (if *local-debugging*
      (format t "~&You are not running with a debug board!")
    (dotimes (i 16.)
      (format t "~&Index ~d.: " i)
      (format t "~s" (read-config-prom-via-debug-board i))
    )))


(defun read-config-prom-via-debug-board (slot &aux s)
  ;This only works via HOULT debug board!
  (setq s "                                ")
  (if (fixp (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff7fc)))
      (dotimes (i (string-length s))
	(aset (logand #xff (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff800 (* i 4))))
	      s i)))
  s)
;;;****************************************************************

(defsubst read-debug (addr)
  (%nubus-read khh-coupler-slot addr))

(defsubst write-debug (addr data)
  (%nubus-write khh-coupler-slot addr data))

(defsubst read-debug-byte (addr)
  (sys:%nubus-read-8 khh-coupler-slot addr))

(defsubst write-debug-byte (addr data)
  (sys:%nubus-write-8 khh-coupler-slot addr data))




(defsubst read-debug-mode ()		;refers to mode register on debug board, not processor.
  (logand #xff (read-debug #xfff7fc)))

(defsubst write-debug-mode (data)
  (write-debug #xfff7fc data))

(defsubst write-debug-addr (addr)
  (write-debug #xfff7f8 addr))

(defsubst read-debug-addr()
  (read-debug #xfff7f8))

(defsubst write-debug-data (data)
  (write-debug #xfff7f4 data))

(defsubst read-debug-response-data ()
  (read-debug #xfff7f4))

(defsubst write-debug-control (ctl)
  (write-debug #xfff7f0 ctl))

(defsubst read-debug-response-control ()
  (logand #x3f (read-debug #xfff7f0)))

(defsubst write-debug-analyzer-pointer (data)
  (write-debug #xfff7ec data))

(defsubst read-debug-analyzer-pointer ()
  (logand #x8fff (read-debug #xfff7ec)))

(defsubst read-debug-analyzer-data ()
  (read-debug #xfff7e8))

(defsubst write-debug-analyzer-data (data)
  (write-debug #xfff7e8 data))

(defsubst read-debug-analyzer-control ()
  (logand #xff (read-debug #xfff7e4)))

(defsubst write-debug-analyzer-control (data)
  (write-debug #xfff7e4 data))

(defsubst write-debug-analyzer-function (data)
  (write-debug #xfff7e0 data))


(defsubst remote-write-debug-analyzer-pointer (data)
  (write-debug #xfff7ec data))

(defsubst remote-read-debug-analyzer-pointer ()
  (logand #x8fff (read-debug #xfff7ec)))

(defsubst remote-read-debug-analyzer-data ()
  (read-debug #xfff7e8))

(defsubst remote-write-debug-analyzer-data (data)
  (write-debug #xfff7e8 data))

(defsubst remote-read-debug-analyzer-control ()
  (logand #xff (read-debug #xfff7e4)))

(defsubst remote-write-debug-analyzer-control (data)
  (write-debug #xfff7e4 data))

(defsubst remote-write-debug-analyzer-function (data)
  (write-debug #xfff7e0 data))


(defsubst read-debug-explorer-ram ()
  (logand #xffff (read-debug #xfff7cc)))

(defsubst write-debug-explorer-ram (data)
  (write-debug #xfff7cc data))

(defsubst read-debug-explorer-pointer ()
  (logand #xfff (read-debug #xfff7c8)))

(defsubst write-debug-explorer-pointer (data)
  (write-debug #xfff7c8 data))

(defsubst read-debug-explorer-control ()
  (read-debug #xfff7c4))

(defsubst write-debug-explorer-control (data)
  (write-debug #xfff7c4 data))

(defsubst read-debug-explorer-status ()
  (read-debug #xfff7c0))

(defsubst read-debug-semaphore ()
  ;; Returns T if you got it.
  (not (ldb-test (byte 1. 0.) (read-debug #xfff7dc))))

(defsubst clear-debug-semaphore ()
  (write-debug #xfff7dc 0.))

(defparameter *debug-semaphore-timeout* 10.)

(defmacro locking-debug-board (&body body)	;yuck
  ;; Hang until we get it.
  ;; I am not sure why the obvious unwind protect version doesn't work
  ;; maybe I am broken.  I tried it several different ways, and each
  ;; way left the lock seized in certain cases.  This way works,
  ;; although I don't like leaving interrupts off for so long.
;  `(progn ,@body)	;this is what is should be these days.  unfortunately,
;			; lack of delay seems to cause flakey operation...
;  foo:  below lost, also delays (count of 14.) before and after body lost.
;  `(prog1 (progn (read-debug-semaphore) ,@body) (clear-debug-semaphore))
; so just use the old thing, forcing the semaphore to look available.
  `(block locking-debug-board
     (let ((time 0))
       (loop
	 (let ((inhibit-scheduling-flag t)
	       )
	   (when (progn (read-debug-semaphore) t)	;foo.
	     (return-from locking-debug-board
	       (multiple-value-prog1 (progn ,@body)
				     (clear-debug-semaphore)))))
	 (if (< time *debug-semaphore-timeout*)
	     (progn (lisp::sleep .1 "Debug board")
		    (incf time .1))
	   (ferror nil "Timed out on the semaphore.")))))
  )
	   

;;; The 45 other ways to do it that don't work.
;       (when (read-debug-semaphore)
;	 (RETURN-FROM LOCKING-DEBUG-BOARD
;	   (multiple-value-prog1 (PROGN ,@body)
;				 (clear-debug-semaphore)))))))

;       (retur
;(defmacro locking-debug-board (&body body)
;  `(BLOCK LOCKING-DEBUG-BOARD

;     (loop
;       (catch 'try-again
;       (LET* ((INHIBIT-SCHEDULING-FLAG T)
;	      (GOT-IT (READ-DEBUG-SEMAPHORE)))
;	 (WHEN (NOT GOT-IT)
;	   (throw 'TRY-AGAIN nil)))
;	 (RETURN-FROM LOCKING-DEBUG-BOARD
;	   (multiple-value-prog1 (PROGN ,@body)
;				 (clear-debug-semaphore)))))))
	 




;  ;; This is extremely gross.
;  `(LET ((.DEBUG-BOARD-SEIZED. NIL))
;     (UNWIND-PROTECT
;	 (IF (WITHOUT-INTERRUPTS (SETQ .DEBUG-BOARD-SEIZED. (READ-DEBUG-SEMAPHORE)))
;	     (PROGN ,@body)
;	   (BLOCK .DEBUG-BOARD-SEIZER.
;	   (LOOP
;	     (IF (PROCESS-WAIT-WITH-TIMEOUT
;		      "Debug board" (* 10. 60.)
;		      #'(LAMBDA () (SETQ .DEBUG-BOARD-SEIZED. (READ-DEBUG-SEMAPHORE))))
;		 (RETURN-FROM .DEBUG-BOARD-SEIZER. (PROGN ,@body))
;		 (CERROR "try again." "Couldn't seize debug-board.")))))
		    
;     (without-interrupts
;	 (when .DEBUG-BOARD-SEIZED.
;	     (CLEAR-DEBUG-SEMAPHORE))))))

(defun setup-debug-board-for-loopback ()
  (write-debug-mode 1)				; reset the board
  (write-debug-mode #x1a)			; fast speed, loopback, master enable
  (process-sleep 2)				; let it idle down
  (read-debug-response-control)
  (write-debug-analyzer-function 0)
  (read-debug-analyzer-data)
  nil)

(defun wait-for-debug-xmit ()
  (dotimes (i 4000.)
    (if (equal 0 (logand #x40 (read-debug-mode)))
	(return t))))

(defun wait-for-debug-response (&optional no-error &aux temp)
  (setq temp (dotimes (i 4000. (if (not no-error) (fsignal "no response")))
	       (when (equal #x80 (logand #x80 (read-debug-mode)))
		 (return t))))
  (cond
    ((null temp) 'NO-RESPONSE)
    ((equal #x0e (read-debug-response-control))
     (read-debug-response-data))
    (t nil)))


(defun il (&aux temp)
  (setup-debug-board-for-loopback)
  (write-debug-explorer-control -1)
  (do-forever
    (setq temp (debug-read-word debug-test-reg-addr))
    (when (not (equal temp #xffffffff))
      (format t "Error --- wrote FFFFFFFF --- read ~A~%" temp))))


(defun rl ()
  (do-forever
    (read-debug-mode)))

(defun wl ()
  (do-forever
    (write-debug-mode #xffffffff)))


(defun read-config-prom (&aux s)
  (setq s "                                ")
  (dotimes (i 25.)
    (aset
      (logand #xff (read-debug (+ #xfff800 (* i 4))))
      s i))
  s)


(defun debug-read-word (addr)
  (if *local-debugging*
   ;this is right for either local mode or coupler mode!  In coupler mode, the base addresses had
   ; better be offset by *local-mapped-falcon-slot-base-address* tho.
      (si:%nubus-read (ldb (byte 8. 24.) addr) (logand #o77777777 addr))
    (locking-debug-board
      (write-debug-addr addr)
      (write-debug-control #x01)
      (wait-for-debug-response))))

(defun debug-read-byte (addr)
  (if *local-debugging*
      (si:%nubus-read-8 (ldb (byte 8. 24.) addr) (logand #o77777777 addr))
    (locking-debug-board
      (write-debug-addr addr)
      (write-debug-control #x05)
      (wait-for-debug-response))))

(defun debug-write-word (addr data)
  (if *local-debugging*
      (si:%nubus-write (ldb (byte 8. 24.) addr) (logand #o77777777 addr) data)
    (locking-debug-board
      (write-debug-data data)
      (write-debug-addr addr)
      (write-debug-control #x09)
      (wait-for-debug-response))))

(defun debug-write-byte (addr data)
  (if *local-debugging*
      (si:%nubus-write-8
	(ldb (byte 8. 24.) addr)
	(logand #xffffff addr)
	(logand #xff (ash data (* -8. (logand addr 3)))))
    (locking-debug-board
      (write-debug-addr data)
      (write-debug-control 8 )
      (wait-for-debug-xmit)
      (write-debug-addr addr)
      (write-debug-control #x0d)
      (wait-for-debug-response))))

(defun debug-rw-test (&aux temp)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug #xfff7c4 (ash 1 i))
      (setq temp (read-debug #xfff7c4))
      (when (not (equal temp (ash 1 i)))
	(format "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))



(defun debug-iloop-test (&aux temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug-explorer-control (ash 1 i))
      (setq temp (debug-read-word debug-test-reg-addr))
      (when (not (equal temp (ash 1 i)))
	(format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp))))
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (debug-write-word debug-test-reg-addr (ash 1 i))
      (setq temp (read-debug-explorer-control))
      (when (not (equal temp (ash 1 i)))
	(format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))

(defun debug-iloop ()
  (setup-debug-board-for-loopback)
  (do-forever
    (write-debug-explorer-control 0)
    (when (not(equal 0 (debug-read-word debug-test-reg-addr)))
      (tyo #/*))
    (write-debug-explorer-control #xffffffff)
    (when (not(equal #xffffffff (debug-read-word debug-test-reg-addr)))
      (tyo #/#))))

(defun debug-serial-loop-test (&aux data temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (setq data (ash 1 i))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
	(format t "Error --- wrote ~A --- read ~A~%" data temp))
      (setq data (logand #xffffffff (lognot (ash 1 i))))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
	(format t "Error --- wrote ~A --- read ~A~%" data temp)))))

(defun test-debug-analyzer-ram (&aux temp data master-data-list data-list)
  (setup-debug-board-for-loopback)
  (setq master-data-list (copylist '(#x01010101 #x02020202 #x04040404 #x08080808
		   #x10101010 #x20202020 #x40404040 #x80808080 #x55555555)))
  (rplacd (last master-data-list) master-data-list)
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (write-debug-analyzer-data (car data-list))
    (write-debug-analyzer-control (car data-list))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (car data-list))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data  (logand #xffffffff (lognot (car data-list))))
    (write-debug-analyzer-data data)
    (write-debug-analyzer-control data)
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (logand #xffffffff (lognot (car data-list))))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list))))

(defconst analyzer-codes (make-array 16. :initial-contents
   '("Null cycle 0"     "Start read"      "ACK Try Again Later"     "Idle Cycle"
     "Intermediate ACK" "Start byte read" "ACK Parity Error"	    "Bad Idle cycle 1"
     "Null cycle 2"     "Start write"     "ACK NUBUS Timeout"       "Bad Idle cycle 2"
     "Null cycle 3"     "Start byte write" "ACK OK"                 "Bad Idle cycle 3")))

     
(defun print-debug-analyzer (&optional (count 30.) &aux ptr data ctl master code-string)
  (read-debug-analyzer-data)
  (setq ptr (logand #xfff (read-debug-analyzer-pointer)))
  (format t "~%SEQ#~8TADDRESS~18TDATA~28TMaster~36TINFO~%")
  (dotimes (i count)
    (write-debug-analyzer-pointer (- ptr 2 i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (setq master (ash (logand khh-coupler-slot ctl) -4.))
    (write-debug-analyzer-pointer ptr)
    (setq code-string (aref analyzer-codes (logand ctl #x0f)))
    (cond
      ((eq 1 (logand ctl 1))
       (format t "~@4X~8T~@8X~28T~X~36T~A~%" (- ptr i) data master code-string))
      (t
       (format t "~@4X~18T~@8X~28T~X~36T~A~%" (- ptr i)  data master code-string)))))


(defun flog-debug-analyzer(&aux ptr data ctl)
  (start-debug-analyzer 0)
  (write-debug-explorer-control 0)
  (read-debug-explorer-control)
  (write-debug-explorer-control -1)
  (read-debug-explorer-control)
  (read-debug-mode)
  (read-debug-analyzer-data)
  (setq ptr (read-debug-analyzer-pointer))
  (dotimes (i 16.)
    (write-debug-analyzer-pointer (- ptr i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (format t "~@8X~10T~@3X~%" data ctl)))



(defun start-debug-analyzer (&optional slave master &aux temp)
  (setq temp 0)
  (write-debug-analyzer-pointer 0)
  (if slave
    (setq temp (logior temp (logand slave #x0f)))
    (setq temp (logior temp #x100)))
  (if master
    (setq temp (logior temp (ash (logand master #x0f) 4)))
    (setq temp (logior temp #x200)))
  (write-debug-analyzer-function (logior temp #x400)))

(defun debug-read-word-fast-repeat (addr)
  (write-debug-addr addr)
  (write-debug-control #x11)
  (tyi)
  (wait-for-debug-xmit)
  (write-debug-control #x01)
  (process-sleep 2)
  (read-debug-response-control))

(defun debug-write-word-fast-repeat (addr data)
  (write-debug-addr data)
  (write-debug-control #x08)
  (wait-for-debug-xmit)
  (write-debug-addr addr)
  (write-debug-control #x19)
  (tyi)
  (wait-for-debug-xmit)
  (write-debug-control #x09)
  (process-sleep 2)
  (read-debug-response-control))

(defun init-debug-board (&aux rmt)
  (assure-debug-board)
  (write-debug-mode 1)				; reset the board
  (write-debug-mode #x12)			; fast, master enable
  (process-sleep 2)				; let it idle down
  (read-debug-response-control)
  (write-debug-analyzer-function 0)
  (read-debug-analyzer-data)
  (if (setq rmt (remote-debug-mode-reg-address))
      (debug-write-word rmt #x12)
    (format t "~%*** Remote debug board not found ***~%"))
  rmt)
  

(defun remote-debug-mode-reg-address (&aux temp)
  (write-debug-control 0)		; Poll request
  (setq temp (dotimes (i 1000.)
	       (when (equal #x80 (logand #x80 (read-debug-mode)))
		 (return t))))
  (cond
    (temp
     (logior #xf0fff7fc (ash (logand #x0f (read-debug-response-data)) 24.)))
    (t nil)))
  


;(defun bus-read (byte-address &optional ignore-bus-errors byte-mode &aux temp)
;  (if byte-mode
;    (setq temp (debug-read-byte byte-address))
;    (setq temp (debug-read-word byte-address))
;    (cond
;      ((fixp temp) temp)
;      (ignore-bus-errors 0)
;      (t (error 'nubus-error)))))

;(defun bus-read-byte (byte-address &optional ignore-bus-errors &aux temp)
;    (setq temp (debug-read-byte byte-address))
;    (cond
;      ((fixp temp) (logand #xff (ash temp (* -8. (logand 3 byte-address)))))
;      (ignore-bus-errors 0)
;      (t (error 'nubus-error))))

;(defun bus-write (byte-address data &optional ignore-bus-errors byte-mode &aux temp)
;  (if byte-mode
;    (setq temp (debug-write-word byte-address data))
;    (cond
;      ((fixp temp) nil)
;      (ignore-bus-errors nil)
;      (t (error 'nubus-error)))))		      

;(defun bus-write-byte (byte-address data &optional ignore-bus-errors &aux temp)
;  (setq temp (debug-write-word byte-address (dpb data (byte 8. (* 8. (logand 3 byte-address))) data)))
;    (cond
;      ((fixp temp) nil)
;      (ignore-bus-errors nil)
;      (t (error 'nubus-error))))

    
(defun debug-iloop-test-remote (&aux temp addr)
  (format t "Starting remote loopback test with mastership.")
  (setq addr (logior #xfff7c4 (logand #xff000000 (init-debug-board))))
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (debug-write-word addr (ash 1 i))
      (setq temp (debug-read-word addr))
      (when (not (equal temp (ash 1 i)))
	(format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))


(defun fast-address-test-memory-via-debug-board (slot size-in-megs)
  (let ((base-address (ash (logior #xf0 slot) 24.)))
    (fast-address-test-kernal #'(lambda (address data)
				  (debug-write-word (+ (ash address 2) base-address) data))
			    #'(lambda (address)
				(debug-read-word (+ (ash address 2) base-address)))
			    0 32. (+ (haulong size-in-megs) 17.) "Nubus-Memory")
    ))