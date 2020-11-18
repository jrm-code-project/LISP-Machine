;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-


;;;****************************************************************
;;;
;;; Notes
;;;
;;;****************************************************************

;;; see daisy-prom.lisp and daisy-sim.lisp
;;; for code which transfers and runs test vectors on the daisy

;;; see k;trap.lisp for trap vectors

;;; the daisy-prom.lisp code uses nc:link
;;; nc:link differs from cold-link in that it ignores a negative TOFFSET and the ENTRY-POINT to external refs
;;; nc:link sets the starting address

;;; cold-link is in kold-loader.lisp

;;; certain things not allowed in tests

;;;  no functional arguments  (which would be fixed up by cold-link negative toffset
;;;  no code that requires runtime support functions (FUNCALL, etc..)

;;; code lives in top half of virtual memory  (implied high bit set, implied low bit off for 64bit instruction pc)

;;; immediates   0 => unboxed zero  but  (eval 0)  => boxed zero
;;;  immediate-32 in assem.lisp
;;;     if it is handed an integer it will simply grab its 32low order bits and put them into the movei instruction
;;;     otherwise it evals what it gets
;;;       if the result is a fixnum that will fit in 24 bits it becomes a boxed fixnum
;;;       if the form evaled was (hw:unboxed-constant ???) the value is stored in the movei instruction
;;;       floats are handled specially
;;;       any other object (bignums, strings, symbols, etc.) are downloaded specailly by fasadump
;;;       THESE OBJECTS CANNOT APPEAR IN TEST VECTORS !!!
;;;  hw:unboxed-constant makes sure the number will fit in 32 bits


;;; -no use k-kbug:disassemble-fcn-from-memory to see downloaded code
;;; use (kbug-disassemble <start> <how-munch>)
;;;****************************************************************
;;;
;;; Questions
;;;
;;;****************************************************************

;;; What is the best way to handle addressing individual words in a two word instruction ??

;;; The original DAISY-SIM defafun has a jump to location 32 at location 3.
;;; Does the daisy simulation always start at location zero ??
;;; And if it does why is the jump at location 3 ??
;;; (Perhaps the last question is answered by studying the trap PAL logic.)

;;; How do I access code locations ???
;;;  Same as data memory locations.
;;;  It all depends on how the Memory Map is set up.
;;;  We can make the first cluster of virtual memory point to the
;;;  same cluster that the code lives in.

;;; Global Registers ???
;;;   perhaps lisp constants
;;;   but in general for assembly code don't depend on them

;;;****************************************************************
;;;
;;; Improvements
;;;
;;;****************************************************************

;;; Test result analysis:
;;;   If halted in test code show disassembled code
;;;   Decode failure status in registers or memory locations
;;;
;;; Specify test dependancies
;;;   run regression tests
;;;
;;; Keep track of test results and hardware configuration
;;;
;;; Record link and download status for each test
;;;   Disassembler can use this info to print branch locations
;;;    either as offsets from start of function(decimal)
;;;    or linked up values (hex addresses)

;;;****************************************************************
;;;
;;; Assumptions
;;;
;;;****************************************************************

;;; Code Page Zero and Data Page Zero both map to Physical Page Zero.

;;;****************************************************************
;;;
;;; K Test Code Memory Map : Code Locations
;;;
;;;****************************************************************
(eval-when (compile load eval)	;these are used by #. frobs.

;; The usual form for using these constants in assembly code is:
;; (jump #.k-test-???-loc ())

;;; $$$ chagned defconst to defvar <pace>

;;; K Test Trap Vector Locations
(defvar k-test-first-trap-vector-loc  #x00  "First location reserved for trap code and trap vectors.")
(defvar k-test-last-trap-vector-loc   #x3f  "Last location reserved for trap code and trap vectors.")

;;; K Test Entry and Exit Locations
(defvar k-test-entry-loc              #x40  "This is normally a jump to the real entry.")
(defvar k-test-pass-exit-loc          #x41  "Test passed. Normally a jump to pass halt zone.")
(defvar k-test-fail-exit-loc          #x42  "Test failed. Normally a jump to fail halt zone.")
(defvar k-test-spare-exit-loc         #x43  "Spare exit location.")

;;; K Test Trap Code Location
(defvar k-test-trap-handler-loc       #x50  "Traps vector to this location.")

;;; K Test Halt Zones
(defvar k-test-pass-halt-loc          #x60  "Runaway ramp for halt.  Test passed.")
(defvar k-test-fail-halt-loc          #x70  "Runaway ramp for halt.  Test failed.")

;;; K Test Start of Code Location
(defvar k-test-code-loc               #x80  "Test code lives here.")

;;;****************************************************************
;;;
;;; K Test Code Memory Map : Memory Locations
;;;
;;;****************************************************************

;; the usual form for using these constant in assembly code is:
;; (move vma #.k-test-???-loc)

;;; K Test Switch and Argument Locations
;; These are translated into memory addresses
(defvar k-test-bit-args0-loc          (* 2 #x44)       "(low word) 32 bits of switches passed to tests.")
(defvar k-test-bit-args1-loc          (1+ (* 2 #x44))  "(low word) 32 bits of switches passed to tests.")
(defvar k-test-word-arg0-loc          (* 2 #x45)       "(low word)One word argument to tests.")
(defvar k-test-word-arg1-loc          (1+ (* 2 #x45))  "(high word)One word argument to tests.")
(defvar k-test-word-arg2-loc          (* 2 #x46)       "(low word)One word argument to tests.")
(defvar k-test-word-arg3-loc          (1+ (* 2 #x46))  "(high word)One word argument to tests.")
(defvar k-test-word-arg4-loc          (* 2 #x47)       "(low word)One word argument to tests.")
(defvar k-test-word-arg5-loc          (1+ (* 2 #x47))  "(high word)One word argument to tests.")

;;; K Test Result Locations
;; These are translated into memory addresses
(defvar k-test-internal-result-loc    (* 2 #x48)       "(low word)")
(defvar k-test-external-result-loc    (1+ (* 2 #x48))  "(high word)")
(defvar k-test-expected-value-loc     (* 2 #x49)       "(low word)")
(defvar k-test-incorrect-value-loc    (1+ (* 2 #x49))  "(high word)")
(defvar k-test-address-loc            (* 2 #x4a)       "(low word)")

;;;****************************************************************
;;;
;;; K Test Status Codes
;;;
;;;****************************************************************

(defvar k-test-null-status            #x00000000 "Null Status.")
(defvar k-test-passed                 #x00000001 "Passed the test.")
(defvar k-test-failed                 #x00000002 "Failed the test.")

)	;end eval-when

;;;****************************************************************
;;;
;;; K Test Driver
;;;
;;;****************************************************************

(defvar *k-tests* '() )
(defvar *last-test-downloaded* nil)

(defmacro def-k-test (name lambda-list support-functions &body instructions)
 ;  (lisp:format t "~%Please notice: DEF-K-TEST takes a new argument, SUPPORT-FUNCTIONS.")
  (when (and support-functions (symbolp support-functions))
    (setq support-functions (list support-functions)))
  `(progn
     (defafun ,name ,lambda-list . ,instructions)
     (setf (lisp:get ',name 'vc-support-functions) ',support-functions)
     (lisp:pushnew ',name *k-tests*))
  )

(defun vc-run-all-tests (&key loop-it leave-running load-global-register-constants)
  (dolist (test *k-tests*)
    (vc-test-driver test
		    :loop-it loop-it
		    :leave-running leave-running
		    :load-global-register-constants load-global-register-constants)))

(defun vc-test-driver (test
		       &key halt loop-it leave-running
		           leave-running-and-wait
		           (load-global-register-constants t) (sleep-time 200. sleep-specified-p)
		            map-entry-for-nubus nubus-slot
		            arg0 arg1 arg2 arg3 arg4 arg5 switch0 switch1)

  (unless (nc:nsymbol-function test)
    (lisp:error "~&~s is has not been assembled. (Are you looking in the li: package?)" test))
  (vc-clear-k)
  (setq k-kbug:*code-start* 0)		;read-inst-physical-with-offset (in turn disassemble) looks at this.
  (when load-global-register-constants
    (vc-load-global-constant-frame))
  (vc-download-trap-handlers)
  (vc-link-and-download-test-code test)
  (lam:write-inst k-test-entry-loc
		  (nc:assemble-inst `(k:jump , (vc-test-entry test) nil)))
  (cond (loop-it
	 (lam:write-inst k-test-pass-exit-loc (nc:assemble-inst `(k:jump ,k-test-entry-loc nil)))
	 (lam:write-inst k-test-fail-exit-loc (nc:assemble-inst `(k:jump ,k-test-entry-loc nil))))
	(halt
	 (lam:write-inst k-test-pass-exit-loc (nc:assemble-inst `(k:jump ,k-test-pass-halt-loc nil)))
	 (lam:write-inst k-test-fail-exit-loc (nc:assemble-inst `(k:jump ,k-test-fail-halt-loc nil))))
	(t
	 (lam:write-inst k-test-pass-exit-loc (nc:assemble-inst `(k:jump ,k-test-pass-exit-loc nil)))
	 (lam:write-inst k-test-fail-exit-loc (nc:assemble-inst `(k:jump ,k-test-fail-exit-loc nil)))
	 ))

  (when (integerp arg0) (lam:k-mem-write-word-address k-test-word-arg0-loc arg0))
  (when (integerp arg1) (lam:k-mem-write-word-address k-test-word-arg1-loc arg1))
  (when (integerp arg2) (lam:k-mem-write-word-address k-test-word-arg2-loc arg2))
  (when (integerp arg3) (lam:k-mem-write-word-address k-test-word-arg3-loc arg3))
  (when (integerp arg4) (lam:k-mem-write-word-address k-test-word-arg4-loc arg4))
  (when (integerp arg5) (lam:k-mem-write-word-address k-test-word-arg5-loc arg5))
  (when (integerp switch0) (lam:k-mem-write-word-address k-test-bit-args0-loc switch0))
  (when (integerp switch1) (lam:k-mem-write-word-address k-test-bit-args1-loc switch1))



  (vc-initialize-memory-map)			;map code pages 0 and 1 and data pages 0 and 1.
  (when (and map-entry-for-nubus nubus-slot)
    (vc-map-nubus-memory map-entry-for-nubus nubus-slot))
  ;(global:fsignal "foo")
  (lam:falcon-set-pc-and-go  (vc-test-entry test)
			     :do-init t
			     :do-initialize-call-hardware t
			     :do-init-virtual-memory nil
			     :memory-control-register #x40000)  ;disable prom and traps off.
  (unless leave-running
    (if (and (null sleep-specified-p)
	     (global:get test 'default-sleep-time))
	(setq sleep-time (global:get test 'default-sleep-time)))
    (global:process-sleep sleep-time))
  (when (not (or loop-it leave-running leave-running-and-wait))
    (lisp:format t "~% The K Processor is ~:[running~;halted~]." (k-kbug:k-halted-p))
    (let ((pc (lam:k-stop))
	  (status (k-kbug:kbg-read-active 14.))
	  (a0 (k-kbug:kbg-read-active 0))
	  (a1 (k-kbug:kbg-read-active 1))
	  (a2 (k-kbug:kbg-read-active 2))
	  (a3 (k-kbug:kbg-read-active 3)))
     (vc-decode-pc pc)
;    (lisp:format t "~% *trap-temp1* = ")
;    (k-kbug:show-global 'k-gr:*trap-temp1*)
     (lisp:format t "~% *trap-temp2* (datatype traps) = ")
     (k-kbug:show-global 'k-gr:*trap-temp2*)
     (lisp:format t "~% A14 (fail status) = #x~x, A0 =#x~x A1 =#x~x A2 =#x~x  A3 =#x~x" status a0 a1 a2 a3)
    ))
  (when leave-running-and-wait
    (let ((pc (k-kbug:kbug-run-loop)))
      (global:format t "~%The Falcon stopped at #x~x" pc)))
  )

(defun vc-test-entry (test)
  (nc:ncompiled-function-starting-address (nc:nsymbol-function test)))

(defun vc-clear-k ()
  (lam:k-setup)
  (lam:k-reset)
  (lam:k-stop))

(defun vc-load-global-constant-frame ()
  (let* ((frame-num (nc:frame-num 'gr:constants))
	 (constants-frame (global:nth frame-num nc:*global-frames*)))
    (dolist (c (cdr constants-frame))
      (vc-load-global-constant c))))

(defun vc-load-global-constant (c)
  (let* ((variable (nc:variable-loc (nc:global-register c)))
	 (value (car (nc:rassoc variable nc:*global-constants* :test #'equal)))
	 (locn-list (lisp:get c :register))
	 (frame (second locn-list))
	 (offset (third locn-list))
	 (boxed 1))
    (cond ((numberp value))
	  ((global:memq value '(t nil))	;	;dont bomb on t and nil
	   (setq value 0))
	  ((listp value)
	   (if (eq (car value) 'hw:unboxed-constant)
	       (setq value (cadr value)
		     boxed 0)))
	  (t (ferror nil "Cant convert constant ~s" value)))
    (k-kbug:kbg-write-reg (global:dpb frame (global:byte 8 4) offset) value boxed)))

(defun vc-check-global-constant-frame (&optional and-print)
  (let* ((frame-num (nc:frame-num 'gr:constants))
	 (constants-frame (global:nth frame-num nc:*global-frames*)))
    (dolist (c (cdr constants-frame))
      (vc-check-global-constant c and-print))))

(defun vc-check-global-constant (c and-print)
  (let* ((variable (nc:variable-loc (nc:global-register c)))
	 (value (car (nc:rassoc variable nc:*global-constants* :test #'equal)))
	 (locn-list (lisp:get c :register))
	 (frame (second locn-list))
	 (offset (third locn-list))
	 (boxed 1)
	 (value-from-machine (k-kbug:kbg-read-reg-with-boxed (global:dpb frame (global:byte 8 4) offset))))
    (cond ((numberp value))
	  ((global:memq value '(t nil))	;	;dont bomb on t and nil
	   (setq value 0))
	  ((listp value)
	   (if (eq (car value) 'hw:unboxed-constant)
	       (setq value (cadr value)
		     boxed 0)))
	  (t (ferror nil "Cant convert constant ~s" value)))
    (setq value (cons (= boxed 1) value))
    (if (or and-print (not (equal value value-from-machine)))
	(global:format t "~%~S: should be #x~x is #x~x" c value value-from-machine))
    ))

(defun vc-download-trap-handlers ()
  ;; traps will simply loop at the same location
  (lam:write-inst k-test-trap-handler-loc (nc:assemble-inst `(k:jump ,k-test-trap-handler-loc nil))))

(defun vc-link-and-download-test-code (test)
  (let ((starting-address 0))
  ;link test code
    (setq starting-address (vc-link-and-increment 'k-test-setup starting-address))	;must be first.
    (setq starting-address (vc-link-and-increment test starting-address))
    (dolist (fctn (lisp:get test 'vc-support-functions))
      (when (null (nc:nsymbol-function fctn))
	(lisp:error "Test ~s requires support function ~s." test fctn))
      (setq starting-address (vc-link-and-increment fctn starting-address))))
  (let ((starting-address 0))
  ;link again and download
    (setq starting-address (vc-link-and-increment-and-download 'k-test-setup starting-address))
    (setq starting-address (vc-link-and-increment-and-download test starting-address))
    (dolist (fctn (lisp:get test 'vc-support-functions))
      (setq starting-address (vc-link-and-increment-and-download fctn starting-address))))
  (setq *last-test-downloaded* test))

(defun vc-link-and-increment (fctn starting-address)
  (nc:link fctn starting-address)
  (+ starting-address (nc:ncompiled-function-length (nc:nsymbol-function fctn))))

(defun vc-link-and-increment-and-download (fctn starting-address)
  (let ((next-sa (vc-link-and-increment fctn starting-address)))
    (vc-download-test-code fctn)
    next-sa))

(defun vc-download-test-code (test)
  (let* ((func (nc:nsymbol-function test))
	 (address (nc:ncompiled-function-starting-address func)))
    (dolist (inst (nc:ncompiled-function-code (nc:nsymbol-function test)))
      (lam:write-inst address inst)
      (incf address))
    (push func k-kbug:*loaded-functions*)))

(defun vc-initialize-memory-map ()
  ;map code pages 0 and 1 to physical pages 0 and 1
  (lam:falcon-write-map-and-check #x8000 #x8f)
  (lam:falcon-write-map-and-check #x8001 #x108f)
  ;map data pages 0 and 1 to physical pages 0 and 1
  (lam:falcon-write-map-and-check #x0 #x8f)
  (lam:falcon-write-map-and-check #x1 #x108f))


;;(defvar wired-array (si:%wire-structure (lisp:make-array (1- si:page-size))))


(defun vc-map-nubus-memory (map-entry-for-nubus nubus-slot)
  ;top 20 bits of NuBus address live in the memory map
  ;of these 20 bits (15 : 12) are the NuBus slot number
  ;of the remaining 12 low order bits which make up a 32 bit NuBus address
  ;10 are taken from the K's virtual memory address
  ;and the low 2 bits are zeros because the K addresses 32 bits words and the NuBus 8 bit bytes.
  (let ((map-value 0))
    (setq map-value (lisp:dpb k-hw:$$map-non-local    k-hw:%%map-local-memory-bit                 map-value))
    (setq map-value (lisp:dpb nubus-slot              k-hw:%%map-off-board-address-nubus-slot     map-value))
    (setq map-value (lisp:dpb #xF                     k-hw:%%map-off-board-address-nubus-constant map-value))
    (setq map-value (lisp:dpb k-hw:$$map-valid        k-hw:%%map-c-valid-bit                      map-value))
    (setq map-value (lisp:dpb k-hw:$$map-write-enable k-hw:%%map-c-write-enable-bit               map-value))
    (setq map-value (lisp:dpb k-hw:$$map-valid        k-hw:%%map-lisp-valid-bit                   map-value))
    (setq map-value (lisp:dpb k-hw:$$map-write-enable k-hw:%%map-lisp-write-enable-bit            map-value))
    (setq map-value (lisp:dpb k-hw:$$map-non-local    k-hw:%%map-local-memory-bit                 map-value))

;    ;; offset address up into the memory board a bit
;    (setq map-value (lisp:dpb #x100                   (lisp:byte 9 12)                            map-value))

    (lisp:format t "~&Map Entry = #x~x   Map Value = #x~x" map-entry-for-nubus map-value)
    (lam:falcon-write-map-and-check map-entry-for-nubus map-value)))


(defun vc-decode-pc (pc)
  (lisp:format t "~%PC = #x~x" pc)
  (cond ((<= pc k-test-last-trap-vector-loc)
	 (lisp:format t " is in the trap code or trap vectors."))
	((= pc k-test-entry-loc)
	 (lisp:format t " is the test entry location."))
	((or (= pc k-test-pass-exit-loc)
	     (and (>= pc k-test-pass-halt-loc)
		  (<  pc k-test-fail-halt-loc)))
	 (lisp:format t " The test passed."))
	((or (= pc k-test-fail-exit-loc)
	     (and (>= pc k-test-fail-halt-loc)
		  (<  pc k-test-code-loc)))
	 (lisp:format t " The test failed.")
	 (lisp:format t " ~&Expected value(result)  = #x~x" (lam:k-mem-read-word-address k-test-expected-value-loc))
	 (lisp:format t " ~&Incorrect value(status) = #x~x" (lam:k-mem-read-word-address k-test-incorrect-value-loc))
;	 (lisp:format t " ~&Address         = #x~x" (lam:k-mem-read-word-address k-test-address-loc))
	 )
	((= pc k-test-spare-exit-loc)
	 (lisp:format t " is the spare exit location."))
	((= pc k-test-trap-handler-loc)
	 (lisp:format t " Test took a trap."))
	((>= pc k-test-code-loc)
	 ;; This case can be improved to figure out which test we are in
	 ;; and to disassemble the test either from the K or the lamdba.
	 ;; To do this right requires keeping track of which tests are
	 ;; currently downloaded to the K.
	 (when (pc-in-function? pc *last-test-downloaded*)
	   (when (lisp:y-or-n-p " is in ~S.  Disassemble? " *last-test-downloaded*)
	     (vc-disassemble-function *last-test-downloaded*)
	     ;(k-kbug:disassemble-fcn-from-memory *last-test-downloaded*)
	     )
	   (return-from vc-decode-pc))
	 (dolist (f (lisp:get *last-test-downloaded* 'vc-support-functions))
	   (when (pc-in-function? pc f)
	     (when (lisp:y-or-n-p " is in ~S.  Disassemble? " f)
	       (vc-disassemble-function f)
	       ;(k-kbug:disassemble-fcn-from-memory f)
	       )
	     (return-from vc-decode-pc)))
	 (lisp:format t " is not in the test function or its support functions."))
	(t
	 (lisp:error "Should never get here."))))

(defun pc-in-function? (pc function)
  (setq function (nc:nsymbol-function function))
  (and (>= pc (nc:ncompiled-function-starting-address function))
       (<  pc (+ (nc:ncompiled-function-starting-address function)
		 (nc:ncompiled-function-length function)))))

(defun vc-disassemble-function (function)
  (global:format t "~%~s:~%" function)
  (setq function (nc:nsymbol-function function))
  (let ((start (nc:ncompiled-function-starting-address function))
	(length (nc:ncompiled-function-length function)))
    (k-kbug:kbug-disassemble start length)))


;;;****************************************************************
;;;
;;; K Test Setup Code
;;;
;;;****************************************************************

(eval-when (compile load eval)

(defvar trap-restore-test-status #x0f0f)
(defvar trap-restore-test-result #x14fc0000)

)	;end eval-when

;; this code is designed to work in conjuction with the defvars in the memory map above
;; the exact number of instructions is #x60

(defafun k-test-setup ()
trap  ;; #x00
  (alu setl gr::*save-oreg* r0 r0 bw-32 boxed-left)
  (alu setl gr::*save-left* r0 r0 bw-32 boxed-left)
  (alu setr gr::*save-right* r0 r0 bw-32 boxed-right)
  (alu pass-status gr::*save-status* r0 r0 bw-32 unboxed)
  (alu-field extract-bit-right gr::*save-jcond* r0 processor-status (byte 1. (+ 32. 17.)) unboxed)
  (alu-field field-and gr::*save-trap* gr::*trap-mask* trap-register (byte 31. 0.) unboxed)
  (alu prioritize-r gr::*trap-temp1* r0 gr::*save-trap* bw-32 unboxed)
  (alu-field set-bit-right gr::*trap-temp1* r0 gr::*trap-temp1* (byte 1. 5.) unboxed)
  (alu merge-r gr::*save-trap-pc*  gr::*trap-dtp-code-5* trap-pc bw-24 boxed)
  (alu merge-r gr::*save-trap-pc+* gr::*trap-dtp-code-5* trap-pc+  bw-24 boxed next-pc-dispatch)
  (nop)
  (nop)
non-modifying-exit  ;; #x0c
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
modifying-exit ;; #x14
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
diagnostic-trap-exit ;;  #x1c
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch)
  (nop)
  ;; trap-vector-table    #x20
trap-vector-reset					;Bit 31 - addr 32. - Highest priority
  (unconditional-branch reset-trap-handler ())
trap-vector-trace  					;Bit 30 - addr 33.
  (unconditional-branch trace-trap-handler ())
trap-vector-icache-parity				;Bit 29 - addr 34.
  (unconditional-branch icache-parity-trap-handler ())
trap-vector-icache-nubus-err				;Bit 28 - addr 35.
  (unconditional-branch icache-nubus-error-trap-handler ())
trap-vector-icache-nubus-timeout		 	;Bit 27 - addr 36.
  (unconditional-branch icache-nubus-timeout-trap-handler ())
trap-vector-icache-page-fault				;Bit 26 - addr 37.
  (unconditional-branch icache-map-fault-trap-handler ())
trap-vector-proc-mread-parity				;Bit 25 - addr 38.
  (unconditional-branch memory-read-parity-trap-handler ())
trap-vector-proc-mread-nubus-err		 	;Bit 24 - addr 39.
  (unconditional-branch memory-read-nubus-error-trap-handler ())
trap-vector-proc-mread-nubus-timeout			;Bit 23 - addr 40.
  (unconditional-branch memory-read-nubus-timeout-trap-handler ())
trap-vector-proc-mread-page-fault			;Bit 22 - addr 41.
  (unconditional-branch memory-read-page-fault-trap-handler ())
trap-vector-proc-mread-transporter			;Bit 21 - addr 42.
  (unconditional-branch memory-read-transporter-trap-handler ())
trap-vector-proc-mwrite-nubus-err			;Bit 20 - addr 43.
  (unconditional-branch memory-write-nubus-error-trap-handler ())
trap-vector-proc-mwrite-nubus-timeout			;Bit 19-  addr 44.
  (unconditional-branch memory-write-nubus-timeout-trap-handler ())
trap-vector-proc-mwrite-page-fault			;Bit 18 - addr 45.
  (unconditional-branch memory-write-page-fault-trap-handler ())
trap-vector-proc-mwrite-gc				;Bit 17 - addr 46.
  (unconditional-branch memory-write-gc-trap-handler ())
trap-vector-floating-point				;Bit 16 - addr 47.
  (unconditional-branch floating-point-trap-handler ())
trap-vector-heap-empty					;Bit 15 - addr 48.
  (unconditional-branch heap-empty-trap-handler ())
trap-vector-instruction-bit				;Bit 14 - addr 49.
  (unconditional-branch instruction-trap-handler ())
trap-vector-datatype					;Bit 13 - addr 50.
  (unconditional-branch datatype-trap-handler ())
trap-vector-overflow					;Bit 12 - addr 51.
  (unconditional-branch overflow-trap-handler ())
trap-vector-spare11					;Bit 11 - addr 52.
  (unconditional-branch spare11-trap-handler ())
trap-vector-interrupt7					;Bit 10 - addr 53.
  (unconditional-branch debugger-trap-handler ())
trap-vector-interrupt6					;Bit 09 - addr 54.
  (unconditional-branch interrupt6-trap-handler ())
trap-vector-interrupt5					;Bit 08 - addr 55.
  (unconditional-branch interrupt5-trap-handler ())
trap-vector-interrupt4					;Bit 07 - addr 56.
  (unconditional-branch iop-trap-handler ())
trap-vector-interrupt3					;Bit 06 - addr 57.
  (unconditional-branch interrupt3-trap-handler ())
trap-vector-interrupt2					;Bit 05 - addr 58.
  (unconditional-branch interrupt2-trap-handler ())
trap-vector-interrupt1					;Bit 04 - addr 59.
  (unconditional-branch interrupt1-trap-handler ())
trap-vector-interrupt0					;Bit 03 - addr 60.
  (unconditional-branch interrupt0-trap-handler ())
trap-vector-timer-1024					;Bit 02 - addr 61.
  (unconditional-branch timer-1024-trap-handler ())
trap-vector-timer-16384					;Bit 01 - addr 62.
  (unconditional-branch timer-16384-trap-handler ())
trap-vector-spurious					;Bit 00 - addr 63.
  (unconditional-branch spurious-trap-handler ())


;; 16 locations reserved for use by the test code
;; see the memory map above for constants used to reference these locations

  (nop)						; #x40
  (nop)						; #x41
  (nop)						; #x42
  (nop)						; #x43
  (nop)						; #x44
  (nop)						; #x45
  (nop)						; #x46
  (nop)						; #x47
  (nop)						; #x48
  (nop)						; #x49
  (nop)						; #x4a
  (nop)						; #x4b
  (nop)						; #x4c
  (nop)						; #x4d
  (nop)						; #x4e
  (nop)						; #x4f

;; 16 locations reserved for use by trap handlers
;; the basic model of K-TEST-SETUP has all traps branch to the same location
;; these are the labels that traps vector to

  (nop)						; #x50
  (nop)						; #x51
  (nop)						; #x52
  (nop)						; #x53
;datatype-trap-handler
  (alu l+1 gr:*trap-temp2* gr:*trap-temp2* ignore)     ; #x54
  (movei   gr:*save-right* #.trap-restore-test-result unboxed)      ; #x55
  (movei   gr:*save-status* #.trap-restore-test-status unboxed)     ; #x56
  (unconditional-branch modifying-exit ())	; #x57
  (nop)						; #x58
  (nop)						; #x59
  (nop)						; #x5a
  (nop)						; #x5b
  (nop)						; #x5c
  (nop)						; #x5d
  (nop)						; #x5e
  (nop)						; #x5f
pass-halt-zone
  (alu-field set-bit-right			; #x60
	     processor-control
	     r0 processor-control
	     hw:%%processor-control-halt-processor)
  (nop)						; #x61
  (nop)						; #x62
  (nop)						; #x63
  (nop)						; #x64
  (nop)						; #x65
  (nop)						; #x66
  (nop)						; #x67
  (nop)						; #x68
  (nop)						; #x69
  (nop)						; #x6a
  (nop)						; #x6b
  (nop)						; #x6c
  (nop)						; #x6d
  (nop)						; #x6e
  (unconditional-branch pass-halt-zone ())	; #x6f
fail-halt-zone
  (alu-field set-bit-right			; #x70
	     processor-control
	     r0 processor-control
	     hw:%%processor-control-halt-processor)
  (nop)						; #x71
  (nop)						; #x72
  (nop)						; #x73
  (nop)						; #x74
  (nop)						; #x75
  (nop)						; #x77
  (nop)						; #x77
  (nop)						; #x78
  (nop)						; #x79
  (nop)						; #x7a
  (nop)						; #x7b
  (nop)						; #x7c
  (nop)						; #x7d
  (nop)						; #x7e
  (unconditional-branch fail-halt-zone ())	; #x7f


 reset-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o0)
 trace-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o1)
 icache-parity-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o2)
 icache-nubus-error-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o3)
 icache-nubus-timeout-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o4)
 icache-map-fault-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o5)
 memory-read-parity-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o6)
 memory-read-nubus-error-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o7)
 memory-read-nubus-timeout-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o8)
 memory-read-page-fault-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o9)
 memory-read-transporter-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o10)
 memory-write-nubus-error-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o11)
 memory-write-nubus-timeout-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o12)
 memory-write-page-fault-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o13)
 memory-write-gc-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o14)
 floating-point-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop o15)
 heap-empty-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r0)
 instruction-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r1)
 datatype-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r2)
 overflow-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r3)
 spare11-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r4)
 debugger-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r5)
 interrupt6-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r6)
 interrupt5-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r7)
 iop-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r8)
 interrupt3-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r9)
 interrupt2-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r10)
 interrupt1-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r11)
 interrupt0-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r12)
 timer-1024-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r13)
 timer-16384-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r14)
 spurious-trap-handler 
  (move r0 processor-control)
  (alu-field set-bit-right processor-control r0 processor-control hw:%%processor-control-halt-processor)(nop)(nop)(nop)
  (move nop r15)


  )


;;;****************************************************************
;;;
;;;  DEF-K-TESTs
;;;
;;;****************************************************************

;;; See K-SYS:KBUG;TEST-VECTORS-SUPPORT.LISP for lisp code
;;; runtime support functions which must be compiled.
;;; Use Super-Shift-C to compile support functions.

(defun build-OAR (frame-num)
  (lisp:dpb frame-num  k-hw:%%ch-oar-open
	    (lisp:dpb frame-num k-hw:%%ch-oar-active
		      (lisp:dpb frame-num k-hw:%%ch-oar-return 0))))

(defconstant %%reg-test-frame-number    (lisp:byte 8 4))
(defconstant %%reg-test-register-offset (lisp:byte 4 0))

(defun build-test-value (frame-num register-offset)
  (lisp:dpb frame-num %%reg-test-frame-number
	    (lisp:dpb register-offset %%reg-test-register-offset 0)))

(defun dummy-left (test-case)
  (lisp:first test-case))

(defun dummy-right (test-case)
  (lisp:second test-case))

(defun left (test-case)
  (lisp:third test-case))

(defun right (test-case)
  (lisp:fourth test-case))

;; we will use the history ram to determine which instruction failed

;** incomplete
;(defmacro def-register-test (name)
;  (let ((max-global-frames    (k-vinc:field-maximum k-hw:%%i-global-frame))
;	(max-register-offset  4)   ;(k-vinc:field-maximum k-hw:%%i-reg-offset)
;	(instructions         '())
;	dummy-register-offset
;	dummy-G-reg dummy-O-reg dummy-A-reg dummy-R-reg
;	G-reg O-reg A-reg R-reg)
       
;       (dolist (pass '(:write :test))
	 
;	 (dolist (frame-num   '(4) )		;(k-vinc:field-maximum k-hw:%%ch-oar-open)	;256 frames
	   
;	   (setq instructions
;		 `(,@instructions
;		   (MOVEI OPEN-ACTIVE-RETURN (HW:UNBOXED-CONSTANT ,(build-OAR frame-num)))))
	   
;	   (dotimes (register-offset max-register-offset)
	     
;	     (when (eq pass :write)
	       
;	       (setq O-reg (lisp:intern (lisp:format nil "O~d" register-offset) 'k))
;	       (setq instructions
;		     `(,@instructions
;		       (MOVEI ,O-reg (HW:UNBOXED-CONSTANT ,(build-test-value frame-num register-offset))))))
	     
;	     (when (eq pass :test)
;	       (when (< frame-num max-global-frames)
;		 (setq dummy-G-reg `(register foo ,frame-num ,(lisp:logxor register-offset #b1111)))
;		 (setq G-reg `(register foo ,frame-num ,register-offset)))
;	       (setq dummy-O-reg (lisp:intern (lisp:format nil "O~d" ,(lisp:logxor register-offset #b1010)) 'k))
;	       (setq dummy-A-reg (lisp:intern (lisp:format nil "A~d" ,(lisp:logxor register-offset #b0101)) 'k))
;	       (setq dummy-R-reg (lisp:intern (lisp:format nil "R~d" ,(lisp:logxor register-offset #b0110)) 'k))
;	       (setq O-reg (lisp:intern (lisp:format nil "O~d" register-offset) 'k))
;	       (setq A-reg (lisp:intern (lisp:format nil "A~d" register-offset) 'k))
;	       (setq R-reg (lisp:intern (lisp:format nil "R~d" register-offset) 'k))
	       
;	       (dolist (regs `(;;test left and right banks
;			       ,@(when (< frame-num max-global-frames)
;				   `((,dummy-G-reg ,dummy-G-reg ,G-reg ,G-reg)))
;			       (,dummy-O-reg ,dummy-O-reg ,O-reg ,O-reg)
;			       (,A-reg  ,A-reg)
;			       (,R-reg  ,R-reg)
;			       ;;test different addressing modes
;			       ,@(when (< frame-num max-global-frames)
;				   `((,G-reg  ,O-reg)
;				     (,G-reg  ,A-reg)
;				     (,G-reg  ,R-reg)))
;			       (,O-reg  ,A-reg)
;			       (,O-reg  ,R-reg)
;			       (,A-reg  ,R-reg)))
		 
;		 (setq instructions `(,@instructions
;				      (ALU XOR NOP ,(left regs) ,(right regs))
;				      (TEST BR-NOT-EQUAL)
;				      (BRANCH FAIL ())))))
;	     )))
       
;       `(DEF-K-TEST ,name ()
;		    ()
;	  ,@instructions
;	  PASS
;	  (JUMP #.k-test-pass-exit-loc ())
;	  FAIL
;	  (JUMP #.k-test-fail-exit-loc ()))
       
;       ))



;(def-register-test register-read-test)


(defmacro def-fdest-test (fdest value)
  `(def-k-test ,(intern (lisp:format nil "~A-TEST" fdest)) () ()
    (movei a14 0)				;exit status
    (movei a1 (hw:unboxed-constant ,value))
    (move ,fdest a1 unboxed)
    (nop)
    (nop)
    (move a2 ,fdest unboxed)
    (alu xor nop a1 a2 bw-32)
    (test br-not-equal)
    (branch fail () )
   pass
    (movei a14 1)
    (jump #.k-test-pass-exit-loc ())
   fail
    (movei a14 2)
    (jump #.k-test-fail-exit-loc ())
    )
  )


(def-fdest-test MEMORY-CONTROL    #xaaaaaaaa)
(def-fdest-test BUS-CONTROL       #xaaaaaaaa)
(def-fdest-test MICROSECOND-CLOCK #xaaaaaaaa)
(def-fdest-test PROCESSOR-CONTROL #xaaaaaaaa)


;; these are the functional destinations that are also functional sources  
;(def-fdest K:MEMORY-MAP                  hw:$$i-fd-memory-map)
;(def-fdest K:GC-RAM                      hw:$$i-fd-gc-ram)
;(def-fdest K:MEMORY-CONTROL              hw:$$i-fd-memory-control-register)
;(def-fdest K:BUS-CONTROL                 hw:$$i-fd-bus-control-register)
;(def-fdest K:MICROSECOND-CLOCK           hw:$$i-fd-microsecond-clock)
;(def-fdest K:PROCESSOR-CONTROL           hw:$$i-fd-processor-control-register)
;(def-fdest K:OPEN-ACTIVE-RETURN          hw:$$i-fd-call-hardware-o-a-r)
;(def-fdest K:RETURN-PC-RETURN-DEST       hw:$$i-fd-return-pc-return-dest)
;(def-fdest K:CALL-SP-HP                  hw:$$i-fd-call-hardware-hp-sp)
  


(def-k-test tranporter-trap-test ()
	    (vc-write-md-generic vc-vma-start-read-generic vc-store-into-transporter-ram vc-write-transporter-ram
	     vc-load-transporter-ram-pattern)
  )


(eval-when (compile load eval)
(defconstant vc-map-cluster (ash 2 10))
(defconstant vc-map-value   #x0)
) ;end eval-when

(def-k-test wipe-map ()
	    ()
  (movei a14 0)					;exit status
  (movei a1 #.vc-map-value)			;value to write in map
  (movei a2 #.(lisp:ash 1 10.))			;increment to next cluster
  (movei a3 #xffffffff)
  (movei a6 (hw:unboxed-constant #x2000000))	;first address past "data half" of map.
outter-loop
  (movei a0 0)					;cluster to map
loop
  (move vma a0 unboxed-vma)
  (nop)
  (nop)
  (move memory-map a1 unboxed)
  (move memory-map a1 unboxed)
  (nop)
  (nop)
  (alu l+r a0 a0 a2 bw-32 unboxed)
  (alu xor nop a0 a6)
  (test br-not-equal)
  (branch loop () )
  (alu xor a1 a1 a3 bw-32 unboxed)		;write the other thing.
  (unconditional-branch outter-loop ())
pass
  (movei a14 1)
  (jump #.k-test-pass-exit-loc ())
  )



(def-k-test map-data-independance-test ()
; write one map location with something, then scan thru wiping everything else.
; at end, check original location is still there.
	    ()
  (movei a1 #.vc-map-value)			;value to write in map
  (movei a2 #.(lisp:ash 1 10.))			;increment to next cluster
  (movei a3 #xffffffff)
  (movei a6 (hw:unboxed-constant #x2000000))	;first address past "data half" of map.
  (movei a14 0)					;check data
outter-check-loop
  (movei a15 0)					;map location to distinguish
check-loop
  (move vma a15 unboxed-vma)
  (nop)
  (nop)
  (nop)
  (move memory-map a14 unboxed)			;write check data
  (move memory-map a14 unboxed)			;write check data
  (nop)		;avoid mmfio collision
  (movei a0 0)					;cluster to map
loop
  (move vma a0 unboxed-vma)
  (alu xor nop a0 a15 bw-32 unboxed)
  (test br-equal)
  (branch next-loop ())		;dont clobber locn to distinguish
  (move memory-map a1 unboxed)
  (move memory-map a1 unboxed)
  (nop)
  (nop)
next-loop
  (alu l+r a0 a0 a2 bw-32 unboxed)
  (alu xor nop a0 a6)
  (test br-not-equal)
  (branch loop () )
  (alu xor a1 a1 a3 bw-32 unboxed)		;alternate background data.
  (move vma a15 unboxed-vma)
  (nop)
  (nop)
  (nop)
  (move a13 memory-map unboxed)
  (alu xor nop a13 a14)
  (test br-not-equal)
  (branch fail ())
  (alu r+1 a14 a14 a14 bw-32 unboxed)		;increment check data.
  (alu l+r a15 a15 a2 bw-32 unboxed)		;increment distinguished locn
  (alu xor nop a15 a6)
  (test br-not-equal)
  (branch check-loop ())
  (unconditional-branch outter-check-loop ())
  
fail
  (jump #.k-test-fail-halt-loc ())

pass
  (movei a14 1)
  (jump #.k-test-pass-exit-loc ())
  )


(def-k-test read-map-loop ()	;just sits in a loop reading map at high speed.
	    ()
  (movei a14 0)					;exit status
  (movei a2 #.(lisp:ash 1 10))			;increment to next cluster
  (movei a6 (hw:unboxed-constant #x4000000))	;first address past map.
outter-loop
  (movei a0 0)					;cluster to map
loop
  (move vma a0 unboxed-vma)
  (nop)
  (nop)
  (nop)
  (move a1 memory-map unboxed)
  (nop)
  (nop)
  (alu l+r a0 a0 a2 bw-32 unboxed)
  (alu xor nop a0 a6)
  (test br-not-equal)
  (branch loop () )
  (unconditional-branch outter-loop ())
pass
  (movei a14 1)
  (jump #.k-test-pass-exit-loc ())
  )


(def-k-test memory-map-ones-and-zeros ()
	    ()
  (movei a14 0)					;exit status
  (movei a1 0)					;value to write in map
  (movei a2 #.(lisp:ash 1 10))			;increment to next cluster
  (movei a3 (hw:unboxed-constant #xffffffff))	;used to switch between ones and zeros
  (movei a4 0)					;skip vma cluster zero
  (movei a5 (hw:unboxed-constant #x2000000))	;skip code cluster zero
  (movei a6 (hw:unboxed-constant #x4000000))	;first address past map.
zeros-ones
  (movei a0 0)					;cluster to map
  (alu xor a1 a1 a3 bw-32 unboxed)		;switch between ones and zeros
loop
  (alu xor nop a0 a4 bw-32 unboxed)		;don't map vma cluster zero
  (test br-equal)
  (branch skip ())
  (alu xor nop a0 a5 bw-32 unboxed)		;don't map code cluster zero
  (test br-equal)
  (branch skip ())
  (move vma a0 unboxed-vma)
  (nop)
  (nop)
  (move memory-map a1 unboxed)
  (move memory-map a1 unboxed)
  (nop)
  (nop)
skip
  (alu l+r a0 a0 a2 bw-32 unboxed)
  (alu xor nop a0 a6)
  (test br-not-equal)
  (branch loop () )
  (nop)
  (nop)
  (unconditional-branch zeros-ones ())
  )



(def-k-test memory-map-test ()
	    ()
  (movei a14 0)					;exit status
  (movei a0 #.vc-map-cluster)			;cluster to map
  (movei a1 #.vc-map-value)			;value to write in map
  (move vma a0 unboxed-vma)
  (nop)
  (nop)
  (nop)
  (move memory-map a1 unboxed)
  (move memory-map a1 unboxed)
  (nop)
  (nop)
  (move vma a0 unboxed-vma)
  (nop)
  (nop)
  (nop)
  (move a2 memory-map)
  (nop)
  (nop)
  (alu xor nop a1 a2)
  (test br-not-equal)
  (branch fail () )
pass
  (movei a14 1)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei a14 2)
  (jump #.k-test-fail-exit-loc ())
  )
  
  
  

(global:defprop trap-restore-test 1000. default-sleep-time)	;it really takes a while to load the dt ram especially
					;with cache off.
(def-k-test trap-restore-test ()
  (write-dt-ram write-dt-ram-dispatch load-dt-ram-pattern
   load-initial-datatype-ram
   vc-trap-on initialize-call-hardware )  ;vc-dt-and-ovf-trap-handler

  ;; Global Registers set up by spy initialization.
  ;; Traps turned off by spy initialization.
  ;;
  (open-call initialize-call-hardware ignore ())
  (open-call load-initial-datatype-ram ignore ())
  (movei processor-control #x0)		;disable flt pt trap, call hardware stack ovflo, all caches
  (movei gr:*trap-mask* #x2000)		;datatype trap is only one we expect.
  (movei memory-control #x90040000)	;master trap on, datatype trap on, boot-prom off.
;  (open-call vc-trap-on r0 ())
  (movei a0 #x14fa0000 boxed)
  (movei a1 #x14fa0000 boxed)
  (movei a2 0)
  (movei a3 0)
  (movei gr:*trap-temp2* 0)		;counter of number datatype traps processed.
  (movei a14  0)			;initialize exit status to zero.
  ;; should trap on this instruction
  ;; and trap handler and modifying exit should set status register and result
  (alu l+r a2 a1 a0 bw-24 boxed dt-both-fixnum-with-overflow)
  ;;
  (alu pass-status a3 r0 r0)
;  (movei a4 #.trap-restore-test-status)      ;;  if these lines are not in
;  (alu xor nop a3 a4)                        ;;  we see the move work
;  (test br-not-equal)                        ;;  but not the branch
;  (branch fail (move a14 gr:*one*))          ;;  (on the new board set)
  (movei a4 #.trap-restore-test-result)
  (alu xor nop a2 a4)
  (test br-not-equal)
  (branch fail (move a14 gr:*two*))
pass
   (movei a14 0)
;  (movei md #.k-test-passed)
;  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
;  (movei md #.k-test-failed)
;  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
;  (nop)
;  (move md a2)
;  (movei vma-start-write-no-gc-trap #.k-test-expected-value-loc unboxed-vma)
;  (nop)
;  (move md a3)
;  (movei vma-start-write-no-gc-trap #.k-test-incorrect-value-loc unboxed-vma)
 ;begin halt procedure!
  ;; Get processor control to a convenient place.
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (movei memory-control #x40000)	   ;master trap off, 
  (nop)
  (nop)

;  (alu-field set-bit-right processor-control r0 processor-control
;	     hw:%%processor-control-halt-processor)
;  (movei processor-control #x80)	;foo halt already..
; foo  (unconditional-branch foo ())
  (nop)
  (nop)
  (nop)
  (nop)
  (jump #.k-test-fail-exit-loc ())

)


(def-k-test open-call-next-pc-pc+1-test ()
	    (initialize-call-hardware )

  ;; Global Registers set up by spy initialization.
  ;; Traps turned off by spy initialization.
  ;;
  (open-call initialize-call-hardware ignore ())
  (movei processor-control #x0)		;disable flt pt trap, call hardware stack ovflo, all caches
  (movei return-pc-return-dest #x12345)
  (nop)						;timing
  (open-call (0 0) ignore () next-pc-pc+1)	;due to pipelining, data stored in return-pc-.. gets written here.
  (move return r0 ch-return)			;note: this does not generate next-pc-return.
 ;begin halt procedure!
  ;; Get processor control to a convenient place.
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
;  (alu-field set-bit-right processor-control r0 processor-control
;	     hw:%%processor-control-halt-processor)
  (movei processor-control #x80)	;foo halt already..
; foo  (unconditional-branch foo ())
  (nop)
  (nop)
  (nop)
  (nop)
  (jump #.k-test-fail-exit-loc ())

)

(def-k-test nubus-test ()
	    ()
  (movei vma-start-read-no-transport (hw:unboxed-constant #x400))
  (memory-wait)
  (move a4 md)
  (movei a6 #xaa)
  (alu xor nop a4 a6 bw-32 unboxed)
  (test br-not-equal)
  (branch fail ())
pass
  (movei md #.k-test-passed)
  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )



;; k-test-word-arg0-loc contains starting address
;; k-test-word-arg1-loc contains number of locations to test as a power of two
;; when running this test make sure the memory map is set up to include the
;; locations being tested or you may write over the code in page zero.
;; should we check for overflow?
(def-k-test mem-test-address ()
	    ()
  (movei a5 0)					;a5: rotate loop count
  (movei a7 32.)				;a7: rotate loop limit
shift-loop
  (movei vma-start-read #.k-test-word-arg0-loc)
  (memory-wait)
  (move a0 md unboxed-md)			;a0: starting address
  (movei vma-start-read #.k-test-word-arg1-loc)
  (memory-wait)
  (move a1 md unboxed-md)			;a1: log base 2 number of locations
  (movei a2 1 unboxed)
  (alu load-status-l nop a1 ignore)		;setup for shift
  (alu-field nb-shift-0f-l a2 a2 ignore 0 pw-ri);number of locations to test
  (move a3 a0)					;a3: address to write to
  (alu l+r a2 a3 a2 unboxed)			;a2: first location not to test
  ;;
write-loop
  (alu load-status-l nop a5 ignore)			;setup for shift
  (alu-field rotate-l md a3 ignore 0 pw-ri)
  (move vma-start-write a3)
  (alu l+1 a3 a3 ignore)
  (alu xor nop a3 a2)
  (test br-not-equal)
  (branch write-loop ())
  ;;
read-loop
  (move vma-start-read a0)			;a0: address to read from
  (memory-wait)
  (move a4 md)					;a4: data read back
  (alu load-status-l nop a5 ignore)			;setup for shift
  (alu-field rotate-l a6 a0 ignore 0 pw-ri)	;a6: expected value
  (alu xor nop a4 a6 bw-32 unboxed)
  (test br-not-equal)
  (branch fail ())
  (alu l+1 a0 a0 ignore)
  (alu xor nop a0 a2)
  (test br-not-equal)
  (branch read-loop ())
  ;;
  (alu l+1 a5 a5 ignore)			;increment rotate count
  (alu xor nop a5 a7)
  (test br-not-equal)
  (branch shift-loop ())			;do next shift loop
pass
  (movei md #.k-test-passed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (move md a0)
  (movei vma-start-write #.k-test-address-loc unboxed-vma)
  (move md a6)
  (movei vma-start-write #.k-test-expected-value-loc unboxed-vma)
  (move md a4)
  (movei vma-start-write #.k-test-incorrect-value-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )


(def-k-test mem-test-floating-bit ()
	    ()
  (movei a3 0 unboxed)
  (movei a4 1000 boxed)
  (movei a0 32.)
 loop
  (alu load-status-r nop a0 a0 bw-8)
  (alu-field set-bit-left a1 a3 ignore 0 pw-ri)
  (move md a1)
  (move vma-start-write a4 unboxed-md)
  (memory-wait)
  (movei md #xaaaaaaaa unboxed)			;set memory-data to impossible value
  (move vma-start-read a4 unboxed-md)
  (memory-wait)
  (move a2 md)
  (alu xor nop a1 a2 bw-32 unboxed)
  (test br-not-equal)
  (branch fail ())
  (alu l-1 a0 a0 ignore bw-8)
  (test br-not-zero)
  (branch loop ())
 pass
  (movei md #.k-test-passed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
 fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (move md a4)
  (movei vma-start-write #.k-test-address-loc unboxed-vma)
  (move md a1)
  (movei vma-start-write #.k-test-expected-value-loc unboxed-vma)
  (move md a2)
  (movei vma-start-write #.k-test-incorrect-value-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )


(def-k-test vc-memory-write-count-loop ()
	    ()
    ;counts in processor, and writes count to memory.  Always loops, never returns.
    ;use (lam:k-mem-read-word-address #o400) to see word being counted in memory.
  (movei a3 0 unboxed)		;data to write to initialize memory counter.
 loop
  (movei a4 #o400 bw-24 unboxed)		;address to be hacked.
  (move md a3 unboxed)
  (move vma-start-write-no-gc-trap a4 unboxed-md)
  (memory-wait)
  (alu l+1 a3 a3 ignore bw-32 dt-none)
  (unconditional-branch loop ())
  )

(def-k-test vc-test-branch-move ()
	    ()
  (movei gr:*zero* 0. boxed)		;initialize standard constants
  (movei gr:*one* 1 boxed)
  (movei gr:*minus-one* -1. boxed)
  (movei gr:*two* 2. boxed)
  (movei gr:*T*  t boxed)
  (movei gr:*NIL*  nil boxed)
  (movei gr:*all-zero* 0 unboxed)
  (movei gr:*all-ones* (hw:unboxed-constant #xffffffff) unboxed)
  (movei gr:*three* 3. boxed)
  (movei gr:*four* 4. boxed)
  (movei gr:*five* 5. boxed)
  (movei gr:*six* 6. boxed)
  (movei gr:*seven* 7. boxed)
  (movei gr:*eight* 8. boxed)
  (movei gr:*nine* 9. boxed)
  (movei gr:*ten* 10. boxed)
  (unconditional-branch test1 (move a14 gr:*one*))		;traps enabled?
  (unconditional-branch fail ())				;branch failed to jump.
  test1 (movei a0 1)
        (alu xor nop a14 a0 bw-32 unboxed)
  	(test br-not-zero)
	(branch fail ())
  pass	(movei a0 1)
	(jump #.k-test-pass-exit-loc ())
  fail
	(jump #.k-test-fail-exit-loc ())
	)

(def-k-test vc-test-read-trap-enable-and-disable ()	;rte-and-d
	    ()
  (movei gr:*zero* 0. boxed)		;initialize standard constants
  (movei gr:*one* 1 boxed)
  (movei gr:*minus-one* -1. boxed)
  (movei gr:*two* 2. boxed)
  (movei gr:*T*  t boxed)
  (movei gr:*NIL*  nil boxed)
  (movei gr:*all-zero* 0 unboxed)
  (movei gr:*all-ones* (hw:unboxed-constant #xffffffff) unboxed)
  (movei gr:*three* 3. boxed)
  (movei gr:*four* 4. boxed)
  (movei gr:*five* 5. boxed)
  (movei gr:*six* 6. boxed)
  (movei gr:*seven* 7. boxed)
  (movei gr:*eight* 8. boxed)
  (movei gr:*nine* 9. boxed)
  (movei gr:*ten* 10. boxed)

  ;make sure traps are disabled
       (movei a0 0 unboxed)
       (move a1 memory-control unboxed)
       (alu xor nop a0 a1 bw-32 unboxed)
       (test br-negative)
       (branch fail (move a14 gr:*one*))			;traps enabled?
  ;make sure rte-and-d shows disabled
       (move a2 trap-off unboxed)       	;reads trap state to low order bit
       (alu-field extract-bit-right a3 ignore a2 (byte 1. 0.) unboxed)
       (test br-not-zero)
       (branch fail (move a14 gr:*two*))
  ;make sure still disabled
       (movei a0 0 unboxed)
       (move a1 memory-control unboxed)
       (alu xor nop a0 a1 bw-32 unboxed)
       (test br-negative)
       (branch fail (move a14 gr:*three*))			;traps enabled?
  ;state marker in case it traps from here.
       (movei a0 2 unboxed)
       (movei memory-control #.(ash 1 31.) unboxed)		;#.(ash 1 31.)
  ;       (nop)
  ;       (nop)
  ;       (nop)
  ;traps should be enabled now.
       (movei a0 0 unboxed)
       (move a1 memory-control unboxed)
       (alu xor nop a0 a1 bw-32 unboxed)
       (test br-not-negative)
       (branch fail (move a14 gr:*four*))			;traps not enabled?
  ;make sure rte-and-d shows they were enabled.
       (move a2 trap-off unboxed)       	;reads trap state to low order bit
       (alu-field extract-bit-right a3 ignore a2 (byte 1. 0.) unboxed)
       (test br-zero)
       (branch fail (move a14 gr:*five*))	;trap-off failed to read that traps were previously enabled.
  ;should be disabled now.
       (movei a0 0 unboxed)
       (move a1 memory-control unboxed)
       (alu xor nop a0 a1 bw-32 unboxed)
       (test br-negative)
       (branch fail (move a14 gr:*six*))			;traps enabled?
       
  pass
	(movei a0 1)
	(jump #.k-test-pass-exit-loc ())
  fail
	(jump #.k-test-fail-exit-loc ())


       (move a1 trap-off)
  ;make sure traps stay disabled
  ;enable traps
  ;make sure rte-and-d shows enabled and disables.
  )


(def-k-test vc-register-initialize-loop ()
	    ()
  loop
  (movei gr:*zero* 0. boxed)		;initialize standard constants
  (movei gr:*one* 1 boxed)
  (movei gr:*minus-one* -1. boxed)
  (movei gr:*two* 2. boxed)
  (movei gr:*T*  t boxed)
  (movei gr:*NIL*  nil boxed)
  (movei gr:*all-zero* 0 unboxed)
  (movei gr:*all-ones* (hw:unboxed-constant #xffffffff) unboxed)
  (movei gr:*three* 3. boxed)
  (movei gr:*four* 4. boxed)
  (movei gr:*five* 5. boxed)
  (movei gr:*six* 6. boxed)
  (movei gr:*seven* 7. boxed)
  (movei gr:*eight* 8. boxed)
  (movei gr:*nine* 9. boxed)
  (movei gr:*ten* 10. boxed)
  (unconditional-branch loop ()))


(def-k-test read-cdr-test ()
	    ()
  (movei a0 #x200)
  (movei a1 #x201)
  (move md a0)
  (move vma-start-write a0)
  (memory-wait)
  (move md a1)
  (move vma-start-write a1)
  (memory-wait)
  (movei md #xaaaa)
  (move vma-start-read-cdr a0)
  (memory-wait)
  (alu xor nop a1 md)
  (test br-not-equal)
  (branch fail ())
pass
  (movei md #.k-test-passed)
  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )

(defafun call-return-test-aux ()
  (movei a3 #xbbbb)
  (return a3 k:stat-1))

(def-k-test call-return-test ()
	    (call-return-test-aux initialize-call-hardware)
  (open-call initialize-call-hardware ignore ())
  (movei a0 #xaaaa)
  (movei a1 #xbbbb)
  (movei a2 #xcccc)
  (open-call call-return-test-aux a0 ())
  (alu xor nop a0 a1)
  (test br-not-equal)
  (branch fail ())
pass
  (movei md #.k-test-passed)
  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )


(def-k-test inc-and-passaround-test ()
	    ()
  (movei gr:*quantum-map-semaphore* '-1)
  (movei a0 #xaaaa)
  (movei a1 #xbbbb)
  (movei a2 #xaaab)
  (alu r+1 a0 a0 a0 bw-24)
  (move a1 a0)
  (nop)
  (nop)
  (alu xor nop a0 a2)
  (test br-not-equal)
  (branch fail ())
  (alu r+1 gr:*quantum-map-semaphore* gr:*quantum-map-semaphore* gr:*quantum-map-semaphore*
       boxed dt-both-fixnum-with-overflow)
  (move r2 gr:*quantum-map-semaphore* boxed-right)
  (move nop r2 boxed-right bw-24 dt-both-fixnum)
  (test br-not-equal)
  (branch fail ())
pass
  (movei md #.k-test-passed)
  (movei vma-start-write-no-gc-trap #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
fail
  (movei md #.k-test-failed)
  (movei vma-start-write #.k-test-external-result-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
)

(def-k-test alu-tests ()
	    ()
  )

(global:defprop trap-restore-test 1000. default-sleep-time)	;it really takes a while to load the dt ram especially
					;with cache off.
(def-k-test page-fault-and-trap-test ()
    (write-dt-ram write-dt-ram-dispatch load-dt-ram-pattern
     load-initial-datatype-ram
     vc-trap-on initialize-call-hardware )  ;vc-dt-and-ovf-trap-handler

  ;; Global Registers set up by spy initialization.
  ;; Traps turned off by spy initialization.
  ;;
  (open-call initialize-call-hardware ignore ())
  (open-call load-initial-datatype-ram ignore ())
  (movei processor-control #x0)		;disable flt pt trap, call hardware stack ovflo, all caches
  (movei gr:*trap-mask* #x2000)		;datatype trap is only one we expect.
  (movei memory-control #x90040000)	;master trap on, datatype trap on, boot-prom off.

  )