;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10.; Readtable:CL -*-

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

;;; Questions

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
;;;   perhaps lisp cpnstants
;;;   but in general for assembly code don't depend on them


;;;****************************************************************
;;;
;;; K Test Code Memory Map
;;;
;;;****************************************************************

;; These locations are in code memory and are counted in 4 byte (2 word or 64 bit) chunks.
;; To generate byte addresses for NuBus access shift these values left 2 bits.

;;; K Test Trap Vector Locations
(defconstant k-test-first-trap-vector-loc  #o000  "First location reserved for trap vectors.")
(defconstant k-test-last-trap-vector-loc   #o077  "Last location reserved for trap vectors.")

;;; K Test Entry and Exit Locations
(defconstant k-test-entry-loc              #o100  "This is normally a jump to the real entry.")
(defconstant k-test-pass-exit-loc          #o101  "Test passed. Normally a halt instruction.")
(defconstant k-test-fail-exit-loc          #o106  "Test failed. Normally a halt instruction.")

;;; K Test Switch and Argument Locations
;; These are translated into memory addresses
(defconstant k-test-bits-arg-loc           (* 2 #o102)  "(low word) 32 bits of switches passed to tests.")
(defconstant k-test-word-arg-loc           (1+ (* 2 #o102))  "(high word)One word argument to tests.")

;;; K Test Result Locations
;; These are translated into memory addresses
(defconstant k-test-internal-result-loc    (* 2 #o103)  "(low word)")
(defconstant k-test-external-result-loc    (1+ (* 2 #o103))  "(high word)")
(defconstant k-test-expected-value-loc     (* 2 #o104)  "(low word)")
(defconstant k-test-incorrect-value-loc    (1+ (* 2 #o104))  "(high word)")
(defconstant k-test-address-loc            (* 2 #o105)  "(low word)")

;;; K Test Trap Code Location
(defconstant k-test-trap-code-loc          #o120  "Traps vector to this location.")

;;; K Test Start of Code Location
(defconstant k-test-code-loc               #o140  "Test code lives here.")

;;;****************************************************************
;;;
;;; K Test Status Codes
;;;
;;;****************************************************************

(defconstant k-test-null-status            #x00000000 "Null Status.")
(defconstant k-test-passed                 #x00000001 "Passed the test.")
(defconstant k-test-failed                 #x00000002 "Failed the test.")

;;;****************************************************************
;;;
;;; K Test Driver
;;;
;;;****************************************************************

(defvar *k-tests* '() )


(defun vc-run-all-tests ()
  (dolist (test *k-tests*)
    (vc-test-driver test)))

(defun vc-test-driver (test &key loop-it)
  (let ((test-entry nil))
    (vc-clear-k)				;reset hardware.
 ;    (download-trap-vectors)		;trap vectors are always the same.
    (vc-link-and-download-test-code test)
    (setq test-entry (vc-test-entry test))
    (vc-initialize-memory-map)		;map code page 0 and data page 0.
    (lam:write-inst k-test-entry-loc
		  (nc:assemble-inst `(k:jump ,test-entry nil)))
    (cond (loop-it
	   (lam:write-inst k-test-pass-exit-loc (nc:assemble-inst `(k:jump #o100 nil)))
	   (lam:write-inst k-test-fail-exit-loc (nc:assemble-inst `(k:jump #o100 nil))))
	  )
    (lam:falcon-set-pc-and-go test-entry :do-init t :do-initialize-call-hardware t :do-init-virtual-memory nil)
    (cond ((k-kbug:k-halted-p)
	   (lisp:format t "~%Halted at PC = #x~x" (lam:k-stop)))
	  (t (lisp:format t "~%K is running!")))
    ))

(defun vc-test-entry (test)
  (nc:ncompiled-function-starting-address (lisp:get test 'nc:ncompiled-function)))

(defun vc-clear-k ()
  (lam:k-setup)
  (lam:k-reset)
  (lam:k-stop))

(defun vc-link-and-download-test-code (test)
  (let ((starting-address 0))
  ;link test code
    (setq starting-address (vc-link-and-increment 'k-test-setup starting-address))	;must be first.
    (setq starting-address (vc-link-and-increment test starting-address))
    (dolist (fctn (lisp:get test 'vc-aux-functions))
      (setq starting-address (vc-link-and-increment fctn starting-address))))
  (let ((starting-address 0))
  ;link again and download
    (setq starting-address (vc-link-and-increment-and-download 'k-test-setup starting-address))
    (setq starting-address (vc-link-and-increment-and-download test starting-address))
    (dolist (fctn (lisp:get test 'vc-aux-functions))
      (setq starting-address (vc-link-and-increment-and-download fctn starting-address)))))

(defun vc-link-and-increment (fctn starting-address)
  (nc:link fctn starting-address)
  (+ starting-address (nc:ncompiled-function-length (nc:nsymbol-function fctn))))

(defun vc-link-and-increment-and-download (fctn starting-address)
  (let ((next-sa (vc-link-and-increment fctn starting-address)))
    (vc-download-test-code fctn)
    next-sa))

  ;; link test code
;  (let ((starting-address (nc:ncompiled-function-length (nc:nsymbol-function 'k-test-setup))))
;    (dolist (test *k-tests*)
;      (nc:link test starting-address)
;      (setq starting-address (+ starting-address (nc:ncompiled-function-length (nc:nsymbol-function test))))))
;  ;; link again and download
;  (nc:link 'k-test-setup 0)
;  (download-test-code 'k-test-setup)
;  (dolist (test *k-tests*)
;    (nc:link test (nc:ncompiled-function-starting-address (nc:nsymbol-function test)))
;    (download-test-code test))
;    (setq starting-address (link-and-increment (nc:link 'k-test-setup 0)))
;    (setq starting-address (link-and-increment (nc:link test starting-address)))
;    (dolist (fctn (get test 'vc-aux-functions))
;      (setq starting-address (link-and-increment (nc:link fctn starting-address))))
  

(defun vc-download-test-code (test)
  (let ((address (nc:ncompiled-function-starting-address (nc:nsymbol-function test))))
    (dolist (inst (nc:ncompiled-function-code (nc:nsymbol-function test)))
      (lam:write-inst address inst)
      (incf address))))


(defun vc-initialize-memory-map ()
  ;map code page 0 to physical page 0
  (lam:falcon-write-map-and-check #x8000 #x8f)
  ;map data page 0 to physical page 0
  (lam:falcon-write-map-and-check #x0 #x8f))

(defmacro def-k-test (name lambda-list &body instructions)
  `(progn
     (defafun ,name ,lambda-list . ,instructions)
     (lisp:pushnew ',name *k-tests*))
  )


;;;****************************************************************
;;;
;;; K Test Code
;;;
;;;****************************************************************

(defafun k-test-setup ()
k-test-first-trap-vector-loc  ; #o000  "First location reserved for trap vectors."
  ;; trap
loc-0
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
  ;; non-modifying-exit
loc-12
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right*  bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
  ;; modifying-exit
loc-20
  (alu-field field-pass processor-control gr::*save-jcond* processor-control (byte 1. 4.))
  (alu load-status-r nop r0 gr::*save-status* bw-32)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc*  gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-trap-pc+* gr::*save-right* bw-32 boxed-left)
  (alu setl gr:*trap-temp1* gr::*save-oreg* gr::*save-right* bw-32 next-pc-dispatch br-jindir boxed-left)
  (nop)
  (nop)
  (nop)
  ;; diagnostic-trap-exit
loc-28
  (alu setl nop gr::*save-trap-pc*  gr::*save-right* bw-32)
  (alu setl nop gr::*save-trap-pc+* gr::*save-right* bw-32)
  (move nop gr::*save-oreg* bw-32 next-pc-dispatch)
  (nop)
  ;; trap-vector-table
loc-32
trap-vector-reset					;Bit 31 - addr 32 - Highest priority
  (unconditional-branch reset-trap-handler ())
trap-vector-trace  					;Bit 30 - addr 33
  (unconditional-branch trace-trap-handler ())
trap-vector-icache-parity				;Bit 29 - addr 34
  (unconditional-branch icache-parity-trap-handler ())
trap-vector-icache-nubus-err				;Bit 28 - addr 35
  (unconditional-branch icache-nubus-error-trap-handler ())
trap-vector-icache-nubus-timeout		 	;Bit 27 - addr 36
  (unconditional-branch icache-nubus-timeout-trap-handler ())
trap-vector-icache-page-fault				;Bit 26 - addr 37
  (unconditional-branch icache-map-fault-trap-handler ())
trap-vector-proc-mread-parity				;Bit 25 - addr 38
  (unconditional-branch memory-read-parity-trap-handler ())
trap-vector-proc-mread-nubus-err		 	;Bit 24 - addr 39
  (unconditional-branch memory-read-nubus-error-trap-handler ())
trap-vector-proc-mread-nubus-timeout			;Bit 23- addr 40
  (unconditional-branch memory-read-nubus-timeout-trap-handler ())
trap-vector-proc-mread-page-fault			;Bit 22 - addr 41
  (unconditional-branch memory-read-page-fault-trap-handler ())
trap-vector-proc-mread-transporter			;Bit 21 - addr 42
  (unconditional-branch memory-read-transporter-trap-handler ())
trap-vector-proc-mwrite-nubus-err			;Bit 20 - addr 43
  (unconditional-branch memory-write-nubus-error-trap-handler ())
trap-vector-proc-mwrite-nubus-timeout			;Bit 19-  addr 44
  (unconditional-branch memory-write-nubus-timeout-trap-handler ())
trap-vector-proc-mwrite-page-fault			;Bit 18 - addr 45
  (unconditional-branch memory-write-page-fault-trap-handler ())
trap-vector-proc-mwrite-gc				;Bit 17 - addr 46
  (unconditional-branch memory-write-gc-trap-handler ())
trap-vector-floating-point				;Bit 16 - addr 47
  (unconditional-branch floating-point-trap-handler ())
trap-vector-heap-empty					;Bit 15 - addr 48
  (unconditional-branch heap-empty-trap-handler ())
trap-vector-instruction-bit				;Bit 14 - addr 49
  (unconditional-branch instruction-trap-handler ())
trap-vector-datatype					;Bit 13 - addr 50
  (unconditional-branch datatype-trap-handler ())
trap-vector-overflow					;Bit 12 - addr 51
  (unconditional-branch overflow-trap-handler ())
trap-vector-spare11					;Bit 11 - addr 52
  (unconditional-branch spare11-trap-handler ())
trap-vector-interrupt7					;Bit 10 - addr 53
  (unconditional-branch debugger-trap-handler ())
trap-vector-interrupt6					;Bit 09 - addr 54
  (unconditional-branch interrupt6-trap-handler ())
trap-vector-interrupt5					;Bit 08 - addr 55
  (unconditional-branch interrupt5-trap-handler ())
trap-vector-interrupt4					;Bit 07 - addr 56
  (unconditional-branch iop-trap-handler ())
trap-vector-interrupt3					;Bit 06 - addr 57
  (unconditional-branch interrupt3-trap-handler ())
trap-vector-interrupt2					;Bit 05 - addr 58
  (unconditional-branch interrupt2-trap-handler ())
trap-vector-interrupt1					;Bit 04 - addr 59
  (unconditional-branch interrupt1-trap-handler ())
trap-vector-interrupt0					;Bit 03 - addr 60
  (unconditional-branch interrupt0-trap-handler ())
trap-vector-timer-1024					;Bit 02 - addr 61
  (unconditional-branch timer-1024-trap-handler ())
trap-vector-timer-16384					;Bit 01 - addr 62
  (unconditional-branch timer-16384-trap-handler ())
trap-vector-spurious					;Bit 00 - addr 63
k-test-last-trap-vector-loc ; #o077  "Last location reserved for trap vectors."
  (unconditional-branch spurious-trap-handler ())


;; locations reserved for use by the test code

k-test-entry-loc            ; #o100  "This is normally a jump to the real entry."
  (nop)
k-test-pass-exit-loc             ; #o101  "Normally a halt instruction."
  (unconditional-branch k-test-pass-exit-loc ())
k-test-bits-arg-loc         ; #o102  "(low word) 32 bits of switches passed to tests."
k-test-word-arg-loc         ; #o102  "(high word)One word argument to tests."
  (nop)
k-test-internal-result-loc  ; #o103  "(low word)"
k-test-external-result-loc  ; #o103  "(high word"
  (nop)
k-test-expected-value-loc   ; #o104  "(low word)"
k-test-incorrect-value-loc  ; #o104  "(high word"
  (nop)
k-test-address-loc          ; #o105  "(low word)"
  (nop)
k-test-fail-exit-loc        ; #o106  "Test failed.  Normally a halt instruction."
  (unconditional-branch k-test-fail-exit-loc ())
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)


k-test-trap-code-loc        ; #o120  "Traps vector to this location."
 ;; these are the labels that traps vector to
 ;; I've kept them around in case we want to handle certain traps specially.
 ;; For now they all refer to the same location which simply loops on itself.
 reset-trap-handler 
 trace-trap-handler 
 icache-parity-trap-handler 
 icache-nubus-error-trap-handler 
 icache-nubus-timeout-trap-handler 
 icache-map-fault-trap-handler 
 memory-read-parity-trap-handler 
 memory-read-nubus-error-trap-handler 
 memory-read-nubus-timeout-trap-handler 
 memory-read-page-fault-trap-handler 
 memory-read-transporter-trap-handler 
 memory-write-nubus-error-trap-handler 
 memory-write-nubus-timeout-trap-handler 
 memory-write-page-fault-trap-handler 
 memory-write-gc-trap-handler 
 floating-point-trap-handler 
 heap-empty-trap-handler 
 instruction-trap-handler 
 datatype-trap-handler 
 overflow-trap-handler 
 spare11-trap-handler 
 debugger-trap-handler 
 interrupt6-trap-handler 
 interrupt5-trap-handler 
 iop-trap-handler 
 interrupt3-trap-handler 
 interrupt2-trap-handler 
 interrupt1-trap-handler 
 interrupt0-trap-handler 
 timer-1024-trap-handler 
 timer-16384-trap-handler 
 spurious-trap-handler 

  ;; loop if we get a trap
  (unconditional-branch k-test-trap-code-loc ())
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop)
  (nop))

(def-k-test memory-test-bits ()
  (movei a3 0 bw-32 unboxed)
  (movei a4 1000 bw-24 boxed)
  (movei a0 32.)
 loop
  (alu load-status-r nop a0 a0 bw-8)
  (alu-field set-bit-left a1 a3 ignore 0 pw-ri)
  (move md a1)
  (move vma-start-write a4 unboxed-md)
  (memory-wait)
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
  ;; set up success completion code and jump to exit location
  (movei md k-test-passed)
  (movei vma-start-write k-test-external-result-loc unboxed-vma)
  (jump #.k-test-pass-exit-loc ())
 fail
  ;; set up 
  ;;        failure completion code
  ;;        address being tested
  ;;        expected value
  ;;        incorrect value
  ;; and jump to exit location
    ;this loses, since it clobbers the VMA before it write it!  Save in temp first.
  (movei md k-test-failed)
  (movei vma-start-write k-test-external-result-loc unboxed-vma)
  (move md a4)
  (movei vma-start-write k-test-address-loc unboxed-vma)
  (move md a1)
  (movei vma-start-write k-test-expected-value-loc unboxed-vma)
  (move md a2)
  (movei vma-start-write k-test-incorrect-value-loc unboxed-vma)
  (jump #.k-test-fail-exit-loc ())
  )


(def-k-test vc-memory-write-count-loop ()
    ;counts in processor, and writes count to memory.
  (movei a3 0 bw-32 unboxed)		;data to write to initialize memory counter.
 loop
  (movei a4 #o400 bw-24 unboxed)		;address to be hacked.
  (move md a3 unboxed)
  (move vma-start-write-no-gc-trap a4 unboxed-md)
  (memory-wait)
  (alu l+1 a3 a3 ignore bw-32 dt-none)
  (unconditional-branch loop ())
  )

(def-k-test vc-test-read-trap-enable-and-disable ()	;rte-and-d
  ;make sure traps are disabled
  ;make sure rte-and-d shows disabled
       (move gr::*trap-temp1* trap-off)
  ;make sure traps stay disabled
  ;enable traps
  ;make sure rte-and-d shows enabled and disables.
  )
;------

(defun vc-trap-on ()		;lifted from regular code, prefixed vc-
  (let ((old-trap-state (hw:trap-off)))
    (hw:write-memory-control
      (hw:dpb-unboxed hw:$$trap-enable hw:%%memory-control-master-trap-enable
		      (hw:read-memory-control)))
    ;; Let mmfio clear out.
    (hw:nop)
    (hw:nop)
    (hw:nop)
    old-trap-state))

(defun vc-trap-restore (old-trap-state)		;lifted from regular code, prefixed vc-
  (hw:write-memory-control
    (hw:dpb-unboxed old-trap-state hw:%%memory-control-master-trap-enable
		    (hw:read-memory-control)))
    ;; Let mmfio clear out.
  (hw:nop)
  (hw:nop)
  (hw:nop))  

#|

;;;****************************************************************
;;;
;;; K Test Example Boot Code  (Lambda side)
;;;
;;;****************************************************************


(defun pseudo-boot ()
  (k-init)
  (k-kbug:setup-processor-control-register)
  (k-kbug:setup-memory-control-register)
  ;; Traps are off here.
  (k-kbug:direct-map-location-zero)
  ;; Now, we figure out where to map the initial instructions.
  (labels ((map-n-instruction-clusters (n physical virtual)
	     (unless (zerop n)
	     (format t "~&Mapping ~X to ~X" virtual physical)

	       (map::write-map virtual
		 (map::inject-map-status
		   (dpb-multiple
		     physical       hw:%%map-on-board-address
		     hw:$$map-local hw:%%map-local-memory-bit
		     0              hw:%%map-volatility
		     0              hw:%%map-c-trap-bits
		     0)
		   map::$$map-status-read-only))
	       (map-n-instruction-clusters (1- n) (1+ physical) (1+ virtual)))))
    (map-n-instruction-clusters
      (dpb 0 vinc:%%data-type (boot::read-boot-vector **initial-code-size-in-clusters-bv-offset**))
      (ash (dpb 0 vinc:%%data-type (boot::read-boot-vector **initial-code-physical-location-bv-offset**)) -10.)
      *first-instruction-cluster*))

  ;; Size the physical memory
  (let ((size (k-kbug:find-physical-memory)))		;return size in megabytes.
    (format t "~%Physical memory = ~8,'0x~%~%" size)
    (write-boot-vector **physical-memory-block-map** size))
  ;; Inform the K about the bootprom version
  (write-boot-vector **bootprom-version** 0.)
  ;; Mapped in some instructions, jump to them.
  (hw:jump (read-boot-vector **initial-code-entry-point-bv-offset**))
  )



(defun k-reset ()
   "Issue a reset to the K processor and stops the clocks"
   (do ((foo nil (format t "J100 failed "))) ((= #x100 (k-read-spy-pc)))
     (k-write-mode 1.)
     (k-stop)
     (k-init)))

(defun k-init ()
  "Init some registers, and set the PC to #x0100"
  (k-stop)
  (k-execute KIH-JUMP #x100)
  ;; Magic machine unwedger.
  (dotimes (i 5)
;    (k-spy-cmd $$spy-command-reload-instruction)
;    (k-spy-cmd $$spy-command-clear-spy-mode)
;    (k-spy-cmd $$spy-command-step)
;    (k-spy-cmd $$spy-command-set-spy-mode)
    (k-execute4 kih-jump #x100))
  (k-execute KIH-LOAD-VMA 0)
  (k-execute KIH-LOAD-MAP #x8f)
  (k-execute3 KIH-JUMP    #x100))

(defun k-stop ()
  "Stop the processor clocks, and init spy modes"
  (k-spy-cmd $$spy-command-stop)
  (k-spy-cmd $$spy-command-stepmode-full-clock)
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-cmd $$spy-command-clear-opc-clock)     ; clear opc clk
  (setq k-run-flag nil)
  (k-read-spy-pc))


(defun setup-processor-control-register ()
  (hw:write-processor-control
    (dpb-multiple
      hw:$$icache-set-disable		    hw:%%processor-control-icache-a-enable
      hw:$$icache-set-disable		    hw:%%processor-control-icache-b-enable
      hw:$$icache-set-disable		    hw:%%processor-control-icache-z-enable
      0					    hw:%%processor-control-spare-3
      0					    hw:%%processor-control-jump-indirect
      hw:$$floating-point-status-ram-read   hw:%%processor-control-floating-point-status-ram-write-enable
      hw:$$box-mode-normal		    hw:%%processor-control-box-mode
      hw:$$run				    hw:%%processor-control-halt-processor
      0					    hw:%%processor-control-data-bit
      0					    hw:%%processor-control-misc
      0					    hw:%%processor-control-stack-group-number
      0					    hw:%%processor-control-spare-17
      hw:$$call-heap-underflow-trap-disable hw:%%processor-control-heap-underflow-trap-enable
      hw:$$floating-point-trap-disable      hw:%%processor-control-floating-point-trap-enable
      0)))


(defun setup-memory-control-register ()
  (hw:write-memory-control
    (dpb-multiple
      ;; Top bits will be zero, so traps will be off.
      hw:$$reset-trap-bit-off            hw:%%memory-control-reset-trap-bit
      hw:$$dram-parity-disable           hw:%%memory-control-dram-parity-enable
      hw:$$bootprom-off                  hw:%%memory-control-bootprom-disable
      0                                  hw:%%memory-control-transporter-mode
      hw:$$lisp-map-bits                 hw:%%memory-control-l-c-map-select
      hw:$$write-normal-parity           hw:%%memory-control-write-wrong-parity
      hw:$$timer-interrupt-disable-reset hw:%%memory-control-16384-interrupt
      hw:$$timer-interrupt-disable-reset hw:%%memory-control-1024-interrupt
      hw:$$icache-trap-disable-reset     hw:%%memory-control-icache-error-enable
      hw:$$nubus-transfer-32-bits        hw:%%memory-control-nubus-transfer-mode
      7.                                 hw:%%memory-control-leds
      0)))


(defun direct-map-location-zero ()
  (map::write-map 0
    (map::inject-map-status
      (dpb-multiple
	hw:$$map-local           hw:%%map-local-memory-bit
	0                        hw:%%map-on-board-address
	map::$$cluster-not-fresh map::%%map-fresh-cluster
	0)
      map:$$map-status-normal)))


(defun find-physical-memory (&optional (max-chunk 32.))
  (if lam:*local-debugging* (setq max-chunk 16.))  ;better not get bus timeouts in local mode.
  (labels ((mark-physical-memory (chunk)
	    (if (minusp chunk)
		(locate-physical-memory 0 0)
	      (progn (k-mem-write (ash chunk 20.) chunk)
		     (mark-physical-memory (1- chunk)))))
	     
	   (locate-physical-memory (chunk map)
	    (cond ((= chunk max-chunk) map)	;check first before you reference!
		  (t
		   (let ((data (k-mem-read (ash chunk 20.))))
		     (cond ((and (numberp data) (= chunk data))
			    (locate-physical-memory (1+ chunk)
						    (logior (ash 1. chunk) map)))
			   (t (locate-physical-memory (1+ chunk) map))))))))
    (mark-physical-memory (1- max-chunk))))


;;; K-BOOT calls  KBUG-LOAD-COLD-INFO and then WARM-LOAD

;; from KOLD-LOADER
(defun fasd-cold-function-info (stream)
  (let ((count (length cold:*cold-loaded-functions*)))
    (fasdump:fasd-fixnum-internal count stream)
    (dolist (fcn cold:*cold-loaded-functions*)
      (let ((name (nc::ncompiled-function-name fcn)))
	(format t "~&~3d  ~A" (setq count (1- count)) name)
	(fasdump:fasd-cold-compiled-function-info
	  name
	  (nc::ncompiled-function-local-refs fcn)
	  (nc::ncompiled-function-refs fcn)
	  (nc::ncompiled-function-immediates fcn)
	  (nc::ncompiled-function-entry-points fcn)
	  (nc::ncompiled-function-length fcn)
	  (nc::ncompiled-function-starting-address fcn)
	  stream)))))


;; from NEW-FASDUMP
(defun fasd-cold-compiled-function-info (name local-refs refs
					 immediates entry-points length starting-address
					 stream)
  (fasd-object name stream)
  (fasd-link-info local-refs refs entry-points stream)
  (fasd-fixnum-internal length stream)
  (fasd-fixnum-internal starting-address stream)
  (fasd-immediates immediates stream))

;;;****************************************************************
;;;
;;; K Test Example Boot Code  (K side)
;;;
;;;****************************************************************

;;; This code runs in the K after the cold load is down-loaded

;; from BOOT.LISP
(defun cold-boot-function ()
  (hw:write-open-active-return #x101112) ;temp O=10, A=11, R=12
  (hw:nop)
  (prims::setup-initial-values-of-global-registers)
  (cold-initialize-call-hardware)
  (event-horizon)
  (labels ((loop-forever ()
	     (trap::illop "Unexpected return from the event horizon.")
	     (loop-forever)))
    (loop-forever)))

(defun event-horizon ()
  (load-up-runtime-global-constants)
  (modify-icache-enables       hw:$$icache-enable-all-sets)
  (map::direct-map (read-boot-vector **physical-memory-block-map**))
  (gc-ram::load-ram (read-boot-vector *initial-gc-ram-data-physical-location*))
  (transporter-ram:load-transporter-ram (read-boot-vector *initial-transporter-ram-data-physical-location*))
  (datatype-ram:load-initial-datatype-ram)
  (pcd:create-physical-cluster-data-table)
  (pcd:initialize-physical-cluster-data *initial-physical-cluster-data-physical-location*)
  (pcd:free-unused-physical-clusters (read-boot-vector **physical-memory-block-map**))
  (map:flush-direct-map)
  (nubus-stuff::map-in-k-io-cluster)
  (modify-asynchronous-traps hw:$$trap-enable)
  (modify-synchronous-traps  hw:$$trap-enable)
  (modify-icache-traps       hw:$$trap-enable)
  (trap::trap-on)
  (k2::init-kbug)
  (modify-datatype-traps hw:$$trap-enable)
  (modify-overflow-traps hw:$$trap-enable)
  (if (read-boot-vector *cold-load-flag*)
      (synthesize-cold-load)
      (trap::illop "I want to call LISP-REINITIALIZE.")))

(defun synthesize-cold-load ()
  ;; Make region to hold region-data
  (synthesize-region-data)
;  (trap::illop "Made region data.")
  (synthesize-area-data)
;  (test-tak-with-lights)
;  (trap::illop "made area data.")
 (setq gr::*desperate-consing-area*
	(area-data:make-area 1.
			     (region-bits:encode-region-bits
			       region-bits:$$region-fixed
			       region-bits:$$region-new-space
			       region-bits:$$region-space-unboxed
			       region-bits:$$region-read-write
			       region-bits:$$scavenge-enabled
			       region-bits:$$region-internal-memory
			       0.)
			     1.))
; (trap::illop "Made desparate-consing-area")
  ;; Make the default consing area and load up the cons cache.
  (let ((default-consing-area
	  (area-data:make-area 7.
			     (region-bits:encode-region-bits
			       region-bits:$$region-fixed
			       region-bits:$$region-new-space
			       region-bits:$$region-space-unboxed
			       region-bits:$$region-read-write
			       region-bits:$$scavenge-enabled
			       region-bits:$$region-internal-memory
			       5.)
			     5.)))
    (setq gr:*cons-cache-area*           default-consing-area)
    (setq gr:*structure-cons-cache-area* default-consing-area)
    (region-data::invalidate-cons-cache)
    (setq gr:*default-code-area*
	  (area-data:make-area 3
			       (region-bits:encode-region-bits
				 region-bits:$$region-fixed
				 region-bits:$$region-new-space
				 region-bits:$$region-space-code
				 region-bits:$$region-read-write
				 region-bits:$$scavenge-enabled
				 region-bits:$$region-internal-memory
				 5.)
			       5.))
;    (setq gr::*cons-cache-region* -1)
;    (setq gr::*structure-cons-cache-region* -1)
;    (area-data::get-active-region
;      default-consing-area
;      region-bits::$$region-space-cons
;      region-bits::$$region-new-space
;      nil
;      0)
;;    (trap::illop "loaded cons cache.")
;    (area-data::get-active-region
;      default-consing-area
;      region-bits::$$region-space-structure
;      region-bits::$$region-new-space
;      nil
;      0)
    (setq gr::*default-consing-area* default-consing-area))
  (cons::initialize-structure-handles)
  ;;; The are for the "other side" of dt-right-array-and-left-structure
  (setq gr:*random-structure* (li:make-structure 1))
  (setq gr:*random-array* (array:make-vector 0))
  (let ((lisp-name (array::make-string 4)))
    (array::aset-1 #\L lisp-name 0)
    (array::aset-1 #\I lisp-name 1)
    (array::aset-1 #\S lisp-name 2)
    (array::aset-1 #\P lisp-name 3)

    ;; Fixup NIL
    (symbol::%fmakunbound nil)
    (setf (symbol::symbol-plist 'nil) nil)
    (setf (symbol::symbol-package nil) lisp-name)

    ;; Make T
    (let ((t-print-name (array::make-string 1)))
      (array::aset-1 #\T t-print-name 0)
      ;; Put the print name in.
      (hw::write-md-boxed (cons:make-header vinc:$$dtp-symbol-header
					    t-print-name))
      (hw::vma-start-write-boxed gr:*t*)
      (symbol::%set gr:*t* gr:*t*)
      (symbol::%fmakunbound gr:*t*)
      (setf (symbol::symbol-plist gr:*t*) nil)
      (setf (symbol::symbol-package gr:*t*) lisp-name)
      (setq gr:*warm-symbols* (cons:cons gr:*t* nil))))
  (trap::illop "Cold load finished!")
  (warm-start)
  )

(defun warm-start ()
   ;; un-Halt the machine.
  (hw:write-processor-control
    (hw:dpb-unboxed 0 hw:%%processor-control-halt-processor (hw:read-processor-control)))
  (hw:nop) (hw:nop) ;allow relinking
  (trap:without-traps
    #'(lambda ()
	(vinc:flush-icache)
	(modify-asynchronous-traps hw:$$trap-enable)
	(modify-synchronous-traps  hw:$$trap-enable)
	(k2::init-kbug)
	(modify-icache-traps       hw:$$trap-enable)
	(modify-datatype-traps hw:$$trap-enable)
	(modify-overflow-traps hw:$$trap-enable)
	;; flush out memory traps
	(hw:write-md-unboxed 0)
	(hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
	(hw:vma-start-read-no-transport-vma-unboxed-md-unboxed 0)
	(hw:read-md)	
	))
  (trap::trap-on)
  (li:flush-call-stack))

;;; This code recieves the Cold-Load Info send down by FASD-COLD-FUNCTION-INFO

;; from WARM-LOADER
(defun kbug-load-cold-info ()			;implements KBUG-COMMAND-LOAD-COLD-INFO
  (setq gr:*mini-fasl-byte-counter* 0)
  (setq gr:*mini-fasl-top-level-opcode-byte-count* gr:*mini-fasl-byte-counter*)
  (setq gr:*mini-fasl-top-level-opcode* -1)
  (dotimes (nfcns (mini-fasl-read-fixnum))
    (mini-fasl-read-cold-fcn-info)))

(defun mini-fasl-read-cold-fcn-info ()
  (let* ((name         (mini-fasl-read-object))
	 (local-refs   (read-local-refs))
	 (refs         (read-refs))
	 (entry-points (read-entry-points))
	 (length       (mini-fasl-read-fixnum))
	 (pc           (mini-fasl-read-fixnum))
	 (starting-addr (pc->addr pc))
	 (function (make-compiled-function name entry-points
					    local-refs refs
					    length)))
    (setf (%compiled-function-code function)
	  (cons:make-pointer vinc:$$dtp-code pc))
;    (setf (%compiled-function-starting-address function) starting-addr)
    (when (li:symbolp name)
      (setf (symbol:symbol-function name) function))
    (when (>= pc 64.)
      (map-fault:call-while-allowing-write-in-read-only
	#'(lambda ()
	    (hw:write-md-unboxed cons:code-header-instruction-high)
	    (hw:vma-start-write-no-gc-trap-unboxed (hw:24+ -1 starting-addr))
	    (cons:store-contents-offset starting-addr -2 function))))
    (read-and-link-immediates starting-addr)))

(defun read-and-link-immediates (base)
  (dotimes (i (mini-fasl-read-fixnum))
    (write-boxed-immediate 
      (hw:24+ (ash (mini-fasl-read-fixnum) 1.) base)
      (mini-fasl-read-object))))

(defun write-boxed-immediate (address immediate)
  (map-fault:call-while-allowing-write-in-read-only
    #'(lambda ()
	(hw:write-md-boxed immediate)
	(hw:vma-start-write-boxed address)
	nil
	)))

;;; to see how the K side of the warm-load streams work
;;; look in these files
;;;		"jb:kbug2;streams"	    loaded both on K and lambda
;;;		"jb:kbug2;k2"			
;;;		"jb:k;warm-loader"		

;;;****************************************************************
;;;
;;; K Memory Tests (from kbug;spy-diags.lisp)
;;;
;;;****************************************************************


(defun k-test14 (&aux temp)
  "Memory Control Register Test."
  (format t "Starting Test 14 - Memory control register.~%")
  (k-init)
  (dotimes (i 31.)
    (k-execute  KIH-LOAD-MCTL (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute  KIH-ALU-NOP KIL-READ-MCTL)
    (k-execute2 KIH-NOP 0.)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-14 MCTL" nil (ash 1. i) temp))))

;;;****************************************************************

;;; tests 41,42,43,44 write and read memory from the lambda

(defun k-test41 ()
  "Simple memory test - location 0 data patterns."
  (format t "Starting Test 41 - Memory loc 0 data patterns.~%")
  (let*
    (temp
     (patterns '(0 #xffffffff #x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x100 #x200
		   #x400 #x800 #x1000 #x2000 #x4000 #x8000 #x10000 #x20000 #x40000
		   #x80000 #x100000 #x200000 #x400000 #x800000 #x1000000 #x2000000
		   #x4000000 #x8000000 #x10000000 #x20000000 #x40000000 #x80000000)))
    (dolist (pat patterns)
      (k-mem-write 0 pat)
      (when (not (equal pat (setq temp (k-mem-read 0))))
	(k-diag-error "TEST-41 Memory loc 0 data patterns" 0 pat temp)))))


(defun k-test42 ()
  "Memory Sizer"
  (format t "Starting test 42 - Memory sizer.~%")
  (k-init)
  (let*
    ((addr 0)
     (max (progn
	    (k-execute3 kih-alu-nop kil-read-mstat)
	    (if (equal 1. (ldb (byte 1. 23.) (k-read-spy-mmfio))) 4. 8.)))
     (temp nil)
     (gap t))
    (setq k-mem-list nil)
    (dotimes (i max)
      (setq addr (ash i 22.))
      (if (k-test-42-mworks-? addr)
	  (progn
	    (if gap
		(setq temp addr))
	    (setq gap nil))
	(progn
	  (if (not gap)
	      (setq k-mem-list (nconc k-mem-list (cons (list temp (sub1 addr)) nil))))
	  (setq gap t))))
    (when (not gap)
      (setq addr (ash max 22.))
      (setq k-mem-list (nconc k-mem-list (cons (list temp (sub1 addr)) nil))))
    k-mem-list))


(defun k-test-42-mworks-? (addr)
  (k-mem-write addr #x12345678)
  (if (equal #x12345678 (k-mem-read addr))
      (progn
	(k-mem-write addr #xedcba987)
	(if (equal #xedcba987 (k-mem-read addr)) t nil))
    nil))

(defun k-test43 (&optional fast)
  "Memory address test"
  (format t "Starting Test 43 - Memory address~%")
  (let*
    ((temp nil)
     (m-list (if fast '((0 #x1000)) k-mem-list)))
    (if (null m-list)
	(format t "*** Can't run test 43 - Memory not sized~%")
      (dolist (mrange m-list)
	(do*
	  ((addr (first mrange) (+ addr 4.))
	   (max  (second mrange)))
	  ((> addr max))

	  (k-mem-write addr addr)))
      (dolist (mrange m-list)
	(do*
	  ((addr (first mrange) (+ addr 4.))
	   (max  (second mrange)))
	  ((> addr max))

	  (when (not (equal addr (setq temp (k-mem-read addr))))
	    (k-diag-error "TEST-43 - Memory address" addr addr temp))))

      (dolist (mrange m-list)
	(do*
	  ((addr (first mrange) (+ addr 4.))
	   (max  (second mrange)))
	  ((> addr max))

	  (k-mem-write addr (logxor #xffffffff addr))))

      (dolist (mrange m-list)
	(do*
	  ((addr (first mrange) (+ addr 4.))
	   (max  (second mrange)))
	  ((> addr max))

	  (when (not (equal (logxor #xffffffff addr) (setq temp (k-mem-read addr))))
	    (k-diag-error "TEST-43 - Memory address"
			  addr (logxor #xffffffff addr) temp)))))))

(defun k-test44 (&optional (delta #x100000))
  "Simple memory test - One word in each bank (actually every Nth)"
  (format t "Starting Test 44 - First word of each bank (actually every #x~Xth)~%" delta)
  (k-init)
  (k-execute kih-load-mctl 0)
  (k-execute4 kih-jump #x100)
  (let*
    (temp
     (patterns '(0 #xffffffff #x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x100 #x200
		   #x400 #x800 #x1000 #x2000 #x4000 #x8000 #x10000 #x20000 #x40000
		   #x80000 #x100000 #x200000 #x400000 #x800000 #x1000000 #x2000000
		   #x4000000 #x8000000 #x10000000 #x20000000 #x40000000 #x80000000)))
    (dolist (mrange k-mem-list)
	(do
	  ((addr (first mrange) (+ addr delta))
	   (max  (second mrange)))
	  ((> addr max))
	  (dolist (pat patterns)
	    (k-mem-write 0 pat)
	    (when (not (equal pat (setq temp (k-mem-read 0))))
		  (k-diag-error "TEST-44 Memory Bank data patterns" addr pat temp)))))
    (dolist (mrange k-mem-list)
      (do
	((addr (first mrange) (+ addr delta))
	 (max (second mrange)))
	((> addr max))
	(k-mem-write addr addr)))
    (dolist (mrange k-mem-list)
      (do
	((addr (first mrange) (+ addr delta))
	 (max (second mrange)))
	((> addr max))
	(setq temp (k-mem-read addr))
	(when (not (equal temp addr))
	  (k-diag-error "TEST-44 Memory Bank Addressing" addr addr temp))))))

;;;****************************************************************

;;; tests 50,51,52,53,58 run inside the K and check results via the SPY MMFIO

(defun k-test50 (&aux temp)
  "TEST-50 Proc - local memory data test loc 0"
  (k-init)
  (format t "Starting Test 50 - Proc - local memory data test loc 0~%")
  (dotimes (i 32.)
	   (k-execute kih-load-md (ash 1 i))
	   (k-execute kih-load-vma-sw 0)
	   (k-execute kih-jump #x100)
	   (k-execute kih-load-vma-sr 0)
	   (k-execute kih-jump #x100)
	   (k-execute3 kih-alu-nop kil-read-md)
	   (setq temp (k-read-spy-mmfio))
	   (when (not (equal temp (ash 1 i)))
		 (k-diag-error "TEST-50 Proc - local mem data" 0 (ash 1 i) temp))))

(defun k-test51 (&optional fast &aux temp)
  "TEST-51 Proc - local mem data 0-255"
  (k-init)
  (format t "Starting Test 51 - Proc - local memory data 0-255~%")
  (dotimes (pass (if fast 1. 32.))
    (dotimes (i 256.)
      (k-execute kih-load-md (ash 1 (logand 31. (+ pass i))))
      (k-execute kih-load-vma-sw i)
      (k-execute kih-jump #x100))
    (dotimes (i 256.)
      (k-execute kih-load-vma-sr i)
      (k-execute kih-jump #x100)
      (k-execute3 kih-alu-nop kil-read-md)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp (ash 1 (logand 31. (+ pass i)))))
	(k-diag-error "TEST-51 Proc - local mem data" i
		      (ash 1 (logand 31. (+ pass i))) temp)))))

(defun k-test52 (&optional fast &aux temp)
  "TEST-52 Proc - local mem address 0-255"
  (k-init)
  (format t "Starting Test 52 - Proc - local memory address 0-255~%")
  (dotimes (pass (if fast 1. 32.))
    (dotimes (i 256.)
      (k-execute kih-load-md (ash 1 (logand i 31.)))
      (k-execute kih-load-vma-sw i)
      (k-execute kih-jump #x100))
    (dotimes (i 256.)
      (k-execute kih-load-vma-sr i)
      (k-execute kih-jump #x100)
      (k-execute3 kih-alu-nop kil-read-md)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp (ash 1 (logand i 31.))))
	(k-diag-error "TEST-52 Proc - local mem address" i
		      (ash 1 (logand i 31.)) temp)))))

(defun k-test53 (&aux temp pat)
  "Test-53 Proc - local mem special reads"
  (k-init)
  (k-execute kih-load-mctl 0)
  (k-execute4 kih-jump #x100)
  (format t "Starting Test 53 - Proc - local mem special reads~%")
  (k-execute kih-load-g0 0)
  (k-execute kih-load-md #x55555555)
  (k-execute kih-load-vma-sw 0)
  (k-execute2 kih-jump #x100)
  (k-execute kih-load-md #xAAAAAAAA)
  (k-execute kih-load-vma-sw 1)
  (k-execute2 kih-jump #x100)
  (k-execute kih-load-md 0)
  (k-execute kih-load-vma-sw 2)
  (k-execute2 kih-jump #x100)
  (dotimes (i 16.)
    (k-execute kih-load-vma-sr 2)
    (k-execute3 kih-jump #x100)
    (setq pat (if (zerop (logand 4. i)) #x55555555 #xAAAAAAAA))
    (k-execute (logior (ash i 9.) kih-load-vma-sr-r) kil-readr-g0)
    (when (zerop (logand 8. i)) (k-execute kih-nop 0))
    (k-execute3 kih-alu-nop kil-read-md)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal pat temp))
      (k-diag-error "TEST-53 VMA-Start-read type" i pat temp))
    (k-execute3 kih-alu-nop kil-read-mstat)
    (setq temp (ldb (byte 2. 13.) (k-read-spy-mmfio)))
    (setq pat (logand 3. (lognot i)))
    (when (not (equal pat temp))
      (k-diag-error "TEST-53 Memory Cycle Type Status" i pat temp))))


(defun k-test58 (&aux expect result)
  "TEST-58 Local Memory Parity"
  (format t "Starting Test 58 - Local Memory Parity~%")
  (k-init)
  (k-execute kih-load-pctl 0)
  (dolist (mrange k-mem-list)
    (do*
      ((addr (ash (first mrange) -2.) (+ addr #x100000))
       (max  (ash (second mrange) -2.)))
      ((> addr max))
      (k-execute2 kih-load-vma addr)
      (k-execute2 kih-load-map (logior #x8f (ash addr 2)))
      (dotimes (pbad 2)
	(dotimes (i 256.)
	  (k-execute kih-load-mctl 0)
	  (k-execute kih-jump #x100)
	  (if (zerop pbad)
	      (k-execute kih-load-mctl #x80000)	;Parity enable
	    (k-execute kih-load-mctl #x84000)	;Write wrong parity
	    (k-execute4 kih-jump #x100)
	    (setq expect (k-test58-genpat i))
	    (k-execute kih-load-md expect)
	    (k-execute kih-load-vma-sw addr)
	    (k-execute kih-jump #x100)
	    (k-execute kih-load-vma-sr addr)
	    (k-execute kih-jump #x100)
	    (k-execute3 kih-alu-nop kil-read-md)
	    (setq result (k-read-spy-mmfio))
	    (when (not (equal expect result))
	      (k-diag-error "Test 58 - Proc Parity (Data error)" addr expect result))
	    (k-execute3 kih-alu-nop kil-read-mstat)
	    (setq result (ldb (byte 1. 21.) (k-read-spy-mmfio)))
	    (when (not (equal result (- 1. pbad)))
	      (k-diag-error "Test 58 - Proc Parity" addr expect expect)))))
      (dotimes (i 256.)
	(k-execute kih-load-mctl 0)
	(k-execute kih-jump #x100)
	(k-execute kih-load-mctl #x80000)	;Parity enable
	(k-execute4 kih-jump #x100)
	(setq expect (k-test58-genpat i))
	(k-mem-write (ash addr 2.) expect)
	(setq result (k-mem-read (ash addr 2.)))
	(when (not (numberp result))
	  (k-diag-error "Test 58 - NUBUS Parity" (ash addr 2.) expect expect))))))

(defun k-test58-genpat (n)
  (logior n (ash n 8.) (ash n 16.) (ash n 24.)))



|#