;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-
;-- this is a hacked version of the new-spy-utilities file
;-- it is intended for use only when generating nubus "device" VLA files for use with
;-- the daisy simulator.

(export '(
	  $$halt-bit-halted
	  $$halt-bit-running

	  get-history

	  k-init
	  k-go
	  k-read-oar k-set-pc
	  k-load-memory-control
	  k-mem-write
	  k-mem-read
	  k-read-current-map-address
	  k-read-frame
	  k-read-gc-ram
	  k-read-hp-sp
	  k-read-memory-control
	  k-read-memory-map
	  k-read-memory-status
	  k-read-processor-control
	  k-read-virtual-memory
	  k-read-register
	  k-read-retpc-rdest
	  k-read-spy-halt
	  k-read-spy-mmfio
	  k-read-spy-pc
	  k-read-transporter-ram
	  k-read-trap-off
	  k-read-trap-register
	  k-run
	  k-run-flag
	  k-start
	  k-step
	  k-stop
	  k-vma-start-read
	  k-write-current-map-entry
	  k-write-gc-ram
	  k-write-hp-csp
	  k-write-memory-map
	  k-write-processor-control
	  k-write-transporter-ram
	  k-write-virtual-memory
	;  read-inst write-inst       ;tags search done to de-export these 11/24/87  rg
	  read-open read-active read-return read-q
	  read-md-boxed
	  read-vma-boxed
	  r-md
	  r-vma
	  saving-hp-csp
	  saving-oar
	  screen-virtual-address
	  w-vma))

  
(defvar *daisy-time* 1000.)		;simulated time on NUBUS
(defvar *daisy-out*)		;output stream to daisy VLA file.
(defconst *daisy-nubus-cycle-time* 400.)
(defconst *daisy-k-slot* #XF0)

(defun daisy-debug-write-word (nubus-byte-address data)
  ;generate start cycle
  (format *daisy-out* "~5D ~1B~1B~1B~1B~1B~32,48B~%" *daisy-time*
	  1 1 0 1 0 nubus-byte-address)
  (incf *daisy-time* 100.)
  (format *daisy-out* "~5D ~1B~1B~1B~1B~1B~32,48B~%" *daisy-time*
	  1 0 0 0 0 data)
  (incf *daisy-time* *daisy-nubus-cycle-time*)
  )


(defvar *local-k-slot* nil "If non-NIL, slot K is in on LOCAL NUBUS")
(declare (special *local-debugging*))	;looked at by DEBUG-READ-WORD, etc, in DEBUG-BOARD file.

;;; The top 32 bits of some instructions.
(defconstant KIH-NOP          #x0F009E00 "loadi-32 to no-op")
(defconstant KIH-ALU-NOP      #x03009E00 "generic ALU instruction w/NOP dest")
(defconstant KIH-ALU-NOPBR    #x03409E00 "generic ALU instruction w/NOP dest, boxed right")
(defconstant KIH-ALU-TRANSP   #x0300CA00 "generic ALU instruction w/transporter dest")
(defconstant KIH-ALU-G0       #x03406000 "generic ALU instruction w/g0 dest, boxed right")
(defconstant KIH-ALU-A0       #x03404000 "generic ALU instruction w/A0 dest")
(defconstant KIH-ALU-A0BL     #x03004000 "generic ALU instruction w/A0 dest boxed left")
(defconstant KIH-ALU-A0BR     #x03404000 "generic ALU instruction w/A0 dest boxed right")
(defconstant KIH-ALU-A0BU     #x13804000 "generic ALU instruction w/A0 dest & unboxed macrolink")
(defconstant KIH-ALU-O0       #x03400000 "generic ALU instruction w/O0 dest, boxed right")
(defconstant KIH-DISPATCH     #x01009E00 "dispatch instruction")
(defconstant KIH-DISPATCH-X16 #x21009E00 "dispatch x16 instruction")
(defconstant KIH-JUMP         #x24009E00 "jump unconditional")
(defconstant KIH-JCOND        #x08009E00 "jump conditional")
(defconstant KIH-CALL         #x28029E18 "call")
(defconstant kih-open         #x0F019E00 "loadi-32 to no-op, open")
(defconstant KIH-OPEN-CALL    #x08039E18 "open-call")
(defconstant KIH-RETURN       #x02049E00 "return")
(defconstant KIH-RETURN-FD    #x0E048200 "return w/FDEST")
(defconstant KIH-XRETURN      #x03049E00 "strange return")
(defconstant KIH-OPEN-CALL-X  #x08039E00 "open-call")
(defconstant KIH-TOPEN        #x0F059E00 "topen")
(defconstant KIH-TCALL        #x08069E00 "tcall")
(defconstant KIH-TOPEN-TCALL  #x08079E00 "topen-tcall")
(defconstant KIH-LOAD-G0      #x0F806000 "load G0 register instruction")
(defconstant KIH-LOAD-G1      #x0F806200 "load G1 register instruction")
(defconstant KIH-LOAD-G2      #x0F806400 "load G2 register instruction")
(defconstant KIH-LOAD-G3      #x0F806600 "load G3 register instruction")
(defconstant KIH-LOAD-G9      #x0F804000 "load A0 register instruction")
(defconstant KIH-LOAD-A0      #x0F002000 "load A0 register instruction")
(defconstant KIH-LOAD-O0      #x0F000000 "load O0 register instruction")
(defconstant KIH-LOAD-R0      #x0F004000 "load R0 register instruction")
(defconstant KIH-LOAD-PCTL    #x0F009200 "load processor control reg")
(defconstant KIH-LOAD-MCTL    #x0F00C400 "load memory control reg")
(defconstant KIH-LOAD-VMA     #x0F00D000 "load VMA")
(defconstant KIH-LOAD-VMA-SR  #x0F00E000 "load VMA start read")
(defconstant KIH-LOAD-VMA-SR-R #x0300E000 "load VMA start read from reg")
(defconstant KIH-LOAD-VMA-SW  #x0F00D800 "load VMA start write")
(defconstant KIH-LOAD-MD      #x0F00D400 "load Write-MD")
(defconstant KIH-LOAD-MD-SW   #x0F00DC00 "load MD-START-WRITE")
(defconstant KIH-LOAD-MAP     #x0F00C000 "load map")
(defconstant KIH-LOAD-OAR     #x0F009400 "load Open, Active, Return")
(defconstant KIH-LOAD-CSP     #x0F009800 "load call stack pointer & heap pointer")
(defconstant KIH-LOAD-RPC     #x0F009600 "load return PC and dest")
(defconstant KIH-LOAD-USC     #X0F00C600 "load microsecond clock")
(defconstant KIH-LOAD-STAT    #X0F00C800 "load statistics counter")
(defconstant KIH-LOAD-DT      #X0F009000 "datatype ram write pulse")
(defconstant kih-load-gc-ram  #x0F00C200 "gc ram write")
(defconstant kih-load-transporter
			      #x0F00CA00 "transporter ram write")
(defconstant KIH-FMUL-G0      #x1F006000 "Floating point multiplier op")
(defconstant KIH-FALU-G0      #x1B006000 "Floating point alu op")
(defconstant kih-branch       #x20009E00 "Short branch instruction.")


;;; The bottom 32 bits of some instructions.
(defconstant KIL-READ-Q       #x80005000 "read q reg")
(defconstant KIL-READL-G0     #x81800000 "read G0 as left source")
(defconstant KIL-READR-G0     #x60001000 "read G0 as right source")
(defconstant KIL-READ-A0      #x20001000 "read A0 as right source")
(defconstant KIL-READ-O0      #x00001000 "read O0 as right source")
(defconstant KIL-READ-R0      #x40001000 "read R0 as right source")
(defconstant KIL-READ-OAR     #x94001000 "read OAR")
(defconstant KIL-READ-PCTL    #x92001000 "read PCTL")
(defconstant KIL-READ-PSTAT   #x90001000 "read PSTAT")
(defconstant KIL-READ-MSTAT   #xCC001000 "read MSTAT")
(defconstant KIL-READ-MCTL    #xC4001000 "read MCTL")
(defconstant KIL-READ-CSP     #x98001000 "read CSP")
(defconstant KIL-READ-RPC     #x96001000 "read RPC")
(defconstant KIL-READ-OPC     #x9A001000 "read OPC")
(defconstant KIL-READ-OPC+    #x9C001000 "read OPC+")
(defconstant KIL-LR-SUB       #x61844000 "G0 - G0")
(defconstant KIL-READ-VMA     #xD0001000 "Read VMA")
(defconstant KIL-READ-MD      #xD4001000 "Read MD")
(defconstant KIL-READ-MAP     #xC0001000 "Read MAP")
(defconstant KIL-READ-USC     #xC6001000 "Read Microsecond clock")
(defconstant KIL-READ-STAT    #xC8001000 "Read Statistics counter")
(defconstant KIL-READ-TRAP    #xCA001000 "Read Trap Register")
(defconstant KIL-ADD-G0-G0    #x61842000 "Add G0 + G0")
(defconstant KIL-READ-GC-RAMS #xC2001000 "Read gc ram and transporter ram")
(defconstant KIL-FADD-G1-G2   #x63902038 "FADD single G1 to G2")
(defconstant KIL-FIADD-G1-G2  #x6390A02C "FIADD single G1 to G2")
(defconstant KIL-FMUL-G1-G2   #x63900038 "FMUL single G1 to G2")
(defconstant KIL-FMODE0       #x80000210 "FMODE0 Fast, Round nearest")
(defconstant KIL-FMODE1       #x80002010 "FMODE1 DM,DL,P2,P3,P4 Transparent")
(defconstant KIL-FMODE2       #x80004010 "FMODE2 P1 transparent, Max accumulate")
(defconstant KIL-FMODE3       #x80006610 "FMODE3 2264//2265 mode, min latency")
(defconstant kil-read-trap-off #x86001000 "read trap off")
(defconstant kil-field-pass-g0-g1 #x63872000 "field pass g0 g1")

;;; Global variables set up by (k-setup)

(defvar k-slot       nil "High 8 bits of NUBUS address for K processor")
(defvar k-mem-addr   nil "K Processor - Memory address base")
(defvar k-io-addr    nil "K Processor - I/O address base")
(defvar k-mode-addr  nil "K Processor - Mode register address")
(defvar k-hptr-addr  nil "K Processor - History RAM pointer address")
(defvar k-hram-addr  nil "K Processor - History RAM data address")
(defvar k-pc-addr    nil "K Processor - Program Counter address")
(defvar k-mmfio-addr nil "K Processor - MMFIO bus address")
(defvar k-spy0-addr  nil "K Processor - Low spy Instruction Register address")
(defvar k-spy1-addr  nil "K Processor - Hi spy Instruction Register address")
(defvar k-spyc-addr  nil "K Processor - Spy command register address")
(defvar k-int-addr   nil "K Processor - NUBUS interrupt register base address")	

(defvar k-mem-list nil "List of installed memory - Setup by TEST-22")

(defconstant *hram-address-mask* #xFFFFFF)

(defconstant %%program-counter-pc   (byte 24.  0.))
(defconstant %%program-counter-halt (byte  1. 29.))

(defconstant $$halt-bit-running 0)
(defconstant $$halt-bit-halted  1)

(defun daisy-k-write-mode (n)
  (daisy-debug-write-word k-mode-addr n))

(defun daisy-k-read-mode ()
  (daisy-debug-read-word k-mode-addr))

(defun daisy-k-spy-cmd (n)
  (daisy-debug-write-word k-spyc-addr n))

(defconstant $$spy-command-stop                0.)
(defconstant $$spy-command-run                 1.)
(defconstant $$spy-command-step                2.)
(defconstant $$spy-command-reload-instruction  3.)
(defconstant $$spy-command-clear-opc-clock     4.)
(defconstant $$spy-command-set-opc-clock       5.)
(defconstant $$spy-command-clear-spy-mode      6.)
(defconstant $$spy-command-set-spy-mode        7.)
(defconstant $$spy-command-stepmode-full-clock 8.)

;contents of mode register:
;bit 0  write 1 to reset
;bit 1  led
;bit 2  enable mastership
;bits 3,4,5  boot-mode  these are readable by processor
;    0   normal reset
;    1   nubus short reset  (1 tick long)
;    2-7  can be written here, interpreted by software in any case.

(defun daisy-k-spy-i0 (n)
  (daisy-debug-write-word k-spy0-addr n))

(defun daisy-k-spy-i1 (n)
  (daisy-debug-write-word k-spy1-addr n))

(defsubst k-read-spy-halt ()
  (let ((response (debug-read-word k-pc-addr)))
    (if (numberp response)
	(ldb %%program-counter-halt response)
        $$halt-bit-halted)))

(defsubst k-read-spy-pc ()
  (ldb %%program-counter-pc (debug-read-word k-pc-addr)))

(defsubst k-read-spy-mmfio ()
  (debug-read-word k-mmfio-addr))

(defsubst k-read-hptr ()
  (debug-read-word k-hptr-addr))

(defsubst k-read-hram ()
  (logand *hram-address-mask* (debug-read-word k-hram-addr)))

(defsubst k-write-int (int-num value)
  (debug-write-word (+ k-int-addr (ash int-num 2.)) value))

(defsubst k-mem-read (addr)
  (debug-read-word (logior k-mem-addr addr)))

(defsubst k-mem-write (addr data)
  (debug-write-word (logior k-mem-addr addr) data))

(defsubst k-mem-write-byte (addr data)
  (debug-write-byte (logior k-mem-addr addr) data))

(defun dpb32 (value ppss word)
  (let* ((mask1 (ash #xffffffff (byte-size ppss)))
	 (mask2 (logand #xffffffff (lognot (ash mask1 (byte-position ppss)))))
	 (mask3 (logxor #xffffffff mask2)))
    (logior (logand mask3 word)
	    (logand mask2 (ash value (byte-position ppss))))))


(defun check-k-prom (slot &aux s)
  "Routine to check if a slot contains a K processor"
  (setq s "LMI FALCON PROCESSOR")
  (if (fixp (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff7fc)))
      (dotimes (i 15.)
	(cond
	  ((not (equal (logand #xff (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff800 (* i 4))))
		       (aref s i)))
	   (return nil))
	  ((equal i 14.) (return t))))))

(defun k-local-setup (local-slot)
  (setq *local-k-slot* (dpb local-slot (byte 4 0) #xf0))
  (k-setup))

(defun daisy-k-setup (&optional (filename "ed-buffer:daisy-vla"))
  (with-open-file (*daisy-out* filename :direction :output)
    (format *daisy-out* 
"$DATA_HEADER$
$TYPE$
I//O
$FORMAT$
TIME_VALUE
$TOTAL_COLUMNS$
5 37
$BASE$
D B
$END$
")
  ;file format:
  ;5 digit decimal time number
  ;37 bit state.  <control-enable> <start> <ack> <tm1> <tm0> <ad31> .. 32 address-data
  ;always drive START
  ;control enables drive of ACK, TM1, TM0,
  ;AD <enable> enables AD lines.
    (SETQ *LOCAL-K-SLOT* *daisy-k-slot*)
    (SETQ *DAISY-TIME* 1000.)		;allow reset to happen.

    (setq k-slot *local-k-slot*)
    (setq *local-debugging* t)	;looked at by fcns in DEBUG-BOARD file
    (setq k-mem-addr  (ash k-slot 24.))
    (setq k-io-addr   (logior #xfff000 k-mem-addr))
    (setq k-mode-addr (logior #x7fc k-io-addr))
    (setq k-hptr-addr (logior #x600 k-io-addr))
    (setq k-hram-addr (logior #x640 k-io-addr))
    (setq k-pc-addr   (logior #x680 k-io-addr))
    (setq k-mmfio-addr(logior #x6c0 k-io-addr))
    (setq k-spy0-addr (logior #x6c0 k-io-addr))
    (setq k-spy1-addr (logior #x6c4 k-io-addr))
    (setq k-spyc-addr (logior #x700 k-io-addr))
    (setq k-int-addr  (logior #x780 k-io-addr))
    (daisy-k-reset)
    ;(daisy-k-init)	;already does this.
    ))


(defun k-reset ()
   "Issue a reset to the K processor and stops the clocks"
   (do () ((= #x100 (k-read-spy-pc)))
     (k-write-mode 1.)
     (k-stop)
     (k-init)))

(defun daisy-k-reset ()
   "Issue a reset to the K processor and stops the clocks"
   (daisy-k-write-mode 1.)
;   (daisy-k-stop)
   (daisy-k-init))

(defvar k-run-flag nil)

(defun daisy-k-stop ()
  "Stop the processor clocks, and init spy modes"
  (daisy-k-spy-cmd $$spy-command-stop)
  (daisy-k-spy-cmd $$spy-command-stepmode-full-clock)
  (daisy-k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (daisy-k-spy-cmd $$spy-command-clear-opc-clock)     ; clear opc clk
;  (setq k-run-flag nil)
;  (k-read-spy-pc)
  )

(defsubst k-stop-if-running ()
  (when k-run-flag (k-stop)))

(defun k-spy-run ()
  "Start the processor running while fetching from the spy reg"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-opc-clock)
  (k-spy-cmd $$spy-command-set-spy-mode)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-spy-cmd $$spy-command-run)
  (setq k-run-flag t)
  (k-read-spy-pc))

(defun k-run ()
  "Start the processor running"
  (k-spy-cmd $$spy-command-stop)
  (k-spy-cmd $$spy-command-clear-spy-mode)
  (k-spy-cmd $$spy-command-set-opc-clock)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-spy-cmd $$spy-command-run)
  (setq k-run-flag t)
  (k-read-spy-pc))


(defun k-step ()
  "Step the processor one clock cycle"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)
  (k-spy-cmd $$spy-command-clear-spy-mode)
  (k-spy-cmd $$spy-command-set-opc-clock)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-spy-cmd $$spy-command-step)
  (k-read-spy-pc))

;a convenience for figuring out these funny constants.
(defun k-execute-dis (instr-h instr-l)
  (format t "~&~A" (nc:dis (+ (ash instr-h 32.) instr-l))))

(defun k-execute (instr-h instr-l)
  "Load a spy instruction and step the clock once"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step))    ; spy step command

(defun daisy-k-execute (instr-h instr-l)
  "Load a spy instruction and step the clock once"
 ;  (k-stop-if-running)
  (daisy-k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (daisy-k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (daisy-k-spy-i0 instr-l) ; low half instruction
  (daisy-k-spy-i1 instr-h) ; high half instruction
  (daisy-k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (daisy-k-spy-cmd $$spy-command-step))

(defun k-executex (instr-h instr-l)
  "Load a spy instruction and step the clock once"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i1 instr-l) ; low half instruction
  (k-spy-i0 instr-h) ; high half instruction
 (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step))    ; spy step command

(defun k-execute2 (instr-h instr-l)
  "Load a spy instruction and step the clock twice"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step))    ; spy step command


(defun k-execute3 (instr-h instr-l)
  "Load a spy command and step the clock 3 times"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step))    ; spy step command

(defun daisy-k-execute3 (instr-h instr-l)
  "Load a spy command and step the clock 3 times"
;  (k-stop-if-running)
  (daisy-k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (daisy-k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (daisy-k-spy-i0 instr-l) ; low half instruction
  (daisy-k-spy-i1 instr-h) ; high half instruction
  (daisy-k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (daisy-k-spy-cmd $$spy-command-step)     ; spy step command
  (daisy-k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (daisy-k-spy-cmd $$spy-command-step)     ; spy step command
  (daisy-k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (daisy-k-spy-cmd $$spy-command-step))

(defun k-execute4 (instr-h instr-l)
  "Load a spy command and step the clock 4 times"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step))    ; spy step command


(defun k-execute-run (instr-h instr-l)
  "Load a spy command and let the processor run"
  (k-stop-if-running)
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-run))    ; run


(defvar *default-pctl* 0)

(defun k-setup-pctl ()
  (k-execute KIH-LOAD-PCTL 0.)			;flush cache
  (k-execute4 KIH-NOP 0.)
  (k-execute KIH-LOAD-PCTL *default-pctl*))

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

(defun daisy-k-init ()
  "Init some registers, and set the PC to #x0100"
  (daisy-k-stop)
  (daisy-k-execute KIH-JUMP #x100)
  ;; Magic machine unwedger.
;  (dotimes (i 5)
;    (k-spy-cmd $$spy-command-reload-instruction)
;    (k-spy-cmd $$spy-command-clear-spy-mode)
;    (k-spy-cmd $$spy-command-step)
;    (k-spy-cmd $$spy-command-set-spy-mode)
;    (k-execute4 kih-jump #x100))
  (daisy-k-execute KIH-LOAD-VMA 0)
  (daisy-k-execute KIH-LOAD-MAP #x8f)
  (daisy-k-execute3 KIH-JUMP    #x100))

(defvar hbuf (make-array 4096. :type :art-32b))

(defun dump-history (&optional (n 10.))
  (k-stop)
  (setq n (logand 4095. n))
  (do ((i 4094. (1- i)))
      ((minusp i))
    (k-spy-cmd $$spy-command-set-opc-clock)
    (k-spy-cmd $$spy-command-clear-opc-clock)
    (aset (k-read-hram) hbuf i))
  (format t "~%")
  (do ((i (- n 1) (1- i)))
      ((minusp i))
    (format t "HBUF(~A)   PC = ~X~%" (- i) (aref hbuf i))
    (k-spy-cmd $$spy-command-set-opc-clock)
    (k-spy-cmd $$spy-command-clear-opc-clock)))

(defun get-history ()
  (let ((hbuf (make-array 4096.)))
    (k-stop)
    (do ((i 4094. (1- i)))
	((minusp i))
      (k-spy-cmd $$spy-command-set-opc-clock)
      (k-spy-cmd $$spy-command-clear-opc-clock)
      (setf (aref hbuf i) (logand #xFFFFFF (k-read-hram))))
    hbuf))

(defun set-nu (slot)
  (k-reset)
  (k-init)
  (k-execute kih-load-map (logior #xF000000F (ash slot 24.)))
  (k-execute3 kih-nop 0)
  (k-write-mode 6.))

(defun rnu (addr)
  (k-execute kih-load-vma-sr addr)
  (k-execute kih-nop 0)
  (k-execute4 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))

(defun wnu (addr data)
  (k-execute kih-load-vma addr)
  (k-execute kih-load-md-sw data)
  (k-execute4 kih-nop 0))

  
(defun w-vma (data)
  (k-execute kih-load-vma data)
  (k-execute4 kih-nop 0))

(defun r-vma ()
  (k-execute4 kih-alu-nop kil-read-vma)
  (k-read-spy-mmfio))

  
(defun w-md (data)
  (k-execute kih-load-md data)
  (k-execute4 kih-nop 0))

(defun r-md ()
  (k-execute4 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))


(defun x100 ()
  (k-stop)
  (k-reset)
  (dotimes (i 1024.)
    (debug-write-word (logior #xfc000000 (* i 4)) i))
  (debug-write-word #xfc000800 #x100)
  (debug-write-word #xfc000804 kih-jump)
  (k-init)
  (k-execute kih-load-vma #x02000000)
  (k-execute kih-load-map #xfc000005)
  (k-execute kih-load-mctl #x40000)
  (k-execute4 kih-jump #x100)
  (k-step)
  (k-read-spy-pc))

(defun zap-mem ()
  (dotimes (i 4096.)
    (k-mem-write (* i 8) 0)
    (k-mem-write (logior 4 (* i 8)) kih-jump)))

(defun zorch-mem ()
  (dotimes (i 4096.)
    (k-mem-write (* i 8) -1)
    (k-mem-write (logior 4 (* i 8)) 0)))



(defun ch-init ()
  (k-init)
  (k-execute kih-load-csp #xffff)
  (k-execute3 kih-nop 0)
  (do ((i 255. (sub1 i)))
      ((< i 0))
    (k-execute kih-load-oar i)
    (k-execute3 kih-nop 0)
    (k-execute kih-xreturn 0))
  (k-execute3 kih-nop 0)
  (k-execute kih-load-csp #xec00)
  (k-execute kih-load-oar #x111110)
  (k-execute3 kih-nop 0))

(defun dump-ch ()
  (let* ((hp-csp (progn
		  (k-execute3 kih-alu-nop kil-read-csp)
		  (k-read-spy-mmfio)))
	 (oar    (progn
		   (k-execute3 kih-alu-nop kil-read-oar)
		   (k-read-spy-mmfio))))
    (format t "~%OAR - ~X        HP-CSP - ~X~%" (logand #xffffff oar) (logand #xffff hp-csp))
    (do ((hp (logand 255. (ash hp-csp -8.)) (logand 255. (sub1 hp))))
	 ((= hp #xF0))
      (when (= hp #XFF) (format t "*Yellow alert frames*~%"))
      (k-execute kih-open-call #x100)
      (k-execute4 kih-alu-nop kil-read-oar)
      (format t "HP - ~X       HEAP - ~X~%" hp (logand 255. (ash (k-read-spy-mmfio) -16.))))
    (k-execute kih-load-oar oar)
    (k-execute kih-load-csp hp-csp)
    (k-execute4 kih-nop 0)))
      

(defun k-set-pc (addr)
  (k-execute4 KIH-JUMP addr))

(defconstant map-bits-invalid #x0)
(defconstant map-bits-r       #x5)
(defconstant map-bits-rw      #xF)

(defconstant cluster-size     #x400)

(defun k-map-cluster (virtual-address physical-address bits)
;    (format t "~%VA ~X     PA ~X" virtual-address physical-address)
  (k-execute kih-jump #x100)
  (k-execute kih-load-vma virtual-address)
  (k-execute kih-load-map (logior physical-address bits)))

(defun k-map-local-ram (virtual-address length bits &optional (offset 0))
  (do ((pa offset (+ pa (ash cluster-size 2)))
       (va virtual-address (+ va cluster-size))
       (len length (- len cluster-size)))
      ((zerop len))
    (k-map-cluster va pa (logior #x80 bits))))


(defun k-map-nubus-slot (virtual-address slot length bits &optional (offset 0))
  (do ((pa (logior (ash (logior #xF0 slot) 24.) offset)
	   (+ pa (ash cluster-size 2)))
       (va virtual-address (+ va cluster-size))
       (len length (- len cluster-size)))
      ((zerop len))
    (k-map-cluster va pa bits)))

(defvar memory-slot #xC)
(defvar vcmem-slot #xC)

(defconstant screen-virtual-address #x03000000)

(defun k-init-virtual-memory ()
  ;; map code space
  (k-map-local-ram #x02000000  #x8000 map-bits-r 0)
  ;; map data space
  (k-map-local-ram #x00000000  #x8000 map-bits-rw 0)
  ;; map vcmem board
;  (k-map-local-ram screen-virtual-address (* 32. 1024.) map-bits-rw #x20000)
  )

(defun map-vcmem ()
  (k-map-nubus-slot screen-virtual-address vcmem-slot (* 32. 1024.) map-bits-rw #x20000))

(defun k-start (starting-address)
  (k-init)
  (ch-init)
  (k-init-virtual-memory)
  (k-execute4 kih-jump #x100)
  (k-execute kih-load-mctl #x40000)
  (k-execute4 kih-jump #x100)
  (k-set-pc starting-address)
  (k-setup-pctl)
  (k-set-pc starting-address)
  (k-spy-cmd 3))


(defun k-go (starting-address)
  (k-setup-pctl)
  (k-start starting-address)
  (k-run))

(defun read-inst (addr)
  (logior
    (k-mem-read (logand (* 8 addr) #xFFFFFF))
    (ash (k-mem-read (logand (+ 4 (* 8 addr)) #xFFFFFF)) 32.)))
    
(defun write-inst (adr inst)
  (k-mem-write (* adr 8) (logand #xffffffff inst))
  (k-mem-write (+ 4 (* adr 8)) (logand #xffffffff (ash inst -32.))))

(defun write-inst-and-check (adr inst)
  (let ((d1 (logand #xffffffff inst))
	(d2 (logand #xffffffff (ash inst -32.)))
	(tem nil))
    (k-mem-write (* adr 8) d1)
    (k-mem-write (+ 4 (* adr 8)) d2)
    (cond ((not (= d1 (setq tem (k-mem-read (* adr 8)))))
	   (error "D1 failed, wrote ~s, read ~s" d1 tem)))
    (cond ((not (= d2 (setq tem (k-mem-read (+ 4 (* adr 8))))))
	   (error "D2 failed, wrote ~s, read ~s" d2 tem)))))

(defun read-active (reg)
  (k-execute4 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-A0))
  (k-read-spy-mmfio))

(defun read-open (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-read-spy-mmfio))

(defun read-open-boxed (reg)
  (k-execute2
    (dpb reg (byte 4. (- 41. 32.)) KIH-ALU-O0)
    (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun read-vma-boxed ()
  (k-execute2
    (dpb 9. (byte 4. (- 41. 32.)) KIH-ALU-G0)
    KIL-READ-VMA)
  (k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun read-md-boxed ()
  (k-execute2
    (dpb 9. (byte 4. (- 41. 32.)) KIH-ALU-G0)
    KIL-READ-MD)
  (k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun read-return (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-R0))
  (k-read-spy-mmfio))

(defun k-read-oar ()
  (k-execute3 KIH-NOP 0)
  (k-execute4 KIH-ALU-NOP KIL-READ-OAR)
  (k-read-spy-mmfio))

(defun k-write-oar (value)
  (k-execute kih-load-oar value)
  (k-execute2 kih-jump #x100)
  (k-execute3 kih-jump #x100))

(defun read-q ()
  (k-execute3 KIH-ALU-NOP KIL-READ-Q)
  (k-read-spy-mmfio))

(defun pr (addr)
  (k-execute kih-load-vma-sr addr)
  (k-execute kih-jump #x100)
  (k-execute3 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))

(defun dump-pr (addr &optional (count 8.))
  (dotimes (i count)
    (format t "~&VMA=~x   MD=~x" (+ addr i) (pr (+ addr i)))))

(defun dump-m (addr &optional (count 8.))
  (dotimes (i count)
    (format t "~&PMA=~x   MD=~x" (+ addr (* i 4)) (k-mem-read (+ addr (* i 4))))))

(defun fill-m ()
  (dotimes (i #x8000)
    (k-mem-write (ash i 2) -1)))

(defun k-read-memory-map (entry)
  (k-execute2 KIH-LOAD-VMA (ash entry 10.))
  (k-execute4 KIH-JUMP #x100)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun k-read-current-map-address ()
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun k-write-current-map-entry (value)
  (k-execute4 kih-load-map value)
  (k-execute4 kih-jump #x100)
  (k-execute4 kih-jump #x100)
  )

(defun k-write-memory-map (entry value)
  (k-execute2 kih-load-vma (ash entry 10.))
  (k-execute4 kih-jump #x100)
  (k-execute4 kih-load-map value)
  (k-execute4 kih-jump #x100)
  (k-execute4 kih-jump #x100)
  )

(defun k-load-memory-control (value)
  (k-execute  kih-load-mctl value)
  (k-execute4 kih-jump #x100))

(defun k-read-memory-control ()
  (k-execute3 kih-alu-nop kil-read-mctl)
  (prog1 (k-read-spy-mmfio)
	 (k-execute kih-jump #X100)))

(defun k-read-processor-control ()
  (k-execute3 kih-alu-nop kil-read-pctl)
  (prog1 (k-read-spy-mmfio)
	 (k-execute kih-jump #X100)))

(defun k-write-processor-control (value)
  (k-execute  kih-load-pctl value)
  (k-execute4 kih-jump #x100))

(defun k-read-virtual-memory (location)
  (k-execute kih-load-vma-sr location)
  (k-execute4 kih-jump #x100)
  (k-execute3 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))

(defun k-write-virtual-memory (location value)
  (k-execute kih-load-md value)
  (k-execute kih-load-vma-sw location)
  (k-execute3 kih-jump #x100))

(defun k-vma-start-read (vma-boxed md-boxed trans-type location)
  (k-execute
    (dpb md-boxed (byte 1. (- 55. 32.))
	 (dpb vma-boxed (byte 1. (- 54. 32.))
	      (dpb trans-type (byte 2. (- 41. 32.))
		   kih-load-vma-sr)))
    location)
  (k-execute4 kih-jump #x100))

(defun k-read-gc-ram (location)
  (k-execute kih-load-md (ash location 14.))
  (k-execute4 kih-jump #x100)
  (k-execute3 kih-alu-nop kil-read-gc-rams)
  (logand #xF (k-read-spy-mmfio)))

(defun k-write-gc-ram (location value)
  (k-execute kih-load-md (ash location 14.))
  (k-execute4 kih-jump #x100)
  (k-execute kih-load-gc-ram value)
  (k-execute3 kih-jump #X100))

(defun k-address-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute3 kih-alu-nop kil-read-mctl)
  (let ((memcontrol (dpb trans-mode hw:%%memory-control-transporter-mode (k-read-spy-mmfio))))
    (k-execute kih-load-mctl memcontrol)
    (k-execute4 kih-jump #x100))
  (let ((vma-start-read-instruction
	  (dpb vma-boxed (byte 1. (- 54. 32.))
	       (dpb trans-type (byte 2. (- 41. 32.))
		    kih-load-vma-sr))))
    (k-execute vma-start-read-instruction 0))
  (k-execute2 kih-jump #x100)
  (k-execute (dpb md-boxed (byte 1. (- 55. 32.)) kih-load-md)
	     (dpb datatype (byte 6. 26.) 0))
  (k-execute4 kih-jump #X100))

(defun k-read-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype)
  (k-address-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute3 kih-alu-nop kil-read-gc-rams)
  (ldb hw:%%transporter-ram-bus-offset (k-read-spy-mmfio)))

(defun k-write-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype data)
  (k-address-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute2 kih-load-transporter (dpb data hw:%%transporter-ram-bus-offset 0))
  (k-execute4 kih-jump #X100))

(defun k-read-trap-register ()
  (k-execute3 kih-alu-nop kil-read-trap)
  (k-read-spy-mmfio))

(defun saving-oar (thunk)
  (let ((oar (k-read-oar)))
    (unwind-protect
	(funcall thunk oar)
      (k-write-oar oar))))

(defun k-read-hp-sp ()
  (k-execute3 kih-alu-nop kil-read-csp)
  (k-read-spy-mmfio))

(defun k-write-hp-csp (value)
  (k-execute kih-load-csp value)
  (k-execute4 kih-jump #X100))

(defun saving-hp-csp (thunk)
  (let ((hp-csp (k-read-hp-sp)))
    (unwind-protect
	(funcall thunk
		 (ldb (byte 8 8) hp-csp)	;hw:%%ch-csphp-heap-pointer
		 (ldb (byte 8 0) hp-csp))	;hw:%%ch-csphp-call-stack-pointer
      (k-write-hp-csp hp-csp))))

(defun k-read-frame (frame-number)
  (saving-oar
    #'(lambda (oar)
	(k-write-oar (dpb frame-number hw:%%ch-oar-open oar))
	(labels ((read-regs (count values)
		   (if (= count hw:frame-size)
		       values
		       (let ((value (read-open       count))
			     (boxed (read-open-boxed count)))
			 (read-regs (1+ count) (cons (list count boxed value) values))))))
      (read-regs 0 '())))))

(defun k-read-register (frame offset)
  (saving-oar
    #'(lambda (ignore)
	(k-write-oar (dpb frame hw:%%ch-oar-open 0))
	(list (read-open offset)
	      (read-open-boxed offset)))))

(defun k-read-retpc-rdest ()
  (k-execute3 kih-alu-nop kil-read-rpc)
  (k-read-spy-mmfio))

(defun k-read-trap-off ()
  (k-execute  kih-alu-nop kil-read-trap-off)
  (k-execute2 kih-alu-nop 0)
  (k-read-spy-mmfio))

(defun k-read-memory-status ()
  (k-execute3 kih-alu-nop kil-read-mstat)
  (k-read-spy-mmfio))