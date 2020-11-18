;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-

(export '(
          k-init
          k-go
          k-read-oar k-set-pc
          k-load-memory-control
          k-mem-write
          k-mem-read
          k-read-gc-ram
          k-read-memory-control
          k-read-memory-map
          k-read-processor-control
          k-read-virtual-memory
          k-read-spy-halt
          k-read-spy-mmfio
          k-read-spy-pc
          k-read-transporter-ram
          k-run
          k-start
          k-step
          k-stop
          k-write-gc-ram
          k-write-memory-map
          k-write-processor-control
          k-write-transporter-ram
          k-write-virtual-memory
          read-inst write-inst
          read-open read-active read-return read-q
          screen-virtual-address))

;;; The top 32 bits of some useful instructions.
(defconstant KIH-NOP          #x0F008000 "loadi-32 to no-op")
(defconstant KIH-ALU-NOP      #x03008000 "generic ALU instruction w/NOP dest")
(defconstant KIH-ALU-A0       #x03004000 "generic ALU instruction w/A0 dest")
(defconstant KIH-DISPATCH     #x01008000 "dispatch instruction")
(defconstant KIH-DISPATCH-X16 #x21008000 "dispatch x16 instruction")
(defconstant KIH-JUMP         #x24008000 "jump unconditional")
(defconstant KIH-JCOND        #x08008000 "jump conditional")
(defconstant KIH-CALL         #x28028018 "call")
(defconstant kih-open         #x0F018000 "loadi-32 to no-op, open")
(defconstant KIH-OPEN-CALL    #x08038018 "open-call")
(defconstant KIH-RETURN       #x02048000 "return")
(defconstant KIH-RETURN-FD    #x0E048200 "return w/FDEST")
(defconstant KIH-XRETURN      #x03048000 "strange return")
(defconstant KIH-OPEN-CALL-X  #x08038000 "open-call")
(defconstant KIH-TOPEN        #x0F058000 "topen")
(defconstant KIH-TCALL        #x08068000 "tcall")
(defconstant KIH-TOPEN-TCALL  #x08078000 "topen-tcall")
(defconstant KIH-LOAD-G0      #x0F006000 "load G0 register instruction")
(defconstant KIH-LOAD-G1      #x0F006200 "load G1 register instruction")
(defconstant KIH-LOAD-G2      #x0F806400 "load G2 register instruction")
(defconstant KIH-LOAD-G9      #x0F804000 "load A0 register instruction")
(defconstant KIH-LOAD-A0      #x0F002000 "load A0 register instruction")
(defconstant KIH-LOAD-O0      #x0F000000 "load O0 register instruction")
(defconstant KIH-LOAD-R0      #x0F004000 "load R0 register instruction")
(defconstant KIH-LOAD-PCTL    #x0F009200 "load processor control reg")
(defconstant KIH-LOAD-MCTL    #x0F00C400 "load memory control reg")
(defconstant KIH-LOAD-VMA     #x0F00D000 "load VMA")
(defconstant KIH-LOAD-VMA-SR  #x0F00E000 "load VMA start read")
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
(defconstant kih-branch       #x20008000 "Short branch instruction.")


;;; The bottom 32 bits of some useful instructions.
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

(defsubst k-write-mode (n)
  (debug-write-word k-mode-addr n))

(defsubst k-read-mode ()
  (debug-read-word k-mode-addr))

(defsubst k-spy-cmd (n)
  (debug-write-word k-spyc-addr n))

(defsubst k-spy-i0 (n)
  (debug-write-word k-spy0-addr n))

(defsubst k-spy-i1 (n)
  (debug-write-word k-spy1-addr n))

(defsubst k-read-spy-halt ()
  (ldb (byte 1. 29.) (debug-read-word k-pc-addr)))

(defsubst k-read-spy-pc ()
  (logand #xffffff (debug-read-word k-pc-addr)))

(defsubst k-read-spy-mmfio ()
  (debug-read-word k-mmfio-addr))

(defsubst k-read-hptr ()
  (debug-read-word k-hptr-addr))

(defsubst k-read-hram ()
  (logand #xffffff (debug-read-word k-hram-addr)))

(defsubst k-write-int (int-num value)
  (debug-write-word (+ k-int-addr (ash int-num 2.)) value))

(defsubst k-mem-read (addr)
  (debug-read-word (logior k-mem-addr addr)))

(defsubst k-mem-write (addr data)
  (debug-write-word (logior k-mem-addr addr) data))

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
          ((not (equal (logand #xff (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff800 (* i 4)))) (aref s i)))
           (return nil))
          ((equal i 14.) (return t))))))

(defun k-setup ()
  "Sets up and initializes a debug link to a K processor"
  (init-debug-board)
  (setq k-slot nil)
  (dotimes (i 16.)
    (when (check-k-prom i)
      (setq k-slot (logior #xf0 i))
      (format t "~%   K processor found in slot ~A.~%" i)))
  (when (null k-slot)
      (format t "~%*** Can't setup, I couldn't find a processor! ***~%")
      (cerror "Choose a slot number" "Can't setup, no processor!")
      (setq k-slot (logior #xf0 (logand #x0f (progn (format t "~&Slot number?") (read))))))

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
    (k-reset))


 (defun k-reset ()
   "Issue a reset to the K processor and stops the clocks"
   (k-write-mode 1.)
   (k-stop))

(defvar k-run-flag nil)

(defun k-stop ()
  "Stop the processor clocks, and init spy modes"
  (k-spy-cmd 0.)     ; stop
  (k-spy-cmd 8.)     ; stepmode - full clock
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-cmd 4.)     ; clear opc clk
  (setq k-run-flag nil)
  (k-read-spy-pc))

(defsubst k-stop-if-running ()
  (when k-run-flag (k-stop)))

(defun k-spy-run ()
  "Start the processor running while fetching from the spy reg"
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 5.)
  (k-spy-cmd 7.)
  (k-spy-cmd 3.)
  (k-spy-cmd 1.)
  (setq k-run-flag t)
  (k-read-spy-pc))

(defun k-run ()
  "Start the processor running"
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 6.)     ; clear spy mode
  (k-spy-cmd 5.)    ; set opc clock
  (k-spy-cmd 3.)    ; reload instruction
  (k-spy-cmd 1.)    ; run
  (setq k-run-flag t)
  (k-read-spy-pc))


(defun k-step ()
  "Step the processor one clock cycle"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 6.)           ; clr spy mode
  (k-spy-cmd 5.)           ; set opc clock
  (k-spy-cmd 3.)           ; reload instr
  (k-spy-cmd 2.)           ; spy step command
  (k-read-spy-pc))

(defun k-execute (instr-h instr-l)
  "Load a spy instruction and step the clock once"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
 (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.))    ; spy step command

(defun k-executex (instr-h instr-l)
  "Load a spy instruction and step the clock once"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-i1 instr-l) ; low half instruction
  (k-spy-i0 instr-h) ; high half instruction
 (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.))    ; spy step command

(defun k-execute2 (instr-h instr-l)
  "Load a spy instruction and step the clock twice"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.))    ; spy step command


(defun k-execute3 (instr-h instr-l)
  "Load a spy command and step the clock 3 times"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.))    ; spy step command

(defun k-execute4 (instr-h instr-l)
  "Load a spy command and step the clock 4 times"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.)     ; spy step command
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 2.))    ; spy step command


(defun k-execute-run (instr-h instr-l)
  "Load a spy command and let the processor run"
  (k-stop-if-running)
  (k-spy-cmd 0.)     ; ensure that the run bit is off
  (k-spy-cmd 7.)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd 3.)     ; force icache load
  (k-spy-cmd 1.))    ; run


(defvar *default-pctl* 4)

(defun k-setup-pctl ()
  (k-execute KIH-LOAD-PCTL 0.)                  ;flush cache
  (k-execute4 KIH-NOP 0.)
  (k-execute KIH-LOAD-PCTL *default-pctl*))

(defun k-init ()
  "Init some registers, and set the PC to #x0100"
  (k-stop)
  (k-execute KIH-JUMP #x100)
  (dotimes (i 5)
    (k-spy-cmd 3)
    (k-spy-cmd 6)
    (k-spy-cmd 2)
    (k-spy-cmd 7))
  (k-execute KIH-LOAD-VMA 0)
  (k-execute KIH-LOAD-MAP #x8f)
  (k-execute3 KIH-JUMP #x100))

(defconst bit-pattern (make-array 32. :initial-contents
                        '(#x00000001 #x00000002 #x00000004 #x00000008
                          #x00000010 #x00000020 #x00000040 #x00000080
                          #x00000100 #x00000200 #x00000400 #x00000800
                          #x00001000 #x00002000 #x00004000 #x00008000
                          #x00010000 #x00020000 #x00040000 #x00080000
                          #x00100000 #x00200000 #x00400000 #x00800000
                          #x01000000 #x02000000 #x04000000 #x08000000
                          #x10000000 #x20000000 #x40000000 #x80000000)))


(defconst k-diag-error-count 0. "Numbers of errors that have occured")


(defun k-diag-reset ()
  "Init the K processor and reset the error count"
  (k-reset)
  (k-stop)
  (setq k-diag-error-count 0.))


(defun k-diag-error (info addr expect got)
  "Issue and error message, and increment the error count"
  (setq k-diag-error-count (add1 k-diag-error-count))
  (if (null addr)
      (format t "   *** Error ~A --- Expected ~X   got ~X~%"
              info expect got)
      (format t "   *** Error ~A at address ~X --- Expected ~X   got ~X~%"
              info addr expect got)))

(defun k-mode-test (&aux temp)
  "NUBUS Mode register test"
  (format t "Starting TEST-MODE - NUBUS Mode register~%")
  (dotimes (i 5.)
    (k-write-mode (ash 1 (add1 i)))
    (when (not (equal (setq temp (logand #x3e (k-read-mode))) (ash 1 (add1 i))))
      (k-diag-error "TEST-MODE Nubus mode register" nil (ash 1 (add1 i)) temp))))

(defun k-test0 (&aux temp)
  "Shifting bit pattern test on the PC."
  (format t "Starting Test 0 - PC~%")
  (k-init)
  (setq temp (k-read-spy-pc))
  (if (not (equal #x100 temp))
      (k-diag-error "TEST-0 - Can't init PC" nil #x100 temp)
      (dotimes (i 24.)
        (k-execute KIH-JUMP (ash 1. i))         ; set PC
        (setq temp (k-read-spy-pc))
        (when (not (equal (ash 1. i) temp))
          (k-diag-error "TEST-0 PC failure" nil (ash 1. i) temp)))))


(defun k-test1 (&aux temp)
  "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 - Simple data path.~%")
  (k-init)
  (k-execute KIH-NOP 1.)
  (k-execute KIH-NOP 2.)
  (dotimes (i 32.)
    (k-execute KIH-NOP (ash 1. (logand 31. (+ i 2.))))
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-1 Data Path" nil (ash 1. i) temp))))


(defun k-test2 (&aux temp)
  "Simple G0 left//right register test."
  (format t "Starting Test 2 - G0 left//right register test.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute2 KIH-LOAD-G0 (ash 1. i))         ; write G0
    (k-execute2 KIH-NOP 0.)                     ; avoid passaround
    (k-execute KIH-ALU-NOP KIL-READL-G0)        ; read G0 left
    (k-execute KIH-ALU-NOP KIL-READR-G0)        ; read G0 right
    (k-execute KIH-NOP 0.)                      ; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-2 G0 Left " nil (ash 1. i) temp))
    (k-execute KIH-JUMP #x100)                  ; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-2 G0 Right" nil (ash 1. i) temp))))


(defun k-test3 (&aux temp)
  "Simple G0 left//right passaround test."
  (format t "Starting Test 3 - G0 left//right passaround.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute KIH-LOAD-G0 (ash 1. i))          ; write G0
    (k-execute KIH-ALU-NOP KIL-READL-G0)        ; read G0 left
    (k-execute KIH-JUMP #x100)                  ; wait for pipeline
    (k-execute KIH-NOP 0.)                      ; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-3 Passaround Left " nil (ash 1. i) temp)))
  (dotimes (i 32.)
    (k-execute KIH-LOAD-G0 (ash 1. i))          ; write G0
    (k-execute KIH-ALU-NOP KIL-READR-G0)        ; read G0 right
    (k-execute KIH-JUMP #x100)                  ; wait for pipeline
    (k-execute KIH-NOP 0.)                      ; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-3 Passaround Right" nil (ash 1. i) temp))))


(defun k-test4 (&aux temp)
  "PC incrementer test."
  (format t "Starting Test 4 - PC Incrementer.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute KIH-JUMP (sub1 (ash 1. i)))      ; PC = (2 ** i) - 1
    (k-execute KIH-NOP 0.)                      ; inc PC
    (setq temp (logand #x0ffffff (k-read-spy-pc)))
      (when (not (equal (ash 1. i) temp))
         (k-diag-error "TEST-4 PC Incrementer" nil (ash 1. i) temp))))

(defun k-test5 (&aux temp)
  "PC Dispatch test."
  (format t "Starting Test 5 - PC Dispatch.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute KIH-NOP (ash 1. i))              ; shifted bit
    (k-execute KIH-NOP 0.)                      ; pipeline
    (k-execute KIH-DISPATCH 0.)                 ; load PC
    (setq temp (k-read-spy-pc))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-5 PC Dispatch" nil (ash 1. i) temp))
    (k-execute KIH-JUMP #x100))
  (k-execute KIH-NOP #x0FFFFFF)                 ; pattern
  (k-execute KIH-NOP 0.)                        ; pipeline
  (k-execute KIH-DISPATCH-X16 0.)               ; load PC w/low 4 bits masked
  (setq temp (k-read-spy-pc))
  (when (not (equal #x0FFFFF0 temp))
    (k-diag-error "TEST-5 PC Dispatch X16" nil #x0FFFFF0 temp)))


(defun w (reg n)
  (k-execute3 KIH-JUMP #x100)
  (k-execute (dpb reg (byte 4. 9.) KIH-LOAD-G0) n)
  (k-execute3 KIH-NOP 0))

(defun r (reg &optional (gframe 0))
  (k-execute4 (dpb gframe (byte 4. 5.) KIH-ALU-NOP) (dpb reg (byte 4. 25.) KIL-READR-G0))
  (k-read-spy-mmfio))


(defun k-test6 (&aux temp)
  "Simple test G0 - G255"
  (format t "Starting Test 6 - Simple G0 - G255.~%")
  (k-init)
  (dotimes (k 16.)                              ;16 global-frames
    (dotimes (j 32.)                            ; 32 patterns
      (k-execute KIH-JUMP #x100)                ; reset PC
      (dotimes (i 16.)                          ; G0 - G15
        (k-execute (dpb i (byte 4. 9.)
                        (dpb k (byte 4. 5.) KIH-LOAD-G0))       ; load reg
                   (aref bit-pattern (logand 31. (+ i j)))))
      (k-execute KIH-NOP 0)
      (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 0. (byte 4. 25.) KIL-READR-G0))  ; read G0
      (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 1. (byte 4. 25.) KIL-READR-G0))  ; read G1
      (dotimes (i 16.)                          ; read the rest, and compare values
        (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb (+ i 2) (byte 4. 25.) KIL-READR-G0))
        (setq temp (k-read-spy-mmfio))
        (when (not (equal (aref bit-pattern (logand 31. (+ i j))) temp))
          (k-diag-error "TEST-6 Global Regs"
                        i (aref bit-pattern (logand 31. (+ i j))) temp))))))


(defun k-test7 (&aux temp)
  "Open, Active, Return (OAR) register test."
  (format t "Starting Test 7 - Open, Active, Return (OAR) register.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute  KIH-LOAD-OAR (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute  KIH-ALU-NOP KIL-READ-OAR)
    (k-execute2 KIH-NOP 0.)
    (setq temp (logand #x0FFFFFF (k-read-spy-mmfio)))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-7 OAR" nil (ash 1. i) temp))))


(defun k-test8 (&aux temp)
  "Register RAM data test."
  (format t "Starting Test 8 - Register RAM data.~%")
  (k-init)
  (dotimes (pat 32.)
    (format t "   Pass ~A~%" pat)
    (dotimes (frame 256.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
      (dotimes (i 16.)
        (k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)     ;load regs
                   (aref bit-pattern (logand 31. (+ i pat))))))
    (dotimes (frame 256.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
;      (k-execute KIH-ALU-NOP (dpb 0 (byte 4. 25.) KIL-READ-A0))
;      (k-execute KIH-ALU-NOP (dpb 1 (byte 4. 25.) KIL-READ-A0))
      (dotimes (i 16.)
        (k-execute3 KIH-ALU-NOP                         ;read regs
                   (dpb i  (byte 4. 25.) KIL-READ-A0))
        (setq temp (k-read-spy-mmfio))
        (when (not (equal (aref bit-pattern (logand 31. (+ i pat))) temp))
          (k-diag-error "TEST-8 Register data" (+ (ash frame 4.) i)
                        (aref bit-pattern (logand 31. (+ i pat))) temp))))))

(defun k-test8a (&aux temp)
  "Register RAM data test."
  (format t "Starting Test 8 - Register RAM data.~%")
  (k-init)
    (dotimes (frame 256.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
      (dotimes (i 16.)
        (k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)     ;load regs
                   (aref bit-pattern (logand 31. i))))))

;(defun k-test8b (&aux temp)
;  "Register RAM data test."
;  (format t "Starting Test 8 - Register RAM data.~%")
;  (k-init)
;;  (k-execute KIH-LOAD-OAR (ash frame 8.))             ;Active = frame
;  (k-execute3 KIH-JUMP #x100)                  ;reset PC
;  (dotimes (i 16.)
;    (k-execute (dpb i (byte 4. 9.) KIH-LOAD-g0)
;              (let ((pat (aref bit-pattern (logand 31. i))))
;                (format t "~&Wrote ~x" pat)
;                pat)))
;  (k-execute3 kih-jump #x100))


;    (dotimes (frame 256.)
;      (k-execute KIH-LOAD-OAR (ash frame 8.))          ;Active = frame
;      (k-execute3 KIH-JUMP #x100)                      ;reset PC
;;      (k-execute KIH-ALU-NOP (dpb32 0 (byte 4. 25.) KIL-READ-A0))
;;      (k-execute KIH-ALU-NOP (dpb32 1 (byte 4. 25.) KIL-READ-A0))
;      (dotimes (i 16.)
;       (k-execute3 KIH-ALU-NOP                         ;read regs
;                  (dpb32 i  (byte 4. 25.) KIL-READ-A0))
;       (setq temp (k-read-spy-mmfio))
;       (when (not (equal (aref bit-pattern (logand 31. (+ i pat))) temp))
;         (k-diag-error "TEST-8 Register data" (+ (ash frame 4.) i)
;                       (aref bit-pattern (logand 31. (+ i pat))) temp))))))


(defun k-test9 (&aux temp)
  "Register RAM address test."
  (format t "Starting Test 9 - Register RAM address.~%")
  (k-init)
  (dotimes (pat 32.)
     (format t "  Pass ~D~%" pat)
     (dotimes (frame 256.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
      (dotimes (i 16.)
        (k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)     ;load regs
                   (+ (ash frame 4.) i))))
    (dotimes (frame 256.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
      (k-execute KIH-ALU-NOP (dpb 0  (byte 4. 25.) KIL-READ-A0))
      (k-execute KIH-ALU-NOP (dpb 1 (byte 4. 25.) KIL-READ-A0))
      (dotimes (i 16.)
        (k-execute KIH-ALU-NOP                          ;load regs
                   (dpb (+ i 2) (byte 4. 25.) KIL-READ-A0))
        (setq temp (k-read-spy-mmfio))
        (when (not (equal (+ (ash frame 4.) i) temp))
          (k-diag-error "TEST-9 Register address" (+ (ash frame 4.) i)
                        (+ (ash frame 4.) i) temp))))))

(defun k-test10-jtest (left right jcond jump msg &aux temp)
  (setq temp (dpb right (byte 4. 25.) KIL-LR-SUB))
  (setq temp (dpb left (byte 4. 19.) temp))
  (k-execute KIH-ALU-NOP temp)                     ; (- left right)
  (k-execute (dpb jcond (byte 3. 2.) KIH-JUMP) #x100)   ; Jump 100, sel jcond
  (k-execute KIH-JCOND #xFFFFFC)                   ; Conditional jump FFFFFC
  (setq temp (k-read-spy-pc))
  (when (and jump (not (equal temp #xFFFFFC)))
        (k-diag-error (format nil "Test 10 - ~A" msg) nil 0 temp))
  (when (and (not jump) (not (equal temp #x101)))
        (k-diag-error (format nil "Test 10 - ~A" msg) nil 0 temp)))

(defun k-test10 ()
  "Branch condition test."
  (format t "Starting Test 10 - Branch conditions.~%")
  (k-init)
  (k-execute (dpb 1 (byte 4. 9.) KIH-LOAD-G0) 1.)     ; G1 = 1
  (k-execute (dpb 2 (byte 4. 9.) KIH-LOAD-G0) 2.)     ; G2 = 2
  (k-execute (dpb 3 (byte 4. 9.) KIH-LOAD-G0) #x80000000) ; G3 = - (BIG)
  (k-execute (dpb 4 (byte 4. 9.) KIH-LOAD-G0) #xFFFFFFFF) ; G4 = -1

  (k-test10-jtest 1. 1. 2. t   "EQ (1 1)")
  (k-test10-jtest 1. 1. 3. nil "NEQ (1 1)")
  (k-test10-jtest 1. 1. 4. nil "LT (1 1)")
  (k-test10-jtest 1. 1. 5. t   "GE (1 1)")
  (k-test10-jtest 1. 1. 6. nil "GT (1 1)")
  (k-test10-jtest 1. 1. 7. t   "LE (1 1)")
  (k-test10-jtest 2. 1. 2. nil "EQ (2 1)")
  (k-test10-jtest 2. 1. 3. t   "NEQ (2 1)")
  (k-test10-jtest 2. 1. 4. nil "LT (2 1)")
  (k-test10-jtest 2. 1. 5. t   "GE (2 1)")
  (k-test10-jtest 2. 1. 6. t   "GT (2 1)")
  (k-test10-jtest 2. 1. 7. nil "LE (2 1)")
  (k-test10-jtest 4. 1. 2. nil "EQ (-1 1)")
  (k-test10-jtest 4. 1. 3. t   "NEQ (-1 1)")
  (k-test10-jtest 4. 1. 4. t   "LT (-1 1)")
  (k-test10-jtest 4. 1. 5. nil "GE (-1 1)")
  (k-test10-jtest 4. 1. 6. nil "GT (-1 1)")
  (k-test10-jtest 4. 1. 7. t   "LE (-1 1)")
  (k-test10-jtest 1. 3. 2. nil "EQ (1 -BIG)")
  (k-test10-jtest 1. 3. 3. t   "NEQ (1 -BIG)")
  (k-test10-jtest 1. 3. 4. nil "LT (1 -BIG)")
  (k-test10-jtest 1. 3. 5. t   "GE (1 -BIG)")
  (k-test10-jtest 1. 3. 6. t   "GT (1 -BIG)")
  (k-test10-jtest 1. 3. 7. nil "LE (1 -BIG)")
  (k-test10-jtest 3. 3. 2. t   "EQ (-BIG -BIG)")
  (k-test10-jtest 3. 3. 3. nil "NEQ (-BIG -BIG)")
  (k-test10-jtest 3. 3. 4. nil "LT (-BIG -BIG)")
  (k-test10-jtest 3. 3. 5. t   "GE (-BIG -BIG)")
  (k-test10-jtest 3. 3. 6. nil "GT (-BIG -BIG)")
  (k-test10-jtest 3. 3. 7. t   "LE (-BIG -BIG)"))


(defun k-test11 (&aux temp)
  "Return PC Test"
  (format t "Starting Test 11 - Return PC reg.~%")
  (k-init)
  (k-execute3 KIH-LOAD-CSP 0.)                    ; Init call stack pointer
  (dotimes (i 24.)
    (k-execute KIH-JUMP (sub1 (ash 1. i))) ; Set PC
    (k-execute KIH-CALL #xFFFFFC)          ; Save PC in RPC
    (k-execute KIH-RETURN 0.)              ; Get it back
    (setq temp (logand (k-read-spy-pc) #x00FFFFFF))
    (when (not (equal temp (ash 1. i)))
      (k-diag-error "TEST-11 RPC" nil (ash 1. i) temp))))

(defun k-test11b (&aux temp)
  "Return RPC Test"
  (format t "Starting Test 11 - Return PC RPC reg.~%")
  (k-init)
  (k-execute3 KIH-LOAD-CSP 0.)                    ; Init call stack pointer
  (dotimes (i 7.)
    (k-execute KIH-JUMP #x100) ; Set PC
    (k-execute (dpb (ldb (byte 1. 6.) (ash 1. i)) (byte 1. 29.)
                    (dpb (ldb (byte 5. 1.) (ash 1. i)) (byte 5. 0.)
                         KIH-OPEN-CALL))
               (dpb (ash 1. i) (byte 1. 24.) #xFFFFFC))          ; Save PC in RPC
    (k-execute2 kih-jump #X100)
    (k-execute KIH-alu-nop kil-read-rpc)
    (k-execute2 kih-jump #x100)
    (setq temp (logand (k-read-spy-mmfio) #xFF000000))
    (when (not (equal temp (ash 1. (+ i 24.))))
      (k-diag-error "TEST-11 RPC" nil (ash 1. (+ i 24.)) temp))))

(defun k-test12 (&aux temp)
  "Call Stack Pointer & Heap Pointer test."
  (format t "Starting Test 12 - Call Stack Pointer & Heap Pointer.~%")
  (k-init)
  (dotimes (i 16.)
    (k-execute  KIH-LOAD-CSP (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute  KIH-ALU-NOP KIL-READ-CSP)
    (k-execute2 KIH-NOP 0.)
    (setq temp (logand #x0FFFF (k-read-spy-mmfio)))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-12 CSP" nil (ash 1. i) temp))))

(defun k-test13 (&aux temp)
  "Processor Control Register Test."
  (format t "Starting Test 13 - Processor control register.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute  KIH-LOAD-PCTL (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute  KIH-ALU-NOP KIL-READ-PCTL)
    (k-execute2 KIH-NOP 0.)
    (setq temp (logand #x0FFFFFF (k-read-spy-mmfio)))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-13 PCTL" nil (ash 1. i) temp))))

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

(defun x14 (i)
    (k-execute  KIH-LOAD-MCTL (ash 1. i))
    (k-execute3 KIH-JUMP #xFFFFF0)
    (k-execute  KIH-ALU-NOP KIL-READ-MCTL)
    (k-execute2 KIH-NOP 0.)
    (k-read-spy-mmfio))

(defun k-test15 (&aux temp)
  "Return PC Simple Call Stack Test"
  (format t "Starting Test 15 - Return PC simple call stack.~%")
  (k-init)
  (k-execute3 KIH-LOAD-CSP 0.)                    ; Init call stack pointer
  (dotimes (i 24.)
    (k-execute KIH-JUMP (sub1 (ash 1. i))) ; Set PC
    (k-execute KIH-CALL #xFFFFFC)          ; Save PC in RPC
    (k-execute KIH-NOP 0.)                 ; Let call stack load
    (k-execute KIH-RETURN 0.)              ; Get it back
    (setq temp (k-read-spy-pc))
    (when (not (equal temp (ash 1. i)))
      (k-diag-error "TEST-15 CS Return PC" 0. (ash 1. i) temp))))


(defun ws (adr n)
        (k-execute3 KIH-LOAD-CSP adr)             ;CSP = 0
        (k-execute KIH-LOAD-PCTL #x20000) ;Stack Group
        (k-execute3 KIH-JUMP #x100)
        (k-execute3 KIH-NOP 0)
        (k-execute KIH-LOAD-RPC n)
        (k-execute KIH-NOP 0.)
        (k-execute KIH-CALL #x100)
        (k-execute3 KIH-NOP 0))

(defun rs (adr)
        (k-execute3 KIH-LOAD-CSP adr)             ;CSP = 0
        (k-execute KIH-LOAD-PCTL #x20000)
        (k-execute3 KIH-JUMP #x100)
        (k-execute3 KIH-NOP 0)
        (k-execute3 KIH-ALU-NOP KIL-READ-RPC)
        (k-execute KIH-XRETURN 0.)
        (k-execute KIH-JUMP #x100)
        (k-read-spy-mmfio))


(defun k-test16 (&aux temp)
  "Call Stack data - PC & RDEST"
  (format t "Starting Test 16 - Call Stack data - PC & RDEST.~%")
  (k-init)
  (dotimes (pat 32.)
    (format t "  Pass ~D~%" pat)
    (dotimes (sgn 16.)
      (k-execute3 KIH-LOAD-CSP 0)               ;CSP = 0
      (k-execute KIH-LOAD-PCTL (ash sgn 13.))   ;Stack Group
      (k-execute3 KIH-NOP 0)
      (dotimes (i 256.)
        (k-execute KIH-LOAD-RPC (ash 1. (logand 31. (+ i pat))))
        (k-execute KIH-NOP 0.)
        (k-execute KIH-OPEN-CALL #x100)))
    (dotimes (sgn 16.)
      (k-execute3 KIH-LOAD-CSP 0)               ;CSP = 0
      (k-execute KIH-LOAD-PCTL (ash sgn 13.))   ;Stack Group
      (k-execute3 KIH-NOP 0)
      (do ((i 255. (sub1 i)))
          ((minusp i))
        (k-execute KIH-ALU-NOP KIL-READ-RPC)
        (k-execute KIH-XRETURN 0.)
        (k-execute KIH-JUMP #x100)
        (setq temp (k-read-spy-mmfio))
        (when (not (equal temp (ash 1. (logand 31. (+ i pat)))))
          (k-diag-error "TEST-16 Call Stack RPC data"
                        (+ (ash sgn 8.) i)
                        (ash 1. (logand 31. (+ i pat))) temp))))))


(defun k-test17 (&aux temp)
  "Call Stack address - PC & RDEST"
  (format t "Starting Test 17 - Call Stack address - PC & RDEST.~%")
  (k-init)
  (dotimes (sgn 16.)
    (k-execute KIH-LOAD-PCTL (ash sgn 13.))     ;Stack Group
    (k-execute KIH-LOAD-CSP #xffff)                     ;CSP = 0
    (k-execute3 KIH-NOP 0)
    (dotimes (i 256.)
      (k-execute KIH-LOAD-RPC (+ (ash sgn 8.) i))
      (k-execute KIH-NOP 0.)
      (k-execute KIH-OPEN-CALL #x100)))
  (dotimes (sgn 16.)
    (k-execute KIH-LOAD-PCTL (ash sgn 13.))     ;Stack Group
    (k-execute KIH-LOAD-CSP #xffff)                     ;CSP = 0
    (k-execute3 KIH-NOP 0)
    (do ((i 255. (sub1 i)))
        ((minusp i))
      (k-execute KIH-ALU-NOP KIL-READ-RPC)
      (k-execute KIH-XRETURN 0.)
      (k-execute KIH-JUMP #x100)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp (+ (ash sgn 8.) i)))
        (k-diag-error "TEST-17 Call Stack RPC address"
                      (+ (ash sgn 8.) i)
                      (+ (ash sgn 8.) i) temp)))))

(defun k-test18 (&aux temp)
  "Call Stack data - open & active"
  (format t "Starting Test 18 - Call Stack data - open & active.~%")
  (k-init)
  (dotimes (pat 16.)
     (format t "  Pass ~D~%" pat)
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (dotimes (i 256.)
           (k-execute  KIH-LOAD-OAR (ash 1. (+ 8. (logand 15. (+ pat i)))))
           (k-execute3 KIH-NOP 0.)
           (k-execute  KIH-OPEN-CALL #x100)))
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (do ((i 255. (sub1 i)))
            ((minusp i))
           (k-execute  KIH-XRETURN 0.)
           (k-execute2  KIH-ALU-NOP KIL-READ-OAR)
           (k-execute2 KIH-JUMP #x100)
           (setq temp (logand #xFFFF (ash (k-read-spy-mmfio) -8.)))
           (when (not (equal temp (ash 1. (logand 15. (+ pat i)))))
                 (k-diag-error "TEST-18 Call Stack OA data"
                               (+ (ash sgn 8.) i)
                               (ash 1. (+ 8. (logand 15. (+ pat i)))) temp))))))


(defun k-test19 (&aux temp)
  "Heap data"
  (format t "Starting Test 19 - Heap Data~%")
  (k-init)
  (dotimes (pat 8.)
     (format t "  Pass ~D~%" pat)
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (dotimes (i 256.)
           (k-execute  KIH-LOAD-OAR (ash 1. (logand 7. (+ i pat))))
           (k-execute3 KIH-JUMP #x100)
           (k-execute  KIH-XRETURN 0.)))
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (do ((i 255. (sub1 i)))
            ((minusp i))
          (k-execute  KIH-OPEN-CALL #x100)
          (k-execute2  KIH-ALU-NOP KIL-READ-OAR)
          (k-execute2 KIH-JUMP #x100)
          (setq temp (logand #xFF (ash (k-read-spy-mmfio) -16.)))
          (when (not (equal temp (ash 1. (logand 7. (+ i pat)))))
            (k-diag-error "TEST-19 Heap data"
                          (+ (ash sgn 8.) i)
                          (ash 1. (logand 7. (+ i pat))) temp))))))

(defun k-test20 (&aux temp)
  "Call Stack data - RIMM"
  (format t "Starting Test 20 - Call Stack data - RIMM.~%")
  (k-init)
  (dotimes (pat 4.)
     (format t "  Pass ~D~%" pat)
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (dotimes (i 256.)
           (k-execute KIH-LOAD-PCTL
              (logior #x20000 (ash sgn 13.)
                      (ash (ash 1. (logand 3. (+ i pat))) 9.)))
           (k-execute KIH-LOAD-RPC 0)
           (k-execute KIH-NOP 0.)
           (k-execute KIH-OPEN-CALL #x100)))
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
        (k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
        (k-execute3 KIH-NOP 0)
        (do ((i 255. (sub1 i)))
            ((minusp i))
          (k-execute KIH-ALU-NOP KIL-READ-PSTAT)
          (k-execute KIH-XRETURN 0.)
          (k-execute KIH-JUMP #x100)
          (setq temp (logand #x0F (ash (k-read-spy-mmfio) -9.)))
          (when (not (equal temp (ash 1. (logand 3. (+ i pat)))))
            (k-diag-error "TEST-20 Call Stack RIMM Data"
                          (+ (ash sgn 8.) i)
                          (ash 1. (logand 3. (+ i pat))) temp))))))


(defun k-test21 (&aux temp)
  "VMA Test."
  (format t "Starting Test 21 -  VMA register.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute  KIH-LOAD-VMA (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute3  KIH-ALU-NOP KIL-READ-VMA)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-21 VMA" nil (ash 1. i) temp))))


(defun k-test22 (&aux temp)
  "MD Test."
  (format t "Starting Test 22 -  MD register.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute  KIH-LOAD-MD-SW (ash 1. i))
    (k-execute3 KIH-JUMP #x100)
    (k-execute3  KIH-ALU-NOP KIL-READ-MD)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-22 MD" nil (ash 1. i) temp))))

(defun k-test23 (&aux temp)
  "Simple MAP data Test.- Location zero"
  (format t "Starting Test 23 -  MAP data RAM - Location zero.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute2 KIH-LOAD-MAP (ash 1 i))
    (k-execute3 KIH-nop #x100)
    (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-23 MAP Data " 0 (ash 1. i) temp))
    )
  )

(defun wm (adr data)
  (k-execute KIH-LOAD-VMA adr)
  (k-execute3 KIH-JUMP #x100)
  (k-execute2 KIH-LOAD-MAP data)
  (k-execute3 KIH-JUMP #x100))

(defun rm (adr)
  (k-execute2 KIH-LOAD-VMA adr)
  (k-execute4 KIH-JUMP #x100)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun dump-map (adr &optional (n 8.))
  (dotimes (i n)
    (format t "~%VMA = ~X        MAP = ~X" adr (rm adr))
    (setq adr (+ #x400 adr))))

(defun k-test24 (&aux temp)
  "Map data Test."
  (format t "Starting Test 24 - MAP data RAM.")
  (k-init)
  (dotimes (pat 32.)
    (format t "~%Pass ~D       Writing." pat)
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute2 KIH-LOAD-MAP (ash 1 (logand 31. (+ adr pat))))
      (k-execute KIH-JUMP #X100))
    (format t "      Reading.")
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute3 KIH-JUMP #X100)
      (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal (ash 1 (logand 31. (+ adr pat))) temp))
        (k-diag-error "TEST-24 MAP Data " adr (ash 1 (logand 31. (+ adr pat))) temp)))))

(defun k-test24b (&aux temp)
  "Map address Test."
  (format t "Starting Test 24 - MAP address RAM.")
  (k-init)
    (format t "      Writing.~%")
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute2 KIH-LOAD-MAP adr)
      (k-execute KIH-JUMP #X100))
    (format t "      Reading.~%")
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute KIH-JUMP #X100)
      (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp adr))
        (k-diag-error "TEST-24 MAP Address" adr adr temp))))


(defun k-test25 (&aux temp)
  "TEST-25 - Subroutine call test"
  (k-init)
  (ch-init)
  (dotimes (i 16.)
    (k-execute (dpb i (byte 4. 9.) kih-load-g0) i))
  (k-execute kih-jump #x100)
  (dotimes (i 8.)
    (k-execute (dpb i (byte 3. 0.) kih-open-call) (+ #x1000110 (* i 16.))))
  (do ((i 7. (sub1 i)))
      ((minusp i))
    (k-execute kih-nop 0)
    (k-execute kih-return-fd (+ i i))
    (setq temp (k-read-spy-pc))
    (when (not (equal temp (+ #x101 (* i 16.))))
      (k-diag-error "TEST25 PC" nil (+ #x101 (* i 16.)) temp)))
  (dotimes (i 16.)
    (k-execute3 kih-alu-nop (dpb i (byte 4. 25.) kil-readr-g0))
    (setq temp (k-read-spy-mmfio))
    (when (not (equal temp (logand 14. i)))
      (k-diag-error "TEST-25 RDEST" i (logand 14. i) temp))))


(defun k-test26 ()
  "TEST-26 - Call hardware functions test"
  (k-init)
  (k-test26-qchi)
  (k-test26-ch-test "nop" #x00ff #x112233)
  (k-test26-qchi)
  (k-execute kih-open #x100)
  (k-test26-ch-test "open" #xff00 #x442233)
  (k-test26-qchi)
  (k-execute kih-call #x100)
  (k-test26-ch-test "call" #x00ff #x111133)
  (k-test26-qchi)
  (k-execute kih-open-call-x #x100)
  (k-test26-ch-test "open-call" #xff00 #x444433)
  (k-test26-qchi)
  (k-execute kih-open-call-x #x100)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return" #x00ff #x112244)
  (k-test26-qchi)
  (k-execute (logior #x20000000 kih-open-call-x) #x100)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return-open" #xff00 #x332244)
  (k-test26-qchi)
  (k-execute (logior #x20000010 kih-open-call-x) #x100)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return-topen" #xffff #x332244)
  (k-test26-qchi)
  (k-execute kih-topen #x100)
  (k-test26-ch-test "topen" #xffff #x442233)
  (k-test26-qchi)
  (k-execute kih-tcall #x100)
  (k-test26-ch-test "tcall" #x01ff #x111122)
  (k-test26-qchi)
  (k-execute kih-topen-tcall #x100)
  (k-test26-ch-test "topen-tcall" #x00ff #x333322))

(defun k-test26-ch-test (info hp-csp o-a-r &aux temp)
  (k-execute2 kih-nop 0)
  (k-execute4 kih-alu-nop kil-read-csp)
  (setq temp (logand #xffff (k-read-spy-mmfio)))
  (when (not (equal temp hp-csp))
    (k-diag-error (format nil "TEST-26 HP-CSP ~S" info) nil hp-csp temp))
  (k-execute4 kih-alu-nop kil-read-oar)
  (setq temp (logand #xffffff (k-read-spy-mmfio)))
  (when (not (equal temp o-a-r))
    (k-diag-error (format nil "TEST-26 Open-Active-Return ~S" info) nil o-a-r temp)))



(defun k-test26-qchi ()
  "TEST-26 quick call hardware init"
  (k-execute kih-load-csp #xffff)
  (k-execute kih-load-oar #x44)
  (k-execute4 kih-nop 0)
  (k-execute kih-xreturn 0)
  (k-execute3 kih-nop 0)
  (k-execute kih-load-csp #x00ff)
  (k-execute kih-load-oar #x112233)
  (k-execute3 kih-nop 0))

(defun k-test27 (&aux temp)
  "Test-27 Microsecond clock"
  (k-init)
  (do ((i 16. (1+ i)))
      ((> i 31.))
    (k-execute kih-load-usc (ash 1 i))
    (k-execute4 kih-nop 0)
    (k-execute3 kih-alu-nop kil-read-usc)
    (setq temp (logand #xffff0000 (k-read-spy-mmfio)))
    (when (not (equal temp (ash 1 i)))
      (k-diag-error "TEST-27 Microsecond clock R/W" nil (ash 1 i) temp)))
    (k-execute kih-load-usc #xffffffff)
    (k-execute4 kih-nop 0)
    (k-execute3 kih-alu-nop kil-read-usc)
    (setq temp (logand #xffff0000 (k-read-spy-mmfio)))
    (when (not (equal temp 0))
      (k-diag-error "TEST-27 Microsecond clock carry" nil 0 temp))
    (setq temp 0)
    (do ((i 0 (1+ i)))
        ((> i 1000.))
      (k-execute3 kih-alu-nop kil-read-usc)
      (setq temp (logior temp (k-read-spy-mmfio))))
      (when (not (equal #xffff (logand #xffff temp)))
        (k-diag-error "TEST-27 Microsecond clock Stuck bits" nil #xffff temp)))

(defun k-test28 (&aux temp)
  "Test-28 Statistics counter"
  (k-init)
  (k-execute kih-load-mctl #x5) ; IR stat bit, duration mode
  (k-execute4 kih-nop 0)
  (dotimes (i 32.)
    (k-execute kih-load-stat (ash 1 i))
    (k-execute4 kih-nop 0)
    (k-execute3 kih-alu-nop kil-read-stat)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal temp (ash 1 i)))
      (k-diag-error "TEST-28 Statistics Counter R/W" nil (ash 1 i) temp)))
  (k-execute kih-load-stat #xfffffffe)
  (k-execute4 kih-nop 0)
  (k-execute2 (logior #x80000000 kih-nop) 0)
  (k-execute4 kih-alu-nop kil-read-stat)
  (setq temp (logand #xffff0000 (k-read-spy-mmfio)))
  (when (not (equal 0 temp))
    (k-diag-error "TEST-28 Statistics Counter Inc" nil 0 temp)))


(defun k-test29 (&aux temp)
  "TEST-29 Trap register"
  (k-reset)
  (k-init)
  (format t "TEST-29 Trap register~%")
  (k-execute kih-jump #x100)
  (k-execute kih-load-csp #x1111)
  (k-execute4 kih-load-g0 0)
  (k-test29-tchk #x80000000 #xc000dfff)                 ;reset

  (k-execute kih-load-mctl #x6d000000)                  ;no reset
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test29-tchk #x00000000 #xc000dfff)

  (k-execute kih-load-mctl #x6d001000)                  ;interval timer 1024
  (k-execute4 kih-nop 0)
  (k-test29-tchk #x00000004 #xc000dfff)

  (k-execute kih-load-mctl #x6d002000)                  ;interval timer 16384
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test29-tchk #x00000002 #xc000dfff)

  (k-execute kih-load-mctl #x6d000000)                  ;nubus interrupts
  (k-execute4 kih-nop 0)
  (dotimes (i 8.)
    (k-write-int i 1.)
    (k-test29-tchk (ash 1. (+ i 3.)) #xc000dfff)
    (k-write-int i 0.))

  (k-execute kih-load-g0 #x80000000)                    ;alu overflow
  (k-test29-tchk #x00001000 #xc000dfff)
  (k-execute3 kih-load-g0 0)

  (k-execute kih-load-csp #x0011)                       ;stack overflow
  (k-execute kih-load-pctl #x0040000)
  (k-execute4 kih-nop 0)
  (k-test29-tchk #x00000000 #xc000dfff)
  (k-execute kih-open-call #x100)
  (k-test29-tchk #x00008000 #xc000dfff)
  (k-execute kih-load-pctl #x0000000))

(defun k-test29-tchk (expect mask &aux temp)
  (k-execute kih-alu-nop kil-add-g0-g0)
  (k-execute3 kih-alu-nop kil-read-mctl)
  (k-execute  kih-load-mctl (k-read-spy-mmfio))
  (k-execute3 kih-jump #x100)
  (k-execute3 kih-alu-nop kil-read-trap)
  (setq temp (logand mask (k-read-spy-mmfio)))
  (when (not (equal expect temp))
    (k-diag-error "TEST-29 Trap register" nil expect temp)))

(defun k-test30 ()
  "TEST-30 Trap request"
  (k-init)
  (format t "TEST-30 Trap register~%")
  (k-execute kih-load-g0 0)
  (k-execute kih-alu-nop kil-add-g0-g0)

  (k-execute kih-load-mctl #xC1001000)                  ;interval timer 1024 uS
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test30-tchk 0)

  (k-execute kih-load-mctl #xC1002000)                  ;interval timer 16384 uS
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test30-tchk 0)

  (dotimes (i 8.)
    (k-execute kih-load-mctl #xC1000000)        ;nubus interrupts
    (k-execute4 kih-nop 0)
    (k-write-int i 1.)
    (k-test30-tchk 0)
    (k-write-int i 0.))

  (k-execute kih-load-mctl #xE1000000)
  (k-execute3 kih-load-g0 #x80000000)                   ;alu overflow
  (k-execute2 kih-alu-nop kil-add-g0-g0)
  ;; This is gross but seems to work. The trap bit is toggled like
  ;; crasy. This will check if all the pc bits get zeroed some time.
  (do ((i 0 (1+ i))
       (j 0 (logior j (logxor (k-read-spy-pc) #xFFFFFF))))
      ((= i 100)
       (when (not (=  #xFFFFFF j))
         (k-diag-error "Test-30 Overflow Request" nil 0 (logxor #xFFFFFF j))
      )))
  )

(defun k-test30-tchk (expect &aux temp)
  (setq temp (k-read-spy-pc))
  ;; clear trap stuff to allow the clock to tick.
  (k-execute kih-jump #x100)
  (k-spy-cmd 3)                                 ; force the icache to reload the inst again.
  (k-spy-cmd 6)                                 ; Turn off spy enable.
  (k-spy-cmd 2)                                 ; Tick clock
  (k-spy-cmd 7)                                 ; Turn on spy enable.
  (when (not (= expect temp))
    (k-diag-error "TEST-30 Trap request" nil expect temp))
  (k-execute4 kih-jump #x100)
  (when (not (= (setq temp (k-read-spy-pc)) #x100))
    (k-diag-error "TEST-30 Trap request" nil #x100 temp))
  )


(defun k-test31 ()
  "Test 31 - Trap State machine"
  (format t "Test 31 - Trap State machine~%")
  (k-init)
  ;; initialize two global regs
  (k-execute kih-load-g0 1)
  (k-execute (dpb 1 (byte 4 9.) kih-load-g0) 2)
  (k-execute (dpb 2 (byte 4 9.) kih-load-g0) -1)
  (k-execute3 kih-nop 0)
  ;; branch to zero, left source G0, right source G1
  (k-execute #X20008000 #X63800000)
  (k-test31-step 0 1 (dpb 2 (byte 4 25.) kil-readl-g0))         ; Freeze clocks
  (k-test31-step 0 2 (dpb 2 (byte 4 25.) kil-readl-g0))         ; write OREG
  (k-test31-step 0 3 (dpb 2 (byte 4 25.) kil-readr-g0))         ; Write Left
  (k-test31-step 1 4 (dpb 2 (byte 4 25.) kil-readr-g0))         ; Write Right
  (k-test31-step 2 4 (dpb 2 (byte 4 25.) kil-readr-g0))
  (k-test31-step #xFFFFFFFF 5 (dpb 2 (byte 4 25.) kil-readr-g0)); New thing through
  )

(defun k-test31-step (expect adr inst-low-half &aux temp)
  (k-execute kih-alu-nop inst-low-half)
  (setq temp (k-read-spy-mmfio))
  (when (not (= temp expect))
    (k-diag-error "Test-31 TSM" adr expect temp)))

(defun k-test32 (&aux temp)
  "TEST-32 Old PC registers"
  (k-init)
  (format t "TEST-32 Old PC registers~%")
  (k-execute kih-jump #x100)
  (dotimes (i 5)
)
    (format t "~X~%" (k-read-spy-mmfio)))

(defun k-test33 (&aux temp)
  "TEST-33 Simple Box bits"
  (k-init)
  (format t "TEST-33 Simple Box Bits~%")

  (k-execute kih-load-oar #xfffefd)
  (k-execute kih-load-pctl #x40)
  (k-execute3 kih-nop 0)
  (dotimes (i 2)
    (k-execute2 kih-load-a0 i)
    (k-execute4 kih-alu-a0 kil-read-pstat)
    (when (neq i (ldb (byte 1. 18.) (k-read-spy-mmfio)))
      (k-diag-error "TEST-33 OREG boxed broken" nil i (- 1 i))))

  (dotimes (i 2)
    (k-execute kih-load-g0 0)
    (k-execute kih-load-a0 i)
    (k-execute3 (logior #x10000000 kih-alu-a0) #x60039000) ;shift left w/link fill from G0
    (setq temp (k-read-spy-mmfio))
    (when (not (equal temp i))
      (k-diag-error "TEST-33 Macro-carry box load" nil i temp)))

  (k-execute kih-load-pctl 0)
  (k-execute3 kih-nop 0)
  (k-execute (logior #x00400000 kih-load-a0) 0) ;unboxed
  (k-execute3 kih-alu-nop kil-read-pstat)
  (when (neq 0 (ldb (byte 1. 18.) (k-read-spy-mmfio)))
    (k-diag-error "TEST-33 Unboxed broken" nil 0 1))

  (k-execute (logior #x00c00000 kih-load-a0) 0) ;boxed
  (k-execute3 kih-alu-nop kil-read-pstat)
  (when (neq 1 (ldb (byte 1. 18.) (k-read-spy-mmfio)))
    (k-diag-error "TEST-33 boxed broken" nil 1 0))

  (dotimes (i 16.)
    (dotimes (j 16.)
      (k-execute (dpb j (byte 4. 9.) KIH-LOAD-G0) 0))
    (k-execute (logior #x00c00000 (dpb i (byte 4. 9.) KIH-LOAD-G0)) 0)
    (dotimes (j 16.)
      (k-execute (logior #x00400000 kih-alu-a0) (dpb j (byte 4. 25.) kil-readr-g0))
      (k-execute3 kih-alu-a0 kil-read-pstat)
      (setq temp (ldb (byte 1. 18.) (k-read-spy-mmfio)))
      (when (neq (eq j i) (equal temp 1))
        (k-diag-error "TEST-33 Simple Box bits right" j (if (eq i j) 1 0) temp))
      (k-execute kih-alu-a0 (dpb j (byte 4. 19.) kil-readl-g0))
      (k-execute3 kih-alu-a0 kil-read-pstat)
      (setq temp (ldb (byte 1. 18.) (k-read-spy-mmfio)))
      (when (neq (eq j i) (equal temp 1))
        (k-diag-error "TEST-33 Simple Box bits left" j (if (eq i j) 1 0) temp)))))

(defun k-test34 (&aux temp)
  "TEST-34 Simple Datatype RAM"
  (k-init)
  (format t "TEST-34 Simple Datatype RAM~%")
  (dotimes (chip 2)
    (dotimes (i 64.)
      (k-execute kih-load-pctl 0)
      (k-execute3 kih-jump #x100)
      (dotimes (j 64.)
        (k-execute (dpb chip (byte 1. 21.) kih-load-dt) (ash j 26.))
        (k-execute (dpb chip (byte 1. 21.) kih-load-g0) (ash j 26.)))
      (k-execute kih-load-pctl #x100)
      (k-execute3 kih-jump #x100)
      (k-execute (dpb chip (byte 1. 21.) kih-load-dt) (ash i 26.))
      (k-execute (dpb chip (byte 1. 21.) kih-load-g0) (ash i 26.))
      (dotimes (j 64.)
        (k-execute (dpb chip (byte 1. 21.) kih-nop) (ash j 26.))
        (k-execute3 kih-alu-nop kil-read-pstat)
        (setq temp (ldb (byte 1. 13.) (k-read-spy-mmfio)))
        (when (neq (eq i j) (equal temp 1.))
          (k-diag-error "TEST-34 Simple datatype RAM" j (if (eq i j) 1 0) temp))))))


(defun k-test50 (&aux temp)
  "TEST-50 Proc - local memory data test loc 0"
  (k-init)
  (format t "TEST-50 Proc - local memory data test loc 0")
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

(defun k-test51 (&aux temp)
  "TEST-51 Proc - local mem data 0-255"
  (k-init)
  (format t "TEST-51 Proc - local memory data 0-255~%")
  (dotimes (pass 32.)
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

(defun k-test52 (&aux temp)
  "TEST-52 Proc - local mem address 0-255"
  (k-init)
  (format t "TEST-52 Proc - local memory address 0-255~%")
  (dotimes (pass 32.)
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
  (format t "Test-53 Proc - local mem special reads~%")
  (k-execute kih-load-md #x55555555)
  (k-execute kih-load-vma-sw 0)
  (k-execute kih-jump #x100)
  (k-execute kih-load-md #xAAAAAAAA)
  (k-execute kih-load-vma-sw 1)
  (k-execute kih-jump #x100)
  (k-execute kih-load-md 0)
  (k-execute kih-load-vma-sw 2)
  (k-execute kih-jump #x100)
  (dotimes (i 16.)
    (k-execute kih-load-vma-sr 2)
    (k-execute3 kih-jump #x100)
    (setq pat (if (zerop (logand 4. i)) #x55555555 #xAAAAAAAA))
    (k-execute (logior (ash i 9.) kih-load-vma-sr) 0)
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

(defun k-test54 (&aux temp)
  "Test 54 - MD/VMA box bits"
  (format t "Test 54 - MD//VMA box bits~%")
  (k-init)
  (k-execute kih-load-vma-sw 0)
  (k-execute kih-nop 0)
  (dotimes (i 4)
     (k-execute (dpb i (byte 2 22.) kih-load-vma-sr) 0)
     (k-execute3 kih-nop 0)
     (k-execute kih-alu-nop kil-read-md)
     (k-execute3 kih-alu-nop kil-read-mstat)
     (setq temp (logxor 1. (ldb (byte 1. 15.) (k-read-spy-mmfio))))
     (when (not (equal temp (ldb (byte 1. 1.) i)))
         (k-diag-error "TEST-54 MD box bit" nil (ldb (byte 1. 1.) i) temp))
     (setq temp (logxor 1. (ldb (byte 1. 16.) (k-read-spy-mmfio))))
     (when (not (equal temp (ldb (byte 1. 0.) i)))
         (k-diag-error "TEST-54 VMA box bit" nil (ldb (byte 1. 0.) i) temp))))

(defun k-test55 (&aux temp)
  "Test-55 GC RAM test"
  (format t "Test-55 - GC RAM~%")
  (k-init)
  (dotimes (i 16.)
     (k-execute kih-load-gc-ram i)
     (k-execute4 kih-jump #x100)
     (k-execute3 kih-alu-nop kil-read-gc-rams)
     (setq temp (ldb (byte 4. 0.) (k-read-spy-mmfio)))
     (when (not (equal temp i))
           (k-diag-error "TEST-55 GC Data" 0 i temp)))
  (dotimes (i 12.)
     (k-execute kih-load-md (ash #x4000 i))
     (k-execute kih-jump #x100)
     (k-execute kih-load-gc-ram i)
     (k-execute kih-jump #x100))
  (dotimes (i 12.)
     (k-execute kih-load-md (ash #x4000 i))
     (k-execute3 kih-jump #x100)
     (k-execute3 kih-alu-nop kil-read-gc-rams)
     (setq temp (ldb (byte 4. 0.) (k-read-spy-mmfio)))
     (when (not (equal i temp))
           (k-diag-error "TEST-55 GC Address" (ash #x4000 i) i temp))))

(defun k-test56 (&aux temp)
   "Test-56 Transporter RAM"
   (format t "Test-56 Transporter RAM~%")
   (k-init)
   (dotimes (i 16.)
     (k-execute kih-load-transporter (ash i 4.))
     (k-execute4 kih-jump #x100)
     (k-execute3 kih-alu-nop kil-read-gc-rams)
     (setq temp (ldb (byte 4. 4.) (k-read-spy-mmfio)))
     (when (not (equal temp i))
           (k-diag-error "TEST-56 Transporter Data" 0 i temp)))
   (dotimes (i 6.)
      (k-test56-chk kih-load-md (ash #x04000000 i) (ash 1 i)))
   (dotimes (i 2.)
      (k-test56-chk kih-load-mctl (ash #x10000 i) (ash #x40 i)))
   (k-test56-chk (logior #x00800000 kih-load-md) 0 #x400)
   (k-test56-chk (logior #x00400000 kih-load-vma) 0 #x800))

(defun k-test56-chk (kih kil addr &aux temp)
   "Checks if the current location equals #x0f and that addressing works"
   (k-execute kih-load-mctl 0)
   (k-execute kih-load-md 0)
   (k-execute kih-load-vma 0)
   (k-execute3 kih-jump #x100)
   (k-execute kih-load-transporter 0)
   (k-execute3 kih-jump #x100)
   (k-execute kih kil)
   (k-execute4 kih-jump #x100)
   (k-execute kih-load-transporter (ash #x0F 4.))
   (k-execute3 kih-jump #x100)
   (k-execute3 kih-alu-nop kil-read-gc-rams)
   (setq temp (ldb (byte 4. 4.) (k-read-spy-mmfio)))
   (when (not (equal temp #x0f))
      (k-diag-error "Test-56 Transporter RAM address" addr #x0f temp))
   (k-execute kih-load-transporter 0)
   (k-execute kih-load-mctl 0)
   (k-execute kih-load-md 0)
   (k-execute kih-load-vma 0)
   (k-execute3 kih-jump #x100)
   (k-execute3 kih-alu-nop kil-read-gc-rams)
   (setq temp (ldb (byte 4. 4.) (k-read-spy-mmfio)))
   (when (not (equal temp 0))
      (k-diag-error "Test-56 Transporter RAM address" addr 0 temp)))

(defun k-test56b (&aux temp data)
  "Transporter ram address test"
  (k-init)
  (format t "Test-56b - Transporter address test~%")
  (dotimes (md-boxed 2.)
    (dotimes (vma-boxed 2.)
      (dotimes (trans-type 4.)
        (dotimes (trans-mode 4.)
          (dotimes (datatype 64.)
            (setq data (logand #x0f (+ md-boxed vma-boxed trans-type trans-mode datatype)))
            (k-write-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype data))))))
  (dotimes (md-boxed 2.)
    (dotimes (vma-boxed 2.)
      (dotimes (trans-type 4.)
        (dotimes (trans-mode 4.)
          (dotimes (datatype 64.)
            (setq data (logand #x0f (+ md-boxed vma-boxed trans-type trans-mode datatype)))
            (setq temp (k-read-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype))
            (when (not (equal temp data))
              (k-diag-error "Test56b - Transporter address"
                            (logior
                              (ash vma-boxed 11.)
                              (ash md-boxed 10.)
                              (ash trans-type 8.)
                              (ash trans-mode 6.)
                              datatype)))))))))

(defun k-test57 (&aux temp)
  "Test-57 Floating point data path tests"
  (k-init)
  (format t "Test-57 Floating Point Data Paths~%")
  (k-execute kih-fmul-g0 kil-fmode0)
  (k-execute kih-fmul-g0 kil-fmode1)
  (k-execute kih-fmul-g0 kil-fmode2)
  (k-execute kih-fmul-g0 kil-fmode3)
  (k-execute kih-falu-g0 kil-fmode0)
  (k-execute kih-falu-g0 kil-fmode1)
  (k-execute kih-falu-g0 kil-fmode2)
  (k-execute kih-falu-g0 kil-fmode3)
  (dotimes (i 23.)
    (k-execute3 kih-jump #x100)
    (k-execute kih-load-g1 (logior #x3f800000 (ash 1. i)))
    (k-execute kih-load-g2 #x3f800000) ; 1.00000
    (k-execute kih-load-g0 0)
    (k-execute3 kih-fmul-g0 kil-fmul-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (logior #x3f800000 (ash 1. i))))
      (k-diag-error "TEST-57 FMUL (2264) right" nil (logior #x3f800000 (ash 1. i) temp))))
  (dotimes (i 23.)
    (k-execute3 kih-jump #x100)
    (k-execute kih-load-g2 (logior #x3f800000 (ash 1. i)))
    (k-execute kih-load-g1 #x3f800000) ; 1.00000
    (k-execute kih-load-g0 0)
    (k-execute3 kih-fmul-g0 kil-fmul-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (logior #x3f800000 (ash 1. i))))
      (k-diag-error "TEST-57 FMUL (2264) left" nil (logior #x3f800000 (ash 1. i) temp))))
  (dotimes (i 32.)
    (k-execute3 kih-jump #x100)
    (k-execute kih-load-g1 (ash 1. i))
    (k-execute kih-load-g2 0)
    (k-execute kih-load-g0 0)
    (k-execute3 kih-falu-g0 kil-fiadd-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (ash 1. i)))
      (k-diag-error "TEST-57 FALU (2265) IADD right" nil (ash 1. i) temp)))
  (dotimes (i 32.)
    (k-execute3 kih-jump #x100)
    (k-execute kih-load-g2 (ash 1. i))
    (k-execute kih-load-g1 0)
    (k-execute kih-load-g0 0)
    (k-execute3 kih-falu-g0 kil-fiadd-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (ash 1. i)))
      (k-diag-error "TEST-57 FALU (2265) IADD left" nil (ash 1. i) temp))))

(defun x ()
  (do-forever (k-execute kih-falu-g0 kil-fiadd-g1-g2)))



(defun k-test98 (&aux temp)
  "Test 98 - History RAM"
  (format t "Test 98 - History RAM~%")
  (k-reset)
  (k-init)
  (dotimes (pass 24.)
    (k-spy-cmd 5.)                              ;set opc clock bit
    (dotimes (i 4096.)
      (k-execute KIH-JUMP (ash 1. (mod (+ pass i) 24.))))
    (k-spy-cmd 4.)
    (k-execute kih-jump #xffffff)
    (dotimes (i 4096.)
      (setq temp (logand #xffffff (k-read-hram)))
      (when (not (equal temp (ash 1. (mod (+ pass i) 24.))))
        (k-diag-error "TEST-98 - History RAM" i (ash 1. (mod (+ pass i) 24.)) temp))
      (k-spy-cmd 5.)
      (k-spy-cmd 4.))))

 (defvar hbuf (make-array 4096. :type :art-32b))

(defun dump-history (&optional (n 10.))
  (k-stop)
  (setq n (logand 4095. n))
  (do ((i 4094. (1- i)))
      ((minusp i))
    (k-spy-cmd 5.)
    (k-spy-cmd 4.)
    (aset (k-read-hram) hbuf i))
  (format t "~%")
  (do ((i (- n 1) (1- i)))
      ((minusp i))
    (format t "HBUF(~A)   PC = ~X~%" (- i) (aref hbuf i))
    (k-spy-cmd 5.)
    (k-spy-cmd 4.)))


(defun k-test99 (slot &aux temp)
  "Simple NUBUS test"
  (k-init)
  (k-execute kih-load-map (logior #xF000000F (ash slot 24.)))
  (k-execute3 kih-nop 0)
  (k-write-mode 6.)
  (dotimes (i 32.)
    (k-execute kih-load-vma 0)
    (k-execute kih-load-md-sw (ash 1 i))
    (k-execute4 kih-nop 0)
    (k-execute kih-load-vma-sr 0)
    (k-execute kih-nop 0)
    (k-execute4 kih-alu-nop kil-read-md)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal temp (ash 1 i)))
      (k-diag-error "Simple NUBUS test" 0 (ash 1 i) temp))))










(defun k-test41 ()
  "Simple memory test - location 0 data patterns."
  (format t "Starting Test 21 - Memory loc 0 data patterns.~%")
  (let*
    (temp
     (patterns '(0 #xffffffff #x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x100 #x200
                   #x400 #x800 #x1000 #x2000 #x4000 #x8000 #x10000 #x20000 #x40000
                   #x80000 #x100000 #x200000 #x400000 #x800000 #x1000000 #x2000000
                   #x4000000 #x8000000 #x10000000 #x20000000 #x40000000 #x80000000)))
    (dolist (pat patterns)
      (k-mem-write 0 pat)
      (when (not (equal pat (setq temp (k-mem-read 0))))
        (k-diag-error "TEST-21 Memory loc 0 data patterns" 0 pat temp)))))

(defun k-test42 ()
  "Memory Sizer"
  (format t "Starting test 22 - Memory sizer.~%")
  (k-init)
  (let*
    ((addr 0)
     (max (progn
            (k-execute3 kih-alu-nop kil-read-mstat)
            (if (equal 1. (ldb (byte 1. 23.) (k-read-spy-mmfio))) 64. 128.)))
     (temp nil)
     (gap t))
    (setq k-mem-list nil)
    (dotimes (i max)
      (setq addr (ash i 20.))
      (if (k-test-22-mworks-? addr)
          (progn
            (if gap
                (setq temp addr))
            (setq gap nil))
        (progn
          (if (not gap)
              (setq k-mem-list (nconc k-mem-list (cons (list temp (sub1 addr)) nil))))
          (setq gap t))))
    (when (not gap)
      (setq addr (ash 128. 20.))
      (setq k-mem-list (nconc k-mem-list (cons (list temp (sub1 addr)) nil))))
    k-mem-list))


(defun k-test-22-mworks-? (addr)
  (k-mem-write addr #x12345678)
  (if (equal #x12345678 (k-mem-read addr))
      (progn
        (k-mem-write addr #xedcba987)
        (if (equal #xedcba987 (k-mem-read addr)) t nil))
    nil))

(defun k-test43 ()
  "Memory address test"
  (let*
    ((temp nil))
    (if (null k-mem-list)
        (format t "*** Can't run test 23 - Memory not sized~%")
      (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr 4.))
           (max  (second mrange)))
          ((> addr max))

          (k-mem-write addr addr)))
      (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr 4.))
           (max  (second mrange)))
          ((> addr max))

          (when (not (equal addr (setq temp (k-mem-read addr))))
            (k-diag-error "TEST-23 - Memory address" addr addr temp))))

      (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr 4.))
           (max  (second mrange)))
          ((> addr max))

          (k-mem-write addr (logxor #xffffffff addr))))

      (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr 4.))
           (max  (second mrange)))
          ((> addr max))

          (when (not (equal (logxor #xffffffff addr) (setq temp (k-mem-read addr))))
            (k-diag-error "TEST-23 - Memory address"
                          addr (logxor #xffffffff addr) temp)))))))

(defun k-test44 ()
  "Simple memory test - One word in each bank"
  (format t "Starting Test 24 - First word of each bank~%")
  (let*
    (temp
     (patterns '(0 #xffffffff #x1 #x2 #x4 #x8 #x10 #x20 #x40 #x80 #x100 #x200
                   #x400 #x800 #x1000 #x2000 #x4000 #x8000 #x10000 #x20000 #x40000
                   #x80000 #x100000 #x200000 #x400000 #x800000 #x1000000 #x2000000
                   #x4000000 #x8000000 #x10000000 #x20000000 #x40000000 #x80000000)))
    (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr #x100000))
           (max  (second mrange)))
          ((> addr max))
          (dolist (pat patterns)
            (k-mem-write 0 pat)
            (when (not (equal pat (setq temp (k-mem-read 0))))
                  (k-diag-error "TEST-24 Memory Bank data patterns" addr pat temp)))))
    (dolist (mrange k-mem-list)
      (do*
        ((addr (first mrange) (+ addr #x10000))
         (max (second mrange)))
        ((> addr max))
        (k-mem-write addr addr)))
    (dolist (mrange k-mem-list)
      (do*
        ((addr (first mrange) (+ addr #x10000))
         (max (second mrange)))
        ((> addr max))
        (setq temp (k-mem-read addr))
        (when (not (equal temp addr))
          (k-diag-error "TEST-44 Memory Bank Addressing" addr addr temp))))))

(defun k-test45 ()
  "Short branch test."
  (k-execute kih-jump #X555555)
  (dotimes (i 12.)
    (k-execute kih-branch (ash 1 i))
    (let ((output (k-read-spy-pc))
          (expect (dpb (ash 1. i) (byte 12. 0.) #x555555)))
      (when (not (= output expect))
        (k-diag-error "TEST-45 Short branch test." i expect output)))))


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
    (debug-write-word (logior #xfc000000 (* i 8)) 0)
    (debug-write-word (logior #xfc000000 (* i 8)) kih-jump)))




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
(defvar vcmem-slot #xA)

(defvar screen-virtual-address #x03000000)

(defun k-init-virtual-memory ()
  ;; map code space
  (k-map-local-ram #x02000000  #x8000 map-bits-r 0)
  ;; map data space
  (k-map-local-ram #x00000000  #x8000 map-bits-rw 0)
  ;; map vcmem board
;  (k-map-local-ram screen-virtual-address (* 32. 1024.) map-bits-rw #x20000)
  )

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

(defun read-active (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-A0))
  (k-read-spy-mmfio))

(defun read-open (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-read-spy-mmfio))

(defun read-return (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-R0))
  (k-read-spy-mmfio))

(defun k-read-oar ()
  (k-execute3 KIH-NOP 0)
  (k-execute4 KIH-ALU-NOP KIL-READ-OAR)
  (k-read-spy-mmfio))




(defun read-q ()
  (k-execute3 KIH-ALU-NOP KIL-READ-Q)
  (k-read-spy-mmfio))


(defun foo ()
  (k-setup)
  (dotimes (i 8.)
    (k-mem-write (logior #x1000 (* i 4)) i))
  (k-init-virtual-memory)
  (k-execute kih-load-mctl #x01040000)
  (k-execute4 kih-jump #x200)
  (k-spy-run))

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

(defun frob ()
  (k-setup)
  (k-init-virtual-memory)
  (fill-m)
;  (k-mem-write #x804 #x101)
;  (k-mem-write #x800 #x28000100)
  (k-mem-write #x800 0)
  (k-start #x100)
  (k-spy-cmd 6) ;spy-mode-off
  (format t "~&Frobbing ----~%")
  (do-forever
    (k-spy-cmd 3))) ;reload icache

(defun k-read-memory-map (entry)
  (k-execute2 KIH-LOAD-VMA (dpb entry hw:%%mapped-vma-byte 0))
  (k-execute4 KIH-JUMP #x100)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun k-write-memory-map (entry value)
  (k-execute2 kih-load-vma (dpb entry hw:%%mapped-vma-byte 0))
  (k-execute4 kih-jump #x100)
  (k-execute4 kih-load-map value)
  (k-execute4 kih-jump #x100)
  (k-execute4 kih-jump #x100)
;  (k-execute kih-load-vma 0)
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
  (let ((memcontrol    (dpb trans-mode (byte 2. 16.) (k-read-spy-mmfio))))
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
  (ldb (byte 4. 4.) (k-read-spy-mmfio)))

(defun k-write-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype data)
  (k-address-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute2 kih-load-transporter (dpb data (byte 4. 4.) 0))
  (k-execute4 kih-jump #X100))
