;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-

;;CAUTION!!!! do not use HW:%%xxx type byte pointers in this file!!!  The ones referenced from
;  here are in the "main" hierarchy and are set up for K-type LDB!!!  --rg
;  (instead, use K-HW: or an absolute (byte <ss> <pp>) and put the symbolic name in a comment!)

(defvar *cache-data-selector* #x100)    ;when randomly clocking machine with jumps,  which were historically
        ;typically #x100, use this variable.  It was once believed that bit 11 of this variable selected
        ;which cache "sector" was addressed.  This misconception has been resolved.  The variable
        ;was introduced to control which cache sector data was used in K-Tests, but is no longer needed.

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
          k-read-spy-program-halt
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


(defvar *local-k-slot* nil "If non-NIL, slot K is in on LOCAL NUBUS")
(declare (special *local-debugging*))   ;looked at by DEBUG-READ-WORD, etc, in DEBUG-BOARD file.
(defvar *k-memory-board-revision*  nil) ;based on config prom.  0 original prototypes, 1 first rev.

;;; The top 32 bits of some instructions.
(defconstant KIH-NOP          #x0F009E00 "loadi-32 to no-op")
(defconstant KIH-ALU-NOP      #x03009E00 "generic ALU instruction w/NOP dest")
;(defconstant KIH-ALU-NOPBR   #x03409E00 "generic ALU instruction w/NOP dest, boxed right")  ;**screw NOP is a functional
                                        ;destination so boxed bit encoding changes!
(defconstant KIH-ALU-G1FBR    #x03407E20 "generic ALU instruction w/global 1F dest, boxed right")
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
(defconstant KIH-ALU-LOAD-DT  #x03009000 "datatype ram write pulse in a ALU instruction")
(defconstant kih-load-gc-ram  #x0F00C200 "gc ram write")
(defconstant kih-load-transporter
                              #x0F00CA00 "transporter ram write")
(defconstant KIH-FMUL-G0      #x1F006000 "Floating point multiplier op")
(defconstant KIH-FALU-G0      #x1B006000 "Floating point alu op")
(defconstant kih-branch       #x20009E00 "Short branch instruction.")   ;Caution! garbage in destination.


;;; The bottom 32 bits of some instructions.
(defconstant KIL-READ-Q       #x80005000 "read q reg")
(defconstant KIL-READL-G0     #x01800000 "read G0 as left source")      ;was #x81800000 R.S. doesnt matter, but make it 0.
(defconstant KIL-READR-G0     #x60001000 "read G0 as right source")
(defconstant KIL-READ-A0      #x20001000 "read A0 as right source")
(defconstant KIL-READ-O0      #x00001000 "read O0 as right source")
(defconstant KIL-READL-O0     #x00000000 "read O0 as left source")
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
(defconstant KIL-ADD-G1-G0    #X618C2000 "Add G1 + G0")
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
; one usused.
(defconstant %%program-counter-trap1 (byte 1 25.))
(defconstant %%program-counter-trap-enable (byte 1 26.))
(defconstant %%program-counter-icache-hit (byte 1 27.))
(defconstant %%program-counter-ec-proc4 (byte 1 28.))

(defconstant %%program-counter-halt (byte  1. 29.))

(defconstant $$halt-bit-running 0)
(defconstant $$halt-bit-halted  1)

(defsubst k-write-mode (n)
  (debug-write-word k-mode-addr n))

(defsubst k-read-mode ()
  (debug-read-word k-mode-addr))

(defsubst k-spy-cmd (n)                 ;write spy pal, using commands below.
  (debug-write-word k-spyc-addr n))

(defconstant $$spy-command-stop                0.)
(defconstant $$spy-command-run                 1.)
(defconstant $$spy-command-step                2.)
(defconstant $$spy-command-reload-instruction  3.)      ;called ICLOAD in spy-pal
(defconstant $$spy-command-clear-opc-clock     4.)
(defconstant $$spy-command-set-opc-clock       5.)
(defconstant $$spy-command-clear-spy-mode      6.)
(defconstant $$spy-command-set-spy-mode        7.)
;deleted $$spy-command-stepmode-full-clock and $$spy-command-set-stepmode

;contents of mode register:
;bit 0  write 1 to reset
;bit 1  led - write 0 to illuminate
;bit 2  write 1 to enable NuBus master interface
;bits 3,4,5  reset-timer. These are readable by processor, and extend a reset
;initiated by writing a 1 to bit 0 of the mode register by 8 NuBus clocks.
;They were formerly a Boot-Mode register.

(defsubst k-spy-i0 (n)
  (debug-write-word k-spy0-addr n))

;?? should this work??  Philosophical considerations aside, it doesn't.
(defun k-spy-i0-and-check (n &aux tem)
  (debug-write-word k-spy0-addr n)
  (if (not (= (setq tem (debug-read-word k-spy0-addr)) n))
      (fsignal "Spy IR low failed, is ~X, should be ~X" tem n)))

(defsubst k-spy-i1 (n)
  (debug-write-word k-spy1-addr n))

(defsubst k-read-spy-program-halt ()    ;note this only senses the PCLT-HALT bit.  It might be halted even
                                        ; tho this function doesnt know about it if SPY-COMMAND-RUN, etc, is absent.
  (let ((response (debug-read-word k-pc-addr)))
    (if (numberp response)
        (ldb %%program-counter-halt response)
      (global:fsignal "debug port timeout"))))          ;was $$halt-bit-halted

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

(defsubst k-mem-read (byte-address)
  (debug-read-word (logior k-mem-addr byte-address)))

(defun k-mem-read-word-address (word-address)
  (k-mem-read (ash word-address 2)))

(defsubst k-mem-write (byte-address data)
  (debug-write-word (logior k-mem-addr byte-address) data))

(defun k-mem-write-word-address (word-address data)
  (k-mem-write (ash word-address 2) data))

(defsubst k-mem-write-byte (addr data)
  (debug-write-byte (logior k-mem-addr addr) data))

(defun dpb32 (value ppss word)
  (let* ((mask1 (ash #xffffffff (byte-size ppss)))
         (mask2 (logand #xffffffff (lognot (ash mask1 (byte-position ppss)))))
         (mask3 (logxor #xffffffff mask2)))
    (logior (logand mask3 word)
            (logand mask2 (ash value (byte-position ppss))))))


(defun check-k-prom (slot error-if-random)
  "Routine to check if a slot contains a K processor"
  (cond ((slot-prom-string-compare slot "LMI FALCON PROCESSOR V0.0")
         (setq *k-memory-board-revision* 1))
        ((slot-prom-string-compare slot "LMI FALCON PROCESSOR")
         (setq *k-memory-board-revision* 0))
        (error-if-random
         (setq *k-memory-board-revision* nil)
         (FSIGNAL "Unable to decode processor config prom"))
        (t nil)))

(defun slot-prom-string-compare (slot string)
  (if (fixp (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff7fc)))
      (dotimes (i (string-length string) t)
        (cond ((not (= (logand #xff (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff800 (* i 4))))
                       (aref string i)))
           (return nil))))))

(defun k-local-setup (local-slot)
  (setq *local-k-slot* (dpb local-slot (byte 4 0) #xf0))
  (k-setup))

(defun k-setup ()
  "Sets up and initializes a debug link to a K processor"
  (cond (*local-k-slot*
         (setq k-slot *local-k-slot*)
         (check-k-prom k-slot nil)      ;Set up variable *k-memory-board-revision*
         (setq *local-debugging* t))    ;looked at by fcns in DEBUG-BOARD file
        (t
         (setq *local-debugging* nil)
         (init-debug-board)
         (setq k-slot nil)
         (let ((possible-slots nil))
           (dotimes (i 16.)
             (when (check-k-prom i nil)
               (push (logior #xf0 i) possible-slots)
               (format t "~%   K processor found in slot ~A.~%" i)))
           (cond ((null possible-slots)
                  (format t "~%*** Can't setup, I couldn't find a processor! ***~%")
                  (cerror "Choose a slot number" "Can't setup, no processor!")
                  (setq k-slot (logior #xf0 (logand #x0f (progn (format t "~&Slot number?") (read))))))
                 ((= (length possible-slots) 1) (setq k-slot (first possible-slots)))
                 (t (setq k-slot
                          (fquery
                            (list ':choices
                                  `(,@(mapcar #'(lambda (possible-slot)
                                                  (list (list possible-slot
                                                              (format nil "Slot ~x" (logand #xf possible-slot)))
                                                        (digit-char (logand #xf possible-slot) 16.)))
                                              (reverse possible-slots)))
                                  :fresh-line 't)
                            "Take which K?")))))))

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
    (k-reset)
    (k-init))


(Defun k-reset ()
   "Issue a reset to the K processor and initialize map page 0, the PC, and NuBus master interface"
   (prog nil
     l  (k-write-mode 1.)
        (setq k-run-flag nil)
        (k-init)
        (cond ((not (= *cache-data-selector* (k-read-spy-pc)))
               (format t "J#x~x failed "*cache-data-selector*)  ;seem to have lost, try again.
               (go l)))))

(defvar k-run-flag nil)  ;this should be used only locally within NEW-SPY-UTILITIES.  In particular,
        ;dont use this to break things if the Falcon gets otherwise started.

(defun k-stop ()
  "Stop the processor"
  (k-spy-cmd $$spy-command-stop)
  (k-spy-cmd $$spy-command-clear-opc-clock)     ; stop gathering PC history
  (k-spy-cmd $$spy-command-clear-spy-mode)      ; flush spy mode
  (setq k-run-flag nil)
  (k-read-spy-pc))

(defsubst k-stop-if-running ()          ;use this only in this file, only within the
  (when k-run-flag (k-stop)))           ; guts of the step routines, etc.

(defun k-spy-run ()                     ; see k-execute-run
  "Start the processor running while fetching from the spy reg"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
;  (k-spy-cmd $$spy-command-set-opc-clock)    why?
  (k-spy-cmd $$spy-command-set-spy-mode)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-spy-cmd $$spy-command-run)
  (setq k-run-flag t)
  (k-read-spy-pc))

(defun k-run ()
  "Start the processor running from memory"
  (k-setup-pctl)                                ;flush caches, NOP the pipe
  (k-spy-cmd $$spy-command-set-opc-clock)       ;enable PC history collection
  (k-spy-cmd $$spy-command-reload-instruction)  ;forget that you may already have filled the cache line
  (k-spy-cmd $$spy-command-run)                 ;and go
  (setq k-run-flag t)
  (k-read-spy-pc))

(defun k-step ()                                ;note that executing spy instructions damages the caches.
  "Step the processor one clock cycle (executing from memory)"
  (k-spy-cmd $$spy-command-stop)
  (k-spy-cmd $$spy-command-clear-spy-mode)
  (k-spy-cmd $$spy-command-set-opc-clock)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-spy-cmd $$spy-command-step)
  (k-read-spy-pc))

;a convenience for figuring out these funny constants.
(defun k-execute-dis (instr-h instr-l)
  (format t "~&~A" (nc:dis (+ (ash instr-h 32.) instr-l))))

(defun k-execute0 (instr-h instr-l)
  "Load a spy instruction and let it sit there (spy-mode) (no step)"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction))     ; force icache load

(defun k-execute-substep ()
  (k-spy-cmd $$spy-command-set-spy-mode)
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)      ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-execute (instr-h instr-l)
  "Load a spy instruction and step the clock once"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)    ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-executex (instr-h instr-l)
  "Load a spy instruction (swapping hi and lo parts) and step the clock once"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i1 instr-l) ; low half instruction
  (k-spy-i0 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)    ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-execute2 (instr-h instr-l)
  "Load a spy instruction and step the clock twice"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; do it again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-execute3 (instr-h instr-l)
  "Load a spy command and step the clock 3 times"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; do it again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; and again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-execute4 (instr-h instr-l)
  "Load a spy instruction and step the clock 4 times"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; do it again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; and again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-cmd $$spy-command-reload-instruction)     ; and yet again
  (k-spy-cmd $$spy-command-step)     ; spy step command
  (k-spy-command $$spy-command-clear-spy-mode))

(defun k-execute-run (instr-h instr-l)          ;see k-spy-run
  "Load a spy instruction and let the processor run it forever"
  (k-spy-cmd $$spy-command-stop)     ; ensure that the run bit is off
  (k-spy-cmd $$spy-command-set-spy-mode)     ; set spy mode
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-spy-cmd $$spy-command-reload-instruction)     ; force icache load
  (k-spy-cmd $$spy-command-run))    ; run


(defvar *default-pctl* 0)

(defun k-setup-pctl ()
  (k-execute KIH-LOAD-PCTL 0.)                  ;flush caches
  (k-execute KIH-LOAD-PCTL *default-pctl*)
  (k-execute4 KIH-NOP 0.))                      ;and fill the instruction pipe with NOPs

(defun k-init ()
  "Init some registers, and set the PC to *cache-data-selector*"
  (k-stop)
  (k-execute KIH-LOAD-VMA 0)
  (k-execute KIH-LOAD-MAP #x8f)
  (k-execute KIH-JUMP    *cache-data-selector*) ;this could cause page faults if selecting "other" cache datapath.
  (k-write-mode 6)                              ;enable nubus master interface
  (k-spy-command $$spy-command-set-opc-clock)   ;enable PC history collection
  (k-read-spy-pc))

(defun k-init0 ()
  "Stop and set the PC to #x0100"
  (k-execute KIH-JUMP *cache-data-selector*))

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
    (cond ((k-kbug::k-halted-p)
           (k-stop))            ;maybe this does some initialization even if halted?
          (t
           (let ((pc (k-stop)))
             (global:format t "Falcon was running, stopped it at PC #x~x" pc))))
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

(defun w-md-sw (data)
  (k-execute kih-load-md-sw data)
  (k-execute4 kih-nop 0))

(defun r-md ()
  (k-execute4 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))


(defun x100 ()
 ;** assumes processor in slot C **
  (k-stop)
  (k-reset)
  (dotimes (i 1024.)
    (debug-write-word (logior #xfc000000 (* i 4)) i))   ;write own location in low 1024. words
  (debug-write-word #xfc000800 #x100)                   ;write in locn #x800,#x801 a jump #x100.
  (debug-write-word #xfc000804 kih-jump)
  (k-init)
  (k-execute kih-load-vma #x02000000)           ;address code location 0
  (k-execute kih-load-map #xfc000005)           ;map in to phys mem page 0, slot #xc, read-only
  (k-execute kih-load-mctl #x40000)             ;disable boot prom
  (k-execute4 kih-jump #x100)
  (k-step)                      ;this should fetch the jump #x100 we stored above.
  (k-read-spy-pc))              ;should be #x100. (I think)

(defun zap-mem ()
  (dotimes (i 4096.)
    (k-mem-write (* i 8) 0)
    (k-mem-write (logior 4 (* i 8)) kih-jump)))

(defun zorch-mem ()
  (dotimes (i 4096.)
    (k-mem-write (* i 8) -1)
    (k-mem-write (logior 4 (* i 8)) 0)))



(defun falcon-initialize-call-hardware ()
  (k-init)
  (k-execute kih-load-csp #xffff)       ;set heap-pointer,, stack-pointer to top.
  (k-execute3 kih-nop 0)
  (do ((i 255. (sub1 i)))
      ((< i 0))
    (k-execute kih-load-oar i)          ;load return register with frame number.
    (k-execute3 kih-nop 0)
    (k-execute kih-xreturn 0))          ;"return" that frame to free list.
  (k-execute3 kih-nop 0)
  (k-execute kih-load-csp #xec00)       ;heap-pointer #xEC, stack-pointer #x00
  (k-execute kih-load-oar #x111110)     ;open frame #x11, active #x11, return #x10.
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
      (k-execute kih-open-call *cache-data-selector*)
      (k-execute4 kih-alu-nop kil-read-oar)
      (format t "HP - ~X       HEAP - ~X~%" hp (logand 255. (ash (k-read-spy-mmfio) -16.))))
    (k-execute kih-load-oar oar)
    (k-execute kih-load-csp hp-csp)
    (k-execute4 kih-nop 0)))


(defun k-set-pc (addr)
  (k-execute4 KIH-JUMP addr))

(defun k-set-pc-and-check (addr &aux temp)
  (k-set-pc addr)
  (when (not (= (setq temp (k-read-spy-pc)) addr))
    (fsignal "PC failed to load, is #x~x should be #x~x" temp addr)))

(defconstant map-bits-invalid #x0)
(defconstant map-bits-r       #x5)
(defconstant map-bits-rw      #xF)

(defconstant cluster-size     #x400)

(defun k-map-cluster (virtual-address physical-address bits)
;    (format t "~%VA ~X     PA ~X" virtual-address physical-address)
  (k-execute kih-jump *cache-data-selector*)
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
  (falcon-initialize-call-hardware)
  (k-init-virtual-memory)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute kih-load-mctl #x40000)             ;disable boot-prom
  (k-execute4 kih-jump *cache-data-selector*)
  (k-set-pc starting-address)
  (k-setup-pctl)
  (k-set-pc starting-address)
  (k-spy-cmd $$spy-command-reload-instruction))


(defun k-go (starting-address)
  (k-setup-pctl)
  (k-start starting-address)
  (k-run))

(defun falcon-set-pc-and-go (starting-address
                             &key do-init do-initialize-call-hardware do-init-virtual-memory
                                  (processor-control-register 0)
                                  (memory-control-register #x40000)     ;disable boot prom.
                                  )
  (if do-init (k-init))         ;this clobbers the  VMA and map-location 0.
  (if do-initialize-call-hardware (falcon-initialize-call-hardware))
  (if do-init-virtual-memory (k-init-virtual-memory))
  (when memory-control-register
    (k-execute kih-load-mctl memory-control-register)
    (k-execute4 kih-jump *cache-data-selector*))
  (when processor-control-register
    (k-execute KIH-LOAD-PCTL 0.)                        ;flush cache
    (k-execute4 KIH-NOP 0.)
    (k-execute KIH-LOAD-PCTL processor-control-register))
  (k-set-pc-and-check starting-address)
  (k-spy-cmd $$spy-command-reload-instruction)
  (k-run)
  )

;read-inst-physical-with-offset takes PC address, however, it does not do a real virtual-to-physical transformation.
; Instead, we assume the code has been loaded in a contiguous block in physical memory starting at k-kbug:*code-start*.
; This corresponds to the assumption of KBUG.  Also, VC-TEST-DRIVER, etc, use this.
(defun read-inst-physical-with-offset (doubleword-address)
  (let ((adr0 (logand (* 8 (+ k-kbug:*code-start* doubleword-address)) #xFFFFFF)))
    (logior (k-mem-read adr0) (ash (k-mem-read (+ 4 adr0)) 32.))))

(defun read-inst (doubleword-address)  ;input is actual physical address.  p-space bit and mapping should already have
  (logior                              ; been dealt with.
    (k-mem-read (logand (* 8 doubleword-address) #xFFFFFF))
    (ash (k-mem-read (logand (+ 4 (* 8 doubleword-address)) #xFFFFFF)) 32.)))

(defun write-inst (doubleword-address inst)
  (k-mem-write (* doubleword-address 8) (logand #xffffffff inst))
  (k-mem-write (+ 4 (* doubleword-address 8)) (logand #xffffffff (ash inst -32.))))

(defun write-inst-and-check (doubleword-address inst)
  (let ((d1 (logand #xffffffff inst))
        (d2 (logand #xffffffff (ash inst -32.)))
        (tem nil))
    (k-mem-write (* doubleword-address 8) d1)
    (k-mem-write (+ 4 (* doubleword-address 8)) d2)
    (cond ((not (= d1 (setq tem (k-mem-read (* doubleword-address 8)))))
           (error "D1 failed, wrote ~s, read ~s" d1 tem)))
    (cond ((not (= d2 (setq tem (k-mem-read (+ 4 (* doubleword-address 8))))))
           (error "D2 failed, wrote ~s, read ~s" d2 tem)))))

(defun read-active (reg)
  (k-execute4 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-A0))
  (k-read-spy-mmfio))

(defun read-open (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-read-spy-mmfio))

(defun read-open-boxed (reg)
  (k-execute2
    KIH-ALU-G1FBR    ;(dpb reg (byte 4. (- 41. 32.)) KIH-ALU-O0)
    (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun read-vma-boxed ()
  (k-execute2
    KIH-ALU-G1FBR       ;(dpb 9. (byte 4. (- 41. 32.)) KIH-ALU-G0)
    KIL-READ-VMA)
  (k-execute3 kih-alu-nop kil-read-pstat)
  (ldb (byte 1. 18.) (k-read-spy-mmfio)))

(defun read-md-boxed ()
  (k-execute2
    KIH-ALU-G1FBR       ;(dpb 9. (byte 4. (- 41. 32.)) KIH-ALU-G0)
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
  (k-execute2 kih-jump *cache-data-selector*)
  (k-execute3 kih-jump *cache-data-selector*))

(defun k-write-oar-multiple (value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  (k-execute kih-load-oar value)
  )


(defun read-q ()
  (k-execute3 KIH-ALU-NOP KIL-READ-Q)
  (k-read-spy-mmfio))

(defun pr (addr)
  (k-execute kih-load-vma-sr addr)
  (k-execute kih-jump *cache-data-selector*)
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
  (k-execute4 KIH-JUMP *cache-data-selector*)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun k-read-current-map-address ()
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun k-write-current-map-entry (value)
  (k-execute4 kih-load-map value)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-jump *cache-data-selector*)
  )

(defun k-write-memory-map (entry value)
  (k-execute2 kih-load-vma (ash entry 10.))
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-load-map value)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-jump *cache-data-selector*)
  )

(defun falcon-read-map (entry)
  (k-read-memory-map entry))

(defun falcon-write-map-and-check (entry value &aux temp)
  (k-write-memory-map entry value)
  (if (not (= (setq temp (k-read-memory-map entry)) value))
      (fsignal "Write map failed, is ~x should be ~x" value temp)))

(defun k-load-memory-control (value)
  (k-execute  kih-load-mctl value)
  (k-execute4 kih-jump *cache-data-selector*))

(defun k-write-memory-control (value)
  (k-load-memory-control value))

(defun k-read-memory-control ()
  (k-execute3 kih-alu-nop kil-read-mctl)
  (prog1 (k-read-spy-mmfio)
         (k-execute kih-jump *CACHE-DATA-SELECTOR*)))

(defun k-read-processor-control ()
  (k-execute3 kih-alu-nop kil-read-pctl)
  (prog1 (k-read-spy-mmfio)
         (k-execute kih-jump *CACHE-DATA-SELECTOR*)))

(defun k-write-processor-control (value)
  (k-execute  kih-load-pctl value)
  (k-execute4 kih-jump *cache-data-selector*))

(defun k-read-processor-status ()
  (k-execute3 kih-alu-nop kil-read-pstat)
  (prog1 (k-read-spy-mmfio)
         (k-execute kih-jump *CACHE-DATA-SELECTOR*)))

(defun k-read-float-status ()
  (ldb k-hw:%%processor-status-floating-point-status
       (k-read-processor-status)))

(defun k-read-virtual-memory (location)
  (k-execute kih-load-vma-sr location)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute3 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))

(defun k-write-virtual-memory (location value &optional inhibit-read-only)
  (k-execute2 kih-load-vma location)
  (k-execute4 KIH-JUMP *cache-data-selector*)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (let ((map (k-read-spy-mmfio)))
    (if (not (= (logand #x3 map) #x3))
        (cond ((zerop (logand #x1 map))
               (fsignal "The map entry for location #x~x is #x~x, which not valid" location map))
              ((null inhibit-read-only)
               (fsignal "The map entry for location #x~x is #x~x, which does not permit writes" location map))
              (t (k-execute4 kih-load-map (logior #x2 map))
                 (k-execute kih-load-md-sw value)
                 (k-execute4 kih-load-map map)
                 (k-execute3 kih-jump *cache-data-selector*)
                 (return-from k-write-virtual-memory nil)))))
  (k-execute kih-load-md-sw value)
  (k-execute3 kih-jump *cache-data-selector*))

(defun k-vma-start-read (vma-boxed md-boxed trans-type location)
  (k-execute
    (dpb md-boxed (byte 1. (- 55. 32.))
         (dpb vma-boxed (byte 1. (- 54. 32.))
              (dpb trans-type (byte 2. (- 41. 32.))
                   kih-load-vma-sr)))
    location)
  (k-execute4 kih-jump *cache-data-selector*))

(defun k-read-gc-ram (location)
  (k-execute kih-load-md (ash location 14.))
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute3 kih-alu-nop kil-read-gc-rams)
  (logand #xF (k-read-spy-mmfio)))

(defun k-write-gc-ram (location value)
  (k-execute kih-load-md (ash location 14.))
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute kih-load-gc-ram value)
  (k-execute3 kih-jump *CACHE-DATA-SELECTOR*))

(defun k-address-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute3 kih-alu-nop kil-read-mctl)
  (let ((memcontrol (dpb trans-mode k-hw:%%memory-control-transporter-mode (k-read-spy-mmfio))))
    (k-execute kih-load-mctl memcontrol)                ;trans-mode is driven from memory-control register.
    (k-execute4 kih-jump *cache-data-selector*))                        ; it is normally always 0.
  (let ((vma-start-read-instruction
          (dpb vma-boxed (byte 1. (- 54. 32.))
               (dpb trans-type (byte 2. (- 41. 32.))    ;trans-type is flavor of vma-start-read, etc.  put in low
                    kih-load-vma-sr))))                 ; order functional destination bits.
    (k-execute vma-start-read-instruction 0))
  (k-execute2 kih-jump *cache-data-selector*)
  (k-execute (dpb md-boxed (byte 1. (- 55. 32.)) kih-load-md)
             (dpb datatype (byte 6. 26.) 0))
  (k-execute4 kih-jump *CACHE-DATA-SELECTOR*))

(defun k-read-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype)
  (k-address-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute3 kih-alu-nop kil-read-gc-rams)
  (ldb k-hw:%%transporter-ram-bus-offset (k-read-spy-mmfio)))

(defun k-write-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype data)
  (k-address-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)
  (k-execute2 kih-load-transporter (dpb data k-hw:%%transporter-ram-bus-offset 0))
  (k-execute4 kih-jump *CACHE-DATA-SELECTOR*))

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
  (k-execute4 kih-jump *CACHE-DATA-SELECTOR*))

(defun saving-hp-csp (thunk)
  (let ((hp-csp (k-read-hp-sp)))
    (unwind-protect
        (funcall thunk
                 (ldb (byte 8 8) hp-csp)        ;hw:%%ch-csphp-heap-pointer
                 (ldb (byte 8 0) hp-csp))       ;hw:%%ch-csphp-call-stack-pointer
      (k-write-hp-csp hp-csp))))

(defun k-read-frame (frame-number)
  (saving-oar
    #'(lambda (oar)
        (k-write-oar (dpb frame-number k-hw:%%ch-oar-open oar))
        (labels ((read-regs (count values)
                   (if (= count k-hw:frame-size)
                       values
                       (let ((value (read-open       count))
                             (boxed (read-open-boxed count)))
                         (read-regs (1+ count) (cons (list count boxed value) values))))))
      (read-regs 0 '())))))

(defun k-read-register (frame offset)
  (saving-oar
    #'(lambda (ignore)
        (k-write-oar (dpb frame k-hw:%%ch-oar-open 0))
        (list (read-open offset)
              (read-open-boxed offset)))))

(defun k-read-retpc-rdest ()
  (k-execute3 kih-alu-nop kil-read-rpc) ;%%ch-rpcd-return-pc %%ch-rpcd-return-dest
  (k-read-spy-mmfio))                   ;(byte 24. 0) is return-pc, (byte 3. 28.) is frame specifier,
                                        ;(byte 4. 24.) is offset within frame.

(defun k-read-trap-off ()
  (k-execute  kih-alu-nop kil-read-trap-off)
  (k-execute2 kih-alu-nop 0)
  (k-read-spy-mmfio))

(defun k-read-memory-status ()
  (k-execute3 kih-alu-nop kil-read-mstat)
  (k-read-spy-mmfio))

(defun k-write-datatype-ram (address data)
  (k-execute kih-load-pctl (dpb data (byte 1 8) 0))     ;data to write.
  (let* ((right-datatype (ldb (byte 6 0) address))
         (right-boxed (ldb (byte 1 6) address))
         (left-datatype (ldb (byte 6 7) address))
         (left-boxed (ldb (byte 1 13.) address))
         (ireg-bits (ldb (byte 3 14.) address)))
    (k-execute KIH-JUMP *cache-data-selector*)
    (k-execute (dpb (+ 2 left-boxed) (byte 2 22.) kih-load-g1)
               (ash left-datatype 26.)) ;write g1 with left datatype and boxed
    (k-execute (dpb (+ 2 right-boxed) (byte 2 22.) kih-load-g0)
               (ash right-datatype 26.))        ;write g0 write right datatype and boxed
    ;construct and execute instruction
    (k-execute2 (dpb ireg-bits (byte 3. 19.) kih-alu-load-dt) kil-add-g1-g0)
    (k-execute3 (dpb ireg-bits (byte 3. 19.) kih-alu-nop) kil-add-g1-g0)))  ;maintain address while write pulse goes away.

(defun k-read-datatype-ram (address)
  (let* ((right-datatype (ldb (byte 6 0) address))
         (right-boxed (ldb (byte 1 6) address))
         (left-datatype (ldb (byte 6 7) address))
         (left-boxed (ldb (byte 1 13.) address))
         (ireg-bits (ldb (byte 3 14.) address)))
    (k-execute KIH-JUMP *cache-data-selector*)
    (k-execute (dpb (+ 2 left-boxed) (byte 2 22.) kih-load-g1)
               (ash left-datatype 26.)) ;write g1 with left datatype and boxed
    (k-execute (dpb (+ 2 right-boxed) (byte 2 22.) kih-load-g0)
               (ash right-datatype 26.))        ;write g0 write right datatype and boxed
    ;construct and execute instruction
    (k-execute (dpb ireg-bits (byte 3. 19.) kih-alu-nop) kil-add-g1-g0)
    (k-execute3 kih-alu-nop kil-read-pstat)
    (ldb (byte 1. 13.) (k-read-spy-mmfio))))

(defun dump-caches ()
  ;this can't really work since it can't access the cache-tag memories.
  ;any attempt to load ireg will clobber 4 cache entries at current pc.
  ;this makes things pretty hopeless.
  (global:fsignal "too bad")
  )
