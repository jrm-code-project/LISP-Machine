;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-

;;; This file is a copy of the diags on the board.
;;; The primitives are hacked however to run the
;;; simulator.

;;; The top 32 bits of some useful instructions.
(defconstant KIH-NOP          #x0F008000 "loadi-32 to no-op")
(defconstant KIH-ALU-NOP      #x03008000 "generic ALU instruction w/NOP dest")
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
(defconstant KIH-LOAD-A0      #x0F002000 "load A0 register instruction")
(defconstant KIH-LOAD-O0      #x0F000000 "load O0 register instruction")
(defconstant KIH-LOAD-R0      #x0F004000 "load R0 register instruction")
(defconstant KIH-LOAD-PCTL    #x0F009200 "load processor control reg")
(defconstant KIH-LOAD-MCTL    #x0F00C400 "load memory control reg")
(defconstant KIH-LOAD-VMA     #x0F00D000 "load VMA")
(defconstant KIH-LOAD-VMA-SR  #x0F00E000 "load VMA")
(defconstant KIH-LOAD-MD      #x0F00D400 "load Write-MD")
(defconstant KIH-LOAD-MD-SW   #x0F00DC00 "load MD-START-WRITE")
(defconstant KIH-LOAD-MAP     #x0F00C000 "load map")
(defconstant KIH-LOAD-OAR     #x0F009400 "load Open, Active, Return")
(defconstant KIH-LOAD-CSP     #x0F009800 "load call stack pointer & heap pointer")
(defconstant KIH-LOAD-RPC     #x0F009600 "load return PC and dest")
(defconstant KIH-LOAD-USC     #X0F00C600 "load microsecond clock")
(defconstant KIH-LOAD-STAT    #X0F00C800 "load statistics counter")

;;; The bottom 32 bits of some useful instructions.
(defconstant KIL-READL-G0     #x81800000 "read G0 as left source")
(defconstant KIL-READR-G0     #x60001000 "read G0 as right source")
(defconstant KIL-READ-A0      #x20001000 "read A0 as right source")
(defconstant KIL-READ-O0      #x00001000 "read O0 as right source")
(defconstant KIL-READ-R0      #x40001000 "read R0 as right source")
(defconstant KIL-READ-OAR     #x94001000 "read OAR")
(defconstant KIL-READ-PCTL    #x92001000 "read PCTL")
(defconstant KIL-READ-PSTAT   #x90001000 "read PSTAT")
(defconstant KIL-READ-MCTL    #xC4001000 "read MCTL")
(defconstant KIL-READ-CSP     #x98001000 "read CSP")
(defconstant KIL-READ-RPC     #x96001000 "read RPC")
(defconstant KIL-LR-SUB       #x61844000 "G0 - G0")
(defconstant KIL-READ-VMA     #xD0001000 "Read VMA")
(defconstant KIL-READ-MD      #xD4001000 "Read MD")
(defconstant KIL-READ-MAP     #xC0001000 "Read MAP")
(defconstant KIL-READ-USC     #xC6001000 "Read Microsecond clock")
(defconstant KIL-READ-STAT    #xC8001000 "Read Statistics counter")

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

;(defsubst k-write-mode (n)
;  (debug-write-word k-mode-addr n))

;(defsubst k-read-mode ()
;  (debug-read-word k-mode-addr))

;(defsubst k-spy-cmd (n)
;  (debug-write-word k-spyc-addr n))

(defun k-spy-i0 (n)
  (setq sim::*spy-ireg-low* n))

(defun k-spy-i1 (n)
  (setq sim::*spy-ireg-high* n))

(defun k-read-spy-pc ()
;  (format t "~%")
  (incf sim::*global-time*)
  (sim::select-next-pc)
  (logand #xFFFFFF sim::*pc*))

(defun k-read-spy-mmfio ()
  sim::*mfo-bus*)

;(defsubst k-mem-read (addr)
;  (debug-read-word (logior k-mem-addr addr)))

;(defsubst k-mem-write (addr data)
;  (debug-write-word (logior k-mem-addr addr) data))

(defun dpb32 (value ppss word)
  (let* ((mask1 (ash #xffffffff (byte-size ppss)))
         (mask2 (logand #xffffffff (lognot (ash mask1 (byte-position ppss)))))
         (mask3 (logxor #xffffffff mask2)))
    (logior (logand mask3 word)
            (logand mask2 (ash value (byte-position ppss))))))


;(defun check-falcon-prom (slot &aux s)
;  "Routine to check if a slot contains a Falcon processor"
;  (setq s "LMI FALCON PROCESSOR")
;  (if (fixp (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff7fc)))
;      (dotimes (i 15.)
;       (cond
;         ((not (equal (logand #xff (debug-read-word (+ (ash (logior #xf0 slot) 24.) #xfff800 (* i 4))))
;                      (aref s i)))
;          (return nil))
;         ((equal i 14.) (return t))))))

(defun k-setup ()
  "Sets up and initializes a debug link to a Falcon processor"
;  (init-debug-board)
;  (setq k-slot nil)
;  (dotimes (i 16.)
;    (when (check-falcon-prom i)
;      (setq k-slot (logior #xf0 i))
;      (format t "~%   Falcon processor found in slot ~A.~%" i)))
;  (when (null k-slot)
;      (format t "~%*** Can't setup, I couldn't find a processor! ***~%")
;      (cerror "Choose a slot number" "Can't setup, no processor!")
;      (setq k-slot (logior #xf0 (logand #x0f (progn (format t "~&Slot number?") (read))))))

;    (setq k-mem-addr  (ash k-slot 24.))
;    (setq k-io-addr   (logior #xfff000 k-mem-addr))
;    (setq k-mode-addr (logior #x7fc k-io-addr))
;    (setq k-hptr-addr (logior #x600 k-io-addr))
;    (setq k-hram-addr (logior #x640 k-io-addr))
;    (setq k-pc-addr   (logior #x680 k-io-addr))
;    (setq k-mmfio-addr(logior #x6c0 k-io-addr))
;    (setq k-spy0-addr (logior #x6c0 k-io-addr))
;    (setq k-spy1-addr (logior #x6c4 k-io-addr))
;    (setq k-spyc-addr (logior #x700 k-io-addr))
;    (setq k-int-addr  (logior #x780 k-io-addr))
  (format t "~&Found a simulated processor!")
  (k-reset))


(defun k-reset ()
   "Issue a reset to the K processor and stops the clocks"
   (k-stop)
   (sim::init-bls))

(defun k-stop ()
  "Stop the processor clocks, and init spy modes"
  (setq sim::*clock-enable* nil)
  (process-wait "K run" #'(lambda () (not sim::*clock-on*)))
  (k-read-spy-pc))

(defun k-run ()
  "Start the processor running"
  (k-stop)
  (setq sim::*clock-function* #'sim::clock)
  (setq sim::*clock-enable*   t)
  (process-wait "K start" #'(lambda () sim::*enable-acknowledge*))
  (setq sim::*enable-acknowledge* nil)
  (k-read-spy-pc))

(defun k-step ()
  "Step the processor one clock cycle"
;  (sleep 1. "sigh")
  (k-stop)
  (setq sim::*clock-function* #'sim::clock-step)
  (setq sim::*clock-enable* t)
  (process-wait "K start" #'(lambda () sim::*enable-acknowledge*))
  (setq sim::*enable-acknowledge* nil)
  (process-wait "K stop" #'(lambda () (not sim::*clock-on*)))
  (k-read-spy-pc))

(defun k-execute (instr-h instr-l)
  (k-stop)
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-step))

(defun k-execute2 (instr-h instr-l)
  "Load a spy instruction and step the clock twice"
  (dotimes (i 2)
    (k-execute instr-h instr-l)))

(defun k-execute3 (instr-h instr-l)
  "Load a spy command and step the clock 3 times"
  (dotimes (i 3)
    (k-execute instr-h instr-l)))

(defun k-execute4 (instr-h instr-l)
  "Load a spy command and step the clock 4 times"
  (dotimes (i 4)
    (k-execute instr-h instr-l)))

(defun k-execute-run (instr-h instr-l)
  "Load a spy command and let the processor run"
  (k-spy-i0 instr-l) ; low half instruction
  (k-spy-i1 instr-h) ; high half instruction
  (k-run))

(defvar *default-pctl* 4)

(defun k-init ()
  "Init some registers, and set the PC to #x0100"
  (k-stop)
  (k-execute3 KIH-JUMP #x100))
  (k-execute KIH-LOAD-PCTL 0.)                  ;flush cache
  (k-execute KIH-NOP 0.)
  (k-execute KIH-LOAD-PCTL *default-pctl*)
  (k-execute KIH-LOAD-MCTL 0.)
  (k-execute KIH-LOAD-VMA  0.)
  (k-execute KIH-LOAD-MAP #x0F)
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

(defun r (reg)
  (k-execute4 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READR-G0))
  (k-read-spy-mmfio))


(defun k-test6 (&aux temp)
  "Simple test G0 - G15."
  (format t "Starting Test 6 - Simple G0 - G15.~%")
  (k-init)
  (dotimes (j 32.)                                              ; 32 patterns
    (k-execute KIH-JUMP #x100)                                  ; reset PC
    (dotimes (i 16.)                                            ; G0 - G15
      (k-execute (dpb i (byte 4. 9.) KIH-LOAD-G0)               ; load reg
                 (aref bit-pattern (logand 31. (+ i j)))))
;    (k-execute KIH-NOP 0)
;    (k-execute KIH-ALU-NOP (dpb 0. (byte 4. 25.) KIL-READR-G0))        ; read G0
;    (k-execute KIH-ALU-NOP (dpb 1. (byte 4. 25.) KIL-READR-G0))        ; read G1
    (dotimes (i 16.)                    ; read the rest, and compare values
      (k-execute3 KIH-ALU-NOP (dpb i (byte 4. 25.) KIL-READR-G0))
      (setq temp (k-read-spy-mmfio))
      (when (not (equal (aref bit-pattern (logand 31. (+ i j))) temp))
        (k-diag-error "TEST-6 Global Regs"
                      i (aref bit-pattern (logand 31. (+ i j))) temp)))))

(defun k-test6a (&aux temp)
  "Simple test G0 - G15."
  (format t "Starting Test 6 - Simple G0 - G15.~%")
  (k-init)
  (dotimes (j 32.)                                              ; 32 patterns
    (k-execute KIH-JUMP #x100)                                  ; reset PC
    (dotimes (i 16.)                                            ; G0 - G15
      (k-execute (dpb i (byte 4. 9.) KIH-LOAD-G0)               ; load reg
                 (aref bit-pattern (logand 31. (+ i j)))))
    (k-execute KIH-NOP 0)
    (k-execute KIH-ALU-NOP (dpb 0. (byte 4. 25.) KIL-READR-G0)) ; read G0
    (k-execute KIH-ALU-NOP (dpb 1. (byte 4. 25.) KIL-READR-G0)) ; read G1
    (dotimes (i 16.)                    ; read the rest, and compare values
      (k-execute KIH-ALU-NOP (dpb (+ i 2) (byte 4. 25.) KIL-READR-G0))
      (setq temp (k-read-spy-mmfio))
      (when (not (equal (aref bit-pattern (logand 31. (+ i j))) temp))
        (k-diag-error "TEST-6 Global Regs"
                      i (aref bit-pattern (logand 31. (+ i j))) temp)))))

(defun k-test6b (&aux temp)
  "Simple test G0 - G15."
  (format t "Starting Test 6 - Simple G0 - G15.~%")
  (k-init)
  (dotimes (k 16.)                              ;16 global-frames
  (dotimes (j 32.)                                              ; 32 patterns
    (k-execute KIH-JUMP #x100)                                  ; reset PC
    (dotimes (i 16.)                                            ; G0 - G15
      (k-execute (dpb i (byte 4. 9.)
                      (dpb k (byte 4. 5.) KIH-LOAD-G0))         ; load reg
                 (aref bit-pattern (logand 31. (+ i j)))))
    (k-execute KIH-NOP 0)
    (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 0. (byte 4. 25.) KIL-READR-G0))    ; read G0
    (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 1. (byte 4. 25.) KIL-READR-G0))    ; read G1
    (dotimes (i 16.)                    ; read the rest, and compare values
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
     (dotimes (frame 16.)
      (k-execute KIH-LOAD-OAR (ash frame 8.))           ;Active = frame
      (k-execute3 KIH-JUMP #x100)                       ;reset PC
      (dotimes (i 16.)
        (k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)     ;load regs
                   (+ (ash frame 4.) i))))
    (dotimes (frame 16.)
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

(defun format-jstuff (time)
  (format t "~&After ~s, *jump* is ~d, *jcond* is ~d" time sim::*jump*
          (ldb hw:%%inst-alu-jump-condition-select sim::*ireg* )))

(defun k-test10-jtest (left right jcond jump msg &aux temp)
  (setq temp (dpb right (byte 4. 25.) KIL-LR-SUB))
  (setq temp (dpb left (byte 4. 19.) temp))
  (k-execute KIH-ALU-NOP temp)                     ; (- left right)
;  (format-jstuff 'subtract)
  (k-execute (dpb jcond (byte 3. 2.) KIH-JUMP) #x100)   ; Jump 100, sel jcond
;  (format-jstuff 'jcond-select)                        ;
  (k-execute KIH-JCOND #xFFFFFC)                   ; Conditional jump FFFFFC
;  (format-jstuff 'jump)
  (setq temp (k-read-spy-pc))
  (when (and jump (not (equal temp #xFFFFFC)))
        (k-diag-error (format nil "Test 10 - ~A" msg) nil #xFFFFFC temp))
  (when (and (not jump) (not (equal temp #x101)))
        (k-diag-error (format nil "Test 10 - ~A" msg) nil #x101 temp))
;  (format t "Test over.")
;  (k-execute kih-nop 0)
;  (format-jstuff 'noop)
;  (k-execute kih-nop 0)
;  (k-execute kih-nop 0)
  )

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

(defun k-test16 (&aux temp)
  "Call Stack data - PC & RDEST"
  (format t "Starting Test 16 - Call Stack data - PC & RDEST.~%")
  (k-init)
  (dotimes (pat 32.)
     (format t "  Pass ~D~%" pat)
     (dotimes (sgn 16.)
        (k-execute3 KIH-LOAD-CSP #xffff)             ;CSP = 0
        (k-execute KIH-LOAD-PCTL (logior #x20000 (ash sgn 13.))) ;Stack Group
        (k-execute3 KIH-NOP 0)
        (k-execute3 KIH-NOP 0)
        (dotimes (i 256.)
           (k-execute KIH-LOAD-RPC (ash 1. (logand 31. (+ i pat))))
           (k-execute KIH-NOP 0.)
           (k-execute KIH-CALL #x100)))
     (dotimes (sgn 16.)
        (k-execute KIH-LOAD-PCTL (logior #x20000 (ash sgn 13.))) ;Stack Group
        (k-execute3 KIH-NOP 0)
        (k-execute3 KIH-NOP 0)
        (dotimes (i 256.)
           (k-execute KIH-ALU-NOP KIL-READ-RPC)
           (k-execute KIH-RETURN 0.)
           (k-execute KIH-JUMP #x100)
           (setq temp (k-read-spy-mmfio))
           (when (not (equal temp (ash 1. (logand 31. (+ i pat)))))
                 (k-diag-error "TEST-16 Call Stack RPC data"
                               (+ (ash sgn 8.) i)
                               (ash 1. (logand 31. (+ i pat))) temp))))))

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


(defun k-test16x (&aux temp)
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
    (k-execute KIH-LOAD-MAP (ash 1 i))
    (k-execute3 KIH-nop #x100)
    (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-23 MAP Data " 0 (ash 1. i) temp))
    )
  )

(defun wm (pc adr data)
  (k-execute KIH-LOAD-VMA (ash adr 10.))
  (k-execute3 KIH-JUMP pc)
  (k-execute KIH-LOAD-MAP data)
  (k-execute3 KIH-JUMP pc))

(defun rm (pc adr)
  (k-execute KIH-LOAD-VMA (ash adr 10.))
  (k-execute3 KIH-JUMP pc)
  (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))


(defun k-test24 (&aux temp)
  "Map data Test."
  (format t "Starting Test 24 - MAP data RAM.")
  (k-init)
  (dotimes (pat 32.)
    (format t "~%Pass ~D       Writing." pat)
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute KIH-LOAD-MAP (ash 1 (logand 31. (+ adr pat))))
      (k-execute KIH-JUMP #X100))
    (format t "      Reading.")
    (dotimes (adr #X10000)
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute3 KIH-JUMP #X100)
      (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal (ash 1 (logand 31. (+ adr pat))) temp))
        (k-diag-error "TEST-24 MAP Data " adr (ash 1 (logand 31. (+ adr pat))) temp)))))


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
  (let*
    ((addr 0)
     (temp nil)
     (gap t))
    (setq k-mem-list nil)
    (dotimes (i 128.)
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
      (setq k-mem-list (nconc k-mem-list (cons (list temp (sub1 addr)) nil))))))


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

          (k-mem-write addr (lognot addr))))

      (dolist (mrange k-mem-list)
        (do*
          ((addr (first mrange) (+ addr 4.))
           (max  (second mrange)))
          ((> addr max))

          (when (not (equal (lognot addr) (setq temp (k-mem-read addr))))
            (k-diag-error "TEST-23 - Memory address"
                          addr (lognot addr) temp)))))))

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
                  (k-diag-error "TEST-24 Memory Bank data patterns" addr pat temp)))))))


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

(defun load-code (code &optional (addr #x100))
  "Load a sequence of instructions at starting address"
  (lisp:map nil #'(lambda (inst)
               (write-inst addr inst)
               (incf addr))
       code))

(defun load-fcn (fcn &optional (starting-address #x100 sa?))
  (let ((f (nc:get-ncompiled-function fcn)))
    (let ((sa (nc:ncompiled-function-starting-address f)))
      (when (or (null sa)
                (and sa?
                     (not (= starting-address sa))))
        (setq sa starting-address)
        (nc:link f sa))
      (load-code (nc:ncompiled-function-code f) sa))))


(defun run-code (code &optional (starting-address #x100))
  (k-stop)
  (k-reset)
  (dotimes (i 1024.)
    (debug-write-word (logior #xfc000000 (* i 4)) i))
  (load-code code starting-address)
  (k-init)
  (k-execute kih-load-vma #x02000000)
  (k-execute kih-load-map #xfc000005)
  (k-execute kih-nop 0)
  (k-execute kih-load-vma #x02000400)
  (k-execute kih-load-map #xfc001005)
  (k-execute kih-load-mctl #x40000)
  (k-execute4 kih-jump starting-address)
  (k-step)
  (k-run)
  (do-forever
    (k-write-mode 2)
    (process-sleep 10.)
    (k-write-mode 6)
    (process-sleep 10.)))


(defun k-start (starting-address)
  (k-init)
  (ch-init)
  (k-execute kih-load-vma #x02000000)
  (k-execute kih-load-map #xfc000005)
  (k-execute kih-nop 0)
  (k-execute kih-load-vma #x02000400)
  (k-execute kih-load-map #xfc001005)
  (k-execute kih-load-mctl #x40000)
  (k-execute4 kih-jump starting-address))


(defun k-setup-pctl ()
  (k-execute KIH-LOAD-PCTL 0.)                  ;flush cache
  (k-execute KIH-NOP 0.)
  (k-execute KIH-LOAD-PCTL *default-pctl*))

(defun k-go (starting-address)
  (k-init)
  (ch-init)
  (k-execute kih-load-vma #x02000000)
  (k-execute kih-load-map #xfc000005)
  (k-execute kih-nop 0)
  (k-execute kih-load-vma #x02000400)
  (k-execute kih-load-map #xfc001005)
  (k-execute kih-load-mctl #x40000)
  (k-setup-pctl)
  (k-execute4 kih-jump starting-address)
  (k-step)
  (k-run))

(defvar *i-mode* :dis)

(defun print-instruction (i)
  (case *i-mode*
    (:hex (format t "~16,'0x" i))
    (:dis (format t "~a" (nc:dis i)))))

(defun ddt ()
  (do ()
      (())
    (let ((addr (logand #xFFFFFF (k-read-spy-pc))))
    (format t "~&~4,'0x: " addr)
    (print-instruction (read-inst addr))
    (tyi)
    (k-step))))


(defun read-inst (addr)
  (logior
    (debug-read-word (logior #xFC000000 (logand (* 8 addr) #xFFFFFF)))
    (ash (debug-read-word (logior #xFC000004 (logand (* 8 addr) #xFFFFFF)))
         32.)))

(defun write-inst (adr inst)
  (debug-write-word (logior #xfc000000 (* adr 8)) (logand #xffffffff inst))
  (debug-write-word (logior #xfc000004 (* adr 8)) (logand #xffffffff (ash inst -32.))))

(defconst blinky '#x( 03C0600061813000
                      03C0620063872663
                      03C0C400C58F00A3
                      03C0840041001000
                      28C0840000000100))



(defun blink ()
  (run-code blinky))


(defconst taktest1
          '#x(
              03C0600061813000
              03C0620060001000
              03C0C400C58F00A3
              0FC1000014000012
              0FC002001400000C
              0FC0040014000006
              08C2841800000108
              28C0840000000107

              ;tak routine:  #x108

              03C08403208C4000
              03C0841441001000
              00C0840041001116
              03C5840041001000
              03C1000320811000
              03C0020022001000
              08C2040024000108
              03C1000322811000
              03C0020024001000
              08C2040021000108
              03C1000324811000
              03C0020020001000
              08C2040122000108
              08C6840000000108
              02C4820024001000))

;start at 100
(defvar blink-call '#x(
                       03C0600061813000
                       03C0620060001000
                       03C0C400C58F00A3
                       0FC1000014000014   ;arg1
                       0FC002001400000C   ;arg2
                       0FC0040014000006   ;arg3
                       08C2841900000200   ;call
                       28C0840000000100))

;start at 200
(defvar fibtest '#x(
                    0FC0400014000002
                    03C0840340844000
                    03C0841441001000
                    00C0840041001205
                    02C4820020001000
                    03C1000320811000
                    08C2840900000200
                    0FC1400014000002
                    03C0000340844000
                    08C2840801000200
                    03C0840041001000
                    02C4820322942000))


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
  (k-execute kih-load-csp #xe000)
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
    (do ((hp (logand 255. (ash hp-csp -8.)) (sub1 hp)))
         ((< hp 0))
      (k-execute kih-open-call #x100)
      (k-execute4 kih-alu-nop kil-read-oar)
      (format t "HP - ~X       HEAP - ~X~%" hp (logand 255. (ash (k-read-spy-mmfio) -16.))))
    (k-execute kih-load-oar oar)
    (k-execute kih-load-csp hp-csp)
    (k-execute4 kih-nop 0)))


(defun read-active (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-A0))
  (k-read-spy-mmfio))

(defun read-open (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-O0))
  (k-read-spy-mmfio))

(defun read-return (reg)
  (k-execute3 KIH-ALU-NOP (dpb reg (byte 4. 25.) KIL-READ-R0))
  (k-read-spy-mmfio))

(defun read-oar ()
  (declare (values open active return))
  (k-execute3 KIH-NOP 0)
  (k-execute4 KIH-ALU-NOP KIL-READ-OAR)
  (let ((oar (k-read-spy-mmfio)))
    (values
      (ldb hw:%%ch-oar-open oar)
      (ldb hw:%%ch-oar-active oar)
      (ldb hw:%%ch-oar-return oar))))

(defun frames (&optional (n 16.))
  (multiple-value-bind (open active return) (read-oar)
    (format t "~&O: ~x  A: ~x  R: ~x" open active return)
    (dotimes (i n)
      (format t "~&O~2d: ~8,'0x  A~2d: ~8,'0x  R~2d: ~8,'0x"
              i (read-open i)
              i (read-active i)
              i (read-return i)))))
