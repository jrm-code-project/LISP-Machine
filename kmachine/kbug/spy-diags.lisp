;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-

;see variable *cache-data-selector* in new-spy-utilities

(defun k-test (&optional (fast nil))
  (k-setup)
  (k-diag-reset)
  (let ((*cache-data-selector* #x100))
    (k-test0)  (k-test1)  (k-test2)  (k-test3)  (k-test4)
    (k-test5)  (k-test6 fast)  (k-test6a) (k-test7) )
  (let ((*cache-data-selector* #x8100))
    (k-test0)  (k-test1)  (k-test2)  (k-test3)  (k-test4)
    (k-test5)  (k-test6 fast)  (k-test6a) (k-test7) )
  (k-test8 fast)  (k-test9 fast)
  (k-test10)
  ;(k-test10b)
  (k-test11) (k-test11b) (k-test12) (k-test13) (k-test14)
  (k-test15) (k-test16 fast) (k-test17 fast) (k-test18 fast) (k-test19 fast)
  (k-test20 fast) (k-test21) (k-test22) (k-test23) (k-test24 fast) (k-test24b fast)
  (k-test25) (k-test26) (k-test27) (k-test28); (k-test29)
  (k-test30) (k-test31) (k-test32) (k-test33) (k-test34 fast)

  (k-test41) (k-test42) (k-test43 fast) (k-test43a)
  (k-test44)
  (k-test45)
  (k-test50) (k-test51 fast) (k-test52 fast) (k-test53) (k-test54)
  (k-test55) (k-test56)
  ;(k-test56b)
  ;(k-test57)	;floating point
  (k-test58) (k-test59)
  (k-test98 fast)
  ;(k-test99 <slot>)
  (if (zerop k-diag-error-count)
      (format t "~%**** No errors in diagnostics ****~%")
    (format t "~%**** There were ~d errors in the diagnostics! ****~%" k-diag-error-count)))

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
      (format t "   *** Error ~A --- Expected #x~X   got #x~X~%"
	      info expect got)
      (format t "   *** Error ~A at address #x~X --- Expected #x~X   got #x~X~%"
	      info addr expect got)))

(defun k-mode-test (&aux temp)
  "NUBUS Mode register test"
  (format t "Starting TEST-MODE - NUBUS Mode register~%")
  (dotimes (i 2.)		;new reset pal uses former boot vector to stretch reset pulse.  was 5
    (k-write-mode (ash 1 (1+ i)))
    (when (not (= (setq temp (logand #x6 (k-read-mode))) (ash 1 (1+ i))))
      (k-diag-error "TEST-MODE Nubus mode register" nil (ash 1 (1+ i)) temp))))

(defun k-test0 (&aux temp)
  "Shifting bit pattern test on the PC."
  (format t "Starting Test 0 - PC~%")
  (k-init)
  (setq temp (k-read-spy-pc))
  (if (not (equal *cache-data-selector* temp))
      (k-diag-error "TEST-0 - Can't init PC" nil *cache-data-selector* temp)
      (dotimes (i 24.)
	(k-execute KIH-JUMP (ash 1. i))		; set PC
	(setq temp (k-read-spy-pc))
	(when (not (equal (ash 1. i) temp))
	  (k-diag-error "TEST-0 PC failure" nil (ash 1. i) temp)))))

(defun j0 () (do-forever(k-execute4 kih-jump *cache-data-selector*)))
(defun j1 () (k-execute kih-jump *cache-data-selector*))
(defun j2 (adr) (k-execute kih-jump adr))
(defun j3 (adr) (k-execute3 kih-jump adr))
(defun j4 (adr) (k-execute4 kih-jump adr))

(defun k-write-ir-bit (bit-no bit)
  (cond ((<= bit-no 31.)
	 (k-execute 0 (ash bit bit-no)))
	(t (k-execute (ash bit bit-no) 0))))



(defsubst fast-debug-write-word (addr data)
  (write-debug-data data)
  (write-debug-addr addr)
  (write-debug-control #x09)
  (wait-for-debug-response))


(defsubst fast-k-spy-i0 (n)
  (fast-debug-write-word k-spy0-addr n))
(defsubst fast-k-spy-i1 (n)
  (fast-debug-write-word k-spy1-addr n))
(defsubst fast-k-spy-cmd (n)			;write spy pal, using commands below.
  (fast-debug-write-word k-spyc-addr n))


(defun fast-instr-alternate ()
  (fast-instr-reg-test kih-nop 0 #xffffffff))

(defun fast-instr-reg-test (instr-h instr-l1 instr-l2)
  (k-init)
  (do-forever
    (fast-k-spy-cmd $$spy-command-stop)		; ensure that the run bit is off
    (fast-k-spy-cmd $$spy-command-set-spy-mode)	; set spy mode
    (fast-k-spy-i0 instr-l1)				; low half instruction
    (fast-k-spy-i1 instr-h)				; high half instruction
    (fast-k-spy-cmd $$spy-command-reload-instruction)	; force icache load
    (fast-k-spy-cmd $$spy-command-step)		; spy step command
    
    (fast-k-spy-cmd $$spy-command-stop)		; ensure that the run bit is off
    (fast-k-spy-cmd $$spy-command-set-spy-mode)	; set spy mode
    (fast-k-spy-i0 instr-l2)				; low half instruction
    (fast-k-spy-i1 instr-h)				; high half instruction
    (fast-k-spy-cmd $$spy-command-reload-instruction)	; force icache load
    (fast-k-spy-cmd $$spy-command-step))		; spy step command
  )

(defun k-test1 (&aux temp)
 ; "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 - Simple data path.~%")
  (k-init)
  (k-execute KIH-NOP 1.)
  (k-execute KIH-NOP 2.)
  (dotimes (i 32.)
    (k-execute KIH-NOP (ash 1. (logand 31. (+ i 2.))))
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-1 Data Path" nil (ash 1. i) temp))))



(defun k-test1-static (&aux temp)
  "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 - Simple data path.~%")
  (k-init)
  (k-execute KIH-NOP 1.)
  (k-execute KIH-NOP 2.)
  (k-execute kih-nop 3.)
  (setq temp (k-read-spy-mmfio))
  (when (not (= 1 temp))
    (k-diag-error "TEST-1 Data Path" nil 1 temp)))

(defun k-test1-loop ()
 ; "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 - Simple data path.~%")
  (k-init)
  (k-execute KIH-NOP 1.)
  (k-execute KIH-NOP 2.)
  (do-forever
    (k-execute KIH-NOP -1)))

(defun k-test1-piped (&aux temp)
 ; "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 - Simple data path.~%")
  (k-init)
  (let ((i0 1)
	(i1 23.)
	(i2 30.)
	(pipe (list -1 -1 -1)))
    (do-forever
   s0 (k-execute KIH-NOP (ash 1. (logand 31. i0)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i0)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (nth 2 pipe) temp)
	     (format t "~%OK ~x" temp))
	    (t
	     (format t "~%Got ~x, pipe ~x" temp pipe)
	     (cond (t nil)
		   ((= (ash 1 i1) temp)
		    (format t "~%Failed to load IR")
		    (go s2))
		   ((= (ash 1 i2) temp)
		    (format t "~%Failed to load IR")
		    (go s0))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
   s1 (k-execute KIH-NOP (ash 1. (logand 31. i1)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i1)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (nth 2 pipe) temp)
	     (format t "~%OK ~x" temp))
	    (t
	     (format t "~%Got ~x, pipe ~x" temp pipe)
	     (cond (t nil)
		   ((= (ash 1 i0) temp)
		    (format t "~%Failed to load IR")
		    (go s1))
		   ((= (ash 1 i2) temp)
		    (format t "~%Failed to load IR")
		    (go s0))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
   s2 (k-execute KIH-NOP (ash 1. (logand 31. i2)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i2)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (nth 2 pipe) temp)
	     (format t "~%OK ~x" temp))
	    (t
	     (format t "~% Got ~x, pipe ~x" temp pipe)
	     (cond (t nil)
		   ((= (ash 1 i0) temp)
		    (format t "~%Failed to load IR")
		    (go s1))
		   ((= (ash 1 i1) temp)
		    (format t "~%Failed to load IR")
		    (go s2))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
      )))
    

(defun k-test1-alt (&aux temp)
 ; "Simple shifting data pattern. IR ->1 MFI -> ALU -> MFO -> MMFIO."
  (format t "Starting Test 1 alternate.~%")
  (k-init)
  (let ((i0 1)
	(i1 23.)
	(i2 30.)
	(pipe nil))
    (do-forever
   s0 (k-execute KIH-NOP (ash 1. (logand 31. i0)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i0)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (ash 1. i0) temp)
	     (format t "~%OK"))
	    (t
	     (format t "~%should have Got ~s, pipe ~S" temp pipe)
	     (cond ((= (ash 1 i1) temp)
		    (format t "~%Failed to load IR")
		    (go s2))
		   ((= (ash 1 i2) temp)
		    (format t "~%Failed to load IR")
		    (go s0))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
   s1 (k-execute KIH-NOP (ash 1. (logand 31. i1)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i1)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (ash 1. i1) temp)
	     (format t "~%OK"))
	    (t
	     (cond ((= (ash 1 i0) temp)
		    (format t "~%Failed to load IR")
		    (go s1))
		   ((= (ash 1 i2) temp)
		    (format t "~%Failed to load IR")
		    (go s0))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
   s2 (k-execute KIH-NOP (ash 1. (logand 31. i2)))
      (if (cddr pipe) (rplacd (cddr pipe) nil))
      (push  (ash 1. (logand 31. i2)) pipe)
      (setq temp (k-read-spy-mmfio))
      (cond ((= (ash 1. i2) temp)
	     (format t "~%OK"))
	    (t
	     (cond ((= (ash 1 i0) temp)
		    (format t "~%Failed to load IR")
		    (go s1))
		   ((= (ash 1 i1) temp)
		    (format t "~%Failed to load IR")
		    (go s2))
		   (t
		    (k-diag-error "TEST-1-alt got" nil nil temp)))
	     ))
      )))

(defun k-test2 (&aux temp)
  "Simple G0 left//right register test."
  (format t "Starting Test 2 - G0 left//right register test.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute2 KIH-LOAD-G0 (ash 1. i))		; write G0
    (k-execute2 KIH-NOP 0.)			; avoid passaround
    (k-execute KIH-ALU-NOP KIL-READL-G0)	; read G0 left
    (k-execute KIH-ALU-NOP KIL-READR-G0)	; read G0 right
    (k-execute KIH-NOP 0.)			; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-2 G0 Left " nil (ash 1. i) temp))
    (k-execute KIH-JUMP *cache-data-selector*)			; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-2 G0 Right" nil (ash 1. i) temp))))


(defun k-test2a (&aux temp)
  "Simple G0+G0 add test."
  (format t "Starting Test 2A - G0+G0 add test.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute2 KIH-LOAD-G0 (ash 1. i))		; write G0
    (k-execute2 KIH-NOP 0.)			; avoid passaround
    (k-execute3 KIH-ALU-G0 KIL-ADD-G0-G0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= (LOGAND #XFFFFFFFF (ash 1. (1+ i))) temp))
      (k-diag-error "TEST-2 G0 Left " nil (LOGAND #XFFFFFFFF (ash 1. (1+ i))) temp))))



(defun k-test3 (&aux temp)
  "Simple G0 left//right passaround test."
  (format t "Starting Test 3 - G0 left//right passaround.~%")
  (k-init)
  (dotimes (i 32.)
    (k-execute KIH-LOAD-G0 (ash 1. i))		; write G0
    (k-execute KIH-ALU-NOP KIL-READL-G0)	; read G0 left
    (k-execute KIH-JUMP *cache-data-selector*)			; wait for pipeline
    (k-execute KIH-NOP 0.)			; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-3 Passaround Left " nil (ash 1. i) temp)))
  (dotimes (i 32.)
    (k-execute KIH-LOAD-G0 (ash 1. i))		; write G0
    (k-execute KIH-ALU-NOP KIL-READR-G0)	; read G0 right
    (k-execute KIH-JUMP *cache-data-selector*)			; wait for pipeline
    (k-execute KIH-NOP 0.)			; wait for pipeline
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-3 Passaround Right" nil (ash 1. i) temp))))


(defun k-test4 (&aux temp)
  "PC incrementer test."
  (format t "Starting Test 4 - PC Incrementer.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute KIH-JUMP (sub1 (ash 1. i)))	; PC = (2 ** i) - 1
    (k-execute KIH-NOP 0.)			; inc PC
    (setq temp (logand #x0ffffff (k-read-spy-pc)))
      (when (not (equal (ash 1. i) temp))
         (k-diag-error "TEST-4 PC Incrementer" nil (ash 1. i) temp))))

(defun k-test4a (&aux temp)
  "PC incrementer test loop."
  (format t "Starting Test 4a - PC Incrementer Infinite Loop.~%")
  (k-init)
  (do-forever
    (k-execute KIH-JUMP 0.)	; PC = 0 (insert number of your choice)
    (k-execute KIH-NOP 0.)))			; inc PC
 

(defun k-test5 (&aux temp)
  "PC Dispatch test."
  (format t "Starting Test 5 - PC Dispatch.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute KIH-NOP (ash 1. i))		; shifted bit
    (k-execute KIH-NOP 0.)			; pipeline
    (k-execute KIH-DISPATCH 0.)			; load PC
    (setq temp (k-read-spy-pc))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-5 PC Dispatch" nil (ash 1. i) temp))
    (k-execute KIH-JUMP *cache-data-selector*))
  (k-execute KIH-NOP #x0FFFFFF)			; pattern
  (k-execute KIH-NOP 0.)			; pipeline
  (k-execute KIH-DISPATCH-X16 0.)		; load PC w/low 4 bits masked
  (setq temp (k-read-spy-pc))
  (when (not (equal #x0FFFFF0 temp))
    (k-diag-error "TEST-5 PC Dispatch X16" nil #x0FFFFF0 temp)))


(defun w (reg n)
  (k-execute3 KIH-JUMP *cache-data-selector*)
  (k-execute (dpb reg (byte 4. 9.) KIH-LOAD-G0) n)
  (k-execute3 KIH-NOP 0))

(defun r (reg &optional (gframe 0))
  (k-execute4 (dpb gframe (byte 4. 5.) KIH-ALU-NOP) (dpb reg (byte 4. 25.) KIL-READR-G0))
  (k-read-spy-mmfio))


(defun k-test6 (&optional (fast nil) &aux temp)
  "Simple test G0 - G255"
  (format t "Starting Test 6 - Simple G0 - G255.~%")
  (k-init)
  (dotimes (k 16.)				;16 global-frames
    (dotimes (j (if fast 1. 32.))		; 32 patterns
      (k-execute KIH-JUMP *cache-data-selector*)		; reset PC
      (dotimes (i 16.)				; G0 - G15
	(k-execute (dpb i (byte 4. 9.)
			(dpb k (byte 4. 5.) KIH-LOAD-G0))	; load reg
		   (aref bit-pattern (logand 31. (+ i j)))))
      (k-execute KIH-NOP 0)
      (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 0. (byte 4. 25.) KIL-READR-G0))	; read G0
      (k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb 1. (byte 4. 25.) KIL-READR-G0))	; read G1
      (dotimes (i 16.)				; read the rest, and compare values
	(k-execute (dpb k (byte 4. 5.) KIH-ALU-NOP) (dpb (+ i 2) (byte 4. 25.) KIL-READR-G0))
	(setq temp (k-read-spy-mmfio))
	(when (not (equal (aref bit-pattern (logand 31. (+ i j))) temp))
	  (k-diag-error "TEST-6 Global Regs"
			i (aref bit-pattern (logand 31. (+ i j))) temp))))))

(defun k-test6a (&aux temp expect)
  "Test ALU SHIFT and MASK fields"
  (format t "Starting Test 6A - ALU Shift and Mask.~%")
  (k-init)
  (k-execute kih-load-g0 #xffffffff)
  (k-execute kih-load-g1 #x0)
  (k-execute3 kih-jump *cache-data-selector*)
  (dotimes (i 32.)
    (k-execute3 kih-alu-nop (logior kil-field-pass-g0-g1 (ash i 5.) 1))
    (setq temp (k-read-spy-mmfio))
    (setq expect (ash 1 i))
    (when (not (= temp expect))
      (k-diag-error "TEST-6A ALU shift" nil expect temp)))
  (dotimes (i 32.)
    (k-execute3 kih-alu-nop (logior kil-field-pass-g0-g1 i))
    (setq temp (k-read-spy-mmfio))
    (setq expect (if (zerop i) #xffffffff (ash #x0ffffffff (+ i -32))))
    (when (not (= temp expect))
      (k-diag-error "TEST-6A ALU mask" nil expect temp))))


(defun k-test7 (&aux temp)
  "Open, Active, Return (OAR) register test."
  (format t "Starting Test 7 - Open, Active, Return (OAR) register.~%")
  (k-init)
  (dotimes (i 24.)
    (k-execute  KIH-LOAD-OAR (ash 1. i))
    (k-execute3 KIH-JUMP *cache-data-selector*)
    (k-execute  KIH-ALU-NOP KIL-READ-OAR)
    (k-execute2 KIH-NOP 0.)
    (setq temp (logand #x0FFFFFF (k-read-spy-mmfio)))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-7 OAR" nil (ash 1. i) temp))))


(defun k-test8 (&optional (fast nil) &aux temp)
  "Register RAM data test."	;tests data in all 16 registers of (16 fast, 256 otherwise) frames.
  (format t "Starting Test 8 - Register RAM data.~%")
  (k-init)
  (dotimes (pat (if fast 1. 32.))
    (if (not fast) (format t "   Pass ~A~%" pat))
    (dotimes (frame (if fast 16. 256.))
      (k-execute KIH-LOAD-OAR (ash frame 8.))		;Active = frame
      (k-execute3 KIH-JUMP *cache-data-selector*)			;reset PC
      (dotimes (i 16.)
	(k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)	;load regs
		   (aref bit-pattern (logand 31. (+ i pat))))))
    (dotimes (frame (if fast 16. 256.))
      (k-execute KIH-LOAD-OAR (ash frame 8.))		;Active = frame
      (k-execute3 KIH-JUMP *cache-data-selector*)			;reset PC
;      (k-execute KIH-ALU-NOP (dpb 0 (byte 4. 25.) KIL-READ-A0))
;      (k-execute KIH-ALU-NOP (dpb 1 (byte 4. 25.) KIL-READ-A0))
      (dotimes (i 16.)
	(k-execute3 KIH-ALU-NOP				;read regs
		   (dpb i  (byte 4. 25.) KIL-READ-A0))
	(setq temp (k-read-spy-mmfio))
	(when (not (equal (aref bit-pattern (logand 31. (+ i pat))) temp))
	  (k-diag-error "TEST-8 Register data" (+ (ash frame 4.) i)
			(aref bit-pattern (logand 31. (+ i pat))) temp))))))


(defun k-test9 (&optional (fast nil) &aux temp)
  "Register RAM address test."
  (format t "Starting Test 9 - Register RAM address.~%")
  (k-init)
  (dotimes (pat (if fast 1. 32.))
     (if (not fast) (format t "  Pass ~D~%" pat))
     (dotimes (frame (if fast 16. 256.))
      (k-execute KIH-LOAD-OAR (ash frame 8.))		;Active = frame
      (k-execute3 KIH-JUMP *cache-data-selector*)			;reset PC
      (dotimes (i 16.)
	(k-execute (dpb i (byte 4. 9.) KIH-LOAD-A0)	;load regs
		   (+ (ash frame 4.) i))))
    (dotimes (frame (if fast 16. 256.))
      (k-execute KIH-LOAD-OAR (ash frame 8.))	;Active = frame
      (k-execute3 KIH-JUMP *cache-data-selector*)			;reset PC
      (k-execute KIH-ALU-NOP (dpb 0  (byte 4. 25.) KIL-READ-A0))
      (k-execute KIH-ALU-NOP (dpb 1 (byte 4. 25.) KIL-READ-A0))
      (dotimes (i 16.)
	(k-execute KIH-ALU-NOP				;load regs
		   (dpb (+ i 2) (byte 4. 25.) KIL-READ-A0))
	(setq temp (k-read-spy-mmfio))
	(when (not (equal (+ (ash frame 4.) i) temp))
	  (k-diag-error "TEST-9 Register address" (+ (ash frame 4.) i)
			(+ (ash frame 4.) i) temp))))))

(defun k-test10-jtest (left right jcond jump msg &aux temp)
  (setq temp (dpb right (byte 4. 25.) KIL-LR-SUB))
  (setq temp (dpb left (byte 4. 19.) temp))
  (k-execute KIH-ALU-NOP temp)                     ; (- left right)
  (k-execute (dpb jcond (byte 3. 2.) KIH-JUMP) *cache-data-selector*)   ; Jump 100, sel jcond
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

(defun k-test10b ()
  "Delayed Jump bit test"
  (format t "Starting test 10b - Delayed Jump bit.~%")
  (k-init)
  (k-execute3 KIH-ALU-NOP KIL-READ-PSTAT)
  (let ((result (ldb (byte 2. 16.) (k-read-spy-mmfio))))
    (when (not (= result 3.)) (k-diag-error "Test-10b DJUMP" nil 3. result)))
  (dotimes (i 4.)
    (k-execute3 (dpb 2. (byte 3. 2.) kih-load-g0) 1)	;jcond = zerop
    (dotimes (j i)
      (k-execute (dpb 2. (byte 3. 2.) kih-load-g0) 0))
    (k-execute3 (dpb 2. (byte 3. 2.) kih-alu-nop) kil-read-pstat)
    (let ((result (ldb (byte 2. 16.) (k-read-spy-mmfio)))
	  (expect (case i
		    (0 3) ;don't jump
		    (1 3) ;don't jump
		    (2 2) ;jump, delayed off
		    (3 0) ;don't jump, delayed on
		    )))
      (when (not (= result expect)) (k-diag-error "Test-10b DJUMP" nil expect result)))))


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
  (format t "Starting Test 11B - Return PC RPC reg.~%")
  (k-init)
  (k-execute3 KIH-LOAD-CSP 0.)                    ; Init call stack pointer
  (dotimes (i 7.)
    (k-execute KIH-JUMP *cache-data-selector*) ; Set PC
    (k-execute (dpb (ldb (byte 1. 6.) (ash 1. i)) (byte 1. 29.)
		    (dpb (ldb (byte 5. 1.) (ash 1. i)) (byte 5. 0.)
			 KIH-OPEN-CALL))
	       (dpb (ash 1. i) (byte 1. 24.) #xFFFFFC))          ; Save PC in RPC
    (k-execute2 kih-jump *CACHE-DATA-SELECTOR*)
    (k-execute KIH-alu-nop kil-read-rpc)
    (k-execute2 kih-jump *cache-data-selector*)
    (setq temp (logand (k-read-spy-mmfio) #xFF000000))
    (when (not (equal temp (ash 1. (+ i 24.))))
      (k-diag-error "TEST-11B RPC" nil (ash 1. (+ i 24.)) temp))))

(defun k-test12 (&aux temp)
  "Call Stack Pointer & Heap Pointer test."
  (format t "Starting Test 12 - Call Stack Pointer & Heap Pointer.~%")
  (k-init)
  (dotimes (i 16.)
    (k-execute  KIH-LOAD-CSP (ash 1. i))
    (k-execute3 KIH-JUMP *cache-data-selector*)
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
    (k-execute3 KIH-JUMP *cache-data-selector*)
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
    (k-execute3 KIH-JUMP *cache-data-selector*)
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
	(k-execute3 KIH-JUMP *cache-data-selector*)
	(k-execute3 KIH-NOP 0)
	(k-execute KIH-LOAD-RPC n)
	(k-execute KIH-NOP 0.)
	(k-execute KIH-CALL *cache-data-selector*)
	(k-execute3 KIH-NOP 0))

(defun rs (adr)
       	(k-execute3 KIH-LOAD-CSP adr)             ;CSP = 0
	(k-execute KIH-LOAD-PCTL #x20000)
	(k-execute3 KIH-JUMP *cache-data-selector*)
	(k-execute3 KIH-NOP 0)
	(k-execute3 KIH-ALU-NOP KIL-READ-RPC)
	(k-execute KIH-XRETURN 0.)
	(k-execute KIH-JUMP *cache-data-selector*)
	(k-read-spy-mmfio))


(defun k-test16 (&optional (fast nil) &aux temp)
  "Call Stack data - PC & RDEST"
  (format t "Starting Test 16 - Call Stack data - PC & RDEST.~%")
  (k-init)
  (dotimes (pat (if fast 1. 32.))
    (if (not fast) (format t "  Pass ~D~%" pat))
    (dotimes (sgn (if fast 1. 16.))
      (k-execute3 KIH-LOAD-CSP 0)		;CSP = 0
      (k-execute KIH-LOAD-PCTL (ash sgn 13.))	;Stack Group
      (k-execute3 KIH-NOP 0)
      (dotimes (i 256.)
	(k-execute KIH-LOAD-RPC (ash 1. (logand 31. (+ i pat))))
	(k-execute KIH-NOP 0.)
	(k-execute KIH-OPEN-CALL *cache-data-selector*)))
    (dotimes (sgn (if fast 1. 16.))
      (k-execute3 KIH-LOAD-CSP 0)		;CSP = 0
      (k-execute KIH-LOAD-PCTL (ash sgn 13.))	;Stack Group
      (k-execute3 KIH-NOP 0)
      (do ((i 255. (sub1 i)))
	  ((minusp i))
	(k-execute KIH-ALU-NOP KIL-READ-RPC)
	(k-execute KIH-XRETURN 0.)
	(k-execute KIH-JUMP *cache-data-selector*)
	(setq temp (k-read-spy-mmfio))
	(when (not (equal temp (ash 1. (logand 31. (+ i pat)))))
	  (k-diag-error "TEST-16 Call Stack RPC data"
			(+ (ash sgn 8.) i)
			(ash 1. (logand 31. (+ i pat))) temp))))))


(defun k-test17 (&optional fast &aux temp)
  "Call Stack address - PC & RDEST"
  (format t "Starting Test 17 - Call Stack address - PC & RDEST.~%")
  (k-init)
  (dotimes (sgn (if fast 1. 16.))
    (k-execute KIH-LOAD-PCTL (ash sgn 13.))	;Stack Group
    (k-execute KIH-LOAD-CSP #xffff)			;CSP = 0
    (k-execute3 KIH-NOP 0)
    (dotimes (i 256.)
      (k-execute KIH-LOAD-RPC (+ (ash sgn 8.) i))
      (k-execute KIH-NOP 0.)
      (k-execute KIH-OPEN-CALL *cache-data-selector*)))
  (dotimes (sgn (if fast 1. 16.))
    (k-execute KIH-LOAD-PCTL (ash sgn 13.))	;Stack Group
    (k-execute KIH-LOAD-CSP #xffff)			;CSP = 0
    (k-execute3 KIH-NOP 0)
    (do ((i 255. (sub1 i)))
	((minusp i))
      (k-execute KIH-ALU-NOP KIL-READ-RPC)
      (k-execute KIH-XRETURN 0.)
      (k-execute KIH-JUMP *cache-data-selector*)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp (+ (ash sgn 8.) i)))
	(k-diag-error "TEST-17 Call Stack RPC address"
		      (+ (ash sgn 8.) i)
		      (+ (ash sgn 8.) i) temp)))))

(defun k-test18 (&optional (fast nil) &aux temp)
  "Call Stack data - open & active"
  (format t "Starting Test 18 - Call Stack data - open & active.~%")
  (k-init)
  (dotimes (pat (if fast 1. 16.))
     (if (not fast) (format t "  Pass ~D~%" pat))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)
	(dotimes (i 256.)
	   (k-execute  KIH-LOAD-OAR (ash 1. (+ 8. (logand 15. (+ pat i)))))
	   (k-execute3 KIH-NOP 0.)
	   (k-execute  KIH-OPEN-CALL *cache-data-selector*)))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)
	(do ((i 255. (sub1 i)))
	    ((minusp i))
	   (k-execute  KIH-XRETURN 0.)
	   (k-execute2  KIH-ALU-NOP KIL-READ-OAR)
	   (k-execute2 KIH-JUMP *cache-data-selector*)
	   (setq temp (logand #xFFFF (ash (k-read-spy-mmfio) -8.)))
	   (when (not (equal temp (ash 1. (logand 15. (+ pat i)))))
		 (k-diag-error "TEST-18 Call Stack OA data"
			       (+ (ash sgn 8.) i)
			       (ash 1. (+ 8. (logand 15. (+ pat i)))) temp))))))


(defun k-test19 (&optional (fast nil) &aux temp)
  "Heap data"
  (format t "Starting Test 19 - Heap Data~%")
  (k-init)
  (dotimes (pat (if fast 1. 8.))
     (if (not fast) (format t "  Pass ~D~%" pat))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)
	(dotimes (i 256.)
	   (k-execute  KIH-LOAD-OAR (ash 1. (logand 7. (+ i pat))))
	   (k-execute3 KIH-JUMP *cache-data-selector*)
	   (k-execute  KIH-XRETURN 0.)))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)
	(do ((i 255. (sub1 i)))
	    ((minusp i))
	  (k-execute  KIH-OPEN-CALL *cache-data-selector*)
	  (k-execute2  KIH-ALU-NOP KIL-READ-OAR)
	  (k-execute2 KIH-JUMP *cache-data-selector*)
	  (setq temp (logand #xFF (ash (k-read-spy-mmfio) -16.)))
	  (when (not (equal temp (ash 1. (logand 7. (+ i pat)))))
	    (k-diag-error "TEST-19 Heap data"
			  (+ (ash sgn 8.) i)
			  (ash 1. (logand 7. (+ i pat))) temp))))))

(defun k-test20 (&optional (fast nil) &aux temp)
  "Call Stack data - RIMM"
  (format t "Starting Test 20 - Call Stack data - RIMM.~%")
  (k-init)
  (dotimes (pat (if fast 1. 4.))
     (if (not fast) (format t "  Pass ~D~%" pat))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)	
	(dotimes (i 256.)
	   (k-execute KIH-LOAD-PCTL
	      (logior #x20000 (ash sgn 13.)
		      (ash (ash 1. (logand 3. (+ i pat))) 9.)))
	   (k-execute KIH-LOAD-RPC 0)
	   (k-execute KIH-NOP 0.)
	   (k-execute KIH-OPEN-CALL *cache-data-selector*)))
     (dotimes (sgn (if fast 1. 16.))
	(k-execute KIH-LOAD-PCTL (ash sgn 13.)) ;Stack Group
	(k-execute KIH-LOAD-CSP 0.)             ;CSP = 0
	(k-execute3 KIH-NOP 0)	
	(do ((i 255. (sub1 i)))
	    ((minusp i))
	  (k-execute KIH-ALU-NOP KIL-READ-PSTAT)
	  (k-execute KIH-XRETURN 0.)
	  (k-execute KIH-JUMP *cache-data-selector*)
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
    (k-execute3 KIH-JUMP *cache-data-selector*)
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
    (k-execute3 KIH-JUMP *cache-data-selector*)
    (k-execute3  KIH-ALU-NOP KIL-READ-MD)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal (ash 1. i) temp))
      (k-diag-error "TEST-22 MD" nil (ash 1. i) temp))))



(defun k-test22a (&aux temp)			;for debugging reoccuring problem with
  "MD test bits 16, 17, 18."			; bits 16 and 17 of memory data register
  (format t "~%Starting Test 22a - MD Reg <-- #x70000~%...hit any key to stop (running)...")
  (k-init)					; helpful in generating signals to follow
  (do-forever					; when k-test22 fails...
    (k-execute  KIH-LOAD-MD-SW  #x70000)
    (k-execute3 KIH-JUMP *cache-data-selector*)
    (k-execute3  KIH-ALU-NOP KIL-READ-MD)
    (setq temp (k-read-spy-mmfio))
    (when (read-char-no-hang)			;hit any key to stop
      (format t "~%Value read was #x~o~%" temp)
      (return))
    )
  (values)
  )

(defun k-test22b (&aux temp)			        ; for debugging  problem with the low
  "MD exercise bits 0 thru 3 in a loop"	       	        ; bits of memory data register
  (format t "~%Starting Test 22b - MD Reg <-- #x1~%...hit any key to stop (running)...")
  (k-init)
  (do-forever
    (dotimes (i 4.)
      (k-execute  KIH-LOAD-MD-SW  (ash 1. i))
      (k-execute3 KIH-JUMP *cache-data-selector*)
      (k-execute3  KIH-ALU-NOP KIL-READ-MD)
      (setq temp (k-read-spy-mmfio))
      (when (read-char-no-hang)			;hit any key to stop
	(format t "~%Value read was #x~o~%" temp)
      (return))
    ))
  (values)
  )

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

(defun spy-write-map (adr data)
  (k-execute KIH-LOAD-VMA adr)
  (k-execute3 KIH-JUMP *cache-data-selector*)
  (k-execute2 KIH-LOAD-MAP data)
  (k-execute3 KIH-JUMP *cache-data-selector*))

(defun spy-read-map (adr)
  (k-execute2 KIH-LOAD-VMA adr)
  (k-execute4 KIH-JUMP *cache-data-selector*)
  (k-execute4 KIH-ALU-NOP KIL-READ-MAP)
  (k-read-spy-mmfio))

(defun dump-map (adr &optional (n 8.))
  (dotimes (i n)
    (format t "~%VMA = ~X        MAP = ~X" adr (spy-read-map adr))
    (setq adr (+ #x400 adr))))

(defun k-test24 (&optional (fast nil) &aux temp)
  "Map data Test."
  (format t "Starting Test 24 - MAP data RAM.~%")
  (k-init)
  (dotimes (pat (if fast 1. 32.))
    (if (not fast) (format t "~%Pass ~D       Writing." pat))
    (dotimes (adr (if fast #x400 #X10000))
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute2 KIH-LOAD-MAP (ash 1 (logand 31. (+ adr pat))))
      (k-execute KIH-JUMP *CACHE-DATA-SELECTOR*))
    (if (not fast) (format t "      Reading."))
    (dotimes (adr (if fast #x400 #X10000))
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute3 KIH-JUMP *CACHE-DATA-SELECTOR*)
      (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal (ash 1 (logand 31. (+ adr pat))) temp))
	(k-diag-error "TEST-24 MAP Data " adr (ash 1 (logand 31. (+ adr pat))) temp)))))

(defun k-test24b (&optional (fast nil) &aux temp)
  "Map address Test."
  (format t "Starting Test 24B - MAP address RAM.~%")
  (k-init)
    (if (not fast) (format t "      Writing.~%"))
    (dotimes (adr (if fast #x400  #X10000))
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute2 KIH-LOAD-MAP adr)
      (k-execute KIH-JUMP *CACHE-DATA-SELECTOR*))
    (if (not fast) (format t "      Reading.~%"))
    (dotimes (adr (if fast #x400 #X10000))
      (k-execute KIH-LOAD-VMA (ash adr 10.))
      (k-execute KIH-JUMP *CACHE-DATA-SELECTOR*)
      (k-execute3 KIH-ALU-NOP KIL-READ-MAP)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp adr))
	(k-diag-error "TEST-24 MAP Address" adr adr temp))))
	

(defun k-test25 (&aux temp)
  "TEST-25 - Subroutine call test"
  (format t "Starting Test 25 - Subroutine call test~%")
  (k-init)
  (falcon-initialize-call-hardware)
  (dotimes (i 16.)
    (k-execute (dpb i (byte 4. 9.) kih-load-g0) i))
  (k-execute kih-jump *cache-data-selector*)
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
  (format t "Starting Test 26 - Call hardware functions~%")
  (k-test26-qchi)
  (k-test26-ch-test "nop" #x00ff #x112233)
  (k-test26-qchi)
  (k-execute kih-open *cache-data-selector*)
  (k-test26-ch-test "open" #xff00 #x442233)
  (k-test26-qchi)
  (k-execute kih-call *cache-data-selector*)
  (k-test26-ch-test "call" #x00ff #x111133)
  (k-test26-qchi)
  (k-execute kih-open-call-x *cache-data-selector*)
  (k-test26-ch-test "open-call" #xff00 #x444433)
  (k-test26-qchi)
  (k-execute kih-open-call-x *cache-data-selector*)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return" #x00ff #x112244)
  (k-test26-qchi)
  (k-execute (logior #x20000000 kih-open-call-x) *cache-data-selector*)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return-open" #xff00 #x332244)
  (k-test26-qchi)
  (k-execute (logior #x20000010 kih-open-call-x) *cache-data-selector*)
  (k-execute kih-return-fd 0)
  (k-test26-ch-test "return-topen" #xffff #x332244)
  (k-test26-qchi)
  (k-execute kih-topen *cache-data-selector*)
  (k-test26-ch-test "topen" #xffff #x442233)
  (k-test26-qchi)
  (k-execute kih-tcall *cache-data-selector*)
  (k-test26-ch-test "tcall" #x01ff #x111122)
  (k-test26-qchi)
  (k-execute kih-topen-tcall *cache-data-selector*)
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
  (format t "Starting Test 27 - Microsecond clock~%")
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
  (format t "Starting Test 28 - Statistics Counter~%")
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

(defun k-test29 ()
  "TEST-29 Trap register"
  (k-reset)
  (k-init)
  (k-execute kih-load-vma 0)
  (k-execute kih-load-map #x8f)		;on board memory, all RW
  (k-execute3 kih-jump *cache-data-selector*)
  (k-execute kih-load-md 0)
  (k-execute kih-load-vma-sw 0)
  (k-execute kih-jump *cache-data-selector*)
  (k-execute kih-load-vma-sr 0)
  (k-execute kih-jump *cache-data-selector*)
  (k-execute3 kih-alu-nop kil-read-md)		
  (format t "Starting Test 29 - Trap register~%")
  (k-execute kih-jump *cache-data-selector*)
  (k-execute kih-load-mctl #x6c000000) ;overflow trap enable, datatype trap enable, sync-trap enable, single-step on trap exit
  (k-execute kih-load-pctl 0)
  (k-execute kih-load-csp #x1111)
  (k-execute kih-load-g0 0)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-test29-tchk #x80000001 #x8000dfff)			;reset

  (k-execute kih-load-mctl #x6d000000)			;no reset
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test29-tchk #x00000001 #x8000dfff)

  (k-execute kih-load-mctl #x6d001000)			;interval timer 1024
  (k-execute4 kih-nop 0)
  (k-test29-tchk #x00000005 #x8000dfff)

  (k-execute kih-load-mctl #x6d002000)			;interval timer 16384
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test29-tchk #x00000003 #x8000dfff)

  (k-execute kih-load-mctl #x6d000000)			;nubus interrupts
  (k-execute4 kih-nop 0)
  (dotimes (i 8.)
    (k-write-int i 1.)				
    (k-test29-tchk (1+ (ash 1. (+ i 3.))) #x8000dfff)
    (k-write-int i 0.))

  (k-execute kih-load-csp #x0011)			;stack overflow
  (k-execute kih-load-pctl #x0040000)
  (k-execute4 kih-nop 0)
  (k-test29-tchk #x00000001 #x8000dfff)
  (k-execute kih-open-call *cache-data-selector*)
  (k-test29-tchk #x00008001 #x8000dfff)
  (k-execute kih-load-pctl #x0000000)
  (k-reset)
  (k-init))

(defun k-test29-tchk (expect mask &aux temp)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-alu-transp kil-add-g0-g0)	;write transp from bits 7-4 of MMFIO.
  (k-execute4 kih-alu-transp kil-add-g0-g0)
  (k-execute4 kih-alu-nop kil-add-g0-g0)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-execute4 kih-alu-nop kil-read-trap)
  (setq temp (logand mask (k-read-spy-mmfio)))
  (when (not (equal expect temp))
    (k-diag-error "TEST-29 Trap register" nil expect temp)))


(defun k-test30 ()
  "TEST-30 Trap request"
  (k-init)
  (format t "Starting Test 30 - Trap register~%")
  (k-execute kih-load-g0 0)
  (k-execute kih-alu-nop kil-add-g0-g0)
  (k-execute kih-load-mctl #xC1000000)
  (k-execute4 kih-jump #x101)
  (k-test30-tchk #x101)

  (k-execute kih-load-mctl #xC1001000)			;interval timer 1024 uS
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test30-tchk #x105) ;; $$$ Changed to expect #x105 from 0. <18-Nov-88 wkf&dgs>

  (k-execute kih-load-mctl #xC1002000)			;interval timer 16384 uS
  (k-execute4 kih-nop 0)
  (process-sleep 2)
  (k-test30-tchk #x105) ;; $$$ Changed to expect #x105 from 0. <18-Nov-88 wkf&dgs>
  
  (dotimes (i 8.)
    (k-execute kih-load-mctl #xC1000000)		;nubus interrupts
    (k-execute4 kih-nop 0)
    (k-write-int i 1.)
    (k-test30-tchk #x105) ;; $$$ Changed to expect #x105 from 0. <18-Nov-88 wkf&dgs>
    (k-write-int i 0.))

  (k-reset)
  (k-init)
  )

(defun k-test30-tchk (expect &aux temp)
  (setq temp #xffffff)
  (dotimes (i 50)
    (setq temp (logand temp (k-read-spy-pc))))
  (k-execute kih-jump *cache-data-selector*)
  (when (not (= expect temp))
    (k-diag-error "TEST-30 Trap request" nil expect temp))
  (k-reset)
  (k-init)
  (k-execute4 kih-jump *cache-data-selector*)
  (when (not (= (setq temp (k-read-spy-pc)) *cache-data-selector*))
    (k-diag-error "TEST-30 Trap request" nil *cache-data-selector* temp))
  )

(defun k-test31 ()
  "Test 31 - Trap State machine"
  (format t "Starting Test 31 - Trap State machine~%")
  (k-init)
  (k-execute kih-load-oar #xffffff)
  (k-execute kih-load-mctl 0)
  (k-execute kih-load-pctl 0)
  (k-execute kih-load-g0 0)
  (k-execute kih-load-g1 1)
  (k-execute kih-load-g2 2)
  (k-execute kih-load-g3 3)
  (k-execute4 kih-jump *cache-data-selector*)
  (k-test31-step 0 3 3)
  (k-test31-step 1 3 3)
  (k-test31-step 2 2 2)
  (k-test31-step 3 2 2))
 
(defun k-test31-step (adr left right &aux temp)
  (k-execute3 kih-jump *cache-data-selector*)
  (k-execute3 kih-nop 3)
  (k-execute  #X20008000 (dpb adr (byte 12. 0.) #X63800000)) ; same but branch adr
    ;(UNCONDITIONAL-BRANCH <adr> (ALU SETL RETURN *SAVE-OREG* *SAVE-LEFT*) BOXED-LEFT DT-NONE)
  (k-execute3 #x20008000 (dpb adr (byte 12. 0.) #X65900000)) ; L=R= -1, branch adr
    ;(UNCONDITIONAL-BRANCH <adr> (ALU SETL RETURN *SAVE-RIGHT* *SAVE-RIGHT*) BOXED-LEFT DT-NONE)
  (setq temp (k-read-spy-mmfio))
  (when (not (equal left temp))
    (k-diag-error "Test 31 - TSM left" adr left temp))
  (k-execute3 #x20008000 (dpb adr (byte 12. 0.) #X65901000)) ; L=R= -1, branch adr
    ;(UNCONDITIONAL-BRANCH 123 (MOVE RETURN *SAVE-RIGHT*) BOXED-LEFT DT-NONE)
  (when (not (equal right temp))
    (k-diag-error "Test 31 - TSM right" adr right temp)))
  
(defun k-test32 (&aux temp addr)
  "TEST-32 Old PC registers"
  (k-init)
  (format t "Starting Test 32 - Old PC registers~%")
  (k-execute kih-jump *cache-data-selector*)
  (dotimes (i 24.)
    (setq addr (if (< i 8.)
		   (logior #x800000 (ash 1 i))
		 (ash 1 i)))
    (k-execute kih-jump addr)
    (k-execute kih-alu-nop kil-read-opc)
    (k-execute kih-alu-nop kil-read-opc+)
    (k-execute kih-jump #x199)
    (setq temp (logand #xffffff (k-read-spy-mmfio)))
    (when (not (equal temp addr))
      (k-diag-error "TEST-32 OPC" nil addr temp))
    (k-execute kih-jump #x199)
    (setq temp (logand #xffffff (k-read-spy-mmfio)))
    (when (not (equal temp addr))
      (k-diag-error "TEST-32 OPC+" nil addr temp)))
  (k-reset)
  (k-init))

(defun k-test33 (&aux temp)
  "TEST-33 Simple Box bits"
  (k-init)
  (format t "Starting Test 33 - Simple Box Bits~%")

  (k-execute kih-load-oar #xfffefd)
  ;the below feature seems to have been removed.  It was pretty random and does not appear to have been used.
  (cond ((not *k-memory-board-revision*)
	 (FSIGNAL "Unable to decode processor config prom"))  ;;See function check-k-prom in file new-spy-utilities. --wkf
	((zerop *k-memory-board-revision*)
	 (k-execute kih-load-pctl #x40)		;set box-mux-mode to register-reload-mode
	 (k-execute3 kih-nop 0)
	 (dotimes (i 2)
	   (k-execute2 kih-load-a0 i)
	   (k-execute3 kih-alu-a0br kil-read-pstat)
	   (when (not (= i (ldb (byte 1. 18.) (k-read-spy-mmfio))))
	     (k-diag-error "TEST-33 OREG boxed broken, reg reload mode" nil i (- 1 i))))))

  (dotimes (i 2)
    (k-execute kih-load-g0 0)
    (k-execute3 (dpb i (byte 1. 22.) kih-alu-a0bu) #x60039000) ;shift up w/link fill from G0
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp i))
      (k-diag-error "TEST-33 Macro-carry box load" nil i temp)))

  (k-execute kih-load-pctl 0)
  (k-execute3 kih-nop 0)
  (k-execute (logior #x00400000 kih-load-a0) 0) ;unboxed
  (k-execute3 kih-alu-a0br kil-read-pstat)
  (when (neq 0 (ldb (byte 1. 18.) (k-read-spy-mmfio)))
    (k-diag-error "TEST-33 Unboxed broken" nil 0 1))

  (k-execute (logior #x00c00000 kih-load-a0) 0) ;boxed
  (k-execute3 kih-alu-a0br kil-read-pstat)
  (when (neq 1 (ldb (byte 1. 18.) (k-read-spy-mmfio)))
    (k-diag-error "TEST-33 boxed broken" nil 1 0))

  (dotimes (i 16.)
    (dotimes (j 16.)
      (k-execute (dpb j (byte 4. 9.) KIH-LOAD-G0) 0))
    (k-execute (logior #x00c00000 (dpb i (byte 4. 9.) KIH-LOAD-G0)) 0)
    (dotimes (j 16.)
      (k-execute (logior #x00400000 kih-alu-a0br) (dpb j (byte 4. 25.) kil-readr-g0))
      (k-execute3 kih-alu-a0br kil-read-pstat)
      (setq temp (ldb (byte 1. 18.) (k-read-spy-mmfio)))
      (when (neq (eq j i) (equal temp 1))
	(k-diag-error "TEST-33 Simple Box bits right" j (if (eq i j) 1 0) temp))
      (k-execute kih-alu-a0bl (dpb j (byte 4. 19.) kil-readl-g0))
      (k-execute3 kih-alu-a0bl kil-read-pstat)
      (setq temp (ldb (byte 1. 18.) (k-read-spy-mmfio)))
      (when (neq (eq j i) (equal temp 1))
	(k-diag-error "TEST-33 Simple Box bits left" j (if (eq i j) 1 0) temp)))))


(defun k-set-boxed-mode (m)	;#x40 sets box-mux-mode to register-reload-mode on rev 0 proto only!
   (k-execute kih-load-pctl (dpb m (byte 1 6) 0)))
						

(defun k-test34 (&optional fast &aux temp)
  "TEST-34 Simple Datatype RAM"
  (k-init)
  (format t "Starting Test 34 - Simple Datatype RAM~%")
  (dotimes (chip 2)			;high bit of dt-ram addr selects chip. this is high bit of dt-ram check field.
    (dotimes (i (if fast 16. 64.))	;datatype field
      (k-execute kih-load-pctl 0)	;#x100 bit is data to write
      (k-execute3 kih-jump *cache-data-selector*)
      (dotimes (j (if fast 16. 64.))	;write loop datatype field, write 0's
	(k-execute (dpb chip (byte 1. 21.) kih-load-dt) (ash j 26.))
	(k-execute (dpb chip (byte 1. 21.) kih-load-g0) (ash j 26.)))
      (k-execute kih-load-pctl #x100)	;prepare to write 1
      (k-execute3 kih-jump *cache-data-selector*)
      (k-execute (dpb chip (byte 1. 21.) kih-load-dt) (ash i 26.))
      (k-execute (dpb chip (byte 1. 21.) kih-load-g0) (ash i 26.))
      (dotimes (j (if fast 16. 64.))	;check loop, read what we wrote, it should be 0 unless i=j means we overwrote a 1.
	(k-execute (dpb chip (byte 1. 21.) kih-nop) (ash j 26.))
	(k-execute3 kih-alu-nop kil-read-pstat)
	(setq temp (ldb (byte 1. 13.) (k-read-spy-mmfio)))
	(when (neq (eq i j) (equal temp 1.))
	  (k-diag-error "TEST-34 Simple datatype RAM" j (if (eq i j) 1 0) temp))))))


(defun k-test50 (&aux temp)
  "TEST-50 Proc - local memory data test loc 0"
  (k-init)
  (format t "Starting Test 50 - Proc - local memory data test loc 0~%")
  (dotimes (i 32.)
    (k-execute kih-load-md (ash 1 i))
    (k-execute kih-load-vma-sw 0)
    (k-execute kih-jump *cache-data-selector*)
    (k-execute kih-load-vma-sr 0)
    (k-execute kih-jump *cache-data-selector*)
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
      (k-execute kih-jump *cache-data-selector*))
    (dotimes (i 256.)
      (k-execute kih-load-vma-sr i)
      (k-execute kih-jump *cache-data-selector*)
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
      (k-execute kih-jump *cache-data-selector*))
    (dotimes (i 256.)
      (k-execute kih-load-vma-sr i)
      (k-execute kih-jump *cache-data-selector*)
      (k-execute3 kih-alu-nop kil-read-md)
      (setq temp (k-read-spy-mmfio))
      (when (not (equal temp (ash 1 (logand i 31.))))
	(k-diag-error "TEST-52 Proc - local mem address" i
		      (ash 1 (logand i 31.)) temp)))))

(defun k-test53 (&aux temp pat)
  "Test-53 Proc - local mem special reads"
  (k-init)
  (k-execute kih-load-mctl 0)
  (k-execute4 kih-jump *cache-data-selector*)
  (format t "Starting Test 53 - Proc - local mem special reads~%")
  (k-execute kih-load-g0 0)
  (k-execute kih-load-md #x55555555)		
  (k-execute kih-load-vma-sw 0)
  (k-execute2 kih-jump *cache-data-selector*)
  (k-execute kih-load-md #xAAAAAAAA)
  (k-execute kih-load-vma-sw 1)
  (k-execute2 kih-jump *cache-data-selector*)
  (k-execute kih-load-md 0)
  (k-execute kih-load-vma-sw 2)
  (k-execute2 kih-jump *cache-data-selector*)
  (dotimes (i 16.)
    (k-execute kih-load-vma-sr 2)
    (k-execute3 kih-jump *cache-data-selector*)
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

(defun k-test54 (&aux temp)
  "Test 54 - MD/VMA box bits"
  (format t "Starting Test 54 - MD//VMA box bits~%")
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

(defun k-write-vma-md-boxed (i)
  (k-execute (dpb i (byte 2 22.) kih-load-vma-sr) 0)
  (k-execute3 kih-nop 0)
  (k-execute kih-alu-nop kil-read-md)
  (k-execute3 kih-alu-nop kil-read-mstat)
  (logxor 3 (ldb (byte 2 15.) (k-read-spy-mmfio))))

(defun k-test55 (&aux temp)
  "Test-55 GC RAM test"
  (format t "Starting Test 55 - GC RAM~%")
  (k-init)
  (dotimes (i 16.)
     (k-execute kih-load-gc-ram i)
     (k-execute4 kih-jump *cache-data-selector*)
     (k-execute3 kih-alu-nop kil-read-gc-rams)
     (setq temp (ldb (byte 4. 0.) (k-read-spy-mmfio)))
     (when (not (equal temp i))
	   (k-diag-error "TEST-55 GC Data" 0 i temp)))
  (dotimes (i 12.)
     (k-execute kih-load-md (ash #x4000 i))
     (k-execute kih-jump *cache-data-selector*)
     (k-execute kih-load-gc-ram i)
     (k-execute kih-jump *cache-data-selector*))
  (dotimes (i 12.)
     (k-execute kih-load-md (ash #x4000 i))
     (k-execute3 kih-jump *cache-data-selector*)
     (k-execute3 kih-alu-nop kil-read-gc-rams)
     (setq temp (ldb (byte 4. 0.) (k-read-spy-mmfio)))
     (when (not (equal i temp))
	   (k-diag-error "TEST-55 GC Address" (ash #x4000 i) i temp))))

(defun k-test56 (&aux temp)
   "Test-56 Transporter RAM"
   (format t "Starting Test 56 - Transporter RAM~%")
   (k-init)
   (dotimes (i 16.)
     (k-execute kih-load-transporter (ash i 4.))
     (k-execute4 kih-jump *cache-data-selector*)
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
   (k-execute3 kih-jump *cache-data-selector*)
   (k-execute kih-load-transporter 0)
   (k-execute3 kih-jump *cache-data-selector*)
   (k-execute kih kil)
   (k-execute4 kih-jump *cache-data-selector*)
   (k-execute kih-load-transporter (ash #x0F 4.))
   (k-execute3 kih-jump *cache-data-selector*)
   (k-execute3 kih-alu-nop kil-read-gc-rams)
   (setq temp (ldb (byte 4. 4.) (k-read-spy-mmfio)))
   (when (not (equal temp #x0f))
      (k-diag-error "Test-56 Transporter RAM address" addr #x0f temp))
   (k-execute kih-load-transporter 0)
   (k-execute kih-load-mctl 0)
   (k-execute kih-load-md 0)
   (k-execute kih-load-vma 0)
   (k-execute3 kih-jump *cache-data-selector*)
   (k-execute3 kih-alu-nop kil-read-gc-rams)
   (setq temp (ldb (byte 4. 4.) (k-read-spy-mmfio)))
   (when (not (equal temp 0))
      (k-diag-error "Test-56 Transporter RAM address" addr 0 temp)))

(defun k-test56b (&optional fast &aux temp data)
  "Transporter ram address test"
  (k-init)
  (format t "Starting Test 56b - Transporter address test~%")
  (dotimes (md-boxed 2.)
    (dotimes (vma-boxed 2.)
      (dotimes (trans-type 4.)
	(dotimes (trans-mode 4.)
	  (dotimes (datatype (if fast 4. 64.))
	    (setq data (logand #x0f (+ md-boxed vma-boxed trans-type trans-mode datatype)))
	    (k-write-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype data))))))
  (dotimes (md-boxed 2.)
    (dotimes (vma-boxed 2.)
      (dotimes (trans-type 4.)
	(dotimes (trans-mode 4.)
	  (dotimes (datatype (if fast 4. 64.))
	    (setq data (logand #x0f (+ md-boxed vma-boxed trans-type trans-mode datatype)))
	    (setq temp (k-read-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype))
	    (when (not (equal temp data))
	      (k-diag-error "Test56b - Transporter address"
			    (logior
			      (ash vma-boxed 11.)
			      (ash md-boxed 10.)
			      (ash trans-type 8.)
			      (ash trans-mode 6.)
			      datatype)
			    data temp))))))))

(defun k-test57 (&aux temp fs)
  "Test-57 Floating point data path tests"
  (k-init)
  (format t "Starting Test 57 - Floating Point Data Paths~%")
  (k-execute kih-fmul-g0 kil-fmode0)
  (k-execute kih-fmul-g0 kil-fmode1)
  (k-execute kih-fmul-g0 kil-fmode2)
  (k-execute kih-fmul-g0 kil-fmode3)
  (k-execute kih-falu-g0 kil-fmode0)
  (k-execute kih-falu-g0 kil-fmode1)
  (k-execute kih-falu-g0 kil-fmode2)
  (k-execute kih-falu-g0 kil-fmode3)
  (dotimes (i 23.)
    (k-execute3 kih-jump *cache-data-selector*)
    (k-execute kih-load-g1 (logior #x3f800000 (ash 1. i)))
    (k-execute kih-load-g2 #x3f800000) ; 1.00000
    (k-execute kih-load-g0 0)
    (k-execute3 kih-fmul-g0 kil-fmul-g1-g2)
 ;    (setq fs (k-read-float-status))		;this can not win, see rg.
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (logior #x3f800000 (ash 1. i))))
      (k-diag-error "TEST-57 FMUL (2264) right" nil (logior #x3f800000 (ash 1. i)) temp))
 ;    (when (not (= 0 fs))
 ;      (k-diag-error "TEST-57 FMUL (2264) right: Incorrect Float Status" nil 0 fs))
    )
  (dotimes (i 23.)
    (k-execute3 kih-jump *cache-data-selector*)
    (k-execute kih-load-g2 (logior #x3f800000 (ash 1. i)))
    (k-execute kih-load-g1 #x3f800000) ; 1.00000
    (k-execute kih-load-g0 0)
    (k-execute3 kih-fmul-g0 kil-fmul-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (logior #x3f800000 (ash 1. i))))
      (k-diag-error "TEST-57 FMUL (2264) left" nil (logior #x3f800000 (ash 1. i)) temp))
 ;    (setq temp (k-read-float-status))
 ;    (when (not (= 0 temp))
 ;      (k-diag-error "TEST-57 FMUL (2264) right: Incorrect Float Status" nil 0 temp))
    )
  (dotimes (i 32.)
    (k-execute3 kih-jump *cache-data-selector*)
    (k-execute kih-load-g1 (ash 1. i))
    (k-execute kih-load-g2 0)
    (k-execute kih-load-g0 0)
    (k-execute3 kih-falu-g0 kil-fiadd-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (ash 1. i)))
      (k-diag-error "TEST-57 FALU (2265) IADD right" nil (ash 1. i) temp))
 ;    (setq temp (k-read-float-status))
 ;    (when (not (= 0 temp))
 ;      (k-diag-error "TEST-57 FALU (2265) right: Incorrect Float Status" nil 0 temp))
    )
  (dotimes (i 32.)
    (k-execute3 kih-jump *cache-data-selector*)
    (k-execute kih-load-g2 (ash 1. i))
    (k-execute kih-load-g1 0)
    (k-execute kih-load-g0 0)
    (k-execute3 kih-falu-g0 kil-fiadd-g1-g2)
    (k-execute3 kih-alu-nop kil-readl-g0)
    (setq temp (k-read-spy-mmfio))
    (when (not (= temp (ash 1. i)))
      (k-diag-error "TEST-57 FALU (2265) IADD left" nil (ash 1. i) temp))
 ;    (setq temp (k-read-float-status))
 ;    (when (not (= 0 temp))
 ;      (k-diag-error "TEST-57 FALU (2265) right: Incorrect Float Status" nil 0 temp))
    ))


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
	  (k-execute kih-jump *cache-data-selector*)
	  (if (zerop pbad)
	      (k-execute kih-load-mctl #x80000)	;Parity enable
	    (k-execute kih-load-mctl #x84000)	;Write wrong parity
	    (k-execute4 kih-jump *cache-data-selector*)
	    (setq expect (k-test58-genpat i))
	    (k-execute kih-load-md expect)
	    (k-execute kih-load-vma-sw addr)
	    (k-execute kih-jump *cache-data-selector*)
	    (k-execute kih-load-vma-sr addr)
	    (k-execute kih-jump *cache-data-selector*)
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
	(k-execute kih-jump *cache-data-selector*)
	(k-execute kih-load-mctl #x80000)	;Parity enable
	(k-execute4 kih-jump *cache-data-selector*)
	(setq expect (k-test58-genpat i))
	(k-mem-write (ash addr 2.) expect)
	(setq result (k-mem-read (ash addr 2.)))
	(when (not (numberp result))
	  (k-diag-error "Test 58 - NUBUS Parity" (ash addr 2.) expect expect))))))

(defun k-test58-genpat (n)
  (logior n (ash n 8.) (ash n 16.) (ash n 24.)))

(defun k-test59 (&aux temp bspec)
  "Test-59 NUBUS byte accesses to RAM"
  (format t "Starting Test 59 - NUBUS byte accesses to RAM~%")
  (k-init)
  (dolist (mrange k-mem-list)
    (do*
      ((addr (first mrange) (+ addr #x400000))
       (max  (second mrange)))
      ((> addr max))
      (dotimes (sel-byte 4.)
	(setq bspec (byte 8. (* 8. sel-byte)))
	(dotimes (i 256.)
	  (k-mem-write addr -1)
	  (k-mem-write-byte (+ addr sel-byte) (dpb i bspec 0))
	  (setq temp (k-mem-read addr))
	  (when (not (equal temp (dpb i bspec #xffffffff)))
	    (k-diag-error "Test 59 - NUBUS byte access" addr (dpb i bspec #xffffffff) temp)))))))


(defun k-test97 (&optional fast)
  "Test 97 - History RAM pointer"
  (format t "Starting Test 97 - History RAM pointer~%")
  (k-write-mode 1.)
  ;(k-reset)
  ;(k-init)
  (do ((count 0 (1+ count))
       (max (if fast 128. 4096.))
       (temp nil))
      ((>= count max))
    (if (not (= count (setq temp (logand #xfff (k-read-hptr)))))
	(format t "~%history pointer is #x~x, should be #x~x" temp count))
    (k-spy-cmd 5.)
    (k-spy-cmd 4.)))
    
(defun k-test97-loop ()
  (do-forever (k-read-hptr)))

(defun k-test98 (&optional fast &aux temp)
  "Test 98 - History RAM"
  (format t "Starting Test 98 - History RAM~%")
  (k-reset)
  (k-init)
  (dotimes (pass (if fast 1 24.))
    (format t "~%Pass ~D")
    (k-spy-cmd 5.)				;set opc clock bit
  ;do 4096 jumps
    (dotimes (i 4096.)
      (k-execute KIH-JUMP (ash 1. (mod (+ pass i) 24.))))
    (k-spy-cmd 4.)                              ;clear opc clock bit
    (k-execute kih-jump #xffffff)
    (dotimes (i 4096.)
      (setq temp (logand #xffffff (k-read-hram)))
      (when (not (= temp (ash 1. (mod (+ pass i) 24.))))
	(k-diag-error "TEST-98 - History RAM" i (ash 1. (mod (+ pass i) 24.)) temp))
      (k-spy-cmd 5.)
      (k-spy-cmd 4.))))

(defun k-test98-clock-opcram ()
  (do-forever
    (k-spy-cmd 5.)
    (k-spy-cmd 4.)))

(defun k-test98-read-loop ()
  (do-forever
    (k-read-hram)))

(defun k-test98-fill-opcram (data &aux temp)
  (do-forever
    (format t "write ")
    (dotimes (c 4096.)
      (k-execute KIH-JUMP data))
    (format t "read ")
    (dotimes (c 4096.)
      (setq temp (logand #xffffff (k-read-hram)))
      (k-spy-cmd 5.)
      (k-spy-cmd 4.))))

(defun k-test99 (slot &aux temp)
  "Simple NUBUS test, floating bit, location 0."
  (k-init)			;leaves 0 in VMA
  (k-execute kih-load-map (logior #xF000000F (ash slot 24.)))	;High F is quad on Nubus, low F is R/W both Lisp and C.
  (k-execute3 kih-nop 0)
  (k-write-mode 6.)		;enable mastership, turn on LED.
  (dotimes (i 32.)
    (k-execute kih-load-vma 0)
    (k-execute kih-load-md-sw (ash 1 i))
    (k-execute4 kih-nop 0)
    (k-execute kih-load-vma-sr 0)
    (k-execute kih-nop 0)
    (k-execute4 kih-alu-nop kil-read-md)
    (setq temp (k-read-spy-mmfio))
    (when (not (equal temp (ash 1 i)))
      (k-diag-error "Simple NUBUS test" 0 (ash 1 i) temp))
    (when (not (equal (setq temp (debug-read-word (logior #xf0000000 (ash slot 24.)))) (ash 1 i)))
      (k-diag-error "data not right in memory" 0 (ash 1 i) temp))
    ))

(defun k-nublink ()
  "Blink the NUBUS LED"
  (k-init)					;leaves 0 in VMA
  (k-write-mode 6)
  (k-execute kih-load-map
	     (logior #xF0FFF00F (ash k-physical-slot 24.)))	;High F is quad on Nubus, low F is R/W both Lisp and C.
  (k-execute3 kih-nop 0)
  (k-execute kih-load-vma (ash #x7fc -2))
  (do-forever
    (k-execute kih-load-md-sw 2)
    (k-execute4 kih-jump #x100)
    (sleep .1)
    (k-execute kih-load-md-sw 6)
    (k-execute4 kih-nop 0)
    (sleep .1)
    ))

(defun k-ledblink (n)		;n 0 1 or 2 or t
  "Blink the Leds controlled from the memory control register"
  (k-init)
  (do-forever
    (k-write-memory-control (dpb 7 (if (numberp n) (byte 1 (+ 5 n)) (byte 3 5)) 0))
    (sleep .1)
    (k-write-memory-control (dpb 0 (if (numberp n) (byte 1 (+ 5 n)) (byte 3 5)) 0))
    (sleep .1)))
  

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
    (format t "~%Memory list: ~x~2%" k-mem-list)
    k-mem-list))


(defun k-test-42-mworks-? (addr)
  (k-mem-write addr #x12345678)
  (if (equal #x12345678 (k-mem-read addr))
      (progn
	(k-mem-write addr #xedcba987)
	(if (equal #xedcba987 (k-mem-read addr)) t nil))
    nil))

(defun k-test43a ()
  (fast-address-test-kernal 'k-mem-write-word-address 'k-mem-read-word-address
			    0 32. 20. "Test 43A - Fast address test main memory via NUBUS")
  (TERPRI)
  )

(defun k-fast-address-test-datatype-ram ()
  (fast-address-test-kernal 'k-write-datatype-ram 'k-read-datatype-ram
			    0 1. 17. "datatype ram")
  (TERPRI)
  )

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
  (k-execute4 kih-jump *cache-data-selector*)
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

(defun test-l ()
  (do-forever (k-mem-read 0)))

(defun test-l1 (d)
  (do-forever (k-mem-write 0 d)))

(defun k-test45 ()
  "Short branch test."
  (k-init)
  (format t "Starting Test 45 - Short branches~&")
  (k-execute kih-jump #X555555)
  (dotimes (i 12.)
    (k-execute kih-branch (ash 1 i))
    (let ((output (k-read-spy-pc))
	  (expect (dpb (ash 1. i) (byte 12. 0.) #x555555)))
      (when (not (= output expect))
	(k-diag-error "TEST-45 Short branch test." i expect output)))))

(defun rvm (a)
  (k-execute kih-load-vma-sr a)
  (k-execute kih-jump *cache-data-selector*)
  (k-execute3 kih-alu-nop kil-read-md)
  (k-read-spy-mmfio))

(defun wvm (a d)
  (k-execute kih-load-md d)
  (k-execute kih-load-vma-sw a)
  (k-execute4 kih-jump *cache-data-selector*))

;(defun foo () (dotimes (i 256.) (wvm i -1) (do-forever (rvm 0) (rvm 16.))))
