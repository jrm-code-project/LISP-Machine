;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:ZL -*-

;Caution!! do not use HW:%%xxx type byte pointers in this file, use K-HW:%%xxx.  See NEW-SPY-UTILITIES.

;these actually load code into the machine and set it running.

(defun falcon-test-halt-static ()
 ;make sure halt bit is clear in processor control register.
  (k-write-processor-control (dpb 0 k-hw:%%processor-control-halt-processor 0))		;0
  (cond ((not (= 0 (ldb k-hw:%%processor-control-halt-processor (k-read-processor-control))))	;k-hw:%%processor-control-halt-processor
	 (fsignal "Unable to clear halt bit in processor control")))
  (cond ((not (= 0 (k-read-spy-halt)))
	 (fsignal "Spy halt reads set when clear in processor-control reg")))
 ;make sure it sets.
  (k-write-processor-control (dpb 1 k-hw:%%processor-control-halt-processor 0))		;1
  (cond ((not (= hw:$$halt (ldb k-hw:%%processor-control-halt-processor (k-read-processor-control))))
	 (fsignal "Unable to set halt bit in processor control")))
  (cond ((not (= 1 (k-read-spy-halt)))
	 (fsignal "Spy halt reads clear when set in processor-control reg")))
  (k-write-processor-control 0)
  )

(defun falcon-test-halt (&aux temp)
  (k-setup)
 ;  (k-diag-reset)
  (k-reset)
  (k-stop)

  (falcon-test-halt-static)
  (k-write-processor-control 0)

 ;map instruction page 0 to physical page 0
  (falcon-write-map-and-check #x8000 #x8f)		;local memory, read-write
 ;map data page 0 to physical page 0
  (falcon-write-map-and-check #x0 #x8f)		;local memory, read-write
 ;store halt inst in instruction locn #x100
  (write-inst-and-check #x100 (+ (ash kih-load-pctl 32.) (dpb 1 k-hw:%%processor-control-halt-processor 0)))
 ;also put halts in low locations in case of spurious trap, (the one at 0 might not work because of TSM, etc)
  (dotimes (locn #x100)
    (write-inst-and-check locn (+ (ash kih-load-pctl 32.) (dpb 1 k-hw:%%processor-control-halt-processor 0))))
  (dotimes (count 10.)
    (write-inst-and-check (+ count #X100) (+ (ash kih-load-pctl 32.) (dpb 1 k-hw:%%processor-control-halt-processor 0))))
  (falcon-set-pc-and-go #x100 :do-init t :do-initialize-call-hardware t :do-init-virtual-memory nil)
  (cond ((not (= 1 (k-read-spy-halt)))
	 (fsignal "Program to halt failed! PC = #x~x" (k-read-spy-pc)))
	((not (= (setq temp (k-read-spy-pc)) #x101))
	 (fsignal "PC not correct after halt, is #x~x, should be #x101" temp)))
  )



(defun falcon-count-loop (&aux temp)
  (k-setup)
 ;  (k-diag-reset)
  (k-reset)
  (k-stop)

  (k-write-processor-control 0)

 ;map instruction page 0 to physical page 0
  (falcon-write-map-and-check #x8000 #x8f)		;local memory, read-write
 ;map data page 0 to physical page 0
  (falcon-write-map-and-check #x0 #x8f)		;local memory, read-write

  (k-execute2 kih-load-a0 0)

  (write-inst-and-check #x100 (+ (ash (+ #x20000000 (ash #x10 (- 41. 32.))) 32.)	;dest A0
				 (ash #x13 12.)			;alu op R+1
				 (ash #x10 25.)			;right source A0
				 #x100))			;short branch address

 ;also put halts in low locations in case of spurious trap, (the one at 0 might not work because of TSM, etc)
  (dotimes (locn #x100)
    (write-inst-and-check locn (+ (ash kih-load-pctl 32.) (dpb 1 k-hw:%%processor-control-halt-processor 0))))
 ;also in #x101 in case branch fails.
  (dotimes (count 10.)
    (write-inst-and-check (+ count 1 #X100) (+ (ash kih-load-pctl 32.) (dpb 1 k-hw:%%processor-control-halt-processor 0))))
  (falcon-set-pc-and-go #x100 :do-init t :do-initialize-call-hardware t :do-init-virtual-memory nil)
  (cond ((not (= 0 (k-read-spy-halt)))
	 (fsignal "Count loop halted! PC = #x~x" (k-read-spy-pc)))
	((not (= (setq temp (k-read-spy-pc)) #x100))
	 (fsignal "Short branch failing to loop, PC is #x~x, should be #x100" temp)))
  )

