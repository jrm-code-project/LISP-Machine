;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10.; Readtable:CL -*-



;; defafun's  Control-Shift-C'ed  (compiled with the lambda compiled, does the assembly)

;; defun's    Hyper-c'ed    (compiled with the hardebach compiler but not downloaded)

;;;****************************************************************
;;;
;;; lifted from   k-sys:k;datatype-ram
;;;
;;;****************************************************************

(defafun write-dt-ram (lbox ldt rbox rdt dt-code value)
  (alu-field field-pass processor-control a5 processor-control (byte 1. 8.)) ;data bit
 left
  (alu-field extract-bit-right nop a0 a0 (byte 1. 0.))   ;left word
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) unboxed br-zero)
  (branch right ())
  (alu-field field-pass a6 a1 a1 (byte 6. 26.) boxed)
 right
  (alu-field extract-bit-right nop a2 a2 (byte 1. 0.))   ;right word
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) unboxed br-zero)
  (branch doit ())
  (alu-field field-pass a7 a3 a3 (byte 6. 26.) boxed)
 doit
  (alu-field field-extract-r a4 a4 a4 (byte 3. 0.) unboxed)
  (alu l+r a4 a4 a4 unboxed)
  (movea a8 write-dt-ram-dispatch)
  (alu l+r nop a8 a4)
  (nop)
  (nop next-pc-dispatch))


(defafun write-dt-ram-dispatch ()
  (alu setr datatype-ram-write-pulse a6 a7 dt-0)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-0))
  (alu setr datatype-ram-write-pulse a6 a7 dt-1)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-1))
  (alu setr datatype-ram-write-pulse a6 a7 dt-2)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-2))
  (alu setr datatype-ram-write-pulse a6 a7 dt-3)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-3))
  (alu setr datatype-ram-write-pulse a6 a7 dt-4)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-4))
  (alu setr datatype-ram-write-pulse a6 a7 dt-5)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-5))
  (alu setr datatype-ram-write-pulse a6 a7 dt-6)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-6))
  (alu setr datatype-ram-write-pulse a6 a7 dt-7)
  (unconditional-branch write-dt-ram-dispatch-exit (alu setr nop a6 a7 dt-7))
 write-dt-ram-dispatch-exit
  (movei a6 '0) ;these two moves kill the illegal stuff in a6 and a7
  (movei a7 '0)
  (return a5))


; load-dt-ram-pattern takes its first 5 arguments as 16 bit numbers (the top doesn't matter)
;   The bottom 8 bits are the pattern to match, and the high 8 bits are the mask to
;   be and'ed with the current test pattern. Only locations that match will be written.
;
;  Example: to set all locations with a left boxed datatype of #x1e in code 3:
;            (dt-ram-load #x0101 #x3f1e 0 0 #x0703 1)
;
(defafun load-dt-ram-pattern (lbox ldt rbox rdt dt-code value)
  (movei gr:*trap-temp1* 0)
  (movei a10 '1)
 lbox-loop
  (alu-field field-and a9 a0 a10 (byte 8. -8.) unboxed)
  (alu xor nop a0 a9 bw-8)
  (test br-not-equal)
  (branch lbox-next ())

  (movei a11 '63)
 ldt-loop
  (alu-field field-and a9 a1 a11 (byte 8. -8.) unboxed)
  (alu xor nop a1 a9 bw-8)
  (test br-not-equal)
  (branch ldt-next ())

  (movei a12 '1)
 rbox-loop
  (alu-field field-and a9 a2 a12 (byte 8. -8.) unboxed)
  (alu xor nop a2 a9 bw-8)
  (test br-not-equal)
  (branch rbox-next ())

  (movei a13 '63)
 rdt-loop
  (alu-field field-and a9 a3 a13 (byte 8. -8.) unboxed)
  (alu xor nop a3 a9 bw-8)
  (test br-not-equal)
  (branch rdt-next ())

  (movei a14 '7)
 dt-code-loop
  (alu-field field-and a9 a4 a14 (byte 8. -8.) unboxed)
  (alu xor nop a4 a9 bw-8)
  (test br-not-equal)
  (branch dt-code-next ())

  (move o5 a5 ch-open)
  (move o4 a14)
  (move o3 a13)
  (move o2 a12)
  (move o1 a11)
  (call (write-dt-ram 6) a6 (o0 a10))

  (alu l+1 gr:*trap-temp1* gr:*trap-temp1* ignore)

 dt-code-next
  (alu r-1 a14 a14 a14 bw-24)
  (test br-not-negative)
  (branch dt-code-loop ())

 rdt-next
  (alu r-1 a13 a13 a13 bw-24)
  (test br-not-negative)
  (branch rdt-loop ())

 rbox-next
  (alu r-1 a12 a12 a12 bw-24)
  (test br-not-negative)
  (branch rbox-loop ())

 ldt-next
  (alu r-1 a11 a11 a11 bw-24)
  (test br-not-negative)
  (branch ldt-loop ())

 lbox-next
  (alu r-1 a10 a10 a10 bw-24)
  (test br-not-negative)
  (branch lbox-loop ())

  (return a6))


;;;****************************************************************
;;;
;;;   defuns below here
;;;
;;;****************************************************************

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


;; must also set up datatype ram
;; clear traps so we can ignore single-step and asynchrnous traps

(defun vc-dt-and-ovf-trap-handler ()
  (setq gr::*save-right*  (hw:unboxed-constant #.trap-restore-test-result))
  (setq gr::*save-status* (hw:unboxed-constant #.trap-restore-test-status))
  )

;;; This function sets up the initial datatype ram
;;;
(defun load-initial-datatype-ram ()

;;; Init the whole datatype ram to ones

  (load-dt-ram-pattern  ; set all the bits
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    #x0000   ;all codes
    1)       

;;; Clear all the no-check code bits

  (load-dt-ram-pattern  ; clear the no-check bits
    #x0000   ;all left boxes
    #x0000   ;all left datatypes
    #x0000   ;all right boxes
    #x0000   ;all right datatypes
    (logior #x0700 vinc:$$dtc-none)	;only the no-check code
    0)

;;; set up the check for both fixnum
  
  (load-dt-ram-pattern  ; clear the fixnum-codes, left-fixnum & right-fixnum
    #x0101				;left-boxed
    (logior #x3f00 vinc:$$dtp-fixnum)	;left-fixnum
    #x0101				;right boxed
    (logior #x3f00 vinc:$$dtp-fixnum)	;right-fixnum
    (logior #x0700 vinc:$$dtc-both-fixnum) ;fixnum without overflow
    0)

  (load-dt-ram-pattern 
    #x0101				;left-boxed
    (logior #x3f00 vinc:$$dtp-fixnum)	;left-fixnum
    #x0101				;right boxed
    (logior #x3f00 vinc:$$dtp-fixnum)	;right-fixnum
    (logior #x0700 vinc:$$dtc-both-fixnum-with-overflow) ;fixnum with overflow
    0)

;;; set up check for list on right side
  
;  (load-dt-ram-pattern  ; clear the list codes, left-any & right-list
;    #x0000				;all left-box
;    #x0000				;all left datatypes
;    #x0101				;right-boxed
;    (logior #x3f00 vinc:$$dtp-cons)		;right-list
;    (logior #x0700 vinc:$$dtc-right-list)	;list code
;    0)
;  (load-dt-ram-pattern  ; clear the list codes, left-any & right-nil
;    #x0000				;all left-box
;    #x0000				;all left datatypes
;    #x0101				;right-boxed
;    (logior #x3f00 vinc:$$dtp-nil)		;right-nil
;    (logior #x0700 vinc:$$dtc-right-list)	;list code
;    0)

;;;; set up check for array on right side and structure on left side

;  (load-dt-ram-pattern  ; clear the list codes, left-structure & right-array
;    #x0101                                 ;left-box
;    (logior #x3f00 vinc:$$dtp-structure)   ;left-structure
;    #x0101				   ;right-boxed
;    (logior #x3f00 vinc:$$dtp-array)	   ;right-array
;    (logior #x0700 vinc:$$dtc-right-array-and-left-structure) ;array code
;    0)
  

;;;; set up check for not both character

;  (load-dt-ram-pattern  ; clear the character-codes, left-character & right-character
;    #x0101				 ;left-boxed
;    (logior #x3f00 vinc:$$dtp-character) ;left-character
;    #x0101				 ;right boxed
;    (logior #x3f00 vinc:$$dtp-character) ;right-character
;    (logior #x0700 vinc:$$dtc-both-character) ;both character codes
;    0)

;;;; set up check for same type of hairy numbers (for EQL)

;  (load-dt-ram-pattern  ; clear the entry
;    #x0000   ;all left boxes
;    #x0000   ;all left datatypes
;    #x0000   ;all right boxes
;    #x0000   ;all right datatypes
;    (logior #x0700 vinc:$$dtc-hairy-number)	;only the no-check code
;    0)

;  (load-dt-ram-pattern  ;trap if left unboxed
;    #x0100				 ;left-unboxed
;    #x0000       			 ;left dt any
;    #x0000				 ;right box any
;    #x0000				 ;right dt any
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both fixnum codes
;    1)

;  (load-dt-ram-pattern  ;trap if right unboxed
;    #x0000				 ;left box any
;    #x0000       			 ;left dt any
;    #x0100				 ;right unbox
;    #x0000				 ;right dt any
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both fixnum codes
;    1)

;  (load-dt-ram-pattern  ; trap if both bignum
;    #x0101				   ;left-boxed
;    (logior #x3f00 vinc:$$dtp-bignum) ;left-short
;    #x0101				   ;right boxed
;    (logior #x3f00 vinc:$$dtp-bignum) ;right-short
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
;    1)

;  (load-dt-ram-pattern  ; trap if both rational
;    #x0101				   ;left-boxed
;    (logior #x3f00 vinc:$$dtp-rational) ;left-short
;    #x0101				   ;right boxed
;    (logior #x3f00 vinc:$$dtp-rational) ;right-short
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
;    1)

;  (load-dt-ram-pattern  ; trap if both short-float
;    #x0101				   ;left-boxed
;    (logior #x3f00 vinc:$$dtp-short-float) ;left-short
;    #x0101				   ;right boxed
;    (logior #x3f00 vinc:$$dtp-short-float) ;right-short
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
;    1)

;  (load-dt-ram-pattern  ; trap if both float
;    #x0101				 ;left-boxed
;    (logior #x3f00 vinc:$$dtp-single-float)     ;left-float
;    #x0101				 ;right boxed
;    (logior #x3f00 vinc:$$dtp-single-float)     ;right-float
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both float codes
;    1)

;  (load-dt-ram-pattern  ; trap if both double-float
;    #x0101				 ;left-boxed
;    (logior #x3f00 vinc:$$dtp-double-float) ;left-double
;    #x0101				 ;right boxed
;    (logior #x3f00 vinc:$$dtp-double-float) ;right-double
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both double codes
;    1)

;  (load-dt-ram-pattern  ; trap if both complex
;    #x0101				   ;left-boxed
;    (logior #x3f00 vinc:$$dtp-complex) ;left-short
;    #x0101				   ;right boxed
;    (logior #x3f00 vinc:$$dtp-complex) ;right-short
;    (logior #x0700 vinc:$$dtc-hairy-number) ;both short codes
;    1)

 )

;*** from k;boot
(defun initialize-call-hardware ()
  "Initialize call hardware and build a heap."
  
  ;; First, we snarf down a valid open and active frame.
  ;; Note that the return frame is different.
  (hw:write-open-active-return (hw:unboxed-constant #xFFFFFE))	;Get a frame
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)

  (let ((free           #xfe)
	(trap-state 0)

	;; Must save return pc, this should be dtp code here.
	(our-return-pc (hw:ldb (hw:read-return-pc-return-dest) hw:%%ch-rpcd-return-pc 0))
	;; Cannot have temporaries, so we declare every local here.

	;; Put 238 (256 total frames - 16 globals - 2 (open and active) and return)
	;; Do not remove this local, execution of code depends on it!
	(number-of-frames   238.)
	(r-frame            #xfe)
	(zero               0.)
	(oar-temp           0.))

    ;; Empty heap and call stack.  Must do after saving return pc locally
    ;; because writing call-stack-pointer will clobber it.
    ;; #xF0 gives us 16 yellow alert frames
    (hw:write-call-hp-sp #xEF00)		;Empty the heap and stack
    (hw:nop)
    (hw:nop)
    (hw:nop)
    (hw:nop)

    (tagbody
     loop
	(if (= number-of-frames zero)
	  (go end))

	(setq oar-temp (hw:read-open-active-return))
	(setq oar-temp (hw:dpb r-frame hw:%%ch-oar-active oar-temp))
	(hw:write-open-active-return
	  (hw:dpb free hw:%%ch-oar-return oar-temp))
	;; 4 nops to get delayed-return loaded
	(hw:nop)
	(hw:nop)
	(hw:nop)
	(hw:nop)
	(hw:nop)
	;;; Clear out the frame
	(setf (hw:r0)  (hw:unboxed-constant 0))
	(setf (hw:r1)  (hw:unboxed-constant 0))
	(setf (hw:r2)  (hw:unboxed-constant 0))
	(setf (hw:r3)  (hw:unboxed-constant 0))
	(setf (hw:r4)  (hw:unboxed-constant 0))
	(setf (hw:r5)  (hw:unboxed-constant 0))
	(setf (hw:r6)  (hw:unboxed-constant 0))
	(setf (hw:r7)  (hw:unboxed-constant 0))
	(setf (hw:r8)  (hw:unboxed-constant 0))
	(setf (hw:r9)  (hw:unboxed-constant 0))
	(setf (hw:r10) (hw:unboxed-constant 0))
	(setf (hw:r11) (hw:unboxed-constant 0))
	(setf (hw:r12) (hw:unboxed-constant 0))
	(setf (hw:r13) (hw:unboxed-constant 0))
	(setf (hw:r14) (hw:unboxed-constant 0))
	(setf (hw:r15) (hw:unboxed-constant 0))
	(hw:nop)
	(hw:nop)
	(hw:ch-tcall)
	(setq free (1- free))
	(setq number-of-frames (1- number-of-frames))
	(go loop)

     end)

    (hw:write-open-active-return (hw:unboxed-constant #xFFFF10))
    (hw:nop)
    (hw:nop)
    (hw:nop)

    ;; Do a "return"  A real one won't work because the
    ;; stack is trashed.
    (hw:dispatch our-return-pc)))


;;;****************************************************************
;;;
;;; lifted from transporter-ram.lisp
;;;
;;;****************************************************************

(defun vc-write-md-generic (md boxed)
  (if (= boxed hw:$$boxed)
      (hw:write-md-boxed   md)
      (hw:write-md-unboxed md)))

(defun vc-vma-start-read-generic (vma-boxed md-boxed cdr trans-type location)
  (if (= vma-boxed hw:$$boxed)
      (if (= md-boxed hw:$$boxed)
	  (if (= cdr vmem:$$read-no-cdr)
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read              location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-no-transport location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-visible-evcp location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-will-write   location :boxed :boxed)))
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read-cdr              location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-cdr-no-transport location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-cdr-visible-evcp location :boxed :boxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-cdr-will-write   location :boxed :boxed))))
	  (if (= cdr vmem:$$read-no-cdr)
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read              location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-no-transport location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-visible-evcp location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-will-write   location :boxed :unboxed)))
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read-cdr              location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-cdr-no-transport location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-cdr-visible-evcp location :boxed :unboxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-cdr-will-write   location :boxed :unboxed)))))
      (if (= md-boxed hw:$$boxed)
	  (if (= cdr vmem:$$read-no-cdr)
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read              location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-no-transport location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-visible-evcp location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-will-write   location :unboxed :boxed)))
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read-cdr              location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-cdr-no-transport location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-cdr-visible-evcp location :unboxed :boxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-cdr-will-write   location :unboxed :boxed))))
	  (if (= cdr vmem:$$read-no-cdr)
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read              location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-no-transport location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-visible-evcp location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-will-write   location :unboxed :unboxed)))
	      (cond ((= trans-type vinc:$$transport-type-transport)
		     (hw:vma-start-read-cdr              location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-no-transport)
		     (hw:vma-start-read-cdr-no-transport location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-visible-evcp)
		     (hw:vma-start-read-cdr-visible-evcp location :unboxed :unboxed))
		    ((= trans-type vinc:$$transport-type-write)
		     (hw:vma-start-read-cdr-will-write   location :unboxed :unboxed)))))))

(defun vc-store-into-transporter-ram (value)
  (hw:write-transporter-ram (hw:dpb value hw:%%transporter-ram-bus-offset 0.))
  (hw:nop)
  (hw:nop)
  nil)

(defun vc-write-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype value)
  ;; set transporter mode
  ;; we are assuming that we do not have to restore the previous mode
  ;; because there is no one we could be interrupting
  (hw:write-memory-control (hw:dpb trans-mode hw:%%memory-control-transporter-mode (hw:read-memory-control)))
  ;; Setup vma boxed and trans type
  (vc-vma-start-read-generic vma-boxed hw:$$unboxed vmem:$$read-no-cdr trans-type 0.)
  ;; setup datatype and md-boxed
  (vc-write-md-generic (hw:dpb datatype hw:%%transporter-ram-md-byte 0.) md-boxed)
  (vc-store-into-transporter-ram value))

(defun vc-load-transporter-ram-pattern (vma-boxed md-boxed datatype mstat mctl value)
  (dotimes (vmab 2.)
    (when (or (eq t vma-boxed) (= vma-boxed vmab))
      (dotimes (mdb 2.)
	(when (or (eq t md-boxed) (= mdb md-boxed))
	  (dotimes (dtp 64.)
	    (when (or (eq t datatype) (= dtp datatype))
	      (dotimes (type 4.)
		(when (or (eq t mstat) (= type mstat))
		  (dotimes (mode 4.)
		    (when (or (eq t mctl) (= mctl mode))
		      (vc-write-transporter-ram vmab mdb type mode dtp value))))))))))))

(defun vc-vc-load-transporter-ram-data ()
  ;; Format of transporter ram data:
  ;;                           vma-boxed md-boxed datatype mstat mctl value
  ;; Value is:
  ;;    box-error trap-if-not-old trap-if-old trappable-pointer

  ;; Anything weird we trap on. 
  (vc-load-transporter-ram-pattern t t t                 t        t      #b1111)
  
  ;; Don't trap on unboxed-write if no gc-write-test, no trap on unboxed read if "no-transport"
  (vc-load-transporter-ram-pattern 0 0 t                 transporter-ram:no-trans transporter-ram:normal #b0000)
  
  ;; Don't trap on unboxed-write if no gc-write-test, trap on unboxed read if not "no-transport"
  (vc-load-transporter-ram-pattern 0 0 t                 transporter-ram:vis-evcp transporter-ram:normal #b0110)
  
  ;; NIL is not treated like a pointer in the transporter ram.
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-nil    transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-nil    transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-nil    transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-nil    transporter-ram:write    transporter-ram:normal #b0000)

  ;; FIXNUMS 
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum transporter-ram:write    transporter-ram:normal #b0000)

  ;; CHARACTERS same as fixnums
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-character transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-character transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-character transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-character transporter-ram:write    transporter-ram:normal #b0000)

  ;; ARRAY HEADER SINGLE not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY HEADER MULTIPLE not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY HEADER EXTENSION not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; STRUCTURE HEADER not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; HASH-TABLE HEADER not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; UNBOXED STRUCTURE HEADER not a pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header transporter-ram:trans    transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header transporter-ram:vis-evcp transporter-ram:normal #b1001)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-array  transporter-ram:write    transporter-ram:normal #b0011)

    ;; STRUCTURE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-structure  transporter-ram:write    transporter-ram:normal #b0011)

    ;; HASH-TABLE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  transporter-ram:write    transporter-ram:normal #b0011)

    ;; CONS
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-cons   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-cons   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-cons   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-cons   transporter-ram:write    transporter-ram:normal #b0011)

    ;; SYMBOL HEADER pointer, but don't bash
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header transporter-ram:write    transporter-ram:normal #b0000) ;Compiler bug - writes temps to stack

  ;; SYMBOL
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   transporter-ram:write    transporter-ram:normal #b0011)

  ;; BIGNUM
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   transporter-ram:write    transporter-ram:normal #b0011)

  ;; RATIONAL
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-rational   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-rational   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-rational   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-rational   transporter-ram:write    transporter-ram:normal #b0011)

  ;; SHORT-FLOAT
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   transporter-ram:write    transporter-ram:normal #b0011)

  ;; SINGLE-FLOAT
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   transporter-ram:write    transporter-ram:normal #b0011)

  ;; DOUBLE-FLOAT
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   transporter-ram:write    transporter-ram:normal #b0011)

  ;; COMPLEX
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-complex   transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-complex   transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-complex   transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-complex   transporter-ram:write    transporter-ram:normal #b0011)

  ;; UNBOUND
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  transporter-ram:trans    transporter-ram:normal #b0111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  transporter-ram:write    transporter-ram:normal #b0011)


  ;-- incomplete..   one-q-forward, external-value-cell-pointer not here. -rg 4/13/88
  ;; I'm not sure this is quite right

  ;; BODY-FORWARD
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  transporter-ram:trans    transporter-ram:normal #b0111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  transporter-ram:vis-evcp transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  transporter-ram:write    transporter-ram:normal #b0011)

  ;; HEADER-FORWARD
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  transporter-ram:trans    transporter-ram:normal #b0111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  transporter-ram:vis-evcp transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  transporter-ram:write    transporter-ram:normal #b0011)
    
  ;; COMPILED-FUNCTION
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  transporter-ram:write    transporter-ram:normal #b0011)

  ;; CODE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-code  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-code  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-code  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-code  transporter-ram:write    transporter-ram:normal #b0011)

  ;; LOCATIVE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-locative  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-locative  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-locative  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-locative  transporter-ram:write    transporter-ram:normal #b0011)

  ;; UNBOXED-LOCATIVE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  transporter-ram:write    transporter-ram:normal #b0011)

  ;; LEXICAL-CLOSURE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  transporter-ram:write    transporter-ram:normal #b0011)

  ;; INTERPRETER-CLOSURE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  transporter-ram:write    transporter-ram:normal #b0011)

  ;; DYNAMIC-CLOSURE
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  transporter-ram:write    transporter-ram:normal #b0011)

  ;; SELECT-METHOD
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  transporter-ram:trans    transporter-ram:normal #b0011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  transporter-ram:vis-evcp transporter-ram:normal #b1011)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  transporter-ram:write    transporter-ram:normal #b0011)

  ;; UNRECONCILED (always trap)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  transporter-ram:trans    transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  transporter-ram:vis-evcp transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  transporter-ram:write    transporter-ram:normal #b1111)

  ;; SELF-REF-POINTER
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  transporter-ram:no-trans transporter-ram:normal #b1111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  transporter-ram:trans    transporter-ram:normal #b0111)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  transporter-ram:vis-evcp transporter-ram:normal #b0000)
  (vc-load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  transporter-ram:write    transporter-ram:normal #b0000)

  )

;from jb:k;memory-map

(defun vc-direct-map (physical-memory-layout)
  ;; First, we blow away the entire map with $$map-status-direct-mapped
  ;; this won't hurt because it is read-only and the instructions are read
  ;; only.  Then, we point the bottom parts of virtual memory at the blocks
  ;; of physical memory to make them appear contiguous.  Later on, when we
  ;; flush this direct map, anything marked as initial code will be thrown
  ;; away, and anything marked as direct map will be placed on the freelist.
  (labels (
	   (zap-map (entry)
	     (if (= entry #o100000)
		 '()
		 (progn (write-map-status entry map::$$map-status-direct-mapped)
			(zap-map (1+ entry)))))

	   (associate-block (virtual physical)
	     (dotimes (cluster-in-block vinc:*clusters-in-physical-block*)
;	       (trap::illop "Calling associate local memory.")
	       (associate-local-memory
		 (hw:dpb-unboxed physical hw:%%cluster-physical-address-block cluster-in-block)
		 (hw:dpb-unboxed virtual  hw:%%cluster-physical-address-block cluster-in-block)
		 map:$$map-status-direct-mapped)
;	       (trap::illop "Returned from associate local memory.")
	       ))

	   (associate-memory (virtual physical)
;	     (trap::illop "associate memory")
	     (cond ((= physical vinc:*blocks-of-physical-memory*) nil)
		   ((physical-block-exists? physical physical-memory-layout)
		    (associate-block virtual physical)
		    (associate-memory (1+ virtual) (1+ physical)))
		   (t (associate-memory virtual (1+ physical))))))
    
;    (trap::illop "Entered direct map.")
    (zap-map 0)
    (associate-memory 0 0)))

(defun write-map-status (virtual-cluster new-status)
  (modify-map virtual-cluster
    #'(lambda (map-value)
	(inject-map-status map-value new-status))))

(defconstant %%map-status-v-we-bits (byte 2. 0.))
(defconstant %%map-status-s-bits    (byte 2. 2.))

(defun inject-map-status (map-bits status)
 ;was defsubst
  (vinc::dpb-multiple-unboxed
    (hw:ldb status %%map-status-s-bits 0) map:%%map-status-bits
    (hw:ldb status %%map-status-v-we-bits 0) hw:%%map-lisp-trap-bits
;   (status-s-bits    status) map:%%map-status-bits
;   (status-v-we-bits status) hw:%%map-lisp-trap-bits
    map-bits))

(defun modify-map (virtual-cluster modifier)
  ;was macro
  ;; Atomically modifies the map by ensuring traps are off.
  ;; Use this function if you are not blasting the map data.
  ;; NOTE:  We don't re-address the map after funcalling the
  ;; modifier.  This can lose if the modifier touches the VMA.
  ;; I don't think we need to "do it right" yet.  (I just know
  ;; I'm going to screw someone with this...)
  `(LET ((VIRTUAL-CLUSTER ,virtual-cluster)
	 (MODIFIER        ,modifier))
     (trap::without-traps
       #'(LAMBDA ()
	   (ADDRESS-MAP VIRTUAL-CLUSTER)
	   ;; $$$ Changed to call hw-read-map-safe <15-Nov-88 JIM>
	   (LET ((NEW-VALUE (FUNCALL MODIFIER (map:HW-READ-MAP-safe))))
	     (HW:WRITE-MAP NEW-VALUE)
	     (HW:NOP)
	     (HW:NOP)
	     nil)))))