;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

;;; New stuff for instruction level simulation.
;;; Registers needed:

;;; PC + 1

;;; TRAP
;;; PROCESSOR-STATUS
;;; PROCESSOR-CONTROL
;;; MEMORY-STATUS
;;; MEMORY-CONTROL
;;; Copy pointer
;;; IREG1 low
;;; IREG1 high
;;; IREG2 low
;;; IREG2 high
;;; IREG3 low
;;; IREG3 high

;;; two copies each of

;;; IR low
;;; IR high
;;; Left source
;;; Left boxed
;;; Right source
;;; Right boxed
;;; ALU status
;;; Outreg
;;; Outreg boxed
;;; MFOREG
;;; OPEN
;;; ACTIVE
;;; RETURN

(eval-when (load compile eval)
  (import '(micro::32-dpb micro::32-ldb)))


;;; This is a crock, but there are too many other reasons
;;; why we cannot have more than one incarnation of the
;;; simulator at any one time.

(defvar *global-time* 0)

(defun memoized (function)
  (let ((last-time 0))
    #'(lambda ()
	(unless (= last-time *global-time*)
	  (funcall function)
	  (setq last-time *global-time*)))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Machine-registers
;;;;;;;;;;;;;;;;;;;;;;

(defstruct (machine-register (:constructor    make-machine-register-internal)
			     (:print-function print-machine-register)
			     (:conc-name "MACHINE-REGISTER-"))
  name
  compute-function
  saved-value
  output-function)

(defun print-machine-register (register stream ignore)
  (sys:printing-random-object (register stream)
    (format stream "MACHINE-REGISTER ~s" (machine-register-name register))))

(defstruct (machine-register-set (:print-function print-machine-register-set)
				 (:conc-name "MACHINE-REGISTER-SET-"))
  (list       '())
  (hash-table  (make-hash-table :test #'eq)))

(defun print-machine-register-set (register-set stream ignore)
  (sys:printing-random-object (register-set stream)
    (format stream "MACHINE-REGISTER-SET")))

(defun make-machine-register (name compute-function output-function register-set)
  (let ((hash-table (machine-register-set-hash-table register-set)))
    (multiple-value-bind (value found?)
	(gethash name hash-table)
      (when found?
	(cerror "Bash the old register." "~&Register ~s already exists." value)
	(setf (machine-register-set-list register-set)
	      (remove value (machine-register-set-list register-set))))
      (let ((register (make-machine-register-internal
			:name             name
			:compute-function compute-function
			:saved-value      0
			:output-function  output-function)))
	(setf (machine-register-set-list register-set)
	      (cons register (machine-register-set-list register-set)))
	(puthash name register hash-table)))))

(defun kill-register (name register-set)
  (let ((hash-table (machine-register-set-hash-table register-set)))
    (multiple-value-bind (register found?)
	(gethash name hash-table)
      (if (not found?)
	  (ferror nil "~&There is no register named ~s in set ~s" name register-set)
	  (progn (remhash name hash-table)
		 (setf (machine-register-set-list register-set)
		       (remove register (machine-register-set-list register-set))))))))

(defun find-register (name register-set)
  (gethash name (machine-register-set-hash-table register-set)))

(defun clock-machine-registers (register-set)
  (let ((reglist (machine-register-set-list register-set)))
;    (do ((tail reglist (rest tail))
;	 (vals '()     (cons (funcall (machine-register-compute-function (first tail))) vals))
;	 (rev  '()     (cons (first tail) rev)))
;	((null tail)
;	 (do ((valtail vals (rest valtail))
;	      (revtail rev  (rest revtail)))
;	     ((null valtail) nil)
;	   (setf (contents (machine-register-output-locative (first revtail)))
;		 (first valtail)))))))
	 
    (block got-inputs
      (do-forever
	(incf *global-time*)
	(catch 'foo ;'recompute-inputs
	  (dolist (reg reglist)
	    (setf (machine-register-saved-value reg)
		  (funcall (machine-register-compute-function reg))))
	  (return-from got-inputs (values)))
	(format t "~&TRAPPED!")))
    (register-file-write-cycle)
    (dolist (reg reglist)
      (funcall (machine-register-output-function reg) (machine-register-saved-value reg)))
    ))

(defmacro def-simple-register (name machine &body compute-function)
  (let ((compute-name (intern (string-append "COMPUTE-" (string name))))
	(output-name  (intern (string-append "OUTPUT-" (string name)))))
    `(PROGN (DEFVAR ,name 0)
	    (DEFUN ,compute-name () ,@compute-function)
	    (DEFUN ,output-name (O) (SETQ ,name O))
	    (EVAL-WHEN (LOAD)
	      (MAKE-MACHINE-REGISTER (QUOTE ,name)
				     (QUOTE ,compute-name)
				     (QUOTE ,output-name) ,machine)))))

(defvar *machine-regs*)

(eval-when (load)
  (setq *machine-regs* (make-machine-register-set)))

(defvar *pc* 0)
(defvar *opc-freeze* 0)

;;;;;;;;;;;;;;;;;;
;;; pc-1 and pc-2
;;;;;;;;;;;;;;;;;;

(def-simple-register *pc-1* *machine-regs*
  (assure-pc-selected)
  (if (zerop *opc-freeze*)
      *pc*
      *pc-1*))

(def-simple-register *pc-2* *machine-regs*
  (if (zerop *opc-freeze*)
      *pc-1*
      *pc-2*))

;;;;;;;;;;;;;;;
;;; eco number
;;;;;;;;;;;;;;;

(defvar *processor-eco-number* 15.)

;;;;;;;;;;;;;;;;;;;
;;; Initialization
;;;;;;;;;;;;;;;;;;;

(defun init-bls ()
  (new-reset-memory-control)
  (setq *processor-control* 0)
  (setq *oreg* 23.)
  (setq *l*    42.)
  (setq *r*    69.)
  (setq *pc-1* 71.)
  (setq *alu-instruction* 0)
  (setq *pc* 100.)
  (setq *ireg* 0.)
  (setq *fdest* 0.)
  (setq *fdest-latch* 0.)
  (setq *trap* 0.))
  (catch 'recompute-inputs
    (cause-trap %%k-trap-reset)))

(defun clock ()
  (if (zerop (ldb %%k-processor-control-halt-processor *processor-control*))
      (progn (clock-machine-registers *machine-regs*)
	     t)
      nil))

(defvar *clock-enable*       nil)
(defvar *enable-acknowledge* nil)
(defvar *clock-on*           nil)
(defvar *clock-function*)
(defvar *k-clock-process* (make-process "K Clock"
					:warm-boot-action 'ignore
					:arrest-reasons   '(:stopped)))

(defun clock-step ()
  (if (clock)
      (progn (setq *clock-enable* nil) t)
      nil))

(defun k-clock-process-function ()
  (do-forever
    (process-wait "Not Enabled"
		  #'(lambda ()
		      (setq *clock-on* *clock-enable*)))
    (setq *enable-acknowledge* t)
    (do () ((or (not *clock-enable*)
		(funcall *clock-function*)
		(progn (cerror "Return" "The K processor has halted.") nil))
	    (setq *clock-enable* nil)
	    (setq *clock-on* nil)))))

(defun k-clock-on ()
  (send *k-clock-process* :preset 'k-clock-process-function)
  (send *k-clock-process* :reset)
  (send *k-clock-process* :run-reason :enable)
  (send *k-clock-process* :revoke-arrest-reason :stopped))

;;;;;;;;;;;;;;;;;;;;;;;
;;; TRAP STATE MACHINE
;;;;;;;;;;;;;;;;;;;;;;;

(defvar tsm-prom nil)

(defvar trap-state-machine-spec
  '(
     ;; tseq   
     ((0 0 0 0  1  x  0 0 0 0 0) (0 0 0 0  0  1  0  0  1  1  1  1  1  1  0  0))
     ((0 0 0 0  1  x  0 0 0 0 1) (0 0 0 0  0  1  0  0  1  1  1  1  1  1  0  0))
     ((0 0 0 0  1  x  0 0 0 1 0) (0 0 0 0  0  1  0  0  0  0  1  1  1  1  0  0))
     ((0 0 0 0  1  x  0 0 0 1 1) (0 0 0 0  0  1  0  0  0  0  0  1  1  1  0  0))
     ((0 0 0 0  1  x  0 0 1 0 0) (0 0 0 0  0  1  0  0  0  0  0  0  0  1  0  0))
     ((0 0 0 0  1  x  0 0 1 0 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  1  0  0))
     ((0 0 0 0  1  x  0 0 1 1 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  1  0  0))
     ((0 0 0 0  1  x  0 0 1 1 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  1  0  0))
     ((0 0 0 0  1  x  0 1 0 0 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  1  0  0))
     ((0 0 0 0  1  x  0 1 0 0 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  0 1 0 1 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  0 1 0 1 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))

     ((0 0 0 0  1  x  0 1 1 0 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  0 1 1 0 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  0 1 1 1 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  0 1 1 1 1) (0 0 0 0  0  0  0  0  0  0  0  1  0  0  0  0))
     ((0 0 0 0  1  x  1 0 0 0 0) (0 0 0 1  0  0  0  0  0  0  0  1  0  0  0  0))
     ((0 0 0 1  x  x  x x x x x) (0 0 1 0  0  1  0  0  0  0  0  0  0  0  0  0))
     ((0 0 1 0  x  0  x x x x x) (0 0 0 0  0  0  1  0  0  0  0  0  0  0  0  0))
     ((0 0 1 0  x  1  x x x x x) (0 0 1 1  0  0  1  0  0  0  0  0  0  0  0  0))
     ((0 0 1 1  x  x  x x x x x) (0 0 0 0  1  0  0  0  0  0  0  0  0  0  0  0))

     ((0 0 0 0  1  x  1 0 1 0 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  1 0 1 0 1) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  1 0 1 1 0) (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ((0 0 0 0  1  x  1 0 1 1 1) (0 0 0 0  0  0  0  0  0  0  1  1  0  0  0  0))
     ((0 0 0 0  1  x  1 1 0 0 0) (0 1 0 0  0  0  0  0  0  0  1  1  0  0  0  0))
     ((0 1 0 0  x  x  x x x x x) (0 1 0 1  0  1  0  0  0  0  1  0  0  0  0  0))
     ((0 1 0 1  x  0  x x x x x) (0 0 0 0  0  0  1  1  0  0  0  0  0  0  1  0))
     ((0 1 0 1  x  1  x x x x x) (0 1 1 0  0  0  1  1  0  0  0  0  0  0  1  0))
     ((0 1 1 0  x  x  x x x x x) (0 0 0 0  1  0  0  0  0  0  0  0  0  0  0  0))

     (("default")                (0 0 0 0  0  0  0  0  0  0  0  0  0  0  0  0))
     ))

(eval-when (load)
  (progn 
    (user::make-prom-array-to-specs trap-state-machine-spec 'tsm-prom 2048. 16. nil)
    (user::convert-prom-array-to-numbers tsm-prom)))

(defvar *tsm-output* 0)

(defun compute-tsm-output ()
  (assure-pc-selected)
  (let ((address 	(dpb (ldb (byte 4. 12.) *tsm-output*)
	     (byte 1. 7.)
	     (dpb (if (zerop (ldb (byte 19. 5.) *pc*)) 1. 0.)
		  (byte 1. 6.)
		  (dpb (ldb %%k-memory-control-single-step-enable *memory-control*)
		       (byte 1. 5.)
		       (ldb (byte 5. 0.) *pc*))))))
;    (format t "~&TSM ~d" address)
  (aref tsm-prom
	(dpb (ldb (byte 4. 12.) *tsm-output*)
	     (byte 1. 7.)
	     (dpb (if (zerop (ldb (byte 19. 5.) *pc*)) 1. 0.)
		  (byte 1. 6.)
		  (dpb (ldb %%k-memory-control-single-step-enable *memory-control*)
		       (byte 1. 5.)
		       (ldb (byte 5. 0.) *pc*)))))))

(defun output-tsm-output (o)
  (setq *left-freeze*  (ldb (byte 1. 6.) o))
  (setq *right-freeze* (ldb (byte 1. 5.) o))
  (setq *oreg-freeze*  (ldb (byte 1. 7.) o))
  (setq *alu-freeze*   (ldb (byte 1. 4.) o))
  (setq *jstat-freeze* (ldb (byte 1. 3.) o))
  (setq *opc-freeze*   (ldb (byte 1. 2.) o))
  (setq *tsm-output* o))

(eval-when (load)
  (make-machine-register '*tsm-output* 'compute-tsm-output 'output-tsm-output *machine-regs*))

;;;;;;;;;
;;; trap
;;;;;;;;;

(def-simple-register *trap1* *machine-regs* 0)
(def-simple-register *trap2* *machine-regs* (if (zerop *trap1*) 0 1))
(def-simple-register *trap3* *machine-regs* (if (zerop *trap2*) 0 1))

(defun cause-trap (bit)
  (setq *trap* (dpb 1. bit *trap*))
  (maybe-enter-trap-sequence))

(defvar *left-freeze*  0)
(defvar *right-freeze* 0)
(defvar *oreg-freeze*  0)
(defvar *alu-freeze*   0)
(defvar *jstat-freeze* 0)

(defun maybe-enter-trap-sequence ()
  (when (processor-traps?)
    (ferror nil "Foo")
    (setq *trap1* 1.)
    (setq *trap2* 1.)
    (setq *trap3* 1.)
    (setq *left-freeze*  1.)
    (setq *right-freeze* 1.)
    (setq *oreg-freeze*  1.)
    (setq *alu-freeze*   1.)
    (setq *jstat-freeze* 1.)
    (setq *opc-freeze*   1.)
    (setq *memory-control* (32-dpb 0. %%k-memory-control-master-trap-enable *memory-control*))
    (setq *trap-mask* (aref *trap-masks* (32-ldb *memory-control* %%k-memory-control-trap-bits 0)))
    (throw 'recompute-inputs nil)))

(defun new-k-write-memory-control (value)
  (setq *memory-control* (datum-word value))
  (setq *trap-mask* (aref *trap-masks* (32-ldb *memory-control* %%k-memory-control-trap-bits 0)))
  (maybe-enter-trap-sequence))

(defun new-reset-memory-control ()
  (setq *trap* 0.)
  (new-k-write-memory-control
    (make-datum $$unboxed
    (32-dpb $$trap-disabled %%k-memory-control-single-step-enable
    (32-dpb $$trap-disabled  %%k-memory-control-synchronous-trap-enable
    (32-dpb $$trap-disabled  %%k-memory-control-datatype-trap-enable   
    (32-dpb $$trap-disabled  %%k-memory-control-overflow-trap-enable   
    (32-dpb $$trap-disabled  %%k-memory-control-asynchronous-trap-enable
    (32-dpb $$trap-disabled  %%k-memory-control-master-trap-enable
	   0.)))))))))

;;;;;;;;;;;
;;; PC+1
;;;;;;;;;;;

(def-simple-register *pc+1* *machine-regs*
  (assure-pc-selected)
  (1+ *pc*))

;;;;;;;;;;;
;;; PC mux
;;;;;;;;;;;

(defvar *g-lo*   0)
(defvar *g-hi*   0)
(defvar *s-hi*  0)
(defvar *s-lo*  0)
(defvar *ir2mfo* 0)

(defmacro pal-equations (&rest stuff)
  `(MACROLET ((* (a b &rest c) (if (null c)
				   `(LOGAND ,a ,b)
				   `(LOGAND ,a (* ,b ,@c))))
	      (+ (a b &rest c) (if (null c)
				   `(LOGIOR ,a ,b)
				   `(LOGIOR ,a (+ ,b ,@c))))
	      (/ (term) `(LOGNOT ,term)))
    ,@stuff))

(defun pcmux-pal ()
  (macrolet ((ir (n) `(LDB (BYTE 1. ,n) *IREG*)))
    (pal-equations
      (setq *g-lo*			
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (ir 59))
		    (* (/ *trap1*) (ir 58))
		    (* (/ *trap1*) (ir 57))
		    (* (/ *trap1*) (/ (ir 59)) (/ (ir 58)) (/ (ir 57))    (ir 56)  (/ (ir 49)) (/ (ir 61)))
		    (* (/ *trap1*) (/ (ir 59)) (/ (ir 58)) (/ (ir 57))    (ir 56)     (ir 49) (ir 3))
		    (* (/ *trap1*) (/ (ir 59)) (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49))))))
      (setq *g-hi*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (ir 59))
		    (* (/ *trap1*) (ir 58))
		    (* (/ *trap1*) (ir 57))
		    (* (/ *trap1*) (ir 56))
		    (* (/ *trap1*) (/ (ir 59)) (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49))))))
      (setq *s-hi*
	    (dpb
	      (/
		(+ (* (/ (ir 57)) (ir 60))
		   (* (/ (ir 57)) (ir 58))
		   (* (/ (ir 57)) (ir 56))
		   (* (/ (ir 57)) (ir 49))
		   (* (/ (ir 56)) (ir 61) (/ (ir 60)) (ir 59)
		      (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49)))
		   (* (/ (ir 61)) (/ (ir 60)) (ir 59) (/ (ir 58))
		      (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*))
		)
	      (byte 1. 1.)
	      (ldb (byte 1. 0.)
	      (/
		(+ (* (/ (ir 56)) (ir 60))
		   (* (/ (ir 56)) (ir 58))
		   (* (/ (ir 56)) (ir 57))
		   (* (/ (ir 56)) (ir 49))
		   (* (/ (ir 56)) (ir 61) (/ (ir 60)) (ir 59)
		      (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49)))
		   (* (/ (ir 61)) (/ (ir 60)) (ir 59) (/ (ir 58))
		      (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*))
		)
	      )))
      (setq *s-lo*
	    (dpb
	      (/
		(+ (* (/ (ir 57)) (ir 61))	;s1-lo
		   (* (/ (ir 57)) (ir 60))
		   (* (/ (ir 57)) (ir 58))
		   (* (/ (ir 57)) (ir 56))
		   (* (/ (ir 57)) (ir 49))
		   (* (/ (ir 61)) (/ (ir 59)) (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*)
		   (* (/ (ir 61)) (/ (ir 60)) (ir 59) (/ (ir 58))
		      (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*))
		)
	      (byte 1. 1.)
	      (ldb (byte 1. 0.)
	      (/
		(+ (* (/ (ir 56)) (ir 61))	;s0-lo
		   (* (/ (ir 56)) (ir 60))
		   (* (/ (ir 56)) (ir 58))
		   (* (/ (ir 56)) (ir 57))
		   (* (/ (ir 56)) (ir 49))
		   (* (/ (ir 61)) (/ (ir 59)) (/ (ir 58)) (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*)
		   (* (/ (ir 61)) (/ (ir 60)) (ir 59) (/ (ir 58))
		      (/ (ir 57)) (/ (ir 56)) (/ (ir 49)) *jump*))
		)
		)))
      (setq *ir2mfo*
	    (+ (* (/ (ir 59)) (ir 58))		;ir2mfo
	       (* (/ (ir 60)) (ir 59) (ir 58))
	       *trap1*)))))

(deff assure-pc-mux-decoded (memoized 'pcmux-pal))

(defun select-next-pc ()
  (assure-pc-mux-decoded)
;  (pcmux-pal)
;  (format t "~&Current PC -> ~d" *pc*)
;  (format t "~&Jump is ~b" *jump*)
;  (format t "~&Selecting PC from ~[ireg~;oreg~;retpc~;pc+1~]" *s-hi*)
;  (format t "~&Should be  ~[ireg~;oreg~;retpc~;pc+1~]" (ldb (byte 2. 58.) *ireg*))
;  (format t "~&Ireg is ~16,'0x" *ireg*)
  (setq *pc*
	(ldb (byte 12. 0.)
	     (dispatch (byte 2. 0.) *s-hi*
		       (0 *IREG*)	
		       (1 *OREG*)	
		       (2 *RETPC*)	
		       (3 *PC+1*))))
  (setq *pc*
	(dpb 
	  (ldb (byte 12. 12.)
	       (dispatch (byte 2. 0.) *s-lo*
		 (0 *IREG*)
		 (1 *OREG*)
		 (2 *RETPC*)
		 (3 *PC+1*)))
	    (byte 12. 12.) *pc*))
  (when (not (zerop *trap1*)) (setq *PC* (dpb 0. (byte 8. 4.) *PC*)))
  (when (zerop *g-lo*) (setq *PC* (dpb 0. (byte 4. 0.) *PC*)))
  (when (zerop *g-hi*) (setq *PC* (dpb 0. (byte 12. 12.) *PC*)))
;  (when (zerop *pc*) (ferror nil "gak"))
;  (format t "~&PC is now ~d" *pc*)
  )

(deff assure-pc-selected (memoized 'select-next-pc))

;;;;;;;;;
;;; IREG
;;;;;;;;;

(defvar *spy-ireg-low*  nil)
(defvar *spy-ireg-high* nil)

(def-simple-register *ireg* *machine-regs*
  (assure-pc-selected)
  (prog1 (if (and (numberp *spy-ireg-low*)
		  (numberp *spy-ireg-high*))
	     (logior (logand *spy-ireg-low* #x00000000FFFFFFFF)
		     (logand (ash *spy-ireg-high* 32.) #xFFFFFFFF00000000))
	     ;; We are coming from the PROM or from virtual memory.
	     (if (= (ldb %%k-memory-control-bootprom-disable *memory-control*) $$bootprom-on)
		 (let ((inst (read-bootprom)))
						;(format t "~&~s" (nc::dis inst))
		   inst)
		 (let* ((virtual-address (dpb 1. (byte 1. 26.) (ash *pc* 1.)))
			(map-data        (micro::read-memory-map virtual-address)))
		   (if (= $$can-read
			  (if (= (ldb %%k-memory-control-low-high-map-select *memory-control*)
				 $$low-trap-bits)
			      (ldb %%k-map-low-valid-bit  map-data)
			      (ldb %%k-map-high-valid-bit map-data)))
		       (if (= $$cluster-non-local-memory (ldb %%k-map-local-memory-bit *map-bits*))
			   (broken-simulation "~&Can't hack non local memory yet.")
			   (let ((address (+ (ash (ldb %%k-map-on-board-address *map-bits*)
						  (byte-size %%k-unmapped-vma-byte))
					     (ldb %%k-unmapped-vma-byte virtual-address))))
			     (+ (phys-mem-read address)
				(ash (phys-mem-read (1+ address)) 32.))))
		       ;; Here if instruction faults.
		       (progn (cause-trap %%k-trap-icache-map-fault)
			      (broken-simulation "IREG map fault didn't cause trap!"))))))
	 (setq *spy-ireg-low* nil)
	 (setq *spy-ireg-high* nil)))

;;;;;;;;;;;;;
;;; Bootprom
;;;;;;;;;;;;;

(defun read-bootprom ()
  (let ((address (ash *pc* 1.)))
    (+ (ash (micro::read-bootprom (+ address 1.)) 32.)
       (micro::read-bootprom address))))


;;;;;;;;;;;;;;;
;;; Passaround
;;;;;;;;;;;;;;;

(defvar *passr0* 0)
(defvar *passr1* 0)
(defvar *passl0* 0)
(defvar *passl1* 0)

(defun compute-passaround ()
  (assure-dest-computed)
  (if (or (not (zerop *trap3*))
	  (zerop *regdest*))
      (progn (setq *passr0* 0)
	     (setq *passr1* 0)
	     (setq *passl0* 0)
	     (setq *passl1* 0))
      (progn
	(assure-ladr-computed)
	(assure-radr-computed)
;	(format t "~&Ladr= ~d; Radr= ~d; Dest= ~d" *ladr* *radr* *d-dest-adr*)
	(if (= *ladr* *d-dest-adr*)
	    (progn (setq *passl0* 1.)
		   (setq *passl1* 1.))
	    (progn (setq *passl0* 0.)
		   (setq *passl1* 0.)))
	(if (= *radr* *d-dest-adr*)
	    (progn (setq *passr0* 1.)
		   (setq *passr1* 1.))
	    (progn (setq *passr0* 0.)
		   (setq *passr1* 0.))))))

(deff assure-passaround-computed (memoized 'compute-passaround))

;;;;;;;;;;;
;;; Reg Pal
;;;;;;;;;;;

(defvar *lregsel* 0)
(defvar *rregsel* 0)
(defvar *mfimuxs* 0)
(defvar *isel1*   0)
(defvar *isel2*   0)
(defvar *ialu*    0)
(defvar *i2264*   0)
(defvar *i2265*   0)

(defun compute-reg-pal-output ()
  (assure-passaround-computed)
  (macrolet ((ir (n) `(LDB (BYTE 1. ,n) *IREG*)))
    (pal-equations
      (setq *lregsel*
	    (ldb (byte 1. 0.)
		 (+ (/ *passl0*) (/ *passl1*))))
      (setq *rregsel*
	    (ldb (byte 1. 0.)
		 (+ (* (/ (ir 59)) (/ (ir 58)) (/ (ir 31)) (/ *passr0*))
		    (* (/ (ir 59)) (/ (ir 58)) (/ (ir 31)) (/ *passr1*))
		    (* (/ (ir 60))    (ir 59)  (/ (ir 58)) (/ (ir 31)) (/ *passr0*))
		    (* (/ (ir 60))    (ir 59)  (/ (ir 58)) (/ (ir 31)) (/ *passr1*))
		    (*    (ir 60)     (ir 59)              (/ (ir 31)) (/ *passr0*))
		    (*    (ir 60)     (ir 59)              (/ (ir 31)) (/ *passr1*)))))
      (setq *mfimuxs*
	    (ldb (byte 1. 0.)
		 (+ (* (/ (ir 59)) (/ (ir 58)) (/ (ir 31))             *passr0* *passr1*)
		    (* (/ (ir 60))    (ir 59)  (/ (ir 58)) (/ (ir 31)) *passr0* *passr1*)
		    (*    (ir 60)     (ir 59)              (/ (ir 31)) *passr0* *passr1*))))
      (setq *isel2*
	    (/ (+ (/ (ir 59)) (* (ir 60) (ir 59)))))
      (setq *isel1* (* (/ (ir 59)) (ir 58)))
      (setq *ialu* (+ (/ (ir 60)) (/ (ir 59))))
      (setq *i2264* (+ (/ (ir 60)) (/ (ir 59)) (/ (ir 58))))
      (setq *i2265* (+ (/ (ir 60)) (/ (ir 59)) (ir 58))))))

(deff assure-reg-pal-computed (memoized 'compute-reg-pal-output))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global frame number
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *global-frame-number* 0)

(defun compute-global-frame-number ()
  (setq *global-frame-number* (ldb (byte 4. 37.) *ireg*)))

(deff assure-global-frame-number-computed  (memoized 'compute-global-frame-number))

;;;;;;;;;;;;;;;;;;
;;; LADR and RADR
;;;;;;;;;;;;;;;;;;

(defvar *LADR* 0)
(defvar *RADR* 0)

(defun compute-ladr ()
  (setq *ladr*
	(dispatch (byte 2. 23.) *ireg*
	  (0 (dpb *OPEN*   (byte 8. 4.) (ldb (byte 4. 19.) *ireg*)))
	  (1 (dpb *ACTIVE* (byte 8. 4.) (ldb (byte 4. 19.) *ireg*)))
	  (2 (dpb *RETURN* (byte 8. 4.) (ldb (byte 4. 19.) *ireg*)))
	  (3 (assure-global-frame-number-computed)
	     (dpb *global-frame-number* (byte 4. 4.) (ldb (byte 4. 19.) *ireg*))))))

(deff assure-ladr-computed (memoized 'compute-ladr))

(defun compute-radr ()
  (setq *radr*
	(dispatch (byte 2. 29.) *ireg*
	  (0 (dpb *OPEN*   (byte 8. 4.) (ldb (byte 4. 25.) *ireg*)))
	  (1 (dpb *ACTIVE* (byte 8. 4.) (ldb (byte 4. 25.) *ireg*)))
	  (2 (dpb *RETURN* (byte 8. 4.) (ldb (byte 4. 25.) *ireg*)))
	  (3 (assure-global-frame-number-computed)
	     (dpb *global-frame-number* (byte 4. 4.) (ldb (byte 4. 25.) *ireg*))))))

(deff assure-radr-computed (memoized 'compute-radr))

;;;;;;;;;;;;;;;;
;;; Destination
;;;;;;;;;;;;;;;;

(defvar *irdest*  0)

(defvar *frdest*  0)

(defvar *regdest* 0)

(defvar *d-dest-adr* 0)

(defun compute-irdest ()
  (setq *irdest* (ldb (byte 7. 41.) *ireg*)))

(deff assure-irdest-computed (memoized 'compute-irdest))

(def-simple-register *dirdest* *machine-regs*
  (assure-irdest-computed)
  *irdest*)

(def-simple-register *drdest* *machine-regs*
  *rdest*)

(defun compute-frdest ()
  (assure-irdest-computed)
  (setq *frdest* (if (= *irdest* #x41) 1. 0.)))

(deff assure-frdest-computed (memoized 'compute-frdest))

(def-simple-register *dfrdest* *machine-regs*
  (assure-frdest-computed)
  *frdest*)

(def-simple-register *d-global-frame-number* *machine-regs*
  *global-frame-number*)

(def-simple-register *dr-global-frame-number* *machine-regs*
  ;(assure-return-global-frame-number-computed)
  ;*return-global-frame-number*
  0)

(defun compute-dest-bits ()
  (let ((destadr (if (zerop *dfrdest*)
		     *dirdest*
		     (dpb (if (ldb-test (byte 1. 6.) *drdest*)
			      0
			      (ldb (byte 2. 4.) *drdest*))
			  (byte 2. 4.)
			  (ldb (byte 4. 0.) *drdest*)))))
    (setq *regdest* (if (zerop (ldb (byte 1. 6.) destadr)) 1. 0.))
    (setq *d-dest-adr*
	  (dispatch (byte 2. 4.) destadr
	    (0 (dpb *OPEN* (byte 8. 4.) destadr))
	    (1 (dpb *ACTIVE* (byte 8. 4.) destadr))
	    (2 (dpb *RETURN* (byte 8. 4.) destadr))
	    (3 (if (zerop *dfrdest*)
		   (dpb *d-global-frame-number* (byte 4. 4.) destadr)
		   (dpb *dr-global-frame-number* (byte 4. 4.) destadr)))))))

(deff assure-dest-computed (memoized 'compute-dest-bits))

(def-simple-register *dregdest* *machine-regs*
  (assure-dest-computed)
  *regdest*)

(def-simple-register *d-d-dest-adr* *machine-regs*
  (assure-dest-computed)
  *d-dest-adr*)

;;;;;;;;;;;;;;;;;;
;;; Register File
;;;;;;;;;;;;;;;;;;

(defun read-register-file (address)
  (values (ldb (byte 1. 0.) (micro::read-register-file (* address 2.)))
	  (micro::read-register-file (1+ (* address 2.)))))

(defun write-register-file (boxed-bit data address)
  (micro::write-register-file (ash address 1.) (ldb (byte 1. 0.) boxed-bit))
  (micro::write-register-file (1+ (ash address 1.)) data))

(defun register-file-write-cycle ()
  (when (and (zerop *trap3*)
	     (zerop (ldb (byte 1. 6.) *fdest*)))
    (write-register-file *oreg-boxed* *oreg* *d-d-dest-adr*)))

;;;;;;;;;;;;
;;; L and R
;;;;;;;;;;;;

(defvar *l*       0)
(defvar *l-boxed* 0)
(defvar *r*       0)
(defvar *r-boxed* 0)

(defun compute-l ()
  (if (zerop *left-freeze*)
      (progn (assure-reg-pal-computed)
	     (if (zerop *lregsel*)
		 (progn (assure-alu-output-computed)
			(list *alu-boxed* *alu-output*))
		 (progn (assure-ladr-computed)
			(multiple-value-list (read-register-file *ladr*)))))
      (list *l-boxed* *l*)))

(defun output-l (o)
  (setq *l-boxed* (first o))
  (setq *l*       (second o)))

(defun compute-r ()
  (if (zerop *right-freeze*)
      (progn (assure-reg-pal-computed)
;	     (format t "~&Right: ~7,'0b" (ldb hw:%%inst-alu-right-source *ireg*))
	     (if (not (zerop *rregsel*))
		 (progn (assure-radr-computed)
			(multiple-value-list (read-register-file *radr*)))
		 (if (zerop *mfimuxs*)
		     (progn (assure-mfi-computed)
			    (list *mfi-boxed* *mfi-bus*))
		     (progn (assure-alu-output-computed)
			    (list *alu-boxed* *alu-output*)))))
      (list *r-boxed* *r*)))

(defun output-r (o)
  (setq *r-boxed* (first o))
  (setq *r*       (second o)))

(eval-when (load)
  (make-machine-register 'l 'compute-l 'output-l *machine-regs*)
  (make-machine-register 'r 'compute-r 'output-r *machine-regs*))

;;;;;;;;;
;;; OREG
;;;;;;;;;

(defvar *oreg* 0)
(defvar *oreg-boxed* 0)

(defun compute-oreg ()
  (assure-alu-output-computed)			;Force alu computation.
  (assure-alu-boxed-computed)
  (if (zerop *oreg-freeze*)
      (list *alu-boxed* *alu-output*)
      (list *oreg-boxed* *oreg*)))

(defun output-oreg (o)
;  (format t "~&OREG has ~8,'0x" (second o))
  (setq *oreg-boxed* (first o))
  (setq *oreg* (second o)))

(eval-when (load)
  (make-machine-register 'oreg 'compute-oreg 'output-oreg *machine-regs*))

;;;;;;;;
;;; ALU
;;;;;;;;

(def-simple-register *alu-instruction* *machine-regs*
  (assure-reg-pal-computed)
  (let ((output
	  (if (zerop *isel2*)
	      (if (zerop *isel1*)
		  (ldb hw:%%inst-alu-alu-operation *ireg*)
		  (ldb hw:%%inst-alu-16i-alu-operation *ireg*))
	      1.)))
    ;(format t "~16,'0x  ~2,'0x" *ireg* output)
    output))

(def-simple-register *alu-shift* *machine-regs*
  (ldb hw:%%inst-alu-alu-shift *ireg*))

(def-simple-register *alu-mask* *machine-regs*
  (ldb hw:%%inst-alu-alu-mask *ireg*))

(def-simple-register *alu-bw* *machine-regs*
  (assure-reg-pal-computed)
  (if (zerop *isel2*)
      (ldb hw:%%inst-alu-byte-width *ireg*)
      0.))

(defvar *alu-output* 0)
(defvar *alu-status* 0)
(defvar *alu-boxed*  0)

(defvar *alu-handlers* (make-array (ash 1. 7.)))

(defun compute-alu-result ()
  (setq *alu-output*
	(let ((operation (aref *alu-handlers* *alu-instruction*)))
	  (format t "~&~%Alu operation is (~s ~x ~x)" operation *l* *r*)
	  (let ((result (funcall operation *l* *r* *alu-bw* *alu-shift* *alu-mask*)))
	    (micro::show-status-register)
	    (format t "<- status")
	    result))))

	(funcall
	  (aref *alu-handlers* *alu-instruction*)
	  *l* *r* *alu-bw* *alu-shift* *alu-mask*)))

(deff assure-alu-output-computed (memoized 'compute-alu-result))

(defmacro define-alu-operation (number name blist &body body)
  `(PROGN (DEFUN ,name ,blist ,@body)
	  (EVAL-WHEN (LOAD)
	    (SETF (AREF *ALU-HANDLERS* ,number) (QUOTE ,name)))))

(defmacro define-unimplemented-alu-operation (number name)
  `(DEFINE-ALU-OPERATION ,number ,name (&REST ARGS)
     ARGS
     (BROKEN-SIMULATION "~&ALU operation ~s (~d) is not implemented" (QUOTE ,name) ,number)))

(define-alu-operation hw:$$inst-alu-op-zero-ext-left zero-ext-left (left right bw shift mask)
  (declare (ignore right shift mask))
  (micro::zero-ext bw left))

(define-alu-operation hw:$$inst-alu-op-zero-ext-right zero-ext-right (left right bw shift mask)
  (declare (ignore left shift mask))
  (micro::zero-ext bw right))

(define-alu-operation hw:$$inst-alu-op-pass-stat pass-stat (left right bw shift mask)
  (declare (ignore left shift mask))
  (micro::pass-stat bw right))

(define-alu-operation hw:$$inst-alu-op-prior-left prior-left (left right bw shift mask)
  (declare (ignore right shift mask))
  (micro::prioritize bw left))

(define-alu-operation hw:$$inst-alu-op-prior-right prior-right (left right bw shift mask)
  (declare (ignore left shift mask))
  (micro::prioritize bw right))

(define-alu-operation hw:$$inst-alu-op-ext-bit-from-left ext-bit-from-left (left right bw shift mask)
  (declare (ignore right mask))
  (micro::ext-bit bw shift left))

(define-alu-operation hw:$$inst-alu-op-ext-bit-from-right ext-bit-from-right (left right bw shift mask)
  (declare (ignore left mask))
  (micro::ext-bit bw shift right))

(define-alu-operation hw:$$inst-alu-op-set-bit-left set-bit-left (left right bw shift mask)
  (declare (ignore right mask))
  (micro::set-bit bw shift left))

(define-alu-operation hw:$$inst-alu-op-set-bit-right set-bit-right (left right bw shift mask)
  (declare (ignore left mask))
  (micro::set-bit bw shift right))

(define-alu-operation hw:$$inst-alu-op-and-f-left and-f-left (left right bw shift mask)
  (micro::non-aligned-logical-op
    tv::alu-and left right shift mask bw))

;(define-unimplemented-alu-operation hw:$$inst-alu-op-zero-ext-left zero-ext-left)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-zero-ext-right zero-ext-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sign-ext-left sign-ext-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sign-ext-right sign-ext-right)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-stat pass-stat)
(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-q pass-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-load-q-left load-q-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-load-q-right load-q-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-merge-right-left merge-right-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-merge-left-right merge-left-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ld-stat-left ld-stat-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ld-stat-right ld-stat-right)

;;; group 2: byte boundary aligned operand - logical
(define-unimplemented-alu-operation hw:$$inst-alu-op-not-left not-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-not-right not-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-or or)
(define-unimplemented-alu-operation hw:$$inst-alu-op-xor xor)
(define-unimplemented-alu-operation hw:$$inst-alu-op-and and)
(define-unimplemented-alu-operation hw:$$inst-alu-op-xnor xnor)
(define-unimplemented-alu-operation hw:$$inst-alu-op-zero zero)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sign sign)

;;; group 3: byte boundary aligned operand - single bit shifts
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-0f-left dnl-0f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-0f-right dnl-0f-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-1f-left dnl-1f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-1f-right dnl-1f-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-lf-left dnl-lf-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-lf-right dnl-lf-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-ar-left dnl-ar-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-ar-right dnl-ar-right)

;;; single bit downshift (double precision)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-0f-left-q dnl-0f-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-0f-right-q dnl-0f-right-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-1f-left-q dnl-1f-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-1f-right-q dnl-1f-right-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-lf-left-q dnl-lf-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-lf-right-q dnl-lf-right-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-ar-left-q dnl-ar-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-dnl-ar-right-q dnl-ar-right-q)

;;; single bit upshift (single-precision)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-0f-left upl-0f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-0f-right upl-0f-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-1f-left upl-1f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-1f-right upl-1f-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-lf-left upl-lf-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-lf-right upl-lf-right)

;;; single bit upshift (double precision)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-0f-left-q upl-0f-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-0f-right-q upl-0f-right-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-1f-left-q upl-1f-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-1f-right-q upl-1f-right-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-lf-left-q upl-lf-left-q)
(define-unimplemented-alu-operation hw:$$inst-alu-op-upl-lf-right-q upl-lf-right-q)

;;; group 4: byte boundary aligned operand - arithmetic
;(define-unimplemented-alu-operation hw:$$inst-alu-op-prior-left prior-left)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-prior-right prior-right)

;;; group 5: byte boundary aligned operand - arithmetic
(define-unimplemented-alu-operation hw:$$inst-alu-op-neg-left neg-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-neg-right neg-right)

(define-unimplemented-alu-operation hw:$$inst-alu-op-decr1-left decr1-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-decr1-right decr1-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-decr2-left decr2-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-decr2-right decr2-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-decr4-left decr4-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-decr4-right decr4-right)

(define-unimplemented-alu-operation hw:$$inst-alu-op-incr1-left incr1-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-incr1-right incr1-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-incr2-left incr2-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-incr2-right incr2-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-incr4-left incr4-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-incr4-right incr4-right)

(define-unimplemented-alu-operation hw:$$inst-alu-op-add add)
(define-unimplemented-alu-operation hw:$$inst-alu-op-addc addc)

(define-alu-operation hw:$$inst-alu-op-sub sub (left right bw shift mask)
  (declare (ignore shift mask))
  (micro::sub bw left right))

;(define-unimplemented-alu-operation hw:$$inst-alu-op-sub sub)
(define-unimplemented-alu-operation hw:$$inst-alu-op-subr subr)
(define-unimplemented-alu-operation hw:$$inst-alu-op-subc subc)
(define-unimplemented-alu-operation hw:$$inst-alu-op-subrc subrc)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sum-corr-left sum-corr-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sum-corr-right sum-corr-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-diff-corr-left diff-corr-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-diff-corr-right diff-corr-right)

;;; group 6: byte boundary aligned operands - division steps
(define-unimplemented-alu-operation hw:$$inst-alu-op-sdiv-first sdiv-first)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sdiv-step sdiv-step)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sdiv-last1 sdiv-last1)
(define-unimplemented-alu-operation hw:$$inst-alu-op-sdiv-last2 sdiv-last2)
(define-unimplemented-alu-operation hw:$$inst-alu-op-udiv-first udiv-first)
(define-unimplemented-alu-operation hw:$$inst-alu-op-udiv-step udiv-step)
(define-unimplemented-alu-operation hw:$$inst-alu-op-udiv-last1 udiv-last1)
(define-unimplemented-alu-operation hw:$$inst-alu-op-remcorr remcorr)
(define-unimplemented-alu-operation hw:$$inst-alu-op-quocorr quocorr)
(define-unimplemented-alu-operation hw:$$inst-alu-op-mp-div-step1 mp-div-step1)
(define-unimplemented-alu-operation hw:$$inst-alu-op-mp-div-step2 mp-div-step2)
(define-unimplemented-alu-operation hw:$$inst-alu-op-mp-s-div-step3 mp-s-div-step3)
(define-unimplemented-alu-operation hw:$$inst-alu-op-mp-u-div-step3 mp-u-div-step3)
(define-unimplemented-alu-operation hw:$$inst-alu-op-umul-first umul-first)
(define-unimplemented-alu-operation hw:$$inst-alu-op-umul-step umul-step)
(define-unimplemented-alu-operation hw:$$inst-alu-op-umul-last umul-last)
	;;; there might be something funny here, these have switched opcode order from the unsigned case
(define-unimplemented-alu-operation hw:$$inst-alu-op-smul-first smul-first)
(define-unimplemented-alu-operation hw:$$inst-alu-op-smul-step smul-step)

;;; group 8: n bit shifts and rotates
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-sn-sh-left nb-sn-sh-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-sn-sh-right nb-sn-sh-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-0f-sh-left nb-0f-sh-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-0f-sh-right nb-0f-sh-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-rot-left nb-rot-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-nb-rot-right nb-rot-right)

;;; group 9: variable length bit field, single bit
;(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-bit-from-left ext-bit-from-left)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-bit-from-right ext-bit-from-right)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-set-bit-left set-bit-left)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-set-bit-right set-bit-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-rst-bit-left rst-bit-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-rst-bit-right rst-bit-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-set-bit-stat set-bit-stat)
(define-unimplemented-alu-operation hw:$$inst-alu-op-rst-bit-stat rst-bit-stat)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-bit-stat ext-bit-stat)

;;; group 10: variable length bit field - aligned
(define-unimplemented-alu-operation hw:$$inst-alu-op-not-f-al-left not-f-al-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-f-al-left pass-f-al-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-or-f-al-left or-f-al-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-xor-f-al-left xor-f-al-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-and-f-al-left and-f-al-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-not-f-al-right not-f-al-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-f-al-right pass-f-al-right)

;;; field logical - non-aligned
(define-unimplemented-alu-operation hw:$$inst-alu-op-not-f-left not-f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-f-left pass-f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-or-f-left or-f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-xor-f-left xor-f-left)
;(define-unimplemented-alu-operation hw:$$inst-alu-op-and-f-left and-f-left)

;;; field logical - extract
(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-f-left ext-f-left)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-f-right ext-f-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-f-left-right ext-f-left-right)
(define-unimplemented-alu-operation hw:$$inst-alu-op-ext-f-right-left ext-f-right-left)

;;; group 11: variable length bit field - mask generation
(define-unimplemented-alu-operation hw:$$inst-alu-op-pass-mask pass-mask)

;;; unused opcodes
(define-unimplemented-alu-operation hw:$$inst-alu-reserved-1e reserved-1e)
(define-unimplemented-alu-operation hw:$$inst-alu-reserved-1f reserved-1f)
(define-unimplemented-alu-operation hw:$$inst-alu-reserved-4c reserved-4c)
(define-unimplemented-alu-operation hw:$$inst-alu-reserved-4d reserved-4d)

(def-simple-register *alu-status-register* *machine-regs*
  (if (zerop *alu-freeze*)
      (progn (assure-alu-output-computed)
	     (let ((status (micro::read-status-register)))
	       ;(format t "Status is")
	       ;(micro::show-status-register)
	       (unless (or (zerop (ldb %%k-memory-control-overflow-trap-enable *memory-control*))
			   (= *fdest* #x42))	;noop no overflow
		 (when (not (zerop (ldb hw:$$alu-status-overflow status)))	;check no-overflow
		   (cause-trap %%k-trap-29332-overflow)))
	       status))
      *alu-status-register*))

(defun compute-alu-status ()
  (if (zerop (ldb (byte 1. 10.) *tsm-output*))
      (progn (assure-alu-output-computed)
	     (setq *alu-status* (micro::read-status-register)))
      (setq *alu-status* *alu-status-register*)))

(deff assure-alu-status-computed (memoized 'compute-alu-status))
  
(defun compute-alu-boxed ()
  (setq *alu-boxed*
	(if (zerop (ldb %%k-processor-control-box-mode *processor-control*))
	    (dispatch (byte 2. 0.) *box-code*
	      (0 *l-boxed*)
	      (1 *r-boxed*)
	      (2 0)
	      (3 1))
	    (dispatch (byte 2. 0.) *box-code*
	      (0 (ldb (byte 1. 0.) *oreg*))
	      (1 0)
	      (2 0)
	      (3 1)))))

(deff assure-alu-boxed-computed (memoized 'compute-alu-boxed))

;;;;;;;;;;;;;
;;; Box code
;;;;;;;;;;;;;

(def-simple-register *box-code* *machine-regs*
  (ldb hw:%%inst-boxed-result *ireg*))

;;;;;;;;;;;;;;;;;;;;
;;; Data type check
;;;;;;;;;;;;;;;;;;;;

(def-simple-register *delayed-data-type-code* *machine-regs*
  (ldb (byte 3. 51.) *ireg*))

(defun read-data-type-ram ()
  (aref *data-type-ram*
	(ldb (byte 1. 0.) *l-boxed*)
	(ldb (byte 6. 26.) *l*)
	(ldb (byte 1. 0.) *r-boxed*)
	(ldb (byte 6. 26.) *r*)
	(ldb (byte 3. 0.) *delayed-data-type-code*)))

(defun write-data-type-ram ()
  (setf (aref *data-type-ram*
	      (ldb (byte 1. 0.) *l-boxed*)
	      (ldb (byte 6. 26.) *l*)
	      (ldb (byte 1. 0.) *r-boxed*)
	      (ldb (byte 6. 26.) *r*)
	      (ldb (byte 3. 0.) *delayed-data-type-code*))
	(ldb %%k-processor-control-data-bit *processor-control*)))

(defun check-data-type ()
  (when (= *fdest* #x48)				;write-pulse
    (write-data-type-ram))
  (when (= 1. (read-data-type-ram))
    (cause-trap %%k-trap-datatype)))

(def-simple-register *data-type-force* *machine-regs*
  (check-data-type))

;;;;;;;;;;;;;;;;;;;
;;; Jump Condition
;;;;;;;;;;;;;;;;;;;

(def-simple-register *jump* *machine-regs*
  (if (zerop *jstat-freeze*)
      (dispatch (byte 3. 0.) (ldb hw:%%inst-alu-jump-condition-select *ireg* ) ;*jcond*
	(0 1)  ;; unconditional
	(1 (broken-simulation "Don't know what JINDR does."))
	(2 (assure-alu-status-computed) (ldb hw:$$alu-status-zero *alu-status*))
	(3 (assure-alu-status-computed) (ldb hw:$$alu-status-zero (lognot *alu-status*)))
	(4 (assure-alu-status-computed)
	   (cerror "foo" "~&Selecting Less than")
	   (let ((result (ldb (byte 1. 0.)
			      (logxor (ldb hw:$$alu-status-overflow *alu-status*)
				      (ldb hw:$$alu-status-negative *alu-status*)))))
	     (format t "Result is ~d" result)
	     result))
	(5 (assure-alu-status-computed)
	   (ldb (byte 1. 0.)
		(lognot
		  (logxor (ldb hw:$$alu-status-overflow *alu-status*)
			  (ldb hw:$$alu-status-negative *alu-status*)))))
	(6 (assure-alu-status-computed)
	   (ldb (byte 1. 0.)
		(lognot
		  (logior
		    (ldb hw:$$alu-status-zero *alu-status*)
		    (logxor (ldb hw:$$alu-status-overflow *alu-status*)
			    (ldb hw:$$alu-status-negative *alu-status*))))))
	(7 (assure-alu-status-computed)
	   (ldb (byte 1. 0.)
		(logior
		  (ldb hw:$$alu-status-zero *alu-status*)
		  (logxor (ldb hw:$$alu-status-overflow *alu-status*)
			  (ldb hw:$$alu-status-negative *alu-status*))))))
      (progn (format t "~&Jstat is frozen")*jump*)))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional Sources
;;;;;;;;;;;;;;;;;;;;;;;

(defvar *functional-sources* (make-array (ash 1. 6.)))

(defvar *mfi-boxed* 0)
(defvar *mfi-bus*   0)
(defvar *mfio-bus*  0)
(defvar *mfio-bus-requester* nil)
(defvar *mmfio-bus* 0)
(defvar *mmfio-bus-requester* nil)

(defvar *mfio-set-for-input?*  nil)
(defvar *mmfio-set-for-input?* nil)

(defun mfi (value ignore)
  (setq *mfio-set-for-input?* nil)
  (setq *mmfio-set-for-input?* nil)
  (setq *mfi-bus* value))

(defun mfio (value requester)
  (setq *mfio-bus-requester* requester)
  (setq *mfio-set-for-input?* t)
  (setq *mmfio-set-for-input?* nil)
  (setq *mfio-bus* value)
  (setq *mfi-bus*  value))

(defun mmfio (value requester)
  (setq *mmfio-bus-requester* requester)
  (setq *mfio-set-for-input?* nil)
  (setq *mmfio-set-for-input?* t)
  (setq *mmfio-bus* value)
  (setq *mfi-bus*   value))

(defun assure-mfi-computed ()
  (assure-pc-mux-decoded)
  (if (zerop *ir2mfo*)
    (funcall
      (let ((rsource (ldb hw:%%inst-alu-right-source *ireg*)))
	(if (zerop (ldb (byte 1. 6.) rsource))
	    (progn (setq *mfio-set-for-input?* nil)
		   (setq *mmfio-set-for-input?* nil)
		   (setq *mfio-bus-requester* nil)
		   (setq *mmfio-bus-requester* nil)
		   #'ignore)
	    (progn ;(format t "~&FSOURCE: ~d" (ldb (byte 6. 0.) rsource))
	      (elt *functional-sources* (ldb (byte 6. 0.) rsource))))))
      (mfi (logand #xFFFFFFFF *ireg*) 'ireg)))

(defun assure-mfio-for-output ()
  (assure-mfi-computed)
  (if *mfio-set-for-input?*
      (broken-simulation "Input and output on MFIO!")
      (setq *mfio-bus* *mfo-bus*)))

(defun assure-mmfio-for-output ()
  (assure-mfi-computed)
  (if *mmfio-set-for-input?*
      (broken-simulation "Input and output on MMFIO!")
      (setq *mmfio-bus* *mfo-bus*)))

(defmacro define-boxed-functional-source (number name bus &body body)
  (let ((name (intern (string-append "FSOURCE-" (string name)))))
    `(PROGN
       (DEFUN ,name ()
	 (MULTIPLE-VALUE-BIND (BOXED-BIT OUTPUT)
	     (PROGN ,@body)
	   (SETQ *MFI-BOXED* BOXED-BIT)
	   (,bus OUTPUT (QUOTE ,name))))
       (EVAL-WHEN (LOAD)
	 (SETF (AREF *FUNCTIONAL-SOURCES* ,number) (QUOTE ,name))))))

(defmacro define-functional-source (number name bus &body body)
  `(DEFINE-BOXED-FUNCTIONAL-SOURCE ,number ,name ,bus
     (VALUES 0. (PROGN ,@body))))

(defmacro define-unimplemented-functional-source (number name bus)
  `(DEFINE-FUNCTIONAL-SOURCE ,number ,name ,bus
     (BROKEN-SIMULATION "~&Functional source ~s (~d) not implemented." (QUOTE ,name) ,number)))
  
(define-functional-source #b000000 unused-0 mfi 0)
(define-functional-source #b000001 unused-1 mfi 0)
(define-unimplemented-functional-source #b000010 unused-2 mfi)
(define-unimplemented-functional-source #b000011 disable-traps mfi)
(define-unimplemented-functional-source #b000100 read-cache-b-lo mfi)
(define-unimplemented-functional-source #b000101 read-cache-a-lo mfi)
(define-unimplemented-functional-source #b000110 read-cache-b-hi mfi)
(define-unimplemented-functional-source #b000111 read-cache-a-hi mfi)

(define-functional-source #b001000 processor-status mfio
  (assure-alu-boxed-computed)
  (dpb *alu-boxed* (byte 1. 18.)
       (dpb (lognot *jump*) (byte 1. 16.)
	    ;;; return destination global frame
	    *processor-eco-number*)))

(define-functional-source #b001001 processor-control mfio
  (32-ldb *processor-control* (byte 24. 0.) 0))

(define-functional-source #b001010 open-active-return mfio
  (dpb *open* (byte 8. 16.)
       (dpb *active* (byte 8. 8.)
	    (dpb *return* (byte 8. 0.)
		 0.))))
  
(define-unimplemented-functional-source #b001011 retpc-retdest mfio)
(define-unimplemented-functional-source #b001100 call-sp-and-hp mfio)
(define-functional-source #b001101 pc-2 mfio
  *pc-2*)
(define-functional-source #b001110 pc-1 mfio
  *pc-1*)
(define-unimplemented-functional-source #b001111 unused-15 mfio)
(define-unimplemented-functional-source #b010000 unused-16 mfi)
(define-unimplemented-functional-source #b010001 unused-17 mfi)
(define-unimplemented-functional-source #b010010 unused-18 mfi)
(define-unimplemented-functional-source #b010011 unused-19 mfi)
(define-unimplemented-functional-source #b010100 unused-20 mfi)
(define-unimplemented-functional-source #b010101 unused-21 mfi)
(define-unimplemented-functional-source #b010110 unused-22 mfi)
(define-unimplemented-functional-source #b010111 unused-23 mfi)
(define-unimplemented-functional-source #b011000 unused-24 mfi)
(define-unimplemented-functional-source #b011001 unused-25 mfi)
(define-unimplemented-functional-source #b011010 unused-26 mfi)
(define-unimplemented-functional-source #b011011 unused-27 mfi)
(define-unimplemented-functional-source #b011100 unused-28 mfi)
(define-unimplemented-functional-source #b011101 unused-29 mfi)
(define-unimplemented-functional-source #b011110 unused-30 mfi)
(define-unimplemented-functional-source #b011111 unused-31 mfi)
(define-unimplemented-functional-source #b100000 memory-maps mmfio)
(define-unimplemented-functional-source #b100001 gc-and-transporter mmfio)
(define-unimplemented-functional-source #b100010 memory-control mmfio)
(define-unimplemented-functional-source #b100011 microsecond-clock mmfio)
(define-unimplemented-functional-source #b100100 stat-counter mmfio)

(define-functional-source #b100101 trap-register mmfio
  *trap*)

(define-unimplemented-functional-source #b100110 memory-status mmfio)
(define-unimplemented-functional-source #b100111 unused-39 mmfio)
(define-unimplemented-functional-source #b101000 vma mfi)
(define-unimplemented-functional-source #b101001 gauche-vma mfi)
(define-unimplemented-functional-source #b101010 rmd mfi)
(define-unimplemented-functional-source #b101011 gauche-rmd mfi)
(define-unimplemented-functional-source #b101100 unused-44 mfi)
(define-unimplemented-functional-source #b101101 unused-45 mfi)
(define-unimplemented-functional-source #b101110 unused-46 mfi)
(define-unimplemented-functional-source #b101111 unused-47 mfi)
(define-unimplemented-functional-source #b110000 unused-48 mfi)
(define-unimplemented-functional-source #b110001 unused-49 mfi)
(define-unimplemented-functional-source #b110010 unused-50 mfi)
(define-unimplemented-functional-source #b110011 unused-51 mfi)
(define-unimplemented-functional-source #b110100 unused-52 mfi)
(define-unimplemented-functional-source #b110101 unused-53 mfi)
(define-unimplemented-functional-source #b110110 unused-54 mfi)
(define-unimplemented-functional-source #b110111 unused-55 mfi)
(define-unimplemented-functional-source #b111000 unused-56 mfi)
(define-unimplemented-functional-source #b111001 unused-57 mfi)
(define-unimplemented-functional-source #b111010 unused-58 mfi)
(define-unimplemented-functional-source #b111011 unused-59 mfi)
(define-unimplemented-functional-source #b111100 unused-60 mfi)
(define-unimplemented-functional-source #b111101 unused-61 mfi)
(define-unimplemented-functional-source #b111110 unused-62 mfi)
(define-unimplemented-functional-source #b111111 unused-63 mfi)

;;;;;;;;;;;
;;; MFOREG
;;;;;;;;;;;

(defvar *mfo-mux-select* 0)

(defun compute-mfo-mux ()
  (assure-early-dest-computed)
  (setq *mfo-mux-select* (ldb (byte 1. 0.) (logand *rregsel* *early-dest*))))

(def-simple-register *mfo-bus* *machine-regs*
  (compute-mfo-mux)
  (if (zerop *mfo-mux-select*)
      (progn (assure-alu-output-computed)
	     ;(format t "~&MFO set for ALU")
	     *alu-output*)
      (progn
	;(format t "~&MFO set for R")
	*r*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional Destinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *early-dest* 0)

(defun compute-early-dest ()
  (assure-irdest-computed)
  (setq *early-dest*
	(if (= (ldb (byte 4. 3.) *irdest*) 15.)
	    1.
	    0.)))

(deff assure-early-dest-computed (memoized 'compute-early-dest))

(def-simple-register *fdest-latch* *machine-regs*
  (assure-early-dest-computed)
  (logand *irdest* (dpb (lognot *early-dest*) (byte 1. 6.) *irdest*)))

(def-simple-register *fdest* *machine-regs*
  (if (zerop *trap2*)
      (progn (assure-future-fdest-computed)
	     *fdest-will-be*)
      0))

(defvar *fdest-will-be* 0)

(defun compute-future-fdest ()
  (assure-early-dest-computed)
  (if (zerop *early-dest*)
      (setq *fdest-will-be* *fdest-latch*)
      (progn (assure-irdest-computed)
	     (setq *fdest-will-be* *irdest*))))

(deff assure-future-fdest-computed (memoized 'compute-future-fdest))

(def-simple-register *dfdest* *machine-regs*
  *fdest*)

(defmacro define-functional-destination (name test if-selected if-not-selected)
  `(DEF-SIMPLE-REGISTER ,name *MACHINE-REGS*
     (ASSURE-FUTURE-FDEST-COMPUTED)
     (IF (FUNCALL ,test *FDEST-WILL-BE*)
	 (FUNCALL ,if-selected)
	 (FUNCALL ,if-not-selected))))

(defmacro define-unimplemented-functional-destination (name test)
  `(DEFINE-FUNCTIONAL-DESTINATION ,name ,test
     #'(lambda () (broken-simulation "Functional destination ~S not implemented." (QUOTE ,name)))
     #'(lambda () 0)))

(defun fdest-is (number)
  #'(lambda (fdest) (= fdest number)))

(defmacro define-functional-destination-write-enable (name test)
  `(DEFINE-functional-destination ,name ,test
     #'(lambda () 1)
     #'(lambda () 0)))

;;; Implemented specially
(define-functional-destination nop (fdest-is #b1000000)
  'assure-mmfio-for-output
  'ignore)

;(define-unimplemented-functional-destination #b000001 return-destination)
;(define-unimplemented-functional-destination #b000010 no-overflow)

(define-unimplemented-functional-destination fdest-unused-3 (fdest-is #b1000011))
(define-unimplemented-functional-destination fdest-unused-4 (fdest-is #b1000100))
(define-unimplemented-functional-destination fdest-unused-5 (fdest-is #b1000101))
(define-unimplemented-functional-destination fdest-unused-6 (fdest-is #b1000110))
(define-unimplemented-functional-destination fdest-unused-7 (fdest-is #b1000111))
(define-unimplemented-functional-destination fdest-datatype-write-pulse (fdest-is #b1001000))
(define-functional-destination-write-enable fdest-processor-control-register (fdest-is #b1001001))

(define-functional-destination-write-enable *write-o-a-r* (fdest-is #b1001010))
(define-unimplemented-functional-destination fdest-call-stack (fdest-is #b1001011))
(define-unimplemented-functional-destination fdest-call-hardware-stack-pointer-and-heap-pointer
					     (fdest-is #b1001100))
(define-unimplemented-functional-destination fdest-unused-13 (fdest-is #b1001101))
(define-unimplemented-functional-destination fdest-unused-14 (fdest-is #b1001110))
(define-unimplemented-functional-destination fdest-unused-15 (fdest-is #b1001111))
(define-unimplemented-functional-destination fdest-unused-16 (fdest-is #b1010000))
(define-unimplemented-functional-destination fdest-unused-17 (fdest-is #b1010001))
(define-unimplemented-functional-destination fdest-unused-18 (fdest-is #b1010010))
(define-unimplemented-functional-destination fdest-unused-19 (fdest-is #b1010011))
(define-unimplemented-functional-destination fdest-unused-20 (fdest-is #b1010100))
(define-unimplemented-functional-destination fdest-unused-21 (fdest-is #b1010101))
(define-unimplemented-functional-destination fdest-unused-22 (fdest-is #b1010110))
(define-unimplemented-functional-destination fdest-unused-23 (fdest-is #b1010111))
(define-unimplemented-functional-destination fdest-unused-24 (fdest-is #b1011000))
(define-unimplemented-functional-destination fdest-unused-25 (fdest-is #b1011001))
(define-unimplemented-functional-destination fdest-unused-26 (fdest-is #b1011010))
(define-unimplemented-functional-destination fdest-unused-27 (fdest-is #b1011011))
(define-unimplemented-functional-destination fdest-unused-28 (fdest-is #b1011100))
(define-unimplemented-functional-destination fdest-unused-29 (fdest-is #b1011101))
(define-unimplemented-functional-destination fdest-unused-30 (fdest-is #b1011110))
(define-unimplemented-functional-destination fdest-unused-31 (fdest-is #b1011111))
(define-unimplemented-functional-destination fdest-memory-map (fdest-is #b1100000))

(define-unimplemented-functional-destination fdest-gc-ram-write-pulse (fdest-is #b1100001))
(define-unimplemented-functional-destination fdest-memory-control-register (fdest-is #b1100010))
(define-unimplemented-functional-destination fdest-microsecond-clock (fdest-is #b1100011))
(define-unimplemented-functional-destination fdest-statistics-counter (fdest-is #b1100100))
(define-unimplemented-functional-destination fdest-transporter-ram-write-pulse (fdest-is #b1100101))
(define-unimplemented-functional-destination fdest-unused-38 (fdest-is #b1100110))
(define-unimplemented-functional-destination fdest-unused-39 (fdest-is #b1100111))
(define-unimplemented-functional-destination fdest-vma (fdest-is #b1101000))
(define-unimplemented-functional-destination fdest-gauche-vma (fdest-is #b1101001))
(define-unimplemented-functional-destination fdest-md (fdest-is #b1101010))
(define-unimplemented-functional-destination fdest-gauche-md (fdest-is #b1101011))
(define-unimplemented-functional-destination fdest-vma-start-write-no-gc-trap (fdest-is #b1101100))
(define-unimplemented-functional-destination fdest-vma-start-write (fdest-is #b1101101))
(define-unimplemented-functional-destination fdest-md-start-write-no-gc-trap (fdest-is #b1101110))
(define-unimplemented-functional-destination fdest-md-start-write (fdest-is #b1101111))
(define-unimplemented-functional-destination fdest-vma-start-read-no-transport (fdest-is #b1110000))
(define-unimplemented-functional-destination fdest-vma-start-read (fdest-is #b1110001))
(define-unimplemented-functional-destination fdest-vma-start-read-visible-evcp (fdest-is #b1110010))
(define-unimplemented-functional-destination fdest-vma-start-read-will-write (fdest-is #b1110011))
(define-unimplemented-functional-destination fdest-vma-start-read-cdr-no-transport (fdest-is #b1110100))
(define-unimplemented-functional-destination fdest-vma-start-read-cdr (fdest-is #b1110101))
(define-unimplemented-functional-destination fdest-vma-start-read-cdr-visible-evcp (fdest-is #b1110110))
(define-unimplemented-functional-destination fdest-vma-start-read-cdr-will-write (fdest-is #b1110111))
(define-unimplemented-functional-destination fdest-vma-start-read-early-no-transport (fdest-is #b1111000))
(define-unimplemented-functional-destination fdest-vma-start-read-early (fdest-is #b1111001))
(define-unimplemented-functional-destination fdest-vma-start-read-early-visible-evcp (fdest-is #b1111010))
(define-unimplemented-functional-destination fdest-vma-start-read-early-will-write (fdest-is #b1111011))
(define-unimplemented-functional-destination fdest-vma-start-read-early-cdr-no-transport (fdest-is #b1111100))
(define-unimplemented-functional-destination fdest-vma-start-read-early-cdr (fdest-is #b1111101))
(define-unimplemented-functional-destination fdest-vma-start-read-early-cdr-visible-evcp (fdest-is #b1111110))
(define-unimplemented-functional-destination fdest-vma-start-read-early-cdr-will-write (fdest-is #b1111111))

(def-simple-register *processor-control* *machine-regs*
  (if (= *fdest* #b1001001)
      (progn (assure-mfio-for-output)
	     (ferror nil "~&Writing processor control ~X" *mfio-bus*)
	     *mfio-bus*)
      *processor-control*))
      

;;;;;;;;;;;;;;;;;;;;;;;
;;; Call hardware pals
;;;;;;;;;;;;;;;;;;;;;;;

(defvar *write-o-a-r*         0)
(defvar *write-call-hardware* 0)
(defvar *write-return-pc-and-destination* 0)

(defvar *heap-pointer-decrement* 0)
(defvar *heap-pointer-count* 0)
(defvar *heap-write-enable-calc*   0)
(defvar *heap-pointer-previous-increment-calc* 0)
(defvar *heap-pointer-previous-decrement-calc* 0)
(defvar *return-frame-clock-enable* 0)
(defvar *return-frame-mux-select* 0)

(defun compute-ch-pal-1 ()
  (assure-frdest-computed)
  (pal-equations
    (macrolet ((ir (n) `(LDB (BYTE 1. ,n) *IREG*))
	       (rd (n) `(LDB (BYTE 1. ,n) *RDEST*)))
      (setq *heap-pointer-decrement*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)) (ir 48))	; open
		    (* (/ *trap1*) (/ (ir 50))    (ir 49)  (ir 48))	; open-call
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (ir 48))	; topen
		    (*    *trap1* *heap-pointer-previous-increment*))))
      (setq *heap-pointer-count*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)) (ir 48))	;open
		    (* (/ *trap1*) (/ (ir 50))    (ir 49)  (ir 48))	;open-call
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (ir 48))	;topen
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48))	;FRDEST normal
		       (/ *frdest*))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48))
		       *frdest* (/ (rd 6)))
		    (* *trap1* *heap-pointer-previous-decrement*)
		    (* *trap1* *heap-pointer-previous-increment*))))
      (setq *heap-write-enable-calc*
	    (ldb (byte 1. 0.)
		 (+ *trap1*
		    (* (/ *trap1*) (/ (ir 50)))
		    (* (/ *trap1*)    (ir 50) (ir 48))
		    (* (/ *trap1*)    (ir 50) (/ (ir 49)) (/ (ir 48)) *frdest* (rd 6)))))
      (setq *heap-pointer-previous-increment-calc*
	    (ldb (byte 1. 0.)
		 (* (/ *trap1*) *heap-pointer-count* (/ *heap-pointer-decrement*))))
      (setq *heap-pointer-previous-decrement-calc*
	    (ldb (byte 1. 0.)
		 (* (/ *trap1*) *heap-pointer-count* *heap-pointer-decrement*)))
      (setq *return-frame-clock-enable*
	    (ldb (byte 1. 0.)
		 (+ *write-o-a-r*
		    (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)))
		    (* (/ *trap1*) (ir 50)    (ir 49)  (/ (ir 48)))
		    (* (/ *trap1*) (ir 50)    (ir 49)     (ir 48))
		    *trap1*)))
      (setq *return-frame-mux-select*
	    (ldb (byte 1. 0.)
		 (+
		   (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)))
		   (* (/ *trap1*) (ir 50)    (ir 49)  (/ (ir 48)))
		   (* (/ *trap1*) (ir 50)    (ir 49)     (ir 48))))))))

(deff assure-ch-pal-1-computed (memoized 'compute-ch-pal-1))

(def-simple-register *heap-pointer-previous-increment* *machine-regs*
  (assure-ch-pal-1-computed)
  *heap-pointer-previous-increment-calc*)

(def-simple-register *heap-pointer-previous-decrement* *machine-regs*
  (assure-ch-pal-1-computed)
  *heap-pointer-previous-decrement-calc*)

(def-simple-register *heap-write-enable* *machine-regs*
  (assure-ch-pal-1-computed)
  *heap-write-enable-calc*)

(defvar *call-stack-pointer-decrement* 0)
(defvar *call-stack-pointer-count* 0)
(defvar *call-stack-oa-write-calc* 0)
(defvar *call-stack-return-pc-write-calc* 0)
(defvar *call-stack-previous-increment-calc* 0)
(defvar *call-stack-previous-decrement-calc* 0)
(defvar *open-frame-select* 0)

(defun compute-ch-pal-2 ()
  (assure-frdest-computed)
  (pal-equations
    (macrolet ((ir (n) `(LDB (BYTE 1. ,n) *IREG*))
	       (rd (n) `(LDB (BYTE 1. ,n) *RDEST*)))
      (setq *call-stack-pointer-decrement*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)) (/ *frdest*))
		    (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)) *frdest* (/ (rd 6)))
		    (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)) *frdest*    (rd 6) (rd 5))
		    (* *trap1* *call-stack-previous-increment*))))
      (setq *call-stack-pointer-count*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)) (ir 48))
		    (* (/ *trap1*) (/ (ir 50))    (ir 49)  (ir 48))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)) (/ *frdest*))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)) *frdest* (/ (rd 6)))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)) *frdest* (rd 6) (rd 5))
		    (* *trap1* *call-stack-previous-increment*)
		    (* *trap1* *call-stack-previous-decrement*))))
      (setq *call-stack-oa-write-calc*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 48)))
		    (* (/ *trap1*)    (ir 50)))))
      (setq *call-stack-return-pc-write-calc*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)))
		    (* (/ *trap1*)    (ir 50)))))
      (setq *call-stack-previous-increment-calc*
	    (ldb (byte 1. 0.) (* (/ *trap1*) *call-stack-pointer-count* (/ *call-stack-pointer-decrement*))))
      (setq *call-stack-previous-decrement-calc*
	    (ldb (byte 1. 0.) (* (/ *trap1*) *call-stack-pointer-count* *call-stack-pointer-decrement*)))
      (setq *open-frame-select*
	    (dpb (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)) (/ (ir 48)))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)) (/ *frdest*))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)) *frdest* (/ (rd 6)))
		    *trap1*)
		 (byte 1. 1.)
		 (ldb (byte 1. 0.)
		      (+ (* (/ *trap1*) (ir 50) (/ (ir 49)) (/ (ir 48)))
			 (* (/ *trap1*) (ir 50)    (ir 49)     (ir 48)))))))))

(deff assure-ch-pal-2-computed (memoized 'compute-ch-pal-2))

(def-simple-register *call-stack-previous-increment* *machine-regs*
  (assure-ch-pal-2-computed)
  *call-stack-previous-increment-calc*)

(def-simple-register *call-stack-previous-decrement* *machine-regs*
  (assure-ch-pal-2-computed)
  *call-stack-previous-decrement-calc*)

(def-simple-register *oa-write* *machine-regs*
  (assure-ch-pal-2-computed)
  *call-stack-oa-write-calc*)

(def-simple-register *return-pc-write* *machine-regs*
  (assure-ch-pal-2-computed)
  *call-stack-return-pc-write-calc*)

(defvar *active-frame-select* 0)
(defvar *active-frame-clock-enable* 0)
(defvar *open-frame-clock-enable* 0)
(defvar *active-frame-delayed-output-enable* 0)
(defvar *active-frame-mux-output-enable* 0)
(defvar *retpc-select* 0)

(defun compute-ch-pal-3 ()
  (assure-frdest-computed)
  (pal-equations
    (macrolet ((ir (n) `(LDB (BYTE 1. ,n) *IREG*))
	       (rd (n) `(LDB (BYTE 1. ,n) *RDEST*)))
      (setq *active-frame-select*
	    (ldb (byte 2. 0.)
		 (dpb (+ (* (/ *trap1*) (/ (ir 50)) (ir 49) (/ (ir 48)))
			 (* (/ *trap1*)    (ir 50)          (/ (ir 48))))
		      (byte 1. 1.)
		      (+ (* (/ *trap1*) (/ (ir 50)) (ir 49) (/ (ir 48)))
			 (* (/ *trap1*)    (ir 50)  (ir 49))))))
      (setq *active-frame-clock-enable*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (ir 49))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)) (/ (ir 48)))
		    (* (/ *trap1*)    (ir 50)     (ir 49))
		    *write-o-a-r*
		    *trap1*)))
      (setq *open-frame-clock-enable*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (ir 48))
		    (* (/ *trap1*)    (ir 50)  (/ (ir 49)))
		    (* (/ *trap1*)    (ir 50)     (ir 49) (ir 48))
		    *write-o-a-r*
		    *trap1*)))
      (setq *active-frame-delayed-output-enable*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (/ (ir 50)) (/ (ir 49)) (/ (ir 48)) (/ *write-o-a-r*))
		    *trap1*)))
      (setq *active-frame-mux-output-enable*
	    (ldb (byte 1. 0.)
		 (+ (* (/ *trap1*) (ir 50) (/ *write-o-a-r*))
		    (* (/ *trap1*) (ir 49) (/ *write-o-a-r*))
		    (* (/ *trap1*) (ir 48) (/ *write-o-a-r*)))))
      (setq *retpc-select*
	    (ldb (byte 1. 0.) (* (/ *trap1*) (/ (ir 50)) (ir 49)))))))

(deff assure-ch-pal-3-computed (memoized 'compute-ch-pal-3))

(def-simple-register *delayed-open* *machine-regs*
  *OPEN*)

(def-simple-register *OPEN* *machine-regs*
  (assure-ch-pal-2-computed)
  (assure-ch-pal-3-computed)
  (if (zerop *open-frame-clock-enable*)
      *OPEN*
      (if (zerop *write-o-a-r*)
	  (dispatch (byte 2. 0.) *open-frame-select*
	    (0 (read-heap))
	    (1 *return*      )
	    (2 *delayed-open*)
	    (3 (get-saved-open))
	    )
	  (progn (assure-mfio-for-output)
		 (ldb (byte 8. 16.) *mfio-bus*)))))

(defun address-saved-o-and-a ()
  (dpb (ldb %%k-processor-control-stack-group-number *processor-control*)
       (byte 4. 8.) *call-stack-pointer*))

(defun save-open-register ()
  (if (zerop *oa-write*)
      ()
      (micro::write-saved-o (address-saved-o-and-a)  *delayed-open*)))

(defun get-saved-open ()
  (if (zerop *oa-write*)
      (ldb (byte 8. 0.) (micro::read-saved-o (address-saved-o-and-a)))
      *delayed-open*))				;emulate write-through.

(eval-when (load)
  (make-machine-register 'saved-o 'save-open-register 'ignore *machine-regs*))

(def-simple-register *retpc* *machine-regs*
  (assure-ch-pal-3-computed)
  (if (zerop *retpc-select*)
      (if (zerop *write-return-pc-and-destination*)
	  (get-saved-pc)
	  (broken-simulation "Can't write return-pc from mfio"))
      *pc+1*))

(defun save-pc ()
  (if (zerop *return-pc-write*)
      ()
      (micro::write-saved-o (address-saved-o-and-a)  *retpc*)))

(defun get-saved-pc ()
  (if (zerop *return-pc-write*)
      (ldb (byte 24. 0.) (micro::read-return-pc (address-saved-o-and-a)))
      *retpc*))				;emulate write-through.

(eval-when (load)
  (make-machine-register 'saved-pc 'save-pc 'ignore *machine-regs*))

(def-simple-register *rdest* *machine-regs*
  (assure-ch-pal-3-computed)
  (if (zerop *retpc-select*)
      (if (zerop *write-return-pc-and-destination*)
	  (get-saved-return-destination)
	  (broken-simulation "Can't write rdest from mfio"))
      (if (zerop (ldb (byte 1. 59.) *ireg*))
	  (dpb (ldb (byte 1. 61.) *ireg*)
	       (byte 1. 6.)
	       (dpb (ldb (byte 3. 34.) *ireg*)
		    (byte 3. 3.)
		    (ldb (byte 3. 0.) *ireg*)))
	  (dpb (ldb (byte 1. 61.) *ireg*)
	       (byte 1. 6.)
	       (dpb (ldb (byte 5. 32.) *ireg*)
		    (byte 5. 1.)
		    (ldb (byte 1. 24.) *ireg*))))))

(defun save-return-destination ()
  (if (zerop *return-pc-write*)
      ()
      (micro::write-return-dest (address-saved-o-and-a) *rdest*)))

(defun get-saved-return-destination ()
  (if (zerop *return-pc-write*)
      (ldb (byte 24. 0.) (micro::read-return-dest (address-saved-o-and-a)))
      *rdest*))				;emulate write-through.

(eval-when (load)
  (make-machine-register 'saved-return-destination 'save-return-destination 'ignore *machine-regs*))

(def-simple-register *return-global-frame* *machine-regs*
  (assure-ch-pal-3-computed)
  (if (zerop *retpc-select*)
      (get-saved-return-global-frame)
      (ldb (byte 4. 37.) *ireg*)))

(def-simple-register *return-global-frame-other-register* *machine-regs*
  (if (zerop *write-return-pc-and-destination*)
      (ldb (byte 4. 37.) *ireg*)
      (ldb %%k-processor-control-misc *processor-control*)))

(defun save-return-global-frame ()
  (if (zerop *return-pc-write*)
      ()
      (micro::write-return-global-frame (address-saved-o-and-a) *return-global-frame-other-register*)))

(defun get-saved-return-global-frame ()
  (if (zerop *return-pc-write*)
      (micro::read-return-global-frame (address-saved-o-and-a))
      *return-global-frame-other-register*))	;emulate write-through

(eval-when (load)
  (make-machine-register 'saved-return-global-frame 'save-return-global-frame 'ignore *machine-regs*))

(def-simple-register *delayed-active* *machine-regs*
  *active*)

(def-simple-register *active* *machine-regs*
  (assure-ch-pal-3-computed)
  (if (zerop *active-frame-clock-enable*)
      *active*
      (if (zerop *write-o-a-r*)
	  (if (zerop *active-frame-mux-output-enable*)
	      *delayed-active*
	      (dispatch (byte 2. 2.) *active-frame-select*
		(0 (read-heap))
		(1 *return*)
		(2 (get-saved-active))
		(3 *open*)
		))
	  (progn (assure-mfio-for-output)
		 (ldb (byte 8. 8.) *mfio-bus*)))))

(defun save-active-register ()
  (if (zerop *oa-write*)
      ()
      (micro::write-saved-a (address-saved-o-and-a) *delayed-active*)))

(defun get-saved-active ()
  (if (zerop *oa-write*)
      (ldb (byte 8. 0.) (micro::read-saved-a (address-saved-o-and-a)))
      *delayed-active*))			;emulate write through.

(eval-when (load)
  (make-machine-register 'saved-a 'save-active-register 'ignore *machine-regs*))

(def-simple-register *delayed-return* *machine-regs*
  *return*)

(def-simple-register *return* *machine-regs*
  (assure-ch-pal-1-computed)
  (if (zerop *return-frame-clock-enable*)
      *return*
      (if (zerop *write-o-a-r*)
	  (if (zerop *return-frame-mux-select*)
	      *delayed-return*
	      *active*)
	  (progn (assure-mfio-for-output)
		 (ldb (byte 8. 0.) *mfio-bus*)))))

(def-simple-register *call-stack-pointer* *machine-regs*
  (assure-ch-pal-2-computed)
  (if (zerop *write-call-hardware*)
      (if (zerop *call-stack-pointer-count*)
	  *call-stack-pointer*
	  (if (zerop *call-stack-pointer-decrement*)
	      (let ((new (1+ *call-stack-pointer*)))
		(if (> new 256.) 0 new))
	      (let ((new (1- *call-stack-pointer*)))
		(if (< new 0.) 256. new))))
      ;; Get it from (byte ??) on MFIO
      (broken-simulation "Can't write call stack from MFIO yet")))

(def-simple-register *heap-pointer* *machine-regs*
  (assure-ch-pal-1-computed)
  (if (zerop *write-call-hardware*)
      (if (zerop *heap-pointer-count*)
	  *heap-pointer*
	  (if (zerop *heap-pointer-decrement*)
	      (let ((new (1+ *heap-pointer*)))
		(if (> new 256.) (broken-simulation "Call heap overflowed (shouldn't happen normally)") new))
	      (let ((new (1- *heap-pointer*)))
		(if (< new 0.) (broken-simulation "Call heap underflowed (cause trap)") new))))
      ;; Get it from MFIO
      (broken-simulation "Can't write heap pointer from MFIO yet")))

(defun address-call-heap ()
  (dpb (ldb %%k-processor-control-stack-group-number *processor-control*)
       (byte 4. 8.) *heap-pointer*))

(defun write-heap ()
  (if (zerop *heap-write-enable*)
      ()
      (micro::write-call-heap (address-call-heap) *delayed-return*)))

(defun read-heap ()
  (if (zerop *heap-write-enable*)
      (micro::read-call-heap (address-call-heap))
      *delayed-return*))

(eval-when (load)
  (make-machine-register 'call-heap 'write-heap 'ignore *machine-regs*))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial code testing.
;;;;;;;;;;;;;;;;;;;;;;;;;;

;(nc::assemble-instruction-list
;  '(CH-INIT_6
;     (MOVEI OPEN-ACTIVE-RETURN (QUOTE 16777215))
;     (nop)
;     (MOVEI A2 (QUOTE 254))
;     (MOVEI CALL-SP-HP (QUOTE 61695))
;     (MOVEI A1 (QUOTE 0))
;    DO8534_15
;     (ALUI-16 L-R NOP-NO-OVERFLOW-TRAP A1 (QUOTE 238) BW-24)
;     (TEST BR-NOT-GREATER-OR-EQUAL)
;     (BRANCH C_19 NIL)
;    C_18
;    B_52
;     (MOVEI OPEN-ACTIVE-RETURN (QUOTE 16776976))
;     (MOVEI (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) (QUOTE 0))
;     (RETURN (%REGISTER *CALL-STACK-POINTER-BASE* 1 0))
;    C_19
;     (MOVE A0 OPEN-ACTIVE-RETURN)
;     (ALU-FIELD FIELD-PASS OPEN-ACTIVE-RETURN A2 A0 (QUOTE 8) PW-II)
;     (TAIL-CALL 0 0 R0 NIL NEXT-PC-PC+1)          	;no quotes
;     (ALUI-16 L-R A2 A2 (QUOTE 1) BW-24)
;     (UNCONDITIONAL-BRANCH DO8534_15 (ALU R+1 A1 A0 A1 BW-32))))

;(nc::assemble-instruction-list
;  '((alu-field set-bit-right processor-control a0 processor-control
;	       hw:%%processor-control-halt-processor pw-ii))) ;halt
     


;NIL
;NIL
;NIL

;(defvar *foo* '(1135069834170597375
;  270361114267619328
;  1134946688851509502
;  1135074232200392959
;  1134944489828253696
;  558591504504324334
;  270361182987096064
;  54188332153835531
;  1135069834170597136
;  1135012796988129280
;  199427359072325632
;  270251164497350656
;  270378705909915656
;  848510715340783616
;  558485951388581889
;  2359923588716376069))


;((15 . 5) (7 . 11))
;NIL
;NIL

(defvar halt-instruction #x03C09200928690E1)
(defvar noop-instruction #X0F00800000000000)

(defvar *initial-bootprom-instructions*
	`#x(
	    (00 0380600000000000)
	    (01 0300620000000000)
	    (02 0340640000001000)
	    (03 0380660000004000)
	    (04 0380680090067600)		;jump bit is inverted
	    (05 03806a00cbc7801f)
	    (06 03806c006a00d000)
	    (07 03806c0001b680a0)
	    (08 03806c009c001000)
	    (09 21806e009a001000)
	    (0a 280080000000000a)
	    (0b 280080000000000b))) 
	  

(defvar the-bits
	#x'(
	    0FC0940014FFFFFF			;call hardware init stuff
	    03C0840041001000
	    0FC02400140000FE
	    0FC098001400F0FF
	    0FC0220014000000
	    07C08403888800EE
	    03C0841041001000
	    00C084004100100B
	    0FC0940014FFFF10
	    0FC0602014000000
	    02C4822060001000
	    03C0200094001000
	    03C0940020972008
	    0BC6840000000000
	    07C0240388900001
	    20C0220022813005))

(eval-when (load eval)

  (dotimes (i 1024.)
    (let ((address (ash i 3.)))
      (micro::write-bootprom (+ address 0) (mod halt-instruction (ash 1. 32.)))
      (micro::write-bootprom (+ address 1) (ash halt-instruction -32.))
      (micro::Write-bootprom (+ address 2) (mod noop-instruction (ash 1. 32.)))
      (micro::write-bootprom (+ address 3) (ash noop-instruction -32.))
      (micro::Write-bootprom (+ address 4) (mod noop-instruction (ash 1. 32.)))
      (micro::write-bootprom (+ address 5) (ash noop-instruction -32.))
      (micro::Write-bootprom (+ address 6) (mod noop-instruction (ash 1. 32.)))
      (micro::write-bootprom (+ address 7) (ash noop-instruction -32.))))

  

  (dolist (instruction *initial-bootprom-instructions*)
    (let ((location (first instruction))
	  (data     (second instruction)))
      (let ((address (ash location 1.)))
	(micro::write-bootprom address      (mod data (ash 1. 32.)))
	(micro::write-bootprom (1+ address) (ash data -32.))))))

;(define-global-frame call-hardware-loader)
;(define-global-variable call-hardware-loader *call-stack-pointer-base*)
;(define-global-variable call-hardware-loader *memory-pointer*)

;(defun ch-init ()
;  (hw:write-open-active-return #xFFFFFF)
;  (let ((free #xfe))
;    (hw:write-call-sp-and-hp #xF0FF)		;Empty the heap and stack.
;    (dotimes (i 238.)
;      (hw:write-open-active-return		;Write Free into RETURN
;	(hw:dpb free hw:%%ch-oar-return (hw:read-open-active-return)))
;      (hw:load-one-frame-to-call-heap)
;      (decf free)))
;  (hw:write-open-active-return #xFFFF10)
;  (setq *call-stack-pointer-base* 0)
;  ;; some sort of open call to get scroller loaded in bottom frame.
;  )
  
