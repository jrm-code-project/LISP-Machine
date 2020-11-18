;;; -*- Mode:LISP; Package:K-KBUG; Readtable:CL; Base:10;  compile-in-roots:("K-GLOBAL") -*-

;added support for lam:*falcon-memory-on-nubus*   RG  10/28/88

;;;$$$ Added *history-ram-saved* <08-Nov-88 wkf>
(defvar *history-ram-saved*     nil "When T the history ram has been saved at least once since MEGA-BOOT")

;;; $$$ Put KBUG at top of file. <08-Nov-88 wkf>

(defun kbug (&optional starting-address offset safe-mode)
  ;safe-mode assumes PCD is set up and does not molest processor
  (unless *history-ram-saved*			; $$$ KBUG now saves the history when needed. <08-Nov-88 wkf>
    (format t "~%*** Saving the history ram! ***")
    (setq *latest-history* (lam:get-history)))
  (when (null offset)
    (setq offset (if (boundp '*code-start*) *code-start* 0)))
  (lam:falcon-stop)
  (when starting-address
    (start starting-address))
  (do ()
      (())
    (kbug-print-instruction safe-mode offset)
    (let ((input (peek-char)))
      (lam:falcon-stop)
      (case input
	((#\space #\c-n)        (read-char) (lam:falcon-step))
	(#\c-p                  (read-char) (when (eq 'lose-big (proceed-kbug)) (return-from kbug)))
	(#\c-z                  (read-char) (lam:falcon-stop))
	(#\c-f                  (read-char) (frames 16))
	(#\c-m-f                (read-char) (frames-with-types))
	(#\q                    (read-char) (return-from kbug))
	((#\c-l #\clear-screen) (read-char) (zl::send *terminal-io* :clear-window))
	(#\m-s                  (read-char) (show-call-stack))
	(#\m-e                  (read-char) (format t "~%") (show-error))
	(#\m-i		        (read-char) (format t "~%") (show-illop))
	(#\m-r		        (read-char) (global:do-forever
					      (cond ((listen *terminal-io*) (return t))
						    ;; $$$ Added halt check. <16-Nov-88 wkf>
						    ((k-stopped-at-halt-inst-p)
						     (format t "~%The falcon has halted.")
						     (return t))
						    (t (lam:falcon-step)
						       (kbug-print-instruction safe-mode offset)))))
	(#\help                 (read-char) (kbug-debugger-help))
	(#\i                    (read-char) (print-single-step-info))
	(#\2                    (read-char) (step-k-till-not-halted)
				            (format t "~%*** Entering KBUG2 ***")
					    (kbug2))
	(otherwise              (kbug-evaluator "Return to KBUG."))))))

(defun kbug-debugger-help ()
  (format t "~%You are typing at the K debugger MARK-I. (KBUG)
 to exit.
SPACE, C-N to single-step.
C-P        to proceed K, remaning in debugger.
C-F        to view register-frames.
C-M-F      to view register-frames with datatypes and values.
C-L        to clear-screen.
C-Z        to stop machine.
M-S        to display the K call stack
M-E        to display an error message
M-I        to display an illop message
M-R        to auto-single-step the falcon until user types at keyboard
HELP       to get this message
I          to print the single step information
2          to attempt to recover from an illop and reenter KBUG2
Q          to quit/return from KBUG
Otherwise  to type a form to the evaluator"))

(defun kbug-print-instruction (safe-mode offset)
  (let* ((addr (logand #xFFFFFF (k-read-spy-pc)))
	 (sym-addr (get-symbolic-address addr))
	 (mmfio (lam::k-read-spy-mmfio)))
    (if sym-addr
	(format t "~&~15a(#x~x) ~8,'0x " sym-addr addr mmfio)
      (format t "~&~6,'0x              ~8,'0x " addr mmfio))
    (if safe-mode
	(multiple-value-bind (inst phys-addr)
	    (kbug-generic-read-inst-safe addr)
	  (print-instruction inst phys-addr))
      (print-instruction (lam:read-inst (+ addr offset)) (+ addr offset))
      ;;** note that the above non-safe mode instruction reads a PHYSICAL address
      ;;   since the PC is really VIRTUAL, we could get the wrong thing.
      )))

(defmacro dpb-multiple (&rest fields)
  (labels ((expander (fields)
	     (cond ((null fields) (error "Even number of arguments to ~s" 'dpb-multiple))
		   ((null (rest fields)) (first fields))
		   (t `(DPB
			 ,(first fields) ,(second fields)
			 ,(expander (rest (rest fields))))))))
    (expander fields)))

(defun hex (n)
  (format t "~x" n))

(defun hex32 (n &optional (stream t))
  (format stream "~8,'0x" n))

(defmacro deff (symbol frob)
  `(EVAL-WHEN (COMPILE LOAD EVAL)
     (SETF (SYMBOL-FUNCTION (QUOTE ,symbol)) ,frob)))

(defvar *system-version* 0)
(defvar *version-cache-generation* 0)

;(eval-when (compile eval load)
;(defun memoized (function)
;  (let ((last-time -1)
;	last-value)
;    #'(lambda ()
;	(if (= last-time *version-cache-generation*)
;	    last-value
;	    (prog1 (setq last-value (funcall function))
;		   (setq last-time *version-cache-generation*))))))
;)

(defun invalidate-version-cache ()
  (incf *version-cache-generation*))

(defun get-appropriate-constants (constant-list)
  (let ((best-list-so-far  '())
	(best-version-so-far 0))
    (dolist (l constant-list)
      (let ((version-number (first l))
	    (this-list      (second l)))
	(when (and (<= version-number *system-version*)
		   (or (null best-list-so-far)
		       (> version-number best-version-so-far)))
	  (setq best-list-so-far this-list)
	  (setq best-version-so-far version-number))))
    (when (null best-list-so-far)
      (error "Couldn't find appropriate constants."))
    best-list-so-far))

;(do-symbols (s 'k)
;  (export s 'k))

(defvar *i-mode* :dis)
(defvar *pc* nil				; $$$ Added. <08-Nov-88 wkf>
  "When non nil is the address of an instruction to be disassembled (used for determining address of local refs)")

(defun print-instruction (i from-pc &optional (stream t))
  (case *i-mode*
    (:hex (format stream "~16,'0x" i))
    (:dis (format stream "~x" (if (= (ash i -32) cons:code-header-instruction-high)
				  (format nil "******   Function Beginning for: ~x   ******"
					  (get-warm-symbolic-address (1+ from-pc)))
				(dis-symbolicify (nc::dis i) from-pc))))))

(defun dis-symbolicify (x from-pc &aux temp jump-adr)
  (cond ((and (global:memq (car x) '(k#:jump k#:call k#:open-call k#:tail-call k#:tail-open-call))
	      (setq temp (get-warm-symbolic-address (setq jump-adr (cadr x)))))
	 (list* (car x) temp (list jump-adr) (cddr x)))
	((and (member (car x) '(k#:branch k#:unconditional-branch))
	      (setq temp (get-warm-symbolic-address (setq jump-adr (dpb (cadr x) hw:%%i-branch-address from-pc)))))
	 (list* (car x) temp (list jump-adr) (cddr x)));$$$ Fixed to show jump address relative to pc not 1+pc <10-Nov-88 wkf>
	(t x)))

(defun make-fancy-wait-function (outer-wait-function inner-wait-function)
  (let ((last-time (zl::time-increment (zl:time) -30)))
    #'(lambda ()
	(or (funcall outer-wait-function)
	    (and (> (zl:time-difference (zl:time) last-time) 30)
		 (progn (setq last-time (zl:time))
			(funcall inner-wait-function)))))))

;;; $$$ Changed file to use this. <16-Nov-88 wkf>
;;; +++ this is still used, I un-commented it out. See me. <19-Nov-88 RG>
(defun k-halted-p ()
  	;dont check k-run-flag here since processor might have been otherwise started (i.e. by MAC hopefully)
  (or (null k-run-flag)		;unfortunately, this is necessary because we cant read the debug-logic COMMAND
				; RUN FUNCTION, can we?
      (= 1 (lam:k-read-spy-program-halt))))

(defun k-running-p ()
  "this function will lie if k-run-flag gets out of sync with the spy pal"
  (and k-run-flag (= 0 (lam:k-read-spy-program-halt))))

(defun k-stopped-at-halt-inst-p ()
  (= 1 (lam:k-read-spy-program-halt)))

;;; $$$ Added. <16-Nov-88 wkf>
(defun step-k-till-not-halted ()
  (do ((c 0 (1+ c)))
      ((= 0 (lam:k-read-spy-program-halt)))
    (lam:falcon-step)
    (when (> c 100)
      (error nil "K is not recovering from halt."))))

(defun proceed-kbug ()
  ;; Crock:  Step first four instructions to make halt bit off.
  ;; This depends upon the behavior of the code at illop.
  (step-k-till-not-halted)
  (lam:k-run)
  (format t "--RUN--")
  (zl:process-wait "Kbug" (let ((stream *terminal-io*))
			    (make-fancy-wait-function
			      #'(lambda () (when (listen stream)
					     (global:send stream :tyi)
					     t))
			      #'(lambda () (not (k-running-p))))))
  
  (if (k-stopped-at-halt-inst-p)
      (progn (format t "~%HALTED:  ") (why) (lam:falcon-stop))
    (progn (lam:falcon-stop) (format t "~%STOP"))))

(defun kbug-step-until-pc (pc)
  (do ()
      ((= (k-read-spy-pc) pc))
    (lam:falcon-step)))

(defun kbug-read-eval-print-one-form ()
  (multiple-value-bind (sexp flag)
      (zl:with-input-editing (*terminal-io* '((:full-rubout :full-rubout)
					      (:activation char= #\end)
					      (:prompt prompt-for-kbug)))
	(read))
    (if (eq flag :full-rubout)
	()
      (progn (terpri) (prin1 (prog1 (eval sexp) (terpri)))))))

(defun kstep (n)
  (dotimes (i n) (k-step)))

(defun print-frame (n stream)
  (let ((frame-contents
	  (saving-current-k-pc
	    #'(lambda ()
		(k-read-frame n)))))
    (format stream "~:{~%~x:~:[ ~;*~]~8,'0x~}"
	   (reverse frame-contents))))

(defun read-global (s)
  (let ((info (get s :register)))
    (when (null info) (error nil "Couldn't find global ~S" s))
    (saving-current-k-pc
      #'(lambda ()
	  (k-read-register (second info) (third info))))))

(defun print-single-step-info ()
  (saving-current-k-pc
    #'(lambda ()
;	(let ((trapped-pc (first (read-global 'gr::*save-trap-pc*))))
;	  (disassemble-from-virtual-memory trapped-pc 1.))
	(let ((hp-sp (k-read-hp-sp)))
	  (format t "~%HP,SP: ~2,'0x ~2,'0x " (ldb (byte 8. 8.) hp-sp) (ldb (byte 8. 0.) hp-sp)))
	(let ((retpc-rdest (k-read-retpc-rdest)))
	  (format t "RETPC, RDEST: ~6,'0x ~[O~;A~;R~;G~;NO~;NO~;NTO~;NTO~]~2,'0x"
		  (ldb (byte 23. 0.) retpc-rdest)
		  (ldb (byte 3. 28.) retpc-rdest)
		  (ldb (byte 4. 24.) retpc-rdest)))
	(mapcar #'(lambda (pair)
		    (format t " ~a"
			  (get-symbolic-address (second pair))))
		(dump-call-stack)))))

(defun dump-call-stack ()
  (saving-hp-csp
    #'(lambda (hp csp)
	(declare (ignore hp))
	(labels ((dump-csp (depth stuff-so-far)
		   (if (minusp depth)
		       (reverse stuff-so-far)
		       (progn (k-write-hp-csp (dpb depth (byte 8. 0.) 0.))
			      (let ((retpc-rdest (k-read-retpc-rdest)))
				(dump-csp (1- depth)
					  (cons (list (ldb (byte 7. 24.) retpc-rdest)
						      (ldb (byte 24. 0.) retpc-rdest))
						stuff-so-far)))))))
	  (dump-csp csp '())))))




(defun get-starting-address (fcn &optional default)
  (cond ((numberp fcn) fcn)
	(t (let ((f (nc::get-ncompiled-function fcn)))
	     (when f
	       (or (nc::ncompiled-function-starting-address f)
		   default))))))
		 


(defun load-code (code &optional (addr #x100))
  "Load a sequence of instructions at starting address"
  (lisp:map nil #'(lambda (inst)
	       (lam:write-inst addr inst)
	       (incf addr))
       code))

(defun load-fcn (fcn &optional (virtual-address
				 (get-starting-address fcn #x100))
			       (physical-address virtual-address))		   
  (let ((f (nc::get-ncompiled-function fcn)))
    (nc::link f virtual-address)
    (load-code (nc::ncompiled-function-code f) physical-address)))

;;; note that this links twice
;;; but that is good because then "forward references" are resolved
(defun load-fcns (fcns &optional (starting-address #x100))
  (let ((cfuns (map 'list #'nc::get-ncompiled-function fcns)))
    (setq *loaded-functions* cfuns)
    (do ((fs cfuns (cdr fs))
	 (addr starting-address (+ addr (nc::ncompiled-function-length (car fs)))))
	((null fs))
      (nc::link (car fs) addr nil))
    (dolist (f cfuns)
      (load-fcn f))))
	 
(defun start (addr)
  (k-start (get-starting-address addr)))

(defun run (&optional addr)
  (if addr
      (k-go (get-starting-address addr))
    (k-run)))


(defun read-oar ()
  (declare (values open active return))
  (let ((oar (k-read-oar)))
    (values
      (ldb hw::%%ch-oar-open oar)
      (ldb hw::%%ch-oar-active oar)
      (ldb hw::%%ch-oar-return oar))))
  
(defun frames-with-types (&optional (stream t) (n 16.))
  (let ((old-pc      (k-read-spy-pc))
	(left        (kbug-left))
	(right       (kbug-right))
	(left-boxed  (kbug-left-boxed))
	(right-boxed (kbug-right-boxed)))
    (multiple-value-bind (open active return-frame) (read-oar)
      (new-format stream
		  newline
		  labeled-boxed-32 "Left "  (ldb-test (byte 1. 0.) left-boxed) left
		  labeled-boxed-32 " Right " (ldb-test (byte 1. 0.) right-boxed) right
		  labeled-hex-2    " Open #x"   open
		  labeled-hex-2    " Active #x" active
		  labeled-hex-2    " Return #x" return-frame
		  newline)
      (if (= open active)
	  (progn (dotimes (i n (format stream "~%"))
		   (new-format stream
			       "~%  ~a~2D" 'a i
			       boxed-32 nil (read-open i) " ")
		   (show-data-type-and-object (read-open i) stream))
		 (dotimes (i n (format stream "~%"))
		   (new-format stream
			       "~%  ~a~2D" 'r i
			       boxed-32 nil (read-return i) " ")
		   (show-data-type-and-object (read-return i) stream)))
	(progn (dotimes (i n (format stream "~%"))
		   (new-format stream
			       "~%  ~a~2D" 'a i
			       boxed-32 nil (read-open i) " ")
		   (show-data-type-and-object (read-open i) stream))
	       (dotimes (i n (format stream "~%"))
		 (new-format stream
			     "~%  ~a~2D" 'r i
			     boxed-32 nil (read-active i) " ")
		 (show-data-type-and-object (read-active i) stream))
	       (dotimes (i n (format stream "~%"))
		 (new-format stream
			     "~%  ~a~2D" 'r i
			     boxed-32 nil (read-return i) " ")
		 (show-data-type-and-object (read-return i) stream)))))
    (format t "~&Q: ~8,'0x" (read-q))
    (k-set-pc old-pc)))

(defun frames (&optional (n 16.))
  (let ((old-pc (k-read-spy-pc)))
    (multiple-value-bind (open active return-frame) (read-oar)
      (format t "~&O: ~x  A: ~x  R: ~x" open active return-frame)
      (dotimes (i n)
	(format t "~&O~2d: ~8,'0x  A~2d: ~8,'0x  R~2d: ~8,'0x"
		i (read-open i)
		i (read-active i)
		i (read-return i))))
    (format t "~&Q: ~8,'0x" (read-q))
    (k-set-pc old-pc)))
  
  

(defun ktrace (fcn &optional (nregs 3))
  (let ((addr (get-starting-address fcn)))
    (do () (())
      (do () ((= (k-read-spy-pc) addr)))
      (k-stop)
      (terpri)
      (dotimes (i nregs)
	(format t "~d "
		(logand #xFFFFFF (read-active i))))
      (k-set-pc (1+ addr))
      (k-run))))


(defun show-address (k-address &optional (stream t))
  (format stream "~%Address #x~x, quantum #x~x, cluster #x~x, within cluster #x~x"
	  k-address
	  (ldb vinc:%%quantum-number k-address)
	  (ldb vinc:%%cluster-number k-address)
	  (logand k-address #X3FF)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Memory map functions
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun show-entire-map (&optional (stream t) (starting-at-quantum 0) &aux last-elided)
  (format stream "~%show-entire-map:~%")
  (do ((quantum starting-at-quantum (1+ quantum)))
      ((>= quantum 4096.))
    (dotimes (c 16.)
      (let* ((cluster  (+ (* quantum 16.) c))
	     (map-entry (kbug-read-map-cluster cluster)))
	(cond ((and last-elided (= map-entry last-elided)))
	      ((member map-entry '(0 #o3200))		;null or fresh, swapped-out, local
	       (format stream "~%...#x~4,'0x  " cluster)
	       (decode-map-entry map-entry stream)
	       (setq last-elided map-entry))
	      (t
	       (setq last-elided nil)
	       (format stream "~%#x~4,'0x  " cluster)
	       (decode-map-entry map-entry stream)))))))

(defun cross-check-map-and-pcd (&optional (stream t) (starting-at-quantum 0))
  (format stream "~%Cross checking map entries up from ~x." (* starting-at-quantum 16.))
  (do ((quantum starting-at-quantum (1+ quantum)))
      ((>= quantum 4096.))
    (dotimes (c 16.)
      (let* ((cluster  (+ (* quantum 16.) c))
	     (map-entry (kbug-read-map-cluster cluster))
	     (map-protection (logand #x3 map-entry))		;lisp bits.
	     (map-physical-address (ldb hw::%%map-on-board-address map-entry)) ;this has now been changed to so as to
		;deal with 16 meg max, which should win in both on-board and *nubus-memory* mode.
	     ;(map-status (map::extract-map-status map-entry))
	     )
	(cond ((not (zerop map-protection))	;for now, cross check only stuff that could be ref'ed.
	       (let* ((pcd-index map-physical-address)
		      (pcd-entry (kbug-read-pcd pcd-index))
		      (pcd-virtual-cluster (ldb pcd::%%pcd-virtual-cluster-number pcd-entry))
		      (pcd-status (ldb pcd::%%pcd-status pcd-entry))
		      )
		 (cond ((or (zerop pcd-status)
			    (not (= pcd-virtual-cluster cluster)))
			(format stream "~%MAP and PCD fail to cross-check, MAP cluster ~X PCD index ~X~%MAP: "
				cluster map-physical-address)
			(decode-map-entry map-entry stream)
			(format stream " PCD: ")
			(decode-pcd pcd-entry stream))))))))))

(defun cross-check-pcd-and-map (&optional (stream t) (starting-at-index 0))
  (format stream "~%Cross checking PCD entries up from ~x." starting-at-index)
  (do ((pcd-index starting-at-index (1+ pcd-index)))
      ((>= pcd-index vinc:*physical-memory-max-clusters*))
    (let* ((pcd-entry (kbug-read-pcd pcd-index))
	   (pcd-virtual-cluster (ldb pcd::%%pcd-virtual-cluster-number pcd-entry))
	   (pcd-status (ldb pcd::%%pcd-status pcd-entry))
	   )
      (cond ((not (zerop pcd-status))
	     (let* ((map-entry (kbug-read-map-cluster pcd-virtual-cluster))
		    (map-protection (logand #x3 map-entry))		;lisp bits.
		    (map-physical-cluster (map::map-on-board-address map-entry)) ;masks NUBUS part if in *NUBUS-MEMORY* mode
		    ;(map-status (map::extract-map-status map-entry))
		    )
	       (cond  ((or (zerop map-protection)
			   (not (= map-physical-cluster pcd-index )))
		       (format stream "~%PCD and MAP fail to cross-check, PCD index ~X MAP cluster ~X~%MAP: "
			       pcd-index map-physical-cluster)
		       (decode-map-entry map-entry stream)
		       (format stream " PCD: ")
		       (decode-pcd pcd-entry stream)))))))))
	       

(defun show-map (entry stream)
  (let ((bits (map::read-map entry)))
    (format stream "~&~5,48x " entry)
    (decode-map-entry bits stream)))

(defun direct-map-location-zero ()		;called by pseudo-boot
  (map::write-map 0
    (map::inject-map-status
      (if (null lam:*falcon-memory-on-nubus*)
	  (dpb-multiple
	    hw:$$map-local           hw:%%map-local-memory-bit
	    0                        hw:%%map-on-board-address
	    map::$$cluster-not-fresh map::%%map-fresh-cluster
	    0)
	(dpb-multiple
	    hw:$$map-non-local       hw:%%map-local-memory-bit
	    (global:lsh lam:*falcon-memory-on-nubus* 12.)    hw:%%map-off-board-address	;this is (byte 20. 12.)
	  ;lines commented out below dont win because of byte overlap.
	    ;0			     hw:%%map-off-board-address
	    ;lam:*falcon-memory-on-nubus* hw:%%map-off-board-address-nubus-quad-slot
	    map::$$cluster-not-fresh map::%%map-fresh-cluster
	    0))      
      map:$$map-status-normal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Downloading the cold load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *loaded-functions* ())
(defvar *code-free-pointer* #x100)
(defvar *code-start* 0.)	;physical memory origin of code loaded by cold loader. (as doubleword address)
(defvar *code-holes* ())

(defun download-cold-load ()
  (lam::k-reset)
  (k-init)
  (setq *breakpoints-installed* nil)
  (dotimes (i cold::*cold-data-size*)
    (k-mem-write (ash i 2.) (cold::cold-data-read i)))
  (let ((instructions-start (truncate (cold::cold-data-read 43.) 2))
	(clusters           (cold::cold-data-read 44.)))
    (dotimes (i (* clusters 512.))		;512 instructions per cluster
      (lam:write-inst (+ i instructions-start)
		      (cold::cold-code-read i)))
    (setq *code-free-pointer* (+ instructions-start
				 (* clusters 512)))
    (setq *code-start* instructions-start)
    (dolist (f cold::*cold-loaded-functions*)
	(setf (nc::ncompiled-function-code f) :in-memory))
    (setq *loaded-functions* cold::*cold-loaded-functions*)
    nil
 ))


;(defun disassemble-fcn-from-memory (fcn)
;  (setq fcn (nc::get-ncompiled-function fcn))
;  (format t "~%In memory:")
;  ;; No, I don't know why disassemble-from-virtual-memory doesn't work here
;  (kbug::disassemble-from-core (+ *code-start* (nc::ncompiled-function-starting-address fcn))
;			       (nc::ncompiled-function-length fcn)))

(defun compile-defun-to-k-core (form)
  (let ((fcn (nc::cc form)))
    (load-fcn fcn (- *code-free-pointer* *code-start*) *code-free-pointer*)
    (incf *code-free-pointer* (nc::ncompiled-function-length fcn))))

;;; $$$ Removed zwei code which was commented out. <08-Nov-88 wkf>

(defun get-symbolic-address (addr)
  (multiple-value-bind (name offset)
      (get-symbolic-address-and-offset addr)
    (cond ((null name)	; $$$ Keep it from bombing. <14-Nov-88 wkf>
	   nil)
	  ((zerop offset)
	   (format nil "~A" name))
	  (t
	   (format nil "~A+#x~X" name offset)))))

;;; $$$ Generalized. <08-Nov-88 wkf>
;;; $$$ This appears to only work for cold downloaded functions <16-Nov-88 wkf>
;;; @@@ We can speed this up by saving the state we compute here. (ie start-address length and name) <16-Nov-88 wkf>
(defun get-symbolic-address-and-offset (addr) "Returns nil if not found."
  (dolist (function *loaded-functions*)
    (when function
      (let ((start-addr (nc::ncompiled-function-starting-address function)))
	(when (and start-addr
		   (>= addr start-addr)
		   (< addr (+ start-addr
			      (nc::ncompiled-function-length function))))
	  (let ((name (nc::ncompiled-function-name function)))
	    (return (values name (- addr start-addr)))))))))

;;; $$$ Make it go faster. <14-Nov-88 wkf>
(defvar *maximum-function-size* 200.)	;scan back no further than this looking for function header.

(defun kbg-symbolic-address (pc)
 ;kbug-symbolic-address for when k is stopped.  scan ourselves.
  (do* ((current-pc pc (1- current-pc))
	(pc-offset  -1 (1+ pc-offset))
	(inst-high (k-read-virtual-memory (logior #X2000001 (ash current-pc 1.)))
		   (k-read-virtual-memory (logior #X2000001 (ash current-pc 1.)))))
       ((= inst-high cons:code-header-instruction-high)
	(let* ((inst-low          (k-read-virtual-memory (logior #X2000000 (ash current-pc 1.))))
	       (back-pointer-type (ldb vinc:%%data-type inst-low)))
	  (cond
	    ;; Normal functions
	    ((= back-pointer-type vinc:$$dtp-compiled-function)
	     (let* ((fef-struct-base inst-low)	;word after function header is struct pointer. This is
		    (function-symbol (k-read-virtual-memory (1+ fef-struct-base)))	; inst-low since we fetched 64. bits.
		    (function-symbol-data-type (hw:ldb function-symbol vinc:%%data-type 0)))
	       (cond ((= function-symbol-data-type vinc:$$dtp-symbol)
		      (list (read-symbol-name function-symbol)
			    pc-offset))
		     ((= function-symbol-data-type vinc:$$dtp-cons)	;(internal fctn ...)
		      (list (with-output-to-string (stream)
			      (show-object function-symbol stream))
			    pc-offset))
		     (t (format t "~%Bad function symbol pointer:")
			(show-object function-symbol)
			(global:fsignal "lose")))))
	    ;; look back again starting from real call (&REST links)
	    ((= back-pointer-type vinc:$$dtp-unboxed-locative)
	     (kbg-symbolic-address (logand #xffffff (ash inst-low -1))))
	    ;; this should only happen before cold symbols are warm loaded
	    (t nil))))
    (when (> pc-offset *maximum-function-size*)
      (return (list "[No-function-found!]" -1)))
    (when (zerop inst-high)
      (return (list "[Zeroed-memory!]" 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Disassemble from core
;;;;;;;;;;;;;;;;;;;;;;;;;;
						;
;(defun disassemble-from-core (start how-much)
;  (dotimes (i how-much)
;    (format t "~%#x~x ~s" (- (+ i start) *code-start*) (nc::Dis (lam:read-inst (+ i start))))))

;(defun disassemble-from-virtual-memory (pc how-much)
;  (dotimes (i how-much)
;    (let ((low-half (k-read-virtual-memory (logior #X2000000 (ash (+ i pc) 1.))))
;	  (high-half (k-read-virtual-memory (logior #x2000001 (ash (+ i pc) 1.)))))
;      (format t "~%~x ~s" (+ i pc) (nc::dis (logior (ash high-half 32.) low-half))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Booting the machine
;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant *first-instruction-cluster* #X8000)

;;; Defined for all time.
;;; These are intended to be blown into the all boot proms.

(defconstant **boot-vector-origin** 42.)
(defconstant **bootprom-version**                         0.)
(defconstant **initial-code-physical-location-bv-offset** 1.)
(defconstant **initial-code-size-in-clusters-bv-offset**  2.)
(defconstant **initial-code-entry-point-bv-offset**       3.)
(defconstant **physical-memory-block-map**                4.)

(defun read-boot-vector (entry)
  (kbug-generic-read-memory
    (+ **boot-vector-origin** entry)))

(defun write-boot-vector (entry value)
  (kbug-generic-write-memory
    (+ **boot-vector-origin** entry)
    value))

;;; End of defined for all time.

(defun pseudo-boot (stream)		;leaves PC set to starting location.
  ;; $$$ added stream <07-Nov-88 pace>
  (k-init)
  (setup-processor-control-register)
  (setup-memory-control-register)
  ;; Traps are off here.
  ;; This is where the virtual page zero location for NIL gets mapped to physical page zero.
  (direct-map-location-zero)
  ;; Now, we figure out where to map the initial instructions.
  (labels ((map-n-instruction-clusters (n physical virtual)
	     (unless (zerop n)
	       (format stream "~&Mapping ~X to ~X" virtual physical)
	       (map::write-map virtual
			       (map::inject-map-status
				 (if (null lam:*falcon-memory-on-nubus*)
				     (dpb-multiple
				       physical       hw:%%map-on-board-address
				       hw:$$map-local hw:%%map-local-memory-bit
				       0              hw:%%map-volatility
				       0              hw:%%map-c-trap-bits
				       0)
				   (dpb-multiple
				     (+ (global:lsh lam:*falcon-memory-on-nubus* 12.)
					physical)		     hw:%%map-off-board-address
				     hw:$$map-non-local       hw:%%map-local-memory-bit
				     ;lam:*falcon-memory-on-nubus* hw:%%map-off-board-address-nubus-quad-slot
				     0              hw:%%map-volatility
				     0              hw:%%map-c-trap-bits
				     0))
				 map::$$map-status-read-only))
	       (map-n-instruction-clusters (1- n) (1+ physical) (1+ virtual)))))
    (let ((n (dpb 0 vinc:%%data-type (boot::read-boot-vector **initial-code-size-in-clusters-bv-offset**)))
	  (physical (ash (dpb 0 vinc:%%data-type (boot::read-boot-vector **initial-code-physical-location-bv-offset**)) -10.))
	  (virtual *first-instruction-cluster*))
      (format stream "~&n = ~s physical = ~s virtual = ~s" n physical virtual)
      (map-n-instruction-clusters n physical virtual)))
  ;; Size the physical memory
  (let ((size (find-physical-memory)))		;return megabyte-per-bit bit-map.
    (format stream "~%Physical memory (megabyte) bit-map = ~B~%" size)
    (write-boot-vector **physical-memory-block-map** size))
  ;; Inform the K about the bootprom version
  (write-boot-vector **bootprom-version** 0.)

  ;(global:fsignal "Ready to proceed K")

  ;; Mapped in some instructions, jump to them.
  (hw:jump (read-boot-vector **initial-code-entry-point-bv-offset**))
  )


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

(defun falcon-complete-status ()
  (format t "~%PC reads #x~x, halt ~s" (k-read-spy-pc) (k-read-spy-program-halt))
  (k-stop)
  (decode-memory-control-register    (hw:read-memory-control))
  (decode-memory-status-register     (hw:read-memory-status))
  (decode-processor-control-register (hw:read-processor-control))
  (decode-processor-status-register  (hw:read-processor-status))
  )

(defun decode-memory-control-register (&optional (memory-bits (hw:read-memory-control)))
  (format t "~%Traps               ~[dis~;en~]abled~
               ~%Asynchronous traps  ~[dis~;en~]abled~
               ~%Overflow trap       ~[dis~;en~]abled~
               ~%Datatype trap       ~[dis~;en~]abled~
               ~%Synchronous traps   ~[dis~;en~]abled~
               ~%Single-step trap    ~[dis~;en~]abled~
               ~[~;~%**Unused bit is on**~]~
               ~%Reset trap bit      ~[on~;off~]~
               ~[~;~%**Unused bits are on **~]~
               ~%Dram parity         ~[dis~;en~]abled~
               ~%Bootprom            ~[on~;off~]~
               ~%Transporter mode    ~D~
               ~%Lisp/C map select   ~[Lisp mode~;C mode~]~
               ~%Parity select       ~[Correct parity~;Wrong parity~]~
               ~%16384 interrupt     ~[dis~;en~]abled~
               ~%1024 interrupt      ~[dis~;en~]abled~
               ~%Icache errors       ~[dis~;en~]abled~
               ~%Nubus transfer mode ~[32-bits~;Byte 0~;Block~;Byte 2~;~
                                       Low 16 bits~;Byte 1~;High 16 bits~;Byte 3~]~
               ~%Leds                ~D~
               ~%Statistics polarity ~[true~;invert~]~
               ~%Statistics source   ~[Icache hit~;Processor memory cycle~;Instruction stat bit~;~
                                       Unused 3~;PC in high core~;Unused 5~;Unused 6~;Unused 7~]~
               ~%Statistics mode     ~[Edge trigger~;Duration~]"
	  
	  (ldb hw:%%memory-control-master-trap-enable       memory-bits)
	  (ldb hw:%%memory-control-asynchronous-trap-enable memory-bits)
	  (ldb hw:%%memory-control-overflow-trap-enable     memory-bits)
	  (ldb hw:%%memory-control-datatype-trap-enable     memory-bits)
	  (ldb hw:%%memory-control-synchronous-trap-enable  memory-bits)
	  (ldb hw:%%memory-control-single-step-enable       memory-bits)
	  (ldb (byte 1. 25.)                                memory-bits)
	  (ldb hw:%%memory-control-reset-trap-bit           memory-bits)
	  (ldb (byte 3. 20.)                                memory-bits)
	  (ldb hw:%%memory-control-dram-parity-enable       memory-bits)
	  (ldb hw:%%memory-control-bootprom-disable         memory-bits)
	  (ldb hw:%%memory-control-transporter-mode         memory-bits)
	  (ldb hw:%%memory-control-l-c-map-select           memory-bits)
	  (ldb hw:%%memory-control-write-wrong-parity       memory-bits)
	  (ldb hw:%%memory-control-16384-interrupt          memory-bits)
	  (ldb hw:%%memory-control-1024-interrupt           memory-bits)
	  (ldb hw:%%memory-control-icache-error-enable      memory-bits)
	  (ldb hw:%%memory-control-nubus-transfer-mode      memory-bits)
	  (logxor 7. (ldb hw:%%memory-control-leds                     memory-bits))
	  (ldb hw:%%memory-control-statistics-polarity      memory-bits)
	  (ldb hw:%%memory-control-statistics-source        memory-bits)
	  (ldb hw:%%memory-control-statistics-mode          memory-bits)))

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
      
(defun decode-processor-control-register (&optional (processor-bits (hw:read-processor-control)))
  (format t "~[~;~:;~:**Undefined processor control bits 20-23 #x~x.~]~
                ~%Floating point trap  ~[dis~;en~]abled~
                ~%Heap underflow trap  ~[dis~;en~]abled~
		~%Call Stack load control ~[normal~;special~]~
                ~%Stack group          #x~x~
                ~%Misc bits            #x~x~
                ~%Data bit             ~b~
		~%Halt bit             ~[Run~;Halt~]~
		~%Box mode	     ~[normal~;register reload~]~
		~%FPS ram write enable ~[Read~;Write~]~
		~%Jump Indirect        ~[Fall through~;Jump~]~
		~[~;~%*Spare bit 3 is on*~]~
		~%Low core cache       ~[dis~;en~]abled~
                ~%Cache set B          ~[dis~;en~]abled~
		~%Cache set A          ~[dis~;en~]abled"
	    (ldb hw:%%processor-control-spare-20-through-23        	       processor-bits)
	    (ldb hw:%%processor-control-floating-point-trap-enable 	       processor-bits)
	    (ldb hw:%%processor-control-heap-underflow-trap-enable 	       processor-bits)
	    (ldb hw:%%processor-control-spare-17		   	       processor-bits)
	    (ldb hw:%%processor-control-stack-group-number	   	       processor-bits)
	    (ldb hw:%%processor-control-misc			   	       processor-bits)
	    (ldb hw:%%processor-control-data-bit		   	       processor-bits)
	    (ldb hw:%%processor-control-halt-processor		   	       processor-bits)
	    (ldb hw:%%processor-control-box-mode		   	       processor-bits)
	    (ldb hw:%%processor-control-floating-point-status-ram-write-enable processor-bits)
	    (ldb hw:%%processor-control-jump-indirect			       processor-bits)
	    (ldb hw:%%processor-control-spare-3				       processor-bits)
	    (ldb hw:%%processor-control-icache-z-enable			       processor-bits)
	    (ldb hw:%%processor-control-icache-b-enable			       processor-bits)
	    (ldb hw:%%processor-control-icache-a-enable			       processor-bits)))

(defun decode-memory-status-register (&optional (memstat (hw:read-memory-status)))
  (format t "~%Memory board has ~[double-~;single-~]sided ram sips.~
               ~%Autoboot jumper      ~[off~;on~]~
               ~%Parity error         ~[on~;off~]~
               ~[~;~:;~:*~%*Undefined memory status bits 19-20 #x~x*~]~
               ~%Transport trap       ~[armed~;unarmed~]~
               ~%Read fault           ~[armed~;unarmed~]~
               ~%VMA boxed bit        ~[boxed~;unboxed~]~
               ~%MD boxed bit         ~[boxed~;unboxed~]~
               ~%Last cycle type      ~[write~*~
               ~%GC trap              ~[enabled~;disabled~]~;~
                                             read~
               ~%Transport type       ~[write~;visible-evcp~;transport~;no-transport~]~*~]~
               ~%MD written lately    ~[yes~;no~]~
               ~%Nubus bootstrap mode ~[RESET~;Short reset~:;*undefined*~]~
               ~%ECO jumpers          #b~4,'0b~
               ~%Nubus slot           #x~1x"
	    (ldb hw:%%memory-status-16meg                   memstat)
	    (ldb hw:%%memory-status-autoboot-jumper-bit	    memstat)
	    (ldb hw:%%memory-status-parity-error	    memstat)
	    (ldb hw:%%memory-status-spare-19-20		    memstat)	;left floating!
	    (ldb hw:%%memory-status-read-md-will-trans-trap memstat)
	    (ldb hw:%%memory-status-read-md-will-fault	    memstat)
	    (ldb hw:%%memory-status-vma-not-boxed-bit       memstat)
	    (ldb hw:%%memory-status-md-not-boxed-bit        memstat)
	    (ldb hw:%%memory-status-cycle-type              memstat)
	    (ldb hw:%%memory-status-transport-type          memstat)
	    (ldb hw:%%memory-status-gc-trap-enable          memstat)
	    (ldb hw:%%memory-status-md-written-lately       memstat)
	    (ldb hw:%%memory-status-nubus-bootstrap-mode    (lognot memstat))
	    (ldb hw:%%memory-status-eco-jumper-number       memstat)
	    (ldb hw:%%memory-status-nubus-slot-id           memstat)))

(defun decode-processor-status-register (&optional (processor-bits (hw:read-processor-status)))
	;< 31:19 @\ Undefined @cr
	;< 18 @\ Processor ALU_BOXED bit @cr
	;< 17 @\ Processor D_JUMP bit (active low) @cr
	;< 16 @\ Processor JUMP bit (active low) @cr
	;< 15:13 @\ Undefined @cr
	;< 12:9 @\ Return Destination Immediate @cr
	;< 8:4 @\ FPU Status Outputs @cr
	;< 3:0 @\ ECO jumper number @cr
  (format t "~%ECO jumpers          #b~4,'0b~
	     ~%Floating point status  #x~x~
	     ~%Floating point ready ~[yes~;no~]~
	     ~%Global return frame #x~x~
	     ~%Jump bit   ~s~
	     ~%Delayed jump bit   ~s~
	     ~%Boxed bit ~s~
	     ~%Return code #x~x~%"
	  (ldb hw:%%processor-status-eco-jumper processor-bits)
	  (ldb hw:%%processor-status-floating-point-status processor-bits)
	  (ldb hw:%%processor-status-floating-point-ready processor-bits)
	  (ldb hw:%%processor-status-global-return-frame processor-bits)	;four bits
	  ;; unused (byte 3. 13.)
	  (ldb hw:%%processor-status-jump-bit processor-bits)
	  (ldb hw:%%processor-status-delayed-jump-bit processor-bits)
	  (ldb hw:%%processor-status-alu-boxed-bit processor-bits)
	  (ldb hw:%%processor-status-return-code processor-bits)
	  ;; unused (byte 12. 18.)
    ))

(defun dump-gc-ram (&optional (stream t))
  (format stream "~%GC RAM:~:[ k halted ~; k running ~]~%" (k-running-p))
  (dotimes (q 4096.)
    (show-gc-ram q stream))
  )

(defun show-gc-ram (quantum stream)
  (let ((bits (read-gc-ram quantum)))
    (format stream "~&~4,48d Volatility ~d ~:[not oldspace~;oldspace~]"
	    quantum
	    (ldb hw:%%gc-ram-quantum-volatility bits)
	    (= (ldb hw:%%gc-ram-quantum-oldspace bits) hw:$$oldspace))))

(defun dump-gc-ram-brief (&optional (stream t))
  (format stream "~%GC RAM brief:~:[ k halted ~; k running ~]~%" (k-running-p))
  (let ((items-per-line 16.))
    (dotimes (q (/ 4096. items-per-line))
      (format stream "~%~X  " q)
      (dotimes (c items-per-line)
	(show-gc-ram-brief (+ (* q items-per-line) c) stream))))
  )

(defun show-gc-ram-brief (quantum stream)
  (let ((bits (read-gc-ram quantum)))
    (format stream "~:[N~;O~]V~d "
	    (= (ldb hw:%%gc-ram-quantum-oldspace bits) hw:$$oldspace)
	    (ldb hw:%%gc-ram-quantum-volatility bits))))

(defun find-physical-memory (&optional (max-chunk 32.))
 ;returns a bit map where each 1 implies the existance of 1 mega-byte of memory.
  (cond (lam:*falcon-memory-on-nubus* (1- (ash 1 lam:*falcon-nubus-memory-board-size-in-megabytes*)))
	(t
	 (if lam:*local-debugging* (setq max-chunk 16.))  ;better not get bus timeouts in local mode.
	 (setq max-chunk (min max-chunk 15.))		; +++  <11-Nov-88 rg>  software memory
	 (labels ((mark-physical-memory (chunk)	;  allocation screws up if 16 meg or more.
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
	   (mark-physical-memory (1- max-chunk))))))

(defun why ()
  (let ((msg (cond ((k-stopped-at-halt-inst-p)
		    (user#:get-illop-string (ldb (byte 24. 0.) (k-read-spy-mmfio))))
		   ;;above, we want real USER, not K-USER, because get-illop-string is in 
		   ;; compiler environment.
		   ((null k-run-flag)
		    "The machine has been stopped.")
		   (t
		    "The machine is running."))))
    (format t "~%~A" msg)
    msg))

(defun dump-transporter-ram (&optional (stream t))
  (format stream "~%Transporter RAM: -- vma-boxed, trans-mode(2), trans-type (2)~%")
  (format stream "--MD boxed(1), datatype(5)~%")
  (dotimes (md-boxed 2.)
    (dotimes (datatype 64.)
      (format stream "~%~B ~2,'0d  ]" md-boxed datatype)
      (dotimes (vma-boxed 2.)
	(dotimes (transporter-mode 4.)
	  (dotimes (transporter-type 4.)
	    (let ((ram-data (k-read-transporter-ram vma-boxed md-boxed
						    transporter-type transporter-mode
						    datatype)))
	      (format stream " ~X" ram-data))))))))

(defun dump-transporter-ram-via-generic (&optional (stream t))
  (let ((items-per-line 32.))
    (format stream "~2%Transporter RAM via generic: ~:[ k halted ~; k running ~]" (k-running-p))
    (dotimes (line (/ 4096. items-per-line))
      (format stream "~&~3x" (* line items-per-line))
      (dotimes (count items-per-line)
	(let ((data (read-transporter-ram (+ (* line items-per-line) count))))
	  (format stream " ~X" (logand data #o17)))))))


(defun fast-address-test-transporter-via-generic ()
  (lam:fast-address-test-kernal 'write-transporter-ram 'read-transporter-ram 0 4 12. "F.A.T. transporter via generic"))

(defun k-decode-transporter-address (address)
  (values (ldb (byte 1 11.) address)		;unfortunately, these dont have symbolic definitions yet.
	  (ldb (byte 1 10.) address)
	  (ldb (byte 2 8) address)	;trans-type from instruction
	  (ldb (byte 2 6) address)	;trans-mode semi-constant from memory-control-register.
	  (ldb (byte 6 0) address)))

(defun kbg-read-transporter-ram (address)
  (multiple-value-bind (vma-boxed md-boxed transporter-type transporter-mode datatype)
      (k-decode-transporter-address address)
    (k-read-transporter-ram vma-boxed md-boxed transporter-type transporter-mode datatype)))

(defun kbg-write-transporter-ram (address data)
  (multiple-value-bind (vma-boxed md-boxed transporter-type transporter-mode datatype)
      (k-decode-transporter-address address)
    (k-write-transporter-ram vma-boxed md-boxed transporter-type transporter-mode datatype data)))

;;;;;;;;
;;; PCD
;;;;;;;;

(defvar pcd-status-values
	     #("invalid"
	       "wired"
	       "normal"
	       "age-1"
	       "age-2"
	       "age-3"
	       "flushable"
	       "pre-paged"))

(defvar pcd-status-values-brief
	     #("I"
	       "W"
	       "N"
	       "A1"
	       "A2"
	       "A3"
	       "F"
	       "P"))

(defun decode-pcd (entry stream)
  (format stream "~5,'0x ~9a ~
                         ~:[    ~;not-~]modified ~
                         read-~:[write~;only ~] ~
                         ~@[~*write-mar~] ~
                         ~@[~*read-mar~]"
	  (ldb pcd::%%pcd-virtual-cluster-number entry)
	  (aref pcd-status-values (ldb pcd::%%pcd-status entry))
	  (ldb-test pcd::%%pcd-clean-bit     entry)
	  (ldb-test pcd::%%pcd-read-only-bit entry)
	  (ldb-test pcd::%%pcd-write-mar-bit entry)
	  (ldb-test pcd::%%pcd-read-mar-bit  entry)))

(defun show-entire-pcd (&optional (stream t))
  (dotimes (index vinc:*physical-memory-max-clusters*)
    (show-pcd index stream)))

(defun show-pcd (index stream)
  (format stream "~&~4,'0x " index)
  (decode-pcd (kbug-read-pcd index) t))			;(pcd::read-pcd index)

(defun show-entire-pcd-brief (&optional (stream t))
  (let ((items-per-line 8))
    (format stream "~2%PCD brief")
    (dotimes (line (/ vinc:*physical-memory-max-clusters* items-per-line))
      (format stream "~&~3x " (* line items-per-line))
      (dotimes (count items-per-line)
	(let* ((index (+ (* line items-per-line) count))
	       (pcd (kbug-read-pcd index))	       ;(pcd::read-pcd index)
	       (pcd-vcn (ldb pcd::%%pcd-virtual-cluster-number pcd))
	       (pcd-status (ldb pcd::%%pcd-status pcd))
	       (pcd-ro (ldb-test pcd::%%pcd-read-only-bit pcd)))
	  (cond ((zerop pcd-status)
		 (format stream ".      "))
		(t (format stream "~A~A~4,'0x "
			   (aref pcd-status-values-brief pcd-status)
			   (if pcd-ro "R" "W")
			   pcd-vcn)))
	  )))))
	
(defun kbug-read-pcd (index)
  ;"safe" with respect to processor!
  ;seems to wind up at physical #x48!
  (lam:k-mem-read-word-address (+ (dpb #x48 vinc:%%cluster-number 0) index))	;boot:*physical-cluster-table-location
  ;(kbug-generic-read-memory (+ #x4000 index))
  )

(defun kbug-map-virtual-address-via-pcd (virt-adr)
  ;because the map is not directly available, we have to manually search the PCD, which goes the other way.
  ;to save time, we use a heuristic.  We first search up from 0 till we get to a bunch of NULLS,
  ;then search down from #x800. (this probably should be related to phys-max-memory..)
  (let ((virt-cluster (ldb vinc::%%cluster-number virt-adr))
	(offset (ldb vinc::%%offset-in-cluster virt-adr)))
    (prog top (index count-nulls pcd)
	  (setq index 0 count-nulls 0)
      l	  (cond ((zerop (setq pcd (kbug-read-pcd index)))
		 (cond ((> (incf count-nulls) 5.)
			(setq index #x800 count-nulls 0)
			(prog ()
			  l   (cond ((zerop (setq pcd (kbug-read-pcd (setq index (1- index)))))
				     (cond ((> (incf count-nulls) 5)
					    (return-from top nil))))
				    ((= virt-cluster (ldb pcd::%%pcd-virtual-cluster-number pcd))
				     (return-from top (+ offset (ash index 10.)))))
			     (go l)))))
		((= virt-cluster (ldb pcd::%%pcd-virtual-cluster-number pcd))
		 (return-from top (+ offset (ash index 10.)))))
         (incf index)
	 (go l))))

(defun kbug-generic-read-memory-safe (addr)
  ;read virtual memory without touching processor or assuming it is in kbug2.
  ;assumes pcd is set up.
  (let ((phys-addr (kbug-map-virtual-address-via-pcd addr)))
    (lam:k-mem-read-word-address phys-addr)))


(defvar qm-status-values-brief #("E" "A" "x" "M"))
;MAPPED seems to mean it has been assigned space on swapping device.
; as it stands, only stuff in the cold load seems to wind up in MAPPED status, other stuff in ALLOCATED.

(defun decode-quantum-map (entry stream)
  (format stream "~[EMPTY    ~;ALLOCATED~;ERROR    ~;MAPPED   ~] ~4,48D ~2,48D ~4,48D"
	  (quantum-map::quantum-status-bits entry)
	  (quantum-map::region-origin      entry)
	  (quantum-map::quantum-device     entry)
	  (quantum-map::quantum-dqin       entry)))

(defun show-quantum-map (quantum stream)
  (let ((bits (quantum-map::read-quantum-map quantum)))
    (format stream "~&~4,'0x " quantum)
    (decode-quantum-map bits stream)
    ))

(defun show-entire-quantum-map (&optional (stream t))
  (dotimes (index 4096.)
    (show-quantum-map index stream)))

(defun show-entire-quantum-map-brief (&optional (stream t))
  (let ((items-per-line 16.))
    (format stream "~2%Quantum map brief")
    (dotimes (line (/  4096. items-per-line))
      (format stream "~&~3x " (* line items-per-line))
      (dotimes (count items-per-line)
	(let* ((index (+ (* line items-per-line) count))
	       (qm (quantum-map::read-quantum-map index))
	       (qm-status (quantum-map::quantum-status-bits qm))
	       (qm-ro (quantum-map::region-origin      qm))
	       (qm-dev (quantum-map::quantum-device    qm))
	       (qm-dq  (quantum-map::quantum-dqin      qm)))
	  (declare (ignore qm-dev qm-dq))
	  (cond ((zerop qm-status)
		 (format stream ".    "))
		(t (format stream "~A~4O "
			   (aref qm-status-values-brief qm-status)
			   qm-ro)))
	  )))))


(defun decode-region-bits (bits stream)
  (format stream "~[static   ~;flippable~] ~
                  ~[copyspace~;newspace ~] ~
                  ~[free     ~;invalid  ~;unboxed  ~;cons     ~;structure~;code     ~:;**error**~] ~
		  read-~:[write~;only ~] ~
                  ~:[not~;   ~] scavengable ~
                  Swapin quantum: ~d"
	  (ldb region-bits::%%region-bits-flippable     bits)
	  (ldb region-bits::%%region-bits-new-space     bits)
	  (region-bits:region-space-type                bits)
	  (region-bits:region-read-only?                bits)
	  (region-bits:region-scavenge-enable?          bits)
	  (region-bits:region-swapin-quantum            bits)))

(defun show-region-bits (entry stream)
  (format stream "~%~4d " entry)
  (decode-region-bits (region-bits::read-region-bits entry) stream))



(defstruct (remote-object 
	     (:constructor make-remote-object (string type address))
	     (:predicate remote-object?)
	     (:print-function print-remote-object))
  string
  type
  address)

(defun print-remote-object (object stream depth)
  (declare (ignore depth))
  (si::printing-random-object (object stream)
    (format stream "~a ~d" (remote-object-string object)
	    (remote-object-address object))))

(defun make-remote-maker (type string)
  `(DEFUN ,(intern (concatenate 'string "MAKE-" (string type))) (ADDRESS)
     (MAKE-REMOTE-OBJECT ,string (QUOTE ,type) ADDRESS)))

(defun make-remote-predicate (type)
  `(DEFUN ,(intern (concatenate 'string (string type) "?")) (OBJECT)
     (AND (REMOTE-OBJECT? OBJECT)
	  (EQ (QUOTE ,type) (REMOTE-OBJECT-TYPE OBJECT)))))

(defmacro define-remote-object (type string)
  `(PROGN ,(make-remote-maker type string)
	  ,(make-remote-predicate type)))

(define-remote-object remote-nil       "Remote NIL")
(define-remote-object remote-cons-cell "Remote CONS")
(define-remote-object remote-symbol    "Remote SYMBOL")
(define-remote-object remote-fixnum    "Remote FIXNUM")

(defun convert-remote-boxed-object (q)
  (let ((datatype (ldb vinc::%%data-type q))
	(contents (logand q #x3FFFFFF)))    ; pointer field
    (cond ((= datatype vinc:$$dtp-nil) (convert-nil contents))
	  ((= datatype vinc:$$dtp-symbol)
	   (convert-symbol contents))
	  ((= datatype vinc:$$dtp-fixnum)
	   (convert-fixnum contents))
	  ((= datatype vinc:$$dtp-cons)
	   (convert-cons-cell contents))
	  (t (zl::ferror nil "Cannot convert ~s" q)))))

(defun convert-nil (contents)
  (if (= contents 0)
      (make-remote-nil 0)
      (error nil "Bogus NIL found.")))

(defun convert-fixnum (contents)
  (make-remote-fixnum contents))

(defun convert-symbol (contents)
  (if (= contents 5.)
      (make-remote-symbol contents)
      (error "Illegal symbol found.")))

(defun convert-cons-cell (address)
  (make-remote-cons-cell address))

(defun check-remote-type (object type)
  (check-type object remote-object)
  (when (not (eq (remote-object-type object) type))
    (error "Wrong type argument")))

(defun remote-car (object)
  (check-type object remote-object)
  (ecase (remote-object-type object)
    (remote-nil        object)
    (remote-cons-cell
     (convert-remote-boxed-object (kbug-generic-read-memory (remote-object-address object))))))

(defun remote-cdr (object)
  (check-type object remote-object)
  (ecase (remote-object-type object)
    (remote-nil   object)
    (remote-cons-cell  (convert-remote-boxed-object
		    (kbug-generic-read-memory
		      (logior 1. (remote-object-address object)))))))

(defun remote-print (object stream)
  (check-type object remote-object)
  (ecase (remote-object-type object)
    (remote-nil       (print-remote-nil stream))
    (remote-fixnum    (print-remote-fixnum object stream))
    (remote-cons-cell (print-remote-cons-cell object stream))))

(defun print-remote-nil (stream)
  (format stream "()"))

(defun print-remote-cons-cell (object stream)
  (format stream "(")
  (block print-loop
    (do ((frob object (remote-cdr frob)))
	(())
      (remote-print (remote-car frob) stream)
      (if (not (remote-cons-cell? (remote-cdr frob)))
	  (if (remote-nil? (remote-cdr frob))
	      (progn (return-from print-loop nil))
	      (progn (format stream " . ")
		     (remote-print (remote-cdr frob) stream)
		     (return-from print-loop nil)))
	  (format stream " "))))
  (format stream ")"))

(defun print-remote-fixnum (object stream)
  (format stream "~s"
	  (let ((contents (logand #x03ffffff (remote-object-address object))))
	  (+ (* (- (expt 2. 24.)) (ldb vinc::%%fixnum-sign-bit contents))
	     (ldb (byte (1- (byte-position vinc::%%fixnum-sign-bit))
			0)
		  contents)))))

;;;;;;;;;;;;;;;;
;;; Region data
;;;;;;;;;;;;;;;;

(defun show-all-regions (&optional (stream t))
  (format stream "~2%All regions:")
  (dotimes (region 4096.)		;max possible number of regions (= # quantums).
    (let* ((start (vinc:quantum->address region))  ;start is implicit in region number.
	   (end (region-data::region-end region))
	   (bits (region-bits::read-region-bits region))
	   (region-space-type (region-bits:region-space-type bits)))
      (cond ((= region-space-type region-bits::$$region-space-free)
	     (do ((first-region region)
		  (r0 (1+ region) (1+ r0)))
		 ((or (= r0 4096.)
		      (not (= (region-bits:region-space-type
				(region-bits:read-region-bits r0))
			      region-bits::$$region-space-free)))
		  (format stream "~%Regions ~D up to ~D free." first-region r0))
	       (incf region)))
	    ((= region-space-type region-bits::$$region-space-invalid)
	     (global:fsignal "Random invalid region ~D" region))
	    (t
	     (show-region region stream)
	     (do ((r0 (1+ region) (1+ r0))
		  (count 1 (1+ count)))
		 ((>= count (vinc:quantum-number (- end start))))
	       (let* ((r0-bits (region-bits::read-region-bits r0))
		      (r0-space-type (region-bits:region-space-type r0-bits)))
		 (cond ((not (= r0-space-type region-bits::$$region-space-invalid))
			(global:fsignal "Region ~D not invalid, it should have been." r0))))
	       (incf region)))
	     ))))

(defun show-region-data (region stream)
  (let ((region-free-pointer (region-data::region-free-pointer region))
	(region-end 	  (region-data::region-end          region)))
    (format stream "~%Origin       #x~8,'0x (cluster #x~x)~
             ~%GC pointer   #x~8,'0x~
             ~%Free pointer #x~8,'0x (cluster #x~x)~
             ~%End          #x~8,'0x (cluster #x~x)"
	    (vinc:quantum->address            region)
	    (vinc:quantum->cluster 	    region)
	    (region-data::region-gc-pointer   region)
	    region-free-pointer
	    (if (numberp region-free-pointer) (ldb vinc:%%cluster-number region-free-pointer))
	    region-end
	    (if (numberp region-end) (ldb vinc:%%cluster-number region-end)))))

(defun show-region (region stream)
  (show-region-bits region stream)
  (show-region-data region stream))



(defvar *latest-history* nil)

(defun kbug-generic-read-inst-via-processor (pc)	;this uses physical machine to do virtual-to-physical translation.
  (setq pc (logior #x2000000 (ash pc 1)))
  (logior (ash (kbug-generic-read-memory (1+ pc)) 32.) (kbug-generic-read-memory pc)))

(defun kbug-generic-read-inst-safe (pc)	;this uses PCD inverse mapping to do virtual-to-physical translation.
  (setq pc (logior #x2000000 (ash pc 1)))
  (let ((phys-adr (kbug-map-virtual-address-via-pcd pc)))
    (values (logior (ash (lam:k-mem-read-word-address (1+ phys-adr)) 32.)
		    (lam:k-mem-read-word-address phys-adr))
	    phys-adr)))

(defun kbug-generic-write-inst-via-processor (pc inst)	;caution! map in machine must be set for this to work.  See above.
  (setq pc (logior #x2000000 (ash pc 1)))
  (let ((w0 (logand inst #xFFFFFFFF inst))
	(w1 (ash inst -32.)))
    (kbug-generic-write-memory pc w0 t)
    (kbug-generic-write-memory (1+ pc) w1 t)))

(defun show-history (&optional inhibit-symbolic-address print-bits reset-history)     ;; $$$ Changed arg order <08-Nov-88 wkf>
  (let ((history (or (and (null reset-history)
			  *latest-history*)
		     (setq *latest-history* (lam:get-history))))
	(current-pointer 0))
    (do ((command (peek-char) (peek-char)))
	(nil)
      (let* ((addr (logand #xFFFFFF (aref history current-pointer))))
	(if inhibit-symbolic-address 
	    (format t "~&~6,'0x               " addr)
	  (format t "~&#x~x ~30a  " addr (kbug-symbolic-address addr)))
	(let ((inst (kbug-generic-read-inst-via-processor addr)))
	  (print-instruction inst addr)
	  (if print-bits (lam:print-bits inst)))
	(case command
	  ((#\c-l #\clear-screen) (read-char) (zl:send *terminal-io* :clear-window))
	  ((#\c-r) (read-char) (setq *latest-history* (lam:get-history)
				     history *latest-history*))
	  ((#\line #\space #\c-n) (read-char) (incf current-pointer))
	  ((#\rubout #\c-p) (read-char) (decf current-pointer)
			    (when (minusp current-pointer)
			      (zl:send *terminal-io* :beep)
			      (incf current-pointer)))
	  ((#\quote) (incf current-pointer))
	  (otherwise
	   (zl:catch-error-restart ((sys:abort) "Return to HISTORY EXAMINER.")
	     (multiple-value-bind (sexp flag)
		 (zl:with-input-editing (*terminal-io* '((:full-rubout :full-rubout)
							 (:activation char= #\end)
							 (:prompt prompt-for-eval)))
		   (read))
	       (if (eq flag :full-rubout)
		   ()
		 (progn (terpri) (prin1 (eval sexp))))))))))))

(defun show-history-to-stream (&optional (stream t) inhibit-symbolic-address print-bits reset-history)
                                         ;;; $$$ Changed arg order <08-Nov-88 wkf>
  (let ((history (or (and (null reset-history)
			  *latest-history*)
		     (setq *latest-history* (lam:get-history)))))
    (do ((current-pointer 0 (1+ current-pointer))
	 (end (global:array-length history))
	 (data nil)
	 (last-inst nil))
	((or (= current-pointer end)
	     (null (setq data (aref history current-pointer)))))
      (let* ((addr (logand #xFFFFFF data))
	     (inst (kbug-generic-read-inst-via-processor addr)))
	(cond ((or (null last-inst)
		   (not (= inst last-inst)))
	       (format stream "~&"))
	      (t (format stream " .. ")))
	(if inhibit-symbolic-address
	    (format stream "~6,'0x " addr)
	  (format stream "#x~x~30a " addr (kbug-symbolic-address addr)))
	(cond ((or (null last-inst)
		   (not (= inst last-inst)))
	       (format stream "	")
	       (print-instruction inst addr stream)))
	;(if print-bits (lam:print-bits inst))		;** fix this.
	(setq last-inst inst)
	))))
			  

(defun prompt-for-eval (stream ignore)
  (format stream "~&Eval> "))
	 
(defun show-entire-datatype-ram (&optional (stream t))
  (let ((items-per-line 32.))
    (format stream "~2%Datatype ram:")
    (dotimes (line (/ (ash 1 17.) items-per-line))
      (format stream "~&~3x " (* line items-per-line))
      (dotimes (count items-per-line)
	(let ((bit (lam:k-read-datatype-ram (+ (* line items-per-line) count))))
	  (format stream "~o " bit))))))


(defun show-entire-datatype-ram-run-length-encoded (&optional (stream t))
  (let ((last-bit 0)
	(first-adr 0)
	(count 0))
    (do ((adr 0 (1+ adr)))
	((>= adr (ash 1 17.))
	 (format stream "~%#x~x bits starting at #x~x, ~x" count first-adr last-bit))
      (let ((bit (lam:k-read-datatype-ram adr)))
	(cond ((= bit last-bit)
	       (incf count))
	      (t (format stream "~%#x~x bits starting at #X~x, ~x" count first-adr last-bit)
		 (setq last-bit bit first-adr adr count 1)))))))

;--- area stuff.

;** kludgy absolute constants **
(defvar *manual-physical-cluster-data-table* #x4000)	;should be same as gr:*physical-cluster-data-table*
(defvar *manual-quantum-map*	  #x6000)		;should be same as gr:*quantum-map*
(defvar *manual-region-bits*	  #x7000)		;should be same as gr:*region-bits*
(defvar *manual-region-free-pointer* #xC000)		;should be same as gr:*region-free-pointer*
(defvar *manual-region-end*	     #xD000)		;should be same as gr:*region-end*
(defvar *manual-region-gc-pointer*   #xE000)		;should be same as gr:*region-gc-pointer*
(defvar *manual-area-region-data* #x11000)	;should be same as gr:*area-region-data*
(defvar *manual-area-region-size* #x11200)	;should be same as gr:*area-region-size*
(defvar *manual-area-region-bits* #x11100)	;should be same as gr:*area-region-bits*
(defvar *manual-region-list-thread* #x10000)	;should be same as gr:*region-list-thread*

;there is a fancier version of this in kbug-generic, if things are winning enuf for that to be useful.
(defun kbug-show-all-areas (&optional (stream t))
  (dotimes (area area-data:*number-of-areas*)
    (kbug-show-area area stream)))

(defun kbug-show-area (area &optional (stream t))
  (let ((area-region-data (kbug-generic-read-memory (+ area *manual-area-region-data*)))
	(area-region-size (kbug-generic-read-memory (+ area *manual-area-region-size*)))
	(area-region-bits (kbug-generic-read-memory (+ area *manual-area-region-bits*))))
    (format stream "~%Area ~X, data ~x, size ~x, bits ~x" area area-region-data area-region-size area-region-bits))
  )

(defun kbug-show-all-regions (&optional (stream t))
  (dotimes (region region-data:*number-of-regions*)
    (kbug-show-region region stream)))

(defun kbug-show-region (region &optional (stream t))
  (let* ((region-bits (kbug-generic-read-memory (+ region *manual-region-bits*)))
	 (region-free-pointer (kbug-generic-read-memory (+ region *manual-region-free-pointer*)))
	 (region-end (kbug-generic-read-memory (+ region *manual-region-end*)))
	 (region-gc-pointer (kbug-generic-read-memory (+ region *manual-region-gc-pointer*)))
	 (region-space-type (ldb region-bits:%%region-bits-space-type region-bits)))
    (format stream "~%Region ~x, bits ~x, free-pointer ~x, end ~x, gc-pointer ~x, space-type ~x~%   "
	    region region-bits region-free-pointer region-end region-gc-pointer region-space-type)
    (decode-region-bits region-bits stream)))




