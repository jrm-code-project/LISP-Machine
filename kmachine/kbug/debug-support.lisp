;;; -*- Mode:LISP; Package:K-KBUG; Base:10.; Readtable:CL -*-

;These functions sort of get called on the lambda if executing "prims" code "intended" for
; the K.  This is a really bad idea.  So lets try to phase this out.  RG 6/19/88

(defun saving-current-k-pc (thunk)	;this has no hope whatsoever of saving what needs
  (let ((current-pc (k-read-spy-pc)))	; to be saved, and will clobber stuff in KBUG2 mode.
    (unwind-protect			; So lets flush it.
      (funcall thunk)
      (k-set-pc current-pc))))

;(defun hw::unboxed-constant (n)
;  n)

(defun 32dpb (source byte destination)
  (let* ((width    (byte-size byte))
	 (position (byte-position byte))
	 (mask     (logand #XFFFFFFFF (1- (expt 2. width)))))
    (logior
      (ash (logand mask source) position)
      (logand (logxor #XFFFFFFFF (ash mask position)) destination))))

(defmacro define-unimplemented-hw-function (name)
  `(DEFUN ,name (&rest args)
     (apply #'error "~S called on ~#[no arguments~;~S~;~S and ~S~:;~
                  ~@{~#[~1;and ~]~S~^, ~}~]." (QUOTE ,name) args)))

(defun hw::24+ (a b)
  (+ a b))
(define-unimplemented-hw-function hw::32-)
(define-unimplemented-hw-function hw::32-1-)
(define-unimplemented-hw-function hw::32-1+)
(define-unimplemented-hw-function hw::32-2-)
(define-unimplemented-hw-function hw::32-2+)
(define-unimplemented-hw-function hw::32-4-)
(define-unimplemented-hw-function hw::32-4+)
(define-unimplemented-hw-function hw::32+)
(defun hw::32= (a b) (= a b))
(define-unimplemented-hw-function hw::32>)
(define-unimplemented-hw-function hw::32>=)
(define-unimplemented-hw-function hw::32logbitp)
(define-unimplemented-hw-function hw::32logior)
(define-unimplemented-hw-function hw::call)
(define-unimplemented-hw-function hw::ch-open-call)
(define-unimplemented-hw-function hw::ch-return)
(define-unimplemented-hw-function hw::ch-return-one-value)
(define-unimplemented-hw-function hw::ch-return-multiple-values)
(define-unimplemented-hw-function hw::ch-tcall)
(define-unimplemented-hw-function hw::ch-topen)
(define-unimplemented-hw-function hw::ch-topen-call)
(define-unimplemented-hw-function hw::dispatch)

(defun hw::dpb (&rest stuff)
  (apply #'32dpb stuff))

(defun hw::dpb-boxed (&rest stuff)
  (apply #'dpb stuff))
(define-unimplemented-hw-function hw::dpb-xor)

(defun hw::dpb-unboxed (&rest stuff)
  (apply #'dpb stuff))

(define-unimplemented-hw-function hw::field=)

;;; Crock.
(defun hw::jump (location)
  (k-set-pc location))

(defun hw::ldb (from byte into)
  (dpb (ldb byte from) (byte (byte-size byte) 0) into))

(define-unimplemented-hw-function hw::ldb-not)
(define-unimplemented-hw-function hw::md-start-write-boxed)
(define-unimplemented-hw-function hw::md-start-write-unboxed)
(define-unimplemented-hw-function hw::md-start-write-no-gc-trap)
(define-unimplemented-hw-function hw::md-start-write-no-gc-trap-boxed)
(define-unimplemented-hw-function hw::md-start-write-no-gc-trap-unboxed)

(defun hw::memory-wait ()
  nil
 ;  (saving-current-k-pc
 ;    #'(lambda () nil))
  )


(defun hw::nop ()
  nil
 ;  (saving-current-k-pc
 ;    #'(lambda () nil))
  )

(define-unimplemented-hw-function hw::open-frame)
(define-unimplemented-hw-function hw::read-gc-ram)

(defun hw::read-map ()
  (k-read-current-map-address)
;  (saving-current-k-pc
;    #'(lambda () (k-read-current-map-address)))
  )

;(defun hw::read-md ()		;for lambda
;  (saving-current-k-pc
;    #'(lambda () (r-md))))

(defun hw::read-memory-control ()
  (k-read-memory-control)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-memory-control)))
  )

(defun hw::read-memory-status ()
  (k-read-memory-status)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-memory-status)))
  )

(defun hw::read-open-active-return ()
  (k-read-oar)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-oar)))
  )

(defun hw::read-processor-control ()
  (k-read-processor-control)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-processor-control)))
  )

(defun hw::write-processor-control (value)
  (k-write-processor-control value))

(defun hw::read-processor-status ()
  (lam:k-read-processor-status)
;  (saving-current-k-pc
;    #'(lambda ()
;	(lam:k-read-processor-status)))
  )

;(define-unimplemented-hw-function hw::read-processor-status)
(define-unimplemented-hw-function hw::read-return-pc-return-dest)

;(defun hw::read-vma ()
;  (saving-current-k-pc
;    #'(lambda () (r-vma))))

(defun hw::trap-off ()
  (k-read-trap-off)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-trap-off)))
  )

(define-unimplemented-hw-function hw::vma-start-read)
(define-unimplemented-hw-function hw::vma-start-read-cdr)
(define-unimplemented-hw-function hw::vma-start-read-cdr-no-transport)
(define-unimplemented-hw-function hw::vma-start-read-cdr-visible-evcp)
(define-unimplemented-hw-function hw::vma-start-read-cdr-will-write)

;-- these sort of amazing kludgy definitions are called from interpreted
; lambda code (such as system-table-ref).
;(defun hw::vma-start-read-no-transport (location boxed-vma boxed-md)
;  boxed-vma
;  boxed-md
;  (saving-current-k-pc
;    #'(lambda  ()
;	(k-read-virtual-memory location))))

;(defun hw::vma-start-read-vma-boxed-md-boxed (location)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-read-virtual-memory location))))

;(define-unimplemented-hw-function hw::vma-start-read-vma-unboxed-md-boxed)

(defun hw::vma-start-read-no-transport-vma-unboxed-md-unboxed (location)
  (k-read-virtual-memory location)
;  (saving-current-k-pc
;    #'(lambda  ()
;	(k-read-virtual-memory location)))
  )

(define-unimplemented-hw-function hw::vma-start-read-visible-evcp)
(define-unimplemented-hw-function hw::vma-start-read-will-write)
(define-unimplemented-hw-function hw::vma-start-read-will-write-vma-boxed-md-boxed)
(define-unimplemented-hw-function hw::vma-start-write)
(define-unimplemented-hw-function hw::vma-start-write-boxed)
(define-unimplemented-hw-function hw::vma-start-write-unboxed)
(define-unimplemented-hw-function hw::vma-start-write-no-gc-trap-boxed)
(define-unimplemented-hw-function hw::vma-start-write-no-gc-trap-unboxed)
(define-unimplemented-hw-function hw::write-call-hp-sp)		;new name
(define-unimplemented-hw-function hw::write-call-sp-hp)		;old name
(define-unimplemented-hw-function hw::write-gc-ram)

(defun hw::write-map (value)
  (k-write-current-map-entry value)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-write-current-map-entry value)))
  )

(define-unimplemented-hw-function hw::write-md)
(define-unimplemented-hw-function hw::write-md-boxed)
(define-unimplemented-hw-function hw::write-md-unboxed)

(defun hw::write-memory-control (value)
  (k-load-memory-control value)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-load-memory-control value)))
  )

(define-unimplemented-hw-function hw::write-open-active-return)

(defun hw::write-processor-control (value)
  (k-write-processor-control value)
;  (saving-current-k-pc
;    #'(lambda ()
;	(k-write-processor-control value)))
  )

(define-unimplemented-hw-function hw::write-transporter-ram)
(define-unimplemented-hw-function hw::write-vma-boxed)

(defun hw::write-vma-unboxed (value)
  (w-vma value)
;  (saving-current-k-pc
;    #'(lambda ()
;	(w-vma value)))
  )

(define-unimplemented-hw-function hw::o0)
(define-unimplemented-hw-function hw::o1)
(define-unimplemented-hw-function hw::o2)
(define-unimplemented-hw-function hw::o3)
(define-unimplemented-hw-function hw::o4)
(define-unimplemented-hw-function hw::o5)
(define-unimplemented-hw-function hw::o6)
(define-unimplemented-hw-function hw::o7)
(define-unimplemented-hw-function hw::o8)
(define-unimplemented-hw-function hw::o9)
(define-unimplemented-hw-function hw::o10)
(define-unimplemented-hw-function hw::o11)
(define-unimplemented-hw-function hw::o12)
(define-unimplemented-hw-function hw::o13)
(define-unimplemented-hw-function hw::o14)
(define-unimplemented-hw-function hw::o15)
(define-unimplemented-hw-function hw::write-o0)
(define-unimplemented-hw-function hw::write-o1)
(define-unimplemented-hw-function hw::write-o2)
(define-unimplemented-hw-function hw::write-o3)
(define-unimplemented-hw-function hw::write-o4)
(define-unimplemented-hw-function hw::write-o5)
(define-unimplemented-hw-function hw::write-o6)
(define-unimplemented-hw-function hw::write-o7)
(define-unimplemented-hw-function hw::write-o8)
(define-unimplemented-hw-function hw::write-o9)
(define-unimplemented-hw-function hw::write-o10)
(define-unimplemented-hw-function hw::write-o11)
(define-unimplemented-hw-function hw::write-o12)
(define-unimplemented-hw-function hw::write-o13)
(define-unimplemented-hw-function hw::write-o14)
(define-unimplemented-hw-function hw::write-o15)
(defsetf hw::o0  hw::write-o0)
(defsetf hw::o1  hw::write-o1)
(defsetf hw::o2  hw::write-o2)
(defsetf hw::o3  hw::write-o3)
(defsetf hw::o4  hw::write-o4)
(defsetf hw::o5  hw::write-o5)
(defsetf hw::o6  hw::write-o6)
(defsetf hw::o7  hw::write-o7)
(defsetf hw::o8  hw::write-o8)
(defsetf hw::o9  hw::write-o9)
(defsetf hw::o10 hw::write-o10)
(defsetf hw::o11 hw::write-o11)
(defsetf hw::o12 hw::write-o12)
(defsetf hw::o13 hw::write-o13)
(defsetf hw::o14 hw::write-o14)
(defsetf hw::o15 hw::write-o15)

(define-unimplemented-hw-function hw::r0)
(define-unimplemented-hw-function hw::r1)
(define-unimplemented-hw-function hw::r2)
(define-unimplemented-hw-function hw::r3)
(define-unimplemented-hw-function hw::r4)
(define-unimplemented-hw-function hw::r5)
(define-unimplemented-hw-function hw::r6)
(define-unimplemented-hw-function hw::r7)
(define-unimplemented-hw-function hw::r8)
(define-unimplemented-hw-function hw::r9)
(define-unimplemented-hw-function hw::r10)
(define-unimplemented-hw-function hw::r11)
(define-unimplemented-hw-function hw::r12)
(define-unimplemented-hw-function hw::r13)
(define-unimplemented-hw-function hw::r14)
(define-unimplemented-hw-function hw::r15)
(define-unimplemented-hw-function hw::write-r0)
(define-unimplemented-hw-function hw::write-r1)
(define-unimplemented-hw-function hw::write-r2)
(define-unimplemented-hw-function hw::write-r3)
(define-unimplemented-hw-function hw::write-r4)
(define-unimplemented-hw-function hw::write-r5)
(define-unimplemented-hw-function hw::write-r6)
(define-unimplemented-hw-function hw::write-r7)
(define-unimplemented-hw-function hw::write-r8)
(define-unimplemented-hw-function hw::write-r9)
(define-unimplemented-hw-function hw::write-r10)
(define-unimplemented-hw-function hw::write-r11)
(define-unimplemented-hw-function hw::write-r12)
(define-unimplemented-hw-function hw::write-r13)
(define-unimplemented-hw-function hw::write-r14)
(define-unimplemented-hw-function hw::write-r15)
(defsetf hw::r0  hw::write-r0)
(defsetf hw::r1  hw::write-r1)
(defsetf hw::r2  hw::write-r2)
(defsetf hw::r3  hw::write-r3)
(defsetf hw::r4  hw::write-r4)
(defsetf hw::r5  hw::write-r5)
(defsetf hw::r6  hw::write-r6)
(defsetf hw::r7  hw::write-r7)
(defsetf hw::r8  hw::write-r8)
(defsetf hw::r9  hw::write-r9)
(defsetf hw::r10 hw::write-r10)
(defsetf hw::r11 hw::write-r11)
(defsetf hw::r12 hw::write-r12)
(defsetf hw::r13 hw::write-r13)
(defsetf hw::r14 hw::write-r14)
(defsetf hw::r15 hw::write-r15)

;(define-unimplemented-hw-function hw::

(defmacro prims::defafun (&rest stuff)
  stuff
  nil)

(defvar *global-frame-table* '())

(defmacro prims::define-global-frame (name)
  (when (null (member name *global-frame-table* :key #'car))
    (setq *global-frame-table* (append *global-frame-table* (list (list name)))))
  `(QUOTE ,name))

(defun find-global-frame (name if-found if-not-found)
  (labels ((scan-list (tail count)
	     (if (null tail)
		 (funcall if-not-found)
		 (let ((this-element (first tail)))
		   (if (eq (first this-element) name)
		       (funcall if-found this-element count)
		       (scan-list (rest tail) (1+ count)))))))
    (scan-list *global-frame-table* 0)))

(defmacro prims::define-global-variable (frame name &optional value documentation)
  value documentation
  (find-global-frame frame
    #'(lambda (frame count)
	count
	(when (null (member name frame))
	  (setf (cdr frame) (append (cdr frame) (list name))))
	`(PROGN (EXPORT (LIST (QUOTE ,name)) (FIND-PACKAGE "GLOBAL-REGISTERS" *package*))
		(SI::PROCLAIM-SPECIAL (QUOTE ,name))
		(QUOTE ,name)))
    #'(lambda ()
	(error "Couldn't find frame named ~s." frame))))
  
(defmacro prims::define-global-constant (frame name value &optional documentation)
  value documentation
  (find-global-frame frame
    #'(lambda (frame count)
	count
	(when (null (member name frame))
	  (setf (cdr frame) (append (cdr frame) (list name))))
	`(PROGN (EXPORT (LIST (QUOTE ,name)) (FIND-PACKAGE "GLOBAL-REGISTERS" *package*))
		(SI::PROCLAIM-SPECIAL (QUOTE ,name))
		(QUOTE ,name)))
    #'(lambda ()
	(error "Couldn't find frame named ~s." frame))))

(defmacro prims::defsubst (&rest stuff)
  `(DEFUN ,@stuff))

(defmacro prims::dispatch (byte word &rest clauses)
  `(ERROR "Calling a dispatch."))

(defmacro prims::select-processor (&body clauses)
  `(PROGN ,@(cdr (assoc :lambda clauses))))

(export '(
	  >
	  >=
	  <
	  -
	  =
	  +
	  1+
	  1-
	  &rest
	  and
	  byte
	  byte-size
	  byte-position
	  prims::%car
	  prims::%cdr
	  compile
	  cond
 ;	  decf
	  prims::defafun
	  defconstant
	  prims::define-global-constant
	  prims::define-global-frame
	  prims::define-global-variable
	  defmacro
	  defsetf
	  prims::defsubst
	  defun
	  defvar
	  do
	  eq
	  eval
	  eval-when
	  export
 ;	  first
	  flet
 ;	  format
	  funcall
	  function
	  go
	  if
	  import
 ;	  incf
	  in-package
	  labels
	  lambda
	  let
	  let*
	  load
	  multiple-value-bind
	  nil
	  not
	  null
	  or
	  prog1
	  progn
	  proclaim
	  rest
	  select-processor
	  setf
	  setq
	  t
	  tagbody
	  unless
	  values
	  when
	  )
	(find-package "PRIMS" *package*))

(export '(
	  push
	  )
	(find-package "LI" *package*))