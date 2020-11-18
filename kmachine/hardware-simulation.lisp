;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

;;; This file defines the lambda code that simulates the hardware of
;;; the K.

(export '(
    k-32=
    k-dpb
    k-dpb-and
    k-dpb-and-boxed
    k-dpb-and-boxed-left
    k-dpb-and-unboxed
    k-dpb-boxed
    k-dpb-boxed-left
    k-dpb-ior
    k-dpb-ior-boxed
    k-dpb-ior-boxed-left
    k-dpb-ior-unboxed
    k-dpb-not
    k-dpb-not-boxed
    k-dpb-not-boxed-left
    k-dpb-not-unboxed
    k-dpb-unboxed
    k-dpb-xor
    k-dpb-xor-boxed
    k-dpb-xor-boxed-left
    k-dpb-xor-unboxed
    k-ldb
    k-ldb-and
    k-ldb-and-boxed
    k-ldb-and-boxed-left
    k-ldb-and-unboxed
    k-ldb-boxed
    k-ldb-boxed-left
    k-ldb-ior
    k-ldb-ior-boxed
    k-ldb-ior-boxed-left
    k-ldb-ior-unboxed
    k-ldb-not
    k-ldb-not-boxed
    k-ldb-not-boxed-left
    k-ldb-not-unboxed
    k-ldb-unboxed
    k-ldb-xor
    k-ldb-xor-boxed
    k-ldb-xor-boxed-left
    k-ldb-xor-unboxed
    k-md-start-write-option-0
    k-md-start-write-option-1
    k-md-start-write-unboxed-option-0
    k-md-start-write-unboxed-option-1
    k-read-gc-ram
    k-read-map
    k-read-md
    k-read-memory-control
    k-read-memory-status
    k-read-processor-control
    k-read-processor-status
    k-read-transporter-ram
    k-read-trap
    k-read-vma
    k-vma-start-read-cdr-option-0
    k-vma-start-read-cdr-option-1
    k-vma-start-read-cdr-option-2
    k-vma-start-read-cdr-option-3
    k-vma-start-read-md-unboxed-cdr-option-0
    k-vma-start-read-md-unboxed-cdr-option-1
    k-vma-start-read-md-unboxed-cdr-option-2
    k-vma-start-read-md-unboxed-cdr-option-3
    k-vma-start-read-md-unboxed-option-0
    k-vma-start-read-md-unboxed-option-1
    k-vma-start-read-md-unboxed-option-2
    k-vma-start-read-md-unboxed-option-3
    k-vma-start-read-option-0
    k-vma-start-read-option-1
    k-vma-start-read-option-2
    k-vma-start-read-option-3
    k-vma-start-read-unboxed-cdr-option-0
    k-vma-start-read-unboxed-cdr-option-1
    k-vma-start-read-unboxed-cdr-option-2
    k-vma-start-read-unboxed-cdr-option-3
    k-vma-start-read-unboxed-md-unboxed-cdr-option-0
    k-vma-start-read-unboxed-md-unboxed-cdr-option-1
    k-vma-start-read-unboxed-md-unboxed-cdr-option-2
    k-vma-start-read-unboxed-md-unboxed-cdr-option-3
    k-vma-start-read-unboxed-md-unboxed-option-0
    k-vma-start-read-unboxed-md-unboxed-option-1
    k-vma-start-read-unboxed-md-unboxed-option-2
    k-vma-start-read-unboxed-md-unboxed-option-3
    k-vma-start-read-unboxed-option-0
    k-vma-start-read-unboxed-option-1
    k-vma-start-read-unboxed-option-2
    k-vma-start-read-unboxed-option-3
    k-vma-start-write-option-0
    k-vma-start-write-option-1
    k-vma-start-write-unboxed-option-0
    k-vma-start-write-unboxed-option-1
    k-write-gc-ram
    k-write-map
    k-write-md-boxed
    k-write-md-unboxed
    k-write-memory-control
    k-write-processor-control
    k-write-transporter-ram
    k-write-vma-boxed
    k-write-vma-unboxed
    ))

(eval-when (compile load eval)
  (import '(micro::32-ldb micro::32-dpb)))

;;;;;;;;;;;;;;;;;;;;;;
;;; simulation errors
;;;;;;;;;;;;;;;;;;;;;;

(defun broken-simulation (string &rest format-args)
  (ferror nil "BROKEN SIMULATION: ~?" string format-args))

;;;;;;;;;;;;;;;;;;;
;;; ART-32B Kludge
;;;;;;;;;;;;;;;;;;;

(defun make-32b-array (size)
  (user:make-array (* size 2.) :type :art-16b))

(defun 32b-aref (array index)
  (let ((real-index (* index 2.)))
    (+ (ash (aref array real-index) 16.)
       (aref array (1+ real-index)))))

(defun 32b-aset (array index new-value)
  (let ((real-index (* index 2)))
    (when (or (not (integerp new-value))
              (minusp new-value)
              (> new-value (expt 2. 32.)))
      (ferror nil "Attempt to ~s an illegal value ~d" 'phys-mem-write new-value))
    (setf (aref array real-index)      (ldb (byte 16. 16.) new-value))
    (setf (aref array (1+ real-index)) (ldb (byte 16. 0.) new-value))))

(defsetf 32b-aref 32b-aset)

;;;;;;;;;;;;;;;;;;;;;;;
;;; DATUM manipulation
;;;;;;;;;;;;;;;;;;;;;;;

;;; In the simulator, we make the data be the cons
;;; of the boxed bit and the value.

;;; Actually, that is too hard and too slow.  Punt
;;; this idea.

(defmacro make-datum (ignore word) word)        ;ignore boxed bit
(defmacro datum-boxed-bit (ignore) (ferror nil "Don't use datum feature."))     ;ignore boxed bit
(defmacro datum-word (datum) datum)

(defmacro unboxed (data-type fixnum)
  (make-datum $$unboxed
              (32-dpb data-type %%k-data-type (datum-word fixnum))))

(defun unboxed-constant (thing) thing)

;;;;;;;;;;;;;;;;
;;; K Registers
;;;;;;;;;;;;;;;;

(defmacro define-k-register (name read-only?)
  (let ((var-name (intern (string-append "*" name "*")))
        (writer   (intern (string-append "K-WRITE-" name)))
        (reader   (intern (string-append "K-READ-"  name))))
    `(PROGN (DEFVAR ,var-name 0.)
            (DEFUN ,reader () (MAKE-DATUM $$UNBOXED ,var-name))
            (if ,read-only?
                '()
                (DEFUN ,writer (NEW-VALUE)
                  (SETQ ,var-name (DATUM-WORD NEW-VALUE)))))))

(define-k-register TRAP                t)
(define-k-register PROCESSOR-STATUS    t)
(define-k-register PROCESSOR-CONTROL nil)
(define-k-register MEMORY-STATUS       t)

;;; This is a pseudo-register that has the same bits as
;;; the trap register.  If it is not zero on
;;; a read, the trap register is loaded from here and
;;; we call trap.  A few of the memory bits in the machine
;;; are actually latched like this, but this is just a
;;; simulation.

(define-k-register MEMORY-TRAP-BITS  nil)

(defun turn-on-memory-trap-bit (byte)
  (setq *memory-trap-bits*
        (32-dpb 1. byte *memory-trap-bits*)))

;;; Other special registers

(defvar *VMA* 0.)
(defvar *RMD* 0.)
(defvar *WMD* 0.)
(defvar *GC-MD* 0.)
(defvar *memory-control* 0.)

;;;;;;;;;;
;;; Traps
;;;;;;;;;;

(defun initialize-trap-masks ()
  (dotimes (single-step-enable 2.)
    (dotimes (synchronous 2.)
      (dotimes (datatype 2.)
        (dotimes (overflow 2.)
          (dotimes (asynchronous 2.)
            (dotimes (master 2.)
              (setf (aref *trap-masks*
                          (dpb single-step-enable (byte 1. 0.)
                               (dpb synchronous (byte 1. 1.)
                                    (dpb datatype (byte 1. 2.)
                                         (dpb overflow (byte 1. 3.)
                                              (dpb asynchronous (byte 1. 4.)
                                                   (dpb master (byte 1. 5.) 0.)))))))
                    (logior *maskable-traps*
                            (logand (if (= master $$trap-enabled)
                                        *master-trap-mask*
                                        0)
                                    (logior (if (= single-step-enable $$trap-enabled)
                                                *single-step-mask*
                                                0)
                                            (if (= synchronous $$trap-enabled)
                                                *synchronous-trap-mask*
                                                0)
                                            (if (= datatype $$trap-enabled)
                                                *datatype-mask*
                                                0)
                                            (if (= overflow $$trap-enabled)
                                                *overflow-mask*
                                                0)
                                            (if (= asynchronous $$trap-enabled)
                                                *asynchronous-trap-mask*
                                                0)
                                            )))))))))))

(eval-when (load)
  (initialize-trap-masks))

(defvar *trap-location* #'(lambda (&rest ignore)
                            (ferror nil "No trap location yet!")))

(defmacro trap-point (thunk)
  `(LET ((THUNK ,thunk))
     (BLOCK TRAP-POINT
       (TAGBODY
        INTERRUPT-RETRY
           (RETURN-FROM TRAP-POINT
             (FUNCALL THUNK
                      #'(LAMBDA (VALUE) (RETURN-FROM TRAP-POINT VALUE))
                      #'(LAMBDA () (GO INTERRUPT-RETRY))))))))

(defun trap-tag-error (value)
  value
  (ferror nil "It is illegal to return from traps here."))

(defun trap (trap-tag interrupt-tag)
  (funcall *trap-location* trap-tag interrupt-tag))

(defvar *trap-mask* 0.)

(defun processor-traps? ()
  (not (zerop (logand *trap-mask* *trap*))))

(defun k-read-memory-control () (make-datum $$unboxed *memory-control*))

(defun k-write-memory-control (value)
  (setq *memory-control* (datum-word value))
  (setq *trap-mask* (aref *trap-masks* (32-ldb *memory-control* %%k-memory-control-trap-bits 0)))
  (trap-point
    #'(lambda (exit retry)
        (if (processor-traps?)
            (trap exit retry)
            '()))))

(defun reset-memory-control ()
  (setq *trap* 0.)
  (k-write-memory-control
    (make-datum $$unboxed
    (32-dpb $$trap-disabled %%k-memory-control-single-step-enable
    (32-dpb $$trap-enabled  %%k-memory-control-synchronous-trap-enable
    (32-dpb $$trap-enabled  %%k-memory-control-datatype-trap-enable
    (32-dpb $$trap-enabled  %%k-memory-control-overflow-trap-enable
    (32-dpb $$trap-enabled  %%k-memory-control-asynchronous-trap-enable
    (32-dpb $$trap-enabled  %%k-memory-control-master-trap-enable
           0.)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simulated memory map
;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is what everyone looks at.  Gets changed when the vma gets changed.
(defvar *map-bits* 0.)

(defun read-map ()
  (micro::read-memory-map *vma*))

(defun setup-maps ()
  (setq *map-bits* (read-map)))

(defun write-map (value)
  (micro::write-memory-map *vma* value)
  (setup-maps))

(defun initialize-maps ()
  ;; Randomize the maps.
  (let ((n (expt 2. 32.))
        (random-list))
    (dotimes (i 1000.)
      (push (random n) random-list))
    (setq random-list (apply #'circular-list random-list))
    (dotimes (i 65536.)
      (micro::write-memory-map i (pop random-list)))))

(defun k-read-map () (make-datum $$unboxed *map-bits*))

(defun k-write-map (value) (write-map (datum-word value)))

(defun read-succeeds? ()
  (= $$can-read
     (ldb (if (= (ldb %%k-memory-control-low-high-map-select *memory-control*) $$low-trap-bits)
              %%k-map-low-valid-bit
              %%k-map-high-valid-bit)
          *map-bits*)))

(defun write-succeeds? ()
  (= $$can-write
     (ldb (if (= (ldb %%k-memory-control-low-high-map-select *memory-control*) $$low-trap-bits)
              %%k-map-low-trap-bits
              %%k-map-high-trap-bits)
          *map-bits*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WRITE-VMA READ-VMA WRITE-MD READ-MD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun k-write-vma-boxed (value)
  (setq *vma* (datum-word value))
  (setq *memory-status*
        (32-dpb (lognot $$boxed) %%k-memory-status-vma-not-boxed-bit *memory-status*))
  (setup-maps))

(defun k-write-vma-unboxed (value)
  (setq *vma* (datum-word value))
  (setq *memory-status*
        (32-dpb (lognot $$unboxed) %%k-memory-status-vma-not-boxed-bit *memory-status*))
  (setup-maps))

(defun k-read-vma ()
  (make-datum (lognot (32-ldb *memory-status* %%k-memory-status-vma-not-boxed-bit 0)) *vma*))

(defun k-write-md-boxed (value)
  (setq *WMD* (setq *gc-md* (datum-word value)))
  (setq *memory-status*
        (32-dpb (lognot $$boxed) %%k-memory-status-md-not-boxed-bit *memory-status*)))

(defun k-write-md-unboxed (value)
  (setq *WMD* (setq *gc-md* (datum-word value)))
  (setq *memory-status*
        (32-dpb (lognot $$unboxed) %%k-memory-status-md-not-boxed-bit *memory-status*)))

(defun k-read-md ()
  (trap-point
    #'(lambda (ignore retry)
        (when (and (= (32-ldb *memory-control* %%k-memory-control-master-trap-enable 0.) $$trap-enabled)
                   (not (zerop *memory-trap-bits*)))
          (setq *trap* (logior *memory-trap-bits* *trap*))
          (when (processor-traps?)
            (trap #'trap-tag-error retry)))))
  (make-datum (lognot (32-ldb *memory-status* %%k-memory-status-md-not-boxed-bit 0)) *RMD*))

;;;;;;;;;;;;;;;;;;;;
;;; Physical memory
;;;;;;;;;;;;;;;;;;;;


;;; These take a *long* time.
;;; about 5 - 10 minutes.
;(defun test-phys-mem ()
;  (format t "~&Initializing...")
;  (dotimes (i (expt 2. 22.))
;    (phys-mem-write-direct i i))
;  (format t "done.")

;(defun test-phys-mem ()
;  (dotimes (i (expt 2. 4.))
;    (format t "~D " i)
;    (dotimes (j (expt 2. 14.))
;      (let ((k (+ (ash i 14.) j)))
;       (when (not (= k (micro:%physical-memory-read-direct k)))
;         (ferror nil "~&wasn't the same at ~d ~d" i (phys-mem-read-direct i)))))))

(defun phys-mem-read (location)
  (setq *rmd* (micro::read-physical-memory location)))

(defun phys-mem-write (location)
  (micro::write-physical-memory location *wmd*))

;;;;;;;;;;;
;;; GC RAM
;;;;;;;;;;;

(defun address-gc-ram ()
  (ldb %%k-gc-ram-md-byte *gc-md*))

(defun ref-gc-ram ()
  (aref *gc-ram* (address-gc-ram)))

(defun k-read-gc-ram ()
  (make-datum $$unboxed (ref-gc-ram)))

(defun k-write-gc-ram (data)
  (setf (ref-gc-ram) (datum-word data)))

;;;;;;;;;;;;;;;;;;;;
;;; Transporter RAM
;;;;;;;;;;;;;;;;;;;;

(defun construct-transporter-ram-address ()
  (dpb (ldb %%k-memory-status-vma-not-boxed-bit *memory-status*) (byte 1. 11.)
       (dpb (ldb %%k-memory-status-md-not-boxed-bit *memory-status*) (byte 1. 10.)
            (dpb (ldb %%k-memory-status-transport-ram-bits *memory-status*) (byte 2. 8.)
                 (dpb (ldb %%k-memory-control-transporter-mode *memory-control*) (byte 2. 6.)
                      (ldb %%k-transporter-md-byte *GC-MD*))))))

(defun ref-transporter-ram ()
  (aref *transporter-ram* (construct-transporter-ram-address)))

;;; The MFO connection to the transporter RAM is shifted by 4 bits.

(defun k-read-transporter-ram ()
  (make-datum $$unboxed (ash (ref-transporter-ram) 4.)))

(defun k-write-transporter-ram (data)
  (setf (ref-transporter-ram) (ash (datum-word data) -4.)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GC write test, Transporter test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *gc-write-and-transporter-checks?* nil)

(defun select-trap-bit ()
  (if (= (ldb %%k-gc-ram-transporter-select (ref-gc-ram)) $$transporter-select-trap-on-bit-1)
      %%k-transporter-ram-bit-1
      %%k-transporter-ram-bit-2))

(defun do-gc-write-test ()
  (when *gc-write-and-transporter-checks?*
    (let ((transporter-bits (ref-transporter-ram)))
      (when (or (= (ldb %%k-transporter-ram-always-gc-trap transporter-bits) $$always-trap)
                (and (= (ldb %%k-transporter-ram-gc-trap-enable transporter-bits) $$gc-trap-enable)
                     (< (ldb %%k-map-volatility    *map-bits*)
                        (ldb %%k-gc-ram-volatility (ref-gc-ram)))))
        (turn-on-memory-trap-bit %%k-trap-memory-write-volatility)))))

(defun do-transporter-check ()
  (if (and *gc-write-and-transporter-checks?*
           (= (ldb (select-trap-bit) (ref-transporter-ram)) $$transporter-traps))
      (progn (setq *memory-status*
                   (32-dpb $$md-will-cause-trap %%k-memory-status-read-md-will-trans-trap
                          *memory-status*))
             (turn-on-memory-trap-bit %%k-trap-memory-read-transport))
      (setq *memory-status*
            (32-dpb $$md-will-not-cause-trap %%k-memory-status-read-md-will-trans-trap
                   *memory-status*))))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Virtual references
;;;;;;;;;;;;;;;;;;;;;;;

(defun memory-cycle ()
  (setq *memory-trap-bits* 0.)
  (setq *trap* (32-dpb 0. %%k-trap-memory-trap-bits *trap*)))

(defun memory-cycle-write (transporter-bits)
  (setq *memory-status*
        (32-dpb $$memory-cycle-type-write %%k-memory-status-cycle-type
               (32-dpb transporter-bits %%k-memory-status-transport-ram-bits
                      *memory-status*)))
  (memory-cycle))

(defun memory-cycle-read (transporter-type)
  (setq *memory-status*
        (32-dpb $$memory-cycle-type-read %%k-memory-status-cycle-type
               (32-dpb transporter-type %%k-memory-status-transport-ram-bits
                      *memory-status*)))
  (memory-cycle))

(defun address-physical-memory ()
  (when (= $$cluster-non-local-memory (ldb %%k-map-local-memory-bit *map-bits*))
    (ferror nil "Attempt to read NUBUS space!"))
  (+ (ash (ldb %%k-map-on-board-address *map-bits*)
          (byte-size %%k-unmapped-vma-byte))
     (ldb %%k-unmapped-vma-byte *vma*)))

(defun vmem-read (transporter-type)
  (memory-cycle-read transporter-type)
  (if (read-succeeds?)
      (progn (setq *gc-md*
                   (setq *rmd*
                         (phys-mem-read (address-physical-memory))))
             (setq *memory-status*
                   (32-dpb $$md-will-not-cause-trap %%k-memory-status-read-md-will-fault *memory-status*))
             (do-transporter-check))
      (progn (setq *memory-status*
                   (32-dpb $$md-will-cause-trap %%k-memory-status-read-md-will-fault *memory-status*))
             (turn-on-memory-trap-bit %%k-trap-memory-read-map-fault))))

(defun vmem-write (transporter-type)
  (trap-point
    #'(lambda (trap-return ignore)
        (memory-cycle-write transporter-type)
        (setq *rmd* *wmd*)
        (if (write-succeeds?)
            (phys-mem-write (address-physical-memory))
            (turn-on-memory-trap-bit %%k-trap-memory-write-map-fault))
        (do-gc-write-test)
        (if (zerop *memory-trap-bits*)
            (values)
            (progn (setq *trap* (logior *memory-trap-bits* *trap*))
                   (if (processor-traps?)
                       (trap #'trap-tag-error
                             #'(lambda () (funcall trap-return (values))))      ;CROCK!
                       (values)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; %VMA-START-READ %VMA-START-WRITE %MD-START-WRITE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro define-vma-start-read-instructions ()
  (let ((vsr-code '()))
    (dolist (vma-boxed *vma-boxed-options*)
      (dolist (md-boxed *md-boxed-options-for-vma*)
        (dolist (cdr-bit *cdr-options*)
          (dolist (trans-type *transporter-options*)
            (let ((name (intern (string-append "K-VMA-START-READ"
                                               (first vma-boxed)
                                               (first md-boxed)
                                               (first cdr-bit)
                                               (first trans-type)))))
              (push `(DEFUN ,name (LOCATION)
                       (,(second vma-boxed) (LOGIOR ,(second cdr-bit) LOCATION))
                       (SETQ *MEMORY-STATUS*
                             (32-DPB ,(lognot (second md-boxed)) %%K-MEMORY-STATUS-MD-NOT-BOXED-BIT
                                    *MEMORY-STATUS*))
                       (VMEM-READ ,(second trans-type)))
                    vsr-code))))))
    `(PROGN ,@(reverse vsr-code))))

(eval-when (load compile)
  (define-vma-start-read-instructions))

(defmacro define-vma-start-write-instructions ()
  (let ((vsw-code '()))
    (dolist (vma-boxed *vma-boxed-options*)
      (dolist (gc-trap *gc-options*)
        (let ((name (intern (string-append "K-VMA-START-WRITE"
                                           (first vma-boxed)
                                           (first gc-trap)))))
          (push `(DEFUN ,name (LOCATION)
                   (,(second vma-boxed) LOCATION)
                   (VMEM-WRITE ,(second gc-trap)))
                vsw-code))))
    `(PROGN ,@(reverse vsw-code))))

(defmacro define-md-start-write-instructions ()
  (let ((msw-code '()))
    (dolist (md-boxed *md-boxed-options*)
      (dolist (gc-trap *gc-options*)
        (let ((name (intern (string-append "K-MD-START-WRITE"
                                           (first md-boxed)
                                           (first gc-trap)))))
          (push `(DEFUN ,name (VALUE)
                   (,(second md-boxed) VALUE)
                   (VMEM-WRITE      ,(second gc-trap)))
                msw-code))))
    `(PROGN ,@(reverse msw-code))))

(eval-when (load compile)
  (define-vma-start-write-instructions)
  (define-md-start-write-instructions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Phony alu instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant alu-and-f-a   1)   ;These numbers are machine specific
(defconstant alu-not-f-a  14)   ;for the LAMBDA.
(defconstant alu-or-f-a    7)
(defconstant alu-pass-f-a  3)
(defconstant alu-xor-f-a   6)

(defmacro define-non-aligned-logical-operation (name alu-op)
  (let ((code '()))
    (do ((type "K-DPB" "K-LDB")
         (sign '+ (if (eq sign '+) '- 'done)))
        ((eq sign 'done) `(PROGN ,@(reverse code)))
      (dolist (boxed '("" "-BOXED" "-BOXED-LEFT" "-UNBOXED"))
        (push `(DEFUN ,(intern (string-append type name boxed)) (SOURCE BYTE DESTINATION)
                 (MICRO::NON-ALIGNED-LOGICAL-OP ,alu-op SOURCE DESTINATION
                                                (,sign 0 (BYTE-POSITION BYTE))
                                                (BYTE-SIZE BYTE)
                                                0))
              code)))))

(define-non-aligned-logical-operation "-AND" alu-and-f-a)
(define-non-aligned-logical-operation "-NOT" alu-not-f-a)
(define-non-aligned-logical-operation "-IOR" alu-or-f-a)
(define-non-aligned-logical-operation "-XOR" alu-xor-f-a)
(define-non-aligned-logical-operation ""     alu-pass-f-a)

(defun k-32= (a b)
  (= a b))

;;;;;;;;;;;;;;;;;;;
;;;; Data type ram
;;;;;;;;;;;;;;;;;;;

;(defun read-data-type-ram (left-boxed left-input right-boxed right-input data-type-check-code)
;  (aref *data-type-ram* left-boxed left-input right-boxed right-input data-type-check-code))

;(defun generate-box-bit (left-boxed right-boxed boxed-code)
;  (dispatch (byte 1. 0.) boxed-code
;    ($$boxed-code-pass-left     left-boxed)
;    ($$boxed-code-pass-right    right-boxed)
;    ($$boxed-code-force-unboxed $$unboxed)
;    ($$boxed-code-force-boxed   $$boxed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Primitive Byte specs
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Note, it is reasonable to load the status
;;;; register with a bytespec.  This is the reason
;;;; for the screwy notation.

;(defvar %%bytespec-width (byte 5. 8.))
;(defvar %%bytespec-lrot  (byte 6. 0.))

;(defun bytespec (width left-rotation)
;  ;; left Rotation is positive for ldb, negative for dpb
;  (dpb width %%bytespec-width
;       (dpb left-rotation %%bytespec-lrot
;           0.)))

;(defun bytespec-width (bytespec)
;  (ldb %%bytespec-width bytespec))

;(defun bytespec-left-rotation (bytespec)
;  (ldb %%bytespec-lrot bytespec))

;(defun negate-byte-position (bytespec)
;  (bytespec (bytespec-width bytespec) (- (bytespec-left-rotation bytespec))))

;;;;;;;;;;;;;;;
;;;; phony alu
;;;;;;;;;;;;;;;

;(defvar *alu-left-input*)
;(defvar *alu-right-input*)

;(defun alu-operation (number-manipulator
;                     extra-data
;                     bw-code
;                     *alu-left-input*
;                     *alu-right-input*
;                     box-code
;                     data-type-check-code)
;  (trap-point
;    #'(lambda (quit retry)
;       (let ((left-boxed  (datum-boxed-bit *alu-left-input*))
;             (left-datum  (datum-word      *alu-left-input*))
;             (right-boxed (datum-boxed-bit *alu-right-input*))
;             (right-datum (datum-word      *alu-right-input*)))
;         (setq *trap*
;               (32-dpb (read-data-type-ram
;                         left-boxed left-datum right-boxed right-datum data-type-check-code)
;                      %%k-trap-datatype
;                      *trap*))
;         (let* ((result (funcall number-manipulator extra-data bw-code left-datum right-datum))
;                (overflow (micro::adjust-status-register)))
;           (when overflow
;             (setq *trap*
;                   (32-dpb 1. %%k-trap-29332-overflow *trap*)))
;           (if (processor-traps?)
;               (trap quit retry)
;               (make-datum
;                 (generate-box-bit left-boxed right-boxed box-code)
;                 result)))))))

;;;;;;;;;;;;;;;;;;;;
;;;; Alu operations
;;;;;;;;;;;;;;;;;;;;

;(defun pass-stat (ignore bw-code &rest ignore)
;  (when (not (zerop bw-code))
;    (ferror nil "Don't call pass status with non-zero bw code."))
;  (micro::read-status-register))

;(defun ld-stat-left (ignore bw-code left-datum ignore)
;  (micro::write-status-register bw-code left-datum))

;(defun ld-stat-right (ignore bw-code ignore right-datum)
;  (micro::write-status-register bw-code right-datum))

;(defmacro define-decr (name bl use amount)
;  `(DEFUN ,name (IGNORE BW-CODE ,@bl)
;     ,@bl
;     (MICRO::SUB BW-CODE ,amount ,use)))

;(define-decr decr1-left  (r l) l 1.)
;(define-decr decr1-right (r l) r 1.)
;(define-decr decr2-left  (r l) l 2.)
;(define-decr decr2-right (r l) r 2.)
;(define-decr decr4-left  (r l) l 4.)
;(define-decr decr4-right (r l) r 4.)

;(defmacro define-incr (name bl use amount)
;  `(DEFUN ,name (IGNORE BW-CODE ,@bl)
;     ,@bl
;     (MICRO::ADD BW-CODE ,amount ,use)))

;(define-incr incr1-left  (r l) l 1.)
;(define-incr incr1-right (r l) r 1.)
;(define-incr incr2-left  (r l) l 2.)
;(define-incr incr2-right (r l) r 2.)
;(define-incr incr4-left  (r l) l 4.)
;(define-incr incr4-right (r l) r 4.)

;(defun add (ignore bw-code left right)
;  (micro::add bw-code left right))

;(defun sub (ignore bw-code left right)
;  (micro::sub bw-code left right))

;(defmacro define-field-logical-operation (name alu-operation)
;  `(DEFUN ,name (BYTE-SPEC INTERNAL-BITS LEFT-INPUT RIGHT-INPUT)
;     (MICRO::NON-ALIGNED-LOGICAL-OP
;       ,alu-operation LEFT-INPUT RIGHT-INPUT
;       (BYTESPEC-LEFT-ROTATION BYTE-SPEC) (BYTESPEC-WIDTH BYTE-SPEC)
;       INTERNAL-BITS)))

;(define-field-logical-operation and-f-a   1)   ;These numbers are machine specific
;(define-field-logical-operation not-f-a  14)   ;for the LAMBDA.
;(define-field-logical-operation or-f-a    7)
;(define-field-logical-operation pass-f-a  3)
;(define-field-logical-operation xor-f-a   6)

;(defun neg-a (ignore bw-code left ignore)
;  (micro::neg bw-code left 1.))

;(defun neg-b (ignore bw-code ignore left)
;  (micro::neg bw-code right 1.))

(defun reset-simulation ()
  (reset-memory-control)
  (initialize-maps))
