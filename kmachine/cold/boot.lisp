;;; -*- Mode:LISP; Package:BOOT; Readtable:CL; Base:10. -*-

;;; Temporary
(defmacro check-nil (message)
  `(when (no-optimize-cdr nil)
     (trap:illop ,(global:format nil "NIL bashed: ~A" message))))

(defun no-optimize-cdr (frob)
  (cons:cdr frob))


;;;;;;;;;;;;;;;;;;;;;;;
;;;; Simulated booting
;;;;;;;;;;;;;;;;;;;;;;;

;;; Defined for all time.
;;; These are intended to be blown into all boot proms.

(defconstant **boot-vector-origin** 42.)
(defconstant **bootprom-version**                         0.)
(defconstant **initial-code-physical-location-bv-offset** 1.)
(defconstant **initial-code-size-in-clusters-bv-offset**  2.)
(defconstant **initial-code-entry-point-bv-offset**       3.)
(defconstant **physical-memory-block-map**                4.)

(defconstant **boot-vector-first-soft-defined-location**  5.)

;;; End of defined for all time.


(eval-when (compile load eval)

;(defvar *boot-vector-slots*)

user::(prims::defmacro boot::define-boot-vector (&rest element-list)
;  (setq boot::*boot-vector-slots* element-list)
  (do ((tail element-list (rest tail))
       (count boot::**boot-vector-first-soft-defined-location** (1+ count))
       (code lisp:nil (lisp:cons `(boot::DEFCONSTANT ,(lisp:first tail) ,count) code)))
      ((null tail) `(boot::PROGN ,@code))))
)

(define-boot-vector
  *initial-gc-ram-data-physical-location*               ;this has been flushed, but leave as place holder.
  *initial-transporter-ram-data-physical-location*
  *cold-load-flag*
  *bv-lowlevel-root-region*         ;this written to preserve info.  see MAP-IN-DEBUG-AND-COMMUNICATION-ROOTS.
  *bv-debug-root-cluster*
  *bv-communication-root-cluster*
  *bv-all-packages*
  )

(defun read-boot-vector (element)
  (hw:vma-start-read-no-transport (+ **boot-vector-origin** element) :unboxed :unboxed)
  (hw:read-md))

(defun write-boot-vector (element data)
  (hw::write-md-unboxed data)
  (hw::vma-start-write-no-gc-trap-unboxed (+ **boot-vector-origin** element))
  (hw:nop)
  )

;;; The physical cluster table will go in quantum 1.
;;; It takes up half of a quantum.
;;; In the second half of the quantum, we put
;;; the quantum map and the region bits.  Each of
;;; these takes up a quater of the quantum.

;;; Cluster addresses
;(defconstant *initial-map-data-physical-location*              (cluster->address 2.))  ;64 clusters

(defconstant *quantum-map-physical-location*                    4.)
(defconstant *quantum-map-clusters*                             4.)

(defconstant *region-bits-physical-location*                    8.)
(defconstant *region-bits-clusters*                             4.)

(defconstant *initial-physical-cluster-data-physical-location*
             (lisp:ash 12. (byte-position vinc:%%cluster-number))) ;1 cluster

;(defconstant *initial-gc-ram-data-physical-location*           (cluster->address 13.)) ;1 cluster

;(defconstant *initial-transporter-ram-data-physical-location*   14.)

;;; Virtual addresses of nifty things.

;;; Quantum 0.
;;; Cluster 1.
(defconstant *temporary-map-entry-location*            (lisp::expt 2 (byte-position vinc::%%cluster-number)))


;;; Quantum 1.
;;; Clusters 0. 7.
(defconstant *physical-cluster-table-location*        (lisp::*
                                                        1
                                                        (lisp::expt 2.
                                                                    (byte-position vinc::%%quantum-number))))

;;; Clusters 8. 11.
(defconstant *quantum-map-virtual-location*            (+ *physical-cluster-table-location*
                                                          (lisp::expt
                                                            2.
                                                            (1- (byte-position vinc::%%quantum-number)))))
;;; Clusters 12. 15.
(defconstant *region-bits-virtual-location*            (+ *quantum-map-virtual-location*
                                                          (lisp::expt
                                                            2.
                                                            (- (byte-position vinc::%%quantum-number) 2))))

;;; This needs to be done better, but let's see how it runs.

(defun cold-boot-function ()
  ;; I will work this backward 'till boot time.
  ;; For now, I will just pile stuff into this function
  ;; as I get the hardware running better and better.

  ;; This is a macro that expands into a bunch of
  ;; setqs.  This doesn't need to make any function
  ;; calls.

 ;  (check-nil "Start of COLD-BOOT-FUNCTION")   ;** this cant win until global constants are loaded!

  (hw:write-open-active-return #x101112) ;temp O=10, A=11, R=12
  (hw:nop)
  (prims::setup-initial-values-of-global-registers)

  ;; Before we can do any function calls, we must initialize the
  ;; call hardware.  (Actually, we can get away with calling
  ;; the initializer, but there is some trickiness going on here.)
  ;; Must be called with traps off.
  (cold-initialize-call-hardware)

;  (trap::illop "Call hardware initialized")

 ;  (check-nil "About to do event-horizon")

  ;; Establish a bottom of the stack trap for debugging porpoises
  ;; We should never return from this call, but if we do, we
  ;; halt the machine.
  (event-horizon)

 ;  (check-nil "Event horizon lost")

  (labels ((loop-forever ()
             (trap::illop "Unexpected return from the event horizon.")
             (loop-forever)))
    (loop-forever)))

(defun event-horizon ()
;  (trap::illop "Reached event-horizon.")

  (load-up-runtime-global-constants)

  (check-nil "Runtime global constants")

;  (modify-lowcore-cache-enable hw:$$icache-set-enable)
   (modify-icache-enables       hw:$$icache-enable-all-sets)    ;A is selected if ICACHE-OSEL is low.
;  (restore-icache-traps        hw:$$icache-trap-enable)
;  (restore-synchronous-traps   hw:$$trap-enable)
;  (trap::trap-on)
                        ;up to here, map is random except for page 0 and code pages mapped in by PSEUDO-BOOT.
        ;foo, zero the entire thing except page 0 and n code clusters!
        ;other random note:  read-boot-vector returns an unboxed frob, and it probably doesnt have a datatype
        ;       either.  However, traps are not on so it presumably doesnt make any difference.
  (map::zero-map-excepting-n-code-clusters (read-boot-vector **initial-code-size-in-clusters-bv-offset**))
  (map::direct-map (read-boot-vector **physical-memory-block-map**))
                        ;this first wipes the whole map to $$map-status-direct-mapped
                        ;       which, in turn, is 3 in the s-bits and 1 (read-only) in the v-we bits
                        ;then, physical memory which exists is associated with a consecutive low virtual pages,
                        ;       also with $$map-status-direct-mapped.  Note this includes the physical
                        ;memory where the code resides which has been mapped by PSEUDO-BOOT.
  (check-nil "Direct Map")

;  (trap::illop "Direct mapped.")

;;; This isn't necessary.
;  (map::unmap-wired-code (cluster-number (read-boot-vector **initial-code-physical-location-bv-offset**))
;                        (read-boot-vector **initial-code-size-in-clusters-bv-offset**))

;  (trap::illop "Unmapped broken region.")

   (gc-ram::zero-ram)   ;this is more the right thing.  Loading it doesnt really make sense.

;  (gc-ram::load-ram (read-boot-vector *initial-gc-ram-data-physical-location*))

  (check-nil "GC RAM")

; (trap::illop "GC ram loaded.")

  (transporter-ram:load-transporter-ram (read-boot-vector *initial-transporter-ram-data-physical-location*))

  (check-nil "Transporter RAM")

;  (trap::illop "Transporter ram loaded.")

  (datatype-ram:load-initial-datatype-ram)

  (check-nil "Datatype RAM")

;  (trap::illop "Datatype ram loaded.")

;  (test-with-16384-interrupt)
;  (test-with-1024-interrupt)
;  (test-with-single-step)

  (pcd:create-physical-cluster-data-table)      ;allocate memory for PCD, map it, and set all PCD entries to $$status-invalid.
                                                ; mark clusters used by PCD itself as wired and dirty.
  (check-nil "Physical Cluster Data Table")

;  (trap::illop "Physical cluster data table allocated, not initialized.")
  (pcd:initialize-physical-cluster-data *initial-physical-cluster-data-physical-location*)
                ;update PCD as per initialization list, which originally came from K-COLD:GET-INITIAL-PHYSICAL-CLUSTER-DATA.
                ;  Associate in clusters in map as directed.  This changes code pages to pcd::$$init-map-wired-read-only,
                ;quantum-map pages to pcd::$init-map-wired,  region-bits to pcd::$$init-map-wired,
                ;shared-memory-cluster (i.e. 2) to pcd:$$init-map-wired, and 0 to $$init-map-wired.

  (check-nil "Physical Cluster Data Init")

;  (trap::illop "Physical cluster data table filled in.")
  (pcd:free-unused-physical-clusters (read-boot-vector **physical-memory-block-map**))
        ;change PCD entry $$status-invalid to free for existing memory, make sure $$status-invalid in non-existant memory.
        ;does not hack map at all.
  (check-nil "Freed physical clusters")

;  (trap::illop "Physical cluster data table initialized.")
  (map:flush-direct-map)        ;anything with $$map-status-direct-mapped gets freed! (as far as map is concerned)
                                ;does not hack PCD.
  (check-nil "Flush direct Map")

;  (trap::illop "Direct map flushed.")

  (nubus-stuff::map-in-k-io-cluster)    ;used for nubus-interrupts, which are in turn used for DEBUGGER-TRAP.

  (check-nil "MAP-IN-K-IO-CLUSTER")

  (map-in-debug-and-communication-roots)        ;allocate virtual space for *falcon-debug-root-cluster* and
                                                ; *falcon-communication-root-cluster*.

;  (trap::illop "Mapped in IO cluster.")
;  (restore-16384-interrupt hw:$$timer-interrupt-enable)
  (restore-asynchronous-traps hw:$$trap-enable) ;enables async traps and updates *trap-mask*.
  (restore-synchronous-traps  hw:$$trap-enable) ;likewise synch
  (restore-icache-traps       hw:$$trap-enable) ;likewise icache.

  (check-nil "Ready to turn on traps")

;  (trap::illop "Ready to turn on traps.")
  (trap::trap-on)

  (check-nil "Traps on")

  (k2::init-kbug)

  (check-nil "KBUG inited")

  (restore-datatype-traps hw:$$trap-enable)
  (restore-overflow-traps hw:$$trap-enable)

  (check-nil "Traps enabled")

  (if (eq (read-boot-vector *cold-load-flag*) 1)
      (synthesize-cold-load)
      (trap::illop "I want to call LISP-REINITIALIZE."))

  (check-nil "Synthesize-cold-load")

  )



(defun synthesize-cold-load ()
  ;;starting here, relavent sections of map are all map-invalid, except for REGION-BITS which was in table which
        ;came from K-COLD:GET-INITIALIZE-PHYSICAL-CLUSTER-DATA.  Traps are off, so we had better
        ; not take any write-faults (if we try, cycle will just be ignored).
  ;; Make region to hold region-data
  (synthesize-region-data)
;  (trap::illop "Made region data.")
  (check-nil "Made region data.")
  (synthesize-area-data)
;  (test-tak-with-lights)
;  (trap::illop "made area data.")
  (check-nil "Made area data.")
 (setq gr::*desperate-consing-area*
        (area-data:make-area 1.         ;volatility
                             (region-bits:encode-region-bits
                               region-bits:$$region-fixed
                               region-bits:$$region-new-space
                               region-bits:$$region-space-unboxed
                               region-bits:$$region-read-write
                               region-bits:$$scavenge-enabled
                               region-bits:$$region-internal-memory
                               0.)
                             1.))       ;recommended region-size
; (trap::illop "Made desparate-consing-area")
 (check-nil "Made desparate consing area")
  ;; Make the default consing area and load up the cons cache.
  (let ((default-consing-area
          (area-data:make-area 7.
                             (region-bits:encode-region-bits
                               region-bits:$$region-fixed
                               region-bits:$$region-new-space
                               region-bits:$$region-space-unboxed
                               region-bits:$$region-read-write
                               region-bits:$$scavenge-enabled
                               region-bits:$$region-internal-memory
                               5.)
                             5.)))
    (check-nil "Made default consing area")
    (setq gr:*cons-cache-area*           default-consing-area)
    (setq gr:*structure-cons-cache-area* default-consing-area)
    (region-data::invalidate-cons-cache)
    (check-nil "consing cache")
    (setq gr:*default-code-area*
          (area-data:make-area 3
                               (region-bits:encode-region-bits
                                 region-bits:$$region-fixed
                                 region-bits:$$region-new-space
                                 region-bits:$$region-space-code
                                 region-bits:$$region-read-write
                                 region-bits:$$scavenge-enabled
                                 region-bits:$$region-internal-memory
                                 5.)
                               5.))
;    (setq gr::*cons-cache-region* -1)
;    (setq gr::*structure-cons-cache-region* -1)
;    (area-data::get-active-region
;      default-consing-area
;      region-bits::$$region-space-cons
;      region-bits::$$region-new-space
;      nil
;      0)
;;    (trap::illop "loaded cons cache.")
;    (area-data::get-active-region
;      default-consing-area
;      region-bits::$$region-space-structure
;      region-bits::$$region-new-space
;      nil
;      0)
    (setq gr::*default-consing-area* default-consing-area))
  (check-nil "Code area")
  (cons::initialize-structure-handles)
  (check-nil "Structure handles")
  ;;; The are for the "other side" of dt-right-array-and-left-structure
  (setq gr:*random-structure* (li:make-structure 1))
  (setq gr:*random-array* (array:make-vector 0))
  (check-nil "Randomness")


  ;; because T and NIL are fasdumped with "PRIMITIVES" as their package name ...

  (let ((primitives-name (array::make-string 10)))
    (array::aset-1 #\P primitives-name 0)
    (array::aset-1 #\R primitives-name 1)
    (array::aset-1 #\I primitives-name 2)
    (array::aset-1 #\M primitives-name 3)
    (array::aset-1 #\I primitives-name 4)
    (array::aset-1 #\T primitives-name 5)
    (array::aset-1 #\I primitives-name 6)
    (array::aset-1 #\V primitives-name 7)
    (array::aset-1 #\E primitives-name 8)
    (array::aset-1 #\S primitives-name 9)

    ;; Fixup NIL
    (symbol::%fmakunbound nil)
    (setf (symbol::symbol-plist 'nil) nil)
    (setf (symbol::symbol-package nil) primitives-name)

    (check-nil "Fixed up NIL, but BLEW IT!")

    ;; Make T
    (let ((t-print-name (array::make-string 1)))
      (array::aset-1 #\T t-print-name 0)
      ;; Put the print name in.
      (hw::write-md-boxed (cons:make-header vinc:$$dtp-symbol-header
                                            t-print-name))
      (hw::vma-start-write-boxed gr:*t*)

      ;; directly set the value cell
      ;; because %set now does type checking to make sure T and NIL aren't bashed
      (cons::store-contents-offset gr:*t* symbol::*symbol-value* gr:*t*)
;      (symbol::%set gr:*t* gr:*t*)

      (symbol::%fmakunbound gr:*t*)
      (setf (symbol::symbol-plist gr:*t*) nil)
      (setf (symbol::symbol-package gr:*t*) primitives-name)
      ;; $$$ Put back old method of *warm-symbols* <22-Nov-88 wkf>
      (setq gr:*warm-symbols* (cons:cons gr:*t* nil))))

  (check-nil "Fixed up T, and broke NIL!")
  (write-boot-vector *cold-load-flag* 2)        ;have now advanced to warm stage.
  (check-nil "Cold load broken!")
  (trap::illop "Cold load finished!")   ;the text of this message is compared against in k-boot!
  (warm-start)
  )

(defun warm-start ()
   ;; un-Halt the machine.
  (hw:write-processor-control
    (hw:dpb-unboxed 0 hw:%%processor-control-halt-processor (hw:read-processor-control)))
  (hw:nop) (hw:nop) ;allow relinking
  (trap:without-traps
    #'(lambda ()
        (vinc:flush-icache)
        (restore-asynchronous-traps hw:$$trap-enable)
        (restore-synchronous-traps  hw:$$trap-enable)
        ;; move down below to FLUSH-CALL-STACK
;       (k2::init-kbug)
        (k2:kbug-vm-write k2:kbug-flag-addr (hw:unboxed-constant 0))
        (restore-icache-traps       hw:$$trap-enable)
        (restore-datatype-traps hw:$$trap-enable)
        (restore-overflow-traps hw:$$trap-enable)
        ;; flush out memory traps
        (hw:write-md-unboxed 0)
        (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed 0)
        (hw:read-md)
        ))
  (check-nil "Hacked Traps")
  (trap::trap-on)
  (check-nil "Traps on again")
  (li:flush-call-stack)
  (check-nil "Flushed call stack"))

(defvar li::*warm-boot-complete* nil "T when the K is warm booted.")

(defun li:flush-call-stack ()
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:trap-off)
  (boot:cold-initialize-call-hardware)
  ;; reset the state of KBUG flags and streams
  (k2::init-kbug)
  (trap:trap-on)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (nubus-stuff:cause-debugger-trap)
  (boot:wait-for-debugger 0)
  (li:setf (li:symbol-function 'li:error) (li:symbol-function 'li:regular-error))
  ;; ||| Put back to normal error mode in case listner was runing. 10/20/88 --wkf
  ;; $$$ added call to reset. <04-Nov-88 JIM>
  (when (and (li:boundp 'li::*warm-boot-complete*)      ; $$$ Cold defvars don't get bound. <18-Nov-88 JIM>
             li::*warm-boot-complete*)
    (li::reset))
  (loop))

(defun wait-for-debugger (n)
  (wait-for-debugger (hw:32-1+ n)))


;;;****************************************************************
;;;
;;;  Support Functions
;;;
;;;****************************************************************

(defun map-in-debug-and-communication-roots ()
  ;unfortunately, we cant write in any special variables at this point.
  ;to at least preserve the info, lets write it in the boot vector!!
  ;the special variables get set later by K2:INITIALIZE-DEBUG-ROOT.
  (let  ((reg (region-data:make-region
                1               ;one quanta in size.
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-new-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  0                             ;region-bits:$$region-external-bus
                  0.)
                0.))
         (debug-root-cluster nil)
         (communication-root-cluster nil))
    (setq debug-root-cluster (vinc:quantum->cluster reg)
          communication-root-cluster (1+ debug-root-cluster))
  (map:associate-local-memory 1 debug-root-cluster map:$$map-status-normal)
  (map:associate-local-memory 3 communication-root-cluster map:$$map-status-normal)
  (write-boot-vector *bv-lowlevel-root-region* reg)
  (write-boot-vector *bv-debug-root-cluster* debug-root-cluster)
  (write-boot-vector *bv-communication-root-cluster* communication-root-cluster)
  ))



(defun synthesize-region-data ()
  ;; 4096 regions, 4 tables = 16 clusters = 1 quantum
  (let* ((region-data (region-bits:make-region 1.
                       (region-bits:encode-region-bits
                         region-bits:$$region-fixed
                         region-bits:$$region-new-space
                         region-bits:$$region-space-unboxed
                         region-bits:$$region-read-write
                         region-bits:$$scavenge-disabled
                         region-bits:$$region-internal-memory
                         0.)
                       0.))
         (origin (memory-management::region-origin region-data)))
;    (trap::illop "Made region data region.")
    (setq gr::*region-free-pointer* origin)
    (setq gr::*region-end*          (hw:32+ gr::*region-free-pointer* memory-management:*number-of-regions*))
    (setq gr::*region-gc-pointer*   (hw:32+ gr::*region-end*          memory-management:*number-of-regions*))
    (region-data:initialize-region-data)))

(defun synthesize-area-data ()
  (let* ((area-data-region (region-data:make-region 1.
                            (region-bits:encode-region-bits
                              region-bits:$$region-fixed
                              region-bits:$$region-new-space
                              region-bits:$$region-space-cons
                              region-bits:$$region-read-write
                              region-bits:$$scavenge-disabled
                              region-bits:$$region-internal-memory
                              0.)
                            0.))
         (origin (memory-management::region-origin area-data-region)))
    ;; area-data-region is full
    (setf (region-data:region-free-pointer area-data-region)
          (region-data:region-end area-data-region))
    (setq gr::*region-list-thread* origin)
    (setq gr::*area-region-data* (hw:24+ gr::*region-list-thread* memory-management:*number-of-regions*))
    (setq gr::*area-region-bits* (hw:24+ gr::*area-region-data* memory-management:*number-of-areas*))
    (setq gr::*area-region-size* (hw:24+ gr::*area-region-bits* memory-management:*number-of-areas*))
    (area-data:initialize-area-data)))

;;&&& ||| Removed commented out code 10/20/88 --wkf

(defun ensure-interrupts-are-on ()
  (when (= hw:$$trap-disable (hw:ldb (hw:read-memory-control) hw:%%memory-control-master-trap-enable 0))
    (trap::illop "Traps are off.")))

;;&&& ||| Removed commented out code 10/20/88 --wkf

;;; Calling this function will of course trash your frame.
;;; The cold boot function doesn't use its frame until it calls this,
;;; so it is safe.  Actually, we are putting our temps in the return
;;; frame, so we cannot use any undeclared locals here..

(defun cold-initialize-call-hardware ()
  ;; I hope this is called with traps off.
  "Initialize call hardware and build a heap."

  (hw:write-processor-control (hw:dpb hw:$$call-heap-underflow-trap-disable
                                      hw:%%processor-control-heap-underflow-trap-enable
                                      (hw:read-processor-control)))

  ;; First, we snarf down a valid open and active frame.
  ;; Note that the return frame is different.
  (hw:write-open-active-return (hw:unboxed-constant #xFFFFFE))  ;Get a frame
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)

  (let ((free           #xfe)

        ;; We don't turn traps back on below, because we
        ;; want to be called with traps off.  If it turns
        ;; out that we are called with traps on, we simply
        ;; illop.
        (trap-state (hw:ldb (hw:trap-off) (byte 1. 0.) 0))

        ;; Must save return pc, this should be dtp code here.
        (our-return-pc (hw:ldb (hw:read-return-pc-return-dest) hw:%%ch-rpcd-return-pc 0))
        ;; Cannot have temporaries, so we declare every local here.

        ;; Put 238 (256 total frames - 16 globals - 2 (open and active) and return)
        ;; Do not remove this local, execution of code depends on it!
        (number-of-frames   238.)
        (r-frame            #xfe)
        (zero               0.)
        (oar-temp           0.))

    (when (not (= hw:$$trap-disable trap-state))
      (trap::illop "Attempt to COLD-INITIALIZE-CALL-HARDWARE with traps on."))

    ;; Empty heap and call stack.  Must do after saving return pc locally
    ;; because writing call-stack-pointer will clobber it.
    ;; #xF0 gives us 16 yellow alert frames
    (hw:write-call-hp-sp #xEF00)                ;Empty the heap and stack ;; This is errics fault......
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
        ;; have call hardware do a tail call but have the pc increment normally (no jump)
        (hw:ch-tcall)
        (setq free (1- free))
        (setq number-of-frames (1- number-of-frames))
        (go loop)

     end)

    (hw:write-open-active-return (hw:unboxed-constant #xFFFF10))
    (hw:nop)
    (hw:nop)
    (hw:nop)
    (setq gr::*ch-base-csp* 1)
    (hw:write-processor-control (hw:dpb hw:$$call-heap-underflow-trap-enable
                                        hw:%%processor-control-heap-underflow-trap-enable
                                        (hw:read-processor-control)))
    ;; Do a "return"  A real one won't work because the
    ;; stack is trashed.
    (hw:dispatch our-return-pc)))


(defun load-up-runtime-global-constants ()
  ;; Free pointer points just beyond the cold boot code segment.
  (setq gr::*physical-cluster-free-pointer*
        (+ (cluster-number (read-boot-vector **initial-code-physical-location-bv-offset**))
           (read-boot-vector **initial-code-size-in-clusters-bv-offset**)))
  (setq gr::*physical-cluster-data-table* *physical-cluster-table-location*)
  (setq gr::*quantum-map*                 *quantum-map-virtual-location*)
  (setq gr::*region-bits*                 *region-bits-virtual-location*)
;  (setq gr::*temporary-map-entry*         1.)
  )

;available to call as a debugging aid to halt machine and preserve history ram.
;args are just available to preserve info.
(defun error-halt (&optional a b c d)
  (trap:illop "error-halt"))
