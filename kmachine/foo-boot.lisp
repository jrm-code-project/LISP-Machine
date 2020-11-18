;;; -*- Mode:LISP; Package:BOOT; Readtable:CL; Base:10 -*-

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
  *initial-gc-ram-data-physical-location*
  *initial-transporter-ram-data-physical-location*
  *cold-load-flag*
  *bv-lowlevel-root-region*         ;this written to preserve info.  see MAP-IN-DEBUG-AND-COMMUNICATION-ROOTS.
  *bv-debug-root-cluster*
  *bv-communication-root-cluster*
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
;   (modify-icache-enables       hw:$$icache-enable-all-sets)
;  (restore-icache-traps         hw:$$icache-trap-enable)
;  (restore-synchronous-traps    hw:$$trap-enable)
;  (trap::trap-on)

  (map::direct-map (read-boot-vector **physical-memory-block-map**))

  (check-nil "Direct Map")

;  (trap::illop "Direct mapped.")

;;; This isn't necessary.
;  (map::unmap-wired-code (cluster-number (read-boot-vector **initial-code-physical-location-bv-offset**))
;                        (read-boot-vector **initial-code-size-in-clusters-bv-offset**))

;  (trap::illop "Unmapped broken region.")

  (gc-ram::load-ram (read-boot-vector *initial-gc-ram-data-physical-location*))

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

  (pcd:create-physical-cluster-data-table)      ;allocate memory for PCD, map, and initialize to $$status-invalid.
                                                ; mark clusters used by PCD itself as wired and dirty.
  (check-nil "Physical Cluster Data Table")

;  (trap::illop "Physical cluster data table allocated, not initialized.")
  (pcd:initialize-physical-cluster-data *initial-physical-cluster-data-physical-location*)
                ;update PCD as per initialization list.  Also associate in clusters in map.
  (check-nil "Physical Cluster Data Init")

;  (trap::illop "Physical cluster data table filled in.")
  (pcd:free-unused-physical-clusters (read-boot-vector **physical-memory-block-map**))
                ;change $$status-invalid to free for existing memory, make sure $$status-invalid in non-exitant memory.
  (check-nil "Freed physical clusters")

;  (trap::illop "Physical cluster data table initialized.")
  (map:flush-direct-map)                ;anything with $$map-status-direct-mapped gets freed!
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

(defun wait-for-debugger (n)
  (wait-for-debugger (hw:32-1+ n)))

(defun synthesize-cold-load ()
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
  (let ((lisp-name (array::make-string 4)))
    (array::aset-1 #\L lisp-name 0)
    (array::aset-1 #\I lisp-name 1)
    (array::aset-1 #\S lisp-name 2)
    (array::aset-1 #\P lisp-name 3)

    ;; Fixup NIL
    (symbol::%fmakunbound nil)
    (setf (symbol::symbol-plist 'nil) nil)
    (setf (symbol::symbol-package nil) lisp-name)

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
      (setf (symbol::symbol-package gr:*t*) lisp-name)
      (setq gr:*warm-symbols* (cons:cons gr:*t* nil))))
  (check-nil "Fixed up T, and broke NIL!")
  (write-boot-vector *cold-load-flag* 2)        ;have now advanced to warm stage.
  (check-nil "Cold load broken!")
  (trap::illop "Cold load finished!")
  (warm-start)
  )

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
        (k2::init-kbug)
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

;(defun test-with-16384-interrupt ()
;  ;;; Experiment, turn on the 16384 interrupt and see what happens.
;  (restore-16384-interrupt hw:$$timer-interrupt-enable)
;  (restore-asynchronous-traps hw:$$trap-enable)
;  (trap::illop "16384 interrupt set.")
;  (trap::trap-on)
;  (do-tak))

;(defun test-with-1024-interrupt ()
;  (restore-1024-interrupt hw:$$timer-interrupt-enable)
;  (restore-asynchronous-traps hw:$$trap-enable)
;  (trap::illop "1024 interrupt set.")
;  (trap::trap-on)
;  (modify-lowcore-cache-enable hw:$$icache-set-enable)
;  (do-tak))

;(defun test-with-single-step ()
;  (vinc:restore-single-step-trap hw:$$trap-enable)
;  (trap::illop "Step trap set.")
;  (trap::trap-on)
;  (step-tak))

;(defafun step-tak ()
;  (movea gr::*save-trap-pc* do-tak)
;  (nop)
;  (nop)
;  (nop)
;  (jump trap::non-modifying-exit nil))

;(defun do-tak ()
;  (hw:nop)
;  (hw:nop)
;  (hw:nop)
;;  (test-fib nil nil)
;;  (test-branch)
;  (test-tak)
;  (modify-memory-control
;    #'(lambda (memory-control)
;       (hw:dpb-xor 1. hw:%%memory-control-led-1 memory-control)))
;  (do-tak))

;(defun test-tak ()
;  (when (not (= (tak 18. 12. 6.) 7.))
;    (trap::illop "Tak returned the wrong result.")))

;(defun tak (x y z)
; ; (ensure-interrupts-are-on)
;  (if (not (< y x))
;      z
;      (tak (tak (1- x) y z)
;          (tak (1- y) z x)
;          (tak (1- z) x y))))

;(defun test-tak-with-lights ()
;    (labels ((loop (n)
;              (modify-leds (hw:ldb-not n (byte 3. 0.) 0))
;              (let ((region (make-test-consing-region)))
;                (tak 18. 12. 6.)
;                ;; Primitive Garbage Collection
;                ;; Reset the free pointer.
;                (region-data:free-region region))
;       ;       (trap::illop "Region freed.")
;              (loop (1+ n))))
;      (loop 0)))

(defun ensure-interrupts-are-on ()
  (when (= hw:$$trap-disable (hw:ldb (hw:read-memory-control) hw:%%memory-control-master-trap-enable 0))
    (trap::illop "Traps are off.")))

;(defun test-fib ()
;  (when (not (= (fib 4.) 3))
;    (trap::illop "Fib returned the wrong result.")))

;(defun fib (n)
;  (if (< n 2)
;      n
;      (+ (fib (- n 1)) (fib (- n 2)))))

;(defun fib (x)
;  (fib-iter x 1 0))

;(defun fib-iter (x ans prev)
;  (if (= x 0)
;      ans
;      (fib-iter (1- x) (+ ans prev) ans)))

;(defun hanoi (from to other n)
;  (prims:when (= n 0)
;      '()
;      (progn
;       (hanoi from other to (1- n))
;       (hanoi other to from (1- n)))))

;(defafun test-branch ()
;  (movei a0 0)

; test-branch-foo
;  (move nop a0)
;  (test br-zero)
;  (branch test-branch-was-not-zero ())

;  (movei a0 0)
;  (unconditional-branch test-branch-foo ())

; test-branch-was-not-zero
;  (movei a0 1)
;  (unconditional-branch test-branch-foo ()))

;(defun make-consing-region-for-initial-list ()
;  (let ((region
;         (region-data:make-region 1.
;                      (region-bits:encode-region-bits
;                        region-bits:$$region-fixed
;                        region-bits:$$region-new-space
;                        region-bits:$$region-space-cons
;                        region-bits:$$region-read-write
;                        region-bits:$$scavenge-enabled
;                              region-bits:$$region-internal-memory
;                        0.)
;                      7.)))
;    (setq gr::*cons-cache-area* 5.)
;    (setq gr::*cons-cache-region* region)
;    (setq gr::*cons-cache-free* (region-data:unsafe-region-free-pointer region))
;    (setq gr::*cons-cache-limit* (region-data:region-end region)))
;  (trap::illop "Made consing region for first list."))

;(defun make-test-consing-region ()
;  (let ((fake-consing-region
;         (region-data:make-region 256.                 ;1 megaQ
;                      (region-bits::encode-region-bits
;                        region-bits:$$region-fixed
;                        region-bits:$$region-new-space
;                        region-bits:$$region-space-cons
;                        region-bits:$$region-read-write
;                        region-bits:$$scavenge-enabled
;                              region-bits:$$region-internal-memory
;                        0.)
;                      7.)))
;    (setq gr::*cons-cache-area*   5.)          ;random number
;    (setq gr::*cons-cache-region* fake-consing-region)
;    (setq gr::*cons-cache-free* (region-data:unsafe-region-free-pointer fake-consing-region))
;    (setq gr::*cons-cache-limit* (region-data:region-end         fake-consing-region))
;    fake-consing-region))


;(defun create-n (n)
;  (do ((n n (1- n))
;       (a () (li:push () a)))
;      ((= n 0) a)
;  ;  (trap::illop "Create N loop.")
;    )
;  )


;(defun iterative-div2 (l)
;  (do ((l l (cons::cddr l))
;       (a () (li:push (cons::car l) a)))
;      ((null l) a)))

;(defun recursive-div2 (l)
;  (cond ((null l) ())
;       (t (cons::cons (cons::car l) (recursive-div2 (cons::cddr l))))))

;(defun test-1 (l)
;  (do ((i 300. (1- i)))
;      ((= i 0))
;    (iterative-div2 l)
;    (iterative-div2 l)
;    (iterative-div2 l)
;    (iterative-div2 l)
;    ))

;(defun test-2 (l)
;  (do ((i 300. (1- i)))
;      ((= i 0))
;    (recursive-div2 l)
;    (recursive-div2 l)
;    (recursive-div2 l)
;    (recursive-div2 l)))

;(defun test-div2-iterative ()
;  (make-consing-region-for-initial-list)
;  (let ((l (create-n 200.)))
;    (trap::illop "I think I made a 200 element list.")
;    (labels ((loop (n)
;              (modify-leds (hw:ldb-not n (byte 3. 0.) 0))
;              (let ((region (make-test-consing-region)))
;                (test-1 l)
;                ;; Primitive Garbage Collection
;                ;; Reset the free pointer.
;                (region-data:free-region region))
;       ;       (trap::illop "Region freed.")
;              (loop (1+ n))))
;      (loop 0))))

;(defun test-div2-recursive ()
;  (labels ((loop (n l)
;            (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
;            (test-2 l)
;            (maybe-reset-area n l))

;          (maybe-reset-area (n l)
;            (if (= n 4.)
;                (progn (area-data::reset-temporary-area gr:*cons-cache-area*)
;                       (loop 0 (create-n 200.)))
;                (loop (1+ n) l))))
;      (loop 0 (create-n 200.))))

;(defun listn (n)
;  (if (not (= 0 n))
;      (cons:cons n (listn (1- n)))))

;(defun mas (x y z)
;  (if (not (shorterp y x))
;      z
;      (mas (mas (cons::%cdr x) y z)
;          (mas (cons::%cdr y) z x)
;          (mas (cons::%cdr z) x y))))

;(defun shorterp (x y)
;  (and y (or (null x)
;            (shorterp (cons::%cdr x) (cons::%cdr y)))))

;(defun test-mas ()
;  (make-consing-region-for-initial-list)
;  (let ((list18 (listn 18.))
;       (list12 (listn 12.))
;       (list6  (listn 6.))
;       (region (make-test-consing-region)))
;    (labels ((loop (n)
;              (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
;              (mas list18 list12 list6)
;              ;; Primitive Garbage Collection
;              ;; Reset the free pointer.
;              (setf (region-data:region-free-pointer region)
;                    (quantum->address region))
;              (loop (1+ n))))
;      (loop 0))))

;(defun length (l)
;  (labels ((scan (l count)
;            (if (null l)
;                count
;                (scan (cons::cdr l) (1+ count)))))
;    (scan l 0)))

;(defun nconc (l1 l2)
;  (labels ((bash-last-pair (l)
;            (let ((c (cons::cdr l)))
;              (if (null c)
;                  (cons:rplacd l l2)
;                  (bash-last-pair c)))))
;    (if (null l1)
;       l2
;       (progn (bash-last-pair l1)
;              l1))
;    ))

;(defafun floor (x y)
;;;; a0 - fixnum dividend
;;;; a1 - fixnum divisor
;;;;
;;;; a2 - remainder
;;;; a3 - quotient
;;;; a4 - temp
;;;; a5 - bignum result pointer

;  (move nop a1 bw-24)                          ;check for zero-divide
;  (alu load-q-r a2 a0 a0 bw-24 br-zero)                ;q <- dividend
;  (branch zdiv (alu sign a2 a0 a0 bw-32))      ;sign extend initial remainder
;  (alu sdiv-first a2 a1 a2 bw-24)              ;step 1
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)
;  (alu sdiv-step  a2 a1 a2 bw-24)

;  (alu sdiv-last1 a2 a1 a2 bw-24)              ;first fixup
;  (alu pass-q a3 a1 a1 br-equal bw-24)         ;no fixup2 if zero, save quotient maybe
;  (branch done (alu setr nop ignore a3 bw-24))

;  (alu sdiv-last2 nop a1 a2 bw-24)             ;second fixup
;  (alu pass-q a3 a1 a1 bw-24)                  ;save fixed quotient
;  (alu quo-corr a3 a3 a3 bw-24)                        ;final fixup
;done
;  (alu pass-status a14 gr::*zero* gr::*zero* bw-24 boxed)
;  (move a15 gr::*two*)
;  (return a3)
;zdiv
;  (open-call (trap::illop-function 0) ignore ())
;  (unconditional-branch done (alu setr a3 ignore a1)))

;;(defun rplaca (list value)
;;  (cons::%set-car list value list))

;;(defun cons:rplacd (list value)
;;  (cons::%set-cdr list value list))

;(defun destructive (n m)
;  (let ((l (do ((i 10. (1- i))
;               (a () (li::push () a)))
;              ((= i 0) a))))
;    (do ((i n (1- i)))
;       ((= i 0))
;      (cond ((null (cons::car l))
;            (do ((l l (cons::cdr l)))
;                ((null l))
;              (or (cons::car l)
;                  (cons:rplaca l (cons::cons () ())))
;              (nconc (cons::car l)
;                     (do ((j m (1- j))
;                          (a () (li::push () a)))
;                         ((= j 0) a)))))
;           (t
;            (do ((l1 l (cons::cdr l1))
;                 (l2 (cons::cdr l) (cons::cdr l2)))
;                ((null l2))
;              (cons:rplacd (do ((j (ash (length (cons::car l2)) -1)
;                              (1- j))
;                           (a (cons::car l2) (cons::cdr a)))
;                          ((zerop j) a)
;                        (cons:rplaca a i))
;                      (let ((n (ash (length (cons::car l1)) -1)))
;                        (cond ((= n 0) (cons:rplaca l1 ())
;                               (cons::car l1))
;                              (t
;                               (do ((j n (1- j))
;                                    (a (cons::car l1) (cons::cdr a)))
;                                   ((= j 1)
;                                    (prog1 (cons::cdr a)
;                                           (cons:rplacd a ())))
;                                 (cons:rplaca a i))))))))))))

;(defun n-element-list (n)
;  (create-n n))

;(defun new-destructive (n m)
;  (let ((l (n-element-list 10.)))
;    (dotimes (pass n)
;;           (trap::illop "Beginning pass")
;      (destroy pass m l))))

;(defun destroy (pass m l)
;  (let ((first-element (cons::car l)))
;    (if (null first-element)
;       (replenish-lists m l)
;       (bash-lists l pass))))

;;(defmacro dolist ((element list) &body body)
;;  `(DO ((,element ,list (cons::cdr ,element)))
;;       ((null ,element))
;;     ,@body))

;(defun replenish-lists (m l)
;;  (trap::illop "Replenishing.")
;  (do ((cdrs l (cons::cdr cdrs)))
;      ((null cdrs))
;;    (trap::illop "Replenish-loop.")
;    (when (null (cons::car cdrs))
;      (cons:rplaca cdrs (cons::cons () ())))
;    (nconc (cons::car cdrs)
;          (n-element-list m)))
;  (trap::illop "Finished replenishing."))

;(defun bash-lists (l pass)
;  (do ((forward (cons::cdr l) (cons::cdr forward))
;       (behind  l             (cons::cdr behind)))
;      ((null forward))
;    (let* ((forward-sublist      (cons::car forward))
;          (half-forward (ash (length forward-sublist) -1))
;          (rest-forward
;            ;; Get second holf, and bash the first.
;            (do ((count half-forward (1- count))
;                 (a     forward-sublist (cons::cdr a)))
;                ((zerop count) a)
;              (cons:rplaca a pass)))
;          (behind-sublist     (cons::car behind))
;          (half-behind (ash (length behind-sublist) -1))
;          (rest-behind
;            ;; Get the second half, bash the first, differently
;            (if (zerop half-behind)
;                (progn (cons:rplaca behind '()) nil)
;                (do ((count half-behind (1- count))
;                     (a     behind-sublist (cons::cdr a)))
;                    ((= count 1)
;                     (prog1 (cons::cdr a)
;                            (cons:rplacd a ())))
;                  (cons:rplaca a pass)))))
;      (cons:rplaca rest-forward rest-behind))))

;(defun test-destructive ()
;  (labels ((loop (n)
;            (modify-leds (hw:ldb-not n (byte 3. 0.) 0.))
;            ;         (trap::illop "About to destruct.")
;            (let ((region (make-test-consing-region)))
;;             (trap::illop "About to destruct.")
;              (destructive 600. 50.)
;;             (trap::illop "Did destructive.")
;              (region-data:free-region region))
;            (loop (1+ n))))
;    (loop 0)))

;(defun test-string ()
;  (let ((test-string (array::make-string 3.)))
;    (array::aset-1 #\F test-string 0)
;    (array::aset-1 #\o test-string 1)
;    (array::aset-1 #\o test-string 2))
;  (trap::illop "Made string."))

;(defun test-symbol ()
;  (let ((test-string (array::make-string 3.)))
;    (array::aset-1 #\F test-string 0)
;    (array::aset-1 #\o test-string 1)
;    (array::aset-1 #\o test-string 2)
;    (let ((sym (symbol::%make-symbol test-string)))
;      (symbol::%set sym sym)
;      (trap::illop "Made a symbol."))))

;  (transporter-ram:initialize-transporter-ram)
;  (global::format t "done.~&Loading boot vector ... ")
;  (sim-debug::initialize-from-boot-vector)

;  ;; Load up the maps with the volatility and fresh cluster info.
;  ;; Note, we do this before setting up the paging system
;  ;; because this info is "outside" the virtual address space
;  ;; and can only be referenced via direct mapping.
;  (global::format t "done.~&Loading map data ... ")
;  (map:load-map *initial-map-data*)

;  (global::format t "done.~&Loading gc ram ...")
;  (gc-ram:load-ram *initial-gc-ram-data*)

;  ;; Should load the transporter ram in here.

;  ;; Setup the paging system.
;  (global::format t "done.~&Creating PCD table ... ")
;  (pcd:create-physical-cluster-data-table)
;  (pcd:initialize-physical-cluster-data)
;  (map:flush-direct-map)
;  )

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
    (hw:write-call-sp-hp #xEF00)                ;Empty the heap and stack ;; This is errics fault......
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
