; -*- Mode:LISP; Package:GLOBAL-REGISTERS; Base:10; Readtable:CL -*-

;;;**********************************************************************************************************
;;;*****   This file should be identical to global-registers(-for-k-debugger) at all times WKF 5/5/88   *****
;;;**********************************************************************************************************

;;; These duplicate versions are needed to fool make-system since COMPILER-FOR-K and K-DEBUGGER
;;;  both use this file and k-debugger loads it in the k-user hierarchy.  WKF

;;;; Global Register Frames

;;; There are 16 global frames
;;; each frame has 16 registers
;;; each register has 33 bits
;;; how many were going to St. Ives?

;;; It is a very bad idea to delete entries from here or insert entries between existing entries
;;;  because it moves the locations of the rest of the globals.  If you do this, you must
;;; recompile the entire system.  What I have done is just picked a name like
;;; "*UNUSED-GLOBAL-<frame>-<offset>*" to indicate the ones I have removed.
;;; Also, changing the name of a global is tricky, you must kill off all of the existing names
;;; so the new ones get allocated in the right order.  A way that worked when I wrote this comment
;;; was to (SETQ NC::*GLOBAL-FRAMES* NIL) before loading this file. - JRM 29-Apr-87 19:58:10

;----------------------------------------------------------------
;;; 0 Trap
(define-global-frame trap)

(define-global-variable trap *save-oreg*)
(define-global-variable trap *save-left*)
(define-global-variable trap *save-right*)
(define-global-variable trap *save-status*)
(define-global-variable trap *save-jcond*)
(define-global-variable trap *save-trap*)
(define-global-variable trap *save-trap-pc*)
(define-global-variable trap *save-trap-pc+*)

(define-global-variable trap *trap-mask*)
(define-global-variable trap *trap-temp1*)      ;Avoid using the higher numbers.
(define-global-variable trap *trap-temp2*)
(define-global-variable trap *trap-temp3*)
(define-global-variable trap *trap-temp4*)
(define-global-variable trap *kbug-trap*)
(define-global-variable trap *save-o-a-r*)
(define-global-constant trap *trap-dtp-code-5*
  (hw:unboxed-constant #.(+ (lisp::ash vinc::$$dtp-code 26.) 4)))

;----------------------------------------------------------------
;;; 1 VMem
(define-global-frame vmem)

(define-global-variable vmem *findcore-pointer*)

(define-global-variable vmem *number-of-physical-clusters*)     ;not currently used.

(define-global-variable vmem *physical-cluster-free-pointer*)   ;used only by boot-allocate-physcial-clusters
(define-global-variable vmem *physical-cluster-data-table*)
(define-global-variable vmem *physical-cluster-free-list*)
(define-global-variable vmem *physical-cluster-free-clusters*)
(define-global-variable vmem *physical-cluster-initially-wired-pointer*)

(define-global-variable vmem *quantum-map*)     ;base vir (and phys) adr as fixnum.
                                ;currently <low half> of <high half> of quantum 1.

(define-global-variable vmem *region-bits*)     ;<high quarter> of quantum 1.  as fixnum.

(define-global-variable vmem *swapping-quantum*)

(define-global-variable vmem *quantum-map-semaphore* -1)

(define-global-variable vmem *page-fault-code* 0)
(define-global-variable vmem *allow-write-in-read-only* nil)
(define-global-variable vmem *temporary-map-entry-virtual-cluster* -1)  ;-1 free
                        ;otherwise in use. Set up to never fault. Used to write in read-only.

;;; 2 unused
;;note!!  register F of this frame is trashed by the kbug routines! see KBG-READ-OPEN-BOXED and KIH-ALU-G1FBR
;----------------------------------------------------------------
;;; 2 Memory Management

(define-global-frame memory-management)

(define-global-variable memory-management *region-free-pointer*)
(define-global-variable memory-management *region-allocation-status*)
(define-global-variable memory-management *region-end*)
(define-global-variable memory-management *region-gc-pointer*)

(define-global-variable memory-management *area-region-data*)   ;base of table
(define-global-variable memory-management *area-region-size*)
(define-global-variable memory-management *area-region-bits*)
(define-global-variable memory-management *region-list-thread*)

(define-global-variable memory-management *desperate-consing-area*)
(define-global-variable memory-management *default-consing-area*)
(define-global-variable memory-management *default-code-area*)
;;; this should not be neccessary, it should be a constant
(define-global-variable memory-management *special-pdl-area*)

;;; 5 unused

;----------------------------------------------------------------
;;; 3 Timer Registers

(define-global-frame timers)

(define-global-variable timers *one-million-count*                     1000000.)
(define-global-variable timers *elapsed-time-since-1900*               (hw:unboxed-constant       0.))
(define-global-variable timers *sequence-break-code*                   (hw:unboxed-constant       0.))
(define-global-variable timers *request-sequence-break*                nil)  ;tested with a
                ;dt-right-list.  Thus will cause a DT-TRAP if T which is the requesting state.
(define-global-variable timers *allow-sequence-break*                   0.)  ;if non-zero, it
                ;means we are in a critical section and cannot allow sequence breaks.
(define-global-variable timers *ticks-between-sequence-break-requests*  0.)
(define-global-variable timers *ticks-till-next-sequence-break*         0.)

(define-global-variable timers *debugger-step-count*                    0.)

;used to by save-state to hold stuff affected by memory cycles.
(define-global-variable timers *state-processor-status* 0)
(define-global-variable timers *state-processor-control* 0)
(define-global-variable timers *state-memory-status* 0)
(define-global-variable timers *state-memory-control* 0)
(define-global-variable timers *state-trap-register* 0)
(define-global-variable timers *state-vma* 0)
(define-global-variable timers *state-md* 0)
(define-global-variable timers *state-bits* 0)  ;boxed-md boxed-vma

;;; 0 unused

;----------------------------------------------------------------
;;; 4 Frequently Used Constants

(define-global-frame constants)

(define-global-constant constants *zero*       0.          "Fixnum zero")
(define-global-constant constants *one*        1.          "Fixnum one")
(define-global-constant constants *minus-one* -1.          "Fixnum negative one")
(define-global-constant constants *two*        2.          "Fixnum two")
(define-global-constant constants *T*          t           "Lisp symbol T")
(define-global-constant constants *NIL*        nil         "Lisp symbol nil")
(define-global-constant constants *all-zero* (hw:unboxed-constant #x00000000)  "32 bits of zero")
(define-global-constant constants *all-ones* (hw:unboxed-constant #xffffffff)  "32 bits of ones")
(define-global-constant constants *three*      3.          "Fixnum three")

(define-global-constant constants *four*       4.          "Fixnum four")
(define-global-constant constants *five*       5.          "Fixnum five")
(define-global-constant constants *six*        6.          "Fixnum six")
(define-global-constant constants *seven*      7.          "Fixnum seven")
(define-global-constant constants *eight*      8.          "Fixnum eight")
(define-global-constant constants *nine*       9.          "Fixnum nine")
(define-global-constant constants *ten*       10.          "Fixnum ten")

;;; 0 unused

;----------------------------------------------------------------
;;; 5 Datatypes

(define-global-frame datatypes)

(define-global-constant datatypes *data-type* #x061A        "Byte specifier for data-type field")
(define-global-constant datatypes *dtp-cons*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-cons 26.)))
(define-global-constant datatypes *dtp-bignum*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-bignum 26.)))
(define-global-constant datatypes *dtp-locative*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-locative 26.)))
(define-global-constant datatypes *dtp-character*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-character 26.)))
(define-global-constant datatypes *dtp-compiled-function*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-compiled-function 26.)))
(define-global-constant datatypes *dtp-unboxed-locative*
  (hw:unboxed-constant #.(lisp::ash vinc::$$dtp-unboxed-locative 26.)))

;;; used for the other side for dt-not-right-array-or-left-structure
(define-global-variable datatypes *random-array*)
(define-global-variable datatypes *random-structure*)

;;; 7 unused

;----------------------------------------------------------------
;;; 6 Lisp Internals

(define-global-frame lisp-internals)

(define-global-variable lisp-internals *stack-pointer*)
(define-global-variable lisp-internals *special-pdl-ptr*)
(define-global-variable lisp-internals *lexical-environment*)

;;; A little "stack" for subroutines which don't want to
;;; mess up the call hardware
(define-global-variable lisp-internals *arg-1*)
(define-global-variable lisp-internals *arg-2*)
(define-global-variable lisp-internals *value-1*)
(define-global-variable lisp-internals *value-2*)
(define-global-variable lisp-internals *return-pc-1*)
(define-global-variable lisp-internals *return-pc-2*)

(define-global-variable lisp-internals gr:*warm-symbols* nil)

(define-global-constant lisp-internals *dtp-code-1*
  (hw:unboxed-constant #.(+ (lisp::ash vinc::$$dtp-code 26.) 1)))

;;; The three "mini-fasl-" registers are a temporary hack for debugging bombs in the loader
(define-global-variable lisp-internals *mini-fasl-byte-counter*)        ;these three are no longer needed
(define-global-variable lisp-internals *mini-fasl-top-level-opcode*)
(define-global-variable lisp-internals *mini-fasl-top-level-opcode-byte-count*)

(define-global-variable lisp-internals *special-pdl-limit*)
(define-global-variable lisp-internals *stack-limit*)


;----------------------------------------------------------------
;;; 7 Call hardware loader

(define-global-frame call-hardware-loader)

;;; These are changed on context switches
(define-global-variable call-hardware-loader *control-pdl*)     ;the control pdl object
(define-global-variable call-hardware-loader *control-pdl-pointer*)     ;next thing to write to
(define-global-variable call-hardware-loader *control-pdl-limit*)       ;grow control pdl when pointer reaches here

(define-global-variable call-hardware-loader *ch-control-pdl-index*)
(define-global-variable call-hardware-loader *ch-temp-0*)
(define-global-variable call-hardware-loader *ch-temp-1*)
(define-global-variable call-hardware-loader *ch-temp-2*)
(define-global-variable call-hardware-loader *ch-temp-3*)
(define-global-variable call-hardware-loader *ch-temp-4*)
(define-global-variable call-hardware-loader *ch-base-csp*)     ;call hardware index to start dumping at
(define-global-variable call-hardware-loader *ch-dumper-return-pc*)
(define-global-variable call-hardware-loader *ch-temp-5*)
(define-global-variable call-hardware-loader *ch-temp-6*)
(define-global-variable call-hardware-loader *ch-stuff*)        ;obsolete
(define-global-variable call-hardware-loader *ch-stuff-pointer*)        ;obsolete
(define-global-variable call-hardware-loader *next-control-pdl*)  ;the control pdl to restore after dumping the current one

;;; 0 unused

;----------------------------------------------------------------
;;; 8 Cons

(define-global-frame cons-cache)

(define-global-variable cons-cache *cons-cache-area*                    -1)
(define-global-variable cons-cache *cons-cache-region*                  -1)
(define-global-variable cons-cache *cons-cache-free*            (hw:unboxed-constant 0))
(define-global-variable cons-cache *cons-cache-limit*           (hw:unboxed-constant 0))

(define-global-variable cons-cache *structure-cons-cache-area*          -1)
(define-global-variable cons-cache *structure-cons-cache-region*        -1)
(define-global-variable cons-cache *structure-cons-cache-free*   (hw:unboxed-constant 0))
(define-global-variable cons-cache *structure-cons-cache-limit*  (hw:unboxed-constant 0))

;;; These can't work like this, need to know volatility, etc.
;(define-global-variable cons-cache *copy-cons-cache-area*)
;(define-global-variable cons-cache *copy-cons-cache-region*)
;(define-global-variable cons-cache *copy-cons-cache-free*)
;(define-global-variable cons-cache *copy-cons-cache-limit*)

;(define-global-variable cons-cache *copy-structure-cons-cache-area*)
;(define-global-variable cons-cache *copy-structure-cons-cache-region*)
;(define-global-variable cons-cache *copy-structure-cons-cache-free*)
;(define-global-variable cons-cache *copy-structure-cons-cache-limit*)

;----------------------------------------------------------------
;;; 9 GC

(define-global-frame gc)

(define-global-variable gc *scavenge-work-while-consing*)
(define-global-variable gc *structure-handles*)


;;; 15 free

;----------------------------------------------------------------
;;; 10 and 11 Return values

(define-global-frame return-values-1)

(define-global-variable return-values-1 *return-0*)
(define-global-variable return-values-1 *return-1*)
(define-global-variable return-values-1 *return-2*)
(define-global-variable return-values-1 *return-3*)
(define-global-variable return-values-1 *return-4*)
(define-global-variable return-values-1 *return-5*)
(define-global-variable return-values-1 *return-6*)
(define-global-variable return-values-1 *return-7*)
(define-global-variable return-values-1 *return-8*)
(define-global-variable return-values-1 *return-9*)
(define-global-variable return-values-1 *return-10*)
(define-global-variable return-values-1 *return-11*)
(define-global-variable return-values-1 *return-12*)
(define-global-variable return-values-1 *return-13*)
(define-global-variable return-values-1 *return-14*)
(define-global-variable return-values-1 *return-15*)


(define-global-frame return-values-2)

(define-global-variable return-values-2 *return-16*)
(define-global-variable return-values-2 *return-17*)
(define-global-variable return-values-2 *return-18*)
(define-global-variable return-values-2 *return-19*)
(define-global-variable return-values-2 *return-20*)
(define-global-variable return-values-2 *return-21*)
(define-global-variable return-values-2 *return-22*)
(define-global-variable return-values-2 *return-23*)
(define-global-variable return-values-2 *return-24*)
(define-global-variable return-values-2 *return-25*)
(define-global-variable return-values-2 *return-26*)
(define-global-variable return-values-2 *return-27*)
(define-global-variable return-values-2 *return-28*)
(define-global-variable return-values-2 *return-29*)
(define-global-variable return-values-2 *save-return-crap-0*) ;; this register is reserved, do not use.
(define-global-variable return-values-2 *number-of-return-values*)

;;; 12 is next

;;; thru 15. is available hardware wise and allocation wise.
