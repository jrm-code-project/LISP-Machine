;;; -*- Mode:Lisp; Readtable:ZL; Package:USER; Base:8; Patch-File:T -*-
;;; Private patches made by naha
;;; Reason:
;;;
;;; Written 16-May-86 15:43:13 by naha (Mark Nahabedian) at site LMI Cambridge
;;; while running on Laurie Anderson from band 1
;;; with Experimental System 110.232, Experimental Lambda-Diag 7.5, Experimental Local-File 68.7, Experimental FILE-Server 18.4, Experimental Unix-Interface 9.1, Experimental ZMail 65.14, Experimental Object Lisp 3.1, Experimental Tape 6.38, Experimental Site Data Editor 3.3, Experimental Tiger 24.0, Experimental KERMIT 31.3, Experimental Window-Maker 1.1, Experimental Gateway 4.7, Experimental TCP-Kernel 39.7, Experimental TCP-User 62.7, Experimental TCP-Server 45.5, Experimental MEDIUM-RESOLUTION-COLOR 3.4, Experimental MICRO-COMPILATION-TOOLS 3.2, microcode 1505, SDU ROM 102, pace's band.



; From modified file DJ: L.IO; DISK.LISP#401 at 16-May-86 15:43:26
#8R SYSTEM-INTERNALS#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "SYSTEM-INTERNALS")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: IO; DISK  "

(defvar *disable-page-out-words* t)

;;; Just mark pages as good to swap out; don't actually write them.
(DEFUN PAGE-OUT-WORDS (ADDRESS NWDS &OPTIONAL ONLY-IF-UNMODIFIED &AUX STS)
  ONLY-IF-UNMODIFIED
  (when (null *disable-page-out-words*)
    (WITHOUT-INTERRUPTS
      (SETQ ADDRESS (%POINTER ADDRESS))
      ;; This DO is over the whole frob
      (DO ((ADDR (LOGAND (- PAGE-SIZE) ADDRESS) (%MAKE-POINTER-OFFSET DTP-FIX ADDR PAGE-SIZE))
           (N (+ NWDS (LOGAND (1- PAGE-SIZE) ADDRESS)) (- N PAGE-SIZE)))
          ((NOT (PLUSP N)))
        (OR (NULL (SETQ STS (%PAGE-STATUS ADDR)))       ;Swapped out
            ( (LDB %%PHT1-SWAP-STATUS-CODE STS)
               %PHT-SWAP-STATUS-WIRED)          ;Wired
            (%CHANGE-PAGE-STATUS ADDRESS %PHT-SWAP-STATUS-FLUSHABLE
                                 (LDB %%REGION-MAP-BITS
                                      (%REGION-BITS (%REGION-NUMBER ADDRESS)))))))))

(DEFUN REALLY-PAGE-OUT-PAGE (ADDRESS &AUX CCWP PS PHYS-ADR)
  "Write it on the disk, changing in-core page table status to RWF, etc."
  (when (null *disable-page-out-words*)
    (select-processor
      ((:lambda :cadr)
       (WITHOUT-INTERRUPTS
         (SETQ ADDRESS (%POINTER ADDRESS))
         (UNWIND-PROTECT
             (PROG ()
                   (WIRE-PAGE-RQB)
                   (SETQ CCWP %DISK-RQ-CCW-LIST)
                   ;; We collect some page frames to put them in, remembering the
                   ;; PFNs as CCWs.
                   (COND ((OR (NULL (SETQ PS (%PAGE-STATUS ADDRESS)))
                              (= 0 (LDB %%PHT1-MODIFIED-BIT PS))
                              (NULL (SETQ PHYS-ADR (%PHYSICAL-ADDRESS ADDRESS))))
                          (RETURN NIL))
                         (T (LET ((PFN (LSH PHYS-ADR -8)))
                              (ASET (1+ (LSH PFN 8)) PAGE-RQB CCWP)
                              (ASET (LSH PFN -8) PAGE-RQB (1+ CCWP)))
                            (SETQ CCWP (+ 2 CCWP))
                            (ASET (LOGAND (AREF PAGE-RQB (- CCWP 2)) -2)        ;Turn off chain bit
                                  PAGE-RQB (- CCWP 2))
                            (DISK-WRITE-WIRED PAGE-RQB 0 (+ (LSH ADDRESS -8) PAGE-OFFSET))
                            (%CHANGE-PAGE-STATUS ADDRESS (+ 1_23. %PHT-SWAP-STATUS-FLUSHABLE)
                                                 (LDB %%REGION-MAP-BITS
                                                      (%REGION-BITS (%REGION-NUMBER ADDRESS))))
                            (RETURN T)
                            )))
           ;; UNWIND-PROTECT forms
           (UNWIRE-PAGE-RQB)
           )))
      (:explorer nil))))
))
