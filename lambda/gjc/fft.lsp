
;;;-*- Mode:LISP; Package:BENCH-FFT; Base:10 -*-
;;; From the "Dick Gabriel" Benchmark Series.
;;; Enhancements (C) Copyright 1983, Lisp Machine, Inc.

;;;BEGIN
;;;FFT
;Barrow FFT
;Here is the Barrow FFT benchmark which tests floating operations
;of various types, including flonum arrays. (ARRAYCALL FLONUM A I)
;accesses the I'th element of the FLONUM array A, where these arrays are
;0-based. (STORE (ARRAYCALL FLONUM A I) V) stores the value V in the
;I'th element of the FLONUM array A.

;There was a fair amount of FLONUM GC's in the SAIL MacLisp run, which,
;when it needed to CORE up during GC, took 4.5 seconds of CPU time for the
;computation and 15 seconds for GC. Other configurations of memory required
;only 1.5 seconds for GC.

;Refer to this as FFT.
;                       -rpg-


(DEFMACRO AREF$ (A &REST L)
  #+MACLISP `(ARRAYCALL FLONUM ,A ,@L)
  #-MACLISP `(AREF ,A ,@L))

;;; *-*lisp*-*
;;; From Rich Duda, by way of Harry Barrow -- 3/26/82

(DEFUN FFT                                      ;Fast Fourier Transform
  (AREAL AIMAG &optional smallp)                ;AREAL = real part
  (PROG                                         ;AIMAG = imaginary part
   (AR AI PI I J K M N LE LE1 IP NV2 NM1 UR UI WR WI TR TI)
    (SETQ AR (PROGN AREAL))                     ;Initialize
    (SETQ AI (PROGN AIMAG))
    (SETQ PI (if smallp 3.14159265s00 3.141592653589793))
    (SETQ N (CADR (ARRAYDIMS AR)))
    (SETQ N (1- N))
    (SETQ NV2 (// N 2))
    (SETQ NM1 (1- N))
    (SETQ M 0)                                          ;Compute M = log(N)
    (SETQ I 1)
   L1 (COND
       ((< I N)(SETQ M (1+ M))(SETQ I (+ I I))(GO L1)))
    (COND ((NOT (EQUAL N (^ 2 M)))
           (ERROR "array size not a power of two.")))
    (SETQ J 1)                                          ;Interchange elements
    (SETQ I 1)                                          ;in bit-reversed order
   L3 (COND ((< I J)
             (SETQ TR (AREF$ AR J))
             (SETQ TI (AREF$ AI J))
             (SETF (AREF$ AR J)
                    (AREF$ AR I))
             (SETF (AREF$ AI J)
                    (AREF$ AI I))
             (SETF (AREF$ AR I) TR)
             (SETF (AREF$ AI I) TI)))
    (SETQ K NV2)
   L6 (COND ((< K J) (SETQ J (- J K))(SETQ K (// K 2))(GO L6)))
    (SETQ J (+ J K))
    (SETQ I (1+ I))
    (COND ((< I N)(GO L3)))
    (DO L 1 (1+ L) (> L M)                              ;Loop thru stages
        (SETQ LE (^ 2 L))
        (SETQ LE1 (// LE 2))
        (SETQ UR (if smallp 1.0s00 1.0))
        (SETQ UI (if smallp 0.0s00 0.0))
        (SETQ WR (COS (//$ PI (if smallp (small-float le1) (FLOAT LE1)))))
        (SETQ WI (SIN (//$ PI (if smallp (small-float le1) (FLOAT LE1)))))
        (DO J 1 (1+ J) (> J LE1)                        ;Loop thru butterflies
            (DO I J (+ I LE) (> I N)                    ;Do a butterfly
                (SETQ IP (+ I LE1))
                (SETQ TR (-$ (*$ (AREF$ AR IP) UR)
                             (*$ (AREF$ AI IP) UI)))
                (SETQ TI (+$ (*$ (AREF$ AR IP) UI)
                             (*$ (AREF$ AI IP) UR)))
                (SETF (AREF$ AR IP)
                       (-$ (AREF$ AR I) TR))
                (SETF (AREF$ AI IP)
                       (-$ (AREF$ AI I) TI))
                (SETF (AREF$ AR I)
                       (+$ (AREF$ AR I) TR))
                (SETF (AREF$ AI I)
                       (+$ (AREF$ AI I) TI)))
            (SETQ TR (-$ (*$ UR WR) (*$ UI WI)))
            (SETQ TI (+$ (*$ UR WI) (*$ UI WR)))
            (SETQ UR TR)
            (SETQ UI TI)))
    (RETURN T)))



;;; Sets up the two arrays
(DECLARE (SPECIAL *RE* *IM*))

(SETQ *RE* (*ARRAY NIL 'FLONUM 1025.))

(SETQ *IM* (*ARRAY NIL 'FLONUM 1025.))

;;; The timer which does 10 calls on FFT

;(include "timer.lsp")
(timer timit
       (do ((ntimes 0 (1+ ntimes)))
           ((= ntimes 10.))
           (fft *re* *im*)))

(DEFVAR *RE$* (make-array 1025. ':initial-value 0.0s00))

(DEFVAR *IM$* (make-array 1025. ':initial-value 0.0s00))

(timer timit$
       (do ((ntimes 0 (1+ ntimes)))
           ((= ntimes 10.))
           (fft *re$* *im$* t)))

;;;END
