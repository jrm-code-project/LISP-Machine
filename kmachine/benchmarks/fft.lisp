;;; -*- Mode:LISP; Package:user; Base:10; Readtable:ZL -*-

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


;;; *-*lisp*-*
;;; From Rich Duda, by way of Harry Barrow -- 3/26/82

(DEFUN FFT                                      ;Fast Fourier Transform
  (AREAL AIMAG &optional smallp)                ;AREAL = real part
  (PROG                                         ;AIMAG = imaginary part
   (AR AI PIE I J K M N LE LE1 IP NV2 NM1 UR UI WR WI TR TI)
    (SETQ AR (PROGN AREAL))                     ;Initialize
    (SETQ AI (PROGN AIMAG))
    (SETQ PIE (if smallp 3.14159265s00 3.141592653589793))
    (SETQ N (car (array:array-dimensions AR)))
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
             (SETQ TR (AREF AR J))
             (SETQ TI (AREF AI J))
             (SETF (AREF AR J)
                    (AREF AR I))
             (SETF (AREF AI J)
                    (AREF AI I))
             (SETF (AREF AR I) TR)
             (SETF (AREF AI I) TI)))
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
        (SETQ WR (COS (//$ PIE (if smallp (small-float le1) LE1))))
        (SETQ WI (SIN (//$ PIE (if smallp (small-float le1) LE1))))
        (DO J 1 (1+ J) (> J LE1)                        ;Loop thru butterflies
            (DO I J (+ I LE) (> I N)                    ;Do a butterfly
                (SETQ IP (+ I LE1))
                (SETQ TR (-$ (*$ (AREF AR IP) UR)
                             (*$ (AREF AI IP) UI)))
                (SETQ TI (+$ (*$ (AREF AR IP) UI)
                             (*$ (AREF AI IP) UR)))
                (SETF (AREF AR IP)
                       (-$ (AREF AR I) TR))
                (SETF (AREF AI IP)
                       (-$ (AREF AI I) TI))
                (SETF (AREF AR I)
                       (+$ (AREF AR I) TR))
                (SETF (AREF AI I)
                       (+$ (AREF AI I) TI)))
            (SETQ TR (-$ (*$ UR WR) (*$ UI WI)))
            (SETQ TI (+$ (*$ UR WI) (*$ UI WR)))
            (SETQ UR TR)
            (SETQ UI TI)))
    (RETURN T)))


(defvar *re*)
(defvar *im*)
(defvar *re$*)
(defvar *im$*)

(defun setup ()
  (setq *re* (array:make-array 1025. :element-type 'array:art-single-float))
  (fillarray *re* '(0.))
  (setq *im* (array:make-array 1025. :element-type 'array:art-single-float))
  (fillarray *im* '(0.))
  (setq *re$* (array:make-array 1025. :element-type 'array:art-single-float ':initial-value 0.0s00))
  (fillarray *re$* '(0.))
  (setq *im$* (array:make-array 1025. :element-type 'array:art-single-float ':initial-value 0.0s00))
  (fillarray *im$* '(0.)))

(defun timit ()
  (do ((ntimes 0 (1+ ntimes)))
      ((= ntimes 10.))
    (fft *re* *im*)))

(defun timit$ ()
  (do ((ntimes 0 (1+ ntimes)))
      ((= ntimes 10.))
    (fft *re$* *im$* t)))

;;;END

;;;;THIS MUST BE COMPILED WITH HARDEBECK COMPILER!!!!!
(defun test-fft ()
  (setup)
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "FFT complete." (timit) (hw:read-microsecond-clock))
  (loop))

(defun test-fft$ ()
  (hw:write-microsecond-clock (hw:unboxed-constant 0))
  (li:error "FFT$ complete." (timit$) (hw:read-microsecond-clock))
  (loop))

