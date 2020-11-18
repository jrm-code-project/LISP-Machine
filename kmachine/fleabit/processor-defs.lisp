;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;;; Processor Definitions

(defvar *frame-size* 16)

(defvar A0 0)
(defvar A1 (+ A0 1))
(defvar AN (+ A0 *frame-size* -1))
(defvar O0 (+ A0 *frame-size*))
(defvar O1 (+ O0 1))
(defvar O2 (+ O1 1))
(defvar ON (+ O0 *frame-size* -1))
(defvar R0 (+ O0 *frame-size*))
(defvar R1 (+ R0 1))
(defvar R2 (+ R1 1))
(defvar RN (+ R0 *frame-size* -1))
(defvar RETURN      (1+ RN))
(defvar RETURN-MV   (1+ RETURN))
(defvar RETURN-TAIL (1+ RETURN-MV))

(defvar IGNORED 'K:IGNORE)
(defvar STACK-0 100)

(defvar *open-regs* '(16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31))
(defvar *return-registers* (list 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))

(defvar *mv-return-nargs-offset* 3)
(defvar *mv-return-number-in-regs* 30.)
(defvar *mv-return-registers*
             '(gr:*return-0*  gr:*return-1*  gr:*return-2*  gr:*return-3*  gr:*return-4*
               gr:*return-5*  gr:*return-6*  gr:*return-7*  gr:*return-8*  gr:*return-9*
               gr:*return-10* gr:*return-11* gr:*return-12* gr:*return-13* gr:*return-14*
               gr:*return-15* gr:*return-16* gr:*return-17* gr:*return-18* gr:*return-19*
               gr:*return-20* gr:*return-21* gr:*return-22* gr:*return-23* gr:*return-24*
               gr:*return-25* gr:*return-26* gr:*return-27* gr:*return-28* gr:*return-29*))


(defvar active-regs '(K:A0 K:A1 K:A2 K:A3 K:A4 K:A5 K:A6 K:A7 K:A8
                           K:A9 K:A10 K:A11 K:A12 K:A13 K:A14 K:A15))
(defvar open-regs '(K:O0 K:O1 K:O2 K:O3 K:O4 K:O5 K:O6 K:O7 K:O8
                         K:O9 K:O10 K:O11 K:O12 K:O13 K:O14 K:O15))
(defvar return-regs '(K:R0 K:R1 K:R2 K:R3 K:R4 K:R5 K:R6 K:R7 K:R8
                           K:R9 K:R10 K:R11 K:R12 K:R13 K:R14 K:R15))
(defvar *all-regs* (concatenate 'vector active-regs open-regs return-regs
                                             '(K:RETURN K:RETURN-MV K:RETURN-TAIL)))

(defun register-number (reg)
  (position reg *all-regs*))

;;; Return T if ACC refers to
;;; any of the A, O or R registers
(defun register-p (acc)
  (and (integerp acc)
       (<= acc RN)))

(defun stack-slot-p (acc)
  (and (integerp acc)
       (>= acc STACK-0)))

(defun O-reg-p (reg)
  (and (integerp reg)
       (>= reg O0)
       (<= reg ON)))

(defun A-reg-p (reg)
  (and (integerp reg)
       (>= reg A0)
       (<= reg AN)))

(defun R-reg-p (reg)
  (and (integerp reg)
       (>= reg R0)
       (<= reg RN)))

(defun MD-p (dest)
  (or (member dest '(K:MD K:MD-START-WRITE K:MD-START-WRITE-NO-GC-TRAP))
      (and (consp dest)
           (member (car dest) '(K:MD K:MD-START-WRITE K:MD-START-WRITE-NO-GC-TRAP)))))

(defun addressable? (val)
  (or (eq val t)
      (eq val nil)
      (eq (zl:data-type val) 'zl:dtp-fix)
      (eq (zl:data-type val) 'zl:dtp-character)))

(defun rep-size (rep)
  1)
