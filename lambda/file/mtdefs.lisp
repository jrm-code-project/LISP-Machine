;;; -*- Mode: Lisp; Package: File-System; Base: 8 -*-

;;; MagTape definitions.  Mostly copied from RG;MT.
;internal functions:
; MT-PRINT-STATUS  prints current status from hardware.
; EXECUTE-MT-RQB actually does it.  This normally done by microcode for DISK-RQBs.
; MT-WAIT-READY, MT-WAIT-UNIT-READY
; UNIBUS-MAP-MT-RQB,  UNIBUS-UNMAP-MT-RQB.
; MT-RUN rqb command &optional minus-byte-count unit density ibm-mode
; MT-RUN-SIMPLE command unit &optional count.  For commands that dont transfer data.
; MT-SPACE, MT-SPACE-TO-EOF, MT-SPACE-REV, MT-SPACE-REV-TO-BOF, MT-SPACE-TO-APPEND
; MT-REWIND, MT-WRITE-EOF, MT-OFFLINE.

; Normal RQBs are now used for magtape, but
; PRINT-MT-RQB, WIRE-MT-RQB, UNWIRE-MT-RQB, UNIBUS-MAP-MT-RQB and UNIBUS-UNMAP-MT-RQB
; must be used with magtape RQBs.

;; This is also in FSDEFS
(REMPROP 'QUOTIENT-CEILING 'SOURCE-FILE-NAME)
(DEFSUBST QUOTIENT-CEILING (Y X) (CEILING Y X))

(DEFVAR FILE-SYSTEM-PACKAGE (PKG-FIND-PACKAGE "FS"))

(DEFCONST %MT-RQ-DONE-FLAG 0)           ;0 RQ ENTERED, -1 COMPLETED
               ;; These are set up by the requester
(DEFCONST %MT-RQ-COMMAND 1)             ;MT COMMAND REGISTER
(DEFCONST %MT-BYTE-COUNT 2)             ;NEGATIVE BYTE COUNT
(DEFCONST %MT-READ 3)                   ;ADDITIONAL COMMAND BITS AND STATUS
              ;; These are stored when the operation completes.
              ;; The order must agree with the order of the UNIBUS addresses.
(DEFCONST %MT-RQ-STATUS 4)              ;MT STATUS REG
(DEFCONST %MT-COMMAND-AFTER 5)          ;MT COMMAND REGISTER AFTER XFER
(DEFCONST %MT-BYTE-COUNT-AFTER 6)       ;MT RECORD COUNT AFTER
(DEFCONST %MT-RQ-MEM-ADDRESS 7)         ;LAST UNIBUS REF ADDR
(DEFCONST %MT-DATA-BUFFER 10)
(DEFCONST %MT-READ-AFTER 11)

(EVAL-WHEN (COMPILE LOAD EVAL)
;This says that the selected drive is ready.
(DEFSUBST MT-STATUS-READY ()
 (LDB-TEST 0001 (%UNIBUS-READ MT-UA-STATUS)))

;This says that the controller is ready.
(DEFSUBST MT-COMMAND-READY ()
  (LDB-TEST 0701 (%UNIBUS-READ MT-UA-COMMAND)))

;These are bits in the status stored after a request is executed.

;End of file mark reached when trying to read something.
(DEFSUBST MT-STATUS-EOF ()
 (LDB-TEST 1601 (AR-1 RQB %MT-RQ-STATUS)))

;End of tape reached.
(DEFSUBST MT-STATUS-EOT ()
 (LDB-TEST 1201 (AR-1 RQB %MT-RQ-STATUS)))

;At beginning of tape (rewound, or spaced back that far).
(DEFSUBST MT-STATUS-BOT ()
 (LDB-TEST 0501 (AR-1 RQB %MT-RQ-STATUS)))

;Any sort of error.
(DEFSUBST MT-STATUS-ERROR ()
 (NOT (ZEROP (LOGAND 115600 (AR-1 RQB %MT-RQ-STATUS)))))
)

;These are the OLD ways of accessing these flags.
(DEFCONST %%MT-STATUS-ILL-COM 1701)
(DEFCONST %%MT-STATUS-EOF 1601)
(DEFCONST %%MT-STATUS-ODD-LENGTH 1501)  ;Last word filled with 0s
(DEFCONST %%MT-STATUS-PARITY-ERR 1401)  ;parity error, LRC error, or postamble error
(DEFCONST %%MT-STATUS-GRANT-LATE 1301)
(DEFCONST %%MT-STATUS-EOT 1201)
(DEFCONST %%MT-STATUS-RLENGTH 1101)     ;RECORD LENGTH ERROR
(DEFCONST %%MT-STATUS-BAD-TAPE 1001)
(DEFCONST %%MT-STATUS-NXM 0701)
(DEFCONST %%MT-STATUS-ON-LINE 0601)
(DEFCONST %%MT-STATUS-BOT 0501)
(DEFCONST %%MT-STATUS-7-TRACK 0401)
(DEFCONST %%MT-STATUS-SETTLE 0301)      ;after rewinding
(DEFCONST %%MT-STATUS-WRITE-LOCK 0201)
(DEFCONST %%MT-STATUS-REWINDING 0101)
(DEFCONST %%MT-STATUS-READY 0001)
(DEFCONST %MT-STATUS-ERROR 115600)               ;Mask for bits which are errors normally

;This is the bit to set to tell the controller to execute the command
;already set up in the various registers.
(DEFCONST %MT-COMMAND-GO 1)

;These are fields to set up in the RQB's %MT-RQ-COMMAND word
;to control a transfer or operation to be done.

(EVAL-WHEN (COMPILE LOAD EVAL)
(DEFSUBST MT-COMMAND-UNIT ()
  (LDB 1003 (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-DENSITY ()
  (LDB 1502 (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-XBA-BITS ()
  (LDB 0402 (AR-1 RQB %MT-RQ-COMMAND)))

(DEFSUBST MT-COMMAND-INTERRUPT-ENABLE ()
  (LDB 0601 (AR-1 RQB %MT-RQ-COMMAND)))

;This field's value is one of the command codes below.
(DEFSUBST MT-COMMAND ()
  (LDB 0103 (AR-1 RQB %MT-RQ-COMMAND)))
)

;Mag tape command codes.
(DEFCONST %MT-COMMAND-OFFLINE 0)        ;unload tape
(DEFCONST %MT-COMMAND-READ 1)
(DEFCONST %MT-COMMAND-WRITE 2)
(DEFCONST %MT-COMMAND-WRITE-EOF 3)
(DEFCONST %MT-COMMAND-SPACE-FOR 4)
(DEFCONST %MT-COMMAND-SPACE-REV 5)
(DEFCONST %MT-COMMAND-WRITE-WITH-EXTENDED-GAP 6)
(DEFCONST %MT-COMMAND-REWIND 7)

(DEFCONST MT-COMMAND-NAMES
          '(%MT-COMMAND-OFFLINE %MT-COMMAND-READ %MT-COMMAND-WRITE
            %MT-COMMAND-WRITE-EOF %MT-COMMAND-SPACE-FOR %MT-COMMAND-SPACE-REV
            %MT-COMMAND-WRITE-WITH-EXTENDED-GAP %MT-COMMAND-REWIND))

(DEFCONST %%MT-COMMAND 0103)
(DEFCONST %%MT-COMMAND-XBA-BITS 0402)   ;EXTENDED UNIBUS ADR 17,16
(DEFCONST %MT-COMMAND-INTERRUPT-ENABLE 1_6.)
(DEFCONST %%MT-COMMAND-READY 0701)
(DEFCONST %%MT-COMMAND-UNIT 1003)
(DEFCONST %MT-COMMAND-POWER-CLEAR 1_12.)
(DEFCONST %%MT-COMMAND-DENSITY 1502)
(DEFCONST %MT-COMMAND-ERROR 100000)

;Unibus register addresses.
(DEFCONST MT-UA-STATUS  772520)
(DEFCONST MT-UA-COMMAND 772522)
(DEFCONST MT-UA-BYTEC   772524)
(DEFCONST MT-UA-CMA     772526)
(DEFCONST MT-UA-BFR     772530)
(DEFCONST MT-UA-DRD     772532)

;Error reporting.

(DEFUN MT-PRINT-STATUS NIL
  (FORMAT T "~%Status ")
  (MT-DECODE-STATUS (%UNIBUS-READ MT-UA-STATUS))
  (FORMAT T "~%Command ")
  (MT-DECODE-COMMAND (%UNIBUS-READ MT-UA-COMMAND)))

(DEFUN MT-DECODE-STATUS (STATUS)
  (CADR:CC-PRINT-SET-BITS STATUS
                     '(TAPE-UNIT-READY REWIND-STATUS WRITE-LOCK SETTLE-DOWN
                       SEVEN-TRACK BOT DRIVE-ON-LINE NXM
                       BAD-TAPE RECORD-LENGTH-ERROR EOT BUS-GRANT-LATE
                       PARITY-ERROR NOT-USED END-OF-FILE ILLEGAL-COMMAND)))

(DEFUN MT-DECODE-COMMAND (COM)
  (CADR:CC-PRINT-SET-BITS COM
                     '(GO FCN-0 FCN-1 FCN-2
                       XBA16 XBA17  INT-ENABLE CONTROLLER-READY
                       UNIT-SEL0 UNIT-SEL1 UNIT-SEL2 PARITY-EVEN
                       POWER-CLEAR DENSITY0 DENSITY1 ERROR)))

(DEFUN PRINT-MT-RQB (RQB)
  (FORMAT T "~%command before:")
  (MT-DECODE-COMMAND (AR-1 RQB %MT-RQ-COMMAND))
  (FORMAT T "~%Byte count before ~s" (AR-1 RQB %MT-BYTE-COUNT))
  (FORMAT T "~%command after:")
  (MT-DECODE-COMMAND (AR-1 RQB %MT-COMMAND-AFTER))
  (FORMAT T "~%status after:")
  (MT-DECODE-STATUS (AR-1 RQB %MT-RQ-STATUS))
  (FORMAT T "~%mem addr after ~S, byte-count-after ~S"
          (AR-1 RQB %MT-RQ-MEM-ADDRESS) (AR-1 RQB %MT-BYTE-COUNT-AFTER))
  RQB)

;Low-level command execution.
;Initialize the various header words of an RQB, such as %MT-RQ-COMMAND,
;then call EXECUTE-MT-RQB.

;; This must be used rather than SI:WIRE-DISK-RQB so not to hack the CCW list.
(DEFUN WIRE-MT-RQB (RQB &OPTIONAL (WIRE-P T) SET-MODIFIED
                    &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
                         (LOW (- (%POINTER RQB) (ARRAY-DIMENSION RQB 0) 2))
                         (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
                                  (FLOOR (ARRAY-LENGTH RQB) 2))))
  (DO LOC (LOGAND LOW (- PAGE-SIZE)) (+ LOC PAGE-SIZE) (>= LOC HIGH)
    (SI:WIRE-PAGE LOC WIRE-P SET-MODIFIED)))

(DEFUN UNWIRE-MT-RQB (RQB)
  (WIRE-MT-RQB RQB NIL))

(DEFUN EXECUTE-MT-RQB (RQB &OPTIONAL SET-MODIFIED)
  (WIRE-MT-RQB RQB T SET-MODIFIED)
  (MT-WAIT-READY)
  (LET ((UA (UNIBUS-MAP-MT-RQB RQB)))
    (%UNIBUS-WRITE MT-UA-COMMAND (AR-1 RQB %MT-RQ-COMMAND))     ;SELECT UNIT
    (MT-WAIT-UNIT-READY)
    (%UNIBUS-WRITE MT-UA-CMA UA)
    (%UNIBUS-WRITE MT-UA-BYTEC (AR-1 RQB %MT-BYTE-COUNT))
    (%UNIBUS-WRITE MT-UA-DRD (AR-1 RQB %MT-READ))
    (%UNIBUS-WRITE MT-UA-COMMAND (+ (AR-1 RQB %MT-RQ-COMMAND)
                                    %MT-COMMAND-GO))    ;TURN ON THE "GO" BIT.
    (MT-WAIT-READY)
    (DOTIMES (W 6)
      (AS-1 (%UNIBUS-READ (+ MT-UA-STATUS (* 2 W)))
            RQB
            (+ %MT-RQ-STATUS W)))
    (UNIBUS-UNMAP-MT-RQB RQB)
    (UNWIRE-MT-RQB RQB)
    RQB))

(DEFUN SIGN-EXTEND-16 (X)
  (IF (ZEROP (LOGAND 100000 X)) X (LOGIOR X -100000)))

(DEFVAR PAGE-SIZE-IN-BYTES (* PAGE-SIZE 4.))    ;8 bit bytes.

(DEFUN MT-WAIT-READY ()
  "Await controller ready"
  (OR (MT-COMMAND-READY)
      (PROCESS-WAIT "MagTape"
        #'(LAMBDA () (MT-COMMAND-READY))))
  NIL)

(DEFUN MT-WAIT-UNIT-READY ()
  "Await selected unit ready"
  (OR (MT-STATUS-READY)
      (PROCESS-WAIT "MT Unit"
        #'(LAMBDA () (MT-STATUS-READY))))
  NIL)

(DEFUN UNIBUS-MAP-MT-RQB (RQB &OPTIONAL (FIRST-UMP 0)
                    &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
                         (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
                                  (FLOOR (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ (%POINTER RQB) PAGE-SIZE)
             (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (UMP FIRST-UMP (1+ UMP))
       (NP 0 (1+ NP)))
      ((>= VADR HIGH))
    (COND ((> NP 14.)
           (FERROR NIL "TOO MANY PAGES")))
    (SETUP-UNIBUS-MAP UMP VADR))
  (+ 140000 (* FIRST-UMP 2000)))

(DEFUN UNIBUS-UNMAP-MT-RQB (RQB &OPTIONAL (FIRST-UMP 0)
                           &AUX (LONG-ARRAY-FLAG (%P-LDB %%ARRAY-LONG-LENGTH-FLAG RQB))
                                (LOW (- (%POINTER RQB) (ARRAY-DIMENSION RQB 0) 2))
                                (HIGH (+ (%POINTER RQB) 1 LONG-ARRAY-FLAG
                                         (FLOOR (ARRAY-LENGTH RQB) 2))))
  (DO ((VADR (+ LOW PAGE-SIZE) (+ VADR PAGE-SIZE)) ;Start with 2nd page of rqb array
       (UMP FIRST-UMP (1+ UMP)))
      ((>= VADR HIGH))
    (%UNIBUS-WRITE (+ 766140 (* 2 UMP)) 0)))

;;; The Unibus map is 16 words at 766140.  It consists of 14 address bits, write-ok, and valid
;;; It controls locations 140000-177777 (2000 byte locations per page).
(DEFUN SETUP-UNIBUS-MAP (UNIBUS-MAP-PAGE XBUS-ADR)
  (%UNIBUS-WRITE (+ 766140 (* 2 UNIBUS-MAP-PAGE))
                 (+ 140000 (LDB 1016 (%PHYSICAL-ADDRESS XBUS-ADR))))
  (+ 140000 (* UNIBUS-MAP-PAGE 2000) (* 4 (LOGAND 377 XBUS-ADR))))      ; returns ubus-adr

;Use MT-RUN to transfer the contents of an RQB.
;Use MT-RUN-SIMPLE to do spacing operations.

(DEFSIGNAL END-OF-TAPE FERROR (UNIT COMMAND BYTE-COUNT DENSITY IBM-MODE RQB)
  "Mag tape runs off end of tape.")

(DEFVAR MT-RETRY-COUNT 5)
(DEFVAR MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT 3)
(DEFUN MT-RUN (RQB COMMAND &OPTIONAL MINUS-BYTE-COUNT (UNIT 0) (DENSITY 0) IBM-MODE)
  (PROG ((RETRIES MT-RETRY-COUNT))
   AGAIN
        (SETF (AREF RQB %MT-RQ-COMMAND) 0)
        (SETF (MT-COMMAND-UNIT) UNIT)
        (SETF (MT-COMMAND-DENSITY) DENSITY)
        (SETF (MT-COMMAND) COMMAND)
        (IF (NULL MINUS-BYTE-COUNT)
            (SETQ MINUS-BYTE-COUNT
                  (MINUS (ARRAY-LENGTH (RQB-8-BIT-BUFFER RQB)))))
        (AS-1 MINUS-BYTE-COUNT RQB %MT-BYTE-COUNT)
        (AS-1 (IF IBM-MODE 1_10. 0)
              RQB %MT-READ)
        (EXECUTE-MT-RQB RQB (= COMMAND %MT-COMMAND-READ))
        (COND ((MT-STATUS-EOT)
               (CERROR ':NO-ACTION NIL 'END-OF-TAPE
                       "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
                       UNIT (NTH COMMAND MT-COMMAND-NAMES)
                       (- MINUS-BYTE-COUNT) DENSITY IBM-MODE RQB)))
        (COND ((NOT (MT-STATUS-ERROR))
               (RETURN T))
              (T (FORMAT T "~%MAGTAPE ERROR!")
                 (PRINT-MT-RQB RQB)
                 (MT-RUN-SIMPLE %MT-COMMAND-SPACE-REV UNIT 1)
                 (AND (= COMMAND %MT-COMMAND-WRITE)
                      ( RETRIES MT-ATTEMPT-TO-WRITE-WITH-EXTENDED-GAP-COUNT)
                      (SETQ COMMAND %MT-COMMAND-WRITE-WITH-EXTENDED-GAP))
                 (IF (>= (SETQ RETRIES (1- RETRIES)) 0)
                     (GO AGAIN)
                   (CATCH-ERROR-RESTART-EXPLICIT-IF T (MT-ERROR :RETRY "Retry magtape operation.")
                     (FERROR 'MT-ERROR "MagTape operation failed."))
                   (GO AGAIN))))))

(DEFUN MT-RUN-SIMPLE (COMMAND UNIT &OPTIONAL COUNT (RQB (GET-DISK-RQB 0) DONT-RETURN-RQB))
  (UNWIND-PROTECT
    (PROGN (IF COUNT (AS-1 (MINUS COUNT) RQB %MT-BYTE-COUNT))
           (SETF (AREF RQB %MT-RQ-COMMAND) 0)
           (SETF (MT-COMMAND) COMMAND)
           (SETF (MT-COMMAND-UNIT) UNIT)
           (EXECUTE-MT-RQB RQB))
    (OR DONT-RETURN-RQB
        (RETURN-DISK-RQB RQB)))
  T)

;Convenient spacing operations.

;Note: NTIMES fed thru to hardware.  NTIMES of 0 means moby many.
(DEFUN MT-SPACE (&OPTIONAL (NTIMES 1) (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-SPACE-FOR UNIT NTIMES))

(DEFUN MT-SPACE-TO-EOF (&OPTIONAL (UNIT 0) (NTIMES 1))
  (DOTIMES (C NTIMES)
    (MT-SPACE 0 UNIT)))

(DEFUN MT-SPACE-REV (&OPTIONAL (NTIMES 1) (UNIT 0))
  (DOTIMES (I NTIMES)
    (MT-RUN-SIMPLE %MT-COMMAND-SPACE-REV UNIT 1)))

;; Reverse through the tape, positioning the tape at the beginning of a file.
;; If SKIP-N-BLOCKS is 0, this positions the tape at the beginning of this file.
;; If SKIP-N-BLOCKS is 1, this positions the tape at the beginning of the previous file, etc.
;; If this reaches the beginning of the tape prematurely, it stops there and returns NIL.
(DEFUN MT-SPACE-REV-TO-BOF (&OPTIONAL (UNIT 0) (SKIP-N-FILES 0) &AUX RQB)
  (UNWIND-PROTECT
    (PROG ()
          (SETQ RQB (GET-DISK-RQB 0))
       L  (MT-RUN-SIMPLE %MT-COMMAND-SPACE-REV UNIT 1 RQB)
          (COND ((MT-STATUS-EOF)
                 (COND ((ZEROP SKIP-N-FILES)
                        ;; If we stop at an EOF block, we must space forward over it.
                        (MT-RUN-SIMPLE %MT-COMMAND-SPACE-FOR UNIT 1 RQB)
                        (RETURN T)))
                 (DECF SKIP-N-FILES))
                ((MT-STATUS-BOT)
                 (RETURN (ZEROP SKIP-N-FILES))))
          (GO L))
    (RETURN-DISK-RQB RQB)))

;; This function attempts to bypass all files on the tape until two
;; consecutive EOFs are found, then positions the tape over the last EOF.
;; The tape is now in a configuration allowing one to append new files.
(DEFUN MT-SPACE-TO-APPEND (&OPTIONAL (UNIT 0) &AUX RQB)
  (UNWIND-PROTECT
    (PROG ((EOF T))
          (SETQ RQB (GET-DISK-RQB 0))
       L  (MT-RUN-SIMPLE %MT-COMMAND-SPACE-FOR UNIT 1 RQB)
          (IF (MT-STATUS-EOF)
              (IF (NOT EOF)
                  (SETQ EOF T)
                  (MT-RUN-SIMPLE %MT-COMMAND-SPACE-REV UNIT 1 RQB)
                  (RETURN T))
              (SETQ EOF NIL))
          ;; MT-RUN-SIMPLE doesn't have this..
          (IF (MT-STATUS-EOT)
              (CERROR ':NO-ACTION NIL 'END-OF-TAPE
                       "End of tape on unit ~D, command ~D, ~D bytes.
Density ~S, IBM-mode ~S, rqb ~S."
                       UNIT '%MT-COMMAND-SPACE-FOR 1 0 NIL RQB))
          (GO L))
    (RETURN-DISK-RQB RQB))
  T)

(DEFUN MT-REWIND (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-REWIND UNIT))

(DEFUN MT-WRITE-EOF (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-WRITE-EOF UNIT))

(DEFUN MT-UNLOAD (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-OFFLINE UNIT))

(DEFUN MT-OFFLINE (&OPTIONAL (UNIT 0))
  (MT-RUN-SIMPLE %MT-COMMAND-OFFLINE UNIT))

;; Standard End of Tape handlers

;; This one is useful when you have transporting things which are bigger than
;; a tape.  it just rewinds, lets you reload, and continues.
;; Install it as a handler for END-OF-TAPE, using CONDITION-BIND.
(DEFUN CONTINUING-MT-EOT-HANDLER (CONDITION &AUX (UNIT (SEND CONDITION ':UNIT)))
  (PROG ((STREAM ERROR-OUTPUT))
        (MT-REWIND UNIT)
        (FUNCALL STREAM ':BEEP)
        (FORMAT STREAM "~%>>> MagTape unit ~D reached end of tape <<<~%" UNIT)
        (MT-OFFLINE UNIT)                       ;This will wait...
     L  (FORMAT STREAM "Please type [Resume] to continue tape operation: ")
        (FUNCALL STREAM ':CLEAR-INPUT)
        (COND ((NOT (CHAR-EQUAL (FUNCALL STREAM ':TYI) #\RESUME))
               (FUNCALL STREAM ':BEEP)
               (FUNCALL STREAM ':TYO #\CR)
               (GO L)))
        (FORMAT STREAM "[Resuming tape operation]~%")
        (RETURN ':NO-ACTION)))
