;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:CL -*-
;;; ** (C) Copyright 1980, Massachusetts Institute of Technology
;;;    Enhancements (C) Copyright 1981, Symbolics, Inc.
;;; The Massachusetts Institute of Technology has acquired the rights from Symbolics
;;; to include the Software covered by the foregoing notice of copyright with its
;;; licenses of the Lisp Machine System **

(DEFUN UNIBUS-EXISTS-P (ADDR)
  "Return T if unibus location ADDR physically exists in the hardware."
  (WITHOUT-INTERRUPTS                           ;Microcode unfortunately hacks the unibus too
    (%UNIBUS-WRITE 766044 0)                    ;Clear error status
    (%UNIBUS-READ ADDR)
    (NOT (BIT-TEST 10 (%UNIBUS-READ 766044))))) ;Text NXM bit

;;; This file contains functions for interrupt-driven input from
;;; simple Unibus devices.  Note that the keyboard uses the same
;;; mechanism, but does not use these functions, so that this file
;;; need not be in the cold load and to avoid the expense of an
;;; extra wired page all the time for the keyboard buffer.

;;; SI:GET-UNIBUS-CHANNEL interrupt-vector CSR-address CSR-bits data-address n-data-words
;;;     &optional output-turnoff-unibus-address output-turnoff-data
;;;     n-data-words can be 1 or 2 (the number of 16-bit words at the data-address)
;;;     The two optional arguments are for output channels; they are the
;;;     arguments to a %UNIBUS-WRITE to be issued if the device interrupts and
;;;     the channel buffer is empty.
;;; SI:RETURN-UNIBUS-CHANNEL chan
;;; SI:READ-UNIBUS-CHANNEL chan -> returns 2 values in case n-data-words was 2
;;; SI:UNIBUS-CHANNEL-NOT-EMPTY chan -> T or NIL

;;; The UNIBUS-CHANNEL-QS in QCOM are known about by the microcode.
;;; They are offsets within a wired Unibus-channel data structure.
;;; This code stores such inside wired-down 1-page-long ART-32B arrays.
;;; These indices start at 1 to allow for the presence of an array header.

(DEFRESOURCE UNIBUS-CHANNEL (SIZE)
  :CONSTRUCTOR (MAKE-ARRAY SIZE ':TYPE 'ART-32B ':AREA DISK-BUFFER-AREA))

(DEFUN GET-UNIBUS-CHANNEL (INTERRUPT-VECTOR CSR-ADDRESS CSR-BITS DATA-ADDRESS N-DATA-REGS
                           BUFFER-SIZE
                           &OPTIONAL OUTPUT-TURNOFF-UNIBUS-ADDRESS OUTPUT-TURNOFF-DATA
                           TIME-STAMPED
                           &AUX CHAN)
  "Allocate a unibus channel for interrupt-driven hardware i//o.
INTERRUPT-VECTOR is the unibus interrupt vector of the device you want to use.
 This is how interrupts get to the propert channel.
CSR-ADDRESS is the unibus address of the device's status register.
CSR-BITS are the bits in that register that say that input is
 available or the device is ready for output.
DATA-ADDRESS is the unibus address that data is read from or written to.
N-DATA-REGS is the number of data registers in the device; normally 1,
 but may be 2 meaning read 32 bits of data from two consecutive registers.
BUFFER-SIZE is the size of array to use, in pages.
 The buffer space is almost equal to the number of words.

For output channels, OUTPUT-TURNOFF-DATA and OUTPUT-TURNOFF-UNIBUS-ADDRESS
specify how to turn off output interrupts when the buffer is empty.
The former is written into the unibus-word (16 bits) specified by the latter.
TIME-STAMPED if non-NIL says that each input word should be preceded
by another word containing the microsecond clock reading when the word arrived,
and each output word for an output channel is preceded by a microsecond clock time
saying when to output that word."
  (CHECK-ARG N-DATA-REGS (OR (= N-DATA-REGS 1) (= N-DATA-REGS 2)) "1 or 2")
  (LET ((SIZE (1- (* BUFFER-SIZE PAGE-SIZE))))
    (IF (> SIZE %ARRAY-MAX-SHORT-INDEX-LENGTH)
        (DECF SIZE))
    (SETQ CHAN (ALLOCATE-RESOURCE 'UNIBUS-CHANNEL SIZE)))
  (LOOP FOR I FROM 1 BELOW PAGE-SIZE            ;Zero out the array, including tag bits
        DO (%P-DPB-OFFSET 0 %%Q-LOW-HALF CHAN I)
           (%P-DPB-OFFSET 0 %%Q-HIGH-HALF CHAN I))
  (%P-DPB-OFFSET INTERRUPT-VECTOR %%Q-POINTER CHAN %UNIBUS-CHANNEL-VECTOR-ADDRESS)
  (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS CSR-ADDRESS) %%Q-POINTER
                 CHAN %UNIBUS-CHANNEL-CSR-ADDRESS)
  (%P-DPB-OFFSET CSR-BITS %%Q-POINTER CHAN %UNIBUS-CHANNEL-CSR-BITS)
  (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS DATA-ADDRESS) %%Q-POINTER
                 CHAN %UNIBUS-CHANNEL-DATA-ADDRESS)
  (WHEN (= N-DATA-REGS 2)
    (%P-DPB-OFFSET 1 %%UNIBUS-CSR-TWO-DATA-REGISTERS CHAN %UNIBUS-CHANNEL-CSR-BITS))
  (COND (OUTPUT-TURNOFF-UNIBUS-ADDRESS
         (%P-DPB-OFFSET (VIRTUAL-UNIBUS-ADDRESS OUTPUT-TURNOFF-UNIBUS-ADDRESS) %%Q-POINTER
                        CHAN %UNIBUS-CHANNEL-OUTPUT-TURNOFF-ADDRESS)
         (%P-DPB-OFFSET OUTPUT-TURNOFF-DATA %%Q-POINTER
                        CHAN %UNIBUS-CHANNEL-OUTPUT-TURNOFF-BITS)
         (%P-DPB-OFFSET 1 %%UNIBUS-CSR-OUTPUT CHAN %UNIBUS-CHANNEL-CSR-BITS)))  ;Output
  (IF TIME-STAMPED
      (%P-DPB-OFFSET 1 %%UNIBUS-CSR-TIMESTAMPED CHAN %UNIBUS-CHANNEL-CSR-BITS))
  (LET ((BUFFER-START (+ (%POINTER CHAN) 20)) ;leave room for expansion
        (BUFFER-END (+ (%POINTER CHAN) (* BUFFER-SIZE PAGE-SIZE))))
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START)
    (%P-DPB-OFFSET BUFFER-END %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
    (%P-DPB-OFFSET BUFFER-START %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
  (WITHOUT-INTERRUPTS
    (DOTIMES (I BUFFER-SIZE)
      (%WIRE-PAGE (%MAKE-POINTER-OFFSET DTP-FIX CHAN (* I PAGE-SIZE)))))
  (WITHOUT-INTERRUPTS
       (%P-DPB-OFFSET (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST)
                      %%Q-POINTER CHAN %UNIBUS-CHANNEL-LINK)
       (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST) CHAN))
  CHAN)

(DEFUN RETURN-UNIBUS-CHANNEL (CHAN)
  "Release unibus channel CHAN for re-use."
  (WHEN CHAN
    (WITHOUT-INTERRUPTS
      (DO ((X (%POINTER (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST))
              (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK)))
           (P NIL X))
          ((ZEROP X))
        (COND ((= (%POINTER CHAN) X)
               (COND ((NULL P)
                      (STORE (SYSTEM-COMMUNICATION-AREA %SYS-COM-UNIBUS-INTERRUPT-LIST)
                             (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK))))
                     ((%P-DPB (%P-LDB %%Q-POINTER (+ X %UNIBUS-CHANNEL-LINK))
                              %%Q-POINTER (+ P %UNIBUS-CHANNEL-LINK))))
               (RETURN NIL))))
      (WITHOUT-INTERRUPTS
        (DOTIMES (I (CEILING (ARRAY-LENGTH CHAN) PAGE-SIZE))
          (%WIRE-PAGE (%MAKE-POINTER-OFFSET DTP-FIX CHAN (* I PAGE-SIZE)))))
      (DEALLOCATE-RESOURCE 'UNIBUS-CHANNEL CHAN)
      NIL)))

(DEFUN UNIBUS-CHANNEL-NOT-EMPTY (CHAN)
  "T if data is available to be read or transmitted in unibus channel CHAN."
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (NEQ (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
       (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR)))

;;; Get next word (pair) from an input channel, or NIL if empty
(DEFUN READ-UNIBUS-CHANNEL (CHAN)
  "Read and return the next word from unibus channel CHAN, as two halfwords.
Two values are returned, each containing 16 bits of the word.
If no data are available, the values are NIL.  We do not wait.
Use UNIBUS-CHANNEL-NOT-EMPTY as a wait predicate if you wait."
  (DECLARE (RETURN-LIST FIRST-WORD SECOND-WORD))
  (AND (UNIBUS-CHANNEL-NOT-EMPTY CHAN)
       (LET* ((OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
              (VAL1 (%P-LDB (BYTE 16. 0.) OUT-PTR))
              (VAL2 (%P-LDB (BYTE 16. 16.) OUT-PTR)))
         (AND (= (SETQ OUT-PTR (1+ OUT-PTR))
                 (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END))
              (SETQ OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START)))
         (%P-DPB-OFFSET OUT-PTR %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR)
         (VALUES VAL1 VAL2))))

(DEFUN UNIBUS-CHANNEL-NOT-FULL (CHAN &OPTIONAL BUFFER-SIZE-LIMIT)
  "T if there is room for more input or output in unibus channel CHAN."
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET ((BUFFER-SIZE (- (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)
                        (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))))
    (< (\ (+ (- (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)
                (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
             BUFFER-SIZE)
          BUFFER-SIZE)
       (OR BUFFER-SIZE-LIMIT (1- BUFFER-SIZE)))))

(DEFUN UNIBUS-CHANNEL-SPACE-AVAILABLE (CHAN)
  "Returns the start and end of the next contiguous empty range in CHAN.
Both pointers are measured in words.
Use this on an output channel; fill up that range with data, call
UNIBUS-CHANNEL-ADVANCE, then call this again."
  ;; Note: we insist on keeping one empty word at all times
  ;; so that the buffer does not appear empty.
  (DECLARE (RETURN-LIST START-INDEX END-INDEX))
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET ((IN-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR))
        (OUT-PTR (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-OUT-PTR))
        (START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
        (END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END)))
    (IF (> OUT-PTR IN-PTR)
        (SETQ END (1- OUT-PTR))
      (IF (= OUT-PTR START)
          (SETQ END (1- END))))
    (VALUES (- IN-PTR (%POINTER CHAN) 1) (- END (%POINTER CHAN) 1))))

(DEFUN UNIBUS-CHANNEL-ADVANCE (CHAN NEW-INDEX)
  "Advance the storing pointer in unibus channel CHAN."
  (CHECK-ARG CHAN ARRAYP "an array (a Unibus channel)")
  (LET* ((START (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-START))
         (END (%P-LDB-OFFSET %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-END))
         (IN-PTR (+ NEW-INDEX (%POINTER CHAN) 1)))
    (OR ( START IN-PTR END) (FERROR NIL "Index lies outside of buffer"))
    (IF (= IN-PTR END) (SETQ IN-PTR START))
    (%P-DPB-OFFSET IN-PTR %%Q-POINTER CHAN %UNIBUS-CHANNEL-BUFFER-IN-PTR)))


(defun print-unibus-channel-list ()
  (without-INTERRUPTS
    (do ((x (%pointer (system-communication-area %sys-com-unibus-interrupt-list))
            (%p-ldb %%q-pointer (+ x %unibus-channel-link)))
         (p nil x))
        ((zerop x))
     (format t "~%channel at ~s: vector ~s, csr adr ~s, csr bits ~s, data adr ~s"
             x
             (%p-ldb %%q-pointer (+ x %unibus-channel-vector-address))
             (%p-ldb %%q-pointer (+ x %unibus-channel-csr-address))
             (%p-ldb %%q-pointer (+ x %unibus-channel-csr-bits))
             (%p-ldb %%q-pointer (+ x %unibus-channel-data-address))))))
