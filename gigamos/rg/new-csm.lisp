;;;;;-*- Mode:LISP; Package:LAMBDA; Base:8; Readtable:ZL -*-
;;;
;;; (c) Copyright 1984,1985,1986 - Lisp Machine, Inc.
;;;


;a CSM program is expressed in the form of BLOCKS and STATEMENTS.

;a BLOCK represents 16 consecutive statements stored at a multiple of 16 boundary.
; the block is intended for use with a particular one of the 8 one-of-16 next address
; selection possibilities of the CSM state machine.  Each BLOCK as a symbolic name which
; is a LISP symbol.  The BLOCK is specified by giving its name, the condition to be tested,
; and the symbolic names of the 16 codewords of the block.

;a STATEMENT specifies a CSM codeword.  A next condition is also specified which can
; be either a BLOCK, or another statement.

;Each statement specifies whether it is allowable for a "foreign" NUBUS write to occur
; during this cycle.  If so, an "call" to the verification subroutine is loaded in
; the "mirror" image, if not, a transfer to CACHE-VALIDATION-ERROR.

;CSMRAM fields:

(defconst csm.state (byte 11. 0))
(defconst csm.condition (byte 3 11.))
(defconst csm.cache.mode (byte 2 14.))  ;0 nothing, 1 read, 2 write if NU.xfer, 3 write if
                                        ; cache.hit
(defconst csm.MD.to.NUDATA.bus (byte 1 16.))
(defconst csm.RD.DATA.to.NUDATA.bus (byte 1 17.))
(defconst csm.cache.adr.from.NUADR (byte 1 18.))
(defconst csm.cache.hit.enable (byte 1 19.))
(defconst csm.memory.cycle.ending-L (byte 1 20.))  ;This bit defaulted high if not spec'd
(defconst csm.count.nuadr (byte 1 21.))  ;this used to be called csm.verify.cycle, which
                        ;was really unused
(defconst csm.virtual.page.group (byte 1 22.))
(defconst csm.NU.idle (byte 1 23.))   ;drives section of quad-latch on bus-arbitration
        ;print which in turn makes STARTs turn into START-ACKs (noops).
        ;this can be used to noop out cycle after bus requested and it turns
        ;out its not needed.
(defconst csm.statistics.bit (byte 1 24.))   ;used for scope trigger on error..
(defconst csm.lambda.STREQ (byte 1 25.))
(defconst csm.lambda.BREQ (byte 1 26.))
(defconst csm.request.noted-L (byte 1 27.))  ;This bit defaulted high if not spec'd
(defconst csm.parity (byte 4 28.))

(defconst csm.low.asserted.bits '(csm.request.noted-L csm.memory.cycle.ending-L))

;the following bits appear in the CSM code as if they were real bits, however,
; they are really ignored except that the one specified in the :statistics-bit-option
; to LOAD-CSM is loaded into the statistics bit.  Default is csm.cache.hit.bit.
(DEFVAR *CSM-STATISTICS-BIT-OPTION*)

(defconst csm.pseudo.bits
          '(csm.error.bit
             csm.requesting.nubus.bit
             csm.cycle.starting.bit
             csm.testing.mastership.bit
             csm.cache.hit.bit
             csm.fast.cache.hit.bit
             csm.slow.cache.hit.bit
             csm.test.slow.cache.bit
             csm.test.completion.bit
             csm.await.completion.bit
             csm.cycle.complete.bit
             csm.verify.bit
             csm.verify.error.bit))

(DEFUN PRINT-CSM-REG ()
  (print-csmram-codeword (read-csm-reg)))

(defun print-csmram-codeword (data)
  (TERPRI)
  (LET ((FIELD-LIST '(csm.state
                      csm.condition
                      csm.cache.mode
                      csm.MD.to.NUDATA.bus
                      csm.RD.DATA.to.NUDATA.bus
                      csm.cache.adr.from.NUADR
                      csm.cache.hit.enable
                      csm.memory.cycle.ending-L
                      csm.count.nuadr
                      csm.virtual.page.group
                      csm.NU.idle
                      csm.statistics.bit
                      csm.lambda.STREQ
                      csm.lambda.BREQ
                      csm.request.noted-L
                      csm.parity)))
    (DOLIST (F FIELD-LIST)
      (FORMAT T "~s ~o " F (LDB (SYMEVAL F) data)))))

;timing note:  cache.request L is initially asserted by memory.start.this.UINST,
;  so the READ-REQUEST or WRITE-REQUEST codewords are fetched during the source cycle
;  of the UINST following ((xxx-start-xxx) ..), which is normally (and will be referred to)
;  as the CALL-CONDITIONAL.  Thus, READ-REQUEST, for example, appears  in the CSMREG
;  at the beginning of the execute cycle of the CALL-CONDITIONAL.  The first level
;  map was also "started" during the source cycle of the CALL-CONDITIONAL, and thus is
;  "valid" when READ-REQUEST appears in CSMREG.  The second level map, however, (which is
;  the one that really matters), has just started fetching when READ-REQUEST is in CSMREG
;  and is NOT valid.  However, READ-REQUEST causes cache to fetch anyway, so that
;  at the end of READ-REQUEST, a grand and glorious multiway branch can be made.

(DEFCONST SIMPLE-CSM
 '(  ;Note!! IDLE-TEST must be at 0 so CSM can be started there and initialize right!
     (IDLE-TEST (CSM.NU.IDLE 1
                 CSM.MEMORY.CYCLE.ENDING-L 1
                 CSM.REQUEST.NOTED-L 1)
                AWAIT-REQUEST
                T)

     (AWAIT-REQUEST
       (BLOCK 1)        ;GND, -DEBUG.CLOCK.MODE.SYNCED, CACHE.REQUEST L, MEMORY.WRITE.CYCLE
       IDLE-TEST IDLE-TEST IDLE-TEST IDLE-TEST  ;in debug.clock mode, dont try anything
                                                ;  until clock running at full speed.
       READ-REQUEST WRITE-REQUEST-TO-MEMORY-0 IDLE-TEST IDLE-TEST ERROR)

     ;this block used by READ-TEST-1.  It tests conditions set up at end of READ.REQUEST,
     ; which corresponds to the end of the first execute cycle of the CALL-CONDITIONAL.
     ; (However, it tests them in "synced" form, which is why it is one cycle later).
     (READ-TEST-PAGE-FAULT
       (BLOCK 2)        ;memory.cycle.permit, cache.permit, v.pg.eq.phys.pg, cache.hit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT     ;all these no memory.cycle.permit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT
        READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no cache.permit
        READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
        READ-TEST-2 READ-TEST-2                    ;WAIT TO CHECK CACHE
        READ-TEST-2 READ-TEST-2
        )

     (READ-TEST-SLOW-CACHE
       (BLOCK 2)        ;memory.cycle.permit, cache.permit, v.pg.eq.phys.pg, cache.hit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT     ;all these no memory.cycle.permit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT
        READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no cache.permit
        READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
        READ-REQUEST-FROM-MEMORY                ;Ignore v.pg.eq.phys.pg
        READ-SLOW-CACHE-HIT
        READ-REQUEST-FROM-MEMORY                ;no hit
        READ-SLOW-CACHE-HIT                     ;!!
        )

     ;this block waits for mastership to be obtained
     (AWAIT-READ-CYCLE-MASTER
       (BLOCK 6)        ;-BUSY, -MASTER, -MYSTRT, -MYRQST
       READ-REQUEST-STARTING READ-REQUEST-STARTING
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-STARTING READ-REQUEST-STARTING    ;same as above, busy not important here
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       )

     ;this block waits for cycle completion once mastership has been noted
     (AWAIT-READ-CYCLE-COMPLETION
       (BLOCK 7)        ;LAMBDA.DMASTER.SYNCED, NU.ACK.SYNCED, NU.TM.1.SYNCED, NU.TM.0.SYNCED
       ;master should not go away before cycle completed
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR      ;these not master now?
       READ-REQUEST-STARTED READ-BLOCK-INTERMEDIATE-ACK ;these no ACK
       READ-REQUEST-STARTED READ-BLOCK-INTERMEDIATE-ACK
       READ-REQUEST-TRY-AGAIN-LATER  READ-REQUEST-TIMEOUT
       READ-REQUEST-ERROR  READ-REQUEST-COMPLETE)

     ;THIS ONE USED ON FIRST CYCLE AFTER READ HAS STARTED. TM1 AND TM0 ARE AS SEND OUT BY US.
     (AWAIT-READ-CYCLE-COMPLETION-FIRST
       (BLOCK 7)        ;LAMBDA.DMASTER.SYNCED, NU.ACK.SYNCED, NU.TM.1.SYNCED, NU.TM.0.SYNCED
       ;master should not go away before cycle completed
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR      ;these not master now?
       READ-REQUEST-STARTED READ-REQUEST-STARTED        ;these no ACK
       READ-REQUEST-STARTED READ-REQUEST-STARTED
       READ-REQUEST-TRY-AGAIN-LATER  READ-REQUEST-TIMEOUT
       READ-REQUEST-ERROR  READ-REQUEST-COMPLETE)

     (TEST-WRITE-CYCLE-PERMIT
       (BLOCK 2)        ;memory.cycle.permit, cache.permit, v.pg.eq.phys.pg, cache.hit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT     ;all these no memory.cycle.permit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT
        WRITE-REQUEST-TO-MEMORY)

     ;this block waits for mastership to be obtained
     (AWAIT-WRITE-CYCLE-MASTER
       (BLOCK 6)        ;-BUSY, -MASTER, -MYSTRT, -MYRQST
       WRITE-REQUEST-STARTING WRITE-REQUEST-STARTING
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-STARTING WRITE-REQUEST-STARTING
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY)

     (AWAIT-WRITE-CYCLE-COMPLETION
                ;LAMBDA.DMASTER.SYNCED, NU.ACK.SYNCED, NU.TM.1.SYNCED, NU.TM.0.SYNCED
       (BLOCK 7)
       WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR
       WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR
       WRITE-REQUEST-STARTED WRITE-REQUEST-STARTED      ;no ACK
       WRITE-REQUEST-STARTED WRITE-REQUEST-STARTED
       WRITE-REQUEST-TRY-AGAIN-LATER WRITE-REQUEST-TIMEOUT
       WRITE-REQUEST-ERROR WRITE-REQUEST-COMPLETE)

     (cache-validation-error    ;this symbol is specially known by the assembler/loaded.
       (csm.verify.error.bit 1  ;codewords in the other image which are supposedly
        csm.error.bit 1)        ;transfer here.
       cache-validation-error)

     (READ-REQUEST              ;This happens during execute cycle of CALL-CONDITIONAL.
       (CSM.VIRTUAL.PAGE.GROUP 0  ;Level 2 map first available just before end of this cycle.
        CSM.CACHE.HIT.ENABLE 0
        CSM.CACHE.MODE 0
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-TEST-1      ;GENERATE CACHE.HIT, WHICH GETS STROBED INTO CACHE.HIT.SYNCED
        T)              ; THIS CYCLE NECESSARY ONLY FOR CACHE TEST.

     (READ-TEST-1               ;This happens during source cycle of ((..) MD) in case
       (                        ; of immediate use of memory data.
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.HIT.ENABLE 0
        CSM.CACHE.MODE 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
        READ-TEST-PAGE-FAULT
       T)

     (READ-TEST-2
       (
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.HIT.ENABLE 1
        CSM.CACHE.MODE 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-TEST-SLOW-CACHE
       T)
     (READ-ABORT
       (CSM.REQUEST.NOTED-L 0
        CSM.MEMORY.CYCLE.ENDING-L 0)
       READ-ABORT-1
       T)

     (READ-ABORT-1
       ()
       AWAIT-REQUEST
       T)

     (READ-SLOW-CACHE-HIT
       (CSM.CACHE.HIT.BIT 1             ;pseudo
        CSM.SLOW.CACHE.HIT.BIT 1        ;pseudo
        CSM.CACHE.HIT.ENABLE 0
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.REQUEST.NOTED-L 0           ;acknowledge cache request.
        CSM.CACHE.MODE 0                ;SEE COMMENT ON READ-FAST-CACHE-HIT.
        CSM.MEMORY.CYCLE.ENDING-L 0)
       READ-SLOW-CACHE-HIT-EXIT-1
       T)

     (READ-SLOW-CACHE-HIT-EXIT-1        ;DELAY TO AVOID RETRIGGERING ON SAME REQUEST
       ()
       AWAIT-REQUEST
       T)

  ;main memory read stuff
     (READ-REQUEST-FROM-MEMORY
       (CSM.REQUESTING.NUBUS.BIT 1      ;PSEUDO
        CSM.TESTING.MASTERSHIP.BIT 1    ;PSEUDO
        CSM.LAMBDA.STREQ 1
        CSM.LAMBDA.BREQ 1
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       AWAIT-READ-CYCLE-MASTER
       T)
     (READ-REQUEST-STARTING
       (CSM.CYCLE.STARTING.BIT 1        ;PSEUDO
        csm.test.completion.bit 1       ;PSEUDO
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.REQUEST.NOTED-L 0           ;acknowledge cache.request
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.MODE 2                ;WRITE CACHE IF NU XFER OCCURS
        CSM.MEMORY.CYCLE.ENDING-L 1)
       AWAIT-READ-CYCLE-COMPLETION-FIRST
       NIL)
     (READ-REQUEST-STARTED
       (csm.test.completion.bit 1       ;pseudo
        csm.await.completion.bit 1      ;pseudo
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.MODE 2                ;WRITE CACHE IF NU XFER OCCURS
        csm.cache.adr.from.NUADR 1      ;address comes from frob within block if write.
        CSM.REQUEST.NOTED-L 1)
       AWAIT-READ-CYCLE-COMPLETION
       T)               ;***
     (READ-REQUEST-FAILED-TO-START
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       READ-REQUEST-FAILED-TO-START)

     (READ-REQUEST-TRY-AGAIN-LATER      ;SORT OF AN IDLE CYCLE, THEN TRY AGAIN.
       (CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-REQUEST-FROM-MEMORY
       T)

     (READ-REQUEST-TIMEOUT
        (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
        READ-REQUEST-TIMEOUT)

     (READ-REQUEST-ERROR                ;error signal received on NUBUS
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       READ-REQUEST-ERROR)

  ;ANYBODY WHO CAN GET HERE SHOULD ASSERT CSM.CACHE.MODE 2 SO CACHE GETS WRITTEN ON SAME
  ; CYCLE AS WE GET HERE.
     (READ-REQUEST-COMPLETE
       (                ;write data in cache..
        CSM.CYCLE.COMPLETE.BIT 1        ;pseudo
        ;CSM.CACHE.MODE 2               ;write on NU xfer NOPE, TOO LATE
        CSM.MEMORY.CYCLE.ENDING-L 0
        CSM.RD.DATA.TO.NUDATA.BUS 1)
       AWAIT-REQUEST
       T)

     (READ-BLOCK-INTERMEDIATE-ACK
       (                ;write data in cache..
        csm.test.completion.bit 1       ;pseudo
        CSM.CACHE.MODE 2                ;write on NU xfer (this would apply if next world of
                                        ; block comes on next cycle)
        csm.cache.adr.from.NUADR 1
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1
        csm.count.nuadr 1
        )
       AWAIT-READ-CYCLE-COMPLETION
       NIL)

     (RR-HBM-ERR                        ;master went away before ACK.
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       RR-HBM-ERR)

     (WRITE-REQUEST-TO-MEMORY-0
       (CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       TEST-WRITE-CYCLE-PERMIT
       T)

     (WRITE-REQUEST-TO-MEMORY
       (CSM.REQUESTING.NUBUS.BIT 1      ;PSEUDO
        CSM.TESTING.MASTERSHIP.BIT 1    ;PSEUDO
        CSM.LAMBDA.STREQ 1
        CSM.LAMBDA.BREQ 1
        CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       AWAIT-WRITE-CYCLE-MASTER
       T)

     (WRITE-REQUEST-STARTING
       (CSM.CYCLE.STARTING.BIT 1
        CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1
        )
       AWAIT-WRITE-CYCLE-COMPLETION-1
       NIL)

     (AWAIT-WRITE-CYCLE-COMPLETION-1    ;first cycle waiting for completion
       (csm.test.completion.bit 1       ;pseudo
        CSM.MD.TO.NUDATA.BUS 1
        CSM.REQUEST.NOTED-L 0           ;ack cache.request
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.MODE 2)
       AWAIT-WRITE-CYCLE-COMPLETION
       NIL)

     (WRITE-REQUEST-STARTED             ;rest of cycles waiting for completion
       (csm.test.completion.bit 1       ;pseudo
        csm.await.completion.bit 1      ;pseudo
        CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.VIRTUAL.PAGE.GROUP 0
        CSM.CACHE.MODE 2)
       AWAIT-WRITE-CYCLE-COMPLETION
       T)

     (WRITE-REQUEST-FAILED-TO-START
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-FAILED-TO-START)

     (WRITE-REQUEST-TRY-AGAIN-LATER
       (CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       WRITE-REQUEST-TO-MEMORY
       T)

     (WRITE-REQUEST-TIMEOUT
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-TIMEOUT)

     (WRITE-REQUEST-ERROR              ;error reply received on NUBUS
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-ERROR)

   ;CACHE IS WRITTEN ON CYCLE FOLLOWING RECEIPT OF ACK.  THUS ANYBODY WHO CAN GET HERE
   ; SHOULD ASSERT  CSM.CACHE.MODE 2
     (WRITE-REQUEST-COMPLETE
       (CSM.CYCLE.COMPLETE.BIT 1        ;pseudo
        CSM.MEMORY.CYCLE.ENDING-L 0
        CSM.REQUEST.NOTED-L 1
        CSM.MD.TO.NUDATA.BUS 1)
       AWAIT-REQUEST
       T)

     (WR-HBM-ERR                        ;master went away before ACK.
       (CSM.MEMORY.CYCLE.ENDING-L 0     ;AVOID HANGING WORLD
        csm.error.bit 1)
       WR-HBM-ERR)
 ))

(defconst cache-validate-subroutine '(  ;NOTE: CODE BELOW ASSUMES THIS TWO WORDS LONG.
      (csm.cache.mode 1
       csm.cache.adr.from.NUADR 1
       csm.virtual.page.group 0
       csm.verify.bit 1
       )
      (csm.cache.mode 3
       csm.cache.adr.from.NUADR 1
       csm.virtual.page.group 0
       )))

;CSM sensing of NUBUS:
;  example(), already NUBUS master, write cycle
;                  codeword in CSMREG.
;                    idle-test                          test AWAIT-REQUEST
;cache.request, memory.write.cycle asserted
;                    write-request-to-memory-0
;                                                       test WRITE-CYCLE-PERMIT

;                    write-request-to-memory(0)
;                       assert STREQ, BREQ              test AWAIT-WRITE-CYCLE-MASTER
; if NO master, loop at write-request-to-memory

;-start asserted     write-request-to-memory(1)
;  on NUBUS--           hold STREQ, BREQ                test AWAIT-WRITE-CYCLE-MASTER

;-data first         write-request-starting
;  asserted on NUBUS

;-ACK may come back  write-cycle-completion-1
;                                                       test AWAIT-WRITE-CYCLE-COMPLETION
; if NO completion, do WRITE-REQUEST-STARTED, or maybe WRITE-REQUEST-TRY-AGAIN
;                       or WRITE-REQUEST-TIMEOUT, WRITE-REQUEST-ERROR, or ...

;-XFER last cycle    WRITE-REQUEST-STARTED
;   asserted                                            test AWAIT-WRITE-CYCLE-COMPLETION
;  cache written
;                    WRITE-REQUEST-COMPLETE
;                                                       test AWAIT-REQUEST

;(W0) sampletime.   csm.breq and csm.streq asserted.
;        CSM codeword:  WRITE-REQUEST-TO-MEMORY   test: AWAIT-WRITE-CYCLE-MASTER
;(W0) drivetime.
; ASSUMING MASTERSHIP IMMEDIATELY AVAILABLE
;(W1) sampletime.   strobbed into PAL.  decides to take start cycle.  PAL outputs drive
;                    CSM condition inputs.
;(W1) drivetime.   PAL outputs transferred to quad latch. begin to drive NUBUS with start.
;(W2) sampletime.   CSM codeword fetched which reflects bus when start was asserted.
;(W3)
;(W4)
;(W5)


;the cache validation feature:
;  the high bit of the CSM address comes from NUBUS.WRITE.STARTING L, which is the
;NAND of -LAMBDA.DMASTER.SYNCED, NU.TM.1.SYNCED, NU.START.SYNCED, AND -NU.ACK.SYNCED.
;Thus, if a write cycle starts on the bus when we are not master, NUBUS.WRITE.STARTING L
;will assert.  The address gets captured in the NUADR register (since that loads
;on every NUBUS address.sampletime.)  The timing looks like this:

;S0                                                     (start cycle on bus)
;C0     start synced, NUADRnn strobed
;S1     random codeword RC1 strobed
;       CSMRAM accesses                                 (write data on bus)
;C1     validation test codeword strobed into CSMREG
;S2     CACHE.read to test for data going stale         (ACK traveling back on bus)
;C2     validation correct codeword strobed into CSMREG
;       CACHE.hit strobed into CACHE.hit.synced
;S3     CACHE.write to invalidate stale data            (soonest possible next start cycle.)
;        (if CACHE.hit.synced)
;C3     soonest time NUADR could be restrobed.  This clock would be C0 for following write
;              cycle.  Codeword which normally follows RC1 strobed.

;(key locn <rest of statement>)
(DEFCONST *CSM-ALIST* NIL)
(defvar *csm-main-block-locn*)  ;highest CSM adr used for main block.  Above here used
                                ; for calls on validate "subroutine".
(defvar *csm-validate-locn-pointer*)  ;Points to free location available for storage of
                                ;call on validate "subroutine".

;CSM words which supposedly cant be reached are filled with CACHE-VALIDATE-ERROR-WORD.
; The address is set to transfer to CACHE-VALIDATE-ERROR in the main image.

(defvar csmram-array (make-array 4096.))

(defun write-csmram-array (adr data)
  (aset (compute-parity-32 data) csmram-array adr))

(DEFUN LOAD-CSM-TO-ARRAY ()
  (load-csm ':fctn 'write-csmram-array))

(defun write-current-csmram-to-file ()
  (LOAD-CSM-TO-ARRAY)
  (with-open-file (standard-output "SYS:lambda-ucode;ulambda-csmram lisp" ':write)
    (format t ";;;-*- Mode:LISP; Package:LAMBDA; Base:16.;readtable:zl -*-")
    (let ((spy-file-info
            (COND ((< (SI:GET-SYSTEM-VERSION) 98.)
                   (cadar (get (si:get-source-file-name 'load-csm)
                               ':file-id-package-alist)))
                  (T (CONS (GET (SI:GET-SOURCE-FILE-NAME 'LOAD-CSM)
                                ':QFASL-SOURCE-FILE-UNIQUE-ID)
                           (THIRD (get (si:get-source-file-name 'load-csm)
                                       ':COMPILE-DATA)))))))
      (format t "~&;;; LAMBDA CSMRAM PROGRAM")
      (format t "~&;;; From file ~S (created ~\time\)"
              (car spy-file-info) (cdr spy-file-info))
      (format t "~&;;; This file created ~\datime\ by ~A" user-id))
    (format t "~&(setq csm-ram-list '(")
    (let ((zero-with-parity (compute-parity-32 0)))
      (dotimes (adr 10000)
        (format t "~&0~16r" (or (aref csmram-array adr) zero-with-parity))))
    (format t "~&))~&")
    (send standard-output ':truename)))


(defun write-current-csmram-to-BINARY-file (FILE)
  (LOAD-CSM-TO-ARRAY)
  (with-open-file (standard-output FILE :write)
    (let ((zero-with-parity (compute-parity-32 0)))
      (dotimes (adr 10000)
        (WRITE-BYTE (LDB (BYTE 8. 0.) (or (aref csmram-array adr) zero-with-parity)))
        (WRITE-BYTE (LDB (BYTE 8. 8.) (or (aref csmram-array adr) zero-with-parity)))
        (WRITE-BYTE (LDB (BYTE 8. 16.) (or (aref csmram-array adr) zero-with-parity)))
        (WRITE-BYTE (LDB (BYTE 8. 24.) (or (aref csmram-array adr) zero-with-parity)))))))



(DEFUN READ-CSM-AND-CHECK (ADR DATA &aux csm-data)
  (setq data (compute-parity-32 data))
  (cond (( (setq csm-data (READ-CSM ADR)) DATA)
         (format T "~%Bad data in csm at address ~o : expected ~o but found ~o, dfrs "
                 adr data (read-csm adr))
         (print-bits (logxor csm-data data)))))

(DEFUN WRITE-CSM-AND-PRINT (ADR DATA)
  (FORMAT T "~%ADR:~s" ADR)
  (PRINT-CSMRAM-CODEWORD DATA)
  (WRITE-CSM-WITH-PARITY ADR DATA))

(defun write-csm-with-parity (adr data)
  (write-csm adr (compute-parity-32 data)))

;the high bit of the CSM address comes from "not (foreign write cycle starting
; on NUBUS)".  Thus, it is normally asserted and the "normal" CSM image is actually the
; high half of the CSM memory.  For each codeword in the normal (high) half, there is
; a corresponding codeword which gets accessed if a foreigh NUBUS memory write
; cycle flies past just then.  In some cases, this is an error, because executing that
; codeword is supposed to imply we have NUBUS mastership.  Otherwise, the mirror location
; contains an entry to the "verify subroutine".   This subroutine is open coded at each
; use, and exits back to the original codeword.

(defun lam-reset-cache nil
  (WRITE-CSM-REG-VIA-CSMRAM (compute-parity-32 0)))

(DEFVAR *NO-VERIFY-ERRORS*)

(DEFUN LOAD-CSM (&KEY &OPTIONAL
                 (PRGM SIMPLE-CSM)
                 (FCTN 'WRITE-CSM-WITH-PARITY)
                 (STATISTICS-BIT-OPTION 'csm.cache.hit.bit)
                 (PRINT NIL)
                 (CHECK NIL)
                 (*NO-VERIFY-ERRORS* t))
  (cond ((not (memq statistics-bit-option csm.pseudo.bits))
         (ferror nil "statistics bit option unknown")))
  (COND ((ACCESS-PATH-LMI-SERIAL-PROTOCOL *PROC*)
         (FORMAT TERMINAL-IO "Telling SDU to load CSM")
         (FORMAT *PROC* "ALoading CSM~%")
         (FUNCALL *PROC* ':STRING-OUT "1c")
         (FUNCALL *PROC* ':READ-32))
        (T
         (cond ((null PRINT))
               ((eq fctn 'write-csm-with-parity)
                (setq fctn 'write-csm-and-print)))
         (cond ((null CHECK))
               ((eq fctn 'write-csm-with-parity)
                (setq fctn 'read-csm-and-check)))
         (LET ((LOCN 20)        ;Dont use block 0 because initialization entry there.
               (BLOCK-LOC-BOUNDARY NIL)
               (*CSM-STATISTICS-BIT-OPTION* STATISTICS-BIT-OPTION))
           (SETQ *CSM-ALIST* NIL)
           (DOLIST (S PRGM)
             (COND ((EQ (CAR (CADR S)) 'BLOCK)
                    (SETQ *CSM-ALIST*
                          (CONS (LIST* (CAR S) LOCN (CDR S))
                                *CSM-ALIST*))
                    (SETQ LOCN (+ LOCN 20)))
                   (T
                    (SETQ *CSM-ALIST*
                          (CONS (LIST* (CAR S) NIL (CDR S))
                                *CSM-ALIST*)))))
           (SETQ BLOCK-LOC-BOUNDARY LOCN)
           (DOLIST (A-L *CSM-ALIST*)            ;assign primary locns to statements
             (COND ((NULL (CADR A-L))
                    (RPLACA (CDR A-L)
                            (COND ((EQ (CAR A-L) 'IDLE-TEST) 0) ;special case, crock crock.
                                  ((CSM-FIND-LOC (CAR A-L)))    ;if stored as part of block.
                                  (T (PROG1 LOCN (SETQ LOCN (1+ LOCN)))))))))
           (setq *csm-main-block-locn* locn
                 *csm-validate-locn-pointer* *csm-main-block-locn*)
           (DOLIST (S PRGM)
             (LET* ((A-L (ASSQ (CAR S) *CSM-ALIST*))
                    (LOCN (CADR A-L)))
               (COND ((EQ (CAR (CADR S)) 'BLOCK)
                      (CSM-STORE-BLOCK FCTN LOCN (CDDR S)))
                     (T
                      (COND ((OR (< LOCN 20)
                                 (NOT (< LOCN BLOCK-LOC-BOUNDARY)))
                             (CSM-STORE-WORD FCTN LOCN (CDR S))))))))
           ))))

(DEFUN CSM-FIND-LOC (SYM &AUX TEM)
  (DOLIST (A-L *CSM-ALIST*)
    (COND ((EQ (CAR (CADDR A-L)) 'BLOCK)
           (COND ((SETQ TEM (FIND-POSITION-IN-LIST SYM (CDDDR A-L)))
                  (RETURN (+ TEM (CADR A-L)))))))))

(DEFUN CSM-STORE-BLOCK (FCTN LOCN STATEMENT-LIST)
  (DOTIMES (REL-ADR 20)
    (LET ((S (ASSQ (CAR STATEMENT-LIST) *CSM-ALIST*)))
      (COND ((EQ (CAR STATEMENT-LIST) 'ERROR)
             (CSM-STORE-ERROR FCTN (+ LOCN REL-ADR)))
            ((NULL S)
             (FERROR NIL "~%~s is an undefined statement" (CAR STATEMENT-LIST)))
            (T (CSM-STORE-WORD FCTN (+ LOCN REL-ADR) (CDDR S))))
      (IF (CDR STATEMENT-LIST)
          (SETQ STATEMENT-LIST (CDR STATEMENT-LIST))))))

(DEFUN CSM-STORE-WORD (FCTN LOCN STATEMENT)
  (LET ((WORD (CAR STATEMENT))
        (NEXT (CADR STATEMENT))
        (XFER-TO-IMAGE-ALLOWABLE (CADDR STATEMENT)))
    (FUNCALL FCTN               ;"NORMAL" IMAGE
             (+ 4000 LOCN)
             (+ (CSM-EVAL-WORD WORD)
                (CSM-EVAL-NEXT NEXT)))
    (FUNCALL FCTN               ;FOREIGN MEMORY WRITE STARTING IMAGE.
             LOCN
             (IF (OR XFER-TO-IMAGE-ALLOWABLE *NO-VERIFY-ERRORS*)
                 (CSM-CALL-CACHE-VALIDATE-SUBROUTINE FCTN STATEMENT LOCN)
               (CSM-CACHE-VALIDATE-ERROR-CODEWORD)))
    ))

(DEFUN CSM-CACHE-VALIDATE-ERROR-CODEWORD ()
  (+ (CSM-EVAL-WORD '(CSM.VERIFY.ERROR.BIT 1
                      CSM.ERROR.BIT 1))
     (CSM-EVAL-NEXT 'CACHE-VALIDATION-ERROR)))

(DEFUN CSM-CALL-CACHE-VALIDATE-SUBROUTINE (FCTN STATEMENT LOCN)
  ;FOR NOW, STORE A SEPARATE COPY OF SUBROUINE FOR EACH LOCN ITS CALLED FROM.
  ; IT COULD STORE ONLY ONE COPY PER STATEMENT ITS CALLED FROM.
  STATEMENT     ;NOT USED FOR NOW.
   ;switch back to main image for second word of subr
  (FUNCALL FCTN (+ 4000 *CSM-VALIDATE-LOCN-POINTER*)
           (+ (CSM-EVAL-WORD (CADR CACHE-VALIDATE-SUBROUTINE))  ;2ND WORD OF SUBR
              LOCN))   ;"return" to caller
  (FUNCALL FCTN *CSM-VALIDATE-LOCN-POINTER*    ;should not start another one..
           (CSM-CACHE-VALIDATE-ERROR-CODEWORD))

  (PROG1 (+ (CSM-EVAL-WORD (CAR CACHE-VALIDATE-SUBROUTINE))
            *CSM-VALIDATE-LOCN-POINTER*)
         (SETQ *CSM-VALIDATE-LOCN-POINTER* (1+ *CSM-VALIDATE-LOCN-POINTER*)))
  )

(DEFUN CSM-EVAL-WORD (FIELD-VALUE-LIST
                      &OPTIONAL
                      (VALUE-SO-FAR 0)
                      (DEFAULT-HIGH-LIST CSM.LOW.ASSERTED.BITS))
  (PROG (TEM FIELD VALUE)
        (SETQ TEM FIELD-VALUE-LIST)
   L    (COND ((NULL TEM)
               (GO DEFAULT)))
        (SETQ FIELD (FIRST TEM)
              VALUE (SECOND TEM))
        (COND ((MEMQ FIELD CSM.PSEUDO.BITS)
               (COND ((EQ FIELD *CSM-STATISTICS-BIT-OPTION*)
                      (SETQ FIELD CSM.STATISTICS.BIT))
                     (T (GO N)))))      ;IGNORE THIS ONE, ITS NOT SELECTED.
        (SETQ VALUE-SO-FAR
              (DPB (EVAL VALUE) (EVAL FIELD) VALUE-SO-FAR))
   N    (SETQ TEM (CDDR TEM))
        (GO L)
  DEFAULT
        (COND ((NULL DEFAULT-HIGH-LIST)
               (RETURN VALUE-SO-FAR))
              ((NULL (GET (LOCF FIELD-VALUE-LIST) (CAR DEFAULT-HIGH-LIST)))
               (SETQ VALUE-SO-FAR
                     (DPB 1 (SYMEVAL (CAR DEFAULT-HIGH-LIST)) VALUE-SO-FAR))))
        (SETQ DEFAULT-HIGH-LIST (CDR DEFAULT-HIGH-LIST))
        (GO DEFAULT)))

(DEFUN CSM-STORE-ERROR (FCTN LOCN)
  (FUNCALL FCTN LOCN (+ LOCN (CSM-EVAL-WORD NIL))))

(DEFUN CSM-EVAL-NEXT (NEXT)
  (LET ((A-L (ASSQ NEXT *CSM-ALIST*)))
    (IF (NULL A-L) (FERROR NIL "~%the symbol ~s is undefined" NEXT))
    (COND ((EQ (CAR (CADDR A-L)) 'BLOCK)
           (CSM-EVAL-WORD `(CSM.CONDITION ,(CADR (CADDR A-L))
                            CSM.STATE ,(CADR A-L))
                          0
                          NIL))
          (T (CSM-EVAL-WORD `(CSM.STATE ,(CADR A-L))
                            0
                            NIL)))))

(DEFUN CSM-SYMBOLIC-LOCATION (ADR)
  (DOLIST (A-L *CSM-ALIST*)
    (COND ((EQ (FIRST (THIRD A-L)) 'BLOCK)
           (COND ((= (LOGAND ADR 7760) (SECOND A-L))
                  (RETURN `(BLOCK ,(FIRST A-L) (,(NTH (LOGAND ADR 17) (CDDDR A-L))))))))
          ((= ADR (CADR A-L))
           (RETURN (CAR A-L))))))

(DEFCONST CSM.BRANCH.TESTS
 '(  ()                                         ;0 selects bits from state.sequence
     (MEMORY.WRITE.CYCLE CACHE.REQUEST 0 0)     ;1
     (CACHE.HIT V.PG.EQ.PHYS.PG CACHE.PERMIT MEMORY.CYCLE.PERMIT) ;2
     ()         ;3
     ()         ;4
     (-END.OF.TRANSFER)         ;5
     ()         ;6
     (NU.TM.0 NU.TM.1 NU.ACK LAMBDA.DMASTER)))  ;7

;each block consists of 16 CSM locations.  Has a symbolic name, an octal address,
; a branch-test-number and a list of following states.


;CSM blocks:
;  AWAIT-REQUEST.  select-condition 1.
;    goes to:   cache-read-request on cache-request and not memory.write.cycle
;               cache-write-request on cache-request and memory.write.cycle.
;       plus (in future)  cache validate request from NUBUS.

;  READ-REQUEST-FROM-MEMORY.
;    request NUBUS

;  AWAIT-NUBUS.   select-condition 7.
;
;  READ-REQUEST-START-CYCLE

;  AWAIT-READ-DATA.  select-condition 7.

;  reply to machine.


(DEFUN WRITE-VMA-START-READ-STEPPING (DATA)
  (WRITE-SPY-REG-AND-CHECK DATA)
  (LAM-EXECUTE (EXECUTOR LAM-EXECUTE-NOCLOCKS)
               LAM-IR-OP LAM-OP-ALU
               LAM-IR-OB LAM-OB-ALU
               LAM-IR-M-SRC lam-m-src-spy-reg
               LAM-IR-FUNC-DEST LAM-FUNC-DEST-VMA-START-READ
               LAM-IR-ALUF LAM-ALU-SETM)
  (WRITE-CSM-REG-FROM-CSMRAM 0)                 ;start off at 0
 ;  (SM-STEP-LOOP ':CSM-PRINTOUT T)
  )

;this one just replies immediately.  Tries no memory cycles to avoid hanging machine
; when stepping.  Always aborts, simulating page fault.  Otherwise as close as possible
; to normal.
;--this one is now somewhat out of date versus the real thing.. it should still serve
; its purpose tho.
(DEFCONST FAKE-CSM
 '(  ;Note!! IDLE-TEST must be at 0 so CSM can be started there and initialize right!
     (IDLE-TEST (CSM.NU.IDLE 1
                 CSM.MEMORY.CYCLE.ENDING-L 1
                 CSM.REQUEST.NOTED-L 1)
                AWAIT-REQUEST)

     (AWAIT-REQUEST
       (BLOCK 1)        ;GND, -DEBUG.CLOCK.MODE.SYNCED, CACHE.REQUEST L, MEMORY.WRITE.CYCLE
       read-request write-request-to-memory-0 idle-test idle-test
                ;this one goes ahead even in debug.clock mode.
                ;regular load waits if debug.clock mode
       READ-REQUEST WRITE-REQUEST-TO-MEMORY-0 IDLE-TEST IDLE-TEST ERROR)
     ;this block used by READ-TEST-1.  It tests conditions set up at end of READ.REQUEST,
     ; which corresponds to the end of the first execute cycle of the CALL-CONDITIONAL.
     ; (However, it tests them in "synced" form, which is why it is one cycle later).
     (READ-TEST-FAST-CACHE
       (BLOCK 2)        ;memory.cycle.permit, cache.permit, v.pg.eq.phys.pg, cache.hit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT     ;all these no memory.cycle.permit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT
 ;read-abort always
 ;      READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no cache.permit
 ;      READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
 ;      READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;could try for slow.hit
 ;      READ-REQUEST-FROM-MEMORY                ;no hit
 ;      READ-FAST-CACHE-HIT                     ;!!
        )
     ;this block waits for mastership to be obtained
     (AWAIT-READ-CYCLE-MASTER
       (BLOCK 6)        ;-BUSY, -MASTER, -MYSTRT, -MYRQST
       READ-REQUEST-STARTING READ-REQUEST-STARTING
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
  ;       READ-REQUEST-FAILED-TO-START READ-REQUEST-FAILED-TO-START
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-STARTING READ-REQUEST-STARTING    ;same as above, busy not important here
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY
  ;       READ-REQUEST-FAILED-TO-START READ-REQUEST-FAILED-TO-START
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       READ-REQUEST-FROM-MEMORY READ-REQUEST-FROM-MEMORY  ;no master
       )
     ;this block waits for cycle completion once mastership has been noted
     (AWAIT-READ-CYCLE-COMPLETION
       (BLOCK 7)        ;LAMBDA.DMASTER.SYNCED, NU.ACK.SYNCED, NU.TM.1.SYNCED, NU.TM.0.SYNCED
       ;master should not go away before cycle completed
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR
       RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR RR-HBM-ERR      ;these not master now?
       READ-REQUEST-STARTED READ-BLOCK-INTERMEDIATE-ACK ;these no ACK
       READ-REQUEST-STARTED READ-BLOCK-INTERMEDIATE-ACK
       READ-REQUEST-TRY-AGAIN-LATER  READ-REQUEST-TIMEOUT
       READ-REQUEST-ERROR  READ-REQUEST-COMPLETE)

     (TEST-WRITE-CYCLE-PERMIT
       (BLOCK 2)        ;memory.cycle.permit, cache.permit, v.pg.eq.phys.pg, cache.hit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT     ;all these no memory.cycle.permit
        READ-ABORT READ-ABORT READ-ABORT READ-ABORT
  ;always ABORT.
  ;     WRITE-REQUEST-TO-MEMORY
        )

     ;this block waits for mastership to be obtained
     (AWAIT-WRITE-CYCLE-MASTER
       (BLOCK 6)        ;-BUSY, -MASTER, -MYSTRT, -MYRQST
       WRITE-REQUEST-STARTING WRITE-REQUEST-STARTING
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
  ;       WRITE-REQUEST-FAILED-TO-START WRITE-REQUEST-FAILED-TO-START
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-STARTING WRITE-REQUEST-STARTING
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
  ;       WRITE-REQUEST-FAILED-TO-START WRITE-REQUEST-FAILED-TO-START
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY
       WRITE-REQUEST-TO-MEMORY WRITE-REQUEST-TO-MEMORY)
     (AWAIT-WRITE-CYCLE-COMPLETION
                ;LAMBDA.DMASTER.SYNCED, NU.ACK.SYNCED, NU.TM.1.SYNCED, NU.TM.0.SYNCED
       (BLOCK 7)
       WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR
       WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR WR-HBM-ERR
       WRITE-REQUEST-STARTED WRITE-REQUEST-STARTED      ;no ACK
       WRITE-REQUEST-STARTED WRITE-REQUEST-STARTED
       WRITE-REQUEST-TRY-AGAIN-LATER WRITE-REQUEST-TIMEOUT
       WRITE-REQUEST-ERROR WRITE-REQUEST-COMPLETE)

     (READ-REQUEST              ;This happens during execute cycle of CALL-CONDITIONAL.
       (CSM.VIRTUAL.PAGE.GROUP 1
  ;     CSM.CACHE.HIT.ENABLE 1
        CSM.CACHE.MODE 0;  1    ;read
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-TEST-1)
     (READ-TEST-1               ;This happens during source cycle of ((..) MD) in case
       (                        ; of immediate use of memory data.
        CSM.VIRTUAL.PAGE.GROUP 1  ;On fast cache hit, memory.replying.now asserts
        CSM.CACHE.HIT.ENABLE 1    ; during this cycle.
        CSM.CACHE.MODE 0 ; 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-TEST-FAST-CACHE)
     (READ-ABORT
       (CSM.REQUEST.NOTED-L 0
        CSM.MEMORY.CYCLE.ENDING-L 0)
       AWAIT-REQUEST)
     (READ-FAST-CACHE-HIT
       (CSM.VIRTUAL.PAGE.GROUP 1
        CSM.REQUEST.NOTED-L 0           ;acknowledge cache request.
        CSM.CACHE.MODE 0 ;1             ;causes data to appear on NUDATA bus.
        CSM.MEMORY.CYCLE.ENDING-L 0)
       AWAIT-REQUEST)
  ;main memory read stuff
     (READ-REQUEST-FROM-MEMORY
       (CSM.LAMBDA.STREQ 1
        CSM.LAMBDA.BREQ 1
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       AWAIT-READ-CYCLE-MASTER)
     (READ-REQUEST-STARTING
       (CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.REQUEST.NOTED-L 0           ;acknowledge cache.request
        CSM.MEMORY.CYCLE.ENDING-L 1)
       AWAIT-READ-CYCLE-COMPLETION)
     (READ-REQUEST-STARTED
       (CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       AWAIT-READ-CYCLE-COMPLETION)
     (READ-REQUEST-FAILED-TO-START
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       READ-REQUEST-FAILED-TO-START)
     (READ-REQUEST-TRY-AGAIN-LATER      ;SORT OF AN IDLE CYCLE, THEN TRY AGAIN.
       (CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       READ-REQUEST-FROM-MEMORY)
     (READ-REQUEST-TIMEOUT
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       READ-REQUEST-TIMEOUT)
     (READ-REQUEST-ERROR                ;error signal received on NUBUS
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       READ-REQUEST-ERROR)
     (READ-REQUEST-COMPLETE
       (                ;write data in cache..
        CSM.CACHE.MODE 2        ;write on NU xfer *** look into this
        CSM.MEMORY.CYCLE.ENDING-L 0
        CSM.RD.DATA.TO.NUDATA.BUS 1)
       AWAIT-REQUEST)
     (READ-BLOCK-INTERMEDIATE-ACK
       (                ;write data in cache..
        CSM.CACHE.MODE 2        ;write on NU xfer *** look into this
        CSM.RD.DATA.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1
        csm.count.nuadr 1
        )
       AWAIT-READ-CYCLE-COMPLETION)
     (RR-HBM-ERR                        ;master went away before ACK.
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       RR-HBM-ERR)

     (WRITE-REQUEST-TO-MEMORY-0
       (CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       TEST-WRITE-CYCLE-PERMIT)
     (WRITE-REQUEST-TO-MEMORY
       (CSM.LAMBDA.STREQ 1
        CSM.LAMBDA.BREQ 1
        CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       AWAIT-WRITE-CYCLE-MASTER)
     (WRITE-REQUEST-STARTING
       (CSM.MD.TO.NUDATA.BUS 1
        CSM.CACHE.MODE 2        ;write on NU xfer *** look into this
                             ;**fine point: try to assure cache and memory same if
        CSM.MEMORY.CYCLE.ENDING-L 1  ;write on bus just as we're trying to write it..
        CSM.REQUEST.NOTED-L 1
        )
       AWAIT-WRITE-CYCLE-COMPLETION-1)
     (AWAIT-WRITE-CYCLE-COMPLETION-1    ;first cycle waiting for completion
       (CSM.MD.TO.NUDATA.BUS 1
        CSM.REQUEST.NOTED-L 0           ;ack cache.request
        CSM.MEMORY.CYCLE.ENDING-L 1)
       AWAIT-WRITE-CYCLE-COMPLETION)
     (WRITE-REQUEST-STARTED             ;rest of cycles waiting for completion
       (CSM.MD.TO.NUDATA.BUS 1
        CSM.MEMORY.CYCLE.ENDING-L 1)
       AWAIT-WRITE-CYCLE-COMPLETION)
     (WRITE-REQUEST-FAILED-TO-START
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-FAILED-TO-START)
     (WRITE-REQUEST-TRY-AGAIN-LATER
       (CSM.MEMORY.CYCLE.ENDING-L 1
        CSM.REQUEST.NOTED-L 1)
       WRITE-REQUEST-TO-MEMORY)
     (WRITE-REQUEST-TIMEOUT
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-TIMEOUT)
     (WRITE-REQUEST-ERROR              ;error reply received on NUBUS
       (CSM.MEMORY.CYCLE.ENDING-L 0
        csm.error.bit 1)
       WRITE-REQUEST-ERROR)
     (WRITE-REQUEST-COMPLETE
       (CSM.MEMORY.CYCLE.ENDING-L 0
        CSM.REQUEST.NOTED-L 1
        CSM.MD.TO.NUDATA.BUS 1)
       AWAIT-REQUEST)
     (WR-HBM-ERR                        ;master went away before ACK.
       (CSM.MEMORY.CYCLE.ENDING-L 0     ;AVOID HANGING WORLD
        csm.error.bit 1)
       WR-HBM-ERR)
))
