;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;; 4-Jun-86 21:34:44 DRIVER FOR THE AUSCOM MODEL 8600
;;;                   MULTIBUS CHANNEL INTERFACE
;;; - GEORGE CARRETTE


(DEFUN RESET (&OPTIONAL NOHACK)
  (WHEN (NOT *USER-MULTIBUS*)
    (MESSAGE "SETUP LAMBDA MULTIBUS INTERFACE")
    (SETQ *XPNT* NIL)
    (SETQ *SBUFF* NIL)
    (SETUP-USER-MULTIBUS))
  (OR *XPNT* (SETQ *XPNT* (ALLOCATE-USER-MULTIBUS-DMA-BUFFER (* 2 *WRLEN*))))
  (OR *SBUFF* (SETQ *SBUFF* (allocate-user-multibus-dma-buffer (* 2 *slen*))))
  (MESSAGE "RESET MASTER CONTROL")
  (WRITE MC 0)
  (MESSAGE "CLEARING REGISTER ARRAY")
  (WRITE RA_B[0] 0) (WRITE RA_B[1] 0) (WRITE RA_B[2] 0) (WRITE RA_B[3] 0)
  (WRITE RA_B[4] 0) (WRITE RA_B[5] 0) (WRITE RA_B[6] 0) (WRITE RA_B[7] 0)
  (WRITE RA_B[8] 0) (WRITE RA_B[9] 0) (WRITE RA_B[10] 0) (WRITE RA_B[11] 0)
  (WRITE RA_B[12] 0) (WRITE RA_B[13] 0) (WRITE RA_B[14] 0) (WRITE RA_B[15] 0)
  (UNLESS NOHACK
    (MESSAGE "WRITE MINIT")
    (MODIFY MINIT
            HSP (BOOLEAN *HSP*)
            SNGL (BOOLEAN *SNGL*)
            SWP (BOOLEAN *SWP*)
            )
    (SETF (ELT *E38BBT* 0) *SUBCH*)             ; set subchannel in burst/byte table
    (SETF (ELT *E38NRT* 0) *SUBCH*)             ; set subchannel in not ready table

    (FILL *VTABLE* :INVALID)
    (FILL *CHSTTABLE* 0) ;; appears to be the only reference to this in the code!
    (MESSAGE "init8259")
    ;; i am not sure what these do. documentation obscure on this point.
    ;; sounds like multibus/8086 considerations we dont care about.
;    (WRITE PIC1 #x13)                          ; edge trig, intvl=8, single, icw4
;    (WRITE PIC2 #x00)                          ; 5 bit int vector, bit0-2 don't care
;    (WRITE PIC2 0)
;    (WRITE PIC2 #x0D)                          ; 8086 set

;    (WRITE PIC2 #xFF)                          ; mask off all 8259 interrupts
    )
  )



(DEFUN MAIN (&OPTIONAL SET-OPTIONS)
  (WHEN SET-OPTIONS
    (OPTIONS))
  (DOLIST (O *OPTIONS*)
    (FORMAT T "~&~10S = ~10S ~A~%"
            (CAR o) (symeval (car o)) (CADR o)))

  (RESET)
  (OR (KLOAD2 *CSBUFF* *BUFFSIZE*)
      (DEVICE-ERROR "ERROR IN DOWNLOAD"))
  (MESSAGE "STARTING MICROPROCESSOR")
  ;(OR (NOT *VERBOSE*) (Y-OR-N-P "START UCODE?") (RETURN-FROM MAIN NIL))
  (MODIFY MC 8XRUN 1)
  (WAIT 1000)
  (MESSAGE "8X305 EXECUTING")
  ;;/* make sure we are offline by issuing an offline command */
  (MODIFY ADDCOM OFLN 1)
  (MODIFY MULCOM GO 1)
  (WAIT 5000)
  (MESSAGE "Initializing valid address RAM")
  (SETF (AREF *VTABLE* *SUBCH*) :VALID)
  (INITVADDR *vTable*)
  (MESSAGE "Zero 256x1 and Status RAM")
  (ZRAMS)
  (MESSAGE "Initializing 256x1 status RAM")
  (init256 *e38cmt* *VLCM* :set)
  (init256 *e38ict* *IMMC* :set)
  (COND (*BRSTFLG*
         (init256 *e38bbt* *BBR* :set))
        ('ELSE
         (init256 *e38bbt* *BBR* :clear)))
  (init256 *e38nrt* *NTRD* :clear)
  (MESSAGE "Initialization complete.")
  (MAIN-1))


(DEFUN MAIN-1 ()
  (GO-ON)
;  (write pic2 0)       ;;  /* clear interrupt mask */
;  (write pic1 #x0A)
;  (do ()
;      ((zerop (read pic1)))
    ;; issue eoi
;    (write pic1 #x10))

  ;; put the 8600 in the idle loop
  (when *as_de*
    (write comsta *de*)
    (write chadd *subch*)
    (modify mulcom endg 1))
  (RESET-MULTIBUS-INTERRUPT-5)
  (modify mulcom go 1)
  (message "Entering interrupt poll loop")
  (interrupt-poll-loop))


(defun interrupt-poll-loop ()
  (do-forever
    ;; instead of interrupt, wait for redy.
    ;;(PROCESS-WAIT-MULTIBUS-INTERRUPT-5)
    ;;    (RESET-MULTIBUS-INTERRUPT-5)
    (process-wait "auscom"
                  #'(lambda ()
                      (boolean (read rtnreg redy))))
    ;;(KIH)
    (chint)))

(defun KIH (&aux pint)
  "KIWI Interrupt handler"
  ;; see what kind of interrupt is requested
  (write pic1 #x0C) ;; set poll bit
  (setq pint (read pic1))
  (do ()
      ((NOT (= (LOGAND pint #x80) #x80)))
    (ecase (logand pint #x7)
      (0
       (write pic1 #x0a)
       (write pic1 #x10) ;; issue eoi
       (chint))
      (1
       (message "reset switch")
       (write pic1 #x0a)
       (write pic1 #x10) ;; issue eoi
       (device-error "reset switch"))
      ((2 3 4 5 6 7)
       (write pic1 #x20)
       (message "Unrecognized int from 8600.")
       (write pic1 #x0A)
       (message "Pic IR = ~X" (read pic1))))
    (write pic1 #x0C) ;; set poll bit
    (setq pint (read pic1))))

(defun chint ()
  ;; this is the real thing, find out what caused the irq
  (WHEN *VERBOSE* ;; **** DEBUGGING
    (DE RTNREG))  ;; **** DEBUGGING
  (if (not (boolean (read rtnreg redy)))
      (device-error "Ready bit not set after interrupt occurred."))
  (if (boolean (read rtnreg errb))
      (IF (EQ (eproc) :LOOP) (RETURN-FROM CHINT NIL)))
  (cond ((boolean (read rtnreg wrcp))
         (message "write complete")
         ;; get buffer size,  odd/even
         (setq *xlen* (- *WRLEN* (dpb (read bfszu) (byte 8 8) (read bfszl))))
         (cond ((boolean (read err1 odby))
                (setq *xodd* t)
                (message "length = ~D bytes" (1- (* *xlen* 2))))
               ('else
                (setq *xodd* nil)
                (message "length = ~D bytes" (* *xlen* 2))))
         (PROCESS-DATA (MBU:USER-MULTIBUS-DMA-BUFFER.ARRAY *XPNT*)
                       (- (* *XLEN* 2)
                          (IF *XODD* 1 0)))
         (COND ((READ RTNREG ENDP)              ; ****
                (MESSAGE "ALSO ENDING PRESENTED")))     ; ****
         (COND ((READ RTNREG CRCV)              ; ****
                (MESSAGE "ALSO COMMAND RECEIVED")       ; ****
                (PCOM))                         ; ****
               ('ELSE                           ; ****
                ;; **** DONT set up DE status
                ;; **** (write comsta *de*)
                ;; **** (write chadd *subch*)
                ;; **** (modify mulcom go 1 endg 1)
                (MODIFY MULCOM GO 1))))         ; ****
        ((boolean (read rtnreg rdcp))
         (cond ((boolean (read rtnreg endp))
                (message "Read complete and ending presented"))
               ('else
                (message "Read complete")))
         (message "length = ~D bytes" (* 2 (dpb (read bfszu) (byte 8 8) (read bfszl))))
         (cond ((boolean (read rtnreg crcv))    ; ****
                (message "also command received")       ; ****
                (pcom))                         ; ****
               ('else                           ; ****
                ;; CE, DE already presented on read, just issue GO
                (modify mulcom go 1))))         ; ****
        ((boolean (read rtnreg endp))
         (cond ((boolean (read rtnreg crcv))
                (message "ending presented and command received")
                (pcom))
               ('else
                (message "Ending presented")
                ;; just issue GO
                (modify mulcom go 1))))
        ((boolean (read rtnreg crcv))
         ;; command received,
         (pcom))
        ('else
         ;; Command not valid or immediate, error
         (message "Not a valid or immediate command.")
         (pra)
         (modify mulcom go 1))))



(DEFUN PRA ()
  (WHEN *VERBOSE*
    (DE)))


(DEFUN PCOM ()
  "process a received command"
  (COND ((= (READ COMSTA) *WRITE*)
         (MESSAGE "Write command received")
         ;; set up for a channel write
         (SETQ *XLEN* *WRLEN*)
         (SETQ *XODD* NIL)
         (SDMA *XPNT* *XLEN* *XODD*)
         ;; issue GO,WRITE plus ending=CE  **** AND ALSO DE
         (WRITE WDCNT *WCOUNT*)
         (WRITE COMSTA (LOGIOR *CE* *DE*))                      ; ****
         (MODIFY MULCOM GO 1 WRIT 1 ENDG 1))
        ((= (READ COMSTA) *READ*)
         (MESSAGE "Read command received")
         (CHREAD))
        ((= (READ COMSTA) *SENSE*)
         (MESSAGE "Sense command received")
         ;; set up for channel read, 24 bytes = 0
         (SDMA *SBUFF* *SLEN* *SODD*)
         ;; issue GO,READ plus ending=CE,DE
         (WRITE WDCNT *WCOUNT*)
         (WRITE COMSTA (LOGIOR *DE* *CE*))
         (MODIFY MULCOM GO 1 READ 1 ENDG 1))
        ((= (READ COMSTA) *DAER*)
         (MESSAGE "Read Backward command received")
         ;; do the same as a read
         (CHREAD))
        ((= (READ COMSTA) *NOP*)
         ;; shouldn't be here
         (MESSAGE "Farmer and Cam said we wouldn't get a NOP command, but we did"))
        ((MEMQ (READ COMSTA) *E38ICT*)
         ;; look up command in immediate command table
         (MESSAGE "Immediate command received ~2X" (READ COMSTA))
         ;; found immediate command
         ;; issue GO plus ending=DE
         (WRITE COMSTA *DE*)
         (MODIFY MULCOM GO 1 ENDG 1))
        ('ELSE
         (DEVICE-ERROR "Unknown command received ~2X" (READ COMSTA)))))

(DEFVAR *CHREAD-DATA-FUNCTION* NIL "if NIL then loop back data from write, otherwise this returns a string and length")

(defun chread ()
  (when *chread-data-function*
    (multiple-value-bind (string length)
        (funcall *chread-data-function*)
      (multiple-value-bind (q r)
          (floor length 2)
        (setq *xlen* q)
        (setq *xodd* (if (zerop r) nil t)))
      (copy-array-portion string
                          0 (min length (* 2 *wrlen*))
                          (mbu:user-multibus-dma-buffer.array *xpnt*)
                          0
                          (min length (* 2 *wrlen*)))))
  ;; set up for a channel read
  (sDma *xpnt* *xlen* *xodd*)
  ;; issue GO,READ plus ending = CD,DE
  (write wdcnt *wcount*)
  (write comsta (logior *de* *ce*))
  (modify mulcom go 1 read 1 endg 1))


(defun SDMA (buff len odd)
  ;; odd byte transsfer, or
  ;; even byte transsfer
  (modify minit odd (boolean odd))
  (write bfszl (ldb (byte 8 0) len))
  (write bfszu (ldb (byte 8 8) len))
  (let ((addr (USER-MULTIBUS-DMA-BUFFER.MULTIBUS-ADDRESS BUFF)))
    (write bfadl (ldb (byte 8 0) addr))
    (write bfadm (ldb (byte 8 8) addr))
    (write bfadu (ldb (byte 8 16) addr))))




(defun eproc ()
  (message "         ****error****")
  (when *verbose*
    (describe-errors))
  (cond ((boolean (read err0 chre))
         (go-on)
         (MODIFY MULCOM GO 1) ;; THIS ADDED TO CLEAR READY BIT
;        (write pic1 #x0A)
;        (write pic1 #x10) ;; issue eoi
         :LOOP
         )
        ((boolean (read err0 offl))
         (go-on)
         (MODIFY MULCOM GO 1) ;; THIS ADDED TO CLEAR READY BIT
;        (write pic1 #x0A)
;        (write pic1 #x10) ;; issue eoi
         :LOOP
         )
        ('else
         (ferror nil "unrecoverable device error"))))


(DEFUN KLOAD2 (addr size &AUX CSADDR P CNT ERROR LSB MSB)
  ;; WRITE THE CONTROL STORE. THEN READ IT BACK TO DOUBLE CHECK.
  (WRITE MC 0) ;RESET 8X305
  (MODIFY MC   ;SET UP FOR DOWNLOAD
          8XRUN 1
          DWNLD 1
          CSRWE 1)
  (SETQ CSADDR 0)
  (SETQ CNT 0)
  (SETQ P ADDR)
  (SETQ LSB (BYTE 8 0)
        MSB (BYTE 8 8))
  (MESSAGE "Loading KIWI control store")
  (DO ()
      ((= CNT SIZE))
    (WHEN (< (CAR P) CSADDR)
      (SETQ CSADDR 0)
      (WRITE MC 0)
      (MODIFY MC 8XRUN 1 DWNLD 1 CSRWE 1))
    (DO ()
        ((= (CAR P) CSADDR))
      (INCF CSADDR)
      (MODIFY-MC-STEP))
    (POP P)
    (WRITE DOWNLOAD-REGISTER-LSB (LDB LSB (CAR P)))
    (WRITE DOWNLOAD-REGISTER-MSB (LDB MSB (CAR P)))
    (POP P)
    (INCF CNT 2)
    (INCF CSADDR)
    (MODIFY-MC-STEP))
  (WRITE MC 0)
  (MODIFY MC DWNLD 1 CSRWE 1)
  (MODIFY MC 8XRUN 1)
  (SETQ CSADDR 0)
  (SETQ CNT 0)
  (SETQ P ADDR)
  (SETQ ERROR NIL)
  (MESSAGE "VERIFY KIWI CONTROL STORE")
  (BLOCK ERROR
    (DO ((VALUE))
        ((= CNT SIZE))
      (DO ()
          ((= (CAR P) CSADDR))
        (MODIFY-MC-STEP)
        (INCF CSADDR))
      (POP P)
      (SETQ VALUE (DPB (READ DOWNLOAD-REGISTER-MSB)
                       MSB
                       (DPB (READ DOWNLOAD-REGISTER-LSB)
                            LSB 0)))
      (COND ((= VALUE (CAR P))
             (INCF CNT 2)
             (INCF CSADDR)
             (POP P)
             (MODIFY-MC-STEP))
            ('ELSE
             (MESSAGE "AT CSADDR = ~D EXPECT ~X GOT ~X" CSADDR (CAR P) VALUE)
             (RETURN-FROM ERROR (SETQ ERROR T))))))
  (WRITE MC 0)
  (NOT ERROR))


(DEFUN MODIFY-MC-STEP ()
  (MODIFY MC STEP 1)
  ;; the "C" code didnt have this process-wait.
  ;; but the documentation indicates that if the STEP bit reads as 1
  ;; then the load operation has not completed.
  ;; Appears that the operation is fast enough so that
  ;; we always read this as zero. So this is just an error check.
  (PROCESS-WAIT "uload" #'(lambda () (not (boolean (read mc step)))))
  )


(DEFUN WAIT (time)
  (DO ((I TIME (1- I)))
      ((BOOLEAN (READ RTNREG REDY)))
    (WHEN (ZEROP I)
      (DEVICE-ERROR "Timed out waiting for 8x305 to acknowledge"))))

(DEFUN INITVADDR (TABLE)
 "initialize the valid address RAM - table is a list of 256
subchannel address Valid/Invalid values"
 (DOTIMES (COUNT 256)
   (WRITE OFFLINE-BUS-OUT COUNT)
   (ECASE (ELT TABLE COUNT)
     (:VALID
      (MODIFY ADDCOM WRVL 1)
      (MODIFY MULCOM GO 1))
     (:INVALID
      (MODIFY ADDCOM WRIN 1)
      (MODIFY MULCOM GO 1)))
   (WAIT 100))
 ;; there appears to be an error in the C-coded version of this
 ;; function.   "while (count <= 256)" would have the clause
 ;; executed for count=256, which would extend beyond the end
 ;; of the table. Since I'm not having much luck getting
 ;;  CHSEL to work later, perhaps this clause will fix things:
 ;;(WRITE OFFLINE-BUS-OUT 256)
 ;;(MODIFY ADDCOM WRVL 1)
 ;;(MODIFY MULCOM GO 1)
 ;;(WAIT 100)
 )

(DEFUN ZRAMS ()
  (WRITE COMSTA 0)
  (DOTIMES (COUNT 256)
    (WRITE CHADD COUNT)
    (MODIFY MULCOM GO 1 C256 1)
    (WAIT 100)
    (MODIFY MULCOM GO 1 CSTA 1)
    (WAIT 100)))


(defun init256 (table mask flip)
  (DOLIST (*table table)
    (write chadd *table)
    (WRITED addcom a256 1)
    (WRITED mulcom go 1)
    (wait 100)
    (ecase flip
      (:set
       (write comsta (logior mask (read comsta))))
      (:clear
       (write comsta (logand (lognot mask) (read comsta)))))
    (modify mulcom go 1 c256 1)
    (wait 100)))


(defun GO-ON ()
  (modify addcom onln 1)
  (modify mulcom go 1)
  (wait 8000)
  (do-forever
    (cond ((boolean (read rtnreg onl))
           (return (message "8600 online")))
          ('else
           (DEVICE-ERROR-R "wait forever for online"
                           "8600 failed to go online. RTNREG = #x~X"
                           (read rtnreg))
           (process-wait "go online" #'(lambda (x)
                                         (or (boolean (read rtnreg onl))
                                             (send x :listen))))
           (send terminal-io :tyi-no-hang)))))


(defun describe-errors ()
  (when (boolean (read rtnreg errb))
    (format t "~&RTNREG has ERRB set~%"))
  (dolist (bit (*de 'err0 nil))
    (when (boolean (cadr bit))
      (format t "~&ERR0: ~A~%" (or (get (car bit) 'err0) (car bit)))))
  (dolist (bit (*de 'err1 nil))
    (when (boolean (cadr bit))
      (format t "~&ERR1: ~A~%" (or (get (car bit) 'err1) (car bit))))))


(defprop device-error t :error-reporter)

(defun device-error (message &rest args)
  (if *verbose* (describe-errors))
  (apply 'ferror nil message args))

(defprop device-error-r t :error-reporter)

(defun device-error-r (resume message &rest args)
  (if *verbose* (describe-errors))
  (apply 'cerror resume message args))



(DEFUN MESSAGE (STRING &REST L)
  (WHEN *VERBOSE*
    (FORMAT T "~&")
    (APPLY 'FORMAT T STRING L)
    (FORMAT T "~%")))


(DEFUN OPTIONS ()
  (TV:CHOOSE-VARIABLE-VALUES *OPTIONS*
                             :LABEL "            Choose Device/Diagnostic Options"))


(defvar *chwrite-data-function* nil "A function to receive a STRING and LENGTH")

(DEFUN PROCESS-DATA (ARRAY LENGTH)
  (cond (*chwrite-data-function*
         (funcall *chwrite-data-function* array length))
        ('else
         (let ((s (SUBSTRING ARRAY 0 LENGTH)))
           (format t "~&DATA = ~S~%" s)
           (string-translate-in-place *ebcdic->lispm* s)
           (FORMAT T "~&EBCDIC-> ~S~%" s)))))
