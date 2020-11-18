;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;; MODIFIED AS PER AUSCOM 27-Jun-86 11:22:20
;;; **** MODIFICATIONS MARKED WITH ****

;;; * modified to assert both DE and CE together on write command
;;;   (needed for present system 38 microcode)
;;; * modified to handle command received in read complete.
;;; * other mods 20-Aug-86 09:47:04
;;;

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

;; modified to signal unrecoverable error in cases it cant handle.

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


;; modified to be able to wait forever for go online.
;; waits for a little bit then signals an error.
;; if user says <RESUME> then it waits forever, or until a character
;; is typed.

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


(defprop device-error-r t :error-reporter)

(defun device-error-r (resume message &rest args)
  (if *verbose* (describe-errors))
  (apply 'cerror resume message args))
