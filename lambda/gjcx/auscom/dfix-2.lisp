;;; the modifications needed to get the read command to work properly
;;; are indicated with "; ****" comments at the end of the line.

(defun chint ()
  ;; this is the real thing, find out what caused the irq
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
         ;; set up DE status
         (write comsta *de*)
         (write chadd *subch*)
         (modify mulcom go 1 endg 1))
        ((boolean (read rtnreg rdcp))
         (cond ((boolean (read rtnreg endp))
                (message "Read complete and ending presented"))
               ('else
                (message "Read complete")
                (message "length = ~D bytes" (* 2 (dpb (read bfszu) (byte 8 8) (read bfszl))))))
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
