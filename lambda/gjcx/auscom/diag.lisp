;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;; 8-Jun-86 17:43:54 DRIVER DIAGS FOR THE AUSCOM MODEL 8600
;;;                   MULTIBUS CHANNEL INTERFACE
;;; - GEORGE CARRETTE


;;; just a couple diagnostics so we can see more of the board functioning
;;; in our multibus before taking it out for a spin with a real IBM channel.


(defvar *diag-level* 0)

(defvar *cmt* '(1 2))
(defvar *bbt* '(1))
(defvar *nrt* '(1))


(defvar *VSBCH* 1 "the subchannel to use for the diagnostic")


(defun diag-loadop ()
  (message "setup for DIAG operational microcode")
  (reset T)
  (OR (KLOAD2 *CSBUFF* *BUFFSIZE*)
      (DEVICE-ERROR "ERROR IN DOWNLOAD"))
  (write rtnreg 0) ;; clear REDY bit to insure handshake
  (write mc 0)
  (message "starting the microcode")
  (MODIFY MC 8XRUN 1)
  (wait 5000)
  (setq *diag-level* 3))

(defvar *vtbl1* (make-list 256))



;; symbols for the bits in offline-tag-out register

(defconst *ADRO* #x80)
(defconst *HLDO* #x40)
(defconst *SELO* #x20)
(defconst *CMDO* #x10)
(defconst *SRVO* #x08)
(defconst *DATO* #x04)
(defconst *SUPO* #x02)
(defconst *BPARO* #x01)

(defconst *ADRI* #x80)
(defconst *OPLI* #x40)
(defconst *SELI* #x20)
(defconst *STAI* #x10)
(defconst *SRVI* #x08)
(defconst *DATI* #x04)
(defconst *REQI* #x02)
(defconst *BPARI* #x01)


(defvar *timg* nil)
(defvar *buserr* nil)
(defvar *tagerr* nil)

(defvar *offline-data-array* nil)
(defvar *offline-data-size* nil)

(defun diag-dmaRead (&optional trace random-data)
  "The board will read data from *XPNT*. We read OFFLINE-BUS-IN register and compare it with *XPNT*"
  (unless (= *diag-level* 3)
    (ferror nil "Must be in level 3 - execute diag-loadop"))
  (message "DMA read - multibus to channel")
  ;; the operational microcode should be in the
  ;; initialization loop.  Make sure it is by reseting
  ;; the 8x305 and starting it back up.  Also, make sure
  ;; the offline registers are cleared.
  (write offline-bus-out 0)
  (setq *timg* *bparo*)
  (write offline-tag-out *timg*)
  (diag-resucode)
  (cond (random-data
           (dotimes (j *wrlen*)
                 (setf (aref (mbu:user-multibus-dma-buffer.array *xpnt*) j) (random 256))))
        ('else
         (dotimes (j (* 2 *wrlen*))
           (setf (aref (mbu:user-multibus-dma-buffer.array *xpnt*) j) (mod j 256)))))
  (when (or (not *offline-data-array*) (not (= (length *offline-data-array*)
                                               (* 2 *wrlen*))))
    (setq *offline-data-array* (make-array (* 2 *wrlen*) :type 'art-string)))
  (setq *xlen* *wrlen*)
  ;; initialize and load the valid address ram table
  (fill *vtbl1* :invalid)
  (setf (elt *vtbl1* *vsbch*) :valid) ;;  set subchannel 1 valid
  (initvaddr *vtbl1*)
  (z256) ;; zero 256 RAM
  (message "init256")
  (let ((*io-reg-trace* (if trace :symbolic)))
    (init256 *cmt* *vlcm* :set)                 ; init valid commands
    (init256 *bbt* *BBR* :set)                  ; init burst/byte
    (init256 *nrt* *NTRD* :clear)               ; init not ready
    )
  (dotimes (config #x10)
    (kcnfg config)
    (when *verbose*
      (de minit))
    (diag-chread config trace)))


(defun z256 ()
  (write comsta 0)
  (dotimes (count 256)
    (write chadd count)
    (WRITE ADDCOM 0)
    (WRITED mulcom c256 1 go 1) ;; read 256 ram
    (wait 100)))


(defun kcnfg (config)
  ;; set up config data
  (modify minit
          sngl (ldb (byte 1 0) config)
          odd (ldb (byte 1 1) config)
          swp (ldb (byte 1 2) config)
          hsp (ldb (byte 1 3) config)))





(defun diag-resUcode ()
  ;; start the microcode from zero
  (write mc 0)                                  ; reset the 8x305
  (write rtnreg 0)                              ; clear REDY bit to insure handshake
  (modify mc 8xrun 1)                           ;  start it back up
  (wait 5000))



(defun diag-chRead (config trace)
  ;; shove 'er into idle
  (let ((*io-reg-trace* (if trace :symbolic)))
    (writed mulcom go 1)
    ;; select valid subchannel, look for selection
    (or (eq :select (chsel *vsbch* :odd))
        (device-error "No response from selection of valid subchannel"))
    ;; continue initial selection
    (contsel *read* :odd)
    ;; 8600 is selected, command is received
    (if (not (= *read* (read comsta)))
        (device-error "8600 received wrong command"))
    (diag-sdma *xpnt* *xlen*)
    (modify mulcom go 1 read 1 endg 1)
    (if trace (message "dataxfr"))
    (dataxfr config))
  (check-data config)
  ;; do an ending
  (wtag *stai* :up "Error occured in ending presentation")
  (dtag (logior *selo* *hldo*))
  (rtag *srvo*)
  (wtag *stai* :down "Error occured in ending presentation")
  (dtag *srvo*))


(defun chSel (cadr parity)
  ;; try to select a subchannel (address = cadr) and return selection
  ;; status to the calling routine
  ;; do initial selection
  (write offline-bus-out cadr)
  (setq *timg* (ecase parity
                 (:odd 0)
                 (:even 1)))
  (write offline-tag-out *timg*)
  (setq *timg* (logior *timg* *adro*))
  (write offline-tag-out *timg*)
  (setq *timg* (logior *timg* *hldo* *selo*))
  (write offline-tag-out *timg*)
  ;; if seli, not valid subchannel
  (setq *buserr* (read offline-bus-in))
  (setq *tagerr* (read offline-tag-in))
  (cond ((boolean (read offline-tag-in seli))
         :nosel)
        ((boolean (read offline-tag-in opli))
         :select)
        ((boolean (read offline-tag-in stai))
         :busy)
        ('else
         (device-error "unrecognized sequence"))))



(defun timg-trace ()
  (format t "~&TIMG = ")
  (MBU:MULTIBUS-REGISTER-PRINT-1 *timg* 'kiwidev 'offline-tag-out))

(defun contSel (xcmd eoflg)
  ;; complete selection and present command
  (dtag *ADRO*)                                 ; drop ADRO
  (wtag *adri* :up)                             ; wait for ADDRI
  (write offline-bus-out xcmd)
;  (timg-trace)
  (ecase eoflg
    (:odd
     (setq *timg* (logand *timg* (lognot #x01))))
    (:even
     (setq *timg* (logior *timg* #x01))))
  ;(timg-trace)
  (write offline-tag-out *timg*)        ; adjust parity bit
  (rtag *cmdo*) ; raise CMDO
  (wtag *adri* :down)
  (dtag *CMDO*) ; drop CMDO
  (wtag *stai* :up)
  (when (not (zerop (read offline-bus-in)))
    (device-error "Expected zero status on initial selection"))
  (rtag *SRVO*) ; raise SRVO
  (wtag *stai* :down)
  (dtag *SRVO*) ; drop SRVO
  ;; look at the CRCV bit response
  (wait 2000)
  (when (not (boolean (read rtnreg crcv)))
    (device-error "Command not received by 8600")))

;;

(defun rtag (x)
  (setq *timg* (logior *timg* x))
  (write offline-tag-out *timg*))

(defun dtag (x)
  (setq *timg* (logand *timg* (lognot x)))
  (write offline-tag-out *timg*))



(defun wtag (bit pol &optional message)
  (ecase pol
    (:up
     (or (= bit (logand bit (read offline-tag-in)))
         (process-wait-with-timeout "wtag up"
                                    (* 60 3)
                                    #'(lambda (bit)
                                        (= bit (logand bit (read offline-tag-in))))
                                    bit))
     (or (= bit (logand bit (read offline-tag-in)))
         (device-error (or message "tag bit failed to rise"))))
    (:down
     ;; wait for bit to drop
     (or (zerop (logand bit (read offline-tag-in)))
         (process-wait-with-timeout "wtag down"
                                    (* 60 3)
                                    #'(lambda (bit)
                                        (zerop (logand bit (read offline-tag-in))))
                                    bit))
     (or (zerop (logand bit (read offline-tag-in)))
         (device-error (or message "tag bit failed to fall"))))))


(defun dataXfr (config &aux wcount i)
  (setq wcount *xlen*)
  (setq i 0)
  (do ()
      ((not (> wcount 0)))
    (wtag *srvi* :up) ;; wait for SRVI up
    (setf (aref *offline-data-array* i) (read offline-bus-in))
    (incf i)
    (rtag *srvo*)
    (wtag *srvi* :down)
    (dtag *srvo*)
    (when (and (= wcount 1) (not (zerop (logand #x02 config))))
      ;; odd byte transfer
      (return nil))
    (cond ((not (zerop (logand #x08 config)))
           ;; high speed transfer
           (wtag *dati* :up)
           (setf (aref *offline-data-array* i) (read offline-bus-in))
           (incf i)
           (rtag *dato*)
           (wtag *dati* :down)
           (dtag *dato*))
          ('else
           (wtag *srvi* :up)
           (setf (aref *offline-data-array* i) (read offline-bus-in))
           (incf i)
           (rtag *srvo*)
           (wtag *srvi* :down)
           (dtag *srvo*)))
    (decf wcount))
  (setq *offline-data-size* i)
  )


(defun diag-SDMA (buff len)
  (write bfszl (ldb (byte 8 0) len))
  (write bfszu (ldb (byte 8 8) len))
  (let ((addr (USER-MULTIBUS-DMA-BUFFER.MULTIBUS-ADDRESS BUFF)))
    (write bfadl (ldb (byte 8 0) addr))
    (write bfadm (ldb (byte 8 8) addr))
    (write bfadu (ldb (byte 8 16) addr))))


(defun check-data (config)
  (if (not (= (* 2 *xlen*)
              (+ (if (zerop (logand config #x02)) 0 1)
                 *offline-data-size*)))
      (device-error "lengths dont match"))
  (cond ((not (zerop (logand config #x02)))
         (if (not (oddp *offline-data-size*))
             (device-error "expected odd transfer")))
        ('else
         (if (not (evenp *offline-data-size*))
             (device-error "expected even transfer"))))
  (let ((swap (= (logand config #x04) #x04))
        (from (mbu:user-multibus-dma-buffer.array *xpnt*))
        (to *offline-data-array*)
        (n *offline-data-size*))
    (format t "~&Swap = ~S Length = ~S~%" swap n)
    (format t "From = ~S~%" (substring from 0 26))
    (format t "TO   = ~S~%" (substring to 0 26))
    (format t "Does this look ok?~%")
    (dotimes (j n)
      (or (= (aref from j)
             (aref to (if swap
                          (+ j (if (zerop (mod j 2))  1 -1))
                        j)))
          (device-error "data mismatch on byte ~D" j)))))
