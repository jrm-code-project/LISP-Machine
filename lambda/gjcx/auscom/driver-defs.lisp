;;; -*- Mode:LISP; Package:AUSCOM; Base:10; Readtable:CL -*-

;;; 4-Jun-86 21:34:44 DRIVER FOR THE AUSCOM MODEL 8600
;;;                   MULTIBUS CHANNEL INTERFACE
;;; - GEORGE CARRETTE


(defvar *kiwi1-io-base* 0)

(define-multibus-io-registers kiwidev *kiwi1*
  ((RA_b[0] FLAG0)  #x00 t
   (mulcom addcom rtnreg wdcnt bfszl bfszu flag1 flag0))

  ((RA_b[1] FLAG1)  #x01 t
   (bfadl bfadm bfadu mini chadd comsta err0 err1))

  ((RA_b[2] MULCOM)  #x02 t
   (RQRD X GO CSTA C256 ENDG READ WRIT))
  ((RA_b[3] ADDCOM)  #x03 t
   (X X OFLN ONLN X  A256 WRIN WRVL))

  ((RA_b[4] RTNREG)  #x04 t
   (REDY ONL ERRB X CRCV ENDP RDCP WRCP))
  ((RA_b[5] WDCNT)  #x05 t)
  ((RA_b[6] BFSZL)  #x06 t)
  ((RA_b[7] BFSZU)  #x07 t)
  ((RA_b[8] BFADL)  #x08 t)

  ((RA_b[9] BFADM)  #x09 t)
  ((RA_b[10] BFADU) #x0A t)
  ((RA_b[11] MINIT) #x0B t
   (X STK ODD DEOI SNGL HSP SWP DIIM))
  ((RA_b[12] CHADD) #x0C t)
  ((RA_b[13] COMSTA) #x0D t)
  ((RA_b[14] ERR0) #x0E t
   (HALT CHRE SERE BPER DVNR ZFWA NEXM OFFL))
  ((RA_b[15] ERR1) #x0F t
   (INCV CPAR STKS EFNX ODBY STRD STWR CTBD))

  ((MC) #x10 t
   (8xrUn DWNLD CSRWE step x x x x))
  ((display)        #x11 :write)
  ((OFFLINE-bus-out) #x12 :write)
  ((OFFLINE-bus-in) #x12 :read)
  ((OFFLINE-tag-out) #x14 :write
   (ADRO HLDO SELO CMDO SRVO DATO SUPO BPAR))
  ((OFFLINE-tag-in) #x14 :read
   (ADRI OPLI SELI STAI SRVI DATI REQI BPAR))
  ((download-register-lsb) #x16 t)
  ((download-register-msb) #x17 t)
  ((pic1) #x18 t)
  ((pic2) #x1A t))

(defprop INVC "Invalid command" err1)
(defprop CPAR "Command Parity Error" err1)
(defprop STKS "Stacked status" err1)
(defprop ODBY "Odd byte write" err1)
(defprop STRD "Stop on read" err1)
(defprop stwr "Stop on write" err1)
(defprop ctbd "Command to busy device" err1)
(defprop halt "Halt I/O" err0)
(defprop chre "Channel Reset" err0)
(defprop sere "Selective Reset" err0)
(defprop bper "Bus out parity error" err0)
(defprop dnvr "Device not ready" err0)
(defprop zfwa "zero function while active" err0)
(defprop nexm "Non-existant memory" err0)
(defprop offl "8600 switched offline" err0)

(defvar *kiwi1*  (make-multibus-io-registers 'kiwidev *kiwi1-io-base* 32))

(DEFMACRO DE (&OPTIONAL REGISTER)
  `(*DE ',REGISTER))

(DEFUN *DE (REGISTER &OPTIONAL (STREAM STANDARD-OUTPUT))
  (COND ((NULL REGISTER)
         (DESCRIBE *KIWI1*))
        ('ELSE
         (multibus-register-print 'KIWIDEV REGISTER STREAM))))


(DEFMACRO READ (REGISTER &REST BITS)
  `(MULTIBUS-REGISTER-READ KIWIDEV ,REGISTER ,@BITS))

(DEFMACRO WRITE (REGISTER &REST BITS)
  `(MULTIBUS-REGISTER-WRITE KIWIDEV ,REGISTER ,@BITS))

(DEFMACRO MODIFY (REGISTER &REST BITS)
  `(MULTIBUS-REGISTER-MODIFY KIWIDEV ,REGISTER ,@BITS))

(DEFMACRO WRITED (REGISTER &REST BITS)
  `(MULTIBUS-REGISTER-WRITE-DEFAULT KIWIDEV ,REGISTER 0 ,@BITS))

(DEFCONST *DEFSBC*  #xb3 "DEFAULT SUBCHANNEL")
(DEFCONST *TSTSBC*  #x01)
(DEFCONST *DEFWCNT* #x10 "DEFAULT WORD COUNT")

(DEFVAR *BRSTFLG* T)
(DEFVAR *INTLEV* NIL)
(DEFVAR *SUBCH* *DEFSBC*)
(DEFVAR *WCOUNT* *DEFWCNT*)

(DEFVAR *E38CMT* (LIST #o1 #o2 #o4 #o7 #o14 #o33
                       #o303 #o313 #o323 #o213
                       #o013 #o023 #o043 #o053
                       #o063 #o073 #o123 #o143
                       #o153 #o163 #o173 #o003
                       #o027 #o047 #o057 #o067
                       #o077 #o017 #o037 #o263
                       #o223 #o273 #o243 #o253
                       #o373 #o103 #o133
                       #o203 #o233 #o333 #o343
                       #o353 #o373 #o363))


(DEFVAR *E38ICT* (LIST #o303 #o313 #o323 #o213 #o013 #o023 #o043 #o053
                       #o063 #o073 #o123 #o143 #o153 #o163 #o173 #o003
                       #o017 #o027 #o037 #o047 #o057 #o067 #o077 #o223
                       #o243 #o253 #o263 #o273 #o373 #o103 #o133 #o007
                       #o203 #o233 #o333 #o343 #o353 #o373 #o363))

(DEFVAR *CSBUFF* NIL)
(DEFVAR *BUFFSIZE* NIL)


(DEFVAR *E38BBT* (LIST *DEFSBC*))

(DEFVAR *E38NRT* (LIST *DEFSBC*))

(DEFVAR *VERBOSE* T)

(DEFVAR *AS_DE* T)


(DEFVAR *hsp* NIL)
(DEFVAR *sngl* T)
(DEFVAR *swp* NIL)

(DEFVAR *VTABLE* (MAKE-ARRAY 256))
(DEFVAR *CHSTTABLE* (MAKE-ARRAY 256))

;; STATUS BYTE IS
;; ATTENTION STATUS_MOD CONTROL_UNIT_END BUSY CHANNEL_END DEVICE_END UNIT_CHECK UNIT_EXCEPTION


(defconst *ce* 8)
(defconst *de* 4)

(DEFUN BOOLEAN (X)
  (COND ((NUMBERP X)
         (IF (ZEROP X) NIL T))
        ('ELSE
         (IF X 1 0))))



(DEFVAR *OPTIONS* '((*VERBOSE* "print messages (verbose)" :boolean)
                    (*AS_DE* "present DE on power up (set async device end flag)" :boolean)
                    (*BRSTFLG* "burst mode (i.e. not byte mode)" :boolean)
                    (*WCOUNT* "word count" :number)
                    (*subch*  "subchannel address" :number)
                    (*hsp*  "high speed" :boolean)
                    (*sngl* "single DMA transfers (vs multiple)" :boolean)
                    (*swp*  "swap data bytes (vs no swap)" :boolean)))






;;; masks used by the ram setups.

(DEFCONST *VLCM* #x80)
(DEFCONST *IMMC* #x40)
(DEFCONST *BBR*  #x20)
(DEFCONST *STS*  #x10)
(DEFCONST *SPAD* #x08)
(DEFCONST *SPCM* #x04)
(DEFCONST *NTRD* #x02)


;; dma related variables

(DEFCONST *WRLEN* 512)
(DEFVAR *XLEN* NIL)
(DEFVAR *XODD* NIL)
(DEFVAR *XPNT* NIL)

(DEFCONST *SBUFF* NIL)
(DEFCONST *SLEN* 12)
(DEFCONST *SODD* NIL)

;; FREE CONSTANTS THAT COULD BE PART OF THE COMSTA REGISTER SPEC

(DEFCONST *WRITE*    1)
(DEFCONST *READ*     2)
(DEFCONST *SENSE*    4)
(DEFCONST *DAER*  #o14)
(DEFCONST *NOP*   #o33)
