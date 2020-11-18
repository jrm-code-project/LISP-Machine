#|  -*- Mode:LISP; Package:MBU; Fonts:(CPTFONTB); Base:10 -*-

Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
licensing and release information.

Device handler for Emulex model MB01, multibus host adaptor
for the SCSI bus. 6/12/85 08:59:57 -George Carrette

Hardware Configuration:
 * Note: The manual is confusing on this point but experience makes it
   clear that a SWITCH ON = 0, SWITCH OFF = 1.

 * Multibus terminated in device, RESISTOR PACKS U18, U23 are in place,
   Diode CR1 and jumper are out. This has no effect on programming
   and may change.

 * Switch SW1

   1  0 ---- SCSI device = 0
   2  0  !
   3  0 -/
   4  1 ---- Serial Bus Arbitration
   5  1 ---- Interrupt #5
   6  0  !
   7  1 -/
   8  0 ---- 8 bit I/O address path
   9  x ---- software programmable 1
  10  x ---- software programmable 2

 * Switch SW2, 1 through 10 are I/O base register address bits 6 through 15.
   Base register may therefore be assigned on 64 byte boundaries.

   1  0 -- Base address = 0.
   2  0 -/
   3  x
   4  x
   5  x
   6  x
   7  x
   8  x
   9  x
  10  x

|#




(eval-when (eval compile load)

(defprop mb01-scsi
         (((current-address-register car)        #x00 :read)
          ((base-address-register bar)           #x00 :write)
          ((current-word-count-register cwcr)    #x01 :read)
          ((base-word-count-register bwcr)       #x01 :write)
          ((command-register cmdr)               #x08 :write
           (sda sdr wrs 0 tms cle x 0))
          ((status-register str)                 #x08 :read
           (0 0 0 0 drq 0 0 0 ctc))
          ((request-register rqr)                #x09 :write
           (x x x x x sr 0 0))
          ((mask-register mskr)                  #x0A :write
           (x x x x x csm 0 0))
          ((mode-register mode)                  #x0B :write
           (mds mds ids ape trt trt 0 0))
          ((clear-byte-pointer cbp)              #x0C :write)
          ((temporary-register temp)             #x0D :read)
          ((master-clear mscl)                   #x0D :write)
          ((clear-mask-register cmsk)            #x0E :write)
          ((mask-register mask)                  #x0F :write
           (x x x x x x x SMB))
          ((dma-address-register dar)            #x23 :write)
          ((data-register datr)                  #x10 t)
          ((other-command-register comd)         #x11 t
           (dmo sbt 0 code code code code code)
           ((code chip-reset #b00000)
            (code disconnect #b00001)
            (code diagnostic #b01011)))
          ((control-register ctrlr)              #x12 t
           (x x x x x pen ren sen))
          ((destination-id-register didr)        #x13 t
           (0 0 0 0 0 0 0 dest dest dest))
          ((auxiliary-status-register asr)       #x14 :read
           (drf per msg cd io ps tcz x))
          ((id-register idr)                     #x15 :read
           (0 0 0 0 0 0 dev dev dev))
          ((interrupt-register ir)               #x16 :read
           (x ivc x res se dsc bus fnc))
          ((source-id-register sidr)             #x17 :read
           (idv x x x x src src src))
          ((diagnostic-status-register dsr)      #x19 :read
           (sdc x dcs dcs dcs stsc stsc stsc)
           ((stsc sucessful    #b000)
            (stsc branch-fail  #b001)
            (stsc drf-fail     #b010)
            (stsc init-fail    #b011)
            (stsc comd-fail    #b100)
            (stsc d-f-fail     #b101)
            (stsc turn-fail    #b110)
            (dcs dmisc-init    #b001)
            (dcs dmisc-final   #b010)
            (dcs good-parity   #b011)
            (dcs bad-parity    #b100)))
          ((scsi-bus-reset-register sbr)         #x20 :write)
          ((release-scsi-bus-reset-register rsr) #x27 :write)
          ((vector-0-register)                   #x21 :write)
          ((vector-1-register)                   #x22 :write)
          ((general-status-register gsr)         #x24 :read
           (posw posw ics ics ics spv sbrf dto)))
         multibus-io-registers)

(defprop mb01-scsi *mb01-scsi-io-registers* multibus-io-register-array)

)

(defvar *mb01-scsi-io-registers* (make-multibus-io-registers 'mb01-scsi 0 64))

(defmacro mb01-read (register &rest bits)
  `(multibus-io-register-read mb01-scsi ,register ,@bits))


(defmacro mb01-write (register &rest bits)
  `(multibus-io-register-write mb01-scsi ,register ,@bits))



(defun init-mb01 (&optional trace)
  (macrolet ((reset (register)
                    `(progn (if trace (format t "~%RESET ~A" ',register))
                            (mb01-write ,register 0))))
    (reset comd)
    (reset ctrlr)
    (reset mode)
    (reset bar)
    (reset bwcr)
    (reset mskr)
    (reset mask)
    (when trace
      (dolist (r (get 'mb01-scsi 'multibus-io-registers))
        (when (memq (caddr r) '(t :read))
          (terpri)
          (multibus-io-register-print 'mb01-scsi (or (cadar r) (caar r))))))
    (if trace
        (format t "~%Turn Around Diagnostic"))
    (data-turn-around-diagnostic trace)))

(defun mb01-print (reg &optional (stream t))
  (and stream (terpri))
  (multibus-io-register-print 'mb01-scsi reg stream))


(defun data-turn-around-diagnostic (&optional verbose (times 3) &aux (wins 0) losses)
  (dotimes (j times)
    (let ((written-data (random 256))
          (read-data)
          (vs (if verbose t nil))
          (result))
      ;; first do self diagnostic
      (mb01-write comd dmo 0 sbt 0 'code chip-reset)
      (setq result (mb01-print 'dsr vs))
      (or (= 1 (cadr (assq 'sdc result)))
          (push 'self-diag-failed-to-complete losses))
      (or (eq 'sucessful (cadr (assq 'stsc result)))
          (push (cadr (assq 'stsc result)) losses))
      (reset-multibus-interrupt-5)
      (mb01-write COMD
                  DMO 0
                  SBT 0
                  'CODE DIAGNOSTIC)
      (setq result (mb01-print 'asr vs))
      (mb01-write datr written-data)
      (process-wait-multibus-interrupt-5 "MBSCSI")
      (setq result (mb01-print 'ir vs))
      (setq result (mb01-print 'asr vs))
      (setq result (mb01-print 'dsr vs))
      (cond ((not (= 1 (cadr (assq 'sdc result))))
             (push 'self-diag-failed-to-complete losses))
            ((eq 'good-parity (cadr (assq 'dcs result)))
             (incf wins))
            ('else
             (push (cadr (assq 'dcs result)) losses)))
      (setq read-data (mb01-read datr))
      (when verbose
        (format t "~%Wrote #x~16,2,'0R read #x~16,2,'0R" written-data read-data))))
  (when (and verbose losses)
    (format t "~%Diagnostics failed because: ~{~A~^, ~}"
            (union () losses)))
  (= wins times))
