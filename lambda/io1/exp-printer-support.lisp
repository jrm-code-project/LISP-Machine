;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL; Fonts:(CPTFONT) -*-
;;
;; Copyright LISP Machine, Inc. 1986
;;   See filename "Copyright" for
;; licensing and release information.


;;; Explorer Parallel Device Stream
;;;
;;;
;;; The parallel port has two registers:
;;;   Parallel Control Register (PCR)
;;;   Parallel Data Register (PDR)

(DefConstant PDR-Addr #xF10000)
(DefConstant PCR-Addr #xF10004)

(DefConstant Parallel-Strobe-Active #x05)
(DefConstant Parallel-Strobe-Inactive #x07)
(DefConstant Parallel-Reset #x02)

(DefConstant Parallel-Port-Not-Busy-Mask #b1100)

(Defun Parallel-Read-Status ()
  (%nubus-read-8 TV:TV-QUAD-SLOT PCR-Addr))

(Defun Parallel-Write-Control (value)
  (%nubus-write-8 TV:TV-QUAD-SLOT PCR-Addr value))

(Defun Parallel-Write-Data (value)
  (%nubus-write-8 TV:TV-QUAD-SLOT PDR-Addr value))


;;; Port not busy or an error has occurred.
(Defun Parallel-Port-Not-Busy ()
  (Let ((control-bits (Parallel-Read-Status)))
    (or (not (ldb-test #o0001 control-bits))
        (ldb-test #o0101 control-bits)
        (not (ldb-test #o0201 control-bits))
;;;     (ldb-test #o0301 control-bits)       ; ignore since it seems to be bogus
        )))

(Defun Parallel-Port-Help (stream ignore ignore)
  (format stream "~&During printing the problem noted above occurred.~
                  ~&Resolve the problem and press any key to continue."))

(Defun Parallel-Port-Exception (exception-print-string)
  (tv:notify nil "Printer Exception: ~s, go to Tiger Operator Window."
             exception-print-string)
  (fquery '(:type :tyi
            :choices (:any)
            :timeout 3600.
            :help-function parallel-port-help)
          exception-print-string)
  )

(Defun Parallel-Port-Check-Status ()
  (Let ((control-bits (Parallel-Read-Status)))
    (Cond ((ldb-test #o0101 control-bits)
           (Parallel-Port-Exception "Printer out of paper "))
          ((Not (ldb-test #o0201 control-bits))
           (Parallel-Port-Exception "Printer offline "))
;;;       ((Not (ldb-test #o0301 control-bits))     ; ignore since it seems to be bogus
;;;        (Parallel-Port-Exception "Printer fault "))
           ))
  )



(DefParameter Parallel-Port-Buffer-Size 512.)


(DefFlavor exp-parallel-stream-mixin
        ((buffer (make-array Parallel-Port-Buffer-Size :type :art-string)))
        (si:buffered-output-stream)
  (:required-flavors si:output-stream si:character-stream
                     si:basic-buffered-output-stream)
  (:initable-instance-variables)
  (:settable-instance-variables))


(DefMethod (exp-parallel-stream-mixin :new-output-buffer) ()
  (Unless (<= Parallel-Port-Buffer-Size (Array-Length buffer))
    (setq buffer (make-array Parallel-Port-Buffer-Size :type :art-string)))
  (values buffer 0 Parallel-Port-Buffer-Size))


(DefMethod (exp-parallel-stream-mixin :send-output-buffer) (output-buffer new-index)
  (dotimes (idx new-index)
    (do ()
        ((= (ldb #o0004 (Parallel-Read-Status))
            Parallel-Port-Not-Busy-Mask))
      (Process-Wait "Parallel Out" #'Parallel-Port-Not-Busy)
      (Parallel-Port-Check-Status))
    (Parallel-Write-Data (aref output-buffer idx))
    (Parallel-Write-Control Parallel-Strobe-Active)
    (Parallel-Write-Control Parallel-Strobe-Inactive))
  )


(DefMethod (exp-parallel-stream-mixin :discard-output-buffer) (ignore)
  nil)



(DefFlavor exp-parallel-stream () (exp-parallel-stream-mixin
                                   si:output-stream
                                   si:character-stream
                                   si:buffered-output-stream)
  (:documentation :combination "Explorer Parallel Output Stream, no character-set translation"))

(Compile-Flavor-Methods exp-parallel-stream)




;;; Explorer Serial Device Stream
;;;
;;;
;;; The serial port has many (il)logical registers,
;;; but only two physical registers per port:
;;;   Serial Control/Status Register (SCSR)
;;;   Serial Data Register (SDR)

(DefConstant SCSR-A-Addr #xFB0004)
(DefConstant SDR-A-Addr  #xFB000C)
(DefConstant SCSR-B-Addr #xFB0000)
(DefConstant SDR-B-Addr  #xFB0010)

;;; Functions for  referencing (il)logical registers

(Defun Write-Serial-Port-A-Register (reg value)
  (If (zerop reg)
      (%nubus-write-8 tv:tv-quad-slot SCSR-A-Addr value)
    (%nubus-write-8 tv:tv-quad-slot SCSR-A-Addr reg)
    (%nubus-write-8 tv:tv-quad-slot SCSR-A-Addr value)))

(Defun Read-Serial-Port-A-Register (reg)
  (If (zerop reg)
      (%nubus-read-8 tv:tv-quad-slot SCSR-A-Addr)
    (%nubus-write-8 tv:tv-quad-slot SCSR-A-Addr reg)
    (%nubus-read-8 tv:tv-quad-slot SCSR-A-Addr)))

(Defun Write-Serial-Port-B-Register (reg value)
  (If (zerop reg)
      (%nubus-write-8 tv:tv-quad-slot SCSR-B-Addr value)
    (%nubus-write-8 tv:tv-quad-slot SCSR-B-Addr reg)
    (%nubus-write-8 tv:tv-quad-slot SCSR-B-Addr value)))

(Defun Write-Serial-Port-AB-Register (reg value)
  (Write-Serial-Port-A-Register reg value)
  (Unless (or (= reg 2) (= reg 9)) ; unless shared
    (Write-Serial-Port-B-Register reg value)))

(Defun Serial-Port-Error-Bits ()
  (ldb #o0403 (Read-Serial-Port-A-Register 1)))

(Defun Serial-Port-Not-Busy ()     ; port not busy or an error has occurred
  (ldb-test #o0201 (%Nubus-Read-8 tv:tv-quad-slot SCSR-A-Addr)))

(Defun Serial-Port-Send-Char (char)
  (%nubus-write-8 TV:TV-QUAD-SLOT SDR-A-Addr char))

(Defun Serial-Port-Char-Ready ()
  (ldb-test #o0001 (%Nubus-Read-8 tv:tv-quad-slot SCSR-A-Addr)))

(Defun Serial-Port-Receive-Char ()
  (Let ((error-bits (Serial-Port-Error-Bits)))
    (If (Not (= 0 error-bits))
        (Ferror nil "Error bits on receive: ~16r" error-bits)
      (%nubus-read-8 tv:tv-quad-slot SDR-A-addr)))
  )



(DefParameter Serial-Port-Buffer-Size 512.)

(DefFlavor exp-serial-stream-mixin
        ((buffer (make-array Serial-Port-Buffer-Size :type :art-string)))
        (si:buffered-stream)
  (:required-flavors si:output-stream si:character-stream
                     si:basic-buffered-output-stream)
  (:initable-instance-variables)
  (:settable-instance-variables))

(DefMethod (exp-serial-stream-mixin :after :init) (ignore)
  (Write-Serial-Port-AB-Register  9. #xC0)         ; reset the Z8530
  (Write-Serial-Port-AB-Register  4. #x04)         ; async 1 stop bit
  (Write-Serial-Port-AB-Register  1. #x12)         ; enable internal interrupts
  (Write-Serial-Port-AB-Register 11. #x50)         ; enable internal baud rate clock
  (Write-Serial-Port-AB-Register 12. #x7E)         ; set baud rate to 9600 (low byte)
  (Write-Serial-Port-AB-Register 13. #x00)         ; set baud rate to 9600 (high byte)
  (Write-Serial-Port-AB-Register 14. #x63)         ; disable sync comm and enable baud rate generator
  (Write-Serial-Port-AB-Register 15. #x00)         ; disable external interrupts
  (Write-Serial-Port-AB-Register  3. #xC1)         ; enable receiver at 8 bits
  (Write-Serial-Port-AB-Register  5. #xEA)         ; enable modem control bits
  )


(DefMethod (exp-serial-stream-mixin :new-output-buffer) ()
  (Unless (<= Serial-Port-Buffer-Size (Array-Length buffer))
    (setq buffer (make-array Serial-Port-Buffer-Size :type :art-string)))
  (values buffer 0 Serial-Port-Buffer-Size))

(DefMethod (exp-serial-stream-mixin :send-output-buffer) (output-buffer new-index)
  (dotimes (idx new-index)
    (Process-Wait "Serial Out" #'Serial-Port-Not-Busy)
    (Serial-Port-Send-Char (aref output-buffer idx)))
  )

(DefMethod (exp-serial-stream-mixin :discard-output-buffer) (ignore)
  nil)

(DefMethod (exp-serial-stream-mixin :setup-next-input-buffer) (&optional no-hang-p)
  (Unless (<= Serial-Port-Buffer-Size (Array-Length stream-input-buffer))
    (setq stream-input-buffer (make-array Serial-Port-Buffer-Size :type :art-string)))
  (setq stream-input-index 0)
  (setq stream-input-limit 0)
  (funcall-self :next-input-buffer no-hang-p)
  )

(DefMethod (exp-serial-stream-mixin :next-input-buffer) (&optional no-hang-p)
  (DoTimes (idx (array-length stream-input-buffer))
    (Cond ((Serial-Port-Char-Ready)
           (aset (Serial-Port-Receive-Char) stream-input-buffer stream-input-limit)
           (incf stream-input-limit))
          (no-hang-p (return))
          (t
           (Process-Wait "Serial In" #'Serial-Port-Char-Ready)
           (aset (Serial-Port-Receive-Char) stream-input-buffer stream-input-limit)
           (incf stream-input-limit))))
  (Values stream-input-buffer stream-input-index stream-input-limit)
  )

(DefMethod (exp-serial-stream-mixin :discard-current-input-buffer) ()
  nil)

(DefMethod (exp-serial-stream-mixin :discard-input-buffer) ()
  NIL)

(DefFlavor exp-serial-stream () (exp-serial-stream-mixin
                                 si:bidirectional-stream
                                 si:character-stream
                                 si:unbuffered-line-input-stream
                                 si:buffered-output-stream)
  (:documentation :combination
                  "Explorer Serial Input/Output Stream, no character-set translation"))



(DefMethod (exp-serial-stream :tyi-no-hang) (&optional ignore)
  (Send self :tyi t))


(DefMethod (exp-serial-stream :tyi) (&optional no-hang-p (whostate "Serial In"))
  (Cond ((Serial-Port-Char-Ready)
         (Serial-Port-Receive-Char))
        (no-hang-p nil)
        (t
         (Process-Wait whostate #'Serial-Port-Char-Ready)
         (Serial-Port-Receive-Char)))
  )


(DefFlavor exp-serial-xon-xoff-stream () (exp-serial-stream)
  (:documentation :combination
                  "Explorer Serial Input/Output Stream with XON/XOFF, no character-set translation"))

;;; +++ For some reason the TI-855 printer XON/XOFF characters are many times received
;;; +++ garbled.  If you just pretend to see the character you expected it seems to work.
;;; +++ The *serial-error-list* records these occurances.
(DefVar *serial-error-list* nil)

(DefConstant XOFF #x13)
(DefConstant XON  #x11)


(Defun Serial-Port-Find-XOFF (stream)
  (Let ((char (send stream :tyi-no-hang)))
    (Unless (Null char)
      (Unless (= char XOFF)
        (push (format nil "Serial Stream: XOFF (#x13) expected, received ~16r." char)
              *serial-error-list*))
      t)
    )
  )

(Defun Serial-Port-Find-XON (stream)
  (Let ((char (send stream :tyi nil "XON")))
    (Unless (= char XON)
      (push (format nil "Serial Stream: XON (#x11) expected, received ~16r." char)
            *serial-error-list*)))
  )


(DefMethod (exp-serial-xon-xoff-stream :send-output-buffer) (output-buffer new-index)
  (DoTimes (idx new-index)
    (Process-Wait "Serial Out" #'Serial-Port-Not-Busy)
    (When (Serial-Port-Find-XOFF self)
      (Serial-Port-Find-XON self))
    (Serial-Port-Send-Char (aref output-buffer idx)))
  )

(Compile-Flavor-Methods exp-serial-xon-xoff-stream)
