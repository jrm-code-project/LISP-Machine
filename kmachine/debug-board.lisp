;;;-*- Mode:LISP; Package:LAMBDA; Base:16; Readtable:ZL -*-


(export '(debug-write-word debug-write-byte
          debug-read-word debug-read-byte))

(defun find-a-debug-board ()
  (let ((this-lambda-processor-slot (si::%processor-conf-slot-number si::*my-proc-conf*))
        (first-debug-board          (si::%system-configuration-lmi-debug-board-owner si::*sys-conf*))
        (second-debug-board         (si::%system-configuration-second-lmi-debug-board-owner si::*sys-conf*)))
    (let ((first-board-exists?  (not (= (ldb (byte 8. 0.) first-debug-board)  (ldb (byte 8. 0.) -1.))))
          (second-board-exists? (not (= (ldb (byte 8. 0.) second-debug-board) (ldb (byte 8. 0.) -1.)))))
      (if (not first-board-exists?)
          (ferror nil "I can't find a debug board, get one and run CONFIG.")
          ;; We have at least one board, do we own it?
          (cond ((= this-lambda-processor-slot first-debug-board)
                 (si::%system-configuration-lmi-debug-board-slot si::*sys-conf*))
                ((= this-lambda-processor-slot second-debug-board)
                 (si::%system-configuration-second-lmi-debug-board-slot si::*sys-conf*))
                ;; I don't own either, offer to take one.
                (t (cerror "Take one." "I don't own a debug board, but there is at least one here.~
                                        You can take it.")
                   (if second-board-exists?
                       (fquery
                         (list ':choices
                               `(((,(si::%system-configuration-lmi-debug-board-slot
                                      si::*sys-conf*) "First board")  #/1)
                                 (( ,(si::%system-configuration-second-lmi-debug-board-slot
                                       si::*sys-conf*) "Second board") #/2))
                               ':fresh-line 't)
                         "Take which board?")
                 (si::%system-configuration-lmi-debug-board-slot si::*sys-conf*)
                     )))))))

(defvar debug-slot nil)
(defvar debug-addr nil)
(defvar debug-test-reg-addr nil)

(defun assure-debug-board ()
  (when (null debug-slot)
    (let ((slot (find-a-debug-board)))
      (setq debug-slot (logior #xF0 slot))
      (setq debug-addr (ash debug-slot 24.))
      (setq debug-test-reg-addr (logior #xF0FFF7C4 debug-addr)))))

(defconst debug-slot #xfa)
(defconst debug-addr (ash debug-slot 24.))
(defconst debug-test-reg-addr (logior #xf0fff7c4 debug-addr))

(defun test-debug-board (slot)
  (setq debug-slot (logior (logand #x0f slot) #xf0))
  (setq debug-addr (ash debug-slot 24.))
  (setq debug-test-reg-addr (logior #xf0fff7c4 debug-addr))
  (format t "~%LMI Debug board diagnostics starting.~%")
  (format t "~%Basic register R/W test -----------~%")
  (debug-rw-test)
  (format t "~%Serial loopback test (no DMA) -----~%")
  (debug-serial-loop-test)
  (format t "~%Serial loopback test (with Mastership) ----~%")
  (debug-iloop-test)
  (format t "~%Nubus analyzer RAM test ---------~%")
  (test-debug-analyzer-ram)
  (format t "~%LMI Debug board diagnostics complete.~%"))


(defsubst read-debug (addr)
  (%nubus-read debug-slot addr))

(defsubst write-debug (addr data)
  (%nubus-write debug-slot addr data))

(defsubst read-debug-byte (addr)
  (sys:%nubus-read-8 debug-slot addr))

(defsubst write-debug-byte (addr data)
  (sys:%nubus-write-8 debug-slot addr data))




(defsubst read-debug-mode ()
  (logand #xff (read-debug #xfff7fc)))

(defsubst write-debug-mode (data)
  (write-debug #xfff7fc data))

(defsubst write-debug-addr (addr)
  (write-debug #xfff7f8 addr))

(defsubst read-debug-addr()
  (read-debug #xfff7f8))

(defsubst write-debug-data (data)
  (write-debug #xfff7f4 data))

(defsubst read-debug-response-data ()
  (read-debug #xfff7f4))

(defsubst write-debug-control (ctl)
  (write-debug #xfff7f0 ctl))

(defsubst read-debug-response-control ()
  (logand #x3f (read-debug #xfff7f0)))

(defsubst write-debug-analyzer-pointer (data)
  (write-debug #xfff7ec data))

(defsubst read-debug-analyzer-pointer ()
  (logand #x8fff (read-debug #xfff7ec)))

(defsubst read-debug-analyzer-data ()
  (read-debug #xfff7e8))

(defsubst write-debug-analyzer-data (data)
  (write-debug #xfff7e8 data))

(defsubst read-debug-analyzer-control ()
  (logand #xff (read-debug #xfff7e4)))

(defsubst write-debug-analyzer-control (data)
  (write-debug #xfff7e4 data))

(defsubst write-debug-analyzer-function (data)
  (write-debug #xfff7e0 data))


(defsubst remote-write-debug-analyzer-pointer (data)
  (write-debug #xfff7ec data))

(defsubst remote-read-debug-analyzer-pointer ()
  (logand #x8fff (read-debug #xfff7ec)))

(defsubst remote-read-debug-analyzer-data ()
  (read-debug #xfff7e8))

(defsubst remote-write-debug-analyzer-data (data)
  (write-debug #xfff7e8 data))

(defsubst remote-read-debug-analyzer-control ()
  (logand #xff (read-debug #xfff7e4)))

(defsubst remote-write-debug-analyzer-control (data)
  (write-debug #xfff7e4 data))

(defsubst remote-write-debug-analyzer-function (data)
  (write-debug #xfff7e0 data))


(defsubst read-debug-explorer-ram ()
  (logand #xffff (read-debug #xfff7cc)))

(defsubst write-debug-explorer-ram (data)
  (write-debug #xfff7cc data))

(defsubst read-debug-explorer-pointer ()
  (logand #xfff (read-debug #xfff7c8)))

(defsubst write-debug-explorer-pointer (data)
  (write-debug #xfff7c8 data))

(defsubst read-debug-explorer-control ()
  (read-debug #xfff7c4))

(defsubst write-debug-explorer-control (data)
  (write-debug #xfff7c4 data))

(defsubst read-debug-explorer-status ()
  (read-debug #xfff7c0))




(defun setup-debug-board-for-loopback ()
  (write-debug-mode 1)                          ; reset the board
  (write-debug-mode #x1a)                       ; fast speed, loopback, master enable
  (process-sleep 2)                             ; let it idle down
  (read-debug-response-control)
  (write-debug-analyzer-function 0)
  (read-debug-analyzer-data)
  nil)

(defun wait-for-debug-xmit ()
  (dotimes (i 1000.)
    (if (equal 0 (logand #x40 (read-debug-mode)))
        (return t))))

(defun wait-for-debug-response (&aux temp)
  (setq temp (dotimes (i 1000.)
               (when (equal #x80 (logand #x80 (read-debug-mode)))
                 (return t))))
  (cond
    ((null temp) 'NO-RESPONSE)
    ((equal #x0e (read-debug-response-control))
     (read-debug-response-data))
    (t nil)))


(defun il (&aux temp)
  (setup-debug-board-for-loopback)
  (write-debug-explorer-control -1)
  (do-forever
    (setq temp (debug-read-word debug-test-reg-addr))
    (when (not (equal temp #xffffffff))
      (format t "Error --- wrote FFFFFFFF --- read ~A~%" temp))))


(defun rl ()
  (do-forever
    (read-debug-mode)))

(defun wl ()
  (do-forever
    (write-debug-mode #xffffffff)))


(defun read-config-prom (&aux s)
  (setq s "                                ")
  (dotimes (i 25.)
    (aset
      (logand #xff (read-debug (+ #xfff800 (* i 4))))
      s i))
  s)


(defun debug-read-word (addr)
  (write-debug-addr addr)
  (write-debug-control #x01)
  (wait-for-debug-response))

(defun debug-read-byte (addr)
  (write-debug-addr addr)
  (write-debug-control #x05)
  (wait-for-debug-response))

(defun debug-write-word (addr data)
  (write-debug-data data)
  (write-debug-addr addr)
  (write-debug-control #x09)
  (wait-for-debug-response))

(defun debug-write-byte (addr data)
  (write-debug-addr data)
  (write-debug-control 8 )
  (wait-for-debug-xmit)
  (write-debug-addr addr)
  (write-debug-control #x0d)
  (wait-for-debug-response))

(defun debug-rw-test (&aux temp)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug #xfff7c4 (ash 1 i))
      (setq temp (read-debug #xfff7c4))
      (when (not (equal temp (ash 1 i)))
        (format "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))



(defun debug-iloop-test (&aux temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (write-debug-explorer-control (ash 1 i))
      (setq temp (debug-read-word debug-test-reg-addr))
      (when (not (equal temp (ash 1 i)))
        (format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp))))
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (debug-write-word debug-test-reg-addr (ash 1 i))
      (setq temp (read-debug-explorer-control))
      (when (not (equal temp (ash 1 i)))
        (format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))

(defun debug-iloop ()
  (setup-debug-board-for-loopback)
  (do-forever
    (write-debug-explorer-control 0)
    (when (not(equal 0 (debug-read-word debug-test-reg-addr)))
      (tyo #/*))
    (write-debug-explorer-control #xffffffff)
    (when (not(equal #xffffffff (debug-read-word debug-test-reg-addr)))
      (tyo #/#))))

(defun debug-serial-loop-test (&aux data temp)
  (setup-debug-board-for-loopback)
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (setq data (ash 1 i))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
        (format t "Error --- wrote ~A --- read ~A~%" data temp))
      (setq data (logand #xffffffff (lognot (ash 1 i))))
      (write-debug-addr data)
      (write-debug-control #x0e)
      (wait-for-debug-response)
      (when (not(equal data (setq temp (read-debug-response-data))))
        (format t "Error --- wrote ~A --- read ~A~%" data temp)))))

(defun test-debug-analyzer-ram (&aux temp data master-data-list data-list)
  (setup-debug-board-for-loopback)
  (setq master-data-list (copylist '(#x01010101 #x02020202 #x04040404 #x08080808
                   #x10101010 #x20202020 #x40404040 #x80808080 #x55555555)))
  (rplacd (last master-data-list) master-data-list)
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (write-debug-analyzer-data (car data-list))
    (write-debug-analyzer-control (car data-list))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (car data-list))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data  (logand #xffffffff (lognot (car data-list))))
    (write-debug-analyzer-data data)
    (write-debug-analyzer-control data)
    (setq data-list (cdr data-list)))
  (setq data-list master-data-list)
  (dotimes (i 4096.)
    (write-debug-analyzer-pointer i)
    (setq data (logand #xffffffff (lognot (car data-list))))
    (when (not(equal data (setq temp (read-debug-analyzer-data))))
      (format t "Error data --- addr ~A --- wrote ~A --- read ~A~%" i data temp))
    (when (not(equal (logand #xff data) (setq temp (read-debug-analyzer-control))))
      (format t "Error control --- addr ~A --- wrote ~A --- read ~A~%" i (logand #xff data) temp))
    (setq data-list (cdr data-list))))

(defconst analyzer-codes (make-array 16. :initial-contents
   '("Null cycle 0"     "Start read"      "ACK Try Again Later"     "Idle Cycle"
     "Intermediate ACK" "Start byte read" "ACK Parity Error"        "Bad Idle cycle 1"
     "Null cycle 2"     "Start write"     "ACK NUBUS Timeout"       "Bad Idle cycle 2"
     "Null cycle 3"     "Start byte write" "ACK OK"                 "Bad Idle cycle 3")))


(defun print-debug-analyzer (&optional (count 30.) &aux ptr data ctl master code-string)
  (read-debug-analyzer-data)
  (setq ptr (logand #xfff (read-debug-analyzer-pointer)))
  (format t "~%SEQ#~8TADDRESS~18TDATA~28TMaster~36TINFO~%")
  (dotimes (i count)
    (write-debug-analyzer-pointer (- ptr 2 i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (setq master (ash (logand #xfa ctl) -4.))
    (write-debug-analyzer-pointer ptr)
    (setq code-string (aref analyzer-codes (logand ctl #x0f)))
    (cond
      ((eq 1 (logand ctl 1))
       (format t "~@4X~8T~@8X~28T~X~36T~A~%" (- ptr i) data master code-string))
      (t
       (format t "~@4X~18T~@8X~28T~X~36T~A~%" (- ptr i)  data master code-string)))))


(defun flog-debug-analyzer(&aux ptr data ctl)
  (start-debug-analyzer 0)
  (write-debug-explorer-control 0)
  (read-debug-explorer-control)
  (write-debug-explorer-control -1)
  (read-debug-explorer-control)
  (read-debug-mode)
  (read-debug-analyzer-data)
  (setq ptr (read-debug-analyzer-pointer))
  (dotimes (i 16.)
    (write-debug-analyzer-pointer (- ptr i))
    (setq data (read-debug-analyzer-data))
    (setq ctl (read-debug-analyzer-control))
    (format t "~@8X~10T~@3X~%" data ctl)))



(defun start-debug-analyzer (&optional slave master &aux temp)
  (setq temp 0)
  (write-debug-analyzer-pointer 0)
  (if slave
    (setq temp (logior temp (logand slave #x0f)))
    (setq temp (logior temp #x100)))
  (if master
    (setq temp (logior temp (ash (logand master #x0f) 4)))
    (setq temp (logior temp #x200)))
  (write-debug-analyzer-function (logior temp #x400)))

(defun debug-read-word-fast-repeat (addr)
  (write-debug-addr addr)
  (write-debug-control #x11)
  (tyi)
  (wait-for-debug-xmit)
  (write-debug-control #x01)
  (process-sleep 2)
  (read-debug-response-control))

(defun debug-write-word-fast-repeat (addr data)
  (write-debug-addr data)
  (write-debug-control #x08)
  (wait-for-debug-xmit)
  (write-debug-addr addr)
  (write-debug-control #x19)
  (tyi)
  (wait-for-debug-xmit)
  (write-debug-control #x09)
  (process-sleep 2)
  (read-debug-response-control))

(defun init-debug-board (&aux rmt)
  (assure-debug-board)
  (write-debug-mode 1)                          ; reset the board
  (write-debug-mode #x12)                       ; fast, master enable
  (process-sleep 2)                             ; let it idle down
  (read-debug-response-control)
  (write-debug-analyzer-function 0)
  (read-debug-analyzer-data)
  (if (setq rmt (remote-debug-mode-reg-address))
      (debug-write-word rmt #x12)
    (format t "~%*** Remote debug board not found ***~%"))
  rmt)


(defun remote-debug-mode-reg-address (&aux temp)
  (write-debug-control 0)               ; Poll request
  (setq temp (dotimes (i 1000.)
               (when (equal #x80 (logand #x80 (read-debug-mode)))
                 (return t))))
  (cond
    (temp
     (logior #xf0fff7fc (ash (logand #x0f (read-debug-response-data)) 24.)))
    (t nil)))



(defun bus-read (byte-address &optional ignore-bus-errors byte-mode &aux temp)
  (if byte-mode
    (setq temp (debug-read-byte byte-address))
    (setq temp (debug-read-word byte-address))
    (cond
      ((fixp temp) temp)
      (ignore-bus-errors 0)
      (t (error 'nubus-error)))))

(defun bus-read-byte (byte-address &optional ignore-bus-errors &aux temp)
    (setq temp (debug-read-byte byte-address))
    (cond
      ((fixp temp) (logand #xff (ash temp (* -8. (logand 3 byte-address)))))
      (ignore-bus-errors 0)
      (t (error 'nubus-error))))

(defun bus-write (byte-address data &optional ignore-bus-errors byte-mode &aux temp)
  (if byte-mode
    (setq temp (debug-write-word byte-address data))
    (cond
      ((fixp temp) nil)
      (ignore-bus-errors nil)
      (t (error 'nubus-error)))))

(defun bus-write-byte (byte-address data &optional ignore-bus-errors &aux temp)
  (setq temp (debug-write-word byte-address (dpb data (byte 8. (* 8. (logand 3 byte-address))) data)))
    (cond
      ((fixp temp) nil)
      (ignore-bus-errors nil)
      (t (error 'nubus-error))))


(defun debug-iloop-test-remote (&aux temp addr)
  (format t "Starting remote loopback test with mastership.")
  (setq addr (logior #xfff7c4 (logand #xff000000 (init-debug-board))))
  (dotimes (j 1000.)
    (dotimes (i 32.)
      (debug-write-word addr (ash 1 i))
      (setq temp (debug-read-word addr))
      (when (not (equal temp (ash 1 i)))
        (format t "Error --- wrote ~A --- read ~A~%" (ash 1 i) temp)))))
