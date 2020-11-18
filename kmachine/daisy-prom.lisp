;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:ZL -*-

;to transfer files from VAX to DAISY:
; (1) boot DNIX on IBM-PC Daisy.
; (2) CD to desired directory i.e.
;     CD /NET/FART/USER/SCOTT/NU-CHIP/PALS
; (3) invoke kermit
; (4) SET BAUD 9600
; (5) CONNECT   now talking to vax
; (6) login, etc
; (7) kermit -s boot*
; (8) control-=c to get back (not what it tells you at step 5)
; (9) rec<cr>

(defvar boot-prom-code (make-array 4096.))
(defvar boot-prom-last-addr ())

(defun make-boot-prom-simulation (fcns)
  (daisy-load-fcns fcns 0)
  (dotimes (prom 4.)
    (with-open-file (this-prom
                      (format nil "angel://lmi//khh//k//BOOT~d.VLA" prom)
                      :write)
      (format this-prom "$DATA_HEADER$~%$TYPE$~%ROM~%$FORMAT$~%ADDRESS_DATA~%$BASE$~%D B~%$TOTAL_COLUMNS$~%4 8~%$END$~%")
      (do*
        ((addr 0 (1+ addr)))
        ((> addr boot-prom-last-addr))
        (let*
          ((inst (aref boot-prom-code addr))
           (inst-1 (ldb (byte 8. (* prom 8.)) inst))
           (inst-2 (ldb (byte 8. (+ 32. (* prom 8.))) inst)))
          (format this-prom "~4,'0d ~8,'0b~%" (ash addr 1) inst-1)
          (format this-prom "~4,'0d ~8,'0b~%" (1+ (ash addr 1)) inst-2))))))

(defun make-boot-ram-simulation (fcns)
  (daisy-load-fcns fcns 0)
  (dotimes (ram 4.)
    (with-open-file (this-ram
                      (format nil "angel://lmi//khh//k//BTRAM~d.VLA" ram)
                      :write)
      (format this-ram "$DATA_HEADER$~%$TYPE$~%RAM~%$FORMAT$~%ADDRESS_DATA~%$BASE$~%D B~%$TOTAL_COLUMNS$~%4 1~%$END$~%")
      (do*
        ((addr 0 (1+ addr)))
        ((> addr boot-prom-last-addr))          ;using same array.
        (let*
          ((inst (aref boot-prom-code addr))
           (inst-1 (ldb (byte 8 (* ram 8.)) inst))                      ;low half of 64. bits.
           (inst-2 (ldb (byte 8 (+ 32. (* ram 8.))) inst)))             ;high half of 64 bits.
          (format this-ram "~4,'0d ~b~%" (ash addr 1) inst-1)
          (format this-ram "~4,'0d ~b~%" (1+ (ash addr 1)) inst-2))))))

(defun daisy-load-fcn (fcn &optional (starting-address
                                       (k-kbug:get-starting-address fcn #x100)))
  (let ((f (nc::get-ncompiled-function fcn)))
    (nc::link fcn starting-address)
    (daisy-load-code (nc::ncompiled-function-code f) starting-address)))

;;; note that this links twice
;;; but that is good because then "forward references" are resolved
(defun daisy-load-fcns (fcns &optional (starting-address #x100))
  (let ((cfuns (map 'list #'nc::get-ncompiled-function fcns)))
    (do ((fs cfuns (cdr fs))
         (addr starting-address (+ addr (nc::ncompiled-function-length (car fs)))))
        ((null fs))
      (nc::link (car fs) addr))
    (dolist (f cfuns)
      (daisy-load-fcn f))))

(defun daisy-load-code (code &optional (addr #x100))
  (lisp:map nil #'(lambda (inst)
               (global:aset inst boot-prom-code addr)
               (setq boot-prom-last-addr addr)
               (incf addr))
       code))
