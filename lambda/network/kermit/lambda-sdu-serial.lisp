;;; -*- Mode:LISP; Package:KERMIT; Readtable:CL; Base:10 -*-

;;;Lambda SDU serial stuff

;;;By default, pick this serial port

(defvar *sdu-serial-default-device-name* "SDU-SERIAL-B")

;;;Modify these to (re)init serial port on open with parameters:

(defvar *sdu-serial-xon-xoff-p* T
  "If non-NIL, serial I/O uses software (Xon/Xoff) flow control")

(defvar *sdu-serial-ascii-p* NIL
  "If non-NIL, serial I/O does remote Ascii <--> local LISPM character conversion")

(defvar *sdu-serial-default-baud-rate* 9600.
  "Baud rate to set in SDU-SERIAL-OPEN")

;;;Make a list of device names and suitable open forms:

(defun gather-sdu-serial-devices(&optional reset-baud-rate)
  (loop for p in fs:*pathname-host-list*
        when (typep p 'si:sdu-serial-b-shared-device)
        collect `(,(send p :name) (sdu-serial-open ,(send p :name) ,reset-baud-rate))))

(defvar *sdu-serial-device-alist*
        (gather-sdu-serial-devices)
  "An a-list of SDU-SERIAL-B-SHARED-DEVICE type devices.
The CADR of each entry is a form suitable for opening the associated device.")

;;;This is the open function:

(defun sdu-serial-open (&optional
                        (name *sdu-serial-default-device-name*)
                        (new-baud-rate T))
  "Open an SDU serial port with options specified by global parameters.
NAME is a serial device name; defaults to *SDU-SERIAL-DEFAULT-DEVICE-NAME*.
NEW-BAUD-RATE should be a
  1) a number to set baud rate to, or
  2) T (the default) to use the global parameter *SDU-SERIAL-DEFAULT-BAUD-RATE*, or
  3) NIL to not specify a baud rate (value defaults from existing or previous
     SDU serial streams).
 Note that changing the baud rate completely resets the serial port."
  ;;;;
  (declare (values sdu-serial-stream))
  (unless (member name *sdu-serial-device-alist* :test #'string-equal :key #'car)
    (cerror "Proceed to open ~s anyway"
            "~s is not the name of a known serial port shared device / pathname host" name))
  ;;;;
  (let ((device (make-pathname :host name)))
    (check-type device si:shared-device-pathname)
    (open device
          :flavor-and-init-options
          `(
            ,(si:combined-sdu-serial-stream-flavor :ascii    *sdu-serial-ascii-p*
                                                   :xon-xoff *sdu-serial-xon-xoff-p*)
            :input-buffer-size  ,(* 3 si:page-size)
            :output-buffer-size ,(* 2 si:page-size)
            ,@(if new-baud-rate (list :baud-rate
                                      (if (eq new-baud-rate t)
                                          *sdu-serial-default-baud-rate*
                                        new-baud-rate)))))))
