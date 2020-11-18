;;; -*- Mode:LISP; Package:TCP-APPLICATION; Patch-File:T; Base:10; Readtable:ZL -*-


;;; patches for dawna for paint program screen dumps.


(defvar *imagen-override-magnification* nil)

(defvar *imagen-paper* (list
                         ;; these may need adjustment on a per-printer
                         ;; basis.
                         :landscape-h-available 11.0
                         :landscape-v-available  8.5
                         :landscape-h-offset     0.0
                         :landscape-v-offset    11.0
                         :portrait-h-available   8.3
                         :portrait-v-available  10.8
                         :portrait-h-offset      0.1
                         :portrait-v-offset      0.1))

(defun imagen-magnification (bits inches)
  (declare (values magnification actual-size))
  (let ((magnification (or *imagen-override-magnification*
                           (do ((mag 0 (1+ mag))
                                (size (quotient bits 300.0) (* size 2)))
                               ((or (= mag *imagen-max-magnification*)
                                    (> (* size 2) inches))
                                mag)))))
    (values magnification
            (* bits (expt 2 magnification) (// 1.0 300.0)))))

(defun transmit-imagen-data-bytes-1 (address)
  (with-open-file (imagen-stream (string-append "TCP-HOST:"
                                                address
                                                "#IMAGEN"))
    (let* ((landscapep (eval *imagen-landscapep*))
           (magnification
             (cond (landscapep
                    (max 0 (min (imagen-magnification (nth 2 *imagen-data-size*)
                                                      (getf *imagen-paper* :landscape-h-available))
                                (imagen-magnification (nth 3 *imagen-data-size*)
                                                      (getf *imagen-paper* :landscape-v-available)))))
                   ('else
                    (max 0 (min (imagen-magnification (nth 2 *imagen-data-size*)
                                                      (getf *imagen-paper* :portrait-h-available))
                                (imagen-magnification (nth 3 *imagen-data-size*)
                                                      (getf *imagen-paper* :portrait-v-available)))))))
           (ascii-stream (make-simple-lispm->ascii-stream imagen-stream)))
      (format ascii-stream "@document(language imPress,~
                      ~% jobheader on,~
                      ~% name ~:[portrait~;landscape~]~D-~D-~D,~
                      ~% spooldate ~S,~
                      ~% owner ~S,~
                      ~% pageReversal)"
              landscapep
              (expt 2 magnification)
              (nth 2 *imagen-data-size*)
              (nth 3 *imagen-data-size*)
              (time:print-current-time nil :dd-mmm-yyyy)
              (string si:user-id))

        (cond (landscapep
               (impress imagen-stream
                        'SET_MAGNIFICATION magnification
                        ;; the measurements units must be off, but this looks ok.
                        'SET_ABS_H (inches-to-points (getf *imagen-paper* :landscape-h-offset))
                        'SET_ABS_V (inches-to-points (getf *imagen-paper* :landscape-v-offset))
                        'SET_HV_SYSTEM (list :byte
                                             0     ; pad
                                             1 1   ; ORIGIN: physical
                                             1 0   ; AXES: +90 deg (regular)
                                             1 1 1 ; Orientation: 270 from X.
                                             )))
              ('else
               (impress imagen-stream
                        'SET_MAGNIFICATION magnification
                        'SET_ABS_H (inches-to-points (getf *imagen-paper* :portrait-h-offset))
                        'SET_ABS_V (inches-to-points (getf *imagen-paper* :portrait-v-offset)))))
        (impress imagen-stream
                 'BITMAP 'OPAQUE (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*))
        (send imagen-stream :string-out *imagen-data-bytes*
              0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
        (impress imagen-stream 'ENDPAGE 'EOF))))

(setq *imagen-bit-array-notifications* nil
      *IMAGEN-TRANSMIT-BACKGROUNDP* nil)
