;;; -*- Mode:LISP; Package:ILLUSTRATE; Base:10; Readtable:ZL -*-

;;; some more Ill commands, 7-Mar-86 12:09:28 -gjc


(define-ill-command :print-screen ()
                    "Hardcopy the bit array in the active graphics area"
                    "Print Screen" ()
  (USING-RESOURCE (ARRAY TV:HARDCOPY-BIT-ARRAY-RESOURCE)
    (MULTIPLE-VALUE-BIND (NIL WIDTH HEIGHT)
        (TV:SNAPSHOT-SCREEN (send self :get-pane 'graphics-pane) ARRAY)
      (TV:BEEP)
      (let ((left 0)
            (top 0)
            (right width)
            (bottom height)
            (tcp-application:*imagen-bit-array-notifications* nil))
        (HARDCOPY-BIT-ARRAY ARRAY left top right bottom))))
  (wait-for-printer-done))

(defvar *default-screen-directory* "DJ:DAWNA;")
(defvar *screen-count* 0)

(defun generate-screen-filename ()
  (send (fs:parse-pathname *default-screen-directory*)
        :new-pathname
        :name (format nil "SCREEN-~D" (incf *screen-count*))
        :type "IMPRESS"
        :version :newest))

(define-ill-command :save-screen ()
  "Save the screen image to a file of IMPRESS code"
  "Save Screen" ()
  (USING-RESOURCE (ARRAY TV:HARDCOPY-BIT-ARRAY-RESOURCE)
    (MULTIPLE-VALUE-BIND (NIL WIDTH HEIGHT)
        (TV:SNAPSHOT-SCREEN (send self :get-pane 'graphics-pane) ARRAY)
      (TV:BEEP)
      (let ((left 0)
            (top 0)
            (right width)
            (bottom height))
        (HARDCOPY-BIT-ARRAY ARRAY left top right bottom :printer :imagen-data)
        (IMAGEN-DATA-DUMP-FORMAT (generate-screen-filename))))))





(defun Imagen-data-dump-format (filename)
  (with-open-file (stream filename :direction :output)
    (TCP-APPLICATION:imagen-data-dump-format-1 stream)
    (setq tcp-application:*imagen-data-size* nil)))


TCP-APPLICATION:

(defun imagen-data-dump-format-1 (imagen-stream)
  (let* ((landscapep (eval *imagen-landscapep*))
         (magnification
           (cond (landscapep
                  (max 0 (min (fix (log *imagen-max-magnification* 2))
                              (imagen-magnification (nth 2 *imagen-data-size*)
                                                    10.5)
                              (imagen-magnification (nth 3 *imagen-data-size*)
                                                    8))))
                 ('else
                  (max 0 (min (fix (log *imagen-max-magnification* 2))
                              (imagen-magnification (nth 2 *imagen-data-size*)
                                                    7)
                              (imagen-magnification (nth 3 *imagen-data-size*)
                                                    10)))))))
    (cond (landscapep
           (impress imagen-stream
                    'SET_MAGNIFICATION magnification
                    ;; the measurements units must be off, but this looks ok.
                    'SET_ABS_H (inches-to-points 0.15)
                    'SET_ABS_V (inches-to-points 10.9)
                    'SET_HV_SYSTEM (list :byte
                                         0      ; pad
                                         1 1    ; ORIGIN: physical
                                         1 0    ; AXES: +90 deg (regular)
                                         1 1 1  ; Orientation: 270 from X.
                                         )))
          ('else
           (impress imagen-stream
                    'SET_MAGNIFICATION magnification
                    'SET_ABS_H (inches-to-points 1)
                    'SET_ABS_V (inches-to-points 1))))
    (impress imagen-stream
             'BITMAP 'OPAQUE (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*))
    (send imagen-stream :string-out *imagen-data-bytes*
          0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
    (impress imagen-stream 'ENDPAGE 'EOF)))




(tcp-application:make-printer-imagen "100.0.0.40")

(defun wait-for-printer-done ()
  (process-wait "print done" #'(lambda ()
                                 (not tcp-application:*imagen-data-size*))))

