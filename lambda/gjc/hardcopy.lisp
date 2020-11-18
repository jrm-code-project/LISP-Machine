;;; -*- Mode:LISP; Package:TCP-APPLICATION; Readtable:ZL; Base:10 -*-


;;; THIS IS WIRED-IN FOR USE AT LMI.

(DEFVAR *IMAGEN-PRINTER* '(:IMAGEN "100.0.0.40"))

(SETQ SI:*DEFAULT-PRINTER* *IMAGEN-PRINTER*)
(SETQ SI:*DEFAULT-BIT-ARRAY-PRINTER *IMAGEN-PRINTER*)

(DEFUN USER:HARDCOPY-SCREEN (SCREEN &OPTIONAL FILEP)
  (USING-RESOURCE (ARRAY TV:HARDCOPY-BIT-ARRAY-RESOURCE)
    (MULTIPLE-VALUE-BIND (NIL WIDTH HEIGHT)
        (TV:SNAPSHOT-SCREEN SCREEN ARRAY)
      (TV:BEEP)
      (let ((left 0)
            (top 0)
            (right width)
            (bottom height))
        (COND (FILEP
               (HARDCOPY-BIT-ARRAY ARRAY left top right bottom :PRINTER :IMAGEN-DATA)
               (IMAGEN-DATA-DUMP-FORMAT (generate-screen-filename)))
              ('ELSE
               (LET ((*imagen-bit-array-notifications* nil)
                     (*IMAGEN-TRANSMIT-BACKGROUNDP* NIL)
                     (*IMAGEN-DATA-SIZE* NIL))
                 (HARDCOPY-BIT-ARRAY ARRAY left top right bottom :PRINTER *IMAGEN-PRINTER*))))))))

(defvar *default-screen-directory* "DJ:DAWNA;")
(defvar *screen-count* 0)

(defun generate-screen-filename ()
  (send (fs:parse-pathname *default-screen-directory*)
        :new-pathname
        :name (format nil "SCREEN-~D" (incf *screen-count*))
        :type "IMPRESS"
        :version :newest))

(DEFUN USER:HARDCOPY-SCREEN-DUMP (FILENAME)
  (LET ((PATH (FS:PARSE-PATHNAME (IF (NUMBERP FILENAME) *DEFAULT-SCREEN-DIRECTORY* FILENAME))))
    (IF (NUMBERP FILENAME)
        (SETQ PATH (SEND PATH :NEW-PATHNAME :NAME (FORMAT NIL "SCREEN-~D" FILENAME) :TYPE "IMPRESS")))
    (HARDCOPY-FILE PATH :FORMAT :IMPRESS :PRINTER *IMAGEN-PRINTER*)))

(defun Imagen-data-dump-format (filename)
  (with-open-file (stream filename :direction :output)
    (imagen-data-dump-format-1 stream)
    (setq *imagen-data-size* nil)))

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

(defun wait-for-printer-done ()
  (process-wait "print done" #'(lambda ()
                                 (not *imagen-data-size*))))
