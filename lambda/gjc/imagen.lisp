#|;;;  -*- Mode:LISP; Package:TCP-APPLICATION; Fonts:(CPTFONTB); Base:10; Patch-File:T -*-

  Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
  licensing and release information.


This is some simple imagen support code, more extensive capabilities
such as multiple fonts and other impress support are available as a
seperate product from LMI.

|#


(defun make-printer-imagen (address)
  "Set the default printer to be an imagen at the given address"
  (check-type address string)
  (setq si:*default-printer* (list :imagen address))
  (setq si:*default-bit-array-printer* (list :imagen address)))

(defvar *imagen-status-ok-wait* nil
  "if not-null the number of seconds to pause before trying to get a
go-ahead status from the imagen")

(defvar *imagen-status-ok-wait-tries* 10)

(defun imagen-status-ok-to-print (address &optional (verbose t))
  (cond ((not *imagen-status-ok-wait*)
         (imagen-status-ok-to-print-1 address verbose))
        ('else
         (do ((j 0 (1+ j)))
             ((imagen-status-ok-to-print-1 address verbose)
              t)
           (if (=  (1- *imagen-status-ok-wait-tries*)
                   (mod j *imagen-status-ok-wait-tries*))
               (cerror "continue waiting for it"
                "Imagen printer at ~S may be wedged" address))
           (process-sleep (fix (* *imagen-status-ok-wait* 60))
                          "imagen queue wait")))))


(defun imagen-status-ok-to-print-1 (address verbose)
  (let ((status (get-imagen-status address)))
    (cond ((null status)
           (format t "~&Imagen printer at ~S not responding~%" address)
           nil)
          ((zerop (getf status :protocols-accepted))
           (format t "~&Printer busy, try again: ~A~%"
                   (getf status :engine-status))
           nil)
          ('else
           (describe-imagen-status status standard-output verbose)
           t))))


(SET-PRINTER-DEFAULT-OPTION :IMAGEN :PAGE-HEADINGS T)

(defun (:imagen si:print-file) (printer filename &rest options)
  (let ((address (cadr printer)))
    (cond ((not (imagen-status-ok-to-print address)))
          ((probe-file filename)
           (print-document-to-imagen filename address options))
          ('else
           (format t "~&File not found: ~S~%" filename)))))


(DEFVAR INCREDIBLE-KLUDGE)

(DEFUN (:IMAGEN SI:PRINT-STREAM) (PRINTER STREAM &REST OPTIONS)
  (LET ((INCREDIBLE-KLUDGE STREAM))
    (APPLY (GET :IMAGEN 'SI:PRINT-FILE)
           PRINTER
           "EVAL-HOST:TCP-APPLICATION:INCREDIBLE-KLUDGE"
           OPTIONS)))

(defun (:imagen si:print-status) (printer &optional (stream standard-output))
  (format stream "~&Status of: ~A~%" printer)
  (describe-imagen-status (get-imagen-status (cadr printer))
                          stream))

(defun describe-imagen-status (status stream &optional (verbose t))
  (cond ((null status)
         (format stream "~&Imagen printer not responding~%"))
        (verbose
         (format stream "~&Imagen printer at ~S for ~D second~p.~%~A~%"
                 (getf status :address) (getf status :age) (getf status :age)
                 (getf status :engine-status))
         (format stream "Job requests will be ~A~%"
                 (if (zerop (getf status :protocols-accepted))
                     "rejected" "accepted"))
         (let ((problems (set-difference (getf status :problems)
                                         '(:job-in-progress))))
           (if problems
               (format t "Problem~p: ~{~A~^, ~}.~%"
                       (length problems)
                       problems))))
        ('else
         (princ (getf status :engine-status) stream)
         (let ((problems (set-difference (getf status :problems)
                                         '(:job-in-progress))))
           (if problems
               (format t " Problem~p: ~{~A~^, ~}.~%"
                       (length problems)
                       problems))))))

(defun print-document-to-imagen (filename address options &aux tm)
  (check-type address string)
  (with-open-file  (file-stream filename)
    (with-open-file (imagen-stream (string-append "TCP-HOST:"
                                                  address
                                                  "#IMAGEN")
                                   :auto-force-output nil)
      (let ((stream (make-simple-lispm->ascii-stream imagen-stream)))
        (format stream "@document(language Printer,~
                               ~% jobheader on,~
                               ~% name ~S,~
                               ~% spooldate ~S,~
                               ~% owner ~S,~
                               ~% pageReversal)"
                (send (send file-stream :truename) :string-for-printing)
                (setq tm (time:print-current-time nil :dd-mmm-yyyy))
                (string si:user-id))
        (let ((heading (if (getf options :page-headings)
                           (format nil "~A printed ~A"
                                   (send (send file-stream :truename)
                                         :string-for-editor)
                                   tm))))
          (let ((lpr (make-lpr-stream stream
                                      :margin 1
                                      :width 80
                                      :length 58
                                      :pageno (not (null heading))
                                      :heading heading)))
            (cond (heading
                   (format lpr "****************  ~A~%~%" heading))
                  ('else (terpri lpr)))
            (stream-copy-until-eof file-stream lpr)))))))


(defun make-simple-lispm->ascii-stream (output)
  (progv '(ftp:*hash*)
         '(nil)
    (closure '(ftp:*hash*)
             (FTP:MAKE-ASCII-TRANSLATING-OUTPUT-STREAM output))))

#||

(DEFUN TEST-LPR-STREAM (&optional no-op &aux tm)
  (WITH-OPEN-FILE (IN "lm:gjc;IMAGEN.LISP")
    (WITH-OPEN-FILE (OUT "lm:gjc;IMAGEN.LPR" :OUT)
      (format t "~&;Timing ...")
      (setq tm (time))
      (STREAM-COPY-UNTIL-EOF IN (if no-op out
                                  (MAKE-LPR-STREAM OUT
                                                   :HEADING
                                                   (send (SEND IN :TRUENAME)
                                                         :string-for-editor)
                                                   :pageno t)))

      (setq tm (quotient (time-difference (time) tm) 60.0))
      (format t "~&;~$ seconds" tm)
      tm)))

||#

(DEFUN MAKE-LPR-STREAM (OUTPUT &OPTIONAL &KEY (MARGIN 10) (WIDTH 69) (LENGTH 50)
                        (PAGENO T)
                        (HEADING NIL)
                        &AUX STREAM (LINE-COUNT 0) (CHAR-COUNT 0) (PAGE-COUNT 1))
  "Make a stream for creating files suitable for simple line-printers"
  (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYO
                       (COND ((< ARG1 #o40))
                             ((= ARG1 #\RETURN)
                              (COND ((= LINE-COUNT LENGTH)
                                     (SEND OUTPUT :TYO #\FORM)
                                     (INCF PAGE-COUNT)
                                     (SETQ LINE-COUNT 0)
                                     (SETQ CHAR-COUNT 0)
                                     (WHEN (OR PAGENO HEADING)
                                       (DOTIMES (J (FLOOR (- WIDTH
                                                             (IF HEADING
                                                                 (LENGTH HEADING)
                                                               0))
                                                          2))
                                         (SEND OUTPUT :TYO #\SPACE))
                                       (COND ((AND PAGENO HEADING)
                                              (FORMAT STREAM "Page ~D of ~A~%~%"
                                                      page-count
                                                      heading
                                                      ))
                                             (PAGENO
                                              (FORMAT STREAM "Page ~D~%~%"
                                                      heading
                                                      ))
                                             (HEADING
                                              (FORMAT STREAM "~A~%~%" HEADING)))))

                                    ('ELSE
                                     (INCF LINE-COUNT)
                                     (SETQ CHAR-COUNT 0)
                                     (SEND OUTPUT :TYO #\RETURN)
                                     (DOTIMES (J MARGIN)
                                       (SEND OUTPUT :TYO #\SPACE)))))
                             ((= ARG1 #\TAB)
                              (DOTIMES (J (- 8 (MOD CHAR-COUNT 8)))
                                (SEND STREAM :TYO #\SPACE)))
                             ((GRAPHIC-CHAR-P ARG1)
                              (COND ((= CHAR-COUNT WIDTH)
                                     (SEND STREAM :TYO #\RETURN)
                                     (SEND STREAM :TYO ARG1))
                                    ('ELSE
                                     (INCF CHAR-COUNT)
                                     (SEND OUTPUT :TYO ARG1))))))
                     ;; handling :STRING-OUT is somewhat of a pain, but
                     ;; runtime efficient, and interesting from the point of
                     ;; view of how one would define a language so as to
                     ;; make this sort of case handling more natural.
                     ;; Timing result: 7.6 seconds with :STRING-OUT
                     ;;               14.2 without. Using TEST-LPR-STREAM
                     ;; on this source file. No-Op case is 0.82 seconds.
                     ;;#||
                     (:string-out
                       ;; we can think of two ways of handling this,
                       ;; the complete way, by taking the :TYO code
                       ;; and optimizing it by keeping local (non enclosed)
                       ;; variables and by delaying the :TYO to OUTPUT until
                       ;; :STRING-OUT, or the EASY-CASES way, by recognising
                       ;; an amount of characters which are an easy case.
                       (do ((start (or (car args) 0)
                                   (let ((s (lpr-stream-string-out-easy-case
                                              output
                                              arg1 start
                                              (min end
                                                   (+ start (- width char-count))))))
                                     (cond ((= s start)
                                            (send stream :tyo (aref arg1 start))
                                            (1+ start))
                                           ('else
                                            (incf char-count (- s start))
                                            s))))
                            (end (or (cadr args) (length arg1))))
                           ((= start end))))
                     ;;||#
                     (T
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))


(defun lpr-stream-string-out-easy-case (output string start end)
  (do ((j start (1+ j))
       (c))
      ((= j end)
       (send output :string-out string start end)
       j)
    (setq c (aref string j))
    (cond ((graphic-char-p c))
          ((= j start)
           (return j))
          ('else
           (send output :string-out string start j)
           (return j)))))

(defun get-imagen-status (address &key &optional
                          (tries 10)
                          (try-pause 0.01)
                          (wait-pause 0.01))
  "Returns a plist describing the status of the imagen at the internet ADDRESS"
  (check-type address string)
  (check-type tries (fixnum 1))
  (check-type try-pause number)
  (check-type wait-pause number)
  (let (result)
    (dotimes (j tries)
      (if (setq result (get-imagen-status-1 address (ROUND (* wait-pause 60))))
          (return-from get-imagen-status result))
      (process-sleep (ROUND (* try-pause 60)) "imagen status sleep"))))

(defconst imagen-udp-status-pkt-size 256)

(defun get-imagen-status-1 (address pause)
  (with-open-file (stream (string-append "TCP-HOST:" address "#IMAGEN")
                          :for-udp t)
    (using-resource (array fs:simple-string-buffer imagen-udp-status-pkt-size)
      (fill array 0)
      (send stream :write-packet array 0 imagen-udp-status-pkt-size)
      (dotimes (j pause)
        ;; :listen cant be use inside a process-wait function because
        ;; it sends a message to the device driver which needs a process
        ;; to run in order to get the reply.
        (if (send stream :listen) (return))
        (process-sleep 1 "imagen status reply"))
      (WHEN (SEND STREAM :LISTEN)
        (SEND STREAM :READ-PACKET ARRAY)
        (let ((plist (PARSE-IMAGEN-STATUS-PACKET ARRAY)))
          (setf (getf plist :address) address)
          plist)))))

(DEFUN BIG-ENDIAN-BYTES (ARRAY &REST BYTES)
  "Combine bytes into an integer, most significant come first"
  (DO ((NUMBER 0 (+ (* NUMBER 256) (AREF ARRAY (CAR L))))
       (L BYTES (CDR L)))
      ((NULL L) NUMBER)))

(DEFUN PARSE-IMAGEN-STATUS-PACKET (ARRAY)
  (DO ((PLIST (LIST :STATUS-BYTE (BIG-ENDIAN-BYTES ARRAY 2)
                    :AGE (BIG-ENDIAN-BYTES ARRAY 4 5 6 7)
                    :PROTOCOLS-SUPPORTED (BIG-ENDIAN-BYTES ARRAY 8 9 10 11)
                    :PROTOCOLS-ACCEPTED  (BIG-ENDIAN-BYTES ARRAY 12 13 14 15)
                    :ENGINE-STATUS (SUBSTRING ARRAY
                                              (BIG-ENDIAN-BYTES ARRAY 16 1)
                                              (+ (BIG-ENDIAN-BYTES ARRAY 16 1)
                                                 (BIG-ENDIAN-BYTES ARRAY 18 19)))))
       (PROBLEMS '((#x01 :OTHER)
                   (#x02 :NO-PAPER)
                   (#x04 :PAPER-JAM)
                   (#x08 :LACKING-CONSUMABLES)
                   (#xF0 :JOB-IN-PROGRESS))
                 (CDR PROBLEMS)))
      ((NULL PROBLEMS) PLIST)
    (IF (BIT-TEST (CAAR PROBLEMS) (GETF PLIST :STATUS-BYTE))
        (PUSH (CADAR PROBLEMS) (GETF PLIST :PROBLEMS)))))




;;; printing arrays is somewhat hairier

;; for simplicity and speed we convert the bit array directly into
;; impress, (even if inside the KBD-SYS process) and then queue it for
;; transmission.

(defvar *imagen-data-bytes* nil)

(defvar *imagen-data-size* nil "A list of (hsize vsize), of 32^2 chunks")

(defun (:imagen si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  (when (null *imagen-data-bytes*)
    (setq *imagen-data-bytes* (make-array (// (* 1024 1024) 8)
                                          :type 'art-string)))
  (let (okp mess)
    (cond ((> (setq okp (round (* (- right left) (- bottom top)) 8))
              (length *imagen-data-bytes*))
           (tv:notify nil "Array too big to process, need ~D bytes" okp))
          (*imagen-data-size*
           (tv:notify nil "Last array still processing, try again later"))
          ((progn (and *imagen-status-ok-wait*
                       ;; if we are going to wait for the imagen to be ready
                       ;; then process the data now.
                       (setup-imagen-data-bytes array left top (- right left)
                                                (- bottom top)))
                  (setq mess (with-output-to-string (standard-output)
                               (setq okp (imagen-status-ok-to-print (cadr printer)
                                                                    nil))))
                  okp)
           (or *imagen-status-ok-wait*
               (setup-imagen-data-bytes array left top (- right left) (- bottom top)))
           (tv:notify nil "~A" mess)
           ;; actually should do waiting in background process.
           ;; Implement full queued situation...
           (process-run-function "imagen transmit"
                                 #'transmit-imagen-data-bytes
                                 (cadr printer))))))



(defun (:imagen-data si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  ;; use this if you just want to setup the DATA for further processing.
  (when (null *imagen-data-bytes*)
    (setq *imagen-data-bytes* (make-array (// (* 1024 1024) 8)
                                          :type 'art-string)))
  (setup-imagen-data-bytes array left top (- right left) (- bottom top)))


(defvar *bitrev-byte-table* nil)

(defun transmit-imagen-data-bytes (address)
  (condition-case (obj)
      (unwind-protect
          (transmit-imagen-data-bytes-1 address)
        (setq *imagen-data-size* nil))
    (error
     (tv:notify nil "~A"
                (with-output-to-string (stream)
                  (send obj :report stream))
                nil))))

(defun impress (stream &rest data)
  (dolist (datum data)
    (cond ((symbolp datum)
           (impress stream (or (get datum 'impress)
                               (ferror nil "Undefined impress symbol: ~S" datum))))
          ((atom datum)
           (if (atom stream)
               (send stream :tyo datum)
             (setf (car stream) (nconc (car stream) (ncons datum)))))
          ((eq (car datum) :word)
           (impress stream
                    (ldb (byte 8 8) (cadr datum))
                    (ldb (byte 8 0) (cadr datum))))
          ((eq (car datum) :byte)
           (do ((x 0 (+ (car v) (* x 2)))
                (v (cdr datum) (cdr v)))
               ((null v)
                (impress stream x))))
          ('else
           (ferror nil "unknown impress datum: ~S" datum)))))


(DEFPROP SET_MAGNIFICATION                  236 IMPRESS)
(DEFPROP SET_ABS_H                  135 IMPRESS)
(DEFPROP SET_ABS_V                  137 IMPRESS)
(DEFPROP BITMAP             235 IMPRESS)
(DEFPROP OPAQUE             3 IMPRESS)
(DEFPROP SET_HV_SYSTEM             205 IMPRESS)
(DEFPROP ENDPAGE   219 IMPRESS)
(DEFPROP EOF   255 IMPRESS)

(defun inches-to-points (x)
  (list :word (fix (* x 300))))

(defun imagen-magnification (bits inches)
  (let ((exact (log (// (* (float inches) 300) bits) 2)))
    (values (fix exact)
            (// (* bits (expt 2 (fix exact))) 300.0))))

(defvar *imagen-landscapep* '(> (car *imagen-data-size*)
                                (cadr *imagen-data-size*))
  "An expression to evaluate. If T then use landscape mode on imagen")

(defvar *imagen-max-magnification* 4)

(defun transmit-imagen-data-bytes-1 (address)
  (with-open-file (imagen-stream (string-append "TCP-HOST:"
                                                address
                                                "#IMAGEN"))
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
                                                      10))))))
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
                        'SET_ABS_H (inches-to-points 0.15)
                        'SET_ABS_V (inches-to-points 10.9)
                        'SET_HV_SYSTEM (list :byte
                                             0     ; pad
                                             1 1   ; ORIGIN: physical
                                             1 0   ; AXES: +90 deg (regular)
                                             1 1 1 ; Orientation: 270 from X.
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
        (impress imagen-stream 'ENDPAGE 'EOF))))


(defun imagen-data-dump (filename &key &optional magnification)
  "Dumps the screen-dump imagen data to a file, straight impress format"
  (with-open-file (stream filename :direction :output :characters nil)
    (when magnification
      (impress stream 'set_magnification magnification))
    (impress stream
             'BITMAP 'OPAQUE (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*))
    (send stream :string-out *imagen-data-bytes*
          0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
    (impress stream 'endpage)))

(defvar *32^32-chunk* nil)
(defvar *128-byte-chunk* nil)

(defun setup-imagen-data-bytes (array x y dx dy)
  (when (null *bitrev-byte-table*)
    (setq *bitrev-byte-table* (make-array 256))
    (dotimes (j 256)
      (setf (aref *bitrev-byte-table* j)
            (do ((value 0 (dpb (ldb (byte 1 k) j) (byte 1 (- 7 k)) value))
                 (k 0 (1+ k)))
                ((= k 8) value)))))
  (when (or (null *32^32-chunk*)
            (null *128-byte-chunk*))
    (setq *32^32-chunk* (make-array '(32 32) :type 'art-1b))
    (setq *128-byte-chunk* (make-array 128 :type 'art-8b
                                       :displaced-to *32^32-chunk*)))
  (multiple-value-bind (easy-y remainder-y)
      (floor dy 32)
    (multiple-value-bind (easy-x remainder-x)
        (floor dx 32)
      (let ((hsize (+ easy-x (if (zerop remainder-x) 0 1)))
            (vsize (+ easy-y (if (zerop remainder-y) 0 1))))
        (setq *imagen-data-size* (list hsize vsize dx dy))
        (do ((ymap 0 (1+ ymap))
             (data-bytes *imagen-data-bytes*)
             (inc 0)
             (bitrev *bitrev-byte-table*)
             (32^32-chunk *32^32-chunk*)
             (128-byte-chunk *128-byte-chunk*))
            ((= ymap vsize))
          (do ((xmap 0 (1+ xmap)))
              ((= xmap hsize))
            (let ((xbase (ash xmap 5))
                  (ybase (ash ymap 5)))
              (let ((x-want (min (- dx xbase) 32))
                    (y-want (min (- dy ybase) 32)))
                (when (or (= x-want remainder-x)
                          (= y-want remainder-y))
                  (fill 128-byte-chunk 0))
                (bitblt tv:alu-seta
                        x-want
                        y-want
                        array
                        (+ xbase x)
                        (+ ybase y)
                        32^32-chunk
                        0
                        0)))
              (copy-array-portion-translated 128-byte-chunk
                                             0
                                             128
                                             data-bytes
                                             inc
                                             (incf inc 128)
                                             bitrev)))))))


(defun copy-array-portion-translated (from-array from-start from-end to-array to-start
                                      to-end translation-table)
  ;; a canditate for microcompilation.
  (copy-array-portion from-array
                      from-start
                      from-end
                      to-array
                      to-start
                      to-end)
  ;; A typical screen array takes 4.2 seconds to process.
  ;; 1.0 seconds with the following code commented out,
  ;; and 0.33 seconds with this entire function a no-op.
  (do ((j to-start (1+ j)))
      ((= j to-end))
    ;; by having only one real array reference here
    ;; we win in the array cache.
    (setf (aref to-array j)
          ;; 3.4 seconds without the %p-contents-offset.
          (%p-contents-offset translation-table (1+ (aref to-array j))))))
