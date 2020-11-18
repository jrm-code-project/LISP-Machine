;;; -*- Mode:LISP; Package:TCP-APPLICATION; Readtable:CL; Base:10 -*-

#||

  Copyright LISP Machine, Inc. 1985, 1987
   See filename "Copyright.Text" for
  licensing and release information.


This is some simple imagen support code, more extensive capabilities
such as multiple fonts, spooling, and other impress support are available as a
seperate product from LMI. Users who want to spool impress files to other hosts
(e.g. some Unix machine) may want to use internals of this code to do the hairy stuff.

||#


(defun make-printer-imagen (address)
  "Set the default printer to be an imagen at the given address"
  (check-type address string)
  (setq si:*default-printer* (list :imagen address))
  (setq si:*default-bit-array-printer* (list :imagen address)))

;;;***IMAGEN STATUS***

(defvar *imagen-status-ok-wait* 3
  "if not-null the number of seconds to pause before trying to get a
go-ahead status from the imagen")

(defvar *imagen-status-ok-wait-tries* 60)

(defun imagen-status-ok-to-print (address &optional (verbose t))
  (udp:using-udp-socket address
                        (sym ipport-imagen)
                        "Imagen Status"
     #'(lambda (socket header)
         (if *imagen-status-ok-wait*
             (do ((j 0 (1+ j)))
                 ((imagen-status-ok-to-print-1 address socket header verbose)
                  t)
               (if (=  (1- *imagen-status-ok-wait-tries*)
                       (mod j *imagen-status-ok-wait-tries*))
                   (cerror "continue waiting for it"
                           "Imagen printer at ~S may be wedged" address))
               (global:process-sleep (floor (* *imagen-status-ok-wait* 60) 1)
                                     "Imagen Queue Wait"))
           (imagen-status-ok-to-print-1 address socket header verbose)))
     #'(lambda ()
         (format *error-output* "~&UDP is down."))))

(defun imagen-status-ok-to-print-1 (address socket header verbose)
  (let ((status (get-imagen-status address socket header)))
    (cond ((null status)
           (when verbose
             (format t "~&Imagen printer at ~S not responding~%" address))
           nil)
          ((zerop (getf status :protocols-accepted))
           (when verbose
             (format t "~&Printer busy, try again: ~A~%" (getf status :engine-status)))
           nil)
          ('else
           (when verbose
             (describe-imagen-status status *standard-output* verbose))
           t))))

(defun (:property :imagen si:print-status) (printer &optional (stream *standard-output*))
  (udp:using-udp-socket (cadr printer)
                        (sym ipport-imagen)
                        "Imagen Status"
     #'(lambda (socket header)
         (format stream "~&Status of: ~A~%" printer)
         (describe-imagen-status (get-imagen-status (cadr printer) socket header :tries 10) stream))
     #'(lambda ()
         (format stream "~&UDP is down."))))

(defun describe-imagen-status (status &optional (stream *standard-output*) (verbose t))
  (cond ((null verbose))
        ((null status)
         (format stream "~&Imagen printer not responding~%"))
        ((eq verbose t)
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

(defun get-imagen-status (address socket header &key (tries 1) (try-pause 1) (wait-pause 1))
  "Returns a plist describing the status of the imagen at the internet ADDRESS"
  (check-type socket udp:udp-socket)
  (check-type header (satisfies ip:ip-header-p))
  (check-type tries (fixnum 1))
  (check-type try-pause number)
  (check-type wait-pause number)
  (let (result)
    (dotimes (j tries)
      (if (setq result (get-imagen-status-1 address socket header (ROUND (* wait-pause 60))))
          (return-from get-imagen-status result))
      (global:process-sleep (ROUND (* try-pause 60)) "Imagen Status Sleep"))))

(defun get-imagen-status-1 (address socket header pause)
  (send socket :receive)
  (send socket :write-packet "" header nil nil)
  (process-wait-with-timeout "Imagen Status Reply"
                             pause
                             #'(lambda ()
                                 (send socket :listen)))
  (let ((elt (send socket :read-packet))
        (plist nil))
    (case (first elt)
      (:data                                                          ;Reply from imagen
       (setq plist (parse-imagen-status-packet (third elt)))
       (setf (getf plist :address) address)
       plist)
      (:close                                                         ;Socket closed from under us
       nil)
      (otherwise                                                      ;ICMP message
       nil))))

(defun big-endian-bytes (array &rest bytes)
  "Combine bytes into an integer, most significant come first"
  (do ((number 0 (+ (* number 256) (aref array (car l))))
       (l bytes (cdr l)))
      ((null l) number)))

(defun parse-imagen-status-packet (array)
  (do* ((plist (list :status-byte (big-endian-bytes array 2)
                     :age (big-endian-bytes array 4 5 6 7)
                     :protocols-supported (big-endian-bytes array 8 9 10 11)
                     :protocols-accepted  (big-endian-bytes array 12 13 14 15)
                     :engine-status (substring array
                                               (big-endian-bytes array 16 1)
                                               (+ (big-endian-bytes array 16 1)
                                                  (big-endian-bytes array 18 19)))))
        (problems '((#x01 :other)
                    (#x02 :no-paper)
                    (#x04 :paper-jam)
                    (#x08 :lacking-consumables)
                    (#xf0 :job-in-progress))
                  (cdr problems))
        (status-byte  (getf plist :status-byte)))
       ((null problems) plist)
    (unless (zerop (logand (caar problems) status-byte))
      (push (cadar problems) (getf plist :problems)))))

;;;***PRINT-FILE and PRINT-STREAM***

(defun (:property :imagen si:print-file) (printer filename &rest options)
  (let ((address (cadr printer))
        (verbose (getf options :verbose nil)))
    (when (imagen-status-ok-to-print address verbose)
      (if (probe-file filename)
          (with-open-file (stream filename)
            (let ((date (file-write-date stream)))
              (when date
                (setq options
                      (append options `(:file-date ,(time:print-universal-time date nil nil :dd-mmm-yyyy))))))
            (print-stream-to-imagen stream address options))
        (format t "~&File not found: ~S~%" filename)))))

(defun (:property :imagen si:print-stream) (printer stream &rest options)
  (let ((address (cadr printer))
        (verbose (getf options :verbose nil)))
    (when (imagen-status-ok-to-print address verbose)
      (print-stream-to-imagen stream address options))))

(defvar *imagen-default-copies* 1 "Copies of each document")
(defvar *imagen-default-form-length* 66 "Lines per logical page")
(defvar *imagen-default-form-width* 80 "Columns per logical page; 80 = portrait, 132 = landscape")
(defvar *imagen-default-line-wrapping* t "Wrap lines?")
(defvar *imagen-default-forms-per-page* 1 "Logical pages per physical page")
(defvar *imagen-default-left-margin* 0 "Blank columns at left margin")
(defvar *imagen-default-line-numbers* nil "T if lines should be numbered")
(defvar *imagen-default-page-headings* t "Page Headings?")

(defparameter *imagen-writes-out* 8 "Outstanding write requests to imagen")

(defun print-owner ()
  (if (and si:user-id (not (string-equal si:user-id "")))
      si:user-id
    "(not logged in)"))

(defun print-stream-to-imagen (file-stream address &optional options)
  (check-type address string)
  (let ((tcp:*tcp-stream-whostate* "Imagen Close"))
    (with-open-stream (tcp-stream (let ((tcp:*tcp-stream-whostate* "Imagen Open"))
                                    (open-easy-tcp-stream address
                                                          (sym-value 'IPPORT-IMAGEN)
                                                          nil
                                                          :direction :output
                                                          :output-buffers *imagen-writes-out*
                                                          :keyword "Imagen Printer")))
      (let* ((format (getf options :format :text))
             (copies (getf options :copies *imagen-default-copies*))
             (form-length (getf options :form-length *imagen-default-form-length*))
             (form-width (getf options :form-width *imagen-default-form-width*))
             (line-wrapping (getf options :line-wrapping *imagen-default-line-wrapping*))
             (forms-per-page (getf options :forms-per-page *imagen-default-forms-per-page*))
             ;;forms-per-page = 1, form-width = 80  --> normal PORTRAIT
             ;;forms-per-page = 1, form-width = 132 --> normal LANDSCAPE
             ;;forms-per-page = 2, form-width = 80  --> two pages per page, side by side, LANDSCAPE
             ;;forms-per-page = 2, form-width = 132 --> two pages per page, one above other, PORTRAIT
             (left-margin (getf options :left-margin *imagen-default-left-margin*))
             (line-numbers (getf options :line-numbers *imagen-default-line-numbers*))
             (truename (global:send-if-handles file-stream :truename))
             (directory (when truename
                          (send (send truename :new-pathname :name nil :type nil :version nil) :string-for-printing)))
             (header-name (if truename
                              (send (send truename :new-directory nil) :string-for-host)
                            "Unnamed Stream"))
             (name (if truename
                       (send truename :string-for-printing)
                     "Unnamed Stream"))
             (spool-date (time:print-current-time nil :dd-mmm-yyyy))
             (file-date (getf options :file-date spool-date))
             (page-headings (getf options :page-headings *imagen-default-page-headings*))
             (page-list (getf options :page-list))
             (stream nil)
             (tcp:*tcp-stream-whostate* "Imagen Output"))
        (setq stream (ftp:make-ascii-translating-output-stream tcp-stream nil))
        (format stream
                "@document(language ~A,jamresistance on,owner ~S,host ~S~@[,directory ~S~],name ~S~
,jobheader on,spooldate ~S~:[~*~;,copies ~D,pagecollation on~],pagereversal on~
,formlength ~D,formwidth ~D,formsperpage ~D,leftmargin ~D)"
                (case format
                  (:impress "Impress")
                  (:tek "Tektronix")
                  (otherwise "Printer"))
                (print-owner)
                (send si:local-host :name)      ;Local host
                directory                       ;Host and directory for Job Header
                header-name                     ;Name for Job Header
                spool-date                      ;Date for Job Header
                (> copies 1)                    ;Conditional for multiple-copy options
                copies                          ;Number of copies
                form-length                     ;lines per logical page
                form-width                      ;columns per logical page
                forms-per-page                  ;logical pages per physical pages
                left-margin                     ;blank columns at left edge of each logical page
                )
        (ecase format
          ((:text nil)
           (global:stream-copy-until-eof file-stream
                                         (make-paginating-stream tcp-stream
                                                                 (and page-headings (list name t file-date))
                                                                 (if page-headings 2 0)
                                                                 line-numbers
                                                                 form-length
                                                                 form-width
                                                                 line-wrapping
                                                                 left-margin
                                                                 t
                                                                 page-list)))
          (:ascii
           (global:stream-copy-until-eof file-stream stream))
          ((:impress :tek)
           (global:stream-copy-until-eof file-stream tcp-stream)))))))

;;;Here is a function that wraps a stream that paginates around an OUTPUT stream
(defun make-paginating-stream (stream page-header header-length line-numbers form-length
                               form-width line-wrapping left-margin &optional (ascii t) page-list)
  (let* ((current-line 0)                       ;line number on page
         (current-column 0)                     ;column number on page
         (chars-per-line (- form-width left-margin))
         (lines-per-page (- form-length header-length))
         (page-number 0)                        ;The page number we are printing
         (line-number 1)                        ;The line of the file we are printing
         (crlf (make-string 2)))
    (labels ((on-page ()
               (or (null page-list)
                   (member page-number page-list)))
             (crlf ()
              (when (on-page)
                (if ascii
                    (send stream :string-out crlf)
                  (send stream :tyo #\RETURN)))
              (setq current-column 0)
              (when (> (incf current-line) lines-per-page)
                (setq current-line 0)))
             (set-item (item)
              (cond ((null item) nil)
                    ((eq item t) (format nil "~D" page-number))
                    (t item)))
             (check-header (continuation)
              (when (zerop current-column)
                (when (zerop current-line)
                  (incf page-number)
                  (when (and (plusp header-length) (on-page))
                    (let* ((left (set-item (first page-header)))
                           (left-length (if left (length left) 0))
                           (center (set-item (second page-header)))
                           (center-length (if center (length center) 0))
                           (right (set-item (third page-header)))
                           (right-length (if right (length right) 0))
                           (total-length (+ left-length center-length right-length))
                           (pad-length (- chars-per-line total-length)))
                      (format stream "~V@A~V@A~V@A"
                              left-length
                              left
                              (+ (floor pad-length 2) center-length)
                              center
                              (+ (ceiling pad-length 2) right-length)
                              right))
                    (dotimes (i header-length)
                      (crlf)))
                  (setq current-line 1))
                (when line-numbers
                  (when (on-page)
                    (format stream "~6D~:[ ~;*~] " line-number continuation))
                  (setq current-column 8))))
             (output-character (char)
              (check-header nil)
              (case (int-char char)
                ((#\RETURN #\LINE)
                 (incf line-number)
                 (crlf))
                (#\TAB
                 (incf current-column (- 8 (mod current-column 8)))
                 (if (and line-wrapping (>= current-column chars-per-line))
                     (let ((overflow (rem current-column chars-per-line)))
                       (crlf)
                       (check-header t)
                       (when (on-page)
                         (dotimes (i overflow)
                           (send stream :tyo (if ascii #o40 #\SPACE)))))
                   (when (on-page)
                     (send stream :tyo (if ascii #o11 #\TAB)))))
                (#\OVERSTRIKE
                 (decf current-column)
                 (when (on-page)
                   (send stream :tyo (if ascii #o10 #\OVERSTRIKE))))
                (#\PAGE
                 (unless (= current-line 1)
                   (setq current-line 0)
                   (setq current-column 0)
                   (when (on-page)
                     (send stream :tyo (if ascii #o14 #\PAGE)))))
                (otherwise
                 (when (and line-wrapping (= current-column chars-per-line))
                   (crlf)
                   (check-header t))
                 (incf current-column)
                 (when (on-page)
                   (send stream :tyo char))))))
      (setf (char crlf 0) #o15)                 ;ASCII CR
      (setf (char crlf 1) #o12)                 ;ASCII LF
      #'(lambda (operation &optional arg1 &rest args)
          (case operation
            (:tyo
             (output-character (int-char arg1)))
            (:string-out
             (do* ((start (or (first args) 0))
                   (end (or (second args) (string-length arg1)))
                   (i start (1+ i)))
                  ((>= i end))
               (output-character (char arg1 i))))
            (:which-operations
             '(:tyo :string-out)))))))

;;;Here is a function that wraps a stream that paginates around an INPUT stream.
;;;It is no longer necessary now that TCP buffered-stream has been defined
#|
(defun make-paginating-filter (file-stream page-header header-length line-numbers form-length
                               form-width line-wrapping left-margin &optional (ascii t))
  "Returns an input stream that translates a Lisp Machine stream into ASCII, and paginates as a line printer"
  ;;page-header is a list of three elements.  One is left justified, one centered, one right justified
  ;;An element that is NIL does not appear.  An element that is T is replaced with page number
  ;;If the list is null, no page header (or following blank lines) appears
  (let* ((char nil)                             ;The character we are saving to pass to user
         (current-line 0)                       ;line number on page
         (current-column 0)                     ;column number on page
         (chars-per-line (- form-width left-margin))
         (lines-per-page (- form-length header-length))
         (page-number 0)                        ;The page number we are printing
         (line-number 1)                        ;The line of the file we are printing
         (header-chars 0)                       ;Characters in header + CRLF's
         (header-limit 0)                       ;number of last line of header
         (header nil)                           ;The current page header
         (header-index 0)                       ;current character being printed from header
         (number-chars 8)                       ;Number of characters in a line-number
         (number-limit (1- number-chars))       ;Index of last char in line-number
         (number nil)                           ;The string with the line number
         (number-index 0)                       ;Index of current character in line-number
         (continuation nil)                     ;t if this is a continued line
         (state :read-character))               ;Input translating state
    (labels ((make-line-number ()
              (setq number (format nil "~6D~:[ ~;*~] " line-number continuation))
              (setq continuation nil)
              (setq number-index 0)
              :line-number)
             (set-item (item)
              (cond ((null item) nil)
                    ((eq item t) (format nil "~D" page-number))
                    (t item)))
             (make-header ()
              (incf page-number)
              (let* ((left (set-item (first page-header)))
                     (left-length (if left (length left) 0))
                     (pad1 nil)
                     (center (set-item (second page-header)))
                     (center-length (if center (length center) 0))
                     (pad2 nil)
                     (right (set-item (third page-header)))
                     (right-length (if right (length right) 0))
                     (total-length (+ left-length center-length right-length))
                     (pad-length (- chars-per-line total-length))
                     (crlfs (make-string (* 2 header-length))))
                (do ((index 0 (+ index 2))
                     (count header-length (1- count)))
                    ((zerop count))
                  (setf (aref crlfs index) #\RETURN)
                  (setf (aref crlfs (1+ index)) #\LINE))
                (setq pad1 (make-string (floor pad-length 2) :initial-element #\SPACE))
                (setq pad2 (make-string (ceiling pad-length 2) :initial-element #\SPACE))
                (setq header (format nil "~A~A~A~A~A~A" left pad1 center pad2 right crlfs))
                (setq header-chars (length header))
                (setq header-limit (1- header-chars))
                (setq header-index 0)
                :header))
             )
      #'(lambda (operation &rest ignore)
          (case operation
            (:tyi
             (loop
               (ecase state
                 (:read-character
                  (setq char (read-char file-stream nil nil))
                  (setq state (if (null char) :eof :next-state)))
                 (:next-state
                  (setq state (cond ((zerop current-column)
                                     (cond ((zerop current-line)
                                            (cond ((plusp header-length) (make-header))
                                                  (line-numbers (make-line-number))
                                                  (t :normal)))
                                           (line-numbers (make-line-number))
                                           (t :normal)))
                                    (t :normal))))
                 (:eof                          ;End of File seen -- return NIL
                  (return nil))
                 (:cr-needed                    ;Carriage return needed
                  (setq state :lf-needed)
                  (setq current-column 0)
                  (return (if ascii #o15 #\RETURN)))
                 (:lf-needed                    ;Line Feed needed
                  (setq state :read-character)
                  (when (> (incf current-line) lines-per-page)
                    (setq current-line 0))
                  (when ascii
                    (return #o12)))
                 (:line-number
                  (when (= number-index number-limit)
                    (setq state :next-state))
                  (incf current-column)
                  (return (prog1
                            (aref number number-index)
                            (incf number-index))))
                 (:header
                  (when (= header-index header-limit)
                    (setq state :next-state)
                    (setq current-line 1))
                  (let ((char (aref header header-index)))
                    (incf header-index)
                    (cond ((eq char #\RETURN)
                           (return (if ascii #o15 char)))
                          ((eq char #\LINE)
                           (if ascii (return #o12)))
                          (t (return char)))))
                 (:normal
                  (cond ((member char '(#\RETURN #\LINE))
                         (setq state :cr-needed)
                         (incf line-number))
                        ((= char #\TAB)
                         (incf current-column (- 8 (mod current-column 8)))
                         (setq state :read-character)
                         (when (and line-wrapping (>= current-column chars-per-line))
                           ;;Note: If page width not a multiple of 8, tabs that cross right margin
                           ;;should be split in middle.  They aren't
                           (setq continuation t)
                           (setq state :cr-needed))
                         (return (if ascii #o11 #\TAB)))
                        ((= char #\OVERSTRIKE)
                         (decf current-column)
                         (setq state :read-character)
                         (return (if ascii #o10 #\OVERSTRIKE)))
                        ((= char #\PAGE)
                         (setq state :read-character)
                         (unless (= current-line 1)     ;We suppress blank pages...
                           (setq current-column 0)
                           (setq current-line 0)
                           (setq state :read-character)
                           (return (if ascii #o14 #\PAGE))))
                        (t
                         (incf current-column)
                         (setq state :read-character)
                         (when (and line-wrapping (= current-column chars-per-line))
                           (setq continuation t)
                           (setq state :cr-needed))
                         (return char))))))))))))
|#

;;;****SCREEN DUMPS***

;;; for simplicity and speed we convert the bit array directly into impress inside the "Handle Terminal Q"
;;; process and then transmit it to the Imagen.  We should queue it somewhere.

(defvar *imagen-data-bytes* nil)
(defvar *imagen-data-size* nil "A list of (hsize vsize), of 32^2 chunks")
(defvar *imagen-notifications* t "T to send notification after the screen dump has been sent to printer")

(defun (:property :imagen si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  (when (null *imagen-data-bytes*)
    (setq *imagen-data-bytes* (make-string (/ (* 1024 1024) 8))))
  (let ((length (round (* (- right left) (- bottom top)) 8)))
    (cond ((> length (length *imagen-data-bytes*))
           (tv:notify nil "Array too big to process, need ~D bytes" length))
          (*imagen-data-size*
           (tv:notify nil "Last array still processing, try again later"))
          ((imagen-status-ok-to-print (cadr printer) nil)
           (unwind-protect
               (progn
                 (setup-imagen-data-bytes array left top (- right left) (- bottom top))
                 (transmit-imagen-data-bytes (cadr printer))
                 (when *imagen-notifications*
                   (tv:notify nil "Screen dump sent to Imagen")))
             (setq *imagen-data-size* nil)))
          (t
           (tv:notify nil "Imagen may be wedged; try (hardcopy-status)")))))

(defun (:property :imagen-data si:print-bit-array) (PRINTER ARRAY LEFT TOP RIGHT BOTTOM &rest ignore)
  (declare (ignore printer))
  ;; use this if you just want to setup the DATA for further processing.
  (when (null *imagen-data-bytes*)
    (setq *imagen-data-bytes* (make-string (/ (* 1024 1024) 8))))
  (setup-imagen-data-bytes array left top (- right left) (- bottom top)))

(defvar *bitrev-byte-table* nil)

(defun impress (stream &rest data)
  (dolist (datum data)
    (cond ((symbolp datum)
           (impress stream (or (get datum 'impress)
                               (error "Undefined impress symbol: ~S" datum))))
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
           (error"unknown impress datum: ~S" datum)))))

(setf (get 'SET_MAGNIFICATION 'IMPRESS) 236)
(setf (get 'SET_ABS_H 'IMPRESS)         135)
(setf (get 'SET_ABS_V 'IMPRESS)         137)
(setf (get 'BITMAP 'IMPRESS)            235)
(setf (get 'OPAQUE 'IMPRESS)              3)
(setf (get 'SET_HV_SYSTEM 'IMPRESS)     205)
(setf (get 'ENDPAGE 'IMPRESS)           219)
(setf (get 'EOF 'IMPRESS)               255)

(defun inches-to-points (x)
  (list :word (truncate (* x 300) 1)))

(defvar *imagen-max-magnification* 2)
(defvar *imagen-override-magnification* nil)
(defvar *imagen-paper* (list
                         :landscape-h-available 11.0
                         :landscape-v-available  8.5
                         :portrait-h-available   8.5
                         :portrait-v-available  11.0))

(defun imagen-magnification (bits inches)
  (declare (values magnification actual-size))
  (let ((magnification (or *imagen-override-magnification*
                           (do ((mag 0 (1+ mag))
                                (size (/ bits 300) (* size 2)))
                               ((or (= mag *imagen-max-magnification*)
                                    (> (* size 2) inches))
                                mag)))))
    (values magnification
            (* bits (expt 2 magnification) (/ 1.0 300.0)))))


(defvar *imagen-landscape-p* '(> (first *imagen-data-size*) (second *imagen-data-size*))
  "An expression to evaluate. If T then use landscape mode on imagen")

(defun transmit-imagen-data-bytes (address)
  (let ((tcp:*tcp-stream-whostate* "Imagen Close"))
    (with-open-stream (imagen-stream (let ((tcp:*tcp-stream-whostate* "Imagen Open"))
                                       (open-easy-tcp-stream address
                                                             (sym-value 'IPPORT-IMAGEN)
                                                             nil
                                                             :direction :output
                                                             :output-buffers *imagen-writes-out*
                                                             :keyword "Imagen Screen Dump")))
      (let* ((landscape-p (eval *imagen-landscape-p*))
             (magnification 0)
             (horizontal-magnification 0)
             (horizontal-available (getf *imagen-paper*
                                         (if landscape-p :landscape-h-available :portrait-h-available)))
             (horizontal-size 0)
             (horizontal-offset 0)
             (vertical-magnification 0)
             (vertical-available (getf *imagen-paper*
                                       (if landscape-p :landscape-v-available :portrait-v-available)))
             (vertical-size 0)
             (vertical-offset 0)
             (tcp:*tcp-stream-whostate* "Imagen Output"))
        (multiple-value-setq (horizontal-magnification horizontal-size)
          (imagen-magnification (third *imagen-data-size*) horizontal-available))
        (multiple-value-setq (vertical-magnification vertical-size)
          (imagen-magnification (fourth *imagen-data-size*) vertical-available))
        (setq magnification (max 0 (min horizontal-magnification vertical-magnification)))
        (unless (= magnification horizontal-magnification)
          (setq horizontal-size (* magnification (/ horizontal-size horizontal-magnification)))
          (setq horizontal-magnification magnification))
        (unless (= magnification vertical-magnification)
          (setq vertical-size (* magnification (/ vertical-size vertical-magnification)))
          (setq vertical-magnification magnification))
        (setq horizontal-offset (/ (- horizontal-available horizontal-size) 2.0))
        (setq vertical-offset (/ (- vertical-available vertical-size) 2.0))
        (when landscape-p
          (setq horizontal-offset (- horizontal-available horizontal-offset)))
        (format imagen-stream
                "@document(language impress,jobheader on,host ~S,name ~:[portrait~;landscape~]~D-~D-~D,spooldate ~S,owner ~S)"
                (send si:local-host :name)
                landscape-p
                (expt 2 magnification)
                (nth 2 *imagen-data-size*)
                (nth 3 *imagen-data-size*)
                (time:print-current-time nil :dd-mmm-yyyy)
                (print-owner))

        (if landscape-p
            (impress imagen-stream
                     'SET_MAGNIFICATION magnification
                     'SET_ABS_H (inches-to-points vertical-offset)
                     'SET_ABS_V (inches-to-points horizontal-offset)
                     'SET_HV_SYSTEM (list :byte
                                          0     ; pad
                                          1 1   ; ORIGIN: physical
                                          1 0   ; AXES: +90 deg (regular)
                                          1 1 1 ; Orientation: 270 from X.
                                          ))
          (impress imagen-stream
                   'SET_MAGNIFICATION magnification
                   'SET_ABS_H (inches-to-points horizontal-offset)
                   'SET_ABS_V (inches-to-points vertical-offset)))
        (impress imagen-stream
                 'BITMAP 'OPAQUE (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*))
        (send imagen-stream :string-out *imagen-data-bytes*
              0 (* 128 (nth 0 *imagen-data-size*) (nth 1 *imagen-data-size*)))
        (impress imagen-stream 'ENDPAGE 'EOF)))))

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
    (setq *32^32-chunk* (make-array '(32 32) :element-type 'bit))
    (setq *128-byte-chunk* (make-array 128 :element-type '(unsigned-byte 8) :displaced-to *32^32-chunk*)))
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
                (global:bitblt tv:alu-seta
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


(defun copy-array-portion-translated (from-array from-start from-end
                                      to-array to-start to-end
                                      translation-table)
  (declare (ignore from-end))
  ;; a candidate for microcompilation.
  (do ((source from-start (1+ source))
       (dest to-start (1+ dest)))
      ((= dest to-end))
    (setf (aref to-array dest) (aref translation-table (aref from-array source)))))

;;;***Option setting

(defun set-imagen-print-options (&optional &key
                                 (copies *imagen-default-copies* c-supplied)
                                 (form-length *imagen-default-form-length* fl-supplied)
                                 (form-width *imagen-default-form-width* fw-supplied)
                                 (line-wrapping *imagen-default-line-wrapping* lw-supplied)
                                 (forms-per-page *imagen-default-forms-per-page* fpp-supplied)
                                 (left-margin *imagen-default-left-margin* lm-supplied)
                                 (line-numbers *imagen-default-line-numbers* ln-supplied)
                                 (page-headings *imagen-default-page-headings* hd-supplied)
                                 (ok-wait *imagen-status-ok-wait* wt-supplied)
                                 (ok-wait-tries *imagen-status-ok-wait-tries* wtt-supplied)
                                 (notifications *imagen-notifications* not-supplied))
  (if (or c-supplied fl-supplied fw-supplied lw-supplied fpp-supplied lm-supplied
          ln-supplied hd-supplied wt-supplied wtt-supplied not-supplied)
      (setq *imagen-default-copies* copies
            *imagen-default-form-length* form-length
            *imagen-default-form-width* form-width
            *imagen-default-line-wrapping* line-wrapping
            *imagen-default-forms-per-page* forms-per-page
            *imagen-default-left-margin* left-margin
            *imagen-default-line-numbers* line-numbers
            *imagen-default-page-headings* page-headings
            *imagen-status-ok-wait* ok-wait
            *imagen-status-ok-wait-tries* ok-wait-tries
            *imagen-notifications* notifications)
    (let ((*print-base* 10.))
      (tv:choose-variable-values
        '((*imagen-default-copies*              "Copies"
                                                :documentation "Number of copies per document" :number)
          (*imagen-default-form-length*         "Page length (1 - 66)"
                                                :documentation "Lines Per Page" :number)
          (*imagen-default-form-width*          "Page width"
                                                :choose (80. 132.) princ
                                                :documentation "Columns Per Page" :number)
          (*imagen-default-line-wrapping*       "Line Wrapping"
                                                :documentation "Yes if lines wrap, No if they are clipped" :boolean)
          (*imagen-default-forms-per-page*      "Pages per sheet"
                                                :choose (1 2) princ
                                                :documentation "Number of logical pages per sheet of paper" :number)
          (*imagen-default-left-margin*         "Left margin"
                                                :documentation "Blank columns reserved at left margin" :number)
          (*imagen-default-line-numbers*        "Line numbers"
                                                :documentation "Yes to number the lines" :boolean)
          (*imagen-default-page-headings*       "Page Headings"
                                                :documentation "Yes to put page headings on the pages" :boolean)
          (*imagen-status-ok-wait*              "Seconds between Imagen polls"
                                                :documentation "How long to wait when Imagen is busy" :number-or-nil)
          (*imagen-status-ok-wait-tries*        "Retries before giving up"
                                                :documentation "How many times to try before deciding Imagen is stuck"
                                                :number)
          (*imagen-notifications*               "Screen Dump notifications"
                                                :documentation "Yes to be notified after dump sent to Imagen" :boolean))
                                                :label "Set Imagen Print Options")))
  nil)

;;;Can't just add to system menu now -- NETWORK loaded before TV, so in Cold Load,
;;;  system menu doesn't exist yet
(add-initialization "Put Imagen onto System Menu"
                    '(and (fboundp 'tv:add-to-system-menu-programs-column)
                          (tv:add-to-system-menu-programs-column "Imagen Options"
                                                                 '(tcpa:set-imagen-print-options)
                                                                 "Set print options for the Imagen Printer"))
                    '(:cold :first))
