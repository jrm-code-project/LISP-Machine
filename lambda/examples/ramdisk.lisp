;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10; Fonts:(CPTFONTB) -*-

;;; RAMDISK, FOR TESTING PROGRAMS THAT DEAL WITH THE DISK.
;;; 29-May-86 16:39:40 -GJC

;;; FILEDISK, to make a file on the local file system appear as a disk.
;;; 11-Jun-86 12:50:28 -GJC


;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;;  licensing and release information.

;;; *********************************************************
;;; *********************************************************
;;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;;; *** information contained in this example is subject  ***
;;; *** to change without notice. The ways of doing       ***
;;; *** the things contained in the example may change    ***
;;; *** between system releases. Some techniques which    ***
;;; *** are mere examples in one release may become built ***
;;; *** in system features in the next release. Use good  ***
;;; *** judgement when copying these techniques. Most     ***
;;; *** examples have been motivated by specific customer ***
;;; *** requests, and may not be the best engineered      ***
;;; *** or most efficient solution for someone else.      ***
;;; *********************************************************
;;; *********************************************************


(SI:DEFINE-HOST "RAMDISK"
  :HOST-NAMES '("RAMDISK")
  :MACHINE-TYPE :KLUDGE
  :SYSTEM-TYPE :KLUDGE
  :RAMDISK '(T))


(PUSHNEW :RAMDISK NETWORK:*NETWORK-PROTOCOLS*)

(SI:DEFINE-HOST "FILEDISK"
   :host-names '("FILEDISK")
   :MACHINE-TYPE :KLUDGE
   :system-type :KLUDGE
   :filedisk '(t))

(PUSHNEW :FILEDISK NETWORK:*NETWORK-PROTOCOLS*)


(DEFVAR *RAMDISK* NIL)

(NET:DEFINE-NETWORK-FUNCTION (NETI:GET-REMOTE-DISK-UNIT :RAMDISK) (HOST DISK-UNIT USE INITP WRITE-P)
  USE INITP WRITE-P
  (OR (CADR (ASSQ DISK-UNIT *RAMDISK*))
      (FERROR NIL "~A unit ~S not available" HOST DISK-UNIT)))

(DEFUN ENABLE-RAMDISK (SIZE UNIT)
  (CAR (PUSH (LIST UNIT (MAKE-INSTANCE 'RAMDISK :BLOCKS SIZE))
             *RAMDISK*)))


(DEFFLAVOR RAMDISK
         (STRING BLOCKS (UNIT 0))
         ()
  (:INITABLE-INSTANCE-VARIABLES BLOCKS))

(DEFMETHOD (RAMDISK :AFTER :INIT) (PLIST)
  PLIST
  (WRITE-DATA (SETQ STRING (MAKE-ARRAY (* BLOCKS 1024) :TYPE 'ART-STRING))
              '(STRING  1)   0 "LABL"           ; MAGIC
              'INTEGER       1 1                ; LABEL VERSION
              'INTEGER       2 1                ; NUMBER OF CYLINDERS
              'INTEGER       3 1                ; NUMBER OF HEADS
              'INTEGER       4 BLOCKS           ; NUMBER OF BLOCKS PER TRACK
              'INTEGER       5 BLOCKS           ; NUMBER OF BLOCKS PER CYLINDER
              '(STRING  1)   6 ""               ; DEFAULT UCODE
              '(STRING  1)   7 ""               ; DEFAULT LOAD
              '(STRING  8)   8 (FORMAT NIL "Ramdisk-~D" (ROUND BLOCKS 100))     ; DRIVE MAKE
              '(STRING  8)  16 "(name)"         ; PACK NAME
              '(STRING 25)  24 "virtual disk drive"     ; COMMENT
              'INTEGER     128 1                ; NUMBER OF PARTITIONS
              'INTEGER     129 7                ; NUMBER OF WORDS PER PARTITION DESCRIPTOR
              '(STRING  1) 130 "DATA"           ; PARTITION DESCRIPTOR 0 - NAME
              'INTEGER     131 6                ;                        - START
              'INTEGER     132 (- BLOCKS 6)     ;                        - LENGTH
              '(STRING  4) 133 "empty partition"))      ;                - COMMENT


(defvar *ramdisk-blt-p* t)

(DEFMETHOD (RAMDISK :READ) (RQB ADDRESS)
  (cond (*ramdisk-blt-p*
         (check-arg address (and (not (< address 0))
                                 (not (> (+ (si:rqb-npages rqb) address) blocks)))
                    "a legal disk address")
         (let ((word-address (* address 256)))
           (%blt (%pointer-plus (%pointer-plus string (si:array-data-offset string))
                                word-address)
                 (%pointer-plus (%pointer-plus rqb (si:array-data-offset rqb))
                               (quotient (si:array-index-offset (si:rqb-buffer rqb)) 2))
                 (* (si:rqb-npages rqb) 256)
                 1)))
        ('else
         (LET ((byte-address (* address 1024))
               (NBYTES (* (SI:RQB-NPAGES RQB) 1024)))
           (COPY-ARRAY-PORTION STRING
                               byte-address
                               (+ byte-address NBYTES)
                               (SI:RQB-8-BIT-BUFFER RQB)
                               0
                               NBYTES)))))

(DEFMETHOD (RAMDISK :WRITE) (RQB ADDRESS)
  (cond (*ramdisk-blt-p*
         (check-arg address (and (not (< address 0))
                                 (not (> (+ (si:rqb-npages rqb) address) blocks)))
                    "a legal disk address")
         (let ((word-address (* address 256)))
           (%blt (%pointer-plus (%pointer-plus rqb (si:array-data-offset rqb))
                                (quotient (si:array-index-offset (si:rqb-buffer rqb)) 2))
                 (%pointer-plus (%pointer-plus string (si:array-data-offset string))
                                word-address)
                 (* (si:rqb-npages rqb) 256)
                 1)))
        ('else
         (LET ((NBYTES (* (SI:RQB-NPAGES RQB) 1024))
               (byte-address (* address 1024)))
           (COPY-ARRAY-PORTION (SI:RQB-8-BIT-BUFFER RQB)
                               0
                               NBYTES
                               STRING
                               byte-address
                               (+ byte-address NBYTES))))))

(DEFMETHOD (RAMDISK :DISPOSE) ()
  NIL)


(DEFMETHOD (RAMDISK :UNIT-NUMBER) ()
  UNIT)

(DEFMETHOD (RAMDISK :MACHINE-NAME) ()
  "RAMDISK")

(DEFUN WRITE-DATUM (STRING KIND WORD VALUE)
  (COND ((EQ KIND 'INTEGER)
         (DO ((J 0 (1+ J)))
             ((= J 4) VALUE)
           (SETF (AREF STRING (+ J (* WORD 4))) (LDB (BYTE 8 (* J 8)) VALUE))))
        ((EQ (CAR KIND) 'STRING)
         (LET ((LENGTH (CADR KIND)))
           (COPY-ARRAY-PORTION VALUE
                               0
                               (LENGTH VALUE)
                               STRING
                               (* WORD 4)
                               (+ (* WORD 4) (LENGTH VALUE)))
           (FILL STRING 0
                 :START (+ (* WORD 4) (LENGTH VALUE))
                 :END (* 4 (+ WORD LENGTH))))
         VALUE)))

(DEFUN READ-DATUM (STRING KIND WORD)
  (COND ((EQ KIND 'INTEGER)
         (DO ((J 0 (1+ J))
              (N 0 (DPB (AREF STRING (+ J (* WORD 4))) (BYTE 8 (* J 8)) N)))
             ((= J 4) N)))
        ((EQ (CAR KIND) 'STRING)
         (LET ((LENGTH (CADR KIND)))
           (LET ((S (SUBSTRING STRING (* WORD 4) (* 4 (+ WORD LENGTH)))))
             (SUBSTRING S 0 (STRING-SEARCH-CHAR 0 S)))))))


(DEFUN WRITE-DATA (STRING &REST KIND-WORD-VALUE)
  (DO ((L KIND-WORD-VALUE (CDDDR L)))
      ((NULL L))
    (WRITE-DATUM STRING (CAR L) (CADR L) (CADDR L))))


(defvar *filedisk* nil)

(defflavor filedisk
         (file unit)
         ()
  :initable-instance-variables)


(defmethod (filedisk :print-self) (stream &rest ignored)
  (format stream "#<FILEDISK ~A>" (and file (fs:file-truename file))))

(defmethod (filedisk :close-file) ()
  (and file (fs:lmfs-close-file file))
  (setq file nil))

(defmethod (filedisk :file) ()
  file)

(defun enable-filedisk (filename unit)
  (let ((pathname (fs:parse-pathname filename)))
    (or (eq (send pathname :host)
            si:local-host)
        (ferror nil "Pathname ~S not on local host" pathname))
    (let ((file (FS:LOOKUP-FILE (FS:PATHNAME-RAW-DIRECTORY PATHNAME)
                                (FS:PATHNAME-RAW-NAME PATHNAME)
                                (FS:PATHNAME-RAW-TYPE PATHNAME)
                                (OR (fs:PATHNAME-RAW-VERSION pathname) :NEWEST)
                                :ERROR
                                NIL
                                T)))
    (push (list unit (make-instance 'filedisk
                                    :file file
                                    :unit unit))
          *filedisk*)
    (fs:file-truename file))))

(defun disable-filedisk (&optional unit &aux f)
  (cond ((not unit)
         (dolist (f (copy-list *filedisk*))
           (send (cadr f) :close-file)
           (setq *filedisk* (delq f *filedisk*))))
        ((setq f (assq unit *filedisk*))
         (send (cadr f) :close-file)
         (setq *filedisk* (delq f *filedisk*))
         f)
        ('else
         (ferror nil "unit not found: ~S" unit))))


(defun make-filedisk (filename &optional blocks)
  "writes FILENAME with length BLOCKS (* 1024), or updates label if blocks is NIL"
  (cond ((null blocks)
         (let ((*filedisk* nil))
           (unwind-protect
               (progn (enable-filedisk filename 0)
                      (send (cadr (car *filedisk*)) :update-label))
             (disable-filedisk))))
        ('else
         (check-type blocks (fixnum 6))
         (let ((string (make-string 1024)))
           (with-open-file (stream filename :direction :output)
             (WRITE-DATA STRING
                         '(STRING  1)   0 "LABL"   ; MAGIC
                         'INTEGER       1 1        ; LABEL VERSION
                         'INTEGER       2 1        ; NUMBER OF CYLINDERS
                         'INTEGER       3 1        ; NUMBER OF HEADS
                         'INTEGER       4 BLOCKS   ; NUMBER OF BLOCKS PER TRACK
                         'INTEGER       5 BLOCKS   ; NUMBER OF BLOCKS PER CYLINDER
                         '(STRING  1)   6 ""       ; DEFAULT UCODE
                         '(STRING  1)   7 ""       ; DEFAULT LOAD
                         '(STRING  8)   8 (FORMAT NIL "FileDisk-~D" (ROUND BLOCKS 100))    ; DRIVE MAKE
                         '(STRING  8)  16 (send (send stream :truename) :name)
                         '(STRING 25)  24 (send (send stream :truename) :string-for-printing)
                         'INTEGER     128 1        ; NUMBER OF PARTITIONS
                         'INTEGER     129 7        ; NUMBER OF WORDS PER PARTITION DESCRIPTOR
                         '(STRING  1) 130 "DATA"   ; PARTITION DESCRIPTOR 0 - NAME
                         'INTEGER     131 6        ;                        - START
                         'INTEGER     132 (- BLOCKS 6)     ;                        - LENGTH
                         '(STRING  4) 133 "empty partition")       ;                - COMMENT
             (send stream :string-out string)
             (fill string #\*)
             (dotimes (j (quotient 1024 64))
               (setf (aref string (* j 64)) #\return))
             (dotimes (j (1- blocks))
               (send stream :string-out string)))))))


(NET:DEFINE-NETWORK-FUNCTION (NETI:GET-REMOTE-DISK-UNIT :FILEDISK) (HOST DISK-UNIT USE INITP WRITE-P)
  USE INITP WRITE-P
  (OR (CADR (ASSQ DISK-UNIT *FILEDISK*))
      (FERROR NIL "~A unit ~S not available" HOST DISK-UNIT)))

;;; now for the required usual messages
;;; :read :write :dispose :unit-number :machine-name

(defmethod (filedisk :unit-number) ()
  unit)

(defmethod (filedisk :machine-name) ()
  (send (fs:file-truename file) :string-for-printing))

(defmethod (filedisk :dispose) ()
  nil)


;; we can use fs:lm-disk-read and fs:lm-disk-write
;; using info in the fs:file-map


;; MAP-NBLOCKS = number of chunks
;; MAP-BLOCK-LOCATION = location of chunk
;; MAP-BLOCK-SIZE = size of chunk in bits.


(defmethod (filedisk :read) (rqb address)
  (transfer-data-via-map rqb address
                         (fs:file-map file)
                         :read))


(defmethod (filedisk :write) (rqb address)
  (transfer-data-via-map rqb address (fs:file-map file)
                         :write))

(defun transfer-data-via-map (rqb address map operation)
  (let ((amount-wanted (si:rqb-npages rqb))
        (function (ecase operation
                    (:read #'fs:lm-disk-read)
                    (:write #'fs:lm-disk-write))))
    (do ((search-index 0 (+ section-size search-index))
         (MAP-INDEX 0 (1+ MAP-INDEX))
         (n-sections (fs:map-nblocks map))
         (section-size))
        ((= MAP-INDEX n-sections)
         (ferror nil "index out of range"))
      (setq section-size (ceiling (fs:map-block-size map MAP-INDEX) (* 1024 8)))
      (let* ((section-offset (- address search-index))
             (amount-available (- section-size section-offset)))
        (COND ((< SECTION-OFFSET 0)
               (FERROR NIL "index out of range"))
              ((NOT (< AMOUNT-AVAILABLE AMOUNT-WANTED))
               ;; easy case. fits in a region
               (RETURN
                 (FUNCALL FUNCTION RQB
                          (+ (FS:MAP-BLOCK-LOCATION MAP MAP-INDEX)
                             SECTION-OFFSET)
                          AMOUNT-WANTED)))
              ((> AMOUNT-AVAILABLE 0)
               ;; in correct section, but not big enough for whole transfer
               (SI:WITH-DISK-RQB (TEMP-RQB AMOUNT-AVAILABLE)
                 (when (eq operation :write)
                     (copy-array-portion (si:rqb-buffer rqb)
                                         0
                                         (times AMOUNT-AVAILABLE 512)
                                         (SI:RQB-BUFFER TEMP-RQB)
                                         0
                                         (times AMOUNT-AVAILABLE 512)))
                 (FUNCALL FUNCTION
                          TEMP-RQB
                          (+ (FS:MAP-BLOCK-LOCATION MAP MAP-INDEX)
                             SECTION-OFFSET)
                          AMOUNT-AVAILABLE)
                 (WHEN (EQ OPERATION :READ)
                     (COPY-ARRAY-PORTION (SI:RQB-BUFFER TEMP-RQB)
                                         0
                                         (TIMES AMOUNT-AVAILABLE 512)
                                         (SI:RQB-BUFFER RQB)
                                         0
                                         (TIMES AMOUNT-AVAILABLE 512))))
               (INCF MAP-INDEX)
               (DO ((RQB-INDEX AMOUNT-AVAILABLE (+ amount-available rqb-index)))
                   ((= RQB-INDEX AMOUNT-WANTED))
                 (IF (= MAP-INDEX N-SECTIONS) (FERROR NIL "transfer address and size out of range"))
                 (SETQ SECTION-SIZE (CEILING (FS:MAP-BLOCK-SIZE MAP MAP-INDEX) (* 1024 8)))
                 (SETQ AMOUNT-AVAILABLE (MIN SECTION-SIZE (- AMOUNT-WANTED RQB-INDEX)))
                 (SI:WITH-DISK-RQB (TEMP-RQB AMOUNT-AVAILABLE)
                   (WHEN (EQ OPERATION :WRITE)
                     (COPY-ARRAY-PORTION (SI:RQB-BUFFER RQB)
                                         (* RQB-INDEX 512)
                                         (* (+ RQB-INDEX AMOUNT-AVAILABLE) 512)
                                         (SI:RQB-BUFFER TEMP-RQB)
                                         0
                                         (TIMES AMOUNT-AVAILABLE 512)))
                   (FUNCALL FUNCTION TEMP-RQB (FS:MAP-BLOCK-LOCATION MAP MAP-INDEX) AMOUNT-AVAILABLE)
                   (WHEN (EQ OPERATION :READ)
                     (COPY-ARRAY-PORTION (SI:RQB-BUFFER TEMP-RQB)
                                         0
                                         (TIMES AMOUNT-AVAILABLE 512)
                                         (SI:RQB-BUFFER RQB)
                                         (* RQB-INDEX 512)
                                         (* (+ RQB-INDEX AMOUNT-AVAILABLE) 512))))
                 (INCF MAP-INDEX))
               (RETURN T)))))))


(defmethod (filedisk :total-pages) ()
  (do ((j 0 (1+ j))
       (n (fs:map-nblocks (fs:file-map file)))
       (sum 0 (+ (ceiling (fs:map-block-size (fs:file-map file) j) (* 1024 8)) sum)))
      ((= j n)
       sum)))

(defmethod (filedisk :describe-map) ()
  (format t "~&~S has ~D pages in ~D sections:~%"
          file
          (send self :total-pages)
          (fs:map-nblocks (fs:file-map file)))
  (dotimes (j (fs:map-nblocks (fs:file-map file)))
    (format t "[~3D] ~3D at ~6D~%"
            j
            (ceiling (fs:map-block-size (fs:file-map file) j) (* 1024 8))
            (fs:map-block-location (fs:file-map file) j))))

(defmethod (filedisk :update-label) (&optional (verbose t))
  ;; update the label to reflect the actual size of the map.
  (si:with-disk-rqb (rqb 1)
    (send self :read rqb 0)
    (let ((string (si:rqb-8-bit-buffer rqb)))
      (let ((label-blocks (read-datum string 'integer 4))
            (actual-blocks (send self :total-pages)))
        (cond ((= label-blocks actual-blocks)
               (and verbose
                    (format t "~&~S has ~D blocks. No need to change.~%" self label-blocks)))
              ((< label-blocks actual-blocks)
               (and verbose
                    (format t "~&~S has ~D actual blocks. Growing label amount by ~D.~%"
                            self actual-blocks (- actual-blocks label-blocks)))
               (write-data string
                           'INTEGER       4 actual-blocks
                           'INTEGER       5 actual-blocks)
               (send self :write rqb 0))
              ((> label-blocks actual-blocks)
               (and verbose
                    (format t "~&~S has only ~D actual blocks. Reducing label amount by ~D.~%"
                            self actual-blocks (- label-blocks actual-blocks)))
               (write-data string
                           'INTEGER       4 actual-blocks
                           'INTEGER       5 actual-blocks)
               (send self :write rqb 0))))))
  self)

(defun randomize-file (filename &optional (percent 10))
  (let ((*filedisk* nil))
    (unwind-protect
        (let ((truename (enable-filedisk filename 0)))
          (si:with-disk-rqb (rqb 1)
            (let* ((unit (cadr (car *filedisk*)))
                   (buffer (si:rqb-buffer rqb))
                   (n (send unit :total-pages))
                   (m (ceiling (* n percent) 100)))
              (format t "~&Randomizing ~D out of the ~D pages of data in ~S~%"
                      m n truename)
              (dotimes (j m)
                (let ((b (random n)))
                  (send unit :read rqb b)
                  (dotimes (k (ceiling (* 512 percent) 100))
                    (let ((i (random 512)))
                      (setf (aref buffer i) (logxor (random (expt 2 16)) (aref buffer i)))))
                  (princ ".")
                  (send unit :write rqb b))))))
      (disable-filedisk))))


(compile-flavor-methods ramdisk filedisk)
