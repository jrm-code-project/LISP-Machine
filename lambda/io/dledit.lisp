;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

;;; Disk label editor

;;; Simple routines for manipulating own label
;;; These are to be called by the user

(DEFUN SET-CURRENT-MICROLOAD (BAND &OPTIONAL UNIT)
  "Specify the MCR band to be used for loading microload at boot time.
Do PRINT-DISK-LABEL to see what bands are available and what they contain.
UNIT can be a string containing a machine's name, or /"CC/";
then the specified or debugged machine's current microload is set.
The last works even if the debugged machine is down.
UNIT can also be a disk drive number; however, it is the disk on
drive zero which is used for booting."
  (setq unit (default-disk-unit unit))
  (SET-CURRENT-BAND BAND UNIT T))

(DEFUN SET-CURRENT-BAND (BAND &OPTIONAL UNIT MICRO-P &AUX LABEL-INDEX)
  "Specify the LOD band to be used for loading the Lisp system at boot time.
If the LOD band you specify goes with a different microcode,
you will be given the option of selecting that microcode as well.  Usually, do so.

Do PRINT-DISK-LABEL to see what bands are available and what they contain.
UNIT can be a string containing a machine's name, or /"CC/";
then the specified or debugged machine's current band is set.
The last works even if the debugged machine is down.
UNIT can also be a disk drive number; however, it is the disk on
drive zero which is used for booting.

Returns T if the band was set as specified, NIL if not
 (probably because user said no to a query)."
  (setq unit (default-disk-unit unit))
  (with-decoded-disk-unit (unit unit
                                (FORMAT NIL "(SET-CURRENT-~:[BAND~;MICROLOAD~] ~D)"
                                        MICRO-P BAND))
    (with-disk-rqb (rqb disk-label-rqb-pages)
      (PROG ((UCODE-NAME (SELECT-PROCESSOR
                           (:CADR "MCR")
                           (:LAMBDA "LMC")
                           (:explorer "MCR")
                           )))
            (SETQ BAND (COND ((OR (SYMBOLP BAND) (STRINGP BAND))
                              (STRING-UPCASE (STRING BAND)))
                             (T (FORMAT NIL "~A~D"
                                        (COND (MICRO-P UCODE-NAME)
                                              (T "LOD"))
                                        BAND))))
            (OR (STRING-EQUAL (SUBSTRING BAND 0 3)
                              (IF MICRO-P UCODE-NAME "LOD"))
                (FQUERY NIL "The specified band is not a ~A band.  Select it anyway? "
                        (IF MICRO-P UCODE-NAME "LOD"))
                (RETURN NIL))

            (MULTIPLE-VALUE (NIL NIL LABEL-INDEX)
              (FIND-DISK-PARTITION-FOR-READ BAND RQB UNIT))     ;Does a READ-DISK-LABEL

            (ecase (get-disk-fixnum rqb 1)
              (1 (PUT-DISK-STRING RQB BAND (COND (MICRO-P 6) (T 7)) 4))
              (2 (if (numberp unit)
                     (if micro-p
                         (set-default-microload-V2 rqb band)
                       (set-default-load-band-V2 rqb band))
                   (ferror "can't set the default band remotely for V2 labels")))
              )

            (IF (NOT MICRO-P)
                (MULTIPLE-VALUE-BIND (NIL MEMORY-SIZE-OF-BAND UCODE-VERSION-OF-BAND)
                    (MEASURED-SIZE-OF-PARTITION BAND UNIT)
                  (LET ((CURRENT-UCODE-VERSION (current-microload-version unit))
                        (MACHINE-MEMORY-SIZE (cond ((and (numberp unit)
                                                         (= (get-disk-fixnum rqb 1) 2))
                                                    (page-partition-size-for-local-machine))
                                                   (t
                                                    (MEASURED-SIZE-OF-PARTITION "PAGE" UNIT)))))
                    (AND (> MEMORY-SIZE-OF-BAND MACHINE-MEMORY-SIZE)
                         (NOT (FQUERY NIL "~A requires a ~D block PAGE partition, but there is only ~D.  Select ~A anyway? "
                                      BAND MEMORY-SIZE-OF-BAND MACHINE-MEMORY-SIZE BAND))
                         (RETURN NIL))
                    (MULTIPLE-VALUE-BIND (BASE-BAND-NAME BASE-BAND-VALID)
                        (INC-BAND-BASE-BAND BAND UNIT)
                      (WHEN BASE-BAND-NAME
                        (FORMAT T "~%Band ~A is an incremental save with base band ~A."
                                BAND BASE-BAND-NAME)
                        (UNLESS BASE-BAND-VALID
                          (FORMAT T "~2%It appears that ~A's contents have been changed
 since ~A was dumped.  Therefore, booting ~A may fail to work!"
                                  BASE-BAND-NAME BAND BAND)
                          (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS "~%Select ~A anyway? "
                                          BAND)
                            (RETURN NIL)))))
                    (IF UCODE-VERSION-OF-BAND
                        (IF (EQ CURRENT-UCODE-VERSION UCODE-VERSION-OF-BAND)
                            (FORMAT T "~%The new current band ~A should work properly
with the ucode version that is already current." BAND)
                          (LET ((BAND-UCODE-PARTITION
                                  (FIND-MICROCODE-PARTITION RQB UCODE-VERSION-OF-BAND)))
                            (IF BAND-UCODE-PARTITION
                                (IF (FQUERY NIL "~A goes with ucode ~D, which is not selected.
Partition ~A claims to contain ucode ~D.  Select it? "
                                            BAND UCODE-VERSION-OF-BAND
                                            BAND-UCODE-PARTITION UCODE-VERSION-OF-BAND)
                                    (ecase (get-disk-fixnum rqb 1)
                                      (1 (PUT-DISK-STRING RQB BAND-UCODE-PARTITION 6 4))
                                      (2 (cond ((numberp unit)
                                                (set-default-microload-V2 rqb band-ucode-partition))
                                               (t
                                                (ferror "can't set remote V2 labels")))))
                                  (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
                                                  "~2%The machine may fail to boot if ~A is selected
 with the wrong microcode version.  It wants ucode ~D.
Currently ucode version ~D is selected.
Do you know that ~A will run with this ucode? "
                                                  BAND UCODE-VERSION-OF-BAND
                                                  CURRENT-UCODE-VERSION BAND)
                                    (RETURN NIL)))
                              ;; Band's desired microcode doesn't seem present.
                              (FORMAT T "~%~A claims to go with ucode ~D,
which does not appear to be present on this machine.
It may or may not run with other ucode versions.
Currently ucode ~D is selected."
                                      BAND UCODE-VERSION-OF-BAND CURRENT-UCODE-VERSION)
                              (UNLESS (FQUERY FORMAT:YES-OR-NO-P-OPTIONS
                                              "~%Should I really select ~A? " BAND)
                                (RETURN NIL))))))))
              ;; Here to validate a MCR partition.
              (WHEN (and (= (get-disk-fixnum rqb 1) 1)
                         (> LABEL-INDEX (- #o400 3)))
                (FORMAT T "~%Band ~A may not be selected since it is past the first page of the label.
The bootstrap prom only looks at the first page.  Sorry.")
                (RETURN NIL)))
            (WRITE-DISK-LABEL RQB UNIT)
            (RETURN T)))))

;;; Version 2 Disk label definitions.

(DefConst %DL-Current-Band 16.)
(DefConst %DL-Current-Microload 17.)

(DefConst %PT-Base 256.)
(DefConst %PT-Number-of-Partitions 2.)
(DefConst %PT-Size-of-Partition-Entries 3.)
(DefConst %PT-Partition-Descriptors 16.)

(DefConst %PD-Attributes 3.)
(DefConst %%Band-Type-Code 0010)
(DefConst %BT-Load-Band 0.)
(DefConst %BT-Microload 1.)
(DefConst %%CPU-type-code 1020)
(DefConst %CPU-chaparral #x+0000)
(DefConst %CPU-Generic-Band #x+FFFF)
(DefConst %%Default-indicator #o3201)



(defun set-default-microload-V2 (rqb band)
  "Sets the default microload partition. This function will search the partition table
  for the specified band and if it finds it, will set the default bit. It will then go
  and reset the default bit in other microcode bands that may be set." ;too much work!

  (PUT-DISK-STRING RQB BAND %DL-CURRENT-MICROLOAD 4)
  (multiple-value-bind (ignore ignore label-loc ignore)
      (find-disk-partition band rqb nil t)
    (when (and label-loc
               (= (ldb %%band-type-code (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                  %BT-Microload)
               (= (ldb %%CPU-type-code (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                  %CPU-chaparral))
      (Put-disk-Fixnum rqb (dpb 1. %%default-indicator
                                (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                       (+ label-loc %PD-Attributes))
;;
;; Now, go through the rest of the partitions looking for microload bands that are of this
;; processor type and reset their (possibly) turned on default bit.
;;
      (loop for index from (+ %pt-base  %pt-partition-descriptors)
            to   (+ %pt-base %pt-partition-descriptors
                    (* (get-disk-fixnum rqb (+ %pt-base %pt-number-of-partitions))
                       (get-disk-fixnum rqb (+ %pt-base %pt-size-of-partition-entries))))
            by   (get-disk-fixnum rqb (+ %pt-base %pt-size-of-partition-entries))
            do (if (and (not (= index label-loc))
                        (= (ldb %%Band-type-code
                                (get-disk-fixnum rqb (+ index %PD-attributes)))
                           %BT-microload)
                        (= (ldb %%CPU-type-code
                                (get-disk-fixnum rqb (+ index %PD-attributes)))
                           %CPU-chaparral))
                   (put-disk-fixnum rqb
                                    (dpb 0.
                                         %%Default-indicator
                                             (get-disk-fixnum rqb (+ index %PD-attributes)))
                                    (+ index %PD-attributes))))
           band)
    ))

(defun set-default-load-band-V2 (rqb band)
  "Sets the default loadband. This function will search the partition table
  for the specified band and if it finds it, will set the default bit. It will then go
  and reset the default bit in other load bands that may be set."

  (PUT-DISK-STRING RQB BAND %DL-CURRENT-BAND 4)
  (multiple-value-bind (ignore ignore label-loc ignore)
      (find-disk-partition band rqb nil t)
    (when (and label-loc
               (= (ldb %%band-type-code (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                  %BT-load-band)
               (= (ldb %%CPU-type-code (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                  %CPU-chaparral))
      (Put-disk-Fixnum rqb (dpb 1. %%default-indicator
                                (get-disk-fixnum rqb (+ label-loc %PD-Attributes)))
                       (+ label-loc %PD-Attributes))
;;
;; Now, go through the rest of the partitions looking for load bands that are of this
;; processor type and reset their (possibly) turned on default bit.
;;
      (loop for index from (+ %pt-base %pt-partition-descriptors)
            to   (+ %pt-base %pt-partition-descriptors
                    (* (get-disk-fixnum rqb (+ %pt-base %pt-number-of-partitions))
                       (get-disk-fixnum rqb (+ %pt-base %pt-size-of-partition-entries))))
            by   (get-disk-fixnum rqb (+ %pt-base %pt-size-of-partition-entries))
            do (if (and (not (= index label-loc))
                        (= (ldb %%Band-type-code
                                (get-disk-fixnum rqb (+ index %PD-attributes)))
                           %BT-load-band)
                        (= (ldb %%CPU-type-code
                                (get-disk-fixnum rqb (+ index %PD-attributes)))
                           %CPU-chaparral))
                   (put-disk-fixnum rqb
                                    (dpb 0.
                                         %%Default-indicator
                                         (get-disk-fixnum rqb (+ index %PD-attributes)))
                                    (+ index %PD-attributes))))
      band)                                     ;return name as successful completion.
    ))

(defun current-microload-version (unit)
  (get-microload-version-from-comment
    (partition-comment (current-microload unit) unit)))

(defun get-microload-version-from-comment (comment)
  (let* ((number-begin (string-search-char #\space comment))
         (first-part-of-comment (substring comment 0 number-begin))
         )
    (cond ((and (not (string-equal first-part-of-comment "ULAMBDA"))
                (not (string-equal first-part-of-comment "UCADR"))
                (not (string-equal first-part-of-comment "CONTROL")))
           nil)
          ((null number-begin)
           nil)
          (t
           (parse-number (substring comment (1+ number-begin)))))))

(DEFUN FIND-MICROCODE-PARTITION (RQB MICROCODE-VERSION
                                 &AUX N-PARTITIONS WORDS-PER-PART)
  (ecase (get-disk-fixnum rqb 1)
    (1
     (SETQ N-PARTITIONS (GET-DISK-FIXNUM RQB 200))
     (SETQ WORDS-PER-PART (GET-DISK-FIXNUM RQB 201)))
    (2
     (setq n-partitions (get-disk-fixnum rqb (+ 256. 2)))
     (setq words-per-part (get-disk-fixnum rqb (+ 256. 3)))))
  (IF ( WORDS-PER-PART 3)                      ;Partition comment
      NIL
    (DO ((I 0 (1+ I))
         (PARTITION-NAME)
         (COMMENT)
         (LOC (ecase (get-disk-fixnum rqb 1)
                (1 #o202)
                (2 (+ #o400 16.)))
              (+ LOC WORDS-PER-PART)))
        ((OR (= I N-PARTITIONS)
             ;; Bootstrap prom only searches first label page,
             ;; so don't consider any MCR partitions outside that!
             (case (get-disk-fixnum rqb 1)
               (1 (> (+ LOC WORDS-PER-PART) #o400))
               (2 nil))))
      (ecase (get-disk-fixnum rqb 1)
        (1
         (SETQ PARTITION-NAME (GET-DISK-STRING RQB LOC 4))
         (SETQ COMMENT (GET-DISK-STRING RQB (+ LOC 3) 16.)))
        (2
         (setq partition-name (get-disk-string rqb loc 4))
         (setq comment (get-disk-string rqb (+ loc 4) 16.))
         ))
      (cond ((eq (get-microload-version-from-comment comment)
                 microcode-version)
             (return partition-name))))))

(DEFUN CURRENT-MICROLOAD (&OPTIONAL UNIT)
  "Return the name of the current microload band.
UNIT can be a name of a machine, a number of a disk drive,
or a string containing CC."
  (setq unit (default-disk-unit unit))
  (CURRENT-BAND UNIT T))

(DEFUN CURRENT-BAND (&OPTIONAL UNIT MICRO-P)
  "Return the name of the current Lisp system (LOD) band.
UNIT can be a name of a machine, a number of a disk drive,
or a string containing CC."
  (setq unit (default-disk-unit unit))
  (with-decoded-disk-unit (unit unit "Reading Label")
    (with-disk-rqb (rqb disk-label-rqb-pages)
      (READ-DISK-LABEL RQB UNIT)
      (ecase (get-disk-fixnum rqb 1)
        (1 (GET-DISK-STRING RQB (IF MICRO-P 6 7) 4))
        (2 (find-v2-band rqb (if micro-p 1 0) nil))
         ))))

;; Copied from LAD: RELEASE-3.IO; DLEDIT.LISP#90 on 2-Oct-86 16:42:47
;; 10/10/86 (Already defined in SYS:IO;DISK) -rpp
;(Defun default-disk-unit (unit)
;  (if (null unit)
;      (select-processor
;       ((:cadr :lambda) 0)
;       (:explorer (explorer-lod-band-logical-unit)))
;    unit)
;  )


(defun explorer-lod-band-logical-unit ()
  (select-processor
    (:explorer
      (let ((phys-unit (%p-ldb (byte 6 0) (+ #o1775 si:a-memory-virtual-address))))
        (dpb (ldb (byte 3 3) phys-unit)
             (byte 3 1)
             (ldb (byte 1 0) phys-unit))))
    ((:lambda :cadr :falcon) (ferror nil "only for LMI//Explorer"))))

(defun find-v2-band (rqb partition-type ok-if-not-default)
  (let ((n-partitions (get-disk-fixnum rqb (+ 256. 2)))
        (words-per-part (get-disk-fixnum rqb (+ 256. 3))))
    (do ((loc (+ 256. 16.) (+ loc words-per-part))
         (parts-to-go n-partitions (1- parts-to-go)))
        ((zerop parts-to-go))
      (if (and (= (ldb (byte 8 0) (get-disk-fixnum rqb (+ loc 3))) partition-type)
               (or ok-if-not-default
                   (ldb-test (byte 1 26.) (get-disk-fixnum rqb (+ loc 3)))))
          (return (values (get-disk-string rqb loc 4) loc))))))

(defun v2-rqb-contains-this-part-p (rqb part-name)
  (let ((n-partitions (get-disk-fixnum rqb (+ 256. 2)))
        (words-per-part (get-disk-fixnum rqb (+ 256. 3))))
    (do ((loc (+ 256. 16.) (+ loc words-per-part))
         (parts-to-go n-partitions (1- parts-to-go)))
        ((zerop parts-to-go)
         nil)
      (if (string-equal part-name (get-disk-string rqb loc 4))
          (return t)))))

(defun page-partition-size-for-local-machine ()
  (nth-value 2 (page-partition-info-for-local-machine)))

(defun page-partition-info-for-local-machine (&aux unit offset size)
  (declare (values unit offset size))
  (select-processor
    (:explorer
      (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
        (setq unit (explorer-lod-band-logical-unit))
        (read-disk-label rqb unit)
        (cond ((not (= (get-disk-fixnum rqb 1) 2))
               (ferror "label not version 2")))
        (multiple-value-bind (page-band label-index)
            (find-v2-band rqb 2 t)
          (cond ((null page-band)
                 (ferror "can't find page band")))
          (setq offset (get-disk-fixnum rqb (+ label-index 1)))
          (setq size (get-disk-fixnum rqb (+ label-index 2)))))
      (values unit offset size))
    ((:cadr :lambda))))


(defun current-band-for-local-machine (micro-p)
  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
    (read-disk-label rqb (explorer-lod-band-logical-unit))
    (ecase (get-disk-fixnum rqb 1)
      (1 (get-disk-string rqb (if micro-p 6 7) 4))
      (2
       (let ((result (find-v2-band rqb (if micro-p 1 0) nil)))
         (cond ((null result)
;               (read-disk-label rqb 1)
;               (if (not (= (get-disk-fixnum rqb 1) 2))
;                   (ferror "inconsistant label versions"))
;               (find-v2-band rqb (if micro-p 1 0) nil)
                )
               (t
                result)))))))


(defun set-current-band-for-local-machine-v2 (part-name micro-p)
  (ferror nil "obsolete")
  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
    (do ((unit 0 (1+ unit))
         found-it)
        ((= unit 2)
         (if (null found-it)
             (ferror "can't find partition ~s" part-name)))
      (read-disk-label rqb unit)
      (if (not (= (get-disk-fixnum rqb 1) 2))
          (ferror "label not version 2"))
      (do ((n-parts-to-go (get-disk-fixnum rqb (+ 256. 2)) (1- n-parts-to-go))
           (words-per-part (get-disk-fixnum rqb (+ 256. 3)))
           (loc (+ 256. 16.) (+ loc words-per-part))
           )
          ((zerop n-parts-to-go))
        (let ((old (get-disk-fixnum rqb (+ loc 3))))
          (cond ((= (ldb (byte 8 0) old) (if micro-p 1 0))
                 (cond ((string-equal (get-disk-string rqb loc 4) part-name)
                        (if found-it
                            (ferror "two partitions named the same!"))
                        (put-disk-fixnum rqb (dpb 1 (byte 1 26.) old) (+ loc 3))
                        (setq found-it t))
                       (t
                        (put-disk-fixnum rqb (dpb 0 (byte 1 26.) old) (+ loc 3))))))))
      (write-disk-label rqb unit)
      )))


(defun loaded-ucode-partition-and-unit ()
  (declare (values part-name unit))
  (select-processor
    ((:lambda :cadr)
     (loaded-ucode-partition-and-unit-lambda))
    (:explorer
      (loaded-ucode-partition-and-unit-explorer))))

(defsubst numeric-aref (array index)
  (let ((char (aref array index)))
    (and (>= char 60) (<= char 71)
         (- char 60))))

;;; we really need to have a master partition name decoding function
;;; and partition canonical-types
(defun loaded-ucode-partition-and-unit-lambda ()
  (declare (values part-name unit))
  (let ((conf-partition (si:%processor-conf-micro-band si:*my-proc-conf*)))
    (if (zerop conf-partition)
        ;;; uload band not set in proc-conf; get it from the disk
        (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
          (read-disk-label rqb 0)
          (if (not (= (get-disk-fixnum rqb 1) 1))
              (ferror "lambda must have V1 disk label"))
          (values (string-append (get-disk-string rqb 6 4)) 0))
      ;;; uload band is set in proc-conf (presumably by newboot)
      ;;; get the partition name from there
      (let ((string (decode-partition-name conf-partition)))
        (values string (or (numeric-aref string 0) 0))))))

(defun encode-partition-name (name &aux (result 0))
  (check-type name string)
  (dotimes (c (string-length name) result)
    (setq result (dpb (aref name c) (byte 8 (* c 8)) result))))

(defun decode-partition-name (number)
  (check-type number (integer 1))
  (do* ((count 0 (add1 count))
        (length (ceiling (integer-length number) 8))
        (string (make-string length)))
       ((= count length) string)
    (setf (aref string count) (ldb (byte 8 (* count 8)) number))))

(defun loaded-ucode-partition-and-unit-explorer ()
  (declare (values part-name unit))
  (let ((part-name (let ((string (make-string 4))
                         (adr (%pointer-plus a-memory-virtual-address #o1776)))
                     (dotimes (i 4)
                       (aset (%p-ldb (byte 8 (* i 8)) adr) string i))
                     string)))
    (let ((unit (explorer-lod-band-logical-unit)))
      (if (null unit)
          nil
        (values part-name unit)))))

(defun unit-for-partition (part-name)
  (select-processor
    (:cadr 0)
    (:lambda 0)
    (:explorer (unit-for-partition-explorer part-name))))

;(defun unit-for-partition-explorer (part-name)
;  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
;    (read-disk-label rqb 0)
;    (if (not (= (get-disk-fixnum rqb 1) 2))
;       (ferror "explorers must have V2 disk labels"))
;    (cond ((v2-rqb-contains-this-part-p rqb part-name)
;          0)
;         (t
;          (read-disk-label rqb 1)
;          (if (not (= (get-disk-fixnum rqb 1) 2))
;              (ferror "explorers must have V2 disk labels"))
;          (cond ((v2-rqb-contains-this-part-p rqb part-name)
;                 1)
;                (t
;                 nil))))))


(defun unit-for-partition-explorer (part-name)
  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
    (let ((unit (explorer-lod-band-logical-unit)))
      (read-disk-label rqb unit)
      (if (not (= (get-disk-fixnum rqb 1) 2))
          (ferror "explorers must have V2 disk labels"))
      (cond ((v2-rqb-contains-this-part-p rqb part-name)
             unit)
            (t
             nil)))))

(defun search-units-for-partition-explorer (part-name &optional (units 8) &aux ans)
  (WITH-DISK-RQB (RQB DISK-LABEL-RQB-PAGES)
    (dotimes (unit units)
      (let ((errorp nil))
        (multiple-value (nil errorp)
          (ignore-errors
            (read-disk-label rqb unit)
            nil))
        (cond ((and (null errorp)
                    (v2-rqb-contains-this-part-p rqb part-name))
               (setq ans unit)
               (return nil))))))
  ans)

(DEFUN PRINT-DISK-LABEL (&OPTIONAL UNIT (STREAM STANDARD-OUTPUT))
  "Print the contents of disk UNIT's label on STREAM.
UNIT can be:
  1) A disk drive number; or
  2) A host object or the name of a host; or
  3) A string containing the name of a machine
     and a disk drive number.
/
  For example:
/
  /(print-disk-label/)             ;unit 0 of this system
  /(print-disk-label 1/)           ;unit 1 of this system
  /(print-disk-label /"LAM-A/"/)     ;unit 0 on remote host LAM-A
  /(print-disk-label /"LAM-A 1/"/)   ;unit 1 on remote host LAM-A
"
  (setq unit (default-disk-unit unit))
  (WITH-DECODED-DISK-UNIT (DECODED-UNIT UNIT "reading label")
    (using-resource (rqb si:rqb disk-label-rqb-pages 4)
      (read-disk-label rqb decoded-unit)
      (print-disk-label-from-rqb stream decoded-unit rqb nil))))

(DEFVAR LE-STRUCTURE NIL
  "LE-STRUCTURE is a list of items for the disk-label editor.
Each item looks like: (name value start-x start-y width)")

;;; This is a subroutine for PRINT-DISK-LABEL-FROM-RQB which implements this.
;;; Note that if not consing up a structure, this must work on a non-display stream
(DEFUN LE-OUT (NAME VALUE STREAM CONS-UP-LE-STRUCTURE-P
               &AUX X Y WIDTH)
  (AND CONS-UP-LE-STRUCTURE-P
       (MULTIPLE-VALUE (X Y) (SEND *TERMINAL-IO* ':READ-CURSORPOS)))
  (FORMAT STREAM "~D" VALUE)
  (WHEN CONS-UP-LE-STRUCTURE-P
    (SETQ WIDTH (- (SEND *TERMINAL-IO* ':READ-CURSORPOS) X))
    (IF (MINUSP WIDTH) (SETQ WIDTH (- (TV:SHEET-INSIDE-RIGHT *TERMINAL-IO*) X))
      (IF (ZEROP WIDTH) (SETQ WIDTH 4)))
    (PUSH (LIST NAME VALUE X Y WIDTH) LE-STRUCTURE))
  NIL)

(defun print-disk-label-from-rqb (stream unit rqb cons-up-le-structure-p)
  (case (get-disk-fixnum rqb 1)
    (1 (print-disk-label-from-rqb-v1 stream unit rqb cons-up-le-structure-p))
    (2 (print-disk-label-from-rqb-v2 stream unit rqb cons-up-le-structure-p))
    (t (ferror "unknown label version"))))

(DEFUN PRINT-DISK-LABEL-FROM-RQB-v1 (STREAM unit RQB CONS-UP-LE-STRUCTURE-P
                                            &AUX N-PARTITIONS WORDS-PER-PART THIS-END NEXT-BASE
                                            CURRENT-MICROLOAD CURRENT-BAND)
  unit
  (TERPRI STREAM)
  (LE-OUT 'PACK-NAME (GET-DISK-STRING RQB #o20 32.) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ": " STREAM)
  (LE-OUT 'DRIVE-NAME (GET-DISK-STRING RQB #o10 32.) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ", " STREAM)
  (LE-OUT 'COMMENT (GET-DISK-STRING RQB #o30 96.) STREAM CONS-UP-LE-STRUCTURE-P)
  (FORMAT STREAM "~%~A version ~D, "            ;You can't edit these
          (GET-DISK-STRING RQB 0 4) (GET-DISK-FIXNUM RQB 1))
  (LE-OUT 'N-CYLINDERS (GET-DISK-FIXNUM RQB 2) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " cylinders, " STREAM)
  (LE-OUT 'N-HEADS (GET-DISK-FIXNUM RQB 3) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " heads, " STREAM)
  (LE-OUT 'N-BLOCKS-PER-TRACK (GET-DISK-FIXNUM RQB 4) STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " blocks//track, " STREAM)
  (FORMAT STREAM "~D" (GET-DISK-FIXNUM RQB 5))
  (PRINC " blocks//cylinder" STREAM)
  (TERPRI STREAM)
  (PRINC "Current microload = " STREAM)
  (LE-OUT 'CURRENT-MICROLOAD (SETQ CURRENT-MICROLOAD (GET-DISK-STRING RQB 6 4))
          STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC ", current virtual memory load (band) = " STREAM)
  (LE-OUT 'CURRENT-BAND (SETQ CURRENT-BAND (GET-DISK-STRING RQB 7 4))
          STREAM CONS-UP-LE-STRUCTURE-P)
  (TERPRI STREAM)
  (LE-OUT 'N-PARTITIONS (SETQ N-PARTITIONS (GET-DISK-FIXNUM RQB #o200))
          STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC " partitions, " STREAM)
  (LE-OUT 'WORDS-PER-PART (SETQ WORDS-PER-PART (GET-DISK-FIXNUM RQB #o201))
          STREAM CONS-UP-LE-STRUCTURE-P)
  (PRINC "-word descriptors:" STREAM)
  (DO ((I 0 (1+ I))
       (PARTITION-NAME)
       (LOC #o202 (+ LOC WORDS-PER-PART)))
      ((= I N-PARTITIONS))
    (SETQ PARTITION-NAME (GET-DISK-STRING RQB LOC 4))
    (IF (OR (STRING-EQUAL PARTITION-NAME CURRENT-MICROLOAD)
            (STRING-EQUAL PARTITION-NAME CURRENT-BAND))
        (FORMAT STREAM "~%* ")
        (FORMAT STREAM "~%  "))
    (LE-OUT 'PARTITION-NAME (GET-DISK-STRING RQB LOC 4) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC " at block " STREAM)
    (LE-OUT 'PARTITION-START (GET-DISK-FIXNUM RQB (1+ LOC)) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC ", " STREAM)
    (LE-OUT 'PARTITION-SIZE (GET-DISK-FIXNUM RQB (+ LOC 2)) STREAM CONS-UP-LE-STRUCTURE-P)
    (PRINC " blocks long" STREAM)
    (WHEN (> WORDS-PER-PART 3)                  ;Partition comment
      (PRINC ", /"" STREAM)
      (LE-OUT 'PARTITION-COMMENT (GET-DISK-STRING RQB (+ LOC 3)
                                                  (* 4 (- WORDS-PER-PART 3)))
              STREAM CONS-UP-LE-STRUCTURE-P)
      (TYO #/" STREAM))
    (SETQ THIS-END (+ (GET-DISK-FIXNUM RQB (1+ LOC)) (GET-DISK-FIXNUM RQB (+ LOC 2)))
          NEXT-BASE (COND ((= (1+ I) N-PARTITIONS)
                           (* (GET-DISK-FIXNUM RQB 2) (GET-DISK-FIXNUM RQB 5)))
                          ((GET-DISK-FIXNUM RQB (+ LOC 1 WORDS-PER-PART)))))
    (COND ((> (- NEXT-BASE THIS-END) 0)
           (FORMAT STREAM ", ~D blocks free at ~D" (- NEXT-BASE THIS-END) THIS-END))
          ((< (- NEXT-BASE THIS-END) 0)
           (FORMAT STREAM ", ~D blocks overlap" (- THIS-END NEXT-BASE))))))

(defun print-disk-label-from-rqb-v2 (stream unit rqb cons-up-le-structure-p
                                  &AUX CURRENT-MICROLOAD CURRENT-BAND)
  (terpri stream)
  (LE-OUT 'pack-name
          (get-disk-string rqb 12. 16.)
          stream cons-up-le-structure-p)
  (Princ ": " stream)
  (LE-OUT 'DRIVE-NAME
          (Get-disk-String rqb 5 12.)
          stream cons-up-le-structure-p)
  (Princ ", " stream)
  (LE-OUT 'COMMENT
          (Get-disk-String rqb 64. 96.) stream cons-up-le-structure-p)
  (Format stream "~%~a version ~d, "    ; You can't edit these
          (Get-disk-String rqb 0 4) (Get-disk-Fixnum rqb 1))
  (let ((type-word (get-disk-fixnum rqb 4)))
    (select (ldb (byte 3 0) type-word)
      (0 ; disk
       (le-OUT 'DEVICE-TYPE "DISK" stream cons-up-le-structure-p)
       (let ((temp (Get-disk-Fixnum rqb 8)))  ;"bytes-per"

            (Terpri stream)
            (LE-OUT 'N-Bytes-per-Block
                        (ldb (byte 16. 0) temp) stream cons-up-le-structure-p)
            (Princ " bytes per block, " stream)
            (LE-OUT 'N-Bytes-per-Sector
                        (ldb (byte 16. 16.) temp) stream cons-up-le-structure-p)
            (Princ " bytes per sector, " stream)

            (Terpri stream)
            (setq temp (Get-disk-Fixnum rqb 9))
            (LE-OUT 'N-Sectors-per-Track
                        (ldb (byte 8 24.) temp) stream cons-up-le-structure-p)
            (Princ " sectors per track, " stream)
            (LE-OUT 'N-Heads
                        (ldb (byte 8 16.) temp) stream cons-up-le-structure-p)
            (Princ " heads, " stream)

            (Terpri stream)
            (setq temp (Get-disk-Fixnum rqb 10.))
            (LE-OUT 'N-Cylinders
                        (ldb (byte 16. 0) temp) stream cons-up-le-structure-p)
            (Princ " cylinders, " stream)
            (LE-OUT 'N-Sectors-for-Defects
                        (ldb (byte 16. 16.) temp) stream cons-up-le-structure-p)
            (Princ " sectors for defects, " stream))

       )
      (1 ;"tape"
       (LE-OUT 'DEVICE-TYPE "TAPE" stream cons-up-le-structure-p))
      (Otherwise
       (LE-OUT 'DEVICE-TYPE (Format nil "UNKNOWN (~d)" (Ldb (byte 3 0) type-word))
               stream cons-up-le-structure-p))
      ))
  (Terpri stream)
  (Princ "Current microload = " stream)
  (LE-OUT 'CURRENT-MICROLOAD
          (Setq current-microload (get-disk-string rqb %dl-current-microload 4))
          stream cons-up-le-structure-p)
  (Princ ", current virtual memory load (band) = " stream)
  (LE-OUT 'CURRENT-BAND
          (Setq current-band (get-disk-string rqb %dl-current-band 4))
          stream cons-up-le-structure-p)
  (Terpri stream)
  (Princ "Partition table " STREAM)
  (LE-OUT 'PARTITION-TABLE-NAME
          (Get-disk-String rqb 20. 4.)
          stream cons-up-le-structure-p)
  (Princ ", starting block " STREAM)
  (LE-OUT 'PARTITION-TABLE-START
          (Get-disk-Fixnum rqb 21.)
          stream cons-up-le-structure-p)
  (Princ ", length " STREAM)
  (LE-OUT 'PARTITION-TABLE-LENGTH
          (Get-disk-Fixnum rqb 22.)
          stream cons-up-le-structure-p)
  (Terpri stream)
  (Princ "Save area " STREAM)
  (LE-OUT 'SAVE-AREA-NAME
          (Get-disk-String rqb 28. 4.)
          stream cons-up-le-structure-p)
  (Princ ", starting block " STREAM)
  (LE-OUT 'SAVE-AREA-START
          (Get-disk-Fixnum rqb 29.)
          stream cons-up-le-structure-p)
  (Princ ", length " STREAM)
  (LE-OUT 'SAVE-AREA-LENGTH
          (Get-disk-Fixnum rqb 30.)
          stream cons-up-le-structure-p)
  (Terpri stream)
  ;; The partition table resides in the disk label buffer starting
  ;; at page 1.
  (Let ((pt-start 256.)
        n-partitions
        words-per-part)
;;;    (Princ "Partition Table Revision: " stream)
;;;    (LE-OUT 'P-TABLE-REVISION (Get-disk-Fixnum rqb (+ pt-start 1))
;;;            stream cons-up-le-structure-p)
    (LE-OUT 'N-PARTITIONS
            (Setq n-partitions
                  (Get-disk-fixnum rqb
                       (+ pt-start 2)))
            stream cons-up-le-structure-p)
    (Princ " partitions, " stream)
    (LE-OUT 'WORDS-PER-PART
            (Setq words-per-part
                  (Get-disk-Fixnum rqb
                       (+ pt-start 3)))
            stream cons-up-le-structure-p)
    (Princ "-word descriptors:" stream)
    ;; print out partition descriptors
    (DO ((i 0 (1+ i))
         (loc (+ pt-start 16.) (+ loc words-per-part)))
        ((= i n-partitions))
      (Let ((partition-name (Get-disk-String rqb loc 4)))
        (If (Or (String-Equal partition-name current-microload)
                (String-Equal partition-name current-band))
            (Format stream "~%* ")
          (Format stream "~%  "))
        (LE-OUT 'PARTITION-NAME
                partition-name stream cons-up-le-structure-p))
      (Princ " " stream)
      (princ "Part-type ")
      (LE-OUT 'PARTITION-TYPE
                (LDB (byte 8 0)
                     (Get-disk-Fixnum rqb (+ loc 3))) ;"***attributes***"
              stream cons-up-le-structure-p)
      (Princ " at block " stream)
      (LE-OUT 'PARTITION-START
              (Get-disk-Fixnum rqb (+ loc 1))
              stream cons-up-le-structure-p)
      (Princ ", " stream)
      (LE-OUT 'PARTITION-SIZE
              (Get-disk-Fixnum rqb (+ loc 2))
              stream cons-up-le-structure-p)
      (Princ " blocks long" stream)
      (When (> words-per-part 4)  ; Partition comment
        (Princ ", /"" stream)
        (LE-OUT 'PARTITION-COMMENT
                (Get-disk-String rqb
                                (+ loc 4) (* 4 (- words-per-part 4)))
                stream cons-up-le-structure-p)
        (Tyo #/" stream))
      (Let ((this-end (+ (Get-disk-Fixnum rqb (+ loc 1))
                         (Get-disk-Fixnum rqb (+ loc 2))))
            (next-base (If (= (1+ i) n-partitions) ; last partition
                           ;; +++ figure this out, should be total number of blocks +++
                           ;; +++ cheat for now +++
                           (+ (Get-disk-Fixnum rqb (+ loc 1))
                              (Get-disk-Fixnum rqb (+ loc 2)))
                         ;; Starting block number of next partition
                         (Get-disk-Fixnum rqb
                                (+ loc 1 words-per-part)))))
        (Cond ((> (- next-base this-end) 0)
               (Format stream ", ~D blocks free at ~D"
                       (- next-base this-end) this-end))
              ((< (- next-base this-end) 0)
               (Format stream ", ~D blocks overlap" (- this-end next-base)))))
      )
    )
  )

(DEFUN P-BIGNUM (ADR)
  (DPB (%P-LDB (BYTE 16. 16.) ADR) (BYTE 16. 16.) (%P-LDB (BYTE 16. 0.) ADR)))

;;; This will get hairier later, e.g. check for wrap around
;;; Also this only understands the Trident controller I guess
(DEFUN PRINT-DISK-ERROR-LOG ()
  "Print a description of remembered disk errors."
  (FORMAT T "~&Disk error count ~D.~%" (READ-METER 'SYS:%COUNT-DISK-ERRORS))
  (DO ((I #o600 (+ I 4))) ((= I #o640))
    (LET ((CLP-CMD (P-BIGNUM I))
          (DA (P-BIGNUM (1+ I)))
          (STS (P-BIGNUM (+ I 2)))
          (MA (P-BIGNUM (+ I 3))))
      (COND ((NOT (ZEROP CLP-CMD))
             (FORMAT T "~%Command ~O ~@[(~A) ~]"
                       (LDB (BYTE 16. 0.) CLP-CMD)
                       (CDR (ASSQ (LDB (BYTE 4. 0.) CLP-CMD) '((0 . "Read")
                                                         (8 . "Read-Compare")
                                                         (9 . "Write")))))
             (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-EARLY CLP-CMD)
                  (PRINC "Data-Strobe-Early "))
             (AND (BIT-TEST %DISK-COMMAND-DATA-STROBE-LATE CLP-CMD)
                  (PRINC "Data-Strobe-Late "))
             (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET CLP-CMD)
                  (PRINC "Servo-offset "))
             (AND (BIT-TEST %DISK-COMMAND-SERVO-OFFSET-FORWARD CLP-CMD)
                  (PRINC "S-O-Forward "))
             (TERPRI)
             (FORMAT T "CCW-list pointer ~O (low 16 bits)~%" (LDB (BYTE 16. 16.) CLP-CMD))
             (FORMAT T "Disk address: unit ~O, cylinder ~O, head ~O, block ~O (~4:*~D ~D ~D ~D decimal)~%"
                       (LDB (BYTE 4. 28.) DA) (LDB (BYTE 12. 16.) DA) (LDB (BYTE 8. 8.) DA) (LDB (BYTE 8. 0.) DA))
             (FORMAT T "Memory address: ~O (type bits ~O)~%"
                       (LDB (BYTE 22. 0.) MA) (LDB (BYTE 2. 22.) MA))
             (FORMAT T "Status: ~O  ~A~%"
                       STS (DECODE-DISK-STATUS (LDB (BYTE 16. 0.) STS) (LDB (BYTE 16. 16.) STS))))))))

;;;; Label editor

(DEFVAR LE-ITEM-NUMBER)
(DEFVAR LE-UNIT)
(DEFVAR LE-RQB)

;;; Change n-words-per-partition of a label sitting in an RQB
(DEFUN CHANGE-PARTITION-MAP (RQB NEW-N-WORDS)
  (lambda-or-cadr-only)
  (LET ((OLD-N-WORDS (GET-DISK-FIXNUM RQB 201))
        (N-PARTITIONS (GET-DISK-FIXNUM RQB 200)))
    (LET ((SAVE (MAKE-ARRAY (LIST N-PARTITIONS (MAX OLD-N-WORDS NEW-N-WORDS)))))
      ;; Fill with zeros
      (DOTIMES (I N-PARTITIONS)
        (DOTIMES (J (MAX OLD-N-WORDS NEW-N-WORDS))
          (ASET 0 SAVE I J)))
      ;; Copy out
      (DOTIMES (I N-PARTITIONS)
        (DOTIMES (J OLD-N-WORDS)
          (ASET (GET-DISK-FIXNUM RQB (+ #o202 (* I OLD-N-WORDS) J)) SAVE I J)))
      ;; Copy back in
      (PUT-DISK-FIXNUM RQB NEW-N-WORDS #o201)
      (DOTIMES (I N-PARTITIONS)
        (DOTIMES (J NEW-N-WORDS)
          (PUT-DISK-FIXNUM RQB (AREF SAVE I J) (+ #o202 (* I NEW-N-WORDS) J)))))))

;;; Known pack types.  The first on this list is the default.
;;; Each element is a 4-list of
;;;   Pack brand name (32 or fewer chars) (as a symbol).
;;;   Number of cylinders.
;;;   Number of heads.
;;;   Number of blocks per track.
;;;   Partition list: name, size (- blocks, + cylinders at cyl bndry)
;;;   First partition starts at block 17. (first track reserved)
(DEFVAR PACK-TYPES
        '#o((|Trident T-80| 815. 5. 17.
                            ((MCR1 -224) (MCR2 -224)
                             (PAGE 340.)
                             (LOD1 200.) (LOD2 200.)
                             (FILE 29.)))
            (|Trident T-300| 815. 19. 17.
                             ((MCR1 -224) (MCR2 -224) (MCR3 -224) (MCR4 -224)
                              (MCR5 -224) (MCR6 -224) (MCR7 -224) (MCR8 -224)
                              (PAGE 202.)       ;Full address space
                              (LOD1 75.) (LOD2 75.) (LOD3 75.) (LOD4 75.)
                              (LOD5 75.) (LOD6 75.) (LOD7 75.) (LOD8 75.)
                              (FILE 9.)))
            (|Fujitsu Eagle| 842. 20. 25.
                             ((LMC1 -224) (LMC2 -224) (LMC3 -224) (LMC4 -224)
                              (LMC5 -224) (LMC6 -224) (LMC7 -224) (LMC8 -224)
                              (PAGE 141.)       ;Full address space
                              (FILE 200.)
                              (LOD1 75.) (LOD2 75.) (LOD3 75.) (LOD4 75.)
                              (LOD5 75.) (LOD6 75.)
                              (METR 9.)))
            (|CDC PA5N| 610. 24. 26.
                             ((LMC1 -224) (LMC2 -224) (LMC3 -224) (LMC4 -224)
                              (LMC5 -224) (LMC6 -224) (LMC7 -224) (LMC8 -224)
                              (PAGE 141.)       ;Full address space
                              (FILE 200.)
                              (LOD1 75.) (LOD2 75.) (LOD3 75.) (LOD4 75.)
                              (LOD5 75.) (LOD6 75.)
                              (METR 9.)))

            ))

(DEFUN LE-INITIALIZE-LABEL (RQB PACK-TYPE)
  (PUT-DISK-STRING RQB "LABL" 0 4)              ;Checkword
  (PUT-DISK-FIXNUM RQB 1 1)                     ;Version number
  (PUT-DISK-FIXNUM RQB (CADR PACK-TYPE) 2)      ;Number of cylinders
  (PUT-DISK-FIXNUM RQB (CADDR PACK-TYPE) 3)     ;Number of heads
  (PUT-DISK-FIXNUM RQB (CADDDR PACK-TYPE) 4)    ;Blocks per track
  (PUT-DISK-FIXNUM RQB (* (CADDR PACK-TYPE) (CADDDR PACK-TYPE)) 5)
  (PUT-DISK-STRING RQB "MCR1" 6 4)              ;Current microload
  (PUT-DISK-STRING RQB "LOD1" 7 4)              ;Current band
  (PUT-DISK-STRING RQB (STRING (CAR PACK-TYPE)) 10 40)  ;Brand name of drive
  (PUT-DISK-STRING RQB "(name)" #o20 #o40)      ;Name of pack
  (PUT-DISK-STRING RQB "(comment)" #o30 #o140)  ;Comment
  (PUT-DISK-FIXNUM RQB (LENGTH (FIFTH PACK-TYPE)) #o200) ;Number of partitions
  (PUT-DISK-FIXNUM RQB 7 #o201)                 ;Words per partition descriptor
  (DO ((LOC #o202 (+ LOC 7))
       (BLOCK (CADDDR PACK-TYPE) (+ BLOCK SZ))
       (SZ)
       (BPC (* (CADDR PACK-TYPE) (CADDDR PACK-TYPE)))
       (PARTS (FIFTH PACK-TYPE) (CDR PARTS)))
      ((NULL PARTS))
    (SETQ SZ (IF (MINUSP (CADAR PARTS))
                 (- (CADAR PARTS))
               (SETQ BLOCK (* BPC (CEILING BLOCK BPC)))
               (* (CADAR PARTS) BPC)))
    (PUT-DISK-STRING RQB (STRING (CAAR PARTS)) LOC 4)
    (PUT-DISK-FIXNUM RQB BLOCK (1+ LOC))
    (PUT-DISK-FIXNUM RQB SZ (+ LOC 2))
    (PUT-DISK-STRING RQB "" (+ LOC 3) 16.)))

;;; Display the label which is sitting in an RQB
(DEFUN LE-DISPLAY-LABEL (RQB UNIT &OPTIONAL NO-PROMPT)
  (SEND *TERMINAL-IO* :CLEAR-WINDOW)
  (IF (NUMBERP UNIT)
      (FORMAT T "Editing label for unit ~D~%" UNIT)
    (FORMAT T "Editing label for unit ~D on ~A~%"
            (SEND UNIT ':UNIT-NUMBER)
            (SEND UNIT ':MACHINE-NAME)))
  (SETQ LE-STRUCTURE NIL)
  (PRINT-DISK-LABEL-FROM-RQB STANDARD-OUTPUT unit RQB T)
  (SETQ LE-STRUCTURE (NREVERSE LE-STRUCTURE))
  (FORMAT T "~%~%~%")
  (UNLESS NO-PROMPT (PRINC "Label Edit Command: "))
  (SETQ LE-ITEM-NUMBER (MIN LE-ITEM-NUMBER (LENGTH LE-STRUCTURE)))
  (LE-UNDERSCORE))

;;; Underscore the selected item
(DEFUN LE-UNDERSCORE ()
  (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
    (IF ITEM
        (SEND *TERMINAL-IO* ':DRAW-RECTANGLE
              (FIFTH ITEM) 1
              (THIRD ITEM) (+ (FOURTH ITEM)
                              (- (TV:SHEET-LINE-HEIGHT *TERMINAL-IO*) 2))
              TV:ALU-XOR)
      ;; Pointing at the line after the last existing partition.
      (SEND *TERMINAL-IO* ':DRAW-RECTANGLE
            1 (SEND TERMINAL-IO ':LINE-HEIGHT)
            0
            (+ (FOURTH (CAR (LAST LE-STRUCTURE)))
               (TV:SHEET-LINE-HEIGHT *TERMINAL-IO*))
            TV:ALU-XOR))))

(DEFVAR LE-SOMETHING-CHANGED NIL "Used to figure out if we've made any editing changes.")

(DEFUN EDIT-DISK-LABEL (&OPTIONAL LE-UNIT (INIT-P NIL)
                                                ;If t, dont try to save page 1 since current
                                                ; label is garbage.  It can bomb setting
                                                ; blocks-per-track to 0, etc.
                        &AUX LE-STRUCTURE (LE-ITEM-NUMBER 0) CH COM ABORT)
  "Edit the label of a disk pack.
LE-UNIT is the disk drive number, or a name of a machine (the chaosnet is used),
or /"CC/" which refers to the machine being debugged by this one."
  (setq le-unit (default-disk-unit le-unit))
  (SETQ LE-SOMETHING-CHANGED NIL)               ;restart
  (WITH-DECODED-DISK-UNIT (LE-UNIT LE-UNIT "editing label" INIT-P)
    (WITH-DISK-RQB (LE-RQB DISK-LABEL-RQB-PAGES)
      (LE-INITIALIZE-LABEL LE-RQB (CAR PACK-TYPES))
      (if (null init-p)
          (READ-DISK-LABEL LE-RQB LE-UNIT))
      (LE-DISPLAY-LABEL LE-RQB LE-UNIT T)
      (FORMAT T "Use Control-R to read and edit existing label; hit HELP for help.~%")
      (PRINC "Label Edit Command: ")
      (*CATCH 'LE-EXIT
        (DO-FOREVER
          (SETQ CH (SEND *TERMINAL-IO* ':TYI))
          (SETQ COM (INTERN-SOFT (STRING-UPCASE (FORMAT NIL "LE-COM-~:C" CH))
                                 "SI"))
          (COND ((OR (NULL COM)                 ;nothing typed
                     (NOT (FBOUNDP COM)))       ;command not defined
                 (BEEP)
                 (FORMAT T "~%~:C is not a known edit-disk-label command.  ~
                                        Type ~:C for help, or ~:C to exit." CH #/HELP #/END))
                (T (MULTIPLE-VALUE (NIL ABORT)
                     (CATCH-ERROR-RESTART ((ERROR SYS:ABORT)
                                           "Return to EDIT-DISK-LABEL.")
                       (FUNCALL COM)))
                   (AND ABORT (LE-DISPLAY-LABEL LE-RQB LE-UNIT)))))))))


;;; Redisplay
(DEFUN LE-COM-FORM ()
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

(DEFUN LE-COM-ABORT ()
  (FORMAT T "~%Type ~:C to exit, or ~:C for help." #/END #/HELP))

;;; Exit
(DEFUN LE-COM-END ()
  (WHEN (OR (NULL LE-SOMETHING-CHANGED)
            (FQUERY NIL "~&It appears to me that you have not written out your changes.
You must type ~:C to write out your changes before typing ~:C.
Do you still want to exit? " #/CONTROL-W #/END))
    (FORMAT T "~%Exiting the disk label editor.")
    (*THROW 'LE-EXIT NIL)))

(DEFUN LE-COM-META-~ ()
  (FORMAT T "~%No longer modified.")
  (SETQ LE-SOMETHING-CHANGED NIL))

;;; Previous item
(DEFUN LE-COM-CONTROL-B ()
  (LE-UNDERSCORE)
  (SETQ LE-ITEM-NUMBER (MAX 0 (1- LE-ITEM-NUMBER)))
  (LE-UNDERSCORE))

(DEFUN LE-COM-CONTROL-D ()
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (IF (= LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
        (BEEP)
      (SI:DESCRIBE-PARTITION (GET-DISK-STRING LE-RQB PLOC 4) le-unit))))


;;; Next item
(DEFUN LE-COM-CONTROL-F ()
  (LE-UNDERSCORE)
  (SETQ LE-ITEM-NUMBER (MIN (LENGTH LE-STRUCTURE)
                            (1+ LE-ITEM-NUMBER)))
  (LE-UNDERSCORE))

;;; First item on next line
(DEFUN LE-COM-CONTROL-N ()
  (LE-UNDERSCORE)
  (DO ((L (NTHCDR LE-ITEM-NUMBER LE-STRUCTURE) (CDR L))
       (N LE-ITEM-NUMBER (1+ N))
       (Y0 (OR (FOURTH (NTH LE-ITEM-NUMBER LE-STRUCTURE)) 0)))
      ((OR (NULL L) (> (FOURTH (CAR L)) Y0))
       (SETQ LE-ITEM-NUMBER (MIN (LENGTH LE-STRUCTURE) N))
       (LE-UNDERSCORE))))

;;; First item on previous line
(DEFUN LE-COM-CONTROL-P ()
  (LE-UNDERSCORE)
  (DO ((Y0 (OR (FOURTH (NTH LE-ITEM-NUMBER LE-STRUCTURE)) 0))
       (L LE-STRUCTURE (CDR L))
       (N 0 (1+ N))
       (Y) (CAND-Y -1) (CAND-N 0))
      (())
    (SETQ Y (FOURTH (CAR L)))
    (COND ((OR (NULL L) (= Y Y0))
           (SETQ LE-ITEM-NUMBER CAND-N)
           (LE-UNDERSCORE)
           (RETURN NIL))
          ((= Y CAND-Y) )                       ;Next thing on same line
          (T (SETQ CAND-Y Y CAND-N N)))))       ;First thing on a line

;;; Read in the label
(DEFUN LE-COM-CONTROL-R ()
  (READ-DISK-LABEL LE-RQB LE-UNIT)
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;;; Write out the label
(DEFUN LE-COM-CONTROL-W ()
  (COND ((Y-OR-N-P "Do you want to write out this label? ")
         (WRITE-DISK-LABEL LE-RQB LE-UNIT)
         (SETQ LE-SOMETHING-CHANGED NIL)
         (FORMAT T "~&Written.~%Type  to exit the disk-label editor."))
        (T
         (FORMAT T "~&Not written.~%"))))

;;; Initialize
(DEFUN LE-COM-CONTROL-I ()
  (FORMAT T "Pack types are:~%")
  (DO ((L PACK-TYPES (CDR L))
       (N 0 (1+ N)))
      ((NULL L))
    (FORMAT T " ~S  ~A~%" N (CAAR L)))
  (SETQ LE-SOMETHING-CHANGED T)
  (FORMAT T "Enter desired number: ")
  (LET ((TEM (NTH (READ) PACK-TYPES)))
    (AND TEM (LE-INITIALIZE-LABEL LE-RQB TEM)))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;;; Delete this partition
(defun le-com-control-k ()
  (ecase (get-disk-fixnum le-rqb 1)
    (1 (le-com-control-k-version-1))
    (2 (le-com-control-k-version-2))))

(DEFUN LE-COM-CONTROL-K-version-1 ()
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (COND ((= LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
           (FORMAT T "~&There is no currently selected partition.")
           (BEEP))
          ((FQUERY NIL "Delete the ~S partition? " (GET-DISK-STRING LE-RQB PLOC 4))
           (SETQ LE-SOMETHING-CHANGED T)
           (LET ((NPARTS (GET-DISK-FIXNUM LE-RQB #o200))
                 (NWORDS (GET-DISK-FIXNUM LE-RQB #o201))
                 (BUF (RQB-BUFFER LE-RQB)))
             (PUT-DISK-FIXNUM LE-RQB (MAX (1- NPARTS) 0) #o200)
             (COPY-ARRAY-PORTION BUF (* (+ PLOC NWORDS) 2) (ARRAY-LENGTH BUF)
                                 BUF (* PLOC 2) (ARRAY-LENGTH BUF)))
           (LE-DISPLAY-LABEL LE-RQB LE-UNIT)))))

(DEFUN LE-COM-CONTROL-K-version-2 ()
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (COND ((= LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
           (FORMAT T "~&There is no currently selected partition.")
           (BEEP))
          ((FQUERY NIL "Delete the ~S partition? " (GET-DISK-STRING LE-RQB PLOC 4))
           (SETQ LE-SOMETHING-CHANGED T)
           (LET ((NPARTS (GET-DISK-FIXNUM LE-RQB (+ 256. 2)))
                 (NWORDS (GET-DISK-FIXNUM LE-RQB (+ 256. 3)))
                 (BUF (RQB-BUFFER LE-RQB)))
             (PUT-DISK-FIXNUM LE-RQB (MAX (1- NPARTS) 0) (+ 256. 2))
             (COPY-ARRAY-PORTION BUF (* (+ PLOC NWORDS) 2) (ARRAY-LENGTH BUF)
                                 BUF (* PLOC 2) (ARRAY-LENGTH BUF)))
           (LE-DISPLAY-LABEL LE-RQB LE-UNIT)))))

;;; Redisplay
(DEFUN LE-COM-CONTROL-L ()
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;;; Add a partition
(defun le-com-control-o ()
  (ecase (get-disk-fixnum le-rqb 1)
    (1 (le-com-control-o-version-1))
    (2 (le-com-control-o-version-2))
    ))

(DEFUN LE-COM-CONTROL-O-version-1 ()
  (SETQ LE-SOMETHING-CHANGED T)
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (LET ((NPARTS (1+ (GET-DISK-FIXNUM LE-RQB 200)))
          (NWORDS (GET-DISK-FIXNUM LE-RQB 201))
          (BUF (RQB-BUFFER LE-RQB)))
      (COND ((> (+ (* NPARTS NWORDS) #o202)
                (FLOOR (ARRAY-LENGTH (RQB-BUFFER LE-RQB)) 2))
             (FORMAT T "~&Partition table full"))
            (T (PUT-DISK-FIXNUM LE-RQB NPARTS 200)
               (LET ((FOO (MAKE-ARRAY #o1000 ':TYPE 'ART-16B)))
                 (COPY-ARRAY-PORTION BUF (* PLOC 2) (ARRAY-LENGTH BUF) FOO (* NWORDS 2) #o1000)
                 (COPY-ARRAY-PORTION FOO 0 #o1000 BUF (* PLOC 2) (ARRAY-LENGTH BUF))
                 (PUT-DISK-STRING LE-RQB "????" PLOC 4)
                 (PUT-DISK-FIXNUM LE-RQB 0 (+ 2 PLOC))
                 (PUT-DISK-FIXNUM LE-RQB
                                  (IF (= LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
                                      (+ (GET-DISK-FIXNUM LE-RQB (+ 1 (- PLOC NWORDS)))
                                         (GET-DISK-FIXNUM LE-RQB (+ 2 (- PLOC NWORDS))))
                                    (GET-DISK-FIXNUM LE-RQB (+ PLOC NWORDS 1)))
                                  (1+ PLOC)))
               (LE-DISPLAY-LABEL LE-RQB LE-UNIT))))))

(DEFUN LE-COM-CONTROL-O-version-2 ()
  (SETQ LE-SOMETHING-CHANGED T)
  (LET ((PLOC (LE-CURRENT-PARTITION)))
    (let ((nparts (1+ (get-disk-fixnum le-rqb (+ 256. 2))))
          (nwords (get-disk-fixnum le-rqb (+ 256. 3)))
          (BUF (RQB-BUFFER LE-RQB)))
      (COND ((> (+ (* NPARTS NWORDS) 256.)
                (FLOOR (ARRAY-LENGTH (RQB-BUFFER LE-RQB)) 2))
             (FORMAT T "~&Partition table full"))
            (T (PUT-DISK-FIXNUM LE-RQB NPARTS (+ 256. 2))
               (LET ((FOO (MAKE-ARRAY #o1000 ':TYPE 'ART-16B)))
                 (COPY-ARRAY-PORTION BUF (* PLOC 2) (ARRAY-LENGTH BUF) FOO (* NWORDS 2) #o1000)
                 (COPY-ARRAY-PORTION FOO 0 #o1000 BUF (* PLOC 2) (ARRAY-LENGTH BUF))
                 (PUT-DISK-STRING LE-RQB "????" PLOC 4)
                 (PUT-DISK-FIXNUM LE-RQB 0 (+ 2 PLOC))
                 (PUT-DISK-FIXNUM LE-RQB
                                  (IF (= LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
                                      (+ (GET-DISK-FIXNUM LE-RQB (+ 1 (- PLOC NWORDS)))
                                         (GET-DISK-FIXNUM LE-RQB (+ 2 (- PLOC NWORDS))))
                                    (GET-DISK-FIXNUM LE-RQB (+ PLOC NWORDS 1)))
                                  (1+ PLOC)))
               (LE-DISPLAY-LABEL LE-RQB LE-UNIT))))))

;;; Sort partitions by address (2nd word) and redisplay
(defun le-com-control-s ()
  (ferror "foo"))

(DEFUN LE-COM-CONTROL-S-version-1 ()
  (SETQ LE-SOMETHING-CHANGED T)                 ;something probably changed
  (DO ((NPARTS (GET-DISK-FIXNUM LE-RQB #o200) (1- NPARTS))
       (NWORDS (GET-DISK-FIXNUM LE-RQB #o201))
       (FROB NIL NIL)
       (PART-LIST NIL (CONS (CONS (GET-DISK-FIXNUM LE-RQB (1+ LOC)) FROB) PART-LIST))
       (LOC #o202 (+ LOC NWORDS))
       (BUF (RQB-BUFFER LE-RQB)))
      ((ZEROP NPARTS)
       (SETQ PART-LIST (SORTCAR PART-LIST #'<))
       (DO ((L PART-LIST (CDR L))
            (LOC #o202 (+ LOC NWORDS)))
           ((NULL L))
         (DO ((K (CDAR L) (CDR K))
              (I (1- (* 2 NWORDS)) (1- I)))
             ((MINUSP I))
           (ASET (CAR K) BUF (+ LOC LOC I)))))
    (DOTIMES (I (* 2 NWORDS))
      (PUSH (AREF BUF (+ LOC LOC I)) FROB)))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;;; This, my friends, is the hairy part
;;; Edit the selected item
(defun le-com-control-e ()
  (ecase (get-disk-fixnum le-rqb 1)
    (1 (le-com-control-e-v1))
    (2 (le-com-control-e-v2))))

(DEFUN LE-COM-CONTROL-E-v1 ()
  (SETQ LE-SOMETHING-CHANGED T)                 ;something probably will...
  (IF (< LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
      (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
        (LET ((NAME (FIRST ITEM))
              (VALUE (SECOND ITEM))
              (*READ-BASE* 10.))
          (WITH-INPUT-EDITING (T `((:INITIAL-INPUT ,(FORMAT NIL "~D" VALUE))))
            (SETQ VALUE (PROMPT-AND-READ (IF (NUMBERP VALUE) ':INTEGER ':STRING)
                                         "Change the ~A to:" NAME)))
          ;; Avoid lossage in lowercase partition names.
          (COND ((MEMQ NAME '(PARTITION-NAME CURRENT-BAND CURRENT-MICROLOAD))
                 (SETQ VALUE (STRING-UPCASE VALUE))))
          (CASE NAME
            (PACK-NAME (PUT-DISK-STRING LE-RQB VALUE #o20 32.))
            (DRIVE-NAME (PUT-DISK-STRING LE-RQB VALUE #o10 32.))
            (COMMENT (PUT-DISK-STRING LE-RQB VALUE #o30 96.))
            (N-CYLINDERS (PUT-DISK-FIXNUM LE-RQB VALUE 2))
            (N-HEADS (PUT-DISK-FIXNUM LE-RQB VALUE 3)
                     (PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 4)) 5))
            (N-BLOCKS-PER-TRACK (PUT-DISK-FIXNUM LE-RQB VALUE 4)
                                (PUT-DISK-FIXNUM LE-RQB (* VALUE (GET-DISK-FIXNUM LE-RQB 3)) 5))
            (CURRENT-MICROLOAD (PUT-DISK-STRING LE-RQB VALUE 6 4))
            (CURRENT-BAND (PUT-DISK-STRING LE-RQB VALUE 7 4))
            (N-PARTITIONS (PUT-DISK-FIXNUM LE-RQB VALUE #o200))
            (WORDS-PER-PART (CHANGE-PARTITION-MAP LE-RQB VALUE))
            ;; These occur in multiple instances; hair is required
            ((PARTITION-NAME PARTITION-START PARTITION-SIZE PARTITION-COMMENT)
             (LET ((PLOC (LE-CURRENT-PARTITION)))
               (CASE NAME
                 (PARTITION-NAME (PUT-DISK-STRING LE-RQB VALUE PLOC 4))
                 (PARTITION-START (PUT-DISK-FIXNUM LE-RQB VALUE (1+ PLOC)))
                 (PARTITION-SIZE (PUT-DISK-FIXNUM LE-RQB VALUE (+ PLOC 2)))
                 (PARTITION-COMMENT
                  (PUT-DISK-STRING LE-RQB VALUE (+ PLOC 3)
                                   (* 4 (- (GET-DISK-FIXNUM LE-RQB #o201) 3)))))))
            (OTHERWISE (FERROR "No editor for ~S" NAME)))))
    (BEEP))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

(DEFUN LE-COM-CONTROL-E-v2 ()
  (SETQ LE-SOMETHING-CHANGED T)                 ;something probably will...
  (IF (< LE-ITEM-NUMBER (LENGTH LE-STRUCTURE))
      (LET ((ITEM (NTH LE-ITEM-NUMBER LE-STRUCTURE)))
        (LET ((NAME (FIRST ITEM))
              (VALUE (SECOND ITEM))
              (*READ-BASE* 10.))
          (WITH-INPUT-EDITING (T `((:INITIAL-INPUT ,(FORMAT NIL "~D" VALUE))))
            (SETQ VALUE (PROMPT-AND-READ (IF (NUMBERP VALUE) ':INTEGER ':STRING)
                                         "Change the ~A from to:" NAME)))
          ;; Avoid lossage in lowercase partition names.
          (COND ((MEMQ NAME '(PARTITION-NAME CURRENT-BAND CURRENT-MICROLOAD))
                 (SETQ VALUE (STRING-UPCASE VALUE))))
          (CASE NAME
            (PACK-NAME (PUT-DISK-STRING LE-RQB VALUE 12. 16.))
            (DRIVE-NAME (PUT-DISK-STRING LE-RQB VALUE 5 12.))
            (COMMENT (PUT-DISK-STRING LE-RQB VALUE 64. 96.))
            (N-CYLINDERS
             (put-disk-fixnum le-rqb
                              (dpb value (byte 16. 0) (get-disk-fixnum le-rqb 10.))
                              10.))
            (N-HEADS
             (put-disk-fixnum le-rqb
                              (dpb value (byte 8 16.) (get-disk-fixnum le-rqb 9))
                              9))
            (N-sectors-PER-TRACK
             (put-disk-fixnum le-rqb
                              (dpb value (byte 8 24.) (get-disk-fixnum le-rqb 9))
                              9))
            (N-PARTITIONS
             (put-disk-fixnum le-rqb value (+ 256. 2)))
            (WORDS-PER-PART
             (put-disk-fixnum le-rqb value (+ 256. 3)))
            ;; These occur in multiple instances; hair is required
            ((PARTITION-NAME PARTITION-START PARTITION-SIZE PARTITION-COMMENT
                             partition-type)
             (LET ((PLOC (LE-CURRENT-PARTITION)))
               (CASE NAME
                 (PARTITION-NAME (PUT-DISK-STRING LE-RQB VALUE PLOC 4))
                 (PARTITION-START (PUT-DISK-FIXNUM LE-RQB VALUE (1+ PLOC)))
                 (PARTITION-SIZE (PUT-DISK-FIXNUM LE-RQB VALUE (+ PLOC 2)))
                 (partition-type (put-disk-fixnum le-rqb
                                                  (dpb value (byte 8 0) (get-disk-fixnum le-rqb (+ ploc 3)))
                                                  (+ ploc 3)))
                 (PARTITION-COMMENT
                  (PUT-DISK-STRING LE-RQB VALUE (+ PLOC 4)
                                   (* 4 (- (GET-DISK-FIXNUM LE-RQB (+ 256. 3)) 4)))))))
            (current-microload
             (set-default-microload-V2 le-rqb value))
            (current-band
             (set-default-load-band-V2 le-rqb value))
            (OTHERWISE (FERROR "No editor for ~S" NAME)))))
    (BEEP))
  (LE-DISPLAY-LABEL LE-RQB LE-UNIT))

;;; Returns the word number of the start of the descriptor for the partition
;;; containing the current item.
(defun le-current-partition ()
  (case (get-disk-fixnum le-rqb 1)
    (1 (le-current-partition-v1))
    (2 (le-current-partition-v2))
    (t (ferror "unknown label version"))))

(DEFUN LE-CURRENT-PARTITION-v1 ()
  (DO ((WORDS-PER-PARTITION (GET-DISK-FIXNUM LE-RQB 201))
       (PNO 0)
       (L LE-STRUCTURE (CDR L))
       (N LE-ITEM-NUMBER (1- N)))
      ((ZEROP N)
       (+ #o202 (* PNO WORDS-PER-PARTITION)))
    (AND (EQ (CAAR L) 'PARTITION-COMMENT) (INCF PNO))))

(DEFUN LE-CURRENT-PARTITION-v2 ()
  (DO ((WORDS-PER-PARTITION (GET-DISK-FIXNUM LE-RQB (+ 256. 3)))
       (PNO 0)
       (L LE-STRUCTURE (CDR L))
       (N LE-ITEM-NUMBER (1- N)))
      ((ZEROP N)
       (+ 256. 16. (* PNO WORDS-PER-PARTITION)))
    (AND (EQ (CAAR L) 'PARTITION-COMMENT) (INCF PNO))))

;;; Give help
(DEFF LE-COM-HELP 'LE-COM-?)
(DEFUN LE-COM-? ()
  (FORMAT T "~2%Commands are as follows:
C-B back, C-F forward, C-P up, C-N down
C-R read label from disk, C-W write label to disk, C-I initialize the label
C-L clear the screen, and redisplay the label
C-E edit selected item
C-D describe the current partition
M-~~ mark buffer unmodified
C-O add partition, C-K delete partition, C-S sort partitions
 exit"))
