;;; -*- Mode:LISP; Package:USER; Base:10 -*-


;;; so that I can give correct information to Bob P about the
;;; layout of the first few blocks of the FILE partition.
;;; Info to be use by a C program to describe the partition.
;;; 6-Dec-85 11:20:51 -George Carrette


;;; structure of the filesystem band.
;;; 24-Bit integers are used quite a bit for block numbers, sizes and offsets.
;;; A number is noted with square brackets [X] and is 3 bytes unless noted.
;;; The numbers are read in a byte at a time, high-byte-first.
;;; Some things are strings terminated by the lispm <RETURN> character #o215.
;;;
;;;
;;; [VERSION][CHECK][SIZE][PUT-BASE][PUT-SIZE]{ROOT-DIRECTORY-MAP}
;;;
;;; A {MAP} is a description of what blocks make up a file.
;;; It is of the form [LENGTH(2 BYTES)][LOCATION0][SIZE0][LOCATION1][SIZE1] ...
;;; The LOCATION is in BLOCKS (1024 bytes) and the SIZE is in BITS.

;;; The PUT (Page Usage Table) is an array of 2-bit quantities
;;; 0 = free block
;;; 1 = reserved
;;; 2 = used
;;; 3 = bad

;;; A directory is a list of directory entries that describe files.
;;;
;;; <filename-as-string><RETURN>
;;; <filetype-as-string><RETURN>
;;; [VERSION][BYTESIZE(1 BYTE)]
;;; <AUTHOR-as-string><RETURN>
;;; [CREATION-DATE(4 bytes)]
;;; {MAP}
;;; [ATTRIBUTES(2 BYTES)]
;;; {PROPERTY-LIST}

;;; There are 7 file attribute bits that are meaningful:
;;; 0  :DONT-DELETE
;;; 1  :CLOSED
;;; 2  :DELETED
;;; 3  :DUMPED
;;; 4  :DONT-REAP
;;; 5  :CHARACTERS
;;; 6  :DIRECTORY

;;; The {PROPERTY-LIST} is
;;; [NUMBER-OF-ENTRIES(1 BYTE)]{STRING}{ENTRY} ....
;;; A {STRING} is [LENGTH(1 BYTE)]<characters...>
;;; An {ENTRY} is [OPCODE(1 BYTE)]{optional-opcode-dependant}
;;; Opcodes are:
;;; 0 = false,                no optional
;;; 1 = true,                 no optional
;;; 2 {STRING}                keyword symbol
;;; 3 {STRING}{STRING}        symbol name and package
;;; 4 [INTEGER]               3 bytes of course.
;;; 5 {STRING}                a string
;;; 6 {READ}                  Must use lisp READ.
;;; 7 {READ}                  Must use lisp READ.

;;; 6 and 7 are rare.

(defun describe-file-partition (partition &optional &key (unit 0))
  (with-open-stream (s (make-partition-input-stream :partition-name partition
                                                    :unit unit
                                                    :byte-size 8))
    (let ((version (read-next-bytes s 3))
          (check   (read-next-bytes s 3))
          (size    (read-next-bytes s 3))
          (page-usage-table-base (read-next-bytes s 3))
          (page-usage-table-size (read-next-bytes s 3)))
      (format t "~&Partition: ~S~%LM-FILE version: ~D (~:[in~;~]valid) Check: ~D (~:[in~;~]valid)~
                 ~%Size ~D blocks, Page Usage Table: Start ~D, length ~D blocks~%"
              (send s :truename)
              version
              (<= 3 version 4)
              check
              (zerop check)
              size
              page-usage-table-base
              page-usage-table-size)
      (let ((map-size (read-next-bytes s 2)))
        (format t "Root directory Map is ~D element~p long~%"
                map-size map-size)
        (dotimes (j map-size)
          (format t "Start: ~D (block), Length ~D (bits)~%"
                  (read-next-bytes s 3)
                  (read-next-bytes s 3))))
      (describe-page-usage-table s page-usage-table-base page-usage-table-size))))


(defun describe-page-usage-table (s base size)
  (format t "Page Usage (. = free, @ = reserved, * = used, ? = bad)")
  (send s :set-pointer (* base 1024))
  (let ((previous-line (make-string 64 :fill-pointer 0))
        (current-line (make-string 64 :fill-pointer 0))
        (previous-page -64)
        (current-page 0)
        (isize (length (format nil "~D" (* size 1024 4)))))
    (do ((j 0 (1+ j))
         (n (* size 1024)))
        ((= j n)
         (or (zerop (fill-pointer current-line))
             (format t "~%~VD~A ~A~%" isize current-page
                     (if (= current-page (+ 64 previous-page)) ":" ">")
                     current-line)))
      (let ((b (read-next-bytes s 1)))
        (dotimes (k 4)
          (array-push current-line (aref #(#\. #\@ #\* #\?) (ldb (byte 2 (* k 2)) b)))))
      (when (= 15 (mod j 16))
        (when (not (string-equal previous-line current-line))
          (format t "~%~VD~A ~A" isize current-page
                  (if (= current-page (+ 64 previous-page)) ":" ">")
                  current-line)
          (setq previous-line (prog1 current-line (setq current-line previous-line)))
          (setq previous-page current-page))
        (setf (fill-pointer current-line) 0)
        (setf current-page (* (1+ j) 4))))))


(defun read-next-bytes (stream nbytes)
  ;; this means that numbers are stored hi-byte first.
  (do ((number 0 (+ (* number 256) (send stream :tyi)))
       (j 0 (1+ j)))
      ((= j nbytes) number)))


(defun make-map-stream (map substream &aux stream (sofar 0) peekc)
  "Map is a list ((pointer byte-count) (pointer byte-count) ...)"
  (send substream :set-pointer (caar map))
  (setq stream  #'(lambda (op &optional arg1 &rest args)
                    (si:selectq-with-which-operations op
                      (:tyi
                        (cond (peekc
                               (prog1 peekc (setq peekc nil)))
                              ((null map)
                               (ferror nil "end of file on map stream"))
                              ((not (= sofar (cadar map)))
                               (incf sofar)
                               (send substream :tyi))
                              ((null (setq map (cdr map)))
                               nil)
                              ('else
                               (send substream :set-pointer (caar map))
                               (setq sofar 0)
                               (send stream :tyi))))
                      (:untyi
                        (setq peekc arg1))
                      (:substream substream)
                      (t
                        (stream-default-handler stream op arg1 args))))))


(defun get-map (s)
  (verbose "file map"
           (map #'(lambda (sublist)
                    (setf (car sublist)
                          (list (* (read-next-bytes s 3) 1024)
                                (quotient (read-next-bytes s 3) 8))))
                (make-list-for-map (read-next-bytes s 2)))))

(defvar *verbose* t)

(defun verbose (comment value)
  (if *verbose*
      (format t " ~A: ~S " comment value))
  value)


(defun make-list-for-map (n)
  (if (> n 100)
      (cerror "make it anyway" "unusually large file map: ~S" n))
  (make-list n))


(defun get-line (s)
  (send s :line-in t))

(defun get-string (s)
  (let ((st (make-string (read-next-bytes s 1))))
    (send s :string-in nil st)
    st))


(defun list-root-directory (partition &key (unit 0) recursive)
  (with-open-stream (s (make-partition-input-stream :partition-name partition
                                                    :unit unit
                                                    :byte-size 8))
    (send s :set-pointer 15)
    (describe-directory-stream NIL (make-map-stream (get-map s) s) recursive)))

(defun view-root-directory (partition &key (unit 0))
  (with-open-stream (s (make-partition-input-stream :partition-name partition
                                                    :unit unit
                                                    :byte-size 8))
    (send s :set-pointer 15)
    (stream-copy-until-eof (make-map-stream (get-map s) s) terminal-io)))



(defun describe-directory-stream (DIR s recursive)
  (do ()
      ((not (describe-directory-entry DIR s recursive)))))

(defun describe-directory-entry (DIR s recursive &aux d a m)
  (cond ((not (send s :tyipeek))
         (format t "~&End of directory~%")
         nil)
        ('else
         (format t "~&~{~A~^.~};~S " dir (setq d (get-line s)))
         (format t "~S " (get-line s))
         (format t "~D " (read-next-bytes s 3))
         (format t "(~D) " (read-next-bytes s 1))
         (format t "~S " (get-line s))
         (time:print-universal-time (read-next-bytes s 4))
         (setq m (get-map s))
         (format t " ~D byte~p"
                 (apply '+ (mapcar 'cadr m))
                 (apply '+ (mapcar 'cadr m)))
         (setq a (read-next-bytes s 2))
         (dolist (x '((0  :DONT-DELETE) (1  :CLOSED) (2  :DELETED)
                      (3  :DUMPED) (4  :DONT-REAP) (5  :CHARACTERS)
                      (6  :DIRECTORY)))
           (if (not (zerop (ldb (byte 1 (car x)) a)))
               (format t " ~S" (cadr x))))
         (let ((nplist (read-next-bytes s 1)))
           (cond ((zerop nplist)
                  (terpri))
                 ('else
                  (terpri)
                  (dotimes (j nplist)
                    (princ (get-string s))
                    (princ ": ")
                    (ecase (read-next-bytes s 1)
                      (0 (princ "NIL"))
                      (1 (princ "T"))
                      (2 (princ (get-string s)))
                      (3 (format t "~A(~A)" (get-string s) (get-string s)))
                      (4 (format t "~D" (read-next-bytes s 3)))
                      (5 (princ (get-string s)))
                      ((6 7) (format t "(READ) ~S" (read s))))
                    (terpri)))))
         (when (and recursive (cond ((eq recursive t))
                                    ((stringp recursive)
                                     (string-equal recursive d))
                                    ((consp recursive)
                                     (string-equal (car recursive) d)))
                    m (not (zerop (ldb (byte 1 6) a))))
           (let ((p (send (send s :substream) :read-pointer)))
             (format t "~&Subdirectory: ~S~%" d)
             (describe-directory-stream (append dir (list d))
                                        (make-map-stream m
                                                         (send s :substream))
                                        (if (consp recursive)
                                            (or (cdr recursive) t)
                                          t))
             (send (send s :substream) :set-pointer p)))
         t)))


(defun make-partition-input-stream (&key partition-name (unit 0) (byte-size 8))
  ;; a non-flavorized version of si:make-partition-input-stream
  (let (rqb u winp)
    (unwind-protect
        (progn (setq u (si:decode-unit-argument unit "random reading"))
               (multiple-value-bind (start length nil truename)
                   (si:find-disk-partition-for-read partition-name nil u)
                 (setq rqb (si:get-disk-rqb 1))
                 (prog1 (make-partition-input-stream-1 truename
                                                       rqb
                                                       u
                                                       start
                                                       length
                                                       0
                                                       (quotient (* 1024 8) byte-size)
                                                       (ecase byte-size
                                                         (16 (si:rqb-buffer rqb))
                                                         (8 (si:rqb-8-bit-buffer rqb))))
                        (setq winp t))))
      (when (not winp)
        (and rqb (si:return-disk-rqb rqb))
        (and u (si:dispose-of-unit u))))))

(defun make-partition-input-stream-1 (nm rqb unit start n pointer block-size buffer &aux stream)
  (setq stream #'(lambda (op &optional arg1 &rest args)
                   (si:selectq-with-which-operations op
                     (:tyi
                       (cond ((null pointer)
                              (ferror nil "end of file on disk partition"))
                             ((= pointer (* block-size n))
                              (setq pointer nil))
                             ('else
                              (if (zerop (mod pointer block-size))
                                  (send stream :set-pointer pointer))
                              (prog1 (aref buffer (mod pointer block-size))
                                     (incf pointer)))))
                     (:set-pointer
                       (check-arg arg1 (and (<= 0 arg1)
                                            (< arg1 (* block-size n)))
                                  "a byte pointer inside the disk partition")
                       (setq pointer arg1)
                       (si:disk-read rqb unit (+ start (floor pointer block-size))))
                     (:read-pointer
                       pointer)
                     (:truename
                       nm)
                     (:close
                       (si:return-disk-rqb (prog1 rqb (setq rqb nil)))
                       (si:dispose-of-unit unit))
                     (t
                       (stream-default-handler stream op arg1 args))))))
