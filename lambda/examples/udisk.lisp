;;; -*- Mode:LISP; Package:(UDISK GLOBAL); Base:10; Readtable:t -*-


#|

Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright.Text" for
 licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

This answers the question:
 "How do I access the disk at a lower level of abstraction
  than the file system or virtual memory?"

  2/25/85 15:21:15 -George Carrette


Usage: First, choose a disk partition from: (print-disk-label)
       that is not being used for anything else such as FILE (filesystem),
       LMC (microcode), LOD (lisp saved environments), PAGE (virual memory support),
       METER (the METER:TEST functions). The METER partition is a good one to
       use for testing purpose if you are not otherwise using it.
       If a free partition does not exist then you will have to use SI:EDIT-DISK-LABEL
       to create one, by (for example) changing the name of an used LOD partition to
       USER, or MINE, etc. Let's say we call this partition "USER".

       Then: (initialize-disk-partition "USER" "For my own database")
       You need to do the above only once.

       Then: (setq p (mount-disk-partition "USER" :ACCESS :WRITE))

       Then: (read-disk-block p 3)
         or: (write-disk-block p 3 "a string")
         or: (setf (read-disk-block p 3) "a string")

More abstract, presumably safer and easier to use routines for hacking
"the disk" at a lower level than the file system allows. This may make
it easier for people to implement their own file systems and databases.

Initialization:
This doesn't do anything but set up the name and comment
and perhaps take optional arguments for zeroing out a partition,
testing for bad blocks etc.

Maybe reserve the first block of the partition for useful information
such to be maintained by the mount/dismount functions about who
initialized and mounted ande when etc.

If software testing for bad blocks is implemented then keep a
bitmap of bad-block information which
the READ-BLOCK and WRITE-BLOCK functions can check to prevent people
from losing by using bad blocks.

We could probably do well to abstract further: Each block on disk
with a type code, a reference count. Have a bit map of block usage,
reserved and free. The overhead for this sort of thing is minimal
Use a type borrowed from lisp implementation experience, a BIBOP
like in PDP-10 maclisp allows raw blocks to be very raw indeed.


We might also want to implement a process-lock on disk partitions.
Note well: There is no INTERPROCESS LOCK at this level.
           The user should handle this himself, by handshaking via
           shared-memory read/write for example.


Stream Level Interface:
 Instead of having just read/write-block you might want to model
 the partition as a continguous stream of bytes. Be able to
 using :TYI, :TYO, :SET-POINTER, and :READ-POINTER.
 And of course, :FORCE-OUTPUT when needed.
 For this use MAKE-DISK-PARTITION-STREAM, called on a disk partition.
 There can be multiple streams pointing to the same partition,
 just be careful that they arent pointing to the same section of
 disk at the same time. TO DO: implement multiple streams with interlocking.

|#

(defun canonicalize-disk-partition-name (name)
  (check-type name (or string symbol))
  (intern (string-upcase name) ""))

(defstruct (disk-partition (conc-name disk-partition.)
                           :named
                           (:print "#<DISK-PARTITION ~A ~:[not ~;~]mounted ~S blocks>"
                                   (disk-partition.name disk-partition)
                                   (disk-partition.mounted disk-partition)
                                   (disk-partition.length disk-partition)))

  name
  unit
  start
  length
  mounted ; NIL, :READ or :WRITE
  rqb
  transfer-size
  lock
  wired?
  max-block
  )

(defun initialize-disk-partition (partition-name partition-comment
                                  &optional &key
                                  (unit 0)
                                  (zero? nil)
                                  (check? nil))
  (check-type partition-name (or string symbol))
  (check-type partition-comment string)
  "Initialize a disk partition for later use by the function
MOUNT-DISK-PARTITION which is in turn a prequisite for using
READ-DISK-BLOCK and WRITE-DISK-BLOCK. Optional arguments specify
the zeroing of blocks and the checking for bad-blocks."
  (let ((p (instantiate-disk-partition partition-name unit)))
    (unwind-protect
        (progn (mount-disk-partition p :access :write :wired? t)
               (sys:update-partition-comment (disk-partition.name p)
                                             partition-comment
                                             unit)
               (if check? (check-disk-partition p))
               (if zero? (zero-disk-partition p)))
      (dismount-disk-partition p))
    p))

(defun return-disk-partition-stores (x)
  (when (disk-partition.rqb x)
    (si:unwire-disk-rqb (disk-partition.rqb x))
    (si:return-disk-rqb (prog1 (disk-partition.rqb x)
                               (setf (disk-partition.rqb x) nil)))))

(defvar *instantiated-disk-partitions* nil)

(add-initialization "dismount user disk partitions"
                    '(mapc #'dismount-disk-partition *instantiated-disk-partitions*)
                    '(:before-cold))

(defun instantiate-disk-partition (partition-name &optional (unit 0))
  (let ((name (canonicalize-disk-partition-name partition-name)))
    (cond ((cadr (assq unit (get name :disk-partitions))))
          ('else
           (unless (SYS:FIND-DISK-PARTITION NAME NIL UNIT)
             (ferror nil "No disk partition named: ~A" name))
           (let ((p (make-disk-partition name name unit unit)))
             (push p *instantiated-disk-partitions*)
             (push (list unit p) (get name :disk-partitions))
             p)))))

(defun mount-disk-partition (x &optional &key
                             (access :read) (transfer-size-in-bytes 1024)
                             (wired? t) (ask-about-write t) (unit 0))
  (check-arg x (typep x '(or disk-partition string)) "An instance or the name of a disk partition")
  (if (stringp x)
      (setq x (instantiate-disk-partition x unit)))
  (check-arg access (memq access '(:read :write)) "either :READ or :WRITE")
  (if (disk-partition.mounted x) (dismount-disk-partition x))
  (return-disk-partition-stores x)
  (if (not (numberp (disk-partition.unit x)))
      (setq wired? nil))
  (multiple-value-bind (start size)
      (funcall (CASE ACCESS
                 (:READ
                  #'si:find-disk-partition-for-read)
                 (:WRITE
                  (IF ASK-ABOUT-WRITE #'SI:FIND-DISK-PARTITION-FOR-WRITE #'SI:FIND-DISK-PARTITION)))
               (disk-partition.name x)
               nil
               (disk-partition.unit x))
    (cond ((null start)
           (ferror nil "partition not found or access not granted"))
          ((not (zerop (mod transfer-size-in-bytes (* 4 si:page-size))))
           (ferror nil "transfer size ~D not a multiple of ~D" transfer-size-in-bytes (* 4 si:page-size)))
          ((not (zerop (mod size
                            (floor transfer-size-in-bytes
                                   (* 4 si:page-size)))))
           (ferror nil "partition not the right size for transfers of ~D bytes"
                   transfer-size-in-bytes))
          ('else
           (setf (disk-partition.rqb x) (si:get-disk-rqb (ceiling transfer-size-in-bytes
                                                                  (* 4 si:page-size))))
           (cond (wired?
                  (si:wire-disk-rqb (disk-partition.rqb x))
                  (setf (disk-partition.wired? x) t))
                 ('else
                  (setf (disk-partition.wired? x) nil)))
           (setf (disk-partition.start x) start)
           (setf (disk-partition.length x) size)
           (setf (disk-partition.max-block x) (- size (si:rqb-npages (disk-partition.rqb x))))
           (setf (disk-partition.transfer-size x) transfer-size-in-bytes)
           (setf (disk-partition.mounted x)
                 (cadr (assq access
                             '((:read (:read))
                               (:write (:read :write))))))
           x))))

(defun dismount-disk-partition (x)
  (check-type x disk-partition)
  (cond ((disk-partition.mounted x)
         (setf (disk-partition.mounted x) nil)
         (return-disk-partition-stores x)
         x)
        ('else
         x)))

(defmacro check-partition-arg (arg access)
  `(progn (check-type ,arg disk-partition)
          (or (memq ,access (disk-partition.mounted ,arg))
              (ferror nil "~S doesnt support ~A access" ,arg ,access))))


(defmacro check-block-arg (part block)
  `(check-arg ,block (and (fixp ,block)
                          (not (< ,block 0))
                          (not (> ,block (disk-partition.max-block ,part))))
              "an integer fitting in the disk partition size"))


;; READ and WRITE:

(defun read-disk-block (part block &optional into)
  "Reads block BLOCK from the partition and copies the data into string INTO"
  (check-partition-arg part :read)
  (check-block-arg part block)
  (let ((rqb (disk-partition.rqb part))
        (temp))
    (cond ((not (disk-partition.wired? part))
           (si:disk-read rqb
                         (disk-partition.unit part)
                         (+ (disk-partition.start part) block)))
          ((and (setq temp (si:%page-status rqb))
                (= (ldb si:%%PHT1-SWAP-STATUS-CODE temp) si:%PHT-SWAP-STATUS-WIRED))
           (si:disk-read-wired rqb
                               (disk-partition.unit part)
                               (+ (disk-partition.start part) block)))
          ('else
           (ferror nil "internal inconsistency disk rqb not wired when expected")))
    (unless into
      (setq into (make-array (disk-partition.transfer-size part)
                             :type 'art-string)))
    (copy-array-contents (sys:rqb-8-bit-buffer rqb) into))
  into)

(defsetf read-disk-block write-disk-block)

(defun write-disk-block (part block value)
  "Write the string VALUE into the block BLOCK inside the partition PART"
  (check-partition-arg part :write)
  (check-block-arg part block)
  (check-type value string)
  (let ((rqb (disk-partition.rqb part))
        (temp))
    (copy-array-contents value (sys:rqb-8-bit-buffer rqb))
    (cond ((not (disk-partition.wired? part))
           (si:disk-write rqb
                          (disk-partition.unit part)
                          (+ (disk-partition.start part) block)))
          ((and (setq temp (si:%page-status rqb))
                (= (ldb si:%%PHT1-SWAP-STATUS-CODE temp) si:%PHT-SWAP-STATUS-WIRED))
           (si:disk-write-wired rqb
                          (disk-partition.unit part)
                          (+ (disk-partition.start part) block)))
          ('else
           (ferror nil "internal inconsistency disk rqb not wired when expected")))))


;;; maintenence and allocation:

(defun zero-disk-partition (x)
  (check-partition-arg x :write)
  (describe x)
  (let* ((length (disk-partition.length x))
         (blockn (floor (disk-partition.transfer-size x)
                        (* 4 si:page-size)))
         (n-writes (floor length blockn))
         (peek-every (ceiling n-writes 500)))
    (format t "Must do ~D writes. Watch for ~D row~p of dots. One dot = ~D write~p~%"
            n-writes
            (ceiling n-writes (* peek-every 50))
            (ceiling n-writes (* peek-every 50))
            peek-every
            peek-every)
    (fill (si:rqb-buffer (disk-partition.rqb x)) 0)
    (dotimes (j n-writes)
      (write-disk-block x (* j blockn) "")
      (if (zerop (mod j peek-every)) (princ "."))
      (if (zerop (mod (1+ j) (* peek-every 50))) (terpri)))))

(defun check-disk-partition (x)
  (format t "~&Checking of ~S is unimplemented~%" x))


;;; tests and examples:

(defun a-simple-test (x)
  (do ((s)(n 0 (1+ n)))
      ((null (setq s (prompt-and-read ':string-or-nil "Line #~D>" n)))
       (do ((j 0 (1+ j))
            (st nil))
           ((= j n))
         (setq st (read-disk-block x j st))
         (send terminal-io ':string-out
               st
               0
               (string-search-char 0 st))
         (terpri terminal-io)))
    (write-disk-block x n s)))


(defflavor stream-default-handler-mixin
         ;; this should be a built-in system feature
         ()
         ())


(defmethod (stream-default-handler-mixin :lossage) (op)
  (ferror :unclaimed-message "The stream operation ~S is not supported by ~S"
          op self))

(defmethod (stream-default-handler-mixin :tyipeek) ()
  (let ((c (send self :tyi)))
    (when c
      (send self :untyi c)
      c)))

(defmethod (stream-default-handler-mixin :any-tyi) ()
  (send self :tyi))

(defmethod (stream-default-handler-mixin :tyi-no-hang) ()
  (send self :tyi))


(defmethod (stream-default-handler-mixin :any-tyi-no-hang) ()
  (send self :any-tyi))

(defmethod (stream-default-handler-mixin :read-char) ()
  (if (not (send self :characters)) (send self :lossage :read-char))
  (let ((c (send self :tyi)))
    (if (fixnump c) (int-char c) c)))

(defmethod (stream-default-handler-mixin :any-read-char) ()
  (send self :read-char))

(defmethod (stream-default-handler-mixin :read-char-no-hang) ()
  (send self :read-char))

(defmethod (stream-default-handler-mixin :any-read-char-no-hang) ()
  (send self :any-read-char))

(defmethod (stream-default-handler-mixin :read-byte) ()
  (send self :tyi))

(defmethod (stream-default-handler-mixin :unread-char) (c)
  (if (not (send self :characters)) (send self :lossage :unread-char))
  (send self :untyi (if (characterp c) (char-int c) c)))

(defmethod (stream-default-handler-mixin :write-char) (c)
  (if (not (send self :characters)) (send self :lossage :write-char))
  (send self :tyo (if (characterp c) (char-int c) c)))

(defmethod (stream-default-handler-mixin :write-byte) (c)
  (send self :tyo c))

(defmethod (stream-default-handler-mixin :clear-output) ()
  ())

(defmethod (stream-default-handler-mixin :clear-input) ()
  ())

(defmethod (stream-default-handler-mixin :force-output) ()
  ())

(defmethod (stream-default-handler-mixin :finish) ()
  ())


(defmethod (stream-default-handler-mixin :close) ()
  ())


(defmethod (stream-default-handler-mixin :eof) ()
  ())

(defmethod (stream-default-handler-mixin :fresh-line) ()
  (send self :tyo #\newline))

(defmethod (stream-default-handler-mixin :string-out) (string &optional start end)
  (do ((j (or start 0) (1+ j))
       (n (or end (length string))))
      ((= j n))
    (send self :tyo (aref string j))))


(defmethod (stream-default-handler-mixin :line-out) (string &optional start end)
  (send self :string-out string start end)
  (send self :tyo #\newline))


(defmethod (stream-default-handler-mixin :line-in) (&optional leader-length)
  (let ((buf (make-string #o100 :leader-length (if (numberp leader-length) leader-length 1))))
    (setf (fill-pointer buf) 0)
    (values buf
            (do ((tem (send self :tyi) (send self :tyi)))
                ((or (null tem)
                     (eq tem (char-int #/Newline))
                     (eq tem (char-int #/End)))
                 (adjust-array-size buf (array-active-length buf))
                 (null tem))
              (vector-push-extend (int-char tem) buf)))))

(defmethod (stream-default-handler-mixin :string-in) (eof string &optional start end)
  (loop with start = (or start 0)
        and end = (or end (array-length string))
        while (< start end)
        as ch = (send self :tyi)
        while ch
        do (aset ch string (prog1 start (incf start)))
        finally (and (array-has-leader-p string)
                     (store-array-leader start string 0))
        (and (null ch) eof (ferror 'si:end-of-file-1 "End of file on ~S." self))
        (return (values start (null ch)))))


(defmethod (stream-default-handler-mixin :string-line-in) (eof string &optional start end)
  (loop with start = (or start 0)
        and end = (or end (array-length string))
        while (< start end)
        as ch = (send self :tyi)
        while (and ch (neq ch (char-int #/Newline)))
        do (setf (char string (prog1 start (incf start))) (int-char ch))
        finally (and (array-has-leader-p string)
                     (setf (fill-pointer string) start))
        (and (null ch) eof (ferror 'si:end-of-file-1 "End of file on ~S." self))
        (return (values start (null ch) (neq ch #/Return)))))


(defmethod (stream-default-handler-mixin :characters) ()
  t)

(defmethod (stream-default-handler-mixin :interactive) ()
  nil)

(defmethod (stream-default-handler-mixin :element-type) ()
  (if (send self :characters) 'character
    (let ((value (send self :send-if-handles :byte-size)))
      (if value `(unsigned-byte ,value) 'fixnum))))

(defmethod (stream-default-handler-mixin :direction) ()
  (let ((ops (send self :which-operations)))
    (if (memq :tyi ops)
        (if (memq :tyo ops) :bidirectional :input)
      (if (memq :tyo ops) :output nil))))

;;; A Stream that could be used as a reliable log-file.

(defflavor disk-partition-stream
         (partition buffer pointer block index size need-force-output)
         (stream-default-handler-mixin)
  :initable-instance-variables)

(defun make-disk-partition-stream (x)
  (check-type x disk-partition)
  (make-instance 'disk-partition-stream
                 :buffer (make-string (disk-partition.transfer-size x))
                 :partition x
                 :pointer 0
                 :block -1
                 :size (* (disk-partition.length x) (* 4 si:page-size))
                 :need-force-output nil))

(defmethod (disk-partition-stream :after :init) (plist)
  plist
  (send self :setup-pointer))

(defmethod (disk-partition-stream :print-self) (stream level flag)
  level flag
  (format stream "#<DISK-PARTITION-STREAM index ~D on ~S>"
          pointer partition))

(defmethod (disk-partition-stream :read-pointer) ()
  pointer)

(defmethod (disk-partition-stream :set-pointer) (n)
  (when (and need-force-output
             (not (= block
                     (* (floor n (disk-partition.transfer-size partition))
                        (quotient (disk-partition.transfer-size partition) (* 4 si:page-size)))
                     block)))
    (send self :force-output))
  (setq pointer n)
  (send self :setup-pointer))

(defmethod (disk-partition-stream :setup-pointer) ()
  (multiple-value-bind (quotient remainder)
      (floor pointer (disk-partition.transfer-size partition))
    (let ((new-block (* quotient (quotient (disk-partition.transfer-size partition) (* 4 si:page-size)))))
      (when (not (= new-block block))
        (setq block new-block)
        (read-disk-block partition block buffer)))
    (setq index remainder)))


(defmethod (disk-partition-stream :force-output) ()
  (when need-force-output
    (write-disk-block partition block buffer)
    (setq need-force-output nil)))

(defmethod (disk-partition-stream :tyi) ()
  (cond ((= pointer size)
         ())
        ((= index (length buffer))
         (send self :setup-pointer)
         (prog1 (aref buffer index)
                (incf index)
                (incf pointer)))
        ('else
         (prog1 (aref buffer index)
                (incf index)
                (incf pointer)))))


(defmethod (disk-partition-stream :untyi) (c)
  (if (zerop index)
      (ferror nil "UNTYI not after TYI"))
  (decf pointer)
  (decf index)
  (unless (= c (aref buffer index))
    (ferror nil "UNTYI of character different from that TYI'd")))


(defmethod (disk-partition-stream :read-input-buffer) ()
  (cond ((= pointer size)
         ())
        ((= index (length buffer))
         (send self :setup-pointer)
         (values buffer index (length buffer)))
        ('else
         (values buffer index (length buffer)))))


(defmethod (disk-partition-stream :advance-input-buffer) ()
  (let ((amount (- (length buffer) index)))
    (incf index amount)
    (incf pointer amount)))

(defmethod (disk-partition-stream :tyo) (c)
  (when (= index (length buffer))
    (send self :setup-pointer))
  (setf (aref buffer index) c)
  (setq need-force-output t)
  (when (= (1+ index) (length buffer))
    (send self :force-output))
  (incf index)
  (incf pointer))

(defmethod (disk-partition-stream :close) (&optional abortp)
  (if (and (not abortp) (memq :write (disk-partition.mounted partition)))
      (send self :force-output))
  (setq buffer nil))


;; example database.


(DEFVAR *DOC-UNIT* 0)
(DEFVAR *DOC-PARTITION* "METR")
(DEFVAR *DOC-STREAM* NIL)
(DEFVAR *DOC-ALIST* NIL)

(DEFUN DOC (F)
  (WHEN (STRINGP *DOC-PARTITION*)
    (FORMAT T "~&Opening up doc database...")
    (SETQ *DOC-PARTITION* (INSTANTIATE-DISK-PARTITION *DOC-PARTITION* *DOC-UNIT*))
    (MOUNT-DISK-PARTITION *DOC-PARTITION*)
    (SETQ *DOC-STREAM* (MAKE-DISK-PARTITION-STREAM *DOC-PARTITION*))
    (DO ((J 0 (1+ J))
         (N 0 (+ N (* (EXPT 256 J) (SEND *DOC-STREAM* :TYI)))))
        ((= J 8)
         (SEND *DOC-STREAM* :SET-POINTER N)
         (SETQ *DOC-ALIST* (LET ((IBASE 10)
                                 (*READTABLE* (SI:FIND-READTABLE-NAMED "ZL"))
                                 (*PACKAGE* (FIND-PACKAGE "UDISK")))
                             (READ *DOC-STREAM*)))))
    (FORMAT T " done.~%"))
  (LET ((DOC (ASS #'STRING-EQUAL F *DOC-ALIST*)))
    (COND ((NOT DOC)
           (format T "~&~S is undocumented~%" F))
          ('ELSE
           (format t "~&Documentation for ~S~%" F)
           (SEND *DOC-STREAM* :SET-POINTER (CADR DOC))
           (STREAM-COPY-N-BYTES (CADDR DOC) *DOC-STREAM* STANDARD-OUTPUT)
           (TERPRI STANDARD-OUTPUT)))))

(DEFUN STREAM-COPY-N-BYTES (N FROM-STREAM TO-STREAM)
  (COND ((AND (MEMQ :READ-INPUT-BUFFER (SEND FROM-STREAM :WHICH-OPERATIONS))
              (MEMQ :STRING-OUT (SEND TO-STREAM :WHICH-OPERATIONS)))
         (DO ((BUF) (OFFSET) (LIMIT)
              (J 0))
             (())
             (MULTIPLE-VALUE (BUF OFFSET LIMIT)
               (SEND FROM-STREAM :READ-INPUT-BUFFER))
             (COND ((NULL BUF) (RETURN J)))
             (LET ((AMOUNT (MIN (- N J) (- LIMIT OFFSET))))
               (SEND TO-STREAM :STRING-OUT BUF OFFSET (+ OFFSET AMOUNT))
               (INCF J AMOUNT))
             (COND ((= J N) (RETURN J)))
             (SEND FROM-STREAM :ADVANCE-INPUT-BUFFER)))
        ('ELSE
         (DO ((J 0 (1+ J))
              (C))
             ((NOT (SETQ C (SEND FROM-STREAM :TYI)))
              J)
           (SEND TO-STREAM :TYO C)))))

(DEFUN DUMP-DOC (&OPTIONAL (PKG "GLOBAL"))
  (LET ((P (MOUNT-DISK-PARTITION *DOC-PARTITION* :ACCESS :WRITE :UNIT *DOC-UNIT*)))
    (WITH-OPEN-STREAM (S (MAKE-DISK-PARTITION-STREAM P))
      (LET (DS)
        (FORMAT T "~&Collecting documentation in package ~S" pkg)
        (MAPATOMS #'(LAMBDA (SYM)
                      (IF (DOCUMENTATION SYM)
                          (PUSH (LIST SYM (DOCUMENTATION SYM) NIL) DS)))
                  PKG
                  NIL)
        (format t " ~D items found, writing strings..." (length ds))
        (SEND S :SET-POINTER 8)
        (DOLIST (D DS)
          (LET ((P (SEND S :READ-POINTER))
                (N (LENGTH (CADR D))))
            (SEND S :STRING-OUT (CADR D))
            (SETF (CADR D) P)
            (SETF (CADDR D) N)))
        (format t " writing alist...")
        (LET ((P (SEND S :READ-POINTER)))
          (LET ((BASE 10)
                (*READTABLE* (SI:FIND-READTABLE-NAMED "ZL"))
                (*PACKAGE* (FIND-PACKAGE "UDISK"))
                (*print-pretty* nil))
            (PRINT DS S))
          (SEND S :SET-POINTER 0)
          (DO ((J 0 (1+ J))
               (N P (QUOTIENT N 256)))
              ((= J 8))
            (SEND S :TYO (REMAINDER N 256))))
        (format t " done.~%")))))
