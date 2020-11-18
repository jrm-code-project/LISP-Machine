;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

#|

Copyright LISP Machine, Inc. 1985
   See filename "Copyright" for
 licensing and release information.

This answers the question:
 "How do I access the disk at a lower level of abstraction
  than the file system or virtual memory?"

  2/25/85 15:21:15 -George Carrette


Usage: First, choose a disk partition from: (print-disk-label),
       that is not being used for anything else such as FILE (filesystem),
       LMC (microcode), LOD (lisp saved environments), PAGE (virual memory support),
       METER (the METER:TEST functions). The METER partition is a good one to
       use for testing purposes if you are not otherwise using it.
       If a free partition does not exist then you will have to use SI:EDIT-DISK-LABEL
       to create one, by (for example) changing the name of an unused LOD partition to
       USER, or MINE, etc. Let's say we call this partition "USER".

       Then: (initialize-disk-partition "USER" "For my own database")
       You need to do the above only once.

       Then: (setq p (mount-disk-partition "USER" :ACCESS :WRITE))

       Then: (read-disk-block p 3)
         or: (write-disk-block p 3 "a string")

More abstract, presumably safer and easier to use routines for hacking
"the disk" at a lower level than the file system allows. This may make
it easier for people to implement their own file systems and databases.

Initialization:
This doesn't do anything but set up the name and comment
and perhaps take optional arguments for zeroing out a partition,
testing for bad blocks etc.

Maybe reserve the first block of the partition for useful information
such to be maintained by the mount/dismount functions about who
initialized and mounted and when etc.

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

|#


(defvar *disk-partition-alist* nil)

(defflavor disk-partition (name
                           unit
                           start
                           length
                           (mounted nil)                ; NIL, :READ or :WRITE
                           (rqb nil)
                           transfer-size
                           lock
                           wired?)
           ()
  :ordered-instance-variables
  :outside-accessible-instance-variables
  (:initable-instance-variables unit name))

(defmethod (disk-partition :print-self) (stream &rest ignore)
  (si:printing-random-object (self stream :type)
    (format stream "~A ~:[not ~;~]mounted ~S blocks"
            name mounted length)))

(add-initialization "Dismount user disk partitions"
                    '(mapc #'(lambda (p)
                               (send (cdr p) :dismount))
                           *disk-partition-alist*)
                    '(:before-cold))

(defmethod (disk-partition :after :init) (ignore)
  (check-type unit (integer 0))
  (push (cons name self) *disk-partition-alist*))

(defun instantiate-disk-partition (partition-name &optional (unit 0))
  (etypecase partition-name
    (symbol (setq partition-name (symbol-name partition-name)))
    (string))
  (unless (SI:FIND-DISK-PARTITION PARTITION-NAME NIL UNIT)
    (ferror "No disk partition named /"~A/"" partition-name))
  (without-interrupts
    (or (cdr (assq partition-name *disk-partition-alist*))
        (make-instance 'disk-partition :name partition-name :unit unit))))

(defun initialize-disk-partition (partition-name partition-comment
                                  &key (unit 0)
                                       (zero? nil)
                                       (check? nil))
  "Initialize a disk partition for later use by the function
MOUNT-DISK-PARTITION which is in turn a prequisite for using
READ-DISK-BLOCK and WRITE-DISK-BLOCK.
Optional arguments specify the zeroing of blocks and the checking for bad-blocks."
  (let ((p (instantiate-disk-partition partition-name unit)))
    (lexpr-send p :initialize partition-name partition-comment :zero? zero? :check? check?)))

(defmethod (disk-partition :initialize) (partition-comment &key (zero? nil)
                                                                (check? nil))
  (check-type partition-comment string)
  (unwind-protect
      (progn (send self :mount :access :write :wired? t)
             (si:update-partition-comment name
                                          partition-comment
                                          unit)
             (if check? (send self :check-for-bad-blocks))
             (if zero? (send self :zero-contents))
             self)
    (send self :dismount)))

(defmethod (disk-partition :mount) (&key (access :read) (transfer-size-in-bytes 1024.)
                                         ((:wired? wire-me) t))
  (check-type access (member :read :write))
  (if mounted (dismount-disk-partition self))
  (when rqb
    (si:return-disk-rqb
      (prog1 rqb (setq rqb nil))))
  (multiple-value-bind (begin size)
      (funcall (cadr (assq access '((:read si:find-disk-partition-for-read)
                                    (:write si:find-disk-partition-for-write))))
               name
               nil
               unit)
    (cond ((null begin)
           (ferror "Partition not found or access not granted"))
          ((not (zerop (mod size
                            (floor transfer-size-in-bytes (* 4 si:page-size)))))
           (ferror "Partition not the right size for transfers of ~D bytes"
                   transfer-size-in-bytes))
          (t
           (setf rqb (si:get-disk-rqb (ceiling transfer-size-in-bytes
                                               (* 4 si:page-size))))
           (if (setf wired? wire-me)
               (si:wire-disk-rqb rqb))
           (setf start begin)
           (setf length size)
           (setf transfer-size transfer-size-in-bytes)
           (setf mounted (cadr (assq access
                                     '((:read (:read))
                                       (:write (:read :write))))))
           self))))

(defun mount-disk-partition (disk-partition-instance &rest options
                             &key (access :read) (transfer-size-in-bytes 1024.)
                                  (wired? t))
  access transfer-size-in-bytes wired?
  (check-type disk-partition-instance disk-partition)
  (lexpr-send disk-partition-instance :mount options))

(defmethod (disk-partition :dismount) ()
  (when mounted
    (setf mounted nil)
    (when rqb
      (si:return-disk-rqb
        (prog1 rqb (setq rqb nil)))))
  self)

(defun dismount-disk-partition (disk-partition-instance)
  (check-type disk-partition-instance disk-partition)
  (send disk-partition-instance :dismount))


;;; maintenence and allocation:

(defmacro check-block-arg (part block)
  `(check-arg ,block (and (fixnump ,block)
                          (not (< ,block 0))
                          (not (> ,block (disk-partition-length ,part))))
              "an integer fitting in the disk partition size"))


(defmethod (disk-partition :zero-contents) (&optional (verbose t))
  (or (memq :write mounted)
      (ferror "~S doesn't support ~A access" self :write))
  (block lose
    (when verbose
      (let ((*standard-output* *error-output*))
        (describe self)
        (unless (yes-or-no-p "Do you REALLY want to zero out ~S" self)
          (return-from lose nil))))
    (let* ((blockn (floor transfer-size (* 4 si:page-size)))
           (n-writes (floor length blockn))
           (peek-every (ceiling n-writes 500.)))
      (when verbose
        (format *error-output*
                "Must do ~D writes. Watch for ~D row~p of dots. One dot = ~D write~p~%"
                n-writes
                (ceiling n-writes (* peek-every 50.))
                (ceiling n-writes (* peek-every 50.))
                peek-every))
      (fill (si:rqb-buffer rqb) 0)
      (dotimes (j n-writes)
        (send self :write-block (* j blockn) "")
        (when verbose
          (if (zerop (mod j peek-every)) (princ "."))
          (if (zerop (mod (1+ j) (* peek-every 50.))) (terpri *error-output*)))))))

(defmethod (disk-partition :check-for-bad-blocks) ()
  (cerror "Ignore the attempt to do checking" "~&Checking of ~S is unimplemented~%" self))


;;; READ and WRITE:

(defun read-disk-block (part block &optional into)
  "Reads block BLOCK from the partition and copies the data into string INTO"
  (check-type part disk-partition)
  (send part :read-block block into))

(defmethod (disk-partition :read-block) (block &optional into &aux temp)
  (or (memq :read mounted)
      (ferror "~S doesn't support ~A access" self :read))
  (check-block-arg self block)
  (cond ((not wired?)
         (si:disk-read rqb
                       unit
                       (+ start block)))
        ((and (setq temp (si::%page-status rqb))
              (= (ldb si::%%PHT1-SWAP-STATUS-CODE temp) si::%PHT-SWAP-STATUS-WIRED))
         (si:disk-read-wired rqb
                             unit
                             (+ start block)))
        (t
         (ferror "Internal inconsistency: disk rqb not wired when expected")))
  (unless into
    (setq into (make-string transfer-size)))
  (copy-array-contents (si:rqb-8-bit-buffer rqb) into)
  into)

;>> WTF?!
;(defsetf read-disk-block write-disk-block)

(defun write-disk-block (part block value)
  "Write the string VALUE into the block BLOCK inside the partition PART"
  (check-type part disk-partition)
  (send part :write-block block value))

(defmethod (disk-partition :write-block) (block from &aux tem)
  (or (memq :write mounted)
      (ferror "~S doesn't support ~A access" self :write))
  (check-block-arg self block)
  (unless (and (arrayp from) (eq (array-type from) 'art-8b))
    (check-type from string))
  (copy-array-contents from (si:rqb-8-bit-buffer rqb))
  (cond ((not wired?)
         (si:disk-write rqb
                        unit
                        (+ start block)))
        ((and (setq tem (si::%page-status rqb))
              (= (ldb si::%%PHT1-SWAP-STATUS-CODE tem) si::%PHT-SWAP-STATUS-WIRED))
         (si:disk-write-wired rqb
                              unit
                              (+ start block)))
        (t
         (ferror "Internal inconsistency: disk rqb not wired when expected"))))



;;;; now streams for partitions

(defun make-partition-input-stream (&key partition-name
                                         (byte-size 8)
                                         (unit 0))
  (make-instance 'partition-input-stream
                 :partition-name partition-name
                 :byte-size byte-size
                 :unit unit))

(defflavor partition-input-stream
           ((byte-size 8.)
            user-elements-per-page
            partition
            (next-page-number 0)
            array-as-characters
            array-for-user)
           (si:input-pointer-remembering-mixin
            si:buffered-input-stream)
  :inittable-instance-variables
  (:init-keywords :partition-name :unit)
  (:required-init-keywords :partition-name))

(defmethod (partition-input-stream :after :init) (init-plist)
  (check-type byte-size (member 8. 16.) "byte-size must be either 8 or 16 bits")
  (setq user-elements-per-page (case byte-size
                                 (8. (* page-size 4))
                                 (16. (* page-size 2))))
  (setq partition (instantiate-disk-partition
                    (get init-plist :partition-name)
                    (get init-plist :unit 0)))
  (mount-disk-partition partition :wired? nil)
  (setq array-for-user (make-array user-elements-per-page
                                   :type (case byte-size
                                           (8. 'art-8b)
                                           (16. 'art-16b))))
  (setq array-as-characters (make-string (* page-size 4)
                                         :displaced-to array-for-user)))

(defmethod (partition-input-stream :next-input-buffer) (no-hang-p)
  (declare (values array start end)
           (ignore no-hang-p))
  (cond ((= next-page-number (disk-partition-length partition))
         ())
        ('else
         (read-disk-block partition next-page-number array-as-characters)
         (incf next-page-number)
         (values array-for-user 0 user-elements-per-page))))

(defmethod (partition-input-stream :length) ()
  (* user-elements-per-page (disk-partition-length partition)))

(defmethod (partition-input-stream :discard-input-buffer) (ignore)
  nil)

(defmethod (partition-input-stream :set-buffer-pointer) (new-pointer)
  (setq next-page-number (floor new-pointer user-elements-per-page))
  (* next-page-number user-elements-per-page))

(defmethod (partition-input-stream :close) (&optional abortp)
  (declare (ignore abortp))
  (dismount-disk-partition partition))


;;; an output stream.   ******* NOT YET TESTED ******* -naha
;;; just great, we all love untested code in system sources.
;;; now test -gjc

(defun make-partition-output-stream (&key partition-name
                                         (byte-size 8)
                                         (unit 0))
  (make-instance 'partition-output-stream
                 :partition-name partition-name
                 :byte-size byte-size
                 :unit unit))

(defflavor partition-output-stream
           ((byte-size 8.)
            user-elements-per-page
            partition
            (next-page-number 0)
            array-as-characters
            array-for-user)
           (si:output-pointer-remembering-mixin
            si:buffered-output-stream)
  :inittable-instance-variables
  :gettable-instance-variables
  (:init-keywords :partition-name :unit)
  (:required-init-keywords :partition-name))

(defmethod (partition-output-stream :after :init) (init-plist)
  (check-type byte-size (member 8. 16.) "byte-size must be either 8 or 16 bits")
  (setq user-elements-per-page (case byte-size
                                 (8. (* page-size 4))
                                 (16. (* page-size 2))))
  (setq partition (instantiate-disk-partition
                    (get init-plist :partition-name)
                    (get init-plist :unit 0)))
  (mount-disk-partition partition :wired? nil :access :write)
  (setq array-for-user (make-array user-elements-per-page
                                   :type (case byte-size
                                           (8. 'art-8b)
                                           (16. 'art-16b))))
  (setq array-as-characters (make-string (* page-size 4)
                                         :displaced-to array-for-user)))

(defmethod (partition-output-stream :new-output-buffer) ()
  (values array-for-user 0 user-elements-per-page))

(defmethod (partition-output-stream :send-output-buffer) (buffer ending-index)
  (declare (ignore buffer ending-index))
  (write-disk-block partition next-page-number array-as-characters)
  (incf next-page-number)
  nil)

(defmethod (partition-output-stream :discard-output-buffer) (buffer)
  (declare (ignore buffer))
  nil)

(defmethod (partition-output-stream :set-buffer-pointer) (new-pointer)
  (setq next-page-number (floor new-pointer user-elements-per-page))
  (* next-page-number user-elements-per-page))

(defmethod (partition-output-stream :get-old-data) (buffer-array lower-output-limit)
  (declare (ignore buffer-array lower-output-limit))
  (read-disk-block partition next-page-number array-as-characters)
  nil)

(defmethod (partition-output-stream :max-pointer) ()
  (* (disk-partition-length partition) user-elements-per-page))

(defmethod (partition-output-stream :close) (&optional abortp)
  (declare (ignore abortp))
  (dismount-disk-partition partition))


(defun copy-file-to-partition (filename part &optional unit)
  (with-open-file (file filename)
    (with-open-stream (disk (make-partition-output-stream :partition-name part
                                                            :unit (default-disk-unit unit)))
      (stream-copy-until-eof file disk)
      (dotimes (j (- (send disk :max-pointer) (send disk :read-pointer)))
        (send disk :tyo #\space)))))

(defun viewf-disk-partition (part &optional unit)
  (with-open-stream (disk (make-partition-input-stream :partition-name part
                                                       :unit (default-disk-unit unit)))
    (format t "~&~A~%" (symeval-in-instance disk 'partition))
    (stream-copy-until-eof disk standard-output)))


;;;; MCR file reader

(defun read-error-table-data-from-ucode-partition ()
  (multiple-value-bind (part-name unit)
      (loaded-ucode-partition-and-unit)
    (with-open-stream (stream (make-partition-input-stream :partition-name part-name
                                                           :unit unit
                                                           :byte-size 8.))
      ;;first the fake section for the entry-point
      (do (section-type
           adr
           size
           (first-time t nil))
          (())
        (setq section-type (get-part-word stream))
        (setq adr (get-part-word stream))
        (setq size (get-part-word stream))
        (case section-type
          (1                                    ; I-mem, size in 64 bit words
           (send stream :set-pointer (+ (send stream :read-pointer)
                                        (* 8 size))))
          ((2 4 5)                              ; Dispatch, A-mem, MID/T-ram; size is 32 bit words
           (send stream :set-pointer (+ (send stream :read-pointer)
                                        (* 4 size))))
          (3                                    ; "Main memory", one argument, except on explorer
                                                ;the first time!
           (send stream :set-pointer (+ (send stream :read-pointer)
                                        (* 4 (if first-time 2 1)))))
          (6
           (let ((result (condition-case ()
                                         (let ((*read-base* 8.)
                                               (*package* (find-package 'eh)))
                                           `(progn ,(cl:read stream) ,(cl:read stream)))
                           (error nil))))
             (return result)))
          (t (return nil)))))))

(defun get-part-word (stream)
  (let* ((b0 (send stream :tyi))
         (b1 (send stream :tyi))
         (b2 (send stream :tyi))
         (b3 (send stream :tyi)))
    (+ (ash b3 24.)
       (ash b2 16.)
       (ash b1 8)
       b0)))


(defun print-ucode-sections (part-name unit)
  (with-open-stream (stream (make-partition-input-stream :partition-name part-name
                                                         :unit unit
                                                         :byte-size 8.))
    ;;first the fake section for the entry-point
    (do (section-type
         adr
         size
         (first-time t nil))
        (())
      (setq section-type (get-part-word stream))
      (setq adr (get-part-word stream))
      (setq size (get-part-word stream))
      (case section-type
        (1                                      ; I-mem, size in 64 bit words
         (format t "~&I-MEM adr ~s size ~s" adr size)
         (send stream :set-pointer (+ (send stream :read-pointer)
                                      (* 8 size))))
        ((2 4 5)                                ; Dispatch, A-mem, MID/T-ram; size is 32 bit words
         (format t "~&~s adr ~s size ~s"
                 (case section-type
                   (2 "d-mem")
                   (4 "a-mem")
                   (5 "mid or t-ram"))
                 adr size)
         (send stream :set-pointer (+ (send stream :read-pointer)
                                      (* 4 size))))
        (3                                      ; "Main memory", one argument, except on explorer
                                                ;the first time!
         (format t "~&Main mem: n-blocks ~s adr-of-first-block ~s " adr size)
         (cond ((null first-time)
                (format t "phys mem adr for first word ~s" (get-part-word stream)))
               (t
                (format t "explorer version ~s " (get-part-word stream))
                (format t "explorer starting address ~s" (get-part-word stream)))))
        (6
         (format t "~&Error table adr ~s size ~s" adr size)
         (return nil))
        (t (return nil))))))

(defun find-instruction-in-i-mem-section (i-mem-adr part unit)
  (declare (values inst partition-offset))
  (setq unit (decode-unit-argument unit "read"))
  (with-open-stream (stream (make-partition-input-stream :partition-name part
                                                         :unit unit
                                                         :byte-size 8.))
    ;;first the fake section for the entry-point
    (do (section-type
         adr
         size
         (first-time t nil))
        (())
      (setq section-type (get-part-word stream))
      (setq adr (get-part-word stream))
      (setq size (get-part-word stream))
      (ecase section-type
        (1                                      ; I-mem, size in 64 bit words
         (when (> i-mem-adr size)
           (ferror nil "i-mem-adr too big: saved i-mem is ~s insts long" size))
         (let ((partition-offset (+ (send stream :read-pointer)
                                    (* 8 i-mem-adr))))
           (send stream :set-pointer partition-offset)
           (let* ((hi (get-part-word stream))
                  (lo (get-part-word stream)))
             (return (values (+ (ash hi 32.) lo) partition-offset)))))
        ((2 4 5)                                ; Dispatch, A-mem, MID/T-ram; size is 32 bit words
         (send stream :set-pointer (+ (send stream :read-pointer)
                                      (* 4 size))))
        (3                                      ; "Main memory", one argument, except on explorer
                                                ;the first time!
         (get-part-word stream)
         (when first-time
           (get-part-word stream)))
        (6
         ;;error table
         (return nil))))))

;;; System Startup File Reader.

(defun execute-system-startup-file ()
  (let ((unit (default-disk-unit nil))
        (part "SSUP"))
    (when (find-disk-partition part nil unit)
      (with-open-stream (disk (make-partition-input-stream :partition-name part
                                                           :unit unit))
        (let ((s (cond ((eq #/; (send disk :tyipeek))
                        (make-echo-stream disk standard-output))
                       ('else
                        disk)))
              (readtable (si:find-readtable-named "CL"))
              (package (find-package "USER"))
              (base 10.)
              (ibase 10.))
          (catch-error
            (do ((form)
                 (eof (list nil))
                 (result))
                ((or (eq eof (setq form (read s eof)))
                     (eq :EOF form)))
              (setq result (eval-special-ok form))
              (unless (eq s disk)
                (terpri standard-output)
                (prin1 result standard-output)))))))))
