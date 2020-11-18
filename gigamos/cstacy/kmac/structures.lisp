;;; -*- Mode:LISP; Package:MAC; Readtable:T; Base:10 -*-
;;;
;;; By Renaud Nadeau October 1988
;;;



;;;
;;;  Constants, variables and functions used by the BC share-memory
;;;


(defconst Falcon-version-number 1)
(defconst BC-base-adr #xfb000000)


;
; All transition state for the KMAC communications
;
(defconst  ST-IDLE    0)
(defconst  ST-FILLING 1)
(defconst  ST-FILLED  2)
(defconst  ST-RUN     3)
(defconst  ST-DONE    4)
(defconst  ST-ERROR   5)
(defconst  ST-FAULT   6)


;
; All device Status Codes for the buffered stream
;
(defconst DST-CLOSED            0)
(defconst DST-OPEN              1)
(defconst DST-EOF               2)
(defconst DST-ERROR             3)
(defconst DST-CONTINUABLE-ERROR 4)




(defun make-ascii-number (ignore stream)
  (loop with char = (character (readch stream)) and n = 0
        until (or (char-lessp char #/A)
                  (char-greaterp char #/Z)) do
        (setq n (+ (character char) (ash n 8)))
        (setq char (character (readch stream)))
        finally (return n)))

(set-syntax-#-macro-char #/& 'make-ascii-number)



(defun keyword->num (keyword)
  (selectq keyword
    (:DEFAULT           0)                      ;keywords for options
    (:WILD              1)
    (:UNSPECIFIC        2)
    (:NEWEST            3)
    (:OLDEST            4)
    (:NEW-VERSION       5)
    (:INPUT             6)
    (:OUTPUT            7)
    (:PROBE             8)
    (:PROBE-DIRECTORY   9)
    (:ERROR             10)
    (:SUPERSEDE         11)
    (:OVERWRITE         12)
    (:TRUNCATE          13)
    (:APPEND            14)
    (:RENAME            15)
    (:RENAME-AND-DELETE 16)
    (:CREATE            17)
    (:ABORT             18)
    (:ROOT              19)
    (:LOOKUP            20)
    (t (ferror "MAC: keyword inconnu"))))


(defun num->keyword (num)
  (selectq num
    (0           :DEFAULT)
    (1              :WILD)
    (2        :UNSPECIFIC)
    (3            :NEWEST)
    (4            :OLDEST)
    (5       :new-VERSION)
    (6              :OPEN)
    (6             :INPUT)
    (7            :OUTPUT)
    (8             :PROBE)
    (9    :PROBE-DIRECTORY)
    (10             :ERROR)
    (11         :SUPERSEDE)
    (12         :OVERWRITE)
    (13          :TRUNCATE)
    (14            :APPEND)
    (15            :RENAME)
    (16 :RENAME-AND-DELETE)
    (17            :CREATE)
    (18             :ABORT)
    (19              :ROOT)
    (20            :lookup)
    (t (ferror "MAC: keyword inconnu"))))





(defconst CMD-QUIT               1)
(defconst CMD-REBOOT             2)
(defconst CMD-SHUTDOWN           3)
(defconst CMD-CREATE-DEVICE      4)
(defconst CMD-DELETE-DEVICE      5)
(defconst CMD-OPEN               6)
(defconst CMD-CLOSE              7)
(defconst CMD-RENAME             8)
(defconst CMD-DELETE             9)
(defconst CMD-CHANGE-PROPERTIES 10)
(defconst CMD-DIRECTORY-LIST    11)
(defconst CMD-EXPUNGE-DIRECTORY 12)
(defconst CMD-CREATE-DIR        13)
(defconst CMD-COMPLETE-STRING   14)
(defconst CMD-REWIND            15)
(defconst CMD-SETEOF            16)



;(defconst CMD-READ              101)
;(defconst CMD-WRITE             102)
;(defconst CMD-NEXT-DIRECTORY    113)
;(defconst CMD-FINISH-OUTPUT     114)
;(defconst CMD-PAGEIN             3)
;(defconst CMD-PAGEOUT            4)



(declare (special *BC-memory-available*  header-table))

;;;
;;; Definition of the methods to allocate the Bus-Coupler memory
;;;

(defun alloc-BC-memory (size &optional (init-list nil))
  (let* ((struct *BC-memory-available*)(adr struct))
    (setq *BC-memory-available* (+ (* 4 size) *BC-memory-available*))
    (loop for each in init-list do
          (write-BC-long adr (if each each 0))
          (decf size)
          (incf adr 4))
    (loop repeat size do
          (write-BC-long adr 0)
          (incf adr 4))
    struct))



;;;
;;; Definition of the structure :bc-struct
;;;

(si:defstruct-define-type :bc-struct
  (:cons-keywords :size )
  (:cons (init description keywords)
         :list
         description
         `(alloc-bc-memory ,(* 4 (or (cdr (assq :size keywords))
                                     (length init)))
                           (list ,@init)))
  (:ref (number description address offset)
        description
        `(read-bc-long (+ ,(* 4 number) ,address ,offset))))

(defsetf read-bc-long write-bc-long)

;;;
;;; All structure use in the communication between Mac <-> Falcon
;;;
; The organisation of the file for each structure look like
;
; Declaration of the constants related to the specific structure
; Declaration of the variables    "     "  "     "         "
; Declaration of the structure    "     "  "     "         "
; Declaration of the functions    "     "  "     "         "
;;;

(defconst kmac-header-CMD-number       #X52010064)
(defconst kmac-header-Mac-CMD-number   #x52100064)


(defstruct (command :conc-name
                    (:type (:bc-struct (:size 25)))
                    (:default-pointer 0))
  header
  (state ST-IDLE)
  command
  arg-count
  val-count
  err-code
  err-string
  stream
  args)


(defun send-cmd-wait (struct cmd narg nval &rest args)
  (setf (Command-state struct) ST-FILLED)
  (setf (Command-command struct) cmd)
  (setf (Command-arg-count struct) narg)
  (setf (Command-val-count struct) nval)
  (setf (Command-err-code struct) 0)
  (setf (Command-err-string struct) 0)
  (setf (Command-stream struct) 0)
  (loop with offset = 0
        for each in args do
        (setf (Command-args struct offset)each)
        (incf offset 4))
  (setf   (Command-state struct) ST-FILLED)
  (process-wait "BC" #'(lambda () (let ((state (Command-state struct)))
                                    (or (= state ST-DONE)
                                        (= state ST-ERROR)
                                        (= state ST-FAULT))))))


;;;
;;; The keyboard and mouse is a fixe structure
;;;
(defconst kmac-header-KBD-number       #x5207001c)
(defconst *kbd-ring-buffer-item-size* 8)
(defconst *kbd-ring-buffer-length* 128)
(defconst *kbd-ring-buffer-size* (* *kbd-ring-buffer-item-size* *kbd-ring-buffer-length*))

(defstruct (KBD  :conc-name
                 (:type :bc-struct)
                 (:default-pointer 0))
  (header  kmac-header-KBD-number)
  buffer-start
  buffer-end
  buffer-input
  buffer-output
  mouse-x
  mouse-y)

(defstruct (kbd-entry :conc-name
                 (:type :bc-struct)
                 (:default-pointer 0))
  type-modifiers
  char-or-mouse)

;;;
;;; The device table is create by the Falcon but
;;; it is maintains by the Mac aplication
;;;
(defconst kmac-header-Dev-Table-number #x520200c8)

(defstruct (Dev-Tab  :conc-name
                    (:type (:bc-struct (:size 50)))
                    (:default-pointer 0))
  (header kmac-header-Dev-table-number)
  name
  blks)

;;;
;;; Allocation for the Screen Descriptor
;;;
(defconst kmac-header-Screen-number    #x52030024)

(defstruct (SD  :conc-name
                (:type :bc-struct)
                (:default-pointer 0))
  (header  kmac-header-Screen-number)
  Falcon-addr
  Falcon-depth
  Falcon-words-per-row
  Falcon-width
  Falcon-height
  Priority-CMD-blk
  CMD-blk
  Mac-window)

;;;
;;; Allocation of pathname free-list
;;;
(defconst kmac-header-pathname         #x5211012c)    ;75 -> 300
(defconst *nb-pre-allocated-pathname* 15)
(defconst *pathname-struct-var-size* 28 )
(defvar *pathname-free-list*)


(defstruct (path  :conc-name
                  (:type (:bc-struct (:size 75)))
                  (:default-pointer 0))
  (header kmac-header-pathname)
  host
  device
  name
  type
  version
  nb-dirs
  dirs)


(defun write-pathname-blk (struct directory name type version)
  (let* ((offset 0)
         (length 0)
         (ndirs 1)
         (string-adr 0))
    (setf (path-header struct) kmac-header-pathname)
    (setf (path-host struct) 0)
    (setf (path-device struct) 0)
    (if (not (listp directory))
        (setq directory (list directory))
      (setq ndirs (length directory)))
    (setq string-adr (+ *pathname-struct-var-size* (* 4 ndirs)))
    (setf (path-nb-dirs struct) ndirs)
    (loop for each-name in directory do
          (if (not (stringp each-name))
              (setf (path-dirs struct offset) (expr->num each-name))
            (setq length (string-length each-name))
            (setf (path-dirs struct offset) (dpb ISA-STRING (byte 8 24.) string-adr))
            (write-BC-mac-string  (+ struct string-adr) each-name length)
            (incf string-adr (1+  length)))
          (incf offset 4))
    (if (not (stringp name))
        (setf (path-name struct) (expr->num name))
            (setq length (string-length name))
            (setf (path-name struct) (dpb ISA-STRING (byte 8 24.) string-adr))
            (write-BC-mac-string  (+ struct string-adr) name length)
            (incf string-adr (1+ length)))
    (if (not (stringp type))
        (setf (path-type struct) (expr->num type))
            (setq length (string-length type))
            (setf (path-type struct) (dpb ISA-STRING (byte 8 24.) string-adr))
            (write-BC-mac-string  (+ struct string-adr) type length))
    (setf (path-version struct) (expr->num version))))


(defun read-pathname-blk (struct)
  (let* (name type version
         (offset 0)
         (directory nil)
         (ndirs (path-nb-dirs struct)))
    (loop repeat ndirs do
          (setq directory (cons (num->expr (path-dirs struct offset) struct) directory))
          (incf offset 4))
    (setq directory (reverse directory)
          name    (num->expr (path-name struct) struct)
          type    (num->expr (path-type struct) struct)
          version (num->expr (path-version struct)))
    (if (= 1(length directory))
        (setq directory (car directory)))
    (values directory name type version)))


(defun init-pathname-free-list ()
  (setq *pathname-free-list*
        (loop repeat *nb-pre-allocated-pathname*
              collect (make-path))))

(defun new-pathname-blk ()
  (process-wait "Wait free pathname"
                #'(lambda () (not (null *pathname-free-list*))))
  (pop *pathname-free-list*))

(defun release-pathname-blk (blk)
  (push blk *pathname-free-list*))


;;;
;;; Allocation of device free-list
;;;
;;;
(defconst kmac-header-Paging-number    #x52040000)
(defconst kmac-header-Device-number    #x52050028)
(defconst kmac-header-Network-number   #x52080000)
(defconst *nb-pre-allocated-dev-blk* 15)
(defvar *device-free-list*)


(defstruct (Dev  :conc-name
                 (:type :bc-struct)
                 (:default-pointer 0))
  (header kmac-header-Device-number)
  (status DST-CLOSED)
  flags
  Buffer-chain
  Current-Mac-buffer
  Current-Lisp-buffer
  Mac-state
  Owing-Process
  pathname
  cmd-blk)

(defun init-device-free-list ()
  (setq *device-free-list*
        (loop repeat *nb-pre-allocated-dev-blk*
              collect (make-dev :cmd-blk (make-command :header kmac-header-CMD-number)))))

(defun new-device-blk ()
  (process-wait "Wait free Dev"
                #'(lambda () (not (null *device-free-list*))))
  (pop *device-free-list*))

(defun release-device-blk (blk)
  (push blk *device-free-list*))


;;;
;;; Allocation for the file information
;;;

(defconst kmac-header-info  #X52120320)
(defconst *info-struct-var-size* 80)
(defconst *nb-pre-allocated-info-blk* 15)

(defvar *info-free-list*)
(defvar  mac-properties  '(:author
                           :byte-size
                           :characters
                           :deleted
                           :dont-delete
                           :dont-reap
                           :qfasl
                           :creation-date
                           :modification-date
                           :not-backup
                           :length-in-bytes
                           :length-in-blocks
                           :mac-type
                           :directory))
(defvar  mac-unsettable-properties  '(:byte-size
                                      :characters
                                      :dont-reap
                                      :qfasl
                                      :not-backup
                                      :length-in-bytes
                                      :length-in-blocks
                                      :directory)
  "Unsettable properties are those which are uniquely determined by the text of the file.")

(defvar  mac-settable-properties  (reverse (set-difference mac-properties mac-unsettable-properties)))


(defstruct (info  :conc-name
                  (:type (:bc-struct (:size 200)))
                  (:default-pointer 0))
  (header kmac-header-info)
  author
  byte-size
  characters
  deleted
  dont-delete
  dont-reap
  qfasl
  creation-date
  modification-date
  not-backup
  length-in-bytes
  length-in-blocks
  mac-type
  directory
  name
  type
  version
  user-plist-size
  user-plist)

(defun init-info-free-list ()
  (setq *info-free-list*
        (loop repeat *nb-pre-allocated-info-blk*
              collect (make-info))))

(defun new-info-blk ()
  (process-wait "Wait free info blk"
                #'(lambda () (not (null *info-free-list*))))
  (pop *info-free-list*))

(defun release-info-blk (blk)
  (push blk *info-free-list*))

(defun read-info-blk (struct)
  (let* ((plist-size (info-user-plist-size struct))
         (plist-addr (ldb (byte 24. 0) (info-user-plist struct))))
    (append (list ':author            (num->expr (info-author      struct) struct)
                  ':characters        (num->expr (info-characters  struct))
                  ':deleted           (num->expr (info-deleted     struct))
                  ':dont-delete       (num->expr (info-dont-delete struct))
                  ':dont-reap         (num->expr (info-dont-reap   struct))
                  ':qfasl             (num->expr (info-qfasl       struct))
                  ':not-backup        (num->expr (info-not-backup  struct))
                  ':directory         (num->expr (info-directory   struct))
                  ':byte-size         (info-byte-size         struct)
                  ':creation-date     (info-creation-date     struct)
                  ':modification-date (info-modification-date struct)
                  ':length-in-bytes   (info-length-in-bytes   struct)
                  ':length-in-blks    (info-length-in-blocks  struct)
                  ':mac-type          (info-mac-type          struct))
            (if (> plist-size 0)
                (let ((string (make-string plist-size)))
                    (read-BC-in-string (+ struct plist-addr) string plist-size)
                    (with-input-from-string (stream string)
                      (fs:read-property-list stream)))))))

(defun dissect-property-list (property-list)
  "Separates a plist into file properties and user properties,
   and verifies if the properties are settable."
  (let ((plist (copy-list property-list))
        file-plist user-plist)
    (do ((prop plist (cddr prop)))
        ((null prop)
         (values file-plist user-plist))
      (cond ((memq (car prop) mac-unsettable-properties)
             (ferror "MAC error: trying to change an unsettable property."))
            ((memq (car prop) mac-settable-properties)
             (push (cadr prop) file-plist)
             (push (car  prop) file-plist))
            (t
             (push (cadr prop) user-plist)
             (push (car  prop) user-plist))))))


(defun write-info-blk (struct plist)
  (let ((string-adr  *info-struct-var-size*))
    (multiple-value-bind (file-plist user-plist)
        (dissect-property-list plist)
      (loop for (prop value) in file-plist
            when (memq prop mac-settable-properties)
            (selectq prop
              (:author
               (let (length)
                 (if (or (not (stringp value))
                         (> (setq length (string-length value)) 15))
                     (ferror "MAC error: Maximum length of author string is 15"))
                 (setf (info-author struct) (dpb ISA-STRING (byte 8 24.) string-adr))
                 (write-BC-mac-string  (+ struct string-adr) value length)
                 (incf string-adr (1+ length))))
              (:deleted           (setf (info-deleted struct) (num->expr value)))
              (:dont-delete       (setf (info-dont-delete struct) (num->expr value)))
              (:mac-type          (setf (info-mac-type struct) value))
              (:creation-date     (setf (info-creation-date struct) value))
              (:modification-date (setf (info-modification-date struct) value))))
      (when user-plist
        (let* ((string (with-output-to-string (stream)
                       (fs:write-property-list stream user-plist)))
               (length (string-length string)))
          (setf (info-user-plist-size struct) length)
          (setf (info-user-plist struct) (dpb ISA-TEXT (byte 8 24.) string-adr))
          (write-BC-from-string string string-adr length))))))


;;;
;;;  Allocation and manipulation of the Chained IObuffers
;;;

(defconst kmac-header-Buffer-number    #x52060028)
(defconst *buffer-descriptor-size* 36)
(defconst *buffer-data-size* 512)
(defconst *nb-input-iobuffer* 3)
(defconst *nb-pre-allocated-buffers* 15)
(defconst *buffer-size* (+ 4 *buffer-descriptor-size* *buffer-data-size*))

(defvar *free-buffer-list*)
(defvar *nb-input-IObuffer*  2)
(defvar *nb-output-IObuffer* 2)

(defstruct (Buffer  :conc-name
                    (:type :bc-struct)
                    (:default-pointer 0))
  (header kmac-header-Buffer-number)
  (state ST-FILLED)
  next-buffer
  alloc-start
  alloc-end
  start
  end
  Master-scan
  Slave-scan
  Error-byte)

(defun allocate-buffers ()
  (setq *free-buffer-list*
        (loop repeat *nb-pre-allocated-buffers*
              collect (make-buffer :state ST-FILLED :alloc-start (alloc-BC-memory *buffer-data-size*)))))

(defun get-buffer ()
  (let (buffer)
    (process-wait "Wait free buffer"
                  #'(lambda () (not (null *free-buffer-list*))))
    (without-interrupts
      (setq buffer (pop *free-buffer-list*))
      (setf (Buffer-state buffer)       ST-FILLED)
      (setf (Buffer-start buffer)       (Buffer-alloc-start buffer))
      (setf (Buffer-end buffer)         0)
      (setf (Buffer-alloc-end buffer)   (+ *buffer-data-size* (Buffer-alloc-start buffer))))
    buffer))

(defun release-buffer (buffer)
  (without-interrupts (push buffer *free-buffer-list*)))


(defun get-chained-iobuffers (how-many)
  (loop with first-buffer = (get-buffer)
        with buffer       = first-buffer
        with next-buffer
        repeat (1- how-many)
        do (setq next-buffer (get-buffer))
           (setf (Buffer-next-buffer buffer) next-buffer)
           (setq buffer next-buffer)
        finally (setf (Buffer-next-buffer buffer) first-buffer)
                (return first-buffer)))

(defun release-chained-iobuffers (first-buffer)
  (loop with buffer = first-buffer
        with next   = (Buffer-next-buffer first-buffer)
        do (release-buffer buffer)
        until (= next first-buffer)
        do (setq buffer next
                 next   (Buffer-next-buffer buffer))))


(defun how-many-chained-iobuffers (first-buffer)
  (loop with buffer = first-buffer
        summing 1
        do (setq buffer (Buffer-next-buffer buffer))
        until (= buffer first-buffer)))


;;;
;;;
;;;



(defconst kmac-header-header-number    #x52000050)

(defstruct (Header-Table (:type (:bc-struct (:size 20)))
                         (:default-pointer 0))
  (kmac-header-header  kmac-header-header-number)
  falcon-protocol-version
  mac-protocol-version
  Major-CMD-blk
  major-Mac-CMD-blk
  paging-CMD-blk
  KBD-mouse-descriptor
  Screen-table
  File-stream-table
  Network-table
  Serial-table
  printer-table)


(defun allocate-communication-root()
  (let (KBD-buffer)
    (setq Header-Table (make-header-table))
    (setf (Major-CMD-blk        Header-Table) (make-command :header  kmac-header-CMD-number))
    (setf (major-Mac-CMD-blk    Header-Table) (make-command :header  kmac-header-Mac-CMD-number))
    (setf (paging-CMD-blk       Header-Table) (make-command :header  kmac-header-Paging-number))
    (setq KBD-buffer (alloc-BC-memory *Kbd-ring-buffer-size*))
    (setf (KBD-mouse-descriptor Header-Table) (make-KBD :buffer-start   KBD-buffer
                                                        :buffer-end     (+ KBD-buffer *kbd-ring-buffer-size*)
                                                        :buffer-input   KBD-buffer
                                                        :buffer-output  KBD-buffer))
    (setf (File-stream-table    Header-Table) (make-Dev-tab :name #&FILE ))
    (setf (Network-table        Header-Table) (make-Dev-tab :name #&ETHR ))
    (setf (Serial-table         Header-Table) (make-Dev-tab :name #&SERD ))
    (setf (printer-table        Header-Table) (make-Dev-tab :name #&PRNT ))
    (setf (Screen-table         Header-Table) (make-Dev-tab :name #&SCRN ))
    (setf (Dev-tab-blks (Screen-table Header-Table)) (make-sd))
    (setf (Dev-tab-blks (Screen-table Header-Table) 4) (make-sd))))
