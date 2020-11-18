#|| -*- Mode:LISP; Package:(MBU GLOBAL); Base:10; Fonts:(CPTFONTB) -*-

Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
licensing and release information.

Multibus User device handling kit. 6/13/85 21:38:17 -George Carrette

This provides high-level numerical and symbolic access to the following
four areas of interest:
 * Multibus I/O space, the I/O registers of a device.
 * Multibus memory space, mapped to lisp data arrays for fast DMA access.
 * Multibus interrupt lines.
 * Multibus memory space for boards that look like memory.

There are two kinds of memory space access because there are two views of
multibus memory:
 * The view from the BOARD
 * The view from the LAMBDA (or other nubus processor).

The view from the BOARD is that when it reads and writes multibus addresses
the operations are forwarded (mapped) to the NuBus, and affect memory or
processor boards on the NuBus. The MAPS may be dynamically changed.

The view from the LAMBDA is that it may do operations such as
%MULTIBUS-READ-8 and %MULTIBUS-WRITE-8 and thereby affect device boards
on the Multibus. (These operations are implemented by having an area in
NuBus address space for the SDU slot such that the SDU forwards NuBus
read and write operations to multibus operations, but it is probably best
not to think about it in that way since it is a static predefined mapping
not under user control.)

It is important to not get confused over what is MAPPED multibus address
space and what space is actual multibus memory.

Software entry points:
 (setup-user-multibus) do this once after loading the file.

Then a proper DMA buffer will be a sub-array of
  (multibus-dma.array *user-multibus*)
   starting at: (multibus-dma.array-offset *user-multibus*)
   ending at: (+ (multibus-dma.array-offset *user-multibus*)
                 (multibus-dma.size *user-multibus*))
Conveniently this is exactly the kind of buffer that buffered streams
want to use. It is (for now) entirely up to the user to subdivide
this big buffer into smaller chunks as needed. It may be convenient
to MAKE-ARRAY with :displaced-to and :displaced-index-offset, and
use possibly use SI:CHANGE-INDIRECT-ARRAY to save consing since
a resource of these chunks is probably most convenient for keeping
track of usage anyway.

 The way that multibus interrupt lines work is that the SDU
 handles the interrupt and writes to a given NuBus location
 to tell nubus processors there was an interrupt.
 (reset-multibus-interrupt-5) sets the location associated with
 interrupt 5 back to zero.
 (process-wait-multibus-interrupt-5) waits until the location is
 written due to an interrupt on line 5.
 The acceptable protocol for handling interrupts is then to
 RESET the interrupt sensor, WRITE the device command register in
 question, then WAIT for the device to interrupt.

Available space of various types.

* Multibus I/O space: (recommended access via DEFINE-MULTIBUS-IO-REGISTERS
  and MAKE-MULTIBUS-IO-REGISTERS)
  This is 8-Bits in SDU revisions up to M. So boards that have 8-bit vs 16-Bit
  should be jumpered for 8-bit addressing. Boards that require 16-bit should
  be avoided, or a latest revision SDU obtained.

        #x40 .. #x43    smd 2181 (disk controller)
        #x60 .. #x63    tapemaster (tape controller)
        #x80 .. #x87    systech (mti)
        #xa0 .. #xa7    excelan (ethernet controller)
        #x-- .. #x..    titn (multibus->multibus mapper for IRIS GRAPHICS)

Customers may use I/O ports #xc0 .. #xff.  LMI will attempt to keep these
free in all future releases. The first 64 bytes, addresses #x00 to #x3F
are also free, and used experimentally in-house at LMI.

* 20-bit multibus memory space, as multibus MEMORY, %MULTIBUS-READ-8 etc.
  Note that the multibus has two connectors. A large one and a small one.
  Some board require 24 bit addressing, using 4 extra lines on the small
  connector. This boards should be avoided, or field-changed with the extra
  address lines connected to +5 through a 200 ohm resistor.
  (Recommended access via DEFINE-MULTIBUS-MEMORY-REGISTORS and
   MAKE-MULTIBUS-MEMORY-REGISTERS.)

          0 .. 63k      sdu RAM
         96 .. 99k      multibus => nubus map control registers
        112k            sdu register page (serial ports etc.)
        120 .. 127k     cmos ram
        190k            second burr-brown (0x#x2f800)
        191k            burr-brown (0x2fc00)
        192 .. 199k     3com ethernet
        256 .. 960k     dynamically allocated area (MAPPED, see next section).
        960 .. 1023k    sdu ROM

  A multibus board which "looks like memory" (such as the 3com ethernet board)
  may be allocated to one of many safe areas. The area above the cmos ram,
  128k ... 192k is safe as long as the burr-brown parallel cards (at this
  time only used in-house at LMI) are not present. The area above the
  3com ethernet board is also safe. The mapped area 256 to 960k may be safe
  if it is not mapped to the NuBus for use by a device that will do multibus
  DMA. (Which is the only reason for setting up such maps).
  A multibus board "may look like memory here," and respond to %MULTIBUS-READ-8 etc.
  However, any other multibus board, such as a disk controller that is
  attempting DMA to such space will probably be confused, because
  two boards (the SDU through its MAPS) and the board in question may
  respond to the request. See the discussion in MAPPED area for more information.

  %MULTIBUS-READ-8, %MULTIBUS-READ-16, and %MULTIBUS-READ-32 may be used
  to generate 8, 16 and 32 bit data reads on multibus devices. The
  indirect array created by MAKE-MULTIBUS-MEMORY-REGISTERS may also
  be utilized, directly or through other indirect arrays. All
  references to it will result in 32 bit operations. A useful operation
  involving devices that have much data in multibus memory would be
  to copy the data from the array using BLIT-ARRAY-PORTION
  into a more suitable structure in lisp virtual space (NuBus memory supported)
  and then deal with the data perhaps in smaller chunks there.

* MAPPED multibus area. i.e. how multibus boards doing memory (e.g. DMA) access
  actually generate NuBus operations.

 The dynamically allocated area is about 700k.  About 100k is required for
 the sdu boot program; the rest is allocated according to what devices
 are present.  Each lambda takes about 140k; each unix takes about 50k.
 Tapemaster 1/2" tape takes 70k; Excelan ethernet takes 30k.

 Space available, not including tapemaster and excelan ethernet:
        lambda  460k
        lambda+ 410k
        2x2     320k
        2x2+    270k
        3x3     184k
        3x3+    130k

 Whatever is left may be reserved by the user, with the sdu config program
 "system maps" command.  Config warns if the reserved space is close to
 the limit.  If too much space is reserved, the machine won't boot.

 The user-defined area is from (960 - N)k to 960k (#xEFFFF), where N is the
 number of 1k pages.  Config prints out the address range that this
 represents.

 The sdu boot program reserves this area at startup.  The starting 1k-byte
 page number and number of 1k pages that are reserved are available in
 the system-configuration structure.  The system-configuration accessor
 functions return numbers from 0..1023.

        (si:%system-configuration-base-user-map-page si:*sys-conf*)
        (si:%system-configuration-user-map-number-of-pages si:*sys-conf*)

  The datastructure *USER-MULTIBUS* setup by the function SETUP-USER-MULTIBUS
  in this package is the safest and most structured way to gain access to
  this information.

* Multibus interrupts:

        0       3com ethernet
        1       IOMSG
        2       tapemaster 1/2" tape (unix)
        3       free
        4       smd 2181 disk (sdu, unix)
        5       free
        6       systech terminal mux (unix)
        7       sharing disk driver (software)
 Lines 0, 4 and 7 are required for normal system operation and must not
 be redefined.  The other lines may be used if the device that normally
 uses it is not present.


Examples:


 Example (1):
    A board has three I/O registers. One for commands, one for status
    the other for address bytes.
    Some commands may do multibus dma. Completion of a command results in the
    status i/o register going to 1.

(defvar *my-device-multibus-io-base* 0)

(define-multibus-io-registers My-device
                              *my-device-io-control-registers*
  ((command) 0 :write
   (code code code code code code code code)
   ((code board-reset 0)
    (code dma-in     1)
    (code dma-out    2)
    (code data-process 3)))
  ((status) 1 :read)
  ((address-byte) 2 :write))

(defvar *my-device-io-control-registers* (make-multibus-io-registers
                                           'my-device 0 3))

(defvar *my-dma-buffer* (allocate-user-multibus-dma-buffer 8192))

(defun my-device-wait ()
  (process-wait-with-timeout
    "my device" (* 60 5)
    #'(lambda ()
        (not (zerop (multibus-register-read my-device status)))))
  (or (= 1 (multibus-register-read my-device status))
      (ferror nil "Device error ~A, code: ~D"
              (or (cadr (assq (multibus-register-read my-device status)
                              '((2 "too much data")
                                (3 "too little data")
                                (4 "internal diagnostic failed"))))
                  "unknown")
              (multibus-register-read my-device status))))

(defun reset-my-device ()
  (multibus-register-write my-device command 'code board-reset)
  (my-device-wait))

(defun my-device-data-setup (length)
  ;; the device wants to see the 3 byte multibus address of the
  ;; data followed by a 3 byte length written to the DATA register.
  (let ((mba (user-multibus-dma-buffer.multibus-address *my-dma-buffer*)))
    (multibus-register-write my-device address-byte (ldb (byte 8 0) mba))
    (multibus-register-write my-device address-byte (ldb (byte 8 8) mba))
    (multibus-register-write my-device address-byte (ldb (byte 8 16) mba)))
    (multibus-register-write my-device address-byte (ldb (byte 8 0) length))
    (multibus-register-write my-device address-byte (ldb (byte 8 8) length))
    (multibus-register-write my-device address-byte (ldb (byte 8 16) length)))

(defun my-device-data-in (string &aux (n (length string)))
  (copy-array-portion string 0 n
                      (user-multibus-dma-buffer.array *my-dma-buffer*) 0 n)
  (multibus-register-write my-device command 'code dma-in)
  (my-device-data-setup n)
  (my-device-wait))

(defun my-device-data-out (string &aux (n (length string)))
  (multibus-register-write my-device command 'code dma-out)
  (my-device-data-setup n)
  (my-device-wait)
  (copy-array-portion (user-multibus-dma-buffer.array *my-dma-buffer*) 0 n
                      string 0 n))

(defun my-device-data-process ()
  (multibus-register-write my-device 'code data-process)
  (my-device-wait))


 Example (2):
 A device has no I/O space registers, but looks like a fairly large amount
 of multibus memory, 256 1k pages to be exact. Note that we must first
 run the CONFIG program to reserve this much space.
 The device is an array processor.

(defvar *array-processor-memory-base* (- #xEFFFF (* *multibus-page-size* 256)))

(defun array-processor-allocation-check ()
  "Run this first to make sure we got the address space we expected"
  (let ((got (multibus-dma.byte-address *user-multibus*))
        (want *array-processor-memory-base*))
    (or (= got want)
        (ferror nil "Proper multibus address not reserved, wanted ~X, got ~X"
                want got))))

(define-multibus-memory-registers array-processor
                                  *array-processor*
  ((command) 0 :write
   (code code code code code code code code)
   ((code reset 0)
    (code fft 1)
    (code ift 2)))
  ((status) 1 :read)
  ((length-byte-0) 2 :write)
  ((length-byte-1) 3 :write)
  ((length-byte-2) 4 :write)
  ((fill-0) 5 :write)
  ((fill-1) 6 :write)
  ((fill-2) 7 :write)
  ;; we give symbolic names for the first 4 bytes of data.
  ((data-byte-0) 8 t)
  ((data-byte-1) 9 t)
  ((data-byte-2) 10 t)
  ((data-byte-3) 11 t)
  ;; the rest of the bytes will be accessed via another function.
  )

(defvar *array-processor-data-offset* 8)

(defvar *array-processor* (make-multibus-memory-registers
                            'array-processor
                            *array-processor-memory-base*
                            (* *multibus-page-size* 256)))

(defvar *data-8b-scratch* (make-array (* 256 *multibus-page-size*)
                                      :type 'art-8b))

(defun array-processor (array operation &aux (n (length array)))
  ;; data is an array of floating point numbers.
  ;; See "SYS:EXAMPLES;DATA-CONVERT" for IEEE floating point manipulation.
  (do ((s *data-8b-scratch*)
       (j 0 (1+ j))
       (i 0 (+ i 4))
       (b))
      ((= j n))
    ;; microcode for this little loop is obviously a good thing.
    (setq b (ieee-32b-bits (aref array j)))
    (setf (aref s i)       (ldb (byte 8 0)  b))
    (setf (aref s (1+ i))  (ldb (byte 8 8)  b))
    (setf (aref s (+ i 2)) (ldb (byte 8 16) b))
    (setf (aref s (+ i 3)) (ldb (byte 8 24) b)))
  (multibus-register-write array-processor length-byte-0 (ldb (byte 8 0) (* n 4)))
  (multibus-register-write array-processor length-byte-1 (ldb (byte 8 8) (* n 4)))
  (multibus-register-write array-processor length-byte-2 (ldb (byte 8 16) (* n 4)))
  (blit-array-portion *data-8b-scratch* 0 (* n 4)
                      *array-processor* *array-processor-data-offset*
                      (+ *array-processor-data-offset* n))
  (ecase operation
    (fft
     (multibus-register-write array-processor command 'code fft))
    (ift
     (multibus-register-write array-processor command 'code ift)))
  (process-wait operation
                #'(lambda ()
                    (not (zerop (multibus-register-read array-processor status)))))
  (blit-array-portion *array-processor* *array-processor-data-offset*
                      (+ *array-processor-data-offset* n)
                      *data-8b-scratch* 0 (* n 4))
  (do ((s *data-8b-scratch*)
       (j 0 (1+ j))
       (i 0 (+ i 4)))
      ((= j n))
    (setf (aref array j) (ieee-32b (dpb (aref s 3) (byte 8 24)
                                        (dpb (aref s 2) (byte 8 16)
                                             (dpb (aref s 1) (byte 8 8)
                                                  (aref s 0)))))))
  array)

||#

(defvar *user-multibus* nil)

(defstruct (multibus-dma :named (:conc-name multibus-dma.))
  byte-address
  size
  array
  array-offset
  multibus-pages
  nubus-pages
  free-area
  allocation-lock)

(defstruct (reg-spec (:conc-name reg-spec.) :list)
  names
  offset
  access-mode
  fields
  symbols)

(defconst *multibus-page-size* 1024)
(defconst *word-size* 4)

(defun setup-user-multibus-dma ()
  (let (base-page n-pages size array vaddr offset mbs nbs)
    (setq base-page (SI:%SYSTEM-CONFIGURATION-USER-BASE-MULTIBUS-MAP SI:*SYS-CONF*))
    (setq n-pages (SI:%SYSTEM-CONFIGURATION-USER-MULTIBUS-MAP-SIZE SI:*SYS-CONF*))
    (setq size (* n-pages *multibus-page-size*))
    (setq array (make-array (+ size (* si:page-size *word-size*)) :type 'art-8b
                            :area si:disk-buffer-area))
    (setq *user-multibus* (make-multibus-dma
                            byte-address (* *multibus-page-size* base-page)
                            size size
                            array array))
    (si:wire-array array)
    (setq vaddr (array-data-base-virtual-addr array))
    (setq offset (* *word-size* (mod vaddr si:page-size)))
    (setf (multibus-dma.array-offset *user-multibus*) offset)
    (setf (multibus-dma.free-area *user-multibus*) (list (list offset size)))
    (dotimes (j n-pages)
      (push (+ base-page j) mbs)
      (push (lambda-vir-to-nubus-physical-page
              (+ vaddr (quotient (* j *multibus-page-size*) *word-size*)))
            nbs))
    (mapc #'setup-mapping-register mbs nbs)
    (setf (multibus-dma.nubus-pages *user-multibus*) (nreverse nbs))
    (setf (multibus-dma.multibus-pages *user-multibus*) (nreverse mbs))
    *user-multibus*))


(DEFUN ARRAY-DATA-BASE-VIRTUAL-ADDR (ARRAY)
  (%POINTER-PLUS
    (%POINTER ARRAY)
    (SI:ARRAY-DATA-OFFSET ARRAY)))

(DEFUN LAMBDA-VIR-TO-NUBUS-PHYSICAL-PAGE (LAMBDA-VIRTUAL-ADDR)
  (COMPILER:%NUBUS-PHYSICAL-ADDRESS
    (LDB
      #O1016
      (SI:%PHYSICAL-ADDRESS LAMBDA-VIRTUAL-ADDR))))


(DEFUN LAMBDA-VIR-TO-NUBUS-PHYSICAL-ADDRESS (LAMBDA-VIRTUAL-ADDR)
  (LET ((PA (SI:%PHYSICAL-ADDRESS LAMBDA-VIRTUAL-ADDR)))
    (ash (+ (* (COMPILER:%NUBUS-PHYSICAL-ADDRESS (LDB #O1016 PA))
               si:page-size)
            (ldb #o0010 PA))
         2)))


(DEFUN SETUP-MAPPING-REGISTER (MULTIBUS-PAGE NUBUS-PAGE)
  (SI:WRITE-MULTIBUS-MAPPING-REGISTER
    MULTIBUS-PAGE
    (LOGIOR (ASH 1 23.) NUBUS-PAGE)))


(defun make-multibus-io-registers (name start length)
  (let ((a (make-array (* length 4)
                       :type 'art-8b
                       :named-structure-symbol 'multibus-registers
                       :leader-length 5
                       :displaced-to (%pointer-plus si:multibus-io-virtual-address
                                                    start))))
    (setf (array-leader a 3) name)
    (setf (array-leader a 4) start)
    a))

(defun make-multibus-memory-registers (name start length)
  (check-arg start (and (fixp start) (zerop (mod start 4))) "a multiple of 4")
  (let ((a (make-array (ceiling length 4)
                       :type 'art-32b
                       :named-structure-symbol 'multibus-registers
                       :leader-length 5
                       :displaced-to (%pointer-plus si:multibus-virtual-address
                                                    (// start 4)))))
    (setf (array-leader a 3) name)
    (setf (array-leader a 4) start)
    a))

(defmacro aref4i (array j)
  `(aref ,array (ash ,j 2)))

(defun aref4i-f (array j)
  (aref array (ash j 2)))

(defun multibus-memory-register-read-8 (x offset)
  (%multibus-read-8 (+ (array-leader x 4) offset)))

(defun multibus-memory-register-write-8 (x offset value)
  (%multibus-write-8 (+ (array-leader x 4) offset) value))


(defun multibus-memory-register-read-16 (x offset)
  (%multibus-read-16 (+ (array-leader x 4) offset)))

(defun multibus-memory-register-write-16 (x offset value)
  (%multibus-write-16 (+ (array-leader x 4) offset) value))

(defun multibus-memory-register-read-32 (x offset)
  (%multibus-read-32 (+ (array-leader x 4) offset)))

(defun multibus-memory-register-write-32 (x offset value)
  (%multibus-write-32 (+ (array-leader x 4) offset) value))

(defsetf multibus-memory-register-read-8 multibus-memory-register-write-8)
(defsetf multibus-memory-register-read-16 multibus-memory-register-write-16)
(defsetf multibus-memory-register-read-32 multibus-memory-register-write-32)

(defun immediate-binary-arrayp (x)
  (and (not (array-indirect-p x))
       (memq (array-type x) '(art-string art-1b art-2b art-8b art-4b
                                         art-16b art-32b))))

(defun array-elements-per-word (array)
  (cdr (assq (array-type array) array-elements-per-q)))

(defun good-start-indexp (start array)
  (and (fixp start)
       (>= start 0)
       (< start (length array))
       (zerop (mod start (array-elements-per-word array)))))

(defun good-end-indexp (start end array)
  (and (fixp start)
       (>= end start)
       (<= end (length array))
       (zerop (mod end (array-elements-per-word array)))))

(defun blit-array-portion (from-array from-start from-end to-array to-start to-end)
  "Like COPY-ARRAY-PORTION but works only on word-aligned data in unstructured arrays"
  (check-arg from-array immediate-binary-arrayp "immediate binary array")
  (check-arg from-array immediate-binary-arrayp "immediate binary array")
  (check-arg from-start (good-start-indexp from-start from-array) "good")
  (check-arg from-end (good-end-indexp from-start from-end from-array) "good")
  (check-arg to-start (good-start-indexp to-start to-array) "good")
  (check-arg to-end (good-end-indexp to-start to-end to-array) "good")
  (let ((from-mod (array-elements-per-word from-array))
        (to-mod (array-elements-per-word to-array)))
    (let ((data-length (// (- from-end from-start) from-mod)))
      (check-arg to-end (= data-length (// (- to-end to-start) to-mod)) "good")
      (%blit-array-portion from-array
                           (// from-start from-mod)
                           to-array
                           (// to-start to-mod)
                           data-length))))

(defun %blit-array-portion (from-array from-array-word-offset
                           to-array   to-array-word-offset
                           word-count)
  (%blt (%pointer-plus (%pointer-plus from-array
                                      (si:array-data-offset from-array))
                       from-array-word-offset)
        (%pointer-plus (%pointer-plus to-array
                                      (si:array-data-offset to-array))
                       to-array-word-offset)
        word-count
        1))

(defselect ((multibus-registers si:named-structure-invoke))
  (:print-self (object stream ignore ignore)
    (si:printing-random-object (object stream :typep)
      (format stream "for ~A starting at #x~16r"
              (array-leader object 3)
              (array-leader object 4))))
  (:describe (struct)
    (format t "~&~S:" struct)
    (let ((alist (get (array-leader struct 3) 'multibus-registers)))
      (cond ((null alist)
             (format t "~%No register description property~%"))
            ('else
             (format t "~%Register contents:~%")
             (dolist (x alist)
               (cond ((eq :write (reg-spec.access-mode x))
                      (format t "~A ~A is write only: ~{~A~^ ~}~%"
                              (array-leader struct 3)
                              (car (last (reg-spec.names x)))
                              (reg-spec.fields x)))
                     ('else
                      (multibus-register-print (array-leader struct 3)
                                               (car (last (reg-spec.names x))))
                      (terpri))))))))

  (:which-operations (ignore)
    '(:print-self :describe :which-operations)))

#|
A MULTIBUS-REGISTERS property is an alist of elements:
 ((<register-name> <alias1> <alias2> ...) <offset-location> <access-mode> <fields>)
The fields: is a list of (bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0)
giving names for all the bits. A name of 0 or 1 is a constant, X means no-care,
and more than one name in a row says to combine those bits into an integer.

|#

(defmacro define-multibus-io-registers (name array-variable &body registers)
  `(eval-when (eval compile load)
     (*define-multibus-registers ',name 'multibus-io-space ',array-variable ',registers)))


(defmacro define-multibus-memory-registers (name array-variable &body registers)
  `(eval-when (eval compile load)
     (*define-multibus-registers ',name 'multibus-memory-space ',array-variable ',registers)))

(defun *define-multibus-registers (name type array-variable registers)
  (when (record-source-file-name name 'define-multibus-registers)
    (setf (get name 'multibus-register-array) array-variable)
    (setf (get name 'multibus-registers) registers)
    (ecase type
      (multibus-io-space
       (putprop name 'aref4i 'aref-macro)
       (putprop name 'aref4i-f 'aref-function))
      (multibus-memory-space
       (putprop name 'multibus-memory-register-read-8 'aref-macro)
       (putprop name 'multibus-memory-register-read-8 'aref-function)))
    name))

(defun get-reg-desc-spec (class name access)
  (declare (values array offset fields))
  (let ((desc (or (ass #'memq name (get class 'multibus-registers))
                  (ferror nil "no ~a register named ~A" class name))))
    (or (eq (reg-spec.access-mode desc) t)
        (eq access (reg-spec.access-mode desc))
        (eq access t)
        (ferror nil "trying to ~A ~A register ~A which is ~A only"
                class access name (caddr desc)))
    (values (get class 'multibus-register-array)
            (reg-spec.offset desc)
            (reg-spec.fields desc)
            desc)))


(defun get-reg-desc (class name access)
  (multiple-value-bind (array offset fields desc)
      (get-reg-desc-spec class name access)
    (values `(,(get class 'aref-macro) ,array ,offset) fields desc)))

(defun byte-access-bit-spec (bit-name descriptor)
  (declare (values width position))
  (do ((position (length descriptor))
       (size nil)
       (l descriptor (cdr l)))
      ((null l)
       (if size
           (values size (- position size))
         (ferror nil "bit named ~A not found in ~A" bit-name descriptor)))
    (cond ((eq (car l) bit-name)
           (if size (incf size) (setq size 1)))
          ((not size)
           (decf position))
          ((memq bit-name (cdr l))
           (ferror nil "bits for ~A are not contiguous in ~S" bit-name descriptor)))))

(defun byte-access-bit (bit-name descriptor)
  (cons 'byte (multiple-value-list (byte-access-bit-spec bit-name descriptor))))

(defmacro multibus-register-read (class register &rest bits)
  (multiple-value-bind (word fields)
      (get-reg-desc class register :read)
    (cond ((null bits)
           word)
          ((null (cdr bits))
           `(ldb ,(byte-access-bit (car bits) fields) ,word))
          ('else
           (let ((temp (gentemp "reg")))
             `(let ((,temp ,word))
                ;; the compiler produces nice code for
                ;; lexically aparent calls to MULTIPLE-VALUE-BIND
                ;; and VALUES.
                (values ,@(mapcar #'(lambda (bit)
                                      `(ldb ,(byte-access-bit bit fields) ,temp))
                                  bits))))))))

(defsetf multibus-register-read multibus-register-write)

(defmacro dpbs (word &rest ppss-value-pairs)
  "A compositional n-arg generalization of DPB"
  (cond ((null ppss-value-pairs)
         word)
        ('else
         `(dpb ,(cadr ppss-value-pairs)
               ,(car ppss-value-pairs)
               (dpbs ,word ,@(cddr ppss-value-pairs))))))

(defmacro multibus-register-write (class register &rest bits)
  (multiple-value-bind (word fields desc)
      (get-reg-desc class register :write)
    (cond ((null bits)
           (ferror nil "no value to write specified for ~A ~A" class register))
          ((null (cdr bits))
           `(setf ,word ,(car bits)))
          ((= (length bits) (do ((uniques 0 (if (and (not (numberp (car l)))
                                                     (not (eq (car l) 'x))
                                                     (not (memq (car l) (cdr l))))
                                                (1+ uniques)
                                              uniques))
                                 (l fields (cdr l)))
                                ((null l)
                                 (* 2 uniques))))
           (do ((l bits (cddr l))
                (v nil (let ((value (cadr l))
                             (bit (car l)))
                         (cond ((atom bit))
                               ((eq (car bit) 'quote)
                                (setq bit (cadr bit))
                                (do ((syms (reg-spec.symbols desc) (cdr syms)))
                                    ((null syms)
                                     (ferror nil "Bit ~A value ~A not found"
                                             bit value))
                                  (when (and (equal bit (caar syms))
                                             (equal value (cadar syms)))
                                    (return (setq value (caddar syms)))))))
                         (list* value
                                (byte-access-bit bit fields)
                                v))))
               ((null l)
                `(setf ,word (dpbs ,(do ((j (1- (length fields)) (1- j))
                                         (f fields (cdr f))
                                         (default 0 (if (numberp (car f))
                                                        (dpb (car f)
                                                             (byte 1 j)
                                                             default)
                                                      default)))
                                        ((null f) default))
                                   ,@(nreverse v))))))
          ('else
           ;; If not fully specified, then, if register is readable, read the
           ;; current value, add the specified new bit values, then write it.
           ;; However, that is another operation that might better be
           ;; called multibus-register-update, since reading an i/o may
           ;; not be a device-transparent operation.
           (ferror nil "bit field values for ~A ~A not properly specified"
                   class register)))))


(defun multibus-register-read-symbolic (class register &optional bits)
  (multiple-value-bind (array offset fields)
      (get-reg-desc-spec class register :read)
    (let ((value (funcall (get class 'aref-function)  array offset)))
      (mapcar #'(lambda (bit)
                  (multiple-value-bind (width position)
                      (byte-access-bit-spec bit fields)
                    (list bit (ldb (byte width position) value))))
              bits))))


(defun multibus-register-print (class register &optional (stream standard-output))
  (multiple-value-bind (array offset fields)
      (get-reg-desc-spec class register :read)
    (let ((value (funcall (get class 'aref-function) (symeval array) offset))
          (result))
      (and stream
          (format stream "~A ~@6A #x~16,2,'0R" class register value))
      (do ((l fields (cdr l)))
          ((null l))
        (when (and (not (numberp (car l)))
                   (not (eq (car l) 'x))
                   (not (memq (car l) (cdr l))))
          (let ((field-sym (car l))
                (field-val (multiple-value-bind (width position)
                               (byte-access-bit-spec (car l) fields)
                             (ldb (byte width position) value))))
            (let ((field-sym-val (multibus-register-bit-field-disassemble
                                   class
                                   register
                                   field-sym
                                   field-val)))
              (and (memq stream '(t nil))
                   (push (list field-sym field-sym-val) result))
              (and stream (format stream " ~@6A ~D" field-sym field-sym-val))))))
      result)))


(defun multibus-register-bit-field-disassemble (class register field value)
  (multiple-value-bind (nil nil nil desc)
      (get-reg-desc-spec class register t)
    (do ((syms (reg-spec.symbols desc) (cdr syms)))
        ((null syms)
         value)
      (when (and (equal field (caar syms))
                 (equal value (caddar syms)))
        (return (cadar syms))))))


(defvar *multibus-interrupt-line-5* nil "an array which gets set by the SDU")

(defun setup-user-multibus-interrupts ()
  (setq *multibus-interrupt-line-5* (make-array 4 :type 'art-string))
  (si:wire-array *multibus-interrupt-line-5*)
  (si:passint 21
              (lambda-vir-to-nubus-physical-address
                (array-data-base-virtual-addr *multibus-interrupt-line-5*))))


(defun reset-multibus-interrupt-5 ()
  (and *multibus-interrupt-line-5*
       (setf (aref *multibus-interrupt-line-5* 0) 0)))

(defun process-wait-multibus-interrupt-5 (&optional (whostate "MultibusDevice"))
  (and *multibus-interrupt-line-5*
       (process-wait whostate
                     #'(lambda (x)
                         (not (zerop (aref x 0))))
                     *multibus-interrupt-line-5*)))



(defun setup-user-multibus ()
  (add-initialization "user multibus disable"
                      '(setq *user-multibus* nil
                             *user-multibus-interrupt-line-5* nil)
                      '(:before-cold))

  (ADD-INITIALIZATION "user multibus enable"
                      '(progn (setup-user-multibus-dma)
                              (setup-user-multibus-interrupts))
                      '(:NOW :WARM)))


(defun describe-user-multibus (&optional (x *user-multibus*))
  (format t "~&Multibus pages allowed,~%page hex-address~%")
  (dolist (page (multibus-dma.multibus-pages x))
    (format t "~4D ~16,6,'0R~%"
            page (* page *multibus-page-size*))))


(defstruct (user-multibus-dma-buffer :named (:conc-name user-multibus-dma-buffer.))
  array
  multibus-address)

(defun allocate-user-multibus-dma-buffer (size)
  (let ((offset (find-free-user-multibus-chunk size)))
    (make-user-multibus-dma-buffer
      array (make-array size
                        :type 'art-string
                        :displaced-to (multibus-dma.array *user-multibus*)
                        :displaced-index-offset offset)
      multibus-address (+ (multibus-dma.byte-address *user-multibus*)
                          (- offset (multibus-dma.array-offset *user-multibus*))))))



(defun find-free-user-multibus-chunk (size)
  size
  (with-lock ((multibus-dma.allocation-lock *user-multibus*))
    ;; look for the smallest chunk that satisfies the size
    ;; requirement.
    (caar (multibus-dma.free-area *user-multibus*))
    ))
