#| -*- Mode:LISP; Package:(MBU GLOBAL); Base:10; Fonts:(CPTFONTB) -*-





Copyright LISP Machine, Inc. 1985
   See filename "Copyright.Text" for
licensing and release information.

Multibus User device handling kit. 6/13/85 21:38:17 -George Carrette

This provides hi-level numerical and symbolic access to the following
three areas of interest:
 * Multibus I/O space, the I/O registers of a device.
 * Multibus memory space, mapped to lisp data arrays for fast DMA access.
 * Multibus interrupt lines.

|#

(defvar *user-multibus* nil)

(defstruct (multibus-dma :named (:conc-name multibus-dma.))
  page-address
  array-size
  phys-address
  array)

(defun setup-user-multibus-dma ()
  (setq *user-multibus*
        (make-multibus-dma
          page-address (SI:%SYSTEM-CONFIGURATION-USER-BASE-MULTIBUS-MAP
                         SI:*SYS-CONF*)
          array-size (* 1024
                        (SI:%SYSTEM-CONFIGURATION-USER-MULTIBUS-MAP-SIZE
                          SI:*SYS-CONF*))))
  (setf (multibus-dma.array *user-multibus*)
        (make-array (multibus-dma.array-size *user-multibus*)
                    :type 'art-8b))
  (si:wire-array (multibus-dma.array *user-multibus*))
  (setf (multibus-dma.phys-address *user-multibus*)
        (multibus-map-array (multibus-dma.array *user-multibus*)
                            (multibus-dma.page-address *user-multibus*)))
  *user-multibus*)

(defun multibus-map-array (array starting-multibus-page-addr)
  (LET ((ARRAY-VADR (ARRAY-DATA-BASE-VIRTUAL-ADDR array)))
    (DO ((TIMES 0 (ADD1 TIMES)))
        ((> TIMES
            (SUB1
              (CEILING (+ (LDB #O0010 ARRAY-VADR)
                          (// (ARRAY-LENGTH array) 4))
                       SI:PAGE-SIZE))))
      (SETUP-MAPPING-REGISTER
        (+ starting-multibus-page-addr TIMES)
        (LAMBDA-VIR-TO-NUBUS-PHYSICAL-PAGE
          (+ (* TIMES SI:PAGE-SIZE) ARRAY-VADR))))

    (+ (ASH starting-multibus-page-addr 10.)
       (* (LDB #o0010 ARRAY-VADR) 4))))


(defselect ((multibus-io-registers si:named-structure-invoke))
  (:print-self (object stream ignore ignore)
    (si:printing-random-object (object stream :typep)
      (format stream "for ~A starting at #x~16r"
              (array-leader object 3)
              (array-leader object 4))))
  (:describe (struct)
    (format t "~&~S:" struct)
    (let ((alist (get (array-leader struct 3) 'multibus-io-registers)))
      (cond ((null alist)
             (format t "~%No register description property~%"))
            ('else
             (format t "~%Register contents:~%")
             (dolist (x alist)
               (cond ((eq :write (io-reg-spec.access-mode x))
                      (format t "~A ~A is write only: ~{~A~^ ~}~%"
                              (array-leader struct 3)
                              (car (last (io-reg-spec.names x)))
                              (io-reg-spec.fields x)))
                     ('else
                      (multibus-io-register-print (array-leader struct 3)
                                                  (car (last (io-reg-spec.names x))))
                      (terpri))))))))

  (:which-operations (ignore)
    '(:print-self :describe :which-operations)))


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
                       :named-structure-symbol 'multibus-io-registers
                       :leader-length 5
                       :displaced-to (%pointer-plus si:multibus-io-virtual-address
                                                    start))))
    (setf (array-leader a 3) name)
    (setf (array-leader a 4) start)
    a))

(defmacro aref4i (array j)
  `(aref ,array (ash ,j 2)))

#|
A MULTIBUS-IO-REGISTERS property is an alist of elements:
 ((<register-name> <alias1> <alias2> ...) <offset-location> <access-mode> <fields>)
The fields: is a list of (bit7 bit6 bit5 bit4 bit3 bit2 bit1 bit0)
giving names for all the bits. A name of 0 or 1 is a constant, X means no-care,
and more than one name in a row says to combine those bits into an integer.

|#

(defstruct (io-reg-spec (:conc-name io-reg-spec.) :list)
  names
  offset
  access-mode
  fields
  symbols)

(defun get-io-reg-desc-spec (class name access)
  (declare (values array offset fields))
  (let ((desc (or (ass #'memq name (get class 'multibus-io-registers))
                  (ferror nil "no ~a register named ~A" class name))))
    (or (eq (io-reg-spec.access-mode desc) t)
        (eq access (io-reg-spec.access-mode desc))
        (eq access t)
        (ferror nil "trying to ~A ~A register ~A which is ~A only"
                class access name (caddr desc)))
    (values (get class 'multibus-io-register-array)
            (io-reg-spec.offset desc)
            (io-reg-spec.fields desc)
            desc)))


(defun get-io-reg-desc (class name access)
  (multiple-value-bind (array offset fields desc)
      (get-io-reg-desc-spec class name access)
    (values `(aref4i ,array ,offset) fields desc)))

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


(defmacro multibus-io-register-read (class register &rest bits)
  (multiple-value-bind (word fields)
      (get-io-reg-desc class register :read)
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

(defsetf multibus-io-register-read multibus-io-register-write)

(defmacro dpbs (word &rest ppss-value-pairs)
  "A compositional n-arg generalization of DPB"
  (cond ((null ppss-value-pairs)
         word)
        ('else
         `(dpb ,(cadr ppss-value-pairs)
               ,(car ppss-value-pairs)
               (dpbs ,word ,@(cddr ppss-value-pairs))))))

(defmacro multibus-io-register-write (class register &rest bits)
  (multiple-value-bind (word fields desc)
      (get-io-reg-desc class register :write)
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
                                (do ((syms (io-reg-spec.symbols desc) (cdr syms)))
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
           ;; called multibus-io-register-update, since reading an i/o may
           ;; not be a device-transparent operation.
           (ferror nil "bit field values for ~A ~A not properly specified"
                   class register)))))


(defun multibus-io-register-read-symbolic (class register &optional bits)
  (multiple-value-bind (array offset fields)
      (get-io-reg-desc-spec class register :read)
    (let ((value (aref4i array offset)))
      (mapcar #'(lambda (bit)
                  (multiple-value-bind (width position)
                      (byte-access-bit-spec bit fields)
                    (list bit (ldb (byte width position) value))))
              bits))))


(defun multibus-io-register-print (class register &optional (stream standard-output))
  (multiple-value-bind (array offset fields)
      (get-io-reg-desc-spec class register :read)
    (let ((value (aref4i (symeval array) offset))
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
            (let ((field-sym-val (multibus-io-register-bit-field-disassemble
                                   class
                                   register
                                   field-sym
                                   field-val)))
              (and (memq stream '(t nil))
                   (push (list field-sym field-sym-val) result))
              (and stream (format stream " ~@6A ~D" field-sym field-sym-val))))))
      result)))


(defun multibus-io-register-bit-field-disassemble (class register field value)
  (multiple-value-bind (nil nil nil desc)
      (get-io-reg-desc-spec class register t)
    (do ((syms (io-reg-spec.symbols desc) (cdr syms)))
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
