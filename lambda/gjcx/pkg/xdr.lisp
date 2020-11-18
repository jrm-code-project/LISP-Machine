;;; -*- Mode:LISP; Package:XDR; Readtable:CL; Base:10 -*-

;;;  (c) LISP Machine, Inc. 1986
;;;      All Rights Reserved
;;;  See filename "copyright.text" for
;;;  further information.

;;; (C) Copyright 1987, LISP MACHINE INC
;;; See filename "Copyright.Text" for more information.
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


;;; Implementation of XDR in Common Lisp.
;;; Following the "External Data Representation Protocol Specification"
;;; Published by Sun Microsystems, Revision B of 17 February 1986.
;;;
;;; Design Discussion in "xdr.notes"
;;; 17-Jul-86 09:30:33 -George Carrette


(defstruct (xdr (:conc-name xdr_) (:print-function xdr-print-function))
  (op :free) ; may be also :ENCODE or :DECODE
  getlong
  putlong
  getbytes
  putbytes
  getpostn
  setpostn
  inline
  destroy)

(defun xdr-print-function (object stream ignore)
  (format stream "#<~A ~S>" (type-of object) (xdr_op object)))

(defstruct (xdrstdio (:conc-name xdrstdio_) (:include xdr))
  stream)

(defstruct (xdrmem (:conc-name xdrmem_) (:include xdr) (:print-function xdrmem-print-function))
  array
  index
  length)

(defstruct (xdrsize (:conc-name xdrsize_) (:include xdr) (:print-function xdrsize-print-function))
  size)

(defstruct (xdrrec (:conc-name xdrrec_) (:include xdr))
  )


(defun xdrmem-print-function (object stream ignore)
  (format stream "#<~A ~S ~D ~D>"
          (type-of object) (xdr_op object)
          (xdrmem_index object) (xdrmem_length object)))

(defun xdrsize-print-function (object stream ignore)
  (format stream "#<XDRSIZE ~D bytes>" (xdrsize_size object)))


(defun xdrstdio_create (x_op fp)
  "Returns an XDR object that writes or reads characters on FP"
  (let ((xdrs (make-xdrstdio :op x_op)))
    (setf (xdrstdio_stream xdrs) fp)
    (setf (xdr_inline xdrs) 'stdio-inline)
    (setf (xdr_destroy xdrs) 'stdio-destroy)
    (ecase x_op
      (:encode
       (setf (xdr_putlong xdrs) 'stdio-putlong)
       (setf (xdr_putbytes xdrs) 'stdio-putbytes))
      (:decode
       (setf (xdr_getlong xdrs) 'stdio-getlong)
       (setf (xdr_getbytes xdrs) 'stdio-getbytes)))
    xdrs))

(defun xdrmem_create (x_op array &optional (start 0) (end (length array)))
  "returns an XDR object to write data into array"
  (let ((xdrs (make-xdrmem :op x_op)))
    (setf (xdrmem_array xdrs) array)
    (setf (xdrmem_index xdrs) start)
    (setf (xdrmem_length xdrs) end)
    (setf (xdr_inline xdrs) 'mem-inline)
    (setf (xdr_destroy xdrs) 'mem-destroy)
    (setf (xdr_getpostn xdrs) 'mem-getpostn)
    (ecase x_op
      (:encode
       (setf (xdr_putlong xdrs) 'mem-putlong)
       (setf (xdr_putbytes xdrs) 'mem-putbytes))
      (:decode
       (setf (xdr_getlong xdrs) 'mem-getlong)
       (setf (xdr_getbytes xdrs) 'mem-getbytes)))
    xdrs))


(defun xdrsize_create ()
  "returns an XDR object to find the size of data"
  (let ((xdrs (make-xdrsize :op :encode)))
    (setf (xdrsize_size xdrs) 0)
    (setf (xdr_putlong xdrs) 'size-putlong)
    (setf (xdr_putbytes xdrs) 'size-putbytes)
    (setf (xdr_getpostn xdrs) 'size-getpostn)
    (setf (xdr_setpostn xdrs) 'size-setpostn)
    xdrs))

(defun xdrrec_create (sendsize recvsize iohandle readproc writeproc)
  sendsize recvsize iohandle readproc writeproc
  (error "unimplemented"))


(defun xdrrec_endofrecord (xdrs flushnow)
  (check-type xdrs xdrrec)
  flushnow
  (error "unimplemented"))


(defun xdrrec_skiprecord (xdrs)
  (check-type xdrs xdrrec)
  (error "unimplemented"))

(defun exdrrec_eof (xdrs)
  (check-type xdrs xdrrec)
  (error "unimplemented"))


(defun xdr_destroy (xdrs)
  (check-type xdrs xdr)
  (funcall (xdr_destroy xdrs) xdrs))


(defun xdr_inline (xdrs)
  (check-type xdrs xdr)
  (funcall (xdr_inline xdrs) xdrs))


(defun xdr_getpos (xdrs)
  (check-type xdrs xdr)
  (funcall (or (xdr_getpostn xdrs)
               (error "operation not handled"))
           xdrs))

(defun xdr_setpos (xdrs n)
  (check-type xdrs xdr)
  (funcall (or (xdr_setpostn xdrs)
               (error "operation not handled"))
           xdrs
           n))


;;; xdr stream implementation.

(defun stdio-getlong (xdrs)
  (let ((stream (xdrstdio_stream xdrs)))
    (dpb (read-byte stream)
         (byte 8 24)
         (dpb (read-byte stream)
              (byte 8 16)
              (dpb (read-byte stream)
                   (byte 8 8)
                   (read-byte stream))))))


(defun stdio-putlong (xdrs x)
  (let ((stream (xdrstdio_stream xdrs)))
    (write-byte (ldb (byte 8 24) x) stream)
    (write-byte (ldb (byte 8 16) x) stream)
    (write-byte (ldb (byte 8 8) x) stream)
    (write-byte (ldb (byte 8 0) x) stream)))

(defun stdio-putbytes (xdrs string start end)
  ;; whats the CL equivalent of string-out ?
  #+LMI
  (send (xdrstdio_stream xdrs) :string-out string start end)
  #-LMI
  (do ((j start (1+ start)))
      ((= j end))
    (write-char (aref string j) (xdrstdio_stream xdrs))))


(defun stdio-getbytes (xdrs string start end)
  #+LMI
  (send (xdrstdio_stream xdrs) :string-in nil string start end)
  #-LMI
  (do ((j start (1+ start)))
      ((= j end))
    (setf (aref string j) (read-char (xdrstdio_stream xdrs)))))


(defun stdio-inline (xdrs)
  xdrs
  ())

(defun stdio-destroy (xdrs)
  (force-output (xdrstdio_stream xdrs))
  (setf (xdr_op xdrs) :free))


(defun mem-getlong (xdrs)
  (let ((i (xdrmem_index xdrs))
        (a (xdrmem_array xdrs)))
    (prog1 (dpb (char-code (aref a i))
                (byte 8 24)
                (dpb (char-code (aref a (setq i (1+ i))))
                     (byte 8 16)
                     (dpb (char-code (aref a (setq i (1+ i))))
                          (byte 8 8)
                          (char-code (aref a (setq i (1+ i)))))))
           (setf (xdrmem_index xdrs) (1+ i)))))

(defun mem-putlong (xdrs x)
  (let ((i (xdrmem_index xdrs))
        (a (xdrmem_array xdrs)))
    (aset (ldb (byte 8 24) x) a i)
    (aset (ldb (byte 8 16) x) a (setq i (1+ i)))
    (aset (ldb (byte 8 8) x) a (setq i (1+ i)))
    (aset (ldb (byte 8 0) x) a (setq i (1+ i)))
    (setf (xdrmem_index xdrs) (1+ i))))


(defun mem-putbytes (xdrs string start end)
  (let ((i (xdrmem_index xdrs)))
    (copy-array-portion string start end
                        (xdrmem_array xdrs) i (setq i (+ (- end start) i)))
    (setf (xdrmem_index xdrs) i)))

(defun mem-getbytes (xdrs string start end)
  (let ((i (xdrmem_index xdrs)))
    (copy-array-portion (xdrmem_array xdrs) i (setq i (+ (- end start) i))
                        string start end)
    (setf (xdrmem_index xdrs) i)))


(defun mem-inline (xdrs)
  xdrs
  ())

(defun mem-destroy (xdrs)
  xdrs
  (setf (xdr_op xdrs) :free))


(defun mem-getpostn (xdrs)
  (xdrmem_index xdrs))

(defun size-putlong (xdrs x)
  x
  (incf (xdrsize_size xdrs) 4))

(defun size-putbytes (xdrs string start end)
  string
  (incf (xdrsize_size xdrs) (- end start)))

(defun size-getpostn (xdrs)
  (xdrsize_size xdrs))

(defun size-setpostn (xdrs n)
  (setf (xdrsize_size xdrs) n))


;; XDR encode/decode procedures

(defun get-xdr-property (type kind)
  (etypecase type
    (symbol
     (or (get type kind)
         (error "Undefined XDR type: ~S" type)))
    (cons
     (get-xdr-property (car type) kind))))

(defun xdr_transmit (xdrs object type)
  (check-type xdrs xdr)
  (funcall (get-xdr-property type 'xdr-transmit)
           xdrs
           object
           type))

(defun xdr_receive (xdrs type)
  (check-type xdrs xdr)
  (funcall (get-xdr-property type 'xdr-receive)
           xdrs
           type))

(defun (void xdr-transmit) (xdrs object type)
  xdrs object type
  nil)

(defun (void xdr-receive) (xdrs type)
  xdrs type
  ())



(defun (int xdr-transmit) (xdrs object type)
  type
  (funcall (xdr_putlong xdrs)
           xdrs
           (int->long object)))

(defun int->long (int)
  (cond ((minusp int)
         (cond ((< int (- (expt 2 31)))
                (error "number too small to be an int: ~S" int))
               ('else
                (+ int (expt 2 32)))))
        ((< int (expt 2 31))
         int)
        ('else
         (error "number too large to be an int: ~S" int))))


(defun long->int (long)
  (cond ((> long (expt 2 31))
         (- long (expt 2 32)))
        ('else
         long)))

(defun (int xdr-receive) (xdrs type)
  type
  (long->int (funcall (xdr_getlong xdrs) xdrs)))


(defun (u_int xdr-transmit) (xdrs object type)
  type
  (funcall (xdr_putlong xdrs) xdrs object))


(defun (u_int xdr-receive) (xdrs type)
  type
  (funcall (xdr_getlong xdrs) xdrs))


(defun (float xdr-receive) (xdrs type)
  xdrs
  type
  (error "unimplemented"))


(defun (double xdr-receive) (xdrs type)
  xdrs
  type
  (error "unimplemented"))


(defun (float xdr-transmit) (xdrs object type)
  xdrs object type
  (error "unimplemented"))

(defun (double xdr-transmit) (xdrs object type)
  xdrs object type
  (error "unimplemented"))


(defun xdr_enumeration (type alist &key relaxed)
  (let ((marker (intern (format nil "xdr_marker-~A" type) "XDR"))
        (max (apply #'max (mapcar #'cadr alist))))
    (when (record-xdr-source type)
          (setf (get type 'enumeration-marker) marker)
          (cond (relaxed
                 (setf (get type 'xdr-transmit) 'relaxed-enumeration-xdr-transmit)
                 (setf (get type 'xdr-receive) 'relaxed-enumeration-xdr-receive))
                ('else
                 (setf (get type 'xdr-transmit) 'enumeration-xdr-transmit)
                 (setf (get type 'xdr-receive) 'enumeration-xdr-receive)))
          (do ((l alist (cdr l))
               (array (make-array (1+ max))))
              ((null l)
               (setf (get type 'enumeration-table) array))
            (let ((symbol (caar l))
                  (value (cadr (car l))))
              (setf (aref array value) symbol)
              (setf (get symbol marker) value)))
          type)))


(defun enumeration-xdr-transmit (xdrs object type)
  (funcall (xdr_putlong xdrs)
           xdrs
           (enumeration->long object type)))

(defun enumeration-xdr-receive (xdrs type)
  (long->enumeration (funcall (xdr_getlong xdrs) xdrs) type))

(defun enumeration->long (object type)
  (or (get object (get type 'enumeration-marker))
      (error "Not an element of the ~S enumeration: ~S" type object)))

(defun long->enumeration (object type)
  (aref (get type 'enumeration-table) object))

(defun relaxed-enumeration-xdr-transmit (xdrs object type)
  (funcall (xdr_putlong xdrs)
           xdrs
           (etypecase object
             (symbol
              (enumeration->long object type))
             (number
              object))))

(defun relaxed-enumeration-xdr-receive (xdrs type)
  (let ((object (funcall (xdr_getlong xdrs) xdrs))
        (table (get type 'enumeration-table)))
    (cond ((< object (length table))
           (or (aref table object) object))
          ('else
           object))))

(defvar *bytesink* (make-string 4))

(defun padbytes-transmit (xdrs n)
  (let ((pad (- 4 (mod n 4))))
    (cond ((= pad 4))
          ('else
           (funcall (xdr_putbytes xdrs) xdrs *bytesink* 0 pad)))))

(defun padbytes-receive (xdrs n)
  (let ((pad (- 4 (mod n 4))))
    (cond ((= pad 4))
          ('else
           (funcall (xdr_getbytes xdrs) xdrs *bytesink* 0 pad)))))

(defun (string xdr-transmit) (xdrs object type)
  (let ((n (length object)))
    (or (atom type)
        (<= n (cadr type))
        (error "string longer than ~D: ~S" (cadr type) object))
    (funcall (xdr_putlong xdrs) xdrs n)
    (funcall (xdr_putbytes xdrs) xdrs object 0 n)
    (padbytes-transmit xdrs n)))

(defun (string xdr-receive) (xdrs type)
  (let ((n (funcall (xdr_getlong xdrs) xdrs))
        (s))
    (or (atom type)
        (<= n (cadr type))
        (error "string longer than ~D" (cadr type)))
    (setq s (make-string n))
    (funcall (xdr_getbytes xdrs) xdrs s 0 n)
    (padbytes-receive xdrs n)
    s))

(defun (opaque xdr-transmit) (xdrs object type)
  (let ((n (cadr type)))
    (funcall (xdr_putbytes xdrs) xdrs object 0 n)
    (padbytes-transmit xdrs n)))

(defun (opaque xdr-receive) (xdrs type)
  (let* ((n (cadr type))
         (s (make-string n)))
    (funcall (xdr_getbytes xdrs) xdrs s 0 n)
    (padbytes-receive xdrs n)
    s))

(defun (array xdr-transmit) (xdrs object type)
  (let ((n (length object))
        (max (or (caddr type) 10000)))
    (if (> n max)
        (error "array longer than ~D" max))
    (xdr_transmit xdrs n 'int)
    (dotimes (j n)
      (xdr_transmit xdrs (aref object j) (cadr type)))))

(defun (array xdr-receive) (xdrs type)
  (let ((n (xdr_receive xdrs 'int))
        (a)
        (max (or (caddr type) 10000)))
    (if (> n max)
        (error "array longer than ~D" max))
    (setq a (make-array n))
    (dotimes (j n)
      (setf (aref a j) (xdr_receive xdrs (cadr type))))
    a))


(defstruct (descriminated-union (:conc-name "") (:print-function print-descriminated-union))
  descriminator
  descriminated-value)

(defun xdr_make_descrimination (descriminator value)
  (make-descriminated-union :descriminator descriminator
                            :descriminated-value value))


(defun print-descriminated-union (object stream ignore)
  (format stream "#<DESCRIMINATED ~S ~S>" (descriminator object) (descriminated-value object)))

(defun xdr_descriminated_union (typename descrim alist)
  (when (record-xdr-source typename)
    (setf (get typename 'xdr-transmit) 'descriminated-union-xdr-transmit)
    (setf (get typename 'xdr-receive) 'descriminated-union-xdr-receive)
    (setf (get typename 'descriminated-union) (cons descrim alist))
    typename))

(defun descriminated-union-xdr-transmit (xdrs object type)
  (check-type object descriminated-union)
  (let ((info (get type 'descriminated-union)))
    (xdr_transmit xdrs (descriminator object) (car info))
    (xdr_transmit xdrs
                  (descriminated-value object)
                  (cadr (or (assq (descriminator object) (cdr info))
                            (assq 'default (cdr info)))))))


(defun descriminated-union-xdr-receive (xdrs type)
  (let* ((info (get type 'descriminated-union))
         (descriminator (xdr_receive xdrs (car info))))
    (make-descriminated-union
      :descriminator descriminator
      :descriminated-value (xdr_receive xdrs
                                        (cadr (or (assq descriminator (cdr info))
                                                  (assq 'default (cdr info))))))))


;; sometimes it is not worth given a name for a descrimination
;; so we say (descrimination dtype . cases)

(defun (descrimination xdr-transmit) (xdrs object type)
  (check-type object descriminated-union)
  (let ((d (descriminator object))
        (info (cdr type)))
    (xdr_transmit xdrs d (car info))
    (xdr_transmit xdrs
                  (descriminated-value object)
                  (cadr (or (assq d (cdr info))
                            (assq 'default (cdr info)))))))


(defun (descrimination xdr-receive) (xdrs type)
  (let* ((info (cdr type))
         (descriminator (xdr_receive xdrs (car info))))
    (make-descriminated-union
      :descriminator descriminator
      :descriminated-value (xdr_receive xdrs
                                        (cadr (or (assq descriminator (cdr info))
                                                  (assq 'default (cdr info))))))))





(defstruct (xdr_structure (:print-function print-xdr-structure)
                          (:constructor cons-xdr-structure (type array)))
  type
  array)

(defun print-xdr-structure (object stream ignore)
  (format stream "#<~S" (xdr_structure-type object))
  (do ((j 0 (1+ j))
       (l (get (xdr_structure-type object) 'xdr-structure-slots) (cdr l)))
      ((null l))
    (format stream " ~S ~S" (car l) (aref (xdr_structure-array object) j)))
  (princ ">" stream))

(defun xdr_make_structure (type &rest elements)
  (let* ((slots (or (get type 'xdr-structure-slots)
                    (error "unknown structure type: ~S" type)))
         (array (make-array (length slots))))
    (do ((l elements (cddr l)))
        ((null l)
         (cons-xdr-structure type array))
      (setf (aref array (or (find-position-in-list (car l) slots)
                            (error "no slot named ~S in ~S structure"
                                   (car l) type)))
            (cadr l)))))

(defun xdr_structure_ref (object slotname)
  (aref (xdr_structure-array object)
        (or (find-position-in-list slotname
                                   (get (xdr_structure-type object) 'xdr-structure-slots))
            (error "no slot named ~S in ~S structure"
                   slotname
                   (xdr_structure-type object)))))


(defsetf xdr_structure_ref xdr_structure_set)

(defun xdr_structure_set (object slotname value)
  (setf (aref (xdr_structure-array object)
              (find-position-in-list slotname
                                     (get (xdr_structure-type object) 'xdr-structure-slots)))
        value))

(defun xdr_structure (typename elements)
  "Elements is a list of (slot-name slot-type)"
  (when (record-xdr-source typename)
    (setf (get typename 'xdr-transmit) 'structure-xdr-transmit)
    (setf (get typename 'xdr-receive) 'structure-xdr-receive)
    (setf (get typename 'xdr-structure) (mapcar #'cadr elements))
    (setf (get typename 'xdr-structure-slots) (mapcar #'car elements))
    typename))


(defun structure-xdr-transmit (xdrs object type)
  (check-type object xdr_structure)
  (do ((j 0 (1+ j))
       (l (get type 'xdr-structure) (cdr l))
       (array (xdr_structure-array object)))
      ((null l))
    (xdr_transmit xdrs (aref array j) (car l))))

(defun structure-xdr-receive (xdrs type)
  (let ((types (get type 'xdr-structure)))
    (do ((array (make-array (length types)))
         (j 0 (1+ j))
         (l types (cdr l)))
        ((null l)
         (cons-xdr-structure type array))
      (setf (aref array j) (xdr_receive xdrs (car l))))))


(defun (list xdr-transmit) (xdrs object type)
  (let ((element-type (cadr type)))
    (dolist (element object)
      (xdr_transmit xdrs t 'boole)
      (xdr_transmit xdrs element element-type))
    (xdr_transmit xdrs nil 'boole)))

(defun (list xdr-receive) (xdrs type)
  (do ((element-type (cadr type))
       (list nil (cons (xdr_receive xdrs element-type) list)))
      ((null (xdr_receive xdrs 'boole))
       (nreverse list))))


(defun xdr_typedef (type definition)
  (when (record-xdr-source type)
    (setf (get type 'xdr_typedef) definition)
    (setf (get type 'xdr-transmit) 'typedef-xdr-transmit)
    (setf (get type 'xdr-receive) 'typedef-xdr-receive)
    type))


(defun typedef-xdr-transmit (xdrs object type)
  (xdr_transmit xdrs object (get type 'xdr_typedef)))

(defun typedef-xdr-receive (xdrs type)
  (xdr_receive xdrs (get type 'xdr_typedef)))


(defun record-xdr-source (type)
  #+LMI (si:record-source-file-name type 'xdr)
  #-LMI t)


(xdr_enumeration 'boole
                 '((nil 0)
                   (t 1)))
