;;; -*- Mode:LISP; Package:(COLD :use (K LISP)); Readtable:CL; Base:10 -*-

;;;; Cold Loader

;;; Builds a Cold Load


(defparameter *cold-files*
              '("jb:k;trap.kbin"
                "jb:k;nuclear-control.kbin"
                "jb:k;gc-ram.kbin"
                "jb:k;datatype-ram.kbin"
                "jb:k;memory-map.kbin"
                "jb:k;vmem.kbin"
                "jb:k;transporter-ram.kbin"
                "jb:k;timers.kbin"
                "jb:k;pcd-table.kbin"
                "jb:k;quantum-map.kbin"
                "jb:k;map-fault.kbin"
                "jb:k;memory-management.kbin"
                "jb:k;region-bits.kbin"
                "jb:k;region-data.kbin"
                "jb:k;gc-fault.kbin"
                "jb:k;cons.kbin"
                "jb:k;area-data.kbin"
                "jb:k;nubus-interrupts.kbin"
                "jb:k.array;array.kbin"
                "jb:k;symbols.kbin"
                "jb:kbug2;common-definitions.kbin"
                "jb:kbug2;streams.kbin"
                "jb:kbug2;k2.kbin"
                "jb:k;warm-loader.kbin"
                "jb:k;boot.kbin"))

(defconstant *cold-data-size* (ash 77. 10.)
  "Size of cold load data space in words")

(defconstant *cold-data* (make-array (ash *cold-data-size* 2)
                                     :element-type '(unsigned-byte 16.)))

(defconstant *cold-code-size* (ash 16 10.)
  "Size of cold load code space in words")

(defconstant *cold-code* (make-array (ash *cold-code-size* 2)
                                     :element-type '(unsigned-byte 16.)))

(defconstant cold-code-start 0)

(defvar *cold-code-pointer* cold-code-start)
(defvar *cold-loaded-functions* '())

(defmacro cold-data-read (address)
  `(let ((.address. (ash ,address 1)))
     (logior (aref *cold-data* .address.)
             (ash (aref *cold-data* (1+ .address.))
                  16.))))

(defmacro cold-data-write (address data)
  `(let ((.address. (ash ,address 1))
         (.data. ,data))
     (setf (aref *cold-data* .address.)      (logand .data. #xFFFF))
     (setf (aref *cold-data* (1+ .address.)) (ash .data. -16.))))


(defun cold-code-read (code-address)
  (let ((address (ash code-address 2)))
    (logior (aref *cold-code* address)
            (ash (aref *cold-code* (+ address 1)) 16.)
            (ash (aref *cold-code* (+ address 2)) 32.)
            (ash (aref *cold-code* (+ address 3)) 48.))))

(defun cold-code-write (code-address data)
  (let ((address (ash code-address 2)))
    (setf (aref *cold-code* address)       (ldb (byte 16. 0) data))
    (setf (aref *cold-code* (+ 1 address)) (ldb (byte 16. 16.) data))
    (setf (aref *cold-code* (+ 2 address)) (ldb (byte 16. 32.) data))
    (setf (aref *cold-code* (+ 3 address)) (ldb (byte 16. 48.) data))))



;;;; Cold Load Initial Code

(defvar *cold-files-loaded* '())

(defun same-file-except-for-version (file1 file2)
  (eq (zl:send file1 :generic-pathname)
      (zl:send file2 :generic-pathname)))

(defun cold-file-needs-loading? (file)
  (not (member file *cold-files-loaded*)))

(defun record-cold-file-loaded (pathname)
  (setq *cold-files-loaded*
        (cons pathname
              (delete pathname *cold-files-loaded*
                      :test #'same-file-except-for-version))))

(defvar *cold-functions-loaded* '())

(defun make-cold-function-entry (compiled-function-structure source-file code)
  (list compiled-function-structure source-file code))

(defsetf cold-function-entry-structure (entry) (new-value)
  `(setf (first ,entry) ,new-value))

(defun cold-function-entry-source-file (entry)
  (second entry))

(defsetf cold-function-entry-source-file (entry) (new-value)
  `(setf (second ,entry) ,new-value))

(defsetf cold-function-entry-code (entry) (new-value)
  `(setf (third ,entry) ,new-value))

(defun enter-compiled-function (structure source-file code)
  (let ((entry (assoc structure *cold-functions-loaded*
                      :test #'(lambda (s1 s2) (eq (nc::ncompiled-function-name s1)
                                                  (nc::ncompiled-function-name s2))))))
    (if (null entry)
        (setq *cold-functions-loaded*
              (append *cold-functions-loaded* (list (make-cold-function-entry structure source-file code))))
        (progn (setf (cold-function-entry-structure entry) structure)
               (setf (cold-function-entry-source-file entry) source-file)
               (setf (cold-function-entry-code        entry) code)))))

(zl:defsubst cold-read (stream)
  (read stream))

(zl:defsubst cold-read-byte (stream)
  (read-byte stream))

(defun cold-read-instruction (stream)
  (let ((i
          (logior
            (ash (cold-read-byte stream)  0.)
            (ash (cold-read-byte stream)  8.)

            (ash (cold-read-byte stream) 16.)
            (ash (cold-read-byte stream) 24.)


            (ash (cold-read-byte stream) 32.)
            (ash (cold-read-byte stream) 40.)

            (ash (cold-read-byte stream) 48.)
            (ash (cold-read-byte stream) 56.))))
;    (format t "~%~s" (nc::dis i))
    i))

(defun cold-load-code (stream)
  (let ((size-in-instructions (cold-read stream)))
    (do ((count size-in-instructions (1- count))
         (instructions '() (cons (cold-read-instruction stream) instructions)))
        ((zerop count) (reverse instructions)))))

(defun create-cold-compiled-function (name local-refs refs immediates length)
  (let ((fcn (nc::make-ncompiled-function
               :name name
               :starting-address nil
               :local-refs local-refs
               :refs refs
               :immediates immediates
               :length length
               :code :in-cold-load)))
    (push fcn *cold-loaded-functions*)
    (setf (nc::nsymbol-function name) fcn)))

(defun find-named-function (name)
  (find name *cold-loaded-functions* :key #'nc::ncompiled-function-name))

(defun cold-disassemble (fcn)
  (format t "~&In cold load:")
  (if (symbolp fcn) (setq fcn (nc::get-ncompiled-function fcn)))
  (let ((sa (nc::ncompiled-function-starting-address fcn)))
    (let ((*print-base* 16.))
      (format t "~&~x:" sa)
      (do ((addr sa (1+ addr))
           (n (nc::ncompiled-function-length fcn) (1- n)))
          ((zerop n))
        (format t "~&  ~a" (nc::dis (cold-code-read addr))))))
  fcn)

(defvar *cold-fasl-op-handler-table* (make-array nc::n-fasl-ops))

(defmacro define-cold-fasl-op-handler (name lambda-list &body body)
  (let ((fname (gensym name)))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (setf (aref *cold-fasl-op-handler-table* ,name) #',fname))))


(defun cold-fasload (filename &key (package *package*))
  (let ((nc::*fasl-op-handler-table* *cold-fasl-op-handler-table*))
    (nc::nfasload filename :package package)))

(define-cold-fasl-op-handler NC::FASL-OP/DEFUN (stream)
  (let ((name        (cold-read stream))
        (lambda-list (cold-read stream))
        (local-refs  (cold-read stream))
        (refs        (cold-read stream))
        (immediates  (cold-read stream))
        (code        (cold-load-code stream)))
    lambda-list
    (enter-compiled-function
      (create-cold-compiled-function name local-refs refs immediates (length code))
      (zl:send stream :pathname)
      code)))

;;; This doesn't install the macro
(define-cold-fasl-op-handler NC::FASL-OP/MACRO (stream)
  (let ((name        (cold-read stream))
        (lambda-list (cold-read stream))
        (local-refs  (cold-read stream))
        (refs        (cold-read stream))
        (immediates  (cold-read stream))
        (code        (cold-load-code stream)))
    name
    lambda-list
    local-refs
    refs
    immediates
    code
    nil))

;    (create-cold-compiled-function name code local-refs refs immediates)))

;;; More here too
(define-cold-fasl-op-handler NC::FASL-OP/SUBST (stream)
  (let ((name        (cold-read stream))
        (lambda-list (cold-read stream))
        (body        (cold-read stream))
        (local-refs  (cold-read stream))
        (refs        (cold-read stream))
        (immediates  (cold-read stream))
        (code        (cold-load-code stream)))
    lambda-list
    body
    (enter-compiled-function
      (create-cold-compiled-function name local-refs refs immediates (length code))
      (zl:send stream :pathname)
      code)))

(define-cold-fasl-op-handler NC::FASL-OP/EVAL (stream)
  (let ((form (cold-read stream)))
    (case (car form)
      ;((in-package export) (eval form))
      ((defconstant))
      (t (format t "~&~s wants to be evaluated" form)))))


(defun cold-link (cfun)
  "Relocate and resolve references of FUNCTION"
  (let ((starting-address (nc::ncompiled-function-starting-address cfun)))
    ;; Link local ref addresses
    (dolist (local-ref (nc::ncompiled-function-local-refs cfun))
      (let ((iaddr (+ starting-address (car local-ref)))
            (toffset (cdr local-ref)))
            (cold-code-write iaddr
                             (if (not (minusp toffset))
                                 (dpb (ldb (byte (byte-size hw:%%i-branch-address)
                                                 0)
                                           (+ starting-address toffset))
                                      hw:%%i-branch-address
                                      (cold-code-read iaddr))
                               ;; negative offset means pc ref (imm32)
                                (dpb vinc:$$dtp-code vinc:%%data-type
                                     (logior (- starting-address toffset)
                                             (logand #xFFFFFFFFFF000000
                                                     (cold-code-read iaddr))))))))
    ;; Link Refs
    (dolist (ref (nc::ncompiled-function-refs cfun))
      (let ((reffun (find-named-function (if (symbolp (cdr ref))
                                                (cdr ref)
                                              (second ref))))
            ;(nargs (third ref))
            )
        (if reffun
            (let ((ref-addr (+ starting-address (car ref))))
              (cold-code-write
                ref-addr
                (logior (logand #xFFFFFFFFFF000000
                                (cold-code-read ref-addr))
                        (nc::ncompiled-function-starting-address reffun)))
              (pushnew (cons cfun (car ref)) (nc::ncompiled-function-callees reffun) :test #'equal))
          (format t "~&~S is undefined in ~s" (cdr ref) (nc::ncompiled-function-name cfun)))))
    ;; Do immediates
    (if (nc::ncompiled-function-immediates cfun)
        (format t "~&Immediates used in ~s: ~s"
                (nc::ncompiled-function-name cfun)
                (nc::ncompiled-function-immediates cfun)))
    ))


(defun link-cold-loaded-functions ()
  (dolist (f *cold-loaded-functions*)
    (cold-link f)))


(defun cold-load-files (files)
  (dolist (file files)
    (let ((real-file (zl:probef file)))
      (when (cold-file-needs-loading? real-file)
        (forget-functions-in-file real-file)
        (cold-fasload real-file)
        (record-cold-file-loaded real-file))))
  (cold-load-functions)
  (link-cold-loaded-functions))

(defun cold-load-functions ()
  (setq *cold-code-pointer* cold-code-start)
  (setq *cold-loaded-functions* '())
  (dolist (function *cold-functions-loaded*)
    (let* ((descriptor (first function))
           (code       (third function))
           (next-block (dpb 0 (byte 12. 0.) (+ *cold-code-pointer* (dpb 1. (byte 1. 12.) 0.))))
           (size-in-instructions (nc::ncompiled-function-length descriptor)))
      (when (> (+ size-in-instructions *cold-code-pointer*) next-block)
        (setq *cold-code-pointer* next-block))
      (setf (nc::ncompiled-function-starting-address descriptor) *cold-code-pointer*)
      (dolist (instruction code)
        (cold-code-write *cold-code-pointer* instruction)
        (incf *cold-code-pointer*))
      (push descriptor *cold-loaded-functions*))))

(defun forget-functions-in-file (file)
  (setq *cold-functions-loaded*
        (remove-if #'(lambda (source-file)
                       (same-file-except-for-version source-file file))
                   *cold-functions-loaded*
                   :key #'cold-function-entry-source-file)))



;;;; Cold Load Initial Data

(defmacro dpb-multiple (&rest fields)
  (labels ((expander (fields)
             (cond ((null fields) (error "Even number of arguments to ~s" 'dpb-multiple))
                   ((null (rest fields)) (first fields))
                   (t `(DPB
                         ,(first fields) ,(second fields)
                         ,(expander (rest (rest fields))))))))
    (expander fields)))

(defun cluster->address (cluster)
  (dpb cluster vinc::%%cluster-number 0))


;;; Physical locations of the initial data.

;;; NIL is at virtual address 0

;;; The Boot vector is an art-32b whose data begins at *boot-vector-origin*
;;; in the middle of cluster 0.   The cold load builder
;;; will place a header of the appropriate type before it.
(defparameter *boot-vector-origin*                             42.)     ;absolute.

;;; The physical cluster table will go in quantum 1.
;;; It takes up half of a quantum.
;;; In the second half of the quantum, we put
;;; the quantum map and the region bits.  Each of
;;; these takes up a quater of the quantum.

;;; Cluster addresses
(defparameter *initial-map-data-physical-location*              (cluster->address 2.))  ;64 clusters

(defparameter *quantum-map-physical-location*                   66.)
(defparameter *quantum-map-clusters*                             4.)

(defparameter *region-bits-physical-location*                   70.)
(defparameter *region-bits-clusters*                             4.)

(defparameter *initial-physical-cluster-data-physical-location* 74.)    ;1 cluster

(defparameter *initial-gc-ram-data-physical-location*           (cluster->address 75.)) ;1 cluster

(defparameter *initial-transporter-ram-data-physical-location*   76.)

;;; Virtual addresses of nifty things.

;;; Quantum 0.
;;; Cluster 1.
(defparameter *temporary-map-entry-location*            (ash 1. (byte-position vinc::%%cluster-number)))


;;; Quantum 1.
;;; Clusters 0. 7.
(defparameter *physical-cluster-table-location*        (* 1 (ash 1 (byte-position vinc::%%quantum-number))))

;;; Clusters 8. 11.
(defparameter *quantum-map-virtual-location*            (+ *physical-cluster-table-location*
                                                          (ash 1. (1- (byte-position vinc::%%quantum-number)))))
;;; Clusters 12. 15.
(defparameter *region-bits-virtual-location*            (+ *quantum-map-virtual-location*
                                                          (ash 1 (- (byte-position vinc::%%quantum-number) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Initial map cluster data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-initial-map-data ()

  ;; Make everything volatility 0.
  (dotimes (i map::*number-of-map-entries*)
    (cold-data-write
      (+ i *initial-map-data-physical-location*)
      (dpb-multiple
        0.                     hw:%%map-volatility
        map::$$cluster-fresh map::%%map-fresh-cluster
        0.)))

  ;; Cluster 0 is volatility 1.
  (cold-data-write
     *initial-map-data-physical-location*
    (dpb-multiple
      1.                         hw:%%map-volatility
      map::$$cluster-not-fresh map::%%map-fresh-cluster
      0.)))

;;; We will move this comment down later.
;(defparameter *boot-quanta*
;            (list
;              (list 0. *null-paging-device* 0)
;              (list 1. *null-paging-device* 1)))

;(defun load-boot-quantum-map ()
;  (do ((devices (reverse *quantum-devices*) (if devices (rest devices) nil))
;       (devnum  0                 (1+ devnum)))
;      ((or (null devices) (= devnum 16.))
;       (when devices
;        (error "Too many quantum devices")))
;    (setf (aref *quantum-device-vector* devnum)
;         (if devices
;             (first devices)
;             *null-paging-device*
;             )))

;  ;; Zero all quanta.
;  (dotimes (i *number-of-quanta*)
;    (cold-data-write
;      (+ i (cluster->address *quantum-map-physical-location*))
;      0.))

;  ;; Write initial quanta.
;  (dolist (q *boot-quanta*)
;    (cold-data-write
;      (+ (first q) (cluster->address *quantum-map-physical-location*))
;      (dpb-multiple-unboxed
;       (find-position-in-list (second q) *quantum-devices*)
;                                      quantum-map::%%quantum-map-device
;       (third  q)                     quantum-map::%%quantum-map-region-origin
;       quantum-map::$$quantum-mapped  quantum-map::%%Quantum-map-status
;       0.))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Initial physical cluster data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-initial-physical-cluster-data ()
        (list

          ;; Map in the wired instructions.
          (list (vinc:cluster-number *cold-data-size*)
                (vinc:cluster->address hw:*first-instruction-cluster*)
                pcd::$$init-map-wired-read-only
                (1+ (vinc:cluster-number (ash *cold-code-pointer* 2))))

          (list *quantum-map-physical-location*
                *quantum-map-virtual-location*
                pcd::$$init-map-wired
                *quantum-map-clusters*)

          (list *region-bits-physical-location*
                *region-bits-virtual-location*
                pcd::$$init-map-wired
                *region-bits-clusters*)

          ;; Shared memory cluster
          (list 2. (cluster->address 2.) pcd::$$init-map-wired 1.)

          ;; This must be last.  It maps NIL.
          (list 0.
                0.
                pcd::$$init-map-wired
                1.)))


;;;; This function fakes up the initial physical cluster data.
(defun load-initial-physical-cluster-data ()
  (let ((pointer (1- *initial-physical-cluster-data-physical-location*)))
  (dolist (record (get-initial-physical-cluster-data))
    (cold-data-write (incf pointer) (first  record))
    (cold-data-write (incf pointer) (vinc:cluster-number (second record)))
    (cold-data-write (incf pointer) (third  record))
    (cold-data-write (incf pointer) (fourth record)))))

(defun get-boot-gc-ram-data ()
  (list
    ;; quantum, volatility, oldspace
    ;; Resident symbols
    (list 0. 0. hw:$$not-oldspace)
    ;; Paging tables
    (list 1. 0. hw:$$not-oldspace)))

(defun load-boot-gc-ram-data ()
  (dotimes (i gc-ram::*number-of-gc-ram-entries*)
    (cold-data-write
      (+ i *initial-gc-ram-data-physical-location*)
      (dpb-multiple
        0.                hw:%%gc-ram-quantum-volatility
        hw:$$not-oldspace hw:%%gc-ram-quantum-oldspace
        0.)))
  (dolist (record (get-boot-gc-ram-data))
    (cold-data-write
      (+ (first record) *initial-gc-ram-data-physical-location*)
      (dpb-multiple
        (second record) hw:%%gc-ram-quantum-volatility
        (third  record) hw:%%gc-ram-quantum-oldspace
        0.))))

;;; This function has a corresponding reader in "TRANSPORTER-RAM"
(defun modify-boot-transporter-ram-data (mode type datatype vma-boxed md-boxed modifier)
  (let ((address (+ (cluster->address *initial-transporter-ram-data-physical-location*)
                    (dpb-multiple
                      datatype   (byte (byte-size hw:%%transporter-ram-md-byte) 0.)
                      type       (byte (byte-size hw:%%memory-status-transport-type)
                                       (byte-size hw:%%transporter-ram-md-byte))
                      mode       (byte (byte-size hw:%%memory-control-transporter-mode)
                                       (+ (byte-size hw:%%memory-status-transport-type)
                                          (byte-size hw:%%transporter-ram-md-byte)))
                      0.)))
        (byte-offset (byte 4. (dpb-multiple
                                vma-boxed (byte 1. 2.)
                                md-boxed  (byte 1. 3.)
                                0))))
    (let ((data-there (cold-data-read address)))
      (let ((new-data (funcall modifier (ldb byte-offset data-there))))

      (cold-data-write address
                       (dpb new-data byte-offset data-there))))))

(defun expand-data-modifier (bit-list)
  (labels ((convert-to-ones (bit)
             (if (null bit) 1 bit))
           (convert-to-zeros (bit)
             (if (null bit) 0 bit))
           (compact-bits (bit-list)
             (compact-iter bit-list 0))
           (compact-iter (bit-list answer)
             (if (null bit-list)
                 answer
                 (compact-iter (rest bit-list)
                               (+ (ash answer 1.) (first bit-list)))))
           )
    (let ((and-pattern (compact-bits (mapcar #'convert-to-ones bit-list)))
          (ior-pattern (compact-bits (mapcar #'convert-to-zeros bit-list))))
      #'(lambda (data-there)
          (logior ior-pattern
                  (logand and-pattern
                          data-there))))))

(defconstant no-trans vinc::$$transport-type-no-transport)
(defconstant trans    vinc::$$transport-type-transport)
(defconstant vis-evcp vinc::$$transport-type-visible-evcp)
(defconstant write    vinc::$$transport-type-write)

(defconstant normal   vinc::$$transporter-mode-normal)

(defun load-boot-transporter-ram-data (data-list)
  (flet ((val (x) (if (eq x '*) '* (eval x))))
    (dolist (data-element data-list)
      (let ((vma-boxed (val (first  data-element)))
            (md-boxed  (val (second data-element)))
            (datatype  (val (third  data-element)))
            (mstat     (val (fourth data-element)))
            (mctl      (val (fifth  data-element)))
            (modifier  (expand-data-modifier (nthcdr 5 data-element))))
        (dotimes (vmab 2.)
          (when (lisp::or (eq '* vma-boxed) (= vma-boxed vmab))
            (dotimes (mdb 2.)
              (when (lisp::or (eq '* md-boxed) (= mdb md-boxed))
                (dotimes (dtp 64.)
                  (when (lisp::or (eq '* datatype) (= dtp datatype))
                    (dotimes (type 4.)
                      (when (lisp::or (eq '* mstat) (= type mstat))
                        (dotimes (mode 4.)
                          (when (lisp::or (eq '* mctl) (= mctl mode))
                            (modify-boot-transporter-ram-data
                              mode type dtp vmab mdb modifier)))))))))))))))

;;; Format of transporter ram data:
;;; (vma-boxed md-boxed datatype mstat mctl box-error trap-if-not-old trap-if-old trappable-pointer)

(defparameter *transporter-ram-initial-data*
  '(;; Anything weird we trap on.
    (* *   *                 *        *      1 1 1 1)
    ;; Don't trap on unboxed-write if no gc-write-test, no trap on unboxed read if "no-transport"
    (0 0   *                 no-trans normal 0 0 0 0)
    ;; Don't trap on unboxed-write if no gc-write-test, trap on unboxed read if not "no-transport"
    (0 0   *                 vis-evcp normal 0 1 1 0)
    ;; NIL is not treated like a pointer in the transporter ram.
    (* 1   vinc:$$dtp-nil    no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-nil    trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-nil    vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-nil    write    normal 0 0 0 0)

    ;; FIXNUMS
    (* 1   vinc:$$dtp-fixnum no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-fixnum trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-fixnum vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-fixnum write    normal 0 0 0 0)

    ;; CHARACTERS same as fixnums
    (* 1   vinc:$$dtp-character no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-character trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-character vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-character write    normal 0 0 0 0)

    ;; ARRAY HEADER SINGLE not a pointer, but don't bash
    (* 1   vinc:$$dtp-array-header-single no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-array-header-single trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-array-header-single vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-array-header-single write    normal 1 1 1 1)

    ;; ARRAY
    (* 1   vinc:$$dtp-array  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-array  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-array  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-array  write    normal 0 0 1 1)

    ;; CONS
    (* 1   vinc:$$dtp-cons   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-cons   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-cons   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-cons   write    normal 0 0 1 1)

    ;; SYMBOL HEADER pointer, but don't bash
    (* 1   vinc:$$dtp-symbol-header no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-symbol-header trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-symbol-header vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-symbol-header write    normal 1 1 1 1)

    ;; SYMBOL
    (* 1   vinc:$$dtp-symbol   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-symbol   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-symbol   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-symbol   write    normal 0 0 1 1)

    ;; UNBOUND
    (* 1   vinc:$$dtp-unbound  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-unbound  trans    normal 0 1 1 1)
    (* 1   vinc:$$dtp-unbound  vis-evcp normal 1 1 1 1)
    (* 1   vinc:$$dtp-unbound  write    normal 0 0 1 1)
    ))


(defun load-transporter-ram-data ()
  (load-boot-transporter-ram-data *transporter-ram-initial-data*))

;;(defun load-boot-transporter-ram-data ()
;;  ;; All unused entries trap.
;;  (dotimes (mode transporter-ram::*number-of-transporter-modes*)
;;    (dotimes (type transporter-ram::*number-of-transport-types*)
;;      (dotimes (md-byte transporter-ram::*number-of-transporter-md-byte-values*)
;;      (let ((transporter-data 0.))
;;        (dotimes (vma-boxed 2.)
;;          (dotimes (md-boxed 2.)
;;            (setq transporter-data
;;                  (sim:k-dpb
;;                    (sim:k-ldb
;;                      (sim:k-dpb
;;                        hw::$$trappable-pointer    sim::%%k-transporter-ram-trappable-pointer
;;                        (sim:k-dpb
;;                          hw::$$trap-if-oldspace     sim::%%k-transporter-ram-trap-if-oldspace
;;                          (sim:k-dpb
;;                            hw::$$trap-if-not-oldspace sim::%%k-transporter-ram-trap-if-not-oldspace
;;                            (sim:k-dpb
;;                              hw::$$box-error            sim::%%k-transporter-ram-box-error
;;                              0))))
;;                      (byte 4. 4.)
;;                      0.)
;;                    (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;;                    transporter-data))))
;;        (cold-data-write
;;          (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;;             (sim:k-dpb
;;               md-byte (byte (byte-size sim::%%k-transporter-md-byte) 0.)
;;               (sim:k-dpb
;;                 type    (byte (byte-size sim::%%k-memory-status-transport-ram-bits)
;;                               (byte-size sim::%%k-transporter-md-byte))
;;                 (sim:k-dpb
;;                   mode    (byte (byte-size sim::%%k-memory-control-transporter-mode)
;;                                 (+ (byte-size sim::%%k-memory-status-transport-ram-bits)
;;                                    (byte-size sim::%%k-transporter-md-byte)))
;;                   0.))
;;               transporter-data)))))

;;  ;; Normal mode, all unboxed never traps.
;;  (let ((mode      vinc::$$transport-mode-normal)
;;      (vma-boxed $$unboxed)
;;      (md-boxed  $$unboxed))
;;    (dotimes (type *number-of-transport-types*)
;;      (dotimes (md-byte *number-of-transporter-md-byte-values*)
;;      (let* ((location
;;               (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;;                  (dpb-multiple-unboxed
;;                    md-byte (byte (byte-size %%k-transporter-md-byte) 0.)
;;                    type    (byte (byte-size %%k-memory-status-transport-type)
;;                                  (byte-size %%k-transporter-md-byte))
;;                    mode    (byte (byte-size %%k-memory-control-transporter-mode)
;;                                  (+ (byte-size %%k-memory-status-transport-type)
;;                                     (byte-size %%k-transporter-md-byte)))
;;                    0.)))
;;             (transporter-data (cold-data-write location)))
;;        (setq transporter-data
;;              (%dpb
;;                (%ldb
;;                  (dpb-multiple-unboxed
;;                    $$non-trappable-pointer     %%k-transporter-ram-trappable-pointer
;;                    $$dont-trap-if-oldspace     %%k-transporter-ram-trap-if-oldspace
;;                    $$dont-trap-if-not-oldspace %%k-transporter-ram-trap-if-not-oldspace
;;                    $$no-box-error              %%k-transporter-ram-box-error
;;                    0.)
;;                  (byte 4. 4.)
;;                  0.)
;;                (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;;                transporter-data))
;;        (cold-data-write location transporter-data)))))

;;  ;; Normal mode, type no-transport, never trans trap.
;;  (let ((mode vinc::$$transport-mode-normal)
;;      (type vinc::$$transport-type-no-transport))
;;    (dotimes (md-byte *number-of-transporter-md-byte-values*)
;;      (let* ((location
;;             (+ (cluster->address *initial-transporter-ram-data-physical-location*)
;;                (dpb-multiple-unboxed
;;                  md-byte (byte (byte-size %%k-transporter-md-byte) 0.)
;;                  type    (byte (byte-size %%k-memory-status-transport-type)
;;                                (byte-size %%k-transporter-md-byte))
;;                  mode    (byte (byte-size %%k-memory-control-transporter-mode)
;;                                (+ (byte-size %%k-memory-status-transport-type)
;;                                   (byte-size %%k-transporter-md-byte)))
;;                  0.)))
;;           (transporter-data (cold-data-read location)))
;;      (dotimes (vma-boxed 2.)
;;        (dotimes (md-boxed 2.)
;;          (setq transporter-data
;;                (%dpb
;;                  (%ldb
;;                    (dpb-multiple-unboxed
;;                      $$dont-trap-if-oldspace     %%k-transporter-ram-trap-if-oldspace
;;                      $$dont-trap-if-not-oldspace %%k-transporter-ram-trap-if-not-oldspace
;;                      $$no-box-error              %%k-transporter-ram-box-error
;;                      (%ldb transporter-data (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed))) 0.))
;;                    (byte 4. 4.)
;;                    0.)
;;                  (byte 4. (* 4. (+ (* vma-boxed 2) md-boxed)))
;;                  transporter-data))))
;;      (cold-data-write location transporter-data)))))



;(defun init-boot-vector-macro ()
;  (do ((tail (get-boot-vector-entries) (rest tail))
;       (count 0 (1+ count))
;       (code '() (cons `(SETQ ,(first (first tail)) (BOOT::READ-BOOT-VECTOR ,count)) code)))
;      ((null tail) `(PROGN ,@(reverse code)))))

;(defun initialize-from-boot-vector ()
;  (eval (init-boot-vector-macro)))             ;GAK! CHOKE! ARGH!


(defparameter *quantum-devices* '(*null-paging-device*))

(defparameter *boot-quanta*
             (list
               ;; nil, t, and boot vector
               (list 0.    '*null-paging-device* 1)
               ;; quantum map, region bits, and pcd
               (list 1.    '*null-paging-device* 1)
               ;; code
               (list 2048. '*null-paging-device* 4.)))

(defun build-cold-quantum-map ()
;;; Don't know where the device vector will be yet.
;  (do ((devices (reverse *quantum-devices*) (if devices (rest devices) nil))
;       (devnum  0                 (1+ devnum)))
;      ((or (null devices) (= devnum 16.))
;       (when devices
;        (ferror nil "Too many quantum devices")))
;    (setf (aref *quantum-device-vector* devnum)
;         (if devices
;             (first devices)
;             *null-paging-device*
;             )))

  ;; Zero all quanta.
  (dotimes (i vinc:*number-of-quanta*)
    (cold-data-write
      (+ i (cluster->address *quantum-map-physical-location*))
      0.))

  ;; Write initial quanta.
  (dolist (q *boot-quanta*)
    (dotimes (number (third q))
      (cold-data-write
        (+ (first q) (cluster->address *quantum-map-physical-location*) number)
        (dpb-multiple
          (zl::find-position-in-list (second q) *quantum-devices*)
                                         quantum-map::%%quantum-map-device
          (first  q)                     quantum-map::%%quantum-map-region-origin
          quantum-map::$$quantum-mapped  quantum-map::%%quantum-map-status
          0.)))))

;;;;;;;;;;;;;;;;;
;;;; Region bits
;;;;;;;;;;;;;;;;;

(defun get-boot-regions ()
        (list

          ;; Resident symbols
          (list 0.                              ;quantum 0
                1.                              ;one quantum long
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-copy-space
                  region-bits:$$region-space-structure
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-enabled
                  0.))                          ;swapin quantum 0

          ;; Paging tables
          (list 1.                              ;quantum 1
                1.                              ;one quantum long
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-copy-space
                  region-bits:$$region-space-unboxed
                  region-bits:$$region-read-write
                  region-bits:$$scavenge-disabled
                  0.))                          ;swapin quantum 0
          (list 2048.
                4.
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-copy-space
                  region-bits:$$region-space-code
                  region-bits:$$region-read-only
                  region-bits:$$scavenge-enabled
                  16.))
          ))

(defun load-boot-region-bits ()
  ;; Zero all the region bits.
  (let ((zapped
          (region-bits:encode-region-bits
            region-bits:$$region-fixed
            region-bits:$$region-copy-space
            region-bits:$$region-space-free
            region-bits:$$region-read-only
            region-bits:$$scavenge-disabled
            0.)))
  (dotimes (i region-bits:*number-of-regions*)
    (cold-data-write (+ i (cluster->address *region-bits-physical-location*))
                     zapped
                     ))

  (dolist (record (get-boot-regions))
    (let ((quantum-start      (first  record))
          (quantum-count      (second record)))
      (do ((quantum     quantum-start (1+ quantum))
           (count       quantum-count (1- count))
           (region-bits (third  record) (dpb region-bits:$$region-space-invalid
                                             region-bits::%%region-bits-space-type
                                             region-bits)))

          ((zerop count))
        (cold-data-write (+ quantum (cluster->address *region-bits-physical-location*))
          region-bits))))))


;;;;;;;;;;;;;;;;;
;;;; Region data
;;;;;;;;;;;;;;;;;



;(defun create-region-data-for-cold-load ()
;  (let ((region-data (region-bits:make-region
;                      (ceiling (* region-bits:*number-of-regions* 4.) *qs-in-quantum*)
;                      (region-bits:parameters->region-bits
;                        region-bits:$$region-space-fixed
;                        region-bits:$$scavenge-disabled
;                        region-bits:$$region-read-write
;                        15.)                   ;Swapin entire thing.
;                      0.)))                    ;volatility 0.

;    (setq region-data::*region-free-pointer*      (quantum->address region-data))
;    (setq region-data::*region-allocation-status* (+ region-data::*region-free-pointer*
;                                                    region-bits:*number-of-regions*))
;    (setq region-data::*region-end*               (+ region-data::*region-allocation-status*
;                                                    region-bits:*number-of-regions*))
;    (setq region-data::*region-gc-pointer*        (+ region-data::*region-end*
;                                                    region-bits:*number-of-regions*))

;    ;; Bash data for initial regions.  Do this right someday.
;    (setf (region-data:region-free-pointer      0) (hw:dpb 1. vinc::%%quantum-number  0.))
;    (setf (region-data:region-allocation-status 0) 0)
;    (setf (region-data:region-end               0) (hw:dpb 1. vinc::%%quantum-number  0.))
;    (setf (region-data:region-gc-pointer        0) (hw:dpb 1. vinc::%%quantum-number  0.))

;    (setf (region-data:region-free-pointer      1) (hw:dpb 2. vinc::%%quantum-number  0.))
;    (setf (region-data:region-allocation-status 1) 0)
;    (setf (region-data:region-end               1) (hw:dpb 2. vinc::%%quantum-number  0.))
;    (setf (region-data:region-gc-pointer        1) (hw:dpb 2. vinc::%%quantum-number  0.))

;    ;; We ourselves is full.
;    (setf (region-data:region-free-pointer region-data)
;         (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))
;    (setf (region-data:region-allocation-status region-data) 0)
;    (setf (region-data:region-end region-data)
;         (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))
;    (setf (region-data:region-gc-pointer region-data)
;         (+ region-data::*region-gc-pointer* region-bits:*number-of-regions*))

;    region-data
;    ))

;(defun create-area-data-for-cold-load (region-data-region)
;  (let* ((qs-needed (+ mem:*number-of-regions* ;Region list thread
;                      mem:*number-of-areas*    ;Area region data
;                      mem:*number-of-areas*    ;Area region bits
;                      mem:*number-of-areas*    ;Area region size
;                      ))
;        (quanta-needed (ceiling qs-needed *qs-in-quantum*))
;        (region-bits (region-bits:parameters->region-bits
;                       region-bits:$$region-space-fixed
;                       region-bits:$$scavenge-disabled
;                       region-bits:$$region-read-write
;                       (max 15. (1- (* quanta-needed *clusters-in-quantum*)))))
;        (region (region-data:make-region quanta-needed region-bits 1)))

;    (setq memlow:*region-list-thread* (region-data::region-free-pointer region))
;    (region-data::advance-free-pointer region mem:*number-of-regions*)

;    (setq memlow:*area-region-data* (region-data::region-free-pointer region))
;    (region-data::advance-free-pointer region mem:*number-of-areas*)

;    (setq memlow::*area-region-bits* (region-data::region-free-pointer region))
;    (region-data::advance-free-pointer region mem:*number-of-areas*)

;    (setq memlow::*area-region-size* (region-data::region-free-pointer region))
;    (region-data::advance-free-pointer region mem:*number-of-areas*)

;    (setf (region-data::region-gc-pointer region) (region-data::region-free-pointer region))

;    ;;; Zero out the area tables
;    (dotimes (area mem:*number-of-areas*)
;      (setf (area-data::area-region-data area)
;           (hw:dpb area-data::$$area-free area-data::%%area-data-area-status 0.)))

;    ;;; Setup RESIDENT SYMBOL AREA
;    (setf (area-data::area-region-data   0.)
;         (dpb-multiple-boxed
;           area-data::$$area-fixed area-data::%%area-data-area-status
;           0.                      area-data::%%area-data-region-thread
;           0.))
;    (setf (area-data::region-list-thread 0.)
;         (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 0.))
;    (setf (area-data::area-region-size   0.) 0.)
;    (setf (area-data::area-region-bits   0.)
;         (dpb-multiple-boxed
;           (region-bits::parameters->region-bits
;             region-bits::$$region-space-fixed
;             region-bits::$$scavenge-enabled
;             region-bits::$$region-read-only
;             0.)))

;    ;;; Setup PAGING TABLE AREA
;    (setf (area-data::area-region-data   1.)
;         (dpb-multiple-boxed
;           area-data::$$area-fixed area-data::%%area-data-area-status
;           1.                      area-data::%%area-data-region-thread
;           0.))
;    (setf (area-data::region-list-thread 1.)
;         (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 1.))
;    (setf (area-data::area-region-size   1.) 0.)
;    (setf (area-data::area-region-bits   1.)
;         (dpb-multiple-boxed
;           (region-bits::parameters->region-bits
;             region-bits::$$region-space-fixed
;             region-bits::$$scavenge-enabled
;             region-bits::$$region-read-only
;             0.)))

;    ;;; Setup MEMORY MANAGEMENT AREA
;    (setf (area-data::area-region-data   2.)
;         (dpb-multiple-boxed
;           area-data::$$area-fixed area-data::%%area-data-area-status
;           region-data-region      area-data::%%area-data-region-thread
;           0.))
;    (setf (area-data::region-list-thread region-data-region)
;         (hw:dpb-boxed area-data::$$thread-continues area-data::%%region-list-thread-end-flag region))
;    (setf (area-data::region-list-thread region)
;         (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag region-data-region))
;    (setf (area-data::area-region-size 2.) 0.)
;    (setf (area-data::area-region-bits 2.) region-bits)))


(defun cold-load-data ()
  (load-initial-map-data)
  (load-boot-gc-ram-data)
  (load-transporter-ram-data)
  (load-initial-physical-cluster-data)
  (build-cold-quantum-map)
  (load-boot-region-bits)
; ...
  )

;(defun make-cold-load-but-dont-load-the-files (initial-function)
;  (link-cold-loaded-functions)
;  (cold-load-data)
;  (cold-data-write (+ boot::**boot-vector-origin** boot::**initial-code-physical-location-bv-offset**)
;                  *cold-data-size*)
;  (cold-data-write (+ boot::**boot-vector-origin** boot::**initial-code-size-in-clusters-bv-offset**)
;                  (1+ (ldb vinc::%%cluster-number (ash *cold-code-pointer* 2))))
;  (cold-data-write (+ boot::**boot-vector-origin** boot::**initial-code-entry-point-bv-offset**)
;                  (nc::ncompiled-function-starting-address (find-named-function initial-function)))
;  (cold-data-write (+ boot::**boot-vector-origin** boot::*initial-gc-ram-data-physical-location*)
;                  *initial-gc-ram-data-physical-location*)
;  (cold-data-write (+ boot::**boot-vector-origin** boot::*initial-transporter-ram-data-physical-location*)
;                  (cluster->address *initial-transporter-ram-data-physical-location*))
;  (user::dump-illop-codes))


(defun cold-write-boot-vector (offset data)
  (cold-data-write (+ boot::**boot-vector-origin** offset) data))

(defun make-cold-load (files initial-function)
  (cold-load-files files)
  (cold-load-data)
  (cold-write-boot-vector boot::**initial-code-physical-location-bv-offset**
                          *cold-data-size*)
  (cold-write-boot-vector boot::**initial-code-size-in-clusters-bv-offset**
                          (1+ (ldb vinc::%%cluster-number (ash *cold-code-pointer* 2))))
  (cold-write-boot-vector  boot::**initial-code-entry-point-bv-offset**
                           (nc::ncompiled-function-starting-address (find-named-function initial-function)))
  (cold-write-boot-vector  boot::*initial-gc-ram-data-physical-location*
                           *initial-gc-ram-data-physical-location*)
  (cold-write-boot-vector boot::*initial-transporter-ram-data-physical-location*
                          (cluster->address *initial-transporter-ram-data-physical-location*))
  (cold-write-boot-vector boot::*cold-load-flag* 1)
  )
