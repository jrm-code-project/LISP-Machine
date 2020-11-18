;;; -*- Mode:LISP; Package:K-COLD; Compile-In-Roots:(K-GLOBAL); Readtable:CL; Base:10 -*-

;;;; Cold Loader

;;; Builds a Cold Load


(defparameter *cold-files*
              '("jb:k;trap"
                "jb:k;trap-handlers"
                "jb:k;nuclear-control"
                "jb:k;gc-ram"
                "jb:k;datatype-ram"
                "jb:k;memory-map"
                "jb:k;vmem"
                "jb:k;transporter-ram"
                "jb:k;timers"
                "jb:k;pcd-table"
                "jb:k;quantum-map"
                "jb:k;memory-management"
                "jb:k;region-bits"
                "jb:k;map-fault"
                "jb:k;region-data"
                "jb:k;gc-fault"
                "jb:k;area-data"
                "jb:k;nubus-interrupts"
                "jb:k;memory-interface"
                "jb:k;type-predicates"
                "jb:k;cons"
                "jb:k.array;array"
                "jb:k;symbols"
                "jb:k;structure"
                "jb:kbug2;common-definitions"
                "jb:kbug2;streams"
                "jb:kbug2;k2"
                "jb:youcef.k;warm-loader"
                "jb:k;lisp-internals"
                "jb:k;error"
                "jb:k;dt-ovf-trap"
                "jb:k;boot"
                "jb:k;control-pdl"
;               "love:naha;test-control-pdl"
                ))

(defconstant *cold-data-size* (ash 15. 10.) ;15 clusters
  "Size of cold load data space in words")

(defconstant *cold-data* (make-array (ash *cold-data-size* 2)
                                     :element-type '(unsigned-byte 16.)))

(defconstant *cold-code-size* (ash 32. 10.)  ;32 clusters
  "Size of cold load code space in words")

(defconstant *cold-code* (make-array (ash *cold-code-size* 2)
                                     :element-type '(unsigned-byte 16.)))

(defconstant cold-code-start 0)

(defvar *cold-code-pointer* cold-code-start)
(defvar *cold-loaded-functions* '())
(defvar *cold-eval-list* '())

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

(defvar *file-being-cold-loaded* nil "Pathname of the file being cold loaded.")
(defvar *cold-files-loaded* '())
(defvar *cold-functions-loaded* '())

(defun cold-loader-forget-everything ()
  (setq *cold-code-pointer* cold-code-start)
  (setq *cold-loaded-functions* nil)
  (setq *cold-eval-list* nil)
  (setq *cold-functions-loaded* nil)
  (setq *cold-files-loaded* nil)
  (dotimes (c (si:array-length *cold-data*))
    (setf (aref *cold-data* c) 0))
  (dotimes (c (si:array-length *cold-code*))
    (setf (aref *cold-code* c) 0)))


(defun same-file-except-for-version (file1 file2)
  (eq (zl:send file1 :generic-pathname)
      (zl:send file2 :generic-pathname)))

(defun cold-file-needs-loading? (file)
  (let ((entry (assoc file *cold-files-loaded*)))
    (or (null entry)
        (< (cdr entry)
           (file-write-date file)))))

(defun cold-file-needs-compiling? (file kfasl-real-pathname)
  (or (null kfasl-real-pathname)
      (< (file-write-date kfasl-real-pathname)
         (file-write-date file))))

(defun record-cold-file-loaded (pathname)
  (setq *cold-files-loaded*
        (cons (cons pathname
                    (file-write-date pathname))
              (delete pathname *cold-files-loaded*
                      :key #'car
                      :test #'same-file-except-for-version))))



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

(zl:defsubst cold-read-byte (stream)
  (read-byte stream))

(zl:defsubst cold-peek-byte (stream)
  (or (zl:tyipeek nil stream nil)
      (zl::ferror nil "Unexpected EOF in ~s" stream)))

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


(defun create-cold-compiled-function (name local-refs refs entry-points immediates length code)
  (let ((fcn (nc::make-ncompiled-function
               :name name
               :starting-address nil
               :entry-points entry-points
               :local-refs local-refs
               :refs refs
               :immediates immediates
               :length length
               :code code)))
    (push fcn *cold-loaded-functions*)
    (if (symbolp name)
        (setf (nc::nsymbol-function name) fcn))
    fcn))

(defun cold-read-opcode (stream)
  (cold-read-byte stream))

(defun cold-peek-opcode (stream)
  (cold-peek-byte stream))

(defun cold-fasload (filename &key (package *package*))
  (let ((path (fs:merge-pathname-defaults filename nil "KFASL")))
    (let ((*file-being-cold-loaded* (zl:send path :generic-pathname)))
      (with-open-file (stream path)
        (do ()
            ((= (cold-peek-opcode stream)
                ;fasdump:$$fasl-op-end-of-file)
                k2:$$fasl-op-end-of-file)
             (cold-read-opcode stream)
             stream)
          (cold-read-object stream))))))


(defvar *cold-fasl-op-handler-table* (make-array 256.))

(defmacro define-cold-fasl-op-handler (name opcode lambda-list &body body)
  `(PROGN
     (DEFUN ,name ,lambda-list
       ,@body)
     (SETF (AREF *COLD-FASL-OP-HANDLER-TABLE* ,opcode) #',name)))


(defun cold-read-object (stream)
  (let* ((op (cold-read-opcode stream))
         (handler (aref *cold-fasl-op-handler-table* op)))
    (if handler
        (funcall handler stream)
      (error "Unknown fasl op: ~a" op))))

(define-cold-fasl-op-handler cold-read-fixnum
                             ;fasdump:$$fasl-op-fixnum     doesn't exist
                             k2:$$fasl-op-fixnum
                             (stream)
  (let ((low-bits    (cold-read-byte stream))
        (medium-bits (cold-read-byte stream))
        (high-bits   (cold-read-byte stream)))
    (let ((result
            (dpb high-bits
                 (byte 8. 16.)
                 (dpb medium-bits
                      (byte 8. 8.)
                      low-bits))))
      (if (ldb-test (byte 1. 23.) result)
          (- result (expt 2. 24.))
          result))))

(define-cold-fasl-op-handler cold-read-string
                             ;fasdump:$$fasl-op-string    doesn't exist
                             k2:$$fasl-op-string
                             (stream)
  (let ((length (cold-read-fixnum stream)))
    (let ((string (make-string length)))
      (dotimes (i length)
        (setf (aref string i) (cold-read-byte stream)))
      string)))

(define-cold-fasl-op-handler cold-read-symbol
                             ;fasdump:$$fasl-op-symbol
                             k2:$$fasl-op-symbol
                             (stream)
  (let ((pname (cold-read-object stream))
        (pack (cold-read-object stream)))       ;caution, the variable PACKAGE is forwarded to
    (intern pname (find-package pack *package*))))

(define-cold-fasl-op-handler cold-read-compiled-function
                             ;fasdump:$$fasl-op-compiled-function
                             k2:$$fasl-op-compiled-function
                             (stream)
  (let ((name         (cold-read-object stream))
        (local-refs   (cold-read-local-refs stream))
        (refs         (cold-read-refs stream))
        (entry-points (cold-read-entry-points stream))
        (code         (cold-load-code stream))
        (immediates   (cold-read-immediates stream)))
    (create-cold-compiled-function name local-refs refs entry-points immediates (length code) code)))

(define-cold-fasl-op-handler cold-read-list
                             ;fasdump:$$fasl-op-list
                             k2:$$fasl-op-list
                             (stream)
  (let ((length (cold-read-fixnum stream)))
    (let ((l '()))
      (let ((tail l))
        (dotimes (i length)
          (let ((cons (cons (cold-read-object stream)
                                    nil)))
            (if tail
              (rplacd tail cons)
              (setq l cons))
            (setq tail cons))))
      l)))

(define-cold-fasl-op-handler cold-read-cons
                             ;fasdump:$$fasl-op-cons
                             k2:$$fasl-op-cons
                             (stream)
  (let ((car (cold-read-object stream))
        (cdr (cold-read-object stream)))
    (cons car cdr)))

(define-cold-fasl-op-handler cold-read-nil
                             k2:$$fasl-op-nil
                             (stream)
  stream
  nil)

(define-cold-fasl-op-handler cold-read-eval
                             k2:$$fasl-op-eval
                             (stream)
  (let ((form (cold-read-object stream)))
    ;(format t "~%Cold eval ~s" form)
    (format t "Pushing ~S onto *cold-eval-list*" form)
    (push form *cold-eval-list*)))


(define-cold-fasl-op-handler cold-read-defafun
                             k2:$$fasl-op-defafun
                             (stream)
  (let ((name (cold-read-object stream))
        (function (cold-read-object stream)))
     name
    (enter-compiled-function
      function
      *file-being-cold-loaded*
      (nc::ncompiled-function-code function))))

(define-cold-fasl-op-handler cold-read-defmacro
                             k2:$$fasl-op-defmacro
                             (stream)
  (let ((name (cold-read-object stream))
        (function (cold-read-object stream)))
    name
    function
    (format t "~%Ignoring macro ~s" name)))

(define-cold-fasl-op-handler cold-read-defun
                             k2:$$fasl-op-defun
                             (stream)
  (let ((name (cold-read-object stream))
        (function (cold-read-object stream)))
    name
    (enter-compiled-function
      function
      *file-being-cold-loaded*
      (nc::ncompiled-function-code function))))

(define-cold-fasl-op-handler cold-read-defconstant
                             k2:$$fasl-op-defconstant
                             (stream)
  (let ((name (cold-read-object stream))
        (value (cold-read-object stream)))
    value
    (format t "~%Pushing DEFCONSTANT ~s onto *cold-eval-list*" name)
    (push `(SETQ ,name ,value) *cold-eval-list*)
    (push `(SYMBOL:%PUT ',name 'LI::CONSTANT T) *cold-eval-list*)
    (push `(SYMBOL:%PUT ',name 'LI::SPECIAL T)  *cold-eval-list*)))

(define-cold-fasl-op-handler cold-read-in-package
                             k2:$$fasl-op-in-package
  (stream)
  (let ((pkg (cold-read-object stream)))
    (format t "~%Ignoring IN-PACKAGE ~s" pkg)))

(define-cold-fasl-op-handler cold-read-defsubst
                             k2:$$fasl-op-defsubst
  (stream)
  (let ((name     (cold-read-object stream))
        (source   (cold-read-object stream))
        (function (cold-read-object stream)))
    name
    source
    (enter-compiled-function
      function
      *file-being-cold-loaded*
      (nc::ncompiled-function-code function))))

(define-cold-fasl-op-handler cold-read-bignum
                             k2:$$fasl-op-bignum
  (stream)
  (let ((size (cold-read-fixnum stream)))
    (do ((count 0  (1+ count))
         (number 0 (dpb (cold-read-byte stream) (byte 8. (* count 8.)) number)))
        ((= count (* size 4.))
         (if (ldb-test (byte 1. (1- (* count 8.))) number)
             (- number (expt 2. (* count 8.)))
             number)))))

(define-cold-fasl-op-handler cold-do-defvar             ;installed 1/5/88 by RG.  Just tries to ignore it, pretty much.
                             k2:$$fasl-op-defvar
  (stream)
  (let ((symbol (cold-read-object stream)))
    (let ((value (cold-read-someones-value stream symbol))
          (documentation (cold-read-object stream)))
      (format t "~%Ignoring defvar ~s ~s ~s" symbol documentation value))
    ))


(defun cold-read-someones-value (stream someone)
  (let ((opcode (cold-read-opcode stream)))
    (cond ((= opcode k2:$$fasl-op-unbound)
           ;(cons:make-pointer $$dtp-unbound someone)
           `(unbound ,someone))
          (t (let ((handler (aref *cold-fasl-op-handler-table* opcode)))
               (if handler
                   (funcall handler stream)
                 (error "Unknown fasl op: ~a" opcode)))))))

(defun cold-read-local-refs (stream)
  (let* ((n (cold-read-fixnum stream))
         (len (+ n n))
         (refs (make-array len)))
      (do ((i 0 (+ i 2)))
          ((>= i len))
        (setf (svref refs i)      (cold-read-fixnum stream))
        (setf (svref refs (1+ i)) (cold-read-fixnum stream)))
      refs))

(defun cold-read-refs (stream)
  (let* ((n (cold-read-fixnum stream))
         (len (+ n n n))
         (refs (make-array len)))
      (do ((i 0 (+ i 3)))
          ((>= i len))
        (setf (svref refs i)       (cold-read-fixnum stream))   ;ref offset
        (setf (svref refs (+ i 1)) (cold-read-object stream))   ;function called
        (setf (svref refs (+ i 2)) (cold-read-fixnum stream)))  ;number of args
      refs))

(defun cold-read-entry-points (stream)
  (let* ((n (cold-read-fixnum stream))
         (len (+ n n))
         (entries (make-array len)))
      (do ((i 0 (+ i 2)))
          ((>= i len))
        (setf (svref entries i)      (cold-read-fixnum stream))    ;number of args
        (setf (svref entries (1+ i)) (cold-read-fixnum stream)))   ;entry point offset
      entries))

(defun cold-read-immediates (stream)
  (let ((len (ash (cold-read-fixnum stream) 1)))
    (let ((immediates (make-array len)))
      (do ((i 0 (+ i 2)))
          ((>= i len))
        (setf (svref immediates i)     (cold-read-fixnum stream))
        (setf (svref immediates (1+ i)) (cold-read-object stream)))
      immediates)))

(defun cold-load-code (stream)
  (let ((size-in-instructions (cold-read-fixnum stream)))
    (do ((count size-in-instructions (1- count))
         (instructions '() (cons (cold-read-instruction stream) instructions)))
        ((zerop count) (reverse instructions)))))

;(define-cold-fasl-op-handler NC::FASL-OP/EVAL (stream)
;  (let ((form (cold-read stream)))
;    (case (car form)
;      ;((in-package export) (eval form))
;      ((defconstant))
;      (t (format t "~&~s wants to be evaluated" form)))))


(defun cold-link-local-refs (cfun starting-address)
  (let* ((local-refs (nc::ncompiled-function-local-refs cfun))
         (length (length local-refs)))
    (do ((i 0 (+ i 2)))
        ((>= i length))
      (let ((iaddr (+ starting-address (aref local-refs i)))
            (toffset (aref local-refs (1+ i))))
        (cold-code-write iaddr
                         (if (not (minusp toffset))
                             (dpb (+ starting-address toffset)
                                  hw:%%i-branch-address
                                  (cold-code-read iaddr))
                           ;; negative offset means pc ref (imm32)
                           (dpb vinc:$$dtp-code vinc:%%data-type
                                (logior (- starting-address toffset)
                                        (logand #xFFFFFFFFFF000000
                                                (cold-code-read iaddr))))))))))

(defun get-entry-point (fcn nargs callee)
  (let ((entry-points (nc::ncompiled-function-entry-points fcn)))
    (let ((length (length entry-points)))
      (do ((i 0 (+ i 2)))
          ((>= i length)
           (format t "~&~s calling ~s with wrong number of args: ~d"
                   callee
                   (nc::ncompiled-function-name fcn)
                   nargs)
           0)
        (let ((ep-nargs (aref entry-points i)))
          (when (or (= ep-nargs nargs)
                    (and (minusp ep-nargs)
                         (>= nargs (1- (- ep-nargs)))))
            (return (aref entry-points (1+ i)))))))))


(defun cold-link-refs (cfun starting-address)
  (let* ((refs (nc::ncompiled-function-refs cfun))
         (length (length refs)))
    (do ((i 0 (+ i 3)))
        ((>= i length))
      (let* ((reffun (find-named-function (aref refs (+ i 1))))
             (reffunx (or reffun (find-named-function 'K2:UNDEFINED-FUNCTION)))
             (entry-pt (get-entry-point reffunx
                                        (if reffun (aref refs (+ i 2)) 0)
                                        cfun))
             (ref-addr (+ starting-address (aref refs i))))
        (cold-code-write
          ref-addr
          (logior (logand #xFFFFFFFFFF000000
                          (cold-code-read ref-addr))
                  (+ (nc::ncompiled-function-starting-address reffunx)
                     entry-pt)))
        (unless reffun
          (format t "~&~S is undefined in ~s"
                  (aref refs (+ i 1))
                  (nc::ncompiled-function-name cfun)))))))

(defun cold-link (cfun)
  "Relocate and resolve references of FUNCTION"
  (let ((starting-address (nc::ncompiled-function-starting-address cfun)))
    (cold-link-local-refs cfun starting-address)
    (cold-link-refs cfun starting-address)
    ;; Do immediates
    (unless (zerop (length (nc::ncompiled-function-immediates cfun)))
        (format t "~&Immediates used in ~s: ~s"
                (nc::ncompiled-function-name cfun)
                (nc::ncompiled-function-immediates cfun)))
    ))


(defun link-cold-loaded-functions ()
  (dolist (f *cold-loaded-functions*)
    (cold-link f)))



(defun cold-load-functions ()
  (setq *cold-code-pointer* cold-code-start)
  (setq *cold-loaded-functions* '())
  (dolist (function *cold-functions-loaded*)
    (let* ((descriptor (first function))
           (code       (third function))
           (next-block (dpb 0 (byte 12. 0.) (+ *cold-code-pointer* (dpb 1. (byte 1. 12.) 0.))))
           (size-in-instructions (1+ (nc::ncompiled-function-length descriptor))))
      (when (> (+ size-in-instructions *cold-code-pointer*) next-block)
        (setq *cold-code-pointer* next-block))

      ;;; This prevents the trap code from being shifted upward in memory (no FEF pointer).
      (when (>= *cold-code-pointer* 64.)
        (cold-code-write *cold-code-pointer* #x7fffffff00000000) ;Mark the function header
        (incf *cold-code-pointer*))

      (setf (nc::ncompiled-function-starting-address descriptor) *cold-code-pointer*)
      (dolist (instruction code)
        (cold-code-write *cold-code-pointer* instruction)
        (incf *cold-code-pointer*))
      (push descriptor *cold-loaded-functions*))))


(defun cold-load-file (file compile-type load-type)
  (let ((kfasl-real-pathname (zl:probef (fs:merge-pathname-defaults file nil "KFASL" :newest))))
    (when (or (and (eq compile-type :compile)
                   (cold-file-needs-compiling? file kfasl-real-pathname))
              (eq compile-type :recompile))
      ;(si:goto-package-environment "COMPILER")
      (global:pkg-goto 'user)
      (compiler:::nlisp:compile-file file)
      ;(si:goto-package-environment "DEBUGGER")
      (global:pkg-goto 'k-user))
    (setq kfasl-real-pathname (zl:probef (fs:merge-pathname-defaults file nil "KFASL" :newest)))
    (when (or (and (eq load-type :load)
                   (cold-file-needs-loading? kfasl-real-pathname))
              (eq load-type :reload))
      (forget-functions-in-file kfasl-real-pathname)
      (cold-fasload kfasl-real-pathname)
      (record-cold-file-loaded kfasl-real-pathname)
      (global:pkg-goto 'user)
      ;(si:goto-package-environment "COMPILER")
      (load (zl:send kfasl-real-pathname :new-pathname :type "KENV" :version :newest))
      (global:pkg-goto 'k-user)
      ;(si:goto-package-environment "DEBUGGER")
      )))

(defun cold-load-files (files compile-type load-type)
  (dolist (file files)
    (cold-load-file file compile-type load-type))
  (cold-load-functions)
  (link-cold-loaded-functions))


(defun forget-functions-in-file (file)
  (let ((this-file (zl:send file :generic-pathname)))
    (setq *cold-functions-loaded*
          (remove-if #'(lambda (source-file)
                         (eq source-file this-file))
                   *cold-functions-loaded*
                   :key #'cold-function-entry-source-file))))

(defun fasd-cold-function-info (stream)
  (let ((count (length cold:*cold-loaded-functions*)))
    (fasdump:fasd-fixnum-internal count stream)
    (dolist (fcn cold:*cold-loaded-functions*)
      (let ((name (nc::ncompiled-function-name fcn)))
        (format t "~&~3d  ~A" (setq count (1- count)) name)
        (fasdump:fasd-cold-compiled-function-info
          name
          (nc::ncompiled-function-local-refs fcn)
          (nc::ncompiled-function-refs fcn)
          (nc::ncompiled-function-immediates fcn)
          (nc::ncompiled-function-entry-points fcn)
          (nc::ncompiled-function-length fcn)
          (nc::ncompiled-function-starting-address fcn)
          stream)))))



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
;;; these takes up a quarter of the quantum.

;;; Cluster addresses
;(defparameter *initial-map-data-physical-location*              (cluster->address 2.)) ;64 clusters

(defparameter *quantum-map-physical-location*                    4.)
(defparameter *quantum-map-clusters*                             4.)

(defparameter *region-bits-physical-location*                    8.)
(defparameter *region-bits-clusters*                             4.)

(defparameter *initial-physical-cluster-data-physical-location* (cluster->address 12.)) ;1 cluster

(defparameter *initial-gc-ram-data-physical-location*           (cluster->address 13.)) ;1 cluster

(defparameter *initial-transporter-ram-data-physical-location*  14.)

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

;(defun load-initial-map-data ()

;  ;; Make everything volatility 0.
;  (dotimes (i map::*number-of-map-entries*)
;    (cold-data-write
;      (+ i *initial-map-data-physical-location*)
;      (dpb-multiple
;       0.                     hw:%%map-volatility
;       map::$$cluster-fresh map::%%map-fresh-cluster
;        0.)))

;  ;; Cluster 0 is volatility 1.
;  (cold-data-write
;     *initial-map-data-physical-location*
;    (dpb-multiple
;      1.                         hw:%%map-volatility
;      map::$$cluster-not-fresh map::%%map-fresh-cluster
;      0.)))

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
;  (dotimes (i vinc:*number-of-quanta*)
;    (cold-data-write
;      (+ i (cluster->address *quantum-map-physical-location*))
;      0.))

;  ;; Write initial quanta.
;  (dolist (q *boot-quanta*)
;    (cold-data-write
;      (+ (first q) (cluster->address *quantum-map-physical-location*))
;      (vinc::dpb-multiple-unboxed
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
    (* 1   vinc:$$dtp-array-header-single write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; ARRAY HEADER MULTIPLE not a pointer, but don't bash
    (* 1   vinc:$$dtp-array-header-multiple no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-array-header-multiple trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-array-header-multiple vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-array-header-multiple write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; ARRAY HEADER EXTENSION not a pointer, but don't bash
    (* 1   vinc:$$dtp-array-header-extension no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-array-header-extension trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-array-header-extension vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-array-header-extension write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; STRUCTURE HEADER not a pointer, but don't bash
    (* 1   vinc:$$dtp-structure-header no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-structure-header trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-structure-header vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-structure-header write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; HASH-TABLE HEADER not a pointer, but don't bash
    (* 1   vinc:$$dtp-hash-table-header no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-hash-table-header trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-hash-table-header vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-hash-table-header write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; UNBOXED STRUCTURE HEADER not a pointer, but don't bash
    (* 1   vinc:$$dtp-unboxed-header no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-unboxed-header trans    normal 0 0 0 0)
    (* 1   vinc:$$dtp-unboxed-header vis-evcp normal 1 0 0 1)
    (* 1   vinc:$$dtp-unboxed-header write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; ARRAY
    (* 1   vinc:$$dtp-array  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-array  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-array  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-array  write    normal 0 0 1 1)

    ;; STRUCTURE
    (* 1   vinc:$$dtp-structure  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-structure  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-structure  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-structure  write    normal 0 0 1 1)

    ;; HASH-TABLE
    (* 1   vinc:$$dtp-hash-table  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-hash-table  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-hash-table  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-hash-table  write    normal 0 0 1 1)

    ;; CONS
    (* 1   vinc:$$dtp-cons   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-cons   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-cons   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-cons   write    normal 0 0 1 1)

    ;; SYMBOL HEADER pointer, but don't bash
    (* 1   vinc:$$dtp-symbol-header no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-symbol-header trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-symbol-header vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-symbol-header write    normal 0 0 0 0) ;Compiler-bug writes temps to stack

    ;; SYMBOL
    (* 1   vinc:$$dtp-symbol   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-symbol   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-symbol   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-symbol   write    normal 0 0 1 1)

    ;; BIGNUM
    (* 1   vinc:$$dtp-bignum   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-bignum   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-bignum   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-bignum   write    normal 0 0 1 1)

    ;; RATIONAL
    (* 1   vinc:$$dtp-rational   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-rational   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-rational   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-rational   write    normal 0 0 1 1)

    ;; SHORT-FLOAT
    (* 1   vinc:$$dtp-short-float   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-short-float   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-short-float   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-short-float   write    normal 0 0 1 1)

    ;; SINGLE-FLOAT
    (* 1   vinc:$$dtp-single-float   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-single-float   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-single-float   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-single-float   write    normal 0 0 1 1)

    ;; DOUBLE-FLOAT
    (* 1   vinc:$$dtp-double-float   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-double-float   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-double-float   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-double-float   write    normal 0 0 1 1)

    ;; COMPLEX
    (* 1   vinc:$$dtp-complex   no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-complex   trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-complex   vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-complex   write    normal 0 0 1 1)

    ;; UNBOUND
    (* 1   vinc:$$dtp-unbound  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-unbound  trans    normal 0 1 1 1)
    (* 1   vinc:$$dtp-unbound  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-unbound  write    normal 0 0 1 1)

    ;; I'm not sure this is quite right

    ;; BODY-FORWARD
    (* 1   vinc:$$dtp-body-forward  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-body-forward  trans    normal 0 1 1 1)
    (* 1   vinc:$$dtp-body-forward  vis-evcp normal 1 1 1 1)
    (* 1   vinc:$$dtp-body-forward  write    normal 0 0 1 1)

    ;; HEADER-FORWARD
    (* 1   vinc:$$dtp-header-forward  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-header-forward  trans    normal 0 1 1 1)
    (* 1   vinc:$$dtp-header-forward  vis-evcp normal 1 1 1 1)
    (* 1   vinc:$$dtp-header-forward  write    normal 0 0 1 1)

    ;; COMPILED-FUNCTION
    (* 1   vinc:$$dtp-compiled-function  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-compiled-function  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-compiled-function  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-compiled-function  write    normal 0 0 1 1)

    ;; CODE
    (* 1   vinc:$$dtp-code  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-code  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-code  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-code  write    normal 0 0 1 1)

    ;; LOCATIVE
    (* 1   vinc:$$dtp-locative  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-locative  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-locative  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-locative  write    normal 0 0 1 1)

    ;; UNBOXED-LOCATIVE
    (* 1   vinc:$$dtp-unboxed-locative  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-unboxed-locative  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-unboxed-locative  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-unboxed-locative  write    normal 0 0 1 1)

    ;; LEXICAL-CLOSURE
    (* 1   vinc:$$dtp-lexical-closure  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-lexical-closure  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-lexical-closure  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-lexical-closure  write    normal 0 0 1 1)

    ;; INTERPRETER-CLOSURE
    (* 1   vinc:$$dtp-interpreter-closure  no-trans normal 1 1 1 1)
    (* 1   vinc:$$dtp-interpreter-closure  trans    normal 0 0 1 1)
    (* 1   vinc:$$dtp-interpreter-closure  vis-evcp normal 1 0 1 1)
    (* 1   vinc:$$dtp-interpreter-closure  write    normal 0 0 1 1)
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
;;                  (vinc::dpb-multiple-unboxed
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
;;                  (vinc::dpb-multiple-unboxed
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
;;                (vinc::dpb-multiple-unboxed
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
;;                    (vinc::dpb-multiple-unboxed
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
               (list 2048. '*null-paging-device* 4.))) ;(ceiling *cold-code-size* vinc:*qs-in-quantum*))))

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
                  region-bits:$$region-internal-memory
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
                  region-bits:$$region-internal-memory
                  0.))                          ;swapin quantum 0
          (list 2048.
                4. ;(ceiling *cold-code-size* vinc:*qs-in-quantum*)
                (region-bits:encode-region-bits
                  region-bits:$$region-fixed
                  region-bits:$$region-copy-space
                  region-bits:$$region-space-code
                  region-bits:$$region-read-only
                  region-bits:$$scavenge-enabled
                  region-bits:$$region-internal-memory
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
            region-bits:$$region-internal-memory
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
;                      (ceiling (* region-bits:*number-of-regions* 4.) vinc:*qs-in-quantum*)
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
;        (quanta-needed (ceiling qs-needed vinc:*qs-in-quantum*))
;        (region-bits (region-bits:parameters->region-bits
;                       region-bits:$$region-space-fixed
;                       region-bits:$$scavenge-disabled
;                       region-bits:$$region-read-write
;                       (max 15. (1- (* quanta-needed vinc:*clusters-in-quantum*)))))
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
;         (vinc::dpb-multiple-boxed
;           area-data::$$area-fixed area-data::%%area-data-area-status
;           0.                      area-data::%%area-data-region-thread
;           0.))
;    (setf (area-data::region-list-thread 0.)
;         (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 0.))
;    (setf (area-data::area-region-size   0.) 0.)
;    (setf (area-data::area-region-bits   0.)
;         (vinc::dpb-multiple-boxed
;           (region-bits::parameters->region-bits
;             region-bits::$$region-space-fixed
;             region-bits::$$scavenge-enabled
;             region-bits::$$region-read-only
;             0.)))

;    ;;; Setup PAGING TABLE AREA
;    (setf (area-data::area-region-data   1.)
;         (vinc::dpb-multiple-boxed
;           area-data::$$area-fixed area-data::%%area-data-area-status
;           1.                      area-data::%%area-data-region-thread
;           0.))
;    (setf (area-data::region-list-thread 1.)
;         (hw:dpb-boxed area-data::$$thread-ends area-data::%%region-list-thread-end-flag 1.))
;    (setf (area-data::area-region-size   1.) 0.)
;    (setf (area-data::area-region-bits   1.)
;         (vinc::dpb-multiple-boxed
;           (region-bits::parameters->region-bits
;             region-bits::$$region-space-fixed
;             region-bits::$$scavenge-enabled
;             region-bits::$$region-read-only
;             0.)))

;    ;;; Setup MEMORY MANAGEMENT AREA
;    (setf (area-data::area-region-data   2.)
;         (vinc::dpb-multiple-boxed
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
;  (load-initial-map-data)
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

(defun make-cold-load (&optional (files cold:*cold-files*) (initial-function 'boot:cold-boot-function) (compile-type :compile) (load-type :load))
  "COMPILE-TYPE is :COMPILE or :RECOMPILE or NIL
LOAD-TYPE is :LOAD or :RELOAD or NIL"
  (cold-load-files files compile-type load-type)
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


(defun user#:ktest (&optional inhibit-cold-load?)
  (si:pkg-goto 'k-kbug)
  (unless inhibit-cold-load?
    (k-cold:make-cold-load))
  (si:pkg-goto 'k-kbug)
  (k-kbug:download-cold-load)
  (k-kbug:pseudo-boot)
  (k-kbug:kbug))
