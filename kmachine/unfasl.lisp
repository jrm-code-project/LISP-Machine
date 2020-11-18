;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-


(defun k-unfasl-file (filename)
  (let ((pathname (parse-namestring filename)))
    (with-open-file (kfasl-stream pathname :direction :input :element-type '(unsigned-byte 8))
      (with-open-file (output-stream (format nil "ed-buffer: unkfasl of ~a" (namestring pathname)) :direction :output)
        (unkfasl-loop kfasl-stream output-stream)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For some objects we can just bullshit our way through to describe them
;;; but for some things like fixnums ans strings, we need to represent them
;;; on the lambda because they are used in the fasl file to describe other
;;; fasl file entries.

(defstruct (unkfasl-object :named (:constructor make-unkfasl-object-internal)
                           (:print-function print-unkfasl-object))
  type print-function)

(defun make-unkfasl-object (type print-function)
  (make-unkfasl-object-internal :type type
                                :print-function print-function))

(defun print-unkfasl-object (obj stream &rest ignore)
  (funcall (unkfasl-object-print-function obj)
             obj stream))

;(defun unkfasl-object-value (object &optional &key (error-p t))
;  (if (typep object 'unkfasl-object)
;      (if (unkfasl-object-value-p object)
;         (if error-p
;             (error nil "K unfasl doesn't understand values of ~a's" (unkfasl-object-type object))
;           object)
;       (unkfasl-object-value object))
;    object))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unkfasl-loop (kfasl-stream output-stream)
  (do ((something (unkfasl-something kfasl-stream output-stream)
                  (unkfasl-something kfasl-stream output-stream)))
      ((and (typep something 'unkfasl-object)
            (eq :end-of-file (unkfasl-object-type something))))
    (print something output-stream)))

(defstruct (unkfasl-desc (:conc-name) (:constructor make-unkfasl-desc
                                                    (unkfasl-name unkfasl-opcode unkfasl-function)))
  unkfasl-name unkfasl-opcode unkfasl-function)

(defmacro make-unkfasl-table (&rest descriptors)
  `(defconstant unkfasl-table
                (mapcar #'(lambda (thing)
                            (let ((opcode-name (first thing))
                                  (unfasl-function (second thing)))
                              (make-unkfasl-desc opcode-name (symbol-value opcode-name) unfasl-function)))
                        ',descriptors)))

;;; for objects that can be exactly represented on the lambda, a LISP object of the correct type
;;; is returned.  For other objects, a named structure of thpe 'UNKFASL-OBJECT is created.

(make-unkfasl-table
  (k2:$$fasl-op-bignum                  unkfasl-bignum)
  (K2:$$fasl-op-compiled-function       unkfasl-compiled-function)      ; OBJECT
  (K2:$$fasl-op-cons                    unkfasl-cons)                   ; VALUE
  (K2:$$fasl-op-create-table            unkfasl-create-table)
  (K2:$$fasl-op-defafun                 unkfasl-defafun)                ; OBJECT
  (K2:$$fasl-op-defconstant             unkfasl-defconstant)            ; OBJECT
  (K2:$$fasl-op-defmacro                unkfasl-defmacro)               ; OBJECT
  (K2:$$fasl-op-defsubst                unkfasl-defsubst)               ; OBJECT
  (K2:$$fasl-op-defun                   unkfasl-defun)                  ; OBJECT
  (K2:$$fasl-op-end-of-object           unkfasl-end-of-object)
  (K2:$$fasl-op-end-of-file             unkfasl-end-of-file)            ; OBJECT
  (K2:$$fasl-op-escape                  unkfasl-escape)
  (K2:$$fasl-op-eval                    unkfasl-eval)                   ; OBJECT
  (K2:$$fasl-op-fixnum                  unkfasl-fixnum)                 ; VALUE
  (K2:$$fasl-op-in-package              unkfasl-in-package)
  (K2:$$fasl-op-nil                     unkfasl-nil)                    ; VALUE
  (K2:$$fasl-op-reference-table-index   unkfasl-ref-table-index)
  (K2:$$fasl-op-store-table-index       unkfasl-store-table-index)
  (K2:$$fasl-op-string                  unkfasl-string)                 ; VALUE
  (K2:$$fasl-op-string-character        unkfasl-string-character)       ; VALUE
  (K2:$$fasl-op-symbol                  unkfasl-symbol)                 ; OBJECT
  (K2:$$fasl-op-short-float             unkfasl-short-float)            ; OBJECT
  (K2:$$fasl-op-single-float            unkfasl-single-float)           ; OBJECT
  (K2:$$fasl-op-double-float            unkfasl-double-float)           ; OBJECT
  (K2:$$fasl-op-defvar                  unkfasl-defvar)                 ; OBJECT
  (K2:$$fasl-op-defparameter            unkfasl-defparameter)           ; OBJECT
  (K2:$$fasl-op-unbound                 unkfasl-unbound)                ; OBJECT
  (K2:$$fasl-op-simple-vector           unkfasl-simple-vector)
  )

(defun unkfasl-something (kfasl-stream output-stream)
  (let* ((fasl-opcode (read-byte kfasl-stream))
         (unfasl (car (member fasl-opcode unkfasl-table :test #'= :key #'unkfasl-opcode))))
    (if unfasl
        (funcall (unkfasl-function unfasl) kfasl-stream output-stream)
      (error nil "Unknown fasl opcode #x~x" fasl-opcode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Now deal with the fasl opcodes                                              ;

(defun unkfasl-nil (kfasl-stream output-stream)
  (declare (ignore kfasl-stream output-stream))
  nil)

(defun unkfasl-fixnum (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (let ((low-bits    (read-byte kfasl-stream))
        (medium-bits (read-byte kfasl-stream))
        (high-bits   (read-byte kfasl-stream)))
    (dpb high-bits (byte 8. 16.)
         (dpb medium-bits (byte 8. 8.) low-bits))))

(defun unkfasl-short-float (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (make-unkfasl-object :short-float
                       (let ((b1 (read-byte kfasl-stream))
                             (b2 (read-byte kfasl-stream))
                             (b3 (read-byte kfasl-stream))
                             (b4 (read-byte kfasl-stream)))
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "<<short-float ~2x ~2x ~2x ~2x>>" b1 b2 b3 b4)))))

(defun unkfasl-single-float (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (make-unkfasl-object :single-float
                       (let ((b1 (read-byte kfasl-stream))
                             (b2 (read-byte kfasl-stream))
                             (b3 (read-byte kfasl-stream))
                             (b4 (read-byte kfasl-stream)))
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "<<single-float ~2x ~2x ~2x ~2x>>" b1 b2 b3 b4)))))

(defun unkfasl-double-float (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (make-unkfasl-object :double-float
                       (let ((b1 (read-byte kfasl-stream))
                             (b2 (read-byte kfasl-stream))
                             (b3 (read-byte kfasl-stream))
                             (b4 (read-byte kfasl-stream))
                             (b5 (read-byte kfasl-stream))
                             (b6 (read-byte kfasl-stream))
                             (b7 (read-byte kfasl-stream))
                             (b8 (read-byte kfasl-stream)))
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "<<single-float ~2x ~2x ~2x ~2x>>" b1 b2 b3 b4 b5 b6 b7 b8)))))

(defun unkfasl-string-character (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (int-char (read-byte kfasl-stream)))

(defun unkfasl-string (kfasl-stream output-stream)
  (let* ((length (unkfasl-fixnum kfasl-stream output-stream))
         (string (make-string length)))
    (dotimes (i length)
      (aset (unkfasl-string-character kfasl-stream output-stream)
            string i))
    string))

(defun unkfasl-symbol (kfasl-stream output-stream)
  (declare (unspecial package))
  (let ((symbol (unkfasl-something kfasl-stream output-stream))
        (package (unkfasl-something kfasl-stream output-stream)))
    (unless (and (stringp symbol) (stringp package))
      (error nil "FASL-OP-SYMBOL not followed by two strings"))
    (make-unkfasl-object :symbol #'(lambda (object stream)
                                     (declare (ignore object))
                                     (format stream "~a:~a" package symbol)))))

(defun unkfasl-unbound (kfasl-stream output-stream)
  (declare (ignore kfasl-stream output-stream))
  (make-unkfasl-object :unbound-token #'(lambda (object stream)
                                          (declare (ignore object))
                                          (format stream "[unbound]"))))

(defun unkfasl-defvar (kfasl-stream output-stream)
  (let ((symbol (unkfasl-something kfasl-stream output-stream))
        (value  (unkfasl-something kfasl-stream output-stream))
        (doc    (unkfasl-something kfasl-stream output-stream)))
    (unless (eq :symbol (unkfasl-object-type symbol))
      (error nil "defvar of other than a symbol"))
    (make-unkfasl-object :defvar
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "{defvar ~a ~s ~s}" symbol value doc)))))

(defun unkfasl-defparameter (kfasl-stream output-stream)
  (let ((symbol (unkfasl-something kfasl-stream output-stream))
        (value  (unkfasl-something kfasl-stream output-stream))
        (doc    (unkfasl-something kfasl-stream output-stream)))
    (unless (eq :symbol (unkfasl-object-type symbol))
      (error nil "defvar of other than a symbol"))
    (make-unkfasl-object :defparameter
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "{defparameter ~a ~s ~s}" symbol value doc)))))

(defun unkfasl-defconstant (kfasl-stream output-stream)
  (let ((symbol (unkfasl-something kfasl-stream output-stream))
        (value  (unkfasl-something kfasl-stream output-stream))
        (doc    (unkfasl-something kfasl-stream output-stream)))
    (unless (eq :symbol (unkfasl-object-type symbol))
      (error nil "defvar of other than a symbol"))
    (make-unkfasl-object :defconstant
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "{defconstant ~a ~s ~s}" symbol value doc)))))

(defun unkfasl-cons (kfasl-stream output-stream)
  (cons (unkfasl-something kfasl-stream output-stream)
        (unkfasl-something kfasl-stream output-stream)))

(defun unkfasl-defun-like (type kfasl-stream output-stream)
  (let ((name     (unkfasl-something kfasl-stream output-stream))
        (function (unkfasl-something kfasl-stream output-stream)))
    (make-unkfasl-object :defun
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "{~a ~a ~a}" type name function)))))

(defun unkfasl-defmacro (kfasl-stream output-stream)
  (unkfasl-defun-like "defmacro" kfasl-stream output-stream))

(defun unkfasl-defun (kfasl-stream output-stream)
  (unkfasl-defun-like "defun" kfasl-stream output-stream))

(defun unkfasl-defafun (kfasl-stream output-stream)
  (unkfasl-defun-like "defafun" kfasl-stream output-stream))

(defun unkfasl-defsubst (kfasl-stream output-stream)
  (let ((defsubst-stuff (unkfasl-something kfasl-stream output-stream)))
    (declare (ignore defsubst-stuff))
    (unkfasl-defun-like "defsubst" kfasl-stream output-stream)))

(defun unkfasl-compiled-function (kfasl-stream output-stream)
  (let ((symbol       (unkfasl-something kfasl-stream output-stream))
        (local-refs   (unkfasl-cf-local-refs kfasl-stream output-stream))
        (refs         (unkfasl-cf-refs kfasl-stream output-stream))
        (entry-points (unkfasl-cf-entry-points kfasl-stream output-stream))
        (length       (unkfasl-fixnum kfasl-stream output-stream)))
    (declare (ignore symbol local-refs refs entry-points))
    (dotimes (i length)
      (let ((*standard-output* output-stream)
            (lo (unkfasl-read-unboxed-word kfasl-stream output-stream))
            (hi (unkfasl-read-unboxed-word kfasl-stream output-stream)))
        (format t "~&~8,48x ~8,48x  " hi lo)
        (k-kbug:kbug2-print-instruction (dpb (ldb (byte 16 16) hi)
                                             (byte 16 48)
                                             (dpb (ldb (byte 16 0) hi)
                                                  (byte 16 32) lo))
                                        i)))
    (make-unkfasl-object :compiled-code #'(lambda (object stream)
                                            (declare (ignore object))
                                            (format stream "[compiled-code]")))))

(defun unkfasl-cf-local-refs (kfasl-stream output-stream)
  (let* ((n (unkfasl-fixnum kfasl-stream output-stream))
         (refs nil))
    (dotimes (i n)
      (push (cons (unkfasl-fixnum kfasl-stream output-stream)
                  (unkfasl-fixnum kfasl-stream output-stream))
            refs))
    (cons :local-refs (nreverse refs))))

(defun unkfasl-cf-refs (kfasl-stream output-stream)
  (let* ((n (unkfasl-fixnum kfasl-stream output-stream))
         (refs nil))
    (dotimes (i n)
      (push (list (unkfasl-fixnum kfasl-stream output-stream)
                  (unkfasl-symbol kfasl-stream output-stream)
                  (unkfasl-fixnum kfasl-stream output-stream))
            refs))
    (cons :refs (nreverse refs))))

(defun unkfasl-cf-entry-points (kfasl-stream output-stream)
  (let ((n (unkfasl-fixnum kfasl-stream output-stream))
        (ent nil))
    (dotimes (i n)
      (push (list :args (unkfasl-fixnum kfasl-stream output-stream)
                  :offset (unkfasl-fixnum kfasl-stream output-stream))
            ent))
    (cons :entry-points (nreverse ent))))

(defun unkfasl-read-16-bits (kfasl-stream output-stream)
  (declare (ignore output-stream))
  (let ((low-half (read-byte kfasl-stream))
        (high-half (read-byte kfasl-stream)))
    (dpb high-half (byte 8. 8.) low-half)))

(defun unkfasl-read-unboxed-word (kfasl-stream output-stream)
  (let ((low-half (unkfasl-read-16-bits kfasl-stream output-stream))
        (high-half (unkfasl-read-16-bits kfasl-stream output-stream)))
    (dpb high-half (byte 16. 16.) low-half)))

(defun unkfasl-end-of-file (kfasl-stream output-stream)
  (declare (ignore kfasl-stream output-stream))
  (make-unkfasl-object :end-of-file #'(lambda (object stream)
                                        (declare (ignore object))
                                        (format stream "[end-of-file]"))))

(defun unkfasl-eval (kfasl-stream output-stream)
  (let ((form (unkfasl-something kfasl-stream output-stream)))
    (make-unkfasl-object :eval
                         #'(lambda (object stream)
                             (declare (ignore object))
                             (format stream "{eval ~s}" form)))))
