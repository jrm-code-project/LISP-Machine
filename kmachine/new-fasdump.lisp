;;; -*- Mode:LISP; Package:FASDUMP; Readtable:CL; Base:10 -*-

;;;*****************************************************************************************************
;;;*****   This file should be identical to new-fasdump(-for-k-debugger) at all times WKF 5/5/88   *****
;;;*****************************************************************************************************

;;; These duplicate versions are needed to fool make-system since COMPILER-FOR-K and K-DEBUGGER
;;;  both use this file and k-debugger loads it in the k-user hierarchy.   WKF

(export '(
          fasd-defafun
          fasd-defconstant
          fasd-defmacro
          fasd-defsubst
          fasd-defun
          fasd-eof
          fasd-eval
          fasd-in-package
          fasd-object

          ))



;;; To see how the K responds to the fasdump stream follow the bouncing balls from
;;;  kbug-trap-handler-1 and kbug-trap-handler-2   in  k-sys:kbug2;k2.lisp    to
;;;  kbug-fasl-stream and mini-fasl-read-top-level-object and  mini-fasl-read-object-1  in k-sys:k;warm-loader.lisp

;; This is a hack to deal with all the top level forms with a minimum of special casing.
;; It is not efficient about what it expects to see (i.e. it doesn't allow you to take
;; advantage of the fact that you know a symbol is coming and that you don't need the opcode.)
;; It was easy to do.  If you want to save a few bytes here and there, go ahead and hack.

(defstruct fasd-special-top-level-form
  fasl-op
  guts)

(defun fasd-opcode (opcode stream)
  (write-byte opcode stream))

(defun fasd-eof (stream)
  (fasd-opcode k2:$$fasl-op-end-of-file stream))

(defmacro define-fasd (name internal opcode object-var stream-var &body body)
  (declare (zwei::indentation 5 2))
  `(PROGN (DEFUN ,name (,object-var ,stream-var)
            (FASD-OPCODE ,opcode ,stream-var)
            (,internal ,object-var ,stream-var))
          (DEFUN ,internal (,object-var ,stream-var)
            ,@body)))

(eval-when (load compile eval)
  (deftype k-size-fixnum () `(signed-byte 24.))
  )

(define-fasd fasd-fixnum fasd-fixnum-internal K2:$$FASL-OP-FIXNUM fixnum stream
   (check-type fixnum k-size-fixnum)
   (write-byte (ldb (byte 8. 0.)  fixnum) stream)
   (write-byte (ldb (byte 8. 8.)  fixnum) stream)
   (write-byte (ldb (byte 8. 16.) fixnum) stream))

(eval-when (load compile eval)
  (deftype k-bignum () `(and integer (not (signed-byte 24.))))
  )

(define-fasd fasd-bignum fasd-bignum-internal K2:$$fasl-op-bignum bignum stream
   (check-type bignum k-bignum)
   (let ((words-needed (ceiling (1+ (integer-length bignum)) 32.)))
     (fasd-fixnum-internal words-needed stream)
     (do ((count (* words-needed 4.) (1- count))
          (number bignum (ash (logand number (ash -1. 8.)) -8.)))
         ((zerop count))
       (write-byte (ldb (byte 8. 0.) number) stream))))

#|  These are for when the K fasdumps for itself
(define-fasd fasd-short-float fasd-short-float-internal K2:$$fasl-op-short-float flonum stream
   (check-type flonum k-short-float)
   (write-byte (ldb (byte 8. 0.)  flonum) stream)
   (write-byte (ldb (byte 8. 8.)  flonum) stream)
   (write-byte (ldb (byte 8. 16.) flonum) stream)
   (write-byte (ldb (byte 2. 24.) flonum) stream)
   )

(define-fasd fasd-single-float fasd-single-float-internal K2:$$fasl-op-single-float flonum stream
   (check-type flonum k-single-float)
   (let ((unboxed-word (array::%vm-read32 flonum 1)))
     (write-byte (ldb (byte 8. 0.)  flonum) stream)
     (write-byte (ldb (byte 8. 8.)  flonum) stream)
     (write-byte (ldb (byte 8. 16.) flonum) stream)
     (write-byte (ldb (byte 8. 24.) flonum) stream))
   )

(define-fasd fasd-double-float fasd-double-float-internal K2:$$fasl-op-double-float flonum stream
   (check-type flonum k-double-float)
   (let ((unboxed-word (array::%vm-read32 flonum 1)))
     (write-byte (ldb (byte 8. 0.)  flonum) stream)
     (write-byte (ldb (byte 8. 8.)  flonum) stream)
     (write-byte (ldb (byte 8. 16.) flonum) stream)
     (write-byte (ldb (byte 8. 24.) flonum) stream))
   (let ((unboxed-word (array::%vm-read32 flonum 2)))
     (write-byte (ldb (byte 8. 0.)  flonum) stream)
     (write-byte (ldb (byte 8. 8.)  flonum) stream)
     (write-byte (ldb (byte 8. 16.) flonum) stream)
     (write-byte (ldb (byte 8. 24.) flonum) stream))
   )
|#

;;; IEEE single precision format is as follows:
;;; bit 31:        sign bit
;;; bits 23-30:    8 bit exponent + 127.
;;; bits  0-22:    23 bit fraction with a hidden one bit before the binary point

;;; IEEE double precision format is as follows:
;;; bit 63:        sign bit
;;; bits 52-62:    exponent + 1023.
;;; bits  0-51:    52 bit fraction with hidden one bit before the binary point

;;; These are for the lambda generating KFASL files.  The lambda doesn't understand how to represent
;;; K flonums so we just dump out lambda flonums so that they can be fasloaded by the K.

(define-fasd fasd-lambda-short-float fasd-lambda-short-float-internal K2:$$fasl-op-short-float flonum stream
   (check-type flonum short-float)
   (let ((unboxed-k-single-float-word 0)
         (minus-p (minusp flonum)))
     (if (zerop flonum)
         (setq unboxed-k-single-float-word 0)   ;zero sign, exponent, mantissa.

       (multiple-value-bind (significand exponent ignore)
           (decode-float (if minus-p (- flonum) flonum))
         ;;; We have to do this MINUS-P hack because DECODE-FLOAT on the Lambda returns a different significand
         ;;; and exponent for -1.0 and 1.0.
         (setq unboxed-k-single-float-word
               (dpb (if minus-p 1 0) (byte 1 31.) unboxed-k-single-float-word))
         (setq unboxed-k-single-float-word
               (dpb (+ exponent 127.) (byte 8. 23.) unboxed-k-single-float-word))
         (setq unboxed-k-single-float-word
               (dpb (ldb (byte 23 0)            ;hide the "hidden" bit
                         (truncate (* (expt 2 24.) significand) 1))
                    (byte 23 0) unboxed-k-single-float-word)) ))

     ;;; Short floats on the K are just like IEEE single floats except they are shifted so that the bottom
     ;;; 6 bits of the mantissa are discarded.
     (write-byte (ldb (byte 8. (+ 6  0.)) unboxed-k-single-float-word) stream)
     (write-byte (ldb (byte 8. (+ 6  8.)) unboxed-k-single-float-word) stream)
     (write-byte (ldb (byte 8. (+ 6 16.)) unboxed-k-single-float-word) stream)
     (write-byte (ldb (byte 2. (+ 6 24.)) unboxed-k-single-float-word) stream)))

(define-fasd fasd-lambda-full-size-float fasd-lambda-full-size-float-internal K2:$$fasl-op-double-float flonum stream
   (check-type flonum single-float)
   ;;; A SINGLE-FLOAT on the Lambda has a 31 bit mantissa and an 11 bit exponent.  This will
   ;;; not fit in a K single float but it will in a K double float.  First we construct the
   ;;; two unboxed words for the K's representation of the double-float then we dump them.
   (let ((unboxed-k-double-float-word 0)        ;the first and second unboxed words of a K double-float as 64 bits.
         (minus-p (minusp flonum)))
     (if (zerop flonum)
         (setq unboxed-k-double-float-word 0)                   ;zero sign, exponent, mantissa.

       (multiple-value-bind (significand exponent ignore)
           (decode-float (if minus-p (- flonum) flonum))
         ;;; We have to do this MINUS-P hack because DECODE-FLOAT on the Lambda returns a different significand
         ;;; and exponent for -1.0 and 1.0.
         (setq unboxed-k-double-float-word      ;the sign bit
               (dpb (if minus-p 1 0) (byte 1 63.) unboxed-k-double-float-word))
         (setq unboxed-k-double-float-word      ;the exponent
               (dpb (+ exponent 1023.) (byte 11. 52.) unboxed-k-double-float-word))

         ;;; normalize the significand into the fixed-width field
         (setq significand (truncate (* (expt 2 53.) significand) 1))

         ;;; move the mantissa 20 bits at a time
         (setq unboxed-k-double-float-word
               (dpb (ldb (byte 20. 0) significand)
                    (byte 20. 0) unboxed-k-double-float-word))
         (setq unboxed-k-double-float-word
               (dpb (ldb (byte 20. 20.) significand)
                    (byte 20. 20.) unboxed-k-double-float-word))
         (setq unboxed-k-double-float-word
               (dpb (ldb (byte 12. 40.) significand)
                    (byte 12. 40.) unboxed-k-double-float-word))
         ))

     (write-byte (ldb (byte 8.  0.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8.  8.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 16.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 24.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 32.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 40.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 48.) unboxed-k-double-float-word) stream)
     (write-byte (ldb (byte 8. 56.) unboxed-k-double-float-word) stream)
   ))

(define-fasd fasd-string-character fasd-string-character-internal K2:$$FASL-OP-STRING-CHARACTER character stream
   (check-type character string-char)
   (write-byte (char-int character) stream))

(define-fasd fasd-simple-string fasd-simple-string-internal K2:$$FASL-OP-STRING string stream
   (check-type string simple-string)
   (let ((length (length string)))
     (fasd-fixnum-internal length stream)
     (write-string string stream)))
;     (dotimes (index length)
;       (fasd-string-character-internal (char string index) stream))))

(define-fasd fasd-simple-vector fasd-simple-vector-internal K2:$$FASL-OP-SIMPLE-VECTOR vector stream
   (check-type vector simple-vector)
   (let ((length (length vector)))
     (fasd-fixnum-internal length stream)
     (dotimes (index length)
       (fasd-object-considering-circularity (svref vector index) stream))))

(defun fasd-unbound (stream)
  (fasd-opcode K2:$$fasl-op-unbound stream))

(defun fasd-nil (stream)
  (fasd-opcode K2:$$fasl-op-nil stream))

(defun fasd-cons (cons stream)
  (fasd-cons-internal cons stream t))

(defun fasd-cons-internal (cons stream &optional (fasd-opcode-p nil))
  (check-type cons cons)
   (let ((len (list-length cons)))
     (if (and len
              (null (nthcdr len cons)))
         (progn
           (when fasd-opcode-p (fasd-opcode K2:$$FASL-OP-LIST stream))
           (fasd-fixnum-internal len stream)
           (dolist (elt cons)
             (fasd-object-considering-circularity elt stream)))
       (progn
         (when fasd-opcode-p (fasd-opcode K2:$$FASL-OP-CONS stream))
         (fasd-object-considering-circularity (car cons) stream)
         (fasd-object-considering-circularity (cdr cons) stream)))))


(defvar *prims-package* (find-package "PRIMITIVES"))
(defvar *li-package* (find-package "LI"))


;(defun get-symbol-package-name (symbol)
;  (si:package-primary-name (symbol-package symbol)))

;;; $$$ Removed old version again. <22-Nov-88 wkf>
;(defun get-symbol-package-name (symbol)
;  (if (eq (symbol-package symbol) *prims-package*)
;      (if (si:intern-soft (symbol-name symbol) si:pkg-global-package)
;         "PRIMITIVES"
;       ;; this is mildly(*) sleazy
;       (let ((s (si:intern-soft (symbol-name symbol) *li-package*)))
;         (if s
;             (si:package-primary-name (symbol-package s))
;           ;(error "Where does this GLOBAL symbol go? ~s" symbol))))
;           "GLOBAL")))
;      (let ((package (symbol-package symbol)))
;       (if package
;           (si:package-primary-name package)
;         ;; This is the wrong thing to do for gensyms
;         ;; needs to use fasl table
;         "NIL"))))

;;; $$$ Put back newer version.  Unfortunatly, Makes (EQ SI:GET LI:GET) <22-Nov-88 wkf>
(defun get-symbol-package-name (symbol)
  (if (eq (symbol-package symbol) si:pkg-global-package)
      (if (si:intern-soft (symbol-name symbol) *prims-package*)
          "PRIMITIVES"
        ;; this is mildly(*) sleazy
        (let ((s (si:intern-soft (symbol-name symbol) *li-package*)))
          (if s
              (si:package-primary-name (symbol-package s))
            ;(error "Where does this GLOBAL symbol go? ~s" symbol))))
            "GLOBAL")))
      (let ((package (symbol-package symbol)))
        (if package
            (si:package-primary-name package)
          ;; This is the wrong thing to do for gensyms
          ;; needs to use fasl table
          "NIL"))))

;; (*) Sometimes words fail completely. -smh


;(fasdump:get-symbol-package-name 'prims:if)
;==> "LISP"

;(fasdump:get-symbol-package-name 'global:if)
;==> "PRIMITIVES"


(define-fasd fasd-symbol fasd-symbol-internal K2:$$FASL-OP-SYMBOL symbol stream
   (check-type symbol symbol)
   (fasd-object-considering-circularity (symbol-name    symbol) stream)
   (fasd-object-considering-circularity (get-symbol-package-name symbol)
                                        stream))


(defun fasd-compiled-function (function stream)
  (let ((fcn (nc::get-ncompiled-function function)))
    (fasd-compiled-function-info
      (nc::ncompiled-function-name fcn)
      (nc::ncompiled-function-local-refs fcn)
      (nc::ncompiled-function-refs fcn)
      (nc::ncompiled-function-immediates fcn)
      (nc::ncompiled-function-entry-points fcn)
      (nc::ncompiled-function-code fcn)
      stream)))

(defun fasd-compiled-function-info (name local-refs refs immediates entry-points code
                                    stream)
  (fasd-opcode K2:$$fasl-op-compiled-function stream)
  (fasd-object name stream)
  (fasd-link-info local-refs refs entry-points stream)
  (fasd-fixnum-internal (length code) stream)
  (map nil #'(lambda (instruction)
               (fasd-instruction instruction stream))
       code)
  (fasd-immediates immediates stream))

(defun fasd-cold-compiled-function-info (name local-refs refs
                                         immediates entry-points length starting-address
                                         stream)
  (fasd-object name stream)
  (fasd-link-info local-refs refs entry-points stream)
  (fasd-fixnum-internal length stream)
  (fasd-fixnum-internal starting-address stream)
  (fasd-immediates immediates stream))


(defun fasd-link-info (local-refs refs entry-points stream)
  (let ((len (length local-refs)))
    (fasd-fixnum-internal (ash len -1) stream)                   ;number of refs
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-fixnum-internal (svref local-refs i) stream)         ;ref offset
      (fasd-fixnum-internal (svref local-refs (1+ i)) stream)))  ;target offset
  (let ((len (length refs)))
    (fasd-fixnum-internal (/ len 3) stream)                ;number of refs
    (do ((i 0 (+ i 3)))
        ((>= i len))
      (fasd-fixnum-internal (svref refs i) stream)         ;ref offset
      (fasd-object          (svref refs (1+ i)) stream)    ;referenced function name
      (fasd-fixnum-internal (svref refs (+ i 2)) stream))) ;number of args
  (let ((len (length entry-points)))
    (fasd-fixnum-internal (ash len -1) stream)                     ;number of entry points
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-fixnum-internal (svref entry-points i) stream)         ;number of args
      (fasd-fixnum-internal (svref entry-points (1+ i)) stream)))) ;entry offset

(defun fasd-immediates (immediates stream)
  (let ((len (length immediates)))
    (fasd-fixnum-internal (ash len -1) stream)
    (do ((i 0 (+ i 2)))
        ((>= i len))
      (fasd-fixnum-internal (svref immediates i) stream)  ;ref offset
      (fasd-object (svref immediates (1+ i)) stream))))   ;immediate object


(eval-when (compile load eval)
  (deftype k-instruction () '(unsigned-byte 64))
  )

(defun fasd-instruction (inst stream)
  (check-type inst k-instruction)
  (write-byte (ldb (byte 8.  0.) inst) stream)
  (write-byte (ldb (byte 8.  8.) inst) stream)
  (write-byte (ldb (byte 8. 16.) inst) stream)
  (write-byte (ldb (byte 8. 24.) inst) stream)
  (write-byte (ldb (byte 8. 32.) inst) stream)
  (write-byte (ldb (byte 8. 40.) inst) stream)
  (write-byte (ldb (byte 8. 48.) inst) stream)
  (write-byte (ldb (byte 8. 56.) inst) stream))

(defun fasd-defconstant (stream name value &optional (doc-string nil))
  (fasd-opcode K2:$$fasl-op-defconstant stream)
  (fasd-object-considering-circularity name stream)
  (fasd-object-considering-circularity value stream)
  (fasd-object-considering-circularity doc-string stream))

(defun fasd-defparameter (stream name value &optional (doc-string nil))
  (fasd-opcode K2:$$fasl-op-defparameter stream)
  (fasd-object-considering-circularity name stream)
  (fasd-object-considering-circularity value stream)
  (fasd-object-considering-circularity doc-string stream))

(defun fasd-defvar (stream name &optional (value nil value-p) (doc-string nil))
  (fasd-opcode K2:$$fasl-op-defvar stream)
  (fasd-object-considering-circularity name stream)
  (if value-p
      (if (constantp value)
          (fasd-object-considering-circularity
            (if (and (consp value)
                     (eq (car value) 'quote))
                (second value)
              value)
            stream)
        (fasd-eval stream value))
    (fasd-unbound stream))
  (fasd-object-considering-circularity doc-string stream))

(defun fasd-special-top-level-form (obj stream)
  (fasd-opcode (fasd-special-top-level-form-fasl-op obj) stream)
  (mapcar #'(lambda (object) (really-fasd-object object stream)) (fasd-special-top-level-form-guts obj)))

(defun really-fasd-object (obj stream)
  (etypecase obj
    (null               (fasd-nil                   stream))
    (symbol             (fasd-symbol            obj stream))
    (cons               (fasd-cons              obj stream))
    (k-size-fixnum      (fasd-fixnum            obj stream))
    (k-bignum           (fasd-bignum            obj stream))
    (string-char        (fasd-string-character  obj stream))
    (simple-string      (fasd-simple-string     obj stream))
    (nc::ncompiled-function (fasd-compiled-function obj stream))
    (fasd-special-top-level-form  (fasd-special-top-level-form      obj stream))
    ;;; The two flonum things will need to be changed when the fasdumper runs on the K:
    (short-float        (fasd-lambda-short-float obj stream))
    (single-float       (fasd-lambda-full-size-float obj stream))
    (simple-vector      (fasd-simple-vector obj stream))
    ))

;;; Circularity checking is truly simple minded.

(defvar *understand-circular-objects?* nil
  "T when the K can fix up circularities.")
(defvar *all-objects*           '())
(defvar *circular-object-list*  '())
(defvar *fasd-table*            '())

(defun fasd-object (obj stream)
  (setq *all-objects* '()
        *fasd-table*  '())
  (when *understand-circular-objects?* (find-circularities obj))
  (if (null *circular-object-list*)
      (fasd-object-considering-circularity obj stream)
      (progn (fasd-fasd-table stream)
             (fasd-object-considering-circularity obj stream)
             (fasd-opcode K2:$$fasl-op-end-of-object stream))))

(defun find-circularities (obj)
  (if (member obj *all-objects* :test #'eq)
      (pushnew obj *circular-object-list* :test #'eq)
      (etypecase obj
        (null           nil)
        (symbol         (push obj *all-objects*)
                        (find-circularities (symbol-name obj))
                        (find-circularities (si:package-primary-name (symbol-package obj))))
        (cons           (push obj *all-objects*)
                        (find-circularities (car obj))
                        (find-circularities (cdr obj)))
        (k-size-fixnum  nil)
        (k-bignum       nil)
        (string-char    nil)
        (simple-string  (push obj *all-objects*))
        (nc::ncompiled-function
         (push obj *all-objects*)
         (let ((refs       (nc::ncompiled-function-refs obj))
               (immediates (nc::ncompiled-function-immediates obj)))
           (find-circularities refs)
           (find-circularities immediates)))
        (fasd-special-top-level-form (push obj *all-objects*)
                                     (mapcar #'find-circularities (fasd-special-top-level-form-guts obj)))
        ;;; The two flonum things will need to be changed when the fasdumper runs on the K:
        (short-float    nil)
        (single-float   nil)
        )))

(defun object-has-multiple-references? (object)
  (member object *circular-object-list* :test #'eq))

(defun object-has-been-fasdumped? (object)
  (member object *fasd-table* :test #'eq))

(defun fasdump-object-index (object stream)
  (let ((index (position object *fasd-table* :test #'eq)))
    (fasd-opcode K2:$$fasl-op-reference-table-index stream)
    (fasd-fixnum-internal index stream)))

(defun assign-fasd-index (obj stream)
  (setq *fasd-table* (append *fasd-table* (list obj)))
  (let ((index (position obj *fasd-table* :test #'eq)))
    (fasd-opcode K2:$$fasl-op-store-table-index stream)
    (fasd-fixnum-internal index stream)))

(defun fasd-fasd-table (stream)
  (fasd-opcode K2:$$fasl-op-create-table stream)
  (fasd-fixnum-internal (length *circular-object-list*) stream))

(defun fasd-object-considering-circularity (obj stream)
  (if (object-has-multiple-references? obj)
      (if (object-has-been-fasdumped? obj)
          (fasdump-object-index obj stream)
          (progn
            (assign-fasd-index obj stream)
            (really-fasd-object obj stream)))
      (really-fasd-object obj stream)))

;(defun mini-fasd (object)
;  (let ((string
;         (zl::with-output-to-string (string)
;           (fasd-object object string)
;           (fasd-opcode K2:$$fasl-op-end-of-file string))))
;    (dotimes (index (length string))
;      (kbug-streams::kbug-stream-write-byte
;       kbug2-common::kbug-k-input-fasl-stream
;       (char string index)))))

(defmacro define-top-level-fasdump (name opcode &rest arglist)
  `(DEFUN ,name ,(cons 'STREAM arglist)
     (FASD-OBJECT
       (MAKE-FASD-SPECIAL-TOP-LEVEL-FORM
         :FASL-OP       ,opcode
         :GUTS          (LIST ,@arglist))
       STREAM)))

;(define-top-level-fasdump fasd-defun K2:$$fasl-op-defun symbol function)
;(define-top-level-fasdump fasd-defsubst K2:$$fasl-op-defsubst symbol function source)
;(define-top-level-fasdump fasd-defmacro K2:$$fasl-op-defmacro symbol expander-function)
;(define-top-level-fasdump fasd-defafun  K2:$$fasl-op-defafun  symbol code)
;(define-top-level-fasdump fasd-defconstant K2:$$fasl-op-defconstant symbol value documentation)
(define-top-level-fasdump fasd-eval K2:$$fasl-op-eval form)
(define-top-level-fasdump fasd-in-package K2:$$fasl-op-in-package cruft)

(defmacro define-fasd-top-level-function (name opcode &rest args)
  `(DEFUN ,name (FCN STREAM ,@args)
     (FASD-OPCODE ,opcode STREAM)
     ;; probably redundant
     (FASD-OBJECT (NC:NCOMPILED-FUNCTION-NAME FCN) STREAM)
     ,@(mapcar #'(lambda (arg)
                   `(FASD-OBJECT ,arg STREAM))
               args)
     (FASD-COMPILED-FUNCTION FCN STREAM)))

(define-fasd-top-level-function fasd-defun    K2:$$fasl-op-defun)
(define-fasd-top-level-function fasd-defsubst K2:$$fasl-op-defsubst source)
(define-fasd-top-level-function fasd-defmacro K2:$$fasl-op-defmacro)
(define-fasd-top-level-function fasd-defafun  K2:$$fasl-op-defafun)
