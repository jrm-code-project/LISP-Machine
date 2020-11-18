;;; -*- Mode:LISP; Package:TRANSPORTER-RAM; Base:10; Readtable:CL -*-

(in-package 'transporter-ram)

(export '(
          initialize-transporter-ram
          load-transporter-ram
          load-transporter-ram-data
          set-transporter-mode
          transporter-trap-handler
          ))

(defconstant *number-of-transporter-modes*
             (vinc:field-maximum hw:%%memory-control-transporter-mode))

(defconstant *number-of-transport-types*
             (vinc:field-maximum hw:%%memory-status-transport-type))

(defconstant *number-of-transporter-md-byte-values*
             (vinc:field-maximum hw:%%transporter-ram-md-byte))

(define-control-register-modifier modify-transporter-mode
                                  modify-memory-control hw:%%memory-control-transporter-mode)

(defun set-transporter-mode (new-value)
  (modify-transporter-mode new-value)
  nil)

(defun store-into-transporter-ram (value)
  (hw:write-transporter-ram (hw:dpb value hw:%%transporter-ram-bus-offset 0.))
  (hw:nop)
  (hw:nop)
  nil)

(defun read-from-transporter-ram ()
  (hw:ldb (hw:read-gc-ram) hw:%%transporter-ram-bus-offset 0)
 ;  (hw:ldb (hw:read-gc-ram) (byte (byte-size hw:%%transporter-ram-bus-offset) 0.) 0.)  ;was
  )

;(defun initialize-transporter-ram ()
;  ;; Load up the vma-unboxed, md-unboxed set so we can read
;  ;; the boot vector.
;  (trap::disable-traps)
;  (let ((transporter-data
;         (vinc::dpb-multiple-unboxed
;           hw:$$non-trappable-pointer     hw:%%transporter-ram-trappable-pointer
;           hw:$$dont-trap-if-oldspace     hw:%%transporter-ram-trap-if-oldspace
;           hw:$$dont-trap-if-not-oldspace hw:%%transporter-ram-trap-if-not-oldspace
;           hw:$$no-box-error              hw:%%transporter-ram-box-error
;           0.)))
;    (set-transporter-mode vinc:$$transporter-mode-normal)
;    (hw:vma-start-read-unboxed-md-unboxed-no-transport 0)
;    (dotimes (md-byte *number-of-transporter-md-byte-values*)
;      (hw:write-md-unboxed (hw:dpb md-byte hw:%%transporter-ram-md-byte 0.))
;      (hw:write-transporter-ram
;       (hw:dpb transporter-data (byte 4. 4.) 0.))))
;  (hw:vma-start-read-unboxed-md-unboxed 0.)
;  (hw:read-md)
;  (trap::enable-traps))

(defun read-transporter-ram-data-from-memory (md-byte trans-type trans-mode base)
  (hw:vma-start-read-no-transport
    (+ base
       (vinc::dpb-multiple-unboxed
         md-byte    (byte (byte-size hw:%%transporter-ram-md-byte) 0.)
         trans-type (byte (byte-size hw:%%memory-status-transport-type)
                          (byte-size hw:%%transporter-ram-md-byte))
         trans-mode (byte (byte-size hw:%%memory-control-transporter-mode)
                          (+ (byte-size hw:%%memory-status-transport-type)
                             (byte-size hw:%%transporter-ram-md-byte)))
         0.)) :unboxed :unboxed)
  (hw:read-md))

(defmacro with-transporter-mode (mode thunk)
  `(LET ((OLD-MODE (MODIFY-TRANSPORTER-MODE ,mode))
         (THUNK ,thunk))
     (PROG1 (FUNCALL THUNK)
            (SET-TRANSPORTER-MODE OLD-MODE))))

(defun write-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype value)
  (trap::without-traps
    #'(lambda ()
        (with-transporter-mode trans-mode
          #'(lambda ()
              ;; Setup vma boxed and trans type
              (vma-start-read-generic vma-boxed hw:$$unboxed $$read-no-cdr trans-type 0.)
              ;; setup datatype and md-boxed
              (write-md-generic (hw:dpb datatype hw:%%transporter-ram-md-byte 0.) md-boxed)
              (store-into-transporter-ram value)
              ))
        )))

(defun read-transporter-ram (vma-boxed md-boxed trans-type trans-mode datatype)
  (trap::without-traps
    #'(lambda ()
        (with-transporter-mode trans-mode
          #'(lambda ()
              ;; Setup vma boxed and trans type
              (vma-start-read-generic vma-boxed hw:$$unboxed $$read-no-cdr trans-type 0.)
              (hw:nop)
              ;; setup datatype and md-boxed
              (write-md-generic (hw:dpb datatype hw:%%transporter-ram-md-byte 0.) md-boxed)
              (read-from-transporter-ram)
              ))
        )))

(defun write-transporter-ram-address (address data)
  (multiple-value-bind (vma-boxed md-boxed trans-type trans-mode datatype)
      (decode-transporter-address address)
    (write-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype data)))

(defun read-transporter-ram-address (address)
  (multiple-value-bind (vma-boxed md-boxed trans-type trans-mode datatype)
      (decode-transporter-address address)
    (read-transporter-ram vma-boxed md-boxed trans-type trans-mode datatype)))

(defun decode-transporter-address (address)
  (values
    (hw:ldb address (byte 1 11.) 0)
    (hw:ldb address (byte 1 10.) 0)
    (hw:ldb address (byte 2 8) 0)
    (hw:ldb address (byte 2 6) 0)
    (hw:ldb address (byte 6 0) 0)
;         (lisp:ldb (byte 1 11.) address)               ;unfortunately, these dont have symbolic definitions yet.
;         (lisp:ldb (byte 1 10.) address)
;         (lisp:ldb (byte 2 8) address) ;trans-type from instruction
;         (lisp:ldb (byte 2 6) address) ;trans-mode semi-constant from memory-control-register.
;         (lisp:ldb (byte 6 0) address)
          ))


(defun load-transporter-ram (data-vector-origin)
  ;this is actually called during initialization.
  ;it loads the whole thing from a vector in memory.
  "  (on the K side:) LOAD-TRANSPORTER-RAM is called by EVENT-HORIZON which is called by COLD-BOOT-FUNCTION.
     (on the  side:) MEGA-BOOT calls K-BOOT calls PSEUDO-BOOT which tells the K to HW:JUMP to COLD-BOOT-FUNCTION.

     LOAD-TRANSPORTER-RAM reads the transporter ram data which was built by LOAD-BOOT-TRANSPORTER-RAM-DATA.
     (MAKE-COLD-LOAD calls COLD-LOAD-DATA calls COLD-TRANSPORTER-RAM-DATA calls LOAD-BOOT-TRANSPORTER-RAM-DATA)"

 (trap::without-traps
    #'(lambda ()
        (dotimes (trans-mode *number-of-transporter-modes*)
          (dotimes (trans-type *number-of-transport-types*)
            (dotimes (md-byte *number-of-transporter-md-byte-values*)
              (let ((transporter-data
                      (read-transporter-ram-data-from-memory md-byte trans-type trans-mode data-vector-origin)))
                (dotimes (vma-boxed 2.)
                  (dotimes (md-boxed 2.)
                    (write-transporter-ram vma-boxed md-boxed trans-type trans-mode md-byte
                      (hw:ldb transporter-data (byte 4. (vinc::dpb-multiple-unboxed
                                                          vma-boxed (byte 1. 2.)
                                                          md-boxed  (byte 1. 3.)
                                                          0))
                              0))))))))
        ;;; Clear out any traps.
        (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed 0.)
        (hw:read-md)
        nil)))


(defun load-transporter-ram-pattern (vma-boxed md-boxed datatype mstat mctl value)
  (dotimes (vmab 2.)
    (when (or (eq t vma-boxed) (= vma-boxed vmab))
      (dotimes (mdb 2.)
        (when (or (eq t md-boxed) (= mdb md-boxed))
          (dotimes (dtp 64.)
            (when (or (eq t datatype) (= dtp datatype))
              (dotimes (type 4.)
                (when (or (eq t mstat) (= type mstat))
                  (dotimes (mode 4.)
                    (when (or (eq t mctl) (= mctl mode))
                      (write-transporter-ram vmab mdb type mode dtp value))))))))))))



(defconstant no-trans vinc::$$transport-type-no-transport)
(defconstant trans    vinc::$$transport-type-transport)
(defconstant vis-evcp vinc::$$transport-type-visible-evcp)
(defconstant write    vinc::$$transport-type-write)

(defconstant normal   vinc::$$transporter-mode-normal)

(defun load-transporter-ram-data ()             ;not used at present.
  ;; Format of transporter ram data:
  ;;                           vma-boxed md-boxed datatype mstat mctl value
  ;; Value is:
  ;;    box-error trap-if-not-old trap-if-old trappable-pointer

  ;; Anything weird we trap on.
  (load-transporter-ram-pattern t t t                 t        t      #b1111)

  ;; Don't trap on unboxed-write if no gc-write-test, no trap on unboxed read if "no-transport"
  (load-transporter-ram-pattern 0 0 t                 no-trans normal #b0000)

  ;; Don't trap on unboxed-write if no gc-write-test, trap on unboxed read if not "no-transport"
  (load-transporter-ram-pattern 0 0 t                 vis-evcp normal #b0110)

  ;; NIL is not treated like a pointer in the transporter ram.
  (load-transporter-ram-pattern t 1 vinc:$$dtp-nil    no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-nil    trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-nil    vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-nil    write    normal #b0000)

  ;; FIXNUMS
  (load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-fixnum write    normal #b0000)

  ;; CHARACTERS same as fixnums
  (load-transporter-ram-pattern t 1 vinc:$$dtp-character no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-character trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-character vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-character write    normal #b0000)

  ;; ARRAY HEADER SINGLE not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-single write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY HEADER MULTIPLE not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-multiple write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY HEADER EXTENSION not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array-header-extension write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; STRUCTURE HEADER not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure-header write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; HASH-TABLE HEADER not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table-header write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; UNBOXED STRUCTURE HEADER not a pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header trans    normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header vis-evcp normal #b1001)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-header write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; ARRAY
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-array  write    normal #b0011)

    ;; STRUCTURE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-structure  write    normal #b0011)

    ;; HASH-TABLE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-hash-table  write    normal #b0011)

    ;; CONS
  (load-transporter-ram-pattern t 1 vinc:$$dtp-cons   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-cons   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-cons   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-cons   write    normal #b0011)

    ;; SYMBOL HEADER pointer, but don't bash
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol-header write    normal #b0000) ;Compiler bug - writes temps to stack

  ;; SYMBOL
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-symbol   write    normal #b0011)

  ;; BIGNUM
  (load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-bignum   write    normal #b0011)

  ;; RATIONAL
  (load-transporter-ram-pattern t 1 vinc:$$dtp-rational   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-rational   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-rational   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-rational   write    normal #b0011)

  ;; SHORT-FLOAT
  (load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-short-float   write    normal #b0011)

  ;; SINGLE-FLOAT
  (load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-single-float   write    normal #b0011)

  ;; DOUBLE-FLOAT
  (load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-double-float   write    normal #b0011)

  ;; COMPLEX
  (load-transporter-ram-pattern t 1 vinc:$$dtp-complex   no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-complex   trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-complex   vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-complex   write    normal #b0011)

  ;; UNBOUND
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  trans    normal #b0111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unbound  write    normal #b0011)


  ;-- incomplete..   one-q-forward, external-value-cell-pointer not here. -rg 4/13/88
  ;; I'm not sure this is quite right

  ;; BODY-FORWARD
  (load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  trans    normal #b0111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  vis-evcp normal #b1111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-body-forward  write    normal #b0011)

  ;; HEADER-FORWARD
  (load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  trans    normal #b0111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  vis-evcp normal #b1111)
  (load-transporter-ram-pattern t 1   vinc:$$dtp-header-forward  write    normal #b0011)

  ;; COMPILED-FUNCTION
  (load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-compiled-function  write    normal #b0011)

  ;; CODE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-code  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-code  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-code  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-code  write    normal #b0011)

  ;; LOCATIVE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-locative  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-locative  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-locative  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-locative  write    normal #b0011)

  ;; UNBOXED-LOCATIVE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unboxed-locative  write    normal #b0011)

  ;; LEXICAL-CLOSURE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-lexical-closure  write    normal #b0011)

  ;; INTERPRETER-CLOSURE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-interpreter-closure  write    normal #b0011)

  ;; DYNAMIC-CLOSURE
  (load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-dynamic-closure  write    normal #b0011)

  ;; SELECT-METHOD
  (load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  trans    normal #b0011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  vis-evcp normal #b1011)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-select-method  write    normal #b0011)

  ;; UNRECONCILED (always trap)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  trans    normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  vis-evcp normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-unreconciled  write    normal #b1111)

  ;; SELF-REF-POINTER
  (load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  no-trans normal #b1111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  trans    normal #b0111)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  vis-evcp normal #b0000)
  (load-transporter-ram-pattern t 1 vinc:$$dtp-self-ref-pointer  write    normal #b0000)

  )


(defun transporter-trap-handler (vma vma-boxed md md-boxed map-bits transp-type)
  (when (= 1 md-boxed)
    (dispatch (byte 2 0) transp-type
              (vinc::$$transport-type-transport
                (dispatch vinc:%%data-type md
                          (vinc:$$dtp-unbound
                            (trap:trap-on)
                            (unless (= 1 vma-boxed)  ;;Dpb Added by --WKF to avoid recursive errors.
                              (setq vma (hw:dpb vinc:$$dtp-unbound vinc:%%data-type vma)))
                            (li:error "Read UNBOUND from memory" vma md))
                          (vinc:$$dtp-header-forward
                            (vmem:vma-start-read-generic
                              vma-boxed md-boxed 0 transp-type
                              (hw:dpb-aligned vma vinc:%%data-type md))
                            (return-from transporter-trap-handler))
                          (vinc:$$dtp-body-forward
                            (trap:trap-on)
                            (let ((index (hw:ldb (hw:32- vma md) vinc:%%fixnum-field 0)))
                              (hw:vma-start-read (hw:dpb vinc:$$dtp-locative vinc:%%data-type md))
                              (hw:read-md)
                              (hw:nop)
                              (hw:nop)
                              (hw:trap-off)
                              (hw:vma-start-read (hw:24+ index (hw:read-vma))))
                            (return-from transporter-trap-handler))
                          (t (li:error "Transporter Trap Error: datatype read was other than VINC::%%DTP-UNBOUND, VINC::%%DTP-HEADER-FORWARD or VINC::DTP-BODY-FORWARD"
                                       vma vma-boxed md md-boxed map-bits transp-type))))
              (t (li:error "Transporter Trap Error: transport type was other than VINC::$$TRANSPORT-TYPE-TRANSPORT"
                           vma vma-boxed md md-boxed map-bits transp-type))))
    (li:tail-error "Transporter trap error: UNBOXED-MD with TRANSPORT on always causes a tranporter trap !!"
                   vma vma-boxed md md-boxed map-bits transp-type))
