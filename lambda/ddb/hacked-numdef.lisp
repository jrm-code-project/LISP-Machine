;;; -*- Mode:LISP; Package:USER; Lowercase:T; Base:10; Readtable:ZL -*-
#|
;; these two are actually initialized by the cold-load builder
(defconstant most-negative-fixnum (%logdpb 1 %%q-boxed-sign-bit 0)
  "Any integer smaller than this must be a bignum.")

(defconstant most-positive-fixnum (%logdpb 0 %%q-boxed-sign-bit -1)
  "Any integer larger than this must be a bignum.")

(defconstant single-float-exponent-offset #o2000)

(defconstant single-float-mantissa-length 31.)

(defconstant single-float-exponent-length 11.)

(defconstant single-float-implicit-sign-bit-p nil)

(defconstant short-float-exponent-offset #o200) ;was #o100 in 98.

(defconstant short-float-mantissa-length 17.)

(defconstant short-float-exponent-length 7)     ;was 6 in 98.

(defconstant short-float-implicit-sign-bit-p t)

(defsubst %short-float-exponent (short-float)
  "Extracts the 8-bit exponent of short-floats as a fixnum, including the sign bit."
  (ldb (byte 8. 17.) (%pointer short-float)))   ;was (byte 7 17) in 98.

(defsubst %single-float-exponent (single-float)
  "Extracts the 11-bit exponent of single-floats as a fixnum, including the sign bit."
  (%p-ldb-offset (byte 11. 8.) single-float 0))

(defsubst %short-float-mantissa (short-float)
  "Extracts the 17-bit mantissa of short-floats as a fixnum, including the implied sign bit."
  (ldb (byte 17. 0) (%pointer short-float)))

(defsubst %single-float-mantissa (single-float)
  "Extracts the 32-bit mantissa of short-floats as a fixnum, including the sign bit."
  (dpb (%p-ldb-offset (byte 8. 0) single-float 0)
       (byte 8. 24.)
       (dpb (%p-ldb-offset (byte 8. 16.) single-float 1) ;Extra DPB fixes negative
            (byte 8. 16.)                               ;fixnum lossages
            (%p-ldb-offset (byte 16. 0) single-float 1))))

(defsetf %single-float-mantissa (single-float) (value)
  `(progn
     (setf (%p-ldb-offset (byte 8. 0) ,single-float 0) (ldb (byte 8. 24.) ,value))
     (setf (%p-ldb-offset (byte 24. 0) ,single-float 1) (ldb (byte 24. 0) ,value))
     ,value))

;;; A rational is of type DTP-EXTENDED-NUMBER and occupies three words.

;;; The second word is the numerator, and the third is the denominator.
;;; Rationals with denominator 0 or 1 are never created.

;;; A complex looks like a rational except it contains %HEADER-TYPE-COMPLEX.
;;; The second word is the real part and the third is the imaginary part.
;;; Rational complexes with imaginary part 0 are not created.
;;; The real and imaginary parts of complexes are either both rational or
;;;  both flonums of the same type

(defsubst %rational-numerator (number)
  (%p-contents-offset number 1))

(defsubst %rational-denominator (number)
  (%p-contents-offset number 2))

(defsubst %complex-real-part (number)
  (%p-contents-offset number 1))

(defsubst %complex-imag-part (number)
  (%p-contents-offset number 2))

;; now microcoded
;; Used only in the macro that follows.
;(defconstant complex-header (dpb %header-type-complex %%header-type-field 0))
;(defsubst %complex-cons (realpart imagpart)
;  (let ((object
;        (%allocate-and-initialize dtp-extended-number dtp-header
;                                  complex-header
;                                  0 number-cons-area 3)))
;    (setf (%complex-real-part object) realpart)
;    (setf (%complex-imag-part object) imagpart)
;    object))

;;; lower-level than complex -- performs no error checks, but does
;;;  complex rational (imagpart=0 => real) canonicalization
(defsubst %complex (realpart imagpart)
  (if (eq imagpart 0) realpart
    (%complex-cons realpart imagpart)))

;; now microcoded
;; Used only in the macro that follows.
;(defconstant ratio-header (dpb %header-type-rational %%header-type-field 0))
;(defsubst %ratio-cons (numerator denominator)
;  "Return a rational number with specified numerator and denominator.
;This can be used to construct rationals not in lowest terms,
;but should not normally be so used."
;  (let ((object
;         (%allocate-and-initialize dtp-extended-number dtp-header
;                                   ratio-header
;                                   0 number-cons-area 3)))
;    (setf (%rational-numerator object) numerator)
;    (setf (%rational-denominator object) denominator)
;    object))

;; now microcoded
;(defsubst complexp (x)
;  "T if X is a complex number.
;Note that this may include complex numbers with an imaginary part of 0.0!
;To avoid this problem, use you may wany to use (NOT (REALP X)) instead."
;  (and (eq (%data-type x) '#,dtp-extended-number)
;       (eq (%p-ldb-offset %%header-type-field x 0)
;         %header-type-complex)))

(defsubst realp (x)
  "T if X is a real number, or a complex number with zero imaginary part."
  (cond ((complexp x) (zerop (%complex-imag-part x)))
        ((numberp x) t)
        (t nil)))

;; now microcoded
;(defsubst rationalp (x)
;  "T if X is an exact rational number (which includes integers).
;ie either a fixnum or a ratio."
;  (or (integerp x)
;      (and (eq (%data-type x) '#,dtp-extended-number)
;          (memq (%p-ldb-offset %%header-type-field x 0)
;                '(#,%header-type-bignum
;                  #,%header-type-rational))
;          t)))

(defsubst bigp (x)
  "Return T if X is a bignum."
  (and (integerp x) (not (fixnump x))))

;;; now microcoded
;(defsubst ratiop (object)
;  "T if OBJECT is a ratio -- a rational number which is not an integer."
;  (and (= (%data-type object) dtp-extended-number)
;       (= (%p-ldb %%header-type-field object) %header-type-rational)
;       t))

;;; single-float-p really...
(defsubst flonump (object)
  "T if OBJECT is a full precision flonum."
  (and (floatp object)
       (not (small-floatp object))))

;;; but is this faster than (+ x (- y y)) ??
(defun %numeric-contage (x y &aux tem)
  "Coerce the number X to be of a supertype of the types (TYPE-OF X) and (TYPE-OF Y)"
  (typecase x
    (rational
     (typecase y
       (rational x)
       (short-float (small-float x))
       (float (float x))
       (t
        (typecase (%complex-real-part y)
          (rational x)                          ;complex rational canonicalization
                                                ; -- should this really be done?
          (short-float (%complex-cons (coerce x 'short-float) 0.0s0))
          (t (%complex-cons (coerce x 'single-float) 0.0f0))))))
    (short-float
     (typecase y
       ((or rational short-float) x)
       (float (float x))
       (t
        (typecase (%complex-real-part y)
          ((or rational short-float) (%complex-cons x 0.0s0))
          (t (%complex-cons (coerce x 'single-float) 0.0f0))))))
    (float
     (typecase y
       ((not complex) x)
       (t (%complex-cons x 0.0f0))))
    (t
     (setq tem (%numeric-contage (%complex-real-part x) y))
     (if (eql tem (%complex-real-part x))
         x
       (typecase tem
         (short-float (%complex-cons tem (coerce (%complex-imag-part x) 'short-float)))
         (t (%complex-cons tem (coerce (%complex-imag-part x) 'single-float))))))))

;;; Does no error checking that NUMBER is a valid number
(defsubst zero-of-type (number)
  "Returns a zero of the same type as NUMBER."
  (typecase number
    (rational 0)
    (short-float 0.0s0)
    (float 0f0)
    (t
     (typecase (%complex-real-part number)
       (rational 0)
       (short-float 0.0s0+0.0s0i)
       (t 0f0+0f0i)))))

;;; returns ANS in stronger type -- should we use %numeric-contage???
(defsubst numeric-contage (ans influencer)
  "Return a number = ANS and of the weakest type stronger than both ANS and INFLUENCER."
  (+ ans (- influencer influencer)))

(defun float-coerce (ans influencer)
  "Return a number = ANS and of the same type as INFLUENCER.
Both ANS and INFLUENCER are assumed to be of type '(OR FLOAT (COMPLEX FLOAT))"
  (typecase influencer
    (short-float (float ans 0s0))
    (float (float ans 0f0))
    (t
     (typecase (%complex-real-part influencer)
       (short-float
        (typecase ans
          (short-float (%complex-cons ans 0s0))
          (float (%complex-cons (float ans 0s0) 0s0))
          (t (typecase (%complex-real-part ans)
               (short-float ans)
               (t (%complex-cons (float (%complex-real-part ans) 0s0)
                                 (float (%complex-imag-part ans) 0s0)))))))
       (t
        (typecase ans
          (short-float (%complex-cons (float ans 0f0) 0f0))
          (float (%complex-cons ans 0f0))
          (t (typecase (%complex-real-part ans)
               (short-float (%complex-cons (float (%complex-real-part ans) 0f0)
                                           (float (%complex-imag-part ans) 0f0)))
               (t ans)))))))))

(defun flonum-exponent (float)
  "Return the exponent of FLONUM to go with (FLONUM-MANTISSA FLONUM).
This exponent, if used to scale the integer which FLONUM-MANTISSA returns,
will produce the original argument.  This is not the same as FLOAT-EXPONENT!"
  (etypecase float
    (short-float
     (- (%short-float-exponent float)
        short-float-mantissa-length short-float-exponent-offset))
    (float
     (- (%single-float-exponent float)
        single-float-mantissa-length single-float-exponent-offset))))

(defun flonum-mantissa (float)
  "Return the mantissa of FLOAT as an integer."
  (if (zerop float) 0
    (etypecase float
      (short-float
       (%short-float-mantissa float))
      (float
       (%single-float-mantissa float)))))

|#
