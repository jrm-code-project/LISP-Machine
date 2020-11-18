;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:ZL -*-

#||

Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright.Text" for
licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************


Some routines for data conversion between different processor representations.
These are not intended to be the final production versions, which would
probably be hand microcoded. -GJC


These routines take the lispm data in lispm form, and the "other-processor"
data in the form of a fixnum or bignum.

Routines:

 IEEE floating point numbers.

 IEEE-32B-BITS, IEEE-32B

 68000 (V7 format) floating point.

 FLOAT-68000-32B
 LISPM-FLOAT->68000

 Integers.

  68000-BYTESWAP
  SIGNED-LISPM->68000
  SIGNED-68000->LISPM

||#

(defun ieee-32b-value (sign exp frac)
  "This is from the definition of IEEE format"
  (if (zerop exp)
      0.0
    (* (expt -1 sign)
       (expt 2.0 (- exp 127))
       (1+ (quotient (float frac) (expt 2 23))))))

(defun ieee-32b-components (x &aux sign exp frac)
  "Returns 3 fixnum values: sign-bit exponent-field fraction-field"
  (cond ((typep x ':short-float)
         (cond ((zerop x)
                (return-from ieee-32b-components (values 0 0 0)))
               ((< x 0.0)
                (setq sign 1)
                (setq x (- x)))
               (t
                (setq sign 0)))
         (setq exp (+ (- (%short-float-exponent x) #o201) 127))
         (setq frac (ash (- (%short-float-mantissa x) (expt 2 16)) (- 23 16))))
        ((typep x ':flonum)
         (cond ((zerop x)
                (return-from ieee-32b-components (values 0 0 0)))
               ((< x 0.0)
                (setq sign 1)
                (setq x (- x)))
               (t
                (setq sign 0)))
         (setq exp (ieee-32b-components-1 x))
         (setq frac (ash (- (%single-float-mantissa x) (expt 2 30)) (- 23 30))))
        (t
         (ferror nil "Not a flonum: ~S" x)))
  (or (<= 1 exp 254)
      (ferror nil "Exponent won't fit in 32b ieee format: ~S" x))
  (values sign exp frac))

(defun ieee-32b-components-1 (x) (declare (special x))  ;yuck! kludge around a bug for now
  (+ (- (%single-float-exponent x) #o2001) 127))

(defun ieee-32b-bits (x)
  "returns the IEEE bit representation as a bignum"
  (multiple-value-bind (sign exp frac)
      (ieee-32b-components x)
    (dpb sign (byte 1 31)
         (dpb exp (byte 8 23)
              (dpb frac (byte 23 0) 0)))))

(defun ieee-32b (bits)
  "takes the IEEE bit representation as a bignum, returns a flonum"
  (ieee-32b-value (ldb (byte 1 31) bits)
                  (ldb (byte 8 23) bits)
                  (ldb (byte 23 0) bits)))

(defun ieee-32b-test (x)
  (multiple-value-bind (a b c)
      (ieee-32b-components x)
    (let ((new-x (ieee-32b-value a b c)))
      (format t "~&~S => ~S (~D ~D ~D)~%" x new-x a b c))
    (let ((n (ieee-32b-bits x)))
      (let ((new-x (ieee-32b n)))
        (format t "~S => ~S (#10R~D #8R~8R #16r~16R)" x new-x n n n)))))



(defun float-68000-32b (x)
  "Take 32bits, a 68000 float, and return a lisp float object"
  ;; note: This takes byte reversal into account. It doesnt try to be
  ;; efficient in its use of lispmachine arithmetic.
  (// (* (expt -1 (ldb #o3701 x))
         (expt 2.0 (- (ldb #o3007 x) #o100))
         (+ (ldb #o2010 x)
            (ash (+ (ldb #o1010 x)
                    (ash (ldb #o0010 x)
                         8.))
                 8.)))
      #o100000000))


(defun lispm-float->68000 (x &aux sign exp frac)
  "Take a lispmachine floating point number and return 32 bits suitable for the 68000"
  (cond ((zerop x)
         (return-from lispm-float->68000 0))
        ((< x 0.0)
         (setq sign 1)
         (setq x (- x)))
        ('else
         (setq sign 0)))
  (etypecase x
    (short-float
     (setq exp (+ (- (si:%short-float-exponent x) #o200) #o100))
     (setq frac (ash (si:%short-float-mantissa x)
                     (- 23 16))))
    (single-float
     (setq exp (+ (- (si:%single-float-exponent x) #o2000) #o100))
     (setq frac (ash (si:%single-float-mantissa x)
                     (- 23 30)))))
  (if (or (< exp 0)
          (> exp #o177))
      (ferror nil "Exponent too big to represent in 68000: ~S" x))
  (dpb sign #o3701
       (dpb exp #o3007
            (dpb (ldb #o0010 frac) #o2010
                 (dpb (ldb #o1010 frac) #o1010
                      (ldb #o2010 frac))))))


(defun 68000-byteswap (integer)
  (dpb (ldb #o0010 integer)
       #o3010
       (dpb (ldb #o1010 integer)
            #o2010
            (dpb (ldb #o2010 integer)
                 #o1010
                 (ldb #o3010 integer)))))

(defun signed-lispm->68000 (integer)
  (68000-byteswap integer))

(defun signed-68000->lispm (integer)
  (let ((n (68000-byteswap integer)))
    (if (bit-test 1_31 n)
        (- n 1_32)
      n)))
