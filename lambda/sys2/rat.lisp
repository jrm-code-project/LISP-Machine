; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Lowercase:T; Readtable:ZL -*-

;COMPLEX moved to QMISC since it needs to be defined early.

(defun numerator (x)
  "Return the numerator of X, which must be rational.
On integers, this is the identity function."
  (typecase x
    (integer x)
    (ratio (%rational-numerator x))
    (t (check-type x rational)
       (numerator x))))

(defun denominator (x)
  "Return the denominator of X, which must be rational.
On integers, this returns 1."
  (typecase x
    (integer x)
    (ratio (%rational-denominator x))
    (t (check-type x rational)
       (denominator x))))


(defun realpart (x)
  "Return the real part of a complex number.  The real part of a real number is itself."
  (typecase x
    (complex (%complex-real-part x))
    (number x)
    (t (check-type x number)
       (realpart x))))

(defun imagpart (x)
  "Return the imaginary part of a complex number, or 0 if given a real number."
  (typecase x
    (complex (%complex-imag-part x))
    (number (- x x))
    (t (check-type x number)
       (imagpart x))))


(defun rational (x)
  "Convert X to a rational number.
If X is a floating point number, it is regarded as completely exact."
  (check-type x non-complex-number)
  (if (rationalp x)
      x
    (make-rational-from-float-exact x)))

(defun make-rational-from-float-exact (x &aux minusflag)
  (if (zerop x) 0
    (if (minusp x)
        (setq minusflag t x (- x)))
    (let* ((mant (flonum-mantissa x))
           (expt (+ (haulong mant) (flonum-exponent x)))
           (zeros (1- (haulong (logand (- mant) mant))))
           (denom (ash 1 (- (haulong mant) zeros))))
      (cond ((plusp expt)
             (setq mant (ash mant (- expt zeros))))
            ((minusp expt)
             (setq mant (ash mant (- zeros)))
             (setq denom (ash denom (- expt))))
            (t (setq mant (ash mant (- zeros)))))
      (if minusflag (setq mant (- mant)))
      (cl:// mant denom))))

(defun rationalize (x &optional tolerance)
  "Return the simplest rational that approximates X well.
TOLERANCE specifies how much precision of X to regard as valid:
 NIL means all that there is of X,
 a positive integer means that many bits of X,
 a negative integer is minus the number of low bits to ignore,
 a flonum is a ratio: it times X gives the magnitude of uncertainty."
  (check-type x number)
  (typecase x
    (integer x)
    (ratio (if tolerance
               (make-rational-from-float (float x) single-float-mantissa-length tolerance)
              x))
    (short-float (make-rational-from-float (float x) short-float-mantissa-length tolerance))
    (float (make-rational-from-float x single-float-mantissa-length tolerance))
    (complex (complex (rationalize (%complex-real-part x) tolerance)
                      (rationalize (%complex-imag-part x) tolerance)))))

(defun make-rational-from-float (x precision tolerance)
  (cond ((null tolerance))
        ((floatp tolerance)
         (setq precision (min 0 (max precision (- (float-exponent tolerance))))))
        ((plusp tolerance)
         (setq precision (min precision tolerance)))
        ((minusp tolerance)
         (setq precision (max 0 (+ precision tolerance)))))
  ;; Continued fraction expansion. This keeps track of precision.
  ;; It also assumes only loss of precision is in the subtraction, and one in
  ;; the division. This seems to be a good assumption - BEE
  (loop with terms = ()
        with pow2 = (%single-float-exponent x)
        as int-part = (floor x)
      do (progn (push int-part terms)
                (decf precision (1+ (haulong int-part)))
                (decf x int-part))
      when (or (zerop x) (> (- pow2 (%single-float-exponent x)) precision))
        do (loop for term in terms
                 with num = 1 and den = 0
              do (psetq num (+ (* term num) den)
                        den num)
              finally (return-from make-rational-from-float (cl:// num den)))
        else do (setq x (// x))))


(defun conjugate (number)
  "Return the complex conjugate of NUMBER.  If NUMBER is real, NUMBER is returned."
  (check-type number number)
  (if (complexp number)
      (if (zerop (%complex-imag-part number))
          number
        (%complex-cons (%complex-real-part number) (- (%complex-imag-part number))))
    number))

(defun phase (number)
  "Return the phase of NUMBER, in radians.
This is the angle in the complex plane from the positive real axis
to the ray from the origin through NUMBER.
It is between - (exclusive) and  (inclusive).
For a positive real, this is 0; for a negative real, this is . For 0, it is zero."
  (check-type number number)
  (if (complexp number)
      (if (zerop number)
          number
        (cl:atan (%complex-imag-part number) (%complex-real-part number)))
    (if (minusp number)
        (typecase number
          (short-float #.(coerce pi 'short-float))
          (t pi))
      (- number number))))

;; need more efficient way to do this than calculate both cos AND sin separately
;; use (cos angle) = (sqrt (1- (sin angle))) ?? Can't be much less accurate.
(defun cis (angle)
  "Return the value of e^(i*ANGLE).  The inverse of the function PHASE."
  (complex (cos angle) (sin angle)))

(defun signum (number)
  "Return a number with the same phase as NUMBER but unit magnitude.
If NUMBER is zero, zero is returned.
For a nonzero non-complex number, the value is either 1 or -1."
  (check-type number number)
  (cond ((zerop number)
         (zero-of-type number))
        ((complexp number)
         (cl:// number (abs number)))
        (t (+ (if (plusp number) 1 -1) (- number number)))))

;;;; Standard arithmetic functions.

(defun numeric-one-argument (code number)
  (declare (dbg:error-reporter))
  (macrolet ((illegal-rational (function number)
               `(ferror "~S illegally applied to the rational number ~S" ,function ,number))
             (illegal-complex (function number)
               `(ferror "~S illegally applied to the complex number ~S" ,function ,number)))
    (unless (eq (%data-type number) dtp-extended-number)
      (ferror "Trap to macrocode for arithmetic on ~S" number))
    (case (%p-ldb-offset %%header-type-field number 0)
      ((#,%header-type-rational)
       (let ((num (%rational-numerator number))
             (den (%rational-denominator number)))
         (case (logand #o77 code)
           (0 (if ( num 0)                                                     ;ABS
                  number
                (%ratio-cons (abs num) den)))
           (1 (%ratio-cons (- num) den))                                        ;MINUS
           (2 (= num 0))                                                        ;ZEROP
           (3 (> num 0))                                                        ;PLUSP
           (4 (< num 0))                                                        ;MINUSP
           (5 (%ratio-cons (+ num den) den))                                    ;ADD1
           (6 (%ratio-cons (- num den) den))                                    ;SUB1
           (7 (case (ldb (BYTE 3. 6.) code)
                (0 (if (plusp num) (truncate num den)                           ; FLOOR
                     (truncate (- num den -1) den)))
                (1 (if (minusp num) (truncate num den)                          ; CEILING
                     (truncate (+ num den -1) den)))
                (2 (truncate num den))                                          ; TRUNCATE
                (3                                                              ; ROUND
                 (let* ((floor (if (plusp num) (truncate num den)
                                 (truncate (- num den -1) den)))
                        (fraction-num (- num (* floor den)))
                        (half-indicator (- (+ fraction-num fraction-num) den)))
                   (if (or (plusp half-indicator)
                           (and (zerop half-indicator)
                                (oddp floor)))
                       (1+ floor)
                     floor)))))
           (8   (// (float num) (float den)))                                   ;FLOAT
           (9   (// (small-float num) (small-float den)))                       ;SMALL-FLOAT
           (10. (illegal-rational 'haulong number))
           (11. (illegal-rational 'ldb number))
           (12. (illegal-rational 'dpb number))
           (13. (illegal-rational 'ash number))
           (14. (illegal-rational 'oddp number))
           (15. (illegal-rational 'evenp number))
           (t (ferror "Arith one-arg op code ~D on ~S" code number)))))
      ((#,%header-type-complex)
       (let ((real (%complex-real-part number))
             (imag (%complex-imag-part number)))
         (case (logand #o77 code)
           (0                                                                   ;ABS
            (if (zerop number)
                0
              (let ((min (min (abs real) (abs imag)))
                    (max (max (abs real) (abs imag)))
                    tem
                    (zunderflow t))
                (if (rationalp max) (setq max (float max)))
                (setq tem (// min max))
                (* (sqrt (+ (* tem tem) 1)) max))))                             ;ABS
           (1 (%complex-cons (- real) (- imag)))                                ;MINUS
           (2 (and (zerop real) (zerop imag)))                                  ;ZEROP
           (3 (illegal-complex 'plusp number))
           (4 (illegal-complex 'minusp number))
           (5 (%complex-cons (1+ real) imag))                                   ;ADD1
           (6 (%complex-cons (1- real) imag))                                   ;SUB1
           (7 (case (ldb (BYTE 3. 6.) code)
                (0 (illegal-complex 'floor number))
                (1 (illegal-complex 'ceiling number))
                (2 (illegal-complex 'truncate number))
                (3 (illegal-complex 'round number))))
           (8 (%complex-cons (float real) (float imag)))                        ;float
           (9 (%complex-cons (small-float real) (small-float imag)))            ;small float
           (10. (illegal-complex 'haulong number))
           (11. (illegal-complex 'ldb number))
           (12. (illegal-complex 'dpb number))
           (13. (illegal-complex 'ash number))
           (14. (illegal-complex 'oddp number)                                  ;ODDP
                ;;(and (zerop imag) (oddp real))
                )
           (15. (illegal-complex 'evenp number)                                 ;EVENP
                ;;(and (zerop imag) (evenp real))
                )
           (t (ferror "Arith one-arg op code ~D on ~S" code number)))))
      (t (ferror "Trap to macrocode for arithmetic on number ~S" number)))))

(defun numeric-two-arguments (code number1 number2 &aux function)
  ;; using these microcode functions is probably slower than calling the macrocode!!
  (setq function (nth code '(#,(function *plus)                                 ;0
                             #,(function *dif)                                  ;1
                             #,(function *times)                                ;2
                             #,(function *quo)                                  ;3
                             #,(function internal-=)                            ;4
                             #,(function internal->)                            ;5
                             #,(function internal-<)                            ;6
                             #,(function *min)                                  ;7
                             #,(function *max)                                  ;8
                             #,(function *boole)                                ;9
                             #,(function %div))))                               ;10.
  (cond ((and (complexp number1) (complexp number2))
         (complex-two-arguments code number1 number2))
        ((complexp number1)
         (complex-two-arguments code number1 (%complex-cons number2 (- number2 number2))))
        ((complexp number2)
         (complex-two-arguments code (%complex-cons number1 (- number1 number1)) number2))
        ((and (floatp number1) (not (small-floatp number1)))
         (funcall function number1 (float number2)))
        ((and (floatp number2) (not (small-floatp number2)))
         (funcall function (float number1) number2))
        ((small-floatp number1)
         (funcall function number1 (small-float number2)))
        ((small-floatp number2)
         (funcall function (small-float number1) number2))
        ((and (rationalp number1) (rationalp number2))
         (rational-two-arguments code number1 number2))
        ((rationalp number1)
         (rational-two-arguments code number1 (rational number2)))
        ((rationalp number2)
         (rational-two-arguments code (rational number1) number2))
        (t
         (ferror "Arith two-arg op code ~D on ~S and ~S" code number1 number2))))

(defun rational-two-arguments (code number1 number2)
  (let (num1 den1 num2 den2)
    (if (integerp number1) (setq num1 number1 den1 1)
      (setq num1 (%rational-numerator number1)
            den1 (%rational-denominator number1)))
    (if (integerp number2) (setq num2 number2 den2 1)
      (setq num2 (%rational-numerator number2)
            den2 (%rational-denominator number2)))
    (case code
      (0 (cl:// (+ (* num1 den2) (* num2 den1)) (* den1 den2)))                 ;ADD
      (1 (cl:// (- (* num1 den2) (* num2 den1)) (* den1 den2)))                 ;SUB
      (2 (cl:// (* num1 num2) (* den1 den2)))                                   ;MUL
      ((3 10.)
       (cl:// (* num1 den2) (* den1 num2)))                                     ;DIV, %DIV
      ((4 11.) (and (= num1 num2) (= den1 den2)))                               ;=, EQL
      (5 (> (* num1 den2) (* num2 den1)))                                       ;GREATERP
      (6 (< (* num1 den2) (* num2 den1)))                                       ;LESSP
      (7 (if (> number1 number2) number2 number1))                              ;MIN
      (8. (if (> number1 number2) number1 number2))                             ;MAX
      (t (ferror "Rational two arg op code ~D on ~S and ~S"
                 code number1 number2)))))

(defun complex-two-arguments (code number1 number2)
  (declare (dbg:error-reporter))
  (let ((real1 (%complex-real-part number1))
        (imag1 (%complex-imag-part number1))
        (real2 (%complex-real-part number2))
        (imag2 (%complex-imag-part number2)))
    (case code
      (0 (%complex (+ real1 real2) (+ imag1 imag2)))                            ;ADD
      (1 (%complex (- real1 real2) (- imag1 imag2)))                            ;SUB
      (2 (%complex (- (* real1 real2) (* imag1 imag2))                          ;MUL
                   (+ (* real1 imag2) (* imag1 real2))))
      ((3 10.)                                                                  ;DIV, %DIV
       (let ((norm2 (+ (* real2 real2) (* imag2 imag2))))
         (%complex (cl:// (+ (* real1 real2) (* imag1 imag2)) norm2)
                   (cl:// (- (* imag1 real2) (* real1 imag2)) norm2))))
      (4 (and (= real1 real2) (= imag1 imag2)))                                 ;=
      (5 (ferror "~S applied to complex numbers ~S and ~S." 'greaterp number1 number2))
      (6 (ferror "~S applied to complex numbers ~S and ~S." 'lessp number1 number2))
      (7 (ferror "~S applied to complex numbers ~S and ~S." 'min number1 number2))
      (8. (ferror "~S applied to complex numbers ~S and ~S." 'max number1 number2))
      (11. (and (eql real1 real2) (eql imag1 imag2)))                           ;EQL
      (t (ferror "Complex two arg op code ~D on ~S and ~S"
                 code number1 number2)))))
