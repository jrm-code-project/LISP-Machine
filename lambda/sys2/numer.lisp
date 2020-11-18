;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Lowercase:T; Base:10 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **


;;; Integer square-root
(defun isqrt (n)
  "Square root of an integer; the greatest positive integer  (SQRT N)
Argument must be a non-negative integer"
  (cond ((not (integerp n))
         (ferror nil "ISQRT called with ~S, which is not an integer" n))
        ((< n 0)
         (%complex 0 (isqrt (- n))))
        ((= n 0) 0)                             ;otherwise, it would loop
        (t (do ((guess (ash 1 (ash (1- (haulong n)) -1))
                   (+ guess epsilon))
                (epsilon))
               ((zerop (setq epsilon (truncate (- n (* guess guess))
                                         (ash guess 1))))
                ;; We are now within 1, but might be too high
                (if (> (* guess guess) n)
                    (1- guess)
                  guess))))))

(defun sqrt (number)
  "Square root of a number, as a float or complex.
Result is a short-float or complex short-float according to type of NUMBER"
  (let* ((n (if (complexp number)
                (if (zerop (%complex-imag-part number))
                    (float (%complex-real-part number))
                  (%complex-cons (float (%complex-real-part number))
                                 (float (%complex-imag-part number))))
              (float number)))
         (val
           (cond ((complexp n)
                  (let* ((abs (abs n))
                         (real (%complex-real-part n))
                         (imag (%complex-imag-part n))
                         (r (sqrt (// (+ real abs) 2))))
                    (%complex-cons r (// imag (+ r r)))))
                 ((< n 0.0)
                  (%complex-cons (- n n) (sqrt (- n))))
                 ((= n 0.0)
                  0.0)
                 (t
                  (let ((f)
                        (i2)
                        (exp (- (%single-float-exponent n) single-float-exponent-offset
                                -2)))
                    (let ((number-cons-area working-storage-area))
                      ;; F and I2 need to be regular-heap-consed to avoid
                      ;; the extra-pdl lossage. Identity switch on stack-group switch.
                      (setq f (+ n 0.0f0)
                            i2 (%float-double 0 1)))
                    (setf (%single-float-exponent f) single-float-exponent-offset)
                    (setf (%single-float-exponent i2)
                          (+ single-float-exponent-offset
                             (if (oddp exp)
                                 (1+ (dpb (ldb (BYTE 23. 1.) exp) (BYTE 23. 0.) exp))
                                 (dpb (ldb (BYTE 23. 1.) exp) (BYTE 23. 0.) exp))))
                    (do ((i 0 (1+ i))
                         (an (* i2 (+ 0.4826004 f (if (oddp exp) -0.25 0.0)))))
                        ((= i 4) an)
                      (setq an (* 0.5 (+ an (// n an))))))))))
    ;number is a complex short float, coerce val's components to shorts.
    ;But when number's imag part was 0, sqrt eliminates the unnecessary work and
    ;val will be a float!  So we have to detect those cases and make the
    ;appropriate type of complex from val.
    (if (complexp number)
        (cond ((typep (%complex-real-part number) 'short-float)
               (if (complexp val)
                (%complex-cons (float (%complex-real-part val) 0s0)
                               (float (%complex-imag-part val) 0s0))
                 (%complex-cons (float val 0s0) 0s0)))
              ((complexp val) val)
              (t (%complex-cons val 0.0)))
      (if (typep number 'short-float)
          (short-float val)
        val))))

(defun log (n &optional b &aux zero)
  "Log of N base BASE, which defaults to e
/(ie by default this is the /"natural/" logarithm function. Supply BASE for an unnatural log."
  (declare (arglist n &optional (base (exp 1))))
  (setq zero (typecase n
               (float (if (typep n 'short-float) 0s0 0f0))
               ((complex float)
                               (if (typep (%complex-real-part n) 'short-float)
                                    #c(0s0 0s0)
                                  #c(0f0 0f0)))
               (complex (setq n (coerce n '(complex float)))
                        #c(0f0 0f0))
               (t (setq n (float n 0f0)) 0f0)))
  (when b
    (if (and (zerop b) (not (zerop n)))
        (return-from log (numeric-contage b n))
      (setq zero (numeric-contage zero b))))
  (setq n (log-aux n))
  (if b (setq n (// n (log-aux b))))
  (float-coerce n zero))

(defun log-aux (n)
  (cond ((= n 0) (ferror 'sys:zero-log "Attempt to take logarithm of zero: ~S." n))
        ((complexp n)
         (%complex-cons (log-aux (abs n)) (phase n)))
        ((= n 1) 0.0)
        ((< n 0) (%complex-cons (log-aux (- n)) pi))
        (t
         (let* ((f (let ((number-cons-area working-storage-area))
                     (float n 0f0)))
                (i (1- (float-exponent f))))    ;i gets the base 2 exponent
           ;; f gets the mantissa (1.0 to 2.0) ie 2x(float-fraction f)
           (setf (%single-float-exponent f) (1+ single-float-exponent-offset))
           (setq f (// (- f 1.414213562374)
                       (+ f 1.414213562374)))
           (setq f (+ .5
                      (* f (+ 2.885390073
                              (* (setq f (* f f))
                                 (+ 0.9618007623
                                    (* f (+ 0.5765843421
                                            (* 0.4342597513 f)))))))))
           (* 0.69314718056 (+ i f))))))

(defun exp (n &aux m f)
  "e to power of a number, as a flonum.  Small flonum arg gets small flonum value."
  (cond ((zerop n)
         (+ 1 n))                               ;makes a 1 of the same type as n
        ((typep n 'complex)
         (setq m (exp (%complex-real-part n))
               f (%complex-imag-part n))
         ;; If I can think of a better way of doing cis than cos + i sin, then change
         ;; this to use that.
         (%complex-cons (* m (cos f)) (* m (sin f))))
        ((typep n 'short-float)
         (setq m (* n 1.442695s0))              ;large e
         (setq n (fix m) f (- m n))
         (ash (+ .5s0 (// f                     ;no doubt there exists a simpler approx for small-floats
                          (+ 9.954596s0
                             (* 0.034657359s0 f f)
                             (- f)
                             (// -617.97227s0
                                 (+ (* f f) 87.4174972s0)))))
              (1+ n)))
        (t
         (setq m (* n 1.44269504))              ;large e
         (setq n (fix m) f (- m n))
         (ash (+ .5 (// f                       ;replace this comment by a reference!
                        (+ 9.95459578
                           (* 0.03465735903 f f)
                           (- f)
                           (// -617.97226953
                               (+ (* f f) 87.417497202)))))
              (1+ n)))))

(defun cosd (ang)
  "Cosine of an angle measured in degrees.
Small flonum arg gets small flonum value."
  (sin (+ (* ang 0.0174532926) 1.570796326) ang))

(defun sind (ang)
  "Sine of an angle measured in degrees.
Small flonum arg gets small flonum value."
  (sin (* ang 0.0174532926) ang))

(defun tand (ang)
  "Tangent of an angle measured in degrees.
Small flonum arg gets small flonum value."
  (tan (* ang 0.0174532926)))

(defun tan (x)
  "Tangent of an angle measured in radians.
Small flonum arg gets small flonum value."
  (float (// (sin x 0.0) (sin (+ x 1.570796326) 0.0)) x))

(defun cos (x)
  "Cosine of an angle measured in radians.
Small flonum arg gets small flonum value."
  (sin (+ x 1.570796326) x))

(defun sin (x &optional (type-specimen x))
  "Sine of an angle measured in radians.
Small flonum arg gets small flonum value.
If TYPE-SPECIMEN is specified, its type determines the type of the result."
  (cond ((complexp x)
         (let ((real (%complex-real-part x))
               (imag (%complex-imag-part x)))
           (%complex-cons (* (sin real) (cosh imag))
                          (- (* (cos real) (sinh imag))))))
        ('else
         (let ((value (if (< (abs x) 1.0s-3)
                          x
                        (min (max (sin-aux (float x)) -1.0) 1.0))))
           (if (small-floatp type-specimen)
               (small-float value)
             (float value))))))

; Sine is computed using the taylor expansion of sin(x) around 0
;
;               x3   x5   x7   x9   x11
;  sin(x) = x - -- + -- - -- + -- - --- + ...
;               3!   5!   7!   9!   11!
;
;                    x2   x4   x6   x8
;  sin(x) = x * (1 - -- + -- - -- + --  ...)
;                    3!   5!   7!   9!
;
; now substitute Q to be x * x
;
;                    Q    Q2   Q3   Q4
;  sin(x) = x * (1 - -- + -- - -- + --  ...)
;                    3!   5!   7!   9!
;
; and finally convert the expansion to a nested form to
; minimize the number of multiplications. The factorials are
; of course pre-computed
;                      -1        1       -1      1
;  sin(x) = x (1 + Q ( -- + Q ( -- + Q ( -- + Q -- ))))
;                      3!       5!       7!     9!
;
; The argument, however, is not constrained to be within +pi/2 to -pi/2
; so we start by dividing the argument by pi/2.  After some messing
; around to figure out what quadrent the angle is in, we take the remainder
; and multiply it by 2/pi and take its sine.  But actually, to save a
; multiply, we work the 2/pi into the coefficients!
;
; For a complex argument z = x + iy,
;
;       sin (z) = sin(x) cosh(y) + i cos(x) sinh(y)
;
; where x any y are real.  So we need a definition of sinh for real values
; (notice that sinh will need sin of real values for it to handle complex
; arguments)
(defun sin-aux (x &aux (pi%2 1.570796326))
  (cond ((complexp x)
         (let ((real (%complex-real-part x))
               (imag (%complex-imag-part x)))
           (%complex-cons (* (sin real) (cosh imag))
                          (* (cos real) (sinh imag)))))
        (t
         (let ((frac (// (abs x) pi%2))         ;only work in the range -pi//2 to pi//2 (90 degreees)
               (d)
               (sign (cond ((> x 0) 1)
                           ((< x 0) -1))))
           (setq d (fix frac))
           (setq frac (- frac d))
           (selectq (ldb (BYTE 2. 0.) d)        ;take low two bits of d to
             (1 (setq sign (minus sign)         ;determine which quadrent the
                      frac (- frac 1)))         ;angle is in.
             (2 (setq sign (minus sign)))
             (3 (setq frac (- frac 1))))
           (let ((y (* frac sign))
                 (y2 (* frac frac)))
             (* y (+ 1.5707963185               ;nested evaluation of the polynomial
                     (* y2 (+ -0.6459637111
                              (* y2 (+ 0.07968967928
                                       (* y2 (+ -0.00467376557
                                                (* y2 0.00015148419))))))))))))))


; Hyperbolic Trancendental Functions
; See pages 83 - 85 in Abramowitz and Stegun
; referenced at top of file.  Taylor expansions
; were not used, they became exponentially inacurate
; for large values of x for sinh

(defun sinh (x)
  "Hyperbolic sine of an angle measured in radians.
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (cond ((complexp x)
           (let ((real (%complex-real-part y))
                 (imag (%complex-imag-part y))
                 (other (%complex-real-part x)))
             (%complex-cons (float (* (sinh real) (cos imag)) other)
                            (float (* (cosh real) (sin imag)) other))))
          (t
           (float (// (- (exp y) (exp (- y))) 2.0) x)))))

(defun cosh (x)
  "Hyperbolic cosine of an angle measured in radians.
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (cond ((complexp x)
           (let ((real (%complex-real-part y))
                 (other (%complex-real-part x))
                 (imag (%complex-imag-part y)))
             (%complex-cons (float (* (cosh real) (cos imag)) other)
                            (float (* (sinh real) (sin imag)) other))))
          (t
           (float (// (+ (exp y) (exp (- y))) 2.0) x)))))

(defun tanh (x)
  "Hyperbolic tangent of an angle measured in radians.
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (cond ((complexp x)
           (let ((2real (* 2 (%complex-real-part y)))
                 (2imag (* 2 (%complex-imag-part y)))
                 (d))
             (setq d (+ (cosh 2real) (cos 2imag)))
             (%complex-cons (// (sinh 2real) d)
                            (// (sin 2imag) d))))
          (t
           (float (// (sinh y) (cosh y)) x)))))


; Inverse Trancendental Functions


;; ASIN(x) = -i log (ix + (sqrt 1 - x**2))
;
; Series expansion from Abramowitz and Stegun, p81
; A&S credits C. Hastings Jr, Approximations for digital computers
; Princeton University Press 1955.  They did not document the
; derivation of the polynomial.  Simple Taylor expansions
; do not behave over the entire range of -1 to 1
;
;            pi                                2
;  asin(x) = -- - (sqrt (1-x))(c0 + c1 x + c2 x + ...)
;            2
;
;
;  which we convert to nested form for efficient calculation
;
;            pi
;  asin(x) = -- - (sqrt (1-x))(c0 + x( c1 + x (c2 + x (c3 + ....))))
;            2
;
; where c0 - c7 are given in A&S
;
; The maximum error for this approximation is given as
; less that 2e-8
;
;                K
;  asin(x) = (-1)  asin(x) + K pi   where K is any integer
;

(defun asin (x &aux (pi%2 1.570796326))
  "Arcsine of x
Small flonum arg gets small flonum value"
  (let ((y (float x)))
    (cond ((= x 0.0) x)                         ;check for x = 0.0
          ((complexp x)
           (let ((real (%complex-real-part x))
                 (imag (%complex-imag-part x)))
             (cond ((minusp (* real imag))
                    (asin-complex-case-1 real imag))
                   ('else
                    (asin-complex-case-2 real imag)))))
          (t
           (let ((z (- 1 y))
                 (answer -0.0012624911))
             (do ((l '(                         ;l is a list of coefficients
                       00.0066700901            ;from inner to outer
                       -0.0170881256            ;except that answer is initially
                       00.0308918810            ;set to the innermost coefficient
                       -0.0501743046
                       00.0889789874
                       -0.2145988016
                       01.5707963050
                       )
                   (cdr l)))
               ((null l))
             (setq answer (+ (car l) (* answer y))))
           (setq answer (float (- pi%2 (* answer (sqrt z))) x))
           )))))


(DEFUN asin-complex-case-2 ($REAL $IMAG)
  ;; output of macsyma->lisp translator for rectform(asin(real+sqrt(-1)*imag)).
  ;; This formula is so bad it isnt even funny.
  (BLOCK $FOO
         NIL
         (PROGN NIL
                ((LAMBDA ($%1 $%2 $%3 $%4 $%5 $%6 $%7)
                   NIL
                   NIL
                   (SETQ $%1 0.707106781)
                   (SETQ $%2 (EXPT $IMAG 2))
                   (SETQ $%3 (EXPT $REAL 2))
                   (SETQ $%4 (-$ (*$ $%3)))
                   (SETQ $%5 (EXPT (+$ (*$ 4.0 $%2 $%3) (EXPT (+$ 1.0 $%2 $%4) 2))
                                   0.5))
                   (SETQ $%6 (EXPT (+$ -1.0 (-$ (*$ $%2)) $%3 $%5) 0.5))
                   (SETQ $%7 (+$ (-$ (*$ $IMAG)) (*$ $%1 (SQRT (+$ 1.0 $%2 $%4 $%5)))))
                   (%complex-cons (-$ (*$ (ATAN2 (+$ (-$ (*$ $REAL)) (*$ $%1 $%6)) $%7)))
                                  (* -0.5
                                     (LOG (+$ (EXPT $%7 2)
                                              (EXPT (+$ $REAL (-$ (*$ $%1 $%6))) 2))))))
                 0.0
                 0.0
                 0.0
                 0.0
                 0.0
                 0.0
                 0.0))))





;
;  acos(x) = pi/2 - asin(x)
;
;  acos(-x) = pi - acos(x) = pi - pi/2 + asin(x) = pi/2 - asin(-x)
;
(defun acos (x &aux (pi%2 1.570796326))
  "Arccosine of x
Small flonum arg gets small flonum value"
  (let ((y (float x)))
    (float
      (- pi%2 (asin y))
      x)))

(defun cl:atan (y &optional x)
  "Arctangent in radians of y//x, between - and .
Small flonum arg gets small flonum value."
  (if (< y 0)
      (- (atan (- y) x))
    (atan y x)))

(deff zl:atan2 'cl:atan)

; For real values,
; ATAN is computed using a polynomial approximation, but
; its derivation was never documented.
;
; The expansion is converted to a nested form to
; minimize the number of multiplications.
; (see SIN and ASIN for examples of simpler expansions)
;
;            y
;  for atan(---)
;            x
;
;        |y| - |x|
; TEMP = ---------- ;  TEMP2 = TEMP * TEMP
;        |y| + |x|
;
;
;  atan(x//y ) = TEMP ( c0 + TEMP2 ( c1 + TEMP2 ( c2 + TEMP2 ( c3 + TEMP2 ( c4 + ... ))))
;
;  The coeficients c0 - c8 are supplied as a list in the function.
;  atan(x) = atan(x) + K pi , where K is any integer
;
; for a complex argument, z = x + yi
;
;            1             2x           i        x2 + ( y + 1)2
; atan (z) = -- atan ( -------------) + -- ln ( ---------------- )
;            2          1 - x2 - y2     4        x2 + ( y - 1)2
;
; for z not equal to i
;
(defun atan (y &optional x )
  "Arctangent in radians of Y//X, between 0 and 2.
Small flonum arg gets small flonum value.
With only one argument Y, Y may be complex
With two arguments, neither argument may be complex,
and the signs of X and Y are used to derive quadrent
information.  X may be zero, provided Y is not zero. "
  (cond ((complexp x)
         (ferror "second argument to ATAN, ~S is complex! ~
                 ~% With two arguments to ATAN, neither may be complex" x))
        ((complexp y)
         (if x (ferror "first argument to ATAN, ~S is complex while second argument = ~S !~
                        ~% With two arguments to ATAN, neither may be complex" y x))
         (if (or (= y #c(0.0 1.0)) (= y #c(0.0 -1.0)))
             (ferror "the points i and -i are excluded from the domain of atan"))
         (let ((i #c(0.0 1.0))
               (-i #c(0.0 -1.0)))
           (* -i (log (*
                        (+ (* i y) 1)
                        (sqrt (// 1 (+ 1 (* y y))))))))
         )
        (t
         (let ((absx (if x (abs (float x)) (setq x 1.0)))
               (absy (abs (float y)))
               (temp)
               (temp2)
               (ans -0.004054058))
           (setq temp (// (- absy absx) (+ absy absx))
                 temp2 (* temp temp))
           (do ((l '( 0.0218612288              ;list of the coefficients,
                     -0.0559098861              ;from inner to outermost,
                     0.0964200441               ;except for the innermost
                     -0.139085335               ;of the nested polynomial.
                     0.1994653499               ;ANS is already set to the
                     -0.3332985605              ;innermost coefficient
                     0.9999993329)
                   (cdr l)))
               ((null l))
             (setq ans (+ (* ans temp2) (car l))))
           (setq ans (* ans temp))
           (setq temp (abs ans))
           (cond ((or ( temp .7855) (< temp .7853))
                  (setq ans (+ ans 0.7853981634)))
                 ((< ans 0) (setq ans (// absy absx)))
                 (t (setq ans (+ (// absx absy) 1.5707963268))))
           (setq temp ans
                 ans (- pi ans))
           (if ( x 0) (swapf temp ans))
           (when (< y 0)
             (setq ans (+ ans (+ temp temp))))
           (if (and (small-floatp x) (small-floatp y))
               (small-float ans)
             ans)))))

; Inverse Hyperbolic Trancendental Functions
; See pages 84 - 85 in Abramowitz and Stegun
; and P.209 in Steele

(defun asinh (x)
  "Hyperbolic arcsine of x
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (float (log (+ y
                   (sqrt (+ 1 (* y y)))))
           x)))

(defun acosh (x)
  "Hyperbolic arccosine of x
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (float (log (+ y (* (+ y 1)
                        (sqrt (// (- y 1)
                                  (+ y 1))))))
           x)))

(defun atanh (x)
  "Hyperbolic arctangent of x
Small flonum arg gets small flonum value."
  (let ((y (float x)))
    (float
      (// (log (// (+ 1 y) (- 1 y))) 2.0)
           x)))


(defun expt-hard (base-number power-number)
  (cond ;; ((eq power-number 0)                 ;integer 0
        ;;  (numeric-contage 1 base-number))
        ((= power-number 0)
         ;; (if (zerop base-number)
         ;; (error 'sys:illegal-expt :base base-number :exponent power-number
           (numeric-contage (numeric-contage 1 power-number) base-number))
        ;;)
        ((zerop base-number)
         (if (plusp (realpart power-number))
             (numeric-contage base-number power-number)
           (error 'sys:illegal-expt :base base-number :exponent power-number)))
        ((integerp power-number)
         (let ((minusp (minusp power-number)))
           (setq power-number (abs power-number))
           (do ((ans (if (oddp power-number) base-number (numeric-contage 1 base-number))
                     (if (oddp power-number) (* ans base-number) ans)))
               ((zerop (setq power-number (ash power-number -1)))
                (if minusp (cl:// ans) ans))
             ;; to avoid overflow, procrastinate squaring
             (setq base-number (* base-number base-number)))))
        ;; this is a truly losing algorithm ...
        (t
         (exp (* power-number (log base-number))))))


;;;; Randomness

(DEFVAR *RANDOM-STATE* NIL
  "Default random number generator data")

(DEFSTRUCT (RANDOM-STATE :NAMED-ARRAY (:CONSTRUCTOR MAKE-RANDOM-STATE-1) (:ALTERANT NIL)
                         (:PRINT-FUNCTION
                           (LAMBDA (RANDOM-STATE STREAM DEPTH)
                             (LET ((*PRINT-ARRAY* T))
                               (PRINT-NAMED-STRUCTURE 'RANDOM-STATE
                                                      RANDOM-STATE DEPTH STREAM)))))

    RANDOM-SEED
    RANDOM-POINTER-1
    RANDOM-POINTER-2
    RANDOM-VECTOR)

(DEFUN MAKE-RANDOM-STATE (&OPTIONAL STATE)
  "Create a new random-state object for RANDOM to use.
If STATE is such a state object, it is copied.
If STATE is NIL or omitted, the default random-state is copied.
If STATE is T, a new state object is created and initialized based on the microsecond clock."
  (COND ((EQ STATE NIL)
         (LET ((NEW (COPY-OBJECT *RANDOM-STATE*)))
           (SETF (RANDOM-VECTOR NEW)
                 (COPY-OBJECT (RANDOM-VECTOR NEW)))
           NEW))
        ((EQ STATE T)
         (RANDOM-CREATE-ARRAY 71. 35. (TIME:FIXNUM-MICROSECOND-TIME)))
        (T (LET ((NEW (COPY-OBJECT STATE)))
             (SETF (RANDOM-VECTOR NEW)
                   (COPY-OBJECT (RANDOM-VECTOR NEW)))
             NEW))))

(DEFUN RANDOM-CREATE-ARRAY (SIZE OFFSET SEED &OPTIONAL (AREA NIL))
  (LET ((DEFAULT-CONS-AREA (OR AREA DEFAULT-CONS-AREA)))
    (LET ((ARRAY (MAKE-RANDOM-STATE-1
                   :RANDOM-VECTOR (MAKE-ARRAY SIZE)
                   :RANDOM-SEED SEED
                   :RANDOM-POINTER-1 0
                   :RANDOM-POINTER-2 OFFSET)))
      (RANDOM-INITIALIZE ARRAY)
      ARRAY)))

(eval-when (compile)
  (unless (member %%q-pointer '(24. 25. 31.))
    (warn "RANDOM-INITIALIZE needs byte-specification for %%Q-POINTER value = ~D." %%q-pointer)))

(DEFUN RANDOM-INITIALIZE (ARRAY &OPTIONAL NEW-SEED &AUX SIZE X POINTER)
   (IF (NOT (NULL NEW-SEED))
       (SETF (RANDOM-SEED ARRAY) NEW-SEED))
   (SETQ SIZE (LENGTH (RANDOM-VECTOR ARRAY))
         POINTER (ALOC (RANDOM-VECTOR ARRAY) 0))
   (SETF (RANDOM-POINTER-2 ARRAY) (CL:REM (+ SIZE (- (RANDOM-POINTER-2 ARRAY)
                                                     (RANDOM-POINTER-1 ARRAY)))
                                          SIZE))
   (SETF (RANDOM-POINTER-1 ARRAY) 0)
   (ARRAY-INITIALIZE (RANDOM-VECTOR ARRAY) 0)
   (SETQ X (RANDOM-SEED ARRAY))
   (DOLIST (BYTE-SPEC
             (CASE %%Q-POINTER
               (24. (list (BYTE 12. 12.) (BYTE 12. 0.)))
               (25. (list (BYTE 12. 12.) (BYTE 12. 0.) (BYTE 1. 24.)))
               (31. (list (BYTE 12. 12.) (BYTE 12. 0.) (BYTE 9. 24.)))
               (T (FERROR "Internal error -- unexpected %%Q-POINTER value = ~D." %%q-pointer))))
     (DO ((I 0 (1+ I))) ((= I SIZE))
       (SETQ X (%POINTER-TIMES X 4093.))                        ;4093. is a prime number.
       (%P-DPB-OFFSET (LDB (BYTE 12. 11.) X) BYTE-SPEC POINTER I))))

(DEFUN RANDOM (&OPTIONAL HIGH ARRAY &AUX PTR1 PTR2 SIZE ANS VECTOR)
  "Returns a randomly chosen number.
With no argument, value is chosen randomly from all fixnums.
If HIGH is an integer, the value is a nonnegative integer and less than HIGH.
If HIGH is a flonum or small flonum, the value is a nonnegative
 number of the same type, and less than HIGH.
ARRAY can be an array used for data by the random number generator (and updated);
 you can create one with RANDOM-CREATE-ARRAY or MAKE-RANDOM-STATE."
  (WHEN HIGH
    (CHECK-TYPE HIGH (NON-COMPLEX-NUMBER 0) "a positive real number"))
  (WHEN (NULL ARRAY)
    (OR (AND (VARIABLE-BOUNDP *RANDOM-STATE*) *RANDOM-STATE*)
        (SETQ *RANDOM-STATE* (RANDOM-CREATE-ARRAY 71. 35. 105)))
    (SETQ ARRAY *RANDOM-STATE*))                ;Initialization as optional arg loses on BOUNDP.
  (WITHOUT-INTERRUPTS
    (SETQ PTR1 (RANDOM-POINTER-1 ARRAY)
          PTR2 (RANDOM-POINTER-2 ARRAY)
          VECTOR (RANDOM-VECTOR ARRAY)
          SIZE (LENGTH VECTOR))
    (OR (< (SETQ PTR1 (1+ PTR1)) SIZE)
        (SETQ PTR1 0))
    (OR (< (SETQ PTR2 (1+ PTR2)) SIZE)
        (SETQ PTR2 0))
    (SETF (RANDOM-POINTER-1 ARRAY) PTR1)
    (SETF (RANDOM-POINTER-2 ARRAY) PTR2)
    (SETQ ANS (%MAKE-POINTER-OFFSET DTP-FIX (AR-1 VECTOR PTR1) (AR-1 VECTOR PTR2)))
    (ASET ANS VECTOR PTR2))
  (COND ((SMALL-FLOATP HIGH)
         (* HIGH (// (SMALL-FLOAT (LOGAND ANS (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1)))
                     (- (SMALL-FLOAT (%LOGDPB 1 %%Q-BOXED-SIGN-BIT 0))))))
        ((FLOATP HIGH)
         (* HIGH (// (FLOAT (LOGAND ANS (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1)))
                     (- (SMALL-FLOAT (%LOGDPB 1 %%Q-BOXED-SIGN-BIT 0))))))
        ((NULL HIGH) ANS)
        (T
         (DO ((BITS 14. (+ BITS %%Q-POINTER))
              (NUMBER (LOGAND ANS (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1))
                      (+ (LOGAND (RANDOM) (%LOGDPB 0 %%Q-BOXED-SIGN-BIT -1))
                         (ASH ANS (1- %%Q-POINTER)))))
             ((> BITS (HAULONG HIGH))
              (MOD NUMBER HIGH))))))

;;; Return a randomly chosen number at least LOW and less than HIGH.
(DEFUN RANDOM-IN-RANGE (LOW HIGH)
  "Randomly chosen flonum not less than LOW and less than HIGH."
  (PROG* ((R (RANDOM))
          (RNORM (// (LOGAND R #o777777) (FLOAT #o1000000))))
     (RETURN (+ LOW (* RNORM (- HIGH LOW))))))

;;; Force *RANDOM-STATE* to get a value.
(RANDOM)

(defconstant pi 3.1415926535
  "The mathematical constant PI.")

;; in numdef.  Set up by the cold-load builder since need in the cold load
;(defconstant most-negative-fixnum (%logdpb 1 %%q-boxed-sign-bit 0)
;  "Any integer smaller than this must be a bignum.")
;
;(defconstant most-positive-fixnum (%logdpb 0 %%q-boxed-sign-bit -1)
;  "Any integer larger than this must be a bignum.")

(defconstant most-positive-short-float (%make-pointer dtp-small-flonum -1)
  "No short float can be greater than this number.")

(defconstant least-positive-short-float (%make-pointer dtp-small-flonum #o600000)
  "No positive short float can be closer to zero than this number.")

(defconstant least-negative-short-float (%make-pointer dtp-small-flonum #o577777)
  "No negative short float can be closer to zero than this (unnormalized) number.")

(defconstant most-negative-short-float (%make-pointer dtp-small-flonum
                                                      (lognot #o377777))
  "No short float can be less than this number.")

(defconstant most-positive-single-float (%float-double (%logdpb 0 %%q-boxed-sign-bit -1)
                                                       (%logdpb 0 %%q-boxed-sign-bit -1))
  "No float can be greater than this number.")

;; BYTE is not loaded
(%p-dpb #o77777 #.(byte 11. 8) most-positive-single-float)
(%p-store-contents-offset -1 most-positive-single-float 1)

(defconstant most-negative-single-float (- 1.0 1.0)
  "No float can be less than this number.")

(%p-dpb #o77777 #.(byte 12. 7) most-negative-single-float)

(defconstant least-positive-single-float (- 1.0 1.0)
  "No positive float can be between zero and this number.")

(%p-dpb 1 #.(byte 11. 8) least-positive-single-float)
(%p-dpb 1 #.(byte 1 6) least-positive-single-float)


(defconstant least-negative-single-float (- 1.0 1.0)
  "No negative float can be between zero and this number.")

(%p-dpb 1 #.(byte 11. 8) least-negative-single-float)
(%p-dpb 1 #.(byte 1 7) least-negative-single-float)
(%p-dpb #o777 #.(byte 5 0) least-negative-single-float)
(%p-store-contents-offset -1 least-negative-single-float 1)

(defconstant most-positive-long-float most-positive-single-float)
(defconstant most-negative-long-float most-negative-single-float)
(defconstant least-positive-long-float least-positive-single-float)
(defconstant least-negative-long-float least-negative-single-float)

(defconstant most-positive-double-float most-positive-single-float)
(defconstant most-negative-double-float most-negative-single-float)
(defconstant least-positive-double-float least-positive-single-float)
(defconstant least-negative-double-float least-negative-single-float)

(defconstant short-float-epsilon
             (+ (small-float (scale-float 1.0s0 -17.))
                (small-float (scale-float 1.0s0 -31.))
                (small-float (scale-float 1.0s0 -33.)))
  "Smallest positive short float which can be added to 1.0s0 and make a difference.")

(defconstant single-float-epsilon (+ (scale-float 1.0 -37) (scale-float 1.0 -75))
  "Smallest positive float which can be added to 1.0 and make a difference.")

(defconstant long-float-epsilon single-float-epsilon)
(defconstant double-float-epsilon single-float-epsilon)

(defconstant short-float-negative-epsilon
             (+ (small-float (scale-float 1.0s0 -18.))
                (small-float (scale-float 1.0s0 -32.))
                (small-float (scale-float 1.0s0 -34.)))
  "Smallest positive short float which can be subtracted from 1.0s0 and make a difference.")

(defconstant single-float-negative-epsilon (scale-float 1.0 -31.)
  "Smallest positive float which can be subtracted from 1.0 and make a difference.")

(defconstant long-float-negative-epsilon single-float-negative-epsilon)
(defconstant double-float-negative-epsilon single-float-negative-epsilon)

; TO DO:
; cl:atan               complex, twoargs
; zl:atan, atan2        complex, twoargs
; sin,cos               complex
; tan                   define
; tand                  define
; asin,acos,atan        define
; sinh,cosh,tanh        define
; asinh,acosh,atanh     define
