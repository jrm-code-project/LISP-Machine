;;; -*- Mode:LISP; Package:USER; Readtable:CL; Base:8 -*-

;;This function lists powers of 2 from 0 to arg-1 with header

(defun powers-of-2 (number-of-bits &aux (slot-value))
  (format t "~%  bit                 decimal                    octal")
  (do ((i 0 (1+ i)))
      ((>= i number-of-bits))
    (setq slot-value (^ 2 i))
    (format t "~%~5d    ~20d   ~22o" i slot-value slot-value))
  (values))

;;Sample output
#|
(powers-of-2 64.)
  bit                 decimal                    octal
    0                       1                        1
    1                       2                        2
    2                       4                        4
    3                       8                       10
    4                      16                       20
    5                      32                       40
    6                      64                      100
    7                     128                      200
    8                     256                      400
    9                     512                     1000
   10                    1024                     2000
   11                    2048                     4000
   12                    4096                    10000
   13                    8192                    20000
   14                   16384                    40000
   15                   32768                   100000
   16                   65536                   200000
   17                  131072                   400000
   18                  262144                  1000000
   19                  524288                  2000000
   20                 1048576                  4000000
   21                 2097152                 10000000
   22                 4194304                 20000000
   23                 8388608                 40000000
   24                16777216                100000000
   25                33554432                200000000
   26                67108864                400000000
   27               134217728               1000000000
   28               268435456               2000000000
   29               536870912               4000000000
   30              1073741824              10000000000
   31              2147483648              20000000000
   32              4294967296              40000000000
   33              8589934592             100000000000
   34             17179869184             200000000000
   35             34359738368             400000000000
   36             68719476736            1000000000000
   37            137438953472            2000000000000
   38            274877906944            4000000000000
   39            549755813888           10000000000000
   40           1099511627776           20000000000000
   41           2199023255552           40000000000000
   42           4398046511104          100000000000000
   43           8796093022208          200000000000000
   44          17592186044416          400000000000000
   45          35184372088832         1000000000000000
   46          70368744177664         2000000000000000
   47         140737488355328         4000000000000000
   48         281474976710656        10000000000000000
   49         562949953421312        20000000000000000
   50        1125899906842624        40000000000000000
   51        2251799813685248       100000000000000000
   52        4503599627370496       200000000000000000
   53        9007199254740992       400000000000000000
   54       18014398509481984      1000000000000000000
   55       36028797018963968      2000000000000000000
   56       72057594037927936      4000000000000000000
   57      144115188075855872     10000000000000000000
   58      288230376151711744     20000000000000000000
   59      576460752303423488     40000000000000000000
   60     1152921504606846976    100000000000000000000
   61     2305843009213693952    200000000000000000000
   62     4611686018427387904    400000000000000000000
   63     9223372036854775808   1000000000000000000000
|#



;;Routines to return a call to the BYTE function with
;; a fixnum byte-specifier constant as an argument


(defun make-byte-specifier-from-constant (const)        ;returns a list
  (let* ((width    (logand const #o77))
         (position (lsh    const -6.)))
    `(byte ,width ,position)))

(defun unbyte (constant)                                ;returns a string with decimal
  (let* ((width     (logand constant #o77))             ; explicit args to BYTE function
         (position  (lsh    constant -6.))
         (result (format nil "(BYTE ~D. ~D.)" width position)))
    (if (= (eval (read-from-string result)) constant)
        result
      (error "Why didn't this work??? (UNBYTE ~S) -> ~A" constant result))))

;;

(defun compare-functions (start end increment &rest functions-to-compare)
  (do ((x start (+ x increment))) ((> x end))
    (format t "~%")
    (do ((i 0 (1+ i))) ((>= i (length functions-to-compare)))
      (format t "~f   " (funcall (elt functions-to-compare i) x)))))


(defun break-abs-or-let (x)
  (format t "~%Argument is a ~a..." (type-of x))
  (format t "~%ABS says arg is a ~a" (type-of (abs x)))
  (let )
  (values)
  )


(defun asin-test (x
                  &aux
                  (pi%2  1.5707926327)
                  (i  #c(0.0 1.0))
                  (-i  #c(0.0 -1.0)))
  "ARCSINE of X - optimized for speed of execution for real number results."
  ;;Test and coerce argument:
  (print (type-of x))
  (print (type-of pi%2))
  (if (zerop x) x
    (setq x (cond
              ((complexp x) x)
              ((<= (abs x) 1.0) (float x))
              (t (complex x)))))
  ;;Dispatch on argument type:
  (if (complexp x)
      ;;;If X is complex use formal definition of arcsin:
      (* -i (log (+ (* i x) (sqrt (- 1.0 (* x x))))))
    ;;Otherwise use fast approximation for real numbers:
    (let* ((z (abs x))
           (asinz -0.0012624911))
      ;;Summation
      (print (type-of z))
      (print (type-of asinz))
      (do ((n 6 (- n 1)))
          ((< n 0))
        (setq asinz (+ (aref '#(  1.5707963050  ;array of coefficients for polynomial part
                                 -0.2145988016
                                  0.0889789874
                                 -0.0501743046
                                  0.0308918810
                                 -0.0170881256
                                  0.0066700901)  n)
                       (* asinz z))))
      (setq asinz (- pi%2 (* (sqrt (- 1.0 z)) asinz)))
      ;;Sign adjust
      (if (minusp x) (- asinz) asinz))))
