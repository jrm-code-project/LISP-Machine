;;; -*- Mode:LISP; Package:HARDWARE; Base:10; Readtable:CL -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2264/65 unload controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %%i-fpu-unload     (byte 3. 0.)    "WTL 2264/65 unload field")

(defconstant $$fpu-unload-low   4.              "WTL 2264/65 unload low 32 bits")
(defconstant $$fpu-unload-high  0.              "WTL 2264/65 unload high 32 bits")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2264/65 load controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %%i-fpu-load       (byte 6. 3.)    "WTL 2264/65 load field")

(defconstant $$fpu-load-mode    #x02            "WTL 2264/65 load mode reg")
(defconstant $$fpu-load-xy-go   #x0f            "WTL 2264/65 load two single precision nums")
(defconstant $$fpu-load-x-go    #x0b            "WTL 2264/65 load double precision x")
(defconstant $$fpu-load-y       #x08            "WTL 2264/65 load double precision y")
(defconstant $$fpu-load-nop     #x00            "WTL 2264/65 load nop")


(defconstant %%i-fpu-function   (byte 8. 9.)    "WTL 2264/65 function field")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2264 functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$falu-single-add          #x10    "WTL 2265 single precision add")
(defconstant $$falu-double-add          #x11    "WTL 2265 double precision add")
(defconstant $$falu-single-subtract     #x00    "WTL 2265 single precision sub")
(defconstant $$falu-double-subtract     #x01    "WTL 2265 double precision sub")
(defconstant $$falu-single-negate       #x08    "WTL 2265 single precision negate")
(defconstant $$falu-double-negate       #x09    "WTL 2265 double precision negate")
(defconstant $$falu-single-compare      #x20    "WTL 2265 single precision compare")
(defconstant $$falu-double-compare      #x21    "WTL 2265 double precision compare")
(defconstant $$falu-single-test         #x28    "WTL 2265 single precision test")
(defconstant $$falu-double-test         #x29    "WTL 2265 double precision test")
(defconstant $$falu-single-fix          #x38    "WTL 2265 single precision fix")
(defconstant $$falu-double-fix          #x39    "WTL 2265 double precision fix")
(defconstant $$falu-single-float        #x3a    "WTL 2265 single precision float")
(defconstant $$falu-double-float        #x3b    "WTL 2265 double precision float")
(defconstant $$falu-single-to-double    #x3c    "WTL 2265 single to double precision")
(defconstant $$falu-double-to-single    #x3d    "WTL 2265 double to single precision")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2265 functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$fmul-single-multiply     #x00    "WTL 2264 single precision multiply")
(defconstant $$fmul-double-multiply     #x01    "WTL 2264 double precision multiply")
(defconstant $$fmul-single-divide       #x40    "WTL 2264 single precision divide")
(defconstant $$fmul-double-divide       #x41    "WTL 2264 double precision divide")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2264/65 initial mode settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$fpu-mode-0               #x01    "Fast underflow, round nearest           (Mode  3- 0)")
(defconstant $$fpu-mode-1               #x10    "Disable pipes 2,3,4,D, fast accumulate  (Mode  7- 4)")
(defconstant $$fpu-mode-2               #x20    "Pipeline advance fast                   (Mode 11- 8)")
(defconstant $$fpu-mode-3               #x33    "2264/65 mode, single/double timing same (Mode 15-12)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; WTL 2264/65 status results - Exceptions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant $$fpu-exact-zero           #x0     "Result = +/- 0, exact")
(defconstant $$fpu-exact-infinity       #x1     "Result = +/- infinity, exact")
(defconstant $$fpu-finite-exact         #x2     "Result finite and non-zero, exact")
(defconstant $$fpu-finite-inexact       #x3     "Result finite and non-zero, inexact")
(defconstant $$fpu-overflow-inexact     #x5     "Overflow, inexact")
(defconstant $$fpu-underflow-exact      #x6     "Underflow, exact")
(defconstant $$fpu-underflow-inexact    #x7     "Underflow, inexact")
(defconstant $$fpu-x-denormalized       #x8     "Denormalized operand X")
(defconstant $$fpu-y-denormalized       #x9     "Denormalized operand Y")
(defconstant $$fpu-xy-denormalized      #xa     "Both operands denormalized")
(defconstant $$fpu-zero-divide          #xb     "Zero divide")
(defconstant $$fpu-X-NAN                #xc     "X operand not-a-number")
(defconstant $$fpu-Y-NAN                #xd     "Y operand not-a-number")
(defconstant $$fpu-XY-NAN               #xe     "X & Y operands not-a-number")
(defconstant $$fpu-invalid-operation    #xf     "Invalid operation requested")

(defconstant $$fpu-equal                #x0     "Compare X = Y")
(defconstant $$fpu-less-than            #x1     "Compare X < Y")
(defconstant $$fpu-greater-than         #x2     "Compare X > Y")
(defconstant $$fpu-unordered            #x3     "Can't compare these")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Floating point formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant %%short-float-mantissa (byte 17. 0))
(defconstant %%short-float-exponent (byte 8. 17.))
(defconstant $$short-float-exponent-excess 127.)     ;;exponent bias (i.e. substact 127 to get actual exponent.)
(defconstant $$short-float-exponent-not-a-number (global#:1+ (global#:* 2 $$short-float-exponent-excess)))
(defconstant %%short-float-sign (byte 1 25.))
(defconstant %%short-float-exponent-and-mantissa (byte 25. 0))

(defconstant %%single-float-mantissa (byte 23. 0))
(defconstant %%single-float-exponent (byte 8. 23.))
(defconstant $$single-float-exponent-excess 127.)    ;;exponent bias (i.e. substact 127 to get actual exponent.)
(defconstant $$single-float-exponent-not-a-number (global#:1+ (global#:* 2 $$single-float-exponent-excess)))
(defconstant %%single-float-sign (byte 1 31.))
(defconstant %%single-float-exponent-and-mantissa (byte 31. 0))

;(defconstant %%double-float-mantissa (byte 52. 0))     ;** loses due to byte spec lossage **
(defconstant %%double-float-exponent (byte 11. 52.))
(defconstant $$double-float-exponent-excess 1023.)   ;;exponent bias (i.e. substact 1023 to get actual exponent.)
(defconstant $$double-float-exponent-not-a-number (global#:1+ (global#:* 2 $$double-float-exponent-excess)))
(defconstant %%double-float-sign (byte 1 63.))
(defconstant %%double-float-mantissa-word1 (byte 32. 0))
(defconstant %%double-float-mantissa-word2 (byte 20. 0))
(defconstant %%double-float-exponent-word2 (byte 11. 20.))
(defconstant %%double-float-sign-word2 (byte 1 31.))
(defconstant %%double-float-exponent-and-mantissa-word1 (byte 32. 0))
(defconstant %%double-float-exponent-and-mantissa-word2 (byte 31. 0))
