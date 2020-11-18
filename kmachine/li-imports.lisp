;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10; compile-in-roots: ("GLOBAL");
;       LOAD-QFASL-IN-ROOTS: ("GLOBAL" "K-GLOBAL") -*-


;;; as far as I can tell, with the IMPORT and EXPORT commented out this file is obsolete
;;; I am therefore removing it from the SYSTEM file
;;;  --pfc 8/25


;;; everything in PRIMS

(lisp:defconstant li-symbols
  '(GLOBAL:NIL

    vinc:atom
    vinc:complexp
    vinc:arrayp
    vinc:compiled-function-p
    vinc:consp
    vinc:integerp
    vinc:rationalp
    vinc:floatp
    vinc:numberp
    vinc:listp
    vinc:symbolp
    vinc:commonp

     cons:caaaar
     cons:caaadr
     cons:caaar
     cons:caadar
     cons:caaddr
     cons:caadr
     cons:caar
     cons:cadaar
     cons:cadadr
     cons:cadar
     cons:caddar
     cons:cadddr
     cons:caddr
     cons:cadr
     cons:car
     cons:cdaaar
     cons:cdaadr
     cons:cdaar
     cons:cdadar
     cons:cdaddr
     cons:cdadr
     cons:cdar
     cons:cddaar
     cons:cddadr
     cons:cddar
     cons:cdddar
     cons:cddddr
     cons:cdddr
     cons:cddr
     cons:cdr
     cons:cons
     cons:endp
     cons:rplaca
     cons:rplacd


     ARRAY:ADJUSTABLE-ARRAY-P
     array:aref
     ARRAY:ARRAY-DIMENSION
     ARRAY:ARRAY-DIMENSION-LIMIT
     ARRAY:ARRAY-DIMENSIONS
     ARRAY:ARRAY-ELEMENT-TYPE
     ARRAY:ARRAY-HAS-FILL-POINTER-P
     ARRAY:ARRAY-IN-BOUNDS-P
     ARRAY:ARRAY-RANK
     ARRAY:ARRAY-RANK-LIMIT
     ARRAY:ARRAY-ROW-MAJOR-INDEX
     ARRAY:ARRAY-TOTAL-SIZE
     ARRAY:ARRAY-TOTAL-SIZE-LIMIT
     array:aset
     ARRAY:BIT-VECTOR-P
     ARRAY:FILL-POINTER
     array:length
     array:make-array
     ARRAY:MAKE-STRING
     ARRAY:SIMPLE-BIT-VECTOR-P
     ARRAY:SIMPLE-STRING-P
     ARRAY:SIMPLE-VECTOR-P
     ARRAY:STRING=
     ARRAY:STRINGP
     ARRAY:SVREF
     ARRAY:VECTOR-POP
     ARRAY:VECTOR-PUSH
     ARRAY:VECTOR-PUSH-EXTEND
     ARRAY:VECTORP
     array:vector

     symbol:boundp
     symbol:fboundp
     symbol:fmakunbound
     symbol:get
     symbol:get-properties
     symbol:getf
     symbol:make-symbol
     symbol:makunbound
     symbol:remf
     symbol:remprop
     symbol:set
     symbol:symbol-function
     symbol:symbol-name
     symbol:symbol-package
     symbol:symbol-plist
     symbol:symbol-value

     new-math:truncate
     new-math:floor
     new-math:ceiling
     new-math:round
     new-math:rem
     new-math:mod

     li:format
     li:expt
;     li:+
;     li:-
;     li:/
;     li:*
;     li:1+
;     li:1-
;     li:=
;     li:/=
;     li:<
;     li:>
;     li:<=
;     li:>=
;     li:min
;     li:max
;     li:evenp
;     li:oddp
;     li:conjgate
;     li:gcd
;     li:lcm
;     li:abs
;     li:signum
;     li:float
;     li:rational
;     li:rationalize
;     li:numerator
;     li:denominator
;     li:ffloor
;     li:fceiling
;     li:ftruncate
;     li:fround
;     li:decode-float
;     li:scale-float
;     li:float-radix
;     li:float-sign
;     li:float-digits
;     li:float-precision
;     li:integer-decode-float
;     li:realpart
;     li:imagpart
;     li:logior
;     li:logxor
;     li:logand
;     li:logeqv
;     li:lognand
;     li:logandc1
;     li:logandc2
;     li:logorc1
;     li:logorc2
;     li:boole
;     li:boole-clr
;     li:boole-set
;     li:boole-1
;     li:boole-2
;     li:boole-c1
;     li:boole-c2
;     li:boole-and
;     li:boole-ior
;     li:boole-xor
;     li:boole-eqv
;     li:boole-nand
;     li:boole-nor
;     li:boole-andc1
;     li:boole-andc2
;     li:boole-orc1
;     li:boole-orc2
;     li:lognot
;     li:logtest
;     li:logbitp
;     li:ash
;     li:logcount
;     li:integer-length
;     li:byte
;     li:byte-size
;     li:byte-position
;     li:ldb
;     li:dpb
;     li:ldb-test
;     li:mask-field
;     li:deposit-field


     ))

;(lisp:import li-symbols 'lisp-internals)  ;;wkf This seems unneeded
;(lisp:export li-symbols 'lisp-internals)  ;;     as does this, since no one uses it.
