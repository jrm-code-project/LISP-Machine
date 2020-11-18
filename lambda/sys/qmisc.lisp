;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-
;;;
;;; This is SYS: SYS; QMISC
;;;
;;; Miscellaneous functions not worthy of being in qfctns, or not able to be in the cold load.
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;; describe and room moved to sys;describe

(DEFUN DATA-TYPE (X)
  "Return the name for the data type of X."
  (AREF (SYMBOL-FUNCTION 'Q-DATA-TYPES) (%DATA-TYPE X)))

(DEFUN %POINTER-UNSIGNED (N)
  "Convert the fixnum N, regarded as unsigned number, into number (maybe big) with same value.
If the argument is negative (if regarded as signed), it is expanded into a bignum."
  ;; dpb makes positive version of most negitive fixnum
  (IF (MINUSP N) (+ N (* (DPB 1 %%Q-BOXED-SIGN-BIT 0) 2)) N))

(DEFUN %MAKE-POINTER-UNSIGNED (N)
  "Convert N to a fixnum which, regarded as unsigned, has same value as N.
Thus, a number just too big to be a signed fixnum
becomes a fixnum which, if regarded as signed, would be negative."
  (IF (FIXNUMP N) N
    (LOGIOR (LDB (byte (1- %%Q-POINTER) 0) N) (ROT (LDB (BYTE 1 (1- %%Q-POINTER)) N) -1))))

(DEFUN CALL (FN &REST ALTERNATES
                &AUX (MAX-ARGS #o100) (ARGS-INF (ARGS-INFO FN)))
  "The first argument is a function to call.
The remaining arguments are in pairs, consisting of a descriptor arg and a data arg.
The descriptor arg says what to do with the data arg.
The descriptor arg value should be either a keyword or a list of keywords or NIL.
NIL means that the data argument is to be treated as a single argument to the
 function.
The allowed keywords are :SPREAD and :OPTIONAL.
:SPREAD means that the data argument is a list of arguments
 rather than a single argument.
:OPTIONAL means that the data argument can be ignored if
 the function being called doesn't ask for it.
 After the first :OPTIONAL, all args supplied are considered optional."
    (AND (ZEROP (LDB %%ARG-DESC-QUOTED-REST ARGS-INF))
         (ZEROP (LDB %%ARG-DESC-EVALED-REST ARGS-INF))
         (SETQ MAX-ARGS (LDB %%ARG-DESC-MAX-ARGS ARGS-INF)))
    (%OPEN-CALL-BLOCK FN 0 4)
    (DO ((Y ALTERNATES (CDDR Y)) (OPTIONAL-FLAG) (SPREAD-FLAG NIL NIL))
        ((NULL Y))
      (IF (AND (SYMBOLP (CAR Y)) (CAR Y))
          (CASE (CAR Y)
            (:SPREAD (SETQ SPREAD-FLAG T))
            (:OPTIONAL (SETQ OPTIONAL-FLAG T))
            (OTHERWISE (FERROR "Invalid ~S keyword ~S." 'CALL (CAR Y))))
        (DOLIST (X (CAR Y))
          (CASE X
            (:SPREAD (SETQ SPREAD-FLAG T))
            (:OPTIONAL (SETQ OPTIONAL-FLAG T))
            (OTHERWISE (FERROR "Invalid ~S keyword ~S." 'CALL X)))))
      (AND OPTIONAL-FLAG ( MAX-ARGS 0)
           (RETURN NIL))
      (IF SPREAD-FLAG
          (DOLIST (X (CADR Y))
            (IF (AND OPTIONAL-FLAG ( MAX-ARGS 0))
                (RETURN NIL)
              (%ASSURE-PDL-ROOM 1)
              (%PUSH X)
              (DECF MAX-ARGS)))
        (%ASSURE-PDL-ROOM 1)
        (%PUSH (CADR Y))
        (DECF MAX-ARGS)))
    (%ACTIVATE-OPEN-CALL-BLOCK))

;;; isn't compatibility wonderful?
;(DEFVAR *RSET NIL)
;(DEFUN *RSET (&OPTIONAL (NEW-MODE T))
;  (SETQ *RSET NEW-MODE))

;;;@@@Since the following are pretty much no-ops, shouldn't they be made obsolete? --Keith 23-oct-88
(DEFUN LEXPR-FUNCALL-WITH-MAPPING-TABLE (FUNCTION &QUOTE TABLE &EVAL &REST ARGS)
  "Call FUNCTION like LEXPR-FUNCALL but provide mapping table TABLE.
If FUNCTION is a flavor method, this saves it from having to find
the correct flavor mapping table, but it will lose if you give the wrong one."
  (DECLARE (IGNORE TABLE))
  (APPLY #'LEXPR-FUNCALL FUNCTION ARGS))
(DEFF LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL 'LEXPR-FUNCALL-WITH-MAPPING-TABLE)

(DEFUN FUNCALL-WITH-MAPPING-TABLE (FUNCTION &QUOTE TABLE &EVAL &REST ARGS)
  "Call FUNCTION like FUNCALL but provide mapping table TABLE.
If FUNCTION is a flavor method, this saves it from having to find
the correct flavor mapping table, but it will lose if you give the wrong one."
  (DECLARE (IGNORE TABLE))
  (APPLY FUNCTION ARGS))
(DEFF FUNCALL-WITH-MAPPING-TABLE-INTERNAL 'FUNCALL-WITH-MAPPING-TABLE)

;;;; No, I don't know why these are here either

(defun area-name-p (x) (memq x (current-area-list)))
(deftype area-name () '(and symbol (satisfies area-name-p)))
(defprop area-name "a valid area name" si:type-name)
(defun area-number-p (number) (if (< -1 number (array-length #'area-name)) (area-name number)))
(deftype area () '(and fixnum (satisfies area-number-p)))
(defprop area "a valid area number" si:type-name)
(deftype area-number () '(and fixnum (satisfies area-number-p)))
(defprop area-number "a valid area number" si:type-name)

(DEFUN HAIPART (X N &AUX TEM)
  "Return N significant bits of the absolute value of X.
N > 0 means high N bits; N < 0 means low -N bits.
If X is too small, all of it is returned."
  ;; Get number of significant bits
  (SETQ TEM (HAULONG (SETQ X (ABS X))))
  ;; Positive N means get high N bits, or as many as there are
  (COND ((> N 0) (SETQ TEM (- N TEM))   ;minus number of low bits to discard
                 (COND ((< TEM 0) (ASH X TEM))
                       (T X)))
        ;; Zero N means return no bits
        ((= N 0) 0)
        ;; Negative N means get low -N bits, or as many as there are
        ((< (SETQ N (MINUS N)) TEM)
         (CL:REM X (ASH 1 N)))
        (T X)))

;;; These definitions work because the compiler open codes FLOOR, CEILING, TRUNCATE and ROUND.

(DEFUN FLOOR (DIVIDEND &OPTIONAL (DIVISOR 1))
  "Return DIVIDEND divided by DIVISOR, rounded down, and the remainder."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (FLOOR DIVIDEND DIVISOR))

(DEFUN CEILING (DIVIDEND &OPTIONAL (DIVISOR 1))
  "Return DIVIDEND divided by DIVISOR, rounded up, and the remainder."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (CEILING DIVIDEND DIVISOR))

(DEFUN TRUNCATE (DIVIDEND &OPTIONAL (DIVISOR 1))
  "Return DIVIDEND divided by DIVISOR, rounded toward zero, and the remainder."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (TRUNCATE DIVIDEND DIVISOR))

(DEFUN ROUND (DIVIDEND &OPTIONAL (DIVISOR 1))
  "Return DIVIDEND divided by DIVISOR, rounded to nearest integer, and the remainder."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (ROUND DIVIDEND DIVISOR))

;;;; Special floating arithmetic functions.

(DEFUN FLOAT (NUMBER &OPTIONAL OTHER)
  "Convert NUMBER to floating point, of same precision as OTHER.
If OTHER is omitted, a full size flonum is returned."
  (IF (SMALL-FLOATP OTHER)
      (SMALL-FLOAT NUMBER)
      (FLOAT NUMBER)))                          ;compiler hacks this

(DEFUN FFLOOR (DIVIDEND &OPTIONAL DIVISOR)
  "Like FLOOR but converts first value to a float."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
      (FLOOR DIVIDEND (OR DIVISOR 1))
    (VALUES (FLOAT QUOTIENT) REMAINDER)))

(DEFUN FCEILING (DIVIDEND &OPTIONAL DIVISOR)
  "Like CEILING but converts first value to a float."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
      (CEILING DIVIDEND (OR DIVISOR 1))
    (VALUES (FLOAT QUOTIENT) REMAINDER)))

(DEFUN FTRUNCATE (DIVIDEND &OPTIONAL DIVISOR)
  "Like TRUNCATE but converts first value to a float."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
      (TRUNCATE DIVIDEND (OR DIVISOR 1))
    (VALUES (FLOAT QUOTIENT) REMAINDER)))

(DEFUN FROUND (DIVIDEND &OPTIONAL DIVISOR)
  "Like ROUND but converts first value to a float."
  (DECLARE (VALUES QUOTIENT REMAINDER))
  (MULTIPLE-VALUE-BIND (QUOTIENT REMAINDER)
      (ROUND DIVIDEND (OR DIVISOR 1))
    (VALUES (FLOAT QUOTIENT) REMAINDER)))

(DEFUN FLOAT-RADIX (FLOAT)
  "Returns the radix of the exponent of a float.  That is always 2."
  (CHECK-TYPE FLOAT FLOAT)
  2)

(DEFUN FLOAT-DIGITS (FLOAT)
  "Returns the number of bits of fraction part FLOAT has.
This depends only on the data type of FLOAT (single-float vs short-float)."
  (TYPECASE FLOAT
    (SHORT-FLOAT SHORT-FLOAT-MANTISSA-LENGTH)
    (T           SINGLE-FLOAT-MANTISSA-LENGTH)))

(DEFUN FLOAT-PRECISION (FLOAT)
  "Returns the number of significant bits of fraction part FLOAT has.
For normalized arguments this is defined to be the same as FLOAT-DIGITS,
and all floats are normalized on the Lisp machine, so they are identical."
  (TYPECASE FLOAT
    (SHORT-FLOAT SHORT-FLOAT-MANTISSA-LENGTH)
    (T           SINGLE-FLOAT-MANTISSA-LENGTH)))

(DEFUN DECODE-FLOAT (FLOAT)
  "Returns three values describing the fraction part, exponent, and sign of FLOAT.
The first is a float between 1//2 and 1 (but zero if the arg is zero).
This value, times two to a suitable power, equals FLOAT except in sign.
The second value is an integer, the exponent of two needed for that calculation.
The third value is a float whose sign and type match FLOAT's
and whose magnitude is 1."
  (DECLARE (VALUES FRACTION-FLONUM EXPONENT SIGN-FLONUM))
  (VALUES (ABS (FLOAT-FRACTION FLOAT))
          (FLOAT-EXPONENT FLOAT)
          (IF (MINUSP FLOAT)
              (IF (SMALL-FLOATP FLOAT) -1.0s0 -1.0)
              (IF (SMALL-FLOATP FLOAT) 1.0s0 1.0))))

(DEFUN INTEGER-DECODE-FLOAT (FLOAT)
  "Returns three values describing the fraction part, exponent, and sign of FLOAT.
The first is an integer representing the fraction part of FLOAT.
This value floated, times two to a suitable power, equals FLOAT except in sign.
The second value is an integer, the exponent of two needed for that calculation.
The third value is a float whose sign and type match FLOAT's
and whose magnitude is 1."
  (DECLARE (VALUES FRACTION EXPONENT SIGN-FLONUM))
  (VALUES (FLONUM-MANTISSA (ABS FLOAT))
          (FLONUM-EXPONENT (ABS FLOAT))
          (IF (MINUSP FLOAT)
              (IF (SMALL-FLOATP FLOAT) -1.0s0 -1.0)
              (IF (SMALL-FLOATP FLOAT) 1.0s0 1.0))))

(DEFUN FLOAT-SIGN (SIGN-FLONUM &OPTIONAL MAGNITUDE-FLONUM)
  "Returns a float whose sign matches SIGN-FLONUM and magnitude matches MAGNITUDE-FLONUM.
If MAGNITUDE-FLONUM is omitted, it defaults to 1.
The type of float returned matches MAGNITUDE-FLONUM
if that is specified; else SIGN-FLONUM."
  (IF MAGNITUDE-FLONUM
      (IF (EQ (MINUSP SIGN-FLONUM) (MINUSP MAGNITUDE-FLONUM))
          MAGNITUDE-FLONUM (- MAGNITUDE-FLONUM))
    (IF (MINUSP SIGN-FLONUM)
        (IF (SMALL-FLOATP SIGN-FLONUM) -1.0s0 -1.0)
        (IF (SMALL-FLOATP SIGN-FLONUM) 1.0s0 1.0))))


(DEFUN COUNT-WIRED-PAGES ()
  (DECLARE (VALUES NUMBER-OF-WIRED-PAGES NUMBER-OF-FIXED-WIRED-PAGES))
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-PAGE-TABLE-SIZE)
                    2)
          (1- N))
       (N-WIRED 0))
      ((ZEROP N)
       (DO ((ADR (%REGION-ORIGIN PHYSICAL-PAGE-DATA) (1+ ADR))
            (N (TRUNCATE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE)
                         PAGE-SIZE)
               (1- N))
            (N-FIXED-WIRED 0))
           ((ZEROP N)
            (RETURN (VALUES (+ N-WIRED N-FIXED-WIRED) N-FIXED-WIRED)))
         (AND (= (%P-LDB (BYTE 16. 0.) ADR) #o177777)
              ( (%P-LDB (BYTE 16. 16.) ADR) #o177777)
              (SETQ N-FIXED-WIRED (1+ N-FIXED-WIRED)))))
    (AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
         (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED)
         (SETQ N-WIRED (1+ N-WIRED)))))

(DEFUN COUNT-DIRTY-PAGES ()
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-PAGE-TABLE-SIZE)
                    2)
          (1- N))
       (N-DIRTY 0))
      ((ZEROP N)
       N-DIRTY)
    (COND ((AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
                (NOT (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED))
                (OR (= (%P-LDB %%PHT1-MODIFIED-BIT ADR) 1)
                    (= (%P-LDB %%PHT2-MAP-STATUS-CODE (1+ ADR))
                       %PHT-MAP-STATUS-READ-WRITE)))
           (SETQ N-DIRTY (1+ N-DIRTY))))))

(DEFUN CLEAN-DIRTY-PAGES ()
  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
       (N (TRUNCATE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-PAGE-TABLE-SIZE)
                    2)
          (1- N))
       (N-DIRTY 0))
      ((ZEROP N)
       N-DIRTY)
    (COND ((AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
                (NOT (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED))
                (OR (= (%P-LDB %%PHT1-MODIFIED-BIT ADR) 1)
                    (= (%P-LDB %%PHT2-MAP-STATUS-CODE (1+ ADR))
                       %PHT-MAP-STATUS-READ-WRITE)))
           (SETQ N-DIRTY (1+ N-DIRTY))
           (REALLY-PAGE-OUT-PAGE (LSH (%P-LDB %%PHT1-VIRTUAL-PAGE-NUMBER ADR) 8.))))))



(DEFUN SET-MEMORY-SIZE (NEW-SIZE)
  "Specify how much main memory is to be used, in words.
/(By default, all the memory on the machine is used.)
This is mainly useful running benchmarks with different memory sizes.
If you specify more memory than is present on the machine,
memory board construction starts; in the meantime, the machine crashes."
  (PROG (OLD-SIZE NEWP OLDP)
        (UNLESS ( NEW-SIZE
                   (+ (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-WIRED-SIZE)
                      #o20000))                 ;8K min
          (FERROR "#o~O is smaller than wired + 8K"  NEW-SIZE))
     L  (SETQ OLD-SIZE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE))
        (SETQ OLDP (CEILING OLD-SIZE PAGE-SIZE))
        (SETQ NEWP (CEILING NEW-SIZE PAGE-SIZE))
        (COND ((OR (> NEWP (%REGION-LENGTH PHYSICAL-PAGE-DATA))
                   (> NEWP (TRUNCATE (* 4 (%REGION-LENGTH PAGE-TABLE-AREA)) 9.)))
               (FERROR "#o~O is bigger than page tables allow"  NEW-SIZE))
              ((= NEWP OLDP) (RETURN T))
              ((< NEWP OLDP) (GO FLUSH)))
     MORE
        (WHEN (%DELETE-PHYSICAL-PAGE OLD-SIZE)
          (PRINT (LIST OLD-SIZE "EXISTED")))
        (%CREATE-PHYSICAL-PAGE OLD-SIZE)
        (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE)
              (+ OLD-SIZE PAGE-SIZE))
        (GO L)

     FLUSH
        (WHEN (NULL (%DELETE-PHYSICAL-PAGE (- OLD-SIZE PAGE-SIZE)))
          (PRINT (LIST (- OLD-SIZE PAGE-SIZE) "DID-NOT-EXIST")))
        (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE)
              (- OLD-SIZE PAGE-SIZE))
        (GO L)))

(DEFUN SET-ERROR-MODE (&OPTIONAL (CAR-SYM-MODE 1) (CDR-SYM-MODE 1)
                                 (CAR-NUM-MODE 0) (CDR-NUM-MODE 0))
  "Sets the error mode.  For the arguments 0 means /"an error/" and 1
means /"NIL/". "
  (SETQ %MODE-FLAGS (%LOGDPB CAR-SYM-MODE %%M-FLAGS-CAR-SYM-MODE %MODE-FLAGS))
  (SETQ %MODE-FLAGS (%LOGDPB CDR-SYM-MODE %%M-FLAGS-CDR-SYM-MODE %MODE-FLAGS))
  (SETQ %MODE-FLAGS (%LOGDPB CAR-NUM-MODE %%M-FLAGS-CAR-NUM-MODE %MODE-FLAGS))
  (SETQ %MODE-FLAGS (%LOGDPB CDR-NUM-MODE %%M-FLAGS-CDR-NUM-MODE %MODE-FLAGS)))



;;;; Apropos

(DEFUN APROPOS-LIST (SUBSTRING &OPTIONAL PKG)
  "Return a list of symbols whose names contain SUBSTRING, but don't print anything.
Like APROPOS with :DONT-PRINT specified as non-NIL."
  (APROPOS SUBSTRING PKG :DONT-PRINT T))

;; Copied from LAD: RELEASE-3.SYS; QMISC.LISP#723 on 2-Oct-86 04:50:15
(DEFUN APROPOS (SUBSTRING
                &OPTIONAL (PKG *ALL-PACKAGES*)
                &KEY (INHERITORS NIL) (INHERITED T) DONT-PRINT PREDICATE BOUNDP FBOUNDP (no-duplicates t))
  "Find all symbols in one or more packages whose names contain SUBSTRING, or
containing each string in it, if SUBSTRING is a list of strings.
If PREDICATE is non-NIL, it is a function to be called with a symbol as arg;
only symbols for which the predicate returns non-NIL will be mentioned.
If BOUNDP is non-NIL, then only bound symbols are mentioned. Likewise FBOUNDP.
The :PACKAGE argument defaults to NIL, meaning do all packages.
The packages which USE that package are processed also, unless :INHERITORS is NIL.
The packages USEd by that package are processed also, unless :INHERITED is NIL.
/(Any other packages which inherit from them also are NOT processed in any case.)
The :NO-DUPLICATES argument will prevent checking for duplicates when NIL;
this speeds things up alot.
The symbols are printed unless DONT-PRINT is set.
A list of the symbols found is returned."
  (LET (RETURN-LIST
        (APROPOS-PREDICATE PREDICATE)
        (APROPOS-DONT-PRINT DONT-PRINT)
        (APROPOS-SUBSTRING SUBSTRING)
        (APROPOS-BOUNDP BOUNDP)
        (APROPOS-FBOUNDP FBOUNDP)
        (apropos-no-duplicates no-duplicates)
        APROPOS-PACKAGES)
    (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-SUBSTRING APROPOS-DONT-PRINT
                      APROPOS-BOUNDP APROPOS-FBOUNDP apropos-no-duplicates))
    (IF (NULL PKG) (SETQ PKG *ALL-PACKAGES*))
    (IF (NOT (CLI:LISTP PKG)) (SETQ PKG (LIST PKG)))
    (DOLIST (P PKG)
      (SETQ P (or (FIND-PACKAGE P)
                  (ferror nil "No such package named: ~A" p)))
      (PUSHNEW P APROPOS-PACKAGES)
      (WHEN INHERITED
        (DOLIST (U (PKG-USE-LIST P))
          (PUSHNEW U APROPOS-PACKAGES)))
      (WHEN INHERITORS
        (DOLIST (U (PKG-USED-BY-LIST P))
          (PUSHNEW U APROPOS-PACKAGES))))
    (DOLIST (P (REVERSE APROPOS-PACKAGES))
      (DO-LOCAL-SYMBOLS (SYMBOL P)
        (APROPOS-1 SYMBOL)))
    RETURN-LIST))

;; Copied from LAD: RELEASE-3.SYS; QMISC.LISP#723 on 2-Oct-86 04:50:16
(DEFUN APROPOS-1 (SYMBOL &AUX (P (SYMBOL-NAME SYMBOL)))
  (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-SUBSTRING
                    APROPOS-DONT-PRINT APROPOS-BOUNDP APROPOS-FBOUNDP apropos-no-duplicates))
  (COND ((AND (IF (CONSP APROPOS-SUBSTRING)
                  (DOLIST (S APROPOS-SUBSTRING T)
                    (UNLESS (STRING-SEARCH S P) (RETURN NIL)))
                (STRING-SEARCH APROPOS-SUBSTRING P))
              (OR (NOT APROPOS-BOUNDP) (BOUNDP SYMBOL))
              (OR (NOT APROPOS-FBOUNDP) (FBOUNDP SYMBOL))
              ;;if you don't care about duplicates don't do
              ;;the time-consuming memq
              (or (not apropos-no-duplicates) (NOT (MEMQ SYMBOL RETURN-LIST)))
              (OR (NULL APROPOS-PREDICATE)
                  (FUNCALL APROPOS-PREDICATE SYMBOL)))
         (PUSH SYMBOL RETURN-LIST)
         (OR APROPOS-DONT-PRINT
             (PROGN
               ;; Binding the package to NIL forces the package to be printed.
               ;; This is better than explicitly printing the package, because
               ;; this way you get the "short" version.
               (LET ((*PACKAGE* NIL))
                 (FORMAT T "~%~S" SYMBOL))
               (AND (FBOUNDP SYMBOL)
                    (FORMAT T " - Function ~:S" (ARGLIST SYMBOL)))
               (AND (BOUNDP SYMBOL)
                    (COND ((FBOUNDP SYMBOL) (PRINC ", Bound"))
                          (T (PRINC " - Bound"))))
               (AND (GET SYMBOL 'FLAVOR)
                    (COND ((OR (BOUNDP SYMBOL) (FBOUNDP SYMBOL))
                           (PRINC ", Flavor"))
                          (T (PRINC " - Flavor")))))))))

(DEFUN SUB-APROPOS (SUBSTRING STARTING-LIST &KEY PREDICATE BOUNDP FBOUNDP DONT-PRINT (no-duplicates t))
  "Find all symbols in STARTING-LIST whose names contain SUBSTRING, or
containing each string in it, if SUBSTRING is a list of strings.
IF PREDICATE is supplied, it should be a function of one arg;
only symbols for which the predicate returns non-NIL are included.
If BOUNDP is non-NIL, then only bound symbols are included. Similarly with FBOUNDP.
The symbols are printed unless DONT-PRINT is non-NIL.
A list of the symbols found is returned."
  (LET (RETURN-LIST
        (APROPOS-PREDICATE PREDICATE)
        (APROPOS-SUBSTRING SUBSTRING)
        (APROPOS-BOUNDP BOUNDP)
        (APROPOS-FBOUNDP FBOUNDP)
        (APROPOS-DONT-PRINT DONT-PRINT)
        (apropos-no-duplicates no-duplicates))
    (DECLARE (SPECIAL RETURN-LIST APROPOS-PREDICATE APROPOS-BOUNDP APROPOS-FBOUNDP
                      APROPOS-SUBSTRING APROPOS-DONT-PRINT apropos-no-duplicates))
    (MAPC #'APROPOS-1 STARTING-LIST)
    RETURN-LIST))

;;;; Closure stuff

(DEFUN SYMEVAL-IN-CLOSURE (CLOSURE PTR)
  "Return the value which the symbol or value cell locative PTR has in CLOSURE.
More precisely, the value which is visible within CLOSURE is returned.
If CLOSURE does not contain a binding for it, the current value is returned."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (ETYPECASE PTR
    (SYMBOL (SETQ PTR (LOCF (SYMBOL-VALUE PTR))))
    (LOCATIVE))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (CAR PTR))
    (WHEN (EQ (CAR L) PTR)
      (RETURN (CAADR L)))))

(DEFUN BOUNDP-IN-CLOSURE (CLOSURE PTR &AUX PTR1)
  "T if the symbol or value cell locative PTR is BOUNDP within CLOSURE.
More precisely, the binding which is visible within CLOSURE is tested.
If CLOSURE does not contain a binding for it, the current binding is tested."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (ETYPECASE PTR
    (SYMBOL (SETQ PTR (LOCF (SYMBOL-VALUE PTR))))
    (LOCATIVE))
  (SETQ PTR1 (IF (SYMBOLP PTR) (LOCF (SYMBOL-VALUE PTR)) PTR))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (LOCATION-BOUNDP PTR1))
    (AND (EQ (CAR L) PTR1)
         (RETURN (LOCATION-BOUNDP (CADR L))))))

(DEFUN MAKUNBOUND-IN-CLOSURE (CLOSURE PTR &AUX PTR1)
  "Make the symbol or value cell locative PTR unbound in CLOSURE.
More precisely, the binding which is visible within CLOSURE is made unbound.
If CLOSURE does not contain a binding for it, the current binding is made unbound."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (SETQ PTR1 (ETYPECASE PTR
               (SYMBOL (LOCF (SYMBOL-VALUE PTR)))
               (LOCATIVE PTR)))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (IF (SYMBOLP PTR)
           (MAKUNBOUND PTR)
           (LOCATION-MAKUNBOUND PTR)))
    (IF (EQ (CAR L) PTR1)
        (RETURN (LOCATION-MAKUNBOUND (CADR L)))))
  NIL)

(DEFUN LOCATE-IN-CLOSURE (CLOSURE PTR)
  "Return the location of the value which the symbol or value cell locative PTR has in CLOSURE.
More precisely, the location of the binding visible within CLOSURE is returned.
If CLOSURE does not contain a binding for it, the value cell
locative itself, or the symbol's value cell location, is returned."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (ETYPECASE PTR
    (SYMBOL (SETQ PTR (LOCF (SYMBOL-VALUE PTR))))
    (LOCATIVE))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       PTR)
    (AND (EQ (CAR L) PTR)
         (RETURN (CADR L)))))

(DEFUN SET-IN-CLOSURE (CLOSURE PTR VAL)
  "Set the value which the symbol or value cell locative PTR has in CLOSURE to VAL.
More precisely, the binding which is visible within CLOSURE is set.
If CLOSURE does not contain a binding for it, the current binding is set."
  (CHECK-TYPE CLOSURE (OR CLOSURE ENTITY))
  (ETYPECASE PTR
    (SYMBOL (SETQ PTR (LOCF (SYMBOL-VALUE PTR))))
    (LOCATIVE))
  (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L)))
      ((NULL L)
       (SETF (CONTENTS PTR) VAL))
    (IF (EQ (CAR L) PTR)
        (RETURN (SETF (CONTENTS (CADR L)) VAL)))))

;;;; Here are some random functions for poking around in ENTITYs and CLOSUREs.

(DEFUN CLOSURE-VARIABLES (CLOSURE)
  "Return a list of variables closed over by CLOSURE."
  (CHECK-TYPE CLOSURE (OR ENTITY CLOSURE))
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
           (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      '(LEXICAL-ENVIRONMENT)
    (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L))
         (ANS NIL (CONS (%MAKE-POINTER-OFFSET DTP-SYMBOL (CAR L) -1) ANS)))
        ((NULL L) ANS))))

(DEFUN CLOSURE-ALIST (CLOSURE)
  "Return an alist of variables closed over by CLOSURE vs their values they have inside it.
If one of the variables is unbound in the closure,
the corresponding cdr in the alist will also be a DTP-NULL.
Storing into the alist cdr's does not affect the values in the closure."
  (CHECK-TYPE CLOSURE (OR ENTITY CLOSURE))
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
           (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      `((LEXICAL-ENVIRONMENT . ,(CADR (%MAKE-POINTER DTP-LIST CLOSURE))))
    (DO ((L (CDR (%MAKE-POINTER DTP-LIST CLOSURE)) (CDDR L))
         (ANS))
        ((NULL L) ANS)
      ;; value-cell is offset 1 from symbol pointer
      (PUSH (CONS (%MAKE-POINTER-OFFSET DTP-SYMBOL (CAR L) -1) NIL) ANS)
      ;; Copy (CAADR L) into (CDAR ANS)
      (%BLT-TYPED (CADR L) (CDR-LOCATION-FORCE (CAR ANS)) 1 1))))

(DEFUN CLOSURE-FUNCTION (CLOSURE)
  "Return the function closed over in CLOSURE."
  (CHECK-TYPE CLOSURE (OR ENTITY CLOSURE))
  (CAR (%MAKE-POINTER DTP-LIST CLOSURE)))

(defun store-closure-function (closure new-function)
  (check-type closure closure)
  (setf (car (%make-pointer dtp-list closure)) new-function))

(defsetf closure-function store-closure-function)

(DEFUN CLOSURE-BINDINGS (CLOSURE)
  "Return the bindings of CLOSURE, shared with CLOSURE.
This is suitable for use in SYS:%USING-BINDING-INSTANCES."
  (CHECK-TYPE CLOSURE (OR ENTITY CLOSURE))
  (CDR (%MAKE-POINTER DTP-LIST CLOSURE)))

(defun store-closure-bindings (closure new-bindings)
  (check-type closure closure)
  (setf (cdr (%make-pointer dtp-list closure)) new-bindings))

(defsetf closure-bindings store-closure-bindings)

(DEFUN COPY-CLOSURE (CLOSURE &AUX CLOSURE1)
  "Return a new closure with the same function, variables and initial values as CLOSURE.
However, the new and old closures do not share the same external value cells."
  (CHECK-TYPE CLOSURE (OR ENTITY CLOSURE))
  (SETQ CLOSURE1 (%MAKE-POINTER DTP-LIST CLOSURE))
  (IF (AND (CDR (%MAKE-POINTER DTP-LIST CLOSURE))
           (NULL (CDDR (%MAKE-POINTER DTP-LIST CLOSURE))))
      (%MAKE-POINTER DTP-CLOSURE (COPY-LIST CLOSURE1))
    (LET ((ANS (MAKE-LIST (LENGTH CLOSURE1))))
      (SETF (CAR ANS) (CAR CLOSURE1))           ;Close over same fctn
      (DO ((L (CDR CLOSURE1) (CDDR L))
           (N (CDR ANS) (CDDR N)))
          ((NULL L) (%MAKE-POINTER (%DATA-TYPE CLOSURE) ANS))
        (SETF (CAR N) (CAR L))                  ;Same internal value cell
        (LET ((NEW-EXVC (MAKE-LIST 1)))
          (IF (NOT (LOCATION-BOUNDP (CADR L)))
              (LOCATION-MAKUNBOUND NEW-EXVC)
            (SETF (CAR NEW-EXVC) (CAR (CADR L))))
          (SETF (CADR N) NEW-EXVC))))))

;;;; Arrayoid functions

(DEFVAR ARRAY-ORDER-INITIALIZATION-LIST NIL
  "Initialization list run after changing the value of ARRAY-INDEX-ORDER.")

(DEFUN MAKE-PIXEL-ARRAY (WIDTH HEIGHT &REST OPTIONS)
  "Make a pixel array of WIDTH by HEIGHT.  You must specify :TYPE as in MAKE-ARRAY.
This will create an array of the apropriate shape and knows whether
the height is supposed to be the first dimension or the second.
Access the resulting array with AR-2-REVERSE and AS-2-REVERSE to make sure
that accessing also is independent of array dimension order."
;>> Bogus.  Mly
; No, it's not.  It improves paging performance significantly by putting
; all sheet bit-arrays in sheet-area, which has a high swap quantum.  KHS.
;>>  Yes it is.  Not all `pixel' arrays are for sheets.  This function was not created for
;  the use of the window system.  If sheets want to cons bit-arrays in the right place, it
;  is up to them to do so themselves.  (that's what the &rest options avove is about)
;  The most horrible thing about this lossage is that the :area tv:sheet-area is OVERRIDING
;  any area specification that the caller may specify by way of options (never minding the
;  fact that make-array has a bug in that it groks its &key arguments from right to left
;  which hides this lossage in some cases.
; Does that mean you volunteer to find all the places in the window system where sheets are
; made and make them use some other function?  Putting sheet bit-arrays in working storage
; are is a monstrously bad idea, for both the GC and the paging system.
;>>  NOT ALL ARRAYS DEPENDING ON ROW-MAJOR-INDEX ARE SHEET BIT-ARRAYS!
;  FIX THE CALLERS!!!!
;>> (I fixed all the callers...)
;  (if (boundp 'tv:sheet-area)
;      (APPLY #'MAKE-ARRAY (LIST HEIGHT WIDTH) :area tv:sheet-area OPTIONS)
    (APPLY #'MAKE-ARRAY (LIST HEIGHT WIDTH) OPTIONS))

(DEFUN PIXEL-ARRAY-WIDTH (ARRAY)
  "Return the width in pixels of an array of pixels.
The width is the dimension which varies more faster."
  (ARRAY-DIMENSION ARRAY 1))

(DEFUN PIXEL-ARRAY-HEIGHT (ARRAY)
  "Return the height in pixels of an array of pixels.
The height is the dimension which varies more slowly."
  (ARRAY-DIMENSION ARRAY 0))

;;; VECTOR-POP, eventually to be micro-coded (ha ha)
;;; undoes (ARRAY-PUSH <ARRAY> <DATA>) and returns <DATA>
(DEFUN VECTOR-POP (ARRAY &OPTIONAL (DEFAULT NIL DEFAULTP))
  "Returns the last used element of ARRAY, and decrements the fill pointer.
For an ART-Q-LIST array, the cdr codes are updated so that the overlayed list
no longer contains the element removed. Signals an error if ARRAY is empty
/(has fill-pointer 0) unless DEFAULT is supplied, in which case DEFAULT is returned instead.
Uses CL:AREF, so will pop character objects out of strings."
  (WITHOUT-INTERRUPTS
    (LET* ((INDEX (1- (FILL-POINTER ARRAY)))    ;1- because fill-pointer is # active elements
           (ARRAY-TYPE (AREF (SYMBOL-FUNCTION 'ARRAY-TYPES)
                             (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
           VAL)
      (COND ((NOT (MINUSP INDEX))
             (SETQ VAL (CLI:AREF ARRAY INDEX))
             (SETF (FILL-POINTER ARRAY) INDEX)
             (WHEN (AND (EQ ARRAY-TYPE 'ART-Q-LIST)
                        (NOT (ZEROP INDEX)))
               (%P-DPB CDR-NIL %%Q-CDR-CODE (LOCF (AREF ARRAY (1- INDEX)))))
             VAL)
          (DEFAULTP
           DEFAULT)
          (T
           (FERROR "~S Overpopped" ARRAY))))))

(DEFUN ARRAY-POP (ARRAY &OPTIONAL (DEFAULT NIL DEFAULTP))
  "Returns the last used element of ARRAY, and decrements the fill pointer.
For an ART-Q-LIST array, the cdr codes are updated so that the overlayed list
no longer contains the element removed. Signals an error if ARRAY is empty
/(has fill-pointer 0) unless DEFAULT is supplied, in which case DEFAULT is returned instead.
Uses ZL:AREF, so will pop fixnums out of strings."
  (WITHOUT-INTERRUPTS
    (LET* ((INDEX (1- (FILL-POINTER ARRAY)))    ;1- because fill-pointer is # active elements
           (ARRAY-TYPE (AREF (SYMBOL-FUNCTION 'ARRAY-TYPES)
                             (%P-LDB-OFFSET %%ARRAY-TYPE-FIELD ARRAY 0)))
           VAL)
      (COND ((NOT (MINUSP INDEX))
             (SETQ VAL (ZL:AREF ARRAY INDEX))
             (SETF (FILL-POINTER ARRAY) INDEX)
             (WHEN (AND (EQ ARRAY-TYPE 'ART-Q-LIST)
                        (NOT (ZEROP INDEX)))
               (%P-DPB CDR-NIL %%Q-CDR-CODE (LOCF (AREF ARRAY (1- INDEX)))))
             VAL)
          (DEFAULTP
           DEFAULT)
          (T
           (FERROR "~S Overpopped" ARRAY))))))

;;; weird brand s function --- here to be compatible, I guess
(defun array-push-portion-extend (to-vector from-vector &optional (from-start 0) from-end)
  "Concatenate the data in FROM-VECTOR between indices FROM-START and FROM-END
to the end of TO-VECTOR, making TO-VECTOR larger if needed.  TO-VECTOR must
have a fill pointer.  FROM-START and FROM-END default to specifying the whole
of FROM-VECTOR.
Returns (possibly enlarged) TO-VECTOR and TO-VECTOR's new fill-pointer."
  (declare (values to-vector to-vector-end))
  (check-type from-vector vector)
  (check-type to-vector (and vector (satisfies array-has-fill-pointer-p)))
  (setq from-end (or from-end (length from-vector)))
  (let* ((portion-length (- from-end from-start))
         (to-start (fill-pointer to-vector))
         (to-end (+ to-start portion-length)))
    (cond ((= portion-length 0))
          ((< portion-length 0)
           (ferror "Attempting to copy ~S elements from ~S to ~S of ~S"
                   portion-length from-start from-end from-vector))
          (t
           (when (> to-end (array-length to-vector))    ;need to grow.
             (setq to-vector (adjust-array-size to-vector
                               (+ (array-length to-vector)
                                  (cond ((< (%structure-total-size to-vector) page-size)
                                         (max (array-length to-vector) #o100))
                                        (t (truncate (array-length to-vector) 4)))))))
           (copy-array-portion from-vector from-start from-end
                               to-vector to-start to-end)
           (setf (fill-pointer to-vector) to-end)
           (when (eq (array-type to-vector) 'art-q-list)
             ;; set cdr-codes of middle elts to CDR-NEXT
             (loop for idx from (max 0 (1- to-start)) below (1- to-end)
                do (setf (%p-cdr-code (locf (aref to-vector idx))) cdr-next)
                ;; set last cdr code to CDR-NIL
                finally
                  (%p-store-cdr-code (locf (aref to-vector (1- to-end))) cdr-nil)))))
    (values to-vector to-end)))

(DEFUN CHANGE-INDIRECT-ARRAY (ARRAY TYPE DIMLIST DISPLACEDP INDEX-OFFSET
                              &AUX INDEX-LENGTH NDIMS INDIRECT-LENGTH TEM
                                   OLD-NDIMS OLD-INDIRECT-LENGTH)
  "Change an indirect array ARRAY's type, size, or target pointed at.
TYPE specifies the new array type, DIMLIST its new dimensions,
DISPLACEDP the target it should point to (array, locative or fixnum),
INDEX-OFFSET the new offset in the new target."
  (CHECK-TYPE ARRAY (AND ARRAY (SATISFIES ARRAY-DISPLACED-P)) "a displaced array")
  (CHECK-TYPE DISPLACEDP (OR ARRAY INTEGER LOCATIVE)
              "an array or physical address to which to indirect")
  (CHECK-ARG TYPE                               ;TEM gets the numeric array type
             (SETQ TEM (COND ((NUMBERP TYPE) (LDB %%ARRAY-TYPE-FIELD TYPE))
                             ((FIND-POSITION-IN-LIST TYPE ARRAY-TYPES))))
             "an array type")
  (SETQ TYPE TEM)
  (IF (NOT (CONSP DIMLIST))
;>> ** BRAINDAMAGE ALERT **
      (SETQ NDIMS 1 INDEX-LENGTH (EVAL DIMLIST))
    (SETQ NDIMS (LENGTH DIMLIST)
          INDEX-LENGTH (LIST-PRODUCT DIMLIST)))
  (SETQ INDIRECT-LENGTH (IF INDEX-OFFSET 3 2)
        OLD-NDIMS (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0)
        OLD-INDIRECT-LENGTH (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))
  (OR (= NDIMS OLD-NDIMS)
      (FERROR "Attempt to change the number of dimensions from ~D to ~D."
              OLD-NDIMS NDIMS))
  (OR (= INDIRECT-LENGTH OLD-INDIRECT-LENGTH)
      (FERROR "Attempt to add or remove index-offset."))
  (%P-DPB-OFFSET TYPE %%ARRAY-TYPE-FIELD ARRAY 0)
  (AND ARRAY-INDEX-ORDER
       (CONSP DIMLIST)
       (SETQ DIMLIST (REVERSE DIMLIST)))
  (AND (CONSP DIMLIST)
       (DO ((I 1 (1+ I))
            (N NDIMS (1- N)))
           ((< N 2))
;>> ** BRAINDAMAGE ALERT **
         (%P-STORE-CONTENTS-OFFSET (EVAL (CAR DIMLIST)) ARRAY I)
         (SETQ DIMLIST (CDR DIMLIST))))
  (%P-STORE-CONTENTS-OFFSET DISPLACEDP ARRAY NDIMS)
  (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ NDIMS))
  (WHEN INDEX-OFFSET
    (%P-STORE-CONTENTS-OFFSET INDEX-OFFSET ARRAY (+ NDIMS 2)))
  (invalidate-array-cache)
  ARRAY)

(DEFUN LIST-ARRAY-LEADER (ARRAY &OPTIONAL LIMIT)
  "Return a list of the contents of ARRAY's leader, up to LIMIT."
;  (IF (AND (SYMBOLP ARRAY)
;          (FBOUNDP ARRAY)
;          (ARRAYP (SYMBOL-FUNCTION ARRAY)))
;      (SETQ ARRAY (SYMBOL-FUNCTION ARRAY)))
  (CHECK-TYPE ARRAY ARRAY)
  (IF (NULL LIMIT)
      (SETQ LIMIT (OR (ARRAY-LEADER-LENGTH ARRAY) 0)))
  (LET ((LIST (MAKE-LIST LIMIT)))
    (DO ((I 0 (1+ I))
         (L LIST (CDR L)))
        (( I LIMIT)
         LIST)
      (SETF (CAR L) (ARRAY-LEADER ARRAY I)))))

(DEFF ARRAY-/#-DIMS 'ARRAY-RANK)
(COMPILER:MAKE-OBSOLETE ARRAY-/#-DIMS "use ARRAY-RANK")

(COMPILER:MAKE-OBSOLETE ARRAY-DIMENSION-N
                        "use ARRAY-DIMENSION (with a different calling sequence)")
(DEFUN ARRAY-DIMENSION-N (N ARRAY)
  "Return the length of dimension N of ARRAY.  The first dimension is N=1.
If N is 0, the leader length is returned.  Use ARRAY-LEADER-LENGTH instead."
  (CHECK-TYPE ARRAY ARRAY)
  (COND ((> N (ARRAY-RANK ARRAY))
         NIL)
        ((NOT (PLUSP N))
         (ARRAY-LEADER-LENGTH ARRAY))
        (T
         (ARRAY-DIMENSION ARRAY (1- N)))))

;;microcoded now.
;(DEFUN ARRAY-RANK (ARRAY)
;  "Return the number of dimensions ARRAY has."
;  (CHECK-ARG ARRAY ARRAYP "an array")
;  (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))


;;;; Crufty FILLARRAY and LISTARRAY

;;; The following definitions of FILLARRAY and LISTARRAY should be completely
;;; compatible with Maclisp.  Slow, maybe, but compatible.

;;; When filling from an array, extra elements in the destination get the default initial
;;; value for the array type.  When filling from a list it sticks at the last element.
;;; Extra elements in the source are ignored.  copy-array-contents
;;; does the right thing for one-d arrays, but for multi-dimensional arrays
;;; uses column-major rather than row-major order.

;;>> These should be in sys2;macarray, except that things in the system use them!! SIGH!

(DEFRESOURCE FILLARRAY-INDEX-ARRAYS ()
  "Resource of vectors used by FILLARRAY and LISTARRAY"
  :CONSTRUCTOR (MAKE-ARRAY 8.)
  :INITIAL-COPIES 2)

(DEFUN FILLARRAY (ARRAY SOURCE)
  "Fill the contents of ARRAY from SOURCE.
If SOURCE is a list, its last element is repeated to fill any part of ARRAY left over.
If SOURCE is an array, elements of ARRAY not filled by SOURCE are left untouched.
If SOURCE is NIL, the array is filled with the default type for the array; this is 0 or NIL.
If ARRAY is NIL, a new list as big as SOURCE is created."
  (LET ((ARRAY (COND ((NULL ARRAY)
                     (SETQ ARRAY
                           (MAKE-ARRAY
                             (COND ((NULL SOURCE) 0)
                                   ((CONSP SOURCE) (LENGTH SOURCE))
                                   ((ARRAYP SOURCE) (ARRAY-DIMENSIONS SOURCE))
                                   (T (FERROR "Unable to default destination array"))))))
                    ((AND (SYMBOLP ARRAY)
                          (FBOUNDP ARRAY)
                          (ARRAYP (SYMBOL-FUNCTION ARRAY)))
                     (SYMBOL-FUNCTION ARRAY))
                    (T ARRAY))))
    (CHECK-TYPE ARRAY ARRAY)
    (CHECK-TYPE SOURCE (OR ARRAY LIST))
    (LET ((DEST-NDIMS (ARRAY-RANK ARRAY))
          (SOURCE-IS-AN-ARRAY-P (ARRAYP SOURCE)))
      (COND (SOURCE-IS-AN-ARRAY-P
             (LET ((SOURCE-NDIMS (ARRAY-RANK SOURCE)))
               (COND ((AND (= DEST-NDIMS 1)
                           (= SOURCE-NDIMS 1))
                      ;; 1d array into a 1d array is in microcode!
                      (LET ((N-ELEMENTS (MIN (ARRAY-LENGTH SOURCE)
                                             (ARRAY-LENGTH ARRAY))))
                        (COPY-ARRAY-PORTION SOURCE 0 N-ELEMENTS ARRAY 0 N-ELEMENTS)))
                     (T
                      ;; Hairy case, some array is multi-dimensional.
                      (USING-RESOURCE (SOURCE-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
                        (USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
                          (DOTIMES (I 10)
                            (SETF (AREF SOURCE-INDEX-ARRAY I) 0)
                            (SETF (AREF DEST-INDEX-ARRAY I) 0))
                          (LET ((SOURCE-ELEMENTS (ARRAY-LENGTH SOURCE))
                                (DEST-ELEMENTS (ARRAY-LENGTH ARRAY)))
                            (DOTIMES (I (MIN SOURCE-ELEMENTS DEST-ELEMENTS))
                              (FILLARRAY-PUT (FILLARRAY-GET SOURCE
                                                            SOURCE-INDEX-ARRAY
                                                            SOURCE-NDIMS)
                                             ARRAY DEST-INDEX-ARRAY DEST-NDIMS)))))))))
            ((NULL SOURCE) (COPY-ARRAY-PORTION ARRAY 0 0 ARRAY 0 (ARRAY-LENGTH ARRAY)))
            (T
             ;; Source is a list.
             (COND ((= DEST-NDIMS 1)
                    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
                      (SETF (AREF ARRAY X) (CAR SOURCE))
                      (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))
                   ((= DEST-NDIMS 2)
                    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
                      (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
                        (SETF (AREF ARRAY X Y) (CAR SOURCE))
                        (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE))))))
                   ((= DEST-NDIMS 3)
                    (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
                      (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
                        (DOTIMES (Z (ARRAY-DIMENSION ARRAY 2))
                          (SETF (AREF ARRAY X Y Z) (CAR SOURCE))
                          (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE)))))))
                   (T
                    (USING-RESOURCE (DEST-INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
                      (DOTIMES (I 8.)
                        (SETF (AREF DEST-INDEX-ARRAY I) 0))
                      (DOTIMES (I (ARRAY-LENGTH ARRAY))
                        (FILLARRAY-PUT (CAR SOURCE) ARRAY DEST-INDEX-ARRAY DEST-NDIMS)
                        (IF (NOT (NULL (CDR SOURCE))) (SETQ SOURCE (CDR SOURCE))))))))))
    ARRAY))

(DEFUN FILLARRAY-GET (ARRAY INDEX-ARRAY NDIMS)
  (%OPEN-CALL-BLOCK ARRAY 0 1)                  ;d-stack
  (%ASSURE-PDL-ROOM NDIMS)
  (DOTIMES (I NDIMS)
    (%PUSH (ZL:AREF INDEX-ARRAY I)))
  (%ACTIVATE-OPEN-CALL-BLOCK)
  (FILLARRAY-INCREMENT-INDEX ARRAY INDEX-ARRAY NDIMS)
  (%POP))

(DEFUN FILLARRAY-PUT (VALUE ARRAY INDEX-ARRAY NDIMS)
  (%OPEN-CALL-BLOCK #'ASET 0 0)                 ;d-ignore
  (%ASSURE-PDL-ROOM (+ 2 NDIMS))
  (%PUSH VALUE)
  (%PUSH ARRAY)
  (DOTIMES (I NDIMS)
    (%PUSH (ZL:AREF INDEX-ARRAY I)))
  (%ACTIVATE-OPEN-CALL-BLOCK)
  (FILLARRAY-INCREMENT-INDEX ARRAY INDEX-ARRAY NDIMS))

(DEFUN FILLARRAY-INCREMENT-INDEX (ARRAY INDEX-ARRAY NDIMS)
  (DO ((DIM (1- NDIMS) (1- DIM)))
      ((< DIM 0))
    (LET ((VAL (1+ (ZL:AREF INDEX-ARRAY DIM))))
      (COND ((< VAL (ARRAY-DIMENSION ARRAY DIM))
             (SETF (AREF INDEX-ARRAY DIM) VAL)
             (RETURN NIL))
            (T
             (SETF (AREF INDEX-ARRAY DIM) 0))))))

;;; LISTARRAY of a one-dimensional array respects the fill pointer, but
;;; for multi-dimensional arrays it ignores the fill pointer.
(DEFUN LISTARRAY (ARRAY &OPTIONAL LIMIT)
  "Return a list of the elements of ARRAY, up to index LIMIT.
If LIMIT is NIL, the array size is used; for one-dimensional arrays,
the fill pointer is used if there is one.
Uses ZL:AREF, so will get fixnums out of strings."
  (IF (AND (SYMBOLP ARRAY)
           (FBOUNDP ARRAY)
           (ARRAYP (SYMBOL-FUNCTION ARRAY)))
      (SETQ ARRAY (SYMBOL-FUNCTION ARRAY)))
  (CHECK-TYPE ARRAY ARRAY)
  (CHECK-TYPE LIMIT (OR NULL INTEGER))
  (LET* ((NDIMS (ARRAY-RANK ARRAY))
         (ELEMENTS (IF (= NDIMS 1)
                       (ARRAY-ACTIVE-LENGTH ARRAY)
                       (ARRAY-LENGTH ARRAY)))
         (TIMES (IF (NULL LIMIT)
                    ELEMENTS
                    (MIN LIMIT ELEMENTS)))
         (LIST (MAKE-LIST TIMES))
         (L LIST)
         (COUNT 0))
    (COND ((= NDIMS 1)
           (DOTIMES (X (ARRAY-ACTIVE-LENGTH ARRAY))
             (SETQ COUNT (1+ COUNT))
             (IF (> COUNT TIMES)
                 (RETURN NIL))
             (SETF (CAR L) (ZL:AREF ARRAY X))
             (SETQ L (CDR L))))
          ((= NDIMS 2)
           (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
             (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
               (SETQ COUNT (1+ COUNT))
               (IF (> COUNT TIMES)
                   (RETURN NIL))
               (SETF (CAR L) (ZL:AREF ARRAY X Y))
               (SETQ L (CDR L)))))
          ((= NDIMS 3)
           (DOTIMES (X (ARRAY-DIMENSION ARRAY 0))
             (DOTIMES (Y (ARRAY-DIMENSION ARRAY 1))
               (DOTIMES (Z (ARRAY-DIMENSION ARRAY 2))
                 (SETQ COUNT (1+ COUNT))
                 (IF (> COUNT TIMES)
                     (RETURN NIL))
                 (SETF (CAR L) (ZL:AREF ARRAY X Y Z))
                 (SETQ L (CDR L))))))
          (T
           (USING-RESOURCE (INDEX-ARRAY FILLARRAY-INDEX-ARRAYS)
             (DOTIMES (I 10) (SETF (AREF INDEX-ARRAY I) 0))
             (DOTIMES (I TIMES)
               (SETF (CAR L) (FILLARRAY-GET ARRAY INDEX-ARRAY NDIMS))
               (SETQ L (CDR L))))))
    LIST))


(DEFUN %MAKE-PAGE-READ-ONLY (P)
  "Make virtual page at address P read only.  Lasts only until it is swapped out!"
  (%CHANGE-PAGE-STATUS P NIL (DPB %PHT-MAP-STATUS-READ-ONLY
                                  (BYTE 3. 6.)
                                  (LDB %%REGION-MAP-BITS        ;Change map-status
                                       (%REGION-BITS (%REGION-NUMBER P))))))

;;;; MAR-hacking functions

(DEFUN CLEAR-MAR ()
  "Clear out the mar setting."
  (DO ((P %MAR-LOW (+ P #o200)))
      ((> P %MAR-HIGH)) ;TROUBLE WITH NEGATIVE NUMBERS HERE!
    (%CHANGE-PAGE-STATUS P NIL (LDB %%REGION-MAP-BITS
                                    (%REGION-BITS (%REGION-NUMBER P)))))
  (SETQ %MAR-LOW -1
        %MAR-HIGH -2
        %MODE-FLAGS (%LOGDPB 0 %%M-FLAGS-MAR-MODE %MODE-FLAGS))
  NIL)

;;;Not GC-safe, additional hair required, also negative number trouble
(DEFUN SET-MAR (LOCATION CYCLE-TYPE &OPTIONAL (N-WORDS 1))
                                        ;N-WORDS SHOULD DEFAULT TO (SIZE LOCATION)
  "Set trap on reference to N-WORDS words starting at LOCATION.
N-WORDS defaults to 1.  CYCLE-TYPE is T, :READ or :WRITE."
  (SETQ CYCLE-TYPE (ECASE CYCLE-TYPE
                     (:READ 1)
                     (:WRITE 2)
                     ((T) 3)))
  (CLEAR-MAR)                                   ;Clear old mar
  (SETQ %MAR-HIGH (+ (1- N-WORDS) (SETQ %MAR-LOW (%POINTER LOCATION))))
  ;; If MAR'ed pages are in core, set up their traps
  (DO ((P %MAR-LOW (+ P #o200)))
      ((> P %MAR-HIGH))
    (%CHANGE-PAGE-STATUS P NIL (DPB 6 (BYTE 4. 6.)
                                    (LDB %%REGION-MAP-BITS      ;Change map-status
                                         (%REGION-BITS (%REGION-NUMBER P))))))
  (SETQ %MODE-FLAGS (%LOGDPB CYCLE-TYPE %%M-FLAGS-MAR-MODE %MODE-FLAGS))        ;Energize!
  T)

(DEFUN MAR-MODE ()
  (LET ((MODE (LDB %%M-FLAGS-MAR-MODE %MODE-FLAGS)))
    (CASE MODE
      (0 NIL)
      (1 :READ)
      (2 :WRITE)
      (3 T)
      (T (FERROR "The MAR mode, ~D, is invalid." MODE)))))


(DEFUN MEXP (&OPTIONAL FORM &AUX EXP)
  "Read-macroexpand-print loop, for seeing how macros expand.
MEXP reads s-expressions and macroexpands each one, printing the expansion.
Type NIL to exit (or Abort)."
  (DO-FOREVER
    (UNLESS FORM
      (FORMAT T "~2%Macro form ")
      (SEND *STANDARD-INPUT* :UNTYI (SEND *STANDARD-INPUT* :TYI)))      ;Allow abort to exit
    (CATCH-ERROR-RESTART ((SYS:ABORT ERROR) "Return to MEXP input loop.")
      (SETQ EXP (OR FORM (READ-FOR-TOP-LEVEL)))
      (AND (SYMBOLP EXP) (RETURN NIL))
      (DO ((LAST NIL EXP))
          ((EQ EXP LAST))
        (SETQ EXP (MACROEXPAND-1 EXP))
        (PRINC "  ")
        (GRIND-TOP-LEVEL EXP))
      (UNLESS (EQUAL EXP (SETQ EXP (MACROEXPAND-ALL EXP)))
        (PRINC "  ")
        (GRIND-TOP-LEVEL EXP)))
    (WHEN FORM (RETURN '*))))



;;;; STATUS and SSTATUS
;;; Note that these have to be Maclisp compatible and therefore have to work
;;; independent of packages.  All symbols on feature lists are in the keyword package.

;;; status and sstaus are obsolete.
;;; Instead, frob the special variable *features* directly


(DEFVAR *FEATURES*
        '(:LISPM :MIT :GIGAMOS :LMI :COMMON     ;":common" is what dec prolelisp says it is
          :CHAOS :SORT :FASLOAD :STRING :NEWIO :ROMAN :TRACE :GRINDEF :GRIND))

(defvar *lambda-features* *features*)

(defvar *target-features* 'nil                  ;4aug88 smh
  "An alternate *FEATURES* list which for purposes of cross compilation may be bound
or otherwise hacked instead of *FEATURES*.  #+(TARGET FOO) looks for FOO on
*TARGET-FEATURES* instead of *FEATURES*.  If NIL, *FEATURES* is used anyway.")

(defvar *falcon-features*
      '(:falcon :gigamos :lexical :commonlisp :loop :defstruct :lispm :mit :lmi
                :common :sort :fasload :string :newio :roman
                :trace :grindef :grind))

(DEFUN PROB-FROCESSOR ()
  ;; This takes a rest arg just in case there are more than two processor types....
  (FLET ((UPDATE (CORRECT-SYMBOL &REST OTHER-SYMBOLS)
          (DOLIST (S OTHER-SYMBOLS)
            (SETQ *FEATURES* (DELQ S *FEATURES*)))
          (PUSHNEW CORRECT-SYMBOL *FEATURES*)))
    ;; The FALCON case is separated out here, since there is no chance of ever
    ;; running the same worlds on the other machine types.
    #+(TARGET FALCON)
    (when (intersection '(:cadr :lambda :explorer) *features*)
      (setq *features* *falcon-features*))
    #+(TARGET (OR LAMBDA CADR EXPLORER))
    (progn
      ;; This should never happen, but what the hell!
      (when (member ':falcon *features*)
        (setq *features* *lambda-features*))
      (SELECT-PROCESSOR
        (:CADR (UPDATE :CADR :LAMBDA :explorer))
        ((:LAMBDA :explorer) (UPDATE :LAMBDA :CADR :explorer))
        (:falcon "FOO")))))

(ADD-INITIALIZATION "Frob *FEATURES* per processor" '(PROB-FROCESSOR) :COLD)

(DEFVAR STATUS-STATUS-LIST '(:FEATURE :FEATURES :NOFEATURE :STATUS :SSTATUS :TABSIZE
                             :USERID :SITE :OPSYS))

(DEFVAR STATUS-SSTATUS-LIST '(:FEATURE :NOFEATURE))

(DEFUN RETURN-STATUS (STATUS-LIST ITEM ITEM-P)
       (COND ((NOT ITEM-P) STATUS-LIST)
             ((NUMBERP ITEM) (MEMBER-EQUAL ITEM STATUS-LIST))
             (T (NOT (NULL (MEM #'STRING-EQUAL ITEM STATUS-LIST))))))


(DEFUN STATUS (&QUOTE STATUS-FUNCTION &OPTIONAL (ITEM NIL ITEM-P))
  "Obsolete Maclisp function. You really want to use the value of, or bind, *FEATURES*.
/(STATUS FEATURES) returns a list of symbols indicating features of the
Lisp environment.
/(STATUS FEATURE SYMBOL) returns T if SYMBOL is on the (STATUS FEATURES)
list,  otherwise NIL.
/(STATUS NOFEATURE SYMBOL) returns T if SYMBOL in *FEATURES*, otherwise NIL.
/(STATUS STATUS) returns a list of all status operations, ie *FEATURES*.
/(STATUS SSTATUS) returns a list of all sstatus operations."
  (SELECTOR STATUS-FUNCTION STRING-EQUAL
    (('FEATURE 'FEATURES) (RETURN-STATUS *FEATURES* ITEM ITEM-P))
    (('NOFEATURE) (UNLESS ITEM-P
                    (FERROR "Too few args to ~S ~S." 'STATUS 'NOFEATURE))
                  (NOT (RETURN-STATUS *FEATURES* ITEM ITEM-P)))
    (('STATUS) (RETURN-STATUS STATUS-STATUS-LIST ITEM ITEM-P))
    (('SSTATUS) (RETURN-STATUS STATUS-SSTATUS-LIST ITEM ITEM-P))
    (('TABSIZE) 8.)
    (('USERID) USER-ID)
    (('SITE) LOCAL-HOST-NAME)
    (('OPSYS) :LISPM)
    (OTHERWISE (FERROR "~S is not a legal STATUS request." STATUS-FUNCTION))))

(DEFUN SSTATUS (&QUOTE STATUS-FUNCTION ITEM
                &AUX (DEFAULT-CONS-AREA WORKING-STORAGE-AREA))
  "(SSTATUS FEATURE ITEM) adds ITEM to the list of features.
/(SSTATUS NOFEATURE ITEM) removes ITEM from the list of features.
New programs should use the variable *FEATURES*"
  (IF (SYMBOLP ITEM)
      (SETQ ITEM (INTERN (STRING ITEM) PKG-KEYWORD-PACKAGE)))   ;These are all keywords
  (SELECTOR STATUS-FUNCTION STRING-EQUAL
    (('FEATURE) (PUSHNEW ITEM *FEATURES* :TEST #'EQUAL)
                ITEM)
    (('NOFEATURE) (IF (MEMBER-EQUAL ITEM *FEATURES*)
                      (SETQ *FEATURES* (DEL #'EQUAL ITEM *FEATURES*)))
                  ITEM)
    (OTHERWISE (FERROR "~S is not a legal ~S request." 'SSTATUS STATUS-FUNCTION))))

;;;; Site stuff

(DEFUN UPDATE-SITE-CONFIGURATION-INFO ()
  "Read the latest site configuration files, including the host table."
  (MAYBE-MINI-LOAD-FILE-ALIST SITE-FILE-ALIST)
  (INITIALIZATIONS 'SITE-INITIALIZATION-LIST T)
  (SET-LOCAL-HOST-VARIABLES))  ;Runs SITE-OPTION-INITIALIZATION-LIST

(DEFVAR SITE-NAME NIL)
(DEFVAR SITE-OPTION-ALIST NIL
  "Alist of site option keywords as specified in SYS: SITE; SITE LISP")

(DEFVAR HOST-OVERRIDDEN-SITE-OPTION-ALIST NIL
  "Alist of site-keywords overridden on a per-machine basis,
as specified in SYS: SITE; LMLOCS LISP")

(DEFVAR SITE-INITIALIZATION-LIST NIL
  "Initializations run after new site tables are loaded.")

(DEFVAR SITE-OPTION-INITIALIZATION-LIST NIL
  "Initializations run when site options change
/(after loading new site tables and after warm boot).")

(DEFMACRO DEFSITE (SITE &BODY OPTIONS)
  "DEFSITE is used only in the file SYS: SITE; SITE LISP."
  `(DEFSITE-1 ',SITE ',OPTIONS))

(DEFUN DEFSITE-1 (NEW-SITE OPTIONS)
  (SETQ SITE-NAME NEW-SITE)
  (SETQ SITE-OPTION-ALIST (LOOP FOR (KEY EXP) IN OPTIONS
                                COLLECT `(,KEY . ,(EVAL EXP)))))

(DEFUN GET-SITE-OPTION (KEY)
  "Return the value at this site for site option KEY (a symbol in the keyword package).
The values of site options are specified in the file SYS: SITE; SITE LISP."
  (CDR (OR (ASSQ KEY HOST-OVERRIDDEN-SITE-OPTION-ALIST)
           (ASSQ KEY SITE-OPTION-ALIST))))

(DEFUN SET-SITE-OPTION (KEY VALUE)
  (LET ((CELL (OR (ASSQ KEY HOST-OVERRIDDEN-SITE-OPTION-ALIST)
                  (ASSQ KEY SITE-OPTION-ALIST))))
    (IF CELL
        (RPLACD CELL VALUE)
      (PUSH (CONS KEY VALUE) SITE-OPTION-ALIST))))

(DEFMACRO DEFINE-SITE-VARIABLE (VAR KEY &OPTIONAL DOCUMENTATION)
  "Define a variable whose value is automatically updated from the site option KEY's value."
  `(PROGN
     ,(IF DOCUMENTATION
          `(DEFVAR ,VAR :UNBOUND ,DOCUMENTATION)
        `(DEFVAR ,VAR))
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" VAR)
                         `(SETQ ,',VAR (GET-SITE-OPTION ',',KEY))
                         '(SITE-OPTION))))

(DEFMACRO DEFINE-SITE-HOST-LIST (VAR KEY &OPTIONAL DOCUMENTATION)
  "Define a variable whose value is a list of hosts, specified by the site option KEY.
The option's value itself will be a list of strings,
but the variable's value is a list of hosts with those names."
  `(PROGN
     ,(IF DOCUMENTATION
          `(DEFVAR ,VAR NIL ,DOCUMENTATION)
        `(DEFVAR ,VAR))
     (ADD-INITIALIZATION ,(FORMAT NIL "SITE:~A" VAR)
                         `(SETQ ,',VAR (MAPCAR #'PARSE-HOST (GET-SITE-OPTION ',',KEY)))
                         '(SITE-OPTION))))

;;; Set by major local network
;;; A function called with a host (string or host-object), a system-type and a local net
;;; address.
(DEFVAR NEW-HOST-VALIDATION-FUNCTION)

(DEFUN SET-SYS-HOST (HOST-NAME &OPTIONAL OPERATING-SYSTEM-TYPE HOST-ADDRESS SITE-FILE-DIRECTORY
                     &AUX HOST-OBJECT)
  "Specify the host to read system files from.
You can specify the operating system type (a keyword), host address, and the directory
for finding the site files, in case the system does not know that host yet."
  (and (stringp host-name)
       (setq host-name (string-upcase host-name)))
  (CHECK-TYPE HOST-NAME (OR STRING HOST) "a host name")
  ;; DWIM the operating system type for those who insist...
  (when (and operating-system-type
             (typep operating-system-type '(or string (and symbol (not keyword)))))
    (setq operating-system-type
          (intern (string-upcase (string-trim " " (string operating-system-type)))
                  'keyword)))
  (CHECK-ARG OPERATING-SYSTEM-TYPE (OR (NULL OPERATING-SYSTEM-TYPE)
                                       (GET OPERATING-SYSTEM-TYPE 'SYSTEM-TYPE-FLAVOR))
             "an operating system type")
  (AND (SETQ HOST-OBJECT (OR (FS:GET-PATHNAME-HOST HOST-NAME T NIL)
                             (si:PARSE-HOST HOST-NAME T NIL)))
       OPERATING-SYSTEM-TYPE
       (NEQ OPERATING-SYSTEM-TYPE (SEND HOST-OBJECT :SYSTEM-TYPE))
       (FERROR "~A is ~A, not ~A." HOST-OBJECT
               (SEND HOST-OBJECT :SYSTEM-TYPE) OPERATING-SYSTEM-TYPE))
  (cond ((null host-address)
         (unless host-object
           (error "No address specified, but unknown host")))
        ((stringp host-address)
         (setq host-object (funcall (get 'si:new-host-validation-function :internet)
                                    (or host-object host-name) operating-system-type host-address)))
        ((numberp host-address)
         (setq host-object (funcall (get 'si:new-host-validation-function :chaos)
                                    (or host-object host-name) operating-system-type host-address)))
        (t
         (error "Unrecognizable address ~A" host-address)))
  ;; FS:MAKE-LOGICAL-PATHNAME-HOST property T lets this host be redefined.
  ;; But we don't want to put it on the fs:*pathname-host-list* -
  ;; see SET-LOGICAL-PATHNAME-HOST. (kmc)
  (let((fs:*pathname-host-list* fs:*pathname-host-list*))
    (send (fs:set-logical-pathname-host "SYS" :physical-host host-object :translations
                                        (if site-file-directory
                                            `(("SITE" ,site-file-directory) ("CHAOS" ,site-file-directory))
                                          '()))
          :set :get 'fs:make-logical-pathname-host t))
  T)


;;; |||ASSIGN-VALUES and friends moved to QFCTNS.LISP
;;; in support of QFASL. -keith 27oct88

;;;; Disk stuff

(DEFUN DISK-RESTORE (&OPTIONAL PARTITION UNIT &AUX NAME COMMENT DESIRED-UCODE)
  "Restore partition PARTITION as a saved Lisp world.
PARTITION can be either a string naming a partition, or a number
which signifies a partition whose name starts with LOD.
Note that this does not change the running microcode.
You cannot successfully DISK-RESTORE a world that will not work
with the microcode that is running."
  ;; If the user didn't specify a unit then select one.
  (When (Null unit)
    (select-processor
      ((:cadr :lambda)
       (setq unit 0))
      (:explorer
        (setq unit (explorer-lod-band-logical-unit)))))
  (LET ((L (DISK-RESTORE-DECODE PARTITION)) BLOCK)
    (using-resource (rqb si:rqb disk-label-rqb-pages 4)
      ;(SETQ RQB (GET-DISK-LABEL-RQB))
      (READ-DISK-LABEL RQB UNIT)
      (SETQ NAME (IF PARTITION
                     (STRING-APPEND (LDB (BYTE 8. 0.) (CADR L)) (LDB (BYTE 8. 8.) (CADR L))
                                    (LDB (BYTE 8. 0.) (CAR L))  (LDB (BYTE 8. 8.) (CAR L)))
                   (GET-DISK-STRING RQB 7 4)))
      (SETQ BLOCK (FIND-DISK-PARTITION-FOR-READ NAME RQB unit)
            COMMENT (PARTITION-COMMENT NAME UNIT))
;;; +++ Incremental bands can't work with current system so don't bother checking +++
;;;     (MULTIPLE-VALUE-BIND (BASE-BAND VALID-FLAG)
;;;         (INC-BAND-BASE-BAND NAME UNIT)
;;;       (WHEN (AND BASE-BAND (NOT VALID-FLAG))
;;;         (FERROR "Band ~A is incremental, and the base band ~A is no longer valid."
;;;                 NAME BASE-BAND)))
        (SETQ DESIRED-UCODE (GET-UCODE-VERSION-OF-BAND NAME UNIT)))
   ;  (RETURN-DISK-RQB RQB)
    (AND ( DESIRED-UCODE %MICROCODE-VERSION-NUMBER)
         (NOT (ZEROP DESIRED-UCODE))            ;Not stored yet
         (FORMAT *QUERY-IO*
                 "~&That band prefers microcode ~D but the running microcode is ~D.~%"
                 DESIRED-UCODE %MICROCODE-VERSION-NUMBER))
    (WHEN (FQUERY FORMAT:YES-OR-NO-QUIETLY-P-OPTIONS
                  "Do you really want to reload ~A (~A)? " NAME COMMENT)
      (INITIALIZATIONS 'DISABLE-SERVICES-INITIALIZATION-LIST)
      ;; Stuff the unit number to restore from.
      (select-processor
        (:explorer
          (%p-dpb (convert-logical-unit-to-physical-unit unit)
                  (byte 6 0)
                  (%pointer-plus si:a-memory-virtual-address #o1775)))
        ((:cadr :lambda)))
      (%DISK-RESTORE (CAR L) (CADR L)))))

(DEFVAR WHO-LINE-JUST-COLD-BOOTED-P NIL) ;Set to T upon cold boot for who-line's benefit

;;; This should go in a network system file, but there really aren't any yet.
(ADD-INITIALIZATION "Disable services" '(INITIALIZATIONS 'DISABLE-SERVICES-INITIALIZATION-LIST)
                    :BEFORE-COLD)

;;; Please do not add garbage to DISK-SAVE if possible.
;;; Put random initializations on the BEFORE-COLD initialization list.
(DEFUN DISK-SAVE (PARTITION &OPTIONAL NO-QUERY INCREMENTAL)
  "Save the current Lisp world in partition PARTITION.
PARTITION can be either a string naming a partition, or a number which signifies
 a partition whose name starts with LOD.
NO-QUERY says do not ask for confirmation (or any keyboard input at all).
INCREMENTAL means to write out only those parts of the world which have changed
 since the it was loaded from disk. (The effect of loading a world from a band
 saved incrementally is that the incremental saves /"patch/" the original full save."
  (PROG* ((L (DISK-RESTORE-DECODE PARTITION))
          (PART-NAME (STRING-APPEND (LDB (BYTE 8. 0.) (CADR L)) (LDB (BYTE 8. 8.) (CADR L))
                                    (LDB (BYTE 8. 0.) (CAR L))  (LDB (BYTE 8. 8.) (CAR L))))
          PART-BASE PART-SIZE SYSTEM-VERSION MAX-ADDR
          (INC-PAGES-SAVED 0)
          unit)
    (setq unit (unit-for-partition part-name))
    (OR (MULTIPLE-VALUE-SETQ (PART-BASE PART-SIZE)
          (IF NO-QUERY
              (FIND-DISK-PARTITION-FOR-READ PART-NAME nil unit)
            (FIND-DISK-PARTITION-FOR-WRITE PART-NAME nil unit)))
        (RETURN NIL))

    (UNLESS NO-QUERY
      (DOLIST (PATCH-SYSTEM PATCH-SYSTEMS-LIST)
        (WHEN (EQ (PATCH-STATUS PATCH-SYSTEM) :INCONSISTENT)
          (BEEP)
          (FORMAT *QUERY-IO* "~&You have loaded patches out of sequence,
 or loaded unreleased patches, in ~A.
As a result, the environment is probably inconsistent with the
current patches and will remain so despite attempts to update it.
Unless you understand these problems well and know how to
be sure whether they are occurring, or how to clean them up,
you should not save this environment."
                  (PATCH-NAME PATCH-SYSTEM))
          (SEND *QUERY-IO* :CLEAR-INPUT)
          (UNLESS (YES-OR-NO-P "Dump anyway? ")
            (RETURN-FROM DISK-SAVE NIL)))))

    ;; This will catch most lossages before the user has waited.
    (UNLESS INCREMENTAL
      (CHECK-PARTITION-SIZE PART-SIZE))

    ;; Prompt now for this rather than waiting through all the initializations.
    (LET ((MAX (OR (MAXIMUM-PARTITION-COMMENT-LENGTH PART-NAME unit) 16.)))
      (SETQ SYSTEM-VERSION
            (IF NO-QUERY
                (LET ((VERS (SYSTEM-VERSION-INFO T)))
                  (SUBSTRING VERS 0 (MIN (LENGTH VERS) MAX)))
              (GET-NEW-SYSTEM-VERSION MAX :INCREMENTAL INCREMENTAL :query T))))

    ;; Cause cold boot initializations to happen when rebooted
    ;; and do the BEFORE-COLD initializations now
    (amazing-kludge-order-cold-initialization-list)
    (format t "~%Beginning shut-down initializations...")
    (INITIALIZATIONS 'BEFORE-COLD-INITIALIZATION-LIST T)
    (RESET-INITIALIZATIONS 'COLD-INITIALIZATION-LIST)
    (let ((gc-status gc:*gc-process-control*))
      (when (eq gc-status :active)
        (gc:gc-off))                            ;Wait for current reclamation (if any) to complete
      (gc:maybe-flip-level-2)                   ;Flip level 2 in batch mode
      (when (eq gc-status :active)
        (gc:gc-on)))                            ;Reenable the GC Process
    (SETQ WHO-LINE-JUST-COLD-BOOTED-P T)
    (LOGOUT)

    ;; Help stop user from getting worried.
    (WHEN INCREMENTAL
      (FORMAT T "~&NOTE: Comparing current memory contents with the original band
will take a few minutes.")
      (PROCESS-SLEEP 120.))

    ;; This can't be a before-cold initialization, because some other
    ;; initializations sometimes type out
    (when (get 'tv:window 'si:flavor)
      TV:(SHEET-FORCE-ACCESS (INITIAL-LISP-LISTENER)
                             (SEND INITIAL-LISP-LISTENER :REFRESH)
                             (SEND INITIAL-LISP-LISTENER :HOME-CURSOR)))

    ;; :before-cold intialization list deconfigures entire network...
    ;;(CHAOS:RESET)  ;Otherwise, UCODE could lose hacking packets as world dumped.

    ;; Compare all pages with band we booted from,
    ;; record unchanged pages in a bitmap in the band being saved in.
    (WHEN INCREMENTAL
      (SETQ INC-PAGES-SAVED (DISK-SAVE-INCREMENTAL PART-BASE)))

    ;; Check again before updating the partition comment.
    (CHECK-PARTITION-SIZE (+ INC-PAGES-SAVED PART-SIZE))
    (UPDATE-PARTITION-COMMENT PART-NAME SYSTEM-VERSION unit)

    ;; Now shut down the world and check the partition size for real, just
    ;; to make sure that we didn't exceed the size very recently.
    (cond ((get 'tv:window 'si:flavor)
           (DOLIST (S TV:ALL-THE-SCREENS) (TV:SHEET-GET-LOCK S))
           (TV:WITH-MOUSE-USURPED
             (WITHOUT-INTERRUPTS
               (SETQ TV:MOUSE-SHEET NIL)
               (DOLIST (S TV:ALL-THE-SCREENS)
                 (SEND S :DEEXPOSE)
                 (TV:SHEET-RELEASE-LOCK S))
               (disk-save-1 max-addr inc-pages-saved part-size incremental l))))
          ('else
           (without-interrupts
             (disk-save-1 max-addr inc-pages-saved part-size incremental l))))))

(defun disk-save-1 (max-addr inc-pages-saved part-size incremental l)
  ;; The process we are now executing in will look like it was warm-booted when
  ;; this saved band is restored.  Suppress the warm-boot message, but disable
  ;; and flush the process so it doesn't start running with its state destroyed.
  ;; We'd like to :RESET it, but can't because we are still running in it.
  ;; If the process is the initial process, it will get a new state and get enabled
  ;; during the boot process.
  (PROCESS-DISABLE CURRENT-PROCESS)
  (SET-PROCESS-WAIT CURRENT-PROCESS 'FLUSHED-PROCESS NIL)
  (SETQ CURRENT-PROCESS NIL)
  ;; Once more with feeling, and bomb out badly if losing.
  (SETQ MAX-ADDR (FIND-MAX-ADDR))
  (CHECK-PARTITION-SIZE (+ INC-PAGES-SAVED PART-SIZE) T)
  ;; Store the size in words rather than pages.  But don't get a bignum!
  (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-HIGHEST-VIRTUAL-ADDRESS)
        (LSH MAX-ADDR 8.))
  ;; Avoid randomness from ucode ethernet routines before initialized.
  (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-ETHER-FREE-LIST)
        NIL)
  (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-ETHER-TRANSMIT-LIST)
        NIL)
  (SETF (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-ETHER-RECEIVE-LIST)
        NIL)
  (DO ((I #o600 (1+ I)))                        ;Clear the disk error log
      ((= I #o640))
    (%P-DPB 0 %%Q-LOW-HALF I)
    (%P-DPB 0 %%Q-HIGH-HALF I))
  (%DISK-SAVE (* (IF INCREMENTAL -1 1)
                 (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-MEMORY-SIZE))
              (CAR L) (CADR L)))

(DEFUN CHECK-PARTITION-SIZE (PART-SIZE &OPTIONAL EXPOSE-P)
  (LET ((DUMP-SIZE (ESTIMATE-DUMP-SIZE)))
    (WHEN (> DUMP-SIZE PART-SIZE)
      ;; This test is not necessarily accurate, since we have not
      ;; yet shut off the world.  However, it should catch most cases,
      ;; so that this error will be detected before the partition comment
      ;; gets clobbered.
      (AND EXPOSE-P (SEND TV:MAIN-SCREEN :EXPOSE))
      (FERROR "Cannot save, partition too small.  Need at least ~D. pages.~@[~@
                      Warm Boot please.~]" DUMP-SIZE EXPOSE-P))
    DUMP-SIZE))

(DEFUN ESTIMATE-DUMP-SIZE NIL
  (DO ((REGION 0 (1+ REGION))
       (SIZE 0))
      ((= REGION (%REGION-LENGTH REGION-LENGTH))
       SIZE)
    ;; Check each region.  If it is free, ignore it.  Otherwise,
    ;; add how many pages it will take to dump it.
    (IF ( (LDB %%REGION-SPACE-TYPE (%REGION-BITS REGION))
           %REGION-SPACE-FREE)
        (SETQ SIZE (+ SIZE (CEILING (REGION-TRUE-FREE-POINTER REGION) PAGE-SIZE))))))

;;; Find the highest address in the virtual memory.  If you call this without
;;; inhibiting interrupts, the result is not strictly correct since some
;;; other process could invalidate it at any time by CONSing.  However,
;;; it gives you a good idea and a lower bound.  The answer is in number
;;; of pages.
(DEFUN FIND-MAX-ADDR ()
  (DO ((REGION 0 (1+ REGION))
       (MAX-ADDR 0))
      ((= REGION (%REGION-LENGTH REGION-LENGTH))
       (TRUNCATE MAX-ADDR PAGE-SIZE))
    ;; Check each region.  If it is free, ignore it.  Otherwise,
    ;; find the highest address of that region, and get the
    ;; highest such address.
    (IF ( (LDB %%REGION-SPACE-TYPE (%REGION-BITS REGION))
           %REGION-SPACE-FREE)
        (SETQ MAX-ADDR (MAX MAX-ADDR (+ (REGION-ORIGIN-TRUE-VALUE REGION)
                                        (REGION-TRUE-LENGTH REGION)))))))

(DEFUN REGION-ORIGIN-TRUE-VALUE (REGION)
  ;; below crock avoids returning a negative number if region starts above
  ;; half way point in address space.  It can make a bignum so be careful!
  (%POINTER-UNSIGNED (%REGION-ORIGIN REGION)))

(DEFUN REGION-TRUE-LENGTH (REGION)
  ;; below crock avoids returning a negative number if region has a large
  ;; length. It can make a bignum so be careful!
  (%POINTER-UNSIGNED (%REGION-LENGTH REGION)))

(DEFUN REGION-TRUE-FREE-POINTER (REGION)
  ;; below crock avoids returning a negative number if region has a large
  ;; length. It can make a bignum so be careful!
  (%POINTER-UNSIGNED (%REGION-FREE-POINTER REGION)))

;disk-restore-decode moved to LTOP for MINI-DISK-SAVE


;;; New microcode meter interface; uses value cell of meter names to store virtual address
;;; of meter.  Call INITIALIZE-MICROCODE-METER-ADDRESSES during initialization or when
;;; meters change.

(defvar *counter-block-virtual-address* :unbound)

(defun initialize-microcode-meter-addresses ()
  "Compute the address of every microcode meter and store it in its value cell."
  (setq *counter-block-virtual-address*
        (%pointer-plus a-memory-virtual-address %counter-block-a-mem-address))
  (loop for name in a-memory-counter-block-names
        for offset = (find-position-in-list name a-memory-counter-block-names)
        do (set name (%pointer-plus offset *counter-block-virtual-address*))))

(add-initialization "Initialize meters" '(initialize-microcode-meter-addresses) :system)

(defmacro coerce-to-meter-location (meter)
  `(etypecase ,meter
     (fixnum)
     (symbol
      (setq ,meter
            (%pointer-plus (or (find-position-in-list ,meter a-memory-counter-block-names)
                               (ferror "~S is not a valid counter name." ,meter))
                           *counter-block-virtual-address*)))))

(defun read-meter (meter)
  "Read microcode meter METER."
  (coerce-to-meter-location meter)
  (without-interrupts
    (dpb (%p-ldb (BYTE 16. 16.) meter)
         (BYTE 16. 16.)
         (%p-ldb (BYTE 16. 0.) meter))))

(defun clear-meter (meter)
  "Clear microcode meter METER, returning its previous value."
  (coerce-to-meter-location meter)
  (without-interrupts
    (prog1
      (dpb (%p-ldb (BYTE 16. 16.) meter)
           (BYTE 16. 16.)
           (%p-ldb (BYTE 16. 0.) meter))
      (%p-dpb 0 (BYTE 16. 16.) meter)
      (%p-dpb 0 (BYTE 16. 0.)  meter))))

(defun write-meter (meter value)
  "Modify microcode meter METER."
  (coerce-to-meter-location meter)
  (without-interrupts
    (%p-dpb (ldb (BYTE 16. 16.) value)
            (BYTE 16. 16.)
            meter)
    (%p-dpb (ldb (BYTE 16. 0.) value)           ;must ldb to get correct low bits if bignum!
            (BYTE 16. 0.)
            meter)))

;;;; Super-hairy module support

(DEFVAR *MODULES* NIL
  "List of modules marked present with PROVIDE.
All systems loaded with MAKE-SYSTEM are also present.
Keyed by STRING=")

(DEFUN PROVIDE (MODULE)
  "Mark MODULE as being already loaded."
  (PUSHNEW (STRING MODULE) *MODULES* :TEST #'STRING=))

(DEFUN REQUIRE (MODULE &OPTIONAL PATHNAMES)
  "Cause MODULE to be loaded if it isn't yet.
If PATHNAMES is specified, it should be a pathname or a list of pathnames;
 those files are loaded.
Otherwise, MAKE-SYSTEM is done on MODULE."
  (UNLESS (MEM #'STRING= MODULE *MODULES*)
    (COND ((CONSP PATHNAMES)
           (MAPC #'LOAD PATHNAMES))
          (PATHNAMES (LOAD PATHNAMES))
          (T (MAKE-SYSTEM MODULE)))))


;;;; Stuff for function specs

;;; These are here because they must be loaded after the package system is operational
;;; (or maybe only because they aren't needed in the cold load?)

;;; This is useful for sorting function specs
(DEFUN FUNCTION-SPEC-LESSP (FS1 FS2)
  "Compares two function specs, approximately alphabetically."
  (STRING-LESSP (IF (SYMBOLP FS1) FS1 (SECOND FS1))
                (IF (SYMBOLP FS2) FS2 (SECOND FS2))))

(DEFUN FUNDEFINE (FUNCTION-SPEC)
  "Makes FUNCTION-SPEC not have a function definition."
  ;; First, validate the function spec and determine its type
  (SETQ FUNCTION-SPEC (DWIMIFY-ARG-PACKAGE FUNCTION-SPEC 'FUNCTION-SPEC))
  (IF (SYMBOLP FUNCTION-SPEC) (FMAKUNBOUND FUNCTION-SPEC)
      (FUNCALL (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER) 'FUNDEFINE FUNCTION-SPEC)))

(DEFUN FDEFINITION-LOCATION (FUNCTION-SPEC &AUX HANDLER)
  "Returns a locative pointer to the cell containing FUNCTION-SPEC's definition."
  ;; First, validate the function spec and determine its type
  (COND ((SYMBOLP FUNCTION-SPEC) (LOCF (FSYMEVAL FUNCTION-SPEC)))
        ((AND (CONSP FUNCTION-SPEC)
              (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))
         (FUNCALL HANDLER 'FDEFINITION-LOCATION FUNCTION-SPEC))
        (T (FERROR 'SYS:INVALID-FUNCTION-SPEC
                   "The function spec ~S is invalid." FUNCTION-SPEC))))

(DEFUN FUNCTION-PARENT (FUNCTION-SPEC &AUX DEF TEM)
  (DECLARE (VALUES NAME TYPE))
  "Returns NIL or the name of another definition which has the same source code.
The second value is the type of that definition (which can be NIL).
This is used for things like internal functions, methods automatically
created by a defflavor, and macros automatically created by a defstruct."
  (COND ((AND (FDEFINEDP FUNCTION-SPEC)
              (SETQ TEM (CDR (ASSQ 'FUNCTION-PARENT
                                   (DEBUGGING-INFO (SETQ DEF (FDEFINITION FUNCTION-SPEC))))))
              ;; Don't get confused by circular function-parent pointers.
              (NOT (EQUAL TEM FUNCTION-SPEC)))
         (VALUES (CAR TEM) (CADR TEM)))
        ((AND (EQ (CAR-SAFE DEF) 'MACRO) (SYMBOLP (CDR DEF))    ;for DEFSTRUCT
              (SETQ DEF (GET (CDR DEF) 'MACROEXPANDER-FUNCTION-PARENT)))
         (FUNCALL DEF FUNCTION-SPEC))
        ((CONSP FUNCTION-SPEC)
         (FUNCALL (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)
                  'FUNCTION-PARENT FUNCTION-SPEC))))

;;; (:LOCATION locative-or-list-pointer) refers to the CDR of the pointer.
;;; This is for pointing at an arbitrary place which there is no special
;;; way to describe.
(DEFPROP :LOCATION LOCATION-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN LOCATION-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((LOC (SECOND FUNCTION-SPEC)))
    (IF (NOT (AND (= (LENGTH FUNCTION-SPEC) 2)
                  (TYPEP LOC '(OR LOCATIVE CONS))))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC
                  "The function spec ~S is invalid." FUNCTION-SPEC))
      (CASE FUNCTION
        (VALIDATE-FUNCTION-SPEC T)
        (FDEFINE (RPLACD LOC ARG1))
        (FDEFINITION (CDR LOC))
        (FDEFINEDP (AND ( (%P-DATA-TYPE LOC) DTP-NULL) (NOT (NULL (CDR LOC)))))
        (FDEFINITION-LOCATION LOC)
        ;; FUNDEFINE could store DTP-NULL, which would only be right sometimes
        (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))

;;; Convert old Maclisp-style property function specs
(DEFUN STANDARDIZE-FUNCTION-SPEC (FUNCTION-SPEC &OPTIONAL (ERRORP T))
  (AND (CONSP FUNCTION-SPEC)
       (= (LENGTH FUNCTION-SPEC) 2)
       (SYMBOLP (CAR FUNCTION-SPEC))
       (NOT (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER))
       (SETQ FUNCTION-SPEC (CONS ':PROPERTY FUNCTION-SPEC)))
  (OR (NOT ERRORP)
      (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
      (FERROR "~S is not a valid function spec." FUNCTION-SPEC))
  FUNCTION-SPEC)

(DEFPROP DEFUN "Function" DEFINITION-TYPE-NAME)
(DEFPROP DEFVAR "Variable" DEFINITION-TYPE-NAME)

(DEFVAR NON-PATHNAME-REDEFINED-FILES NIL
  "Files whose functions it is ok to redefine from the keyboard.")

;;; Query about any irregularities about redefining the given function symbol now.
;;; Return T to tell caller to go ahead and redefine the symbol
;;; (no problems or user says ok), NIL to leave it unchanged.
(DEFUN QUERY-ABOUT-REDEFINITION (FUNCTION-SPEC NEW-PATHNAME TYPE OLD-PATHNAME)
  ;; Detect any cross-file redefinition worth complaining about.
  (IF (OR (EQ (IF (STRINGP OLD-PATHNAME) OLD-PATHNAME
                (AND OLD-PATHNAME (SEND OLD-PATHNAME :TRANSLATED-PATHNAME)))
              (IF (STRINGP NEW-PATHNAME) NEW-PATHNAME
                (AND NEW-PATHNAME (SEND NEW-PATHNAME :TRANSLATED-PATHNAME))))
          (MEMQ OLD-PATHNAME
                (IF NEW-PATHNAME
                    (SEND NEW-PATHNAME :GET :REDEFINES-FILES)
                  NON-PATHNAME-REDEFINED-FILES)))
      T
    ;; This redefinition deserves a warning or query.
    ;; If it is within a file operation with warnings,
    ;; record a warning.
    (WHEN (AND (VARIABLE-BOUNDP FILE-WARNINGS-DATUM) FILE-WARNINGS-DATUM)
      (RECORD-AND-PRINT-WARNING 'REDEFINITION :PROBABLE-ERROR NIL
                                (IF NEW-PATHNAME
                                    "~A ~S being redefined by file ~A.
 It was previously defined by file ~A."
                                  "~A ~S being redefined;~* it was previously defined by file ~A.")
                                (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE) FUNCTION-SPEC
                                NEW-PATHNAME OLD-PATHNAME))
    (LET (CONDITION CHOICE)
      (SETQ CONDITION
            (MAKE-CONDITION 'SYS:REDEFINITION
                            (IF NEW-PATHNAME
                                "~A ~S being redefined by file ~A.
It was previously defined by file ~A."
                              "~A ~S being redefined;~* it was previously defined by file ~A.")
                            (OR (GET TYPE 'DEFINITION-TYPE-NAME) TYPE)
                            FUNCTION-SPEC
                            NEW-PATHNAME OLD-PATHNAME))
      (SETQ CHOICE (SIGNAL CONDITION))
      (UNLESS CHOICE
        (UNLESS (AND INHIBIT-FDEFINE-WARNINGS
                     (NEQ INHIBIT-FDEFINE-WARNINGS :JUST-WARN))
          (FORMAT *QUERY-IO* "~&~A" CONDITION))
        (IF INHIBIT-FDEFINE-WARNINGS
            (SETQ CHOICE T)
          (SETQ CHOICE
                (FQUERY '(:CHOICES (((ERROR "Error.") #/E)
                                    ((PROCEED "Proceed.") #/P)
                                    . #.FORMAT:Y-OR-N-P-CHOICES)
                                   :HELP-FUNCTION
                                   (LAMBDA (STREAM &REST IGNORE)
                                     (PRINC "
  Type Y to proceed to redefine the function, N to not redefine it, E to go into the
  error handler, or P to proceed and not ask in the future (for this pair of files): "
                                            STREAM))
                                   :CLEAR-INPUT T
                                   :FRESH-LINE NIL)
                        " OK? "))))
      (CASE CHOICE
        ((T :NO-ACTION) T)
        ((NIL :INHIBIT-DEFINITION) NIL)
        (ERROR
         (ERROR CONDITION)
         T)
        (PROCEED
         (IF NEW-PATHNAME
             (PUSH OLD-PATHNAME (GET NEW-PATHNAME :REDEFINES-FILES))
           (PUSH OLD-PATHNAME NON-PATHNAME-REDEFINED-FILES))
         T)))))

(DEFUN UNDEFUN (FUNCTION-SPEC &AUX TEM)
  "Restores the saved previous function definition of a function spec."
  (SETQ FUNCTION-SPEC (DWIMIFY-ARG-PACKAGE FUNCTION-SPEC 'FUNCTION-SPEC))
  (SETQ TEM (FUNCTION-SPEC-GET FUNCTION-SPEC :PREVIOUS-DEFINITION))
  (COND (TEM
         (FSET-CAREFULLY FUNCTION-SPEC TEM T))
        ((Y-OR-N-P (FORMAT NIL "~S has no previous definition.  Undefine it? "
                           FUNCTION-SPEC))
         (FUNDEFINE FUNCTION-SPEC))))


;;; Some source file stuff that does not need to be in QRAND
(DEFUN GET-SOURCE-FILE-NAME (FUNCTION-SPEC &OPTIONAL TYPE)
  "Returns pathname of source file for definition of type TYPE of FUNCTION-SPEC.
If TYPE is NIL, the most recent definition is used, regardless of type.
FUNCTION-SPEC really is a function spec only if TYPE is DEFUN;
for example, if TYPE is DEFVAR, FUNCTION-SPEC is a variable name."
  (DECLARE (RETURN-LIST PATHNAME TYPE))
  (LET ((PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC :SOURCE-FILE-NAME)))
    (COND ((NULL PROPERTY) NIL)
          ((ATOM PROPERTY)
           (AND (MEMQ TYPE '(DEFUN NIL))
                (VALUES PROPERTY 'DEFUN)))
          (T
           (LET ((LIST (IF TYPE (ASSQ TYPE PROPERTY) (CAR PROPERTY))))
             (LOOP FOR FILE IN (CDR LIST)
                   WHEN (NOT (SEND FILE :GET :PATCH-FILE))
                   RETURN (VALUES FILE (CAR LIST))))))))

(DEFUN GET-ALL-SOURCE-FILE-NAMES (FUNCTION-SPEC)
  "Return list describing source files for all definitions of FUNCTION-SPEC.
Each element of the list has a type of definition as its car,
and its cdr is a list of generic pathnames that made that type of definition."
  (LET ((PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC :SOURCE-FILE-NAME)))
    (COND ((NULL PROPERTY) NIL)
          ((ATOM PROPERTY)
           (SETQ PROPERTY `((DEFUN ,PROPERTY)))
           ;; May as well save this consing.
           (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PROPERTY :SOURCE-FILE-NAME)
           PROPERTY)
          (T PROPERTY))))

;; documentation and set-documentation are in QRAND as they must be in the cold-load

;;; Old name.
(DEFF FUNCTION-DOCUMENTATION 'DOCUMENTATION)
(MAKE-OBSOLETE FUNCTION-DOCUMENTATION
               "use DOCUMENTATION with a second argument of 'FUNCTION.")

(DEFUN COMBINED-METHOD-DOCUMENTATION (METHOD &OPTIONAL STREAM &KEY (FRESH-LINE T))
  "Returns a string which documents Method.  Method must be a :combined method.
If Stream is a stream, then instead prints documentation to the stream.
Fresh-Line is only used if Stream is a stream.
This documentation string will have the format:

  method combination is <keyword for type>, order is <keyword for order>

  :wrapper methods
    flavor component-flavor-1, arglist: args
      doc string
    flavor component-flavor-2, arglist: args
      doc string
    ...

    :around methods
      flavor component-flavor-1, arglist: args
        doc string
      flavor component-flavor-2, arglist: args
        doc string
      ...

      etc. { the rest depends on which type of method combination is used.
             see SI::DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER, SI::COMBINED-DOC-STRING, and
             (:property SI::COMBINED-METHOD-DOCUMENTATION <method combination keyword>) }

the ordering for component flavors is the order in which the components are combined to form
the combined method.  Note that the following orders always hold:
   :wrappers         :base-flavor-last
   :around methods   :base-flavor-last
   :before methods   :base-flavor-last
   :after methods    :base-flavor-first

A handler for the method-combination type used by the combined method must exist
/(see SI::DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER to define new ones)."
  (LET* ((TEMP (CADR (ASSQ 'COMBINED-METHOD-DERIVATION (DEBUGGING-INFO METHOD))))
         (TYPE (OR (CADR TEMP) :DAEMON))        ; :daemon is default type
         (HANDLER (GET 'COMBINED-METHOD-DOCUMENTATION TYPE))
         (ORDER (IF (EQ TYPE :PASS-ON) (CAADDR TEMP) (CADDR TEMP)))
         (DERIVATION (CDDDR TEMP))
         (FLAVORS (FLAVOR-DEPENDS-ON-ALL (GET (FLAVOR-OF-METHOD METHOD) 'FLAVOR)))
         (INDENT 0)
         STRING ST)
    (WHEN (NULL HANDLER)
      (FERROR "No combined method doc handler for ~S method combination." TYPE))
    (IF (OR (STREAMP STREAM) (EQ STREAM T)) (SETQ STRING STREAM)
      ;; only need string if no stream
      (SETQ STRING (MAKE-STRING (* 100 (LENGTH DERIVATION)) :FILL-POINTER 0))
      (SETQ STREAM NIL))
    ;; put in header
    (SETQ ST (FORMAT STREAM "~@[~&~*~]method combination is ~S~@[, order is ~S~]"
                     (AND STREAM FRESH-LINE) TYPE (AND (NEQ TYPE :DAEMON) ORDER)))
    (UNLESS STREAM
      (STRING-NCONC STRING ST))
    ;; do :wrapper and :around methods
    (COMBINED-DOC-STRING STRING (ASSQ :WRAPPER DERIVATION) FLAVORS INDENT ORDER)
    (WHEN (ASSQ :WRAPPER DERIVATION) (INCF INDENT 2))
    (COMBINED-DOC-STRING STRING (ASSQ :AROUND DERIVATION) FLAVORS INDENT ORDER)
    (WHEN (ASSQ :AROUND DERIVATION) (INCF INDENT 2))
    ;; call the handler appropriate to the type of method combination
    (FUNCALL HANDLER STRING DERIVATION FLAVORS INDENT ORDER)
    (WHEN (AND (STRINGP STRING) (PLUSP (LENGTH STRING))) STRING)))

(DEFUN COMBINED-DOC-STRING (STRING COMPONENTS FLAVORS INDENT &OPTIONAL ORDER)
  "Add the documentation for a component method type to String.
The type of method is in the CAR of Components.  The component methods are the CDR of
Components.  Flavors is the list of component flavors, in the order of flavor combination.
Order is taken from the method combination specifier.  The result will look roughly like:

     <indentation>
    <method type> methods, order is <order of method combination>
      flavor <flavor of first component method>, arglist: <arglist of component method>
          <documentation string for component method>
      flavor <flavor of second component method>, arglist: <arglist of component method>
          <documentation string for component method>
      ...

:CASE methods vary slightly in that they include the suboperation at the beginning of the line
giving the flavor and arglist.  If Order is nil then the header will not include mention of
the order of combination.

String can be either a string (which must have a fill pointer), in which case the modified
string is returned, or it can be a stream."
  (LET ((TYPE (POP COMPONENTS))
        (STREAM (AND (OR (STREAMP STRING) (EQ STRING T)) STRING))
        METHODS ST)
    (IF (NULL COMPONENTS) STRING
      (PKG-BIND 'USER                           ; force printing of package prefix's
        (SETQ ST (FORMAT STREAM "~2%~V,0T~:[~*:PRIMARY~;~S~] method~P~@[, order is ~S~]"
                         INDENT TYPE TYPE (LENGTH COMPONENTS) ORDER))
        (UNLESS STREAM
          (STRING-NCONC STRING ST))
        (DOLIST (FLAVOR FLAVORS STRING)
          (SETQ METHODS (SUBSET `(LAMBDA (M) (EQ ',FLAVOR (FLAVOR-OF-METHOD M))) COMPONENTS))
          (DOLIST (METHOD METHODS)
            (SETQ ST (FORMAT STREAM "~%~V,0T~@[~S suboperation, ~]flavor ~S, arglist: ~S"
                             (+ INDENT 2)
                             (AND (EQ TYPE :CASE) (CAR (LAST (NAME-OF-METHOD METHOD))))
                             FLAVOR
                             (DO ((ARGS (ARGLIST METHOD) (CDR ARGS)))
                                 ((NOT (MEMQ (CAR ARGS) '(.OPERATION. .SUBOPERATION.)))
                                  ARGS))))
            (UNLESS STREAM
              (STRING-NCONC STRING ST))
            (LET ((DOC (DOCUMENTATION METHOD)))
              (WHEN DOC
                (SETQ ST (FORMAT STREAM "~%~V,0T~~A~" (+ INDENT 4) DOC))
                (UNLESS STREAM
                  (STRING-NCONC STRING ST))))))))))

(DEFUN FLAVOR-OF-METHOD (METHOD)
  "Returns the symbol which is the flavor for Method.
Method may be either a method spec (ie. (:method flavor type operation suboperation)) or an
FEF for a method.  Error if Method is not a defined method."
  (IF (TYPEP METHOD 'COMPILED-FUNCTION)
      ;; get name of method from fef and extract flavor
      (CADR (NAME-OF-METHOD METHOD))
    (IF (CONSP (SETQ METHOD (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC METHOD))))
        (IF (EQ (CAR METHOD) 'MACRO) (FLAVOR-OF-METHOD (CDR METHOD))
          (CADR (ASSQ :SELF-FLAVOR              ; named-lambda
                      (NTH-VALUE 1 (EXTRACT-DECLARATIONS
                                     (CDR (LAMBDA-EXP-ARGS-AND-BODY METHOD)) NIL T)))))
      (FLAVOR-OF-METHOD METHOD))))              ; fef returned by fdefinition, try again

(DEFUN NAME-OF-METHOD (METHOD)
  "Returns the list which is the function spec for the method.
Method may be either a method spec (ie. (:method flavor type operation suboperation)) or an
FEF for a method.  Error if Method is not a defined method."
  (IF (TYPEP METHOD 'COMPILED-FUNCTION)
      (%P-CONTENTS-OFFSET METHOD %FEFHI-FCTN-NAME)
    (IF (CONSP (SETQ METHOD (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC METHOD))))
        (IF (EQ (CAR METHOD) 'MACRO) (NAME-OF-METHOD (CDR METHOD))
          (CAADR METHOD))                       ; named-lambda
      (NAME-OF-METHOD METHOD))))                ; fef, try again

(DEFMACRO DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER (TYPE &BODY BODY)
  "Expands to define (:property si::combined-method-documentation <combination type>).
Body can reference the following variables (they are the args to the handler):
 String      the documentation string to be modified (use STRING-NCONC).
 Derivation  the list of component methods for this method.  it is an alist, with each element
             being a method type followed by the component methods of that type.
 Flavors     the list of component flavors, in the order of flavor combination.
 Indent      number of spaces to indent from the left, used for formatting.
 Order       from the method-combination declaration, a keyword.
Typically, Body consists of some number of calls to SI::COMBINED-DOC-STRING interspersed with
adjustments to the indentation."
  (DECLARE (ARGLIST (METHOD-COMBINATION-TYPE) &BODY BODY))
  (SETQ TYPE (INTERN (STRING-UPCASE TYPE) PKG-KEYWORD-PACKAGE))
  (MULTIPLE-VALUE-BIND (BODY DECLARATIONS DOCUMENTATION)
      (EXTRACT-DECLARATIONS BODY NIL T)
    `(DEFUN (COMBINED-METHOD-DOCUMENTATION ,TYPE) (STRING DERIVATION FLAVORS INDENT ORDER)
       ,(FORMAT NIL "Add documentation to string according to ~S method combination.~@[~%~A~]"
                TYPE DOCUMENTATION)
       . ,(APPEND (MAPCAR #'LIST (CIRCULAR-LIST 'DECLARE) DECLARATIONS)
                  BODY))))

;;; define combined-method-documentation handlers for each type of method combination
(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :DAEMON
  "Format is --
  :BEFORE methods
    :PRIMARY method
  :AFTER methods"
  ORDER                                         ; ignored arg
  (COMBINED-DOC-STRING STRING (ASSQ :BEFORE DERIVATION) FLAVORS INDENT)
  (WHEN (OR (ASSQ :BEFORE DERIVATION) (ASSQ :AFTER DERIVATION)) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION) FLAVORS INDENT)
  (DECF INDENT 2)
  (COMBINED-DOC-STRING STRING (ASSQ :AFTER DERIVATION) (REVERSE FLAVORS) INDENT))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :DAEMON-WITH-OR
  "Format is --
  :BEFORE methods
    :OR methods
      :PRIMARY method
  :AFTER methods"
  (COMBINED-DOC-STRING STRING (ASSQ :BEFORE DERIVATION) FLAVORS INDENT)
  (WHEN (OR (ASSQ :BEFORE DERIVATION) (ASSQ :AFTER DERIVATION)) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ :OR DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER)
  (WHEN (ASSQ :OR DERIVATION) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION) FLAVORS INDENT)
  (WHEN (ASSQ :OR DERIVATION) (DECF INDENT 2))
  (DECF INDENT 2)
  (COMBINED-DOC-STRING STRING (ASSQ :AFTER DERIVATION) (REVERSE FLAVORS) INDENT))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :DAEMON-WITH-AND
  "Format is --
  :BEFORE methods
    :AND methods
      :PRIMARY method
  :AFTER methods"
  (COMBINED-DOC-STRING STRING (ASSQ :BEFORE DERIVATION) FLAVORS INDENT)
  (WHEN (OR (ASSQ :BEFORE DERIVATION) (ASSQ :AFTER DERIVATION)) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ :AND DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER)
  (WHEN (ASSQ :AND DERIVATION) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION) FLAVORS INDENT)
  (WHEN (ASSQ :AND DERIVATION) (DECF INDENT 2))
  (DECF INDENT 2)
  (COMBINED-DOC-STRING STRING (ASSQ :AFTER DERIVATION) (REVERSE FLAVORS) INDENT))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :DAEMON-WITH-OVERRIDE
  "Format is --
  :OVERRIDE methods
    :BEFORE methods
      :PRIMARY method
    :AFTER methods"
  (COMBINED-DOC-STRING STRING (ASSQ :OVERRIDE DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER)
  (WHEN (ASSQ :OVERRIDE DERIVATION) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ :BEFORE DERIVATION) FLAVORS INDENT)
  (WHEN (OR (ASSQ :BEFORE DERIVATION) (ASSQ :AFTER DERIVATION)) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION) FLAVORS INDENT)
  (DECF INDENT 2)
  (COMBINED-DOC-STRING STRING (ASSQ :AFTER DERIVATION) (REVERSE FLAVORS) INDENT))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :CASE
  "Format is --
  :OR methods
    :CASE methods
      :PRIMARY method"
  (COMBINED-DOC-STRING STRING (ASSQ :OR DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER)
  (WHEN (ASSQ :OR DERIVATION) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ :CASE DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER)
  (WHEN (ASSQ :CASE DERIVATION) (INCF INDENT 2))
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION) FLAVORS INDENT))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :PROGN
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :OR
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :AND
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :APPEND
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :NCONC
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :LIST
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :INVERSE-LIST
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :PASS-ON
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :MAX
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :MIN
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))

(DEF-COMBINED-METHOD-DOCUMENTATION-HANDLER :+
  (COMBINED-DOC-STRING STRING (ASSQ NIL DERIVATION)
                       (IF (EQ ORDER :BASE-FLAVOR-FIRST) (REVERSE FLAVORS) FLAVORS)
                       INDENT ORDER))



;Here because needs to be defined early in cold load process.
(DEFUN CLASS-SYMBOLP (SYM)
  "Returns T if SYM is a class-symbol.  Otherwise returns NIL."
  (AND (BOUNDP SYM)
       (ENTITYP (SYMEVAL SYM))
       (SUBCLASS-OF-CLASSP (CLASS (SYMEVAL SYM))
                                  CLASS-CLASS)))

;Here because needs to be defined early (in particular, to load STRUCT).
(defun complex (realpart &optional (imagpart 0))
  "Return a complex number with specified real and imaginary parts.
If both realpart imagpart are rational and the imagpart is zero,
then the returned value will not be a complex number, but just realpart."
  (declare (arglist realpart &optional (imagpart (coerce 0 (type-of realpart)))))
  (check-type realpart non-complex-number)
  (check-type imagpart non-complex-number)
  (if (and (eq imagpart 0)                              ;don't use = or zerop
           (rationalp realpart))
      realpart
    (%complex-cons (+ realpart (- imagpart imagpart)) (+ imagpart (- realpart realpart)))))
