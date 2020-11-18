; -*- Mode:LISP; Package:COMPILER; Base:8; Readtable:ZL -*-

;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFVAR FASD-TABLE-CURRENT-INDEX NIL "Allocating index for runtime fasl table")
(DEFVAR FASD-HASH-TABLE NIL "FASD time hash table")
(DEFVAR FASD-EVAL-HASH-TABLE NIL "FASD time hash table for self ref pointers")
(DEFVAR FASD-TYO-BUFFER-ARRAY nil "FASD output buffer")

(DEFVAR FASD-STREAM)

(DEFVAR FASD-PACKAGE)   ;The package in which the fasl file will presumably be loaded

;;; If this is the car of a list, the cdr is a form to be evaluated at load time
;;; The "#," reader macro uses this
(DEFVAR EVAL-AT-LOAD-TIME-MARKER (COPY-SYMBOL 'EVAL-AT-LOAD-TIME-MARKER NIL))

(PUTPROP EVAL-AT-LOAD-TIME-MARKER '(EXECUTION-CONTEXT-EVAL-WARNING) 'OPTIMIZERS)
(DEFUN EXECUTION-CONTEXT-EVAL-WARNING (FORM)
  (WARN 'LOAD-TIME-EVAL :IMPOSSIBLE "Load-time eval (#,~S) not inside quoted structure"
         (CDR FORM))
  (EVAL (CDR FORM)))

;;; If this uninterned symbol is seen as the car of a list, and the cadr of the
;;; list is a named-lambda, it will be compiled.
(DEFVAR FUNCTIONAL-CONSTANT-MARKER (COPY-SYMBOL 'FUNCTIONAL-CONSTANT-MARKER NIL))

;;; Make it the same as FUNCTION for when the interpreter or compiler sees it.
;;; Do NOT make it a displacing macro!
(FSET FUNCTIONAL-CONSTANT-MARKER
      '(MACRO LAMBDA (X) (CONS 'FUNCTION (CDR X))))

;;; This is an a-list of special markers that may exist in the car of a cons
;;; and the function to fasdump such conses.  A typical thing for such a
;;; a function to do is to call FASD-EVAL1 on some suitable form.
(DEFVAR FASD-MARKERS-ALIST
        (LIST (CONS EVAL-AT-LOAD-TIME-MARKER 'FASD-EVAL-AT-LOAD-TIME)
              (CONS FUNCTIONAL-CONSTANT-MARKER 'FASD-FUNCTIONAL-CONSTANT)))

;;; This is an a-list of area numbers and functions to fasdump conses in that
;;; area.  The function is treated just as for fasd-markers.
(DEFVAR FASD-MAGIC-AREAS-ALIST NIL)

(DEFUN FASD-NIBBLE (NUMBER)
  (WHEN (NULL (VECTOR-PUSH NUMBER FASD-TYO-BUFFER-ARRAY))
    (FASD-CLEAR-NIBBLE-BUFFER)
    (VECTOR-PUSH NUMBER FASD-TYO-BUFFER-ARRAY)))

(DEFUN FASD-CLEAR-NIBBLE-BUFFER ()
  (SEND FASD-STREAM ':STRING-OUT FASD-TYO-BUFFER-ARRAY)
  (SETF (FILL-POINTER FASD-TYO-BUFFER-ARRAY) 0))


;;;; Output the things that divide a fasl file into its major subparts

;;; Output sixbit /QFASL/ to start a fasl file.
;;; Also clears out the temp area
(DEFUN FASD-START-FILE ()
  (FASD-NIBBLE #o143150)
  (FASD-NIBBLE #o71660))

(DEFUN FASD-START-GROUP (FLAG LENGTH TYPE)
  (LET* ((OUT-LEN (LSH (MIN LENGTH #o377)
                       (- FASL-GROUP-LENGTH-SHIFT))))
    (FASD-NIBBLE (+ %FASL-GROUP-CHECK
                    (+ (IF FLAG %FASL-GROUP-FLAG 0)
                       (+ OUT-LEN
                          TYPE))))
    (AND ( LENGTH #o377)
         (FASD-NIBBLE LENGTH)))
  NIL)

(DEFUN FASD-FUNCTION-HEADER (FCTN-NAME)
  (FASD-START-GROUP NIL 0 FASL-OP-FUNCTION-HEADER)
  (FASD-CONSTANT FCTN-NAME)
  (FASD-CONSTANT 0)
  NIL)

(DEFUN FASD-FUNCTION-END ()
  (FASD-START-GROUP NIL 0 FASL-OP-FUNCTION-END)
  NIL)

(DEFUN FASD-END-WHACK ()
  (FASD-START-GROUP NIL 0 FASL-OP-END-OF-WHACK)
  ;; Reset fasd table, but not temporary areas
  (CLRHASH FASD-HASH-TABLE)
  (CLRHASH FASD-EVAL-HASH-TABLE)
  (SETQ FASD-TABLE-CURRENT-INDEX FASL-TABLE-WORKING-OFFSET))

(DEFUN FASD-END-FILE ()
  (FASD-START-GROUP NIL 0 FASL-OP-END-OF-FILE)
  (FASD-CLEAR-NIBBLE-BUFFER)
  NIL)

;;;; Given a sexp dump a group to cons up that sexp and return it

;;;  This is the main function of FASD.  It takes a Lisp object and
;;;     dumps it.  The second (optional) arg is a FASL-OP to use
;;;     on any lists in the structure.  It returns the IDX of the object.

(DEFUN FASD-CONSTANT (S-EXP &OPTIONAL (LIST-OP FASL-OP-LIST))
  (BLOCK NIL
    (AND FASD-NEW-SYMBOL-FUNCTION                       ;For FASD-SYMBOLS-PROPERTIES,
         (SYMBOLP S-EXP)                                ;make sure we examine all symbols in
         (FUNCALL FASD-NEW-SYMBOL-FUNCTION S-EXP))      ;the data that we dump.
    (LET ((TEM  (FASD-TABLE-LOOKUP S-EXP)))             ;Check if this object already dumped
      (WHEN TEM                                         ;Yup.
        (COND (( TEM (LSH 1 16.))
               (FASD-START-GROUP NIL 2 FASL-OP-LARGE-INDEX)
               (FASD-NIBBLE (LDB (byte 8. 16.) TEM))
               (FASD-NIBBLE (LDB (byte 16. 0) TEM)))
              (T
               (FASD-START-GROUP NIL 1 FASL-OP-INDEX)   ;Just reference it in the FASL TABLE.
               (FASD-NIBBLE TEM)))
            (RETURN TEM)))
    (TYPECASE S-EXP
      (INTEGER (FASD-FIXED S-EXP))
      (CHARACTER (FASD-CHARACTER S-EXP))
      (SHORT-FLOAT (FASD-SHORT-FLOAT S-EXP))
      (SINGLE-FLOAT (FASD-SINGLE-FLOAT S-EXP))
      (SYMBOL (FASD-SYMBOL S-EXP))
      (STRING (RETURN (FASD-STRING S-EXP)))
      (ARRAY (RETURN (FASD-ARRAY S-EXP)))
      (COMPILED-FUNCTION (FASD-FEF S-EXP))
      (CONS (RETURN (FASD-LIST S-EXP LIST-OP)))
      (INSTANCE (FASD-EVAL-CONSTRUCT-CONSTANT
                  (OR (SEND S-EXP ':SEND-IF-HANDLES ':FASD-FORM)
                      (and (send s-exp :get-handler-for :reconstruction-init-plist)
                           `(APPLY 'MAKE-INSTANCE
                                   '(,(TYPE-OF S-EXP)
                                     . ,(SEND S-EXP ':RECONSTRUCTION-INIT-PLIST))))
                      (ferror "The instance ~S cannot be compiled.~
~&It is an instance of a type which does not provide a way to make a fast-load representation."
                              s-exp))))
      (RATIO (RETURN (FASD-RATIONAL S-EXP)))
      (COMPLEX (RETURN (FASD-COMPLEX S-EXP)))
      (T (FERROR "The constant ~S cannot be compiled.~
~&The data-type ~S is not suitable for compiling as a fast-load constant (FASD-CONSTANT)."
                 S-EXP (TYPE-OF S-EXP))))
    (FASD-TABLE-ADD S-EXP)))

(DEFUN FASD-LIST (S-EXP LIST-OP)
  ;; Determine the size of the list, and check for special markers
  (DO ((L S-EXP (CDR L))
       (N-CONSES 0 (1+ N-CONSES))
       (MARK) (DOTTED))
      ((OR (ATOM L)
           (SETQ MARK (ASSQ (CAR L) FASD-MARKERS-ALIST))
           (AND FASD-MAGIC-AREAS-ALIST
                (SETQ MARK (ASSQ (%AREA-NUMBER L) FASD-MAGIC-AREAS-ALIST))))
       ;; Now dump that many conses and the tail if non-null
       (COND ((ZEROP N-CONSES) (FUNCALL (CDR MARK) S-EXP))
             (T (SETQ DOTTED (NOT (NULL L)))
                (FASD-START-GROUP DOTTED 1 LIST-OP)
                (FASD-NIBBLE (IF DOTTED (1+ N-CONSES) N-CONSES))
                (DO ((L1 S-EXP (CDR L1)))
                    ((EQ L1 L))
                  (FASD-CONSTANT (CAR L1) LIST-OP))
                (COND ((NOT DOTTED))
                      ((NOT MARK) (FASD-CONSTANT L))
                      (T (FUNCALL (CDR MARK) L)))
                ;; FASL-OP-LIST-COMPONENT speeds things up by not bloating the fasl
                ;; table with conses out of the middle of lists.
                (IF (= LIST-OP FASL-OP-LIST-COMPONENT)
                    FASL-EVALED-VALUE
                  (FASD-TABLE-ADD S-EXP)))))))

(DEFUN FASD-EVAL-AT-LOAD-TIME (CONS)
  (LET ((FORM (CDR CONS)))
    (IF (AND (CONSP FORM)
             (EQ (CAR FORM) 'SI::FLAVOR-VAR-SELF-REF-INDEX))
        (FASD-EVAL-MEMOIZED FORM T)
      (FASD-EVAL1 FORM))))

(DEFUN FASD-FUNCTIONAL-CONSTANT (CONS)
  (COND ((AND (CONSP (CADR CONS))
              (FUNCTIONP (CADR CONS) T))
         (IF (VARIABLE-BOUNDP COMPILER-QUEUE)
             (FERROR "Compiler is not recursive -- you will lose somehow"))
         (QC-TRANSLATE-FUNCTION (IF (ATOM (CADADR CONS))
                                    (CADADR CONS)
                                  (CAR (CADADR CONS)))
                                (CADR CONS)
                                'MACRO-COMPILE
                                'QFASL-NO-FDEFINE))
        (T (FASD-CONSTANT (CONS 'FUNCTION (CDR CONS))))))

(DEFUN FASD-SYMBOL (SYM &AUX (STARTED-FLAG NIL))
  (MULTIPLE-VALUE-BIND (PKG-OR-STRING SHARP-FLAG)
      (SI:PKG-PRINTING-PREFIX SYM FASD-PACKAGE)
    (WHEN PKG-OR-STRING
      ;; Here if need a prefix of any kind.
      (SETQ STARTED-FLAG T)
      (FASD-START-GROUP SHARP-FLAG 1 FASL-OP-PACKAGE-SYMBOL)
      ;; This nibble is 0402 if should ignore local nicknames, else 2.
      (FASD-NIBBLE
        (IF (AND (NOT (STRINGP PKG-OR-STRING))
                 (SI:ASSOC-EQUAL (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING)
                                 (DONT-OPTIMIZE (SI:PKG-REFNAME-ALIST PACKAGE))))
            #o0402
            #o0002))
      (FASD-CONSTANT (IF (STRINGP PKG-OR-STRING)
                         PKG-OR-STRING
                       (PACKAGE-PREFIX-PRINT-NAME PKG-OR-STRING))))
    (IF STARTED-FLAG
        (FASD-CONSTANT (SYMBOL-NAME SYM))       ;If there was a prefix
      (FASD-WRITE-STRING                        ;If uninterned or no prefix needed
        SYM
        FASL-OP-SYMBOL
        ;;>> this should really be sharp-flag, except that I don't want to (possibly)
        ;;>>  break things just at this moment Mly 1-Oct-85
        NIL))))

;;; This is expected to do the FASD-TABLE-ADD itself,
;;; since FASD-ARRAY has to do so.
(DEFUN FASD-STRING (STRING)
  (IF (OR (ARRAY-HAS-LEADER-P STRING)
          (> (ARRAY-LENGTH STRING) (LSH 1 16.)))
      (FASD-ARRAY STRING)
    (FASD-WRITE-STRING STRING FASL-OP-STRING NIL)
    (FASD-TABLE-ADD STRING)))

(DEFUN FASD-WRITE-STRING (OBJECT GROUP-TYPE FLAG)
  (LET* ((STRING (STRING OBJECT))
         (LENGTH (LENGTH STRING)))
    (FASD-START-GROUP FLAG (CEILING LENGTH 2) GROUP-TYPE)
    (DO ((I 0 (+ I 2))
         C0 C1)
        (( I LENGTH))
      (SETQ C0 (CHAR-INT (CHAR STRING I))
            C1 (IF (= (1+ I) LENGTH)
                   #o200
                   (CHAR-INT (CHAR STRING (1+ I)))))
      (FASD-NIBBLE (+ (LSH C1 8.) C0)))))

(DEFUN FASD-FIXED (N)
  (LET* ((ABS (ABS N))
         (LENGTH (CEILING (HAULONG ABS) 16.)))
    (FASD-START-GROUP (< N 0) LENGTH FASL-OP-FIXED)
    (DO ((POS (* 16. (1- LENGTH)) (- POS 16.))
         (C LENGTH (1- C)))
        ((ZEROP C))
      (FASD-NIBBLE (LDB (+ (LSH POS 6) 16.) ABS)))))

(DEFUN FASD-CHARACTER (N)
  (LET* ((ABS (ABS N))
         (LENGTH (CEILING (HAULONG ABS) 16.)))
    (FASD-START-GROUP (< N 0) LENGTH FASL-OP-CHARACTER)
    (DO ((POS (* 16. (1- LENGTH)) (- POS 16.))
         (C LENGTH (1- C)))
        ((ZEROP C))
      (FASD-NIBBLE (LDB (+ (LSH POS 6) 16.) ABS)))))

(DEFUN FASD-SINGLE-FLOAT (N)
  (FASD-START-GROUP NIL 3 FASL-OP-FLOAT)
  (FASD-NIBBLE (%P-LDB-OFFSET #o1013 N 0))
  (FASD-NIBBLE (DPB (%P-LDB-OFFSET #o0010 N 0) #o1010 (%P-LDB-OFFSET #o2010 N 1)))
  (FASD-NIBBLE (%P-LDB-OFFSET #o0020 N 1))
  NIL)

;; Can be replaced with %SHORT-FLOAT-EXPONENT once in system 99.
;(defsubst %QCFASD-short-float-exponent (short-float)
;  (ldb (byte 8 17.) (%pointer short-float)))

(DEFUN FASD-SHORT-FLOAT (N)
  (LET ((EXP (- (SI:%SHORT-FLOAT-EXPONENT N) #o200)))
    ;; If exponent is in range for FASL-OP-FLOAT, use it.
    (IF (OR ( #o-100 EXP #o77)
            (ZEROP N))                          ;exp is #o-200 in this case.
        (FASD-OLD-SMALL-FLOAT N)
      (FASD-NEW-SMALL-FLOAT N))))

(DEFUN FASD-OLD-SMALL-FLOAT (N)
  (SETQ N (%MAKE-POINTER DTP-FIX N))            ;So that LDB's will work.
  (UNLESS (ZEROP N)                             ;Convert excess #o200 exponent to excess #o100
    (SETQ N (%POINTER-DIFFERENCE N #o40000000)))
  (FASD-START-GROUP T 2 FASL-OP-FLOAT)
  (FASD-NIBBLE (LDB #o2010 N))
  (FASD-NIBBLE (LDB #o0020 N))
  NIL)

(DEFUN FASD-NEW-SMALL-FLOAT (N &AUX FRACTION EXPONENT)
  (FASD-START-GROUP (MINUSP N) 5 FASL-OP-NEW-FLOAT)
  (SETQ FRACTION (SI::%SHORT-FLOAT-MANTISSA N)
        EXPONENT (SI::%SHORT-FLOAT-EXPONENT N))
  (FASD-NIBBLE 8.)                              ;8 bits for exponent, including sign
  (FASD-NIBBLE EXPONENT)
  (FASD-NIBBLE 17.)                             ;17 bits for mantissa, excluding sign
  (FASD-NIBBLE (LDB (BYTE 16. 0) FRACTION))     ;exclude sign
  (FASD-NIBBLE 1))                              ;implied leading digit

(DEFUN FASD-RATIONAL (RAT)
  (FASD-START-GROUP NIL 0 FASL-OP-RATIONAL)
  (FASD-CONSTANT (NUMERATOR RAT))
  (FASD-CONSTANT (DENOMINATOR RAT))
  (FASD-TABLE-ADD RAT))

(DEFUN FASD-COMPLEX (COMPLEX)
  (FASD-START-GROUP NIL 0 FASL-OP-COMPLEX)
  (FASD-CONSTANT (REALPART COMPLEX))
  (FASD-CONSTANT (IMAGPART COMPLEX))
  (FASD-TABLE-ADD COMPLEX))

(DEFUN FASD-FEF (FEF)
  (LET* ((Q-COUNT (%STRUCTURE-BOXED-SIZE FEF))
         (NON-Q-COUNT (- (%STRUCTURE-TOTAL-SIZE FEF) Q-COUNT)))
    (FASD-START-GROUP NIL 3 FASL-OP-FRAME)
    (FASD-NIBBLE Q-COUNT)
    (FASD-NIBBLE NON-Q-COUNT)
    (FASD-NIBBLE (+ Q-COUNT (LSH NON-Q-COUNT 1)))
    (DO ((I 0 (1+ I)))
        ((= I Q-COUNT))
      (FASD-FEF-Q FEF I))
    (DO ((I Q-COUNT (1+ I)))
        ((= I (+ Q-COUNT NON-Q-COUNT)))
      (FASD-NIBBLE (%P-LDB-OFFSET %%Q-LOW-HALF FEF I))
      (FASD-NIBBLE (%P-LDB-OFFSET %%Q-HIGH-HALF FEF I))))
  NIL)

;when we change this, change also
; FASD-ATTRIBUTES-LIST and COMPILE-STREAM

(defun map-to-old-cdr-code (new-cdr-code)
  (setq new-cdr-code (ldb (byte 2 0) new-cdr-code))
  (cond ((= cdr-next 0)
         (nth new-cdr-code '(3 1 0 2)))
        (t
         new-cdr-code)))

(DEFUN FASD-FEF-Q (FEF I &AUX DATTP PTR PTR1 OFFSET (TYPE 0))
  (SETQ DATTP (%P-LDB-OFFSET %%Q-DATA-TYPE FEF I))
  (SETQ TYPE (LSH (map-to-old-cdr-code (%P-LDB-OFFSET %%Q-CDR-CODE FEF I)) 6))
  (COND ((OR (= DATTP DTP-ONE-Q-FORWARD)
             (= DATTP DTP-LOCATIVE))
         (SETQ PTR1 (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF I))
         (SETQ PTR (%FIND-STRUCTURE-HEADER PTR1))
         (SETQ OFFSET (%POINTER-DIFFERENCE PTR1 PTR))
         (AND (> OFFSET #o17)
              (FERROR "~O is too great an offset into atom while fasdumping FEF ~S"
                      OFFSET (%P-CONTENTS-OFFSET FEF %FEFHI-FCTN-NAME)))
         (FASD-CONSTANT PTR)
         (AND (= DATTP DTP-ONE-Q-FORWARD)
              (SETQ TYPE (+ TYPE 20)))
         (AND (= DATTP DTP-LOCATIVE)
              (SETQ TYPE (+ TYPE 400)))
         ;; LOW 4 BITS OF TYPE ARE OFFSET TO ADD TO POINTER TO MAKE IT POINT AT VALUE CELL, ETC.
         (SETQ TYPE (+ TYPE OFFSET)))
        ((= DATTP DTP-HEADER)
         (FASD-CONSTANT (%P-LDB-OFFSET %%Q-POINTER FEF I)))
        ((= DATTP DTP-SELF-REF-POINTER)
         (INCF TYPE 1000)
         (MULTIPLE-VALUE-BIND (SYMBOL FLAG)
             (SI:FLAVOR-DECODE-SELF-REF-POINTER (SI:FEF-FLAVOR-NAME FEF)
                                                (%P-LDB-OFFSET %%Q-POINTER FEF I))
           (FASD-EVAL1 `(SI:FLAVOR-VAR-SELF-REF-INDEX
                          ',(IF FLAG
                                `(,(SI:FEF-FLAVOR-NAME FEF)
                                  T ,SYMBOL)
                              `(,(SI:FEF-FLAVOR-NAME FEF) ,SYMBOL))))))
        (T (FASD-CONSTANT (%P-CONTENTS-OFFSET FEF I))))
  (FASD-NIBBLE TYPE))

;;; Does its own fasd-table adding since it has to be done in the middle
;;; of this function, after the fasl-op-array but before the initialization data.
(DEFUN FASD-ARRAY (ARRAY &AUX SIZE OBJECTIVE-P FAKE-ARRAY RETVAL NSP DIMS)
  (SETQ NSP (NAMED-STRUCTURE-P ARRAY)
        DIMS (ARRAY-DIMENSIONS ARRAY)
        SIZE (APPLY #'* DIMS)
        OBJECTIVE-P (NULL (CDR (ASSQ (ARRAY-TYPE ARRAY) ARRAY-BITS-PER-ELEMENT))))
  (WHEN (NOT OBJECTIVE-P)
    (LET ((EPQ (CDR (ASSQ (ARRAY-TYPE ARRAY) ARRAY-ELEMENTS-PER-Q))))
      ;; In this case, number of halfwords
      (SETQ SIZE (IF (PLUSP EPQ) (CEILING (* SIZE 2) EPQ) (* SIZE 2 (MINUS EPQ))))))
  (FASD-START-GROUP NIL 0 (IF OBJECTIVE-P FASL-OP-INITIALIZE-ARRAY
                                          FASL-OP-INITIALIZE-NUMERIC-ARRAY))
  (FASD-START-GROUP NSP 0 FASL-OP-ARRAY)
  ;; Area. Don't lose on arrays in QCOMPILE-TEMPORARY-AREA.
  (FASD-CONSTANT NIL)
  ;; Type symbol
  (FASD-CONSTANT (ARRAY-TYPE ARRAY))
  ;; Dimensions
  (FASD-CONSTANT DIMS FASL-OP-TEMP-LIST)
  ;; Displaced-p. For now
  (FASD-CONSTANT NIL)
  ;; Leader
  (FASD-CONSTANT
    (IF (ARRAY-HAS-LEADER-P ARRAY)
        (DO ((I 0 (1+ I))
             (LIST NIL)
             (LIM (ARRAY-LEADER-LENGTH ARRAY)))
            (( I LIM) LIST)
          (PUSH (ARRAY-LEADER ARRAY I) LIST))
      NIL)
    FASL-OP-TEMP-LIST)
  ;; Index-offset. For now
  (FASD-CONSTANT NIL)
  ;; Named-structure-p
  (AND NSP (FASD-CONSTANT T))
  ;; Now that six values have been given, the group is over.
  (SETQ RETVAL (FASD-TABLE-ADD ARRAY))
  ;; Next, continue to initialize the array.
  (FASD-CONSTANT SIZE)
  (SETQ FAKE-ARRAY
        (MAKE-ARRAY SIZE ':TYPE (IF OBJECTIVE-P 'ART-Q 'ART-16B) ':DISPLACED-TO ARRAY))
  (IF OBJECTIVE-P
      (DOTIMES (I SIZE)
        (IF (LOCATION-BOUNDP (AP-1-FORCE ARRAY I))
            (FASD-CONSTANT (AREF FAKE-ARRAY I))
          (FASD-NIBBLE (+ %FASL-GROUP-CHECK FASL-OP-NULL-ARRAY-ELEMENT))))
    (DOTIMES (I SIZE)
      (FASD-NIBBLE (AREF FAKE-ARRAY I))))
  ;(RETURN-ARRAY (PROG1 FAKE-ARRAY (SETQ FAKE-ARRAY NIL)))
  RETVAL)

;;;; Low level routines to dump groups to deposit things in various places

(DEFUN FASD-SET-PARAMETER (PARAM VAL)
  (declare (ignore param val))
  (ferror "The function FASD-SET-PARAMETER is obsolete; please send a bug report.")
; (PROG (C-VAL)
;       (COND ((NULL (SETQ C-VAL (ASSQ PARAM FASD-TABLE)))
;              (FERROR "~S is an unknown FASL parameter" PARAM)))
;       (COND ((EQUAL VAL (CDR C-VAL))(RETURN NIL)))
;       (FASD-START-GROUP NIL 0 FASL-OP-SET-PARAMETER)
;       (FASD-CONSTANT PARAM)
;       (FASD-CONSTANT VAL)))
  )

(DEFUN FASD-STORE-ARRAY-LEADER (VALUE ARRAY SUBSCR)
  (FASD-START-GROUP NIL 3 FASL-OP-STOREIN-ARRAY-LEADER)
  (FASD-NIBBLE ARRAY)
  (FASD-NIBBLE SUBSCR)
  (FASD-NIBBLE VALUE)                           ;NOTE: Nibbles not in same order as
  0)                                            ; STORE-ARRAY-LEADER!


(DEFUN FASD-STORE-FUNCTION-CELL (SYM IDX)
  ;; IDX an fasd-table index that has stuff desired to store.
  (FASD-START-GROUP NIL 1 FASL-OP-STOREIN-FUNCTION-CELL)
  (FASD-NIBBLE IDX)
  (FASD-CONSTANT SYM)
  0)

(DEFUN FASD-STORE-VALUE-CELL (SYM IDX)
  (FASD-START-GROUP NIL 1 FASL-OP-STOREIN-SYMBOL-VALUE)
  (FASD-NIBBLE IDX)
  (FASD-CONSTANT SYM)
  0)
(DEFF FASD-STOREIN-FUNCTION-CELL 'FASD-STORE-FUNCTION-CELL)

(DEFUN FASD-STORE-PROPERTY-CELL (SYM IDX)
  (FASD-START-GROUP NIL 1 FASL-OP-STOREIN-PROPERTY-CELL)
  (FASD-NIBBLE IDX)
  (FASD-CONSTANT SYM)
  0)

(DEFUN FASD-FILE-PROPERTY-LIST (PLIST)
  (FASD-ATTRIBUTES-LIST PLIST NIL))

;;; NOTE: This SETQ's FASD-PACKAGE if a package is specified in PLIST
(DEFUN FASD-ATTRIBUTES-LIST (PLIST &OPTIONAL (ADD-FASD-DATA T))
  (WHEN ADD-FASD-DATA
    (MULTIPLE-VALUE-BIND (MAJOR MINOR)
        (SI:GET-SYSTEM-VERSION "System")
      (SETQ PLIST (LIST* ':FASD-DATA
                         `(,USER-ID
                           ,SI:LOCAL-PRETTY-HOST-NAME
                           ,(TIME:GET-UNIVERSAL-TIME)
                           ,MAJOR ,MINOR
                           (NEW-DESTINATIONS T   ;; NOT :new-destinations!!
                ;add this when we change FASD-FEF-Q
                ;           new-cdr-codes ,(eq sys:cdr-next 0)
                            :SITE ,(SHORT-SITE-NAME)))
                         PLIST))))
  (LET ((P (GETL (LOCF PLIST) '(:PACKAGE))))
    (WHEN P
      (SETQ FASD-PACKAGE (PKG-FIND-PACKAGE (CADR P)))))
  (FASD-START-GROUP NIL 0 FASL-OP-FILE-PROPERTY-LIST)
  ;; Put package prefixes on everything in the plist since it will be loaded in
  ;; the wrong package.  This way the symbols in the plist will always be loaded
  ;; into exactly the same package they were dumped from, while the rest of the
  ;; symbols in the file will be free to follow the usual rules for intern.
  (LET ((FASD-PACKAGE NIL))
    (FASD-CONSTANT PLIST)))

;;; The old way of doing EVAL (FASD-EVAL) unfortunately does not nest properly,
;;;  ie Cannot be used to load into a FEF, because the loader is expecting to see
;;;  a single next-value.  So this is the way it probably should have been done in
;;;  the first place..
(DEFUN FASD-EVAL1 (SEXP &OPTIONAL TEMPORARY)
  (FASD-START-GROUP NIL 0 FASL-OP-EVAL1)
  (FASD-CONSTANT SEXP (IF TEMPORARY FASL-OP-TEMP-LIST FASL-OP-LIST))
  ;(FASD-TABLE-ADD FASD-TABLE-IGNORE)
  (FASD-TABLE-NEXT-INDEX))

(DEFUN FASD-EVAL-CONSTRUCT-CONSTANT (SEXP)
  "Fasdump a group to eval FORM, but let our caller record it in the fasd table.
He will record the index we use under the object that FORM
is supposed to reconstruct at load time."
  (FASD-START-GROUP NIL 0 FASL-OP-EVAL1)
  (FASD-CONSTANT SEXP))

(DEFUN FASD-EVAL-MEMOIZED (FORM &OPTIONAL TEMPORARY &AUX TEM)
  (COND ((SETQ TEM (FASD-EVAL-TABLE-LOOKUP FORM))       ;If this object already dumped,
         (COND (( TEM (LSH 1 16.))
                (FASD-START-GROUP NIL 2 FASL-OP-LARGE-INDEX)
                (FASD-NIBBLE (LDB (byte 8. 16.) TEM))
                (FASD-NIBBLE (LDB (byte 16. 0) TEM)))
               (T
                (FASD-START-GROUP NIL 1 FASL-OP-INDEX)  ;Just reference it in the FASL table.
                (FASD-NIBBLE TEM)))
         TEM)
        (T (LET ((INDEX (FASD-EVAL1 FORM TEMPORARY)))
             (FASD-EVAL-TABLE-ADD FORM INDEX)
             INDEX))))
 
;;;; Routines to manipulate the FASD table

;;; FASD simulates keeping a table that looks just like the one FASLOAD will keep.
;;; FASD uses it to refer back to atoms which have been seen before,
;;; so that no atom need be interned twice.

(defun fasd-table-next-index nil
  (prog1 fasd-table-current-index
         (setq fasd-table-current-index (1+ fasd-table-current-index))))

(defun fasd-table-add (data)
  (let ((index (fasd-table-next-index)))
    (puthash data index fasd-hash-table)
    index))

(defun fasd-table-lookup (data)
  (cond ((numberp data) nil)
        (t (gethash data fasd-hash-table))))

;;; The EVAL hash table is used to record data constructed by evaluations at load time,
;;; in case we want to reuse the data instead of computing them twice.
(DEFUN FASD-EVAL-TABLE-LOOKUP (DATA)
  (GETHASH DATA FASD-EVAL-HASH-TABLE))

(DEFUN FASD-EVAL-TABLE-ADD (DATA INDEX)
  (PUTHASH DATA INDEX FASD-EVAL-HASH-TABLE))

;;; Set one of the parameters at the front of the FASD-TABLE, as in
;;; (FASD-TABLE-SET FASL-SYMBOL-STRING-AREA PN-STRING)
(DEFUN FASD-TABLE-SET (PARAM DATA)
  (declare (ignore param data))
  (ferror "The function FASD-TABLE-SET is obsolete; please send a bug report.")
; (AS-1 DATA FASD-TABLE PARAM)
 )

(DEFUN FASD-TABLE-LENGTH ()
  FASD-TABLE-CURRENT-INDEX)

(DEFUN FASD-INITIALIZE (&AUX SI:FASL-TABLE)
  (UNLESS FASD-TYO-BUFFER-ARRAY
    (FERROR "~S must be called inside ~S"
            'FASD-INITIALIZE 'LOCKING-RESOURCES))
  (SETQ FASD-NEW-SYMBOL-FUNCTION NIL)
  (SETQ FASD-PACKAGE PACKAGE)
  (SETQ FASD-TABLE-CURRENT-INDEX FASL-TABLE-WORKING-OFFSET)
  (SETF (FILL-POINTER FASD-TYO-BUFFER-ARRAY) 0))

;;;; Dump forms to be evaluated with hair for defun and setq

;;; Dump a group to evaluate a given form and return its value.
;;; If OPTIMIZE is set, SETQ and DEFUN are handled specially,
;;; in a way appropriate for the top level of fasdump or qc-file.
(DEFUN FASD-FORM (FORM &OPTIONAL OPTIMIZE)
  "Put something to execute FORM into the QFASL file being written.
If OPTIMIZE is true, many common types of forms are handled specially,
including SETQ, DEFF, DEFUN, etc.  In particular, (DEFUN FOO)
is processed by dumping FOO's current function definition."
  (COND ((OR (MEMQ FORM '(T NIL))
             (STRINGP FORM)
             (NUMBERP FORM))
         (FASD-CONSTANT FORM))
        ((SYMBOLP FORM) (FASD-SYMEVAL FORM))
        ((ATOM FORM) (FASD-RANDOM-FORM FORM))
        ((NOT (SYMBOLP (CAR FORM))) (FASD-RANDOM-FORM FORM))
        ((EQ (CAR FORM) 'QUOTE)
         (FASD-CONSTANT (CADR FORM)))
        ((NOT OPTIMIZE)
         (FASD-RANDOM-FORM FORM))
        ((EQ (CAR FORM) 'SETQ)
         (FASD-SETQ FORM))
        ((EQ (CAR FORM) 'DEFF)
         (FASD-STORE-FUNCTION-CELL (CADR FORM) (FASD-FORM (CADDR FORM))))
        ((AND (EQ (CAR FORM) 'FSET-CAREFULLY)
              (CONSP (CADR FORM))
              (EQ (CAADR FORM) 'QUOTE))
         (FASD-STORE-FUNCTION-CELL (CADADR FORM) (FASD-FORM (CADDR FORM))))
        ((EQ (CAR FORM) 'DEFUN)
         (FASD-FUNCTION (CADR FORM)
                        (FDEFINITION (SI:UNENCAPSULATE-FUNCTION-SPEC (CADR FORM)))))
        (T (FASD-RANDOM-FORM FORM))))

;(DEFUN FASD-DECLARATION (DCL)
;    (AND (MEMQ (CAR DCL) '(SPECIAL UNSPECIAL)
;         (FASD-FORM DCL)))

;;; Dump something to eval some random form (which is the argument).
(DEFUN FASD-RANDOM-FORM (FRM)
  (FASD-EVAL1 FRM))

;;; Given the body of a DEFUN, dump stuff to perform it.
(DEFUN FASD-FUNCTION (FUNCTION DEFINITION)
  (FASD-STORE-FUNCTION-CELL FUNCTION (FASD-CONSTANT DEFINITION)))

;;; Given the body of a SETQ, dump stuff to perform it.
(DEFUN FASD-SETQ (SETQ-FORM)
  (DO ((PAIRS (CDR SETQ-FORM) (CDDR PAIRS)))
      ((NULL PAIRS))
    (CHECK-ARG PAIRS (ATOM (CAR PAIRS)) "a SETQ form")
    (FASD-STORE-VALUE-CELL (CAR PAIRS) (FASD-FORM (CADR PAIRS)))))

(DEFUN FASD-SYMEVAL (SEXP)
  (FASD-START-GROUP NIL 0 FASL-OP-FETCH-SYMBOL-VALUE)
  (FASD-CONSTANT SEXP)
  ;(FASD-TABLE-ADD FASD-TABLE-IGNORE)
  (FASD-TABLE-NEXT-INDEX))

(DEFUN FASD-SYMBOL-VALUE (FILENAME SYMBOL &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME containing SYMBOL's value.
Loading the file will set the symbol back to the same value."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME
                                                           FS:LOAD-PATHNAME-DEFAULTS
                                                           ':QFASL)
                               ':DIRECTION ':OUTPUT
                               ':CHARACTERS NIL
                               ':BYTE-SIZE 16.)
    (LET ((FASD-PACKAGE NIL))                   ;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
        (FASD-INITIALIZE)
        (FASD-START-FILE)
        (FASD-ATTRIBUTES-LIST
          (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
              ATTRIBUTE-LIST
            (LIST* ':PACKAGE (PACKAGE-NAME (SYMBOL-PACKAGE SYMBOL)) ATTRIBUTE-LIST)))
        (FASD-FORM `(SETF (SYMBOL-VALUE ',SYMBOL) ',(SYMBOL-VALUE SYMBOL)))
        (FASD-END-WHACK)
        (FASD-END-FILE)))))

;; Copied from LAD: RELEASE-3.SYS; QCFASD.LISP#258 on 2-Oct-86 05:07:32
(defun dump-forms-to-fasd-stream (fasd-stream forms-list)
  "Dump forms to a fasd-stream only within a with-open-fasd-file form."
  (dolist (form forms-list)
    (if ( (fasd-table-length) qc-file-whack-threshold)
        (fasd-end-whack))
    (fasd-form form)))

;; Copied from LAD: RELEASE-3.SYS; QCFASD.LISP#258 on 2-Oct-86 05:07:32
(defun open-fasd-file (filename)
  (open (fs:merge-pathname-defaults
          filename fs:load-pathname-defaults ':qfasl)
        ':direction ':output
        ':characters nil
        ':byte-size 16.))

;; Copied from LAD: RELEASE-3.SYS; QCFASD.LISP#258 on 2-Oct-86 05:07:33
(defmacro with-open-fasd-file ((stream-variable filename &optional attribute-list) &body body)
  "Open filename and bind stream-variable to a fasd-stream.
No output operations should be performed on the fasd-stream except as a side-affect of
invoking dump-forms-to-fasd-stream."
  (once-only (attribute-list)
    `(with-open-stream (,stream-variable (open-fasd-file ,filename))
       (let ((fasd-stream ,stream-variable))
         (let ((fasd-package nil))              ;in case fasd-attributes-list bashes it
           (locking-resources
             (fasd-initialize)
             (fasd-start-file)
             (fasd-attributes-list
               (if (getl (locf ,attribute-list) '(:package))
                   ,attribute-list
                 (list* ':package ':user ,attribute-list)))
             ,@body
             (fasd-end-whack)
             (fasd-end-file)))))))


;; Copied from LAD: RELEASE-3.SYS; QCFASD.LISP#258 on 2-Oct-86 05:07:34
(DEFUN DUMP-FORMS-TO-FILE (FILENAME FORMS-LIST &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME which, when loaded, will execute the forms in FORMS-LIST.
ATTRIBUTE-LIST is a file attribute list which controls, among other things,
what package the file is dumped and loaded in (default is USER)."
  (with-open-fasd-file (stream filename attribute-list)
    (dump-forms-to-fasd-stream stream forms-list)))

(DEFUN FASD-FONT (FONT-SYMBOL)
  "Write the font FONT into a QFASL file named SYS: FONTS; name-of-font QFASL."
  (DUMP-FORMS-TO-FILE (FS:MAKE-PATHNAME ':HOST "SYS"
                                        ':DIRECTORY "FONTS"
                                        ':NAME (SYMBOL-NAME FONT-SYMBOL))
                      `((proclaim (special ,font-symbol))
                        (SETQ ,FONT-SYMBOL ,(TV::FONT-EVALUATE FONT-SYMBOL)))
                      '(:PACKAGE :FONTS)))

;; Copied from LAD: RELEASE-3.SYS; QCFASD.LISP#258 on 2-Oct-86 05:07:34
(DEFUN FASD-FILE-SYMBOLS-PROPERTIES (FILENAME SYMBOLS PROPERTIES
                                              DUMP-VALUES-P DUMP-FUNCTIONS-P
                                              NEW-SYMBOL-FUNCTION
                                              &OPTIONAL ATTRIBUTE-LIST)
  "Write a QFASL file named FILENAME containing data on SYMBOLS.
The data can include the symbols' values, function definitions, and properties.
PROPERTIES is a list of which properties should be dumped.
DUMP-VALUES-P says whether to dump their values.
DUMP-FUNCTIONS-P says whether to dump their function definitions.
NEW-SYMBOL-FUNCTION is a function to call whenever a new symbol
not previously seen is found in a value being dumped.  The function
can cause the new symbol's data to be dumped like the specified symbols.
When the NEW-SYMBOL-FUNCTION is called, FASD-SYMBOL-LIST will be a list
of symbols waiting to be dumped, and FASD-ALREADY-DUMPED-SYMBOL-LIST a
list of those already dumped.  To make a new symbol be dumped, push it
on the former if it is not in either of those two."
  (WITH-OPEN-FILE (FASD-STREAM (FS:MERGE-PATHNAME-DEFAULTS FILENAME
                                                           FS:LOAD-PATHNAME-DEFAULTS
                                                           ':QFASL)
                               ':DIRECTION ':OUTPUT
                               ':CHARACTERS NIL
                               ':BYTE-SIZE 16.)
    (LET ((FASD-PACKAGE NIL))                   ;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
        (FASD-INITIALIZE)
        (FASD-START-FILE)
        (FASD-ATTRIBUTES-LIST
          (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
              ATTRIBUTE-LIST
            (LIST* ':PACKAGE ':USER ATTRIBUTE-LIST)))
        (FASD-SYMBOLS-PROPERTIES SYMBOLS PROPERTIES DUMP-VALUES-P
                                 DUMP-FUNCTIONS-P NEW-SYMBOL-FUNCTION)
        (FASD-END-WHACK)
        (FASD-END-FILE)))))

(DEFVAR FASD-SYMBOL-LIST)
(DEFVAR FASD-ALREADY-DUMPED-SYMBOL-LIST)
(DEFVAR FASD-NEW-SYMBOL-FUNCTION)

;;; Take each symbol in SYMBOLS and do a FASD-SYMBOL-PROPERTIES on it.
;;; The symbols already thus dumped are put on FASD-ALREADY-DUMPED-SYMBOL-LIST.
;;; The NEW-SYMBOL-FUNCTION can add more symbols to FASD-SYMBOL-LIST
;;; to cause them to be dumped as well.
(DEFUN FASD-SYMBOLS-PROPERTIES (SYMBOLS PROPERTIES DUMP-VALUES
                                        DUMP-FUNCTIONS NEW-SYMBOL-FUNCTION)
  (DO ((FASD-SYMBOL-LIST SYMBOLS)
       (FASD-ALREADY-DUMPED-SYMBOL-LIST)
       (SYMBOL))
      ((NULL FASD-SYMBOL-LIST))
    (SETQ SYMBOL (POP FASD-SYMBOL-LIST))
    (PUSH SYMBOL FASD-ALREADY-DUMPED-SYMBOL-LIST)
    (FASD-SYMBOL-PROPERTIES SYMBOL PROPERTIES
                            DUMP-VALUES DUMP-FUNCTIONS NEW-SYMBOL-FUNCTION)))

;;; Dump into the FASD file the properties of SYMBOL in PROPERTIES,
;;; and the value if DUMP-VALUES, and the function cell if DUMP-FUNCTIONS.
;;; NEW-SYMBOL-FUNCTION will be called on appropriate symbols in the
;;; structures which are dumped.
(DEFUN FASD-SYMBOL-PROPERTIES (SYMBOL PROPERTIES DUMP-VALUES
                                      DUMP-FUNCTIONS NEW-SYMBOL-FUNCTION)
  (WHEN (AND DUMP-VALUES (BOUNDP SYMBOL))
    (FASD-STORE-VALUE-CELL SYMBOL (FASD-CONSTANT-TRACING-SYMBOLS (SYMBOL-VALUE SYMBOL)
                                                                 NEW-SYMBOL-FUNCTION)))
  (WHEN (AND DUMP-FUNCTIONS (FBOUNDP SYMBOL))
    (FASD-STORE-FUNCTION-CELL SYMBOL (FASD-CONSTANT-TRACING-SYMBOLS (SYMBOL-FUNCTION SYMBOL)
                                                                 NEW-SYMBOL-FUNCTION)))
  (MAPC #'(LAMBDA (PROP &AUX (TEM (GET SYMBOL PROP)))
            ;; If this atom has this property, dump a DEFPROP to be evaled.
            (WHEN TEM
              (FASD-START-GROUP NIL 0 FASL-OP-EVAL1)
              (PROGN
                (FASD-START-GROUP NIL 1 FASL-OP-LIST)
                (FASD-NIBBLE 4)                 ;4 is length of the DEFPROP form.
                (FASD-CONSTANT 'DEFPROP)        ;Don't use FASD-FORM, since we want to detect
                (FASD-CONSTANT SYMBOL)          ; new symbols in the value of the property.
                (FASD-CONSTANT-TRACING-SYMBOLS TEM NEW-SYMBOL-FUNCTION)
                (FASD-CONSTANT PROP)
                (FASD-TABLE-NEXT-INDEX))
              (FASD-TABLE-NEXT-INDEX)))
        PROPERTIES))

(DEFUN FASD-CONSTANT-TRACING-SYMBOLS (OBJECT FASD-NEW-SYMBOL-FUNCTION)
  (FASD-CONSTANT OBJECT))

;;; Use this as the NEW-SYMBOL-FUNCTION, for nice results:
;;; All the substructures of the structures being dumped are also dumped.
(DEFUN FASD-SYMBOL-PUSH (SYMBOL)
  (OR (MEMQ SYMBOL FASD-SYMBOL-LIST)
      (MEMQ SYMBOL FASD-ALREADY-DUMPED-SYMBOL-LIST)
      (PUSH SYMBOL FASD-SYMBOL-LIST)))
