;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Cold-Load:T; Base:8; Readtable:ZL -*-

;;; (c) Copyright 1980 Massachusetts Institute of Technology

;;; This file contains random functions which must be in the cold load


(DEFUN IGNORE (&REST IGNORE)
  "Discard any number of arguments and return NIL."
  NIL)

;;; Note: this used to be done with an explicit SETQ so that it would happen
;;; inside the cold-load generator rather than as part of LISP-CRASH-LIST.
;;; However, now it should happen inside the cold load generator anyway.
;;; +++ This will need to be looked at when this code really runs on the FALCON <03-Nov-88 wkf>
#+(target lambda)
(DEFVAR AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA
  "Area for consing property lists of interned symbols in.")

;;; This is now microcoded.
;(DEFSUBST PACKAGE-CELL-LOCATION (SYMBOL)
;  "Return a locative pointing to the cell in which SYMBOL's package is stored."
;  (%MAKE-POINTER-OFFSET DTP-LOCATIVE SYMBOL SYMBOL-PACKAGE-OFFSET))

(DEFUN PLIST (OBJECT)
  "Return the contents of the property list of SYMBOL.
SYMBOL may be a symbol, an instance supporting the :PROPERTY-LIST operation,
or a locative or cons cell whose cdr is the property list."
  (ETYPECASE OBJECT
    (SYMBOL (SYMBOL-PLIST OBJECT))
    ((OR INSTANCE NAMED-STRUCTURE) (SEND OBJECT :PROPERTY-LIST))
    (LIST (CDR OBJECT))
    (LOCATIVE (CONTENTS OBJECT))))

(DEFUN SETPLIST (SYMBOL L)
  "Set the property list of SYMBOL to be L (a list of alternating properties and values).
SYMBOL may be an instance that handles the :SET-PROPERTY-LIST operation, instead of a symbol."
  (ETYPECASE SYMBOL
    (SYMBOL
     (SETF (SYMBOL-PLIST SYMBOL) L))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND SYMBOL :SET-PROPERTY-LIST L)))
  L)

;; used by (locf (plist ...))
(DEFUN PLIST-LOCATION (OBJECT-WITH-PROPERTY-LIST)
  (ETYPECASE OBJECT-WITH-PROPERTY-LIST
    (SYMBOL (LOCF (SYMBOL-PLIST OBJECT-WITH-PROPERTY-LIST)))
    ((OR CONS LOCATIVE) (LOCF (CDR OBJECT-WITH-PROPERTY-LIST)))
    ((OR INSTANCE NAMED-STRUCTURE) (SEND OBJECT-WITH-PROPERTY-LIST :PROPERTY-LIST-LOCATION))))

(DEFUN GET (SYMBOL-OR-PLIST PROPERTY &OPTIONAL DEFAULT)
  "Returns the value of SYMBOL-OR-PLIST's PROPERTY property.
If there is no property, DEFAULT is returned.
SYMBOL-OR-PLIST may be a symbol or a disembodied property list -
a list or locative whose cdr stores the properties.
It may also be an instance or named structure; then its :GET method is used."
  #+(target lambda)
  (GET SYMBOL-OR-PLIST PROPERTY DEFAULT)
  #-(target lambda)
  ;; $$$ removed symbol: <04-Nov-88 JIM>
  (let ((place (if (symbolp symbol-or-plist)
                   ;; $$$ Added symbol: <04-Nov-88 JIM>
                   (symbol:%symbol-plist symbol-or-plist)
                 (cdr symbol-or-plist))))       ; +++ Handle instances and named-structures. <27-Oct-88 wkf>
    ;; $$$ Removed symbol: <04-Nov-88 JIM>
    (getf place property default)))

#+(target lambda)
(defun getf (place property &optional (default nil))
  "Returns the PROPERTY property from the plist stored in PLACE.
If there is no such property, DEFAULT is returned.
PLACE should be such that simply evaluating it would return
the contents of the property list."
  (getf place property default))

#+(target lambda)
(defun get-properties (place list-of-properties)
  "Finds the first property of any in LIST-OF-PROPERTIES from the plist PLACE.
The first value is the property found first,
the second is the value of that property,
the third is the tail of the plist, whose car is the property
and whose cadr is the value.
PLACE should be such that simply evaluating it would return
the contents of the property list."
  (get-properties-internal (locf place) list-of-properties))

#+(target lambda)
;; GET-PROPERTIES optimizes into this.
(DEFUN GET-PROPERTIES-INTERNAL (LOCATION LIST-OF-PROPERTIES)
  (LET ((TEM (GETL LOCATION LIST-OF-PROPERTIES)))
    (IF TEM
        (VALUES (CAR TEM) (CADR TEM) TEM)
      NIL)))

(DEFUN PUTPROP (SYMBOL-OR-PLIST VALUE PROPERTY)
  "Make the value of SYMBOL-OR-PLIST's PROPERTY property be VALUE.
SYMBOL-OR-PLIST may be a symbol or a disembodied property list -
a list or locative whose cdr stores the properties.
It may also be an instance or named structure; then its :PUTPROP method is used.
VALUE is returned."
; (UNLESS (VARIABLE-BOUNDP AREA-FOR-PROPERTY-LISTS)
;   (SETQ AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA))
  #+(target lambda)
  (progn (ETYPECASE SYMBOL-OR-PLIST
           ((OR SYMBOL CONS LOCATIVE)
            (WITHOUT-INTERRUPTS
              (LET ((PLLOC (GET-LOCATION-OR-NIL SYMBOL-OR-PLIST PROPERTY)))
                (IF PLLOC
                    (SETF (CAR PLLOC) VALUE)
                  (progn
                    (SETQ PLLOC (IF (SYMBOLP SYMBOL-OR-PLIST)
                                    (LOCF (SYMBOL-PLIST SYMBOL-OR-PLIST))
                                  SYMBOL-OR-PLIST))
                    (RPLACD PLLOC (LIST*-IN-AREA (IF (= (%AREA-NUMBER SYMBOL-OR-PLIST) NR-SYM)
                                                     PROPERTY-LIST-AREA
                                                   BACKGROUND-CONS-AREA)
                                                 PROPERTY VALUE (CDR PLLOC))))))))
           ((OR INSTANCE NAMED-STRUCTURE)
            (SEND SYMBOL-OR-PLIST :PUTPROP VALUE PROPERTY)))
         VALUE)
  ;; $$$ Made a mistake and made my comments with &&& changed them to $$$ <03-Nov-88 JIM>
  #-(target lambda)
  ;; $$$ Removed the trap: infront of the without interrrupts <03-Nov-88 JIM>
  (without-interrupts
    ;; $$$ Removed the symbol: <03-Nov-88 JIM>
    (if (symbolp symbol-or-plist)
        ;; +++ This symbol: should be removed... <03-Nov-88 JIM>
        (setf (symbol:%get symbol-or-plist) value)
      ;; $$$ Removed the symbol: <03-Nov-88 JIM>
      (setf (getf (cdr symbol-or-plist)) value))))      ; +++ Handle instances and named-structures. <28-Oct-88 wkf>

(DEFUN PUTPROP-IN-AREA (SYMBOL-OR-PLIST VALUE PROPERTY area)
  "Make the value of SYMBOL-OR-PLIST's PROPERTY property be VALUE.
SYMBOL-OR-PLIST may be a symbol or a disembodied property list -
a list or locative whose cdr stores the properties."
  #+(target lambda)
  (progn (ETYPECASE SYMBOL-OR-PLIST
           ((OR SYMBOL CONS LOCATIVE)
            (WITHOUT-INTERRUPTS
              (LET ((PLLOC (sys:GET-LOCATION-OR-NIL SYMBOL-OR-PLIST PROPERTY)))
                (IF PLLOC
                    (SETF (CAR PLLOC) VALUE)
                  (progn
                    (SETQ PLLOC (IF (SYMBOLP SYMBOL-OR-PLIST)
                                    (LOCF (SYMBOL-PLIST SYMBOL-OR-PLIST))
                                  SYMBOL-OR-PLIST))
                    (RPLACD PLLOC (LIST*-IN-AREA area PROPERTY VALUE (CDR PLLOC))))))))
           ((OR INSTANCE NAMED-STRUCTURE)
            (SEND SYMBOL-OR-PLIST :PUTPROP-IN-AREA VALUE PROPERTY area)))
         VALUE))

;;; Implements SETF of GET.
(DEFUN SETPROP (SYMBOL-OR-PLIST PROPERTY VALUE)
; (UNLESS (VARIABLE-BOUNDP AREA-FOR-PROPERTY-LISTS)
;   (SETQ AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA))
  (ETYPECASE SYMBOL-OR-PLIST
    ((OR SYMBOL CONS LOCATIVE)
     (WITHOUT-INTERRUPTS
       (LET ((PLLOC (GET-LOCATION-OR-NIL SYMBOL-OR-PLIST PROPERTY)))
         (IF PLLOC
             (SETF (CAR PLLOC) VALUE)
           (SETQ PLLOC (IF (SYMBOLP SYMBOL-OR-PLIST)
                           (LOCF (SYMBOL-PLIST SYMBOL-OR-PLIST))
                         SYMBOL-OR-PLIST))
           (RPLACD PLLOC (LIST*-IN-AREA (IF (= (%AREA-NUMBER SYMBOL-OR-PLIST) NR-SYM)
                                            PROPERTY-LIST-AREA
                                            BACKGROUND-CONS-AREA)
                                        PROPERTY VALUE (CDR PLLOC)))))))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND SYMBOL-OR-PLIST :PUTPROP VALUE PROPERTY)))
  VALUE)

;;; Implements SETF of GET-FROM-AREA.
(DEFUN SETPROP-in-area (SYMBOL-OR-PLIST PROPERTY VALUE area)
; (UNLESS (VARIABLE-BOUNDP AREA-FOR-PROPERTY-LISTS)
;   (SETQ AREA-FOR-PROPERTY-LISTS PROPERTY-LIST-AREA))
  (ETYPECASE SYMBOL-OR-PLIST
    ((OR SYMBOL CONS LOCATIVE)
     (WITHOUT-INTERRUPTS
       (LET ((PLLOC (GET-LOCATION-OR-NIL SYMBOL-OR-PLIST PROPERTY)))
         (IF PLLOC
             (SETF (CAR PLLOC) VALUE)
           (SETQ PLLOC (IF (SYMBOLP SYMBOL-OR-PLIST)
                           (LOCF (SYMBOL-PLIST SYMBOL-OR-PLIST))
                         SYMBOL-OR-PLIST))
           (RPLACD PLLOC (LIST*-IN-AREA area
                                        PROPERTY VALUE (CDR PLLOC)))))))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND SYMBOL-OR-PLIST :PUTPROP-in-area VALUE PROPERTY area)))
  VALUE)

(DEFSPECIALK DEFPROP (&QUOTE SYMBOL VALUE PROPERTY)
  "Make the value of SYMBOL's PROPERTY property be VALUE."
  (PUTPROP SYMBOL VALUE PROPERTY)
  SYMBOL)

(DEFUN REMPROP (SYMBOL-OR-PLIST PROPERTY)
  "Remove a property.  Returns NIL if not present, or a list whose CAR is the property.
SYMBOL-OR-PLIST may be a symbol or a disembodied property list -
a list or locative whose cdr stores the properties.
It may also be an instance or named structure; then it is sent a :REMPROP message."
  (ETYPECASE SYMBOL-OR-PLIST
    ((OR SYMBOL CONS LOCATIVE)
     (LET ((PLLOC (IF (SYMBOLP SYMBOL-OR-PLIST)
                      (LOCF (SYMBOL-PLIST SYMBOL-OR-PLIST))
                    SYMBOL-OR-PLIST)))
       (WITHOUT-INTERRUPTS
         (DO ((PL (CDR PLLOC) (CDDR PL))
              (PPL PLLOC (CDR PL)))
             ((NULL PL) NIL)
           (WHEN (EQ (CAR PL) PROPERTY)
             (SETF (CDR PPL) (CDDR PL))
             (RETURN (CDR PL)))))))
    ((OR INSTANCE NAMED-STRUCTURE)
     (SEND SYMBOL-OR-PLIST :REMPROP PROPERTY))))

(DEFUN PROPERTY-LIST-HANDLER (OP PLIST &REST ARGS)
  (CASE OP
    (:GET
     (GET PLIST (FIRST ARGS) (SECOND ARGS)))
    (:GET-LOCATION-OR-NIL
     (GET-LOCATION-OR-NIL PLIST (FIRST ARGS)))
    (:GET-LOCATION
      (LOCF (GET PLIST (FIRST ARGS))))
    (:GETL
     (GETL PLIST (CAR ARGS)))
    (:PUTPROP
     (SETF (GET PLIST (SECOND ARGS)) (FIRST ARGS)))
    (:PUTPROP-in-area
     (SETF (GET-from-area PLIST (SECOND ARGS) (third args)) (FIRST ARGS)))
    (:REMPROP
     (REMPROP PLIST (CAR ARGS)))
    (:PUSH-PROPERTY
     (PUSH (FIRST ARGS) (GET PLIST (SECOND ARGS))))
    ((:PROPERTY-LIST :PLIST)
     (CONTENTS PLIST))
    ((:PROPERTY-LIST-LOCATION :PLIST-LOCATION)
     PLIST)
    ((:SET-PROPERTY-LIST :SETPLIST)
     (SETF (CONTENTS PLIST) (FIRST ARGS)))
    (:SET
     (CASE (FIRST ARGS)
       (:GET
        (SETF (GET PLIST (SECOND ARGS)) (CAR (LAST ARGS))))
       ((:PROPERTY-LIST :PLIST) (SETF (CONTENTS PLIST) (SECOND ARGS)))
       (:WHICH-OPERATIONS '(:GET :PROPERTY-LIST :PLIST :WHICH-OPERATIONS))
       (T (FERROR "Don't know how to ~S ~S" ':SET (FIRST ARGS)))))
    (:WHICH-OPERATIONS '(:GET :GET-LOCATION :GET-LOCATION-OR-NIL :GETL :PUTPROP :PUTPROP-IN-area :REMPROP
                         :PUSH-PROPERTY :PROPERTY-LIST :PLIST :PROPERTY-LIST-LOCATION
                         :PLIST-LOCATION :SET :SET-PROPERTY-LIST :SETPLIST
                         :WHICH-OPERATIONS))
    (T (FERROR "Don't know how to ~S a plist" OP))))

;This function does not have a % in its name because it claims to be "safe".
; -- actually, it might not be if handed a PDL-ARRAY, that would be a good thing to fix.
#+(target lambda)
(defun wipe-structure (structure)
  "Clobber any pointers in structure to NIL.  Assures no unnecessary data is retained
   when returning an object to a free list, etc."
  (setq structure (follow-structure-forwarding structure))
  (select (%data-type structure)
    (dtp-stack-group
     (stack-group-preset structure 'ferror "Attempted to run wiped stack-group")
     (setf (sg-saved-qlaryh structure) nil)
     (setf (SG-AC-K structure) nil)
     (setf (SG-AC-S structure) nil)
     (setf (SG-AC-J structure) nil)
     (setf (SG-AC-I structure) nil)
     (setf (SG-AC-Q structure) nil)
     (setf (SG-AC-R structure) nil)
     (setf (SG-AC-T structure) nil)
     (setf (SG-AC-E structure) nil)
     (setf (SG-AC-D structure) nil)
     (setf (SG-AC-C structure) nil)
     (setf (SG-AC-B structure) nil)
     (setf (SG-AC-A structure) nil)
     (setf (SG-AC-ZR structure) nil)
     (setf (sg-saved-vma structure) nil))
    (otherwise
     (let ((base (%find-structure-leader structure))
           (boxed (%structure-boxed-size structure)))
       (declare (unspecial base))
       (do ((c 1 (1+ c)))       ;its never necessary to clobber the first Q, and could be very
           (( c boxed))        ; harmful in case of DTP-INSTANCE.
         (when (%p-pointerp-offset base c)
           (%p-store-contents-offset nil base c)))))))

(defsubst %sxhash-string (string character-mask)
  (compiler::%sxhash-substring string character-mask 0 nil))


;;; We hairily arrange to return the same value as we did in the days of 24 bit pointers.
;;; This is to avoid making everything look "changed" when the switch happens.
(DEFUN SXHASH (X &OPTIONAL RANDOM-OBJECT-ACTION)
  "Return a hash code for object X.  EQUAL objects have the same hash code.
The hash code is always a positive fixnum.
Flavor instances and named structures may handle the :SXHASH operation.
The hash code of an object does not change even if it is printed out and read
into a different system version."
  (DECLARE (IGNORE RANDOM-OBJECT-ACTION))
  (MACROLET ((ROT-24-BIT (VALUE BITS)
               (ONCE-ONLY (VALUE BITS)
                 `(DPB ,VALUE (BYTE (- 24. ,BITS) ,BITS)
                       (LSH ,VALUE (- ,BITS 24.))))))
    (COND ((SYMBOLP X) (%SXHASH-STRING (SYMBOL-NAME X) #o337))
          ((STRINGP X) (%SXHASH-STRING X #o337))        ;Ignores case!
          ((OR (INTEGERP X) (CHARACTERP X))
           (IF (MINUSP X) (LOGXOR (LDB (BYTE 23. 0.) X) 1)
             (LDB (BYTE 23. 0.) X)))
          ((CONSP X)            ;Rotate car by 11. and cdr by 7, but do it efficiently
           (DO ((ROT 4) (HASH 0) Y (X X))
               ((ATOM X)
                (OR (NULL X)
                    (SETQ HASH (LOGXOR (ROT-24-BIT (SXHASH X)
                                                   (IF (< ROT 4) (+ ROT 20.) (- ROT 4)))
                                       HASH)))
                (LOGAND #o37777777 (IF (LDB-TEST (BYTE 1 23.) HASH) (LOGXOR HASH 1) HASH)))
             (SETQ Y (CAR X) X (CDR X))
             (UNLESS (< (SETQ ROT (+ ROT 7)) 24.)
               (SETQ ROT (- ROT 24.)))
             (SETQ HASH (LOGXOR (ROT-24-BIT
                                  (COND ((SYMBOLP Y) (%SXHASH-STRING (SYMBOL-NAME Y) #o337))
                                        ((STRINGP Y) (%SXHASH-STRING Y #o337))
                                        ((OR (INTEGERP Y) (CHARACTERP Y))
                                         (LDB (BYTE 24. 0.) Y))
                                        (T (SXHASH Y)))
                                  ROT)
                                HASH))))
          ((TYPEP X 'SINGLE-FLOAT)
           (LOGXOR (%P-LDB-OFFSET (BYTE 23. 0.) X 1)
                   (%P-LDB-OFFSET (BYTE 1. 23.) X 1)
                   (%P-LDB (BYTE 18. 0.) X)))
          ((AND (TYPEP X 'INSTANCE)
                (SEND-IF-HANDLES X :SXHASH NIL)))
          ((AND (TYPEP X 'NAMED-STRUCTURE)
                (MEMQ ':SXHASH (NAMED-STRUCTURE-INVOKE :WHICH-OPERATIONS X)))
           (NAMED-STRUCTURE-INVOKE :SXHASH X NIL))
          ((TYPEP X 'SHORT-FLOAT)
           (SETQ X (%POINTER X))
           (LET ((Y (LOGXOR (LDB (byte (- %%Q-POINTER 24.) 0) X)
                            (LSH X (- 24. %%Q-POINTER)))))
             (LOGAND #o37777777
                     (IF (MINUSP X) (LOGXOR Y 1) Y))))
          ((ARRAYP X)
           (LENGTH X))
          (T 0))))                                      ;0 for random things

(DEFUN GET-MACRO-ARG-DESC-POINTER (FEF-POINTER &AUX ORIGIN)
  "Return a pointer to the argument descriptor list of a compiled function.
This list describes how to bind the arguments and how to initialize them."
  (check-type fef-pointer compiled-function)
  (COND ((= 1
            (cond ((= (%p-ldb %%header-type-field fef-pointer) %header-type-fef)
                   (%P-LDB %%FEFH-NO-ADL FEF-POINTER))  ;old-style
                  (t (%p-ldb-offset %%fefsl-no-adl fef-pointer %fefhi-storage-length))))
         NIL)
        ((= 0 (SETQ ORIGIN
                    (%P-LDB-OFFSET %%FEFHI-MS-ARG-DESC-ORG FEF-POINTER %FEFHI-MISC)))
         NIL)
        (T (%MAKE-POINTER-OFFSET DTP-LIST FEF-POINTER ORIGIN))))

(defun maybe-change-fef-type (fef-pointer &aux (sys:%inhibit-read-only t))
  (check-type fef-pointer compiled-function)
  (cond ((= (%P-LDB-OFFSET %%HEADER-TYPE-FIELD FEF-POINTER %FEFHI-IPC) %header-type-fef)
         (let ((get-self-mapping-table
                 (%p-ldb-offset %%fefh-get-self-mapping-table fef-pointer %fefhi-ipc))
               (sv-bind (%p-ldb-offset %%fefh-sv-bind fef-pointer %fefhi-ipc))
               (fast-arg (%p-ldb-offset %%fefh-fast-arg fef-pointer %fefhi-ipc))
               (no-adl (%p-ldb-offset %%fefh-no-adl fef-pointer %fefhi-ipc))
               (min-args (%p-ldb-offset %%ARG-DESC-MIN-ARGS fef-pointer %fefhi-fast-arg-opt))
               (max-args (%p-ldb-offset %%ARG-DESC-MAX-ARGS fef-pointer %fefhi-fast-arg-opt))
               (local-block (%p-ldb-offset %%FEFHI-MS-LOCAL-BLOCK-LENGTH fef-pointer %fefhi-misc))
               (fast (%p-contents-offset fef-pointer %fefhi-fast-arg-opt)))
           (cond ((or (not (zerop get-self-mapping-table))
                      (not (zerop sv-bind))
                      (not (= fast-arg 1))
                      (bit-test %arg-desc-quoted-rest fast)
                      (bit-test %arg-desc-evaled-rest fast)
                      (bit-test %arg-desc-interpreted fast)
                      (bit-test %arg-desc-fef-quote-hair fast)
                      (bit-test %arg-desc-fef-bind-hair fast))
                  nil)
                 ((and (= min-args max-args)
                       (zerop local-block)
                       (<= min-args (dpb -1 (logand 77 %%fefh-args-for-fanl) 0)))
                  (%p-dpb-offset min-args %%fefh-args-for-fanl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset no-adl %%fefsl-no-adl fef-pointer %fefhi-storage-length)
                  (%p-dpb-offset %HEADER-TYPE-FAST-FEF-FIXED-ARGS-NO-LOCALS
                                 %%header-type-field
                                 fef-pointer
                                 %fefhi-ipc)
                  'FIXED-ARGS-NO-LOCALS)
                 ((and (zerop local-block)
                       (<= min-args (dpb -1 (logand 77 %%fefh-min-args-for-vanl) 0))
                       (<= max-args (dpb -1 (logand 77 %%fefh-max-args-for-vanl) 0)))
                  (%p-dpb-offset min-args %%fefh-min-args-for-vanl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset max-args %%fefh-max-args-for-vanl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset no-adl %%fefsl-no-adl fef-pointer %fefhi-storage-length)
                  (%p-dpb-offset %HEADER-TYPE-FAST-FEF-VAR-ARGS-NO-LOCALS
                                 %%header-type-field
                                 fef-pointer
                                 %fefhi-ipc)
                  'VARIABLE-ARGS-NO-LOCALS)
                 ((and (= min-args max-args)
                       (<= min-args (dpb -1 (logand 77 %%fefh-args-for-fawl) 0))
                       (<= local-block (dpb -1 (logand 77 %%fefh-locals-for-fawl) 0)))
                  (%p-dpb-offset min-args %%fefh-args-for-fawl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset local-block %%fefh-locals-for-fawl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset no-adl %%fefsl-no-adl fef-pointer %fefhi-storage-length)
                  (%p-dpb-offset %HEADER-TYPE-FAST-FEF-FIXED-ARGS-WITH-LOCALS
                                 %%header-type-field
                                 fef-pointer
                                 %fefhi-ipc)
                  'FIXED-ARGS-WITH-LOCALS)
                 ((and (<= min-args (dpb -1 (logand 77 %%fefh-min-args-for-vawl) 0))
                       (<= max-args (dpb -1 (logand 77 %%fefh-max-args-for-vawl) 0))
                       (<= local-block (dpb -1 (logand 77 %%fefh-locals-for-vawl) 0)))
                  (%p-dpb-offset min-args %%fefh-min-args-for-vawl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset max-args %%fefh-max-args-for-vawl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset local-block %%fefh-locals-for-vawl fef-pointer %fefhi-ipc)
                  (%p-dpb-offset no-adl %%fefsl-no-adl fef-pointer %fefhi-storage-length)
                  (%p-dpb-offset %HEADER-TYPE-FAST-FEF-VAR-ARGS-WITH-LOCALS
                                 %%header-type-field
                                 fef-pointer
                                 %fefhi-ipc)
                  'VARIABLE-ARGS-WITH-LOCALS)
                 (t nil))))))

(defun maybe-change-fef-type-all (&aux (total 0) (fast 0))
  (do-all-symbols (symbol nil)
    (when (and (fboundp symbol) (typep (fsymeval symbol) :compiled-function))
      (when (maybe-change-fef-type (fsymeval symbol))
        (incf fast))
      (incf total)))
  (format t "~%~D total functions, ~D fast." total fast))

;;; Ordered as per ARRAY-TYPES
(DEFCONST ARRAY-ELEMENT-TYPE-ALIST
          '((NIL . ART-ERROR)
            (BIT . ART-1B)
            ((MOD 4) . ART-2B)
            ((MOD #o20) . ART-4B)
            ((MOD #o400) . ART-8B)
            ((MOD #o200000) . ART-16B)
            (FIXNUM . ART-32B)
            (T . ART-Q)
            (T . ART-Q-LIST)
            (STRING-CHAR . ART-STRING)
            (T . ART-STACK-GROUP-HEAD)
            (T . ART-SPECIAL-PDL)
            ((SIGNED-BYTE #o20) . ART-HALF-FIX)
            (T . ART-REG-PDL)
            (FLOAT . ART-FLOAT)
            (FLOAT . ART-FPS-FLOAT)
            (FAT-CHAR . ART-FAT-STRING)
            ((COMPLEX FLOAT) . ART-COMPLEX-FLOAT)
            (COMPLEX . ART-COMPLEX)
            ((COMPLEX FLOAT) . ART-COMPLEX-FPS-FLOAT)
            ((UNSIGNED-BYTE 1) . ART-1B)
            ((UNSIGNED-BYTE 2) . ART-2B)
            ((UNSIGNED-BYTE 4) . ART-4B)
            ((UNSIGNED-BYTE #o10) . ART-8B)
            ((UNSIGNED-BYTE #o20) . ART-16B)
            ((SIGNED-BYTE #o20) . ART-HALF-FIX)))

;;; can't find anything that uses this...
(DEFCONST ARRAY-ELEMENT-SIZE-ALIST
          '((#o2 . ART-1B)
            (#o4 . ART-2B)
            (#o20 . ART-4B)
            (#o400 . ART-8B)
            (#o200000 . ART-16B)))

;;; array-type-from-element-type now in SYS2; TYPES

(DEFUN ARRAY-CANONICALIZE-TYPE (TYPE &AUX FOO)
  (COND ((MEMQ TYPE ARRAY-TYPES) TYPE)
        ((SETQ FOO (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS))
         (NTH FOO ARRAY-TYPES))
        ((FIXNUMP TYPE)
         (IF (NOT (ZEROP (LDB %%ARRAY-TYPE-FIELD TYPE)))
             (SETQ TYPE (LDB %%ARRAY-TYPE-FIELD TYPE)))
         (NTH TYPE ARRAY-TYPES))
        (T (ARRAY-TYPE-FROM-ELEMENT-TYPE TYPE))))

(DEFUN ARRAY-ELEMENT-TYPE (ARRAY)
  "Return a Common Lisp data type describing the objects that could be stored in ARRAY."
  (OR (CAR (RASSQ (ARRAY-TYPE ARRAY) ARRAY-ELEMENT-TYPE-ALIST))
      T))

(DEFUN ADJUSTABLE-ARRAY-P (ARRAY)
  "A Common Lisp function which returns T if ARRAY is an adjustable array (ie may have
ADJUST-ARRAY applied to it) This is true for all arrays on the Lisp Machine."
  (CHECK-TYPE ARRAY ARRAY)
  T)

(DEFUN ARRAY-TYPE-NULL-ELEMENT (TYPE)
  (CDR (ASSQ (ARRAY-CANONICALIZE-TYPE TYPE)
             '((ART-ERROR . NIL)
               (ART-1B . 0)
               (ART-2B . 0)
               (ART-4B . 0)
               (ART-8B . 0)
               (ART-16B . 0)
               (ART-32B . 0)
               (ART-Q . NIL)
               (ART-Q-LIST . NIL)
               (ART-STRING . #/ )
               (ART-STACK-GROUP-HEAD . NIL)
               (ART-SPECIAL-PDL . NIL)
               (ART-HALF-FIX . 0)
               (ART-REG-PDL . NIL)
               (ART-FLOAT . 0f0)
               (ART-FPS-FLOAT . 0f0)
               (ART-FAT-STRING . #/ )
               (ART-COMPLEX-FLOAT . 0f0+0f0i)
               (ART-COMPLEX . 0)
               (ART-COMPLEX-FPS-FLOAT 0f0+0f0i)))))

;;; DEFVAR to NIL with a SETQ insures it will never be unbound.
;;; The SETQ now happens in installing packages.
(DEFVAR ARRAY-TYPE-KEYWORDS NIL
  "List of keywords which have pnames matching the array type symbols.")

(DEFCONST ARRAY-TOTAL-SIZE-LIMIT (%logdpb 0 %%q-boxed-sign-bit -1)
  "The total number of elements in any array must be less than this.")

(DEFCONST ARRAY-DIMENSION-LIMIT (%logdpb 0 %%q-boxed-sign-bit -1)
  "Every dimension of an array must be less than this.")

(DEFCONST ARRAY-RANK-LIMIT 8
  "The rank of an array must be less than this.")

;;; New microcodes cache the parameters associated with the last array
;;; referenced.  Any function that modifies the internal parameters of an
;;; array should call this function afterwards to make sure that the array
;;; is properly decoded the next time through.

(DEFSUBST INVALIDATE-ARRAY-CACHE () (AREF "INVA" 0))

(DEFMACRO ARRAY-TOTAL-DATA-SIZE (ARRAY-TYPE INDEX-LENGTH)
  `(LET ((ELEMENTS-PER-WORD (AREF #'ARRAY-ELEMENTS-PER-Q ,ARRAY-TYPE)))
     (IF (PLUSP ELEMENTS-PER-WORD)
         (CEILING ,INDEX-LENGTH ELEMENTS-PER-WORD)
       (* ,INDEX-LENGTH (- ELEMENTS-PER-WORD)))))

(DEFMACRO ARRAY-ELEMENTS-FOR-GIVEN-DATA-SIZE (ARRAY-TYPE WORDS)
  `(LET ((ELEMENTS-PER-WORD (AREF #'ARRAY-ELEMENTS-PER-Q ,ARRAY-TYPE)))
     (IF (PLUSP ELEMENTS-PER-WORD)
         (* ,WORDS ELEMENTS-PER-WORD)
       (CEILING ,WORDS (- ELEMENTS-PER-WORD)))))

(DEFMACRO ARRAY-BOXED-DATA-SIZE (ARRAY-TYPE INDEX-LENGTH)
  `(* ,INDEX-LENGTH (AREF #'ARRAY-BOXED-WORDS-PER-ELEMENT ,ARRAY-TYPE)))

(DEFUN MAKE-ARRAY (DIMENSIONS &REST OPTIONS)
  "Create an array of size DIMENSIONS (a number or list of numbers).
The keywords are as follows:
:TYPE - specify array type, controlling type of elements allowed.  Default is ART-Q.
 ART-Q (any elements), ART-Q-LIST (any elements, and the contents looks like a list),
 ART-STRING (elements 0 through 255, printed with quotes),
 ART-FAT-STRING (16 bit unsigned elements, printed with quotes),
 ART-1B (elements 0 and 1), ART-2B (elements 0 through 3), ART-4B, ART-8B, ART-16B,
 ART-32B, ART-INUM (any fixnum, low 25. bits of bignum stored),
 ART-FLOAT (elements any full-size flonum),
 ART-COMPLEX (elements any number including complex numbers),
 ART-COMPLEX-FLOAT (elements complex numbers composed of two full-size flonums),
 ART-HALF-FIX (16 bit signed fixnum elements),
 ART-FPS-FLOAT ART-COMPLEX-FPS-FLOAT (used with floating point array processor),
 ART-STACK-GROUP-HEAD, ART-REGULAR-PDL, ART-SPECIAL-PDL (parts of stack groups).
:ELEMENT-TYPE - specify array type by specifying Common Lisp
 data type of elements allowed.  For example,
 an :ELEMENT-TYPE of (MOD 4) would get an ART-2B array.
:AREA - specify area to create the array in.
:LEADER-LENGTH - specify number of elements of array leader to make.
:LEADER-LIST - list whose elements are used to initialize the leader.
:FILL-POINTER - specify initial fill pointer value (ARRAY-ACTIVE-LENGTH of the array).
 Requests a leader of length 1 and specifies the contents of the slot.
:INITIAL-ELEMENT - value used to initialize all elements of the array.
:DISPLACED-TO - array, locative or fixnum specifying address of data
 that this array should overlap.
:DISPLACED-INDEX-OFFSET - if displaced to another array, this specifies
 which element of that array should correspond to element 0 of the new one.
:NAMED-STRUCTURE-SYMBOL - if not NIL, specifies a named structure symbol
 to be stored in the array, which should have its named-structure bit set.
:INITIAL-CONTENTS - value is a sequence of sequences of sequences...
 where the leaves are the values to initialize the array from.
 The top level of sequence corresponds to the most slowly varying subscript.
:ADJUSTABLE - ignored.  (for Common Lisp compatibility)."
  (DECLARE (ARGLIST DIMENSIONS &KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
                                    FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET
                                    TYPE AREA LEADER-LENGTH LEADER-LIST NAMED-STRUCTURE-SYMBOL
                                    ADJUSTABLE))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
        LEADER-LIST LEADER-LENGTH LEADER-SIZE FILL-POINTER NAMED-STRUCTURE-SYMBOL
        DISPLACED-TO DISPLACED-INDEX-OFFSET
        AREA ARRAY N-DIMENSIONS INDEX-LENGTH LONG-ARRAY?
        (TYPE ART-Q) TYPE? ELEMENT-TYPE?
        INITIAL-ELEMENT INITIAL-ELEMENT?
        INITIAL-CONTENTS INITIAL-CONTENTS?
        BOXED-SIZE TOTAL-SIZE HEADER-SIZE HEADER)
    (UNLESS (EVENP LENGTH-OF-OPTIONS)
      (FERROR "Odd-length options list: ~S" OPTIONS))
    (DO ((O OPTIONS (CDDR O)))
        ((NULL O))
      (LET ((VALUE (CADR O)))
        (CASE (CAR O)
          (:AREA (SETQ AREA VALUE))
          (:TYPE (SETQ TYPE VALUE TYPE? T))
          (:ELEMENT-TYPE (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE) ELEMENT-TYPE? T))
          (:DISPLACED-INDEX-OFFSET (SETQ DISPLACED-INDEX-OFFSET VALUE))
          (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
          ((:INITIAL-ELEMENT :INITIAL-VALUE) (SETQ INITIAL-ELEMENT VALUE INITIAL-ELEMENT? T))
          (:INITIAL-CONTENTS (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS? T))
          (:FILL-POINTER (SETQ FILL-POINTER VALUE))
          (:ADJUSTABLE)
          (:LEADER-LIST (SETQ LEADER-LIST VALUE))
          (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
          (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
          (OTHERWISE
           (FERROR "~S is not a known ~S keyword." (CAR O) 'MAKE-ARRAY)))))
    (IF (AND TYPE? ELEMENT-TYPE?)
        (FERROR "Both ~S and ~S specified." :TYPE :ELEMENT-TYPE))
    (IF (AND DISPLACED-INDEX-OFFSET (NOT DISPLACED-TO))
        (FERROR "The ~S option specified without ~S." :DISPLACED-INDEX-OFFSET :DISPLACED-TO))
    (IF (AND INITIAL-ELEMENT? INITIAL-CONTENTS?)
        (FERROR "Both ~S and ~S specified." :INITIAL-ELEMENT :INITIAL-CONTENTS))
    (TYPECASE TYPE
      (FIXNUM
       ;; Perfunctory test to decide whether the array-type is shifted or not.
       (UNLESS (ZEROP (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))
         (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))))
      (SYMBOL
       (COND ((MEMQ TYPE ARRAY-TYPES)
              (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD (SYMBOL-VALUE TYPE))))
             ((MEMQ TYPE ARRAY-TYPE-KEYWORDS)
              (SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS)))
             (T
              (FERROR "~S is not a valid array type." TYPE))))
      (OTHERWISE
       (FERROR "~S is not a valid array type." TYPE)))
    (TYPECASE DIMENSIONS
      ((FIXNUM 0)
       (SETQ N-DIMENSIONS 1)
       (SETQ INDEX-LENGTH DIMENSIONS))
      (LIST
       (WHEN (> (SETQ N-DIMENSIONS (LENGTH DIMENSIONS)) ARRAY-RANK-LIMIT)
         (FERROR "Arrays may have at most ~D dimensions, not ~D." ARRAY-RANK-LIMIT N-DIMENSIONS))
       (DOLIST (DIMENSION DIMENSIONS)
         (CHECK-TYPE DIMENSION (FIXNUM 0)))
       (SETQ INDEX-LENGTH (APPLY #'* DIMENSIONS))
       (UNLESS (FIXNUMP INDEX-LENGTH)
         (FERROR "Array too large; dimensions ~D ==> total size ~D" DIMENSIONS INDEX-LENGTH)))
      (OTHERWISE
       (FERROR "~S is not a valid array dimension specification." DIMENSIONS)))
    (CHECK-TYPE DISPLACED-TO (OR NULL FIXNUM ARRAY LOCATIVE))
    (CHECK-TYPE DISPLACED-INDEX-OFFSET (OR NULL (FIXNUM 0)))
    (TYPECASE LEADER-LENGTH
      (NULL         (SETQ LEADER-LENGTH 0))
      ((FIXNUM 0))
      (OTHERWISE    (FERROR "~S is not a valid leader-length specification." LEADER-LENGTH)))
    (TYPECASE LEADER-LIST
      (NULL)
      (CONS         (SETQ LEADER-LENGTH (MAX LEADER-LENGTH (LENGTH LEADER-LIST))))
      (OTHERWISE    (FERROR "~S is not a valid leader-list." LEADER-LIST)))
    (TYPECASE NAMED-STRUCTURE-SYMBOL
      (NULL)
      (SYMBOL
       ;;Make sure NAMED-STRUCTURE-SYMBOL gets stored and/or handled correctly later.
       (cond
         ((eq named-structure-symbol T))                ;Structure symbol doesn't get stored
         ((and leader-length (plusp leader-length))     ;Store structure symbol in leader elt 1
          (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 2)))
         ((= N-DIMENSIONS 1))                           ;Store structure symbol in array element 0
         (t                                             ;Can't handle structure name
          (FERROR "Named-structure arrays without leaders must be one-dimensional."))))
      (OTHERWISE
       (FERROR "~S is not a valid named-structure symbol." NAMED-STRUCTURE-SYMBOL)))
    (TYPECASE FILL-POINTER
      (NULL)
      ((OR (FIXNUM 0) (CL:MEMBER T))
       (UNLESS (= N-DIMENSIONS 1)
         (FERROR "Only 1-dimensional arrays may have fill-pointers."))
       (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 1))
       (OR (FIXNUMP FILL-POINTER) (SETQ FILL-POINTER index-length)))
      (OTHERWISE (FERROR "~S is not a valid fill-pointer." FILL-POINTER)))
    (COND ((ZEROP LEADER-LENGTH)
           (SETQ LEADER-SIZE 0)
           (SETQ HEADER (%LOGDPB N-DIMENSIONS
                                 %%ARRAY-NUMBER-DIMENSIONS
                                 (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
          (T
           (SETQ LEADER-SIZE (+ 2 LEADER-LENGTH))
           (SETQ HEADER (%LOGDPB 1
                                 %%ARRAY-LEADER-BIT
                                 (%LOGDPB N-DIMENSIONS
                                          %%ARRAY-NUMBER-DIMENSIONS
                                          (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))))
    (COND ((NULL DISPLACED-TO)
           (COND ((NOT (SETQ LONG-ARRAY? (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)))
                  (SETQ HEADER (+ INDEX-LENGTH HEADER))
                  (SETQ HEADER-SIZE (MAX 1 N-DIMENSIONS)))
                 (T
                  (SETQ HEADER (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER))
                  (SETQ HEADER-SIZE (1+ (MAX 1 N-DIMENSIONS)))))
           (SETQ ARRAY (%MAKE-ARRAY
                         HEADER
                         INDEX-LENGTH
                         LEADER-LENGTH
                         AREA
                         (SETQ TOTAL-SIZE (+ LEADER-SIZE
                                             HEADER-SIZE
                                             (ARRAY-TOTAL-DATA-SIZE TYPE INDEX-LENGTH)))
                         (SETQ BOXED-SIZE (+ LEADER-SIZE
                                             HEADER-SIZE
                                             (ARRAY-BOXED-DATA-SIZE TYPE INDEX-LENGTH))))))
          (T
           (SETQ ARRAY (%MAKE-ARRAY
                         (+ (%LOGDPB 1 %%ARRAY-DISPLACED-BIT HEADER)
                            (IF DISPLACED-INDEX-OFFSET 3 2))
                         INDEX-LENGTH
                         LEADER-LENGTH
                         AREA
                         (SETQ TOTAL-SIZE (+ LEADER-SIZE
                                             (MAX 1 N-DIMENSIONS)
                                             (IF DISPLACED-INDEX-OFFSET 3 2)))
                         (SETQ BOXED-SIZE TOTAL-SIZE)))
           (%P-STORE-CONTENTS-OFFSET DISPLACED-TO ARRAY N-DIMENSIONS)
           (%P-STORE-CONTENTS-OFFSET INDEX-LENGTH ARRAY (1+ N-DIMENSIONS))
           (UNLESS (NULL DISPLACED-INDEX-OFFSET)
             (%P-STORE-CONTENTS-OFFSET DISPLACED-INDEX-OFFSET ARRAY (+ N-DIMENSIONS 2)))))
    (WHEN (> N-DIMENSIONS 1)
      ;; Initialize the dimension list.  Last dimension goes last, the first is already done.
      (LOOP FOR (DIMENSION) ON (CDR DIMENSIONS)
            FOR INDEX = (IF LONG-ARRAY? N-DIMENSIONS (1- N-DIMENSIONS)) THEN (1- INDEX)
         DO (%P-STORE-CONTENTS-OFFSET DIMENSION ARRAY INDEX)))
    (WHEN (NOT (NULL LEADER-LIST))
      (LOOP FOR (ELEMENT) ON LEADER-LIST
            FOR INDEX = 0 THEN (1+ INDEX)
         DO (SETF (ARRAY-LEADER ARRAY INDEX) ELEMENT)))
    (WHEN (NOT (NULL FILL-POINTER))
      (SETF (FILL-POINTER ARRAY) FILL-POINTER))

    ;; Cretinism associated with make-array, in that the leader list can overlap
    ;; with the name-structure slot, which is how fasd dumps the named-structure-symbol
    ;; So we check for the symbol being t and not smash it in that case
    (WHEN (NOT (NULL NAMED-STRUCTURE-SYMBOL))
      (IF (ZEROP LEADER-LENGTH)
          ;; There is no leader; put it in element zero of the body.
          (SETF (AREF ARRAY 0) NAMED-STRUCTURE-SYMBOL)
        ;; There is a leader; use element one of the leader.
        (UNLESS (EQ NAMED-STRUCTURE-SYMBOL T)
          (SETF (ARRAY-LEADER ARRAY 1) NAMED-STRUCTURE-SYMBOL)))
      ;; It is a named structure.  Set the flag.
      (%P-DPB-OFFSET 1 %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY 0))

    ;; %MAKE-ARRAY initializes boxed qs to nil, so only initialize to non-nil values.
    (WHEN (AND INITIAL-ELEMENT (NOT DISPLACED-TO))
      (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT))
    (WHEN (NOT (NULL INITIAL-CONTENTS?))
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))

    ;; If there is a fill pointer on an art-q-list array, then it should control
    ;; the length of the list as well.  See ARRAY-PUSH and ARRAY-POP.
    (WHEN (AND (OR FILL-POINTER LEADER-LIST)
               (= N-DIMENSIONS 1)
               (= TYPE (LDB %%ARRAY-TYPE-FIELD ART-Q-LIST)))
      (UNLESS FILL-POINTER (SETQ FILL-POINTER (CAR LEADER-LIST)))
      (WHEN (AND (FIXNUMP FILL-POINTER)
                 (> FILL-POINTER 0)
                 (< FILL-POINTER (ARRAY-LENGTH ARRAY)))
        (%P-DPB CDR-NIL %%Q-CDR-CODE (AP-1 ARRAY (1- FILL-POINTER)))))

    (VALUES ARRAY TOTAL-SIZE BOXED-SIZE)))

(defun set-fill-pointer (array value)
  (cond ((not (arrayp array))
         (ferror "Argument ~S must be an array" array))
        ((not (array-has-leader-p array))
         (ferror "Array ~S has no fill-pointer" array))
        ((not (fixnump value))
         (ferror "New fill-pointer must be a fixnum"))
        ((or (minusp value) (> value (array-length array)))
         (ferror "New fill pointer ~A out of range for array ~A" value array))
        (t
         (setf (array-leader array 0) value))))

(defun cli:make-array (dimensions &rest options
                       &key element-type initial-element initial-contents
                       adjustable fill-pointer displaced-to displaced-index-offset)
  (declare (arglist dimensions &key element-type initial-element initial-contents
                    adjustable fill-pointer displaced-to displaced-index-offset)
           ;; Just using &KEY for checking.
           (ignore element-type initial-element initial-contents
                   adjustable fill-pointer displaced-index-offset)
           (values array))
  ;; Don't allow fixnums for displaced location.
  (check-type displaced-to (or null array))
  ;; ZL:MAKE-ARRAY returns three values.
  (values (apply #'zl:make-array dimensions options)))

;; Copied from LAD: RELEASE-3.SYS; QRAND.LISP#485 on 3-Oct-86 15:11:58
(DEFUN ARRAY-SIZE-IF-MADE (DIMENSIONS &REST OPTIONS)
  "Args like MAKE-ARRAY, values total-size boxed-size"
  (DECLARE (ARGLIST DIMENSIONS &KEY ELEMENT-TYPE INITIAL-ELEMENT INITIAL-CONTENTS
                                    FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET
                                    TYPE AREA LEADER-LENGTH LEADER-LIST NAMED-STRUCTURE-SYMBOL
                                    ADJUSTABLE)
           (VALUES TOTAL-SIZE BOXED-SIZE))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
        LEADER-LIST LEADER-LENGTH LEADER-SIZE FILL-POINTER NAMED-STRUCTURE-SYMBOL
        DISPLACED-TO DISPLACED-INDEX-OFFSET
        AREA N-DIMENSIONS INDEX-LENGTH LONG-ARRAY?
        (TYPE ART-Q) TYPE? ELEMENT-TYPE?
        INITIAL-ELEMENT INITIAL-ELEMENT?
        INITIAL-CONTENTS INITIAL-CONTENTS?
        BOXED-DATA-SIZE TOTAL-DATA-SIZE BOXED-SIZE TOTAL-SIZE
        HEADER-SIZE WIREABLE?)
    (UNLESS (EVENP LENGTH-OF-OPTIONS)
      (FERROR "Odd-length options list: ~S" OPTIONS))
    (DO ((O OPTIONS (CDDR O)))
        ((NULL O))
      (LET ((VALUE (CADR O)))
        (CASE (CAR O)
          (:AREA (SETQ AREA VALUE))
          (:TYPE (SETQ TYPE VALUE TYPE? T))
          (:ELEMENT-TYPE (SETQ TYPE (ARRAY-TYPE-FROM-ELEMENT-TYPE VALUE) ELEMENT-TYPE? T))
          (:DISPLACED-INDEX-OFFSET (SETQ DISPLACED-INDEX-OFFSET VALUE))
          (:DISPLACED-TO (SETQ DISPLACED-TO VALUE))
          ((:INITIAL-ELEMENT :INITIAL-VALUE) (SETQ INITIAL-ELEMENT VALUE INITIAL-ELEMENT? T))
          (:INITIAL-CONTENTS (SETQ INITIAL-CONTENTS VALUE INITIAL-CONTENTS? T))
          (:FILL-POINTER (SETQ FILL-POINTER VALUE))
          (:ADJUSTABLE)
          (:WIREABLE (SETQ WIREABLE? VALUE))
          (:LEADER-LIST (SETQ LEADER-LIST VALUE))
          (:LEADER-LENGTH (SETQ LEADER-LENGTH VALUE))
          (:NAMED-STRUCTURE-SYMBOL (SETQ NAMED-STRUCTURE-SYMBOL VALUE))
          (OTHERWISE
           (FERROR "~S is not a known ~S keyword." (CAR O) 'MAKE-ARRAY)))))
    (IF (AND TYPE? ELEMENT-TYPE?)
        (FERROR "Both ~S and ~S specified." :TYPE :ELEMENT-TYPE))
    (IF (AND DISPLACED-INDEX-OFFSET (NOT DISPLACED-TO))
        (FERROR "The ~S option specified without ~S." :DISPLACED-INDEX-OFFSET :DISPLACED-TO))
    (IF (AND INITIAL-ELEMENT? INITIAL-CONTENTS?)
        (FERROR "Both ~S and ~S specified." :INITIAL-ELEMENT :INITIAL-CONTENTS))
    (TYPECASE TYPE
      (FIXNUM
       ;; Perfunctory test to decide whether the array-type is shifted or not.
       (UNLESS (ZEROP (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))
         (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))))
      (SYMBOL
       (COND ((MEMQ TYPE ARRAY-TYPES)
              (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD (SYMBOL-VALUE TYPE))))
             ((MEMQ TYPE ARRAY-TYPE-KEYWORDS)
              (SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS)))
             (T
              (FERROR "~S is not a valid array type." TYPE))))
      (OTHERWISE
       (FERROR "~S is not a valid array type." TYPE)))
    (TYPECASE DIMENSIONS
      ((FIXNUM 0)
       (SETQ N-DIMENSIONS 1)
       (SETQ INDEX-LENGTH DIMENSIONS))
      (LIST
       (unless (< (SETQ N-DIMENSIONS (LENGTH DIMENSIONS)) ARRAY-RANK-LIMIT)
         (FERROR "Arrays may have at most ~S dimensions, not ~S." (- array-rank-limit 1) N-DIMENSIONS))
       (DOLIST (DIMENSION DIMENSIONS)
         (CHECK-TYPE DIMENSION (FIXNUM 0) "a non-negative fixnum"))
       (SETQ INDEX-LENGTH (APPLY #'* DIMENSIONS))
       (UNLESS (FIXNUMP INDEX-LENGTH)
         (FERROR "Array too large; dimensions ~S ==> total size ~S" DIMENSIONS INDEX-LENGTH)))
      (OTHERWISE
       (FERROR "~S is not a valid array dimension specification." DIMENSIONS)))
    (CHECK-TYPE DISPLACED-TO (OR NULL FIXNUM ARRAY LOCATIVE))
    (CHECK-TYPE DISPLACED-INDEX-OFFSET (OR NULL (FIXNUM 0)))
    (TYPECASE LEADER-LENGTH
      (NULL         (SETQ LEADER-LENGTH 0))
      ((FIXNUM 0))
      (OTHERWISE    (FERROR "~S is not a valid leader-length specification." LEADER-LENGTH)))
    (TYPECASE LEADER-LIST
      (NULL)
      (CONS         (SETQ LEADER-LENGTH (MAX LEADER-LENGTH (LENGTH LEADER-LIST))))
      (OTHERWISE    (FERROR "~S is not a valid leader-list." LEADER-LIST)))
    (TYPECASE NAMED-STRUCTURE-SYMBOL
      (NULL)
      (SYMBOL
       ;;Make sure NAMED-STRUCTURE-SYMBOL gets stored and/or handled correctly later.
       (cond
         ((eq named-structure-symbol T))                ;Structure symbol doesn't get stored
         ((and leader-length (plusp leader-length))     ;Store structure symbol in leader elt 1
          (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 2)))
         ((= N-DIMENSIONS 1))                           ;Store structure symbol in array element 0
         (t                                             ;Can't handle structure name
          (FERROR "Named-structure arrays without leaders must be one-dimensional."))))
      (OTHERWISE
       (FERROR "~S is not a valid named-structure symbol." NAMED-STRUCTURE-SYMBOL)))
    (TYPECASE FILL-POINTER
      (NULL)
      ((FIXNUM 0)
       (UNLESS (= N-DIMENSIONS 1)
         (FERROR "Only 1-dimensional arrays may have fill-pointers."))
       (SETQ LEADER-LENGTH (MAX LEADER-LENGTH 1)))
      (OTHERWISE
       (FERROR "~S is not a valid fill-pointer." FILL-POINTER)))
    (COND ((ZEROP LEADER-LENGTH)
           (SETQ LEADER-SIZE 0))
          (T
           (SETQ LEADER-SIZE (+ 2 LEADER-LENGTH))))
    (COND ((NOT (NULL WIREABLE?))
           (SETQ TOTAL-DATA-SIZE (ARRAY-TOTAL-DATA-SIZE TYPE INDEX-LENGTH))
           (SETQ BOXED-DATA-SIZE (ARRAY-BOXED-DATA-SIZE TYPE INDEX-LENGTH))
           (COND ((NOT (SETQ LONG-ARRAY? (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)))
                  (SETQ HEADER-SIZE (MAX 1 N-DIMENSIONS)))
                 (T
                  (SETQ HEADER-SIZE (1+ (MAX 1 N-DIMENSIONS)))))
           (SETQ BOXED-SIZE (+ LEADER-SIZE HEADER-SIZE BOXED-DATA-SIZE))
           (SETQ TOTAL-SIZE (+ LEADER-SIZE HEADER-SIZE TOTAL-DATA-SIZE)))
          ((NULL DISPLACED-TO)
           (SETQ TOTAL-DATA-SIZE (ARRAY-TOTAL-DATA-SIZE TYPE INDEX-LENGTH))
           (SETQ BOXED-DATA-SIZE (ARRAY-BOXED-DATA-SIZE TYPE INDEX-LENGTH))
           (COND ((NOT (SETQ LONG-ARRAY? (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)))
                  (SETQ HEADER-SIZE (MAX 1 N-DIMENSIONS)))
                 (T
                  (SETQ HEADER-SIZE (1+ (MAX 1 N-DIMENSIONS)))))
           (SETQ BOXED-SIZE (+ LEADER-SIZE HEADER-SIZE BOXED-DATA-SIZE))
           (SETQ TOTAL-SIZE (+ LEADER-SIZE HEADER-SIZE TOTAL-DATA-SIZE)))
          (T
           (SETQ BOXED-DATA-SIZE (IF DISPLACED-INDEX-OFFSET 3 2))
           (SETQ TOTAL-SIZE (+ LEADER-SIZE (MAX 1 N-DIMENSIONS) BOXED-DATA-SIZE))
           (SETQ BOXED-SIZE TOTAL-SIZE)))
    (VALUES TOTAL-SIZE BOXED-SIZE)))

(DEFUN ADJUST-ARRAY (ARRAY NEW-DIMENSIONS &REST KEYARGS
                     &KEY ELEMENT-TYPE
                          (INITIAL-ELEMENT NIL INITIAL-ELEMENT-P)
                          (INITIAL-CONTENTS NIL INITIAL-CONTENTS-P)
                          FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)
  "Alter dimensions, contents or displacedness of ARRAY.
May modify ARRAY or forward it to a new array.  In either case ARRAY is returned.
The dimensions are altered to be those in the list NEW-DIMENSIONS.
DISPLACED-TO and DISPLACED-INDEX-OFFSET are used to make ARRAY be displaced.
 They mean the same as in MAKE-ARRAY.
INITIAL-CONTENTS is as in MAKE-ARRAY.
 ARRAY's entire contents are initialized from this
 after its shape has been changed.  The old contents become irrelevant.
If neither INITIAL-CONTENTS nor DISPLACED-TO is specified, the old contents
 of ARRAY are preserved.  Each element is preserved according to its subscripts.

INITIAL-ELEMENT, if specified, is used to init any new elements created
 by the reshaping; that is, elements at subscripts which were previously
 out of bounds.  If this is not specified, NIL, 0 or 0.0 is used acc. to array type.
ELEMENT-TYPE if non-NIL causes an error if ARRAY is not of the array type
 which MAKE-ARRAY would create given the same ELEMENT-TYPE.  Just an error check.
FILL-POINTER, if specified, sets the fill pointer of ARRAY."
  (CHECK-TYPE ARRAY ARRAY)
  (typecase new-dimensions
    ((fixnum 0)
     (unless (= (array-rank array) 1)
       (ferror "1 is the wrong number of dimensions for ~S" array))
     (setq new-dimensions (ncons new-dimensions)))
    (list
     (unless (= (array-rank array) (length new-dimensions))
       (ferror "~S is the wrong number of dimensions for ~S" (length new-dimensions) array))
     (dolist (dimension new-dimensions)
       (check-type dimension (fixnum 0))))
    (otherwise
     (ferror "~S is not a valid array dimension specification." new-dimensions)))
  (when (and displaced-index-offset (null displaced-to))
    (cond ((array-displaced-p array)
           (setq displaced-to (array-indirect-to array))
           (setq keyargs (append keyargs `(:displaced-to ,displaced-to))))
          (t
           (ferror ":DISPLACED-INDEX-OFFSET but no :DISPLACED-TO"))))
  (when (or (and initial-element-p initial-contents-p)
            (and initial-element-p displaced-to)
            (and initial-contents-p displaced-to))
    (ferror "Only one of :INITIAL-ELEMENT, :INITIAL-CONTENTS, and :DISPLACED-TO may be specified"))
  (WHEN ELEMENT-TYPE
    (LET ((ELT (CAR (RASSQ (ARRAY-TYPE-FROM-ELEMENT-TYPE ELEMENT-TYPE)
                           ARRAY-ELEMENT-TYPE-ALIST))))
      (UNLESS (EQ ELT (CAR (RASSQ (ARRAY-TYPE ARRAY) ARRAY-ELEMENT-TYPE-ALIST)))
        (FERROR "~S is ~S~:[ (=> ~S)~;~*~], but ~S is of element-type ~S"
                'ELEMENT-TYPE ELEMENT-TYPE (EQUAL ELEMENT-TYPE ELT) ELT
                'ARRAY (CAR (RASSQ (ARRAY-TYPE ARRAY) ARRAY-ELEMENT-TYPE-ALIST))))))
  (IF DISPLACED-TO
      (cond ((do ((x displaced-to (array-indirect-to x)))
                 ((null x) nil)
               (when (eq x array)
                 (ferror "You can't ~:[indirectly~;directly~] displace array ~S to itself" (eq x displaced-to) array))))
            ((neq (array-type array) (array-type displaced-to))
             (ferror "~S has element type ~S, and can't be displaced to ~S with element type ~S"
                     array
                     (car (rassq (array-type array) array-element-type-alist))
                     displaced-to
                     (car (rassq (array-type displaced-to) array-element-type-alist))))
            ((and displaced-index-offset
                  (or (not (fixnump displaced-index-offset))
                      (minusp displaced-index-offset)))
             (ferror "displaced-index-offset must be a non-negative fixnum"))
            ((> (+ (or displaced-index-offset 0)
                   (apply #'* new-dimensions))
                (array-length displaced-to))
             (ferror "~s would be displaced past the end of ~S" array displaced-to))
            ((AND (ARRAY-DISPLACED-P ARRAY)
                  (EQ (NULL DISPLACED-INDEX-OFFSET)
                      (NULL (ARRAY-INDEX-OFFSET ARRAY))))
             (CHANGE-INDIRECT-ARRAY ARRAY (ARRAY-TYPE ARRAY) NEW-DIMENSIONS
                                    DISPLACED-TO DISPLACED-INDEX-OFFSET))
            (t
             (STRUCTURE-FORWARD ARRAY
                                (APPLY #'MAKE-ARRAY NEW-DIMENSIONS
                                       :LEADER-LIST (LIST-ARRAY-LEADER ARRAY)
                                       :TYPE (ARRAY-TYPE ARRAY)
                                       KEYARGS))))
    (case (ARRAY-RANK ARRAY)
      (0)
      (1
       (LET ((OLD-LEN (ARRAY-LENGTH ARRAY)))
         (SETQ ARRAY (ADJUST-ARRAY-SIZE ARRAY (CAR NEW-DIMENSIONS)))
         (WHEN INITIAL-ELEMENT-P
           (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT OLD-LEN (CAR NEW-DIMENSIONS)))))
      (otherwise
       (ARRAY-GROW-1 ARRAY NEW-DIMENSIONS INITIAL-ELEMENT-P INITIAL-ELEMENT))))
  (IF INITIAL-CONTENTS-P
      (FILL-ARRAY-FROM-SEQUENCES ARRAY INITIAL-CONTENTS 0 0))
  (IF FILL-POINTER
      (SETF (FILL-POINTER ARRAY)
            (IF (EQ FILL-POINTER T) (ARRAY-LENGTH ARRAY) FILL-POINTER)))
  (INVALIDATE-ARRAY-CACHE)
  ARRAY)

(DEFUN ARRAY-GROW-1 (ARRAY DIMENSIONS INITIAL-ELEMENT-P INITIAL-ELEMENT
                     &AUX (OLD-DIMS (ARRAY-DIMENSIONS ARRAY))
                     INDEX NEW-ARRAY)
  (declare (ignore initial-element-p))
  (PROG ()
        ;; Make the new array.
        (SETQ NEW-ARRAY (MAKE-ARRAY DIMENSIONS
                                    :AREA (%AREA-NUMBER ARRAY)
                                    :TYPE (ARRAY-TYPE ARRAY)
                                    :INITIAL-ELEMENT INITIAL-ELEMENT
                                    :LEADER-LENGTH (ARRAY-LEADER-LENGTH ARRAY)))
        ;; Copy the array leader.
        (DO ((I 0 (1+ I))
             (N (OR (ARRAY-LEADER-LENGTH ARRAY) 0) (1- N)))
            ((ZEROP N))
          (SETF (ARRAY-LEADER NEW-ARRAY I) (ARRAY-LEADER ARRAY I)))
        ;; Check for zero-size array, which the code below doesn't handle correctly
        (DO ((L DIMENSIONS (CDR L)) (L1 OLD-DIMS (CDR L1)))
            ((NULL L) NIL)
          (WHEN (OR (ZEROP (CAR L)) (ZEROP (CAR L1)))
            (GO DONE)))
        ;; Create a vector of fixnums to use as subscripts to step through the arrays.
        (SETQ INDEX (MAKE-LIST (LENGTH DIMENSIONS) :INITIAL-ELEMENT 0))
        ;; Make the first increment of INDEX bring us to element 0 0 0 0..
        (SETF (CAR INDEX) -1)
     LOOP
        ;; Increment the vector of subscripts INDEX.
        ;; Go to DONE if we have exhausted all elements that need copying.
        (DO ((I INDEX (CDR I))
             (O OLD-DIMS (CDR O))
             (N DIMENSIONS (CDR N)))
            ((NULL I) (GO DONE))
          ;; Increment one index
          (INCF (CAR I))
          ;; and decide whether to "carry" to the next one.
          (IF (OR ( (CAR I) (CAR O)) ( (CAR I) (CAR N)))
              (SETF (CAR I) 0)
            (RETURN NIL)))
        (APPLY #'ASET (APPLY #'AREF ARRAY INDEX) NEW-ARRAY INDEX)
        (GO LOOP)
     DONE
        ;; The contents have been copied.  Copy a few random things.
        (%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
                %%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
        (STRUCTURE-FORWARD ARRAY NEW-ARRAY)
        (INVALIDATE-ARRAY-CACHE)
        (RETURN NEW-ARRAY)))

(DEFUN VECTOR (&REST OBJECTS)
  "Return a vector (1-dimensional array) whose elements are the OBJECTS."
  ;; Could actually %blt-typed, since &rest list is contiguous
  (LET ((RESULT (MAKE-ARRAY (LENGTH OBJECTS))))
    (DO ((I 0 (1+ I))
         (TAIL OBJECTS (CDR TAIL)))
        ((NULL TAIL))
      (SETF (AREF RESULT I) (CAR TAIL)))
    RESULT))

(defvar *MAKE-LIST-CDR-CODE-DEFAULT* t)  ;Someday, set this to NIL.  Note optimizer in QCOPT
                ;which also looks at this.
(DEFUN MAKE-LIST (LENGTH &REST OPTIONS)
  "Create a list LENGTH long.  :AREA keyword says where, :INITIAL-ELEMENT sets each element.
CDR-CODED specifies whether to use CDR-NEXT, etc.  If not supplied, this default to T for now,
but someday will change to NIL."
  (DECLARE (ARGLIST LENGTH &KEY AREA INITIAL-ELEMENT CDR-CODED))
  (LET ((LENGTH-OF-OPTIONS (LENGTH OPTIONS))
        (AREA NIL) (INITIAL-ELEMENT NIL)
        (cdr-coded *make-list-cdr-code-default*))
    ;; Figure out whether it is old-style.
    ;;>> There is a compiler style-checker against this.  Flush it soon.
    (IF (= LENGTH-OF-OPTIONS 1)
        ;; It is old-style.
        (SETQ AREA LENGTH
              LENGTH (FIRST OPTIONS))
      ;; It is new-style.
      (IF (ODDP LENGTH-OF-OPTIONS)
          (FERROR "Odd-length options list: ~S" OPTIONS))
      (DO ((OPTIONS OPTIONS (CDDR OPTIONS)))
          ((NULL OPTIONS))
        (LET ((VALUE (SECOND OPTIONS)))
          (CASE (FIRST OPTIONS)
            (:AREA (SETQ AREA VALUE))
            ((:INITIAL-ELEMENT :INITIAL-VALUE)
             (SETQ INITIAL-ELEMENT VALUE))
            (:CDR-CODED (setq cdr-coded value))
            (OTHERWISE
             (FERROR "~S is not a known keyword." (FIRST OPTIONS)))))))
    (if cdr-coded (%MAKE-LIST INITIAL-ELEMENT AREA LENGTH)
      (make-list-with-cons initial-element area length))))

(defun make-list-with-cons (initial-element area length)
  (let ((ans nil))
    (dotimes (c length)
      (setq ans (cons-in-area initial-element ans area)))
    ans))

;;; This is an internal function designed to be called by code generated
;;; be a compiler optimizer of simple calls to MAKE-ARRAY.
(DEFUN SIMPLE-MAKE-ARRAY (DIMENSIONS &OPTIONAL (TYPE 'ART-Q) AREA LEADER-LENGTH INITIAL-ELEMENT)
  (LET (BOXED-DATA-SIZE TOTAL-DATA-SIZE BOXED-SIZE TOTAL-SIZE LEADER-SIZE
        ARRAY HEADER HEADER-SIZE)
    (TYPECASE TYPE
      (FIXNUM
       ;; Perfunctory test to decide whether the array-type is shifted or not.
       (UNLESS (ZEROP (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))
         (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD TYPE))))
      (SYMBOL
       (COND ((MEMQ TYPE ARRAY-TYPES)
              (SETQ TYPE (%LOGLDB %%ARRAY-TYPE-FIELD (SYMBOL-VALUE TYPE))))
             ((MEMQ TYPE ARRAY-TYPE-KEYWORDS)
              (SETQ TYPE (FIND-POSITION-IN-LIST TYPE ARRAY-TYPE-KEYWORDS)))
             (T
              (FERROR "~S is not a valid array type." TYPE))))
      (OTHERWISE
       (FERROR "~S is not a valid array type." TYPE)))
    (TYPECASE DIMENSIONS
      ((FIXNUM 0))
      (LIST
       (IF (= (LENGTH DIMENSIONS) 1)
           (SETQ DIMENSIONS (CAR DIMENSIONS))
         (RETURN-FROM SIMPLE-MAKE-ARRAY
           (DONT-OPTIMIZE
             (MAKE-ARRAY DIMENSIONS
                         :TYPE TYPE :AREA AREA :LEADER-LENGTH LEADER-LENGTH
                         :INITIAL-ELEMENT INITIAL-ELEMENT)))))
      (OTHERWISE
       (FERROR "~S is not a valid array dimension specifier." DIMENSIONS)))
    (TYPECASE LEADER-LENGTH
      (NULL         (SETQ LEADER-LENGTH 0))
      ((FIXNUM 0))
      (OTHERWISE    (FERROR "~S is not a valid leader-length specification." LEADER-LENGTH)))
    (COND ((ZEROP LEADER-LENGTH)
           (SETQ LEADER-SIZE 0)
           (SETQ HEADER (%LOGDPB 1
                                 %%ARRAY-NUMBER-DIMENSIONS
                                 (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))
          (T
           (SETQ LEADER-SIZE (+ 2 LEADER-LENGTH))
           (SETQ HEADER (%LOGDPB 1
                                 %%ARRAY-LEADER-BIT
                                 (%LOGDPB 1
                                          %%ARRAY-NUMBER-DIMENSIONS
                                          (%LOGDPB TYPE %%ARRAY-TYPE-FIELD 0))))))
    (SETQ TOTAL-DATA-SIZE (ARRAY-TOTAL-DATA-SIZE TYPE DIMENSIONS))
    (SETQ BOXED-DATA-SIZE (ARRAY-BOXED-DATA-SIZE TYPE DIMENSIONS))
    (COND ((NOT (> DIMENSIONS %ARRAY-MAX-SHORT-INDEX-LENGTH))
           (SETQ HEADER (+ DIMENSIONS HEADER))
           (SETQ HEADER-SIZE 1))
          (T
           (SETQ HEADER (%LOGDPB 1 %%ARRAY-LONG-LENGTH-FLAG HEADER))
           (SETQ HEADER-SIZE 2)))
    (SETQ BOXED-SIZE (+ LEADER-SIZE HEADER-SIZE BOXED-DATA-SIZE))
    (SETQ TOTAL-SIZE (+ LEADER-SIZE HEADER-SIZE TOTAL-DATA-SIZE))
    (SETQ ARRAY (%MAKE-ARRAY
                  HEADER
                  DIMENSIONS
                  LEADER-LENGTH
                  AREA
                  TOTAL-SIZE
                  BOXED-SIZE))
    (UNLESS (NULL INITIAL-ELEMENT)
      (ARRAY-INITIALIZE ARRAY INITIAL-ELEMENT))
    (VALUES ARRAY TOTAL-SIZE BOXED-SIZE)))

;A further optimization of the above!
(DEFUN SIMPLE-MAKE-ARRAY-1D-Q-SHORT (DIMENSION)
  ;dimension is guaranteed to be positive by test made in optimizer.
  ;not LONG
  ;type is ART-Q
  ;data-length is dimension.
  (VALUES (%make-array
            (+ DIMENSION (%LOGDPB 1 %%ARRAY-NUMBER-DIMENSIONS ART-Q))
            DIMENSION
            0
            NIL
            (1+ dimension)                      ;One extra for header.
            (1+ dimension))
          DIMENSION))

(DEFUN SIMPLE-MAKE-ARRAY-1D-STRING-WITH-FILL-POINTER (INDEX-LENGTH)
  (ETYPECASE INDEX-LENGTH
    ((FIXNUM 0))
    (LIST (SETQ INDEX-LENGTH (CAR INDEX-LENGTH))))
  (IF (> INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)
      (VALUES (%MAKE-ARRAY (%LOGDPB 1
                                    %%ARRAY-LONG-LENGTH-FLAG
                                    (%LOGDPB 1
                                             %%ARRAY-LEADER-BIT
                                             (%LOGDPB 1
                                                      %%ARRAY-NUMBER-DIMENSIONS
                                                      ART-STRING)))
                           INDEX-LENGTH
                           1
                           NIL
                           (+ 5 (CEILING INDEX-LENGTH 4))
                           5))
    (VALUES (%MAKE-ARRAY (%LOGDPB 1
                                  %%ARRAY-LEADER-BIT
                                  (%LOGDPB 1
                                           %%ARRAY-NUMBER-DIMENSIONS
                                           (+ ART-STRING INDEX-LENGTH)))
                         INDEX-LENGTH
                         1
                         NIL
                         (+ 4 (CEILING INDEX-LENGTH 4))
                         4))))

(DEFUN FILL-ARRAY (ARRAY SIZE VALUE)
  (ARRAY-INITIALIZE ARRAY VALUE 0 SIZE))

(DEFUN ARRAY-INITIALIZE (ORIGINAL-ARRAY VALUE &OPTIONAL (START 0) END
                         &AUX (ARRAY ORIGINAL-ARRAY) (UNFORWARDED-ARRAY ORIGINAL-ARRAY)
                         (OFFSET 0))
  "Set all the elements of ARRAY to VALUE, or all elements from START to END.
If END is NIL or not specified, the active length of ARRAY is used."
  (OR END (SETQ END (LENGTH ARRAY)))
  (ASSERT ( 0 START END (ARRAY-LENGTH ARRAY)) (START END)
          "START is ~S and END is ~S, for ~S." START END ARRAY)
  (cond ((zerop (array-rank array))
         (setf (aref array) value))
        ((< END (+ START 30.))
         ;; If number of elements to be hacked is small, just do them.
         (DO ((I START (1+ I))) (( I END))
           (SETF (AR-1-FORCE ARRAY I) VALUE)))
        (t
         ;; Handle indirect arrays by finding the array indirected to
         ;; and updating the start and end indices if appropriate.
         (DO () ((NOT (ARRAY-INDIRECT-P ARRAY)))
           (AND (ARRAY-INDEX-OFFSET ARRAY)
                (INCF OFFSET (ARRAY-INDEX-OFFSET ARRAY)))
           (SETQ ARRAY (ARRAY-INDIRECT-TO ARRAY)))
         ;; Handle forwarded arrays.
         (UNLESS (= (%P-DATA-TYPE ARRAY) DTP-ARRAY-HEADER)
           (SETQ ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY)))
         (UNLESS (= (%P-DATA-TYPE UNFORWARDED-ARRAY) DTP-ARRAY-HEADER)
           (SETQ UNFORWARDED-ARRAY (FOLLOW-STRUCTURE-FORWARDING UNFORWARDED-ARRAY)))
         (LET* ((ENTRIES-PER-Q (ARRAY-ELEMENTS-PER-Q (%P-LDB %%ARRAY-TYPE-FIELD UNFORWARDED-ARRAY)))
                (BITS-PER-ELEMENT (ARRAY-BITS-PER-ELEMENT (%P-LDB %%ARRAY-TYPE-FIELD UNFORWARDED-ARRAY)))
                (START (+ START OFFSET))
                (END (+ END OFFSET))
                (DATA-OFFSET (ARRAY-DATA-OFFSET ARRAY))
                ;; Compute how many words are in the repeating unit that we replicate with %BLT.
                ;; This is 1 word unless an element is bigger than that.
                (BLT-DISTANCE (IF (PLUSP ENTRIES-PER-Q) 1 (- ENTRIES-PER-Q)))
                ;; This is how many elements it takes to make BLT-DISTANCE words.
                (Q-BOUNDARY-ELTS (MAX 1 ENTRIES-PER-Q))
                ;; We must deposit element by element until this element
                ;; in order to make sure we have a full word of elements stored
                ;; Beyond this, we can blt entire words.
                (STOP-ELEMENT-BY-ELEMENT
                  (MIN END
                       (* Q-BOUNDARY-ELTS
                          (1+ (CEILING START Q-BOUNDARY-ELTS)))))
                ;; We must stop our word-wise copying before this element number
                ;; to avoid clobbering any following elements which are beyond END.
                (END-WORD-WISE
                  (MAX START (* Q-BOUNDARY-ELTS (FLOOR END Q-BOUNDARY-ELTS))))
                ;; Compute index in words, wrt array data, of the first data word
                ;; that we will not fill up an element at a time.
                (UNINITIALIZED-DATA-OFFSET
                  (+ DATA-OFFSET
                     (* BLT-DISTANCE (CEILING STOP-ELEMENT-BY-ELEMENT Q-BOUNDARY-ELTS))))
                ;; Compute the length of the data in the array, in Qs, if caller didn't supply it.
                (DATA-LENGTH
                  (IF (PLUSP ENTRIES-PER-Q)
                      (TRUNCATE END-WORD-WISE ENTRIES-PER-Q)
                    (* END-WORD-WISE (- ENTRIES-PER-Q)))))
           ;; Fill in any elements in an incomplete first word,
           ;; plus one full word's worth.
           ;; We must use the original array to store element by element,
           ;; since the element size of the array indirected to may be different.
           (DO ((I START (1+ I)))
               ((= I STOP-ELEMENT-BY-ELEMENT))
             (SETF (AR-1-FORCE ORIGINAL-ARRAY (- I OFFSET)) VALUE))
           ;; Now fill in the elements in the incomplete last word.
           (DO ((I END-WORD-WISE (1+ I)))
               (( I END))
             (SETF (AR-1-FORCE ORIGINAL-ARRAY (- I OFFSET)) VALUE))
           ;; Now copy the data word by word (or by two words for ART-FLOAT!)
           ;; There is no hope of passing %BLT pointers that are GC-safe.
           (IF (PLUSP (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET)))
               (WITHOUT-INTERRUPTS
                 ;; If the array is displaced to a random location, use that location
                 ;; as the data start.  Arrays displaced to other arrays
                 ;; were handled above.
                 (IF (ARRAY-DISPLACED-P ARRAY)
                     (SETQ ARRAY (- (%POINTER (ARRAY-INDIRECT-TO ARRAY)) DATA-OFFSET)))
                 (IF BITS-PER-ELEMENT
                     ;; Numeric array.
                     (%BLT (%MAKE-POINTER-OFFSET DTP-LOCATIVE
                                                 ARRAY (- UNINITIALIZED-DATA-OFFSET BLT-DISTANCE))
                           (%MAKE-POINTER-OFFSET DTP-LOCATIVE
                                                 ARRAY UNINITIALIZED-DATA-OFFSET)
                           (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET))
                           1)
                   (%BLT-TYPED (%MAKE-POINTER-OFFSET DTP-LOCATIVE
                                                     ARRAY (- UNINITIALIZED-DATA-OFFSET BLT-DISTANCE))
                               (%MAKE-POINTER-OFFSET DTP-LOCATIVE
                                                     ARRAY UNINITIALIZED-DATA-OFFSET)
                               (- DATA-LENGTH (- UNINITIALIZED-DATA-OFFSET DATA-OFFSET))
                               1)))))))
  ORIGINAL-ARRAY)

(DEFUN ARRAY-DATA-OFFSET (ARRAY)
  "Return the offset in Qs of the first array element from the array header.
Not meaningful for displaced arrays."
  (+ (ARRAY-RANK ARRAY) (%P-LDB %%ARRAY-LONG-LENGTH-FLAG ARRAY)))

(DEFUN MAKE-SYMBOL (PNAME &OPTIONAL PERMANENT-P)
  "Create a symbol with name PNAME.
The symbol starts out with no value, definition or properties.
PERMANENT-P forces areas to those normally used for symbols."
  (CHECK-TYPE PNAME STRING)
  (LET ((LENGTH (LENGTH PNAME))
        NEW)
    (COND ((AND PERMANENT-P
                (NOT (= (%AREA-NUMBER PNAME) P-N-STRING)))
           (LET* ((%INHIBIT-READ-ONLY T))
             (SETQ NEW (MAKE-STRING LENGTH :AREA P-N-STRING))
             (COPY-ARRAY-PORTION PNAME 0 LENGTH NEW 0 LENGTH)
             (SETQ PNAME NEW)))
          ((SIMPLE-ARRAY-P PNAME))
          (T
           (SETQ NEW (MAKE-STRING LENGTH))
           (COPY-ARRAY-PORTION PNAME 0 LENGTH NEW 0 LENGTH)
           (SETQ PNAME NEW))))
  (LET ((SYMBOL (%make-structure dtp-symbol
                                 dtp-symbol-header
                                 pname
                                 nil
                                 (and permanent-p nr-sym)
                                 length-of-atom-head
                                 length-of-atom-head)))
    ;; symbol-value and symbol-function were initialized to NIL -- (F)MAKUNBOUND
    (WITHOUT-INTERRUPTS
      (%P-STORE-POINTER (LOCF (SYMBOL-FUNCTION SYMBOL)) SYMBOL)
      (%P-STORE-DATA-TYPE (LOCF (SYMBOL-FUNCTION SYMBOL)) DTP-NULL)
      (%P-STORE-POINTER (LOCF (SYMBOL-VALUE SYMBOL)) SYMBOL)
      (%P-STORE-DATA-TYPE (LOCF (SYMBOL-VALUE SYMBOL)) DTP-NULL))
    SYMBOL))

;;;; Runtime support for compiled functions that use &key.

;;; Required keyword args are initialized to this value.
;;; We compare the values against it to see which ones are missing.
;;; This number is used only at the beginning of cold-load time,
;;; because the value is set up by the cold-load builder
;;; and is available from the very beginning.
(DEFVAR KEYWORD-GARBAGE 643643)

(ADD-INITIALIZATION "Keyword-garbage" '(SETQ KEYWORD-GARBAGE (NCONS NIL)) :ONCE)

;;; Given ARGS, the list of key names and values;
;;; KEYKEYS, the list of keywords we understand, in their order;
;;; and FRAME-POINTER, a locative pointing at our caller's frame;
;;; decode the ARGS and stick the values into the right slots in the frame.
;;; SPECVAR-LIST is a list of NIL for nonspecial keyword args
;;; and symbols for special ones.
;;; It runs in parallel with KEYKEYS.
;;; If there are duplicate keywords in the supplied args, all but the first are ignored.

;;; New calling sequence:
;;;  FIRST-KEYARG-POINTER points to the local slot for the first keyword arg.
;;>>> Slow slow slow slow.  Oh god this is slow.
(DEFUN STORE-KEYWORD-ARG-VALUES (FIRST-KEYARG-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS
                                 &optional kludge)
  (DECLARE (DBG:ERROR-REPORTER))
  ;;>> This is for compatibility with an old calling sequence. 22-Dec-85
  (if kludge (setq first-keyarg-pointer kludge))
  (DO* ((ARGS-LEFT ARGS (IF (ENDP (CDR ARGS-LEFT))
                            (FERROR "Odd number of keyword args: ~S" ARGS)
                          (CDDR ARGS-LEFT)))
        (KEYWORD (CAR ARGS-LEFT) (CAR ARGS-LEFT))
        (FOUND-FLAGS 0))
       ((NULL ARGS-LEFT))
   RETRY
    (LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
      (COND (INDEX
             (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- INDEX))))
               (SETQ FOUND-FLAGS (DPB 1 (BYTE 1 INDEX) FOUND-FLAGS))
               (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) FIRST-KEYARG-POINTER INDEX)))
            (ALLOW-OTHER-KEYS)
            ((EQ KEYWORD ':ALLOW-OTHER-KEYS))
            ((SETQ ALLOW-OTHER-KEYS (GETF ARGS ':ALLOW-OTHER-KEYS)))
            (T
             (SETQ KEYWORD (CERROR :NEW-KEYWORD NIL 'SYS:UNDEFINED-KEYWORD-ARGUMENT
                                   "Keyword arg keyword ~S unrecognized."
                                   KEYWORD (CADR ARGS-LEFT)))
             (IF KEYWORD (GO RETRY)))))))

;;; Given ARGS, the list of key names and values;
;;; KEYKEYS, the list of keywords we understand, in their order;
;;; and PREV-SLOT-POINTER, a locative pointing at the local slot
;;; just before the first of those for our keyword args;
;;; decode the ARGS and stick the values into the right slots in the frame.
;;; If there are duplicate keywords in the supplied args, all but the first are ignored.
(DEFUN STORE-KEYWORD-ARG-VALUES-INTERNAL-LAMBDA
       (PREV-SLOT-POINTER ARGS KEYKEYS ALLOW-OTHER-KEYS &optional ignore)
  (DO* ((ARGS-LEFT ARGS  (IF (ENDP (CDR ARGS-LEFT))
                             (FERROR "Odd number of keyword args: ~S" ARGS)
                           (CDDR ARGS-LEFT)))
        (KEYWORD (CAR ARGS-LEFT) (CAR ARGS-LEFT))
        (FOUND-FLAGS 0))
       ((NULL ARGS-LEFT))
   RETRY
    (LET ((INDEX (FIND-POSITION-IN-LIST KEYWORD KEYKEYS)))
      (COND (INDEX
             (WHEN (ZEROP (LOGAND 1 (ASH FOUND-FLAGS (- INDEX))))
               (SETQ FOUND-FLAGS (DPB 1 (BYTE 1 INDEX) FOUND-FLAGS))
               (%P-STORE-CONTENTS-OFFSET (CADR ARGS-LEFT) PREV-SLOT-POINTER (1+ INDEX))))
            (ALLOW-OTHER-KEYS)
            ((EQ KEYWORD ':ALLOW-OTHER-KEYS))
            ((SETQ ALLOW-OTHER-KEYS (GETF ARGS ':ALLOW-OTHER-KEYS)))
            (T
             (SETQ KEYWORD (CERROR :NEW-KEYWORD NIL 'SYS:UNDEFINED-KEYWORD-ARGUMENT
                                   "Keyword arg keyword ~S unrecognized."
                                   KEYWORD (CADR ARGS-LEFT)))
             (IF KEYWORD (GO RETRY)))))))

;;; Now microcoded.
;(DEFUN NTH (N OBJECT)
;   (CHECK-ARG N (AND (FIXNUMP N) (NOT (MINUSP N))) "a non-negative integer")
;   (DO ((N N (1- N))
;       (OBJECT OBJECT (CDR OBJECT)))
;       ((ZEROP N) (CAR OBJECT))))

;;; Now microcoded.
;(DEFUN ARRAY-LEADER-LENGTH (ARRAY)
;  "Return the number of elements in ARRAY's leader, or NIL if no leader."
;  (COND ((ARRAY-HAS-LEADER-P ARRAY)
;        (%P-LDB-OFFSET %%ARRAY-LEADER-LENGTH ARRAY -1)))))

;;; Now microcoded.
;(DEFUN ARRAY-DIMENSION (ARRAY DIMENSION-NUMBER &AUX RANK INDEX-LENGTH LONG-ARRAY-P)
;  "Return the length of dimension DIMENSION-NUMBER of ARRAY.  The first dimension is number 0."
;  (CHECK-ARG ARRAY ARRAYP "an array")
;  (SETQ LONG-ARRAY-P (%P-LDB-OFFSET %%ARRAY-LONG-LENGTH-FLAG ARRAY 0))
;  (SETQ RANK (%P-LDB-OFFSET %%ARRAY-NUMBER-DIMENSIONS ARRAY 0))
;  (COND ((NOT (< -1 DIMENSION-NUMBER RANK))
;        NIL)
;       ((AND (< DIMENSION-NUMBER (1- RANK)) (NOT ARRAY-INDEX-ORDER))
;        (%P-LDB-OFFSET %%Q-POINTER ARRAY (+ DIMENSION-NUMBER 1 LONG-ARRAY-P)))
;       ((AND (> DIMENSION-NUMBER 0) ARRAY-INDEX-ORDER)
;        (%P-LDB-OFFSET %%Q-POINTER ARRAY (+ (- RANK DIMENSION-NUMBER) LONG-ARRAY-P)))
;       (T
;        (SETQ INDEX-LENGTH
;              (COND ((NOT (ZEROP (%P-LDB-OFFSET %%ARRAY-DISPLACED-BIT ARRAY 0)))
;                     (%P-LDB-OFFSET %%Q-POINTER ARRAY (1+ RANK)))
;                    ((= LONG-ARRAY-P 1) (%P-LDB-OFFSET %%Q-POINTER ARRAY 1))
;                    (T (%P-LDB-OFFSET %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY 0))))
;        ;; As far as I can tell, there's no way to determine the last dimension
;        ;; if the index-length is 0.  Might as well just give it as zero? -- DLA
;        (OR (ZEROP INDEX-LENGTH)
;            (DO I 1 (1+ I) ( I RANK)
;                (SETQ INDEX-LENGTH
;                      (TRUNCATE INDEX-LENGTH
;                                (%P-LDB-OFFSET %%Q-POINTER ARRAY (+ LONG-ARRAY-P I))))))
;        INDEX-LENGTH)))

(DEFUN ARRAY-ROW-MAJOR-INDEX (ARRAY &REST SUBSCRIPTS &AUX (RANK (ARRAY-RANK ARRAY)))
  "Return the combined index in ARRAY of the element identified by SUBSCRIPTS.
This value could be used as the second argument of AR-1-FORCE to access that element."
  ;; I suppose these should signal something ontaining bad-array-mixin or the like
  (ASSERT (= (LENGTH SUBSCRIPTS) RANK) ()
          "The number of subscripts (~D) given does not equal the rank of ~S"
          (LENGTH SUBSCRIPTS) ARRAY)
  (DO ((DIM 0 (1+ DIM))
       (RESULT 0)
       (TAIL SUBSCRIPTS (CDR TAIL)))
      ((= DIM RANK)
       RESULT)
    (LET ((SUBSCRIPT (CAR TAIL)))
      (ASSERT (AND (FIXNUMP SUBSCRIPT) (< -1 SUBSCRIPT (ARRAY-DIMENSION ARRAY DIM))) ()
              "Subscript ~D, ~S, is out of range (0 to ~D) for ~S"
              DIM SUBSCRIPT (ARRAY-DIMENSION ARRAY DIM) ARRAY)
      (SETQ RESULT (+ (* RESULT (ARRAY-DIMENSION ARRAY DIM)) SUBSCRIPT)))))

;; isn't compatibility a wonderful thing?
(DEFUN ARRAY-COLUMN-MAJOR-INDEX (ARRAY &REST SUBSCRIPTS)
  (APPLY #'ARRAY-ROW-MAJOR-INDEX ARRAY (REVERSE SUBSCRIPTS)))

(DEFUN ARRAY-IN-BOUNDS-P (ARRAY &REST SUBSCRIPTS)
  "T if the indices are in bounds for the dimensions of ARRAY."
  (CHECK-TYPE ARRAY ARRAY)
  (ASSERT (= (LENGTH SUBSCRIPTS) (ARRAY-RANK ARRAY)) ()
          "The number of subscripts (~D) given does not equal the rank of ~S"
          (LENGTH SUBSCRIPTS) ARRAY)
  (WHEN (= (LENGTH SUBSCRIPTS) (ARRAY-RANK ARRAY))
    (LOOP FOR S IN SUBSCRIPTS
          FOR D = 0 THEN (1+ D)
       WHEN (OR (not (fixnump s)) (< S 0) ( S (ARRAY-DIMENSION ARRAY D)))
         DO (RETURN NIL)
       FINALLY (RETURN T))))

;;; This is in microcode.
;(DEFUN EQUAL (A B)
;  (PROG NIL
;    L  (COND ((EQ A B) (RETURN T))
;             ((NOT (= (%DATA-TYPE A) (%DATA-TYPE B))) (RETURN NIL))
;             ((NUMBERP A) (RETURN (= A B)))
;             ((ARRAYP A)
;              (RETURN (AND (STRINGP A)
;                           (STRINGP B)
;                           (%STRING-EQUAL A 0 B 0 NIL))))
;             ((ATOM A) (RETURN NIL))
;             ((NOT (EQUAL (CAR A) (CAR B))) (RETURN NIL)))
;       (SETQ A (CDR A))
;       (SETQ B (CDR B))
;       (GO L)))

;;; This is here because the TV package calls it during initialization.
;;;>>>This is here because everything else is.

(defun delq (item list &optional (times -1))
  "Destructively remove some or all occurrences of ITEM from LIST.
If TIMES is supplied, it must be an integer specifying how many occurrences to remove.
You must do (SETQ FOO (DELQ X FOO)) to make sure FOO changes,
in case the first element of FOO is X."
  (declare (arglist item list &optional times))
  #+(target lambda)
  (compiler::%internal-delq item list times)
;;; Microcoded in System 101.  Don't delete this, we may need it later.
;;;>>>>Thanks, whoever you are.  And it even still works. -Keith
  #-(target lambda)
  (PROG (LL PL)
     A  (COND ((OR (= 0 TIMES) (ATOM LIST))
               (RETURN LIST))
              ((EQ ITEM (CAR LIST))
               (SETQ LIST (CDR LIST))
               (SETQ TIMES (1- TIMES))
               (GO A)))
        (SETQ LL LIST)
     B  (COND ((OR (= 0 TIMES) (ATOM LL))
               (RETURN LIST))
              ((EQ ITEM (CAR LL))
               (RPLACD PL (CDR LL))
               (SETQ TIMES (1- TIMES)))
              ((SETQ PL LL)))
        (SETQ LL (CDR LL))
        (GO B)))

(defun zl:every (list &functional pred &optional (step 'cdr))
  "T if every element of LIST satisfies PRED.
If STEP is specified, it is a function to move down the list
/(default is CDR.)."
  (do ((tail list (funcall step tail)))
      ((null tail) t)
    (unless (funcall pred (car tail))
      (return nil))))

(defun zl:some (list &functional pred &optional (step 'cdr))
  "Non-NIL if some element of LIST satisfies PRED.
If STEP is specified, it is a function to move down the list
/(default is CDR.).  The actual value is the tail of the list
whose car is the first element that satisfies PRED."
  (do ((tail list (funcall step tail)))
      ((null tail) nil)
    (when (funcall pred (car tail))
      (return tail))))


;;; The following are here because MAKE-SYMBOL uses them.  Perhaps some of them should be in
;;; microcode.

(DEFUN MAKUNBOUND (SYMBOL)
  "Cause SYMBOL to have no value.  It will be an error to evaluate it."
  (IF (OR (MEMQ SYMBOL '(T NIL))
          (KEYWORDP SYMBOL))
      (FERROR "Don't makunbound ~S please" SYMBOL)
    ;; Value cell could be forwarded somewhere, e.g. into microcode memory
    (DO ((LOC (LOCF (SYMBOL-VALUE SYMBOL)) (%P-CONTENTS-AS-LOCATIVE LOC)))
        (( (%P-DATA-TYPE LOC) DTP-ONE-Q-FORWARD)
         (WITHOUT-INTERRUPTS
           (%P-STORE-POINTER LOC SYMBOL)
           (%P-STORE-DATA-TYPE LOC DTP-NULL))))
    SYMBOL))

(DEFUN FMAKUNBOUND (SYMBOL)
  "Cause SYMBOL to have no function definition.  It will be an error to call it."
  (WITHOUT-INTERRUPTS
    (%P-STORE-POINTER (LOCF (SYMBOL-FUNCTION SYMBOL)) SYMBOL)
    (%P-STORE-DATA-TYPE (LOCF (SYMBOL-FUNCTION SYMBOL)) DTP-NULL))
  SYMBOL)

(DEFUN LOCATION-MAKUNBOUND (LOCATION &OPTIONAL VARIABLE-NAME)
  "Cause the word LOCATION points to to be unbound.
If LOCATION points to a symbol value cell, the symbol becomes unbound.
VARIABLE-NAME is a value to put in the pointer field of the DTP-NULL
value; it should be the variable or function-spec whose value,
in some sense, LOCATION represents."
  (CHECK-TYPE LOCATION (OR CONS LOCATIVE) "a location (something to take CONTENTS of)")
  (TYPECASE LOCATION
    (CONS (LOCATION-MAKUNBOUND (CDR-LOCATION-FORCE LOCATION)))
    (T
     ;; Cell could be forwarded somewhere, e.g. into microcode memory
     (DO ((LOC LOCATION (%P-CONTENTS-AS-LOCATIVE LOC)))
         (( (%P-DATA-TYPE LOC) DTP-ONE-Q-FORWARD)
          (WITHOUT-INTERRUPTS
            (%P-STORE-DATA-TYPE LOC DTP-NULL)
            (%P-STORE-POINTER LOC (OR VARIABLE-NAME LOCATION))))))))

(DEFUN LOCATION-BOUNDP (LOCATION)
  "T if the contents of LOCATION is not /"unbound/"."
  (CHECK-TYPE LOCATION (OR CONS LOCATIVE) "a location (something to take CONTENTS of)")
  (TYPECASE LOCATION
    (CONS
      ;; If the list has an explicit cdr-pointer, check its data type.
      ;; Otherwise the answer is always T.
     (SELECT (%P-CDR-CODE LOCATION)
       (CDR-NORMAL
        ( DTP-NULL (%P-LDB-OFFSET %%Q-DATA-TYPE (follow-structure-forwarding location) 1)))
       (CDR-ERROR
        (FERROR "Invalid CDR code in list at #o~O." (%POINTER LOCATION)))
       (OTHERWISE T)))
    (OTHERWISE
     ;; Cell could be forwarded somewhere, e.g. into microcode memory
     (LOOP FOR LOC = LOCATION THEN (%P-CONTENTS-AS-LOCATIVE LOC)
           FOR DTP = (%P-DATA-TYPE LOC)
           ;; EVCPs are now used for forwarding by the interpreter and lexical closures.
        UNTIL (AND ( DTP DTP-ONE-Q-FORWARD) ( DTP DTP-EXTERNAL-VALUE-CELL-POINTER))
        FINALLY (RETURN ( DTP DTP-NULL))))))

(DEFUN CDR-LOCATION-FORCE (LIST)
  "Return a locative pointing at the place where the cdr pointer of a list cell is stored.
This causes the list to be forwarded if it did not have an explicit cdr pointer."
  (SELECT (%P-CDR-CODE LIST)
    (CDR-NORMAL
     (%MAKE-POINTER-OFFSET DTP-LOCATIVE (FOLLOW-STRUCTURE-FORWARDING LIST) 1))
    (CDR-ERROR
     (FERROR "Invalid CDR code in list at #o~O." (%POINTER LIST)))
    (OTHERWISE
     (WITHOUT-INTERRUPTS                        ;CDR-NIL or CDR-NEXT.
       (SETF (CDR LIST) (CDR LIST)))
       (%MAKE-POINTER-OFFSET DTP-LOCATIVE (FOLLOW-STRUCTURE-FORWARDING LIST) 1))))

(DEFUN STORE-CONDITIONAL (LOCATION OLD NEW)
  "If the cdr of LOCATION matches OLD, store NEW there instead."
  (CHECK-TYPE LOCATION (OR CONS LOCATIVE) "a location (something to take CONTENTS of)")
  (WHEN (CONSP LOCATION) (SETQ LOCATION (CDR-LOCATION-FORCE LOCATION)))
  (%STORE-CONDITIONAL LOCATION OLD NEW))

(DEFUN FSET (SYMBOL DEFINITION)
  "Set the function definition of SYMBOL to DEFINITION.
This works only on symbols, and does not make warnings, record source files, etc.
To do those things, use FDEFINE."
  (CHECK-TYPE SYMBOL SYMBOL)
  ;; must use this, as (defsetf symbol-function fset)
  (SETF (CONTENTS (LOCF (SYMBOL-FUNCTION SYMBOL))) DEFINITION)
  DEFINITION)

(MAKE-OBSOLETE NAMED-STRUCTURE-SYMBOL "use NAMED-STRUCTURE-P")
;;; Now microcoded.  But I thought it was obsolete!
;(DEFUN NAMED-STRUCTURE-SYMBOL (NAMED-STRUCTURE)
;  "Given an array which is a named structure, return its named structure type."
;  (LET ((SYM (IF (ARRAY-HAS-LEADER-P NAMED-STRUCTURE)
;                (ARRAY-LEADER NAMED-STRUCTURE 1)
;              (AREF NAMED-STRUCTURE 0))))
;    (IF (SYMBOLP SYM) SYM
;      (AND (CLOSUREP SYM)
;          (SETQ SYM (CAR (%MAKE-POINTER DTP-LIST SYM))))
;      (OR (SYMBOLP SYM)
;         (FERROR "~S not a symbol in named-structure-symbol slot of ~S"
;                 SYM NAMED-STRUCTURE))
;      SYM)))

;;; Now microcoded.
;(DEFUN NAMED-STRUCTURE-P (STRUCTURE)
;   "If argument is a named-structure, return its name, otherwise NIL"
;   (AND (ARRAYP STRUCTURE)
;       (NOT (ZEROP (%P-LDB-OFFSET %%ARRAY-NAMED-STRUCTURE-FLAG STRUCTURE 0)))
;       (CONDITION-CASE ()
;           (NAMED-STRUCTURE-SYMBOL STRUCTURE)
;         (ERROR NIL)))


;;; This function exists mostly for easing the phaseover to the new OBJECT scheme
;;;  (which flushes the SELF argument to the named-structure handler, and uses instead
;;;  a free reference to the variable SELF).
(DEFUN NAMED-STRUCTURE-INVOKE (OPERATION STRUCTURE &REST ARGS)
  ;; This function used to take its first two arguments in the other order.
  ;; We are comitted to supporting the old argument order indefinitely.
  (IF (ARRAYP OPERATION)
      (PSETQ OPERATION STRUCTURE STRUCTURE OPERATION))
  (CHECK-TYPE OPERATION SYMBOL)
  (CHECK-TYPE STRUCTURE ARRAY)
  (LET* ((SELF STRUCTURE)
         (C (NAMED-STRUCTURE-P SELF)))
    (IF (SYMBOLP C)
        (SETQ C (OR (GET C 'NAMED-STRUCTURE-INVOKE)
                    (GET C ':NAMED-STRUCTURE-INVOKE))))
    (COND ((NULL C) NIL)
          ((TYPEP C 'CLOSURE)                           ;If a closure, assume knows about SELF
           (APPLY C OPERATION ARGS))
          (T (APPLY C OPERATION SELF ARGS)))))          ;flush the SELF arg
                                                        ;when the phaseover is made (if ever).

;;; Called by ucode through support vector when a named structure is funcalled.
;;; ARGS are operation and the associated args.
;;; The structure was pushed on the pdl after the last arg - ugh.
(DEFUN CALL-NAMED-STRUCTURE (&REST ARGS)
  (LET ((TEM (LAST ARGS)))
    (APPLY #'NAMED-STRUCTURE-INVOKE (CAR ARGS) (%P-CONTENTS-OFFSET TEM 1) (CDR ARGS))))


;;;; Basic Time Stuff

;;; A number which increments approximately 60 times a second, and wraps
;;; around now and then (about once a day); it's only 23 bits long.
;;; 60-cycle clock not hooked up yet, simulate with microsecond clock.

;;; Remembers high-order bits and detects carries
;;; Maintenance of this depends on TIME being called periodically by the scheduler
(DEFVAR TIME-LAST-VALUE 0)
(DEFVAR LAST-MICROSECOND-TIME 0)
(DEFVAR MICROSECOND-INCREMENT 0)
(DEFVAR SIXTIETH-TIME 0)

(DEFUN TIME-IN-60THS (&AUX INCREMENT NEW QUOTIENT)
  (WITHOUT-INTERRUPTS
    (SETQ INCREMENT (+ MICROSECOND-INCREMENT
                       (%POINTER-DIFFERENCE
                         (SETQ NEW (SYS:%FIXNUM-MICROSECOND-TIME))
                         LAST-MICROSECOND-TIME)))
    (SETQ LAST-MICROSECOND-TIME NEW)
    (COND ((> INCREMENT 16667.)
           (MULTIPLE-VALUE-SETQ (QUOTIENT MICROSECOND-INCREMENT) (TRUNCATE INCREMENT 16667.))
           (SETQ SIXTIETH-TIME (%POINTER-PLUS SIXTIETH-TIME QUOTIENT)))
          (T
           (SETQ MICROSECOND-INCREMENT INCREMENT)
           SIXTIETH-TIME))))

(defmacro cli:time (form)
  "Evaluate FORM in the current lexical environment, returning the values it returns,
while printing to *TRACE-OUTPUT* a message saying how long it took."
  (let ((xtime (gensym)) (otime (gensym)) (ntime (gensym)) (values (gensym)))
    `(let ((,xtime (time:microsecond-time))
           (,otime (time:microsecond-time))
           (,values (multiple-value-list ,form))
           (,ntime (time:microsecond-time)))
       (format *trace-output*
               "~&Evaluation of ~S took ~:D microseconds."
               ',form (- (+ ,ntime ,xtime) ,otime ,otime))
       (values-list ,values))))

(defun time (&optional form)
  "Time in 60'ths of a second.  Only differences between values are significant.
Time values wrap around about once a day, so use TIME-LESSP, TIME-INCREMENT
and TIME-DIFFERENCE to compare and compute times.

If FORM is specified, we evaluate it, return the values it returns,
while printing to *TRACE-OUTPUT* a message saying how long it took."
  (if form
      (eval `(cli:time ,form)) ;; Blech, but I don't really care
    (time-in-60ths)))

;;; TIME-DIFFERENCE, TIME-LESSP, TIME-INCREMENT moved to LMMAC.  850811 KHS.

;;; These are for hacking microsecond times.

(defmacro %mask-32-bits (x)
  `(logand (1- (^ 2 32.)) ,x))

(defun %32-bit-plus (x y)
  (%mask-32-bits (+ x y)))

(defun %32-bit-difference (x y)
  (%mask-32-bits (- x y)))

(defun %32-bit-lessp (x y)
  (< (%mask-32-bits x) (%mask-32-bits y)))

(defun %32-bit-greaterp (x y)
  (> (%mask-32-bits x) (%mask-32-bits y)))


(DEFUN FUNCALL (&FUNCTIONAL FN &EVAL &REST ARGS)
  "Apply FN to the ARGS."
  (APPLY FN ARGS))

(DEFUN APPLY (FUNCTION &REST ARGS)
  "Apply FUNCTION to the ARGS, except that the last element of ARGS is a list of args.
Thus, (APPLY #'FOO 'X '(Y Z)) does (FOO 'X 'Y 'Z).
If there are no ARGS, the car of FUNCTION is applied to the cdr of FUNCTION."
  (COND ((NULL ARGS)
         (APPLY (CAR FUNCTION) (CDR FUNCTION)))
        ((NULL (CDR ARGS))
         (APPLY FUNCTION (CAR ARGS)))
        (T
         (%OPEN-CALL-BLOCK FUNCTION 0 4)        ;No ADI, D-RETURN
         (%ASSURE-PDL-ROOM (+ (1- (LENGTH ARGS)) (LENGTH (LAST ARGS))))
         (DO ((ARGL ARGS (CDR ARGL)))
             ((NULL (CDR ARGL))
              (COND ((NULL (CAR ARGL))
                     (%ACTIVATE-OPEN-CALL-BLOCK))
                    ((CONSP (CAR ARGL))
                     (DO ((RESTARGL (CAR ARGL) (CDR RESTARGL)))
                         ((NULL RESTARGL)
                          (%ACTIVATE-OPEN-CALL-BLOCK))
                       (%PUSH (CAR RESTARGL))))
                    (T
                     (FERROR "The last argument to ~S was not a list: ~S"
                             'APPLY (CAR ARGL)))))
           (%PUSH (CAR ARGL))))))
(DEFF LEXPR-FUNCALL 'APPLY)

(DEFUN ADJUST-ARRAY-SIZE (ARRAY NEW-INDEX-LENGTH)
  "Make ARRAY larger or smaller.  NEW-INDEX-LENGTH is the new size.
For multi-dimensional arrays, changes the last dimension (the one which varies slowest).
If array displaced, adjust request refers to the displaced header, not pointed-to data.
Making an array larger may forward it.  The value returned is the new array,
not the old, forwarded one."
  (CHECK-TYPE ARRAY ARRAY)
  ;; Inhibit GC (flipping), references to the array, and allocation in this region.
  ;; At this point we can rest assured that the array has been transported.
  (WITHOUT-INTERRUPTS
    (LET* ((ARRAY (FOLLOW-STRUCTURE-FORWARDING ARRAY))
           (NDIMS (%P-LDB %%ARRAY-NUMBER-DIMENSIONS ARRAY))
           (LONG-ARRAY? (%P-LDB %%ARRAY-LONG-LENGTH-FLAG ARRAY))
           (DISPLACED? (NOT (ZEROP (%P-LDB %%ARRAY-DISPLACED-BIT ARRAY))))
           (ARRAY-ORIGIN (%MAKE-POINTER-OFFSET DTP-FIX ARRAY (+ LONG-ARRAY? NDIMS)))
           (OLD-INDEX-LENGTH (IF (ZEROP LONG-ARRAY?)
                                 (%P-LDB %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
                               (%P-CONTENTS-OFFSET ARRAY 1)))
           (ARRAY-TYPE (%P-LDB %%ARRAY-TYPE-FIELD ARRAY))
           (NEW-DATA-LENGTH (ARRAY-TOTAL-DATA-SIZE ARRAY-TYPE NEW-INDEX-LENGTH))
           (OLD-DATA-LENGTH (ARRAY-TOTAL-DATA-SIZE ARRAY-TYPE OLD-INDEX-LENGTH)))
      (COND ((NOT (NULL DISPLACED?))
             (SETQ OLD-INDEX-LENGTH (%P-CONTENTS-OFFSET ARRAY-ORIGIN 1))
             (IF (> NEW-INDEX-LENGTH OLD-INDEX-LENGTH)
                 (FERROR "Can't make displaced array ~S bigger" ARRAY))
             (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY-ORIGIN 1)
             (INVALIDATE-ARRAY-CACHE)
             ARRAY)
            ((AND ( NEW-DATA-LENGTH OLD-DATA-LENGTH)   ;No new storage required
                  (EQ (ZEROP LONG-ARRAY?) ( NEW-INDEX-LENGTH %ARRAY-MAX-SHORT-INDEX-LENGTH)))
             (WHEN (EQ (ARRAY-TYPE ARRAY) 'ART-Q-LIST)
               (%P-DPB-OFFSET CDR-NIL %%Q-CDR-CODE ARRAY-ORIGIN (1- NEW-DATA-LENGTH)))
             (WHEN (< NEW-DATA-LENGTH OLD-DATA-LENGTH)
               ;; Fill hole in region with dummy array headers.  No need to reset scavenger.
               (LET ((P (%POINTER-PLUS ARRAY-ORIGIN NEW-DATA-LENGTH)))
                 (%P-DPB CDR-ERROR %%Q-CDR-CODE P)
                 (%P-DPB DTP-ARRAY-HEADER %%Q-DATA-TYPE P)
                 (%P-DPB (%LOGDPB 1 %%ARRAY-NUMBER-DIMENSIONS ART-Q) %%Q-POINTER P)
                 (%BLT P (%POINTER-PLUS P 1) (- OLD-DATA-LENGTH NEW-DATA-LENGTH 1) 1)))
             (IF (ZEROP LONG-ARRAY?)
                 (%P-DPB NEW-INDEX-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY)
               (%P-STORE-CONTENTS-OFFSET NEW-INDEX-LENGTH ARRAY 1))
             (INVALIDATE-ARRAY-CACHE)
             ARRAY)
            ;; Need increased storage.  Either make fresh copy or extend existing copy.
            (T
             (LET ((NEW-ARRAY (MAKE-ARRAY (IF (= NDIMS 1)
                                              NEW-INDEX-LENGTH
                                            (LET ((DIMS (ARRAY-DIMENSIONS ARRAY)))
                                              (SETF (CAR (LAST DIMS)) 1)
                                              (SETF (CAR (LAST DIMS))
                                                (TRUNCATE NEW-INDEX-LENGTH (APPLY #'* DIMS)))
                                              DIMS))
                                          :AREA (%AREA-NUMBER ARRAY)
                                          :TYPE ARRAY-TYPE
                                          :LEADER-LENGTH (ARRAY-LEADER-LENGTH ARRAY))))
               (COPY-ARRAY-CONTENTS-AND-LEADER ARRAY NEW-ARRAY)
               (%P-DPB (%P-LDB %%ARRAY-NAMED-STRUCTURE-FLAG ARRAY)
                       %%ARRAY-NAMED-STRUCTURE-FLAG NEW-ARRAY)
               (STRUCTURE-FORWARD ARRAY NEW-ARRAY)
               (INVALIDATE-ARRAY-CACHE)
               NEW-ARRAY))))))

;(defun return-storage (object &aux region object-origin object-size)
;  "Dispose of OBJECT, returning its storage to free if possible.
;If OBJECT is a displaced array, the displaced-array header is what is freed.
;You had better get rid of all pointers to OBJECT before calling this,
; e.g. (RETURN-STORAGE (PROG1 FOO (SETQ FOO NIL)))
;Returns T if storage really reclaimed, NIL if not."
;  ;; Turn off garbage collection, allocation in this region.
;  (without-interrupts
;    ;; Make sure object gets transported.
;    (setq object-origin (%p-pointer object))
;    (setq region (%region-number object))
;    (setq object-origin (%pointer (%find-structure-leader object)))
;    (setq object-size (%structure-total-size object-origin))
;    (invalidate-array-cache)
;    (when (= (%pointer-plus object-origin object-size)
;            (%pointer-plus (%region-origin region) (%region-free-pointer region)))
;      (%reset-region-free-pointer
;       region (%pointer-difference object-origin (%region-origin region)))
;      t)))

(defun return-storage (object)
  "Once backed up the free pointer in the region containing object;
however, it is impossible to implement safely."
  (si:wipe-structure object))

(compiler:make-obsolete return-storage "do nothing to let the GC take care of it,
or call SI:WIPE-STRUCTURE")

(deff return-array 'return-storage)

(compiler:make-obsolete return-array "do nothing to let the GC take care of it,
or call SI:WIPE-STRUCTURE")

;;;; Function spec and source file name stuff

;;; (The rest of this is in QMISC)

;;; A function-specifier is just a way of talking about a function
;;; for purposes other than applying it.  It can be a symbol, in which case
;;; the function cell of the symbol is used.  Or it can be a list of one of
;;; these formats:
;;; (:METHOD class-name operation) refers to the method in that class for
;;;   that operation; this works for both Class methods and Flavor methods.
;;;   In the case of Flavor methods, the specification may also be of the form
;;;   (:METHOD flavor-name type operation).
;;; (:INSTANCE-METHOD exp operation).  exp should evaluate to an entity.
;;;   Reference is then to the operation directly on that instance.
;;; (:HANDLER flavor operation) refers to the function that is called when
;;;   an object of flavor FLAVOR is sent the message OPERATION.
;;; (:WITHIN within-function renamed-function) refers to renamed-function,
;;;   but only as called directly from within-function.
;;;   Actually, renamed-function is replaced throughout within-function
;;;   by an uninterned symbol whose definition is just renamed-function
;;;   as soon as an attempt is made to do anything to a function spec
;;;   of this form.  The function spec is from then on equivalent
;;;   to that uninterned symbol.
;;; (:PROPERTY symbol property) refers to (GET symbol property).
;;; (:LOCATION locative-or-list-pointer) refers to the CDR of the pointer.
;;;   This is for pointing at an arbitrary place
;;;   which there is no special way to describe.
;;; One place you can use a function specifier is in DEFUN.

;;; For Maclisp compatibility, a list whose car is not recognized is taken
;;; to be a list of a symbol and a property, by DEFUN and DEFMACRO.  They
;;; standardize this by putting :PROPERTY on the front.  These
;;; non-standard function specs are not accepted by the rest of the
;;; system.  This is done to avoid ambiguities and inconsistencies.

;;; The SYS:FUNCTION-SPEC-HANDLER property of a symbol, if present means that that
;;; symbol is legal as the car of a function spec.  The value of the property
;;; is a function whose arguments are the function in behalf
;;; of which to act (not a keyword symbol!) and the arguments to that
;;; function (the first of which is always the function spec).
;;; Functions are:
;;;     FDEFINE definition
;;;     FDEFINEDP
;;;     FDEFINITION
;;;     FDEFINITION-LOCATION
;;;     FUNDEFINE
;;;     FUNCTION-PARENT
;;;     COMPILER-FDEFINEDP -- returns T if will be fdefinedp at run time
;;;     GET indicator
;;;     PUTPROP value indicator
;;;     REMPROP indicator
;;;     DWIMIFY original-spec def-decoder (see below, DWIMIFY-PACKAGE-2).

(DEFVAR-RESETTABLE INHIBIT-FDEFINE-WARNINGS NIL NIL
  "T turns off warnings of redefining function in different file.
:JUST-WARN turns off queries, leaving just warnings.")

(DEFUN VALIDATE-FUNCTION-SPEC (FUNCTION-SPEC &AUX HANDLER)
  "Predicate for use with CHECK-ARG.  Returns non-nil if FUNCTION-SPEC really is one.
The value is the type of function spec (T for a symbol)."
  (COND ((ATOM FUNCTION-SPEC)
         (SYMBOLP FUNCTION-SPEC))
        ((AND (SYMBOLP (CAR FUNCTION-SPEC))
              (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER))
              (FUNCALL HANDLER 'VALIDATE-FUNCTION-SPEC FUNCTION-SPEC))
         (CAR FUNCTION-SPEC))))

(DEFPROP ENCAPSULATION "encapsulation" DEFINITION-TYPE-NAME)

(DEFUN FDEFINE (FUNCTION-SPEC DEFINITION &OPTIONAL CAREFULLY-FLAG NO-QUERY-FLAG
                                         &AUX TYPE INNER-SPEC DEFINEDP)
  "Alter the function definition of a function specifier.
CAREFULLY-FLAG means preserve any tracing or advice,
and save the old definition, when possible.
This function returns T if it does define the function, or NIL if it does not.
If FDEFINE-FILE-PATHNAME is non-NIL, then it is the file which this definition
was read from, and we make a note of that fact when possible."

  ;; Get error if invalid fun spec.  Also find out whether defined.
  (SETQ DEFINEDP (FDEFINEDP FUNCTION-SPEC))
  (IF (CONSP FUNCTION-SPEC) (SETQ TYPE (CAR FUNCTION-SPEC)))

  ;; Record the source file name, if desired, and check for redefinition errors
  (COND ((OR (EQ TYPE :INTERNAL)
             (RECORD-SOURCE-FILE-NAME FUNCTION-SPEC
                                      (IF CAREFULLY-FLAG 'DEFUN 'ENCAPSULATION)
                                      (OR NO-QUERY-FLAG (NOT CAREFULLY-FLAG)
                                          (EQ INHIBIT-FDEFINE-WARNINGS T))))

         ;; If there is a previous definition, save it (if desired).
         ;; Also if it is encapsulated, set INNER-SPEC to the symbol
         ;; which holds the real definition before encapsulation, and
         ;; save that definition.
         (COND ((AND DEFINEDP CAREFULLY-FLAG)
                (SETQ INNER-SPEC (UNENCAPSULATE-FUNCTION-SPEC FUNCTION-SPEC))
                (MULTIPLE-VALUE-BIND (DEFP DEFN)
                    (FDEFINEDP-AND-FDEFINITION INNER-SPEC)
                  (AND DEFP
                       (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC DEFN :PREVIOUS-DEFINITION)))
                ;; Carry over renamings from previous definition
                (AND (NEQ FUNCTION-SPEC INNER-SPEC)     ;Skip it if no encapsulations.
                     (FBOUNDP 'RENAME-WITHIN-NEW-DEFINITION-MAYBE)
                     (SETQ DEFINITION (RENAME-WITHIN-NEW-DEFINITION-MAYBE FUNCTION-SPEC
                                                                          DEFINITION))))
               (T (SETQ INNER-SPEC FUNCTION-SPEC)))

         ;; Now store the new definition in type-dependent fashion
         (IF (SYMBOLP INNER-SPEC) (FSET INNER-SPEC DEFINITION)
             (FUNCALL (GET TYPE 'FUNCTION-SPEC-HANDLER) 'FDEFINE INNER-SPEC DEFINITION))

         ;; Return T since we did define the function
         T)
        ;; Return NIL since we decided not to define the function
        (T NIL)))

;;; Is a function specifier defined?  A generalization of FBOUNDP.
(DEFUN FDEFINEDP (FUNCTION-SPEC &AUX HANDLER)
  "Returns T if the function spec has a function definition."
  ;; Then perform type-dependent code
  (COND ((SYMBOLP FUNCTION-SPEC) (FBOUNDP FUNCTION-SPEC))
        ((AND (CONSP FUNCTION-SPEC)
              (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))
         (FUNCALL HANDLER 'FDEFINEDP FUNCTION-SPEC))
        (T (FERROR 'SYS:INVALID-FUNCTION-SPEC
                   "The function spec ~S is invalid." FUNCTION-SPEC))))

;;; Get the definition of a function specifier.  Generalized FSYMEVAL.
(DEFUN FDEFINITION (FUNCTION-SPEC &AUX HANDLER)
  "Returns the function definition of a function spec"
  ;; First, validate the function spec.
  (SETQ FUNCTION-SPEC (DWIMIFY-ARG-PACKAGE FUNCTION-SPEC 'FUNCTION-SPEC))
  (COND ((SYMBOLP FUNCTION-SPEC) (FSYMEVAL FUNCTION-SPEC))
        ((AND (CONSP FUNCTION-SPEC)
              (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))
         (FUNCALL HANDLER 'FDEFINITION FUNCTION-SPEC))
        (T (FERROR 'SYS:INVALID-FUNCTION-SPEC
                   "The function spec ~S is invalid." FUNCTION-SPEC))))

(DEFUN FDEFINEDP-AND-FDEFINITION (FUNCTION-SPEC &AUX HANDLER)
  "Returns whether the FUNCTION-SPEC is defined, and its definition if so.
The first value is T or NIL, the second is the definition if the first is T."
  ;; First, validate the function spec.
  (COND ((SYMBOLP FUNCTION-SPEC)
         (IF (FBOUNDP FUNCTION-SPEC)
             (VALUES T (FSYMEVAL FUNCTION-SPEC))))
        ((AND (CONSP FUNCTION-SPEC)
              (SETQ HANDLER (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))
         (MULTIPLE-VALUE-BIND (DEFINEDP DEFN)
             (FUNCALL HANDLER 'FDEFINEDP FUNCTION-SPEC)
           (IF DEFINEDP
               (VALUES T (OR DEFN (FDEFINITION FUNCTION-SPEC))))))
        (T (FERROR 'SYS:INVALID-FUNCTION-SPEC
                   "The function spec ~S is invalid." FUNCTION-SPEC))))

(DEFF DWIMIFY-FUNCTION-SPEC 'DWIMIFY-PACKAGE)
(DEFUN DWIMIFY-PACKAGE (FUNCTION-SPEC &OPTIONAL (DEFINITION-TYPE 'FDEFINEDP))
  "Return a function spec similar to FUNCTION-SPEC but which is defined.
We check for symbols in the wrong package, and various other things.
The user is asked to confirm the candidate values.
If the user does not accept some candidate, we get an error and he can
continue with some other function spec.
DEFINITION-TYPE is a symbol which has properties that say what /"defined/" means.
Two possibilities are FDEFINEDP meaning /"is defined as a function/"
and BOUNDP meaning /"is a symbol with a value/"."
  (DO (TEM
        (DEF-DECODER (GET DEFINITION-TYPE 'DWIMIFY)))
      (())
    (COND ((AND (FUNCALL (FIRST DEF-DECODER) FUNCTION-SPEC)
                (FUNCALL (SECOND DEF-DECODER) FUNCTION-SPEC))
           (RETURN FUNCTION-SPEC))
          ((SETQ TEM (DWIMIFY-PACKAGE-0 FUNCTION-SPEC DEFINITION-TYPE))
           (RETURN TEM)))
    (SETQ FUNCTION-SPEC
          (CERROR T NIL 'SYS:WRONG-TYPE-ARGUMENT "~1@*~S is not ~A"
                  DEFINITION-TYPE FUNCTION-SPEC
                  (GET DEFINITION-TYPE 'DWIMIFY-ERROR-MESSAGE)))))

;;; Do (SETQ FUNCTION-SPEC (DWIMIFY-ARG-PACKAGE FUNCTION-SPEC 'MY-ARG-NAME))
;;; to dwimify and get a suitable error message for MY-ARG-NAME if that fails.
(DEFUN DWIMIFY-ARG-PACKAGE (FUNCTION-SPEC ARG-NAME &OPTIONAL (DEFINITION-TYPE 'FDEFINEDP))
  "Like DWIMIFY-PACKAGE but error message is different if fail to dwimify.
The error message says that the bad value was the arg named ARG-NAME
of the function that called this one."
  (DECLARE (DBG:ERROR-REPORTER))
  (DO (TEM (DEF-DECODER (GET DEFINITION-TYPE 'DWIMIFY)))
      (())
    (COND ((AND (FUNCALL (FIRST DEF-DECODER) FUNCTION-SPEC)
                (FUNCALL (SECOND DEF-DECODER) FUNCTION-SPEC))
           (RETURN FUNCTION-SPEC))
          ((SETQ TEM (DWIMIFY-PACKAGE-0 FUNCTION-SPEC DEFINITION-TYPE))
           (RETURN TEM)))
    (SETQ FUNCTION-SPEC
          (CERROR T NIL 'SYS:WRONG-TYPE-ARGUMENT "The argument ~3@*~S is ~1@*~S,~%which is not ~A"
                  DEFINITION-TYPE FUNCTION-SPEC
                  (GET DEFINITION-TYPE 'DWIMIFY-ERROR-MESSAGE)
                  ARG-NAME))))

(DEFPROP FDEFINEDP (VALIDATE-FUNCTION-SPEC FDEFINEDP FDEFINITION-LOCATION "definition"
                                           FUNCTION-SPEC-DWIMIFY)
         DWIMIFY)
(DEFPROP FDEFINEDP "a valid, defined function spec" DWIMIFY-ERROR-MESSAGE)

(DEFPROP BOUNDP (SYMBOLP BOUNDP VALUE-CELL-LOCATION "value" IGNORE) DWIMIFY)
(DEFPROP BOUNDP "a symbol with a value" DWIMIFY-ERROR-MESSAGE)

;;; Given a maybe invalid, maybe undefined function spec (or other sort of object),
;;; ask the user about possible alternatives he might have meant.
;;; If the user accepts one, we return it.  Otherwise we return nil.
;;; DEFINITION-TYPE says what kind of object we are looking for.
;;; It should be a symbol with a SI:DWIMIFY property.
(DEFVAR-RESETTABLE DWIMIFY-PACKAGE-0-TOPIC-PRINTED NIL NIL)
(DEFUN DWIMIFY-PACKAGE-0 (FUNCTION-SPEC DEFINITION-TYPE)
  "Like DWIMIFY-PACKAGE except return NIL we do not find a replacement function spec."
  (LET (DWIMIFY-PACKAGE-0-TOPIC-PRINTED)
    (DWIMIFY-PACKAGE-1 FUNCTION-SPEC FUNCTION-SPEC (GET DEFINITION-TYPE 'DWIMIFY))))

;;; DWIMIFY-INFO should be something like
;;;  (VALIDATE-FUNCTION-SPEC FDEFINEDP FDEFINITION-LOCATION "defn" FUNCTION-SPEC-DWIMIFY)
;;; If the third element is nil, the option of linking the symbols is not offered.
(DEFUN DWIMIFY-PACKAGE-1 (FUNCTION-SPEC ORIGINAL-SPEC DWIMIFY-INFO
                          &AUX TEM (PREDICATE (SECOND DWIMIFY-INFO))
                          (VALIDATOR (FIRST DWIMIFY-INFO))
                          (AUX-DWIMIFIER (FIFTH DWIMIFY-INFO)))
  (COND ((AND (VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
              (FUNCALL PREDICATE FUNCTION-SPEC))
         FUNCTION-SPEC)
        ((SYMBOLP FUNCTION-SPEC)
         ;; If it's a symbol, try symbols in other packages.
         (CATCH 'DWIMIFY-PACKAGE
           (MAP-OVER-LOOKALIKE-SYMBOLS (SYMBOL-NAME FUNCTION-SPEC) NIL
                                       'DWIMIFY-PACKAGE-2
                                       ORIGINAL-SPEC DWIMIFY-INFO)
           NIL))
        ((ATOM FUNCTION-SPEC) NIL)
        ;; If the function spec's handler has any ideas, try them first.
        ((AND (FUNCALL VALIDATOR FUNCTION-SPEC)
              (FUNCALL AUX-DWIMIFIER FUNCTION-SPEC ORIGINAL-SPEC DWIMIFY-INFO)))
        ;; Maybe we can get something by standardizing a maclisp function spec.
        ((AND (NEQ (STANDARDIZE-FUNCTION-SPEC FUNCTION-SPEC NIL) FUNCTION-SPEC)
              (DWIMIFY-PACKAGE-1 (STANDARDIZE-FUNCTION-SPEC FUNCTION-SPEC NIL)
                                 ORIGINAL-SPEC DWIMIFY-INFO)))
        ((AND (SYMBOLP (CAR FUNCTION-SPEC))
              (SETQ TEM (INTERN-SOFT (CAR FUNCTION-SPEC) PKG-KEYWORD-PACKAGE))
              (NEQ (CAR FUNCTION-SPEC) TEM)
              ;; If list whose car is a symbol not in Keyword,
              ;; try replacing the car with a symbol in Keyword.
              (DWIMIFY-PACKAGE-1 (CONS TEM (CDR FUNCTION-SPEC))
                                 ORIGINAL-SPEC DWIMIFY-INFO)))
        ((AND (CDDR FUNCTION-SPEC) (SYMBOLP (CADDR FUNCTION-SPEC))
              (SETQ TEM (INTERN-SOFT (CADDR FUNCTION-SPEC) PKG-KEYWORD-PACKAGE))
              (NEQ (CADDR FUNCTION-SPEC) TEM)
              ;; Try a similar thing with the third element
              (DWIMIFY-PACKAGE-1
                `(,(CAR FUNCTION-SPEC) ,(CADR FUNCTION-SPEC)
                  ,TEM . ,(CDDDR FUNCTION-SPEC))
                ORIGINAL-SPEC DWIMIFY-INFO)))
        ((AND (CDDDR FUNCTION-SPEC) (SYMBOLP (CADDDR FUNCTION-SPEC))
              (SETQ TEM (INTERN-SOFT (CADDDR FUNCTION-SPEC) PKG-KEYWORD-PACKAGE))
              (NEQ (CADDDR FUNCTION-SPEC) TEM)
              ;; and the fourth element.
              (DWIMIFY-PACKAGE-1
                `(,(CAR FUNCTION-SPEC) ,(CADR FUNCTION-SPEC) ,(CADDR FUNCTION-SPEC)
                  ,TEM . ,(CDDDDR FUNCTION-SPEC))
                ORIGINAL-SPEC DWIMIFY-INFO)))
        ((AND (CDR FUNCTION-SPEC) (SYMBOLP (CADR FUNCTION-SPEC)))
         ;; Try replacing the second element with symbols in other packages.
         (CATCH 'DWIMIFY-PACKAGE
           (MAP-OVER-LOOKALIKE-SYMBOLS
             (SYMBOL-NAME (CADR FUNCTION-SPEC))
             NIL
             (LAMBDA (NEW-SYMBOL SPEC ORIGINAL-SPEC DWIMIFY-INFO)
               (OR (EQ NEW-SYMBOL (CADR SPEC))
                   (DWIMIFY-PACKAGE-2 `(,(CAR SPEC) ,NEW-SYMBOL . ,(CDDR SPEC))
                                      ORIGINAL-SPEC DWIMIFY-INFO)))
             FUNCTION-SPEC ORIGINAL-SPEC DWIMIFY-INFO)
           NIL))))

(DEFUN MAP-OVER-LOOKALIKE-SYMBOLS (PNAME IGNORE FUNCTION &REST ADDITIONAL-ARGS &AUX SYM)
  "Call FUNCTION for each symbol in any package whose name matches PNAME.
The args to FUNCTION are the symbol and the ADDITIONAL-ARGS."
  (DOLIST (PKG *ALL-PACKAGES*)
    (IF (AND (NEQ PKG PKG-KEYWORD-PACKAGE)
             (SETQ SYM (INTERN-LOCAL-SOFT PNAME PKG)))
        (APPLY FUNCTION SYM ADDITIONAL-ARGS)))
  (if (and (boundp '*spelling-dwim-is-loaded?*)
           *enable-spelling-dwim?*
           (null sym))
      (find-candidate-for-poor-spelling
                   pname *package*
                   #'(lambda (s)
                       (apply function s additional-args))))
  )

;;; Consider one suggested dwimification, NEW-SPEC, of the ORIGINAL-SPEC.
;;; If the user accepts it, throw it to DWIMIFY-PACKAGE.
;;; DWIMIFY-INFO should be something like
;;;  (VALIDATE-FUNCTION-SPEC FDEFINEDP FDEFINITION-LOCATION "defn" FUNCTION-SPEC-DWIMIFY)
;;; If the third element is nil, the option of linking the symbols is not offered.

#-(target falcon)                               ; +++ Breaks cross compiler. <27-Oct-88 wkf>
(DEFUN DWIMIFY-PACKAGE-2 (NEW-SPEC ORIGINAL-SPEC DWIMIFY-INFO
                          &OPTIONAL NO-RECURSION &AUX ANS
                          (VALIDATOR (FIRST DWIMIFY-INFO))
                          (PREDICATE (SECOND DWIMIFY-INFO))
                          (LOCATOR (THIRD DWIMIFY-INFO))
                          (PRETTY-NAME (FOURTH DWIMIFY-INFO))
                          (AUX-DWIMIFIER (FIFTH DWIMIFY-INFO)))
  "Subroutine of DWIMIFY-PACKAGE: ask user about one candidate.
This can be used by handlers of types of function specs,
for handling the :DWIMIFY operation.
NEW-SPEC is the candidate.  ORIGINAL-SPEC is what was supplied to DWIMIFY-PACKAGE.
DWIMIFY-INFO is data on the type of definition being looked for, and what to tell the user.
ORIGINAL-SPEC and DWIMIFY-INFO are provided with the :DWIMIFY operation.
NO-RECURSION means do not use this candidate to generate other candidates."
  (AND (NOT (EQUAL NEW-SPEC ORIGINAL-SPEC))
       (FUNCALL VALIDATOR NEW-SPEC)
       (CATCH 'QUIT
         (OR (COND ((AND (FUNCALL PREDICATE NEW-SPEC)
                         (PROGN
                           (OR DWIMIFY-PACKAGE-0-TOPIC-PRINTED
                               (FORMAT *QUERY-IO* "~&~S does not have a ~A.~%"
                                       ORIGINAL-SPEC PRETTY-NAME
                                       (SETQ DWIMIFY-PACKAGE-0-TOPIC-PRINTED T)))
                           (SETQ ANS
                                 (FQUERY `(:CHOICES
                                            (((T "Yes.") #/Y #/T #/SPACE #/HAND-UP)
                                             ((NIL "No.") #/N #/RUBOUT #/C-Z #/HAND-DOWN)
                                             ,@(AND (SYMBOLP NEW-SPEC)
                                                    LOCATOR
                                                    '(((P "Permanently link ") #/P)))
                                             ((G "Go to package ") #/G))
                                            :HELP-FUNCTION DWIMIFY-PACKAGE-2-HELP)
                                         "Use the ~A of ~S? "
                                         PRETTY-NAME NEW-SPEC))))
                    (COND ((EQ ANS 'P)
                           (FORMAT *QUERY-IO* "~S to ~S." ORIGINAL-SPEC NEW-SPEC)
                           (LET ((LOC2 (FUNCALL LOCATOR ORIGINAL-SPEC))
                                 (LOC1 (FUNCALL LOCATOR NEW-SPEC)))
                             (SETF (CDR LOC2) LOC1)
                             (%P-STORE-DATA-TYPE LOC2 DTP-ONE-Q-FORWARD)))
                          ((EQ ANS 'G)
                           (LET ((PKG (SYMBOL-PACKAGE
                                        (IF (SYMBOLP NEW-SPEC) NEW-SPEC
                                          (CADR NEW-SPEC)))))
                             (FORMAT *QUERY-IO* "~A." (PKG-NAME PKG))
                             (PKG-GOTO PKG))))
                    (THROW 'DWIMIFY-PACKAGE NEW-SPEC)))
             ;; If this one isn't defined of isn't wanted,
             ;; try any others suggested by this kind of function spec's handler.
             ;; For :METHOD, this should try inherited methods.
             ;; With each suggestion, the dwimifier should call this function again,
             ;; perhaps setting NO-RECURSION.
             (AND (NOT NO-RECURSION)
                  (LET ((VALUE
                          (FUNCALL AUX-DWIMIFIER NEW-SPEC ORIGINAL-SPEC DWIMIFY-INFO)))
                    (AND VALUE (THROW 'DWIMIFY-PACKAGE VALUE))))))))

(DEFUN DWIMIFY-PACKAGE-2-HELP (S IGNORE IGNORE)
  (FORMAT S "~&Y to use it this time.
P to use it every time (permanently link the two symbols).
G to use it this time and do a pkg-goto.
N to do nothing special and enter the normal error handler.
"))

;;; This is the AUX-DWIMIFIER in the DWIMIFY property of FDEFINEDP.
(DEFUN FUNCTION-SPEC-DWIMIFY (NEW-SPEC ORIGINAL-SPEC DWIMIFY-INFO)
  (AND (CONSP NEW-SPEC)
       (LET ((HANDLER (GET (CAR NEW-SPEC) 'FUNCTION-SPEC-HANDLER)))
         (CONDITION-CASE ()
             (FUNCALL HANDLER 'DWIMIFY NEW-SPEC
                      ORIGINAL-SPEC DWIMIFY-INFO)
           (ERROR NIL)))))

;;; Default handler called by function-spec-handlers to do functions they don't
;;; handle specially.
(DEFUN FUNCTION-SPEC-DEFAULT-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  "This subroutine handles various operations for other function spec handlers."
  (CASE FUNCTION
    (VALIDATE-FUNCTION-SPEC T)          ;Used only during system build, via FUNCTION-SPEC-GET
    (FUNCTION-PARENT NIL)               ;Default is no embedding in other definitions
    (COMPILER-FDEFINEDP NIL)            ;Default is no remembering of compiled definitions
    (DWIMIFY NIL)
    (GET (IF FUNCTION-SPEC-HASH-TABLE
             ;; Default is to use plist hash table
             (WITH-STACK-LIST (KEY FUNCTION-SPEC ARG1)
               (SEND FUNCTION-SPEC-HASH-TABLE :GET-HASH KEY ARG2))
           (LOOP FOR (FS IND PROP) IN COLD-LOAD-FUNCTION-PROPERTY-LISTS
              WHEN (AND (EQUAL FS FUNCTION-SPEC) (EQ IND ARG1))
                RETURN PROP
              FINALLY (RETURN ARG2))))
    (PUTPROP (LET ((DEFAULT-CONS-AREA BACKGROUND-CONS-AREA)
                   (AREA (%AREA-NUMBER FUNCTION-SPEC)))
               (IF (OR (AREA-TEMPORARY? AREA)
                       (= AREA PDL-AREA))
                   (SETQ FUNCTION-SPEC (COPYTREE FUNCTION-SPEC)))
               (IF FUNCTION-SPEC-HASH-TABLE
                   (SEND FUNCTION-SPEC-HASH-TABLE :PUT-HASH (LIST FUNCTION-SPEC ARG2) ARG1)
                 (PUSH (LIST FUNCTION-SPEC ARG2 ARG1) COLD-LOAD-FUNCTION-PROPERTY-LISTS))))
    (PUSH-PROPERTY
     (WITH-STACK-LIST (KEY FUNCTION-SPEC ARG2)
       (PUSH ARG1 (GETHASH KEY FUNCTION-SPEC-HASH-TABLE))))
    (OTHERWISE (FERROR "~S is not implemented by the function spec ~S"
                       FUNCTION FUNCTION-SPEC))))

;;; (:PROPERTY symbol property) refers to (GET symbol property).
;;; This has to be defined with a separate DEFPROP for reasons which should be obvious.
(DEFPROP :PROPERTY PROPERTY-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN PROPERTY-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((SYMBOL (SECOND FUNCTION-SPEC))
        (INDICATOR (THIRD FUNCTION-SPEC)))
    (IF (NOT (AND (= (LENGTH FUNCTION-SPEC) 3) (SYMBOLP SYMBOL)))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC "Invalid function spec ~S." FUNCTION-SPEC))
      (CASE FUNCTION
        (VALIDATE-FUNCTION-SPEC T)
        (FDEFINE (SETF (GET SYMBOL INDICATOR) ARG1))
        ((FDEFINITION FDEFINEDP) (GET SYMBOL INDICATOR))
        (FDEFINITION-LOCATION (LOCF (GET SYMBOL INDICATOR)))    ;Not perfect, but close
        (FUNDEFINE (REMPROP SYMBOL INDICATOR))
        (DWIMIFY
         (AND (SYMBOLP INDICATOR)
              (MULTIPLE-VALUE-BIND (NEW-SYM DWIM-P)
                  (CATCH 'DWIMIFY-PACKAGE
                    (MAP-OVER-LOOKALIKE-SYMBOLS
                      (SYMBOL-NAME INDICATOR)
                      NIL
                      (LAMBDA (NEW-SYMBOL SPEC ORIGINAL-SPEC DWIMIFY-INFO)
                        (OR (EQ NEW-SYMBOL (CADDR SPEC))
                            (DWIMIFY-PACKAGE-2 `(,(CAR SPEC) ,(CADR SPEC) ,NEW-SYMBOL)
                                               ORIGINAL-SPEC DWIMIFY-INFO T)))
                      FUNCTION-SPEC ARG1 ARG2))
                (AND DWIM-P NEW-SYM))))
        (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))

;;; (:INTERNAL parent-function index) refers to the index'th unnamed
;;; broken-off lambda in the parent function.
;;; parent-function is normally a function-spec, but it may also be a FEF.
;;; Note that VALIDATE-FUNCTION-SPEC for :INTERNAL returns NIL if the
;;; function-spec itself is malformed, however if the spec is well-formed
;;; but the parent doesn't have internal functions, an error is signalled
;;; giving a detailed explanation.
(DEFPROP :INTERNAL INTERNAL-FUNCTION-SPEC-HANDLER FUNCTION-SPEC-HANDLER)
(DEFUN INTERNAL-FUNCTION-SPEC-HANDLER (FUNCTION FUNCTION-SPEC &OPTIONAL ARG1 ARG2)
  (LET ((PARENT (SECOND FUNCTION-SPEC))
        (INDEX (THIRD FUNCTION-SPEC))
        DIRECT-FEF)
    (when (eq (car-safe parent) 'macro)
      (setq parent (cdr parent)))
    (SETQ DIRECT-FEF (= (%DATA-TYPE PARENT) DTP-FEF-POINTER))
    (IF (NOT (AND (OR (AND (FIXNUMP INDEX) (NOT (MINUSP INDEX)))
                      (SYMBOLP INDEX))
                  (= (LENGTH FUNCTION-SPEC) 3)))
        (UNLESS (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (FERROR 'SYS:INVALID-FUNCTION-SPEC
                  "The function spec ~S is invalid." FUNCTION-SPEC))
      (IF (EQ FUNCTION 'VALIDATE-FUNCTION-SPEC)
          (OR DIRECT-FEF
              (AND (VALIDATE-FUNCTION-SPEC PARENT) (FDEFINEDP PARENT)))
        (LET ((FEF (IF DIRECT-FEF PARENT
                     (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC PARENT))))
              TABLE OFFSET LOCAL-FUNCTION-MAP DEBUGGING-INFO)
          (WHEN (EQ (CAR-SAFE FEF) 'MACRO)
            (SETQ FEF (CDR FEF)))
          (UNLESS (= (%DATA-TYPE FEF) DTP-FEF-POINTER)
            (FERROR 'SYS:INVALID-FUNCTION-SPEC
                    "The function spec ~S refers to ~S, which is not a FEF"
                    FUNCTION-SPEC FEF))
          (SETQ DEBUGGING-INFO (DEBUGGING-INFO FEF)
                TABLE (CDR (ASSQ :INTERNAL-FEF-OFFSETS DEBUGGING-INFO))
                LOCAL-FUNCTION-MAP (CADR (ASSQ 'COMPILER::LOCAL-FUNCTION-MAP DEBUGGING-INFO)))
          (UNLESS TABLE
            (FERROR 'SYS:INVALID-FUNCTION-SPEC
                    "The function spec ~S refers to ~S, which has no internal functions"
                    FUNCTION-SPEC FEF))
          (WHEN (SYMBOLP INDEX)
            (UNLESS (MEMQ INDEX LOCAL-FUNCTION-MAP)
              (FERROR 'SYS:INVALID-FUNCTION-SPEC
                      "The function spec ~S refers to a non-existent internal function"
                      FUNCTION-SPEC))
            (SETQ INDEX (POSITION INDEX LOCAL-FUNCTION-MAP)))
          (UNLESS (SETQ OFFSET (NTH INDEX TABLE))
            (FERROR 'SYS:INVALID-FUNCTION-SPEC
                    "The function spec ~S is out of range" FUNCTION-SPEC))

          ;; Function spec fully parsed, we can now earn our living
          (CASE FUNCTION
            (VALIDATE-FUNCTION-SPEC T)
            (FDEFINE (LET ((%INHIBIT-READ-ONLY T))
                       (%P-STORE-CONTENTS-OFFSET ARG1 FEF OFFSET)))
            (FDEFINITION (%P-CONTENTS-OFFSET FEF OFFSET))
            (FDEFINEDP          ;Random: look for what the compiler puts there initially
             (NOT (EQUAL (%P-CONTENTS-OFFSET FEF OFFSET) FUNCTION-SPEC)))
            (FDEFINITION-LOCATION (%MAKE-POINTER-OFFSET DTP-LOCATIVE FEF OFFSET))
            (FUNCTION-PARENT (VALUES PARENT 'DEFUN))
            (OTHERWISE (FUNCTION-SPEC-DEFAULT-HANDLER FUNCTION FUNCTION-SPEC ARG1 ARG2))))))))

;;; This is setup by QLD as soon as everything it will need is loaded in and pathnames work
;;; and so on.
(DEFVAR FUNCTION-SPEC-HASH-TABLE NIL)
;;; In the meantime, and from the cold load, this remembers non symbol source files,
;;; elements are (function-spec indicator value).
(DEFVAR COLD-LOAD-FUNCTION-PROPERTY-LISTS)

(DEFUN FUNCTION-SPEC-PUTPROP (FUNCTION-SPEC VALUE PROPERTY)
  "Put a PROPERTY property with value VALUE on FUNCTION-SPEC.
For symbols, this is just PUTPROP, but it works on any function spec."
  (IF (SYMBOLP FUNCTION-SPEC)
      (SETF (GET FUNCTION-SPEC PROPERTY) VALUE)
    (LET ((HFUN
            (IF (NULL FUNCTION-SPEC-HASH-TABLE)
                ;; While loading files with MINI during system build,
                ;; always use the default handler,
                ;; which stores on COLD-LOAD-FUNCTION-PROPERTY-LISTS.
                ;; This is so that the property's pathnames will be canonicalized later.
                'FUNCTION-SPEC-DEFAULT-HANDLER
              (AND (CONSP FUNCTION-SPEC)
                   (SYMBOLP (CAR FUNCTION-SPEC))
                   (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))))
      (IF HFUN
          (FUNCALL HFUN 'PUTPROP FUNCTION-SPEC VALUE PROPERTY)
        (FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
                FUNCTION-SPEC)))))

(DEFUN FUNCTION-SPEC-PUSH-PROPERTY (FUNCTION-SPEC VALUE PROPERTY)
  "PUSH VALUE onto the PROPERTY property of FUNCTION-SPEC.
Like (PUSH VALUE (FUNCTION-SPEC-GET FUNCTION-SPEC PROPERTY)) but faster."
  (IF (SYMBOLP FUNCTION-SPEC)
      (SETF (GET FUNCTION-SPEC PROPERTY) (CONS VALUE (GET FUNCTION-SPEC PROPERTY)))
    (LET ((HFUN
            (IF (NULL FUNCTION-SPEC-HASH-TABLE)
                ;; While loading files with MINI during system build,
                ;; always use the default handler,
                ;; which stores on COLD-LOAD-FUNCTION-PROPERTY-LISTS.
                ;; This is so that the property's pathnames will be canonicalized later.
                'FUNCTION-SPEC-DEFAULT-HANDLER
              (AND (CONSP FUNCTION-SPEC)
                   (SYMBOLP (CAR FUNCTION-SPEC))
                   (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))))
      (IF HFUN
          (FUNCALL HFUN 'PUSH-PROPERTY FUNCTION-SPEC VALUE PROPERTY)
        (FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
                FUNCTION-SPEC)))))

(DEFUN FUNCTION-SPEC-GET (FUNCTION-SPEC PROPERTY &OPTIONAL DEFAULT)
  "Get the PROPERTY property of FUNCTION-SPEC.
For symbols, this is just GET, but it works on any function spec."
  (IF (SYMBOLP FUNCTION-SPEC)
      (GET FUNCTION-SPEC PROPERTY DEFAULT)
    ;; Look for a handler for this type of function spec.
    (LET ((HFUN
            (IF (NULL FUNCTION-SPEC-HASH-TABLE)
                ;; While loading files with MINI during system build,
                ;; always use the default handler,
                ;; which stores on COLD-LOAD-FUNCTION-PROPERTY-LISTS.
                ;; This is so that the property's pathnames will be canonicalized later.
                'FUNCTION-SPEC-DEFAULT-HANDLER
              (AND (CONSP FUNCTION-SPEC)
                   (SYMBOLP (CAR FUNCTION-SPEC))
                   (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))))
      (IF HFUN
          (AND (FUNCALL HFUN 'VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
               ;;previous line avoids lossage when compiling defselects which aren't present
               ;; in run time environment (yet), for example.
               (FUNCALL HFUN 'GET FUNCTION-SPEC PROPERTY DEFAULT))
        (FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
                FUNCTION-SPEC)))))

;;; This function exists only for the sake of an &rest argument to avoid consing
(DEFUN FUNCTION-SPEC-GET-1 (&REST KEY)
  (DECLARE (ARGLIST FUNCTION-SPEC INDICATOR))
  (SEND FUNCTION-SPEC-HASH-TABLE :GET-HASH KEY))

(DEFUN FUNCTION-SPEC-REMPROP-1 (&REST KEY)
  (DECLARE (ARGLIST FUNCTION-SPEC INDICATOR))
  (REMHASH KEY FUNCTION-SPEC-HASH-TABLE))

(DEFUN FUNCTION-SPEC-PUSH-PROPERTY-1 (VALUE &REST KEY)
  (DECLARE (ARGLIST VALUE FUNCTION-SPEC INDICATOR))
  (PUTHASH KEY (CONS VALUE (GETHASH KEY FUNCTION-SPEC-HASH-TABLE))
           FUNCTION-SPEC-HASH-TABLE))

(DEFUN FUNCTION-SPEC-REMPROP (FUNCTION-SPEC PROPERTY)
  "Remove the PROPERTY property, if any, from FUNCTION-SPEC.
For symbols, this is just REMPROP, but it works on any function spec."
  (IF (SYMBOLP FUNCTION-SPEC)
      (REMPROP FUNCTION-SPEC PROPERTY)
    (LET ((HFUN (IF (NULL FUNCTION-SPEC-HASH-TABLE)
                ;; While loading files with MINI during system build,
                ;; always use the default handler,
                ;; which stores on COLD-LOAD-FUNCTION-PROPERTY-LISTS.
                ;; This is so that the property's pathnames will be canonicalized later.
                'FUNCTION-SPEC-DEFAULT-HANDLER
              (AND (CONSP FUNCTION-SPEC)
                   (SYMBOLP (CAR FUNCTION-SPEC))
                   (GET (CAR FUNCTION-SPEC) 'FUNCTION-SPEC-HANDLER)))))
      (IF HFUN
          (FUNCALL HFUN 'REMPROP FUNCTION-SPEC PROPERTY)
        (FERROR 'SYS:INVALID-FUNCTION-SPEC "The function spec ~S is invalid."
                FUNCTION-SPEC)))))


(SETQ FS:THIS-IS-A-PATCH-FILE NIL)      ;For the cold load

(DEFVAR-RESETTABLE FDEFINE-FILE-PATHNAME NIL NIL
  "Generic pathname of source file being loaded or evaluated, or NIL.")

(DEFVAR-RESETTABLE PATCH-SOURCE-FILE-NAMESTRING NIL NIL
  "While loading a patch, holds namestring of generic pathname of the source of the patch.")

;;; If the above is not NIL, this variable accumulates a list of all function specs defined.
(DEFVAR FDEFINE-FILE-DEFINITIONS NIL
  "List of definitions made while loading this source file.")

(DEFVAR NON-FILE-REDEFINED-FUNCTIONS NIL
  "Functions from files redefined from the keyboard and confirmed by the user.")

;;; A :SOURCE-FILE-NAME property is a single pathname for DEFUN of a single file,
;;; or ((type . files) (type . files) ...).
;;; Value returned indicates whether to go ahead with the definition.
(DEFUN RECORD-SOURCE-FILE-NAME (FUNCTION-SPEC
                                &OPTIONAL (TYPE 'DEFUN)
                                          (NO-QUERY (EQ INHIBIT-FDEFINE-WARNINGS T))
                                &AUX (DEFAULT-CONS-AREA BACKGROUND-CONS-AREA))
  "Record a definition of FUNCTION-SPEC, of type TYPE, in the current source file.
The source file's generic-pathname is found in FDEFINE-FILE-PATHNAME.
FUNCTION-SPEC is actually only a function spec if TYPE is 'DEFUN,
which is the default.  If TYPE is 'DEFVAR, the first arg is a variable name, etc.
NO-QUERY inhibits warnings about redefinition in a different file.

The value is T if you should go ahead and perform the definition,
NIL if the user was asked and said no."
  ;; When defining a function in a patch, record it as coming
  ;; from its real source file.  So the editor knows where to find it.
  (IF (AND FS:THIS-IS-A-PATCH-FILE PATCH-SOURCE-FILE-NAMESTRING)
      (LET* ((FDEFINE-FILE-DEFINITIONS NIL)
             (FDEFINE-FILE-PATHNAME
               (SEND (FS:PARSE-PATHNAME PATCH-SOURCE-FILE-NAMESTRING) :GENERIC-PATHNAME))
             (PATCH-SOURCE-FILE-NAMESTRING NIL)
             (PKG-SPEC (SEND FDEFINE-FILE-PATHNAME :GET :PACKAGE))
             (*PACKAGE* (OR (PKG-FIND-PACKAGE PKG-SPEC :FIND) *PACKAGE*)))
        ;; Record the source file as having defined this function.
        ;; THIS-IS-A-PATCH-FILE is still set, to prevent querying,
        ;; but PATCH-SOURCE-FILE-NAMESTRING is not, so we don't recurse forever.
        (RECORD-SOURCE-FILE-NAME FUNCTION-SPEC TYPE NO-QUERY)
        ;; Add the function to the source's list of definitions.
        (RECORD-FILE-DEFINITIONS FDEFINE-FILE-PATHNAME FDEFINE-FILE-DEFINITIONS
                                 NIL FDEFINE-FILE-PATHNAME)))
  (LET ((PATHNAME FDEFINE-FILE-PATHNAME)
        (DEF (CONS-IN-AREA FUNCTION-SPEC TYPE BACKGROUND-CONS-AREA))
        (PROPERTY (FUNCTION-SPEC-GET FUNCTION-SPEC :SOURCE-FILE-NAME)))
    (OR (NULL FDEFINE-FILE-PATHNAME)
        (MEMBER-EQUAL DEF FDEFINE-FILE-DEFINITIONS)
        (SETQ FDEFINE-FILE-DEFINITIONS
              (CONS-IN-AREA DEF FDEFINE-FILE-DEFINITIONS BACKGROUND-CONS-AREA)))
    (COND ((AND (NULL PROPERTY)                 ;Check most common case first
                (EQ TYPE 'DEFUN))
           ;; We don't record the keyboard as a "source file"
           ;; so things like the editor don't get confused.
           (IF FDEFINE-FILE-PATHNAME
               (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PATHNAME :SOURCE-FILE-NAME))
           T)
          ((IF (ATOM PROPERTY)
               (AND (EQ TYPE 'DEFUN) (EQ PATHNAME PROPERTY))
             (EQ PATHNAME (CADR (ASSQ TYPE PROPERTY))))
           T)                                   ;This pathname already known
          (T
           (AND PROPERTY (ATOM PROPERTY)
                (SETQ PROPERTY `((DEFUN ,PROPERTY))))
           (LET ((THIS-TYPE (ASSQ TYPE PROPERTY))
                 (OLD-FILE))
             (COND ((COND ((NULL THIS-TYPE)
                           (IF FDEFINE-FILE-PATHNAME
                               (SETQ THIS-TYPE `(,TYPE)
                                     PROPERTY (NCONC PROPERTY
                                                     (NCONS THIS-TYPE))))
                           T)
                          (NO-QUERY T)
                          (FS:THIS-IS-A-PATCH-FILE T)
                          ((AND (NOT FDEFINE-FILE-PATHNAME)
                                (MEMBER-EQUAL FUNCTION-SPEC NON-FILE-REDEFINED-FUNCTIONS))
                           ;; If user has ever confirmed redefining this fn from the kbd,
                           ;; it is ok to do so again.
                           T)
                          ;; Before format is loaded, don't bomb out trying to query.
                          ((NOT (FBOUNDP 'FQUERY)) T)
                          ;; If all the old definitions are from patch files, don't query.
                          ((NULL (SETQ OLD-FILE
                                       (LOOP FOR FILE IN (CDR THIS-TYPE)
                                          UNLESS (OR (STRINGP FILE)     ;During QLD
                                                     (SEND FILE :GET :PATCH-FILE))
                                            RETURN FILE)))
                           T)
                          ((and (typep pathname 'fs:pathname) ;never query during cold-load
                                ;; $$$ Don't query about redefinitions between
                                ;; two files with same generic name,
                                ;; e.g. "FOO.LISP" and "FOO.FDEF". <22-Nov-88 keith>
                                (or (eq (send pathname :generic-pathname)
                                        (send old-file :generic-pathname))
                                    (QUERY-ABOUT-REDEFINITION FUNCTION-SPEC PATHNAME TYPE
                                                              OLD-FILE)))
                           ;; Though we don't record the keyboard as a "source file",
                           ;; once the user confirms redefining a certain function
                           ;; from the keyboard, we don't ever ask about it again.
                           (UNLESS FDEFINE-FILE-PATHNAME
                             (PUSH FUNCTION-SPEC NON-FILE-REDEFINED-FUNCTIONS))
                           T))
                    ;; We don't record the keyboard as a "source file"
                    ;; so things like the editor don't get confused.
                    (WHEN FDEFINE-FILE-PATHNAME
                      (SETF (CDR THIS-TYPE)
                            (CONS PATHNAME (DELQ PATHNAME (CDR THIS-TYPE))))
                      (FUNCTION-SPEC-PUTPROP FUNCTION-SPEC PROPERTY :SOURCE-FILE-NAME))
                    T)
                   (T NIL)))))))

(DEFUN MAPATOMS-NR-SYM (FUNCTION)
  "Call FUNCTION on every symbol in the world, regardless of packages."
  (FUNCALL FUNCTION NIL)                        ;T and NIL are magic.
  (FUNCALL FUNCTION T)
  (FOR-EVERY-REGION-IN-AREA (REGION NR-SYM)
    (DO ((SYM (%MAKE-POINTER DTP-SYMBOL (%REGION-ORIGIN REGION))
              (%MAKE-POINTER-OFFSET DTP-SYMBOL SYM LENGTH-OF-ATOM-HEAD))
         (CT (TRUNCATE (%REGION-FREE-POINTER REGION) LENGTH-OF-ATOM-HEAD) (1- CT)))
        ((ZEROP CT))
      (FUNCALL FUNCTION SYM))))

(DEFUN FOLLOW-STRUCTURE-FORWARDING (OBJECT)
  "Get the final structure this one may be forwarded to.
Given a pointer to a structure, if it has been forwarded by STRUCTURE-FORWARD,
ADJUST-ARRAY-SIZE, or the like, this will return the target structure,
following any number of levels of forwarding."
  (WITHOUT-INTERRUPTS
    (TAGBODY
     AGAIN
        (SELECT (%P-DATA-TYPE OBJECT)
          (DTP-HEADER-FORWARD
           (SETQ OBJECT (%MAKE-POINTER (%DATA-TYPE OBJECT) (%P-CONTENTS-AS-LOCATIVE OBJECT)))
           (GO AGAIN))
          (DTP-RPLACD-FORWARD
           (SETQ OBJECT (%MAKE-POINTER (%DATA-TYPE OBJECT) (%P-CONTENTS-AS-LOCATIVE OBJECT)))
           (GO AGAIN))
          (DTP-BODY-FORWARD
           (LET ((HEADER-LOCATION (%P-CONTENTS-AS-LOCATIVE OBJECT)))
             (SETQ OBJECT (%MAKE-POINTER-OFFSET (%DATA-TYPE OBJECT)
                                                (%P-CONTENTS-AS-LOCATIVE HEADER-LOCATION)
                                                (%POINTER-DIFFERENCE OBJECT HEADER-LOCATION)))
             (GO AGAIN)))
          (OTHERWISE
           (RETURN-FROM FOLLOW-STRUCTURE-FORWARDING OBJECT))))))

;;>> No longer used --- now use the zwei:indentation delcaration which lives in debugging-info
;;; This is defined here since macros can be defined before the editor is loaded
;;; In fact even before DEFMACRO is loaded
;(DEFVAR ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST* NIL)
;(PROCLAIM '(SPECIAL ZWEI:*LISP-INDENT-OFFSET-ALIST*))

(FSET 'DEFMACRO-SET-INDENTATION-FOR-ZWEI 'IGNORE)

;(DEFUN DEFMACRO-SET-INDENTATION-FOR-ZWEI (NAME NUMBER)
;  (LET ((VARIABLE (IF (BOUNDP 'ZWEI:*LISP-INDENT-OFFSET-ALIST*)
;                     'ZWEI:*LISP-INDENT-OFFSET-ALIST*
;                   'ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST*)))
;    (LET ((X (ASSQ NAME (SYMEVAL VARIABLE))))
;      (IF (NULL X)
;         (PUSH (LIST NAME NUMBER 1) (SYMEVAL VARIABLE))
;       (SETF (CDR X) (LIST NUMBER 1))))))

;(DEFUN DEFMACRO-COPY-INDENTATION-FOR-ZWEI (NAME NAME1)
;  (LET ((VARIABLE (IF (BOUNDP 'ZWEI:*LISP-INDENT-OFFSET-ALIST*)
;                     'ZWEI:*LISP-INDENT-OFFSET-ALIST*
;                   'ZWEI:*INITIAL-LISP-INDENT-OFFSET-ALIST*)))
;    (LET ((X (ASSQ NAME (SYMEVAL VARIABLE)))
;         (Y (ASSQ NAME1 (SYMEVAL VARIABLE))))
;      (IF Y
;         (IF (NULL X)
;             (PUSH (CONS NAME (CDR Y)) (SYMEVAL VARIABLE))
;           (SETF (CDR X) (CDR Y)))))))

;some other documentation stuff is in QMISC.
(DEFUN DOCUMENTATION (SYMBOL &OPTIONAL (DOC-TYPE 'FUNCTION))
  "Try to return the documentation string for SYMBOL, else return NIL.
Standard values of DOC-TYPE are: FUNCTION, VARIABLE, TYPE, STRUCTURE and SETF,
but you can put on and retrieve documentation for any DOC-TYPE.
Documentation strings are installed by SETFing a call to DOCUMENTATION."
  (COND ((AND (EQ DOC-TYPE 'VALUE)
              (GET SYMBOL ':DOCUMENTATION)))
        ((AND (SYMBOLP SYMBOL)
              (LET ((DOC-PROP (GET SYMBOL 'DOCUMENTATION-PROPERTY)))
                (CDR (ASSOC-EQUAL (STRING DOC-TYPE) DOC-PROP)))))
        ((AND (EQ DOC-TYPE 'TYPE)
              (GET SYMBOL 'TYPE-EXPANDER)
              (DOCUMENTATION (GET SYMBOL 'TYPE-EXPANDER) 'FUNCTION)))
        ((AND (EQ DOC-TYPE 'SETF)
              (GET SYMBOL 'SETF-METHOD)
              (DOCUMENTATION (GET SYMBOL 'SETF-METHOD) 'FUNCTION)))
        ((NEQ DOC-TYPE 'FUNCTION) NIL)
        ((SYMBOLP SYMBOL)
         (COND ((INTERPRETER-SPECIAL-FORM SYMBOL)
                (DOCUMENTATION (INTERPRETER-SPECIAL-FORM-HANDLER (INTERPRETER-SPECIAL-FORM SYMBOL))))
               ((FBOUNDP SYMBOL)
                (DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL))))))
        ((CONSP SYMBOL)
         (IF (FUNCTIONP SYMBOL T)
             (IF (EQ (CAR SYMBOL) 'MACRO)
                 (DOCUMENTATION (CDR SYMBOL))
               (OR (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
                   (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
                   (NTH-VALUE 2 (EXTRACT-DECLARATIONS
                                  (CDR (LAMBDA-EXP-ARGS-AND-BODY SYMBOL)) NIL T NIL))))
           (AND (FDEFINEDP SYMBOL)
                (DOCUMENTATION (FDEFINITION (UNENCAPSULATE-FUNCTION-SPEC SYMBOL))))))
        ((COMPILED-FUNCTION-P SYMBOL)
         (IF (ASSQ 'COMBINED-METHOD-DERIVATION (DEBUGGING-INFO SYMBOL))
             ;; its an FEF for a combined method, so do special handling
             (COMBINED-METHOD-DOCUMENTATION SYMBOL)
           (OR (CADR (ASSQ 'DOCUMENTATION (DEBUGGING-INFO SYMBOL)))
               (CADR (ASSQ :DOCUMENTATION (DEBUGGING-INFO SYMBOL))))))))

(defsetf documentation (sym &optional (type nil type-supplied)) (value)
  (when (null type-supplied)
    (setq type (quote 'function)))
  `(si:set-documentation ,sym ,type ,value))

;;; SETF of DOCUMENTATION expands into this.

(DEFUN SET-DOCUMENTATION (SYMBOL DOC-TYPE VALUE)
  (LET* ((S (SYMBOL-NAME DOC-TYPE))
         (C (ASSOC-EQUAL S (GET SYMBOL 'DOCUMENTATION-PROPERTY))))
    (IF C
        (SETF (CDR C) VALUE)
      (when value ;; don't make an entry if it's null
        (LET ((DEFAULT-CONS-AREA (IF (= (%AREA-NUMBER SYMBOL) NR-SYM)
                                     PROPERTY-LIST-AREA
                                   BACKGROUND-CONS-AREA)))
          (PUSH (CONS S VALUE) (GET SYMBOL 'DOCUMENTATION-PROPERTY))))))
  VALUE)

;;; Allow functions to be made obsolete wherever their definitions may be.

(DEFMACRO MAKE-OBSOLETE (FORM-NAME REASON &optional (thing "form"))
  "Mark FORM-NAME as an obsolete THING, with string REASON as the reason.
REASON should be a clause starting with a non-capitalized word.
Uses of FUNCTION will draw warnings from the compiler."
  (declare (zwei:indentation 1 2))
  `(eval-when (eval load compile)
     (PUTPROP ',FORM-NAME 'COMPILER:OBSOLETE 'COMPILER:STYLE-CHECKER)
     (PUTPROP ',FORM-NAME ,(format nil  "is an obsolete ~A; ~A"
                                   (or thing "form")
;;;@@@This wants to inspect the compilation environment to figger out
;;;what FORM-NAME is/will be. T.B.D. --Keith 27oct88
;                                  (cond
;                                    (thing)
;                                    ((special-form-p form-name) "special-form")
;                                    ((macro-function form-name) "macro")
;                                    ((fboundp form-name) "function")
;                                    (t "form"))
                                   reason)
              'COMPILER:OBSOLETE)))
