;; -*- Mode:LISP; Package:MICRO-ASSEMBLER; Base:8 -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

(PROCLAIM '(SPECIAL SPECIAL-OUT-FILE))

(PROCLAIM '(SPECIAL RACMO RADMO RAAMO
                    AREA-LIST COLD-LOAD-AREA-SIZES PAGE-SIZE CONSLP-INPUT CONSLP-OUTPUT))

;;; DEFVARs are in CADRLP.
(PROCLAIM '(SPECIAL A-MEM I-MEM D-MEM MICRO-CODE-SYMBOL-IMAGE))

(DEFUN DUMP-MEM-ARRAY (ARRAYP RA-ORG OUT-FILE)
  (PROG (IDX LIM TEM)
        (SETQ IDX 0)
        (SETQ LIM (CADR (ARRAYDIMS ARRAYP)))
  L     (COND ((NOT (< IDX LIM))
                (RETURN T))
              ((SETQ TEM (ARRAYCALL T ARRAYP IDX))
                (PRIN1 (+ RA-ORG IDX) OUT-FILE)
                (PRINC " "  OUT-FILE)
                (PRIN-16 TEM OUT-FILE)
                (TERPRI OUT-FILE)))
        (INCF IDX)
        (GO L)))

(DEFUN CONS-DUMP-ARRAY (ARRAYP OUT-FILE)
  (PROG (IDX LIM)
        (SETQ IDX 0)
        (SETQ LIM (CADR (ARRAYDIMS ARRAYP)))
  L     (COND ((NOT (< IDX LIM))
                (TERPRI OUT-FILE)
                (RETURN T)))
        (PRINT (ARRAYCALL T ARRAYP IDX) OUT-FILE)
        (INCF IDX)
        (GO L)))

(DEFUN PRIN-16 (NUM OUT-FILE)
  (COND ((MINUSP NUM) (SETQ NUM (PLUS NUM #o40000000000))))
  ;; Turn it into a 32 bit +ve number
  (PRIN1 (LDB (byte #o20 #o40) NUM) OUT-FILE)
  (PRINC " " OUT-FILE)
  (PRIN1 (LDB (byte #o20 #o20) NUM) OUT-FILE)
  (PRINC " " OUT-FILE)
  (PRIN1 (LDB (byte #o20 0) NUM) OUT-FILE)
  (PRINC " " OUT-FILE))

(DEFUN CONS-DUMP-MEMORIES NIL
  (LET ((*PACKAGE* (PKG-FIND-PACKAGE "MICRO-ASSEMBLER")))
    (unless (BOUNDP 'RACMO)
      (READFILE "SYS:CC;CADREG LISP >" *PACKAGE*))
    (with-open-file (OUT-FILE (FS:MAKE-PATHNAME ':HOST "SYS"
                                                ':DIRECTORY "UBIN"
                                                ':NAME (STRING CONSLP-OUTPUT)
                                                ':TYPE "ULOAD"
                                                ':VERSION ':NEWEST)
                              :direction :output)
      (DUMP-MEM-ARRAY I-MEM RACMO OUT-FILE)
      (DUMP-MEM-ARRAY D-MEM RADMO OUT-FILE)
      (DUMP-MEM-ARRAY A-MEM RAAMO OUT-FILE)
      (TERPRI OUT-FILE)
      (unless (NULL (AREF MICRO-CODE-SYMBOL-IMAGE 0))   ;IF HAVE WIPED SYMBOL VECTOR
        (PRINT -3 OUT-FILE)                     ;DUMP MICRO-CODE-SYMBOL AREA
        (PRINT (CONS-DUMP-FIND-AREA-ORIGIN 'MICRO-CODE-SYMBOL-AREA) OUT-FILE))
      (CONS-DUMP-ARRAY MICRO-CODE-SYMBOL-IMAGE OUT-FILE)
      (PRINT -2 OUT-FILE)                       ;NOW DUMP SYMBOLS
      (TERPRI OUT-FILE)
      (CONS-DUMP-SYMBOLS OUT-FILE)
      (PRINT -1 OUT-FILE)))             ;EOF
  t)

(DEFUN CONS-DUMP-FIND-AREA-ORIGIN (AREA)
  (PROG (ADR LST TEM)
        (SETQ ADR 0)
        (SETQ LST AREA-LIST)
   L    (COND ((NULL LST)(BREAK "CANT-FIND-AREA-ORIGIN"))
              ((EQ (CAR LST) AREA) (RETURN ADR))
              (T (OR (SETQ TEM (LIST-ASSQ (CAR LST) COLD-LOAD-AREA-SIZES))
                                      (SETQ TEM 1))))
        (SETQ ADR (+ ADR (* TEM PAGE-SIZE)))
        (SETQ LST (CDR LST))
        (GO L)))

(DEFUN LIST-ASSQ (ITEM IN-LIST)
  (PROG NIL
    L   (COND ((NULL IN-LIST) (RETURN NIL))
              ((EQ ITEM (CAR IN-LIST))
                (RETURN (CADR IN-LIST))))
        (SETQ IN-LIST (CDDR IN-LIST))
        (GO L)))

(DEFUN CONS-DUMP-SYMBOLS (SPECIAL-OUT-FILE)
        (MAPATOMS (FUNCTION CONS-LAP-DUMP-SYMTAB-ELEMENT))
)

(DEFUN CONS-LAP-DUMP-SYMTAB-ELEMENT (SYM)
  (PROG (VAL DMP-TYPE TEM
;; not needed after building 99
         (*PRINT-GENSYM* NIL))  ;Somehow DMP-TYPE can be an uninterned symbol named NUMBER.
        (SETQ VAL (GET SYM 'CONS-LAP-USER-SYMBOL))
    L   (COND ((NULL VAL) (RETURN NIL))
              ((NUMBERP VAL)
                (SETQ DMP-TYPE 'NUMBER))
              ((ATOM VAL)
                (SETQ VAL (CONS-LAP-SYMEVAL VAL))
                (GO L))
             ((AND (SETQ TEM (ASSQ (CAR VAL)
                        '( (I-MEM JUMP-ADDRESS-MULTIPLIER)
                           (D-MEM DISPATCH-ADDRESS-MULTIPLIER)
                           (A-MEM A-SOURCE-MULTIPLIER)
                           (M-MEM M-SOURCE-MULTIPLIER))))
                   (EQ (CAADR VAL) 'FIELD)
                   (EQ (CADADR VAL) (CADR TEM)))
              (SETQ DMP-TYPE (CAR VAL) VAL (CADDR (CADR VAL))))
             (T (RETURN NIL)))
        (PRIN1 SYM SPECIAL-OUT-FILE)
        (PRINC " "  SPECIAL-OUT-FILE)
        (PRIN1 DMP-TYPE SPECIAL-OUT-FILE)
        (PRINC " "  SPECIAL-OUT-FILE)
        (PRIN1 VAL SPECIAL-OUT-FILE)
        (PRINC " "  SPECIAL-OUT-FILE)
        (TERPRI SPECIAL-OUT-FILE)
        (RETURN T)))
