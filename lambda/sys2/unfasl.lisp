;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-
;;; QFASL File Disassembler
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFVAR *UNFASL-TABLE*)
(DEFVAR *UNFASL-TABLE-FILL-POINTER*)
(DEFVAR *UNFASL-INDENTATION*)
(DEFVAR UNFASL-GROUP-DISPATCH)
(DEFVAR UNFASL-GROUP-DISPATCH-SIZE)
(DEFVAR *UNFASL-FILE*)

(defvar *unfasl-print*)
(defvar *unfasl-verbose*)

(MAKUNBOUND 'UNFASL-GROUP-DISPATCH)             ;So don't get screwed if reload it

(defvar *unfasl-symbol-hash-table*)
(defresource unfasl-symbol-hash-table ()
  :constructor (make-hash-table :test #'equal :size 2000.)
  :deinitializer (send object :clear-hash)
  :free-list-size 5)
(defflavor unfasl-symbol (name (pkg) (internalp)) (property-list-mixin)
  (:init-keywords :name :package :internalp)
  (:required-init-keywords :name))
(defmethod (unfasl-symbol :init) (plist)
  (check-type (get plist :name) string)
  (setq name (intern (get plist :name) pkg-keyword-package))
  (let ((package (get plist :package)))
    (declare (unspecial package))
    (when package
      (check-type package string)
      (setq pkg (intern package pkg-keyword-package)))))
(defmethod (unfasl-symbol :print-self) (stream &rest ignore)
  (send stream :string-out "<")  ;extra <> so it doesnt look like a regular symbol, which
                ;would tend to fake people out.
  (cond ((eq pkg #.(intern "" "")) ;107 reader bug
         (write-char #/: stream))
        (pkg
         (print-symbol-name (symbol-name pkg) stream)
         (send stream :string-out (if internalp "::" ":"))))
  (print-symbol-name (symbol-name name) stream)
  (send stream :string-out ">"))
(defun get-unfasl-symbol (name pkg internalp)
  (with-stack-list (key name pkg internalp)
    (or (send *unfasl-symbol-hash-table* :get-hash key)
        (let ((val (make-instance 'unfasl-symbol :name name :package pkg :internalp internalp)))
          (send *unfasl-symbol-hash-table* :put-hash (copy-list key) val)
          val))))

(DEFSUBST UNFASL-NIBBLE () (SEND *UNFASL-FILE* :TYI))

;;; User calls this
(DEFUN UNFASL-FILE (INPUT-FILE &KEY OUTPUT-FILE verbose terse)
  "Write a description of the contents of QFASL file INPUT-FILE into OUTPUT-FILE.
The output file defaults to same name as input, with type = UNFASL."
  (SETQ INPUT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS INPUT-FILE FS:LOAD-PATHNAME-DEFAULTS
                                                       :QFASL)
        OUTPUT-FILE (SEND (IF OUTPUT-FILE
                              (FS:MERGE-PATHNAME-DEFAULTS OUTPUT-FILE INPUT-FILE)
                              INPUT-FILE)
                          ;;>> Mungs type regardless of merging
                          :NEW-TYPE :UNFASL))
  (OR (BOUNDP 'UNFASL-GROUP-DISPATCH) (INITIALIZE-UNFASL-ENVIRONMENT))
  (WITH-OPEN-FILE (*UNFASL-FILE* INPUT-FILE :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
    (OR (AND (= (UNFASL-NIBBLE) #o143150)       ;Check magic ID
             (= (UNFASL-NIBBLE) #o71660))
        (FERROR "~A not a qfasl file" INPUT-FILE))
    (WITH-OPEN-FILE (*STANDARD-OUTPUT* OUTPUT-FILE :DIRECTION :OUTPUT :CHARACTERS T)
      (FORMAT T "; -*- Mode:TEXT -*-~%; This is the UNFASL for ~A~2%"
                (SEND *UNFASL-FILE* :TRUENAME))
      (let ((*unfasl-print* (if terse :terse t))
            (*unfasl-verbose* verbose))
        (UNFASL-TOP-LEVEL))))
  OUTPUT-FILE)

(DEFUN UNFASL-PRINT (INPUT-FILE &key verbose terse)
  "Print a description of the contents of QFASL file INPUT-FILE."
  (SETQ INPUT-FILE (FS:MERGE-AND-SET-PATHNAME-DEFAULTS INPUT-FILE FS:LOAD-PATHNAME-DEFAULTS
                                                       :QFASL))
  (OR (BOUNDP 'UNFASL-GROUP-DISPATCH) (INITIALIZE-UNFASL-ENVIRONMENT))
  (WITH-OPEN-FILE (*UNFASL-FILE* INPUT-FILE :DIRECTION :INPUT :CHARACTERS NIL :BYTE-SIZE 16.)
    (OR (AND (= (UNFASL-NIBBLE) #o143150)       ;Check magic ID
             (= (UNFASL-NIBBLE) #o71660))
        (FERROR "~A not a qfasl file" INPUT-FILE))
    (FORMAT T "; -*- Mode:TEXT -*-~%; This is the UNFASL for ~A~2%"
            (SEND *UNFASL-FILE* :TRUENAME))
    (let ((*unfasl-print* (if terse :terse t))
          (*unfasl-verbose* verbose))
      (UNFASL-TOP-LEVEL)))
  T)

(DEFUN UNFASL-TOP-LEVEL ()
  (LOOP UNTIL (EQ (UNFASL-WHACK) 'EOF)))

(DEFUN UNFASL-WHACK ()
  (LET ((*UNFASL-TABLE* (MAKE-ARRAY LENGTH-OF-FASL-TABLE
                                    :AREA 'FASL-TABLE-AREA
                                    :TYPE 'ART-Q-LIST
                                    :FILL-POINTER FASL-TABLE-WORKING-OFFSET))
        (*UNFASL-INDENTATION* 0)
        FASL-RETURN-FLAG)
    (SETQ *UNFASL-TABLE-FILL-POINTER* FASL-TABLE-WORKING-OFFSET)
    (INITIALIZE-UNFASL-TABLE)
    (using-resource (*unfasl-symbol-hash-table* unfasl-symbol-hash-table)
      (LOOP DOING (UNFASL-GROUP t) UNTIL FASL-RETURN-FLAG)
      FASL-RETURN-FLAG)))

(DEFUN INITIALIZE-UNFASL-TABLE ()
  (SETF (AREF *UNFASL-TABLE* FASL-SYMBOL-HEAD-AREA) 'NR-SYM)
  (SETF (AREF *UNFASL-TABLE* FASL-SYMBOL-STRING-AREA) 'P-N-STRING)
  (SETF (AREF *UNFASL-TABLE* FASL-ARRAY-AREA) 'WORKING-STORAGE-AREA)
  (SETF (AREF *UNFASL-TABLE* FASL-FRAME-AREA) 'MACRO-COMPILED-PROGRAM)
  (SETF (AREF *UNFASL-TABLE* FASL-LIST-AREA) 'WORKING-STORAGE-AREA)
  (SETF (AREF *UNFASL-TABLE* FASL-TEMP-LIST-AREA) 'FASL-TEMP-AREA))

(DEFUN UNFASL-GROUP (&optional top-level-p)
  (LET (FASL-GROUP-FLAG
        FASL-GROUP-BITS
        FASL-GROUP-TYPE
        FASL-GROUP-LENGTH)
    (SETQ FASL-GROUP-BITS (UNFASL-NIBBLE))
    (IF (= 0 (LOGAND FASL-GROUP-BITS %FASL-GROUP-CHECK))
        (FERROR "Fasl group nibble without check bit: ~O" FASL-GROUP-BITS))
    (SETQ FASL-GROUP-FLAG (NOT (= 0 (LOGAND FASL-GROUP-BITS %FASL-GROUP-FLAG))))
    (SETQ FASL-GROUP-LENGTH (LDB %%FASL-GROUP-LENGTH FASL-GROUP-BITS))
    (AND (= FASL-GROUP-LENGTH #o377)
         (SETQ FASL-GROUP-LENGTH (UNFASL-NIBBLE)))
    (SETQ FASL-GROUP-TYPE (LOGAND FASL-GROUP-BITS %FASL-GROUP-TYPE))
    (OR (< FASL-GROUP-TYPE UNFASL-GROUP-DISPATCH-SIZE)
        (FERROR "erroneous fasl group type: ~O" FASL-GROUP-TYPE))
    (when top-level-p
      (unfasl-terpri))                          ;blank line between top level groups
    (when (or top-level-p *unfasl-print*)
      (UNFASL-TERPRI)
      (WRITE-CHAR #/()
      (PRIN1 (NTH FASL-GROUP-TYPE FASL-OPS)))
    (PROG1 (FUNCALL (CL:AREF UNFASL-GROUP-DISPATCH FASL-GROUP-TYPE))
           (UNLESS (ZEROP FASL-GROUP-LENGTH)
             (FORMAT T "~%FASL-GROUP-COUNT wrong: ~D nibbles left over.~%"
                     FASL-GROUP-LENGTH))
           (when (or top-level-p *unfasl-print*)
             (WRITE-CHAR #/))))))

(DEFUN UNFASL-TERPRI ()
  (TERPRI)
  (DOTIMES (I *UNFASL-INDENTATION*)
    (WRITE-CHAR #/SPACE)))

(DEFUN UNFASL-NEXT-NIBBLE ()
  (SETQ FASL-GROUP-LENGTH (1- FASL-GROUP-LENGTH))
  (UNFASL-NIBBLE))

(DEFUN UNFASL-NEXT-NIBBLE-PR ()
  (LET ((NIBBLE (UNFASL-NEXT-NIBBLE)))
    (when *unfasl-print*
      (FORMAT T " [~D]" NIBBLE))
    NIBBLE))

(DEFUN UNFASL-NEXT-VALUE ()
  ;;>> with-indentation
  (LET ((*UNFASL-INDENTATION* (+ 2 *UNFASL-INDENTATION*)))
    (LET ((IDX (UNFASL-GROUP)))
      (VALUES (AREF *UNFASL-TABLE* IDX)
              IDX))))

(DEFUN ENTER-UNFASL-TABLE (V)
  (UNLESS (< *UNFASL-TABLE-FILL-POINTER* LENGTH-OF-FASL-TABLE)
    (FERROR "FASL table overflow: ~S" V))
  (SETF (AREF *UNFASL-TABLE* *UNFASL-TABLE-FILL-POINTER*) V)
  (when (and *unfasl-print*
             *unfasl-verbose*)
    (FORMAT T "  --> ~D" *UNFASL-TABLE-FILL-POINTER*))
  (PROG1 *UNFASL-TABLE-FILL-POINTER*
         (INCF *UNFASL-TABLE-FILL-POINTER*)))

(DEFUN UNFASL-STORE-EVALED-VALUE (V)
  (UNFASL-TERPRI)
  (FORMAT T " -> FASL-EVALED-VALUE(~D)" FASL-EVALED-VALUE)
  (SETF (AREF *UNFASL-TABLE* FASL-EVALED-VALUE) V)
  FASL-EVALED-VALUE)


;;;; FASL OPS

(DEFUN UNFASL-OP-ERR ()
  (PRINC "*** not handled ***")
  (UNLESS (ZEROP FASL-GROUP-LENGTH)
    (PRINC " - following nibbles: ")
    (DO ((I FASL-GROUP-LENGTH (1- I)))
        ((= I 0))
      (UNFASL-NEXT-NIBBLE-PR)))
  0)

(DEFUN UNFASL-OP-INDEX ()
  (LET ((TEM (UNFASL-NEXT-NIBBLE-PR)))
    (when *unfasl-print*
      (FORMAT T " {~S}" (AREF *UNFASL-TABLE* TEM)))
    TEM))

(DEFUN UNFASL-OP-NOOP ()
  T)

(DEFUN UNFASL-OP-STRING ()
  (LET ((STR (UNFASL-OP-SYMBOL//STRING-AUX)))
    (when *unfasl-print*
      (FORMAT T " ~S" STR))
    (ENTER-UNFASL-TABLE STR)))


(DEFUN UNFASL-OP-SYMBOL ()
  (when *unfasl-print*
    (IF FASL-GROUP-FLAG (PRINC " #:")))
  (LET ((SYM (GET-UNFASL-SYMBOL (UNFASL-OP-SYMBOL//STRING-AUX) NIL NIL)))
    (when *unfasl-print*
      (FORMAT T " ~S" SYM))
    (ENTER-UNFASL-TABLE SYM)))


(DEFUN UNFASL-OP-SYMBOL//STRING-AUX ()
  (WITH-OUTPUT-TO-STRING (S)
    (LOOP UNTIL (ZEROP FASL-GROUP-LENGTH)
          AS TEM = (UNFASL-NEXT-NIBBLE)
          ;; TEM contains two 8-bit Lisp Machine characters.
          ;; #o200 is a null character.
       DO (SEND S :TYO (LOGAND #o377 TEM))
          ;;>> Blorp
          (UNLESS (= (SETQ TEM (LSH TEM -8.)) #o200)
            (SEND S :TYO TEM)))))


(defun unfasl-op-package-symbol (&aux (len fasl-group-length) internalp pkg str)
  (let-if (eq *unfasl-print* :terse) ((*unfasl-print* nil))
    (if ( len 1)
        (ferror "Foo"))
    (setq len (unfasl-next-nibble))
    ;; This kludge is so that we can win without the package feature loaded.
    ;; Values of LEN that are meaningful nowadays are:
    ;; 402 - one prefix, double colon (ignore local package nicknames).
    ;; 2 -- one prefix, single colon.
    ;; FASL-GROUP-FLAG is non-NIL to allow internal symbols and creation of symbols.
    (cond ((= len 2))
          ((= len 402)
           (setq internalp t))
          (t (ferror "Bar")))
    (setq pkg (unfasl-next-value))
    (setq str (unfasl-next-value)))
  (setq str (get-unfasl-symbol str pkg internalp))
  (case *unfasl-print*
    ((nil))
    (:terse
     (write-char #/space)
     (prin1 str))
    (t
     (unfasl-terpri)
     (format t "  ~S" str)))
  (enter-unfasl-table str))


;(DEFUN UNFASL-OP-PACKAGE-SYMBOL ()
;  (LET ((SYM (MAKE-SYMBOL (WITH-OUTPUT-TO-STRING (S)
;                           (LOOP FOR I FROM (UNFASL-NEXT-NIBBLE) ABOVE 0
;                              DO (SEND S :STRING-OUT (STRING (UNFASL-NEXT-VALUE)))
;                              UNLESS (= I 1)
;                                DO (SEND S :TYO #/:))))))
;    (when *unfasl-print*
;      (UNFASL-TERPRI)
;      (FORMAT T "  ~S" SYM))
;    (ENTER-UNFASL-TABLE SYM)))

(DEFUN UNFASL-OP-FLOAT ()
  (IF FASL-GROUP-FLAG                           ;Small float
      (LET ((ANS (%MAKE-POINTER DTP-SMALL-FLONUM
                                (%LOGDPB (UNFASL-NEXT-NIBBLE) (BYTE 8. 16.) (UNFASL-NEXT-NIBBLE)))))
        (when *unfasl-print*
          (FORMAT T "  ~S" ANS))
        (ENTER-UNFASL-TABLE ANS))
    (LET ((ANS (FLOAT 0)) TEM)                  ;Big float
      (%P-DPB-OFFSET (UNFASL-NEXT-NIBBLE) (BYTE 11. 8.) ANS 0)
      (SETQ TEM (UNFASL-NEXT-NIBBLE))
      (%P-DPB-OFFSET (LDB (BYTE 8. 8.) TEM) (BYTE 8. 0.) ANS 0)
      (%P-DPB-OFFSET (%LOGDPB TEM (BYTE 8. 16.) (UNFASL-NEXT-NIBBLE)) (BYTE 24. 0.) ANS 1)
      (when *unfasl-print*
        (FORMAT T "  ~S" ANS))
      (ENTER-UNFASL-TABLE ANS))))

;;>> ****** fasl-op-new-float ******

(DEFUN UNFASL-OP-RATIONAL ()
  (LET ((RAT (%RATIO-CONS (UNFASL-NEXT-VALUE) (UNFASL-NEXT-VALUE))))
    (when *unfasl-print*
      (FORMAT T "  ~S" RAT))
    (ENTER-UNFASL-TABLE RAT)))

(DEFUN UNFASL-OP-COMPLEX ()
  (LET ((COMP (COMPLEX (UNFASL-NEXT-VALUE) (UNFASL-NEXT-VALUE))))
    (when *unfasl-print*
      (FORMAT T "  ~S" COMP))
    (ENTER-UNFASL-TABLE COMP)))

(DEFUN UNFASL-OP-LIST (&OPTIONAL AREA COMPONENT-FLAG)
  (IF (NULL AREA) (SETQ AREA (AREF *UNFASL-TABLE* FASL-LIST-AREA)))
  (LET ((LIST-LENGTH (UNFASL-NEXT-NIBBLE-PR)))
    (when *unfasl-print*
      (FORMAT T " Area=~A~:[~; (dotify)~]" AREA FASL-GROUP-FLAG))
    (LET ((LST (LOOP UNTIL (ZEROP LIST-LENGTH)
                  COLLECTING (UNFASL-NEXT-VALUE)
                  DOING (SETQ LIST-LENGTH (1- LIST-LENGTH)))))
      (AND FASL-GROUP-FLAG (DOTIFY (SETQ LST (COPY-LIST LST))))
      (when *unfasl-print*
        (unfasl-terpri)
        (format t " => ~S" lst))
      (IF (NULL COMPONENT-FLAG)
          (ENTER-UNFASL-TABLE LST)
        (UNFASL-STORE-EVALED-VALUE LST)))))

(DEFUN UNFASL-OP-TEMP-LIST ()
  (UNFASL-OP-LIST (AREF *UNFASL-TABLE* FASL-TEMP-LIST-AREA)))

(DEFUN UNFASL-OP-LIST-COMPONENT ()
  (UNFASL-OP-LIST NIL T))


;;; Generate a FIXNUM (or BIGNUM) value.
(DEFUN UNFASL-OP-FIXED ()
  ;;;+++Need to rewrite byte-position arithmetic here! --Keith 23-oct-88
  (DO ((POS (* (1- FASL-GROUP-LENGTH) #o20) (- POS #o20))
       (C FASL-GROUP-LENGTH (1- C))
      (ANS 0))
      ((ZEROP C)
       (IF FASL-GROUP-FLAG (SETQ ANS (MINUS ANS)))
       (WHEN *UNFASL-PRINT*
         (WRITE-CHAR #/SPACE)
         (PRIN1 ANS))
       (ENTER-UNFASL-TABLE ANS))
    (SETQ ANS (DPB (UNFASL-NEXT-NIBBLE) (+ (LSH POS 6) #o20) ANS))))

(DEFUN UNFASL-OP-CHARACTER ()
  (DO ((POS (* (1- FASL-GROUP-LENGTH) #o20) (- POS #o20))
       (C FASL-GROUP-LENGTH (1- C))
       (ANS 0))
      ((ZEROP C)
       (IF FASL-GROUP-FLAG (SETQ ANS (MINUS ANS)))
       (WHEN *UNFASL-PRINT*
         (FORMAT T " ~:C" ANS))
       (ENTER-UNFASL-TABLE ANS))
    (SETQ ANS (DPB (UNFASL-NEXT-NIBBLE) (+ (LSH POS 6) #o20) ANS))))

;>> *******
(DEFUN UNFASL-OP-ARRAY ()
  (LET ((FLAG FASL-GROUP-FLAG))
    (UNFASL-NEXT-VALUE)
    (PRINC " =AREA")
    (UNFASL-NEXT-VALUE)
    (PRINC " =TYPE")
    (UNFASL-NEXT-VALUE)
    (PRINC " =DIMLIST")
    (UNFASL-NEXT-VALUE)
    (PRINC " =DISPLACED-P")
    (UNFASL-NEXT-VALUE)
    (PRINC " =LEADER")
    (UNFASL-NEXT-VALUE)
    (PRINC " =INDEX-OFFSET")
    (WHEN FLAG
      (UNFASL-NEXT-VALUE)
      (PRINC " =NAMED-STRUCTURE"))
;>>
    (ENTER-UNFASL-TABLE 'ARRAY)))

(DEFUN UNFASL-OP-MOVE ()
  (LET ((FROM (UNFASL-NEXT-NIBBLE-PR))
        (TO (UNFASL-NEXT-NIBBLE-PR)))
    (IF (= TO #o177777)
        (ENTER-UNFASL-TABLE (AREF *UNFASL-TABLE* FROM))
      (SETF (AREF *UNFASL-TABLE* TO) (AREF *UNFASL-TABLE* FROM))
      TO)))

(DEFUN UNFASL-OP-FRAME (&aux fef)
  (let-if (not *unfasl-verbose*) ((*unfasl-print* nil))
    (LET ((Q-COUNT (UNFASL-NEXT-NIBBLE))
          (UNBOXED-COUNT (UNFASL-NEXT-NIBBLE))
          (FASL-GROUP-LENGTH (UNFASL-NEXT-NIBBLE))
          (SIZE NIL)                            ;Total number of Qs
          (OBJ NIL)
          (TEM NIL)
          (OFFSET NIL))
      (when *unfasl-print*
        (FORMAT T " Q-count=~D, unboxed-count=~D, group-length=~D"
                Q-COUNT UNBOXED-COUNT FASL-GROUP-LENGTH))
      (SETQ FEF (%make-structure dtp-fef-pointer
                                 dtp-header
                                 (unfasl-next-value)
                                 (setq size (+ q-count unboxed-count))
                                 default-cons-area
                                 size
                                 q-count))
      (UNFASL-NEXT-NIBBLE)                              ;Skip modifier nibble for header Q
      (DO ((I 1 (1+ I)))                                ;Fill in boxed Qs
          (( I Q-COUNT))
        ;; OBJ gets the object to be stored.
        (SETQ OBJ (UNFASL-NEXT-VALUE))
        (SETQ TEM (UNFASL-NEXT-NIBBLE))
        (when *unfasl-print*
          (FORMAT T " CDRC=~O" (LSH TEM -6))
          (OR (ZEROP (LOGAND 1 (LSH TEM -5))) (PRINC " FLAGB"))
          (OR (ZEROP (LOGAND #o20 TEM)) (PRINC " E-V-C-P"))
          (OR (ZEROP (LOGAND #o400 TEM)) (PRINC " LOCATIVE"))
          (OR (ZEROP (LOGAND #o17 TEM)) (FORMAT T " Offset=~D" (LOGAND #o17 TEM))))
        (OR (ZEROP (SETQ OFFSET (LOGAND TEM #o17)))     ;Add offset if necessary
            (SETQ OBJ (%MAKE-POINTER-OFFSET DTP-LOCATIVE OBJ OFFSET)))
        (%P-STORE-CONTENTS-OFFSET OBJ FEF I)            ;Store it
        (%P-DPB-OFFSET (map-to-new-cdr-code (LSH TEM -6)) %%Q-CDR-CODE FEF I)   ;Mung cdr code
        (AND (BIT-TEST #o20 TEM)                        ;Make into external value cell pointer
             (%P-DPB-OFFSET DTP-EXTERNAL-VALUE-CELL-POINTER
                            %%Q-DATA-TYPE FEF I))
        (AND (BIT-TEST #o400 TEM)                       ;Make into locative
             (%P-DPB-OFFSET DTP-LOCATIVE %%Q-DATA-TYPE FEF I))
        (AND (BIT-TEST #o1000 TEM)
             (%P-DPB-OFFSET DTP-SELF-REF-POINTER %%Q-DATA-TYPE FEF I)))
      (DO ((I Q-COUNT (1+ I)))                          ;Now store unboxed Qs
          (( I SIZE))
        (when *unfasl-print*
          (UNFASL-TERPRI)
          (FORMAT T "   UNBOXED ~6,'0O" TEM))
        (%P-DPB-OFFSET (SETQ TEM (UNFASL-NEXT-NIBBLE))  ;Store low-order halfword
                       %%Q-LOW-HALF FEF I)
        (%P-DPB-OFFSET (SETQ TEM (UNFASL-NEXT-NIBBLE))  ;Then high-order halfword
                       %%Q-HIGH-HALF FEF I)
        (when *unfasl-print*
          (format t " ~6,'0O" tem)))))
  (when *unfasl-print*
    (unfasl-terpri)
    (prin1 (function-name fef))
    (disassemble fef))
  (ENTER-UNFASL-TABLE FEF))

(DEFUN UNFASL-OP-ARRAY-PUSH ()
  (UNFASL-NEXT-VALUE)
  (UNFASL-NEXT-VALUE))

(DEFUN UNFASL-OP-FILE-PROPERTY-LIST ()
  (UNFASL-NEXT-VALUE))

(DEFUN UNFASL-OP-STOREIN-SYMBOL-VALUE ()
  (UNFASL-OP-INDEX)
  (UNFASL-NEXT-VALUE))

(DEFUN UNFASL-OP-STOREIN-FUNCTION-CELL ()
  (UNFASL-OP-INDEX)
  (UNFASL-NEXT-VALUE))

(DEFUN UNFASL-OP-STOREIN-PROPERTY-CELL ()
  (UNFASL-OP-INDEX)
  (UNFASL-NEXT-VALUE))

(DEFUN UNFASL-OP-STOREIN-ARRAY-LEADER ()
  (PRINC " ARRAY") (UNFASL-OP-INDEX)
  (PRINC " SUBSCR") (UNFASL-OP-INDEX)
  (PRINC " VALUE") (UNFASL-OP-INDEX))

(DEFUN UNFASL-OP-FETCH-SYMBOL-VALUE ()
  (ENTER-UNFASL-TABLE `(**symbol-value** ',(UNFASL-NEXT-VALUE))))

(DEFUN UNFASL-OP-FETCH-FUNCTION-CELL ()
  (ENTER-UNFASL-TABLE `(**symbol-function** ',(UNFASL-NEXT-VALUE))))

(DEFUN UNFASL-OP-FETCH-PROPERTY-CELL ()
  (ENTER-UNFASL-TABLE `(**symbol-plist** ',(UNFASL-NEXT-VALUE))))

(DEFUN UNFASL-OP-END-OF-WHACK ()
  (SETQ FASL-RETURN-FLAG 'END-OF-WHACK)
  0)

(DEFUN UNFASL-OP-END-OF-FILE ()
  (SETQ FASL-RETURN-FLAG 'EOF)
  0)

(DEFUN UNFASL-OP-SOAK ()
  (LOOP FOR I FROM (UNFASL-NEXT-NIBBLE-PR) ABOVE 0
     DO (UNFASL-NEXT-VALUE)))

(DEFUN UNFASL-OP-FUNCTION-HEADER ()
  (UNFASL-NEXT-VALUE)
  (UNFASL-NEXT-VALUE)
  0)

(DEFUN UNFASL-OP-FUNCTION-END ()
  0)

;; unused
(DEFUN UNFASL-OP-SET-PARAMETER ()
  (ferror "this operation decommitted"))

(DEFUN UNFASL-OP-INITIALIZE-ARRAY ()
  (MULTIPLE-VALUE-BIND (NIL IDX) (UNFASL-NEXT-VALUE)
    (LET ((NUM (UNFASL-NEXT-VALUE)))            ;# Of vals to initialize
      (DO ((IDX 0 (1+ IDX)))
          ((= IDX NUM))
        (UNFASL-NEXT-VALUE)))
    IDX))

(DEFUN UNFASL-OP-INITIALIZE-NUMERIC-ARRAY ()
  (MULTIPLE-VALUE-BIND (NIL IDX) (UNFASL-NEXT-VALUE)
    (LET ((NUM (UNFASL-NEXT-VALUE)))            ;# Of vals to initialize
      (SETQ FASL-GROUP-LENGTH NUM)
      (UNFASL-TERPRI)
      (DO ((IDX 0 (1+ IDX)))
          ((= IDX NUM))
        (WRITE-CHAR #/SPACE)
        (PRIN1 (UNFASL-NEXT-NIBBLE))))
    IDX))

(DEFUN UNFASL-OP-EVAL ()
  (ferror "this operation decommitted"))

(DEFUN UNFASL-OP-EVAL1 ()
  (LET ((FORM (let-if (eq *unfasl-print* :terse) ((*unfasl-print* nil)) (UNFASL-NEXT-VALUE))))
    (when *unfasl-print*
      (UNFASL-TERPRI)
      (FORMAT T "  (~S '~S)" 'EVAL FORM))
    (ENTER-UNFASL-TABLE `(**EVAL** ',FORM))))

(defun unfasl-op-version-info ()
 (unfasl-next-value)
 (unfasl-next-value)
 (enter-unfasl-table ()))

;;; ||| Moved the k unfasl ops to falcon:k.compiler;falcon-unfasl.lisp JIIM 10/24/88
;;;****************************************************************
;;;
;;; unfasl ops for K-Compiled-Functions
;;;
;;;****************************************************************

;(defun unfasl-make-vector (size)
;  (make-array size))

;(defun unfasl-op-k-compiled-function ()
;  (let ((length   (ash fasl-group-length -2))
;       (function (nc::make-ncompiled-function)))

;    (setf (nc:ncompiled-function-code function)
;         (unfasl-k-function-instructions length))

;    (let ((name         (unfasl-next-value))
;         (local-refs   (unfasl-next-value))
;         (refs         (unfasl-next-value))
;         (entry-points (unfasl-next-value)))
;      (setf (nc::ncompiled-function-name         function) name)
;      (setf (nc::ncompiled-function-length       function) length)
;      (setf (nc::ncompiled-function-local-refs   function) local-refs)
;      (setf (nc::ncompiled-function-refs         function) refs)
;      (setf (nc::ncompiled-function-entry-points function) entry-points))

;    (setf (nc:ncompiled-function-immediates function)
;         (unfasl-k-function-immediates))
;    (setf (nc:ncompiled-function-load-time-evals function)
;         (unfasl-k-function-load-time-evals))

;    ;; Now we've got it all hooked up, let's put it into the UNFASL table.
;    (enter-unfasl-table function)


;    (let ((*unfasl-indentation* (+ *unfasl-indentation*  2)))
;      (unfasl-terpri)

;      ;; FASL-OP-STOREIN-FUNCTION-CELL
;      (unfasl-group)

;      (when *unfasl-print*
;       (unfasl-terpri)
;       (unfasl-terpri)
;       (format t "Disassembled Code for ~s" (nc::ncompiled-function-name function))
;       (unfasl-terpri)
;       (dolist (inst (nc::ncompiled-function-code function))
;         (unfasl-terpri)
;         (format t "~a" (nc:dis inst)))
;       (unfasl-terpri)
;       (unfasl-terpri)))
;    ))


;(defun unfasl-read-k-instruction ()
;  (let ((1st (unfasl-next-nibble))
;       (2nd (unfasl-next-nibble))
;       (3rd (unfasl-next-nibble))
;       (4th (unfasl-next-nibble)))
;    (dpb 4th (byte 16. 48.)
;        (dpb 3rd (byte 16. 32.)
;             (dpb 2nd (byte 16. 16.)
;                  (dpb 1st (byte 16. 0.) 0))))))

;(defun unfasl-k-function-instructions (length)
;  (let ((code '()))
;    (dotimes (i length)
;      (push (unfasl-read-k-instruction) code))
;    (nreverse code)))


;(defun unfasl-op-k-local-refs ()
;  (let ((locals (unfasl-next-value)))
;    (when *unfasl-print* (format t "    number of local-refs"))
;    (do ((i 0 (+ i 2))
;        (locs (unfasl-make-vector (* 2 locals))))
;       ((>= i (* 2 locals))
;        (enter-unfasl-table locs))
;      (setf (aref locs i) (unfasl-next-value))
;      (when *unfasl-print* (format t "      ref offset"))
;      (setf (aref locs (1+ i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      target offset")))))

;(defun unfasl-op-k-refs ()
;  (let ((k-refs (unfasl-next-value)))
;    (when *unfasl-print* (format t "    number of refs"))
;    (do ((i 0 (+ i 3))
;        (refs (unfasl-make-vector (* 3 k-refs))))
;       ((>= i (* 3 k-refs))
;        (enter-unfasl-table refs))
;      (setf (aref refs i) (unfasl-next-value))
;      (when *unfasl-print* (format t "      ref offset"))
;      (setf (aref refs (1+ i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      referenced function name"))
;      (setf (aref refs (+ 2 i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      number of args")))))

;(defun unfasl-op-k-entry-points ()
;  (let ((entries (unfasl-next-value)))
;    (when *unfasl-print* (format t "    number of entry points"))
;    (do ((i 0 (+ i 2))
;        (ents (unfasl-make-vector (* 2 entries))))
;       ((>= i (* 2 entries))
;        (enter-unfasl-table ents))
;      (setf (aref ents i) (unfasl-next-value))
;      (when *unfasl-print* (format t "      number of args"))
;      (setf (aref ents (1+ i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      entry offset")))))

;(defun unfasl-k-function-immediates ()
;  (let ((immeds (unfasl-next-value)))
;    (when *unfasl-print* (format t "    number of immediates"))
;    (do ((i 0 (+ i 2))
;        (imms (unfasl-make-vector (* 2 immeds))))
;       ((>= i (* 2 immeds))
;        imms)
;      (setf (aref imms i) (unfasl-next-value))
;      (when *unfasl-print* (format t "      ref offset"))
;      (setf (aref imms (1+ i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      immediate object")))))

;(defun unfasl-k-function-load-time-evals ()
;  (let ((number-of-evals (unfasl-next-value)))
;    (when *unfasl-print* (format t "    number of load-time evals"))
;    (do ((i 0 (+ i 2))
;        (evals (unfasl-make-vector (* 2 number-of-evals))))
;       ((>= i (* 2 number-of-evals))
;        evals)
;      (setf (aref evals i) (unfasl-next-value))
;      (when *unfasl-print* (format t "      ref offset"))
;      (setf (aref evals (1+ i)) (unfasl-next-value))
;      (when *unfasl-print* (format t "      form to eval")))))


;;;****************************************************************
;;;
;;; end of unfasl ops
;;;
;;;****************************************************************

(DEFUN INITIALIZE-UNFASL-ENVIRONMENT ()
  (SETQ UNFASL-GROUP-DISPATCH-SIZE (LENGTH FASL-OPS))
  (SETQ UNFASL-GROUP-DISPATCH (MAKE-ARRAY UNFASL-GROUP-DISPATCH-SIZE))
  (DO ((I 0 (1+ I))
       (L FASL-OPS (CDR L))
       (TEM))
      ((NULL L))
    (SETQ TEM (INTERN (FORMAT NIL "UN~A" (CAR L)) PKG-SYSTEM-INTERNALS-PACKAGE))
    (SETF (AREF UNFASL-GROUP-DISPATCH I) (IF (FBOUNDP TEM) TEM 'UNFASL-OP-ERR))))
