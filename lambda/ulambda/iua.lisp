
;; -*- Mode:LISP; Package:LAMBDA; Base:8 -*-
;       ** (c) Copyright 1984, Lisp Machine Inc **
;;;     incremental micro assembler for lambda.  makes MCLAP properties compatible with
;;;     micro-compiler.

;;; this takes input compatible (as much as possible) with the regular micro-assemble (LAMLP)
;;; but makes output compatible with the micro-compiler system.  This means the result of
;;; assembling a storage word has two parts, a number and also an S-expression which
;;; is processed by MCLAP at load time.  C-mem addresses, for example, are handled by
;;; the S-expression, so the code is "relocatable".  In fact, it is even "insertable",
;;; ie, at load time, instructions can be inserted to avoid XCT-NEXT across micro-page
;;; boundaries, etc.

;;; The MLAP format is described in the file SYS:MICRO-COMPILER;MLAP.

;;; in more detail:
;   symbol tags are just copied into the output, where they serve the same purpose.
;   tag references turn into (mclap-evaluate-tag <tag>), which can then be DPB'ed as needed.
;;  A and M register references are evaluated at assembly time and thus appear in the constant.
;;  A-CONSTANT refs turn into (mclap-get-a-constant <x>).
;;  MC-LINKAGEs are used to reference main body ucode entry points.  IUA generates these
;;    if it sees a tag defined as a MC-LINKAGE.
;;  MC-LINKAGEs are also used to reference pre-existing dispatch tables.

;;;pseudo ops
; MISC-INST-ENTRY
; ERROR-TABLE

(defun iua (symbol-head &aux ans)
  (let ((code (symeval symbol-head)))
    (loop for exp in code
          as v = (iu-word exp)
          when v
          collect v))))

(defun iu-word (exp)
  (let ((*val* 0)
        (*load-time-field-values* nil))
    (cond ((symbolp exp) exp)
          ((numberp exp)
           (ferror nil "Bad exp ~s" exp))
          (t (iu-eval-fields exp)
             (cond ((null *load-time-field-values*)
                    (list *val*))
                   (t (list *val* *load-time-field-values*)))))))

(defun iu-eval-fields (exp)
  (cond ((memq (car exp) '(misc-inst-entry error-table))
         )
        (t (iu-eval-exps exp))))

(defun iu-eval-exps (exps)
  (cond ((null exps) nil)
        ((symbolp (car exps))
         (let ((type (iu-symbol-type (car exps))))
           ))
        ((memq (caar exps)
               '(a-constant m-constant byte-value byte-mask
                 byte-field lisp-byte all-but-lisp-byte))
         )
        (t <destination>))
  )

;possible types  M A D C BYTE-FIELD
 FUNCTIONAL MC-LINKAGE
(defun iu-symbol-type (sym)
  )

(comment
(DEFUN LAM-LAP-PASS2 (WD)
  (PROG (V)
        (COND (*COMMENT-FLAG*
                (COND ((AND (LISTP WD)
                            (EQ (CAR WD) 'END-COMMENT))
                       (SETQ *COMMENT-FLAG* NIL)))
                (RETURN NIL))
              ((ATOM WD)
               (SETQ LAM-LAP-LAST-SYM WD)
               (SETQ LAM-LAP-WDS-SINCE-LAST-SYM 0)
               (COND ((NOT (EQUAL
                            (LAM-LAP-SYMEVAL WD)
                            (LIST LOCALITY
                                  (CONS 'FIELD
                                        (COND ((EQ LOCALITY 'I-MEM)
                                               (LIST 'JUMP-ADDRESS-MULTIPLIER I-MEM-LOC))
                                              (T (LAM-LAP-BARF LOCALITY
                                                                'BAD-LOCALITY
                                                                'BARF))) )) ))
                      (LAM-LAP-BARF WD 'DEF-DFRS-ON-PASS2 'BARF))))
              ((MEMQ (CAR WD) '(DEF-DATA-FIELD ASSIGN ASSIGN-EVAL DEF-NEXT-BIT
                                               RESET-BIT-POINTER
                                               DEF-NEXT-FIELD END-DISPATCH
                                               DEF-BIT-FIELD-IN-REG)))
              ((MEMQ (CAR WD) '(LOC MODULO))
               (LAM-LAP-LOC-MODULO WD))
              ((EQ (CAR WD) 'REPEAT)
               (LAM-LAP-REPEAT-2 (LAM-LAP-ARG-EVAL (CADR WD))
                                  (CDDR WD)))
              ((EQ (CAR WD) 'MISC-INST-ENTRY)
               (LET ((OPCODE (GET (CADR WD) 'QLVAL)))
                 (COND ((NULL OPCODE)
                        (LAM-LAP-BARF (CADR WD) 'NO-UCODE-ENTRY-INDEX 'WARN))
                       (T
                         (SETQ CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY
                               (MAX OPCODE CURRENT-ASSEMBLY-HIGHEST-MISC-ENTRY))
                         (COND ((NULL LAM-LAP-INIT-STATE)
                                (SETF (AREF MICRO-CODE-SYMBOL-IMAGE (- OPCODE 200))
                                      I-MEM-LOC))
                               (T (SETQ CURRENT-ASSEMBLY-MICRO-ENTRIES;in incremental assembly
                                        (CONS (LIST 'MISC-INST-ENTRY (CADR WD) I-MEM-LOC)
                                              CURRENT-ASSEMBLY-MICRO-ENTRIES))))))))
              ((EQ (CAR WD) 'MICRO-CODE-ILLEGAL-ENTRY-HERE)
               (SETQ MICRO-CODE-SYMBOL-TABLE-FILL-VALUE I-MEM-LOC)
               (LAM-LAP-WIPE-SYMBOL-VECTOR I-MEM-LOC))
              ((AND (EQ (CAR WD) 'MC-LINKAGE)
                    (LISTP (CADR WD)))
               (MAPC (FUNCTION LAM-LAP-MC-LINKAGE-STORE) (CADR WD)))
              ((EQ (CAR WD) 'ERROR-TABLE)
               (SETQ CURRENT-ASSEMBLY-TABLE
                     (NCONC CURRENT-ASSEMBLY-TABLE
                            (LIST (CONS (1- I-MEM-LOC) (CDR WD))))))
              ((EQ (CAR WD) 'COMMENT))
              ((EQ (CAR WD) 'IF)
               (COND ((EVAL (CADR WD))
                      (LAM-LAP-PASS2 (CADDR WD)))
                     (T (MAPC (FUNCTION LAM-LAP-PASS2) (CDDDR WD)))))
              ((EQ (CAR WD) 'BEGIN-COMMENT)
               (SETQ *COMMENT-FLAG* T)
               (GO X))
              ((EQ (CAR WD) 'MACRO-IR-DISPATCH-SPEC)
               (GO X))
              ((EQ (CAR WD) 'MACRO-IR-DECODE)
               (PUSH (CONS I-MEM-LOC (CADR WD))
                     *MACRO-IR-DISPATCH-ALIST*)
               (GO X))
              ((EQ (CAR WD) 'MACRO-IR-MISC-DECODE)
               (PUSH (CONS I-MEM-LOC (CADR WD))
                     *MACRO-IR-MISC-DISPATCH-ALIST*))
              ((get (car wd) 'lam-lap-macro)
               (lam-lap-call-macro 'lam-lap-pass2 wd))
              (T (GO W1)))
     X  (RETURN NIL)

     W1 (COND (DISPATCH-ARM
               (LAM-LAP-BARF WD 'STORAGE-WD-IN-UNLOCATED-DISPATCH-BLOCK 'DATA)))
        (SETQ V (LAM-WORD-EVAL WD))
        (SETQ LAM-LAP-WDS-SINCE-LAST-SYM (1+ LAM-LAP-WDS-SINCE-LAST-SYM))
        (COND ((EQ LOCALITY 'A-MEM)
               (COND ((>= A-MEM-LOC (ARRAY-ACTIVE-LENGTH A-MEM))
                      (LAM-LAP-BARF A-MEM-LOC 'A-MEM-OVERFLOW 'DATA))
                     ((>= A-MEM-LOC 40)         ;The rest is really m-memory.
                      (SETF (AREF A-MEM A-MEM-LOC) V)))
               (SETQ A-MEM-LOC (1+ A-MEM-LOC)))
              ((EQ LOCALITY 'M-MEM)
               (COND ((< M-MEM-LOC 40)
                      (SETF (AREF A-MEM M-MEM-LOC) V))
                     (T (LAM-LAP-BARF M-MEM-LOC 'M-MEM-OVERFLOW 'DATA)))
               (SETQ M-MEM-LOC (1+ M-MEM-LOC)))
              ((EQ LOCALITY 'D-MEM)
               (SETQ V (+ V DISPATCH-CONSTANT)) ;CONSTANT FOR ENTIRE BLOCK
               (SETQ V (+ (LSH (LDB LAM-IR-RPN V) 16.)  ;RPN BITS FROM JUMP
                          (LDB LAM-IR-JUMP-ADDR V)              ;PC FROM JUMP
                          (DPB (LDB 0001 V) LAM-DISP-START-MEM-READ-BIT 0))) ;SPECIAL CROCK
               (SETF (AREF D-MEM D-MEM-LOC) V)
               (SETQ D-MEM-LOC (1+ D-MEM-LOC)))
              ((EQ LOCALITY 'I-MEM)
               (COND ((AND *PAGABLE-UCODE-MODE*
                           (LAM-INSERT-NOOP-FOR-PAGING V))
                      (SETF (AREF I-MEM I-MEM-LOC) 0)   ;INSERT NOOP.
                      (SETQ I-MEM-LOC (1+ I-MEM-LOC))))
               (IF ( I-MEM-LOC (ARRAY-ACTIVE-LENGTH I-MEM))
                   (LAM-LAP-BARF I-MEM-LOC 'I-MEM-OVERFLOW 'DATA)
                 (SETF (AREF I-MEM I-MEM-LOC) V))
               (SETQ I-MEM-LOC (1+ I-MEM-LOC)))
              (T (LAM-LAP-BARF WD 'STORAGE-WD-IN-BAD-LOCALITY 'DATA)))
        (RETURN NIL)
        ))
 )
