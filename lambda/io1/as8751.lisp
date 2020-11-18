;;; -*- Mode: LISP;  Package: LMUNET; Base: 8 -*-

;       ** (c) Copyright 1982 Massachusetts Institute of Technology **
;Converted from as8748 by RG, 1/7/82

;;; Assemble for 8x51 series of microprocessors

(declare (special as-pc as-labels as-assignments as-pass as-must-be-assigned as-form as-code
                  as-known-new-pc))

(defun as-init ()
  (setq as-pc 0
        as-must-be-assigned (eq as-pass 'PASS-2)
        as-code (ncons (list '= '0))
        as-known-new-pc nil)
  (selectq as-pass
    (PASS-1 (setq as-labels nil
                  as-assignments nil))))

(defun as-pass-1 (forms &aux (as-pass 'PASS-1))
  (as-init)
  (mapc 'as-compile-form forms))

(defun as-pass-2 (forms &aux (as-pass 'PASS-2))
  (as-init)
  (mapc 'as-compile-form forms))

(defun as-internal (forms)
  "Assemble a list of input forms, giving a list of assembled code."
  (as-pass-1 forms)
  (as-pass-2 forms)
  (nreverse as-code))

(defun as (program &aux as-code)
  "Assemble the program PROGRAM.
The source code is the 'CODE property of the symbol PROGRAM.
The assembled code (a list) is put on the 'ASSEMBLED-CODE property.
The symbol PROGRAM's value is set to the prom-image array."
  (setq as-code (as-internal (or (get program 'code)
                                 (ferror nil "~A is not a defined program" program))))
  (putprop program as-code 'assembled-code)
  (set program (as-convert-to-prom-image as-code))
  (putprop program program 'LOCATION)
  program)

(defun as-load-assembled-code (program)
  (let ((code (get program 'assembled-code))
        (pc -1))
    (dolist (elt code)
      (cond ((and (listp elt)
                  (eq (car elt) '=))
             (setq pc (cadr elt)))
            (t (rc-write-umem pc elt)
               (setq pc (1+ pc)))))
    pc))

(defun as-convert-to-prom-image (code)
  (let ((pc -1)
        (high -1)
        (array))
    ;; Calculate highest address used
    (dolist (elt code)
      (cond ((and (listp elt)
                  (eq (car elt) '=))
             (setq pc (cadr elt)))
            (t (setq pc (1+ pc))))
      (and (> pc high)
           (setq high pc)))
    (setq array (make-array (1+ high)))
    (fillarray array '(0))
    (setq pc -1)
    (dolist (elt code)
      (cond ((and (listp elt)
                  (eq (car elt) '=))
             (setq pc (cadr elt)))
            (t (aset elt array pc)
               (setq pc (1+ pc)))))
    array))

(defun as-generate-8-bits (value)
  (setq as-pc (1+ as-pc))
  (and (eq as-pass 'PASS-2)
       (setq as-code (cons (logand value 377) as-code))))

(defun as-compile-form (as-form)
  (setq as-known-new-pc nil)
  (*catch 'AS-ERROR
    (cond ((symbolp as-form)
           (selectq as-pass
             (PASS-1 (and (assq as-form as-labels)
                          (as-error "Duplicate label ~A" as-form))
                     (push (cons as-form as-pc) as-labels))
             (PASS-2 (let ((label (assq as-form as-labels)))
                       (or label
                           (as-error "Label ~A seen on Pass 2 but not on Pass 1" as-form))
                       (or (= (cdr label) as-pc)
                           (as-error "Phase error: Label ~A, Old PC=~O, New PC=~O"
                                     as-form (cdr label) as-pc))))))
          ((listp as-form)
           (let ((dispatch (get (car as-form) 'AS-DISPATCH)))
             (cond (dispatch (funcall dispatch as-form))
                   (( (length as-form) 1)
                    (as-error "Undefined operation in form ~A" as-form))
                   (t (as-generate-8-bits (as-hack-expression (car as-form)))))))
          ((numberp as-form)
           (as-generate-8-bits as-form))
          ((as-error "Garbage form")))))

(do ((r '(R0 R1 R2 R3 R4 R5 R6 R7) (cdr r))
     (n 0 (1+ n)))
    ((null r))
  (putprop (car r) `(REGISTER ,n) 'AS-REGISTER))

(putprop '@R0 '(REGISTER-INDIRECT 0) 'AS-REGISTER)
(putprop '@R1 '(REGISTER-INDIRECT 1) 'AS-REGISTER)

;The 8751 has two forms of direct addressing, BIT and BYTE.  Fortunately,
; you always know from context when you want a BIT specifier before you look
; at it at all.  Thus, BIT specifiers are parsed by AS-PARSE-BIT-SPEC and dont
; go thru here at all.  Direct BYTE specifiers, on the other hand, frequently occur
; in the same contexts as other addressing modes.  Direct BYTE specifiers must
; always be symbols or numbers.  Note that to reference the accumulator
; as a direct byte, the ACC symbol must be used.

(defun as-parse-arg (arg)
  (prog (tem)
    (cond ((numberp arg) (return arg 'NUMBER))
          ((symbolp arg)
           (cond ((setq tem (get arg 'AS-REGISTER))
                  (return (cadr tem) (car tem)))
                 ((eq arg 'A) (return 'A 'A))           ;accumulator
                 ((eq arg 'C) (return 'C 'C))           ;carry bit/ bit accumulator
                 ((eq arg 'DPTR) (return 'DPTR 'DPTR))  ;data pointer
                 ((SETQ TEM (GET ARG 'AS-DIRECT-BYTE-ADDRESS))
                  (RETURN TEM 'DIRECT))
                 (t (return (as-hack-expression arg) 'ADDRESS))))
          ((listp arg)
           (cond ((eq (car arg) '/#)
                  (return (as-hack-expression (cadr arg)) 'IMMEDIATE))
                 (t (return (as-hack-expression arg) 'ADDRESS))))
          (t (as-error "~A is illegal arg" arg)))))

(DEFUN AS-PARSE-BIT-SPEC (ARG)
  (COND ((NUMBERP ARG) ARG)
        ((NOT (EQ (CAR ARG) 'BIT))
         (AS-ERROR "BAD BIT SPEC ~S" ARG))
        (T
         (LET ((REG (CADR ARG))
               (BITNUM (AS-PARSE-ARG (CADDR ARG))))
           (LET ((BASE-LOCN
                   (COND ((NUMBERP REG)
                          (IF (OR (< REG 32.)
                                  (> REG 47.))
                              (AS-ERROR "~A bit reference out of range" ARG)
                              (- REG 32.)))
                         ((AND (SYMBOLP REG)
                               (GET REG 'AS-BIT-ADDRESSIBLE))
                          (GET REG 'AS-DIRECT-BYTE-ADDRESS))
                         ((AND (SYMBOLP REG)
                               (GET REG 'AS-DIRECT-BYTE-ADDRESS))
                          (AS-ERROR "~A is only byte addressible, not bit addressible"
                                    REG))
                         (T (MULTIPLE-VALUE-BIND (VALUE FLAG)
                                (AS-PARSE-ARG REG)
                              (SELECTQ FLAG
                                (ADDRESS
                                 (IF (OR (< VALUE 32.)
                                         (> VALUE 47.))
                                     (AS-ERROR "~A bit reference out of range" ARG))))
                              VALUE)))))
             (IF (OR (NOT (NUMBERP BITNUM))
                     (< BITNUM 0)
                     (> BITNUM 10))
                 (AS-ERROR "~A bad bit number spec" ARG))
             (+ BASE-LOCN BITNUM))))))

(defun as-error (error-string &rest args)
  (lexpr-funcall #'format t error-string args)
  (format t " while assembling ~A~%" as-form)
  (and as-known-new-pc (setq as-pc as-known-new-pc))
  (*throw 'AS-ERROR nil))

(DEFUN AS-NONEXISTANT-INSTRUCTION (FORM)
  (AS-ERROR "Instruction ~S does not exist" FORM))

;;; "Pseduo-ops"
(defun as-set-pc (form)
  (setq as-pc (as-hack-expression (cadr form)))
  (and (eq as-pass 'PASS-2)
       (setq as-code (cons `(= ,as-pc) as-code))))


;;; Standard forms
(defun as-arithmetic (form)
  (as-generate-8-bits (as-hack-expression form)))

(defun as-hack-expression (form)
  (cond ((symbolp form)
         (or (and (eq form 'pc) as-pc)
             (cdr (assq form as-labels))
             (cdr (assq form as-assignments))
             (and (boundp form) (symeval form))
             (and as-must-be-assigned
                  (as-error "~A is undefined" form))
             1))
        ((numberp form) form)
        (t (apply (car form) (mapcar #'as-hack-expression (cdr form))))))

;;; ADD, ADDC, SUBB, XCH instructions
; A must be first arg.
(defun as-add (form)
  (or (eq (cadr form) 'A)
      (as-error "~A has ~A, not A, as first operand" (car form) (cadr form)))
  (let ((value) (flag))
    (multiple-value (value flag)
      (as-parse-arg (caddr form)))
    (selectq flag
      (REGISTER
       (as-generate-8-bits (logior (GET (CAR FORM) 'REGISTER-TO-A) value)))
      (REGISTER-INDIRECT
       (as-generate-8-bits (logior (GET (CAR FORM) 'REGISTER-INDIRECT-TO-A) value)))
      (IMMEDIATE
       (as-generate-8-bits (GET (CAR FORM) 'IMMEDIATE-TO-A))
       (as-generate-8-bits value))
      ((DIRECT NUMBER)
       (AS-GENERATE-8-BITS (GET (CAR FORM) 'DIRECT-TO-A))
       (AS-GENERATE-8-BITS VALUE))
      (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))

;;; two byte jumps SJMP JB JNB JC JNC
(defun as-2B-jump-class (form &aux value flag)
  (setq as-known-new-pc (+ 2 as-pc))            ;We will always generate two bytes
  (multiple-value (value flag)
    (as-parse-arg (cadr form)))
  (as-generate-8-bits (get (car form) 'as-jump-instruction))
  (selectq flag
    ((ADDRESS NUMBER)
     (as-generate-relative-jump-byte value))
    (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))

(COMMENT
(defun as-assemble-relative-jump-byte (sym-adr &aux value flag)
  (multiple-value (value flag)
    (as-parse-arg sym-adr))
  (selectq flag
    ((address NUMBER)
     (as-generate-relative-jump-byte value))
    (OTHERWISE (AS-ERROR "~A bad jump address field")))) )

(defun as-generate-relative-jump-byte (new-pc)
  (COND ((EQ AS-PASS 'PASS-1)
         (AS-GENERATE-8-BITS 0))
        (T
         (LET ((DISP (- new-pc as-known-new-pc)))
           (IF (OR (> DISP 177)
                   (< DISP -200))
               (AS-ERROR "branch out of range"))
           (as-generate-8-bits disp)))))

(defun as-bit-conditional-jump-class (form &aux value flag bit-spec)
  (setq as-known-new-pc (+ 3 as-pc))            ;We will always generate three bytes
  (setq bit-spec (as-parse-bit-spec (cadr form)))
  (multiple-value (value flag)
    (as-parse-arg (caddr form)))
  (selectq flag
    ((ADDRESS NUMBER)
     (as-generate-8-bits (get (car form) 'as-jump-instruction))
     (as-generate-8-bits bit-spec)
     (as-generate-relative-jump-byte VALUE))
    (OTHERWISE (as-nonexistant-instruction form))))

(DEFUN AS-SJMP (FORM)
  (SETQ AS-KNOWN-NEW-PC (+ 2 AS-PC))
  (MULTIPLE-VALUE-BIND (VALUE FLAG)
      (AS-PARSE-ARG (CADR FORM))
    (SELECTQ FLAG
      ((ADDRESS NUMBER)
       (AS-GENERATE-8-BITS (GET (CAR FORM) 'AS-JUMP-INSTRUCTION))
       (AS-GENERATE-RELATIVE-JUMP-BYTE VALUE))
      (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))

(DEFUN AS-AJMP (form &aux value flag)
  (setq as-known-new-pc (+ 2 as-pc))            ;We will always generate two bytes
  (multiple-value (value flag)
    (as-parse-arg (cadr form)))
  (selectq flag
    ((ADDRESS NUMBER)
     (as-generate-8-bits (logior (GET (CAR FORM) 'AS-JUMP-INSTRUCTION)
                                 (lsh (ldb 1003 value) 5)))
     (as-generate-8-bits value))
    (OTHERWISE (as-error "~A is illegal type argument to ~A" (cadr form) (car form)))))

(DEFUN AS-LJMP (FORM &AUX VALUE FLAG)
  (SETQ AS-KNOWN-NEW-PC (+ 3 AS-PC))
  (MULTIPLE-VALUE (VALUE FLAG)
    (AS-PARSE-ARG (CADR FORM)))
  (SELECTQ FLAG
    ((ADDRESS NUMBER)
     (AS-GENERATE-8-BITS (GET (CAR FORM) 'AS-JUMP-INSTRUCTION))
     (AS-GENERATE-8-BITS (LSH VALUE -8))
     (AS-GENERATE-8-BITS VALUE))))

(defun as-DJNZ (form &aux value flag)
  (multiple-value (value flag)
    (as-parse-arg (cadr form)))
  (selectq flag
    (REGISTER
     (let ((reg value))
       (setq as-known-new-pc (+ 2 as-pc))
       (multiple-value (value flag)
         (as-parse-arg (caddr form)))
       (selectq flag
         ((ADDRESS NUMBER)
          (as-generate-8-bits (logior 330 reg))
          (as-generate-RELATIVE-JUMP-BYTE value))
         (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))
    ((DIRECT NUMBER)
     (SETQ AS-KNOWN-NEW-PC (+ 3 AS-PC))
       (MULTIPLE-VALUE-BIND (VALUE2 FLAG2)
           (AS-PARSE-ARG (CADDR FORM))
         (SELECTQ FLAG2
           ((ADDRESS NUMBER)
            (AS-GENERATE-8-BITS 325)
            (as-generate-8-bits VALUE)
            (as-generate-relative-jump-byte VALUE2))
           (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))
    (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))

(defun as-INC (form &aux (DEC-FLAG (COND ((EQ (CAR FORM) 'DEC) 20) (T 0))))
  (multiple-value-bind (value flag)
      (as-parse-arg (cadr form))
    (SELECTQ FLAG
      (A (as-generate-8-bits (LOGIOR DEC-FLAG 4)))
      (DPTR
       (IF (NOT (= DEC-FLAG 0))
           (AS-NONEXISTANT-INSTRUCTION FORM)
           (AS-GENERATE-8-BITS 243)))
      (REGISTER
       (as-generate-8-bits (logior DEC-FLAG VALUE 10)))
      (REGISTER-INDIRECT
       (as-generate-8-bits (logior DEC-FLAG value 6)))
      ((DIRECT NUMBER)
       (AS-GENERATE-8-BITS (LOGIOR DEC-FLAG 5))
       (AS-GENERATE-8-BITS VALUE))
      (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))

;ORL ANL XRL
(DEFUN AS-LOGICAL-CLASS (FORM &AUX (BASE-INS (GET (CAR FORM) 'AS-INSTRUCTION)))
  (MULTIPLE-VALUE-BIND (VALUE1 FLAG1)
      (AS-PARSE-ARG (CADR FORM))
    (MULTIPLE-VALUE-BIND (VALUE2 FLAG2)
        (AS-PARSE-ARG (CADDR FORM))
      (SELECTQ FLAG1
        (A
         (SELECTQ FLAG2
           (REGISTER
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS VALUE2 10)))
           (REGISTER-INDIRECT
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS VALUE2 6)))
           (IMMEDIATE
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS 4))
            (AS-GENERATE-8-BITS VALUE2))
           ((DIRECT NUMBER)
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS 5))
            (AS-GENERATE-8-BITS VALUE2))
           (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
        (C
                ;the values of these are not bit decodable.  use this
                ; variable to hold not flag.
         (LET ((BIT-OPERAND (CADDR FORM))
               NOT-FLAG)
           (COND ((AND (LISTP BIT-OPERAND)
                       (EQ (CAR BIT-OPERAND) 'NOT))
                  (SETQ NOT-FLAG T)
                  (SETQ BIT-OPERAND (CADR BIT-OPERAND))))
           (LET ((BIT-SPEC (AS-PARSE-BIT-SPEC BIT-OPERAND))
                 (OPCODE (COND ((NULL NOT-FLAG)
                                (CDR (ASSQ (CAR FORM) '((ANL . 202)
                                                        (ORL . 162)))))
                               (T (CDR (ASSQ (CAR FORM) '((ANL . 260)
                                                          (ORL . 240))))))))
             (IF (NULL OPCODE)
                 (AS-NONEXISTANT-INSTRUCTION FORM))
             (AS-GENERATE-8-BITS OPCODE)
             (AS-GENERATE-8-BITS BIT-SPEC))))
        ((DIRECT NUMBER)
         (SELECTQ FLAG2
           (A
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS 2))
            (AS-GENERATE-8-BITS VALUE1))
           (IMMEDIATE
            (AS-GENERATE-8-BITS (LOGIOR BASE-INS 3))
            (AS-GENERATE-8-BITS VALUE1)
            (AS-GENERATE-8-BITS VALUE2))
           (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
        (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))))


(DEFUN AS-CJNE (FORM &AUX ARG1-VALUE ARG1-FLAG ARG2-VALUE ARG2-FLAG ARG3-VALUE ARG3-FLAG)
  (SETQ AS-KNOWN-NEW-PC (+ AS-PC 3))
  (MULTIPLE-VALUE (ARG1-VALUE ARG1-FLAG) (AS-PARSE-ARG (CADR FORM)))
  (MULTIPLE-VALUE (ARG2-VALUE ARG2-FLAG) (AS-PARSE-ARG (CADDR FORM)))
  (MULTIPLE-VALUE (ARG3-VALUE ARG3-FLAG) (AS-PARSE-ARG (CADDDR FORM)))
  (SELECTQ ARG1-FLAG
    (A (SELECTQ ARG2-FLAG
         (IMMEDIATE
          (AS-GENERATE-8-BITS 264)
          (AS-GENERATE-8-BITS ARG2-VALUE)
          (AS-GENERATE-RELATIVE-JUMP-BYTE ARG3-VALUE))
         ((DIRECT NUMBER)
          (AS-GENERATE-8-BITS 265)
          (AS-GENERATE-8-BITS ARG2-VALUE)
          (AS-GENERATE-RELATIVE-JUMP-BYTE ARG3-VALUE))
         (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
    (REGISTER
     (SELECTQ ARG2-FLAG
       (IMMEDIATE
        (AS-GENERATE-8-BITS (LOGIOR 270 ARG1-VALUE))
        (AS-GENERATE-8-BITS ARG2-VALUE)
        (AS-GENERATE-RELATIVE-JUMP-BYTE ARG3-VALUE))
       (OTHERWISE
        (AS-NONEXISTANT-INSTRUCTION FORM))))
    (REGISTER-INDIRECT
     (SELECTQ ARG2-FLAG
       (IMMEDIATE
        (AS-GENERATE-8-BITS (LOGIOR 266 ARG1-VALUE))
        (AS-GENERATE-8-BITS ARG2-VALUE)
        (AS-GENERATE-RELATIVE-JUMP-BYTE ARG3-VALUE))
       (OTHERWISE
        (AS-NONEXISTANT-INSTRUCTION FORM))))
    (OTHERWISE
     (AS-NONEXISTANT-INSTRUCTION FORM)))
  )

(DEFUN AS-MOV (FORM &AUX ARG1-VALUE ARG1-FLAG ARG2-VALUE ARG2-FLAG)
  (COND ((EQ (CADDR FORM) 'C)                   ;THIS KLUDGE TO AVOID FEEDING BIT SPECIFIER AT
         (AS-GENERATE-8-BITS 222)               ; AS-PARSE-ARG.
         (AS-GENERATE-8-BITS (AS-PARSE-BIT-SPEC (CADR FORM))))
        (T
         (MULTIPLE-VALUE (ARG1-VALUE ARG1-FLAG)
           (AS-PARSE-ARG (CADR FORM)))
         (MULTIPLE-VALUE (ARG2-VALUE ARG2-FLAG)
           (AS-PARSE-ARG (CADDR FORM)))
         (SELECTQ ARG1-FLAG
           (A (SELECTQ ARG2-FLAG
                (REGISTER
                 (AS-GENERATE-8-BITS (LOGIOR ARG2-VALUE 350)))
                (REGISTER-INDIRECT
                 (AS-GENERATE-8-BITS (LOGIOR ARG2-VALUE 346)))
                (IMMEDIATE
                 (AS-GENERATE-8-BITS 164)
                 (AS-GENERATE-8-BITS ARG2-VALUE))
                ((DIRECT NUMBER)
                 (AS-GENERATE-8-BITS 345)
                 (AS-GENERATE-8-BITS ARG2-VALUE))
                (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
           (C (AS-GENERATE-8-BITS 242)
              (AS-GENERATE-8-BITS (AS-PARSE-BIT-SPEC (CADDR FORM))))
           (REGISTER
            (SELECTQ ARG2-FLAG
              (A
               (AS-GENERATE-8-BITS (LOGIOR 370 ARG1-VALUE)))
              (IMMEDIATE
               (AS-GENERATE-8-BITS (LOGIOR 170 ARG1-VALUE))
               (AS-GENERATE-8-BITS ARG2-VALUE))
              ((DIRECT NUMBER)
               (AS-GENERATE-8-BITS (LOGIOR 250 ARG1-VALUE))
               (AS-GENERATE-8-BITS ARG2-VALUE))
              (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
           (REGISTER-INDIRECT
            (SELECTQ ARG2-FLAG
              (A (AS-GENERATE-8-BITS (LOGIOR ARG1-VALUE 366)))
              (IMMEDIATE
               (AS-GENERATE-8-BITS (LOGIOR ARG1-VALUE 166))
               (AS-GENERATE-8-BITS ARG2-VALUE))
              ((DIRECT NUMBER)
               (AS-GENERATE-8-BITS (LOGIOR ARG1-VALUE 246))
               (AS-GENERATE-8-BITS ARG2-VALUE))
              (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
           (DPTR
            (AS-GENERATE-8-BITS 220)
            (AS-GENERATE-8-BITS (LSH ARG2-VALUE -8))
            (AS-GENERATE-8-BITS ARG2-VALUE))
           ((DIRECT NUMBER)
            (SELECTQ ARG2-FLAG
              (A
               (AS-GENERATE-8-BITS 365)
               (AS-GENERATE-8-BITS ARG1-VALUE))
              (REGISTER
               (AS-GENERATE-8-BITS (LOGIOR 210 ARG2-VALUE))
               (AS-GENERATE-8-BITS ARG1-VALUE))
              (REGISTER-INDIRECT
               (AS-GENERATE-8-BITS (LOGIOR 206 ARG2-VALUE))
               (AS-GENERATE-8-BITS ARG1-VALUE))
              (IMMEDIATE
               (AS-GENERATE-8-BITS 165)
               (AS-GENERATE-8-BITS ARG1-VALUE)
               (AS-GENERATE-8-BITS ARG2-VALUE))
              ((DIRECT NUMBER)
               (AS-GENERATE-8-BITS 205)
               (AS-GENERATE-8-BITS ARG2-VALUE)  ;source
               (AS-GENERATE-8-BITS ARG1-VALUE)  ;dest
               )
              (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))
           (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM))))))

(DEFUN AS-MOVX (FORM)
  (IF (EQ (CADR FORM) 'A)
      (MULTIPLE-VALUE-BIND (VALUE FLAG)
          (AS-PARSE-ARG (CADDR FORM))
        (SELECTQ FLAG
          (REGISTER
           (AS-GENERATE-8-BITS (LOGIOR 342 VALUE)))
          (DPTR
           (AS-GENERATE-8-BITS 340))
          (OTHERWISE (AS-ERROR "~A instruction doesnt exist" FORM))))
    (IF (EQ (CADDR FORM) 'A)
        (MULTIPLE-VALUE-BIND (VALUE FLAG)
            (AS-PARSE-ARG (CADR FORM))
          (SELECTQ FLAG
            (REGISTER
             (AS-GENERATE-8-BITS (LOGIOR 362 VALUE)))
            (DPTR
             (AS-GENERATE-8-BITS 360))
            (OTHERWISE
             (AS-NONEXISTANT-INSTRUCTION FORM))))
      (AS-NONEXISTANT-INSTRUCTION FORM))))

(DEFUN AS-PUSH-POP (FORM)
  (MULTIPLE-VALUE-BIND (VALUE FLAG)
      (AS-PARSE-ARG (CADR FORM))
    (SELECTQ FLAG
      ((DIRECT NUMBER)
       (AS-GENERATE-8-BITS (CDR (ASSQ (CAR FORM) '( (PUSH . 320) (POP . 300)))))
       (AS-GENERATE-8-BITS VALUE))
      (OTHERWISE (AS-NONEXISTANT-INSTRUCTION FORM)))))

(DEFUN AS-CLR (FORM)
  (SELECTQ (CADR FORM)
    (A (AS-GENERATE-8-BITS 344))
    (C (AS-GENERATE-8-BITS 303))
    (OTHERWISE
     (LET ((BIT-SPEC (AS-PARSE-BIT-SPEC (CADR FORM))))
       (AS-GENERATE-8-BITS 302)
       (AS-GENERATE-8-BITS BIT-SPEC)))))

(DEFUN AS-SETB (FORM)
  (SELECTQ (CADR FORM)
    (C (AS-GENERATE-8-BITS 323))
    (OTHERWISE
     (LET ((BIT-SPEC (AS-PARSE-BIT-SPEC (CADR FORM))))
       (AS-GENERATE-8-BITS 322)
       (AS-GENERATE-8-BITS BIT-SPEC)))))

(DEFUN AS-CPL (FORM)
  (SELECTQ (CADR FORM)
    (A (AS-GENERATE-8-BITS 364))
    (C (AS-GENERATE-8-BITS 263))
    (OTHERWISE
     (LET ((BIT-SPEC (AS-PARSE-BIT-SPEC (CADR FORM))))
       (AS-GENERATE-8-BITS 262)
       (AS-GENERATE-8-BITS BIT-SPEC)))))

(DEFUN AS-SINGLE-BYTE (FORM)
  (AS-GENERATE-8-BITS (GET (CAR FORM) 'AS-INSTRUCTION)))



;;; Known operations
(defmacro as-ops (&rest type-op-list)
  `(dolist (x ',type-op-list)
     (dolist (y (cdr x))
       (putprop y (car x) 'AS-DISPATCH))))

(as-ops (as-set-pc =))

(as-ops (as-arithmetic + - // * \ \\ LSH ASH ^))

(as-ops (as-add ADD ADDC SUBB XCH))             ;ok
(DEFPROP ADD 50 REGISTER-TO-A)
(DEFPROP ADD 45 DIRECT-TO-A)
(DEFPROP ADD 46 INDIRECT-REGISTER-TO-A)
(DEFPROP ADD 44 IMMEDIATE-TO-A)
(DEFPROP ADDC 70 REGISTER-TO-A)
(DEFPROP ADDC 65 DIRECT-TO-A)
(DEFPROP ADDC 66 INDIRECT-REGISTER-TO-A)
(DEFPROP ADDC 64 IMMEDIATE-TO-A)
(DEFPROP SUBB 230 REGISTER-TO-A)
(DEFPROP SUBB 225 DIRECT-TO-A)
(DEFPROP SUBB 226 INDIRECT-REGISTER-TO-A)
(DEFPROP SUBB 224 IMMEDIATE-TO-A)
(DEFPROP XCH 310  REGISTER-TO-A)
(DEFPROP XCH 305  DIRECT-TO-A)
(DEFPROP XCH 306  INDIRECT-REGISTER-TO-A)

(DEFPROP XCHD 326 INDIRECT-REGISTER-TO-A)

(as-ops (as-2B-jump-class SJMP JC JNC JZ JNZ))  ;ok
(AS-OPS (AS-BIT-CONDITIONAL-JUMP-CLASS JB JNB JBC))     ;ok
(AS-OPS (AS-AJMP AJMP ACALL))                           ;ok
(AS-OPS (AS-LJMP LJMP LCALL))

(dolist (j '((JC 100) (JNC 120) (JZ 140) (JNZ 160)
             (JB 40) (JNB 60) (JBC 20)
             (ACALL 11)  (AJMP 1) (LCALL 22) (LJMP 2) (SJMP 200)
             ) )
  (putprop (car j) (cadr j) 'AS-JUMP-INSTRUCTION))

(as-ops (as-DJNZ DJNZ)          ;ok
        (AS-CJNE CJNE))

(as-ops (as-INC INC)            ;ok
        (as-DEC DEC))           ;ok

(as-ops (as-logical-class ORL ANL XRL))

(DOLIST (X '( (ORL 100) (ANL 120) (XRL 140)))
  (putprop (CAR X) (CADR X) 'AS-INSTRUCTION))

(as-ops (as-MOV MOV)
        (AS-MOVX MOVX)
        (AS-PUSH-POP PUSH POP)
        (AS-XCH XCH)
        (AS-XCHD XCHD)
)

(as-ops (AS-CLR CLR)
        (AS-CPL CPL)
        (AS-SETB SETB)
        (AS-NOP NOP))

(as-ops (as-CALL CALL)
        (as-RET RET)
        (as-RETR RETR))

(as-ops (as-EN EN)
        (as-DIS DIS))

(as-ops (as-swap SWAP))

(as-ops (as-XCH XCH)
        (as-ROTATE RLC RRC RL RR))

(as-ops (as-CPL CPL))

(DOLIST (X '((DA 324) (DIV 204) (JMP-TO-DPTR+A 163) (MOVC-A-@A+DPTR 223)
             (INC-PC-THEN-MOVC-A-@A+PC 203) (MUL 244) (NOP 0)
             (RET 42) (RETI 62)
             (RL 43) (RLC 63) (RR 3) (RRC 23) (SWAP 304)))
  (PUTPROP (CAR X) 'AS-SINGLE-BYTE 'AS-DISPATCH)
  (PUTPROP (CAR X) (CADR X) 'AS-INSTRUCTION))


(as-ops (as-STRT STRT)
        (as-STOP STOP))

(as-ops (as-OUTL OUTL)
        (as-IN IN INS))

(as-ops (as-SEL SEL))

;BYTE STYLE DIRECT ADDRESSES
(DOLIST (X '( (ACC 224.) (B 240.) (PSW 208.) (SP 129.) (DPH 131.) (DPL 130.)
             (P3 176.) (P2 160.) (P1 144.) (P0 128.) (IPC 184.) (IEC 168.)
             (TMOD 137.) (TCON 136.) (T1-HIGH 141.) (T1-LOW 139.)
             (T0-HIGH 140.) (T0-LOW 138.) (SCON 152.) (SBUF 153.)))
  (PUTPROP (CAR X) (CADR X) 'AS-DIRECT-BYTE-ADDRESS))

;BIT ADDRESSIBLE SPECIAL FUNCTION REGISTERS

(DOLIST (X '( ACC B PSW P3 P2 P1 P0 IPC IEC TCON SCON))
  (PUTPROP X T 'AS-BIT-ADDRESSIBLE))
