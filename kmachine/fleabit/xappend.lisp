(defun xappend (lists)
  "append any number of lists.
the value is a list whose elements are those of the argument lists, in order."
;; it would be nice if this would error on circular lists
  (prog (total-length lists-to-copy new-list-start new-list-pointer)
        (cond ((atom lists)
               (return nil))                    ;if the arg is an atom, return NIL
              ((atom (cdr lists))
                (return (car lists))))
        (setq total-length 0)                   ;;; Accumulate length of args we must copy
        (do ((lists-to-measure lists (cdr lists-to-measure)))
            ((atom (cdr lists-to-measure))  ;; plus one more if the last arg is not NIL.
                                            ;; But if all are NIL so far, leave it 0 as signal to COND that follows.
             (and (car lists-to-measure)
                  (not (zerop total-length))
                  (incf total-length)))
          (unless (listp (car lists-to-measure))                ;; verify that all args (except perhaps the last) are lists.
              (let ((list-to-measure (car lists-to-measure)))
                (check-type list-to-measure list)
                (setf (car lists-to-measure) list-to-measure))) ;whaa???
          (setq total-length (+ total-length (length (car lists-to-measure)))))
        (cond ((zerop total-length) (return (car (last lists)))))
        (setq new-list-pointer  (setq new-list-start (make-list total-length)))
        (setq lists-to-copy lists)
     l2 (cond ((null (cdr lists-to-copy))                       ;;; when we reach the last arg, if it's nil, we are done.
               (unless (car lists-to-copy) (return new-list-start))             ;;; otherwise, stick in a pointer to the last arg,
               (setf (car new-list-pointer) (caar lists-to-copy))       ;;; and then change it from an element to a cdr.
               (rplacd new-list-pointer (cdar lists-to-copy))
;              (without-interrupts              ;no cdr codes, change this!!
;                (%p-dpb-offset cdr-error %%q-cdr-code new-list-pointer 0)
;                (%p-dpb-offset cdr-normal %%q-cdr-code new-list-pointer -1))
               (return new-list-start)))
        (do ((list-to-copy (car lists-to-copy) (cdr list-to-copy)))
            ((atom list-to-copy)
             (setq lists-to-copy (cdr lists-to-copy))
             (go l2))
          (setf (car new-list-pointer) (car list-to-copy))
          (setq new-list-pointer (cdr new-list-pointer)))))

P_21
   (MOVE A4 (QUOTE NIL))
   (MOVE A3 (QUOTE NIL))
   (MOVE A2 (QUOTE NIL))
   (MOVE A1 (QUOTE NIL))
   (OPEN-CALL ATOM (QUOTE 1) A5 (O0 A0))
C_136
   (ALU L-R GARBAGE A5 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_118)
C_115
   (MOVE RETURN (QUOTE NIL) CH-RETURN)
C_118
   (KOPEN)
   (OPEN-CALL CDR (QUOTE 1) O0 (O0 A0))
C_130
   (KCALL NIL ATOM (QUOTE 1) A6)
C_133
   (ALU L-R GARBAGE A6 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_127)
C_121
   (OPEN-CALL CAR (QUOTE 1) A7 (O0 A0))
C_124
   (MOVE RETURN A7 CH-RETURN)
C_127
   (MOVE A1 (QUOTE 0))
   (MOVE A7 A0)
   (JUMP DO6036_144)
B_249
   (ALU L-R GARBAGE A1 (QUOTE 0))
   (TEST BR-NOT-EQUAL)
   (BRANCH C_244)
C_234
   (KOPEN)
   (OPEN-CALL LAST (QUOTE 1) O0 (O0 A0))
C_238
   (KCALL NIL CAR (QUOTE 1) A7)
C_241
   (MOVE RETURN A7 CH-RETURN)
C_244
   (OPEN-CALL MAKE-LIST (QUOTE 1) A3 (O0 A1))
C_253
   (MOVE A4 A3)
   (MOVE A2 A0)
   (JUMP L2_29)
DO6036_144
   (KOPEN)
   (OPEN-CALL CDR (QUOTE 1) O0 (O0 A7))
C_225
   (KCALL NIL ATOM (QUOTE 1) A8)
C_228
   (ALU L-R GARBAGE A8 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_171)
C_147
   (OPEN-CALL CAR (QUOTE 1) A9 (O0 A7))
C_170
   (ALU L-R GARBAGE A9 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_167)
C_150
   (ALU L-R GARBAGE A1 (QUOTE 0))
   (TEST BR-NOT-EQUAL)
   (BRANCH C_153)
C_157
   (JUMP B_249)
C_153
   (ALU L+R A1 A1 (QUOTE 1))
   (JUMP B_249)
C_167
   (JUMP B_249)
C_171
   (KOPEN)
   (OPEN-CALL CAR (QUOTE 1) O0 (O0 A7))
C_201
   (KCALL NIL LISTP (QUOTE 1) A9)
C_204
   (ALU L-R GARBAGE A9 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_175)
C_174
   (JUMP B_216)
C_175
   (OPEN-CALL CAR (QUOTE 1) A10 (O0 A7))
C_196
   (MOVE O0 A10 CH-OPEN)
   (KCALL TYPEP (QUOTE 2) A11 (O1 (QUOTE LIST)))
C_188
   (ALU L-R GARBAGE A11 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_180)
C_179
   (JUMP B_190)
C_180
   (MOVE O0 A10 CH-OPEN)
   (MOVE O1 (QUOTE LIST-TO-MEASURE))
   (KCALL CHECK-TYPE-INTERNAL (QUOTE 3) A10 (O2 (QUOTE LIST)))
C_184
B_190
   (MOVE O0 A7 CH-OPEN)
   (KCALL SETCAR (QUOTE 2) IGNORE (O1 A10))
C_318
B_216
   (KOPEN)
   (OPEN-CALL CAR (QUOTE 1) O0 (O0 A7))
C_208
   (KCALL NIL LENGTH (QUOTE 1) A9)
C_211
   (ALU L+R A1 A1 A9)
   (OPEN-CALL CDR (QUOTE 1) A7 (O0 A7))
C_219
   (JUMP DO6036_144)
L2_29
   (OPEN-CALL CDR (QUOTE 1) A5 (O0 A2))
C_67
   (ALU L-R GARBAGE A5 (QUOTE NIL))
   (TEST BR-NOT-EQUAL)
   (BRANCH C_63)
C_32
   (OPEN-CALL CAR (QUOTE 1) A6 (O0 A2))
C_42
   (ALU L-R GARBAGE A6 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_36)
C_35
   (KOPEN)
   (OPEN-CALL CAAR (QUOTE 1) O1 (O0 A2))
C_47
   (KCALL SETCAR (QUOTE 2) IGNORE (O0 A4))
B_58
   (KOPEN)
   (OPEN-CALL CDAR (QUOTE 1) O1 (O0 A2))
C_55
   (KCALL RPLACD (QUOTE 2) IGNORE (O0 A4))
B_62
   (MOVE RETURN A3 CH-RETURN)
C_36
   (MOVE RETURN A3 CH-RETURN)
C_63
   (OPEN-CALL CAR (QUOTE 1) A6 (O0 A2))
C_109
DO6035_76
   (OPEN-CALL ATOM (QUOTE 1) A7 (O0 A6))
C_106
   (ALU L-R GARBAGE A7 (QUOTE NIL))
   (TEST BR-EQUAL)
   (BRANCH C_87)
C_79
   (OPEN-CALL CDR (QUOTE 1) A2 (O0 A2))
C_82
   (JUMP L2_29)
C_87
   (KOPEN)
   (OPEN-CALL CAR (QUOTE 1) O1 (O0 A6))
C_90
   (KCALL SETCAR (QUOTE 2) IGNORE (O0 A4))
B_96
   (OPEN-CALL CDR (QUOTE 1) A4 (O0 A4))
C_94
   (OPEN-CALL CDR (QUOTE 1) A6 (O0 A6))
C_99
   (JUMP DO6035_76)
