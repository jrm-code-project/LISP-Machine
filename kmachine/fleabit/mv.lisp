;;; -*- Mode:LISP; Package:SIMULATOR; Readtable:CL; Base:10 -*-


(defun foo (x)
  (values t
          4
          'foo
          x))


(DEFKFUN FOO
         (X)
         TAG::P_5
         (MOVE RETURN 'T)
         (MOVE A1 '4)
         (MOVE A2 'FOO)
         (MOVE A3 A0)
         (MOVE A0 '7 CH-RETURN))


(multiple-value-bind (x y z w) (f)  ;f will return # returned values + 3 in r0
  ...

  (call f a0)
  (alui16 l-r garbage r0 '<n+3>)   ;(jump 1) (.+5) if f is not mv function (or remove .-.+5)
  (alu l+r garbage r0 opc jcond-ge)
  (branch bind)
  (dispatch)
0 (move a0 'nil)
1 (move r1 'nil)
2 (move r2 'nil)
3 (move r3 'nil)
  ...
n-1
  (move rn-1 'nil)
bind
  (move a1 r1)
  (move a2 r2)
  (move a3 r3)
  ...
  (move an-1 rn-1)




---------


(defun foo ()
  (multiple-value-bind (x y z) (bar)
     (bletch x y z)))




simplified tree:
10281509    ((T_6 NIL C_5) ($*DEFINE 1 ^B_13 FOO ^P_7))   NIL
10282237     ((B_13 IGNORE_12) (C_5 0 (QUOTE T)))   NIL
10281642     ((P_7 NIL K_0) ($OPEN 1 ^C_8 (QUOTE #{NC::CALL-NODE (BAR 1 ^R_11) 10281680})))   NIL
10281758      ((C_8)    (BAR 1 ^R_11))   NIL
10282108       ((R_11 NIL X_1 Y_2 Z_3) ($OPEN 1 ^C_10 (QUOTE #{NC::CALL-NODE (BLETCH 1 K_0 X_1 Y_2 Z_3) 10281830})))   NIL
10282031        ((C_10)   (BLETCH 1 K_0 X_1 Y_2 Z_3))   NIL
;;; FOO

Register preference classes:
(A*  Z_3)
(A*  Y_2)
(A*  X_1)
Z_3: 0
Y_2: 1
X_1: 2


(DEFKFUN FOO NIL
   TAG::P_7
         (OPEN-CALL BAR '0 A2 NIL)
         (ALU L-R NOOP-NO-OVERFLOW-TRAP R0 '6)
         (ALU L+R NOOP-NO-OVERFLOW-TRAP R0 OPC BRANCH-GREATER-THAN-OR-EQUAL)
         (BRANCH #:MVBIND8506)
         (DISPATCH)
         (MOVE A2 'NIL)
         (MOVE R1 'NIL)
         (MOVE R2 'NIL)
   #:MVBIND8506
         (MOVE A2 R1)
         (MOVE A1 R2)
   TAG::R_11
         (MOVE O0 A2 CH-TAIL-OPEN)
         (MOVE O1 A1)
         (TAIL-CALL BLETCH '3 (O2 A0)))





(defun foo (x y z)
  (bletch x y z)
  (multiple-value-setq (x y z) (bar y))
  (frotz z y x))

(DEFKFUN FOO (X Y Z)
      TAG::P_10
         (MOVE O0 A0 CH-OPEN)
         (MOVE O1 A1)
         (KCALL BLETCH '3 IGNORE (O2 A2))
      TAG::B_20
         (OPEN-CALL BAR '1 A0 (O0 A1))
         (ALU L-R NOOP-NO-OVERFLOW-TRAP R0 '6)
         (ALU L+R NOOP-NO-OVERFLOW-TRAP R0 OPC BRANCH-GREATER-THAN-OR-EQUAL)
         (BRANCH #:MVBIND8613)
         (DISPATCH)
         (MOVE A0 'NIL)
         (MOVE R1 'NIL)
         (MOVE R2 'NIL)
      #:MVBIND8613
         (MOVE A0 R1)
         (MOVE A1 R2)
      TAG::R_18
         (MOVE O0 A2 CH-TAIL-OPEN)
         (MOVE O1 A1)
         (TAIL-CALL FROTZ '3 (O2 A0)))



;;; could use open frame but its hard to get a pointer to it



(call mvfun a0)
(alu-field extract-bit-right nop ignore gr:*number-of-return-values*
           hw:%%processor-status-return-code pw-ii)
(movei r0 '<n+3> br-zero)
(branch one-value (alu l-r nop gr:*number-of-return-values* r0))
(alu l+r nop gr:*number-of-return-values* trap-pc br-greater-or-equal)
(branch all-or-more ())
(nop next-pc-dispatch)
