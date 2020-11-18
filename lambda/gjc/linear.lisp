;;; -*- Mode: Lisp; Package: Macsyma;base:10 -*-
;;; Translated code for LAM6: GJC; LINEAR.MC#2
;;; Written on 10/16/85 18:57:43, from MACSYMA 9
;;; Translated for GJC

;;; TRANSL-AUTOLOAD version NIL
;;; TRANSS version NIL TRANSL version NIL TRUTIL version NIL
;;; TRANS1 version NIL TRANS2 version NIL TRANS3 version NIL
;;; TRANS4 version NIL TRANS5 version NIL TRANSF version NIL
;;; TROPER version NIL TRPRED version NIL MTAGS version NIL
;;; MDEFUN version NIL TRANSQ version NIL FCALL version NIL
;;; ACALL version NIL TRDATA version NIL MCOMPI version NIL
;;; TRMODE version NIL TRHOOK version NIL
(eval-when (compile eval)
      (setq *infile-name-key* "GJC; LINEAR.MC#2"))

(eval-when (compile)
   (setq $tr_semicompile 'NIL)
   (setq forms-to-compile-queue ()))

(comment "GJC; LINEAR.MC#2")

;;; General declarations required for translated MACSYMA code.
(DECLARE (SPECIAL $X $A $B))
(DECLARE (SPECIAL $B))
(DECLARE (SPECIAL $A))
(DEF-MTRVAR $X '$X 1)
(DEFMTRFUN-EXTERNAL ($NONZ $BOOLEAN MDEFINE NIL NIL))

(MEVAL* '(($MODEDECLARE) $B $ANY))
(MEVAL* '(($DECLARE) $B $SPECIAL))
(DEF-MTRVAR $B '$B)
(MEVAL* '(($MODEDECLARE) $A $ANY))
(MEVAL* '(($DECLARE) $A $SPECIAL))
(DEF-MTRVAR $A '$A)
(ADD2LNC '$A $PROPS)
(MDEFPROP $A ((($NONZ))) MATCHDECLARE)
(ADD2LNC '$B $PROPS)
(MDEFPROP $B ((($FREEOF) $X)) MATCHDECLARE)
(DEFMTRFUN ($NONZ $BOOLEAN MDEFINE NIL NIL)
           ($E)
           NIL
           (AND (NOT (LIKE $E 0))
                ($FREEOF (TRD-MSYMEVAL $X '$X) $E)))
(DEFUN $LIN (G07762 $X)
  (*CATCH 'MATCH
          (PROG (G07773)
                (SETQ G07773 (RATDISREP (RATCOEF G07762 $X)))
                (COND ((MFUNCTION-CALL $NONZ G07773)
                       (SETQ $A G07773))
                      ((MATCHERR)))
                (SETQ G07762 (SIMPLIFY ($RATSIMP (ADD* G07762 (*MMINUS (MUL* G07773 $X))))))
                (COND (($FREEOF $X G07762)
                       (SETQ $B G07762))
                      ((MATCHERR)))
                (RETURN (RETLIST_TR '$B
                                    (TRD-MSYMEVAL $B '$B)
                                    '$A
                                    (TRD-MSYMEVAL $A '$A)
                                    '$X
                                    $X)))))
(ADD2LNC '$LIN $RULES)
(MDEFPROP $LIN ((MLIST) ((MPLUS SIMP) $B ((MTIMES SIMP) $A $X)) ((MLIST) $X)) $RULE)

(compile-forms-to-compile-queue)
