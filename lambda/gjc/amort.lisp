;;; -*- Mode: Lisp; Package: Macsyma;base:10 -*-
;;; Translated code for LAM6: GJC; AMORT.MC#2
;;; Written on 1/28/86 09:24:16, from MACSYMA 9
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
      (setq *infile-name-key* "GJC; AMORT.MC#2"))

(eval-when (compile)
   (setq $tr_semicompile 'NIL)
   (setq forms-to-compile-queue ()))

(comment "GJC; AMORT.MC#2")

;;; General declarations required for translated MACSYMA code.
(DECLARE (SPECIAL $RATEPSILON $TEMP))
(DEFMTRFUN-EXTERNAL ($MONTHLY_PAYMENT $ANY MDEFINE NIL NIL))
(DEF-MTRVAR $TEMP '$TEMP 1)
(DEF-MTRVAR $RATEPSILON '$RATEPSILON 1)
(DEFMTRFUN-EXTERNAL ($APR $ANY MDEFINE NIL NIL))
(DEFMTRFUN-EXTERNAL ($RUN_LOOP $ANY MDEFINE NIL NIL))

(MEVAL* '((MDEFINE SIMP)
          ((MTIMES SIMP) $RATE
                         ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE) $TIME)
                         ((MEXPT SIMP) ((MPLUS SIMP) -1
                                                     ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE)
                                                                   $TIME))
                                       -1)
                         $STARTING_VALUE)
          ((MTIMES SIMP) $RATE
                         ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE) $TIME)
                         ((MEXPT SIMP) ((MPLUS SIMP) -1
                                                     ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE)
                                                                   $TIME))
                                       -1)
                         $STARTING_VALUE)))
(DEFMTRFUN ($MONTHLY_PAYMENT $ANY MDEFINE NIL NIL)
           ($STARTING_VALUE $YEARLY_PERCENTAGE_RATE $YEARS)
           NIL
           (SIMPLIFY (MFUNCTION-CALL $PAYMENT
                                     $STARTING_VALUE
                                     (DIV (DIV $YEARLY_PERCENTAGE_RATE 100) 12)
                                     (MUL* 12 $YEARS))))
(MEVAL*
 (QUOTE
  ((MDEFINE SIMP)
   ((MPLUS SIMP)
    $RATE_APPROX
    ((MTIMES SIMP)
     -1
     ((MPLUS SIMP) ((MTIMES SIMP) -1
                                  $PAYMENT
                                  ((MEXPT SIMP) $RATE_APPROX -1)
                                  ((MPLUS SIMP) -1
                                                ((MEXPT SIMP) ((MPLUS SIMP) 1
                                                                            $RATE_APPROX)
                                                              $TIME)))
                   ((MTIMES SIMP) ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) $TIME)
                                  $STARTING_VALUE))
     ((MEXPT SIMP)
      ((MPLUS SIMP)
       ((MTIMES SIMP) $PAYMENT
                      ((MEXPT SIMP) $RATE_APPROX -2)
                      ((MPLUS SIMP) -1 ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) $TIME)))
       ((MTIMES SIMP) -1
                      $PAYMENT
                      ((MEXPT SIMP) $RATE_APPROX -1)
                      ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) ((MPLUS SIMP) -1 $TIME))
                      $TIME)
       ((MTIMES SIMP) ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) ((MPLUS SIMP) -1 $TIME))
                      $STARTING_VALUE
                      $TIME))
      -1)))
   ((MPLUS SIMP)
    $RATE_APPROX
    ((MTIMES SIMP)
     -1
     ((MPLUS SIMP) ((MTIMES SIMP) -1
                                  $PAYMENT
                                  ((MEXPT SIMP) $RATE_APPROX -1)
                                  ((MPLUS SIMP) -1
                                                ((MEXPT SIMP) ((MPLUS SIMP) 1
                                                                            $RATE_APPROX)
                                                              $TIME)))
                   ((MTIMES SIMP) ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) $TIME)
                                  $STARTING_VALUE))
     ((MEXPT SIMP)
      ((MPLUS SIMP)
       ((MTIMES SIMP) $PAYMENT
                      ((MEXPT SIMP) $RATE_APPROX -2)
                      ((MPLUS SIMP) -1
                                    ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) $TIME)))
       ((MTIMES SIMP) -1
                      $PAYMENT
                      ((MEXPT SIMP) $RATE_APPROX -1)
                      ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX) ((MPLUS SIMP) -1 $TIME))
                      $TIME)
       ((MTIMES SIMP) ((MEXPT SIMP) ((MPLUS SIMP) 1 $RATE_APPROX)
                                    ((MPLUS SIMP) -1 $TIME))
                      $STARTING_VALUE
                      $TIME))
      -1))))))
(DEFMTRFUN
 ($APR $ANY MDEFINE NIL NIL)
 ($MORT_AMOUNT $POINTS $PAYMENT $STATED_RATE $YEARS)
 NIL
 ((LAMBDA ($STARTING_VALUE $MPR $TEMP)
    NIL
    (DO ((MDO 1 (+ 1 MDO)))
        ((NOT (PROGN (SETQ $TEMP (SIMPLIFY (MFUNCTION-CALL $RATE_APPROX
                                                           $STARTING_VALUE
                                                           $PAYMENT
                                                           $MPR
                                                           (MUL* $YEARS 12))))
                     (IS-BOOLE-CHECK (MGRP (SIMPLIFY (LIST '(MABS)
                                                           (ADD* (TRD-MSYMEVAL $TEMP
                                                                               '$TEMP)
                                                                 (*MMINUS $MPR))))
                                           (TRD-MSYMEVAL $RATEPSILON '$RATEPSILON)))))
         '$DONE)
      ($PRINT '|&MONTHLY PRECENTAGE APPROXIMATELY =| (MUL* (TRD-MSYMEVAL $TEMP '$TEMP) 100))
      (SETQ $MPR (TRD-MSYMEVAL $TEMP '$TEMP)))
    (MUL* (ADD* (POWER (ADD* $MPR 1) 12) -1) 100))
  (ADD* $MORT_AMOUNT (*MMINUS (DIV (MUL* $MORT_AMOUNT $POINTS) 100)))
  (DIV (DIV $STATED_RATE 100) 12)
  '$TEMP))
(DEFMTRFUN ($RUN_LOOP $ANY MDEFINE NIL NIL)
           NIL
           NIL
           ((LAMBDA ($MA $PAY $YPR $YRS $PNTS $APR $INS)
              NIL
              (PROG NIL
               $LOOP
                    (SETQ $YPR ($FLOAT (SIMPLIFY ($READ '|&Percentage|))))
                    (SETQ $YRS (SIMPLIFY ($READ '|&Years|)))
                    (SETQ $PNTS (SIMPLIFY ($READ '|&Points|)))
                    (SETQ $INS ($FLOAT (SIMPLIFY ($READ '|&Insurance factor (or zero)|))))
                    ($PRINT '|&Calculations for|
                            $MA
                            '|&at|
                            $YPR
                            '|&for|
                            $YRS
                            '|&years. with|
                            $PNTS
                            '|&points|)
                    (SETQ $PAY (ADD* (SIMPLIFY (MFUNCTION-CALL $MONTHLY_PAYMENT
                                                               $MA
                                                               $YPR
                                                               $YRS))
                                     (DIV (MUL* $MA $INS) 12)))
                    ($PRINT '|&Monthly Payment =| $PAY)
                    (SETQ $APR (SIMPLIFY (MFUNCALL $APR $MA $PNTS $PAY $YPR $YRS)))
                    ($PRINT '|&Actual Rate (APR) =| $APR)
                    (RETURN (GO $LOOP))))
            (SIMPLIFY ($READ '|&Mortgage amount|))
            '$PAY
            '$YPR
            '$YRS
            '$PNTS
            '$APR
            '$INS))

(compile-forms-to-compile-queue)
