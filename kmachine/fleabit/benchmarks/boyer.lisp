;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:ZL -*-


(defun keql (x y)
  (or (eq x y)
      nil))  ;etc


(DEFKFUN KEQL (X Y)
  TAG::EQL_5
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A0 A1 BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_10)
  TAG::C_17
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_10
         (MOVE RETURN 'NIL CH-RETURN))

(defun kmember (item list)
  (do ((list list (cdr list)))
      ((or (null list)
           (eql item (car list)))
       list)))


(DEFKFUN KMEMBER (ITEM LIST)
  TAG::KMEMBER_8
         (MOVE A3 A1)
  TAG::DO1522_14
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_17)
  TAG::C_30
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A3))
  TAG::C_35
         (KCALL KEQL '2 A2 (O0 A0))
  TAG::C_42
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_17)
  TAG::C_18
         (OPEN-CALL KWRAP::CDR '1 A3 (O0 A3))
  TAG::C_22
         (JUMP TAG::DO1522_14)
  TAG::C_17
         (MOVE RETURN A3 CH-RETURN))

(defun kequal (x y)
  (or (keql x y)
      (and (consp x)
           (consp y)
           (kequal (car x) (car y))
           (kequal (cdr x) (cdr y)))))


(DEFKFUN KEQUAL (X Y)
  TAG::KEQUAL_5
         (MOVE O0 A0 CH-OPEN)
         (KCALL KEQL '2 A5 (O1 A1))
  TAG::C_52
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A5 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_10)
  TAG::C_9
         (MOVE RETURN A5 CH-RETURN)
  TAG::C_10
         (OPEN-CALL KWRAP::CONSP '1 A4 (O0 A0))
  TAG::C_48
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A4 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_45)
  TAG::C_13
         (OPEN-CALL KWRAP::CONSP '1 A3 (O0 A1))
  TAG::C_44
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_40)
  TAG::C_16
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_36
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A1))
  TAG::C_34
         (KCALL KEQUAL '2 A2 NIL)
  TAG::C_39
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_29)
  TAG::C_19
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_27
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_25
         (TAIL-CALL KEQUAL '2)
  TAG::C_29
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_40
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_45
         (MOVE RETURN 'NIL CH-RETURN))


(defun kassq (x alist)
  (do ((next alist (cdr next)))
      ((or (null next)
           (eq (caar next) x))
       (car next))))


(DEFKFUN KASSQ (X ALIST)
  TAG::KASSQ_8
         (MOVE A3 A1)
  TAG::DO1822_14
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_17)
  TAG::C_32
         (OPEN-CALL KWRAP::CAAR '1 A2 (O0 A3))
  TAG::C_36
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 A0 BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_17)
  TAG::C_20
         (OPEN-CALL KWRAP::CDR '1 A3 (O0 A3))
  TAG::C_24
         (JUMP TAG::DO1822_14)
  TAG::C_17
         (OPEN-TAIL-CALL KWRAP::CAR '1 (O0 A3)))


(defun kget2 (symbol prop)
  (do ((plist (symbol-plist symbol) (cddr plist)))
      ((null plist))
    (if (eq (car plist) prop)
        (return (cadr plist)))))


(DEFKFUN KGET2 (SYMBOL PROP)
  TAG::KGET2_7
         (OPEN-CALL KWRAP::SYMBOL-PLIST '1 A4 (O0 A0))
  TAG::C_49
  TAG::DO1856_14
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A4 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_17
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_18
         (OPEN-CALL KWRAP::CAR '1 A3 (O0 A4))
  TAG::C_32
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 A1 BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_28)
  TAG::C_21
         (OPEN-CALL KWRAP::CADR '1 A2 (O0 A4))    ;tail-open-call
  TAG::C_25
         (MOVE RETURN A2 CH-RETURN)
  TAG::C_28
         (OPEN-CALL KWRAP::CDDR '1 A4 (O0 A4))
  TAG::C_39
         (JUMP TAG::DO1856_14))


(defkfun set-symbol-plist (symbol plist)
  (setf (symbol-plist A0) A1)
  (incf *i-count*)
  (MOVE RETURN 'NIL CH-RETURN))

(defun ksetprop (symbol property value)
  (let ((plist (symbol-plist symbol)))
    (do ((plist-rest plist (cddr plist-rest)))
        ((null plist-rest)
         (set-symbol-plist symbol (cons property (cons value plist)))
         value)
      (when (eq (car plist-rest) property)
        (rplaca (cdr plist-rest) value)
        (return value)))))


(DEFKFUN KSETPROP (SYMBOL PROPERTY VALUE)
  TAG::KSETPROP_10
         (OPEN-CALL KWRAP::SYMBOL-PLIST '1 A5 (O0 A0))
  TAG::C_69
         (MOVE A4 A5)
  TAG::DO1878_18
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A4 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_33)
  TAG::C_21
         (KOPEN)
         (KOPEN)
         (MOVE O0 A2 CH-OPEN)
         (KCALL KWRAP::CONS '2 O1 (O1 A5))
  TAG::C_26
         (KCALL KWRAP::CONS '2 O1 (O0 A1))
  TAG::C_29
         (KCALL SET-SYMBOL-PLIST '2 IGNORE (O0 A0))
  TAG::B_32
         (MOVE RETURN A2 CH-RETURN)
  TAG::C_33
         (OPEN-CALL KWRAP::CAR '1 A3 (O0 A4))
  TAG::C_51
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 A1 BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_47)
  TAG::C_36
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A4))
  TAG::C_41
         (KCALL KWRAP:RPLACA '2 IGNORE (O1 A2))
  TAG::B_46
         (MOVE RETURN A2 CH-RETURN)
  TAG::C_47
         (OPEN-CALL KWRAP::CDDR '1 A4 (O0 A4))
  TAG::C_58
         (JUMP TAG::DO1878_18))



(defvar unify-subst)
(defvar temp-temp)

(defun add-lemma (term)
  (cond ((and (not (atom term))
              (eq (car term)
                  (quote equal))
              (not (atom (cadr term))))
         (setf (get (car (cadr term)) (quote lemmas))
               (cons term (get (car (cadr term))
                               (quote lemmas)))))
        (t (error "~%ADD-LEMMA did not like term:  ~a" term))))


(DEFKFUN ADD-LEMMA (TERM)
  TAG::ADD-LEMMA_2
         (OPEN-CALL Kwrap:ATOM '1 A3 (O0 A0))
  TAG::C_64
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_30)
  TAG::C_35
         (OPEN-CALL KWRAP::CAR '1 A2 (O0 A0))
  TAG::C_53
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'EQUAL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_30)
  TAG::C_38
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_46
         (KCALL KWRAP:ATOM '1 A1 NIL)
  TAG::C_49
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_30)
  TAG::C_5
         (TAIL-OPEN)
         (KOPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_17
         (KCALL KWRAP::CAR '1 O0 NIL)
  TAG::C_20
         (KCALL KGET2 '2 O1 (O1 'LEMMAS))
  TAG::C_23
         (KCALL KWRAP::CONS '2 O2 (O0 A0))
  TAG::C_28
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_11
         (KCALL KWRAP::CAR '1 O0 NIL)
  TAG::C_26
         (TAIL-CALL KSETPROP '3 (O1 'LEMMAS))
  TAG::C_30
         (MOVE O0 '"~%ADD-LEMMA did not like term:  ~a" CH-TAIL-OPEN)
         (TAIL-CALL ERROR '2 (O1 A0)))








(defun add-lemma-lst (lst)
  (cond ((null lst)
         t)
        (t (add-lemma (car lst))
           (add-lemma-lst (cdr lst)))))

(DEFKFUN ADD-LEMMA-LST (LST)
  TAG::ADD-LEMMA-LST_2
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A0 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_6)
  TAG::C_5
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_6
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_11
         (KCALL ADD-LEMMA '1 IGNORE NIL)
  TAG::B_20
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_17
         (TAIL-CALL ADD-LEMMA-LST '1))




(defun apply-subst (alist term)
  (cond ((atom term)
         (cond ((setq temp-temp (assq term alist))
                (cdr temp-temp))
               (t term)))
        (t (cons (car term)
                 (apply-subst-lst alist (cdr term))))))


(DEFKFUN APPLY-SUBST (ALIST TERM)
  TAG::APPLY-SUBST_3
         (OPEN-CALL KWRAP:ATOM '1 A3 (O0 A1))
  TAG::C_38
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_21)
  TAG::C_6
         (MOVE O0 A1 CH-OPEN)
         (KCALL KASSQ '2 A2 (O1 A0))
  TAG::C_18
         (MOVE (%VALUE-CELL TEMP-TEMP) A2)
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_14)
  TAG::C_9
         (OPEN-TAIL-CALL KWRAP::CDR '1 (O0 (%VALUE-CELL TEMP-TEMP)))
  TAG::C_14
         (MOVE RETURN A1 CH-RETURN)
  TAG::C_21
         (TAIL-OPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_28
         (KCALL APPLY-SUBST-LST '2 O1 (O0 A0))
  TAG::C_33
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A1))
  TAG::C_31
         (TAIL-CALL KWRAP::CONS '2))






(defun apply-subst-lst (alist lst)
  (cond ((null lst)
         nil)
        (t (cons (apply-subst alist (car lst))
                 (apply-subst-lst alist (cdr lst))))))


(DEFKFUN APPLY-SUBST-LST (ALIST LST)
  TAG::APPLY-SUBST-LST_3
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_7)
  TAG::C_6
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_7
         (TAIL-OPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A1))
  TAG::C_13
         (KCALL APPLY-SUBST '2 O0 (O0 A0))
  TAG::C_24
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_19
         (KCALL APPLY-SUBST-LST '2 O1 (O0 A0))
  TAG::C_22
         (TAIL-CALL KWRAP::CONS '2))


(defun falsep (x lst)
  (or (equal x (quote (f)))
      (member x lst)))


(DEFKFUN FALSEP (X LST)
  TAG::FALSEP_5
         (MOVE O0 A0 CH-OPEN)
         (KCALL KEQUAL '2 A2 (O1 '(F)))
  TAG::C_16
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_10)
  TAG::C_9
         (MOVE RETURN A2 CH-RETURN)
  TAG::C_10
         (MOVE O0 A0 CH-TAIL-OPEN)
         (TAIL-CALL KMEMBER '2 (O1 A1)))





(defun one-way-unify (term1 term2)
  (progn (setq unify-subst nil)
         (one-way-unify1 term1 term2)))


(DEFKFUN ONE-WAY-UNIFY (TERM1 TERM2)
  TAG::ONE-WAY-UNIFY_3
         (MOVE (%VALUE-CELL UNIFY-SUBST) 'NIL)
         (MOVE O0 A0 CH-TAIL-OPEN)
         (TAIL-CALL ONE-WAY-UNIFY1 '2 (O1 A1)))




(defun one-way-unify1 (term1 term2)
  (cond ((atom term2)
         (cond ((setq temp-temp (assq term2 unify-subst))
                (equal term1 (cdr temp-temp)))
               (t (setq unify-subst (cons (cons term2 term1)
                                          unify-subst))
                  t)))
        ((atom term1)
         nil)
        ((eq (car term1)
             (car term2))
         (one-way-unify1-lst (cdr term1)
                             (cdr term2)))
        (t nil)))


(DEFKFUN ONE-WAY-UNIFY1 (TERM1 TERM2)
  TAG::ONE-WAY-UNIFY1_3
         (OPEN-CALL KWRAP:ATOM '1 A7 (O0 A1))
  TAG::C_71
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A7 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_38)
  TAG::C_6
         (MOVE O1 (%VALUE-CELL UNIFY-SUBST) CH-OPEN)
         (KCALL KASSQ '2 A6 (O0 A1))
  TAG::C_35
         (MOVE (%VALUE-CELL TEMP-TEMP) A6)
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A6 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_9
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 (%VALUE-CELL TEMP-TEMP)))
  TAG::C_16
         (TAIL-CALL KEQUAL '2 (O0 A0))
  TAG::C_18
         (KOPEN)
         (MOVE O0 A1 CH-OPEN)
         (KCALL KWRAP::CONS '2 O0 (O1 A0))
  TAG::C_24
         (KCALL KWRAP::CONS '2 A5 (O1 (%VALUE-CELL UNIFY-SUBST)))
  TAG::C_27
         (MOVE (%VALUE-CELL UNIFY-SUBST) A5)
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_38
         (OPEN-CALL KWRAP:ATOM '1 A4 (O0 A0))
  TAG::C_68
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A4 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_42)
  TAG::C_41
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_42
         (OPEN-CALL KWRAP::CAR '1 A3 (O0 A0))
  TAG::C_61
         (OPEN-CALL KWRAP::CAR '1 A2 (O0 A1))
  TAG::C_59
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 A2 BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_54)
  TAG::C_45
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_52
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_50
         (TAIL-CALL ONE-WAY-UNIFY1-LST '2)
  TAG::C_54
         (MOVE RETURN 'NIL CH-RETURN))





(defun one-way-unify1-lst (lst1 lst2)
  (cond ((null lst1)
         t)
        ((one-way-unify1 (car lst1)
                         (car lst2))
         (one-way-unify1-lst (cdr lst1)
                             (cdr lst2)))
        (t nil)))


(DEFKFUN ONE-WAY-UNIFY1-LST (LST1 LST2)
  TAG::ONE-WAY-UNIFY1-LST_3
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A0 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_7)
  TAG::C_6
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_7
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_28
         (OPEN-CALL KWRAP::CAR '1 O1 (O0 A1))
  TAG::C_26
         (KCALL ONE-WAY-UNIFY1 '2 A2 NIL)
  TAG::C_31
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_20)
  TAG::C_10
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_18
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_16
         (TAIL-CALL ONE-WAY-UNIFY1-LST '2)
  TAG::C_20
         (MOVE RETURN 'NIL CH-RETURN))






(defun rewrite (term)
  (cond ((atom term)
         term)
        (t (rewrite-with-lemmas
             (cons (car term)
                   (rewrite-args (cdr term)))
             (get (car term)
                  (quote lemmas))))))


(DEFKFUN REWRITE (TERM)
  TAG::REWRITE_2
         (OPEN-CALL KWRAP:ATOM '1 A1 (O0 A0))
  TAG::C_35
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_6)
  TAG::C_5
         (MOVE RETURN A0 CH-RETURN)
  TAG::C_6
         (TAIL-OPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_15
         (KCALL REWRITE-ARGS '1 O1 NIL)
  TAG::C_20
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_18
         (KCALL KWRAP::CONS '2 O0 NIL)
  TAG::C_30
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_25
         (KCALL KGET2 '2 O1 (O1 'LEMMAS))
  TAG::C_28
         (TAIL-CALL REWRITE-WITH-LEMMAS '2))







(defun rewrite-args (lst)
  (cond ((null lst)
         nil)
        (t (cons (rewrite (car lst))
                 (rewrite-args (cdr lst))))))


(DEFKFUN REWRITE-ARGS (LST)
  TAG::REWRITE-ARGS_2
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A0 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_6)
  TAG::C_5
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_6
         (TAIL-OPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A0))
  TAG::C_12
         (KCALL REWRITE '1 O0 NIL)
  TAG::C_23
         (KOPEN)
         (OPEN-CALL KWRAP::CDR '1 O0 (O0 A0))
  TAG::C_18
         (KCALL REWRITE-ARGS '1 O1 NIL)
  TAG::C_21
         (TAIL-CALL KWRAP::CONS '2))

(defun rewrite-with-lemmas (term lst)
  (cond ((null lst)
         term)
        ((one-way-unify term (cadr (car lst)))
         (rewrite (apply-subst unify-subst (caddr (car lst)))))
        (t (rewrite-with-lemmas term (cdr lst)))))


(DEFKFUN REWRITE-WITH-LEMMAS (TERM LST)
  TAG::REWRITE-WITH-LEMMAS_3
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_7)
  TAG::C_6
         (MOVE RETURN A0 CH-RETURN)
  TAG::C_7
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A1))
  TAG::C_38
         (KCALL KWRAP::CADR '1 O1 NIL)
  TAG::C_41
         (KCALL ONE-WAY-UNIFY '2 A2 (O0 A0))
  TAG::C_44
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_27)
  TAG::C_10
         (TAIL-OPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A1))
  TAG::C_17
         (KCALL KWRAP::CADDR '1 O1 NIL)
  TAG::C_22
         (KCALL APPLY-SUBST '2 O0 (O0 (%VALUE-CELL UNIFY-SUBST)))
  TAG::C_25
         (TAIL-CALL REWRITE '1)
  TAG::C_27
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CDR '1 O1 (O0 A1))
  TAG::C_32
         (TAIL-CALL REWRITE-WITH-LEMMAS '2 (O0 A0)))

(defun setup ()
  (add-lemma-lst
    (quote ((equal (compile form)
                   (reverse (codegen (optimize form)
                                     (nil))))
            (equal (eqp x y)
                   (equal (fix x)
                          (fix y)))
            (equal (greaterp x y)
                   (lessp y x))
            (equal (lesseqp x y)
                   (not (lessp y x)))
            (equal (greatereqp x y)
                   (not (lessp x y)))
            (equal (boolean x)
                   (or (equal x (t))
                       (equal x (f))))
            (equal (iff x y)
                   (and (implies x y)
                        (implies y x)))
            (equal (even1 x)
                   (if (zerop x)
                       (t)
                       (odd (sub1 x))))
            (equal (countps- l pred)
                   (countps-loop l pred (zero)))
            (equal (fact- i)
                   (fact-loop i 1))
            (equal (reverse- x)
                   (reverse-loop x (nil)))
            (equal (divides x y)
                   (zerop (remainder y x)))
            (equal (assume-true var alist)
                   (cons (cons var (t))
                         alist))
            (equal (assume-false var alist)
                   (cons (cons var (f))
                         alist))
            (equal (tautology-checker x)
                   (tautologyp (normalize x)
                               (nil)))
            (equal (falsify x)
                   (falsify1 (normalize x)
                             (nil)))
            (equal (prime x)
                   (and (not (zerop x))
                        (not (equal x (add1 (zero))))
                        (prime1 x (sub1 x))))
            (equal (and p q)
                   (if p (if q (t)
                             (f))
                       (f)))
            (equal (or p q)
                   (if p (t)
                       (if q (t)
                           (f))
                       (f)))
            (equal (not p)
                   (if p (f)
                       (t)))
            (equal (implies p q)
                   (if p (if q (t)
                             (f))
                       (t)))
            (equal (fix x)
                   (if (numberp x)
                       x
                       (zero)))
            (equal (if (if a b c)
                       d e)
                   (if a (if b d e)
                       (if c d e)))
            (equal (zerop x)
                   (or (equal x (zero))
                       (not (numberp x))))
            (equal (plus (plus x y)
                         z)
                   (plus x (plus y z)))
            (equal (equal (plus a b)
                          (zero))
                   (and (zerop a)
                        (zerop b)))
            (equal (difference x x)
                   (zero))
            (equal (equal (plus a b)
                          (plus a c))
                   (equal (fix b)
                          (fix c)))
            (equal (equal (zero)
                          (difference x y))
                   (not (lessp y x)))
            (equal (equal x (difference x y))
                   (and (numberp x)
                        (or (equal x (zero))
                            (zerop y))))
            (equal (meaning (plus-tree (append x y))
                            a)
                   (plus (meaning (plus-tree x)
                                  a)
                         (meaning (plus-tree y)
                                  a)))
            (equal (meaning (plus-tree (plus-fringe x))
                            a)
                   (fix (meaning x a)))
            (equal (append (append x y)
                           z)
                   (append x (append y z)))
            (equal (reverse (append a b))
                   (append (reverse b)
                           (reverse a)))
            (equal (times x (plus y z))
                   (plus (times x y)
                         (times x z)))
            (equal (times (times x y)
                          z)
                   (times x (times y z)))
            (equal (equal (times x y)
                          (zero))
                   (or (zerop x)
                       (zerop y)))
            (equal (exec (append x y)
                         pds envrn)
                   (exec y (exec x pds envrn)
                         envrn))
            (equal (mc-flatten x y)
                   (append (flatten x)
                           y))
            (equal (member x (append a b))
                   (or (member x a)
                       (member x b)))
            (equal (member x (reverse y))
                   (member x y))
            (equal (length (reverse x))
                   (length x))
            (equal (member a (intersect b c))
                   (and (member a b)
                        (member a c)))
            (equal (nth (zero)
                        i)
                   (zero))
            (equal (exp i (plus j k))
                   (times (exp i j)
                          (exp i k)))
            (equal (exp i (times j k))
                   (exp (exp i j)
                        k))
            (equal (reverse-loop x y)
                   (append (reverse x)
                           y))
            (equal (reverse-loop x (nil))
                   (reverse x))
            (equal (count-list z (sort-lp x y))
                   (plus (count-list z x)
                         (count-list z y)))
            (equal (equal (append a b)
                          (append a c))
                   (equal b c))
            (equal (plus (remainder x y)
                         (times y (quotient x y)))
                   (fix x))
            (equal (power-eval (big-plus1 l i base)
                               base)
                   (plus (power-eval l base)
                         i))
            (equal (power-eval (big-plus x y i base)
                               base)
                   (plus i (plus (power-eval x base)
                                 (power-eval y base))))
            (equal (remainder y 1)
                   (zero))
            (equal (lessp (remainder x y)
                          y)
                   (not (zerop y)))
            (equal (remainder x x)
                   (zero))
            (equal (lessp (quotient i j)
                          i)
                   (and (not (zerop i))
                        (or (zerop j)
                            (not (equal j 1)))))
            (equal (lessp (remainder x y)
                          x)
                   (and (not (zerop y))
                        (not (zerop x))
                        (not (lessp x y))))
            (equal (power-eval (power-rep i base)
                               base)
                   (fix i))
            (equal (power-eval (big-plus (power-rep i base)
                                         (power-rep j base)
                                         (zero)
                                         base)
                               base)
                   (plus i j))
            (equal (gcd x y)
                   (gcd y x))
            (equal (nth (append a b)
                        i)
                   (append (nth a i)
                           (nth b (difference i (length a)))))
            (equal (difference (plus x y)
                               x)
                   (fix y))
            (equal (difference (plus y x)
                               x)
                   (fix y))
            (equal (difference (plus x y)
                               (plus x z))
                   (difference y z))
            (equal (times x (difference c w))
                   (difference (times c x)
                               (times w x)))
            (equal (remainder (times x z)
                              z)
                   (zero))
            (equal (difference (plus b (plus a c))
                               a)
                   (plus b c))
            (equal (difference (add1 (plus y z))
                               z)
                   (add1 y))
            (equal (lessp (plus x y)
                          (plus x z))
                   (lessp y z))
            (equal (lessp (times x z)
                          (times y z))
                   (and (not (zerop z))
                        (lessp x y)))
            (equal (lessp y (plus x y))
                   (not (zerop x)))
            (equal (gcd (times x z)
                        (times y z))
                   (times z (gcd x y)))
            (equal (value (normalize x)
                          a)
                   (value x a))
            (equal (equal (flatten x)
                          (cons y (nil)))
                   (and (nlistp x)
                        (equal x y)))
            (equal (listp (gopher x))
                   (listp x))
            (equal (samefringe x y)
                   (equal (flatten x)
                          (flatten y)))
            (equal (equal (greatest-factor x y)
                          (zero))
                   (and (or (zerop y)
                            (equal y 1))
                        (equal x (zero))))
            (equal (equal (greatest-factor x y)
                          1)
                   (equal x 1))
            (equal (numberp (greatest-factor x y))
                   (not (and (or (zerop y)
                                 (equal y 1))
                             (not (numberp x)))))
            (equal (times-list (append x y))
                   (times (times-list x)
                          (times-list y)))
            (equal (prime-list (append x y))
                   (and (prime-list x)
                        (prime-list y)))
            (equal (equal z (times w z))
                   (and (numberp z)
                        (or (equal z (zero))
                            (equal w 1))))
            (equal (greatereqpr x y)
                   (not (lessp x y)))
            (equal (equal x (times x y))
                   (or (equal x (zero))
                       (and (numberp x)
                            (equal y 1))))
            (equal (remainder (times y x)
                              y)
                   (zero))
            (equal (equal (times a b)
                          1)
                   (and (not (equal a (zero)))
                        (not (equal b (zero)))
                        (numberp a)
                        (numberp b)
                        (equal (sub1 a)
                               (zero))
                        (equal (sub1 b)
                               (zero))))
            (equal (lessp (length (delete x l))
                          (length l))
                   (member x l))
            (equal (sort2 (delete x l))
                   (delete x (sort2 l)))
            (equal (dsort x)
                   (sort2 x))
            (equal (length (cons x1
                                 (cons x2
                                       (cons x3 (cons x4
                                                      (cons x5
                                                            (cons x6 x7)))))))
                   (plus 6 (length x7)))
            (equal (difference (add1 (add1 x))
                               2)
                   (fix x))
            (equal (quotient (plus x (plus x y))
                             2)
                   (plus x (quotient y 2)))
            (equal (sigma (zero)
                          i)
                   (quotient (times i (add1 i))
                             2))
            (equal (plus x (add1 y))
                   (if (numberp y)
                       (add1 (plus x y))
                       (add1 x)))
            (equal (equal (difference x y)
                          (difference z y))
                   (if (lessp x y)
                       (not (lessp y z))
                       (if (lessp z y)
                           (not (lessp y x))
                           (equal (fix x)
                                  (fix z)))))
            (equal (meaning (plus-tree (delete x y))
                            a)
                   (if (member x y)
                       (difference (meaning (plus-tree y)
                                            a)
                                   (meaning x a))
                       (meaning (plus-tree y)
                                a)))
            (equal (times x (add1 y))
                   (if (numberp y)
                       (plus x (times x y))
                       (fix x)))
            (equal (nth (nil)
                        i)
                   (if (zerop i)
                       (nil)
                       (zero)))
            (equal (last (append a b))
                   (if (listp b)
                       (last b)
                       (if (listp a)
                           (cons (car (last a))
                                 b)
                           b)))
            (equal (equal (lessp x y)
                          z)
                   (if (lessp x y)
                       (equal t z)
                       (equal f z)))
            (equal (assignment x (append a b))
                   (if (assignedp x a)
                       (assignment x a)
                       (assignment x b)))
            (equal (car (gopher x))
                   (if (listp x)
                       (car (flatten x))
                       (zero)))
            (equal (flatten (cdr (gopher x)))
                   (if (listp x)
                       (cdr (flatten x))
                       (cons (zero)
                             (nil))))
            (equal (quotient (times y x)
                             y)
                   (if (zerop y)
                       (zero)
                       (fix x)))
            (equal (get j (set i val mem))
                   (if (eqp j i)
                       val
                       (get j mem)))))))


(DEFKFUN SETUP NIL
  TAG::SETUP_1
         (OPEN-TAIL-CALL ADD-LEMMA-LST
                         '1
                         (O0 '((EQUAL (COMPILE FORM)
                                      (REVERSE (CODEGEN (OPTIMIZE FORM) (NIL))))
                               (EQUAL (EQP X Y) (EQUAL (FIX X) (FIX Y)))
                               (EQUAL (GREATERP X Y) (LESSP Y X))
                               (EQUAL (LESSEQP X Y) (NOT (LESSP Y X)))
                               (EQUAL (GREATEREQP X Y) (NOT (LESSP X Y)))
                               (EQUAL (BOOLEAN X)
                                      (OR (EQUAL X (T))
                                          (EQUAL X (F))))
                               (EQUAL (IFF X Y)
                                      (AND (IMPLIES X Y)
                                           (IMPLIES Y X)))
                               (EQUAL (EVEN1 X) (IF (ZEROP X) (T) (ODD (SUB1 X))))
                               (EQUAL (COUNTPS- L PRED) (COUNTPS-LOOP L PRED (ZERO)))
                               (EQUAL (FACT- I) (FACT-LOOP I 1))
                               (EQUAL (REVERSE- X) (REVERSE-LOOP X (NIL)))
                               (EQUAL (DIVIDES X Y) (ZEROP (REMAINDER Y X)))
                               (EQUAL (ASSUME-TRUE VAR ALIST) (CONS (CONS VAR (T)) ALIST))
                               (EQUAL (ASSUME-FALSE VAR ALIST) (CONS (CONS VAR (F)) ALIST))
                               (EQUAL (TAUTOLOGY-CHECKER X)
                                      (TAUTOLOGYP (NORMALIZE X) (NIL)))
                               (EQUAL (FALSIFY X) (FALSIFY1 (NORMALIZE X) (NIL)))
                               (EQUAL (PRIME X)
                                      (AND (NOT (ZEROP X))
                                           (NOT (EQUAL X (ADD1 (ZERO))))
                                           (PRIME1 X (SUB1 X))))
                               (EQUAL (AND P Q)
                                      (IF P (IF Q (T) (F)) (F)))
                               (EQUAL (OR P Q)
                                      (IF P (T) (IF Q (T) (F)) (F)))
                               (EQUAL (NOT P) (IF P (F) (T)))
                               (EQUAL (IMPLIES P Q) (IF P (IF Q (T) (F)) (T)))
                               (EQUAL (FIX X) (IF (NUMBERP X) X (ZERO)))
                               (EQUAL (IF (IF A B C) D E) (IF A (IF B D E) (IF C D E)))
                               (EQUAL (ZEROP X)
                                      (OR (EQUAL X (ZERO))
                                          (NOT (NUMBERP X))))
                               (EQUAL (PLUS (PLUS X Y) Z) (PLUS X (PLUS Y Z)))
                               (EQUAL (EQUAL (PLUS A B) (ZERO))
                                      (AND (ZEROP A)
                                           (ZEROP B)))
                               (EQUAL (DIFFERENCE X X) (ZERO))
                               (EQUAL (EQUAL (PLUS A B) (PLUS A C)) (EQUAL (FIX B) (FIX C)))
                               (EQUAL (EQUAL (ZERO) (DIFFERENCE X Y)) (NOT (LESSP Y X)))
                               (EQUAL (EQUAL X (DIFFERENCE X Y))
                                      (AND (NUMBERP X)
                                           (OR (EQUAL X (ZERO))
                                               (ZEROP Y))))
                               (EQUAL (MEANING (PLUS-TREE (APPEND X Y)) A)
                                      (PLUS (MEANING (PLUS-TREE X) A)
                                            (MEANING (PLUS-TREE Y) A)))
                               (EQUAL (MEANING (PLUS-TREE (PLUS-FRINGE X)) A)
                                      (FIX (MEANING X A)))
                               (EQUAL (APPEND (APPEND X Y) Z) (APPEND X (APPEND Y Z)))
                               (EQUAL (REVERSE (APPEND A B))
                                      (APPEND (REVERSE B) (REVERSE A)))
                               (EQUAL (TIMES X (PLUS Y Z)) (PLUS (TIMES X Y) (TIMES X Z)))
                               (EQUAL (TIMES (TIMES X Y) Z) (TIMES X (TIMES Y Z)))
                               (EQUAL (EQUAL (TIMES X Y) (ZERO))
                                      (OR (ZEROP X)
                                          (ZEROP Y)))
                               (EQUAL (EXEC (APPEND X Y) PDS ENVRN)
                                      (EXEC Y (EXEC X PDS ENVRN) ENVRN))
                               (EQUAL (MC-FLATTEN X Y) (APPEND (FLATTEN X) Y))
                               (EQUAL (MEMBER X (APPEND A B))
                                      (OR (MEMBER X A)
                                          (MEMBER X B)))
                               (EQUAL (MEMBER X (REVERSE Y)) (MEMBER X Y))
                               (EQUAL (LENGTH (REVERSE X)) (LENGTH X))
                               (EQUAL (MEMBER A (INTERSECT B C))
                                      (AND (MEMBER A B)
                                           (MEMBER A C)))
                               (EQUAL (NTH (ZERO) I) (ZERO))
                               (EQUAL (EXP I (PLUS J K)) (TIMES (EXP I J) (EXP I K)))
                               (EQUAL (EXP I (TIMES J K)) (EXP (EXP I J) K))
                               (EQUAL (REVERSE-LOOP X Y) (APPEND (REVERSE X) Y))
                               (EQUAL (REVERSE-LOOP X (NIL)) (REVERSE X))
                               (EQUAL (COUNT-LIST Z (SORT-LP X Y))
                                      (PLUS (COUNT-LIST Z X) (COUNT-LIST Z Y)))
                               (EQUAL (EQUAL (APPEND A B) (APPEND A C)) (EQUAL B C))
                               (EQUAL (PLUS (REMAINDER X Y) (TIMES Y (QUOTIENT X Y)))
                                      (FIX X))
                               (EQUAL (POWER-EVAL (BIG-PLUS1 L I BASE) BASE)
                                      (PLUS (POWER-EVAL L BASE) I))
                               (EQUAL (POWER-EVAL (BIG-PLUS X Y I BASE) BASE)
                                      (PLUS I
                                            (PLUS (POWER-EVAL X BASE) (POWER-EVAL Y BASE))))
                               (EQUAL (REMAINDER Y 1) (ZERO))
                               (EQUAL (LESSP (REMAINDER X Y) Y) (NOT (ZEROP Y)))
                               (EQUAL (REMAINDER X X) (ZERO))
                               (EQUAL (LESSP (QUOTIENT I J) I)
                                      (AND (NOT (ZEROP I))
                                           (OR (ZEROP J)
                                               (NOT (EQUAL J 1)))))
                               (EQUAL (LESSP (REMAINDER X Y) X)
                                      (AND (NOT (ZEROP Y))
                                           (NOT (ZEROP X))
                                           (NOT (LESSP X Y))))
                               (EQUAL (POWER-EVAL (POWER-REP I BASE) BASE) (FIX I))
                               (EQUAL (POWER-EVAL (BIG-PLUS (POWER-REP I BASE)
                                                            (POWER-REP J BASE)
                                                            (ZERO)
                                                            BASE)
                                                  BASE)
                                      (PLUS I J))
                               (EQUAL (GCD X Y) (GCD Y X))
                               (EQUAL (NTH (APPEND A B) I)
                                      (APPEND (NTH A I) (NTH B (DIFFERENCE I (LENGTH A)))))
                               (EQUAL (DIFFERENCE (PLUS X Y) X) (FIX Y))
                               (EQUAL (DIFFERENCE (PLUS Y X) X) (FIX Y))
                               (EQUAL (DIFFERENCE (PLUS X Y) (PLUS X Z)) (DIFFERENCE Y Z))
                               (EQUAL (TIMES X (DIFFERENCE C W))
                                      (DIFFERENCE (TIMES C X) (TIMES W X)))
                               (EQUAL (REMAINDER (TIMES X Z) Z) (ZERO))
                               (EQUAL (DIFFERENCE (PLUS B (PLUS A C)) A) (PLUS B C))
                               (EQUAL (DIFFERENCE (ADD1 (PLUS Y Z)) Z) (ADD1 Y))
                               (EQUAL (LESSP (PLUS X Y) (PLUS X Z)) (LESSP Y Z))
                               (EQUAL (LESSP (TIMES X Z) (TIMES Y Z))
                                      (AND (NOT (ZEROP Z))
                                           (LESSP X Y)))
                               (EQUAL (LESSP Y (PLUS X Y)) (NOT (ZEROP X)))
                               (EQUAL (GCD (TIMES X Z) (TIMES Y Z)) (TIMES Z (GCD X Y)))
                               (EQUAL (VALUE (NORMALIZE X) A) (VALUE X A))
                               (EQUAL (EQUAL (FLATTEN X) (CONS Y (NIL)))
                                      (AND (NLISTP X)
                                           (EQUAL X Y)))
                               (EQUAL (LISTP (GOPHER X)) (LISTP X))
                               (EQUAL (SAMEFRINGE X Y) (EQUAL (FLATTEN X) (FLATTEN Y)))
                               (EQUAL (EQUAL (GREATEST-FACTOR X Y) (ZERO))
                                      (AND (OR (ZEROP Y)
                                               (EQUAL Y 1))
                                           (EQUAL X (ZERO))))
                               (EQUAL (EQUAL (GREATEST-FACTOR X Y) 1) (EQUAL X 1))
                               (EQUAL (NUMBERP (GREATEST-FACTOR X Y))
                                      (NOT (AND (OR (ZEROP Y)
                                                    (EQUAL Y 1))
                                                (NOT (NUMBERP X)))))
                               (EQUAL (TIMES-LIST (APPEND X Y))
                                      (TIMES (TIMES-LIST X) (TIMES-LIST Y)))
                               (EQUAL (PRIME-LIST (APPEND X Y))
                                      (AND (PRIME-LIST X)
                                           (PRIME-LIST Y)))
                               (EQUAL (EQUAL Z (TIMES W Z))
                                      (AND (NUMBERP Z)
                                           (OR (EQUAL Z (ZERO))
                                               (EQUAL W 1))))
                               (EQUAL (GREATEREQPR X Y) (NOT (LESSP X Y)))
                               (EQUAL (EQUAL X (TIMES X Y))
                                      (OR (EQUAL X (ZERO))
                                          (AND (NUMBERP X)
                                               (EQUAL Y 1))))
                               (EQUAL (REMAINDER (TIMES Y X) Y) (ZERO))
                               (EQUAL (EQUAL (TIMES A B) 1)
                                      (AND (NOT (EQUAL A (ZERO)))
                                           (NOT (EQUAL B (ZERO)))
                                           (NUMBERP A)
                                           (NUMBERP B)
                                           (EQUAL (SUB1 A) (ZERO))
                                           (EQUAL (SUB1 B) (ZERO))))
                               (EQUAL (LESSP (LENGTH (DELETE X L)) (LENGTH L)) (MEMBER X L))
                               (EQUAL (SORT2 (DELETE X L)) (DELETE X (SORT2 L)))
                               (EQUAL (DSORT X) (SORT2 X))
                               (EQUAL (LENGTH (CONS X1
                                                    (CONS X2
                                                          (CONS X3
                                                                (CONS X4
                                                                      (CONS X5
                                                                            (CONS X6
                                                                                  X7)))))))
                                      (PLUS 6 (LENGTH X7)))
                               (EQUAL (DIFFERENCE (ADD1 (ADD1 X)) 2) (FIX X))
                               (EQUAL (QUOTIENT (PLUS X (PLUS X Y)) 2)
                                      (PLUS X (QUOTIENT Y 2)))
                               (EQUAL (SIGMA (ZERO) I) (QUOTIENT (TIMES I (ADD1 I)) 2))
                               (EQUAL (PLUS X (ADD1 Y))
                                      (IF (NUMBERP Y) (ADD1 (PLUS X Y)) (ADD1 X)))
                               (EQUAL (EQUAL (DIFFERENCE X Y) (DIFFERENCE Z Y))
                                      (IF (LESSP X Y)
                                          (NOT (LESSP Y Z))
                                          (IF (LESSP Z Y)
                                              (NOT (LESSP Y X))
                                              (EQUAL (FIX X) (FIX Z)))))
                               (EQUAL (MEANING (PLUS-TREE (DELETE X Y)) A)
                                      (IF (MEMBER X Y)
                                          (DIFFERENCE (MEANING (PLUS-TREE Y) A)
                                                      (MEANING X A))
                                          (MEANING (PLUS-TREE Y) A)))
                               (EQUAL (TIMES X (ADD1 Y))
                                      (IF (NUMBERP Y) (PLUS X (TIMES X Y)) (FIX X)))
                               (EQUAL (NTH (NIL) I) (IF (ZEROP I) (NIL) (ZERO)))
                               (EQUAL (LAST (APPEND A B))
                                      (IF (LISTP B)
                                          (LAST B)
                                          (IF (LISTP A) (CONS (CAR (LAST A)) B) B)))
                               (EQUAL (EQUAL (LESSP X Y) Z)
                                      (IF (LESSP X Y) (EQUAL T Z) (EQUAL F Z)))
                               (EQUAL (ASSIGNMENT X (APPEND A B))
                                      (IF (ASSIGNEDP X A)
                                          (ASSIGNMENT X A)
                                          (ASSIGNMENT X B)))
                               (EQUAL (CAR (GOPHER X))
                                      (IF (LISTP X) (CAR (FLATTEN X)) (ZERO)))
                               (EQUAL (FLATTEN (CDR (GOPHER X)))
                                      (IF (LISTP X) (CDR (FLATTEN X)) (CONS (ZERO) (NIL))))
                               (EQUAL (QUOTIENT (TIMES Y X) Y)
                                      (IF (ZEROP Y) (ZERO) (FIX X)))
                               (EQUAL (GET J (SET I VAL MEM))
                                      (IF (EQP J I) VAL (GET J MEM)))))))

(defun tautologyp (x true-lst false-lst)
  (cond ((truep x true-lst)
         t)
        ((falsep x false-lst)
         nil)
        ((atom x)
         nil)
        ((eq (car x)
             (quote if))
         (cond ((truep (cadr x)
                       true-lst)
                (tautologyp (caddr x)
                            true-lst false-lst))
               ((falsep (cadr x)
                        false-lst)
                (tautologyp (cadddr x)
                            true-lst false-lst))
               (t (and (tautologyp (caddr x)
                                   (cons (cadr x)
                                         true-lst)
                                   false-lst)
                       (tautologyp (cadddr x)
                                   true-lst
                                   (cons (cadr x)
                                         false-lst))))))
        (t nil)))


(DEFKFUN TAUTOLOGYP (X TRUE-LST FALSE-LST)
  TAG::TAUTOLOGYP_4
         (MOVE O0 A0 CH-OPEN)
         (KCALL TRUEP '2 A9 (O1 A1))
  TAG::C_98
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A9 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_8)
  TAG::C_7
         (MOVE RETURN 'T CH-RETURN)
  TAG::C_8
         (MOVE O0 A0 CH-OPEN)
         (KCALL FALSEP '2 A8 (O1 A2))
  TAG::C_95
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A8 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_12)
  TAG::C_11
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_12
         (OPEN-CALL KWRAP:ATOM '1 A7 (O0 A0))
  TAG::C_92
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A7 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_16)
  TAG::C_15
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_16
         (OPEN-CALL KWRAP::CAR '1 A6 (O0 A0))
  TAG::C_85
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A6 'IF BW-32)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_81)
  TAG::C_19
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_77
         (KCALL TRUEP '2 A5 (O1 A1))
  TAG::C_80
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A5 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_29)
  TAG::C_22
         (TAIL-OPEN)
         (OPEN-CALL KWRAP::CADDR '1 O0 (O0 A0))
  TAG::C_27
         (MOVE O1 A1)
         (TAIL-CALL TAUTOLOGYP '3 (O2 A2))
  TAG::C_29
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_70
         (KCALL FALSEP '2 A4 (O1 A2))
  TAG::C_73
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A4 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_38)
  TAG::C_32
         (TAIL-OPEN)
         (OPEN-CALL KWRAP:CADDDR '1 O0 (O0 A0))
  TAG::C_36
         (MOVE O1 A1)
         (TAIL-CALL TAUTOLOGYP '3 (O2 A2))
  TAG::C_38
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_58
         (KCALL KWRAP::CONS '2 O1 (O1 A1))
  TAG::C_63
         (OPEN-CALL KWRAP::CADDR '1 O0 (O0 A0))
  TAG::C_61
         (KCALL TAUTOLOGYP '3 A3 (O2 A2))
  TAG::C_66
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A3 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_54)
  TAG::C_41
         (TAIL-OPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CADR '1 O0 (O0 A0))
  TAG::C_47
         (KCALL KWRAP::CONS '2 O2 (O1 A2))
  TAG::C_52
         (OPEN-CALL KWRAP:CADDDR '1 O0 (O0 A0))
  TAG::C_50
         (TAIL-CALL TAUTOLOGYP '3 (O1 A1))
  TAG::C_54
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_81
         (MOVE RETURN 'NIL CH-RETURN))


(defun tautp (x)
  (tautologyp (rewrite x)
              nil nil))


(DEFKFUN TAUTP (X)
  TAG::TAUTP_2
         (TAIL-OPEN)
         (OPEN-CALL REWRITE '1 O0 (O0 A0))
  TAG::C_7
         (MOVE O1 'NIL)
         (TAIL-CALL TAUTOLOGYP '3 (O2 'NIL)))



(defun test nil
  (prog (ans term)
        (setq term
              (apply-subst
                (quote ((x f (plus (plus a b)
                                   (plus c (zero))))
                        (y f (times (times a b)
                                    (plus c d)))
                        (z f (reverse (append (append a b)
                                              (nil))))
                        (u equal (plus a b)
                           (difference x y))
                        (w lessp (remainder a b)
                           (member a (length b)))))
                (quote (implies (and (implies x y)
                                     (and (implies y z)
                                          (and (implies z u)
                                               (implies u w))))
                                (implies x w)))))
        (setq ans (tautp term))))


(DEFKFUN boyer:TEST NIL
  TAG::TEST_4
         (MOVE A0 'NIL)
         (MOVE A1 'NIL)
         (MOVE O0 '((X F (PLUS (PLUS A B) (PLUS C (ZERO))))
                    (Y F (TIMES (TIMES A B) (PLUS C D)))
                    (Z F (REVERSE (APPEND (APPEND A B) (NIL))))
                    (U EQUAL (PLUS A B) (DIFFERENCE X Y))
                    (W LESSP (REMAINDER A B) (MEMBER A (LENGTH B)))) CH-OPEN)
         (KCALL APPLY-SUBST '2 A0 (O1 '(IMPLIES
                                        (AND (IMPLIES X Y)
                                             (AND (IMPLIES Y Z)
                                                  (AND (IMPLIES Z U)
                                                       (IMPLIES U W))))
                                        (IMPLIES X W))))
  TAG::C_9
         (OPEN-CALL TAUTP '1 A1 (O0 A0))
  TAG::C_13
         (MOVE RETURN A1 CH-RETURN))


(defun trans-of-implies (n)
  (list (quote implies)
        (trans-of-implies1 n)
        (list (quote implies)
              0 n)))


(DEFKFUN TRANS-OF-IMPLIES (N)
  TAG::TRANS-OF-IMPLIES_2
         (TAIL-OPEN)
         (OPEN-CALL TRANS-OF-IMPLIES1 '1 O1 (O0 A0))
  TAG::C_10
         (MOVE O0 'IMPLIES CH-OPEN)
         (MOVE O1 '0)
         (KCALL KWRAP::LIST3 '3 O2 (O2 A0))
  TAG::C_8
         (TAIL-CALL KWRAP::LIST3 '3 (O0 'IMPLIES)))


(defun trans-of-implies1 (n)
  (cond ((equal n 1)
         (list (quote implies)
               0 1))
        (t (list (quote and)
                 (list (quote implies)
                       (1- n)
                       n)
                 (trans-of-implies1 (1- n))))))


(DEFKFUN TRANS-OF-IMPLIES1 (N)
  TAG::TRANS-OF-IMPLIES1_2
         (MOVE O0 A0 CH-OPEN)
         (KCALL KEQUAL '2 A1 (O1 '1))
  TAG::C_26
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_8)
  TAG::C_5
         (MOVE O0 'IMPLIES CH-TAIL-OPEN)
         (MOVE O1 '0)
         (TAIL-CALL KWRAP::LIST3 '3 (O2 '1))
  TAG::C_8
         (TAIL-OPEN)
         (ALU L+R-1 O1 A0 '0 BW-24 CH-OPEN)
         (MOVE O0 'IMPLIES)
         (KCALL KWRAP::LIST3 '3 O1 (O2 A0))
  TAG::C_21
         (ALU L+R-1 O0 A0 '0 BW-24 CH-OPEN)
         (KCALL TRANS-OF-IMPLIES1 '1 O2 NIL)
  TAG::C_19
         (TAIL-CALL KWRAP::LIST3 '3 (O0 'AND)))



(defun truep (x lst)
  (or (equal x (quote (t)))
      (member x lst)))


(DEFKFUN TRUEP (X LST)
  TAG::TRUEP_5
         (MOVE O0 A0 CH-OPEN)
         (KCALL KEQUAL '2 A2 (O1 '(T)))
  TAG::C_16
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A2 'NIL BW-32)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_10)
  TAG::C_9
         (MOVE RETURN A2 CH-RETURN)
  TAG::C_10
         (MOVE O0 A0 CH-TAIL-OPEN)
         (TAIL-CALL KMEMBER '2 (O1 A1)))


;;; (SETUP) (TEST)
