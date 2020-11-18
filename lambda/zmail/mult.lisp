;;; Some menus used by ZMail -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;; This is SYS: ZMAIL; MULT
;;; This file defines various kinds of multiple choice menus, which are documented in the
;;;  Window System Manual.
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

(DEFFLAVOR MULTIPLE-MENU-CHOOSE-MENU-MIXIN () (MENU-HIGHLIGHTING-MIXIN)
  (:DEFAULT-INIT-PLIST :FONT-MAP '(FONTS:MEDFNT FONTS:HL12I))
  (:DOCUMENTATION :MIXIN "A special menu that allows multiple selections, one from each
of several columns."))

(DEFMETHOD (MULTIPLE-MENU-CHOOSE-MENU-MIXIN :MULTIPLE-CHOOSE) (ITEMS DEFAULTS
                                                         &OPTIONAL (NEAR-MODE '(:MOUSE))
                                                         &AUX OLD-STATUS)
  (SEND SELF :SET-GEOMETRY (MAX 2 (LENGTH ITEMS)))
  (DO ((I 0 (1+ I))
       (MAXL (DO ((L ITEMS (CDR L))
                  (N 0))
                 ((NULL L) N)
               (SETQ N (MAX N (LENGTH (CAR L))))))
       (IL NIL))
      (( I MAXL)
       (SEND SELF :SET-ITEM-LIST (APPEND (NREVERSE IL)
                                             '(("Do It" :VALUE DONE :FONT FONTS:HL12I
                                                :DOCUMENTATION
                                                "Use these highlighted choices.")
                                               ("Abort" :VALUE ABORT :FONT FONTS:HL12I
                                                :DOCUMENTATION "Abort this command.")))))
    (DO ((L ITEMS (CDR L)))
        ((NULL L))
      (PUSH (OR (NTH I (CAR L)) '("" :NO-SELECT T)) IL)))
  (SEND SELF :SET-HIGHLIGHTED-ITEMS (COPYLIST DEFAULTS))
  (SETQ OLD-STATUS (SEND SELF :STATUS))
  (EXPOSE-WINDOW-NEAR SELF NEAR-MODE)
  (SETQ DEFAULTS (COPYLIST DEFAULTS))           ;We will destroy it
  (UNWIND-PROTECT
    (DO ((RES) (L) (I) (X))
        (NIL)
      (SETQ RES (SEND SELF :CHOOSE))
      (COND ((EQ RES 'ABORT) (RETURN NIL))
            ((EQ RES 'DONE) (RETURN DEFAULTS))
            (T
             (DO ((L1 ITEMS (CDR L1))           ;Find the item
                  (I1 0 (1+ I1)))
                 ((NULL L1))
               (AND (MEMQ LAST-ITEM (CAR L1))
                    (RETURN (SETQ L L1 I I1))))
             (SETQ X (NTH I DEFAULTS))
             (IF (EQ LAST-ITEM X)       ;Already selected
                 (BEEP)
                 (SEND SELF :REMOVE-HIGHLIGHTED-ITEM X) ;Erase old
                 (SETF (NTH I DEFAULTS) LAST-ITEM)
                 (SEND SELF :ADD-HIGHLIGHTED-ITEM LAST-ITEM)))))
    (SEND SELF :SET-STATUS OLD-STATUS)))

(DEFFLAVOR MULTIPLE-MENU-CHOOSE-MENU () (MULTIPLE-MENU-CHOOSE-MENU-MIXIN MENU))

(DEFFLAVOR POP-UP-MULTIPLE-MENU-CHOOSE-MENU ()
           (MULTIPLE-MENU-CHOOSE-MENU-MIXIN TEMPORARY-MENU))

(COMPILE-FLAVOR-METHODS  POP-UP-MULTIPLE-MENU-CHOOSE-MENU)

(DEFWINDOW-RESOURCE POP-UP-MULTIPLE-MENU-CHOOSE-MENU-RESOURCE ()
        :MAKE-WINDOW (POP-UP-MULTIPLE-MENU-CHOOSE-MENU)
        :REUSABLE-WHEN :DEACTIVATED)

(DEFUN MULTIPLE-COLUMN-MENU-CHOOSE (ITEMS DEFAULTS &OPTIONAL (NEAR-MODE '(:MOUSE))
                                            &AUX (SUPERIOR MOUSE-SHEET))
  (AND (EQ (CAR NEAR-MODE) :WINDOW) (SETQ SUPERIOR (SHEET-SUPERIOR (CADR NEAR-MODE))))
  (USING-RESOURCE (MENU POP-UP-MULTIPLE-MENU-CHOOSE-MENU-RESOURCE SUPERIOR)
    (SEND MENU :MULTIPLE-CHOOSE ITEMS DEFAULTS NEAR-MODE)))

(DEFUN DEFAULTED-MULTIPLE-MENU-CHOOSE (ALIST DEFAULTS &OPTIONAL (NEAR-MODE '(:MOUSE)))
  (DO ((L1 ALIST (CDR L1))
       (L2 DEFAULTS (CDR L2))
       (L3 NIL))
      ((NULL L2)
       (SETQ DEFAULTS (NREVERSE L3)))
    (PUSH (DO ((ITEMS (CAR L1) (CDR ITEMS))
               (DEFAULT (CAR L2)))
              ((NULL ITEMS))
            (AND (EQUAL DEFAULT (MENU-EXECUTE-NO-SIDE-EFFECTS (CAR ITEMS)))
                 (RETURN (CAR ITEMS))))
          L3))
  (SETQ DEFAULTS (MULTIPLE-COLUMN-MENU-CHOOSE ALIST DEFAULTS NEAR-MODE))
  (DO L DEFAULTS (CDR L) (NULL L)
    (SETF (CAR L) (MENU-EXECUTE-NO-SIDE-EFFECTS (CAR L))))
  DEFAULTS)

(DEFFLAVOR MULTIPLE-ITEM-LIST-MENU-MIXIN ((ITEM-LISTS NIL)) ()
  (:REQUIRED-FLAVORS BASIC-MENU))

(DEFMETHOD (MULTIPLE-ITEM-LIST-MENU-MIXIN :SET-ITEM-LISTS) (&REST NEW-ITEM-LISTS)
  (COND ((NOT (EQUAL NEW-ITEM-LISTS ITEM-LISTS))
         (SETQ ITEM-LISTS (COPYLIST NEW-ITEM-LISTS))
         (DO ((I 0)
              (L NEW-ITEM-LISTS (CDR L)))
             ((NULL L)
              (SEND SELF :SET-GEOMETRY I))
           (AND (CAR L) (SETQ I (1+ I))))
         (SEND SELF :SET-ITEM-LIST (APPLY #'APPEND-ITEM-LISTS ITEM-LISTS))))
  ITEM-LISTS)

(DEFUN APPEND-ITEM-LISTS (&REST ITEM-LISTS &AUX (N 0) LIST)
  (DOLIST (L ITEM-LISTS)
    (AND L (SETQ LIST L N (1+ N))))
  (IF ( N 1) LIST                              ;Only one non-NIL will do
      (DO L ITEM-LISTS (CDR L) (NULL L)
        (OR (CAR L) (SETF (CAR L) T)))
      (SETQ LIST NIL)
      (DO ()
          ((NOT (DO L ITEM-LISTS (CDR L) (NULL L)
                  (AND (LISTP (CAR L)) (RETURN T))))
           (NREVERSE LIST))
        (DO L ITEM-LISTS (CDR L) (NULL L)
          (COND ((EQ (CAR L) T))
                ((NULL (CAR L))
                 (PUSH '("" :NO-SELECT T) LIST))
                (T
                 (PUSH (CAAR L) LIST)
                 (SETF (CAR L) (CDAR L))))))))

(DEFFLAVOR MOMENTARY-MULTIPLE-ITEM-LIST-MENU () (MULTIPLE-ITEM-LIST-MENU-MIXIN
                                                 BASIC-MOMENTARY-MENU
                                                 TEMPORARY-WINDOW-MIXIN
                                                 BORDERS-MIXIN TOP-BOX-LABEL-MIXIN
                                                 BASIC-SCROLL-BAR MINIMUM-WINDOW))

(COMPILE-FLAVOR-METHODS MOMENTARY-MULTIPLE-ITEM-LIST-MENU)
