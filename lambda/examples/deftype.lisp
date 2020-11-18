;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Fonts:(CPTFONTB); Readtable:CL; Base:10 -*-

;;; Example hairy usage of Defstruct-Define-Type.

;;; Copyright LISP Machine, Inc. 1986
;;;   See filename "Copyright.Text" for
;;; licensing and release information.

;;; *********************************************************
;;; *********************************************************
;;; *** NOTE: This is an EXAMPLE, not LMI supported code. ***
;;; *** information contained in this example is subject  ***
;;; *** to change without notice. The ways of doing       ***
;;; *** the things contained in the example may change    ***
;;; *** between system releases. Some techniques which    ***
;;; *** are mere examples in one release may become built ***
;;; *** in system features in the next release. Use good  ***
;;; *** judgement when copying these techniques. Most     ***
;;; *** examples have been motivated by specific customer ***
;;; *** requests, and may not be the best engineered      ***
;;; *** or most efficient solution for someone else.      ***
;;; *********************************************************
;;; *********************************************************

;;; Actually the usual "examples" caveat may be too strongly worded
;;; here. defstruct-define-type is a documented feature
;;; and as is the :print-self message. The thing to look for though
;;; is the :instance type becoming a built in feature in future release.
;;; 13-Aug-86 10:34:21 -GJC

(defstruct-define-type :instance
  :named
  (:defstruct-keywords :ordered :VOID-DEFAULTS :make-instance)
  (:cons (init desc kwds) :list
         desc kwds
         `(%make-instance ',(defstruct-description-name desc) ,@init))
  (:ref (slot-number desc obj)
        (cond ((cdr (assq :ordered (defstruct-description-property-alist desc)))
               `(%instance-ref ,obj ,(1+ slot-number)))
              ('else
               `(symeval-in-instance ,obj
                                     ',(caar (mem #'(lambda (n e)
                                                      (= n (defstruct-slot-description-number (cdr e))))
                                                  slot-number
                                                  (defstruct-description-slot-alist desc)))))))
  (:PREDICATE (desc name)
              `(defun ,name (obj)
                 (typep obj ',(defstruct-description-name desc))))
  (:defstruct (desc)
              (DEFSTRUCT-INSTANCE-TYPE-DEFSTRUCT DESC)))

(DEFUN DEFSTRUCT-INSTANCE-TYPE-DEFSTRUCT (DESCRIPTION)
  ;; THIS CODE GETS CALLED FIRST WHEN A DEFSTRUCT DEFINITION IS MADE.
  ;; THEREFORE WE CAN OVERRIDE CERTAIN FEATURES AND IMPLEMENT THEM
  ;; OURSELVES.
  `((defflavor ,(defstruct-description-name)
                         ,(mapcar #'(lambda (slot)
                                      (let ((code (defstruct-slot-description-init-code (cdr slot))))
                                        ;; question of init value. add an option later
                                        (cond ((NOT (defstruct-emptyp code))
                                               (list (car slot) code))
                                              ((cdr (assq :VOID-DEFAULTS
                                                          (defstruct-description-property-alist)))
                                               (CAR SLOT))
                                              ('ELSE
                                               (list (car slot) nil)))))
                                  (defstruct-description-slot-alist))
                         ,(defstruct-description-include)
      :INITABLE-INSTANCE-VARIABLES
      ,@(if (cdr (assq :ordered (defstruct-description-property-alist)))
            '(:ordered-instance-variables)))
    ,(LET ((P (DEFSTRUCT-DESCRIPTION-PRINT)))
       (COND ((NOT P)
              `(DEFMETHOD (,(defstruct-description-name) :PRINT-SELF) (STREAM DEPTH ESCAPE-P)
                 DEPTH ESCAPE-P
                 (PRINT-INSTANCE-AS-#S SELF STREAM)))
             ('ELSE
              (SETF (DEFSTRUCT-DESCRIPTION-PRINT) NIL)
              (COND ((CAR P)
                     ;; :PRINT-FUNCTION
                     `(DEFMETHOD (,(defstruct-description-name) :PRINT-SELF) (STREAM DEPTH ESCAPE-P)
                        ESCAPE-P
                        (IF PRINT-READABLY
                            (PRINT-NOT-READABLE SELF)
                          (FUNCALL ',(CADR P) SELF STREAM DEPTH))))
                    ('ELSE
                     `(DEFMETHOD (,(defstruct-description-name) :PRINT-SELF) (STREAM DEPTH ESCAPE-P)
                        DEPTH ESCAPE-P
                        (IF PRINT-READABLY
                            (PRINT-NOT-READABLE SELF)
                          (LET ((,(defstruct-description-name) SELF))
                            (FORMAT STREAM ,@(CDR P))))))))))
    ,@(LET ((DEFAULT-CS (AND (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS)
                             (cdr (assq :make-instance (defstruct-description-property-alist)))
                             (SUBSET #'(LAMBDA (CS) (NULL (CDR CS)))
                                     (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS)))))
        (WHEN DEFAULT-CS
          (SETF (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS)
                (SET-DIFFERENCE (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS) DEFAULT-CS))
          (CONS
            ;; PUT'EM BACK AT RUNTIME FOR #S() IN READ. ETC.
            `(SETF (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
                     (GET-DEFSTRUCT-DESCRIPTION ',(DEFSTRUCT-DESCRIPTION-NAME)))
                   (APPEND ',DEFAULT-CS
                           (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS
                             (GET-DEFSTRUCT-DESCRIPTION ',(DEFSTRUCT-DESCRIPTION-NAME)))))
            (MAPCAR #'(LAMBDA (CS)
                        `(DEFUN ,(CAR CS) (&REST L)
                           (DECLARE (ARGLIST &KEY ,@(mapcar #'car (defstruct-description-slot-alist))))
                           (APPLY #'MAKE-INSTANCE ',(DEFSTRUCT-DESCRIPTION-NAME) L)))
                    DEFAULT-CS))))))


(DEFUN PRINT-INSTANCE-AS-#S (OBJECT STREAM)
  (FORMAT STREAM "#S(~S" (TYPE-OF OBJECT))
  (DO ((IVARS (FLAVOR-ALL-INSTANCE-VARIABLES (INSTANCE-FLAVOR OBJECT))
              (CDR IVARS))
       (I 1 (1+ I)))
      ((NULL IVARS))
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE OBJECT I) DTP-NULL))
          ('ELSE
           (FORMAT STREAM " ~S ~S" (CAR IVARS) (%INSTANCE-REF OBJECT I)))))
  (FORMAT STREAM ")"))
