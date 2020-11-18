#| -*- Mode:LISP; Package:COMPILER; Base:10; Fonts:(CPTFONTB) -*-

  Copyright LISP Machine, Inc. 1985,1986
   See filename "Copyright.Text" for
  licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************

  This is how one might want to store lisp forms in an array for later
  putting in some database. for MCC sometime in 1985 -GJC

Example use:

(SETQ A (MAKE-ARRAY-FOR-DUMPING-EXAMPLE 100))

(DUMP-FORMS-TO-ARRAY A '((print "what a hack")))

(FASLOAD-FROM-ARRAY A)

|#

(DEFUN MAKE-STREAM-TO-ARRAY (ARRAY &AUX STREAM)
  (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYO
                       (VECTOR-PUSH-EXTEND ARG1 ARRAY))
                     (:STRING-OUT
                       (DO ((J (OR (CAR ARGS) 0) (1+ J))
                            (END (OR (CADR ARGS) (LENGTH ARG1))))
                           ((= J END))
                         (VECTOR-PUSH-EXTEND (AREF ARG1 J) ARRAY)))
                     (T
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))


(DEFUN MAKE-STREAM-FROM-ARRAY (ARRAY &AUX STREAM (INDEX 0) PLIST)
    (SETQ STREAM #'(LAMBDA (OP &OPTIONAL ARG1 &REST ARGS)
                   (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                     (:TYI
                       (IF (= INDEX (LENGTH ARRAY))
                           NIL
                         (AREF ARRAY (PROG1 INDEX (INCF INDEX)))))
                     (:UNTYI
                       (DECF INDEX))
                     (:GENERIC-PATHNAME
                       #'(LAMBDA (OP &REST ARGS)
                           (SI:SELECTQ-WITH-WHICH-OPERATIONS OP
                             (:PUTPROP
                               (SETF (GETF PLIST (CADR ARGS)) (CAR ARGS)))
                             (:PLIST
                               PLIST)
                             (:REMPROP
                               (REMF PLIST (CAR ARGS)))
                             (:GET
                               (GETF PLIST (CAR ARGS))))))
                     (:INFO NIL)
                     (:PATHNAME "an array")
                     (T
                       (STREAM-DEFAULT-HANDLER STREAM OP ARG1 ARGS))))))

(DEFUN DUMP-FORMS-TO-ARRAY (ARRAY FORMS-LIST &OPTIONAL ATTRIBUTE-LIST)
  (LET ((FASD-STREAM (MAKE-STREAM-TO-ARRAY ARRAY)))
    (LET ((FASD-PACKAGE NIL))                   ;in case fasd-attributes-list bashes it
      (LOCKING-RESOURCES
        (FASD-INITIALIZE)
        (FASD-START-FILE)
        (FASD-ATTRIBUTES-LIST
          (IF (GETL (LOCF ATTRIBUTE-LIST) '(:PACKAGE))
              ATTRIBUTE-LIST
            (LIST* ':PACKAGE ':USER ATTRIBUTE-LIST)))
        (DOLIST (FORM FORMS-LIST)
          (IF ( (FASD-TABLE-LENGTH) QC-FILE-WHACK-THRESHOLD)
              (FASD-END-WHACK))
          (FASD-FORM FORM))
        (FASD-END-WHACK)
        (FASD-END-FILE)))))


(DEFUN MAKE-ARRAY-FOR-DUMPING-EXAMPLE (size)
  (make-array size :fill-pointer 0 :adjustable t
              :type 'art-16b))

(DEFUN FASLOAD-FROM-ARRAY (ARRAY &OPTIONAL PKG NO-MSG-P)
  (SI:FASLOAD-INTERNAL (MAKE-STREAM-FROM-ARRAY ARRAY) PKG NO-MSG-P))
