;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:CL; Base:10 -*-

;;;; maclisp braindamage --- very obsolete array-hackery

;;;; NOTE: Things like GET-LOCATIVE-POINTER-INTO-ARRAY are not in MACLISP.
;;;;       The person who wrote this stuff obviously doesnt know what he
;;;;       is talking about. (Listen up MLY!) Stupidly I have fixed stuff up
;;;;       that I needed for Macsyma, instead of shadowing and implementing a private
;;;;       copy of the winning (for the purpose) code.

;; fillarray, listarray should also be here -- except that the system uses them. FMH!

(defmacro array (x type &rest dimlist)
  "Obsolete Maclisp function for creating an array.  Don't use it."
  `(*array ',x ',type ,@dimlist))
(make-obsolete array "use MAKE-ARRAY")

(defun *array (x type &rest dimlist &aux array)
  "Obsolete Maclisp function for making an array.  Don't use it."
  (cond ((memq type '(readtable obarray))
         (ferror "The array type ~S is not defined in Zetalisp" type))
        (t
         (setq array
               (zl:make-array dimlist :type (if (eq type 'flonum) 'art-float 'art-q)))
         (if (eq type 'fixnum)
             (fill-array array nil 0))
         (cond ((null x)
                array)
               ((symbolp x)
                (setf (symbol-function x) array)
                x)
               (t
                (ferror "~S is not a legal first arg for ~S"
                        x 'array))))))
(make-obsolete *array "use MAKE-ARRAY")


(defmacro get-locative-pointer-into-array (array-reference &environment env)
  "Similar to GET-LIST-POINTER-INTO-ARRAY except that it returns a
locative and doesn't require the array to be ART-Q-LIST.
Use LOCF of AREF instead of this in new programs."
  (let* ((arraycall (macroexpand array-reference env)))
    (case (car arraycall)
      (funcall `(locf (aref ,(cadr arraycall) . ,(cddr arraycall))))
      (arraycall `(locf (aref ,(caddr arraycall) . ,(cdddr arraycall))))
      ((apply funcall* apply)
       `(apply #'aloc ,(cadr arraycall) . ,(cddr arraycall)))
      (t `(locf (aref (symbol-function ,(car arraycall)) . ,(cdr arraycall)))))))
(make-obsolete get-locative-pointer-into-array "use LOCF and AREF instead")


(defmacro get-list-pointer-into-array (array-reference-form &environment env)
  "A Maclisp-compatibility function that returns a list pointer to part of an ART-Q-LIST
array.  This should not be used in new programs, use G-L-P and list decomposition
operations instead.  This function is not fully Maclisp-compatible in that the
array-reference form determines the starting element, not the last element referenced
by array invocation."
  (let ((reference (macroexpand array-reference-form env)))
    (case (car reference)
      (funcall
       `(make-list-pointer-to-array-element ,(cadr reference) ,@(cddr reference)))
      ((apply lexpr-funcall)
       `(apply #'make-list-pointer-to-array-element ,(cadr reference) ,@(cddr reference)))
      (otherwise
       `(make-list-pointer-to-array-element (function ,(car reference)) ,@(cdr reference))))))
(make-obsolete get-list-pointer-into-array "use G-L-P and list operations instead")

(defun make-list-pointer-to-array-element (&rest args)
  "An auxiliary routine for GET-LIST-POINTER-INTO-ARRAY. Like ALOC but returns a list pointer."
  (declare (arglist array &rest subscripts))
  (%make-pointer dtp-list (apply #'aloc args)))

(defmacro arraycall (ignore array &rest dims)
  `(funcall ,array . ,dims))

(defun arraydims (array &aux type)
  "Return a list of the array-type and dimensions of ARRAY.
This is an obsolete Maclisp function."
  (and (symbolp array) (setq array (symbol-function array)))
  (check-type array array)
        ;SHOULD CHECK FOR INVZ
  (setq type (nth (%p-ldb-offset %%array-type-field array 0) array-types))
  (cons type (array-dimensions array)))


;;;; Store
;;; Copyright (c) Jan 1984 by Glenn S. Burke and Massachusetts Institute of Technology.
;;; GSB's new version of STORE in NIL is not maclisp compatible.
;;; So dont inflict it on the lispmachine. I have put back my correct maclisp
;;; compatible version.


;(DEFMACRO STORE (ARRAY-REFERENCE VALUE)
;  (LET* ((ARRAYCALL (MACROEXPAND ARRAY-REFERENCE)))
;    (CASE (CAR ARRAYCALL)
;      (FUNCALL `(ASET ,VALUE ,(CADR ARRAYCALL) . ,(CDDR ARRAYCALL)))
;      (ARRAYCALL `(ASET ,VALUE ,(CADDR ARRAYCALL) . ,(CDDDR ARRAYCALL)))
;      ((APPLY FUNCALL* APPLY)
;       `(APPLY #'ASET ,VALUE ,(CADR ARRAYCALL) . ,(CDDR ARRAYCALL)))
;      (T `(ASET ,VALUE (FUNCTION ,(CAR ARRAYCALL)) . ,(CDR ARRAYCALL))))))

;(defmacro store (array-form value &environment env)
;  (let* ((inversions '((zl:aref . aset)
;                      (cl:aref . aset)
;                      (char . aset)
;                      (bit . aset)
;                      (sbit . aset)
;                      (svref . aset)
;                      (schar . aset)
;                      (funcall . maclisp-store-hack)))
;        (foo (macroexpand array-form env))
;        (invert (and (consp foo)
;                     (symbolp (car foo))
;                     (cdr (assq (car foo) inversions))))
;        (tem nil))
;    (cond ((not (null invert)) `(,invert ,value ,@(cdr foo)))
;         ((and (consp foo) (symbolp (car foo)))
;          (cond ((not (memq (car foo) '(apply lexpr-funcall)))
;                 `(maclisp-store-hack ,value ',(car foo) ,@(cdr foo)))
;                ((and (consp (setq tem (macroexpand (cadr foo) env)))
;                      (memq (car tem) '(quote function))
;                      (setq tem (cdr (assq (cadr tem) inversions))))
;                 `(apply #',tem ,value ,@(cddr array-form)))
;                (t `(apply #'maclisp-store-hack ,value ,@(cdr foo)))))
;         (t
;          (ferror "The array reference form ~S in not understood by ~S"
;                  array-form 'store)))))


;(defun maclisp-store-hack (value frob &rest subscripts &aux tem)
;  (do-forever
;    (etypecase frob
;      (array (return (apply #'aset value frob subscripts)))
;      (symbol (and (fboundp frob)
;                  (typep (setq tem (symbol-function frob)) 'array)
;                  (return (apply #'aset value tem subscripts)))))))

(DEFMACRO STORE (ARRAY-REFERENCE VALUE)
  "STORE: Maclisp compatible for all but the most obscure unreasonable usages"
  ;; YOU SEE. ANY TIME YOU DO AN ARRAY REFERENCE IN MACLISP THE
  ;; ACTUALLY ADDRESS (SORT OF A LOCATIVE) OF THE VALUE THAT WAS ACCESSED
  ;; ENDS UP IN PDP-10 REGISTER TT (OR WAS IT T?). STORE IS THEN EASY TO
  ;; IMPLEMENT, HENCE THE RIGHT TO LEFT EVALUATION ORDER, VALUE FIRST,
  ;; THEN THE ARRAY-REFERENCE FORM. AT SOME TIME THE CADR/LISPM SOFTWARE
  ;; HAD SIMILAR REGISTERS (IN THE MICROCODE) AND SIMILIAR IMPLEMENTATION
  ;; DETAILS FOR STORE. THIS DEFINITION AS A MACRO FOLLOWS WHAT
  ;; WAS NEEDED TO BRING UP MACSYMA IN VAX-NIL.
  (LET* ((ARRAYCALL (MACROEXPAND ARRAY-REFERENCE)))
    (SELECTQ (CAR ARRAYCALL)
      (FUNCALL `(ASET ,VALUE (STORE-MEDIATION-ROUTINE-INTERNAL ,(CADR ARRAYCALL))
                      ,@(CDDR ARRAYCALL)))
      (ARRAYCALL `(ASET ,VALUE ,(CADDR ARRAYCALL)  ,@(CDDDR ARRAYCALL)))
      ((LEXPR-FUNCALL FUNCALL* APPLY)
       `(LEXPR-FUNCALL 'ASET ,VALUE (STORE-MEDIATION-ROUTINE-INTERNAL ,(CADR ARRAYCALL))
                       ,@(CDDR ARRAYCALL)))
      (T
       ;; THIS IS CORRECT IFF (CAR ARRAYCALL) IS THE ACTUAL ARRAY.
       ;; IF IT WAS THE OBSCURE USAGE OF A GENERAL FUNCTION CALL THAT
       ;; DOES SOME ARRAY REFERENCE, THEREBY LEAVING REGISTER "TT" IN
       ;; A CERTAIN STATE, THEN WE WILL LOSE.
       ;; IF WE TOOK SERIOUSLY THE MACLISP (DECLARE (ARRAY ...)) FORM
       ;; THEN AT LEAST WE WOULD KNOW AS MUCH AS WHAT THE USER INTENDED
       ;; TO TELL THE MACLISP COMPILER IN DAYS PAST. THAT NOT BEING THE
       ;; CASE, HE WILL GET SOME OTHER WARNING, PERHAPS.
       ;; NOTE WELL: Even though it is possible on the LISPM to have a function
       ;; cell contain a symbol (which may also be an array) it is NOT POSSIBLE
       ;; IN MACLISP to have this sort of indefinite indirection in the case
       ;; of the ARRAY property. (Although it is in the case of EXPR property).
       `(ASET ,VALUE (FUNCTION ,(CAR ARRAYCALL))  ,@(CDR ARRAYCALL))))))



(DEFUN STORE-MEDIATION-ROUTINE-INTERNAL (X)
  (DO ((Y X (COND ((SYMBOLP Y)
                   (FSYMEVAL Y))
                  ('ELSE
                   ;; NOTE USE OF UPPER CASE ERROR MESSAGE IN MACLISP STYLE
                   (FERROR NIL "CAN'T GET ARRAY POINTER FOR: ~S" X)))))
      ((ARRAYP Y) Y)))
