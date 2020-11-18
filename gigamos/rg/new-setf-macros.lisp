;;; -*- Mode:LISP; Package:SETF; Base:10; Readtable:CL -*-

;;; Contains:
;;;    - Definitions of the SETF methods required in Common Lisp
;;;      [still have to do: char-bit, mask-field]
;;;    - Common Lisp macros that use generalized variables (GETF, REMF, INCF, DECF, PUSH,
;;;      POP, ASSERT, CTYPECASE, CCASE, SHIFTF, PSETF, ROTATEF)
;;;      [done so far: INCF, DECF, PUSH, POP]
;;;



;(shadow
;  '(
; ;    :incf
; ;    :decf
; ;    :push
; ;    :pop
; ;    :getf
;    ))

;(export
;  '(
; ;    incf
; ;    decf
; ;    push
; ;    pop
; ;    getf
;    ))

;(export '(
;         li::push
;         )
;       (find-package "LI" *package*))


;;; Yes, this is a simple as it appears, but the effect may not
;;; be.  Unless you know what you are doing, it would probably
;;; be better to use the hairy form.  Also, this is to remain an
;;; implementation feature only.
(define-simple-setf-macro li::rest            (LI:SET-CDR :args :value))
(define-simple-setf-macro li::first           (LI:SET-CAR :args :value))
(define-simple-setf-macro li::second          (LI:SET-CAR (LI:CDR :args) :value))
(define-simple-setf-macro li::third           (LI:SET-CAR (LI:CDDR :args) :value))
(define-simple-setf-macro li::fourth          (LI:SET-CAR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::fifth           (LI:SET-CAR (LI:CDDDDR :args) :value))
(define-simple-setf-macro li::sixth           (LI:SET-CAR (LI:NTHCDR 5 :args) :value))
(define-simple-setf-macro li::seventh         (LI:SET-CAR (LI:NTHCDR 6 :args) :value))
(define-simple-setf-macro li::eighth          (LI:SET-CAR (LI:NTHCDR 7 :args) :value))
(define-simple-setf-macro li::ninth           (LI:SET-CAR (LI:NTHCDR 8 :args) :value))
(define-simple-setf-macro li::tenth           (LI:SET-CAR (LI:NTHCDR 9 :args) :value))
(define-simple-setf-macro li::car             (LI:SET-CAR :args :value))
(define-simple-setf-macro li::cdr             (LI:SET-CDR :args :value))
(define-simple-setf-macro li::caar            (LI:SET-CAR (LI:CAR :args) :value))
(define-simple-setf-macro li::cadr            (LI:SET-CAR (LI:CDR :args) :value))
(define-simple-setf-macro li::cdar            (LI:SET-CAR (LI:CAR :args) :value))
(define-simple-setf-macro li::cddr            (LI:SET-CDR (LI:CDR :args) :value))
(define-simple-setf-macro li::caaar           (LI:SET-CAR (LI:CAAR :args) :value))
(define-simple-setf-macro li::caadr           (LI:SET-CAR (LI:CADR :args) :value))
(define-simple-setf-macro li::cadar           (LI:SET-CAR (LI:CDAR :args) :value))
(define-simple-setf-macro li::caddr           (LI:SET-CAR (LI:CDDR :args) :value))
(define-simple-setf-macro li::cdaar           (LI:SET-CDR (LI:CAAR :args) :value))
(define-simple-setf-macro li::cdadr           (LI:SET-CDR (LI:CADR :args) :value))
(define-simple-setf-macro li::cddar           (LI:SET-CDR (LI:CDAR :args) :value))
(define-simple-setf-macro li::cdddr           (LI:SET-CDR (LI:CDDR :args) :value))
(define-simple-setf-macro li::caaaar          (LI:SET-CAR (LI:CAAAR :args) :value))
(define-simple-setf-macro li::caaadr          (LI:SET-CAR (LI:CAADR :args) :value))
(define-simple-setf-macro li::caadar          (LI:SET-CAR (LI:CADAR :args) :value))
(define-simple-setf-macro li::caaddr          (LI:SET-CAR (LI:CADDR :args) :value))
(define-simple-setf-macro li::cadaar          (LI:SET-CAR (LI:CDAAR :args) :value))
(define-simple-setf-macro li::cadadr          (LI:SET-CAR (LI:CDADR :args) :value))
(define-simple-setf-macro li::caddar          (LI:SET-CAR (LI:CDDAR :args) :value))
(define-simple-setf-macro li::cadddr          (LI:SET-CAR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::cdaaar          (LI:SET-CDR (LI:CAAAR :args) :value))
(define-simple-setf-macro li::cdaadr          (LI:SET-CDR (LI:CAADR :args) :value))
(define-simple-setf-macro li::cdadar          (LI:SET-CDR (LI:CADAR :args) :value))
(define-simple-setf-macro li::cdaddr          (LI:SET-CDR (LI:CADDR :args) :value))
(define-simple-setf-macro li::cddaar          (LI:SET-CDR (LI:CDAAR :args) :value))
(define-simple-setf-macro li::cddadr          (LI:SET-CDR (LI:CDADR :args) :value))
(define-simple-setf-macro li::cdddar          (LI:SET-CDR (LI:CDDAR :args) :value))
(define-simple-setf-macro li::cddddr          (LI:SET-CDR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::nth             (LI:SET-CAR (LI:NTHCDR :args) :value))
;;;(define-simple-setf-macro li::elt             (SI::SETELT :args :value));***NOT CL***
;;;(define-simple-setf-macro li::svref           (SI::SET-AR-1 :args :value)) ;***NOT CL***
;;;(define-simple-setf-macro li::documentation   (SI::SET-DOCUMENTATION :args :value))  ;***NOT CL***
;;;(define-simple-setf-macro li::symbol-value    (SET :args :value))
;;;(define-simple-setf-macro li::symbol-function (SI::FDEFINE :args :value))            ;***NOT CL***
;;;(define-simple-setf-macro li::symbol-plist    (RPLACD (SI::PROPERTY-CELL-LOCATION :args) :value)) ;*** NOT CL***
;;;(define-simple-setf-macro li:macro-function  (SI::SET-MACRO-FUNCTION :args :value)) ;***NOT CL***

;;; Other required CL SETF methods.
;;;
;;; Note- none of the defsetf'd methods except subseq expand into Common Lisp
;;;       gethash might need to be redone with DEFINE-SETF-METHOD.

(setf:define-setf-method-for-k li:apply (#|*** &environment environment***|# function &rest args)
  (unless (and (consp function)
               (member (car function) '(function quote))
               (= (length function) 2)
               (symbolp (cadr function)))
    (error ;'sys:unknown-setf-reference
           "In SETF of APPLY, the function applied must be a constant."))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method (cons (cadr function) args) #|*** environment ***|#)
;    (if (memq (cadr function) '(zl:aref cl:aref))      ;why special-cased???
;       (setq storeform
;             `(aset ,(car (last storeform)) . ,(butlast (cdr storeform)))))
    (if (not (eq (car (last storeform)) (car (last tempvars))))
        (error ;'sys:unknown-setf-reference
          "~S not acceptable within ~S within SETF." function)
      (values tempvars tempargs storevars
              `(LI:APPLY #',(car storeform) . ,(cdr storeform))
              `(LI:APPLY #',(car refform) . ,(cdr refform))))))

(setf:defsetf li::aref (array &rest subscripts) (x)
  `(LI:ASET ,x ,array ,@subscripts))

(setf:defsetf li::bit (bit-array &rest subscripts) (x)
  `(LI:ASET (THE BIT ,x) ,bit-array ,@subscripts))

(setf:defsetf li::char (string index) (x)
  `(LI:ASET (THE STRING-CHAR ,x) ,string ,index))

(setf:defsetf li::fill-pointer (vector) (x)
  `(PROGN (SET-ARRAY-LEADER ,vector 0 ,x)
          ,x))

;(setf:defsetf li::get (symbol property-name &optional default) (x)
;  `(SETF (li:GETF (SYMBOL-PLIST ,symbol) ,property-name ,default) ,x))

;(setf:define-setf-method-for-k li::getf (place indicator &optional default)
;  (multiple-value-bind (temps vals stores store-form access-form)
;      (get-setf-method place)
;    (let ((itemp (gensym))
;         (dtemp (gensym))
;         (store (gensym))
;         (stemp (first stores)))
;      (values (list* itemp dtemp temps)
;             (list* indicator default vals)
;             (list store)
;             (let ((old-plist-temp (gensym)))
;               `(LET ((,old-plist-temp ,access-form))
;                  ,dtemp ;; Ignore default
;                  (IF ,old-plist-temp
;                      ;; If there is an old plist, destructively modify or extend it.
;                      (LABELS ((FOO (PLIST)
;                                 (COND ((NULL PLIST)
;                                        (RPLACD (LAST ,old-plist-temp)
;                                                (LIST ,itemp ,store)))
;                                       ((EQ (CAR PLIST) ,itemp)
;                                        (RPLACA (CDR PLIST) ,store))
;                                       (T
;                                        (FOO (CDDR PLIST))))))
;                        (FOO ,old-plist-temp))
;                      ;; If there isn't an old plist, make one.
;                      (LET ((,stemp (LIST ,itemp ,store)))
;                        ,store-form))
;                  ,store))
;             `(li:GETF ,access-form ,itemp ,dtemp)))))

(setf:define-setf-method-for-k symbol::getf (place indicator &optional default)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method place)
    (let ((itemp (gensym 'ind))
          (dtemp (gensym 'def))
          (store (gensym 'stor))
          (stemp (first stores)))
      (values (list* itemp dtemp temps)
              (list* indicator default vals)
              (list store)
              `(LET ((,stemp (SYMBOL:%PUTF ,access-form ,itemp ,store)))
                 ,store-form
                 ,store)
              `(symbol:GETF ,access-form ,itemp ,dtemp)))))

(setf:defsetf li::gethash (key hash-table &optional default) (value) ;;will this work for incf?
  `(PROGN (PUTHASH ,key ,value ,hash-table)
          ,value))

(setf:define-setf-method-for-k li::ldb (bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int)
    (let ((btemp (gensym))
          (store (gensym))
          (stemp (first stores)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                 ,store-form
                 ,store)
              `(ldb ,btemp ,access-form)
              ))))

(setf:defsetf li::sbit (simple-bit-array &rest subscripts) (x)
  `(LI:ASET (THE BIT ,x) ,simple-bit-array ,@subscripts))

(setf:defsetf li::schar (string index) (x)
  `(LI:ASET (THE STRING-CHAR ,x) ,string ,index))

(setf:defsetf li::subseq (sequence start &optional end) (new-seq)
  `(PROGN (REPLACE ,sequence (THE SEQUENCE ,new-seq) :START-1 ,start :START-2 ,end)
          ,new-seq))


;;; COMMON LISP READ-MODIFY-WRITE MACROS
;;;

;now using prims:incf defined in PRIMITIVE-SETF
;(prims:define-modify-macro li:incf (&optional (delta 1)) +
;  "Increment place's value by DELTA")

;(prims:define-modify-macro li:decf (&optional (delta 1)) -
;  "Decrement place's value by DELTA")

(prims:define-modify-macro li::triv-setf (new-value) (lambda (ignore u) u))

(prims::defmacro li::push (item place)
  "Add ITEM to the front of the list at PLACE."
  (multiple-value-bind (temps values storevars storeform accessform)
      (get-setf-method place)
    (let ((item-temp (gensym)))
      `(LI::LET (,@(mapcar #'list temps values)
             (,item-temp ,item))
         (LI::LET ((,(car storevars) (LI::CONS ,item-temp ,accessform)))
           ,storeform)))))

(prims::defmacro li::pop (place)
  "Remove the first element from the list at PLACE, and return that element."
  (multiple-value-bind (temps values storevars storeform accessform)
      (get-setf-method place)
    (let ((access-temp (gensym)))
      `(LET* (,@(mapcar #'list temps values)
              (,access-temp ,accessform)
              (,(car storevars) (LI:CDR ,access-temp)))
       (PROG1 (LI:CAR ,access-temp)
              ,storeform)))))

;(defun li::getf (place property &optional (default nil))
;  "Returns the PROPERTY property from the plist stored in PLACE.
;If there is no such property, DEFAULT is returned.
;PLACE should be such that simply evaluating it would return
;the contents of the property list."
;  (cond ((null place)
;        default)
;       ((eq (car place) property)
;        (cadr place))
;       (t
;        (getf (cddr place) property default))))
