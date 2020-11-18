;;; -*- Mode:LISP; Package:SETF; Base:10; Readtable:CL -*-

;;; Contains:
;;;    - Definitions of the SETF methods required in Common Lisp
;;;      [still have to do: char-bit, mask-field]
;;;    - Common Lisp macros that use generalized variables (GETF, REMF, INCF, DECF, PUSH,
;;;      POP, ASSERT, CTYPECASE, CCASE, SHIFTF, PSETF, ROTATEF)
;;;      [done so far: INCF, DECF, PUSH, POP]
;;;



(export
  '(
;    incf
;    decf
    push
    pop
    getf
    ))

(export '(
          li::push
          )
        (find-package "LI" *package*))


;;; Yes, this is a simple as it appears, but the effect may not
;;; be.  Unless you know what you are doing, it would probably
;;; be better to use the hairy form.  Also, this is to remain an
;;; implementation feature only.
(define-simple-setf-macro li::rest            (CONS:SET-CDR :args :value))
(define-simple-setf-macro li::first           (CONS:SET-CAR :args :value))
(define-simple-setf-macro li::second          (CONS:SET-CAR (LI:CDR :args) :value))
(define-simple-setf-macro li::third           (CONS:SET-CAR (LI:CDDR :args) :value))
(define-simple-setf-macro li::fourth          (CONS:SET-CAR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::fifth           (CONS:SET-CAR (LI:CDDDDR :args) :value))
(define-simple-setf-macro li::sixth           (CONS:SET-CAR (LI:NTHCDR 5 :args) :value))
(define-simple-setf-macro li::seventh         (CONS:SET-CAR (LI:NTHCDR 6 :args) :value))
(define-simple-setf-macro li::eighth          (CONS:SET-CAR (LI:NTHCDR 7 :args) :value))
(define-simple-setf-macro li::ninth           (CONS:SET-CAR (LI:NTHCDR 8 :args) :value))
(define-simple-setf-macro li::tenth           (CONS:SET-CAR (LI:NTHCDR 9 :args) :value))
(define-simple-setf-macro li::car             (CONS:SET-CAR :args :value))
(define-simple-setf-macro li::cdr             (CONS:SET-CDR :args :value))
(define-simple-setf-macro li::caar            (CONS:SET-CAR (LI:CAR :args) :value))
(define-simple-setf-macro li::cadr            (CONS:SET-CAR (LI:CDR :args) :value))
(define-simple-setf-macro li::cdar            (CONS:SET-CAR (LI:CAR :args) :value))
(define-simple-setf-macro li::cddr            (CONS:SET-CDR (LI:CDR :args) :value))
(define-simple-setf-macro li::caaar           (CONS:SET-CAR (LI:CAAR :args) :value))
(define-simple-setf-macro li::caadr           (CONS:SET-CAR (LI:CADR :args) :value))
(define-simple-setf-macro li::cadar           (CONS:SET-CAR (LI:CDAR :args) :value))
(define-simple-setf-macro li::caddr           (CONS:SET-CAR (LI:CDDR :args) :value))
(define-simple-setf-macro li::cdaar           (CONS:SET-CDR (LI:CAAR :args) :value))
(define-simple-setf-macro li::cdadr           (CONS:SET-CDR (LI:CADR :args) :value))
(define-simple-setf-macro li::cddar           (CONS:SET-CDR (LI:CDAR :args) :value))
(define-simple-setf-macro li::cdddr           (CONS:SET-CDR (LI:CDDR :args) :value))
(define-simple-setf-macro li::caaaar          (CONS:SET-CAR (LI:CAAAR :args) :value))
(define-simple-setf-macro li::caaadr          (CONS:SET-CAR (LI:CAADR :args) :value))
(define-simple-setf-macro li::caadar          (CONS:SET-CAR (LI:CADAR :args) :value))
(define-simple-setf-macro li::caaddr          (CONS:SET-CAR (LI:CADDR :args) :value))
(define-simple-setf-macro li::cadaar          (CONS:SET-CAR (LI:CDAAR :args) :value))
(define-simple-setf-macro li::cadadr          (CONS:SET-CAR (LI:CDADR :args) :value))
(define-simple-setf-macro li::caddar          (CONS:SET-CAR (LI:CDDAR :args) :value))
(define-simple-setf-macro li::cadddr          (CONS:SET-CAR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::cdaaar          (CONS:SET-CDR (LI:CAAAR :args) :value))
(define-simple-setf-macro li::cdaadr          (CONS:SET-CDR (LI:CAADR :args) :value))
(define-simple-setf-macro li::cdadar          (CONS:SET-CDR (LI:CADAR :args) :value))
(define-simple-setf-macro li::cdaddr          (CONS:SET-CDR (LI:CADDR :args) :value))
(define-simple-setf-macro li::cddaar          (CONS:SET-CDR (LI:CDAAR :args) :value))
(define-simple-setf-macro li::cddadr          (CONS:SET-CDR (LI:CDADR :args) :value))
(define-simple-setf-macro li::cdddar          (CONS:SET-CDR (LI:CDDAR :args) :value))
(define-simple-setf-macro li::cddddr          (CONS:SET-CDR (LI:CDDDR :args) :value))
(define-simple-setf-macro li::nth             (CONS:SET-CAR (LI:NTHCDR :args) :value))
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

(define-setf-method li:apply (#|*** &environment environment***|# function &rest args)
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

(defsetf li::aref (array &rest subscripts) (x)
  `(LI:ASET ,x ,array ,@subscripts))

(defsetf li::bit (bit-array &rest subscripts) (x)
  `(LI:ASET (THE BIT ,x) ,bit-array ,@subscripts))

(defsetf li::char (string index) (x)
  `(LI:ASET (THE STRING-CHAR ,x) ,string ,index))

(defsetf li::fill-pointer (vector) (x)
  `(PROGN (array:SET-ARRAY-LEADER ,vector 0 ,x)
          ,x))

;(defsetf li::get (symbol property-name &optional default) (x)
;  `(SETF (GETF (SYMBOL-PLIST ,symbol) ,property-name ,default) ,x))

;(define-setf-method li::getf (place indicator &optional default)
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
;             `(GETF ,access-form ,itemp ,dtemp)))))

(define-setf-method symbol::getf (place indicator &optional default)
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

(defsetf li::gethash (key hash-table &optional default) (value) ;;will this work for incf?
  `(PROGN (PUTHASH ,key ,value ,hash-table)
          ,value))

(define-setf-method li::ldb (bytespec int)
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

(defsetf li::sbit (simple-bit-array &rest subscripts) (x)
  `(LI:ASET (THE BIT ,x) ,simple-bit-array ,@subscripts))

(defsetf li::schar (string index) (x)
  `(LI:ASET (THE STRING-CHAR ,x) ,string ,index))

(defsetf li::subseq (sequence start &optional end) (new-seq)
  `(PROGN (REPLACE ,sequence (THE SEQUENCE ,new-seq) :START-1 ,start :START-2 ,end)
          ,new-seq))


;;; COMMON LISP READ-MODIFY-WRITE MACROS
;;;

;;; defined by PRIMITIVE-SETF
;(define-modify-macro li::incf (&optional (delta 1)) +
;  "Increment place's value by DELTA")

;(define-modify-macro li::decf (&optional (delta 1)) -
;  "Decrement place's value by DELTA")

(define-modify-macro li::triv-setf (new-value) (lambda (ignore u) u))

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


;;; $$$ From "k-sys:warm;array2" <19-Nov-88 wkf>
(defsetf array:array-leader array:set-array-leader)
(defsetf array:aref (array &rest subscripts) (value)
  `(array:aset ,value ,array . ,subscripts))

;;; $$$ From "k-sys:warm;lists" <19-Nov-88 wkf>
(defsetf li:FIRST CONS:SET-CAR)

;;; $$$ "k-sys:warm;hash" <19-Nov-88 wkf>
(defsetf li:%hash-table-size      li:%hash-table-size-set)
(defsetf li:%hash-table-use-count li:%hash-table-use-count-set)
(defsetf li:%hash-table-test      li:%hash-table-test-set)
(defsetf li:%hash-table-rehash-size li:%hash-table-rehash-size-set)
(defsetf li:%hash-table-rehash-threshold li:%hash-table-rehash-threshold-set)
(defsetf li:%hash-table-data-ptr li:%hash-table-data-ptr-set)
(defsetf li:gethash  (key hash-table &optional default) (value)
  `(li:%sethash ,key ,hash-table ,value))
