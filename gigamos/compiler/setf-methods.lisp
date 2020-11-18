;;; -*- Mode:LISP; Package:COMPILER; Readtable:CL; Base:10 -*-

;; ||| New file -- smh 27sep88
;; This is the cross compiler analog of the fleabit new-setf-macros.
;; It contains cross-compiler-compatible setf method definitions.
;; Mostly it layers another layer of fixup on top of the falcon
;; package lossage, because nowhere on the Lambda are there
;; setf method definitions for global-package symbols -- there are only
;; definitions for CONS::CAR, etc.

(defsetf rest            CONS::SET-CDR)
(defsetf first           CONS::SET-CAR)
(defsetf second (x) (v)  `(CONS::SET-CAR (CDR ,x) ,v))
(defsetf third (x) (v)   `(CONS::SET-CAR (CDDR ,x) ,v))
(defsetf fourth (x) (v)  `(CONS::SET-CAR (CDDDR ,x) ,v))
(defsetf fifth (x) (v)   `(CONS::SET-CAR (CDDDDR ,x) ,v))
(defsetf sixth (x) (v)   `(CONS::SET-CAR (NTHCDR 5 ,x) ,v))
(defsetf seventh (x) (v) `(CONS::SET-CAR (NTHCDR 6 ,x) ,v))
(defsetf eighth (x) (v)  `(CONS::SET-CAR (NTHCDR 7 ,x) ,v))
(defsetf ninth (x) (v)   `(CONS::SET-CAR (NTHCDR 8 ,x) ,v))
(defsetf tenth (x) (v)   `(CONS::SET-CAR (NTHCDR 9 ,x) ,v))
(defsetf car             CONS::SET-CAR)
(defsetf cdr             CONS::SET-CDR)
(defsetf caar (x) (v)    `(CONS::SET-CAR (CAR ,x) ,v))
(defsetf cadr (x) (v)    `(CONS::SET-CAR (CDR ,x) ,v))
(defsetf cdar (x) (v)    `(CONS::SET-CAR (CAR ,x) ,v))
(defsetf cddr (x) (v)    `(CONS::SET-CDR (CDR ,x) ,v))
(defsetf caaar (x) (v)   `(CONS::SET-CAR (CAAR ,x) ,v))
(defsetf caadr (x) (v)   `(CONS::SET-CAR (CADR ,x) ,v))
(defsetf cadar (x) (v)   `(CONS::SET-CAR (CDAR ,x) ,v))
(defsetf caddr (x) (v)   `(CONS::SET-CAR (CDDR ,x) ,v))
(defsetf cdaar (x) (v)   `(CONS::SET-CDR (CAAR ,x) ,v))
(defsetf cdadr (x) (v)   `(CONS::SET-CDR (CADR ,x) ,v))
(defsetf cddar (x) (v)   `(CONS::SET-CDR (CDAR ,x) ,v))
(defsetf cdddr (x) (v)   `(CONS::SET-CDR (CDDR ,x) ,v))
(defsetf caaaar (x) (v)  `(CONS::SET-CAR (CAAAR ,x) ,v))
(defsetf caaadr (x) (v)  `(CONS::SET-CAR (CAADR ,x) ,v))
(defsetf caadar (x) (v)  `(CONS::SET-CAR (CADAR ,x) ,v))
(defsetf caaddr (x) (v)  `(CONS::SET-CAR (CADDR ,x) ,v))
(defsetf cadaar (x) (v)  `(CONS::SET-CAR (CDAAR ,x) ,v))
(defsetf cadadr (x) (v)  `(CONS::SET-CAR (CDADR ,x) ,v))
(defsetf caddar (x) (v)  `(CONS::SET-CAR (CDDAR ,x) ,v))
(defsetf cadddr (x) (v)  `(CONS::SET-CAR (CDDDR ,x) ,v))
(defsetf cdaaar (x) (v)  `(CONS::SET-CDR (CAAAR ,x) ,v))
(defsetf cdaadr (x) (v)  `(CONS::SET-CDR (CAADR ,x) ,v))
(defsetf cdadar (x) (v)  `(CONS::SET-CDR (CADAR ,x) ,v))
(defsetf cdaddr (x) (v)  `(CONS::SET-CDR (CADDR ,x) ,v))
(defsetf cddaar (x) (v)  `(CONS::SET-CDR (CDAAR ,x) ,v))
(defsetf cddadr (x) (v)  `(CONS::SET-CDR (CDADR ,x) ,v))
(defsetf cdddar (x) (v)  `(CONS::SET-CDR (CDDAR ,x) ,v))
(defsetf cddddr (x) (v)  `(CONS::SET-CDR (CDDDR ,x) ,v))
(defsetf nth (n x) (v)   `(CONS::SET-CAR (NTHCDR ,n ,x) ,v))


;; +++ These locf methods are not yet really supported in the runtime code.

(defsetf symbol-value    SET)
(deflocf symbol-value    value-cell-location)
(deflocf symeval         value-cell-location)
(defsetf symbol-function symbol::set-symbol-function)
(deflocf symbol-function function-cell-location)
(deflocf fsymeval        function-cell-location)
(defsetf symbol-plist    symbol::set-symbol-plist)
(deflocf symbol-plist    plist-location)
(defsetf symbol-package  symbol::set-symbol-package)
(deflocf symbol-package  package-cell-location)

(deflocf %INSTANCE-REF   instance-variable-location)

(defsetf svref           array::svset)
;; One of the next two is probably wrong.
(deflocf aref            array::aloc)           ;presently unimplemented
(deflocf zl:aref         array::aloc)           ;presently unimplemented

;;;(defsetf elt             (SI::SETELT x v))
;;;(defsetf documentation   (SI::SET-DOCUMENTATION x v))
;;;(defsetf macro-function  (SI::SET-MACRO-FUNCTION x v))

;;; Other required CL SETF methods.
;;;
;;; Note- none of the defsetf'd methods except subseq expand into Common Lisp
;;;       gethash might need to be redone with DEFINE-SETF-METHOD.

;; This needs to be checked for sanity. --smh 28sep88

(define-setf-method apply (#|*** &environment environment***|# function &rest args)
  (unless (and (consp function)
               (member (car function) '(function quote))
               (= (length function) 2)
               (symbolp (cadr function)))
    (error                                      ;'sys:unknown-setf-reference
      "In SETF of APPLY, the function applied must be a constant."))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method (cons (cadr function) args) #|*** environment ***|#)
;    (if (memq (cadr function) '(zl:aref cl:aref))      ;why special-cased???
;       (setq storeform
;             `(aset ,(car (last storeform)) . ,(butlast (cdr storeform)))))
    (if (not (eq (car (last storeform)) (car (last tempvars))))
        (error "~S not acceptable within ~S within SETF." function)
      (values tempvars tempargs storevars
              `(APPLY #',(car storeform) . ,(cdr storeform))
              `(APPLY #',(car refform) . ,(cdr refform))))))

(defsetf aref (array &rest subscripts) (x)
  (case (length subscripts)
    (1 `(array::aset-1 ,x ,array ,@subscripts))
    (2 `(array::aset-2 ,x ,array ,@subscripts))
    (t `(array::ASET ,x ,array ,@subscripts))))

(defsetf bit (bit-array &rest subscripts) (x)
  (case (length subscripts)
    (1 `(array::aset-1 (the bit ,x) ,bit-array ,@subscripts))
    (2 `(array::aset-2 (the bit ,x) ,bit-array ,@subscripts))
    (t `(array::ASET (the bit ,x) ,bit-array ,@subscripts))))

(defsetf sbit (simple-bit-array &rest subscripts) (x)
  (case (length subscripts)
    (1 `(array::aset-1 (the bit ,x) ,simple-bit-array ,@subscripts))
    (2 `(array::aset-2 (the bit ,x) ,simple-bit-array ,@subscripts))
    (t `(array::ASET (the bit ,x) ,simple-bit-array ,@subscripts))))

(defsetf char (string index) (x)
  `(array::ASET-1 (THE STRING-CHAR ,x) ,string ,index))

(defsetf schar (string index) (x)
  `(array::ASET-1 (THE STRING-CHAR ,x) ,string ,index))

(defsetf fill-pointer array::SET-fill-pointer)

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

(define-setf-method getf (place indicator &optional default)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method place)
    (let ((itemp (gensym 'ind))
          (dtemp (gensym 'def))
          (store (gensym 'stor))
          (stemp (first stores)))
      (values (list* itemp dtemp temps)
              (list* indicator default vals)
              (list store)
              `(LET ((,stemp (SYMBOL::%PUTF ,access-form ,itemp ,store)))
                 ,store-form
                 ,store)
              `(GETF ,access-form ,itemp ,dtemp)))))

(defsetf get (object property &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,property ,default) property)))
    `(symbol::%put ,object ,tem ,value)))

;; It remains to be proven that this gethash method works with complete
;; generality for embedded forms.
;; There is also the language-definition question whether the default form
;; is supposed to be evaluated, even though it is not used.

(defsetf gethash (hashtable key &optional default) (value)
  `(li::%sethash ,hashtable ,key ,value))

(define-setf-method ldb (bytespec int)
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



(define-setf-method progn (&environment environment &rest forms)
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method-multiple-value (car (last forms)) environment)
    (values tempvars tempargs storevars
            `(progn ,@(butlast forms) ,storeform)
            `(progn ,@(butlast forms) ,refform))))

(define-setf-method locally (&environment environment &rest forms)
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method-multiple-value (car (last forms)) environment)
    (values tempvars tempargs storevars
            `(locally ,@(butlast forms) ,storeform)
            `(locally ,@(butlast forms) ,refform))))

(deflocf locally (&rest forms)
  (let ((new-body (copy-list forms)))
    (rplaca (last new-body)
            `(locf ,(car (last new-body))))
    `(locally . ,new-body)))

(define-setf-method let (&environment environment boundvars &rest forms)
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method-multiple-value (car (last forms)) environment)
    (values tempvars tempargs storevars
            `(let ,boundvars ,@(butlast forms) ,storeform)
            `(let ,boundvars ,@(butlast forms) ,refform))))

(deflocf progn (&rest forms)
  (let ((new-body (copy-list forms)))
    (rplaca (last new-body)
            `(locf ,(car (last new-body))))
    `(progn . ,new-body)))

(deflocf let (boundvars &rest forms)
  (let ((new-body (copy-list forms)))
    (rplaca (last new-body)
            `(locf ,(car (last new-body))))
    `(let ,boundvars . ,new-body)))

(deflocf setq (a b)
  (let ((at (gensym)))
    `(let ((,at (locf ,b)))
       (setq ,a (car ,at))
       ,at)))



;(defsetf li::subseq (sequence start &optional end) (new-seq)
;  `(PROGN (REPLACE ,sequence (THE SEQUENCE ,new-seq) :START-1 ,start :START-2 ,end)
;         ,new-seq))

(defmacro push (item place)
  "Add ITEM to the front of the list at PLACE."
  (multiple-value-bind (temps values storevars storeform accessform)
      (get-setf-method place)
    (let ((item-temp (gensym)))
      `(LET (,@(mapcar #'list temps values)
             (,item-temp ,item))
         (LET ((,(car storevars) (CONS ,item-temp ,accessform)))
           ,storeform)))))

(defmacro pop (place)
  "Remove the first element from the list at PLACE, and return that element."
  (multiple-value-bind (temps values storevars storeform accessform)
      (get-setf-method place)
    (let ((access-temp (gensym)))
      `(LET* (,@(mapcar #'list temps values)
              (,access-temp ,accessform)
              (,(car storevars) (CDR ,access-temp)))
       (PROG1 (CAR ,access-temp)
              ,storeform)))))
