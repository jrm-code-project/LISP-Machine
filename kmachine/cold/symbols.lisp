;;; -*- Mode:LISP; Package:SYMBOL; Readtable:CL; Base:10 -*-

;;; &&& Adopted %VARIABLE-NAME to specify arguments which are assumed by the code to be of a certain type. <01-Nov-88 wkf>
;;; &&& Continued work through out.   <31-Oct-88 wkf>
;;; &&& Major reworking of this file. <28-Oct-88 wkf>

(export '(boundp
          fboundp
          fmakunbound
          get
          get-properties
          getf
          make-symbol
          makunbound
          remf
          remprop
          set
          symbol-function
          symbol-name
          symbol-package
          symbol-plist
          symbol-value
 ))

;;;&&&||| Made error messages more useful 10/19/88 --wkf

(defconstant *symbol-size* 5)

(defconstant *symbol-pname*    0)
(defconstant *symbol-value*    1)
(defconstant *symbol-function* 2)
(defconstant *symbol-package*  3)
(defconstant *symbol-plist*    4)

;;; Isn't Common LISP wonderful, NIL is a symbol and a LIST

;; T if object is DTP-SYMBOL
(defsubst %symbolp (object) "Does not return T for the symbol 'NIL"
  (hw:field= gr:*T* object vinc::%%data-type))

;;; $$$ Removed %symbol? <28-Oct-88 wkf>

;; T if object is DTP-SYMBOL or DTP-NIL
(defsubst symbolp (object)
  (hw:32logbitp (hw:ldb object vinc::%%data-type 0)
                (hw:unboxed-constant
                  #.(lisp:logior (lisp:ash 1 $$dtp-nil)
                                 (lisp:ash 1 $$dtp-symbol)))))

;;; $$$ Removed symbol? <28-Oct-88 wkf>

;;; Print Name

;;; 1.  Pnames are not allowed to be altered, just read.
;;; 2.  PNAME of NIL is funny because of how we make CAR and CDR
;;;     of NIL fast.

;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %symbol-name (%symbol) "Assumes %SYMBOL is a symbol.  Does not work for 'NIL"
  (make-pointer $$dtp-array (contents-offset %symbol *symbol-pname*)))

(defun symbol-name (symbol)
  (cond ((%symbolp symbol) (%symbol-name symbol))
        ((null symbol)     "NIL")
        (t (li:tail-error "Wasn't a symbol to symbol name." symbol))))

;;; Value Cell

;;; +++ Look at the following list. <14-Nov-88 wkf>
;;; 1. SYMBOL-VALUE is the real symbol value
;;; 2. %SET does not check to see if the symbol is constant.
;;;    SET probably should
;;; 3. Therefore, we do not defsetf symbol-value to %set
;;; 4. Same thing goes for %make-unbound
;;; 5. Implement SETQ as a call to SET.

;;; $$$ Added. <14-Nov-88 wkf>
;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %symbol-value-internal (%symbol) "Assumes %SYMBOL is a symbol and is BOUND."
  (contents-offset %symbol *symbol-value*))

(defun %symbol-value (%symbol) "Assumes %SYMBOL is a symbol."
  ;; $$$ Fixed %symbol-value to trap on unbound symbols. <14-Nov-88 wkf>
  (let ((ans (%symbol-value-internal %symbol)))
    (if (%word-boundp ans)
        ans
      (li:tail-error "%Symbol-value called on an unbound symbol.  symbol: ~s  value: ~s" %symbol ans))))

(defun symbol-value (symbol)
  (if (symbolp symbol)
      (%symbol-value symbol)
    (li:tail-error "SYMBOL-VALUE not called on a symbol" symbol)))

;;; $$$ Renamed %%set to %set-internal. <02-Nov-88 wkf>
;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %set-internal (%symbol value) "Will let you bash T and NIL.  Assumes %SYMBOL is a symbol."
  ;; $$$ Put back fast version <28-Oct-88 wkf>
  (store-contents-offset %symbol *symbol-value* value))

(defun set-error (symbol value)
  (if (eq symbol 'NIL)
      (li:error "Nihil ex nilhil: don't set NIL")
    (li:tail-error "SET not called on a symbol" symbol value)))

(defun %set (symbol value) "Will let you bash T."
  ;; $$$ Put back fast version. <28-Oct-88 wkf>
  (if (%symbolp symbol)                         ; $$$ Not true for NIL. <31-Oct-88 wkf>
      (%set-internal symbol value)
    (set-error symbol value)))

(defun set (symbol value)
  (if (eq symbol 'T)
      (li:tail-error "Veritas aeterae: don't set T")
    (%set symbol value)))

;;; @@@ Do we want a %make-unbound-internal or another name. to call %set-internal
;;;                (assumes a symbol and not t or nil.) <28-Oct-88 wkf>

(defun %make-unbound (symbol) "Will let you bash T."
  (%set symbol (make-pointer $$dtp-unbound symbol))
  symbol)

(defsubst %word-boundp (word)                   ; $$$ Added. <14-Nov-88 wkf>
  (not (vinc:data-type= word (hw:unboxed-constant #.(lisp:ash $$dtp-unbound (byte-position vinc:%%data-type))))))

(defsubst %cell-boundp (cell)
  (hw:vma-start-read-will-write-vma-unboxed-md-boxed cell)
  (%word-boundp (hw:read-md)))

(defun %boundp (%symbol) "Assumes %SYMBOL is a symbol."
  (%cell-boundp (hw:24+ *symbol-value* %symbol)))       ;+++ hw:24+ is not really sufficient

(defun boundp (symbol)
  (if (symbolp symbol)
      (%boundp symbol)
    (li:tail-error "Boundp not called on a symbol" symbol)))

(defun makunbound (symbol)
  (set symbol (make-pointer $$dtp-unbound symbol))
  symbol)

;;; Function Cell

;;; 1. SYMBOL-FUNCTION is the common lisp function
;;; 2. When bashing the function cell of a symbol, it is important
;;;    to unlink any FEF it may be pointing at.

;;; $$$ Added. <21-Nov-88 wkf>
(defun %symbol-function-internal (%symbol) "Assumes %SYMBOL is a symbol and is FBOUND"
  (contents-offset %symbol *symbol-function*))

;;;@@@ Turn into a macro for speed. --wkf
(defun %symbol-function (%symbol) "Assumes %SYMBOL is a symbol."
  ;; $$$ Fixed %symbol-function to trap on unfbound symbols. <21-Nov-88 wkf>
  (let ((ans (%symbol-function-internal %symbol)))
    (if (%word-boundp ans)
        ans
      (li:tail-error "%Symbol-function called on an unfbound symbol.  symbol: ~s  fvalue: ~s" %symbol ans))))

(defun symbol-function (symbol)
  (if (symbolp symbol)
      (%symbol-function symbol)
    (li:tail-error "SYMBOL-FUNCTION not called on a symbol" symbol)))

;;; @@@ Do we want a symbol-function which does not work for NIL? (ie use %symbolp)  <28-Oct-88 wkf>

(defun %set-symbol-function (%symbol value)  ;;@@@ Turn into a macro for speed. --wkf
  "Assumes %SYMBOL is a symbol.  Returns value."
  (store-contents-offset %symbol *symbol-function* value))

(defun set-symbol-function (symbol value)
  (if (symbolp symbol)
      (if (%fboundp symbol)
          (let ((old-function (%symbol-function symbol)))
            (if (eq old-function value)
                value
              (prog1 (%set-symbol-function symbol value)
                     (k2:kill-old-function old-function))))
        (%set-symbol-function symbol value))
    (li:tail-error "~A is not a symbol in SET-SYMBOL-FUNCTION" symbol value)))

;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
;;; @@@ We need versions of these types of functions where we don't care about the return value. <28-Oct-88 wkf>
(defun %fmakunbound (%symbol) "Assumes %SYMBOL is a symbol."
  (%set-symbol-function %symbol (make-pointer $$dtp-unbound %symbol))
  %symbol)

(defun fmakunbound (symbol)                   ; @@@ This could be optimized better by calling special routine. <28-Oct-88 wkf>
  (set-symbol-function symbol (make-pointer $$dtp-unbound symbol))
  symbol)

;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %fboundp (%symbol) "Assumes %SYMBOL is a symbol."
  (%cell-boundp (hw:24+ *symbol-function* %symbol)))

(defun fboundp (symbol)
  (if (symbolp symbol)
      (%fboundp symbol)
    (li:tail-error "FBOUNDP not called on a symbol" symbol)))

(defsetf  symbol-function  set-symbol-function)
(defsetf %symbol-function %set-symbol-function)

;;; Package cells

;;; 1. No restrictions on SYMBOL-PACKAGE

;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %symbol-package (%symbol) "Assumes %SYMBOL is a symbol."
  (contents-offset %symbol *symbol-package*))

(defun symbol-package (symbol)
  (if (symbolp symbol)
      (%symbol-package symbol)
    (li:tail-error "SYMBOL-PACKAGE not called on a symbol" symbol)))

;;; @@@ Turn into a macro for speed. <28-Oct-88 wkf>
(defun %set-symbol-package (%symbol value) "Assumes %SYMBOL is a symbol."
  (store-contents-offset %symbol *symbol-package* value))

(defun set-symbol-package (symbol value)
  (if (symbolp symbol)
      (%set-symbol-package symbol value)
    (li:tail-error "SET-SYMBOL-PACKAGE not called on a symbol" symbol value)))

(defsetf  symbol-package  set-symbol-package)
(defsetf %symbol-package %set-symbol-package)

;;; Property List

;;; @@@ Turn into a macro. <27-Oct-88 wkf>
(defun %symbol-plist (%symbol) "Assumes %SYMBOL is a symbol."
  (contents-offset %symbol *symbol-plist*))

(defun symbol-plist (symbol)
  (if (symbolp symbol)
      (%symbol-plist symbol)
    (li:tail-error "SYMBOL-PLIST not called on a symbol" symbol)))

;;; @@@ Turn into a macro. <27-Oct-88 wkf>
(defun %set-symbol-plist (%symbol value) "Assumes %SYMBOL is a symbol."
  (store-contents-offset %symbol *symbol-plist* value))

(defun set-symbol-plist (symbol value)
  (if (symbolp symbol)
      (%set-symbol-plist symbol value)
    (li:tail-error "SET-SYMBOL-PLIST not called on a symbol" symbol value)))

(defsetf  symbol-plist  set-symbol-plist)
(defsetf %symbol-plist %set-symbol-plist)

(defun get (symbol property &optional default)  ; $$$ Changed to use GETF. <27-Oct-88 wkf>
  "Returns the value of SYMBOL's PROPERTY property.
If there is no property, DEFAULT is returned."
  (getf (symbol-plist symbol) property default))        ; @@@ Compiler optimization would make this a jump <27-Oct-88 wkf>

(defun %get (%symbol property &optional default)
  "Assumes %SYMBOL is a symbol.
Returns the value of SYMBOL's PROPERTY property.
If there is no property, DEFAULT is returned."
  (getf (%symbol-plist %symbol) property default))      ; @@@ Compiler optimization would make this a jump <27-Oct-88 wkf>

(defun getf (place property &optional default)  ; $$$ Added documentation. <27-Oct-88 wkf>
  "Returns the PROPERTY property from the plist stored in PLACE.
If there is no such property, DEFAULT is returned.
PLACE should be such that simply evaluating it would return
the contents of the property list."
  (do ((plist place (cons:cddr plist)))
      ((null plist) default)
    (when (eq property (cons:car plist))
      (return (cons:cadr plist)))))

;;; ||| Fixed the definition of %putf to return the head of the list
;;; ||| That was being putf'd on JIM 10/26/88
(defun %putf (plist property data)
  (if (null plist)
      (cons:cons property (cons:cons data nil))
    (do* ((plist-part   plist                  plist-next)
          (plist-next   (cons:cddr plist-part) (cons:cddr plist-part))
          (current-prop (cons:car  plist-part) (cons:car  plist-part)))
         ((eq current-prop property)
          (setf (cons:cadr plist-part) data)
          plist)
      (unless plist-next
        (setf (cons:cddr plist-part) (cons:cons property (cons:cons data nil)))
        (return plist)))))

(defun %put (%symbol property data)
  "Assumes %SYMBOL is a symbol."
  ;; @@@ store-contents-offset also does a w/o-interrupts. <28-Oct-88 wkf>
  (trap:without-interrupts
    (store-contents-offset %symbol *symbol-plist* (%putf (%symbol-plist %symbol) property data)))
  data)

(defun put (symbol property data)
  (if (symbolp symbol)
      (%put symbol property data)       ; @@@ Compiler optimization would make this a jump <28-Oct-88 wkf>
    (li:tail-error "Not a symbol to PUT" symbol property data)))        ; $$$ Removed extraneous arg here <17-Nov-88 wkf>

;; +++ Does this need a reader macro? <28-Oct-88 wkf>
;;#+(target falcon) $$$ Removed reader conditionalization. <31-Oct-88 wkf>
(defsetf get  (symbol property) (value)         ; +++ Removed optional default NOP. <31-Oct-88 wkf>
  `(put ,symbol ,property ,value))

;;; $$$ uncommented out. <31-Oct-88 wkf>
(defsetf %get (%symbol property) (value)        ; +++ Removed optional default NOP. <31-Oct-88 wkf>
  `(%put ,%symbol ,property ,value))

(defun remprop (symbol-or-plist property)
  "Remove a property.  Returns NIL if not present, or a list whose CAR is the property.
SYMBOL-OR-PLIST may be a symbol or a disembodied property list -
a list or locative whose cdr stores the properties.
It may also be an instance or named structure; then it is sent a :REMPROP message."
  ;; +++ Does not handle instances or named-structures. <31-Oct-88 wkf>
  (trap:without-interrupts
    (let* ((symbolp (symbolp symbol-or-plist))
           (place   (if symbolp
                        (%symbol-plist symbol-or-plist)
                      (cons:cdr symbol-or-plist))))
      (if (eq property (cons:car place))
          (progn (if symbolp
                     (%set-symbol-plist symbol-or-plist (cons:cddr place))
                   (setf (cons:cdr symbol-or-plist) (cons:cddr place)))
                 place)
        (do* ((odd-plist (cons:cdr place)      (cons:cdr plist))
              (plist     (cons:cdr odd-plist)  (cons:cdr odd-plist)))
             ((null plist))
          (when (eq property (cons:car plist))
            (setf (cons:cdr odd-plist) (cons:cddr plist))
            (return plist)))))))

(defun %remf (place property)
  (trap:without-interrupts
    (if (eq property (cons:car place))
        (cons:cddr place)
      (do* ((odd-plist (cons:cdr place)      (cons:cdr plist))
            (plist     (cons:cdr odd-plist)  (cons:cdr odd-plist)))
           ((null plist))
        (when (eq property (cons:car plist))
          (setf (cons:cdr odd-plist) (cons:cddr plist))
          (return place))))))

(defmacro remf (place property)
  `(%remf ,place ,property))

(defun get-properties (place list-of-properties)        ; $$$ Added documentation <27-Oct-88 wkf>
  "Finds the first property of any in LIST-OF-PROPERTIES from the plist PLACE.
The first value is the property found first,
the second is the value of that property,
the third is the tail of the plist, whose car is the property
and whose cadr is the value.
PLACE should be such that simply evaluating it would return
the contents of the property list."
  (do* ((plist place (cons:cddr plist)))
       ((null plist))
    (let ((current-property (cons:car plist)))
      (li:dolist (property list-of-properties)
        (when (eq property current-property)
          (return-from get-properties (values property (cons:cadr plist) plist)))))))


;;; Making symbols

(defun %make-symbol (%pname) "Assumes %PNAME is a string."
  (let ((symbol (allocate-structure *symbol-size* 0
                                    $$dtp-symbol
                                    (make-header $$dtp-symbol-header %pname))))
    ;; %PNAME is set up by allocate structure

    ;; This setf is not strictly necessary as structures are filled with NIL
    ;; when created. @@@ <28-Oct-88 wkf>
    (setf (symbol-plist   symbol) nil
          (symbol-package symbol) nil)

    ;; We know that the symbol is not constant because we just made it.
    (%make-unbound symbol)                      ; @@@ Use a version where we don't care about return value <28-Oct-88 wkf>
    (%fmakunbound  symbol))) ;;Return the new symbol.

(defun make-symbol (pname)
  (if (array:stringp pname)
      (%make-symbol pname)
    (li:tail-error "Make-symbol not handed a string: ~a" pname)))
