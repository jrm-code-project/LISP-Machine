;-*- Mode:LISP; Package:EH; Readtable:ZL; Base:8 -*-

;;>> missing handlers for:
;  cons-in-inappropriate-region
;  attempt-to-extend-fixed-area
;  attempt-to-close-over-a-memory-location
;  new-nonexistent-instance-variable

(defflavor trap () ()
  :abstract-flavor
  (:included-flavors condition)
  ;;>> this is really a defflavor bug
  ;;>>  --- it should notice the method-combinations from included flavors
  ;;>> FIX!
  (:method-combination (:case :base-flavor-last
                              :proceed
                              :proceed-asking-user
                              :document-proceed-type
                              :proceed-ucode-with-args))
  (:documentation "Error signalled by the microcode"))
(defmethod (trap :default :ucode-proceed-types) ())

(defflavor random-trap () (trap format-condition-mixin error))
(defflavor random-dangerous-trap () (random-trap dangerous-condition))

(defmethod (trap :print-error-message-prefix) (sg brief stream &aux tem flag)
  (if brief
      (princ ">>TRAP: " stream)
    (let (trap-micro-pc ete saved-micro-pcs)
      (setf (list* trap-micro-pc ete 'a 'a saved-micro-pcs)
            (symeval-in-stack-group '*ucode-error-status* sg))
      (format stream ">>TRAP ~A ~A"
              trap-micro-pc ete)
      (dolist (pc saved-micro-pcs)
        (when (setq tem (assq (1- (%pointer pc)) calls-sub-list))
          (or flag (format stream " ->"))
          (setq flag t)
          (format stream "  ~A " (cdr tem))))
      ;show whole stack if active function is micro-compiled
;we need something like this, but it needs to be smarter about errors
;      (if (fboundp 'compiler:show-micro-stack)
;         (compiler:show-micro-stack stream sg))
      (terpri))))

;;; Conventions for the error handler routines:  how to add new ones.
;;;
;;; Each place in the microcode where TRAP can be called is followed by
;;; an ERROR-TABLE pseudo-op.  This should appear at the PC which is
;;; going to be TRAP's return address.  An example is
;;;    (ERROR-TABLE ARGTYP FIXNUM M-T 0)
;;; (for example).  The CDR of this list is the ETE.  So, the FIRST element
;;; is the name of the error, and the SECOND is the first "argument" to that
;;; error's associated routines.
;;;
;;; All ETEs should be a list whose car is a symbol.
;;; That symbol should be defined in a DEF-UCODE-ERROR in this file.
;;; Within the DEF-UCODE-ERROR, the variable ETE can be used to
;;; refer to the entire ETE list.

(defmacro def-ucode-error (trap-name condition-flavor &body init-options)
  (let ((condition-flavor (if (consp condition-flavor) (car condition-flavor) condition-flavor))
        (condition-names (cdr-safe condition-flavor)))
    `(defun (:property ,trap-name make-ucode-error-function) (ignore sg ete)
       (declare (function-parent ,trap-name def-ucode-error))
       sg ete
       (make-instance ',condition-flavor
                      ,@(if (consp condition-flavor) `(:condition-names ',condition-names))
                      ,@init-options))))

(defun make-ucode-error (error-name sg ete)
  (let ((error-fctn (get error-name 'make-ucode-error-function)))
    (if (null error-fctn)
        (ferror "A ~s trap was received from the microcode, which does not have a make-ucode-error-function associated with it!" error-name)
      (funcall error-fctn error-name sg ete))))

(defmacro def-ucode-format-error (error-name &body format-args)
  (declare (arglist error-name error-flavor format-string &body format-args))
  `(def-ucode-error ,error-name random-trap
     :format-string ,(car format-args)
     :format-args (list . ,(cdr format-args))))

(defun sg-proceed-micro-pc (sg tags)
  "Restart SG from an error in the microcode, at micro-pc determined by TAG.
TAG may be the name of an (ERROR-TABLE RESTART tag) in the microcode source.
If TAG is NIL, restart at the pc where the error happened.
If a PROCEED routine doesn't call SG-PROCEED-MICRO-PC, then
control will be returned from the micro-routine that got the error."
  (setq tags (if (consp tags) tags (list tags)))        ;yes, consp not cl:listp
  ;; Operates by pushing the specified PC onto the saved microstack,
  ;; since resuming the stack group will do a POPJ.
  (dolist (tag tags)
    (let ((pc (if tag (cdr (assq tag restart-list)) (1+ (sg-trap-micro-pc sg)))))
      (when (null pc)
        (bad-hacker tag " no such restart!")
        (throw 'quit nil))
      ;; Since the micro stack is saved backwards, the top of the stack is buried
      ;; where it is hard to get at.
      (let ((rp (sg-regular-pdl sg))
            (sp (sg-special-pdl sg))
            (spp (sg-special-pdl-pointer sg))
            (frame (sg-ap sg)))
        (or (zerop (rp-micro-stack-saved rp frame))     ;Shuffle up stack to make room
            (do ((flag 0)) ((not (zerop flag)))
              (setf (aref sp (1+ spp)) (aref sp spp))
              (%p-dpb 0 %%specpdl-block-start-flag (locf (aref sp (1+ spp))))
              (setq flag (%p-ldb %%specpdl-block-start-flag (locf (aref sp spp))))
              (setq spp (1- spp))))
        (setf (aref sp (setq spp (1+ spp))) pc)
        (%p-dpb 1 %%specpdl-block-start-flag (locf (aref sp spp)))
        (setf (sg-special-pdl-pointer sg) (1+ (sg-special-pdl-pointer sg)))
        (setf (rp-micro-stack-saved rp frame) 1)
        (setf (rp-attention rp frame) 1)))))

(defun sg-data-type (sg register)
  (%p-data-type (sg-locate sg register)))

(defun sg-pointer-careful (sg register)
  (let* ((location (sg-locate sg register)))
    (cond ((%p-contents-safe-p location)
           (contents location))
          ((%p-pointerp location)
           (%p-contents-as-locative location))
          (t
           (%p-pointer location)))))

(def-ucode-error argtyp arg-type-error
  :type-specifier (description-type-spec (second ete))
  :arg-location-in-sg (third ete)
  :arg-pointer (sg-pointer-careful sg (third ete))
  :arg-data-type (sg-data-type sg (third ete))
  ;; arg-number is T if it was the only arg
  :arg-number (fourth ete)
  :restart-tag (fifth ete)
  :function (or (sixth ete)
                (and (eq (fifth ete) 'array-decode-n-error-restart)
                     (not (memq (sg-erring-function sg) '(aref aset aloc)))
                     (aref (sg-regular-pdl sg) (sg-ipmark sg)))
                (sg-erring-function sg)))

(defflavor arg-type-error
        (function arg-number arg-pointer arg-data-type arg-location-in-sg restart-tag)
        (trap wrong-type-value)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (arg-type-error :after :init) (ignore)
  (setq place (and arg-number
                   (neq arg-number 't)
                   function
                   (fboundp function) (nth arg-number (arglist function)))))

(defmethod (arg-type-error :arg-name) ()
  (send self :place))

(defmethod (arg-type-error :arg-number) ()
  (and arg-number (if (eq arg-number 't) 0 arg-number)))

;; this is what wrong-type-value conditions expect
(defmethod (arg-type-error :value) ()
  (%make-pointer arg-data-type arg-pointer))
;; I don't know what this is used by.
(defmethod (arg-type-error :old-value) ()
  (%make-pointer arg-data-type arg-pointer))

(defmethod (arg-type-error :report) (stream)
  (format:output stream
    (format t "~:[Some~*~*~;The~:[ ~:R~;~*~]~] argument to ~S, "
            arg-number (eq arg-number t) (and (numberp arg-number) (1+ arg-number))
            function)
    (if (null arg-data-type)
        (format t "whose value has not been preserved")
      (unless (condition-case ()
                  (prin1 (%make-pointer arg-data-type arg-pointer))
                (:no-error t)
                (error nil))
        (printing-random-object (nil *standard-output*)
          (format t "~A #o~O" (nth arg-data-type q-data-types) (%pointer arg-pointer)))))
    (format t (if (arrayp function) ", was an invalid array subscript.~%Use "
                ", was of the wrong type.~%The function expected "))
    (princ description)
    "."))


;;; Translate symbols appearing in ARGTYP entries into type specs.
;;; Those that do not appear in the alist are left unchanged.
;;; SYMBOL-OR-LOCATIVE is translated because that's as good as defining it.
;;; The others are translated because they are global and we don't want
;;; to define any global type specs that aren't documented.
(defvar *description-type-specs*
        '((plusp (non-complex-number (0)))
          (nonnegative-fixnum (fixnum 0))
          (positive-fixnum (fixnum (0)))
          (area si::area)
          ;(nil null)
          (non-nil (not null))
          (symbol-or-locative (or symbol locative))
          (art-q-array (array t))))

(defun description-type-spec (desc)
  (if (symbolp desc)
      (or (cadr (assq desc *description-type-specs*))
          desc)
    `(or . ,(mapcar #'description-type-spec desc))))

;;; Define, as type specs, all the symbols that appear in ARGTYP entries.
;;; Specify pretty names for those for which the default is not pretty.
;;; For some, we must define new predicates to make them work in TYPEP.

(deftype fixnum-field () '(satisfies fixnum-byte-spec-p))
(defun fixnum-byte-spec-p (x)
  (and (fixnump x)
       (< (byte-size x) %%q-pointer)))
(defprop fixnum-field "a byte spec for a field that fits in a fixnum" si::type-name)

(deftype art-q-list-array () '(and array (satisfies art-q-list-array-p)))
(defun art-q-list-array-p (array)
  (eq (array-type array) 'art-q-list))
(defprop art-q-list-array "an ART-Q-LIST array" si::type-name)

(deftype non-displaced-array () '(and array (not (satisfies array-displaced-p))))
(defprop non-displaced-array "a non-displaced array" si::type-name)

(deftype reasonable-size-array () '(and array (satisfies array-size-reasonable-p)))
(defun array-size-reasonable-p (array)
  (declare (ignore array))
  t)                                            ; I wonder what uses this...

(deftype numeric-array () '(and array (not (array t))))
(defprop numeric-array "an array which cannot contain arbitrary lisp data." si::type-name)

;(deftype fixnum-greater-than-0 () '(and fixnum (integer 1)))
;(deftype fixnum-greater-than-1 () '(and fixnum (integer 2)))

;(deftype q-array () '(array t))
;(deftype byte-array () '(and array (not (array t))))

;(deftype art-4b-array () '(array (mod 16.)))
;(defprop art-4b-array "an ART-4B array" si::type-name)

;(deftype art-16b-array () '(array (unsigned-byte 16.)))
;(defprop art-16b-array "an ART-16B array" si::type-name)


(defmethod (arg-type-error :ucode-proceed-types) ()
  (if restart-tag '(:argument-value)))

(defmethod (arg-type-error :case :proceed-asking-user :argument-value)
           (continuation read-object-function)
  "Use a different argument.  You type an expression for the new value."
  (funcall continuation :argument-value
           (funcall read-object-function :eval-read
                    "Form to evaluate and use as replacement argument: ")))

(defmethod (arg-type-error :case :proceed-ucode-with-args :argument-value)
           (sg value-to-proceed-with &rest ignore) ;avoid using value as arg because its an instance var.
  (sg-store value-to-proceed-with sg arg-location-in-sg)
  (sg-proceed-micro-pc sg (and (neq restart-tag 'fall-through) restart-tag)))

;>> This seems pretty bogoid.  Don't have to ucode with me now, though.
(def-ucode-error flonum-no-good (arg-type-error wrong-type-argument) ;huh? (wrong-type-argument..)
  :description 'integer
  :arg-location-in-sg nil
  :arg-pointer nil
  :arg-data-type nil
  :arg-number nil
  :restart-tag nil
  :function (sg-erring-function sg))


;;; FIXNUM-OVERFLOW
;;; First arg is M-T to show that that is where the value should
;;;   get stored.  Maybe it will someday be other things, too.
;;; Second is either PUSH or NOPUSH.
;;; Recover by storing a new value in the place where the
;;;   value would have been stored if it hadn't overflowed.
;;;   This is M-T, and also the regpdl if the second arg is PUSH.
;;;   Force return from the microroutine executing at the time.
; not used 5-Jan-86
;(def-ucode-error fixnum-overflow fixnum-overflow-error
;  :function (sg-erring-function sg)
;  :operands (list (sg-contents sg (second ete)))
;  :number (sg-contents sg (second ete))
;  :location-in-sg (second ete)
;  :push-new-value-flag
;    (progn (or (memq (third ete) '(push nopush))
;              (bad-hacker ete "Bad ETE, must be PUSH or NOPUSH."))
;          (third ete)))

;(defflavor fixnum-overflow (function location-in-sg push-new-value-flag number)
;          (trap arithmetic-error)
;  :gettable-instance-variables :initable-instance-variables)

;(defmethod (fixnum-overflow :report) (stream)
;  (format stream "~S got a fixnum overflow." function))

;(defmethod (fixnum-overflow :ucode-proceed-types) ()
;  '(:new-value))

;(defmethod (fixnum-overflow :case :proceed-asking-user :new-value)
;          (continuation read-object-function &aux num)
;  "Return a value specified by you.  You type an expression for the new value."
;  (setq num (funcall read-object-function :eval-read
;                    "Form to evaluate to get fixnum to return instead: "))
;  (check-type num fixnum)
;  (funcall continuation :new-value num))

;(defmethod (fixnum-overflow :case :proceed-ucode-with-args :new-value) (sg value &rest ignore)
;  (sg-fixnum-store value sg location-in-sg)
;  (and (eq push-new-value-flag 'push)
;       (sg-regpdl-push sg value)))


(defflavor floating-point-exception (small-float-p function) (trap arithmetic-error)
  :initable-instance-variables :gettable-instance-variables
  :abstract-flavor)
(defmethod (floating-point-exception :operation) ()
  (function-name function))
; :operands, :non-trap-result

;;; FLOATING-EXPONENT-UNDERFLOW
;;; Arg is SFL or FLO

(def-ucode-error floating-exponent-underflow floating-exponent-underflow
  :small-float-p (eq (second ete) 'sfl)
  :function (sg-erring-function sg))

(defflavor floating-exponent-underflow ()
           (floating-point-exception)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (floating-exponent-underflow :report) (stream)
  (format stream "~S produced a result too small in magnitude to be a ~:[~;short~] float."
          function
          small-float-p))

; Should have :new-value proceed type
(defmethod (floating-exponent-underflow :ucode-proceed-types) ()
  '(:use-zero))

(defmethod (floating-exponent-underflow :case :proceed-asking-user :use-zero)
           (continuation read-object-function)
  "Use 0.0 as the result."
  (when (funcall read-object-function
                 '(:fquery :list-choices nil :fresh-line nil)
                 "Proceed using 0.0~:[s~;f~]0 as the value instead? "
                 (not small-float-p))
    (funcall continuation :use-zero)))

(defmethod (floating-exponent-underflow :case :proceed-ucode-with-args :use-zero)
           (sg &rest ignore)
  (sg-proceed-micro-pc sg nil))

;;; FLOATING-EXPONENT-OVERFLOW
;;; Result is to be placed in M-T and pushed on the pdl.
;;; Arg is SFL or FLO
;;; In the case of SFL the pdl has already been pushed.

(def-ucode-error floating-exponent-overflow floating-exponent-overflow
  :small-float-p (eq (second ete) 'sfl)
  :function (sg-erring-function sg))

(defflavor floating-exponent-overflow ()
           (floating-point-exception)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (floating-exponent-overflow :report) (stream)
  (format stream "~S produced a result too large in magnitude to be a ~:[~;short~] float."
          function
          small-float-p))

(defmethod (floating-exponent-overflow :ucode-proceed-types) ()
  '(:new-value))

(defmethod (floating-exponent-overflow :case :proceed-asking-user :new-value)
           (continuation read-object-function &aux num)
  "Use a float specified by you as the result."
  (do-forever
    (setq num (funcall read-object-function :eval-read
                       (if small-float-p "Form evaluating to short-float to return instead: "
                         "Form evaluating to float to return instead: ")))
    (cond ((and small-float-p
                (small-floatp num))
           (return nil))
          ((floatp num)
           (return nil)))
    (format t "Please use a ~:[~;short~] float.~%" small-float-p))
  (funcall continuation :new-value num))

(defmethod (floating-exponent-overflow :case :proceed-ucode-with-args :new-value)
           (sg value &rest ignore)
  (sg-store value sg 'm-t)
  (and small-float-p
       (sg-regpdl-pop sg))
  (sg-regpdl-push value sg))


;;; DIVIDE-BY-ZERO
;;; You cannot recover.
;;; The second element of the ETE can be the location of the dividend.

(defun (:property divide-by-zero make-ucode-error-function) (ignore sg ete)
  (declare (ignore ete))
  (if (eq (sg-erring-function sg) '^)
      (make-instance 'illegal-expt-trap :base 'unknown :exponent (sg-contents sg 'pp))
    (apply #'make-instance 'divide-by-zero
           :function (sg-erring-function sg)
           ;(if (second ete) `(:dividend ,(sg-contents sg (second ete))))
           ()
           )))

(defflavor divide-by-zero (function) (trap arithmetic-error)
  :initable-instance-variables :gettable-instance-variables)
(defmethod (divide-by-zero :report) (stream)
  (format stream "There was an attempt to divide a number by zero in ~S." function))

(defflavor illegal-expt-trap () (trap illegal-expt))

(defflavor bad-array-mixin (array)
           ()
  :abstract-flavor
  (:required-flavors error)
  :gettable-instance-variables :initable-instance-variables
  (:method-combination (:case :base-flavor-last
                              :proceed
                              :proceed-asking-user
                              :document-proceed-type
                              ;>> This is a kludge.  It should only be on trap,
                              ;>>  but some compile-flavor-methods lossage is getting in my
                              ;>>  way on this cold-load.  Excise it when convenient.
                              :proceed-ucode-with-args)))

(defmethod (bad-array-mixin :case :proceed-asking-user :new-array)
           (continuation read-object-function)
  "Use a different array.  You type an expression for the array to use."
  (funcall continuation :new-array
           (funcall read-object-function :eval-read
                    "Form to eval to get array to use instead: ")))

(defflavor bad-array-trap (array-location-in-sg
                           (restart-tag nil)
                           function)
           (bad-array-mixin trap error)
  :initable-instance-variables :gettable-instance-variables)

(defmethod (bad-array-trap :ucode-proceed-types) ()
  (if restart-tag '(:new-array)))

(defmethod (bad-array-trap :case :proceed-ucode-with-args :new-array)
           (sg -array-)
  (sg-store -array- sg array-location-in-sg)
  (sg-proceed-micro-pc sg restart-tag))

;;; ARRAY-NUMBER-DIMENSIONS
;;; First arg is no longer used.
;;; Second arg is how many dimensions we want (as a constant), or NIL if variable number.
;;; Third arg is the array
;;; Fourth arg is restart tag.
;;; Fourth arg is QARYR if this is array called as function.

(def-ucode-error array-number-dimensions
                 (array-number-dimensions-error array-wrong-number-of-dimensions)
  :array (sg-contents sg (fourth ete))
  :array-location-in-sg (fourth ete)
  :subscripts-given (array-number-dimensions-subscript-list sg ete)
  :restart-tag (fifth ete))

(defun array-number-dimensions-subscript-list (sg ete)
  (let* ((rp (sg-regular-pdl sg)))
    (if (not (fixnump (third ete)))
        ;; This is AREF, ALOC, ASET or array called as function.
        (do ((p (sg-regular-pdl-pointer sg) (1- p))
             subscripts
             (c (sg-fixnum-contents sg 'm-r) (1- c)))
            (( c 0) subscripts)
          (push (aref rp p) subscripts))
      ;; (THIRD ETE) tells us whether the error was from ARRAY-DECODE-1, -2 or -3.
      (case (third ete)
        (1 (list (sg-fixnum-contents sg 'm-q)))
        (2 (list (sg-contents sg 'm-j)
                 (sg-contents sg 'm-q)))
        (3 (list (sg-contents sg 'm-i)
                 (sg-contents sg 'm-j)
                 (sg-contents sg 'm-q)))))))

(defflavor array-number-dimensions-error
        (subscripts-given)
        (bad-array-trap)
  :initable-instance-variables :gettable-instance-variables)

(defmethod (array-number-dimensions-error :number-of-subscripts-expected) ()
  (array-rank array))

(defmethod (array-number-dimensions-error :number-of-subscripts-given) ()
  (length subscripts-given))

(defmethod (array-number-dimensions-error :report) (stream)
  (format stream
          ;; Was this array applied or aref'ed?
          (if (eq restart-tag 'qaryr)
              "The ~D-dimensional array ~S was erroneously applied to ~D argument~:P ~S."
            "The ~D-dimensional array ~S was given ~D subscript~:P: ~S.")
          (array-rank array) array
          (length subscripts-given)
          subscripts-given))


;;; NUMBER-ARRAY-NOT-ALLOWED
;;; First arg is where to find the array.
;;; Second arg is restart tag for new array.

(def-ucode-error number-array-not-allowed number-array-not-allowed
  :array (sg-contents sg (second ete))
  :array-location-in-sg (second ete)
  :restart-tag (third ete)
  :referencing-function (sg-erring-function sg))

(defflavor number-array-not-allowed (referencing-function) (bad-array-trap)
  :initable-instance-variables :gettable-instance-variables)
(defmethod (number-array-not-allowed :report) (stream)
  (format stream  "The array ~S, which was given to ~S, is not allowed to be a number array."
          array referencing-function))

;;; INDIVIDUAL-SUBSCRIPT-OOB
;;; First arg is location of array
;;; second arg is dimension number.
;;; We assume that the current frame's args are the array and the subscripts,
;;; and find the actual losing subscript that way.
; not used 5-Jan-86
;(def-ucode-error individual-subscript-oob (subscript-error subscript-out-of-bounds)
;  :function (sg-erring-function sg)
;  :object (sg-contents sg (second ete))
;  :subscripts-given (subscript-oob-subscript-list sg ete)
;  :dimension-number (sg-contents sg (third ete))
;  :restart-tag (fourth ete))

;;; SUBSCRIPT-OOB
;;; First arg is how many we gave.
;;; Second is the legal limit.
;;; Third optional arg is a restart tag.
;;;  It can also be a list of restart tags.  Then their addresses are pushed sequentially.
;;;  This is used to get the effect of making the microcode restart by calling
;;;  a subroutine which will return to the point of the error.
;;; Fourth optional arg is where the array is.
;;; Fifth is either T if indices are on the stack,
;;;  or 1 if this is AR-1-FORCE or such like and there is only one index (first arg says where)
;;;  or missing if this is AR-1, AR-2, AR-3 and the array's rank should be
;;;  used to decide where the args are.

(def-ucode-error subscript-oob (subscript-error subscript-out-of-bounds)
  :function (sg-erring-function sg)
  :index-location-in-sg (second ete)
  :subscript-used (sg-fixnum-contents sg (second ete))
  :subscripts-given (subscript-oob-subscript-list sg ete)
  :subscript-limit (sg-fixnum-contents sg (third ete))
  :restart-tag (fourth ete)
  :object (if (fifth ete) (sg-contents sg (fifth ete))))

(defun subscript-oob-subscript-list (sg ete)
  (let* ((object (if (fifth ete) (sg-contents sg (fifth ete))))
         (frame (sg-ipmark sg))
         (rp (sg-regular-pdl sg))
         (fn (rp-function-word rp frame)))
    (if (locativep object)      ;for reasons having to do with array-caching,
                                ; the ucode changes the data-type to DTP-LOCATIVE
                                ; for displaced or indirect arrays.
        (setq object (%make-pointer dtp-array-pointer object)))
    (if (and (arrayp object) (neq (sixth ete) 1))
        (if (sixth ete)
            ;; This is AREF, ALOC, ASET or array called as function.
            (do ((p (sg-regular-pdl-pointer sg) (1- p))
                 subscripts
                 (limit (+ frame (cond ((eq fn #'aset) 2)
                                       ((typep fn 'array) 0)
                                       (t 1)))))
                ((= p limit) subscripts)
              (push (aref rp p) subscripts))
          ;; It is AX-1, AX-2 or AX-3.  Since we are past the point of getting
          ;; a wrong-number-dimensions error, we can tell which by the rank of the array.
          ;; The ETE cannot distinguish since the errors come from the same spot.
;         (if array-index-order
          (let ((rank (array-rank object)))
            (case rank
              (1 (list (sg-fixnum-contents sg (second ete))))
              (2 (list (sg-contents sg 'm-j)
                       (- (sg-fixnum-contents sg (second ete))
                          (* (array-dimension object 1)
                             (sg-contents sg 'm-j)))))
              (3 (list (sg-contents sg 'm-i)
                       (sg-contents sg 'm-j)
                       (- (sg-fixnum-contents sg (second ete))
                          (* (array-dimension object 2)
                             (+ (sg-contents sg 'm-j)
                                (* (array-dimension object 1)
                                   (sg-contents sg 'm-i)))))))))
;           (let ((rank (array-rank object)))
;             (case rank
;               (1 (list (sg-fixnum-contents sg (second ete))))
;               (2 (list (- (sg-fixnum-contents sg (second ete))
;                           (* (array-dimension object 0)
;                              (sg-contents sg 'm-j)))
;                        (sg-contents sg 'm-j)))
;               (3 (list (- (sg-fixnum-contents sg (second ete))
;                           (* (array-dimension object 0)
;                              (+ (sg-contents sg 'm-j)
;                                 (* (array-dimension object 1)
;                                    (sg-contents sg 'm-i)))))
;                        (sg-contents sg 'm-j)
;                        (sg-contents sg 'm-i))))))
          )
      ;; If object is not known or not an array, or if AX-1-FORCE.
      (list (sg-fixnum-contents sg (second ete))))))

(defflavor subscript-error
         (function subscripts-given subscript-used index-location-in-sg
          subscript-limit restart-tag object (dimension-number nil))
         (trap error)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (subscript-error :report) (stream)
  (cond (dimension-number
         (format stream "The subscript for dimension ~D was ~S, which is out of range for ~S."
                 dimension-number (nth dimension-number subscripts-given) object))
        (object
         (if (locativep object)         ;see comment above.
             (setq object (%make-pointer dtp-array-pointer object)))
         (if (= (length subscripts-given) 1)
             (format stream "The subscript ~S for ~S was out of range in ~S."
                     subscript-used object function)
           (format stream "The subscripts ~S for ~S were out of range in ~S."
                   subscripts-given object function)))
        ((< subscript-used 0)
         (format stream "The index, ~S, was negative in ~S."
                 subscript-used function))
        (t
         (format stream "The index, ~S, was beyond the length, ~S, in ~S."
                 subscript-used subscript-limit function))))

(defmethod (subscript-error :ucode-proceed-types) ()
  '(:new-subscript))

(defmethod (subscript-error :case :proceed-asking-user :new-subscript)
           (continuation read-object-function)
  "Use different subscripts.  You type expressions for them."
  (if (or (not (arrayp object))
          ( (length subscripts-given) (array-rank object)))
      (let (num)
        (do-forever
          (setq num (funcall read-object-function :eval-read
                             "Form evaluating to index to use instead: "))
          (if (and (integerp num)
                   (< -1 num subscript-limit))
              (return (values)))
          (format t "Please use a positive fixnum less than ~D.~%" subscript-limit))
        (funcall continuation :new-subscript num))
    (do ((i 0 (1+ i))
         subscripts)
        ((= i (length subscripts-given))
         (apply continuation :new-subscript (nreverse subscripts)))
      (push (funcall read-object-function :eval-read
                     " Subscript ~D: " i)
            subscripts))))

(defmethod (subscript-error :case :proceed-ucode-with-args :new-subscript)
           (sg &rest subscripts)
  (if dimension-number
      ;; For error in ARRAY-ROW-MAJOR-INDEX,
      ;; store back all the subscripts into the frame.
      ;; We will restart the function to examine them.
      (do ((tail subscripts (cdr tail))
           (i 0 (1+ i)))
          ((= i (length subscripts-given)))
        (setf (aref (sg-regular-pdl sg)
                    (+ (sg-ap sg) i 2))
              (car tail)))
    ;; Errors based on the cumulative index: store the updated cumulative index.
    (sg-fixnum-store
;     (if array-index-order
      (do ((i 0 (1+ i))
           (index 0)
           (rest subscripts (cdr rest)))
          ((null rest) index)
        (setq index (* index (array-dimension object i)))
        (incf index (car rest)))
;       (do ((i (array-rank object) (1- i))
;            (index 0)
;            (rest (reverse subscripts) (cdr rest)))
;           ((null rest) index)
;         (setq index (* index (array-dimension object (1- i))))
;         (incf index (car rest))))
      sg
      index-location-in-sg))
  (if (consp restart-tag)
      (dolist (tag restart-tag)
        (sg-proceed-micro-pc sg tag))
    (sg-proceed-micro-pc sg restart-tag)))

;;; First arg is where to find array, second is where to find dimension number.
;>> this is inadequate -- needs bad-array-mixin stuff
(def-ucode-format-error bad-array-dimension-number
  "The dimension number ~S is out of range for ~S."
  (sg-fixnum-contents sg (third ete))
  (sg-contents sg (second ete)))


;;; BAD-ARRAY-TYPE
;;; First arg is where array header is. Note that it may well have a data type of DTP-TRAP.
;;; You cannot recover.

(def-ucode-format-error bad-array-type
  "The array type, ~S, was invalid in ~S."
  (ldb %%array-type-field (%p-pointer (sg-locate sg (second ete))))
  (sg-erring-function sg))


;;; ARRAY-HAS-NO-LEADER
;;; First arg is where array pointer is.
;;; Second arg is restart tag for new array.

(def-ucode-error array-has-no-leader array-has-no-leader
  :array (sg-contents sg (second ete))
  :array-location-in-sg (second ete)
  :restart-tag (third ete)
  :function (sg-erring-function sg))
(defflavor array-has-no-leader (function) (bad-array-trap)
  :initable-instance-variables)
(defmethod (array-has-no-leader :report) (stream)
  (format stream "The array given to ~S, ~S, has no leader."
          function array))

;;; FILL-POINTER-NOT-FIXNUM
;;; First arg is where array pointer is.
;;; Second arg is restart tag for new array.

(def-ucode-error fill-pointer-not-fixnum bad-array-trap
  :array (sg-contents sg (second ete))
  :array-location-in-sg (second ete)
  :restart-tag (third ete)
  :format-string
  :format-args (list (sg-erring-function sg) (sg-contents sg (second ete))))
(defflavor fill-pointer-not-fixnum (function) (bad-array-trap)
  :initable-instance-variables)
(defmethod (fill-pointer-not-fixnum :report) (stream)
  (format stream  "The fill-pointer of the array given to ~S, ~S, is not a fixnum."
          function array))


;;;; More random losses.

;;; IALLB-TOO-SMALL
; not used 5-Jan-86
;; First arg is how many we asked for.
;(def-ucode-format-error iallb-too-small
;  "There was a request to allocate ~S cells."
;  (sg-fixnum-contents sg (second ete)))
;
;(def-ucode-format-error cons-zero-size
;  "There was an attempt to allocate zero storage by ~S."
;  (sg-erring-function sg))

;>> this is a bit of a crock.  Differs from real invalid-function in that instead of
;>>  calling out to apply-lambda with the losing function, it errs when pushing the
;>>  "function" on the pdl
(def-ucode-error number-called-as-function invalid-function-trap
  :function (sg-contents sg (second ete)))
(defflavor invalid-function-trap () (invalid-function trap))
(defmethod (invalid-function-trap :ucode-proceed-types) ()
  '(:new-function))
(defmethod (invalid-function-trap :case :proceed-ucode-with-args :new-function)
           (sg new-function)
  (sg-store new-function sg 'm-a)
  (sg-proceed-micro-pc sg nil))


;;; WRONG-SG-STATE
;;; Arg is where sg is.
;;; You cannot recover.

;>>
(def-ucode-error wrong-sg-state (error wrong-stack-group-state)
  :format-string "The state of the stack group, ~S, given to ~S, was invalid.~%"
  :format-args (list (sg-contents sg (second ete))
                     (sg-erring-function sg)))

;;; SG-RETURN-UNSAFE
;;; No args, since the frob is in the previous-stack-group of the current one.
;;; You cannot recover.

(def-ucode-format-error sg-return-unsafe
  "An /"unsafe/" stack group attempted to ~S." 'stack-group-return)

;;; TV-ERASE-OFF-SCREEN
;;; No arg.
(def-ucode-format-error tv-erase-off-screen ;draw-off-end-of-screen
  "An attempt was made to do graphics past the end of the screen.")


;;; THROW-TAG-NOT-SEEN
;; This comes from throw-trap: below

;;>> See dj:mly;throw-trap
(defflavor throw-tag-not-seen (tag value count action)
           (trap error)
  :gettable-instance-variables :initable-instance-variables)

(defmethod (throw-tag-not-seen :values) ()
  (list value))

(defmethod (throw-tag-not-seen :report) (stream)
  (format stream "There was no pending ~S for the tag ~S."
          'catch tag))

(defmethod (throw-tag-not-seen :after :print-error-message) (sg brief stream)
  (declare (ignore sg))
  (unless brief
    (format stream "The value being thrown was ~S." value)
    (and (or count action)
         (format stream "~&While in a *UNWIND-STACK with remaining count of ~D and action ~S."
                 count action))))

(defmethod (throw-tag-not-seen :ucode-proceed-types) ()
  '(:new-tag))

(defmethod (throw-tag-not-seen :case :proceed-asking-user :new-tag)
           (continuation read-object-function)
  "Throw to another tag.  You type an expression for it."
  (funcall continuation :new-tag
           (funcall read-object-function :eval-read "Form evaluating to tag to use instead: ")))

(defmethod (throw-tag-not-seen :case :proceed-ucode-with-args :new-tag)
           (sg new-tag)
  (sg-store new-tag sg 'm-a)
  (sg-proceed-micro-pc sg ()))


;;; MVR-BAD-NUMBER
;;; Where the # is.

; not used 5-Jan-86
;(def-ucode-format-error mvr-bad-number
;  "The function attempted to return ~D. values."
;  (sg-fixnum-contents sg (second ete)))

(def-ucode-format-error zero-args-to-select-method
  "~S was applied to no arguments."
  (sg-contents sg (second ete)))

(defflavor select-method-not-found () (trap unclaimed-message))
(def-ucode-error selected-method-not-found select-method-not-found
  :object (sg-contents sg (second ete))
  :message (sg-contents sg (third ete))
  :arguments (cdr (sg-accumulated-arguments sg)))

(defun sg-accumulated-arguments (sg)
  (do ((idx (1+ (sg-ipmark sg)) (1+ idx))
       (limit (sg-regular-pdl-pointer sg))
       (rp (sg-regular-pdl sg))
       args)
      ((> idx limit)
       (nreverse args))
    (push (aref rp idx) args)))

(def-ucode-format-error select-method-garbage-in-select-method-list
  "The weird object ~S was found in a select-method alist."
  (sg-contents sg (second ete)))

(def-ucode-format-error select-method-bad-subroutine-call
  "A bad /"subroutine call/" was found inside ~S."
  (sg-contents sg (second ete)))

(def-ucode-format-error no-mapping-table
  "Flavor ~S is not a component of SELF's flavor, ~S,
on a call to a function which assumes SELF is a ~S."
  (si:fef-flavor-name (aref (sg-regular-pdl sg) (sg-ap sg)))
  (type-of (symeval-in-stack-group 'self sg))
  (si:fef-flavor-name (aref (sg-regular-pdl sg) (sg-ap sg))))

(def-ucode-format-error no-mapping-table-1
  "SYS:SELF-MAPPING-TABLE is NIL in a combined method.")

(def-ucode-format-error self-not-instance
  "A method is referring to an instance variable,
but SELF is ~S, not an instance."
  (symeval-in-stack-group 'self sg))

;;; Signaled by LOCATE-IN-INSTANCE
(def-ucode-format-error instance-lacks-instance-variable
  "There is no instance variable ~S in ~S."
  (sg-contents sg (second ete))
  (sg-contents sg (third ete)))

;This is no longer called.
; Instead, new-nonexistent-instance-variable is the guy.
; Unfortunately, that is not handled.
(def-ucode-format-error nonexistent-instance-variable
  "Compiled code referred to instance variable ~S, no longer present in flavor ~S."
; was ... (si:flavor-decode-self-ref-pointer <flavor> (%p-pointer (sg-saved-vma sg)))
  (si:flavor-decode-self-ref-pointer (si:fef-flavor-name (aref (sg-regular-pdl sg) (sg-ap sg)))
                                     (sg-regpdl-pop sg))
  (si:fef-flavor-name (aref (sg-regular-pdl sg) (sg-ap sg))))

(def-ucode-format-error micro-code-entry-out-of-range
  "MISC-instruction ~S is not an implemented instruction."
  (sg-fixnum-contents sg (second ete)))

(def-ucode-format-error bignum-not-big-enough-dpb
  "There is an internal error in bignums; please report this bug.")

(def-ucode-format-error bad-internal-memory-selector-arg
  "~S is not valid as the first argument to %WRITE-INTERNAL-PROCESSOR-MEMORIES."
  (sg-fixnum-contents sg (second ete)))

(def-ucode-format-error bitblt-destination-too-small
  "The destination of a BITBLT was too small.")

(def-ucode-format-error bitblt-array-fractional-word-width
  "An array passed to BITBLT has an invalid width.
The width, times the number of bits per pixel, must be a multiple of 32.")

(defflavor write-in-read-only (address) (trap error)
  :gettable-instance-variables :initable-instance-variables)
(defmethod (write-in-read-only :report) (stream &aux name)
  (format stream " There was an attempt to write into #o~O, which is a read-only address.~
                ~% #o~O is in area #~D, ~S"
          address
          address
          (%area-number address)
          (setq name (area-name (%area-number address))))
  (if (eq name 'macro-compiled-program)
      (format stream "~% Compiled-in LISTS and STRINGS are consed in the MACRO-COMPILED-PROGRAM area,~
                      ~% and attempts to modify them are often the source of this error.")))
(def-ucode-error write-in-read-only write-in-read-only
  :address (sg-contents sg (second ete)))

(def-ucode-error turd-alert (turd-alert-error draw-on-unprepared-sheet)
  :sheet (sg-contents sg (second ete)))

(defflavor turd-alert-error (sheet) (trap error)
  :gettable-instance-variables :initable-instance-variables)
(defmethod (turd-alert-error :report) (stream)
  (format stream "There was an attempt to draw on the sheet ~S without preparing it first.~%"
          sheet))

(defmethod (turd-alert-error :ucode-proceed-types) ()
  '(:no-action))

(defmethod (turd-alert-error :case :proceed-asking-user :no-action) (continuation ignore)
  "Proceed, perhaps writing garbage on the screen."
  (funcall continuation :no-action))

(defmethod (turd-alert-error :case :proceed-ucode-with-args :no-action) (sg &rest ignore)
  (sg-proceed-micro-pc sg nil))

(def-ucode-format-error physical-address-not-in-sys-conf
 "The nubus physical address #x~16r is not part of the system configuration structure."
 (sg-contents sg (second ete)))

(def-ucode-format-error virtual-address-not-in-sys-conf
 "The virtual address ~o is not part of the system configuration structure."
 (sg-fixnum-contents sg (second ete)))


;;;; General Machine Lossages.

;;; PDL-OVERFLOW
;;; Arg is either SPECIAL or REGULAR

(def-ucode-error pdl-overflow pdl-overflow
  :pdl-name (cdr (assq (second ete) '((regular . :regular) (special . :special)))))

(defflavor pdl-overflow (pdl-name)
           (trap debugger-condition)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (pdl-overflow :report) (stream)
  (format stream "The ~A push-down list has overflown."
          (cadr (assq pdl-name '((:regular "regular") (:special "special"))))))

(defmethod (pdl-overflow :ucode-proceed-types) () '(:grow-pdl))

(defmethod (pdl-overflow :case :proceed-asking-user :grow-pdl) (continuation ignore)
  "Make the stack larger and proceed."
  (funcall continuation :grow-pdl))

(defmethod (pdl-overflow :case :proceed-ucode-with-args :grow-pdl) (sg &rest ignore)
  (format t "Continuing with more pdl.~%")
  (sg-maybe-grow-pdls sg t nil nil t)   ;Make very sure that there is enough room
  (sg-proceed-micro-pc sg nil))         ;Then continue after ucode check for room


;;; ILLEGAL-INSTRUCTION
;;; No args.

(defflavor illegal-instruction (compiled-function pc) (trap error)
  :initable-instance-variables :gettable-instance-variables)
(defmethod (illegal-instruction :report) (stream)
  (if pc
      (format stream "Illegal instruction #o~O at pc ~D in ~S (~S)"
              (fef-instruction compiled-function pc) pc compiled-function
              (string-trim '(#/space #/newline #/tab)
                           (with-output-to-string (*standard-output*)
                             (compiler:disassemble-instruction compiled-function pc))))
    (write-string "There was an attempt to execute an invalid instruction." stream)))
(defun (:property illegal-instruction make-ucode-error-function) (ignore sg ignore)
  (let ((fef (aref (sg-regular-pdl sg) (sg-ap sg)))
        (pc (1- (rp-exit-pc (sg-regular-pdl sg) (sg-ap sg)))))
    (make-instance 'illegal-instruction
                   :compiled-function fef
                   :pc (if (and (compiled-function-p fef)
                                (< (floor pc 2) (%structure-total-size fef)))
                           pc
                         nil))))


; M-1 contains nubus physical address
; M-2 contains the data (on a write)
; M-A has cycle code:
;       1 = %nubus-read
;       2 = %nubus-read-byte
;       3 = %nubus-write
;       4 = %nubus-write-byte
; M-B has low 16 bits of memory status register
; (M-A, M-B have 0 in their data-types)
; No ucode generates this 5-Jan-86.  What's the story
(defun (:property nubus-error make-ucode-error-function) (ignore sg ignore)
  (let ((phys-adr (sg-contents sg 'M-1))
        (data (sg-contents sg 'M-2))
        (cycle-type (cdr (assq (sg-fixnum-contents sg 'M-A)
                               '((1 . :read)
                                 (2 . :read-byte)
                                 (3 . :write)
                                 (4 . :write-byte)))))
        (memory-status-reg (sg-fixnum-contents sg 'M-B))
        cycle-status)
    (setq cycle-status
          (cond ((ldb-test (byte 1 14.) memory-status-reg)
                 :nubus-timeout)
                ((ldb-test (byte 1 15.) memory-status-reg)
                 :parity-error)
                ((= (ldb (byte 2 6) memory-status-reg) 1)
                 :parity-error)
                ((= (ldb (byte 2 6) memory-status-reg) 2)
                 :nubus-timeout)
                (t
                 nil)))
    (make-instance 'nubus-error
                   :physical-address phys-adr
                   :data data
                   :cycle-type cycle-type
                   :cycle-status cycle-status)))

(defflavor nubus-error
           (physical-address data cycle-type cycle-status)
           (trap error)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (nubus-error :report) (stream)
  (format stream "Nubus error type ~a while trying to do ~a of #x~x."
          cycle-status cycle-type physical-address)
  (if (memq cycle-type '(:write :write-byte))
      (format stream "  The data was #x~x" data)))

(defmethod (nubus-error :ucode-proceed-types) ()
  (cond ((memq cycle-type '(:write :write-byte))
         '(:try-again :dont-write))
        (t
         '(:try-again :return-0))))

(defmethod (nubus-error :case :proceed-asking-user :try-again) (continuation ignore)
  "Try again."
  (funcall continuation :try-again))

(defmethod (nubus-error :case :proceed-asking-user :dont-write) (continuation ignore)
  "Proceed skipping the write operation."
  (funcall continuation :dont-write))

(defmethod (nubus-error :case :proceed-asking-user :return-0) (continuation ignore)
  "Proceed returning 0 from the read operation."
  (funcall continuation :return-0))

(defmethod (nubus-error :case :proceed-ucode-with-args :dont-write) (sg &rest ignore)
  (sg-proceed-micro-pc sg nil))

(defmethod (nubus-error :case :proceed-ucode-with-args :return-0) (sg &rest ignore)
  (sg-store 0 sg 'M-T)
  (sg-proceed-micro-pc sg nil))

;some day...
(defmethod (nubus-error :case :proceed-ucode-with-args :try-again) (sg &rest ignore)
  (sg-store 0 sg 'M-T)
  (sg-proceed-micro-pc sg nil)
  )


;;; BAD-CDR-CODE
;;; Arg is where loser is.
(def-ucode-format-error bad-cdr-code
  "A bad cdr-code was found in memory (at address ~O)."
  (sg-fixnum-contents sg (second ete)))  ;Can't use Lisp print since will err again

;;; DATA-TYPE-SCREWUP
;;; This happens when some internal data structure contains wrong data type.  arg is name.
;;; As it happens, all the names either start with a vowel or do if pronounced as letters
;;; Not continuable
(def-ucode-format-error data-type-screwup
  "A bad data-type was found in the internal guts of an ~A."
  (second ete))

;;; STACK-FRAME-TOO-LARGE
(def-ucode-format-error stack-frame-too-large
  "Attempt to make a stack frame larger than 256. words.")

;;; AREA-OVERFLOW
;;; arg is register containing area#
; This "feature" punted 851802
;(def-ucode-format-error area-overflow
;  "Allocation in the /"~A/" area exceeded the maximum of ~D. words."
;  (area-name (sg-fixnum-contents sg (second ete)))
;  (area-maximum-size (sg-fixnum-contents sg (second ete))))

;;; VIRTUAL-MEMORY-OVERFLOW
(def-ucode-error virtual-memory-overflow random-dangerous-trap
  :format-string "You've used up all available virtual memory!~%
This can happen even if you have the garbage collector on.
Perhaps increasing the frequency or volatility of GC flips will
help in the future.  (Try GC:GC-ON with an argument of 2 or 3)")

;;; RCONS-FIXED
; Not used 5-Jan-86
;(def-ucode-error rcons-fixed (error cons-in-fixed-area)
;  :property-list `(:area ,(area-name (sg-contents sg 'm-s)))
;  :format-string "There was an attempt to allocate storage in the fixed area ~S."
;  :format-args (list (area-name (sg-contents sg 'm-s))))

;;; REGION-TABLE-OVERFLOW
(def-ucode-error region-table-overflow random-dangerous-trap
  :format-string "Unable to create a new region because the region tables are full.")

;;; RPLACD-WRONG-REPRESENTATION-TYPE
;;; arg is first argument to RPLACD
; not used 5-Jan-86
;(def-ucode-format-error rplacd-wrong-representation-type
;  "Attempt to RPLACD a list which is embedded in a structure and therefore
;cannot be RPLACD'ed.  The list is ~S."
;  (sg-contents sg (second ete)))

;;;; Special cases.

;;; MAR-BREAK
;;; This code won't work if write-data is a DTP-NULL because of trap out of MAKUNBOUND

(defun mar-break-decode (sg ete)
  (let ((direction (cdr (assq (second ete) '((write . :write) (read . :read)))))
        object-data-type
        object-pointer
        value-data-type
        value-pointer
        value
        object offset
        value-has-dtp-null
        )
    ;first get the MD
    (setq value-data-type (sg-regpdl-pop sg))
    (setq value-pointer (sg-regpdl-pop sg))
    ;then the VMA
    (setq object-data-type (sg-regpdl-pop sg))
    (setq object-pointer (sg-regpdl-pop sg))

    (cond ((eq direction :write)
           (cond ((si:%data-type-safe-p value-data-type)
                  (setq value (%make-pointer value-data-type value-pointer)))
                 ((= value-data-type dtp-null)
                  (setq value (%make-pointer dtp-locative value-pointer))
                  (setq value-has-dtp-null t))
                 (t
                  (ferror "bad MD in mar break"))))
          (t
           (setq value-data-type (%p-data-type object-pointer))
           (setq value-pointer (%p-pointer object-pointer))
           (cond ((= value-data-type dtp-null)
                  (setq value-has-dtp-null t))
                 ((not (si:%data-type-safe-p value-data-type))
                  (ferror "VMA points to bad data in MAR read trap"))))
           )

    (setq object (%find-structure-header object-pointer))
    (setq offset (%pointer-difference object-pointer object))

    (values direction value object offset value-has-dtp-null)))

(defun (:property mar-break make-ucode-error-function) (ignore sg ete)
  (multiple-value-bind (dir val obj off dtp-null-flag)
      (mar-break-decode sg ete)
    (make-instance 'mar-break
                   :direction dir
                   :value val
                   :object obj
                   :offset off
                   :value-has-dtp-null dtp-null-flag)))

(defflavor mar-break (direction value object offset value-has-dtp-null)
           (trap debugging-condition)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (mar-break :report) (stream)
  (if (eq direction ':write)
      (format stream
              "The MAR has gone off because of an attempt to write ~S into offset ~O in ~S."
              (if (null value-has-dtp-null) value "void")
              offset
              object)
    (format stream "The MAR has gone off because of an attempt to read from offset ~O in ~S."
            offset object)))

(defmethod (mar-break :ucode-proceed-types) ()
  (cond ((eq direction ':write)
         '(:no-action :proceed-no-write))
        (t '(:no-action))))

(defmethod (mar-break :case :proceed-asking-user :no-action) (continuation ignore)
  "Proceed."
  (funcall continuation :no-action))

(defmethod (mar-break :case :proceed-asking-user :proceed-no-write) (continuation ignore)
  "Proceed, not changing the cell contents."
  (funcall continuation :proceed-no-write))

(defmethod (mar-break :case :proceed-ucode-with-args :no-action) (sg &rest ignore)
  ;; By simply returning without calling SG-PROCEED-MICRO-PC, the PGF-R will return
  (sg-funcall sg #'(lambda (vma md)                             ;Simulate the write
                     (let ((%mar-high -2) (%mar-low -1))        ;Disable MAR
                       (rplaca vma md)))
              (%make-pointer-offset dtp-locative object offset)
              value))

(defmethod (mar-break :case :proceed-ucode-with-args :proceed-no-write) (&rest ignore)
  ;; By simply returning without calling SG-PROCEED-MICRO-PC, the PGF-R will return
  )

;;;; TRANS-TRAP

(defun %p-contents-eq (p x)
  (and (neq (%p-data-type p) dtp-null)
       (eq (car p) x)))

;;; Given the address on which a trans-trap occurred, determine where the contents
;;; of that address is stored now, and if it is a null pointer, what cell of which
;;; symbol was unbound.  The "symbol" can actually be a function-spec.
(defun trans-trap-decode (sg)
  (declare (values original-address current-address cell-type symbol))
  (let (original-address current-address cell-type symbol contents
        pp rp)
    (setq pp (sg-regular-pdl-pointer sg))
    (setq rp (sg-regular-pdl sg))
    (cond ((assq 'trans-trap-restart-new restart-list) ;hack!
           (setq original-address (%make-pointer dtp-locative (aref rp (- pp 3)))))
          (t
           (setq original-address (sg-saved-vma sg))))
    (setq current-address (cell-location-in-stack-group original-address sg))
    (setq cell-type nil)                               ;NIL means not a null pointer

    (when (= (aref rp pp) dtp-null)
      (setq cell-type ':closure)                ;Jumping to conclusions, default to this
      (setq contents (%make-pointer dtp-locative (aref rp (1- pp)))
            symbol (%find-structure-header contents))
      (cond ((symbolp symbol)
             (case (%pointer-difference original-address symbol)
               (1 (setq cell-type ':value))
               (2 (setq cell-type ':function))
               (t
                (if (fboundp 'si:identify-locative-to-index-cell-array)
                    (let ((type (si:identify-locative-to-index-cell-array original-address)))
                      (if type
                          (setq cell-type type)))))))
            ((and (consp symbol)
                  (= (%p-data-type symbol) dtp-list)
                  (%p-contents-eq (car symbol) ':method))
             (setq cell-type ':function
                   symbol (si::meth-function-spec symbol)))
            (t (setq cell-type nil))))
    (values original-address current-address cell-type symbol)))

(defun (:property trans-trap make-ucode-error-function) (ignore sg ete)
  (declare (ignore ete))
  (without-interrupts
    (multiple-value-bind (original-address current-address cell-type symbol)
        (trans-trap-decode sg)
      (setq current-address (%make-pointer dtp-locative current-address))
      (make-instance (case cell-type
                       ((:value :closure)
                        'unbound-variable)
                       (:function
                        'undefined-function)
                       (t 'cell-contents-error))
                     :address original-address
                     :current-address current-address
                     :cell-type cell-type
                     :symbol symbol
                     :data-type (aref (sg-regular-pdl sg) (sg-regular-pdl-pointer sg))
                     :pointer (aref (sg-regular-pdl sg) (1- (sg-regular-pdl-pointer sg)))
                     :containing-structure (%find-structure-header original-address)
                     :condition-names
                       (case cell-type
                         (:value '(unbound-symbol))
                         (:closure
                          (typecase (%find-structure-header original-address)
                            (instance '(unbound-instance-variable))
                            (list '(unbound-closure-variable))
                            (t '(bad-data-type-in-memory))))
                         (:function `(undefined-function))
                         (t '(bad-data-type-in-memory)))))))

(defflavor cell-contents-error
        (address current-address
         cell-type symbol containing-structure
         data-type pointer)
        (trap error)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (cell-contents-error :current-address) ()
  (%pointer current-address))

(defflavor unbound-variable () (cell-contents-error))

(defmethod (unbound-variable :variable-name) ()
  symbol)

(defmethod (unbound-variable :instance) ()
  (and (typep containing-structure 'instance) containing-structure))

(defflavor undefined-function () (cell-contents-error))

(defmethod (undefined-function :function-name) ()
  symbol)

(defmethod (cell-contents-error :report) (stream &aux contents-changed verb)
  (setq contents-changed
        (or ( (%p-data-type current-address)
               data-type)
            ( (%p-pointer current-address)
               pointer)))
  (setq verb (if contents-changed "was" "is"))
  (case cell-type
    (:value (format stream "The variable ~S ~A unbound." symbol verb))
    (:function (format stream "The function ~S ~A undefined." symbol verb))
    (:closure (if (typep containing-structure 'instance)
                  (format stream "The instance variable ~S ~A unbound in ~S."
                          symbol verb containing-structure)
                (format stream "The variable ~S ~A unbound (in a closure value-cell)."
                        symbol verb)))
    (otherwise
     (format stream "The word #<~S ~S> was read from location ~O ~@[(in ~A)~]."
             (q-data-types data-type)
             pointer
             (%pointer address)
             (let ((area (%area-number address)))
               (and area (area-name area)))))))

(defmethod (cell-contents-error :after :print-error-message)
           (sg brief stream &aux prop contents-changed)
  (unless brief
    (setq contents-changed
          (or ( (%p-data-type current-address)
                 data-type)
              ( (%p-pointer current-address)
                 pointer)))
    (case cell-type
      (:value (if contents-changed
                  (cell-contents-error-print-new-contents-1 stream "It now has the value"
                                                            current-address)))
      (:function (if contents-changed
                     (cell-contents-error-print-new-contents-1
                       stream "It now has the definition"
                       current-address))
                 (and (symbolp symbol)
                      (get symbol 'compiler::qintcmp)
                      (let ((fn (rp-function-word (sg-regular-pdl sg)
                                                  (sg-out-to-interesting-active
                                                    sg *error-locus-frame*))))
                        (format stream
                                "Note: ~S is supported as a function only when used in compiled code.~%"
                                symbol)
                        (if (consp fn)
                            (format stream
                                    "You may have evaluated the definition of ~S, which is now not compiled.~%"
                                    (function-name fn)))))
                 (and (symbolp symbol)
                      (setq prop (getl symbol '(expr fexpr macro subr fsubr lsubr autoload)))
                      (format stream "Note: the symbol has a ~S property, ~
                                so this may be a Maclisp compatibility problem.~%"
                              (car prop))))
      (:closure (if contents-changed
                    (cell-contents-error-print-new-contents-1 stream "It now has the value"
                                                              current-address)))
      (otherwise
       (if contents-changed
           (cell-contents-error-print-new-contents-1
             stream "The cell now contains" current-address))))))

(defun cell-contents-error-print-new-contents-1 (stream cell-description address)
  (if (%p-contents-safe-p address)
      (format stream "~A ~S.~%" cell-description (%p-contents-offset address 0))
    (format stream "~A #<~S ~S>.~%" cell-description
            (q-data-types (%p-data-type address))
            (%p-pointer address))))

;;; Some people would rather not spend the time for this feature, so let them turn it off
(defconst enable-trans-trap-dwim t
  "Non-NIL means look spontaneously in other packages on undefined function or variable error.")

;;; If problem is symbol in wrong package, offer some dwimoid assistance.
(defmethod (cell-contents-error :debugger-command-loop) (sg &optional (error-object self))
  (catch 'quit
    (and enable-trans-trap-dwim
         (catch-error-restart ((sys:abort error) "Return to debugger command loop.")
           (let ((*error-sg* sg))
             (send error-object :proceed-asking-user :package-dwim
                                'proceed-error-sg 'read-object)))))
  nil)

(defmethod (cell-contents-error :case :proceed-asking-user :package-dwim)
           (continuation ignore &aux cell new-val)
  "Look for symbols with the same name but in different packages."
  (and (symbolp symbol)
       (setq cell (assq cell-type '((:value boundp symeval)
                                    (:function fdefinedp fdefinition))))
       (car (setq new-val
                  (sg-funcall *error-sg* 'cell-contents-error-dwimify symbol cell terminal-io)))
       (funcall continuation :new-value (cadr new-val))))

;;; CELL is a list (symbolic-name dwimify-definition-type value-extractor)
(defun cell-contents-error-dwimify (sym cell *query-io*)
  (declare (return-list success-p new-value new-symbol))
  (let ((dwim-value (dwimify-package-0 sym (second cell))))
    (send *query-io* :fresh-line)
    (and dwim-value (values t (funcall (third cell) dwim-value) dwim-value))))

(defmethod (cell-contents-error :user-proceed-types) (proceed-types)
  (remq ':new-value proceed-types))

(defmethod (cell-contents-error :case :proceed-asking-user :no-action)
           (continuation read-object-function)
  "Proceed, using current contents if legal, or reading replacement value."
  (if (not (%p-contents-safe-p current-address))
      ;; Location still contains garbage, get a replacement value.
      (send self :proceed-asking-user :new-value continuation read-object-function)
    (funcall continuation :no-action)))

(defmethod (cell-contents-error :case :proceed-asking-user :new-value)
           (continuation read-object-function)
  "Use the value of an expression you type."
  (let ((prompt "Form to evaluate and use instead of cell's contents:~%"))
    (case cell-type
      (:value
       (setq prompt (format nil "Form to evaluate and use instead of ~S's value:~%"
                            symbol)))
      (:function
       (setq prompt (format nil
                            "Form to evaluate and use instead of ~S's function definition:~%"
                            symbol))))
    (funcall continuation :new-value (funcall read-object-function :eval-read prompt))))

(defmethod (cell-contents-error :case :proceed-asking-user :store-new-value)
           (continuation read-object-function)
  "Use the value of an expression you type, and store that value."
  (let ((value (funcall read-object-function :eval-read
                 (or (case cell-type
                       (:value
                        (format nil "Form to evaluate and SETQ ~S to: " symbol))
                       (:function
                        (format nil "Form to evaluate and FSET ~S to: " symbol)))
                     "Form to evaluate and store back: "))))
    (funcall continuation :store-new-value value)))

(defmethod (cell-contents-error :ucode-proceed-types) ()
  (if (memq cell-type '(:value :function))
      '(:no-action :new-value :store-new-value :package-dwim)
    '(:no-action :new-value :store-new-value)))

(defmethod (cell-contents-error :case :proceed-ucode-with-args :store-new-value) (sg value)
  (%p-store-contents current-address value)
  (sg-regpdl-push value sg)
  (sg-proceed-micro-pc sg 'trans-trap-restart))

(defmethod (cell-contents-error :case :proceed-ucode-with-args :new-value) (sg value)
  (sg-regpdl-push value sg)
  (sg-proceed-micro-pc sg 'trans-trap-restart))

;;; This has to exist, but it's not intended to ever be used,
;;; so do the same thing as :NO-ACTION if it ever does get used.
(defmethod (cell-contents-error :case :proceed-ucode-with-args :package-dwim) (sg)
  (sg-regpdl-push 0 sg)
  ;; Transfer the current contents to what MD will be got from.
  (%blt-typed current-address (sg-locate sg 'pp) 1 0)
  (sg-proceed-micro-pc sg 'trans-trap-restart))

(defmethod (cell-contents-error :case :proceed-ucode-with-args :no-action) (sg)
  (sg-regpdl-push 0 sg)
  ;; Transfer the current contents to what MD will be got from.
  (%blt-typed current-address (sg-locate sg 'pp) 1 0)
  (sg-proceed-micro-pc sg 'trans-trap-restart))

;;;; FUNCTION-ENTRY

;;; Special case.
;;; The ucode kindly leaves the M-ERROR-SUBSTATUS pushed onto the
;;; regular pdl so that we can find it.
;;; The meanings of %%M-ESUBS-BAD-QUOTED-ARG, %%M-ESUBS-BAD-EVALED-ARG
;;; and %%M-ESUBS-BAD-QUOTE-STATUS are not clear, as they are not used
;;; by the microcode.

(defflavor function-entry-error-trap () (trap function-entry-error)
  :abstract-flavor)

(defun function-entry-error-trap (sg)
  (loop with error-code = (aref (sg-regular-pdl sg) (sg-regular-pdl-pointer sg))
        for symbol in '(%%m-esubs-too-few-args %%m-esubs-too-many-args %%m-esubs-bad-dt)
        for flag in '(too-few-arguments-trap too-many-arguments-trap function-entry-error-trap)
     when (ldb-test (symbol-value symbol) error-code)
       return flag))

(defun (:property function-entry make-ucode-error-function) (ignore sg ignore)
  (make-instance (function-entry-error-trap sg)
                 :function (aref (sg-regular-pdl sg) (sg-ap sg))
                 :argument-list (cdr (get-frame-function-and-args sg (sg-ap sg)))
                 :nargs (rp-number-args-supplied (sg-regular-pdl sg) (sg-ap sg))))

(defmethod (function-entry-error-trap :default :report) (stream)
  (format stream "Function ~S called with an argument of bad data type."
          (function-name function)))

(defmethod (function-entry-error-trap :ucode-proceed-types) ()
   '(:fewer-arguments :additional-arguments :new-argument-list))
(defflavor too-many-arguments-trap () (too-many-arguments function-entry-error-trap))
(defflavor too-few-arguments-trap () (too-few-arguments function-entry-error-trap))
(defmethod (too-few-arguments-trap :ucode-proceed-types) ()
  '(:additional-arguments :fewer-arguments :new-argument-list))

(defmethod (function-entry-error-trap :case :proceed-ucode-with-args :fewer-arguments) (sg n)
  (send self :proceed-ucode-with-args :new-argument-list sg
             (firstn n argument-list)))

(defmethod (function-entry-error-trap :case :proceed-ucode-with-args :additional-arguments)
           (sg args)
  (send self :proceed-ucode-with-args :new-argument-list sg
             (append argument-list (copy-list args))))

;; Copied from LAD: RELEASE-3.DEBUGGER; TRAP.LISP#28 on 2-Oct-86 06:00:49
(defmethod (function-entry-error-trap :case :proceed-ucode-with-args :new-argument-list)
           (sg arguments &aux (form (cons function arguments)))
  (let* ((frame (sg-ap sg))
         (*error-locus-frame* (sg-ap sg))
         (*current-frame* (sg-ap sg))
         (*innermost-visible-frame* (sg-ap sg)))
    ;; If we haven't quit before getting here, he wants to proceed and FORM is set up
    (sg-unwind-to-frame-and-reinvoke sg frame form)
    (leaving-error-handler)
    (without-interrupts
      (if *error-handler-running*
          (wipe-current-stack-group-and-resume sg)
        (stack-group-resume sg nil)))))

(defflavor dont-clear-input-ucode-breakpoint () (trap debugging-condition))

(defmethod (dont-clear-input-ucode-breakpoint :maybe-clear-input) (stream)
  (declare (ignore stream))
  ;; Don't clear input
  nil)

(defmethod (dont-clear-input-ucode-breakpoint :print-error-message-prefix) (sg brief stream)
  (declare (ignore sg brief))
  (princ ">> " stream))

(defmethod (dont-clear-input-ucode-breakpoint :ucode-proceed-types) ()
  '(:no-action))

(defmethod (dont-clear-input-ucode-breakpoint :case :proceed-ucode-with-args :no-action)
           (sg &rest ignore)
  (sg-proceed-micro-pc sg nil))

(defmethod (dont-clear-input-ucode-breakpoint :case :proceed-asking-user :no-action)
           (continuation ignore)
  "Proceed."
  (format t " Continue from break.~%")
  (funcall continuation :no-action))

;>> BOGUS
(defsignal trace-breakpoint step-break ()
  "Used by (TRACE (FOO ERROR))")

;>> DEFICIENT
(def-ucode-error breakpoint step-break
  :format-string "Breakpoint")

;>>
(defvar *step-break-instance*)
(defun (:property step-break make-ucode-error-function) (ignore ignore ignore)
  (if (variable-boundp *step-break-instance*)
      *step-break-instance*
    (setq *step-break-instance* (make-instance 'step-break))))

(defflavor step-break () (dont-clear-input-ucode-breakpoint))

(defmethod (step-break :ucode-proceed-types) ()
  '(:no-action))

;added 12/4/86 by RG.  Enables proceed from :ERROR option in trace.
(defmethod (step-break :user-proceed-types) (proceed-types)
  (if (memq :no-action proceed-types)
      proceed-types
    (cons :no-action proceed-types)))

(defmethod (step-break :inhibit-backtrace) () t)
(defmethod (step-break :inhibit-proceed-prompt) () t)
(defmethod (step-break :print-error-message) (sg brief stream)
  (declare (ignore brief))
  (let ((rp (sg-regular-pdl sg)))
    (format stream ">> Step break at pc ~D in ~S"
            (rp-exit-pc rp *error-locus-frame*)
            (rp-function-word rp *error-locus-frame*))))

(def-ucode-error call-trap call-trap
  :function (rp-function-word (sg-regular-pdl sg) (sg-ipmark sg))
  :catch-value (aref (sg-regular-pdl sg) (+ (sg-ipmark sg) 2)))

(defflavor call-trap (function catch-value)
           (dont-clear-input-ucode-breakpoint)
  :gettable-instance-variables
  :initable-instance-variables)

(defun (:property call-trap enter-error-handler) (sg ignore)
  (let ((*innermost-visible-frame* (sg-ipmark sg)))     ;Make frame being entered visible.
    ;; Trap on exit from this frame -- unless it is a CATCH.
    ;; In that case, it is redundant to trap again,
    ;; and suspected of causing bugs.
    (unless (eq (rp-function-word (sg-regular-pdl sg) (sg-ipmark sg))
                #'*catch)
      (setf (rp-trap-on-exit (sg-regular-pdl sg) *innermost-visible-frame*) 1)
      (setf (rp-attention (sg-regular-pdl sg) *innermost-visible-frame*) 1))))

(defmethod (call-trap :around :find-current-frame) (cont mt args sg)
  (let ((ipmark (fourth (symeval-in-stack-group '*ucode-error-status* sg))))
    (multiple-value-bind (nil nil innermost-frame innermost-visible-p)
        (lexpr-funcall-with-mapping-table cont mt args)
      (values ipmark ipmark innermost-frame innermost-visible-p))))

(defmethod (call-trap :report) (stream)
  (if (neq function #'*catch)
      (format stream "Break on entry to function ~S."
              (function-name function))
    (format stream "Break on call to CATCH (about to do normal exit from CATCH frame); ~
value is ~S." catch-value)))

(defmethod (call-trap :inhibit-proceed-prompt) () t)

(def-ucode-error exit-trap exit-trap
  :function (rp-function-word (sg-regular-pdl sg) (sg-ap sg))
  :values (sg-frame-value-list sg (sg-ap sg)))

(defflavor exit-trap (function values)
           (dont-clear-input-ucode-breakpoint)
  :gettable-instance-variables
  :initable-instance-variables)

(defun (:property exit-trap enter-error-handler) (sg ignore)
  ;; Don't catch this trap again if user tries to return, etc.
  (setf (rp-trap-on-exit (sg-regular-pdl sg) (sg-ap sg)) 0)
  (let ((*innermost-visible-frame* (sg-ap sg)))
    ;; Add our last value onto list of all multiple values returned
    ;; so the user sees them all in the same place.
    (sg-return-additional-value sg *innermost-visible-frame* (sg-ac-t sg))))

(defmethod (exit-trap :around :find-current-frame) (cont mt args sg)
  (let ((ap (third (symeval-in-stack-group '*ucode-error-status* sg))))
    (multiple-value-bind (nil nil innermost-frame innermost-visible-p)
        (lexpr-funcall-with-mapping-table cont mt args)
      (values ap ap innermost-frame innermost-visible-p))))

(defmethod (exit-trap :report) (stream)
  (format stream "Break on exit from ~S." (function-name function)))

(defmethod (exit-trap :after :print-error-message) (sg brief stream)
  (declare (ignore sg brief))
  (if (null values)
      (format stream " No values being returned.~%")
    (format stream " Values being returned are:")
    (let ((*print-length* error-message-prinlength)
          (*print-level* error-message-prinlevel))
      (dolist (val values)
        (format stream "~%    ~S" val)))
    (terpri)))

(defmethod (exit-trap :inhibit-proceed-prompt) () t)

(defmethod (exit-trap :case :proceed-ucode-with-args :no-action) (sg &rest ignore)
  ;; Un-return the last returned value and put it in M-T to be returned over.
  ;; This is a no-op if we are not feeding multiple values
  ;; since the value just comes from M-T in that case.
  (setf (sg-ac-t sg) (sg-discard-last-value sg (sg-ap sg)))
  (sg-proceed-micro-pc sg nil))

;;; THROW-TRAP is used for both exit trap and tag not seen, starting in UCADR 260.
;;; If M-E contains NIL, the tag was not seen.
(defun (:property throw-trap make-ucode-error-function) (ignore sg ignore)
  (cond ((sg-contents sg 'm-e)
         ;; If tag was found, it must be trap-on-exit.
         (make-instance 'throw-exit-trap
                        :function (function-name (car (%p-contents-as-locative
                                                        (locf (sg-ac-d sg)))))))
        (t
         ;; Otherwise tag was not found.
         (make-instance 'throw-tag-not-seen
                        :tag (sg-ac-a sg)
                        :value (sg-ac-t sg)
                        :count (sg-ac-b sg)
                        :action (sg-ac-c sg)))))

(defun (:property throw-trap enter-error-handler) (sg ignore)
  (if (sg-contents sg 'm-e)
      ;; Do this only for trap-on-exit, not for tag not seen.
      (let ((cur-frame
              (- (%pointer-difference (%p-contents-as-locative (locf (sg-ac-d sg)))
                                      (sg-regular-pdl sg))
                 2)))
        (setf (rp-trap-on-exit (sg-regular-pdl sg) cur-frame) 0))))

(defflavor throw-exit-trap (function) (dont-clear-input-ucode-breakpoint)
  :gettable-instance-variables
  :initable-instance-variables)

(defmethod (throw-exit-trap :around :find-current-frame) (cont mt args ignore)
  (multiple-value-bind (x y z)
      (around-method-continue cont mt args)
    (values x y z t)))

(defmethod (throw-exit-trap :report) (stream)
  (format stream "Break on throw through marked call to ~S." function))

;;; List problems with currently-loaded error table

(defun list-problems ()
  (let ((unhandled-traps ())
        (missing-restart-tags ())
        (argtyp-unknown-types ())
        (orphaned-traps ())
        (tem))
    (dolist (ete error-table)
      (or (get (setq tem (second ete)) 'make-ucode-error-function)
          (pushnew tem unhandled-traps :test #'eq))
      (if (eq tem 'argtyp)
          (let ((type (third ete)))
            (if (symbolp type)
                (setq type (ncons type)))
            (when (dolist (type type)
                    (or (type-defined-p type)
                        (assq type *description-type-specs*)
                        (return t)))
              (pushnew (third ete) argtyp-unknown-types))))
      (and (setq tem (assq tem                  ;Anything that calls SG-PROCEED-MICRO-PC
                           '((argtyp . 5) (subscript-oob . 4))))
           (setq tem (nth (cdr tem) ete))
           (progn (if (consp tem) (setq tem (cadr tem))) t)
           (neq tem 'fall-through)
           (not (assq tem restart-list))
           (pushnew tem missing-restart-tags :test #'eq)))
    (do-symbols (sym 'eh)
      (and (get sym 'make-ucode-error-function)
           (not (find sym error-table :key #'second))
           (push sym orphaned-traps)))
    (if (not (null unhandled-traps))
        (format t "~&Traps without handler: ~S" unhandled-traps))
    (if (not (null orphaned-traps))
        (format t "~&Trap handlers defined, but no ucode for: ~S" orphaned-traps))
    (if (not (null missing-restart-tags))
        (format t "~&Missing RESTART tags: ~S" missing-restart-tags))
    (if (not (null argtyp-unknown-types))
        (format t "~&ARGTYP types not defined for TYPEP: ~S" argtyp-unknown-types))
    (if (or unhandled-traps orphaned-traps missing-restart-tags argtyp-unknown-types)
        t nil)))

(defun type-defined-p (type)
  (let ((type1 (if (consp type) (car type) type)))
    (and (symbolp type1)
         (or (getl type1 '(si::type-predicate si::type-expander si::type-alias-for
                           si::flavor si::defstruct-description #|si::defstruct-named-p|#))
             (rassq type si::type-of-alist) (rassq type si::typep-one-arg-alist)
             #|(and (fboundp 'class-symbolp) (class-symbolp type1))|#
             ))))

(compile-flavor-methods
  trap
  random-trap
  random-dangerous-trap
  arg-type-error
  ;fixnum-overflow
  floating-point-exception
  floating-exponent-overflow
  floating-exponent-underflow
  divide-by-zero
  bad-array-mixin
  bad-array-trap
  array-number-dimensions-error
  number-array-not-allowed
  subscript-error
  array-has-no-leader
  fill-pointer-not-fixnum
  invalid-function-trap
  throw-tag-not-seen
  select-method-not-found
  turd-alert-error
  write-in-read-only
  pdl-overflow
  illegal-instruction
  illegal-expt-trap
  nubus-error
  mar-break
  cell-contents-error
  unbound-variable
  undefined-function
  function-entry-error-trap
  too-many-arguments-trap
  too-few-arguments-trap
  dont-clear-input-ucode-breakpoint
  step-break
  call-trap
  exit-trap
  throw-exit-trap
  )
