;;; -*- Mode:Lisp; Package:(NC LISP); Readtable:CL; Base:10 -*-


;;;; New Macros


(defun get-subst (name)
  (nc:get-declaration name 'prims:subst))

(eval-when (eval compile load)
(defvar *nmacro-package* (or (find-package "NMacro") (make-package "NMacro")))
)

(defmacro ndefmacro (name args &body body)
  (let ((nname (intern (format nil "~a:~a" (symbol-package name) (symbol-name name)) *nmacro-package*)))
    `(progn
       (defmacro ,nname ,args
         ,@body)
       (setf (get ',name 'nmacro) (macro-function ',nname))
       (setf (get ',name 'si:arglist) ',args))))


(zl:defsubst nlisp:macro-function (sym)
  (get sym 'nmacro))

(defsetf nlisp:macro-function (sym) (fcn)
  `(setf (get ,sym 'nmacro) ,fcn))

(defvar nlisp:*macroexpand-hook* 'funcall
  "The value is a function called to expand a macro call.
The first arg is the macro's expander function.
The second arg is the macro call itself.")

(defun nlisp:macroexpand-1 (form &optional environment &aux expander)
  "Macroexpand FORM once, if possible.
If there is nothing to expand, return it unchanged.
The value of *MACROEXPAND-HOOK* (which should behave like FUNCALL)
is used to invoke the expander function."
  (declare (values expansion expanded-p))
  (if (atom form) (values form nil)
    (let ((name (car form)))
      (cond ((not (atom name))
             (values form nil))
            ((nlisp:special-form-p name)
             (values form nil))
            ((and environment
                  (setq expander (obtain-fvariable environment name)))
             (if (and (consp expander)
                      (eq (car expander) 'macro))
                 (values (funcall nlisp:*macroexpand-hook* (cdr expander) form environment)
                         t)
               (values form nil)))
             ((setq expander (nlisp:macro-function name))
              ; (record-macro-expanded name)
              (values (funcall nlisp:*macroexpand-hook* expander form environment)
                      t))
             (t (values form nil))))))


(defun nlisp:macroexpand (form &optional environment)
  "Expand FORM repeatedly until the result is not a macrocall."
  (do ((old-form form new-form)
       (new-form (nlisp:macroexpand-1 form environment)
                 (nlisp:macroexpand-1 new-form environment)))
      ((or (eq new-form old-form)
           (atom new-form))
       (values new-form (not (eq new-form form))))))




;;; this is all common lisp requires
(ndefmacro loop (&body body)
  `(do () (()) ,@body))


(ndefmacro prims:multiple-value-setq (var-list exp)
  (let ((temps (mapcar #'(lambda (var) (gensym var))
                       var-list)))
    `(multiple-value-bind ,temps ,exp
       . ,(mapcar #'(lambda (var temp)
                      `(setq ,var ,temp))
                  var-list
                  temps))))


(ndefmacro let (varlist &body body)
  (let ((vars '())
        (vals '()))
    (dolist (var varlist)
      (cond ((consp var)
             (push (car var) vars)
             (push (cadr var) vals))
            (t (push var vars)
               (push nil vals))))
    (mapc #'si:require-bindable-symbol vars)
    `((lambda ,(nreverse vars)
        ,@body)
      ,@(nreverse vals))))

;;; This is not right, need to put
;;; declarations in the right place
(ndefmacro let* (varlist &body body)
  (let ((form `(progn . ,body)))
    (dolist (var (reverse varlist))
      (cond ((consp var)
             (si:require-bindable-symbol (car var))
             (setq form
                   `((lambda (,(car var))
                       ,form)
                     ,(cadr var))))
            (t (si:require-bindable-symbol var)
               (setq form
                     `((lambda (,var)
                         ,form)
                       nil)))))
    form))



(ndefmacro return (&optional value)
  `(return-from nil ,value))


;;; get declarations
(ndefmacro prog (varlist &rest body)
  `(block nil
     (let ,varlist
       (tagbody
           . ,body))))

(ndefmacro prog1 (first &body forms)
  (let ((value (gensym 'g)))
    `((lambda (,value)
        ,@forms
        ,value)
      ,first)))

(ndefmacro or (&rest expressions)
  (cond ((null expressions) NIL)
        ((null (cdr expressions)) (car expressions))
        (t (let ((v (gensym 'g)))
             `((lambda (,v)
                (if ,v ,v (or ,@(cdr expressions))))
               ,(car expressions))))))

(ndefmacro and (&rest expressions)
  (cond ((null expressions) T)
        ((null (cdr expressions)) (car expressions))
        (t `(if ,(car expressions) (and ,@(cdr expressions))))))

;;; This is wrong because do doesn't rebind
;;; but setqs. (you could tell the difference
;;; if you closed over the vars)
;(ndefmacro do (var-specs (end-test . end-forms) &body body)
;  (let ((name (gensym 'do)))
;    `(block nil
;       (labels ((,name ,(mapcar #'(lambda (var-spec)
;                                   (if (symbolp var-spec)
;                                       var-spec
;                                     (car var-spec)))
;                               var-specs)
;                (if ,end-test
;                    (progn ,@end-forms)
;                  (progn
;                    (tagbody ,@body)
;                    ;;"such go statements may not appear
;                    ;; in the variable specifiers..." CLtL p 123
;                    (,name ,@(mapcar #'(lambda (var-spec)
;                                         (if (consp var-spec)
;                                             (if (cddr var-spec)
;                                               ;; step form
;                                               (third var-spec)
;                                               ;; the var itself
;                                               (car var-spec))
;                                           var-spec))
;                                     var-specs))))))
;        (,name ,@(mapcar #'(lambda (var-spec)
;                             (if (and (consp var-spec)
;                                      (cdr var-spec))
;                                 (second var-spec)
;                               nil))
;                         var-specs))))))

;;; DO, DO*, DO-NAMED, DO*-NAMED
(defun separate-do-bindings (binding-list receiver)
  (labels ((scan-bindings (tail binding-names initial-values iteration-clauses)
             (if (null tail)
                 (funcall receiver
                          (reverse binding-names)
                          (reverse initial-values)
                          (reverse iteration-clauses))
                 (let ((this-clause (first tail)))
                   (if (symbolp this-clause)
                       (scan-bindings (rest tail)
                                      (cons this-clause binding-names)
                                      (cons 'nil initial-values)
                                      (cons this-clause iteration-clauses))
                       (let
                         ((this-binding (first this-clause))
                          (init-and-step (rest this-clause)))
                         (if (null init-and-step)
                             (scan-bindings (rest tail)
                                            (cons this-binding binding-names)
                                            (cons 'nil initial-values)
                                            (cons this-binding iteration-clauses))
                             (let ((init (first init-and-step))
                                   (step (rest init-and-step)))
                               (if (null step)
                                   (scan-bindings (rest tail)
                                                  (cons this-binding binding-names)
                                                  (cons init initial-values)
                                                  (cons this-binding iteration-clauses))
                                   (scan-bindings (rest tail)
                                                  (cons this-binding binding-names)
                                                  (cons init initial-values)
                                                  (cons (first step) iteration-clauses)))))))))))
    (scan-bindings binding-list '() '() '())))

(defun expand-do-macro (do-form let-type setq-type)
  (separate-do-bindings (second do-form)
    #'(lambda (binding-names initial-values iteration-clauses)
        (let* ((loop-tag (gensym 'do))
               (test-form (third do-form))
               (test (first test-form))
               (result (if (null (rest test-form)) '(PROGN NIL) `(PROGN ,@(rest test-form))))
               (body (rest (rest (rest do-form)))))
          (labels ((interleave (x y)
                               (cond ((null x) y)
                                     ((null y) x)
                                     (t (cons (car x) (interleave y (cdr x)))))))
            `(BLOCK NIL
               (,let-type ,(mapcar #'list binding-names initial-values)
                (TAGBODY
                    ,loop-tag
                    (WHEN ,test (RETURN-FROM NIL ,result))
                    (PROGN ,@body)
                    (,setq-type ,@(interleave binding-names iteration-clauses))
                    (GO ,loop-tag)))))))))

(defun do-expander (do-form env)
  (declare (ignore env))
  (expand-do-macro do-form 'LET 'PSETQ))

(defun do*-expander (do-form env)
  (declare (ignore env))
  (expand-do-macro do-form 'LET* 'SETQ))

(setf (nlisp:macro-function 'do)  'do-expander)
(setf (nlisp:macro-function 'do*) 'do*-expander)

(ndefmacro PSETQ (&rest var-form-pairs)
  (if (oddp (length var-form-pairs))
      (error "Odd number of arguments to PSETQ")
    (let ((vars '())
          (forms '())
          (temps '()))
      (do ((vf var-form-pairs (cddr vf)))
          ((null vf))
        (let ((var (first vf)))
          (if (not (symbolp var))
              (error "Variable given to PSETQ: ~s, is not a symbol." var)
            (progn
              (push var vars)
              (push (gensym var) temps)
              (push (second vf) forms)))))
      (setq vars (nreverse vars))
      (setq temps (nreverse temps))
      (setq forms (nreverse forms))
      `((LAMBDA ,temps
          ,@(mapcar #'(lambda (var temp)
                        `(SETQ ,var ,temp))
                    vars temps))
        ,@forms))))


(ndefmacro DOTIMES ((var limit &optional resultform) &body body)
  "Iterate BODY with VAR bound to successive integers from 0 up to LIMIT's value.
LIMIT is evaluated only once.  When it is reached, RESULTFORM is executed and returned.
RETURN and GO can be used inside the BODY."
  (zl:once-only (limit)
    `(DO ((,var 0 (1+ ,var)))
         ((>= ,var ,limit) ,resultform)
       . ,body)))


(nDEFMACRO li::DOLIST ((VAR LIST &OPTIONAL RESULTFORM) &BODY BODY)
  "Iterate BODY with VAR bound to successive elements of the value of LIST.
If LIST is exhausted, RESULTFORM is executed and returned.
RETURN and GO can be used inside the BODY."
  (LET ((ITERATION-VAR (GENSYM 'l)))
    `(DO ((,ITERATION-VAR ,LIST (CONS::CDR ,ITERATION-VAR))
          (,VAR ))
         ((NULL ,ITERATION-VAR) ,RESULTFORM)
       (SETQ ,VAR (CONS::CAR ,ITERATION-VAR))
       . ,BODY)))


(ndefmacro WHEN (pred &body body)
  "(WHEN pred form1 from2 ...) ==> (IF pred (PROGN form1 form2 ...))
WHEN first evaluates PRED.  If the result is NIL, WHEN returns NIL.
Otherwise, the BODY is executed and its last expression's value returned."
  `(IF ,pred (PROGN ,@body)))

(ndefmacro UNLESS (pred &body body)
  "(UNLESS pred form1 form2 ...) ==> (IF pred NIL (PROGN form1 form2 ...))
UNLESS first evaluates PRED.  If the result is non-NIL, UNLESS returns NIL.
Otherwise, the BODY executed and its last expression's value is returned."
  `(IF ,pred NIL (PROGN ,@body)))

(ndefmacro cond (&rest clauses)
  (if (null clauses)
      NIL
    (let ((clause (car clauses)))
      (cond ((not (consp clause))
             (error "The atom ~s is not a valid COND clause." clause))
            ((member (car clause) '(t otherwise) :test #'eq)
             `(progn ,@(cdr clause)))
            ((null (cdr clause))
             `(or ,(car clause)
                  (cond ,@(cdr clauses))))
            (t `(if ,(car clause)
                (progn ,@(cdr clause))
              (cond ,@(cdr clauses))))))))


(setf (nlisp:macro-function 'case) (macro-function 'case))



;(nDEFMACRO zl:DISPATCH (byte-spec WORD &BODY CLAUSES)
;  "Extract the byte BYTE-SPEC from WORD and execute a clause selected by the value.
;The first element of each clause is a value to compare with the byte value,
;or a list of byte values.  These byte values are evaluated!.
;T or OTHERWISE as the first element of a clause matches any test object.
;This is a special exception, in that OTHERWISE is not evaluated."
;  (declare (zwei:indentation 1 1))
;  (LET ((FOO (GENSYM)))
;    `(LET ((,FOO (LDB ,WORD ,byte-spec 0)))
;       (COND ,@(MAPCAR #'(LAMBDA (CLAUSE)
;                        (si:MACRO-TYPE-CHECK-WARNING 'DISPATCH (CAR CLAUSE))
;                        `(,(COND ((MEMber (CAR CLAUSE) '(OTHERWISE :OTHERWISE T))
;                                  'T)
;                                 ((ATOM (CAR CLAUSE))
;                                  `(= ,FOO ,(CAR CLAUSE)))
;                                 (T
;                                    `(OR ,@(MAPCAR #'(LAMBDA (ITEM)
;                                                       `(= ,FOO ,ITEM))
;                                                   (CAR CLAUSE)))))
;                            NIL . ,(CDR CLAUSE)))
;                      CLAUSES)))))


;;;; TYPECASE, ETYPECASE, CTYPECASE, ECASE, CCASE

(DEFUN DEAD-CLAUSES-WARNING (COND-CLAUSES FUNCTION-NAME)
  "Given a list of COND-clauses, warn if any but the last starts with T.
FUNCTION-NAME (usually a macro name) is used in the warning.
The warning is made iff we are now accumulating warnings for an object."
  (DO ((CLAUSES COND-CLAUSES (CDR CLAUSES)))
      ((NULL (CDR CLAUSES)))
    (AND (EQ (CAAR CLAUSES) T)
         ; OBJECT-WARNINGS-OBJECT-NAME
         (RETURN
;          (COMPILER:WARN 'COMPILER::DEAD-CODE :IMPLAUSIBLE
           (warn
             "Unreachable clauses following otherwise-clause in ~S." FUNCTION-NAME)))))

(defun expand-case-macro (macro-name place clauses clause-function &optional last)
  (declare (values test-exp let clauses))
  ;; If place is an eval-at-load-time,
  ;; we will treat it as a random expression, which is right.
  (flet ((hack (test-exp)
           (setq clauses
                 (mapcar #'(lambda (x)
                             (si:macro-type-check-warning macro-name (car x))
                             `(,(funcall clause-function test-exp (car x))
                               nil . ,(cdr x)))
                         clauses))
           (when last
             (setq clauses `(,@clauses (t ,(funcall last test-exp)))))
           (dead-clauses-warning clauses macro-name)
           clauses))
      (cond ((or (atom place)
                 (and (member (car place) '(car cdr caar cadr cdar cddr))
                      (atom (cadr place))))
             `(cond . ,(hack place)))
            ((and (null (cdr clauses))
                  (not last))
             ;; one clause case.
             `(cond . ,(hack place)))
            (t
             `(LET ((.SELECTQ.ITEM. ,place))
                (COND . ,(hack '.selectq.item.)))))))


(ndefmacro prims:case (&whole whole place &body clauses)
  "Execute the first clause that matches PLACE.
The first element of each clause is a match value or a list of match values.
PLACE is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the CASE.
T or OTHERWISE as the first element of a clause matches any test object."
  (declare (zwei:indentation 1 1))
  (expand-case-macro (car whole) place clauses
                     (lambda (test-exp c)
                       (cond ((li:member c '(otherwise t))
                              `t)
                             ((and (eq c ':otherwise)
                                   ;; Old zetalisp cases
                                   ;;  >> Do not do this for the commonlisp CASE!!
                                   (li:member (car whole) '(selectq caseq)))
                              ;(compiler:warn 'foo :obsolete
                              (warn
                                             "~S case used with a ~S clause; use ~S"
                                             (car whole) ':otherwise 'otherwise)
                              `t)
                             ((atom c)
                              `(eql ,test-exp ',c))
                             (t
                              `(li:member ,test-exp ',c))))))

;; this really should try to be a lot smarter...
(ndefmacro li:typecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of PLACE.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the TYPECASE form.
If no clause fits, the value of the TYPECASE is NIL."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'typecase place clauses
                     (lambda (test-exp c)
                       (if (member c '(otherwise t))
                           `t
                         `(li:typep ,test-exp ',c)))))

;(defun no-case-error (proceedable function place value typespec)
;  (declare (dbg:error-reporter))
;  (let ((cond (make-condition 'si::no-case-found
;                             :function function
;                             :place place :value value :type-specifier typespec)))
;    (if proceedable
;       (signal-proceed-case ((val) cond)
;         (:new-value val))
;      (signal cond :proceed-types ()))))

(ndefmacro li:etypecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of PLACE.
If the result is T, the rest of that clause is excuted and the values
 of the last form in it are the values of the TYPECASE form.
If no clause fits, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'etypecase place clauses
                     (lambda (test-exp c)
                       (if (member c '(otherwise t))
                           `t
                         `(li:typep ,test-exp ',c)))
                     (lambda (test-exp)
                       `(no-case-error nil 'etypecase ',place ,test-exp
                                       '(or . ,(mapcar #'car clauses))))))


(ndefmacro li:ctypecase (place &body clauses)
  "Execute the first clause whose type specifier PLACE fits.
The first element of each clause is a type specifier.
It is used as the second argument to TYPEP to test the type of PLACE.
If the result is T, the rest of that clause is executed and the values
 of the last form in it are the values of the TYPECASE form.
If no clause fits, the value of the TYPECASE is NIL."
  (declare (zwei:indentation 1 1))
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (return-from ,tag
             ,(expand-case-macro 'ctypecase place clauses
                                 (lambda (test-exp c)
                                   (if (member c '(otherwise t))
                                       `t
                                     `(li:typep ,test-exp ',c)))
                                 (lambda (test-exp)
                                   `(progn
                                      (setf ,place
                                            (no-case-error t 'ctypecase ',place ,test-exp
                                                           '(or . ,(mapcar #'car clauses))))
                                      (go ,tag)))))))))

(ndefmacro li:ecase (place &body clauses)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
PLACE is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (expand-case-macro 'ecase place clauses
                     (lambda (test-exp c)
                       (cond ((member c '(otherwise t))
                              `t)
                             ((atom c)
                              `(eql ,test-exp ',c))
                             (t
                              `(member ,test-exp ',c))))
                     (lambda (test-exp)
                       `(no-case-error nil 'ecase ',place ,test-exp
                                       '(member
                                          . ,(mapcan (lambda (clause)
                                                       (let ((match (car clause)))
                                                         (if (not (consp match))
                                                             (list match)
                                                           (copy-list match))))
                                                     clauses))))))

(ndefmacro li:ccase (place &body clauses)
  "Execute the first clause that matches TEST-OBJECT.
The first element of each clause is a match value or a list of match values.
TEST-OBJECT is compared with the match values using EQL.
When a match-value matches, the rest of that clause is executed
and the value of the last thing in the clause is the value of the ECASE.
If no clause matches, an uncorrectable error is signaled."
  (declare (zwei:indentation 1 1))
  (let ((tag (gensym)))
    `(block ,tag
       (tagbody
        ,tag
           (return-from ,tag
             ,(expand-case-macro 'ccase place clauses
                                 (lambda (test-exp c)
                                   (cond ((member c '(otherwise t))
                                          `t)
                                         ((atom c)
                                          `(eql ,test-exp ',c))
                                         (t
                                          `(member ,test-exp ',c))))
                                 (lambda (test-exp)
                                   `(progn
                                      (setf ,place
                                            (no-case-error t 'ccase ',place ,test-exp
                                              '(member
                                                 . ,(mapcan (lambda (clause)
                                                              (let ((match (car clause)))
                                                                (if (not (consp match))
                                                                    (list match)
                                                                  (copy-list match))))
                                                            clauses))))
                                      (go ,tag)))))))))


;;;; A crock for Naha

(defmacro prims:select-processor (&body clauses)
  `(progn . ,(cdr (assoc :lambda clauses))))

(ndefmacro prims:select-processor (&body clauses)
  `(progn . ,(cdr (assoc :k clauses))))

;(defun foo ()
;  (prims:select-processor
;    (:lambda (halt-and-catch-fire))
;    (:k      (run-like-a-bat-out-of-hell))))


;;; move this
(ndefmacro vinc::data-type= (x y)
  `(hw:field= ,x ,y vinc::%%data-type))

;;;; Global Frames

(defvar *global-frames* '() "A-list of global register frames")

(defvar *global-constants* '() "A-list of constants")

(defvar *global-initial-values* '())

(defun frame-num (frame)
  (position frame *global-frames* :key #'car))

(defmacro prims:define-global-frame (frame-name)
  `(eval-when (compile load eval)
     (define-global-frame-1 ',frame-name)))

(defun define-global-frame-1 (frame-name)
  (unless (member frame-name *global-frames* :key #'car)
    (if (< (length *global-frames*) hw:number-of-global-frames)
        (setq *global-frames*
              (nconc *global-frames* (list (list frame-name))))
      (error "No more global frames"))))

(defmacro prims:define-global-variable (frame-name register-name &optional value documentation)
  `(eval-when (compile load eval)
     (export ',register-name)
     (define-global-register-1 ',frame-name ',register-name ',value)
     ,@(if documentation `((setf (documentation ',register-name 'variable) ,documentation)))))

(defmacro prims:define-global-constant (frame-name register-name value &optional documentation)
  `(eval-when (compile load eval)
     (export ',register-name)
     (define-global-constant-1 ',frame-name ',register-name ',value)
     (setf (get ',register-name 'si:system-constant) t)  ;get a new constantp
     ,@(if documentation `((setf (documentation ',register-name 'variable) ,documentation)))))

(defun define-global-constant-1 (frame-name register-name initial-value)
  (let ((value (eval initial-value)))
    (define-global-register-1 frame-name register-name value)
    (let ((variable (variable-loc (global-register register-name))))
      (let ((entry (rassoc variable *global-constants* :test #'equal)))
        (if (null entry)
            (push (cons value variable) *global-constants*)
          (setf (car entry) value))))))

(defun define-global-register-1 (frame-name register-name initial-value)
  (let ((frame (assoc frame-name *global-frames*))
        (old-initial-value (assoc register-name *global-initial-values*)))
    (if old-initial-value
        (setf (cdr old-initial-value) initial-value)
        (push (cons register-name initial-value) *global-initial-values*))
    (if frame
        (unless (member register-name (cdr frame))
          (let ((frame-num (frame-num frame-name))
                (frame-length (length (cdr frame))))
            (if (< frame-length hw:frame-size)
                (progn
                  (setf (cdr frame)
                        (nconc (cdr frame) (list register-name)))
                  (setf (get register-name 'global-register)
                        (let ((var (create-variable register-name)))
                          (setf (variable-loc var)
                                (list 'K:REGISTER
                                      register-name
                                      frame-num
                                      frame-length))
                          var))
                  (setf (get register-name :register)
                        (list frame-name frame-num frame-length)))
              (error "No more slots in frame ~a" frame-name))))
      (error "No frame named ~a" frame-name))))

(defun global-register (register-name)
  (get register-name 'global-register))

;;; this does not use setq because:
;;;   1. You aren't supposed to setq constants
;;;   2. We don't want global constants
;;;      to be substituted for the values
(defmacro prims::setup-initial-values-of-global-registers ()
  `(PROGN ,@(mapcar #'(lambda (spec) `(INITIALIZE-GLOBAL ',(car spec) ,(cdr spec)))
                    *global-initial-values*)))

(defun global-name (frame offset)
  (nth offset (cdr (nth frame *global-frames*))))
