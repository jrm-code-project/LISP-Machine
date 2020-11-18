;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:10; Readtable:ZL -*-

;;; Each element of NONCONSTANT-ALIST is a list of five elements.
;;; The first one is the temporary variable described.
;;; The second is the number of times that variable has been seen,
;;; not counting the time when its value was computed.
(defsubst seo-count (x) (cadr x))

;;; The third element is the specified value expression to "substitute".
(defsubst seo-exp (x) (caddr x))

;;; The fourth element is the temporary variable to hold this value in during execution.
(defsubst seo-tempvar (x) (cadddr x))

;;; The fifth element points to a PROGN which contains a SETQ
;;; that sets the seo-tempvar from the seo-exp.
(defsubst seo-first-use (x) (fifth x))

(defvar *seo-first-uninserted-var*)

;;; Called like SUBLIS, but makes the replacement expressions
;;; be evaluated only once and in the same order as they appear in ALIST.
;>>>>> This is a total dog.  It has no hope of working without doing any code-walking.
;>>>>>  The right thing is to use Moon's language-tools, rather than hack up losing QCLUKE
(defun sublis-eval-once (alist exp &optional reuse-flag sequential-flag
                         environment)
  "Effectively substitute for symbols in EXP according to ALIST, preserving execution order.
Each element of ALIST describes one symbol (the car)
and what it stands for (the cdr).
We replace each symbol with the corresponding value expression,
not with straight textual substitution but so that
the value expression will be evaluated only once.

If SEQUENTIAL-FLAG is non-NIL, the value substituted for each symbol
may refer to the previous symbols substituted for.

This may require the use of temporary variables.
The first use of a symbol would be replaced by a SETQ of the tempvar
to the symbol's corresponding expression.  Later uses would be
replaced with just the tempvar.  A LET to bind the tempvars is
wrapped around the whole expression.

If REUSE-FLAG is non-NIL, the symbols themselves can be used
as their own tempvars when necessary.  Otherwise tempvars are gensymmed.

It may be necessary to expand macros in EXP in order to process it.
In this case, ENVIRONMENT is passed as the environment arg to MACROEXPAND."
  (let (constant-alist nonconstant-alist value-so-far)
    ;; First, divide replacements up into constant values vs nonconstants.
    ;; Order of evaluation never matters for the constants so we will
    ;; put them in with SUBLIS.
    (dolist (elt alist)
      (let ((tem (if sequential-flag
                     (sublis constant-alist (cdr elt))
                   (cdr elt))))
        (if (constantp tem)
            (push (if (eq tem (cdr elt)) elt (cons (car elt) tem))
                  constant-alist)
          (push (list (car elt) 0 tem nil nil)
                nonconstant-alist))))
    ;; The nonconstants must remain in the proper order!
    (setq nonconstant-alist (nreverse nonconstant-alist))
    ;; If the only things not constant are variables,
    ;; then they are ok.
    (when (loop for elt in nonconstant-alist
                always (symbolp (seo-exp elt)))
      (dolist (elt nonconstant-alist)
        (push (cons (car elt)
                    (if sequential-flag (sublis constant-alist (seo-exp elt)) (seo-exp elt)))
              constant-alist))
      (setq nonconstant-alist nil))
    (setq value-so-far (sublis constant-alist exp))
    (when nonconstant-alist
      ;; If the expression to be substituted in
      ;; contains any kind of branching,
      ;; we must calculate all the variables at the beginning
      ;; to avoid having the calculation be skipped by a branch.
      ;; Hairier analysis might detect certain cases
      ;; such as a variable being used before the branch, or only after a join,
      ;; but that is probably not worth thinking about.
 ;*** the following attempted optimization loses because it winds fails to detect
 ; special forms implemented by P1, P2 props like IF.  That could be fixed.
 ; However, this optimization seems to be misdirected.  The only thing that saves
 ; is if the guy is used once and a variable can be avoided.  Otherwise,
 ; doing a LET at the top and a SETQ somewhere down inside actually COSTS you.
 ;O foo, I dont feel like rewriting this now, so I guess I'll sort of toss in some
 ; of the other functions it needs to look for... --rg 1/6/85.
      (multiple-value-bind (nil functions-used)
          (compiler:cw-top-level exp nil '(cond and or return return-from go
                                           *catch *throw catch throw
                                           do do* do-named do*-named
                                           if unwind-protect tagbody)
                                 (car environment))     ;car of env is function-env always
        (if functions-used
            (setq value-so-far `(progn ,@(mapcar 'car nonconstant-alist) ,value-so-far))))
      ;; Each nonconstant value should be inserted only once, and in correct order.
      ;; *SEO-FIRST-UNINSERTED-VAR* points to the first one we have not yet inserted.
      ;; All the ones before that have had temporary variables (gensyms) made.
      (let* ((*seo-first-uninserted-var* nonconstant-alist))
        (setq value-so-far (sublis-eval-once-1 value-so-far nonconstant-alist
                                               reuse-flag sequential-flag))
        ;; Now stick on evaluations of any values that weren't really used.
        (if *seo-first-uninserted-var*
            (setq value-so-far
                  `(multiple-value-prog1
                     ,value-so-far
                     . ,(if sequential-flag
                            (list (sublis-eval-once-1 (caar (last nonconstant-alist))
                                                      nonconstant-alist
                                                      reuse-flag t))
                          (mapcar 'seo-exp *seo-first-uninserted-var*))))))
      ;; If a temp var is not used again after it is set,
      ;; flush the temp var from the code -- just use its value straight.
      (dolist (elt nonconstant-alist)
        (let ((tem (seo-first-use elt)))
          (when (zerop (seo-count elt))
            (do ((tail (cdr tem) (cdr tail)))
                ((null tail))
              (when (and (eq (caar-safe tail) 'setq)
                         (eq (cadar tail) (seo-tempvar elt)))
                (setf (car tail) (caddar tail))
                (return)))))))
    ;; Now see which temp vars still remain in use,
    ;; and put on a binding for them.
    (let ((tempvars-used
            (loop for elt in nonconstant-alist
                  when (not (zerop (seo-count elt)))
                  collect (list (seo-tempvar elt) '(compiler:undefined-value)))))
      (if tempvars-used
          `(let ,tempvars-used ,value-so-far)
        value-so-far))))

(defun sublis-eval-once-1 (exp alist &optional reuse-flag sequential-flag)
  (cond ((null alist) exp)
        ((symbolp exp)
         (let ((tem (assq exp alist)))
           (cond ((null tem) exp)
                 ((seo-tempvar tem)
                  (incf (seo-count tem))
                  (seo-tempvar tem))
                 ((eq (seo-count tem) t)
                  (seo-exp tem))
                 (t
                  (setf (seo-tempvar tem)
                        (if reuse-flag (car tem) (gensym)))
                  (setf (seo-count tem) 0)
                  (setf (seo-first-use tem)
                        (cons 'progn nil))
                  (let ((e1
                          `(,@(loop for tail on *seo-first-uninserted-var*
                                    until (eq (car tail) tem)
                                    do (setf (seo-tempvar (car tail))
                                             (if reuse-flag (caar tail) (gensym)))
                                    (setf (seo-first-use (car tail)) (seo-first-use tem))
                                    collect `(setq ,(seo-tempvar (car tail))
                                                   ,(if sequential-flag
                                                        (sublis-eval-once-1
                                                          (seo-exp (car tail))
                                                          (ldiff alist tail))
                                                      (seo-exp (car tail))))
                                    finally (setq *seo-first-uninserted-var* (cdr tail)))
                            (setq ,(seo-tempvar tem)
                                  ,(if sequential-flag
                                       (sublis-eval-once-1 (seo-exp tem)
                                                           (ldiff alist (memq tem alist)))
                                     (seo-exp tem))))))
                    (setf (cdr (seo-first-use tem)) e1)
                    (seo-first-use tem))))))
        ((atom exp) exp)
        ;; Why do I bother?
        ;; EFH 4/7/87
        ((eq (car exp) 'si:displaced)
         `(si:displaced ,(second exp)
                        ,(sublis-eval-once-1 (third exp) alist reuse-flag sequential-flag)))
        (t
         (do ((tail exp (cdr tail))
              accum)
             ((atom tail)
              (nreconc accum tail))
           (push (sublis-eval-once-1 (car tail) alist reuse-flag sequential-flag)
                 accum)))))

(defun get-setf-method-multiple-value (form &optional (environment *macroexpand-environment*)
                                            &key short-cut &aux tem)
  "Return the canonical five values that say how to do SETF on FORM.
The values are:
* a list of symbols, gensyms, that stand for parts of FORM
* a list of the parts of FORM that they stand for
* a list of symbols, gensyms, that stand for the values to be stored
* an expression to do the storing.  It contains the gensyms described already.
* an expression to refer to the existing value of FORM.
  It differs from FORM in that it has the gensyms replacing the
  parts of FORM that they stand for.
These values give all the information needed to examine and set
 FORM repeatedly without evaluating any of its subforms more than once.

If SHORT-CUT is non-NIL, and if FORM's method of SETFing was defined
by a simple DEFSETF that just gives a function to do the setting,
then we return just two values: the setting function and a replacement FORM
/(differing from FORM by having macros expanded, CADR -> CAR (CDR ...), etc.).
The caller can tell that this case occurred because the first value
is a non-NIL symbol in this case, and is always a list in the normal case."
  (declare (values tempvars tempargs storevars storeform refform))
  (cond ((symbolp form)
         (let ((g (gensym)))
           (values nil nil (list g) `(setq ,form ,g) form)))
        ((atom form))
        ((not (symbolp (car form)))
         (ferror "~S non-symbolic function in ~S." (car form) 'setf))
        ((or (eq (getdecl (car form) 'setf-method) 'unsetfable)
             (eq (getdecl (car form) 'setf) 'unsetfable))
         (nosetf form))
        ((setq tem (getdecl (car form) 'setf-method))
         (if (symbolp tem)
             (if short-cut
                 (values tem form)
               (let ((gs (mapcar (lambda (ignore) (gensym)) (cdr form)))
                     (g (gensym)))
                 (values gs (cdr form) (list g)
                         `(,tem ,@gs ,g)
                         `(,(car form) ,@gs))))
           (if (eq (cdr tem) 'nosetf)
               (nosetf form))
           (call (cdr tem) () form :optional environment)))
        ((setq tem (getdecl (car form) 'setf-expand))
         (get-setf-method-multiple-value (funcall tem form) environment :short-cut short-cut))
        ((and (fboundp (car form))
              (arrayp (symbol-function (car form))))
         (get-setf-method-multiple-value `(zl:aref #',(car form) . ,(cdr form))
                                         environment :short-cut short-cut))
        ((and (fboundp (car form))
              (symbolp (symbol-function (car form))))
         (get-setf-method-multiple-value `(,(symbol-function (car form)) . ,(cdr form))
                                         environment :short-cut short-cut))
        ((not (eq form (setq form (macroexpand-1 form environment))))
         (get-setf-method-multiple-value form environment :short-cut short-cut))
        (t (ferror 'sys:unknown-setf-reference
                   "No way known to do SETF of ~S." (car form)))))

(defun nosetf (form)
  (declare (dbg:error-reporter))
  (ferror 'unknown-setf-reference
          "SETF is explicitly forbidden on ~S." (car form)))

(defun get-setf-method (form &optional (environment *macroexpand-environment*))
  "Return the canonical five values that say how to do SETF on FORM.
Like GET-SETF-METHOD-MULTIPLE-VALUE except that it will never return
more than one element in the third value, the STOREVARS."
  (declare (values tempvars tempargs storevars storeform refform))
  (multiple-value-bind (tempvars argforms storevars storeform accessform)
      (get-setf-method-multiple-value form environment)
    (if ( (length storevars) 1)
        (ferror "Number of store-variables not one, for ~S method of ~S."
                'setf form))
    (values tempvars argforms storevars storeform accessform)))

(defmacro define-setf-method (&environment env access-function lambda-list &body body)
  "General way to define how to SETF forms starting with ACCESS-FUNCTION.
This form defines a macro which will be invoked by GET-SETF-METHOD-MULTIPLE-VALUE.
The LAMBDA-LIST is matched, DEFMACRO-style, against the form to be SETF'd.
Then the BODY is executed and should produce five values to return from
GET-SETF-METHOD-MULTIPLE-VALUE.
See that function for a description of what the five values mean.
This is more general than DEFSETF because it can decide how to parse
the form to be SETF'd, decide which parts to replace with tempvars, and so on.

A trivial example would be
/(DEFINE-SETF-METHOD CAR (LIST)
  (LET ((TEMPVARS (LIST (GENSYM)))
        (TEMPARGS (LIST LIST))
        (STOREVAR (GENSYM)))
    (VALUES TEMPVARS TEMPARGS (LIST STOREVAR)
            `(SYS:SETCAR ,(FIRST TEMPVARS) ,STOREVAR)
            `(CAR ,(FIRST TEMPVARS)))))
which is equivalent to (DEFSETF CAR SETCAR)."
  (multiple-value-bind (real decls doc-string)
      (extract-declarations body nil t env)
    `(progn
       (set-documentation ',access-function 'setf ,doc-string)
       (defmacro (:property ,access-function setf-method) ,lambda-list
         (declare (function-parent ,access-function define-setf-method)
                  (documentation . ,doc-string)
                  . ,decls)
         . ,real))))

(defmacro defsetf (&environment environment access-function &optional arg1 arg2 &body body)
  "Define a SETF expander for ACCESS-FUNCTION.
DEFSETF has three forms:

The simple form  (DEFSETF access-function update-function [doc-string])
can be used as follows: After (DEFSETF GETFROB PUTFROB),
/(SETF (GETFROB A 3) FOO) ==> (PUTFROB A 3 FOO).

The complex form is like DEFMACRO:

/(DEFSETF access-function access-lambda-list newvalue-lambda-list body...)

except there are TWO lambda-lists.
The first one represents the argument forms to the ACCESS-FUNCTION.
Only &OPTIONAL and &REST are allowed here.
The second has only one argument, representing the value to be stored.
The body of the DEFSETF definition must then compute a
replacement for the SETF form, just as for any other macro.
When the body is executed, the args in the lambda-lists will not
really contain the value-expression or parts of the form to be set;
they will contain gensymmed variables which SETF may or may not
eliminate by substitution.

The third form is to prohibit SETF:

/(DEFSETF access-function)."
  ;; REF and VAL are arguments to the expansion function
  (if (null body)
      `(defdecl ,access-function setf-method
                ,(or arg1
                     '(macro . nosetf)))
    (let ((expansion (expand-two-list-defsetf access-function
                                           arg1 arg2 body)))
      (multiple-value-bind (nil decls doc-string)
          (extract-declarations body nil t environment)
        `(define-setf-method ,access-function ,arg1
           (declare . ,decls)
           ,doc-string
           ,expansion)))))

;;; >> We need a general argument-list parser !
(defun expand-two-list-defsetf (access-function access-ll value-names body)
  (let ((single-arg-names '())
        (rest-arg nil)) ; has to be treated specially
    (do* ((ll access-ll (cdr ll))
          (element (car ll) (car ll)))
         ((null ll))
      (cond ((eq element '&rest)
             (pop ll)
             (pop ll rest-arg))
            ((lisp:member element lambda-list-keywords))
            (t (push (etypecase element
                       (symbol element)
                       (cons (car element)))
                     single-arg-names))))
    (setq single-arg-names (nreverse single-arg-names))
    `(let* ((single-tempvars (setf-make-n-tempvars ,(length single-arg-names)))
            ,@(when rest-arg
                `((rest-tempvars (setf-make-n-tempvars (length ,rest-arg)))))
            (storevar (gensym))
            (tempvars ,(if rest-arg ; list of temporaries
                           '(append single-tempvars rest-tempvars)
                         'single-tempvars)))
       (values tempvars
               ,(if rest-arg ; list of forms that correspond to the temporaries
                    `(list* ,@single-arg-names ,rest-arg)
                  `(list ,@single-arg-names))
               (list storevar)
               ;; We put the body inside a let where the values of the parameters
               ;; of the lambda list aren't the actual forms, but the gensyms instead
               ;; (and actually, the rest arg is a LIST of gensyms).
               (let ((,(car value-names) storevar)
                     ;; Rebind single-argument parameters
                     ,@(loop for arg in single-arg-names
                              for i from 0
                              collect `(,arg (nth ,i tempvars)))
                     ;; Rebind &REST argument parameter
                     ,@(when rest-arg
                         `((,rest-arg rest-tempvars))))
                 . ,body)
               `(,',access-function . ,tempvars)))))

(defun setf-make-n-tempvars (n)
  (let ((temps (make-list n)))
    (mapl #'(lambda (cell) (setf (car cell) (gensym))) temps)))

(defmacro define-modify-macro (name additional-arglist action-function &optional doc-string)
  "Define a construct which, like INCF, modifies the value of its first argument.
NAME is defined so that (NAME place additional-args) expands into
  (SETF place (action-function place additional-args))
except that subforms of place are evaluated only once."
  (let (additional-arg-names)
    (dolist (x additional-arglist
               (setq additional-arg-names (reverse additional-arg-names)))
      (cond ((symbolp x)
             (if (not (memq x lambda-list-keywords)) (push x additional-arg-names)))
            (t ; it's a list after &optional
             (push (car x) additional-arg-names))))
    `(defmacro ,name (&environment .environment. place . ,additional-arglist)
       ,doc-string
       (if (symbolp place)
           ;; Special case this to speed up the expansion process and make better code.
           `(setq ,place (,',action-function ,place . ,(list . ,additional-arg-names)))
         (multiple-value-bind (tempvars tempargs storevars storeform refform)
             (get-setf-method place .environment.)
           (let ((additional-temps (mapcar (lambda (ignore) (gensym)) ',additional-arg-names)))
             (sublis-eval-once (pairlis tempvars tempargs
                                        (pairlis additional-temps (list . ,additional-arg-names)))
                               (sublis-eval-once (list (cons (car storevars)
                                                             `(,',action-function ,refform
                                                               . ,additional-temps)))
                                                 storeform
                                                 t t .environment.)
                               t t .environment.)))))))


(define-modify-macro incf (&optional (delta 1)) +
  "Increment PLACE's value by DELTA.")

(define-modify-macro decf (&optional (delta 1)) -
  "Decrement PLACE's value by DELTA.")

(defmacro push (&environment environment value place &key area)
  "Add ITEM to the front of the list PLACE, using CONS.
Works by SETF'ing PLACE."
  (if (symbolp place)
      ;; Special case this to speed up the expansion process and make better code.
      (if area
          `(setq ,place (cons-in-area ,value ,place ,area))
          `(setq ,place (cons ,value ,place)))
    (multiple-value-bind (tempvars tempargs storevars storeform refform)
        (get-setf-method place environment)
      (let ((val (gensym))
            (air (if area (gensym))))
        (sublis-eval-once (if area
                              (list* `(,val . ,value) `(,air . ,area) (pairlis tempvars tempargs))
                              (list* `(,val . ,value) (pairlis tempvars tempargs)))
                          (sublis-eval-once (list (cons (car storevars)
                                                        (if area
                                                            `(cons-in-area ,val ,refform ,area)
                                                            `(cons ,val ,refform))))
                                            storeform
                                            t t environment)
                          t t environment)))))

(defmacro push-in-area (area value place)
  ;; who cares about order-of-evaluation for this?
  `(push ,value ,place :area ,area))
(compiler:make-obsolete push-in-area "use PUSH with the :AREA keyword.")

(defmacro pop (&environment environment place &optional into-place)
  "Remove the first element from the list PLACE, and return that element.
Works by SETF'ing PLACE.  If INTO-PLACE is specified, store the
value there as well as returning it."
  (if (and (symbolp place)
           (symbolp into-place))
      ;; Special case this to speed up the expansion process and make better code.
      (if into-place
          `(prog1 (setf ,into-place (car ,place)) (setq ,place (cdr ,place)))
        `(prog1 (car ,place) (setq ,place (cdr ,place))))
    (if into-place
        (multiple-value-bind (into-tempvars into-tempargs into-storevars into-storeform)
            (get-setf-method into-place environment)
          (multiple-value-bind (tempvars tempargs storevars storeform refform)
              (get-setf-method place environment)
            (sublis-eval-once (pairlis tempvars tempargs
                                       (pairlis into-tempvars into-tempargs))
                              `(prog1 ,(sublis (list (cons (car into-storevars)
                                                           `(car ,refform)))
                                               into-storeform)
                                      ,(sublis-eval-once (list (cons (car storevars)
                                                                     `(cdr ,refform)))
                                                         storeform
                                                         t t environment))
                              t t environment)))
      (multiple-value-bind (tempvars tempargs storevars storeform refform)
          (get-setf-method place environment)
        (sublis-eval-once (pairlis tempvars tempargs)
                          `(prog1 (car ,refform)
                                  ,(sublis-eval-once (list (cons (car storevars)
                                                                 `(cdr ,refform)))
                                                     storeform
                                                     t t environment))
                          t t environment)))))

(defmacro pushnew (&environment environment value place &rest options)
  "Add VALUE to the front of the list PLACE, if it's not already MEMQ there.
Equivalent to (SETF PLACE (ADJOIN VALUE PLACE OPTIONS...))
but evaluates subforms of PLACE only once."
  (declare (arglist value place &key test test-not key area))
  (or (optimize-pushnew value place options environment)
      (multiple-value-bind (tempvars tempargs storevars storeform refform)
          (get-setf-method place environment)
        (let ((val (gensym)))
          (sublis-eval-once (cons `(,val . ,value) (pairlis tempvars tempargs))
                            (sublis-eval-once (list (cons (car storevars)
                                                          `(adjoin ,val ,refform . ,options)))
                                              storeform
                                              t t environment)
                            t t environment)))))

(defvar *test-member-alist*
        '((eq . memq)
          (equal . member-equal)
          (eql . member-eql)
          (equalp . member-equalp))
  "Alist of test functions and functions which can be used to check whether any member
of a list satisfies the test. Eg (EQ . MEMQ)")

;;>> This is wrong:  ADJOIN is the thing which should be optimized.
;;>>>>> Yes. In fact, ADJOIN really is better already; the only case OPTIMIZE-PUSHNEW covers
;;      and ADJOIN doesn't is MEMBER-EQUALP, which isn't itself optimized beyond
;;      normal tree-walking.  The optimizer below perversely doesn't want to do :TEST-NOT,
;;      nor is it general to any kind of function.  To be flushed in rel 5?   -Keith 6/25/88

(defun optimize-pushnew (value place options environment)
  (if (or (not (symbolp place)) (oddp (length options)))
      ;; stupid optimizer doesn't try to win on this.
      nil
    (let (test xtest)
      (do ((x options (cddr x)))
          ((null x))
        (cond ((or (eq (car x) ':test)
                   (equal (car x) '':test))
               (if (not test)
                   ;; note: cannot use (function test) as this would lose inside FLET
                   ;;  redefinition of test
                   (cond ((list-match-p (cadr x) `(quote ,test)))
                         ((and (list-match-p (cadr x) `(function ,test))
                               (symbolp test)   ;or else fsymeval-in-environment blows out
                               (not (fsymeval-in-environment test environment nil))))
                         (t (return (setq test nil))))))
              (t (setq test nil) (return))))
      (if (null options) (setq test 'eql))
      (cond ((and (symbolp test) (not (null test)))
             (setq xtest (cdr (assq test *test-member-alist*)))
             (flet ((make-test (item)
                               (if xtest
                                   `(,xtest ,item ,place)
                                 (let ((x (gensym)))
                                   `(do ((,x ,place (cdr ,x)))
                                        ((null ,x))
                                      ;;Compare ITEM to (FUNCALL <:key> ELEMENT) !
                                      (if (,test ,item (car ,x)) (return ,x)))))))
               (if (or (symbolp value) (constantp value))
                   `(progn (or ,(make-test value)
                               (push ,value ,place))
                           ,place)
                   (let ((tem (gensym)))
                     `(let ((,tem ,value))
                        (or ,(make-test tem)
                            (push ,tem ,place))
                        ,place)))))
            ;; stupid optimizer doesn't try to hack this either...
            (t nil)))))

(defmacro setf (&environment environment &rest places-and-values)
  "Sets the value of PLACE to be VALUE.  Allows any number of places and values, like SETQ.
For example, (SETF (AREF A I) NEWVALUE) sets the value of (AREF A I)."
  (declare (arglist place value /...))
  (if (oddp (length places-and-values)) (ferror "Odd number of arguments to ~S" 'setf))
  `(progn
     . ,(loop for (place value) on places-and-values by 'cddr
              collect
              (multiple-value-bind (tempvars tempargs storevars storeform)
                  (get-setf-method-multiple-value place environment :short-cut t)
                (if (and tempvars (symbolp tempvars))
                    ;; Handle case of simple DEFSETF as fast as possible.
                    `(,tempvars ,@(cdr tempargs) ,value)
                  (sublis-eval-once (pairlis tempvars tempargs
                                             (list (cons (car storevars) value)))
                                    storeform t t environment))))))

(define-setf-method ldb (&environment environment bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((btemp (gensym))
          (store (gensym))
          (itemp (first stores)))
;      ;; If the accessor form for the word the byte is in takes some time,
;      ;; make a variable to hold its value.
;Problem is, it cannot get rid of this temp variable even if it is used only once,
;because it is needed AFTER the value-to-store-in-the-byte
;but it is positioned before that tempvar.
;      (when (and (consp access-form)
;                (not (memq (car access-form) '(car cdr caar cadr cdar cddr ar-1 array-leader))))
;       (let ((oldvaltemp (gensym)))
;         (setq temps (append temps (list oldvaltemp)))
;         (setq vals (append vals (list access-form)))
;         (setq access-form oldvaltemp)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              (sublis-eval-once (list (cons itemp `(dpb ,store ,btemp ,access-form)))
                                `(progn
                                   ,store-form
                                   ,store)
                                t t environment)
              `(ldb ,btemp ,access-form)))))

(define-setf-method ldb-test (&environment environment bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((btemp (gensym))
          (store (gensym))
          (itemp (first stores)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              (sublis-eval-once (list (cons itemp `(dpb (if ,store 1 0) ,btemp ,access-form)))
                                `(progn
                                   ,store-form
                                   ,store)
                                t t environment)
              `(ldb-test ,btemp ,access-form)))))

(define-setf-method %logldb (&environment environment bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((btemp (gensym))
          (store (gensym))
          (itemp (first stores)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              `(progn
                 ,(sublis (list (cons itemp `(%logdpb ,store ,btemp ,access-form)))
                          store-form)
                 ,store)
              `(%logldb ,btemp ,access-form)))))

(define-setf-method %logldb-test (&environment environment bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((btemp (gensym))
          (store (gensym))
          (itemp (first stores)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              `(progn
                 ,(sublis (list (cons itemp `(%logdpb (if ,store 1 0) ,btemp ,access-form)))
                          store-form)
                 ,store)
              `(%logldb-test ,btemp ,access-form)))))

(define-setf-method mask-field (&environment environment bytespec int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((btemp (gensym))
          (store (gensym))
          (itemp (first stores)))
      (values (cons btemp temps)
              (cons bytespec vals)
              (list store)
              `(progn
                 ,(sublis (list (cons itemp `(deposit-field ,store ,btemp ,access-form)))
                          store-form)
                 ,store)
              `(mask-field ,btemp ,access-form)))))

;; Copied from LAD: RELEASE-3.SYS2; SETF.LISP#117 on 2-Oct-86 04:41:10
(defsetf char-bit (char bit-name) (new-value)
  `(progn (setf-char-bit ,char ,bit-name ,new-value)
          ,new-value))

;; Copied from LAD: RELEASE-3.SYS2; SETF.LISP#117 on 2-Oct-86 04:41:11
(define-modify-macro setf-char-bit (bit-name new-value)
  set-char-bit
  "Modify macro version of SET-CHAR-BIT,
Modifies the bits of the character in PLACE.
For internal use of expansion of (SETF (CHAR-BIT ...)).")


(defmacro rotatef (&environment environment &rest places)
  "Rotates the values between all the specified PLACEs.
The second PLACE's value is put into the first PLACE,
the third PLACE's value into the second PLACE, and so on,
and the first PLACE's value is put into the last PLACE."
  (let ((setf-methods
          (mapcar (lambda (place)
                    (multiple-value-list (get-setf-method place environment)))
                  places)))
    (sublis-eval-once
      (mapcan (lambda (setf-method)
                (pairlis (first setf-method)
                         (second setf-method)))
              setf-methods)
      `(let ((,(car (third (car (last setf-methods))))
              ,(fifth (car setf-methods))))
         ,.(loop for i from 1 below (length places)
                 collect (sublis (list (cons (car (third (nth (1- i) setf-methods)))
                                             (fifth (nth i setf-methods))))
                                 (fourth (nth (1- i) setf-methods))))
         ,(fourth (car (last setf-methods)))
         nil)
      t t environment)))

(deff swapf 'rotatef)

(defmacro shiftf (&environment environment &rest places-and-final-value)
  "Copies values into each PLACE from the following one.
The last PLACE need not be SETF'able, as it is only accessed
 to get a value to put in the previous PLACE; it is not set.
The first PLACE's original value is returned as the value of the SHIFTF form."
  (declare (arglist place place /...))
  (let* ((places (butlast places-and-final-value))
         (value (car (last places-and-final-value)))
         (setf-methods
           (mapcar (lambda (place)
                     (multiple-value-list (get-setf-method place environment)))
                   places)))
    (sublis-eval-once
      (nconc (mapcan (lambda (setf-method)
                       (pairlis (first setf-method)
                                (second setf-method)))
                     setf-methods)
             (list (cons (car (third (car (last setf-methods)))) value)))
      `(prog1
         ,(fifth (car setf-methods))
         ,.(loop for i from 1 below (length places)
                 collect (sublis (list (cons (car (third (nth (1- i) setf-methods)))
                                             (fifth (nth i setf-methods))))
                                 (fourth (nth (1- i) setf-methods))))
         ,(fourth (car (last setf-methods))))
      t t environment)))

(defmacro psetf (&environment environment &rest rest)
  "Like SETF, but no variable value is changed until all the values are computed.
The returned value is undefined."
  ;; To improve the efficiency of do-stepping, by using the SETE-CDR, SETE-CDDR,
  ;; SETE-1+, and SETE-1- instructions, we try to do such operations with SETF
  ;; rather than PSETF.  To avoid having to do full code analysis, never rearrange
  ;; the order of any code when doing this, and only do it when there are no
  ;; variable name duplications.
  (loop for (val var) on (reverse rest) by 'cddr
        with setfs = nil with psetfs = nil
     do (if (and (null psetfs)
                 (memq (car-safe val) '(1+ 1- cdr cddr))
                 (not (fsymeval-in-environment (car val) environment nil))
                 (eq (cadr val) var)
                 (not (memq var setfs)))
            (setq setfs (cons var (cons val setfs)))
            (setq psetfs (cons var (cons val psetfs))))
     finally
       (labels ((prog1ify (x)
                   (cond ((null x) nil)
                         ((null (cddr x)) (cons 'setf x))
                         (t `(setf ,(car x) (prog1 ,(cadr x) ,(prog1ify (cddr x))))))))
         (setq psetfs (prog1ify psetfs))
         (return (cond ((null setfs) psetfs)
                       ((null psetfs) (cons 'setf setfs))
                       (t `(progn ,psetfs (setf . ,setfs))))))))

(define-setf-method apply (&environment environment function &rest args)
  (unless (and (consp function)
               (memq (car function) '(function quote))
               (eq (length function) 2)
               (symbolp (cadr function)))
    (ferror 'sys:unknown-setf-reference
            "In ~S of ~S, the function ~Sed must be a constant." 'self 'apply 'apply))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method (cons (cadr function) args) environment)
    (if (memq (cadr function) '(zl:aref cl:aref))       ;why special-cased???
        (setq storeform
              `(aset ,(car (last storeform)) . ,(butlast (cdr storeform)))))
    (if (not (eq (car (last storeform)) (car (last tempvars))))
        (ferror 'sys:unknown-setf-reference
                "~S not acceptable within ~S within SETF." function)
      (values tempvars tempargs storevars
              `(apply #',(car storeform) . ,(cdr storeform))
              `(apply #',(car refform) . ,(cdr refform))))))

(defmacro locf (accessor &environment environment)
  "Return a locative pointer to the place where ACCESSOR's value is stored.
Note that (LOCF (CDR SOMETHING)) is normally equivalent to SOMETHING,
which may be a list rather than a locative."
  (loop
    (let (fcn)
      (cond ((symbolp accessor)                 ;Special case needed.
             (return `(variable-location ,accessor)))
            ((not (symbolp (car accessor)))
             (ferror "~S non-symbolic function in ~S" (car accessor) 'locf))
            ;;>> This is OK for now, since environment only includes the lexical stuff
            ;;>>  around the current function.  However, when environment includes stuff
            ;;>>  for a whole compilation or set of compilations, we will have to getdecl
            ;;>>  again (being careful not to getdecl for lexically defined functions!)
            ((unless (fsymeval-in-environment (car accessor) environment nil)
               (cond ((eq (getdecl (car accessor) 'locf) 'unlocfable)
                      (nolocf accessor))
                     ((setq fcn (getdecl (car accessor) 'locf-method))
                      (if (symbolp fcn)
                          (return (cons fcn (cdr accessor)))
                        (if (eq (cdr fcn) 'nolocf)
                            (nolocf accessor))
                        (return (call (cdr fcn) nil accessor :optional environment))))
                     ((setq fcn (getdecl (car accessor) 'setf-expand))
                      (setq accessor (funcall fcn accessor)))
                     ((and (fboundp (car accessor)) (arrayp (symbol-function (car accessor))))
                      (return `(aloc #',(car accessor) . ,(cdr accessor))))
                     ((and (fboundp (car accessor)) (symbolp (symbol-function (car accessor))))
                      (return `(locf (,(symbol-function (car accessor)) . ,(cdr accessor))))))))
            ((not (eq accessor (setq accessor (macroexpand-1 accessor environment)))))
            (t (ferror 'sys:unknown-locf-reference
                       "No way known to do LOCF on ~S." (car accessor)))))))

(defmacro deflocf (access-function &optional arg1 &body body)
  "Define how to LOCF forms starting with ACCESS-FUNCTION
Examples:
/(DEFLOCF CAR SI:CAR-LOCATION) or
/(DEFLOCF CAR (X) `(SI::CAR-LOCATION ,X)).
/(DEFLOCF FOO) explicitly forbids doing LOCF on (FOO ...)."
  (declare (arglist access-function &optional lambda-list-or-location-function &body body))
  (if (null body)
      `(defdecl ,access-function
                locf-method
                ,(or arg1
                     '(macro . nolocf)))
    `(defmacro (:property ,access-function locf-method)
               ,arg1 . ,body)))

(defun nolocf (form)
  (declare (dbg:error-reporter))
  (ferror 'unknown-locf-reference
          "LOCF is explicitly forbidden on ~S." (car form)))

;(GET-LIST-POINTER-INTO-STRUCT (element pntr))
(defun get-list-pointer-into-struct macro (x)
  (prog (ref)
    (setq ref (macroexpand (cadr x)     ;EXPAND MACROS LOOKING AT BAG-BITING MACRO LIST
                           nil))        ;**** environment BLETCH
    (cond ((eq (car ref) 'ar-1)
           (return (list 'get-list-pointer-into-array
                         (list 'funcall (cadr ref) (caddr ref)))))
          ((error "LOSES - GET-LIST-POINTER-INTO-STRUCT" x)))))
(compiler:make-obsolete get-list-pointer-into-struct "if you are using this, you are losing")

;;;; Primitive DEFSETFs for simple functions.

(defsetf zl:aref set-aref)
(defsetf common-lisp-aref set-aref)
(defsetf cl:aref set-aref)
(defsetf zl:ar-1 set-ar-1)
(defsetf common-lisp-ar-1 set-ar-1)
(defsetf cli:ar-1 set-ar-1)
(defsetf zl:ar-1-force set-ar-1-force)
(defsetf common-lisp-ar-1-force set-ar-1-force)
(defsetf cli:ar-1-force set-ar-1-force)
(defsetf char set-ar-1)
(defsetf schar set-ar-1)
(defsetf bit set-aref)
(defsetf sbit set-aref)
(defsetf svref set-ar-1)

(defsetf compiler::%reference-simple-q-vector compiler::%set-simple-q-vector)

(defsetf ar-2 set-ar-2)
(defsetf ar-3 set-ar-3)
(defsetf array-leader set-array-leader)
(defsetf fill-pointer set-fill-pointer)

(defsetf %instance-ref set-%instance-ref)
(defsetf elt setelt)
(defsetf plane-aref plane-aset)

(deflocf zl:aref aloc)
(deflocf cl:aref aloc)
(deflocf common-lisp-aref aloc)
(deflocf zl:ar-1 ap-1)
(deflocf common-lisp-ar-1 ap-1)
(deflocf cli:ar-1 ap-1)
(deflocf zl:ar-1-force ap-1-force)
(deflocf common-lisp-ar-1-force ap-1-force)
(deflocf cli:ar-1-force ap-1-force)
(deflocf char ap-1)
(deflocf schar ap-1)
(deflocf bit)
(deflocf sbit)
(deflocf svref ap-1)

(deflocf ar-2 ap-2)
(deflocf ar-3 ap-3)
(deflocf array-leader ap-leader)

(deflocf %instance-ref %instance-loc)

(defsetf ar-2-reverse (array i j) (value)
  `(as-2-reverse ,value ,array ,i ,j))

(defun (:property arraycall setf-expand) (form)
  `(zl:aref . ,(cddr form)))

(defsetf car setcar)
(defsetf cdr setcdr)
(deflocf car car-location)
(deflocf cdr identity)

(defun (:property caar setf-expand) (form)
  `(car (car ,(cadr form))))
(defun (:property cadr setf-expand) (form)
  `(car (cdr ,(cadr form))))
(defun (:property cdar setf-expand) (form)
  `(cdr (car ,(cadr form))))
(defun (:property cddr setf-expand) (form)
  `(cdr (cdr ,(cadr form))))
(defun (:property caaar setf-expand) (form)
  `(car (caar ,(cadr form))))
(defun (:property caadr setf-expand) (form)
  `(car (cadr ,(cadr form))))
(defun (:property cadar setf-expand) (form)
  `(car (cdar ,(cadr form))))
(defun (:property caddr setf-expand) (form)
  `(car (cddr ,(cadr form))))
(defun (:property cdaar setf-expand) (form)
  `(cdr (caar ,(cadr form))))
(defun (:property cdadr setf-expand) (form)
  `(cdr (cadr ,(cadr form))))
(defun (:property cddar setf-expand) (form)
  `(cdr (cdar ,(cadr form))))
(defun (:property cdddr setf-expand) (form)
  `(cdr (cddr ,(cadr form))))
(defun (:property caaaar setf-expand) (form)
  `(car (caaar ,(cadr form))))
(defun (:property caaadr setf-expand) (form)
  `(car (caadr ,(cadr form))))
(defun (:property caadar setf-expand) (form)
  `(car (cadar ,(cadr form))))
(defun (:property caaddr setf-expand) (form)
  `(car (caddr ,(cadr form))))
(defun (:property cadaar setf-expand) (form)
  `(car (cdaar ,(cadr form))))
(defun (:property cadadr setf-expand) (form)
  `(car (cdadr ,(cadr form))))
(defun (:property caddar setf-expand) (form)
  `(car (cddar ,(cadr form))))
(defun (:property cadddr setf-expand) (form)
  `(car (cdddr ,(cadr form))))
(defun (:property cdaaar setf-expand) (form)
  `(cdr (caaar ,(cadr form))))
(defun (:property cdaadr setf-expand) (form)
  `(cdr (caadr ,(cadr form))))
(defun (:property cdadar setf-expand) (form)
  `(cdr (cadar ,(cadr form))))
(defun (:property cdaddr setf-expand) (form)
  `(cdr (caddr ,(cadr form))))
(defun (:property cddaar setf-expand) (form)
  `(cdr (cdaar ,(cadr form))))
(defun (:property cddadr setf-expand) (form)
  `(cdr (cdadr ,(cadr form))))
(defun (:property cdddar setf-expand) (form)
  `(cdr (cddar ,(cadr form))))
(defun (:property cddddr setf-expand) (form)
  `(cdr (cdddr ,(cadr form))))

(defun (:property nth setf-expand) (form)
  `(car (nthcdr . ,(cdr form))))

(defun (:property nthcdr setf-expand) (form)
  `(cdr (nthcdr (1- ,(cadr form)) ,(caddr form))))

(defsetf symeval set)
(deflocf symeval value-cell-location)
(defsetf fsymeval fset)
(deflocf fsymeval function-cell-location)
(defsetf symbol-value set)
(deflocf symbol-value value-cell-location)
(defsetf symbol-function fset)
(deflocf symbol-function function-cell-location)
(defsetf symeval-in-closure set-in-closure)
(deflocf symeval-in-closure locate-in-closure)
(defsetf symbol-package (symbol) (package)
  `(setcar (package-cell-location ,symbol) ,package))
(deflocf symbol-package package-cell-location)

(defsetf plist setplist)
(deflocf plist plist-location)

(defsetf get (object property &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,property ,default) property)))
    `(setprop ,object ,tem ,value)))
(deflocf get get-location)

(defsetf get-from-area (object property area &optional (default nil defaultp)) (value)
  (let ((tem (if defaultp `(prog1 ,property ,default) property)))
    `(setprop-in-area ,object ,tem ,value ,area)))
(deflocf get-from-area get-location-from-area)

;;; A SETF for GETF that doesn't use VARIABLE-LOCATION, the only locative-generating
;;; special form in the LMI implementation of Zetalisp.

;;; Unfortunately, this is inadequate.
;;; Try compiling (SETF (GETF (GETHASH SYM (STRUCT-HASH-TABLE-SLOT STRUCT)) IND) VAL).
;;; - smh 9aug88
#|
(defsetf getf (place property &optional default) (new)
  ;; We expand into something called SETPROPF so that the compiler will notice this
  ;; and optimize the case of using a variable as a place back into using a locative.
  `(setpropf ,place ,property ,new))
|#

(define-setf-method getf (&environment environment place indicator &optional default)
  (if (symbolp place)
      ;; $$$ A recent fix here was creating circular lists in the
      ;; (admittedly weird) case of (PUSH 'hi (GETF foo :prop)).  Now we
      ;; do essentially what the old simple setf-method did (see above)
      ;; in the case of a PLACE that is a symbol.  <31-Oct-88 keith+jim>
      (let ((new (gensym "NEW-"))
            (tempvars (setf-make-n-tempvars 3)))
        (values tempvars
                (list place indicator default)
                (list new)
                ;; We expand into something called SETPROPF so that the
                ;; compiler will notice this and optimize the case of
                ;; using a variable as a place back into using a
                ;; locative.
                `(setpropf ,(nth 0 tempvars) ,(nth 1 tempvars) ,new)
                `(getf . ,tempvars)))
    ;; An important optimization: handle non-symbol PLACEs differently:
    (let ((indicator-temp (gensym "INDICATOR-"))
          (default-temp (gensym "DEFAULT-"))
          (plloc (gensym "PLLOC-"))
          (store-temp (gensym "STORE-")))
      (multiple-value-bind (tempvars tempargs storevars storeform refform)
          (get-setf-method place environment)
        (values (list* indicator-temp default-temp tempvars)
                (list* indicator      default      tempargs)
                (list store-temp)
                `(let ((,(car storevars) ,refform))
                   (do ((,plloc ,(car storevars) (cddr ,plloc)))
                       ((null ,plloc)
                        (setq ,(car storevars) (list* ,indicator-temp ,store-temp ,(car storevars)))
                        ,storeform)
                     (when (eq (car ,plloc) ,indicator-temp)
                       (setf (cadr ,plloc) ,store-temp)
                       (return)))
                   ,store-temp)
                `(getf ,refform ,indicator-temp))))))

(deflocf getf (place &rest args)
  `(get-location (locf ,place) ,@args))

;;; >>> I'm not sure if it's worth going to this trouble to avoid generating
;;; >>> VARIABLE-LOCATION.  It seems trivial to just teach a Common Lisp code walker
;;; >>> about it. --RpK 21-Oct-86 15:23:29
(defmacro setpropf (place property new)
  ;; (if (symbolp place) ;; Avoid using locatives
  ;;    `(setq ,place (altered-property-list ,place ,property ,new))
    `(setf (get (locf ,place) ,property) ,new))

;;; Should only end up being used in interpreted code, due to the optimizer
(defun altered-property-list (plist property newval)
  (let ((newplist plist))
    (setprop (locf newplist) property newval)
    newplist))

(defsetf zl:arg setarg)
(defsetf %unibus-read %unibus-write)
(defsetf %xbus-read %xbus-write)
(defsetf %io-space-read %io-space-write)
(defsetf %nubus-read %nubus-write)

(defsetf %p-contents-offset (base offset) (value)
  (declare (unspecial base))                    ;gag me with a forwarded value cell
  `(%p-store-contents-offset ,value ,base ,offset))

(defsetf %p-ldb (ppss base) (value)
  (declare (unspecial base))
  `(%p-dpb ,value ,ppss ,base))

(defsetf %p-ldb-offset (ppss base offset) (value)
  (declare (unspecial base))
  `(%p-dpb-offset ,value ,ppss ,base ,offset))

(defsetf %p-mask-field (ppss base) (value)
  (declare (unspecial base))
  `(%p-deposit-field ,value ,ppss ,base))

(defsetf %p-mask-field-offset (ppss base offset) (value)
  (declare (unspecial base))
  `(%p-deposit-field-offset ,value ,ppss ,base ,offset))

(defsetf %p-data-type %p-store-data-type)
(defsetf %p-cdr-code %p-store-cdr-code)
(defsetf %p-pointer %p-store-pointer)

(define-setf-method %pointer (&environment environment int)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-method int environment)
    (let ((store (gensym))
          (itemp (first stores)))
      (values temps
              vals
              (list store)
              (sublis-eval-once (list (cons itemp `(%make-pointer (%data-type ,access-form)
                                                                  ,store)))
                                `(progn
                                   ,store-form
                                   ,store)
                                t t environment)
              `(%pointer ,access-form)))))

(defsetf fdefinition (function-spec) (definition)
  `(fdefine ,function-spec ,definition nil t))
(deflocf fdefinition fdefinition-location)

(defsetf function-spec-get (function-spec property) (value)
  `(function-spec-putprop ,function-spec ,value ,property))

(defsetf documentation set-documentation)
(defsetf macro-function set-macro-function)

(define-setf-method the (type place)
  ;; Handle (values t1 t2 .. tn), but we can't do anything interesting with
  ;; the other values for now, though this method could handle it.
  ;; By the way, this is mostly for the benefit of the microcompiler.
  (when (eq (car-safe type) 'values)
    (setq type (second type)))
  (multiple-value-bind (tempvars tempargs storevars storeform refform)
      (get-setf-method place)
    (values tempvars tempargs storevars
            (sublis (mapcar (lambda (var) (cons var `(the ,type ,var)))
                            storevars)
                    storeform)
            `(the ,type ,refform))))

(define-setf-method values (&rest places)
  (let ((g (gensym)))
    (values nil nil (list g)
            ;;>> this is multiple-value-setf, not multiple-value-setq
            `(multiple-value ,places ,g)
            g)))

;;; Consider these for a while and tell me honestly that Common LOSS
;;; is well defined.
;(defun foo ()
;  (labels ((ifoo () (format t "Loss")))
;    (setf (function ifoo) #'(lambda () (format t "Win")))
;    (function ifoo)))

;(defun bar ()
;  (let ((ifoo #'(lambda () (format t "Loss"))))
;    (setf ifoo #'(lambda () (format t "Win")))
;    ifoo))

(define-setf-method function (function-spec)
  (if (validate-function-spec function-spec)
      (let ((g (gensym)))
;;; It is unclear if this is too obnoxious to put in the system, certainly we should
;;; let the luser know if his code is going to fail, but this sort of construct
;;; may be used frequently.
        (compiler:warn 'setf-of-function :implausible
                       "(SETF (FUNCTION ~S)  ...) wont work on an internal function.~@
                        Use FDEFINE on global functions." function-spec)
        (values nil nil (list g)
                ;;>> BUG!  Doesn't work on lexically-defined functions
                ;;>>  (not that anybody ever said that it should)
                `(fdefine ',function-spec ,g)
                `(function ,function-spec)))
    (ferror 'sys:unknown-setf-reference
            "Cannot SETF a form (FUNCTION x) unless x is a function spec.")))

(deflocf function (function-spec)
  (if (validate-function-spec function-spec)
      ;;>> BUG!  Doesn't work on lexically-defined functions
      `(fdefinition-location ',function-spec)
    (ferror 'sys:unknown-locf-reference
            "Cannot SETF a form (FUNCTION x) unless x is a function spec.")))

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
    (setf (car (last new-body))
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
    (setf (car (last new-body))
          `(locf ,(car (last new-body))))
    `(progn . ,new-body)))

(deflocf let (boundvars &rest forms)
  (let ((new-body (copy-list forms)))
    (setf (car (last new-body))
          `(locf ,(car (last new-body))))
    `(let ,boundvars . ,new-body)))

(deflocf setq (a b)
  (let ((at (gensym)))
    `(let ((,at (locf ,b)))
       (setq ,a (car ,at))
       ,at)))


(define-setf-method send (function arg1 &rest args)
  (let ((tempvars (list* (gensym) (mapcar (lambda (ignore) (gensym)) args)))
        (storevar (gensym)))
    (values tempvars (cons function args) (list storevar)
            `(send ,(car tempvars) :set ,arg1 ,@(cdr tempvars)
                   ,storevar)
            `(send ,(car tempvars) ,arg1 . ,(cdr tempvars)))))

;Wasn't really needed - but is it a good idea?
;(define-setf-method not (&environment environment boolean)
;  (multiple-value-bind (tempvars tempargs storevars storeform refform)
;      (get-setf-method boolean environment)
;    (values tempvars tempargs storevars
;           (sublis (list (cons (car storevars)
;                               `(not ,(car storevars))))
;                   storeform)
;           `(not ,refform))))

;;; Handle (SETF (DONT-OPTIMIZE (defsubst-function args)) ...)
;;; Return a call to a function that will be created at load time.

(define-setf-method dont-optimize (ref)
  (let* ((fun (car ref))
         (def (and (fdefinedp fun) (fdefinition (unencapsulate-function-spec fun)))))
    (unless (memq (car (cond ((consp def) def)
                             ((typep def 'compiled-function)
                              (cadr (assq 'interpreted-definition
                                          (debugging-info def))))))
                  '(subst cl:subst named-subst))
      (ferror 'unknown-setf-reference "~S is not a subst function." fun))
    (let ((tempvars (mapcar (lambda (ignore) (gensym)) (cdr ref)))
          (storevar (gensym)))
      (values tempvars (cdr ref) (list storevar)
              `(funcall (quote-eval-at-load-time (setf-function ',fun ,(1- (length ref))))
                        ,@tempvars ,storevar)))))

;;; Return the function to do the work of setf'ing FUNCTION applied to NARGS args.
;;; If no such function has been created yet, one is created now.
;;; We must have different functions for different numbers of args
;;; so that problems with how args get defaulted are avoided.
(defun setf-function (function nargs)
  (or (nth nargs (get function 'run-time-setf-functions))
      (let* ((vars (setf-function-n-vars nargs))
             (name (make-symbol (format nil "SETF-~A-~D-ARGS" function nargs) t)))
;This may not be a good idea, really.
;       (if (symbol-package function)
;           (intern name (symbol-package function)))
        (compiler::compile-now-or-later name
                                        `(lambda (,@vars val)
                    (setf (,function . ,vars) val)))
        (unless (> (length (get function 'run-time-setf-functions)) nargs)
          (setprop function 'run-time-setf-functions
                   (append (get function 'run-time-setf-functions)
                           (make-list (- (1+ nargs)
                                         (length (get function 'run-time-setf-functions)))))))
        (setf (car (nthcdr nargs (get function 'run-time-setf-functions)))
              name)
        name)))

(deflocf dont-optimize (form)
  (let* ((fun (car form))
         (def (and (fdefinedp fun) (fdefinition (unencapsulate-function-spec fun)))))
    (unless (memq (car (cond ((consp def) def)
                             ((typep def 'compiled-function)
                              (cadr (assq 'interpreted-definition
                                          (debugging-info def))))))
                  '(subst cl:subst named-subst))
      (ferror 'unknown-locf-reference "~S is not a subst function." fun))
    `(funcall (quote-eval-at-load-time (locf-function ',fun ,(1- (length form))))
              ,@(cdr form))))

(defun locf-function (function nargs)
  (or (nth nargs (get function 'run-time-locf-functions))
      (let* ((vars (setf-function-n-vars nargs))
             (name (make-symbol (format nil "LOCF-~A-~D-ARGS" function nargs) t)))
;This may not be a good idea, really.
;       (if (symbol-package function)
;           (intern name (symbol-package function)))
        (compiler::compile-now-or-later name
                                        `(lambda ,vars
                                           (locf (,function . ,vars))))
        (unless (> (length (get function 'run-time-locf-functions)) nargs)
          (setprop function 'run-time-locf-functions
                   (append (get function 'run-time-locf-functions)
                           (make-list (- (1+ nargs)
                                         (length (get function 'run-time-locf-functions)))))))
        (setf (car (nthcdr nargs (get function 'run-time-locf-functions)))
              name)
        name)))

(defvar *setf-function-n-vars* nil)

(defun setf-function-n-vars (n)
  "Return a list of N distinct symbols.
The symbols are reused each time this function is called."
  (do () (( (length *setf-function-n-vars*) n))
    (push (gensym) *setf-function-n-vars*))
  (firstn n *setf-function-n-vars*))

;;;; Not yet converted.

;;; Handle SETF of backquote expressions, for decomposition.
;;; For example, (SETF `(A ,B (D ,XYZ)) FOO)
;;; sets B to the CADR and XYZ to the CADADDR of FOO.
;;; The constants in the pattern are ignored.

;;; Backquotes which use ,@ or ,. other than at the end of a list
;;; expand into APPENDs or NCONCs and cannot be SETF'd.

;;; This was used for making (setf `(a ,b) foo) return t if
;;; foo matched the pattern (had A as its car).
;;; The other change for reinstalling this
;;; would be to replace the PROGNs with ANDs
;;; in the expansions produced by (LIST SETF), etc.
;;;(DEFUN SETF-MATCH (PATTERN OBJECT)
;;;  (COND ((NULL PATTERN) T)
;;;     ((SYMBOLP PATTERN)
;;;      `(PROGN (SETQ ,PATTERN ,OBJECT) T))
;;;     ((EQ (CAR PATTERN) 'QUOTE)
;;;      `(EQUAL ,PATTERN ,OBJECT))
;;;     ((MEMQ (CAR PATTERN)
;;;            '(CONS LIST LIST*))
;;;      `(SETF ,PATTERN ,OBJECT))
;;;     (T `(PROGN (SETF ,PATTERN ,OBJECT) T))))

;;; This is used for ignoring any constants in the
;;; decomposition pattern, so that (setf `(a ,b) foo)
;;; always sets b and ignores a.
(defun setf-match (pattern object)
  (cond ((eq (car-safe pattern) 'quote)
         nil)
        (t `(setf ,pattern ,object))))

(define-setf-method list (&rest elts)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
            (do ((i 0 (1+ i))
                 (accum)
                 (args elts (cdr args)))
                ((null args)
                 (cons 'progn (nreverse accum)))
              (push (setf-match (car args) `(nth ,i ,storevar)) accum))
            `(incorrect-structure-setf list . ,elts))))

(define-setf-method list* (&rest elts)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
            (do ((i 0 (1+ i))
                 (accum)
                 (args elts (cdr args)))
                ((null args)
                 (cons 'progn (nreverse accum)))
              (cond ((cdr args)
                     (push (setf-match (car args) `(nth ,i ,storevar)) accum))
                    (t (push (setf-match (car args) `(nthcdr ,i ,storevar)) accum))))
            `(incorrect-structure-setf list* . ,elts))))

(define-setf-method cons (car cdr)
  (let ((storevar (gensym)))
    (values nil nil (list storevar)
            `(progn ,(setf-match car `(car ,storevar))
                    ,(setf-match cdr `(cdr ,storevar)))
            `(incorrect-structure-setf cons ,car ,cdr))))

(defmacro incorrect-structure-setf (&rest args)
  (ferror "You cannot ~S the place ~S~% in a way that refers to its old contents."
          'setf args))
