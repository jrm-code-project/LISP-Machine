;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:8; Readtable:CL -*-

;;; this file contains macro definitions for zetalisp special forms
;;; for use with the common-lisp MACRO-FUNCTION function.

;;; Note that many special forms have been punted as they are either
;;; too obscure or kludgey or too implementation-dependant to make
;;; any sense to a common-lisp program in any case.

;;;; list of all fexprs in the source as of 23-Apr-84 09:12:05
;;; ;  perviously defined, or commonlisp special form
;;; *  defined in this file
;;; ~  puntable
;;; ?  ?
;;;------------------------------------------------------------------------------------------
;;; ? catch
;;; ? multiple-value-call
;;; ? the
;;;<L.DEMO>VOTRAX.LISP.1
;;; ~ (defun speak-words (&quote &rest list-of-words)
;;;<L.IO>GRIND.LISP.143
;;; ~ (defun grindef (&quote &rest fcns)
;;;<L.IO1>HACKS.LISP.190
;;; ~ (defun skipa (&quote var form)
;;; ~ (defun skipe (&quote var form)
;;; ~ (defun skipn (&quote var form)
;;;<L.SYS>CADRLP.LISP.148
;;; ~ (defun defmic (&quote name opcode arglist lisp-function-p &optional no-qintcmp
;;;<L.SYS>COLDUT.LISP.91
;;; ~ (defun defmic (&quote name opcode arglist lisp-function-p &optional no-qintcmp)
;;;<L.SYS>COMPAT.LISP.32
;;; ~ (defun include (&quote &rest ignore))
;;;<L.SYS>EVAL.LISP.43
;;; ~ (defun *catch-for-eval (tag &quote &rest body)
;;; ; (defun setq (&quote &rest symbols-and-values)
;;; ~ (defun variable-boundp (&quote variable)
;;; ~ (defun variable-location (&quote variable)
;;; ~ (defun variable-makunbound (&quote variable)
;;; * (defun multiple-value (&quote var-list exp)
;;; * (defun nth-value (value-number &quote exp)
;;; * (defun multiple-value-list (&quote exp)
;;; ; (defun multiple-value-prog1 (&quote value-form &rest forms)
;;; * (defun multiple-value-bind (&quote var-list exp &rest body)
;;; * (defun dont-optimize (&quote &rest body)
;;; ; (defun progn (&quote &rest body)
;;; * (defun comment (&quote &rest ignore)
;;; * (defun with-stack-list (&quote variable-and-elements &rest body)
;;; * (defun with-stack-list* (&quote variable-and-elements &rest body)
;;; * (defun and (&quote &rest expressions)
;;; * (defun or (&quote &rest expressions)
;;; * (defun cond (&quote &rest clauses)
;;; ; (defun let (&quote varlist &rest body)
;;; ; (defun let* (&quote varlist &rest body)
;;; ; (defun flet (&quote function-list &rest body)
;;; ; (defun macrolet (&quote macro-list &rest body)
;;; ; (defun labels (&quote function-list &rest body)
;;; ; (defun progv (vars vals &quote &rest body)
;;;(defun progw (vars-and-vals &quote &rest body)
;;;(defun let-if (cond &quote var-list &quote &rest body)
;;; ; (defun unwind-protect (&quote body-form &rest cleanup-forms)
;;; ; (defun *throw (tag &quote value-expression)
;;; ; (defun block (&quote name &rest body)
;;; ; (defun return-from (&quote blockname &rest vals)
;;; * (defun return (&quote &rest vals)
;;; ; (defun tagbody (&quote &rest body)
;;; ; (defun go (&quote tag &aux tem)
;;; * (defun prog (&quote &rest prog-arguments)
;;; * (defun prog* (&quote &rest prog-arguments)
;;; * (defun do (&quote &rest x)
;;; * (defun do-named (&quote name &rest x)
;;; * (defun do* (&quote &rest x)
;;; * (defun do*-named (&quote name &rest x)
;;; ; (defun function (&quote function)
;;; ; (defun quote (&quote x) x)
;;; ~ (defun functional-alist (&quote x)   ;just like quote interpreted.  however, the compiler
;;;<L.SYS>LTOP.LISP.464
;;;(defun break (&optional &quote format-string &eval &rest args
;;;<L.SYS>QCFILE.LISP.307
;;;(defun patch-source-file (&quote si:patch-source-file-namestring &rest body)
;;;(defun special (&rest &quote symbols)
;;;(defun unspecial (&rest &quote symbols)
;;; ~ (defun defmic (&quote name opcode arglist lisp-function-p &optional (no-qintcmp nil)
;;;<L.SYS>QCOPT.LISP.99
;;; ~ (defun *lexpr (&quote &rest l)
;;; ~ (defun *expr (&quote &rest l)
;;; ~ (defun *fexpr (&quote &rest l)
;;;<L.SYS>QFCTNS.LISP.734
;;; * (defun deff (&quote function-spec &eval definition)
;;; * (defun def (&quote function-spec &rest defining-forms)
;;; * (defun defun (&quote &rest arg)
;;; * (defun macro (&quote function-spec &rest def)
;;; * (defun deff-macro (&quote function &eval definition)
;;;(defun defsubst (&quote symbol &rest def)
;;; ~ (defun signp (&quote test &eval num)
;;; ~ (defun array (&quote x type &eval &rest dimlist)
;;; ; (defun declare (&quote &rest declarations)
;;; ; (defun eval-when (&quote times &rest forms &aux val)
;;;<L.SYS>QMISC.LISP.605
;;; ~ (defun status (&quote status-function &optional (item nil item-p))
;;; ~ (defun sstatus (&quote status-function item
;;; ; (defun compiler-let (&quote bindlist &rest body)
;;; ~ (defun lexpr-funcall-with-mapping-table (function &quote table &eval &rest args)
;;; ~ (defun funcall-with-mapping-table (function &quote table &eval &rest args)
;;;<L.SYS>QRAND.LISP.374
;;;(defun special (&rest &quote symbols)
;;;(defun unspecial (&rest &quote symbols)
;;; * (defun defprop (&quote symbol value property)
;;;(defun defvar-1 (&quote symbol &optional (value ':unbound) documentation)
;;;(defun defconst-1 (&quote symbol &eval value &optional documentation)
;;;<L.SYS2>CLASS.LISP.88
;;; ~ (defun defclass-1 (&quote class-symbol superclass-symbol instance-pattern)
;;; ~ (defun defclass-bootstrap (&quote nm c-s method-tail variables)
;;;<L.SYS2>FLAVOR.LISP.258
;;; ~ (defun with-self-accessible (&quote flavor-name &rest body)
;;;<L.SYS2>LOGIN.LISP.80
;;; ~ (defun login-setq (&quote &rest l)  ;undoing setq
;;;<L.SYS2>LOOP.LISP.795
;;; ~ (defun loop-featurep (&quote f)
;;; ~ (defun loop-nofeaturep (&quote f)
;;; ~ (defun loop-set-feature (&quote f)
;;; ~ (defun loop-set-nofeature (&quote f)
;;;<L.SYS2>QTRACE.LISP.149
;;; ? (defun trace (&quote &rest specs)
;;; ? (defun untrace (&quote &rest fns)
;;;<L.SYS2>SELEV.LISP.21
;;;(defun matchcarcdr (&quote arg car cdr)
;;;<L.SYS2>USYMLD.LISP.186
;;; ~ (defun ua-defmic (&quote name opcode arglist lisp-function-p &optional (no-qintcmp nil))
;;;<L.ZWEI>COMTAB.LISP.307
;;; ~ (defun set-comtab-return-undo (&rest &quote form &aux undo)


;;; ADVISE crap

(defmacro (advise-let alternate-macro-definition) (&rest expressions)
  `(LET ,@expressions))

(defmacro (advise-multiple-value-list alternate-macro-definition) (&rest expressions)
  `(MULTIPLE-VALUE-LIST ,@expressions))

(defmacro (advise-prog alternate-macro-definition) (&rest expressions)
  `(PROG ,@expressions))

(defmacro (advise-progn alternate-macro-definition) (&rest expressions)
  `(PROGN ,@expressions))

(defmacro (advise-return-list alternate-macro-definition) (&rest expressions)
  `(RETURN-LIST ,@expressions))

(defmacro (advise-setq alternate-macro-definition) (&rest expressions)
  `(SETQ ,@expressions))

;;; AND
;;;(and a b c d) => (if a (if b (if c d)))
(defmacro (and alternate-macro-definition) (&rest expressions)
  (case (length expressions)
    (0 t)
    (1 (car expressions))
    (t (do* ((foo (cdr (reverse expressions)) (cdr foo))
             (result `(,(car (last expressions)))))
            ((null foo)
             (car result))
         (setq result `((if ,(car foo) . ,result)))))))

;;; COMMENT
(defmacro (comment alternate-macro-definition) (&rest ignore)
  `(QUOTE IGNORE))

;;; COND
;;;(cond (a b c) (d) (e f)) => (if a (progn b c) (let (d) (if d (if e f)))
;;; This is inadequate, fix it.
(defmacro (cond alternate-macro-definition) (&rest clauses)
  (do ((foo (reverse clauses) (cdr foo))
       (result nil)
       loser)
      ((null foo)
       (if loser `(let (,loser) . ,result) (car result)))
    (if (> (length (car foo)) 1)
        (setq result `((if ,(caar foo) (progn . ,(cdar foo)) . ,result)))
      (progn
        (or loser (setq loser (make-symbol "LOSER" t)))
        (setq result `((if (setq ,loser ,(caar foo)) ,loser . ,result)))))))

;;; DEF
;;; This is a crock for the editor.
(defmacro (def alternate-macro-definition) (function-spec &rest defining-forms)
  `(PROGN ,@defining-forms ',function-spec))

;;; DEFF
(defmacro (deff alternate-macro-definition) (function-spec definition)
  `(PROGN (FSET-CAREFULLY (QUOTE ,function-spec) ,definition)
          (QUOTE ,function-spec)))

;;; DEFF-MACRO
(defmacro (deff-macro alternate-macro-definition) (function definition)
  `(PROGN (FDEFINE ',function ,definition t)
          ',function))

;;; DEFPROP
(defmacro (defprop alternate-macro-definition) (symbol value property)
  `(progn (putprop ',symbol ',value ',property)
          ',symbol))

;;; DEFUN
;;; This is inadequate, fix it.
(defmacro (defun alternate-macro-definition) (name &rest body)
  `(PROGN (FDEFINE ',name (PROCESS-DEFUN-BODY ',name ',body) T)
          ',name))

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

(defun expand-do-macro (let-type setq-type)
  #'(lambda (do-form ignore)
      (separate-do-bindings (second do-form)
        #'(lambda (binding-names initial-values iteration-clauses)
            (let* ((loop-tag (gensym))
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
                      (GO ,loop-tag))))))))))

(deff do-expander  (expand-do-macro 'let  'psetq))
(deff do*-expander (expand-do-macro 'let* 'setq))

(defprop do (macro . do-expander) alternate-macro-definition)
(defprop do* (macro . do*-expander) alternate-macro-definition)

(defmacro (do-named alternate-macro-definition) (name vars test-and-result &body body)
  `(BLOCK ,name
     (DO ,vars ,test-and-result ,@body)))

(defmacro (do*-named alternate-macro-definition) (name vars test-and-result &body body)
  `(BLOCK ,name
     (DO* ,vars ,test-and-result ,@body)))

;;; DONT-OPTIMIZE
(defmacro (dont-optimize alternate-macro-definition) (&body body)
  `(PROGN ,@body))

;;; ENCAPSULATION-LET
(defmacro (encapsulation-let alternate-macro-definition) (&body body)
  `(LET ,@body))

;;; FUNCALL-WITH-MAPPING-TABLE
(defmacro (funcall-with-mapping-table alternate-macro-definition) (function table &rest args)
  (declare (ignore table))
  `(APPLY ,function (LIST ,@(copylist args))))

;;; FUNCALL-WITH-MAPPING-TABLE-INTERNAL
(defmacro (funcall-with-mapping-table-internal alternate-macro-definition) (function table &rest args)
  (declare (ignore table))
  `(APPLY ,function (LIST ,@(copylist args))))

;;; ZL:IF
;;; This needs fixing.
(defmacro (zl:if alternate-macro-definition) (test then &rest elses)
  `(cli:if ,test ,then (progn ,@elses)))

;;; LET-IF (gak)
(defmacro (let-if alternate-macro-definition) (condition binding-list &rest body)
  (let ((thunk (gensym)))
    (labels ((split-bindings (bindings variables values)
               (if (null bindings)
                   `(LET ((,thunk ,@body))
                      (IF ,condition
                          (PROGV ,variables ,values (FUNCALL ,thunk))
                          (FUNCALL ,thunk)))
                   (let ((this-binding (first bindings)))
                     (split-bindings (rest bindings)
                                     (cons (if (listp this-binding)
                                               (first this-binding)
                                               this-binding)
                                           variables)
                                     (cons (if (and (listp this-binding)
                                                    (cdr this-binding))
                                               (second this-binding)
                                               'nil)
                                           values))))))
      (split-bindings binding-list '() '()))))

;;; LEXPR-FUNCALL-WITH-MAPPING-TABLE
(defmacro (lexpr-funcall-with-mapping-table alternate-macro-definition) (function table &rest args)
  (declare (ignore table))
  `(APPLY (FUNCTION LEXPR-FUNCALL) ,function (LIST ,@(copylist args))))

;;; LEXPR-FUNCALL-WITH-MAPPING-TABLE-INTERNAL
(defmacro (lexpr-funcall-with-mapping-table-internal alternate-macro-definition) (function table &rest args)
  (declare (ignore table))
  `(APPLY (FUNCTION LEXPR-FUNCALL) ,function (LIST ,@(copylist args))))

;;; MACRO
(defmacro (macro alternate-macro-definition) (function-spec &rest def)
  (unless (symbolp function-spec)
    (setq function-spec (standardize-function-spec function-spec)))
  `(PROGN (FDEFINE (QUOTE ,function-spec)
                   (CONS (QUOTE MACRO) (QUOTE (LAMBDA ,(car def) ,@(cdr def)))) T)
          (QUOTE ,function-spec)))

;;; MULTIPLE-VALUE
(defmacro (multiple-value alternate-macro-definition) (vars exp)
  `(MULTIPLE-VALUE-SETQ ,vars ,exp))

;;; MULTIPLE-VALUE-BIND
(defmacro (multiple-value-bind alternate-macro-definition) (vars exp &body body)
  `(MULTIPLE-VALUE-CALL
     #'(LAMBDA ,vars ,@body)
     ,exp))

;;; MULTIPLE-VALUE-LIST
(defmacro (multiple-value-list alternate-macro-definition) (exp)
  `(MULTIPLE-VALUE-CALL #'LIST ,exp))

;;; MULTIPLE-VALUE-SETQ
(defmacro (multiple-value-setq alternate-macro-definition) (vars form)
  (labels ((pair-up (vars bindings setqs)
             (if (null vars)
                 `(MULTIPLE-VALUE-CALL
                   #'(LAMBDA ,(cons '&OPTIONAL (reverse bindings))
                       ,@(reverse setqs) ,(first (reverse bindings)))
                   ,form)
                 (let ((temp (gensym)))
                   (pair-up (rest vars)
                            (cons temp bindings)
                            (cons `(SETQ ,(first vars) ,temp) setqs))))))
    (pair-up vars nil nil)))

;;; NTH-VALUE
(defmacro (nth-value alternate-macro-definition) (value-number exp)
  `(NTH ,value-number (MULTIPLE-VALUE-LIST ,exp)))

;;; OR
;;; (or a c b d) => (cond (a) (b) (c) (t d))
;;; This is inadequate, fix it.
(defmacro (or alternate-macro-definition) (&rest expressions)
  (case (length expressions)
    (0 nil)
    (1 (car expressions))
    (t (do ((x expressions (cdr x))
            (result (list 'COND) (cons (list (car x)) result)))
           ((null (cdr x))
            (push (list t (car x)) result)
            (nreverse result))))))

;;; PROG, PROG*
;;; Bletch.
(defun make-prog-macroexpander (prog-type let-type)
  #'(lambda (prog-form macroenvironment)
      (when (not (>= (length prog-form) 2))
        (ferror nil "Too few arguments to ~S in ~S." prog-type prog-form))
      (let ((binding-list (second prog-form))
            (body         (rest (rest prog-form))))
        ;; Macroexpand body to get declarations.
        (let ((macroexpanded-body
                (mapcar #'(lambda (form) (macroexpand form macroenvironment)) body)))
          ;; Scan out declarations.
          (do ((forms macroexpanded-body (rest forms))
               (decls '() (cons (first forms) decls)))
              ((or (not (consp (first forms)))
                   (not (eq (car (first forms)) 'LISP::DECLARE)))
               `(BLOCK NIL
                  (,let-type ,binding-list
                   ,@(reverse decls)
                   (TAGBODY ,@forms)))))))))

(deff prog-macro-expander  (make-prog-macroexpander 'PROG  'LET))
(deff prog*-macro-expander (make-prog-macroexpander 'PROG* 'LET*))

(defprop prog  (macro . prog-macro-expander)  alternate-macro-definition)
(defprop prog* (macro . prog*-macro-expander) alternate-macro-definition)

;;; RETURN
(defmacro (return alternate-macro-definition) (&rest values)
  `(RETURN-FROM NIL ,@values))

;;; RETURN-LIST
;;; Crock, this is an old interpreter function that the
;;; advice system uses randomly.
(defmacro (return-list alternate-macro-definition) (l)
  `(RETURN-FROM NIL (VALUES-LIST ,l)))

;;; WITH-STACK-LIST, WITH-STACK-LIST*
(defmacro (with-stack-list alternate-macro-definition) ((var . elts) &body body)
  `(LET ((,var (LIST ,@elts)))
      ,@body))

(defmacro (with-stack-list* alternate-macro-definition) ((var . elts) &body body)
  `(LET ((,var (LIST* ,@elts)))
      ,@body))

(defmacro (prog1 alternate-macro-definition) (first &body forms)
  (let ((result (gensym)))
    `(let ((,result ,first))
       ,@forms
       ,result)))

(defmacro (prog2 alternate-macro-definition) (first second &body forms)
  `(progn
     ,first
     (prog1
       ,second
       ,@forms)))
