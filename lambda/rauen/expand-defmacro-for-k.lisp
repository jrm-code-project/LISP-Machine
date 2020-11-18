;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;;
;;; EXPAND-DEFMACRO-FOR-K
;;;
;;; This is needed by the compiler to correctly expand DEFMACRO forms for
;;; inclusion in KFASL files.


(defun expand-defmacro-for-k (x env &optional extra-declarations)
  (declare (ignore env)) ;Until macroexpand works
  (let ((name        (first x))
        (lambda-list (second x))
        (body        (cddr x))
        (macro-form  (gentemp 'MACROFORM))
        (macro-env   (gentemp 'MACROENV)))
    (multiple-value-bind (body decls doc-string)
        (GOBBLE-DECLARATIONS body T NIL)  ;make this last argument env when macroexpand works.
      (when doc-string (push `(DOCUMENTATION ,doc-string) decls))
      `(NAMED-LAMBDA ,name (,macro-form &OPTIONAL ,macro-env)
         (DECLARE ,@decls)
         ,macro-env
         (,(if (symbolp name) 'BLOCK 'PROGN) ,(if (symbolp name) name NIL)
          (LET* ,(make-defmacro-let-bindings lambda-list macro-form macro-env)
            (DECLARE ,@decls)
            ,@extra-declarations
            ,@body))))))


(defun make-defmacro-let-bindings (lambda-list macro-form macro-env)
  (let ((bindings NIL))
    (flet ((add-binding (binding)
             (push binding bindings)))
      (make-let-bindings-for-nested-lambda-list lambda-list
                                                `(CDR ,macro-form)
                                                #'add-binding
                                                macro-form
                                                macro-env))
    (reverse bindings)))


(defun make-let-bindings-for-nested-lambda-list (lambda-list path push-proc macro-form macro-env)
  (let ((index 0))
    (flet
      (

       ;; This is mapped onto required parameters.
       (make-binding-or-recurse (var)
         (cond
           ((symbolp var)
            (funcall push-proc `(,var (NTH ,index ,path)))
            (incf index))
           ((listp var)
            (make-let-bindings-for-nested-lambda-list var `(NTH ,index ,path) push-proc macro-form macro-env)
            (incf index))
           (t
            (error "~S not a variable or a list" var))))

       ;; This is mapped onto &OPTIONAL parameters.
       (do-optional-variable (parameter)
         (multiple-value-bind (var initform svar)
             (USER::PARSE-OPTIONAL-PARAMETER parameter)
           (funcall push-proc `(,var  (IF (<= (LENGTH ,path) ,index)
                                          ,initform
                                          (NTH ,index ,path))))
           (when svar
             (funcall push-proc `(,svar (IF (<= (LENGTH ,path) ,index)
                                            NIL
                                            T))))
           (incf index)))

       ;; This is mapped onto &KEY parameters.
       (do-key-parameter (parameter)
         (multiple-value-bind (var initform svar keyword)
             (USER::PARSE-KEY-PARAMETER parameter)
           (funcall push-proc `(,var (GETF (NTHCDR ,index ,path) ,keyword ,initform)))
           (when svar
             (funcall push-proc
                      `(,svar (LABELS ((FOO (LIST)
                                            (COND ((NULL LIST)              NIL)
                                                  ((EQ (CAR LIST) ,keyword) T)
                                                  (T                        (FOO (CDDR LIST))))))
                                (FOO (NTHCDR ,index ,path))))))))

       ;; This is mapped onto &AUX parameters.
       (do-aux-parameter (parameter)
         (multiple-value-bind (var value)
             (USER::PARSE-AUX-PARAMETER parameter)
           (funcall push-proc `(,var ,value))))

       )

      ;; Undot the lambda list if it's dotted
      (when (not (null (cdr (last lambda-list))))
        (setq lambda-list (rplacd (last (copy-list lambda-list))
                                  `(&REST ,(cdr (last lambda-list))))))

      ;; Parse it
      (multiple-value-bind (required optional rest key allow aux body whole environment)
          (user::moby-parse-lambda-list lambda-list)

        ;; Make a dummy binding that checks for ample arguments
;       (let ((min-args (length required))
;             (max-args (if (or rest key aux whole) NIL (+ (length required) (length optional)))))
;         (funcall push-proc `(IGNORE (REQUIRE-AMPLE-MACRO-ARGUMENTS
;                                       ,min-args ,max-args ,path ',lambda-list (CAR ,macro-form)))))

        ;; Make a binding for the &WHOLE variable, if present
        (when whole
          (funcall push-proc `(,whole ,macro-form)))

        ;; Make a binding for the &ENVIRONMENT variable, if present
        (when environment
          (funcall push-proc `(,environment ,macro-env)))

        ;; Make bindings for the required parameters
        (mapcar #'make-binding-or-recurse required)

        ;; Make bindings for the &OPTIONAL parameters
        (mapcar #'do-optional-variable optional)

        ;; Make a binding for the &REST or &BODY parameter, if present
        (when (or rest body)
          (funcall push-proc `(,(if rest rest body) (NTHCDR ,index ,path))))

        ;; Make bindings for the &KEY parameters
        (mapcar #'do-key-parameter key)

        ;; Ignore the &ALLOW-OTHER-KEYS business entirely
        allow

        ;; Make bindings for the &AUX parameters
        (mapcar #'do-aux-parameter aux)))))


(defun require-ample-macro-arguments (min max arglist lambda-list macro-name)
  (unless (listp arglist)
    (error "Attempt to bind lambda list ~S in macro ~S to ~S; a list is required."
            lambda-list macro-name arglist))
  (let ((length (length arglist)))
    (cond
      ((< length min)
       (error "Too few arguments to lambda list ~S in macro ~S; ~S provided, ~S expected."
               lambda-list macro-name length min))
      ((and max (> length max))
       (error "Too many arguments to lambda list ~S in macro ~S; ~S provided, ~S allowed."
               lambda-list macro-name length max))
      (T
       NIL))))


;;; GOBBLE

(defun gobble-declarations (list-of-forms &optional gobble-doc-strings-too env)
  (let ((declarations '())
        (doc-string nil)
        (body nil))
    (loop
      (let ((form (MACROEXPAND (car list-of-forms) env)))
        (cond ((and (listp form) (eq (car form) 'declare))
               (push form declarations))
              ((and (stringp form) gobble-doc-strings-too (not doc-string))
               (setq doc-string form))
              (t
               (setq body list-of-forms)
               (return))))
      (pop list-of-forms))
    (when (and (null body) doc-string) (setq body (list doc-string)))
    (values body (reverse declarations) doc-string)))
