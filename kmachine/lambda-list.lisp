;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-
;;; WKF: changed to USER package from LI 5/5/88 since not loaded on K.
;;;
;;; LAMBDA-LIST.LISP
;;;
;;; Lambda list parsers
;;;
;;; The first set of procedures in this file parses lambda lists into their different
;;; kinds of parameters.
;;;
;;; The second set of procedures parses &OPTIONAL, &KEY, and &AUX parameters into
;;; their parts (var, initform, svar, etc.)


;(export '(
;         parse-aux-parameter
;         parse-key-parameter
;         parse-lambda-list
;         parse-let-binding
;         parse-optional-parameter
;         ))


;; reverting to a version which loads on the lambda #16
;; LAMBDA-LIST does not appear in  *COLD-FILES* *WARM-LOADED-FILES*  *HOT-LOADED-FILES*
;; and therefore is no longer loaded on the K
;; --pfc  5/3/88


;***note!! this tries to execute both on the lambda and on the K.  Unfortunately,
;  this causes extreme lossage.  To win for the lambda, put global before
;  MULTIPLE-VALUE-LIST PAIRLIS RPLACA and VALUES-LIST.  We will normally
;  leave the file without these so as to win for the K.
;MULTIPLE-VALUE-LIST has a definite problem: it is a macro for the k in LISP-INTERNALS
; which is incompatible with the Lambda.  There is probably no real conflict with the others.

(defun parse-lambda-list (lambda-list keyword-list)
  "Parse LAMBDA-LIST, only allowing the keywords given in KEYWORD-LIST.  Use the
bogus keyword :REQUIRED to indicate required arguments.  Return as many values as
there are elements in KEYWORD-LIST, each value being the corresponding part of
LAMBDA-LIST.
   In plainer English: If the KEYWORD-LIST is '(:REQUIRED :OPTIONAL :REST), then
three values will be returned: the required, optional, and rest args in LAMBDA-LIST.
If there are any other kinds of arguments, an error will be raised.
   The order of keywords in KEYWORD-LIST doesn't bother the parser, but duplications
will."
  (let*
    ((keywords   '(:REQUIRED :OPTIONAL :REST :KEY :ALLOW-OTHER-KEYS :AUX :BODY :WHOLE :ENVIRONMENT))
     (parameters (global:multiple-value-list (moby-parse-lambda-list lambda-list)))
     (pairlist   (global:pairlis keywords parameters))
     (results    '()))
    (global:dolist (keyword keyword-list)
      (let ((pair (assoc keyword pairlist)))
        (if pair
            (progn (user:push (cdr pair) results)       ; USER:PUSH (not li:push) because this is only
                                                ; to be compiled and laoded on the lambda  --pfc/wkf
                   (global:rplaca pair 'DONE))
            (ferror "~S is an invalid or duplicated keyword in PARSE-LAMBDA-LIST." keyword))))
    (global:dolist (pair pairlist)
      (unless (or (eq (car pair) 'DONE)
                  (eq (cdr pair) NIL))
        (ferror "~S keyword not permitted in lambda-list ~S." (car pair) lambda-list)))
    (global:values-list (reverse results))))


;;; MOBY PARSER
;;;
;;; This does all the work.  It takes a lambda list and returns nine values.
;;;
;;; (1) List of required parameters
;;; (2) List of optional parameters
;;; (3) Rest parameter, or NIL if not present
;;; (4) List of key parameters
;;; (5) T if &allow-other-keys appears, NIL otherwise
;;; (6) List of aux parameters
;;; (7) Body parameter, or NIL if not present
;;; (8) Whole parameter, or NIL if not present
;;; (9) Environment parameter, or NIL if not present

;;; The &whole parameter must be the first element of the lambda-list.

(defun moby-parse-lambda-list (lambda-list)
  (let (required optional rest key allow-other-keys aux body whole environment)
    (labels
      (

       ;;; We parse the lambda-list sequentially by using this function only.
       (next-parameter ()
         (cond ((null lambda-list) nil)
               ((symbolp lambda-list)
                (raise-lambda-list-error "Dotted tail notation for &REST arg is illegal in Common LISP."))
               (t
                (let ((p (first lambda-list)))
                  (if (null p)
                      (raise-lambda-list-error "~S is not allowed in a lambda list." p)
                      p)))))

       (pop-parameter ()
         (prog1 (next-parameter)
                (setq lambda-list (cdr lambda-list))))

       (more-parameters-p () (not (null lambda-list)))

       (available-parameter-p ()
         (and (more-parameters-p)
              (not (lambda-list-keyword-p (next-parameter)))))

       (set-environment-if-its-there ()
         (when (eq (next-parameter) '&environment)
           (setq environment (pop-keyword-and-one-argument))))

       (pop-keyword-and-one-argument ()
         (let ((parameter (pop-parameter)))
           (if (more-parameters-p)
               (let ((argument (pop-parameter)))
                 (if (lambda-list-keyword-p argument)
                     (raise-lambda-list-error "An argument must follow a ~S in lambda list." parameter)
                     argument))
               (raise-lambda-list-error "No arguments follow ~S in lambda list." parameter))))

       (pop-until-next-keyword ()
         (if (available-parameter-p)
             (cons (pop-parameter) (pop-until-next-keyword))
             '()))

       (test-parameter (what if-true)
         (let ((parameter (next-parameter)))
           (cond ((eq parameter what) (funcall if-true))
                 ((eq parameter '&allow-other-keys)
                  (raise-lambda-list-error "~S in illegal position." parameter))
                 (t nil))))
       )

      (test-parameter '&whole #'(lambda () (setq whole (pop-keyword-and-one-argument))))
      (set-environment-if-its-there)

      (setq required (pop-until-next-keyword))
      (set-environment-if-its-there)

      (test-parameter '&optional
        #'(lambda ()
            (pop-parameter)
            (setq optional (pop-until-next-keyword))))
      (set-environment-if-its-there)

      (let ((p (next-parameter)))
        (when (member p '(&rest &body &allow-other-keys))
          (case p
            (&rest (setq rest (pop-keyword-and-one-argument)))
            (&body (setq body (pop-keyword-and-one-argument)))
            (&allow-other-keys (raise-lambda-list-error "~s in illegal position." p)))
          (when (available-parameter-p)
            (raise-lambda-list-error "Only one argument may follow a ~S in the lambda list." p ))))
      (set-environment-if-its-there)

      (test-parameter '&key
        #'(lambda ()
            (pop-parameter)
            (setq key (pop-until-next-keyword))
            (when (eq (next-parameter) '&allow-other-keys)
              (pop-parameter)
              (setq allow-other-keys T)
              (when (available-parameter-p)
                (raise-lambda-list-error
                  "An argument may not follow a &allow-other-keys in the lambda list.")))))
      (set-environment-if-its-there)

      (test-parameter '&aux
                     #'(lambda ()
                         (pop-parameter)
                         (setq aux (pop-until-next-keyword))))
      (set-environment-if-its-there)

      (if (more-parameters-p)
          (raise-lambda-list-error "I've lost.  I didn't expect to see a ~s" (next-parameter))
          (values required optional rest key allow-other-keys aux body whole environment)))))

(defun lambda-list-keyword-p (sym)
  (member sym '(&optional &rest &key &allow-other-keys &aux &body &whole &environment)))

(defun raise-lambda-list-error (message &rest args)
  (error "~?" message args))


;;; PARAMETER PARSERS
;;;
;;; These procedures parse &OPTIONAL, &KEY, and &AUX parameters within a lambda list.
;;; Each takes two arguments: the parameter, and an optional error procedure to call if the
;;; parameter does not have a valid syntax.
;;;
;;; PARSE-OPTIONAL-PARAMETER returns three values: the variable, the initform, and the svar
;;; in the optional parameter.
;;;
;;; PARSE-KEY-PARAMETER returns four values: the variable, the initform, the svar, and the
;;; keyword in the key parameter.
;;;
;;; PARSE-AUX-PARAMETER returns two values: the variable and the value in the aux parameter.
;;;
;;; All values not supplied in the parameter default to NIL, except for the keyword in a
;;; key parameter, which defaults to the parameter's variable interned in the keyword
;;; package.
;;;
;;; A fourth procedure, PARSE-LET-BINDING, parses a let binding.  It returns two values:
;;; the variable and the value.  It is almost equivalent to PARSE-AUX-PARAMETER.


(defun parse-optional-parameter (optional-parameter &optional error-proc)
  "Return three values: VARIABLE, INITFORM, and SVAR"
  (let*
    ((err-proc (if error-proc
                   error-proc
                   #'(lambda ()
                       (ferror "~S is not valid syntax for an optional parameter."
                               optional-parameter))))
     (parsed-expr (parse-expr optional-parameter
                              '(:var
                                (:var)
                                (:var :initform)
                                (:var :initform :svar))
                              err-proc
                              '(:initform))))
    (values (getf parsed-expr :var)
            (getf parsed-expr :initform)
            (getf parsed-expr :svar))))


(defun parse-key-parameter (key-parameter &optional error-proc)
  "Return four values: VARIABLE, INITFORM, SVAR, and KEYWORD"
  (let*
    ((err-proc (if error-proc
                   error-proc
                   #'(lambda ()
                       (ferror "~S is not valid syntax for a key parameter."
                               key-parameter))))
     (parsed-expr (parse-expr key-parameter
                              '(:var
                                (:var)
                                (:var :initform)
                                (:var :initform :svar)
                                ((:keyword :var))
                                ((:keyword :var) :initform)
                                ((:keyword :var) :initform :svar))
                              err-proc
                              '(:initform)))
     (var      (getf parsed-expr :var))
     (initform (getf parsed-expr :initform))
     (svar     (getf parsed-expr :svar))
     (keyword  (getf parsed-expr :keyword)))
    (values var
            initform
            svar
            (if keyword
                keyword
                (intern (symbol-name var) (global:find-package 'keyword))))))


(defun parse-aux-parameter (aux-parameter &optional error-proc)
  "Return two values: VAR and VALUE"
  (let*
    ((err-proc (if error-proc
                   error-proc
                   #'(lambda ()
                       (ferror "~S is not valid syntax for an &AUX parameter."
                               aux-parameter))))
     (parsed-expr (parse-expr aux-parameter
                              '(:var
                                (:var)
                                (:var :value))
                              err-proc
                              '(:value))))
    (values (getf parsed-expr :var)
            (getf parsed-expr :value))))


(defun parse-let-binding (let-binding &optional error-proc)
  "Return two values: VAR and VALUE"
  (let*
    ((err-proc (if error-proc
                   error-proc
                   #'(lambda ()
                       (ferror "~S is not valid syntax for a let binding."
                               let-binding))))
     (parsed-expr (parse-expr let-binding
                              '(:var
                                (:var)
                                (:var :value))
                              err-proc
                              '(:value))))
    (values (getf parsed-expr :var)
            (getf parsed-expr :value))))


;;; Pattern matcher.
;;;
;;; EXPR is the expression to parse,
;;; PATTERN-LIST is a list of allowed patterns
;;; ERROR-PROC is a procedure to call if none of the patterns match
;;; MATCH-ANYTHING-LIST is a list of atoms in pattern-list that match anything
;;;
;;; Returns a list of the form (pattern-atom-1 matched-object-1 pattern-atom-2 ...)
;;; using the first pattern on PATTERN-LIST that matches EXPR.
;;;
;;; Examples:
;;;   EXPR = ((a b) c)
;;;   pattern = ((:foo :bar) :baz)
;;;   result = (:foo a :bar b :baz c)
;;;
;;;   EXPR = ((a b) c)
;;;   pattern = (:foo :bar)
;;;   result = (:foo (a b) :bar c) if MATCH-ANYTHING-LIST contains :foo
;;;            call to ERROR-PROC otherwise

(defun parse-expr (expr pattern-list error-proc match-anything-list)
  (when (null pattern-list)
    (funcall error-proc))
  (let ((trial-match (pattern-match (car pattern-list) expr match-anything-list)))
    (if trial-match
        trial-match
        (parse-expr expr (cdr pattern-list) error-proc match-anything-list))))

(defun pattern-match (pattern expr match-anything-list)
  (cond
    ((and (symbolp pattern) (member pattern match-anything-list))
     (list pattern expr))
    ((and (null pattern) (null expr))
     'nil-match)
    ((or (null pattern) (null expr))
     nil)
    ((and (symbolp pattern) (symbolp expr))
     (list pattern expr))
    ((and (consp pattern) (consp expr))
     (let ((car-match (pattern-match (car pattern) (car expr) match-anything-list))
           (cdr-match (pattern-match (cdr pattern) (cdr expr) match-anything-list)))
       (if (and car-match cdr-match)
           (if (eq cdr-match 'nil-match)
               car-match
               (append car-match cdr-match))
           nil)))
    (t
     nil)))
