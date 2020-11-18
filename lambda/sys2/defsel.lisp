;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:T; Base:8; Readtable:ZL -*-
;       ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; This package provides for the definition and use of SELECTQ type
;;; objects that use the DTP-SELECT-METHOD microcode feature to allow
;;; destructuring of args on a per-operation basis

#| (DEFSELECT <function-name or (<function-spec> <function-to-be-called-if-no-match>)>
  (<keyword or (<keyword> <keyword> ... <keyword>)>
   . <function or (<arglist> . <body>)>)

 (DEFSELECT FILE-CHAOSNET-COMMAND
   (:FOO (BAZ &REST BAR)
         (DO-SOME-WORK))
   (:SPAZZ (&OPTIONAL (BAR 1))
           (SPZZA)))
 |#

;;; The :SELECT-METHOD function spec.
(defun (:property :select-method function-spec-handler)
       (function function-spec &optional arg1 arg2)
  (let ((select-method-function-spec (second function-spec))
        (message (third function-spec))
        select-method-alist elem fn new-p)
    (if (not (and (= (length function-spec) 3)
                  (validate-function-spec select-method-function-spec)))
        (if (eq function 'validate-function-spec)
            nil
          (ferror 'sys:invalid-function-spec
                  "The function spec ~S is invalid." function-spec))
      (selectq function
        (validate-function-spec t)
        (function-parent (values (cadr function-spec) 'defun))
        (t (unless (or (not (memq function '(fdefine fdefinition fdefinition-location
                                                     fundefine fdefinedp)))
                       (and (fdefinedp select-method-function-spec)
                            (typep (setq fn (fdefinition select-method-function-spec))
                                   'select-method)))
             (ferror 'sys:invalid-function-spec
                     "The function spec ~S is invalid;~%~S is not a DEFSELECT."
                     function-spec select-method-function-spec))
           (if fn (setq elem (assq-careful message
                                   (setq select-method-alist (%make-pointer dtp-list fn)))))
           (when (and (null elem) (memq function '(fdefine fdefinition-location)))
             ;; cons up a select-method
             (setq elem (cons message nil) new-p t)
             (fdefine select-method-function-spec
                      (%make-pointer dtp-select-method (cons elem select-method-alist)))
             (let ((closure (cdr (assq-careful ':which-operations select-method-alist))))
               (when (closurep closure)
                 (pushnew message (symeval-in-closure closure '.defselect.which.operations.))
                                  ':test 'eq)))
           (selectq function
             (fdefine (setf (cdr elem) arg1))
             (fdefinition (cdr elem))
             (fdefinition-location elem)
             (fdefinedp (cdr elem))
             (fundefine
              (fdefine select-method-function-spec
                       (%make-pointer dtp-select-method (remq elem select-method-alist)))
              (let* ((closure (cdr (assq-careful ':which-operations select-method-alist)))
                     loc)
                (when (closurep closure)
                  (setq loc (locate-in-closure closure '.defselect.which.operations.))
                  (setf (contents loc) (remq message (contents loc))))))
             (t (function-spec-default-handler function function-spec arg1 arg2))))))))

(defmacro defselect (fspec &body methods &aux no-which-operations tail-pointer methods-list)
  "Define a function named FSPEC which dispatches on its first argument to find a method.
Each element of METHODS is a method for one or several possible first arguments.
Each method's car is a keyword, or a list of keywords.
Its cdr is a lambda list for that method.  The rest of the method is a body.
When the function is called, the first argument should be a keyword.
The first method which matches that keyword is run.  Its lambda-list is bound
to the remaining arguments (the keyword is not included).  Its body is run
and the value is the value of the function FSPEC itself.
FSPEC is either a symbol, or a list of a function spec
 and another function to be called if no method matches the keyword.
 This other function must be a symbol; #'(lambda ...) and even #'foo will not work.
/(DEFSELECT <function-name or (<function-spec> <function-to-be-called-if-no-match>)>
/  (<keyword or (<keyword> <keyword> ... <keyword>)>
/   . <function or (<arglist> . <body>)>)"
  ;; Decode FSPEC
  (and (consp fspec)
       (setq tail-pointer (cadr fspec)
             no-which-operations (caddr fspec)
             fspec (car fspec)))
  ;; Turn (FOO BAR) into (:PROPERTY FOO BAR)
  (setq fspec (standardize-function-spec fspec))
  (setq methods-list
        (loop for method in methods
              when (consp (car method)) append (car method)
              else collect (car method)))
  (or no-which-operations
      (setq methods-list `(,@methods-list
                           :which-operations
                           :operation-handled-p
                           :send-if-handles
                           :get-handler-for)))
  `(def ,fspec
     (defselect-internal ',fspec ',tail-pointer ',methods-list ,(not no-which-operations))
     ,@(loop for method in methods
             when (consp (car method))
             append `(,(select-method-definition fspec (caar method) (cdr method))
                      . ,(loop for m in (cdar method)
                               collect `(deff (:select-method ,fspec ,m)
                                              #'(:select-method ,fspec ,(caar method)))))
             else collect (select-method-definition fspec (car method) (cdr method)))
     ',fspec))

(defun select-method-definition (fspec method definition)
  (if (atom definition)
      `(deff (:select-method ,fspec ,method) ',definition)
    `(defun (:select-method ,fspec ,method) (ignore . ,(car definition))
       . ,(cdr definition))))

; This function ALWAYS returns nil, since **defselect-op** is not ever used anywhere else.
; What was this used for? mly
;(defun select-needed-op (body)
;  (IF (atom body)
;      (if (eq body '**DEFSELECT-OP**) '**DEFSELECT-OP**  'IGNORE)
;    (LOOP FOR sub-body IN body
;         AS result = (select-needed-op sub-body)
;         UNTIL (eq result '**DEFSELECT-OP**)
;         FINALLY (return result))))


(defun defselect-internal (fspec tail-pointer method-list auto-which-operations
                           &aux tem old-alist new-alist)
  (and (fdefinedp fspec)
       (typep (setq tem (fdefinition fspec)) 'select-method)
       (setq old-alist (%make-pointer dtp-list tem)))
  ;; Go through extra pains to make the select method cdr-coded.
  (setq tem (length method-list))
  (setq new-alist (make-list (if tail-pointer (1+ tem) tem)))
  (when tail-pointer
    (setq tem (last new-alist))
    (rplaca tem tail-pointer)
    (without-interrupts
      (%p-dpb-offset cdr-error %%q-cdr-code tem 0)
      (%p-dpb-offset cdr-normal %%q-cdr-code tem -1)))
  (do ((method method-list (cdr method))
       (sublist new-alist (cdr sublist)))
      ((null method))
    (setf (car sublist) (cons (car method) 'select-method-undefined-message))
    (if (setq tem (assq-careful (car method) old-alist))
        (setf (cdr (car sublist)) (cdr tem))))
  (setq tem (%make-pointer dtp-select-method new-alist))
  (fdefine fspec tem t)
  (when auto-which-operations
    (setq tem (let-closed ((.defselect.which.operations. nil)
                           (.defselect.self. tem))
                'defselect-which-operations))
    (fdefine `(:select-method ,fspec :operation-handled-p) tem t)
    (fdefine `(:select-method ,fspec :send-if-handles) tem t)
    (fdefine `(:select-method ,fspec :get-handler-for) tem t)
    (fdefine `(:select-method ,fspec :which-operations) tem t))
  t)

(defun defselect-which-operations (op &rest rest &aux nsi)
  (declare (special .defselect.which.operations. .defselect.self.))
  ;; gak.
  ;; if this select-method is used as a NAMED-STRUCTURE-INVOKation,
  ;; then the first arg passed is "self"
  ;; more vile kludgery used below for :send-if-handles
  (unless (symbolp (car rest))                  ;let's fail to win...
    (setq nsi (pop rest)))
  (or .defselect.which.operations.
      (setq .defselect.which.operations.
            (defselect-make-which-operations .defselect.self.)))
  (selectq op
    (:which-operations .defselect.which.operations.)
    (:operation-handled-p (memq (car rest) .defselect.which.operations.))
    (:send-if-handles (and (memq (car rest) .defselect.which.operations.)
                           (if nsi (apply .defselect.self. (car rest) nsi (cdr rest))
                             (apply .defselect.self. rest))))
    (:get-handler-for (cdr (assq-careful (car rest)
                                         (%make-pointer dtp-list .defselect.self.))))))

(defun defselect-make-which-operations (fctn &aux ops subr)
  ;; Ignore tracing, decode full hair, (:property foo bar), etc
  (or (typep fctn 'select-method)
      (setq fctn (fdefinition (unencapsulate-function-spec fctn))))
  (do ()
      ((or (null fctn)
           (and (symbolp fctn)
                (not (fboundp fctn))))
       ;; This cdr-codes the list, and conses it safely away from temporary areas.
       (copy-list (nreverse ops) permanent-storage-area))
    (typecase fctn
      (symbol
       (setq fctn (fsymeval fctn)))
      (cons
       (cond ((symbolp (car fctn))
              (cond (subr (setq fctn subr)      ;Already one deep, return
                          (setq subr nil))
                    (t (setq subr (cdr fctn)    ;explore subroutine
                             fctn (car fctn)))))
;            ((MEMQ (CAAR FCTN)                 ;Don't add these
;                   '(:WHICH-OPERATIONS :OPERATION-HANDLED-P
;                     :SEND-IF-HANDLES :GET-HANDLER-FOR))
;             (SETQ FCTN (CDR FCTN)))
             (t (setq ops (cons (caar fctn) ops))
                (setq fctn (cdr fctn)))))
      ((and array (satisfies hash-array-funcallable-p))
       (setq fctn
             (let ((alist nil))
               (maphash #'(lambda (op meth-locative &rest ignore)
                            (push (cons op (car meth-locative)) alist))
                        fctn)
               alist)))
      (select-method
       (setq fctn (%make-pointer dtp-list fctn)))
      ((or closure entity)
       (setq fctn (car (%make-pointer dtp-list fctn))))
      (instance
       (setq fctn (flavor-method-hash-array (get (type-of fctn) 'flavor))))
      (t (setq fctn nil)))))

;(defun select-method-undefined-message (message &rest arguments)
;  (declare (dbg:error-reporter))
;  (error 'unclaimed-message
;        :object "some select-method"
;         :message message :arguments arguments))

(defmacro defselect-incremental (function-spec &optional default)
  "Define a select-method function to exist; let its methods be defined separately.
Defines FUNCTION-SPEC as a select-method function, but does not create
any methods for it (except for :WHICH-OPERATIONS, etc).

You define the methods with individual DEFUNs, such as
/(DEFUN (:SELECT-METHOD FUNCTION-SPEC OPERATION) (IGNORE ARG1 ARG2) ...)
Note that the lambda list must include a variable, possibly ignored,
to receive the operation name itself, since that is the first argument in the call.

DEFAULT is a symbol which is a function to be called to handle
operations that there are no methods for.

Both arguments are unevaluated."
  `(defselect-incremental-internal ',function-spec ',default))

(defun defselect-incremental-internal (fspec tail-pointer &aux tem fn self)
  (if (and (fdefinedp fspec)
           (typep (setq tem (fdefinition fspec)) 'select-method))
      (setf (cdr (last (setq fn (%make-pointer dtp-list tem))))
            tail-pointer)
    (setq tem (let-closed ((.defselect.which.operations. nil)
                           (.defselect.self. tem))
                'defselect-which-operations))
    (setq self (%make-pointer dtp-select-method
                              `((:which-operations . ,tem)
                                (:operation-handled-p . ,tem)
                                (:send-if-handles . ,tem)
                                (:get-handler-for . ,tem)
                                . ,tail-pointer)))
    (set-in-closure tem '.defselect.self. self)
    (fdefine fspec self t))
  fspec)

;;; Differs from DEFSELECT-WHICH-OPERATIONS in not making a permanent list
;;; of the operations (since more could be added).
; no longer used, now that updating of which-operations is designed to win
;(DEFUN DEFSELECT-WHICH-OPERATIONS-DONT-CACHE (OP &REST REST)
;  (DECLARE (SPECIAL .DEFSELECT-LOCATION.))
;  (SELECTQ OP
;    (:WHICH-OPERATIONS (DEFSELECT-MAKE-WHICH-OPERATIONS (CONTENTS .DEFSELECT-LOCATION.)))
;    (:OPERATION-HANDLED-P (ASSQ-CAREFUL (CAR REST)
;                                       (%MAKE-POINTER DTP-LIST
;                                                      (CONTENTS .DEFSELECT-LOCATION.))))
;    (:SEND-IF-HANDLES (AND (ASSQ-CAREFUL (CAR REST)
;                                        (%MAKE-POINTER DTP-LIST
;                                                       (CONTENTS .DEFSELECT-LOCATION.)))
;                          (APPLY (CONTENTS .DEFSELECT-LOCATION.) REST)))))
