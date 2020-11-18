;;; -*- Mode:LISP; Base:10; Readtable:CL; Package:HUNLA -*-
;;; Copyright (c) 1988 by GigaMos Systems, Inc.
;;;; New WHO-CALLS (replaces versions in ANALYZE)

;;; (WHO-CALLS-INTERNAL SYMBOL)
;;;  Returns a (new) alist of callers and keywords indicating how they are called,
;;;  including :FUNCTION, :VARIABLE, :CONSTANT, and :MACRO-EXPANDED.
;;;
;;; (WHO-CALLS SYMBOL) prints it out nicely.
;;; (WHO-CALLS-1 CALLERS PRINT-FUN &OPTIONAL STREAM) is a subroutine.
;;; (PRINT-WHO-CALLS-ITEM (CALLER-INFO &OPTIONAL (STREAM *STANDARD-OUTPUT*)) is a printer.
;;;
;;; (UPDATE-WHO-CALLS REMAKE-FROM-WORLD?)
;;;  Sets up the database, optionally rebuilding it from scratch.
;;;  Otherwise, processes outstanding update requests.
;;;  Only one process at a time should call UPDATE-WHO-CALLS, which actually
;;;  updates the database.  The locking only allows people to access the
;;;  database during updates; it does not support multiple updaters.
;;;  Requests are PUSHed onto the *WHO-CALLS-QUEUE*.
;;;  These are lists beginning with an keyword specifying the operation to be performed.
;;;  The rest of the request's elements are arguments for the operation.
;;;
;;;
;;; +++TO DO:
;;;     More analyzers
;;;     COMPILER interface
;;;     ZWEI interface
;;;     Documented interface (change documentation, probably)
;;; IWBNI the compiler, when expanding macros, could somehow figure out which ones
;;; were really in the source code, and mark those specially on the debugging info.
;;; As it is now, who-calls a macro generates massive spurious results.


(defvar *who-calls-area* (zl:make-area :name 'who-calls-area
                                       :gc :dynamic
                                       :room t)
  "Area for WHO-CALLS database.")

(defparameter *who-calls-database-size* 20000.)

(defvar *who-calls-database* nil
  "The WHO-CALLS database.")

(defvar *who-calls-lock* nil
  "Keeps *WHO-CALLS-DATABASE* consistent during read-modify-write.")

(defvar *who-calls-queue* nil)

(defun who-calls-internal (symbol)
  (check-type symbol symbol)
  (unless *who-calls-database*
    (error "The WHO-CALLS database has not been initialized."))
  (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
    (copy-list (gethash symbol *who-calls-database*))))

(defun who-calls (symbol)
  (who-calls-check-for-magic symbol)
  (let ((callers (who-calls-internal symbol)))
    (cond (callers
           (printing-package-names
             (format t "~&There ~:[is~;are~] ~D caller~P of ~S:"
                     (> (length callers) 1) (length callers) (length callers) symbol)
             (who-calls-1 callers
                          #'(lambda (caller-info tab-column stream)
                              (format stream "~&  ")
                              (print-who-calls-item caller-info tab-column stream))
               *standard-output*)))
          (t
           (format t "~&No callers of ~S found." symbol))))
  (terpri)
  (values))

(defun who-calls-check-for-magic (symbol &optional (stream *standard-output*))
  (when (fboundp symbol)
    (cond ((and (consp (symbol-function symbol))
                (eq (car (symbol-function symbol)) 'si:macro))
           (printing-package-names
             (format stream "~&~S is a macro; there may be other callers of it."
                     symbol)))
          ((or (typep (symbol-function symbol) 'si:microcode-function)
               (assoc 'compiler::placeholder-to-micro-function-table
                       (si:debugging-info (symbol-function symbol))))
           (let ((*package* (find-package "LISP")))
             (format stream "~&~S is a microcoded function; there may be other callers of it."
                     symbol))))))

(defvar *who-calls-does-width-pretty* t)
(defparameter *who-calls-max-pretty-width* 90)

(defun who-calls-1 (callers print-fun &optional stream)
  (declare (zwei:indentation 2 1))
  (let ((sym-width 0)
        (mth-width 0))
    (printing-package-names
      (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
        (sort callers #'string<
              :key #'(lambda (x) (if (atom (car x)) (car x) #\Null)))
        (when *who-calls-does-width-pretty*
          (dolist (caller callers)
            (if (atom (car caller))
                (setq sym-width (min *who-calls-max-pretty-width*
                                     (max sym-width (zl:flatsize (car caller)))))
              (setq mth-width (min *who-calls-max-pretty-width*
                                   (max mth-width (zl:flatsize (car caller)))))))))
      (incf sym-width 3)
      (incf mth-width 3)
      (dolist (caller-info callers)
        (funcall print-fun
                 caller-info
                 (if (atom (car caller-info)) sym-width mth-width)
                 stream)))))

;;; PRINT-WHO-CALLS-ITEM is a printer.

(defparameter *who-calls-descriptions*
              '((:function "function")
                (:macro-expanded "macro expanded")
                (:flavor "flavor definition")
                (:value "variable")
                (:instance-variable "instance variable")
                (:constant "constant")))

(defun print-who-calls-item (caller-info &optional
                             (tab-column 1) (stream *standard-output*))
  (let ((caller-name (car caller-info)))
    (printing-package-names
      (format stream "~S~VT(" caller-name tab-column)
;     (format stream "~S" caller-name)
;     (if (and *who-calls-does-width-pretty*
;              (>= tab-column *who-calls-max-pretty-width*))
;         (format stream "~%   ")
;       (format stream "~VT" tab-column))
;      (write-string "(" stream)
      (let ((how (cdr caller-info))
            (prev-flag nil))
        ;; Print out the HOWs we know in a canonical order.
        (dolist (which-one *who-calls-descriptions*)
          (when (member (car which-one) how)
            (format stream "~:[~;, ~]~A" prev-flag (cadr which-one))
            (setq prev-flag t)))
        ;; Then do the ones we never heard of.
        (dolist (x how)
          (when (not (assoc x *who-calls-descriptions*))
            (format stream "~:[~;, ~]~S" prev-flag x)
            (setq prev-flag t))))
      (write-string ")" stream))
    caller-name))


;;;; Update functions

(defun update-who-calls (&key remake include-properties)
  (when (null *who-calls-database*)
    (si:without-interrupts
      (setq *who-calls-lock* nil
            *who-calls-database* (make-hash-table
                                   :test #'equal
                                   :size (si:pkg-good-size *who-calls-database-size*)
                                   :area *who-calls-area*))))
  (cond (remake
         (setq *who-calls-lock* nil)
         (when (eq remake t)
           (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
             (clrhash *who-calls-database*)
             (setq *who-calls-queue* nil)))
        (si:report-elapsed-time t 0 "building the who-calls database"
          #'analyze-packages
          (if (eq remake t) (list-all-packages) remake)
          include-properties)
        (format t "~2&~D entries in the database.~%" (hash-table-count *who-calls-database*))
        (room '*who-calls-area*)
        (si:report-elapsed-time t 0 "cdr-coding the who-calls database"
          #'(lambda ()
              (maphash #'(lambda (key value)
                           (let ((si:default-cons-area *who-calls-area*))
                             (setf (gethash key *who-calls-database*) (copy-tree value))))
                       *who-calls-database*)))
        (gc:flip :areas (list hl:*who-calls-area*) :reclaim-mode :batch :volatility 0))
  (*who-calls-queue*
    (let (requests)
      (si:without-interrupts                    ;Gobble down the list of requests.
        (setq requests (reverse *who-calls-queue*)
              *who-calls-queue* nil))
      (dolist (request requests)
        (case (first request)
          (:record-function
           (analyze-function-definition (second request) (third request) #'record-caller))
          (:erase-function
           (analyze-function-definition (second request) (third request) #'erase-caller))))))))

(defun analyze-packages (packages include-properties)
  (format t "~&Analyzing packages: ")
  (dolist (pkg packages)
    (format t "~A " pkg)
    (do-symbols (s pkg)
      (analyze-symbol s #'record-caller-consing
                      :include-properties include-properties))))


;;; The hash table is keyed by callee; each entry is an alist of callers
;;; telling how they are called.  Entires are fully cdr-coded, and thrown
;;; away upon update.

#+IGNORE
(defun record-caller-debug (caller callee how)
  (printing-package-names
    (format t "~&~S calls ~S as a ~(~A~)" caller callee how)))

(defun record-caller (caller callee how)
  (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
    (let ((si:default-cons-area *who-calls-area*)
          (alist (gethash callee *who-calls-database*)))
      (if (null alist)
          (setf (gethash callee *who-calls-database*)
                (list (list caller how)))
        (let ((known-caller (assoc caller alist :test #'equal)))
          (setf (gethash callee *who-calls-database*)
                (if known-caller
                    (when (not (member how (cdr known-caller) :test #'equal))
                      (append (delete known-caller alist)
                              (list (append (list caller) (cdr known-caller) (list how)))))
                  (append alist (list (list caller how))))))))))

(defun record-caller-consing (caller callee how)
  (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
    (let ((si:default-cons-area *who-calls-area*)
          (alist (gethash callee *who-calls-database*)))
      (if (null alist)
          (setf (gethash callee *who-calls-database*)
                (acons caller (list how) nil))
        (let ((known-caller (assoc caller alist :test #'equal)))
          (if known-caller
              (when (not (member how (cdr known-caller) :test #'equal))
                (rplacd known-caller (nconc (cdr known-caller) (list how))))
            (rplacd alist (acons caller (list how) (cdr alist)))))))))

(defun erase-caller (caller callee &rest ignore)
  (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
    (let ((si:default-cons-area *who-calls-area*)
          (alist (gethash callee *who-calls-database*)))
      (when alist
        (let ((known-caller (assoc caller alist)))
          (when known-caller
            (let ((new-entry (delete known-caller alist)))
              (if (eq new-entry (list nil))
                  (remhash callee *who-calls-database*)
                (setf (gethash callee *who-calls-database*) new-entry)))))))))


;;;; Analysis routines (culled from the versions in ANALYZE)

(defvar *who-calls-losers*)

(defun find-who-calls-losers (&key (size 500) pkg)
  (setq *who-calls-losers* nil)
  (si:with-lock (*who-calls-lock* :whostate "Who Calls Lock")
    (fresh-line)
    (printing-package-names
      (maphash #'(lambda (key val)
                   (when (and (symbolp key)
                              (> (length val) size)
                              (or (null pkg)
                                  (eq (symbol-package key) pkg)))
                     (format t "~&~S" key)
                     (pushnew key *who-calls-losers*)))
               *who-calls-database*)))
  (format t "~&There are ~D entries in the database; ~D of them might be losers"
          (hash-table-count *who-calls-database*)
          (length *who-calls-losers*)))


(defparameter
  *who-calls-ignores-these-properties*
  '(:PREVIOUS-DEFINITION
    COMPILER::P1
    COMPILER::P2))

(defparameter                                   ;Avoid symbols which are either spurious or too common.
  *who-calls-ignores-these-symbols*
  '(GLOBAL:T
    GLOBAL:NIL
    GLOBAL:QUOTE
    COMPILER:WARN
    :CONDITION-NAMES
    :FORMAT-ARGS
    :FORMAT-STRING
    :IMPOSSIBLE
    :PROPERTY-LIST
    GLOBAL:INHIBIT-SCHEDULING-FLAG
    GLOBAL:INHIBIT-STYLE-WARNINGS
    GLOBAL:SELF
    GLOBAL:*STANDARD-OUTPUT*
    GLOBAL: GLOBAL:= GLOBAL:*
    GLOBAL:AREF LISP:AREF
    GLOBAL:BLOCK
    GLOBAL:CAR
    GLOBAL:CDR
    GLOBAL:AND
    GLOBAL:CERROR
    GLOBAL:CONSP
    GLOBAL:COPY-LIST
    GLOBAL:DECLARE
    GLOBAL:EVAL
    GLOBAL:FIRST GLOBAL:SECOND GLOBAL:THIRD
    GLOBAL:FORMAT
    GLOBAL::FUNCTION
    GLOBAL:GENSYM
    GLOBAL:GO
    GLOBAL:CAAR-SAFE
    GLOBAL:CONTENTS
    GLOBAL:ERROR
    GLOBAL:FERROR
    GLOBAL:NEQ
    GLOBAL:LET
    GLOBAL:LAMBDA
    GLOBAL:LISTP LISP:LISTP
    GLOBAL:PATHNAME
    GLOBAL:PRINC
    GLOBAL:PROGN LAMBDA::PROGEN
    GLOBAL:PUTPROP
    GLOBAL:MAPC
    GLOBAL:MAPCAR
    GLOBAL:NOT
    GLOBAL:NREVERSE
    GLOBAL:RETURN
    GLOBAL:SEND
    GLOBAL:SETQ
    GLOBAL:STRING-EQUAL
    GLOBAL:STRING-LENGTH
    GLOBAL:STRING-APPEND
    GLOBAL:SYMEVAL-IN-INSTANCE
    GLOBAL:TYPEP
    EH::CHECK-TYPE-INTERNAL
    EH:CONDITION-HANDLERS
    SI::*INTERPRETER-FRAME-ENVIRONMENT*
    SI::*INTERPRETER-FUNCTION-ENVIRONMENT*
    SI::*INTERPRETER-VARIABLE-ENVIRONMENT*
    SI::COMPILE-TIME-REMEMBER-MAPPING-TABLE
    SI::INFIX-PARSE-LIS
    SI::MACRO-REPORT-ARGS-ERROR
    SI::NON-COMPLEX-NUMBER-IN-RANGE-P
    SI::OPTIMIZE-NUMERIC-TYPE-TEST
    SI::SIMPLE-MAKE-ARRAY
    SYSTEM:EVAL1
    SYSTEM:SELF-MAPPING-TABLE
    SYSTEM:STORE-KEYWORD-ARG-VALUES))

(defparameter
  *who-calls-ignores-these-macros*
  `(
    GLOBAL:CASE
    GLOBAL:DECF
    GLOBAL:DOLIST
    GLOBAL:DOTIMES
    GLOBAL:FUNCALL-SELF
    GLOBAL:LOCF
    GLOBAL:ONCE-ONLY
    GLOBAL:WITH-LIST
    GLOBAL:WITH-LIST*
    GLOBAL:IF LISP:IF
    GLOBAL:INCF
    GLOBAL:LEXPR-SEND
    GLOBAL:LOOP
    GLOBAL:POP
    GLOBAL:PSETQ
    GLOBAL:PUSH
    GLOBAL:PUSHNEW
    GLOBAL:SELECTQ
    GLOBAL:SEND
    GLOBAL:SETF
    GLOBAL:TYPECASE
    GLOBAL:UNLESS
    GLOBAL:WHEN
    SI::DO-BODY
    SI::EVAL-BODY
    SI::GOBBLE-DECLARATIONS-FROM-BODY
    SI::GOBBLE-DECLARATIONS-INTERNAL
    SI::LOOP-LIST-COLLECTOR
    SI::MACROCALL
    SI::METHOD-MAPPING-TABLE
    SI::XR-BQ-LIST
    SI::XR-BQ-LIST*
    SI::LOOP-COLLECT-RPLACD))



;;; ANALYZE-SYMBOL is the toplevel analyzer.
;;;
;;; +++ More things to maybe look at:
;;;     Files; MISC instructions; compiled ADVICE

(defun analyze-symbol (symbol fun &key include-properties)
  (declare (sys:downward-funarg fun))
  (unless (member symbol *who-calls-ignores-these-symbols*)
    (when (and (fboundp symbol)
               (eq (si:locf (symbol-function symbol))
                   (si:follow-cell-forwarding (si:locf (symbol-function symbol)) t)))
      (analyze-function-definition symbol (symbol-function (si:unencapsulate-function-spec symbol))
                                   fun))
    (when (and (symbol-plist symbol)
               (eq (si:locf (symbol-plist symbol))
                   (si:follow-cell-forwarding (si:locf (symbol-plist symbol)) t)))
      (when (typep (get symbol 'si:flavor) 'si:flavor)
        (analyze-flavor symbol (get symbol 'si:flavor) fun))
      (when include-properties
        (do ((l (symbol-plist symbol) (cddr l)))
            ((null l))
          (unless (member (car l) *who-calls-ignores-these-properties*)
            (when (typep (cadr l) 'compiled-function)
              (analyze-function-definition `(:property ,symbol ,(car l)) (cadr l) fun))))))
    (when (and (boundp symbol)
               (not (member symbol *who-calls-ignores-these-symbols*))
               (eq (si:locf (symbol-value symbol))
                   (si:follow-cell-forwarding (si:locf (symbol-value symbol)) t)))
      (analyze-list symbol (symbol-value symbol) fun))))

(defun analyze-function-definition (symbol definition fun)
  (declare (sys:downward-funarg fun))
  (unless (member symbol *who-calls-ignores-these-symbols*)
    (when (and (eq (si:car-safe definition) 'global:macro)
               (not (member symbol *who-calls-ignores-these-macros*)))
      (setq definition (cdr definition)))
    (typecase definition
      (compiled-function (analyze-fef symbol definition fun))
      (si:closure (analyze-fef symbol (si:closure-function definition) fun))
      (si:flavor (analyze-flavor symbol definition fun))
      (si:select-method (analyze-list symbol definition fun))
      (list (analyze-list symbol (si:lambda-exp-args-and-body definition) fun)))))

(defun analyze-list (symbol list fun)
  (declare (sys:downward-funarg fun))
  (flet ((record-list-element (caller element)
           (when (or (member caller '(*who-calls-ignores-these-symbols*
                                       *who-calls-ignores-these-macros*
                                       *who-calls-ignores-these-properties*))
                     (not (member element *who-calls-ignores-these-symbols*)))
             (funcall fun caller element ':list))))
    (when (list-length list)
      (do ((rest list (cdr rest)))
          ((atom rest) (when (symbolp rest)
                         (record-list-element symbol rest)))
        (let ((element (car rest)))
          (cond ((symbolp element)
                 (record-list-element symbol element))
                ((zl:list-match-p element `(global:function ,ignore))
                 (funcall fun symbol (cadr element) :function))
                ((consp element)
                 (analyze-list symbol element fun))))))))


(defun analyze-fef (caller fef fun)
  (declare (sys:downward-funarg fun))
  (do ((i si:%fef-header-length (1+ i))
       (lim (si:%structure-boxed-size fef)))
      ((>= i lim) nil)
    (cond ((= (si:%p-ldb-offset si:%%q-data-type fef i) si:dtp-one-q-forward)
           ;; Reference to some function or value cell.
           (let* ((tem (si:%p-contents-as-locative-offset fef i))
                  (symbol (si:%find-structure-header tem))
                  (offset (si:%pointer-difference tem symbol)))
             (if (consp symbol)
                 (setq symbol (car symbol)))
             (unless (member symbol *who-calls-ignores-these-symbols*)
               (funcall fun caller symbol (ecase offset
                                            (2 :function)
                                            (1 :value))))))
          ((= (si:%p-ldb-offset si:%%q-data-type fef i) si:dtp-self-ref-pointer)
           (let ((fn (si:fef-flavor-name fef)))
             (when fn
               (multiple-value-bind (symbol use)
                   (si:flavor-decode-self-ref-pointer fn (si:%p-ldb-offset si:%%q-pointer fef i))
                 (funcall fun caller symbol (if use :flavor :value))))))
          ((eq (si:%p-contents-offset fef i) (si:debugging-info fef)))  ;No further processing of such frobs.
          ((and (symbolp (si:%p-contents-offset fef i))
                (not (member (si:%p-contents-offset fef i) *who-calls-ignores-these-symbols*)))
           (funcall fun caller (si:%p-contents-offset fef i) ':constant))))
  ;; Now analyze any internal functions that are part of this one.
  (loop for offset in (cdr (assoc ':internal-fef-offsets (si:debugging-info fef)))
        for i from 0
        doing (analyze-fef caller (si:%p-contents-offset fef offset) fun))
  ;; See if we called any macros.
  (dolist (macro (cadr (assoc ':macros-expanded (si:debugging-info fef))))
    (when (consp macro) (setq macro (car macro)))
    (unless (or (member macro *who-calls-ignores-these-macros*)
                (member macro *who-calls-ignores-these-symbols*))
      (funcall fun caller macro ':macro-expanded))))


(defun analyze-flavor (symbol flavor fun)
  (declare (sys:downward-funarg fun))
  (dolist (iv (si:flavor-local-instance-variables flavor))
    (funcall fun symbol (if (atom iv) iv (car iv)) ':instance-variable))
  (dolist (iv (si:flavor-inittable-instance-variables flavor))
    (funcall fun symbol (cdr iv) ':instance-variable))
  (dolist (k (si:flavor-init-keywords flavor))
    (funcall fun symbol k ':flavor-init-keyword))
  (dolist (iv (si:flavor-inittable-instance-variables flavor))
    (funcall fun symbol (car iv) ':flavor-init-keyword))
  ;; Methods.
  (dolist (mte (si:flavor-method-table flavor))
    (dolist (meth (cdddr mte))
      (if (si:meth-definedp meth)
          (analyze-function-definition (si:meth-function-spec meth)
                                       (si:meth-definition meth)
                                       fun)))))



;;;; Compiler Interface

;(si:advise compiler:compile :before 'update-who-calls nil
;  (let ((fname (si:unencapsulate-function-spec (first si:arglist))))
;    (when (si:fdefinedp fname)
;      (push (list :erase-function fname (si:fdefinition fname))
;           *who-calls-queue*))))

;(si:advise compiler:compile :after 'update-who-calls nil
;  (let* ((fname (si:unencapsulate-function-spec (first si:arglist)))
;        (fef (if fname (si:fdefinition fname) (first si:values))))
;    (push (list :record-function fname fef)
;         *who-calls-queue*)))


;;; +++ Additional interfaces:
;;;     Editor .................. ZWEI:COMPILE-INTERVAL or COMPILER:COMPILE-STREAM
;;;     Make-system ............. SI:QC-FILE-1 and SI:READFILE-1
;;;     File Compiler ........... COMPILER:QC-FILE
;;;     Loader .................. SI:READFILE





;;;; Edit History for WHO-CALLS.LISP.1
;;; [11/01/88 01:38 CStacy] New WHO-CALLS feature.
