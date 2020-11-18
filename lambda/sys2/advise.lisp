;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10; Lowercase:T -*-

;;; Implements the mechanism by which advised functions operate.

(deff (:special-form advise-prog) #'(:special-form prog))
(deff (:special-form advise-setq) #'(:special-form setq))
(deff (:special-form advise-progn) #'(:special-form progn))
(deff (:special-form advise-multiple-value-list) #'(:special-form multiple-value-list))
(deff advise-return-list #'return-list)
(deff advise-apply #'apply)
(deff (:special-form advise-let) #'(:special-form let))
(deff advise-list* #'list*)

;;; NOTE!!  Each of the above must have an optimizer in QCOPT, to compile properly.

(defmacro advised-function (before after around inner-function-expression)
  "Expands into the code that executes advice in the proper order."
  (let ((default-cons-area background-cons-area))
    `(advise-prog (values)
                  (declare (special values))
                  (advise-setq values
                               (advise-multiple-value-list
                                 (advise-progn
                                   ,@before
                                   ,(advise-merge-arounds around inner-function-expression))))
                  ,@after
                  (advise-return-list values))))

;;; Take the list of around advise and merge it together
;;; Producing a form which evaluates them around the body.

(defun advise-merge-arounds (advice-list inner-function-expression)
  (if (null advice-list)
      `(advise-apply ,inner-function-expression arglist)
    (cl:subst (advise-merge-arounds (cdr advice-list) inner-function-expression) ':do-it
              (car advice-list) :test #'eq)))

(defun (:property advise encapsulation-grind-function) (function def width real-io untyo-p)
  (when def                                     ; Print the advice as calls to advise.
    (if (typep def 'compiled-function)
        (setq def (cadr (assq 'interpreted-definition (debugging-info def)))))
    (let ((body (encapsulation-body def)))
      (when (eq (car (car body)) 'si:displaced)
        (setf (car body) (cadr (car body))))
      (grind-print-advice-slot (cadr (car body)) ':before
                               function width real-io untyo-p)
      (grind-print-advice-slot (caddr (car body)) ':after
                               function width real-io untyo-p)
      (grind-print-advice-slot (cadddr (car body)) ':around
                               function width real-io untyo-p))))

(defun grind-print-advice-slot (slot-contents slot-name function width real-io untyo-p)
  (do ((l slot-contents (cdr l)) (i 0 (1+ i)))  ((null l))
    (grind-top-level `(advise ,function ,slot-name ,(cadr (cadar l)) ,i . ,(cddar l))
                     width real-io untyo-p)))

(defvar *advised-functions* nil
  "List of all function specs that have been advised.")

;;; Make a specifed function into an advised function
;;; (with no advice, as yet) if it isn't one already.
;;; Undisplace the advised-function macro if it has displaced itself.
(defun advise-init (function-spec)
  (let ((default-cons-area background-cons-area)
        (spec1 (unencapsulate-function-spec function-spec 'advise)))
    (cond ((neq spec1 (unencapsulate-function-spec spec1 '(advise)))
           (uncompile spec1 t)
           (let ((body (encapsulation-body (fdefinition spec1))))
             ;; (car body) looks like:
             ;;        (advised-function nil nil nil encapsulated-function)
             (when (eq (car (car body)) 'si:displaced)
               (setf (car body) (cadr (car body))))))
          (t
           (push function-spec *advised-functions*)
           (encapsulate spec1 function-spec 'advise
                        `(advised-function nil nil nil ,encapsulated-function))))))

(defmacro advise (&optional function-spec class name position &body forms)
  "Put advice on FUNCTION-SPEC to perform FORMS.
CLASS is :BEFORE, :AFTER or :AROUND.
NAME is the name for this piece of advice;
any existing piece with the same name and class will be replaced.
POSITION says where to put this advice wrt others of same class;
it is a number, or the name of some other piece of advice to go after,
or NIL meaning put this one first.
If given no arguments, ADVISE returns a list of functions which are presently advised."
  (if (null function-spec)
      '*advised-functions*
      `(advise-1 ',function-spec ',class ',name ',position ',forms)))

(defun advise-1 (function-spec class name position forms)
  (setq function-spec (dwimify-arg-package function-spec 'function))
  (advise-init function-spec)
  (setq forms (rename-within-new-definition-maybe function-spec forms))
  (advise-update-list (advise-find-slot (unencapsulate-function-spec function-spec 'advise)
                                        class)
                      name position forms)
  (if compile-encapsulations-flag
      (compile-encapsulations function-spec 'advise))
  function-spec)

(defun advise-find-slot (function-spec class &aux body)
  (uncompile function-spec t)
  (setq body (encapsulation-body (fdefinition function-spec)))
  (nthcdr (ecase class
            (:before 1)
            (:after 2)
            (:around 3))
          (car body)))

(defun advise-update-list (slot-location name position forms)
  (let* ((default-cons-area background-cons-area)
         preceding (new-unit `(progn ',name . ,forms)))
    (cond ((numberp position)
           (or (setq preceding (nthcdr position (locf (car slot-location))))
               (progn (setq preceding (locf (car slot-location)))
                      (do () ((null (cdr preceding)))
                        (pop preceding)))))
          ((and (null name) (null position)))
          ((or (symbolp position) (null position))
           (setq preceding (mem #'(lambda (p elt) (eq p (cadadr elt)))
                                (or position name) (locf (car slot-location))))))
    ;; If the symbol isn't found, or no position is specified,
    ;; insert new advice at the beginning.
    (or preceding (setq preceding (locf (car slot-location))))
    (push new-unit (cdr preceding))
    ;; If we have a name, delete any old advice with that name.
    (and name
         (do ((l (locf (car slot-location)) (cdr l))) ((null l))
           (and (eq (cadadr (cadr l)) name)
                (neq (cadr l) new-unit)
                (return (rplacd l (cddr l))))))
    nil))

(defmacro unadvise (&optional function-spec class position)
  "Remove some or all advice from FUNCTION-SPEC, or from all functions.
With no arguments, all advice is removed.  This is a consequence of these rules:
If FUNCTION-SPEC is non-NIL, advice is removed from that function only.
 Otherwise, advice is removed from all functions if the other args match.
If CLASS is non-NIL, only advice of that class is removed.
If POSITION is non-NIL (a number or name), only advice with that position is removed."
  (if (null function-spec)
      `(dolist (fn *advised-functions*)
         (unadvise-1 fn ',class ',position))
    `(unadvise-1 ',function-spec ',class ',position)))

(defun unadvise-1 (function-spec &optional class position)
  (setq function-spec (dwimify-arg-package function-spec 'function))
  (and (member-equal function-spec *advised-functions*) (advise-init function-spec))
  (check-type class (member nil :before :after :around))
  (check-type position (or symbol (integer 0)))
  (let* ((spec1 (unencapsulate-function-spec function-spec 'advise)))
    (dolist (slot-location
              (if class (list (advise-find-slot spec1 class))
                (list (advise-find-slot spec1 ':before)
                      (advise-find-slot spec1 ':after)
                      (advise-find-slot spec1 ':around))))
      ;; For each slot we are supposed to operate on,
      ;; remove any advice that matches POSITION.
      (cond ((null position)
             (if (consp slot-location)
                 (setf (car slot-location) nil)))
            ((numberp position)
             (let ((preceding (nthcdr position (locf (car slot-location)))))
               (when (cdr preceding) (rplacd preceding (cddr preceding)))))
            ((symbolp position)
             (do ((l (locf (car slot-location)) (cdr l)))
                 ((null l))
               (and (eq (cadadr (cadr l)) position)
                    (return (rplacd l (cddr l))))))))
    ;; Flush the encapsulation if there is no advice in it.
    (and (null (car (advise-find-slot spec1 ':before)))
         (null (car (advise-find-slot spec1 ':after)))
         (null (car (advise-find-slot spec1 ':around)))
         (let ((olddef (fdefinition (unencapsulate-function-spec spec1 '(advise)))))
           (cond ((eq (car (fdefinition spec1)) 'macro)
                  (setq olddef (cons 'macro olddef))))
           (fdefine spec1 olddef)
           (setq *advised-functions* (cl:delete function-spec *advised-functions*
                                                :test #'equal))))
    (if compile-encapsulations-flag
        (compile-encapsulations function-spec 'advise))
    nil))

;;;; ADVISE-WITHIN: advise one function but only when called from another specific one.
;;;    An alternative to advising (:within foo bar).

(defmacro advise-within (within-function-spec function-to-advise class name position &rest forms)
  "Advise FUNCTION-TO-ADVISE, but only when called directly from WITHIN-FUNCTION-SPEC.
This is like using ADVISE on (:WITHIN WITHIN-FUNCTION-SPEC FUNCTION-TO-ADVISE)."
  `(advise-within-1 ',within-function-spec ',function-to-advise
                    ',class ',name ',position ',forms))

(defun advise-within-1 (within-function-spec function-to-advise class name position forms)
  (advise-1 `(:within ,within-function-spec ,function-to-advise)
            class name position forms))

(defmacro unadvise-within (within-function-spec &optional advised-function class position)
  "Remove advice placed on FUNCTION-TO-ADVISE for when called directly from WITHIN-FUNCTION-SPEC.
This is like using UNADVISE on (:WITHIN WITHIN-FUNCTION-SPEC FUNCTION-TO-ADVISE).
If only WITHIN-FUNCTION-SPEC is given, all advice on functions within that is removed.
With no argument, all advice placed on any function within another function is removed."
  `(unadvise-within-1 ',within-function-spec ',advised-function ',class ',position))

;; UNADVISE-WITHIN is not superfluous because if you specify
;; just the within-function-spec, or nothing at all,
;; it eliminates all advising of anything within that within-function-spec,
;; or all advising within anything.

(defun unadvise-within-1 (within-function-spec &optional advised-function class position)
  (if (and within-function-spec advised-function)
      (unadvise-1 `(:within ,within-function-spec ,advised-function) class position)
    (dolist (fn *advised-functions*)
      (when (and (eq (car-safe fn) ':within)
                 (or (null within-function-spec)
                     (eq within-function-spec (second fn)))
                 (or (null advised-function)
                     (eq advised-function (third fn))))
        (unadvise-1 fn class position)))))
