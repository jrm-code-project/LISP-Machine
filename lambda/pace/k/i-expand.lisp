;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:ZL -*-


;;; Expansion of K machine symbolic instructions
;;; into Lisp code with calls to simulator functions

;;; (MOVE O2 A3)
;;; (MOVE RETURN A1 CH-RETURN)
;;; (MOVE O0 A1 CH-OPEN)
;;; (ALU L+R a0 A2 A3 CH-OPEN)
;;; (ALU L+R+1 RETURN '0 A1 CH-RETURN)
;;; (TEST BR-EQUAL)
;;; (BRANCH FOO)
;;; (BRANCH FOO (MOVE A0 A1))
;;; (BRANCH FOO (MOVE A0 A1) CH-OPEN)
;;; (BRANCH FOO (ALU L+R O0 A1 A2) CH-OPEN)
;;; (KCALL FOO '3 A0 (O0 A1) CH-OPEN)
;;; (KCALL FOO '3 O0)
;;; (KOPEN)
;;; (OPEN-CALL CAR '1 O1 (O0 A1))
;;; (TAIL-CALL FOO '2)
;;; (TAIL-CALL FOO '2 (O1 A2))
;;; (JUMP FOO)
;;; (JUMP FOO (A0 A1))

(defparameter *statistics-p* t)
(defparameter *i-time* 75.
  "Instruction cycle time in nanoseconds")
(defvar *i-count* 0)

(defun do-stats ()
  (cond (*statistics-p*
         `(incf *i-count*))
        (t nil)))

(defun stats (form)
  (setq *i-count* 0)
  (prog1
    (eval form)
    (format t "~&~d Instruction~:p, ~,9f Seconds (~dns//Inst)"
            *i-count* (* *i-count* *i-time* 1e-9) *i-time*)))

(defvar *global-registers* '())

(defmacro defreg (name frame offset)
  `(progn (pushnew (list ',name ,frame ,offset) *global-registers*)
          (defvar ,name)))


;;;; Alu Ops

(defmacro K:L+R+1 (l r)
  `(1+ (+ ,l ,r)))

(defmacro K:L+R-1 (l r)
  `(1- (+ ,l ,r)))

(defmacro K:L+R (l r)
  `(+ ,l ,r))

(defmacro K:L-R (l r)
  `(si:%pointer-difference ,l ,r))
;  `(- ,l ,r))


(defmacro K:SETR (l r)
  (declare (ignore l))
  r)

(defmacro K:SETL (l r)
  (declare (ignore r))
  l)

(defmacro K:ALU-OR (l r)
  `(logior ,l ,r))

(defmacro K:ALU-AND (l r)
  `(logand ,l ,r))


;;; Optional fields

(defvar *default-options* ())

(defmacro def-option (name default arglist &body body)
  `(progn (pushnew ',default *default-options*)
          (setf (get ',name 'option-default) ',default)
          (defmacro ,name ,arglist
            ,@body)))


(defun do-options (options left right)
  (let ((defaults *default-options*))
    `(progn
       ,(do-stats)
       ,@(mapcar #'(lambda (option)
                     (setq defaults
                           (remove (get option 'option-default) defaults))
                     (list option left right))
                 options)
       ,@(mapcar #'(lambda (option)
                     (list option left right))
                 defaults))))


;;;; Call Hardware
(def-option K:CH-NOOP K:CH-NOOP (l r)
  nil)

(def-option K:CH-OPEN K:CH-NOOP (l r)
  (declare (ignore l r))
  `(%k-open))

(def-option K:CH-TAIL-OPEN k:CH-NOOP (l r)
  (declare (ignore l r))
  `(%k-t-open))

;;; this is done in the dest
(def-option K:CH-RETURN K:CH-NOOP (l r)
  (declare (ignore l r))
  nil)


;;;; Branch Conditions
;;; This is not quite right
;;; because it should test TEMP of the last instruction
;;; (before the one with the condition) but either this
;;; instruction or the branch might change TEMP ...

(defvar *condition* 'always)

(def-option K:BR-NEVER K:BR-NEVER (l r)
  (setq *condition* 'nil) nil)

(def-option K:BR-ALWAYS K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* 'true) nil)

(def-option K:BR-EQUAL K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* '(zerop temp)) nil)

(def-option K:BR-NOT-EQUAL K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* '(not (zerop temp))) nil)

(def-option K:BR-NOT-LESS-THAN K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* '(not (< temp 0))) nil)

(def-option K:BR-NOT-GREATER-THAN K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* '(not (> temp 0))) nil)

(def-option K:BR-NOT-LESS-THAN-OR-EQUAL K:BR-NEVER (l r)
  (declare (ignore l r))
  (setq *condition* '(not (<= temp 0))))


;;;; Data type checking
(defun trap (trap) (error "Trap: ~a" trap))

(def-option K:DT-NOOP K:DT-NOOP (l r)
  (declare (ignore l r))
  nil)

(def-option K:DT-BOTH-FIX K:DT-NOOP (l r)
  `(unless (and (fixp ,l) (fixp ,r))
     (trap 'not-both-fix)))



;;;; Sources

;;; specifies a global register
(defmacro K:%REGISTER (name frame offset)
  name)

(defmacro K:%VALUE-CELL (name)
  name)


;;;; Destinations

(defvar *functional-destinations*
        '(K:MD K:VMA))

(defun store-dest (dest options)
  (cond ((or (member dest *active-registers*)
             (member dest *open-registers*)
             (member dest *return-registers*))
         `((setq ,dest temp)))
        ((and (eq dest 'K:RETURN)
              (member 'K:CH-RETURN options))
         `((%k-return temp)))
        ((eq (car-safe dest) 'K:%REGISTER)
         (if (memq (cadr dest) *functional-destinations*)
             `((,(cadr dest) temp))
           `((setq ,(cadr dest) temp))))
        (t `((,dest temp)))))


(defmacro K:GARBAGE (value)
  (declare (ignore value))
  ())


;----------------------------------------------------------------
;;;; Instructions


;;; temp represents the value on the output of the ALU
;;; before it gets put in a dest
(defmacro K:ALU (op dest left right &rest options)
  `(progn (setq temp (,op ,left ,right))
          ,(do-options options left right)
          . ,(store-dest dest options)))


(defmacro K:MOVE (dest source &rest options)
  `(K:ALU K:SETR ,dest '0 ,source . ,options))

(defmacro K:KDPB (dest left right ppss &rest options)
 `(progn (setq temp (dpb ,left ,ppss ,right))
         ,(do-options options left right)
         . ,(store-dest dest options)))

(defmacro K:KCALL (tag nargs dret &optional move &rest options)
  (declare (zl:arglist tag nargs dret (dest source) &rest options))
  `(progn
     ,(if move
          `(K:MOVE ,(car move) ,(cadr move) . ,options)
        (do-options options nil nil))
     (%k-call #',tag ,(cadr nargs) (locf ,(if (eq dret 'IGNORE)
                                              'temp
                                            dret)))))

(defmacro K:TAIL-CALL (tag nargs &optional move &rest options)
  (declare (zl:arglist tag nargs (dest source) &rest options))
  `(progn
     ,(if move
          `(K:MOVE ,(car move) ,(cadr move) . ,options)
        (do-options options nil nil))
     (%k-t-call #',tag ,(cadr nargs))))

(defmacro K:OPEN-CALL (tag nargs dret &optional move &rest options)
  (declare (zl:arglist tag nargs dret (dest source) &rest options))
  `(K:KCALL ,tag ,nargs ,dret ,move K:CH-OPEN . ,options))

(defmacro K:OPEN-TAIL-CALL (tag nargs &optional move &rest options)
  (declare (zl:arglist tag nargs (dest source) &rest options))
  `(K:TAIL-CALL ,tag ,nargs ,move K:CH-TAIL-OPEN . ,options))

;;; this is not right
(defmacro K:KOPEN ()
  `(progn ,(do-stats)
          (%k-open)))

(defmacro K:TAIL-OPEN ()
  `(progn ,(do-stats)
          (%k-t-open)))



(defmacro K:JUMP (tag &optional move &rest options)
  (declare (zl:arglist tag (dest source) &rest options))
  `(progn
     ,(if move
          `(K:MOVE ,(car move) ,(cadr move) . ,options)
        (do-options options nil nil))
     (go ,tag)))


(defmacro K:TEST (cond)
  `(progn ,(do-stats)
          (,cond nil nil)))

(defmacro K:BRANCH (tag &optional alu &rest options)
  `(progn
     ,(if alu
          `(K:ALU ,alu . ,options)
        (do-options options nil nil))
     (if ,*condition* (go ,tag))))
