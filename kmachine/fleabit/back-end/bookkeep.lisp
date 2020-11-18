;;; -*- Mode:LISP; Package:(NC LISP); Base:10; Readtable:CL -*-

;;; Copyright (c) 1985 Yale University
;;;     Authors: N Adams, R Kelsey, D Kranz, J Philbin, J Rees.
;;; This material was developed by the T Project at the Yale University Computer
;;; Science Department.  Permission to copy this software, to redistribute it,
;;; and to use it for any purpose is granted, subject to the following restric-
;;; tions and understandings.
;;; 1. Any copy made of this software must include this copyright notice in full.
;;; 2. Users of this software agree to make their best efforts (a) to return
;;;    to the T Project at Yale any improvements or extensions that they make,
;;;    so that these may be included in future releases; and (b) to inform
;;;    the T Project of noteworthy uses of this software.
;;; 3. All materials developed as a consequence of the use of this software
;;;    shall duly acknowledge such use, in accordance with the usual standards
;;;    of acknowledging credit in academic research.
;;; 4. Yale has made no warrantee or representation that the operation of
;;;    this software will be error-free, and Yale is under no obligation to
;;;    provide any services, by way of maintenance, update, or otherwise.
;;; 5. In conjunction with products arising from the use of this material,
;;;    there shall be no use of the name of the Yale University nor of any
;;;    adaptation thereof in any advertising, promotional, or sales literature
;;;    without prior written consent from Yale in each case.
;;;

;;; Copyright (c) 1985 David Kranz

(defun addressable? (val) (or (eq val t)
                              (eq val nil)
                              (eq (zl:data-type val) 'zl:dtp-fix)
                              (eq (zl:data-type val) 'zl:dtp-character)))


(zl:defsubst reg-offset (x y) (cons x y))

(zl:defsubst machine-num (x) ($ x))


(defun lit (x)
  (list 'quote x))


(zl:defsubst register? (x)
  (and (integerp x) (>= x 0) (< x *real-registers*)))


;;; Registers and temps are represented in the same structure

(zl:defsubst reg-node (reg)
  (svref *registers* reg))

(zl:defsubst temp-node (reg) (reg-node reg))


(defun reg-type (reg)
    (if (or (< reg *scratch-registers*)
            (>= reg (+ *real-registers* *pointer-temps*)))
        'scratch
        'pointer))



(defvar *lock-mark* 'lock)  ;(object nil ((identification self) 'lock)))

(zl:defsubst lock (reg)
  (if (< reg *no-of-registers*)
      (push *lock-mark* (reg-node reg))))

(zl:defsubst unlock (reg)
  (if (< reg *no-of-registers*)
      (pop (reg-node reg))))

(zl:defsubst locked? (reg)
  (let ((n (reg-node reg)))
    (and (consp n) (eq (car n) *lock-mark*))))


;;; Locations
;;;==========================================================================
;;;   Keeps track of where values are.
;;; A table of a-lists of form ((<type-of-location> . <index>)...) indexed by
;;; leaf values, i.e. variables, primops, or literals.

(defvar *locations* (make-table 'locations))

(zl:defsubst leaf-locations (value)
   (table-entry *locations* value))

(zl:defsubst get-location (value type)
  (cdr (assoc type (leaf-locations value) :test #'eq)))

(defun set-location (value type number)
  (if (null value) (zl:eh))
  (let* ((locs (leaf-locations value))
         (pair (assoc type locs :test #'eq)))
    (if pair
        (setf (cdr pair) number))
    (setf (table-entry *locations* value) (cons (cons type number) locs))))


(defun clear-location (value type)
  (setf (leaf-locations value)
        (delete type (leaf-locations value) :test #'eq :key #'car))
  nil)


(zl:defsubst register-loc (value)
  (get-location value 'reg))

(zl:defsubst set-register-loc (value register)
  (if (null register)
      (clear-location value 'reg)
    (set-location value 'reg register)))

(defsetf register-loc set-register-loc)

;    ((identification self) 'register-loc)



(zl:defsubst temp-loc (value)
  (get-location value 'temp))

(zl:defsubst set-temp-loc (value temp)
  (if (null temp)
      (clear-location value 'temp)
    (set-location value 'temp temp)))

(defsetf temp-loc set-temp-loc)

;    ((identification self) 'temp-loc)





;;; ->REGISTER Move the value of leaf-node REF into a register of type TYPE
;;; which can be either '* or a specific register. Force an existing value out
;;; if necessary,

(defun ->register (type node var where)
  (let ((accessor (access-value node var)))
    (cond ((and (register? accessor)
                (or (and (eq (reg-type accessor) type)
                         (eq where '*))
                    (eq accessor where)))
           accessor)
          (t
           (cond ((register? accessor)
                  (setf (register-loc var) nil)
                  (cond ((locked? accessor)
                         (setf (cdr (reg-node accessor)) nil))
                        (t
                         (setf (reg-node accessor) nil)))))
           (into-register type node var accessor where)))))

(defun in-register? (type value where)
  (let ((reg (register-loc value)))
    (and reg
         (eq (reg-type reg) type)
         (or (eq where '*)
             (eq where reg)))))


(defun get-target-register (node t-spec)
  (cond ((register? t-spec)
         (cond ((or (not (reg-node t-spec))
                    (and (not (locked? t-spec))
                         (dying? (reg-node t-spec) node)))
                t-spec)
               (t
                (get-register (reg-type t-spec) node '*))))
        (t
         (get-register t-spec node '*))))




(defun get-register (type node where)
  (cond ((not (eq where '*))
         (free-register node where)
         where)
        ((not (eq type 'scratch))
         (really-get-register 'pointer node *scratch-registers* *real-registers* t))
        (t
         (really-get-register 'scratch node
                              0
                              *scratch-registers* t))))

(defun get-reg-if-free (spec node)
  (cond ((register? spec)
          (if (reg-node spec) nil spec))
         ((eq spec 'pointer)
          (really-get-register spec node *scratch-registers* *real-registers* nil))
         ((eq spec 'scratch)
          (really-get-register spec node 0 *scratch-registers* nil))
         ((eq spec '*)
          (really-get-register spec node 0 *real-registers* nil))
         (t (bug "bad spec: ~s" spec))))

(defun really-get-register (type node start stop kick?)
  (do ((i start (1+ i)))
      ((>= i stop)
       (if kick? (select-and-kick-register node type) nil))
    (if (not (reg-node i))
        (return i))))

(defun into-register (type node value access where)
  (cond ((in-register? type value where)
         (register-loc value))
        (t
         (let ((reg (get-register type node where)))
           (generate-move access reg)
           (let ((reg (register-loc value)))
             (if reg (setf (reg-node reg) nil)))
           (mark value reg)
           reg))))


;;; SELECT-AND-KICK-REGISTER The first register which is not locked or used soo
;;; is selected.  If none satisfy then the first register  is selected.

(defun select-and-kick-register (node type)
  (cond ((eq type 'pointer)
         (do ((i (1+ *scratch-registers*) (1+ i))
              (default P)) ;kick P?
             ((>= i *real-registers*)
              (kick-register node default)
              default)
           (cond ((and (not (locked? i))
                       (not (used-soon? node (reg-node i))))
                  (kick-register node i)
                  (return i))
                 (t (setq default i)))))
        (t
         (do ((i 0 (1+ i)) (default nil))
             ((>= i *scratch-registers*)
              (free-register node default)
              default)
           (cond ((and (not (locked? i))
                       (not (used-soon? node (reg-node i))))
                  (free-register node i)
                  (return i))
                 (t (setq default i)))))))


;;; USED-SOON? Is this variable used at this node or at one of its
;;; continuations?

(defun used-soon? (node value)
  (flet ((var-used? (arg)
                    (and (leaf-node? arg)
                         (eq (leaf-value arg) value))))
     (or (some #'var-used? (call-args node))
         (some #'(lambda (cont)
                   (some #'var-used? (call-args (lambda-body cont))))
               (continuations node)))))

(zl:defsubst free-register (node reg)
  (cond ((reg-node reg)
         ;=> (lambda (value)
         ;     (let  ((new (where-used value)))
         ;       (cond (nil ;(and (register? new) (get-reg-if-free new node))
         ;              => (lambda (new)
         ;                   (mark value new)
         ;                   (set (reg-node reg) nil)
         ;                   (generate-move reg new)))
         ;             (else
                       (kick-register node reg))))


(defun kick-register (node reg)
  (let ((value (reg-node reg)))
    (cond ((locked? reg)
           (error "attempt to kick out of locked register"))
          ((or (temp-loc value)
               (not (variable-p value)))
;               (and (not (eq value (lambda-self-var *lambda*)))
;                    (= (variable-number value) 0)))
           (setf (register-loc value) nil)
           (setf (reg-node reg) nil))
          (t
           (let ((temp (get-temp value (reg-type reg) node)))
             (setf (register-loc value) nil)
             (setf (temp-loc value) temp)
             (setf (reg-node reg) nil)
             (generate-move reg temp))))))


;;; ??? we don't have any temps...
(defun really-get-temp (type node)
  (cond ((eq type 'scratch)
         (really-get-register 'scratch node
                              (+ *real-registers* *pointer-temps*)
                              *no-of-registers*
                              nil))
        (t
         (really-get-register 'pointer node
                              A0   ;*real-registers*
                              AN-1 ;(+ *real-registers* *pointer-temps*)
                              nil))))

(defun get-temp (value type node)
  (let ((temp (really-get-temp type node)))
    (cond (temp
           (if (> temp *max-temp*)
               (setq *max-temp* temp))
           (setf (temp-node temp) value)
           temp)
          (t
           (bug "all temps used")))))

(zl:defsubst cont (node)
  (car (call-args node)))

(defun continuations (node)
  (do ((i (call-exits node) (1- i)) (args '()))
      ((zerop i) args)
    (let ((arg (call-arg-n i node)))
      (setq args (cond ((lambda-node? arg) (cons arg args))
                       (t (let ((label (variable-known (leaf-value arg))))
                             (cond (label (cons label args))
                                   (t args)))))))))

(zl:defsubst then-cont (node)
  (car (call-args node)))

(zl:defsubst else-cont (node)
  (cadr (call-args node)))


(defun live? (value node)
  (let ((value (cond ((and (consp value) (variable-p (cdr value)))
                      (cdr value))
                     ((variable-p value) value)
                     (t nil))))
     (cond ((not value) nil)
           (t
            (some #'(lambda (cont)
                      (member value (lambda-live cont) :test #'eq))
                  (continuations node))))))

(zl:defsubst dying? (value node)
  (not (live? value node)))

(defun dead? (value node)
  (let ((parent (node-parent node)))
    (not (and (variable-p value)
              (or (member value (lambda-variables parent) :test #'eq)
                  (member value (lambda-live parent) :test #'eq))))))


(zl:defsubst kill-if-dying (var node)
  (if (dying? var node) (kill var)))


(defun kill-if-dead (node where)
  (if (and (leaf-node? node)
           (or (not (variable-p (leaf-value node)))
               (not (member (leaf-value node) (lambda-live where) :test #'eq))))
      (kill (leaf-value node))))

(defun kill (value)
  (let ((reg (register-loc value)))
    (cond (reg
           (cond ((locked? reg)
                  (unless (eq (cdr (reg-node reg)) value)
                      (bug "horrible inconsistancy reg ~S value ~S"
                           reg
                           value))
                  (setf (cdr (reg-node reg)) nil))
                 (t
                  (unless (eq (reg-node reg) value)
                    (bug "horrible inconsistancy reg ~S value ~S"
                         reg
                         value))
                  (setf (reg-node reg) nil)))
           (setf (register-loc value) nil))))
  (let ((reg (temp-loc value)))
    (cond (reg
           (cond ((locked? reg)
                  (unless (eq (cdr (temp-node reg)) value)
                    (bug "horrible inconsistancy reg ~S value ~S"
                         reg
                         value))
                  (setf (cdr (temp-node reg)) nil))
                 (t
                  (unless (eq (temp-node reg) value)
                    (bug "horrible inconsistancy reg ~S value ~S"
                         reg
                         value))
                  (setf (temp-node reg) nil)))
           (setf (temp-loc value) nil)))))


;;; pools for vector of registers (see ALLOCATE-CONDITIONAL-PRIMOP in reg.t)

;(define register-vector-pool
;        (make-pool 'reg-vec-pool (lambda () (make-vector *no-of-registers*))))

(defun copy-registers ()
  (copy-seq *registers*))

(zl:defsubst return-registers ())

;(define-integrable (return-registers)
;  (return-to-pool register-vector-pool *registers*))

(defun restore-slots ()
  (restore-registers)
  (restore-temps))

(defun restore-registers ()
  (dotimes (i *real-registers*)
    (cond ((reg-node i)
           (setf (register-loc (reg-node i)) i)))))

(defun restore-temps ()
  (dotimes (i *real-registers*)
    (cond ((temp-node i)
           (setf (temp-loc (temp-node i)) i)))))




(defvar *pushed-o-regs* '())

(defun push-o-regs ()
  (do ((o O0 (1+ o))
       (frame ()))
      ((>= o ON) (push frame *pushed-o-regs*))
    (let ((value (reg-node o)))
      (when value
        (push (cons o value) frame)
        (kill value)))))

(defun pop-o-regs ()
  (clear-o-regs)
  (dolist (reg-val (pop *pushed-o-regs*))
    (mark (cdr reg-val) (car reg-val))))

;;; say that the o regs are bashed
;;; called when about to do a call    -efh
(defun clear-o-regs ()
  (do ((o O0 (1+ o)))
      ((>= o ON))
    (unlock o)     ;locked by parallel-assign
    (kill (reg-node o))))

(defun clear-slots ()
  (dotimes (i (array-dimension *registers* 0))
    (setf (svref *registers* i) nil))
;  (recycle *locations*)
  (setq *locations* (make-table 'locations)))



(defun protect-access (access)
  (cond ((integerp access)
         (cond ((>= access 0)
                (lock access))))
;        ((fg? access))
        ((register? (car access))
         (if (/= (car access) SP)
             (lock (car access))))
        ((consp (car access))
         (lock (caar access))
         (lock (cdar access)))))

(defun release-access (access)
  (cond ((integerp access)
         (cond ((>= access 0)
                (unlock access))))
;        ((fg? access))
        ((register? (car access))
         (if (/= (car access) SP)
             (unlock (car access))))
        ((consp (car access))
         (unlock (caar access))
         (unlock (cdar access)))))

(defun mark (value reg)
  (setf (reg-node reg) value)
  (if (register? reg)
      (setf (register-loc value) reg)
      (setf (temp-loc value) reg)))


(defun mark-temp (value reg)
  (setf (temp-node reg) value)
  (setf (temp-loc value) reg))
