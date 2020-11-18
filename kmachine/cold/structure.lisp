;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:ZL -*-

;;;; Structures

(defun MAKE-STRUCTURE (length)
  (cons:allocate-structure
    (1+ length)
    0
    vinc:$$dtp-structure
    (cons:make-header vinc:$$dtp-structure-header length)))

;(defun structure-ref (name index struct)
;  (let ((length (li:read-struct-header struct))
;       (struct-name (cons:contents-offset struct 1)))
;    (if (or (eq struct-name
;               name)
;           (and (consp struct-name)
;                (member name struct-name :test #'eq)))
;       (if (< index length)
;           (cons:contents-offset struct index)
;         (error "Structure too short"))
;      (error "Structure of wrong type"))))

;;; Structure Ref
;;;  Takes:
;;;    A0 NAME:   a symbol, which should match the type slot
;;;    A1 INDEX:  an integer, the type slot is 0
;;;    A2 STRUCT: the structure object
;;;  Checks:
;;;    1. If it's a structure
;;;    2. If it's big enough
;;;    3. If it has the right name
(defafun STRUCTURE-REF (name index struct)
  ;; Read header and check data type of struct
  (alu setl (vma-start-read boxed-vma boxed-md) a2 gr:*random-array* dt-right-array-and-left-structure)
  ;; Check data type of index
  (alu setr a1 a1 a1 dt-both-fixnum boxed-right)
  ;; Check bounds of index
  (alu l-r nop a1 md bw-24)
  ;; Read structure name
  (alu l+r (vma-start-read unboxed-vma boxed-md) a2 gr:*one* br-not-less-than)
  (branch too-short ())
  (move a3 md)
  ;; start read of slot
  (alu l+r+c (vma-start-read unboxed-vma boxed-md) a1 a2 carry-1 bw-24)
  ;; test type
  (alu l-r nop a0 a3 bw-32)
  (test br-not-equal)
  (branch wrong-type ())
  ;; type ok, return slot
  (return md boxed-right)
 wrong-type
  (move o0 a0 ch-tail-open)
  (move o1 a1)
  (move o2 a2)
  (tail-call (structure-ref-wrong-type 4) (o3 a3))
 too-short
  (movei o0 '"The structure ~a does not have a slot ~a" boxed ch-tail-open)
  (move  o1 a2)
  (tail-call (error 3) (o2 a1)))


(defun structure-ref-wrong-type (name index struct type)
  (if (and (consp type)
           (member-eq name type))
      (cons:contents-offset struct (1+ index))
    (error "The structure ~a is not of type ~a" struct name)))


(defafun STRUCTURE-SET (name index struct value)
  ;; Read header and check data type of struct
  (alu setl (vma-start-read boxed-vma boxed-md) a2 gr:*random-array* dt-right-array-and-left-structure)
  ;; Check data type of index
  (alu setr a1 a1 a1 dt-both-fixnum boxed-right)
  (alu l-r nop a1 md bw-24)
  (alu l+r (vma-start-read unboxed-vma boxed-md) a2 gr:*one* br-not-less-than)
  (branch too-short ())
  (move a4 md)
  ;; start read of slot
  (alu l+r+c vma-start-read-will-write a1 a2 bw-24 carry-1 boxed-vma boxed-md)
  (nop)
  (move nop md)
  (move md-start-write a3 boxed-md)
  (return a3 boxed-right)
  ;; test type
  (alu l-r nop a0 a3 bw-32)
  (test br-not-equal)
  (branch wrong-type ())
  ;; type ok, return slot
  (return md boxed-right)
 wrong-type
  (move o0 a0 ch-tail-open)
  (move o1 a1)
  (move o2 a2)
  (move o3 a3)
  (tail-call (structure-set-wrong-type 5) (o4 a4))
 too-short
  (movei o0 '"The structure ~a does not have a slot ~a" boxed ch-tail-open)
  (move  o1 a2)
  (tail-call (error 3) (o2 a1)))

(defun structure-set-wrong-type (name index struct value type)
  (if (and (consp type)
           (member name type :test #'eq))
      (cons:store-contents-offset struct (1+ index) value)
    (error "The structure ~a is not of type ~a" struct name)))


(defmacro MAKE-STRUCTURE-OBJECT (name &rest slot-values)
  (let ((struct-var (gensym 'struct)))
    `(LET ((,struct-var (MAKE-STRUCTURE ,(1+ (length slot-values)))))
       (cons:store-contents-offset ,struct-var 1 ,name)
       ,@(let ((n 1))
           (mapcar #'(lambda (slot-val)
                       `(CONS:STORE-CONTENTS-OFFSET ,struct-var ,(incf n) ,slot-val))
                   slot-values))
       ,struct-var)))


(defun TYPEP-STRUCTURE (thing type)
  (and (vinc:structure-p thing)
       (let ((stypes (cons:contents-offset thing 1)))
         (or (eq stypes type)
             (and (consp stypes)
                  (member type stypes :test #'eq))))))
