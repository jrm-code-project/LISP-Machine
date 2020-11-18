;;; -*- Mode:LISP; Package:SIMULATOR; Readtable:CL; Base:10 -*-

(defsubst cluster->address (cluster)
  (hw:dpb cluster %%k-cluster-number 0.))

(defsubst map:address-map (virtual-cluster)
  (hw:write-vma-unboxed (vini:cluster->address virtual-cluster)))

(defsubst map:read-map (virtual-cluster)
  (map:address-map virtual-cluster)
  (hw:read-map))

(defun dump-map (address)
  (dotimes (i map:*number-of-map-entries*)
    (hw:write-md-unboxed (map:read-map i))
    (hw:vma-start-write-unboxed-no-gc-write-test (+ address i))))


(DEFKFUN DUMP-MAP (ADDRESS)
  TAG::P_8
         (MOVE A1 '0)
  TAG::DO7650_14
         (ALU L-R NOOP-NO-OVERFLOW-TRAP A1 '65536)
         (TEST BR-NOT-GREATER-THAN-OR-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_17
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_18
         (KDPB WRITE-VMA-UNBOXED A1 '0 '656)
         (MOVE WRITE-MD-UNBOXED READ-MAP)
         (ALU L+R VMA-START-WRITE-UNBOXED-NO-GC-WRITE-TEST A0 A1)
         (ALU L+R+1 A1 A1 '0)
         (JUMP TAG::DO7650_14))


(defkfun dump-map (address)
     (move a1 '0)
  do (alu l-r noop-no-overflow-trap a1 '#.map:*number-of-map-entries*)
     (test br-not-equal)
     (branch c1)
     (move return 'nil ch-return)
  c1 (kdpb write-vma-unboxed a1 '0 #.vini:%%k-cluster-number)
     (move write-md-unboxed read-map)
     (alu l+r vma-start-write-unboxed-no-gc-write-test a0 a1)
     (alu l+r+1 a1 a1 '0)
     (jump do))



(defsubst car (x)
  (hw:car x))

(defun foo ()
  (print (hw:read-map)))


(DEFKFUN FOO NIL
  TAG::P_4
     (OPEN-TAIL-CALL PRINT '1 (O0 READ-MAP)))

(defun bar ()
  (hw:read-map))


(DEFKFUN BAR NIL
  TAG::P_4
     (MOVE RETURN READ-MAP CH-RETURN))


(defun foo (x)
  (hw:write-md-unboxed x))


(DEFKFUN FOO (X)
  TAG::P_5
     (MOVE WRITE-MD-UNBOXED A0)
     (MOVE RETURN 'NIL CH-RETURN))


(defun bar (x)
  (hw:write-md-unboxed x)
  x)


(DEFKFUN BAR (X)
  TAG::P_5
     (MOVE WRITE-MD-UNBOXED A0)
     (MOVE RETURN A0 CH-RETURN))
