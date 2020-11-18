;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:ZL -*-

dynamic go/return-from
  some things don't work, tags are not unique, need to use closure slots...

;;; this doesn't work yet because catch needs to be flushed
(defun drf (x)
  (block quit
    (flet ((punt (value) (return-from quit value)))
      (foobar x #'punt)))
  (finish))

      PUNT_13
   0      (MOVEI O0 (QUOTE QUIT_5) BOXED CH-TAIL-OPEN)
   1      (TAIL-CALL (THROW 2) (O1 A0 BOXED-RIGHT))

      DRF_8
      BLOCK_9
   0      (MOVEI O0 (QUOTE .UNWIND-MARKER.) BOXED CH-OPEN)
   1      (MOVEI O1 (QUOTE QUIT_5) BOXED)
   2      (MOVE O2 (REGISTER *SPECIAL-PDL-PTR* 6 1) BOXED-RIGHT)
   3      (MOVE O3 (REGISTER *STACK-POINTER* 6 0) BOXED-RIGHT)
   4      (MOVE-PC O4 B_21)
   5      (MOVE O0 A0 BOXED-RIGHT CH-OPEN)
   6      (MOVEI O1 (QUOTE #<NCOMPILED-FUNCTION (:INTERNAL DRF "PUNT_13") 53475230>) BOXED)
   7      (CALL (FOOBAR 2) IGNORE NIL)
   8      (CALL (FLUSH-CATCH 0) IGNORE NIL)
      B_21
   9      (OPEN-TAIL-CALL (FINISH 0) NIL)


(defun dgo ()
  (tagbody
      (flet ((punt () (go punt)))
        (foobar #'punt))
   punt))

      PUNT_16
   0      (MOVEI O0 (QUOTE .TAG.) BOXED CH-TAIL-OPEN)
   1      (MOVEI O1 (QUOTE PUNT) BOXED)
   2      (TAIL-CALL (THROW 2) NIL)


      DGO_7
   0      (MOVEI R0 (QUOTE .TAGBODY-START.) BOXED)
      LOOP4970
   1      (MOVEI O0 (QUOTE .UNWIND-MARKER.) BOXED CH-OPEN)
   2      (MOVEI O1 (QUOTE .TAG.) BOXED)
   3      (MOVE O2 (REGISTER *SPECIAL-PDL-PTR* 6 1) BOXED-RIGHT)
   4      (MOVE O3 (REGISTER *STACK-POINTER* 6 0) BOXED-RIGHT)
   5      (MOVE-PC O4 CATCH-CONT4971)
   6      (MOVEI R1 (QUOTE .TAGBODY-START.) BOXED)
   7      (ALU L-R NOP R0 R1)
   8      (TEST BR-EQUAL)
   9      (BRANCH C_10 NIL)
   A      (MOVEI R1 (QUOTE PUNT) BOXED)
   B      (ALU L-R NOP R0 R1)
   C      (TEST BR-EQUAL)
   D      (BRANCH PUNT_12 NIL)
      CATCH-CONT4971
   E      (UNCONDITIONAL-BRANCH LOOP4970 NIL)
      C_10
   F      (MOVEI O0 (QUOTE #<NCOMPILED-FUNCTION (:INTERNAL DGO "PUNT_16") 57242310>) BOXED CH-OPEN)
  10      (CALL (FOOBAR 1) IGNORE NIL)
      PUNT_12
  11      (CALL (FLUSH-CATCH 0) IGNORE (A0 (REGISTER *NIL* 4 5) BOXED-RIGHT))
  12      (NOP)
  13      (MOVE RETURN A0 BOXED-RIGHT CH-RETURN NEXT-PC-RETURN)



--------------------------------------------------------------------------------

array functions used to use less registers, what happened?
   new-open?


;; know fcns entry points?
;; foo gets strategy/heap
;; this seems to compile ok but gets a warning
(defun kfep (a)
  (labels ((foo (&optional x (i 0))
             (if (ppp x)
                 i
               (foo (bar x) (1+ i)))))
    (foo a)))

procs (crocked w/ heap now)

multiple-value-setq/bind (nil foo) ...
 common lisp doesn't allow this, but should complain about setqing/binding nil

try to remove spurious branches in simple optional args

do we need a memory-wait between MD-START-W and VMA-START-R with stack vars?
can stack slots be forwarded? (Read-will-write w/o sequence breaks)

declare

(setq p (foo p)) where p is setqed closed var seems to get dest of p in foo
 v and p in same pref class but p is closed env in (loc p)
   - put env-acc somewhere else?
   - make all pref-class have acc to (closure-ref ...
   - check if acc = *env-acc*

;;; Hey, Ed!  Can we have compiler errors such as
;;; "Foo is bound, but never used" because I am truly
;;; losing and maybe this would help.  - JRM

    - use string name in generated vars

;;; this is broken
(defun aref-index-on-stack (a b c d   e f g h   i j k l   m n o p   q r)
  (li:svref a r))

;;; how about this?
;;; we need values-list i think
(ndefmacro multiple-value-prog1 (form1 &body body)
  (let ((var (gensym 'values)))
    `(LET ((,var (LI:MULTIPLE-VALUE-LIST ,form1)))
       ,@body
       (VALUES-LIST ,var))))

;;; broken
(defun foo (x)
  (multiple-value-prog1 (values 3 x)
                        (finish)))

;; doesn't work
(defun bar (x)
  (foo (values 3 x)))

;; this warning is ok
(defun bar ()
  ((lambda (x) (+ x x)) 3 4))


;;; this generates a lot of calls to cons-rest
;;; (it works but is non optimal)
;;; it may also be neccessary if stack slots are
;;; used  (we could alloc before cons-rest
;;; if cons rest took a stack-offset arg or some such)
(defun foo (&optional (a 3 a-p) b &rest x)
  (foo a b x))
