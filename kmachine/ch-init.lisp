;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

(prims:define-global-frame call-hardware-loader)
(prims:define-global-variable call-hardware-loader *call-stack-pointer-base*)
(prims:define-global-variable call-hardware-loader *memory-pointer*)


(defmacro ch-free-frame (frame)
  "Return FRAME to heap, trashes return frame"
  `(progn
     (hw:write-open-active-return               ;Write Free into RETURN
        (hw:dpb ,frame hw:%%ch-oar-return (hw:read-open-active-return)))
     (hw:ch-tcall)))

(defun ch-init ()
  "Initialize Call Hardware and build heap"
  (hw:write-open-active-return #xFFFFFF)
  (hw:nop)
  (let ((free #xfe))
    (hw:write-call-sp-hp #xF0FF)                ;Empty the heap and stack.
    (dotimes (i 238.)
      (ch-free-frame free)
      (decf free)))
  (hw:write-open-active-return #xFFFF10)
  (setq *call-stack-pointer-base* 0)
  ;; some sort of open call to get scroller loaded in bottom frame.
  )

CH-INIT_6
   (MOVEI OPEN-ACTIVE-RETURN (QUOTE 16777215))
   (NOP)
   (MOVEI A2 (QUOTE 254))
   (MOVEI CALL-SP-HP (QUOTE 61695))
   (MOVEI A1 (QUOTE 0))
DO1032_18
   (ALUI-16 L-R NOP-NO-OVERFLOW-TRAP A1 (QUOTE 238) BW-24)
   (TEST BR-NOT-GREATER-OR-EQUAL)
   (BRANCH C_22 NIL)
C_21
B_55
   (MOVEI OPEN-ACTIVE-RETURN (QUOTE 16776976))
   (MOVEI A0 (QUOTE 0))
   (RETURN A0)
C_22
   (MOVE A0 OPEN-ACTIVE-RETURN)
   (ALU-FIELD FIELD-PASS OPEN-ACTIVE-RETURN A2 A0 (QUOTE 8) PW-II)
   (TAIL-CALL (QUOTE 0) (QUOTE 0) R0 NIL NEXT-PC-PC+1)
   (ALUI-16 L-R A2 A2 (QUOTE 1) BW-24)
   (UNCONDITIONAL-BRANCH DO1032_18 (ALU R+1 A1 A0 A1 BW-32))


(defun ch-unload ()
  "This routine takes one active off the call stack,
and reinstalls a catcher"
  (setq *call-stack-pointer-base*
        (logand 255. (1+ *call-stack-pointer-base*)))
  (ch-unload-one-active)
  (setq *call-stack-pointer-base*
        (logand 255. (1- *call-stack-pointer-base*)))
  (ch-put-cs *call-stack-pointer-base*
             #xFF #xFF 'the-stack-catcher 0))

;;; Well, this is crufty, but it works.
CH-UNLOAD_1
   (ALU R+1 A1 A0 (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) BW-24)
   (ALUI-16 ALU-AND A1 A1 (QUOTE 255) BW-24)
   (MOVE (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) A1)
   (OPEN-CALL CH-UNLOAD-ONE-ACTIVE (QUOTE 0) IGNORE NIL)
B_19
   (ALU R-1 A0 A0 (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) BW-24)
   (ALUI-16 ALU-AND A0 A0 (QUOTE 255) BW-24)
   (MOVE (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) A0)
   (MOVE O0 (%REGISTER *CALL-STACK-POINTER-BASE* 1 0) CH-TAIL-OPEN)
   (MOVEI O1 (QUOTE 255))
   (MOVEI O2 (QUOTE 255))
   (MOVEI O3 (QUOTE THE-STACK-CATCHER))
   (MOVEI O4 (QUOTE 0))
   (TAIL-CALL CH-PUT-CS (QUOTE 5))
