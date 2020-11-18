;;; -*- Mode:LISP; Package:NC; Readtable:CL; Base:10 -*-




lights
;;; Count in processor lights
  (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
  (alu-field field-pass (%register g1 0 1) (%register g0 0 0) (%register g1 0 1) (byte 3 -17))
  (alu-field field-not memory-control (%register g1 0 1) memory-control hw:%%memory-control-leds)
  (nop)
  (jump 100 ())



(270321532394352640 270323731451553251 270431485235765411 270361114267619328 2936492092580429924)

03C0600061813000
03C06200638725E3
03C0C400C58F20A3
03C0840041001000
28C0840000000064






(defconstant count-byte (byte 3 17))

(define-global-frame test)
(define-global-variable test g0)
(define-global-variable test g1)

(defun lights ()
  (do () (())
    (setq g0 (1+ g0))
    (setq g1 (hw:ldb count-byte g0))
    (hw:write-memory-control (hw:dpb g1 hw:%%memory-control-leds (hw:read-memory-control)))
    (hw:nop)))


LIGHTS_3
DO4404_9
   (ALU R+1 IGNORE A0 (%REGISTER G0 0 0) BW-24)
   (MOVE (%REGISTER G0 0 0) IGNORE)
   (ALU-FIELD FIELD-PASS IGNORE (%REGISTER G0 0 0) R0 (BYTE 3 -17) PW-II)
   (MOVE (%REGISTER G1 0 1) IGNORE)
   (MOVE A0 READ-MEMORY-CONTROL)
   (ALU-FIELD FIELD-PASS WRITE-MEMORY-CONTROL (%REGISTER G1 0 1) A0 (QUOTE 323) PW-II)
   (NOP)
   (UNCONDITIONAL-BRANCH DO4404_9 NIL)
