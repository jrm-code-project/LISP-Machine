


(defun test-tak ()
  (tak 18 12 6)
  nil)

(TEST-TAK_1
  (alu r+1 (%register g0 0 0) (%register g0 0 0) (%register g0 0 0))
  (alu-field field-pass (%register g1 0 1) (%register g0 0 0) (%register g1 0 1) (byte 3 -13))
  (alu-field field-not memory-control (%register g1 0 1) memory-control hw:%%memory-control-leds)
   (MOVEI O0 (QUOTE 18) CH-OPEN)
   (MOVEI O1 (QUOTE 12))
   (MOVEI O2 (QUOTE 6))
   (KCALL TAK (QUOTE 3) r0 NIL)
B_5
  (jump 100 ()))


(270321532394352640 270323731451553379 270431485235757219 1135188581409619986 1134909305456164876 1134911504479420422 631212033320157184 2936492092580429924)
NIL
((6 . SIMULATOR::TAK))
NIL
