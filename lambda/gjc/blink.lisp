;;; -*- Mode:LISP; Package:USER; Patch-File:T; Base:10 -*-

;;; Reason:
;;;
; From file DJ: L.WINDOW; SHEET.LISP#573 at 6-Oct-85 13:57:47
#8R TV#:
(COMPILER-LET ((*PACKAGE* (PKG-FIND-PACKAGE "TV")))
  (COMPILER::PATCH-SOURCE-FILE "SYS: WINDOW; SHEET  "

(DEFUN BLINKER-CLOCK-INTERNAL (SHEET)
  (DECLARE (SPECIAL *BLINKER-DELTA-TIME*))
  (WHEN (AND (SHEET-EXPOSED-P SHEET)
             (ZEROP (SHEET-DONT-BLINK-BLINKERS-FLAG SHEET)))
    (DOLIST (BLINKER (SHEET-BLINKER-LIST SHEET))
      (LET ((NEW-TIME (MAX 0 (- (OR (BLINKER-TIME-UNTIL-BLINK BLINKER) 0)
                                *BLINKER-DELTA-TIME*))))
        (SETF (BLINKER-TIME-UNTIL-BLINK BLINKER) NEW-TIME)
        (AND (ZEROP NEW-TIME)
             (CASE (BLINKER-VISIBILITY BLINKER)
               ((NIL :OFF)
                (BLINKER-PHASE BLINKER))
               ((T :ON)
                (NULL (BLINKER-PHASE BLINKER)))
               (:BLINK
                T))
             (NOT (SHEET-OUTPUT-HELD-P SHEET))
             (LET ((LV (SHEET-LOCK SHEET)))
               (OR (NULL LV) (CONSP LV)))
              (CATCH-ERROR (BLINK BLINKER) NIL); -- removed Mly 18-Jul-85
             ;   -- the debugger should be enough to hack this
             ;(BLINK BLINKER)
             )))
    (AND (EQ SHEET MOUSE-SHEET)
         (= MOUSE-CURSOR-STATE 1)
         (= MOUSE-CURSOR-CLOSED-STATE 2)
         (NEQ WINDOW-OWNING-MOUSE 'STOP)
         (NOT (SHEET-OUTPUT-HELD-P SHEET))
         ( (OR (BLINKER-TIME-UNTIL-BLINK MOUSE-BLINKER) 0) 0)
         (LET ((LV (SHEET-LOCK SHEET)))
           (OR (NULL LV) (CONSP LV)))
         (SETQ MOUSE-CURSOR-STATE MOUSE-CURSOR-CLOSED-STATE
               PREPARED-SHEET NIL))
    (DOLIST (S (SHEET-EXPOSED-INFERIORS SHEET))
      (BLINKER-CLOCK-INTERNAL S))))

))
