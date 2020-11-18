;;; -*- Mode:LISP; Package:TV; Base:10 -*-


(DEFMETHOD (BOX-LABEL-MIXIN :AFTER :DRAW-LABEL) (SPEC LEFT TOP RIGHT BOTTOM)
  SPEC
  (AND LABEL-BOX-P
       (SHEET-FORCE-ACCESS (SELF)
         (PREPARE-SHEET (SELF)

;          All these seem to do is make the label-bottom stick out through any
;          borders the screen might have, by forcing use of the outside rather
;          than the inside diameter. Compare TV:ERASE-LABEL, which respects
;          borders correctly yet doesn't seem to make anybody malfunction. JCM.
;          (IF (>= RIGHT (SHEET-INSIDE-RIGHT)) (SETQ RIGHT WIDTH))
;          (IF (<= LEFT (SHEET-INSIDE-LEFT)) (SETQ LEFT 0))

           (OR (>= BOTTOM (SHEET-INSIDE-BOTTOM))
               (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- BOTTOM) CHAR-ALUF SELF))
           (OR (<= TOP (SHEET-INSIDE-TOP))
               (%DRAW-RECTANGLE (- RIGHT LEFT) 1 LEFT (1- TOP) CHAR-ALUF SELF))))))


;(DEFUN ERASE-LABEL (&REST IGNORE)
;  (DECLARE (:SELF-FLAVOR ESSENTIAL-LABEL-MIXIN))
;  (AND LABEL
;       (SHEET-FORCE-ACCESS (SELF)
;        (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
;            (COMPUTE-LABEL-POSITION)
;          (PREPARE-SHEET (SELF)
;            (%DRAW-RECTANGLE (- RIGHT LEFT) (- BOTTOM TOP) LEFT TOP ERASE-ALUF SELF))))))


(DEFMETHOD (ESSENTIAL-WINDOW :BEFORE :MOUSE-SELECT) (&REST IGNORE)
;; Snarfing input from the selected window before the new one is selected made
;; characters typed after a new mouse selection go to the old window.  KHS 1/7/85.
;  (WITHOUT-INTERRUPTS
;    (AND SELECTED-WINDOW
;        (SETQ BUF (SEND SELECTED-WINDOW ':IO-BUFFER))
;        (KBD-SNARF-INPUT BUF))))
;  (zwei:event "ENTER :BEFORE :MOUSE-SELECT")
  (UNLESS (NULL SELECTED-WINDOW)
    (SEND SELECTED-WINDOW :IO-BUFFER))
;  (zwei:event "EXIT :BEFORE :MOUSE-SELECT")
  )


(defmethod (TV:ESSENTIAL-WINDOW :AFTER :MOUSE-SELECT) (&rest ignore)
;  (zwei:event "ENTER :AFTER :MOUSE-SELECT")
  (cond ((and (typep self 'zwei:zmacs-frame)
              (typep (send self :selection-substitute) 'zwei:zmacs-window-pane)
              (boundp 'zwei:*dired-constraint-frame*)
              (typep (send self :superior) 'zwei:dired-display-frame))
         (zwei:mouse-select-dired self))
        ((and (boundp 'zwei:*dired-constraint-frame*)      ;;; Prevents action on mini-buffer selection
              (typep self 'zwei:dired-constraint-frame)))
        ((typep self 'zwei:gateway-constraint-frame)
         (zwei:mouse-select-gateway self))
        (t (zwei:mouse-select-standard self)))
;    (zwei:event "EXIT :AFTER MOUSE-SELECT")
    )


;;; *DIRED-DEBUG-PANE* won't instantiate unless this exists. It seems to have
;;; gotten lost out of the window system somehow.
(defmethod (tv:scroll-stuff-on-off-mixin :adjustable-size-p) (&rest ignore))
