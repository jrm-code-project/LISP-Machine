;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:ZL -*-

;;; Trivially modified version of TWO-WINDOWS: takes two windows
;;; (structures, not sheets) and makes them share the area
;;; originally occupied by the first of the two, except that
;;; this version makes two side-by-side VERTICAL windows
;;; rather than one on top of the other.  Very nice for text
;;; editing (with narrow fill columns).

(DEFUN TWO-WINDOWS (ZWEI-WINDOW-1 ZWEI-WINDOW-2)
  (REDISPLAY ZWEI-WINDOW-1 ':NONE)
  (LET ((W1 (WINDOW-SHEET ZWEI-WINDOW-1))
        (W2 (WINDOW-SHEET ZWEI-WINDOW-2))
        (FRAME (WINDOW-FRAME ZWEI-WINDOW-1)))
    (MULTIPLE-VALUE-BIND (LEFT TOP RIGHT BOTTOM)
        (SEND FRAME ':INSIDE-EDGES-WITHOUT-MODE-LINE-WINDOW)
      (TV:PRESERVE-SUBSTITUTE-STATUS (SEND W1 ':SUPERIOR)
        (TV:DELAYING-SCREEN-MANAGEMENT
          (SEND W1 ':DEEXPOSE)
          (SEND W2 ':DEEXPOSE)
          (LET ((width (TRUNCATE (- right left) 2)))
            (SEND W1 ':SET-EDGES LEFT TOP (+ width left) bottom)
            (SEND W2 ':SET-EDGES (+ width left) TOP RIGHT BOTTOM)
          (SEND W1 ':SET-LABEL NIL)
          (SEND W2 ':SET-LABEL NIL)
          (SEND W1 ':EXPOSE NIL ':CLEAN)                ;Make sure they are both there
          (SEND W2 ':EXPOSE NIL ':CLEAN)))))
    (SEND FRAME ':UPDATE-LABELS)))
