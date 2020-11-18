;;; -*- Mode:LISP; Package:OBIE; Readtable:CL; Base:10 -*-

;;; Ports
(defobclass port (window-point)
  owner                                                ;this variable not used?
  (lines nil))

(defobfun (draw port) ()
  (send window :draw-filled-in-circle x y 2 tv:alu-xor))

(defobfun (add-line port) (l)
  (push l lines))

(defobfun (remove-line port) (l)
  (pull l lines))

(defobfun (draw-lines port) ()
  (mapc-ask lines (draw)))

(defobfun (move port) (x y)
  (draw)
  (draw-lines)
  (shadowed-move x y)
  (draw-lines)
  (draw))

(defobfun (disappear port) ()
  (mapc-ask lines (disappear))
  (shadowed-disappear))

(defobclass line (window-object)
  port0
  port1)

(defobfun (exist line) (&rest stuff)
  (apply 'shadowed-exist stuff)
  (when port0
    (ask-funcall port0 'add-line obj:*object))
  (when port1
    (ask-funcall port1 'add-line obj:*object)))

(defobfun (disappear line) ()
  (shadowed-disappear)
  (when port0 (ask-funcall port0 'remove-line obj:*object))
  (when port1 (ask-funcall port1 'remove-line obj:*object)))

(defobfun (draw line) ()
  (send window :draw-line (ask port0 x) (ask port0 y) (ask port1 x) (ask port1 y) tv:alu-xor))

(defobfun (other-port line) (pa)
  (if (eq pa port0) port1 port0))
