; -*-mode:lisp; package:steve; readtable:cl; -*-

;Copyright (c) July 1983 by Christopher Eliot
; and Massachusetts Institute of Technology.
;Permission to copy all or part of this material is granted, provided
; that the copies are not made or distributed for resale, the MIT
; copyright notice and reference to the source file and the software
; distribution version appear, and that notice is given that copying
; is by permission of Massachusetts Institute of Technology.

(defparameter *fundamental-bindings*
        (loop with table = (make-binding-table)
              for i from 0 below 256
              for char = (int-char i)
              if (and (graphic-charp char)
                      (not (lowercasep char)))
              do (send table :define-binding char 'self-insert)
              finally (return table)))

(defparameter *editor-bindings* *fundamental-bindings*)


(defparameter *dired-bindings* (make-binding-table))
