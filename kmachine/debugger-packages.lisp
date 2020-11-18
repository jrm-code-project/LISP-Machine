;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

;;; Contains the cold load builder
(defpackage COLD
  :use '(lisp))

;;; Contains the debuggers
(defpackage KBUG
  :use '(;kbug2-common
         lisp lam)
  (:import-from "ZETALISP" "DEFSUBST"))

;(defpackage setf
;  :use '(lisp))

;(defpackage fasdump
;  :use '(;kbug2-common
;        lisp))
