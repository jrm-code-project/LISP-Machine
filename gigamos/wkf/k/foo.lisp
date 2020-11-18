;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:ZL -*-

(defun foo (a b)
 (max a b))

;(progn (send terminal-io :clear-screen) (do-forever (send terminal-io :set-cursorpos 0 0) (march-print-parameter-block)))

;(run-march 'mtest1 'mtest2)

;(defun foo ()
;  (hw:write-microsecond-clock (hw:unboxed-constant 0))
;  (li:break "Fact complete." (user:fact 100) (hw:read-microsecond-clock))
;  (loop))
