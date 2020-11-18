;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

(defprop com-mouse-copy-thing "Copy from mouse to point" :mouse-short-documentation)
(defcom com-mouse-copy-thing "Insert a copy of what's under the mouse at point"
        (km)
  (with-bp (insertion-point (point) :normal)
    (multiple-value-bind (char-under-mouse nil nil line pos)
        (mouse-char *window* nil)
      (cond ((not char-under-mouse)
             dis-none)
            (t
             (move-bp (point) line pos)
             (funcall (or (call-editing-type-function *major-mode* 'mark-thing-function nil)
                          #'default-mark-thing)
                      (point) (mark) char-under-mouse line pos)
             (kill-ring-save-interval (point) (mark))
             ;; move back to the insertion point, and yank.
             (move-bp (point) insertion-point)
             (com-yank)
             dis-text)))))


(defcom com-describe-symbol-at-point
         "Prints information about symbol at or before cursor."
  ()
  (let* ((bp1 (forward-atom (forward-char (point) 1 t) -1 t))
         (bp2 (forward-atom bp1)))
    (unless bp2 (barf))
    (multiple-value-bind (symbol error)
        (catch-error (with-input-from-string (s (bp-line bp1)
                                                :start (bp-index bp1)
                                                :end (bp-index bp2))
                       (read s)))
      (unless (and (not error) (symbolp symbol)) (barf))
      (hl:printing-package-names
        (format t "~&~S ~:[has no value~; has value: ~S~]"
                symbol (boundp symbol) (ignore-errors (symbol-value symbol)))
        (fresh-line)
        (format t "~:[and has no definition.~; and has definition:~%~S~]"
                (fboundp symbol) (ignore-errors (symbol-function symbol))))))
  dis-none)




;;;; Edit history for HUNLA:L;ZWEI-MISC.LISP.1
;;;
;;; [10/27/88 03:30 CStacy] Function COM-MOUSE-COPY-THING: for quickly pointing-and-copying.
;;; [11/07/88 03:05 CStacy] Function COM-DESCRIBE-SYMBOL-AT-POINT.
