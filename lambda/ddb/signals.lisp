; Signals.Lisp
; Assemble List of proc-mem signals and their schematic page references
; See file "signals.help" for usage.


(defvar *signal-labels* ())


(defun find-position-in-list-equal (item-name list-name)
  (do ((i 0 (1+ i)))
      ((= i (length list-name)))
    (if (equal (elt list-name i) item-name)
        (return-from find-position-in-list-equal i)))
  nil)


(defun build-signal-list ()
  (let ((label-name ())
        (label-position 0)
        (page-number 0)
        (page-number-list ())
        )
    (viewf "signals.help")
    (format t "~%~%Ready...~%~%")
    (do-forever
      (cond
        ((null (setq label-name (read)))              ; no more, go print lists.
         (return))
        ((null (setq label-position (find-position-in-list-equal label-name *signal-labels*)))
         (setq label-position 0
               *signal-labels* (append (list label-name () ) *signal-labels*)
               )))
      (setq page-number-list (elt *signal-labels* (1+ label-position)))
      (do ((i 0 (1+ i)))
          ((= i (length page-number-list)))
        (format t "~D " (elt page-number-list i)))
      (loop while (not (zerop (setq page-number (read))))
        (cond
          ((minusp page-number)
           (setq page-number (* -1 page-number))
           (if (not (member page-number page-number-list))
               (princ "? ")
             (setq page-number-list (remove page-number page-number-list)))
           (setf (elt *signal-labels* (1+ label-position)) page-number-list)
           (when (null page-number-list)
             (setq *signal-labels* (remove () *signal-labels*))
             (setq *signal-labels* (remove label-name *signal-labels*))
             (format t " ~A deleted..." label-name)
             (return)))
          (t
           (setq page-number-list (cons page-number page-number-list))
           (setf (elt *signal-labels* (1+ label-position)) page-number-list))))
      (format t "~%"))
    (do ((i 0 (1+ i)))
        ((= i (length *signal-labels*)))
      (print (elt *signal-labels* i)))
    ))
