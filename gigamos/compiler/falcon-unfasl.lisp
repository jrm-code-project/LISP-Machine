;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-
;;; These are the additions to the lambda unfasler needed to
;;; unfasl FALCON code.

;;;****************************************************************
;;;
;;; unfasl ops for K-Compiled-Functions
;;;
;;;****************************************************************

(defun unfasl-make-vector (size)
  (make-array size))

(defun unfasl-op-k-compiled-function ()
  (let ((length   (ash fasl-group-length -2))
        (function (nc::make-ncompiled-function)))

    (setf (nc:ncompiled-function-code function)
          (unfasl-k-function-instructions length))

    (let ((name         (unfasl-next-value))
          (local-refs   (unfasl-next-value))
          (refs         (unfasl-next-value))
          (entry-points (unfasl-next-value)))
      (setf (nc::ncompiled-function-name         function) name)
      (setf (nc::ncompiled-function-length       function) length)
      (setf (nc::ncompiled-function-local-refs   function) local-refs)
      (setf (nc::ncompiled-function-refs         function) refs)
      (setf (nc::ncompiled-function-entry-points function) entry-points))

    (setf (nc:ncompiled-function-immediates function)
          (unfasl-k-function-immediates))
    (setf (nc:ncompiled-function-load-time-evals function)
          (unfasl-k-function-load-time-evals))

    ;; Now we've got it all hooked up, let's put it into the UNFASL table.
    (enter-unfasl-table function)


    (let ((*unfasl-indentation* (+ *unfasl-indentation*  2)))
      (unfasl-terpri)

      ;; FASL-OP-STOREIN-FUNCTION-CELL
      (unfasl-group)

      (when *unfasl-print*
        (unfasl-terpri)
        (unfasl-terpri)
        (format t "Disassembled Code for ~s" (nc::ncompiled-function-name function))
        (unfasl-terpri)
        (dolist (inst (nc::ncompiled-function-code function))
          (unfasl-terpri)
          (format t "~a" (nc:dis inst)))
        (unfasl-terpri)
        (unfasl-terpri)))
    ))


(defun unfasl-read-k-instruction ()
  (let ((1st (unfasl-next-nibble))
        (2nd (unfasl-next-nibble))
        (3rd (unfasl-next-nibble))
        (4th (unfasl-next-nibble)))
    (dpb 4th (byte 16. 48.)
         (dpb 3rd (byte 16. 32.)
              (dpb 2nd (byte 16. 16.)
                   (dpb 1st (byte 16. 0.) 0))))))

(defun unfasl-k-function-instructions (length)
  (let ((code '()))
    (dotimes (i length)
      (push (unfasl-read-k-instruction) code))
    (nreverse code)))


(defun unfasl-op-k-local-refs ()
  (let ((locals (unfasl-next-value)))
    (when *unfasl-print* (format t "    number of local-refs"))
    (do ((i 0 (+ i 2))
         (locs (unfasl-make-vector (* 2 locals))))
        ((>= i (* 2 locals))
         (enter-unfasl-table locs))
      (setf (aref locs i) (unfasl-next-value))
      (when *unfasl-print* (format t "      ref offset"))
      (setf (aref locs (1+ i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      target offset")))))

(defun unfasl-op-k-refs ()
  (let ((k-refs (unfasl-next-value)))
    (when *unfasl-print* (format t "    number of refs"))
    (do ((i 0 (+ i 3))
         (refs (unfasl-make-vector (* 3 k-refs))))
        ((>= i (* 3 k-refs))
         (enter-unfasl-table refs))
      (setf (aref refs i) (unfasl-next-value))
      (when *unfasl-print* (format t "      ref offset"))
      (setf (aref refs (1+ i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      referenced function name"))
      (setf (aref refs (+ 2 i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      number of args")))))

(defun unfasl-op-k-entry-points ()
  (let ((entries (unfasl-next-value)))
    (when *unfasl-print* (format t "    number of entry points"))
    (do ((i 0 (+ i 2))
         (ents (unfasl-make-vector (* 2 entries))))
        ((>= i (* 2 entries))
         (enter-unfasl-table ents))
      (setf (aref ents i) (unfasl-next-value))
      (when *unfasl-print* (format t "      number of args"))
      (setf (aref ents (1+ i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      entry offset")))))

(defun unfasl-k-function-immediates ()
  (let ((immeds (unfasl-next-value)))
    (when *unfasl-print* (format t "    number of immediates"))
    (do ((i 0 (+ i 2))
         (imms (unfasl-make-vector (* 2 immeds))))
        ((>= i (* 2 immeds))
         imms)
      (setf (aref imms i) (unfasl-next-value))
      (when *unfasl-print* (format t "      ref offset"))
      (setf (aref imms (1+ i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      immediate object")))))

(defun unfasl-k-function-load-time-evals ()
  (let ((number-of-evals (unfasl-next-value)))
    (when *unfasl-print* (format t "    number of load-time evals"))
    (do ((i 0 (+ i 2))
         (evals (unfasl-make-vector (* 2 number-of-evals))))
        ((>= i (* 2 number-of-evals))
         evals)
      (setf (aref evals i) (unfasl-next-value))
      (when *unfasl-print* (format t "      ref offset"))
      (setf (aref evals (1+ i)) (unfasl-next-value))
      (when *unfasl-print* (format t "      form to eval")))))
