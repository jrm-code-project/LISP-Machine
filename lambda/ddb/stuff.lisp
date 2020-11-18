
;;;-*- mode: LISP; Base: 10.; Package: user; Readtable: CL -*-


; FUSE returns list joined from two argument lists.

(defun fuse (x y)                        ;* x is before y, but reversed.
  (if (null x) y
    (fuse (cdr x) (cons (car x) y))))


; DRAC counts elements in arg list (identical to LENGTH).

(defun drac (x)
  (if (null x) 0
    (+ 1 (drac (cdr x)))))


; JOIN returns list fused from two argument lists.

(defun join (x y)                        ;* identical to APPEND.
  (if (null x) y
    (join (reverse (cdr (reverse x))) (cons (car (last x)) y))))


; MEND is another version of (join).

(defun mend (x y)
  (if (null x)
      y
    (progn
      (setq z (drac x))
      (mend (subseq x 0 (- z 1)) (cons (elt x (- z 1)) y)))))


; SIGN is arithmatic sign of argument.

(defun sign (x)
  (cond ((= x 0) 0)
        ((< x 0) -1)
        ((> x 0) 1)))


#|

(setq enharm '
((("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B")
  ("B#" "B##" "C##" "D#" "D##" "E#" "E##" "F##" "G#" "G##" "A#" "A##"))
 (("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B")
  ("Dbb" "Db" "Ebb" "Fbb" "Fb" "Gbb" "Gb" "Abb" "Ab" "Bbb" "Cbb" "Cb"))))

|#


zwei:
(DEFCOM COM-MACRO-EXPAND-EXPRESSION "Print macroexpansion of next s-expression.
The result is printed on the screen with GRIND-TOP-LEVEL."
  ()
  (LET ((STREAM (REST-OF-INTERVAL-STREAM (POINT))))
    (LET ((FORM (READ-OR-BARF STREAM)))
      (GRIND-TOP-LEVEL (if *numeric-arg-p*
                           (macroexpand-1 form)
                         (MACROEXPAND FORM)))))
  DIS-NONE)

(defmacro while (maintained-condition &rest statement-body)
          `(loop
             (if (not ,maintained-condition
                      (return)))
             ,@statement-body))

(loop until (= x 3) (loop (foo)))
