;;; -*- Mode:LISP; Package:FOO; Base:10; Readtable:ZL -*-



(defflavor foo
         ((a 'a) (b 'b))
         ())

(defmethod (foo :bar) ()
  (list a b))

(defmacro frob (a b)
  `'(,a ,b))

(DEFVAR *X* NIL)

(DEFVAR *G* 'G)

(defun f-test ()
  (let ((*x* 'loser))
    (list 'using 'g 'is 'a
          (let ((*g* 'g))
            (f 'winner))
          'using 'g2 'is 'a
          (let ((*g* 'g2))
            (f 'winner)))))


(DEFUN F (X)
  (LIST (FUNCALL *G* X) (FUNCALL *G* X)))

(DEFUN G (*X*)
  (H))

(DEFUN G2 (Y)
  (LET ((*X* Y))
    (H)))

(DEFUN H ()
  (LIST *X*))


(defun c (*x*)
  (closure '(*x*) 'h))

(defun ctest (x)
  (funcall (c x)))



(DEFUN loser ()
  (si:re-indexify-symbol-cells '*x*)
  (si:de-sym '*x*)
  ;; the bind-nil instruction loses.
  (prog1 (LET ((x 4)
               (*x*))
           (prog1 (list x *x*)
                  (si:de-sym '*x*)))
         (si:de-sym '*x*)))


(DEFUN loser-2 ()
  (si:re-indexify-symbol-cells '*x*)
  (si:de-sym '*x*)
  (LET ((x 4)
        (*x* (a-func)))
    (prog1 (list x *x*)
           (si:de-sym '*x*)))
  (si:de-sym '*x*))


(defun loser-3 ()
  (si:re-indexify-symbol-cells '*x*)
  (si:de-sym '*x*)
  (a-bind-x 'yow)
  (si:de-sym '*x*))


(defun a-func ()
  nil)


(defun a-bind-x (*x*)
  (si:de-sym '*x*)
  (h))
