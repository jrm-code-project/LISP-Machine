



  where to keep the env?

   if setqs make env at start

     make-contour
     copy-to-new-contour (setqed vars)


     open
      make-contour -> O0
      offset-1 -> o1
      value-1  -> o2
      move env o0
      call closure-set-0


   if making closure

     make-closure
     copy-to-new-contour (closed-vars)


(defun foo (x)
  (let ((y x))
    (bar #'(lambda () (setq y 3)))
    (print x)))

here it would be nice to cons [x a] when a is bound

(defun flatten-contours ()
 (let ((a 42))
   (let ((x (xxx)))
     (bar #'(lambda () (setq a (1+ a)) x)))))

here we can't

(defun noflatten ()
  (let ((a 42))
    (labels ((foo (x)                           ; env: ((nil t a))
             (when (< x 10)
               (bar #'(lambda () (setq a (1+ a)) x)     ; env: ((x) (nil t a))
                    x)
               (foo x))))
      (foo 0))))

how do we tell the difference?

lambda is label not open...

(defun noflatten ()
  (let ((a 42))
    (dotimes (i 10)
      (let ((ii i))
        (bar #'(lambda () (setq a (+ a ii))))))))


(defun foo (uuu vvv)
  (let ((a (aaa)) (b (bbb)) (c (ccc)))
    (dotimes (i 10)
      (let ((ii i))
        (bar #'(lambda (n) (setq a (1+ a)) (fff uuu vvv n ii b c)))))))

the lambda creates a length 6 lexical-frame for each closure ...

lexical-map: (0 1 -16777216 -16777215 -16777214 -16777212)
 (mapcar #'(lambda (n) (ldb (byte 10 0) n)) *): (0 1 0 1 2 4)
local-map: (COMPILER::LOCAL-MAP ((A) (B) (C) (I) (II)))

(defun fef-lexical-map (fef)
  (si:%p-contents-offset fef (- (si:%p-ldb si:%%fefh-pc-in-words fef) 2)))

;-----------

env in a15 for closures

tell reg alloc

funcall:
  dispatch type
   ...
   closure:
      when nargs > 15
        push a15 ??? (any canonical, nestable place)
      move env to a15
      ...



a
 b
  c
 label
   x
    y
     #'(lambda       env: [[x y] [a b c]]
                  or env: [[a b c x y]]



a
 b -setqed
  c
 label
   x
    y
     #'(lambda     env: [[x y] [a b c]]
                or env: [[a c x y] [b]]


----------------------------------------------------------------------

This is broken:

(defun 2close (x y)
  (bar #'(lambda () (foo x y))
       #'(lambda () (baz x y))))



in the analysis of the first closure, x and y are allocated to a contour,
in the analysis of the second, they are already allocated, and so the
contour number is not incremented.  If a variable can be in different
contours for different closures (have more than one home) then keeping
the location of the variable-closed property will not work.

