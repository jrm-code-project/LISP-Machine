;;; -*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-


(defun foo (x y)
  (bar x y))


(defun foo (x y)
  (bar x)
  (baz y))

(defun foo (x)
  (bar (baz x)))

(defun foo (x)
  (bar x))

(defun bar (n)
  (1+ n))

(defun baz (y)
  (+ y y))








(defun dotriv ()
  (do ((a 0 a))
      (())))

(defun dobody ()
  (do ((a 0 a))
      (())
    (print a)))

(defun docont ()
  (do ((a 0 a))
      (())
    (print a))
  (print 'done))

(defun swap ()
  (do ((a 0 b)
       (b 1 a))
      (())
    (foo a b)))




;;; real-fix-exit-reference
;;; check-continuation-var
;;; fixup-call-node
;;; fixup-node-tree
#||
If there is a var bound to a lambda
 and the var is called (call-proc)
 then any exit-arg refs get turned into
 ((c <vals>) (x <vals)) why?
||#

(defun dostepcont ()
  (do ((i 0 (1+ i)))
      ((minusp i))
    (print i))
  (print 'done))

(defun dovaluecont ()
  (bar (do ((i 0 (1+ i)))
           ((minusp i))
         (print i)))
  (print 'done))

(defun abs (n)
  (if (minusp n)
      (- n) n))


(defun foo ()
  (do ((a 0 a))
      (()))
  (print 'done))


(defun foo ()
  (do ((i 0 (1+ i)))
      (())
    (print i)))

(defun foo (list)
  (do ((l list (cdr l)))
      ((null l))
    (print l))
  (print list))

(defun foo (x)
  (if x
      (do ((a 0 (1+ a)))
          (())
        (print a)))
  (print 'foo)
  (print 'bar))


(defun foo (a b) (setq a 3 b 4))


(defun foo ()
  (do* ((a 0 (1+ a))
        (b (+ a 1) (+ a 1)))
       (())
    (bar a b)))

(defun foo ()
  (labels ((bar (n) (print (1+ n))))
    (print 'start)
    (bar 3)
    (print 'middle)
    (bar 4)
    (print 'done)))

(defun foo ()
  (do ((a 0 a))
      ((= a 10))
    (print a))
  (print 'done))

(defun list-length (list)
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (x list (cdr x)))
      (())
    (when (atom y) (return n))
    (when (atom (cdr y)) (return (1+ n)))
    (when (and (eq x y) (plusp n)) (return nil))))


(defun nth (n list)
  (if (non-negative-fixnump n)
      (do ((l list (cdr l))
           (i n (1- i)))
          ((zerop i)
           (car l))
        (when (null l)
          (return nil)))
    (error "~S is not a valid index into a list: should be non-negative integer" n)
    ))



(defun 2let ()
  (let ((x (xxx))
        (y (yyy)))
    (foo y x)))


(defun foo (x l)
  (setq x (+ x (car l))))


(defun abs (x)
  (if (plusp x)
      x
    (- x)))

(defmacro access (x)
  `(car ,
     x))

(defun foo (l)
  (if (eq (access l) 'foo)
      (print 'yow)))





(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

(defun iftest (x)
  (when (not (zerop x))
    (print 'not)))


;;; variable substitution test

(defun subst-var (x)
  (let ((y x))
    (bar y)
    (baz y))
  (foo x)))

(defun no-subst-for-setqed-var (x)
  (let ((y x))
    (bar y)
    (setq y 3)
    (bar y)))

(defun no-subst-setqed-var (x)
  (let ((y x))
    (bar y)
    (setq x 3)
    (bar y)))

(defun no-subst-setqed-var (x)
  (labels ((foo ()
             (setq x 3)))
    (let ((y x))
      (bar y)
      (foo)
      (bar y))))


;;; this doesn't work, but it would be nice if it did
(defun subst-setqed-ok (x)
  (let ((y x))
    (bar y)
    (baz y))
  (setq x 3))

(prims:defsubst sss (i v)
  (ttt i v))

(defun foo ()
  (let ((i 0))
    (let ((t10 i))
      (let ((val (vvv i)))
        (sss t10 val)))
    (setq i 10)))

(defun copy (a b)
  (dotimes (i 100)
    (setf:setf (svref b i) (svref a i))))

;;; substitute join test

;;; this was substituting when it shouldn't
;;; parameterize was losing f in the body of the let
;;; but the code is not optimal now because it isn't
;;; substituting
(defun foo (p)
  (let* ((f (if p (get-f) nil))
         (s (if f (xxx f))))
    (foo f)))

(defun foo (p)
  (if (if p (foo-p) nil)
      (print 'xxx)
    (print 'yyy)))

(defun foo (p x y)
  (if (if p (zerop x) (zerop y)) (xxx) (yyy)))

;;; Multiple values

(defun multiple-values (x y)
  (values y 23 x))

(defun multiple-values-i (x y)
  (values 23 x y))

(defun no-values ()
  (values))

(defun mvb ()
  (multiple-value-bind (a b)
      (bar)
    (foo a b)))

(defun mvb-unused ()
  (multiple-value-bind (a b)
      (bar)
    (foo b)))

;;; *** this doesn't work
(defun mvb-no-first ()
  (multiple-value-bind (nil b)
      (bar)
    (foo b)))

(defun mv-maybe (p)
  (multiple-value-bind (a b)
      (if p (bar) 42)
    (foo a b)))

(defun mv-closed ()
  (multiple-value-bind (x y z) (foo)
    (bar #'(lambda (n) (+ x n)) y z)))

(defun mvbind-lots ()
  (multiple-value-bind (a b c d e f g h i)
      (bar)
    (foo a b c d e f g h i)))


(ndefmacro multiple-value-prog1 (form1 &body body)
  (let ((var (gensym 'values)))
    `(LET ((,var (LI:MULTIPLE-VALUE-LIST ,form1)))
       ,@body
       (VALUES-LIST ,var))))

;;; broken
(defun foo (x)
  (multiple-value-prog1 (values 3 x)
                        (finish)))

(defun foo (x)
  (bar (values 3 x)))

;;; Return from

(defun tst-return-from ()
  (block bar (return-from bar 3)))

(defun tst-return-from ()
  (+ 5 (block bar (return-from bar 3))))

(defun tst-return-from (x)
  (+ x (block bar (return-from bar 3))))

(defun tst-return-from (x)
  (+ x (block bar
         (do ((p (bletch) (bletch)))
             ((baz) 4)
           (when p
             (return-from bar 3))))))

(defun no-values-return-from (p x y)
  (block find-size
    (do () (())
      (return-from find-size (values)))))


(defun no-values-return-from-cont (p x y)
  (block find-size
    (do () (())
      (return-from find-size (values))))
  nil)

(defun foo ()
  (multiple-value-bind (a b c)
      (block bar
        (return-from bar (values 3 4 5)))
    (list a b c)))



(defun mv-return-from ()
  (multiple-value-bind (a b c)
      (block bar
        (do () (())
          (when (phase-of-moon-ok)
            (return-from bar (values 3 4 5)))))
    (list a b c)))


(defun foo ()
  (multiple-value-bind (a b c)
      (block bar
        (if (phase-of-moon-ok)
            (return-from bar (values 3 4 5))
          (return-from bar (values 11 22 33))))
    (list a b c)))


(defun both-conditional-conts-bound (size how-many best-size)
  (when (and (>= size how-many)
             (or (null best-size)
                 (< size best-size)))
    (print 'yow))
  (print 'done))


(defun foo (x y)
  (let ((a (bar x y)))
    (cond ((= a 0)
           (zero))
          ((or (= a 1)
               (= a 2))
           (one-or-two)))
    (print 'done)))


(defun if-assign (x y)
  (labels ((bar (n)
                (print n)))
    (bar (if x
             0 1))))

(defun if-assign-call (x y)
  (labels ((bar ()
                (print (bletch x)))
           (bletch (p)
             (if p 0 1)))
    (bar)))

(defun count ()
  (dotimes (i 10)
    (print i)))

(defun countcont ()
  (dotimes (i 10)
    (print i))
  (print 'done))


(defun callret (x)
  (bar x 3 4)
  nil)


(defun foo (p)
  (if (not p)
      (print 'x))
  (print 'done))

(defun foo (p)
  (if p
      (print 'x))
  (print 'done))




(defun foo (x)
  (let ((x 3))
    (bar x)
    (setq x 4)
    (bar x)))



(defun test-branch ()
  (let ((x 0))
    (tagbody
     loop
        (if (zerop x)
            (setq x 1)
            (setq x 0))
        (go loop))))


(defun labelrest ()
  (labels ((foo (a &rest more)
             (bar a more)))
    (foo 3 4 5)))


(defun labelrest1 ()
  (labels ((foo (&rest more)
             more))
    (foo 3 4 5)))

(defun calllabel (x)
  (labels ((foo (n) (1+ n)))
    (funcall #'foo x)))

(defun letlabel ()
  (let ((f #'(lambda (n) (1+ n))))
    (funcall f 3)))

(defun letlabel3 ()
  ((lambda (n) (1+ n))
   ((lambda (n) (1+ n))
    ((lambda (f) (funcall f 3))
     #'(lambda (n) (1+ n))))))

(defun letlabel4 (x)
  ((lambda (n) (1+ n))
   ((lambda (n) (1+ n))
    ((lambda (f) (funcall f x))
     #'(lambda (n) (1+ n))))))

(defun letlabel1 (x)
  (let ((f #'(lambda (n) (1+ n))))
    (funcall f x)))

;;; this is broken, f gets strategy/heap
(defun letlabel2 (x)
  (let ((f #'(lambda (n) (1+ n))))
    (funcall f
             (funcall f x))))

(defun label-closure-arg (x)
  (labels ((foo (f n)
             (funcall f n)))
    (foo #'(lambda (i) (+ i x))
         3)))

(defun foo (x)
  (labels ((inc (n) (1+ n))
           (nocall () (frazzle)))
    (bar (inc x))))

(defun letfun (x)
  (let ((f #'(lambda (n) (1+ n))))
    (bar f f x)))

;;; this still doesn't work
;;; because bar and k0 are live in foo
;;; (lambda-live should be union of live of label procs but not body?)
(defun passlabel ()
  (labels ((foo (n) (1+ n)))
    (bar #'foo)))


;;; proc labels

;;; but what happens if the proc closes things???

(defun foo (l)
  (labels ((foo-1 (l)
             (unless (null l)
               (foo-1 (car l))
               (foo-1 (cdr l)))))
    (foo-1 l)))


(defun foo (l)
  (labels ((bar (l)
                (bletch l)
                (bletch l))
           (bletch (l)
              (cons 'x l)))
    (bletch l)
    (bar l)
    (bar l)))


(defun lambdarest ()
  ((lambda (&rest list) list) 3 4 5))


(defun lambdagone (x y)
  (funcall #'(lambda (a b) (foo a b))
           x y))

(defun lambdagone1 (x y)
  ((lambda (a b) (foo a b)) x y))

(defun lambdalose ()
  ((lambda (x y) (bar x y)) 3))

(defun do-in-o ()
  (bar (do ((a 0 (1+ a)))
           ((plusp a) a))
       3))

(defun do-not-in-o ()
  (bar (do ((a 0 (1+ a)))
           ((done a) a))
       3))


(defun newopen (x)
  (bar (baz x))
  (done (frobozz) 3))


(defun testdisp (word)
  (prims:dispatch (prims:byte 3 4) word ;gr:*return-0*
    (0     (code1))
    ((2 4) (code2))
    (6     (+ 3 4))
    (t     (otherwise-code))))


(defun testdisp (l)
  (cons
    (prims:dispatch (prims:byte 2 0) gr:*return-0*
       (0 (hw:a0))
       (1 (hw:a1))
       (2 (hw:a2))
       (3 (hw:a3)))
    l))


;;; special vars

(defvar *foo*)

(defun specref-setq ()
  (setq *foo* (1+ *foo*)))


(defun specialarg (*foo*)
  (bar 3)
  nil)

(defun specialarg-tail (*foo*)
  (bar 3)))

(defun specarg-mv (*foo* x y)
  (values x (bar) y))

(defun unspecialarg (*foo*)
  (declare (unspecial *foo*))
  (bar *foo*))

(defun special-prim-tail (*foo* x)
  (1+ x))

(defun special-prim-tail (*foo* x)
  (+ x 3))

(defun specialarg-values (*foo* x y)
  (values x 3 y))

;;; this is pretty bad
;;; because each return is generated separately
;;;
;;; this could be improved by the *return-point* hack
;;; (if I could figure out a good way of binding *return-point* to nil
;;; at the right times)
(defun specarg-many-vals (*foo* x y z c)
  (case c
    (0 x)
    (1 y)
    (2 z)
    (t nil)))

;;; but that makes this work
;;; because the returns are different
(defun specarg-mv-maybe (*foo* x y p)
  (if p
      x
    (values x y)))


(defun letspec (x)
  (let ((*foo* 3))
    (bar)
    x))

;;; can't substitute a continuation
;;; in if the lambda binds specials
;;; --- was really a special case of no-subst-cont-if-specials below
(defun letspecexit (x)
  (let ((*foo* 3))
    (bar))
  (bletch x))

(defun letspecprimexit (x)
  (let ((*foo* 3))
    (+ x (bar))))

(defun letspec-tail (x)
  (let ((*foo* 3))
    (bar x)))

(defun letspec-values (x y)
  (let ((*foo* 3))
    (values y (bar) x)))

(defun foo (x y)
  (multiple-value-bind (a b)
      (let ((*foo* 3))
        (values (bar x) y))
    (baz a b)))


(defun key-bound-to-special (x &key (foo *foo*))
  (let ((*foo* foo))
    (bar x)))

(defun bind-arg-with-optionals (*foo*  &optional b c)
  (bar 3 b c))


;;; this used to substitute fff in for cont to bletch therefore
;;; having *foo* bound at call to fff
;;; this was fixed up in substitute-lambda? by having it check for
;;; any specials bound, but what about other dynamic state?
(defun no-subst-cont-if-specials ()
  (labels ((bar ()
             (let ((*foo* 3))
               (bletch))))
    (fff (bar))))


(defun foo ()
  (labels ((bar (*foo*)
             (bletch)))
    (fff (bar 3))))

(defun labret (y)
  (labels ((foo (x) (1+ x)))
    (foo y)))

(defun labvalsret (y)
  (labels ((foo (x) (values (1+ x) t)))
    (foo y)))

(defun procret (y)
  (labels ((foo (x) (1+ x)))
    (foo (foo y))w
    ))

;;; order of arg eval with setqs

(defun bar (x)
  (foo x (setq x 5)))

(defun bar (x)
  (setq x 6)
  (foo x (setq x 5)))

(defun bar (x)
  (foo (setq x 4) x (setq x 5)))

(defun bar (x)
  (foo x x x))

(defun bar (x)
  (setq x 3)
  (foo x))

(defun bar (x)
  (foo x)
  (setq x 3))


;;; stack slots


(defun stackargs (a b c d   e f g h   i j k l   m n o p   q r)
  (list r a d))

(defun move-slot-to-slot (a b c d   e f g h   i j k l   m n o p   q r)
  (setq q r)
  (really r a d))


(defun mvsetq-slot (a b c d    e f g h   i j k l   m n o p)
  (let (u)
    (multiple-value-setq (u a) (bar))
    (frotzwith u b)))


(defun stackvars (a b c d   e f g h   i j k l  m n o p)
  (let ((u (bar a b)))
    (foo u u)))

(defun stackvars (a b c d   e f g h   i j k l  m n o p)
  (do ((u o (1+ u))
       (v p (1- v)))
      ((= u v))
    (bar a o p)))


;;; this is broken
(defun aref-index-on-stack (a b c d   e f g h   i j k l   m n o p   q r)
  (li:svref a r))






(defun get-dest-test (x)
  (setq gr:*trap-temp1* gr:*save-o-a-r*)
  (print 'foo)
  (hw:write-open-active-return (hw:ldb x (byte 3 4) 0))
  (print 'done))


(defun foo (x)
  (declare (ftype (function (integer) integer) fff))
  (fff x)
  (fff x))


(defun sigh ()
  (let ((x gr:*trap-temp1*))
    (foo 3)
    (bar x x)))

(defun dif-frame-glob ()
  (bar (hw:dpb-unboxed gr:*trap-temp1*
                       (byte 3 4)
                       (hw:unboxed-constant 0))))

(defun glob-unop-dtp-left ()
  (let ((ptr gr:*cons-cache-free*))
    (setq gr:*cons-cache-free* (+ 1 gr:*cons-cache-free*))
    ptr))


(defun tash (x)
  (bar (ash x 1)
       (ash x -1)
       (ash x 2)
       (ash x -2)
       (ash x 0)
       (ash x 5)
       (ash x (bletch))))

(defun test-ash-both-fix (x)
  (ash (bar x) 1))

;;; i thought get-left-side-for-fixnum-unop would lose here
;;; but register alloc/ read-functional-source is instead...
(defun ash-lossage ()
  (setq gr:*trap-temp1* (ash (hw:read-md) 1)))

(defun tt (x y)
  (if (vinc::data-type= x y)
      (print 'yow)
    (coerce x y)))


(defun dpbtest (w v bs)
  (bar (dpb 3 (byte 3 4) 5)
       (dpb v (byte 3 4) w)
       (dpb v (byte 5 24) w)
       (dpb v bs w)))


;;; Closures

(defun noclose (x)
  (bar #'(lambda (n) (+ n 3))))

(defun nocloselet ()
  (let ((f #'(lambda (n) (foo n))))
    (bar f f)))

(defun doubleclose (x)
  (bar #'(lambda (n) (foo n #'(lambda (i) (+ i x))))))

(defun doublecloselet (x)
  (let ((f #'(lambda (i) (+ i x))))
    (bar #'(lambda (n) (foo n f f)))))

(defun close (x)
  (bar #'(lambda (n) (+ x n))))

(defun setclosearg (x)
  (bar #'(lambda () (prog1 x (setq x (1+ x)))))
  (setq x 33))

(defun setclosearg1 (x)
  (bar #'(lambda () (prog1 x (setq x (1+ x)))))
  (setq x (bar))
  (foo))


(defun setcloselet (x)
  (let ((y 33))
    (bar #'(lambda () (setq y (1+ y))))
    y))

(defun 2close (x y)
  (bar #'(lambda () (foo x y))
       #'(lambda () (baz x y))))


(defun copy-env-slot-to-env-slot (x y)
  (bar #'(lambda () (setq x y))))

;;; actually it doesn't because v becomes a closure slot
(defun copy-env-slot-to-stack-slot (a b c d   e f g h   i j k l   m n o p   u v)
   (bar #'(lambda () (setq v a))))

(defun copy-env-slot-to-stack-slot (a b c d   e f g h   i j k l   m n o p   u v)
  (setq v a)
  (bar #'(lambda () (setq a 3))))

(defun copy-stack-slot-to-env-slot (a b c d   e f g h   i j k l   m n o p   u v)
  (setq a v)
  (bar #'(lambda () (setq a 3))))



(defun stack-args ()
  (yow)
  (bar 1 2 3 4   5 6 7 8   9 10 11 12  13 14 15 16  17 18))


(defun stack-to-key (a b c d   e f g h   i j k l   m n o p   u v &key (x u) (y v))
  (bar u v x y))

(defun stack-alloc-and-cleanup-with-optionals (a &optional (b 3) (c 4 ))
  (multiple-value-bind (x1 x2 x3 x4   x5 x6 x7 x8   x9 x10 x11 x12 x13 x14)
      (bar a b c)
    (baz a b c x1 x2 x3 x4   x5 x6 x7 x8   x9 x10 x11 x12 x13 x14)))


;;; Funcall & Apply

(defun foo (f x)
  (funcall f x))

;;; the dest of get-function is R2 not NEW-OPEN
;;; because reg-alloc doesn't known that $FUNCALL-INTERNAL
;;; is going to do a call
(defun loser (x)
  (print (funcall (get-function) x)))

(defun foo (f x y l)
  (apply f x y l)))

;;; this is not wonderfull
(defun bar (a b &rest args)
  (apply (get-function a b) args))

(defun bar (f)
  (apply f (get-args)))

(defun bar ()
  (apply (get-fun) (get-args)))

(defun apply-constant (x l)
  (apply #'foo x l))

(defun tail-internal (x)
  (ash x 3))





(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo x))
         3))
  (print 'done))

Generating:
77676633  ((DYNAMIC-RF_6 NIL K_0) (^BLOCK_7 1 ^B_14))   STRATEGY/HEAP
77676706   ((BLOCK_7 NIL FOO_4) ($OPEN-FRAME 1 ^C_11 '#{CALL-NODE BAR_1 77676735}))   STRATEGY/OPEN
77677460    ((C_11)   (BAR_1 1 FOO_4 ^P_8 '3))   STRATEGY/OPEN
77677045     ((P_8 NIL K_2 X_3) (FOO_4 0 X_3))   STRATEGY/HEAP
77700113   ((B_14 IGNORE_13) ($OPEN-FRAME 1 ^C_12 '#{CALL-NODE PRINT_5 77677602}))   STRATEGY/HEAP
77677762    ((C_12)   (PRINT_5 1 K_0 'DONE))   STRATEGY/OPEN

;;; throw generated by generate-known-return



(defun dynamic-rf ()
  (block foo
    (bar #'(lambda (x) (return-from foo x))
         3))))

Generating:
77664766  ((DYNAMIC-RF_5 NIL K_0) ($OPEN-FRAME 1 ^C_10 '#{CALL-NODE BAR_1 77665070}))   STRATEGY/HEAP
77665613   ((C_10)   (BAR_1 1 K_0 ^P_7 '3))   STRATEGY/OPEN
77665200    ((P_7 NIL K_2 X_3) (K_0 0 X_3))   STRATEGY/HEAP

;;; throw generated by generate-return



(defun unwind-go ()
  (tagbody
   foo
      (bar #'(lambda () (go foo)) 3)
      (print 'yow))
  (print 'done))



(defun ds-of-crocked-loop-cont (s)
  (labels ((vqm (q c)
             (cond ((= c 100) '())
                   ((foo q c) (illop))
                   (t (bar c))))
           (vqa (count)
                (if (zerop count)
                    '()
                  (progn
                    (vqm count 0)
                    (vqa (1- count))))))
    (vqa s)
    (print 'done)))

(defun fcall (x)
  (list #'sdflkj x))


(defun labelclose ()
  (labels ((inc (n) (1+ n)))
    (bar #'(lambda (x y) (bletch (inc x) (inc y)))
         3)))



(defun ketchup ()
  (catch 'up
    (bar 3)))

(defun catsup ()
  (catch 'up
    (bar 3))
  (print 'done))


;;; this is cool except
;;; truly-label-p is setting the continuation of bletch
;;; to be known to be bletch (because is 1st arg to go)
;;;
;;; how about, switch args to go! what a hack!
;;; (the right thing is probably to make truly-label-p grock go (again))
(defun opengo (x y)
  (tagbody
      (bar (go bletch) (+ x y))
   bletch
      (print 'done)))

;;; this screws up somehow because
;;; after generating bar in generating
;;; the go to bletch, *dynamic-state* gets to be NIL
(defun opengo1 (x y)
  (tagbody
      (bar (if (> x 200)
               (go bletch)
             x)
           (+ x y))
   bletch
      (print 'done)))



;;; this used to not work because z and y would
;;; get into same pref class (y was not live in bar)
(defun labellose (x)
  (labels ((foo (y)
             (bar y)
             (print y))
           (bar (z)
             (when (minusp z)
               (bar (1+ z)))))
    (foo x)))

;; this is not allocating z to x
(defun do2vars (x)
  (do ((y x (1+ y)))
      ((yp y)))
  (do ((z x (1+ z)))
      ((zp z))))

(defun do3vars (x)
  (do ((y x (1+ y)))
      ((yp y)))
  (do ((z x (1+ z)))
      ((zp z)))
  x)

;;; the original register allocator loser (long fixed)
(defun destructive (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (push () a)))
               ((= i 0) a))))
    (print l)))



(defun foo (n)
  (tagbody
   loop
      (if (= n 0.)
          (go end))
      (frotz)
   end)
  (done))


(defun loopsalot ()
  (print 'yow)
  (tagbody
   loop
      (go foo)
   foo
      (go loop)))


(defun loopsome ()
  (labels ((foo ()
                (bar))
           (bar ()
                (foo)))
    (foo)))



(defun do-do (n)
  (let ((l (bar n)))
    (do ((i n (1+ i)))
        (())
      (if (evenp i)
          (print 'yow)
        (do ((l1 l (cdr l1)))
            ((null l1))
          (print l1))))))



(defun proclabel (n m)
  (labels ((foo (x)
                (+ x 1)))
    (bar (foo n) (foo m))))

(defun proc?label (n)
  (labels ((foo (x)
                (+ x 1)))
    (bar (foo n) (foo 27))))



(defun foo (x)
  (setq x (+ x 22))
  (setq x (+ x 33))
  x)



;;; improve tension branch
(defun var-targeted-to-var-in-oreg (x)
  (let ((q (foo)))
    (bar (if (> x q) x q))))


;;; complicated strategy/label
(defun get-command ()
  (labels ((get-command-loop (n)
             (let ((command (read-command)))
               (if (= command 0)
                   (maybe-do-leds n)
                   command)))

           (maybe-do-leds (n)
             (if (> n 100)
                 (progn (toggle-led)
                        (get-command-loop 0))
                 (get-command-loop (1+ n)))))
    (let ((command (get-command-loop 0)))
      (if (>= command 10.)
          (get-command))
          command)))




(zl:defsubst generic (op arg)
  (let ((fcn
          (case op
            (:foo 'foo)
            (:bar 'bar)
            (t 'huh?))))
    (funcall fcn arg)))

(defun tg (x)
  (generic :bar x))


(defun no-side-effects (x)
  (hw:ldb x (byte 3 4) 0)
  nil)


(defun foo (f)
  (funcall f #'(lambda (a b) (+ a b))))


(defun if$test ()
  (if (bar)
      'x 'y))


(defun test-simplify-test (x)
  (if x 'foo (if x 'foo 'bar)))

(defun test-simplify-pred (x)
  ;; if x is not 3 or 4
  (if (/= x 3 4) (print 'foo)))


;;; catch


(defun foo (x)
  (catch 'tag (1+ x)))

(defun foo (x)
  (catch 'tag (bar)))

(defun foo (x)
  (catch 'tag (values 3 4 5)))

(defun foo ()
  (catch 'tag (values)))

(defun foo (x)
  (catch 'tag x))

(defun foo (x)
  (let ((*foo* 3))
    (catch 'tag x)))

(defun foo ()
  (catch 'tag 259))

(defun foo (x)
  (catch 'tag (if (pred) x 259)))

(defun foo ()
  (catch 'tag (case (foo)
                (0 nil)
                (1 (values 3 4 5))
                (2 (bar)))))
(defun foo (x)
  (print (catch 'tag (if (pred) x 259))))

(defun foo (x)
  (print (catch 'tag (if (pred) (bar) 259))))

(defun catch-no-mv ()
  (print (catch 'tag (values 3 4 5))))


(defun catch-exited ()
  (tagbody
      (catch 'tag (bar) (go xxx))
      (print 'caught)
    xxx
      (print 'done)))



(defun catch-2-calls ()
  (print
    (catch 'tag
      (if (pred)
          (foo)
        (bar)))))


(defun foo (x)
  (multiple-value-bind (a b)
      (catch 'tag (if (pred) (values 3 4) (bar)))
    (done a b)))




(defun dynamic-rf-mv (x)
  (multiple-value-bind (a b c)
      (block foo
        (do () (())
          (if (pred)
              (return-from foo (values 3 x 5))
            (bar #'(lambda (y) (return-from foo (values x y 259)))))))
    (foo a b c)))


(defun foo (x)
  (throw 'tag nil))

(defun foo ()
  (throw 'tag (values 3 4 5)))

(defun foo ()
  (throw 'tag (bar)))

(defun foo ()
  (throw (find-tag) (case (foo)
                      (0 nil)
                      (1 (values 3 4 5))
                      (2 (bar)))))


(throw 'foo (if foo-p a b))

 move r2 a
 branch label
 move r2 b
label
 open
 movei o0 'tag
 call throw



(defun blockval (x)
  (block foo
    (do () (())
      (if (pred)
          (return-from foo x)))))





(defun foo (x)
  (block foo
    (if (pred)
        259
      (bar #'(lambda (x) (return-from foo (fff x)))))))


(defvar *foo*)

(defun foo (x y)
  (let ((*foo* nil))
    (values (convert-rational-to-complex y) (convert-rational-to-complex x))))




;; & keywords

(defun foo (a b &optional x y)
  (bar a b x y))

(defun foo (&rest x)
  (bar x))

(defun foo (a &rest x)
  (bar a x))

(defun foo (a b &rest x)
  (bar a b x))

(defun foo (a b &optional x y &rest r)
  (bar a b x y r))


(defun foo (a &key (x t x-p))
  (bar a x x-p))

(defun ignored-rest (&rest args)
  (bar))


(defun foo (x)
  (if (nlisp:typep x 'li:atom)
      (print 'foo)))


(defun foo (x)
  (typecase x
    (li:cons 'cons)
    (li:vector 'vector)
    (li:array 'array)
    (li:symbol 'symbol)
    (t 'foo)))


(defun foo (x)
  (if (li:typep x 'li:bignum) (print 'yow)))


(defun foo (x)
  (if (li:typep x '(li:integer 0 10)) (foo)))

(defun foo (x)
  (if (li:typep x '(or (li:integer * 0)
                       (li:integer 10 *)))
      (foo)))




;;;; floating point primops



(defun foo (x y)
  (let ((a (hw:float-add-single x y)))
    (bar a)))

(defun foo (x y)
  (hw:float-add-single x y))

(defun double-add-test (xhi xlo yhi ylo)
  (multiple-value-bind (hi lo)
      (hw:float-add-double xhi xlo yhi ylo)
    (bar hi lo)))

(defun double-divide-test (xhi xlo yhi ylo)
  (multiple-value-setq (gr:*value-1* gr:*value-2*)
    (hw:float-divide-double xhi xlo yhi ylo))
  (bar))


(defun double-add-test (xhi xlo yhi ylo)
  (multiple-value-bind (hi lo)
      (hw:float-add-double xhi xlo yhi ylo)
    (bar hi lo)))

(defun foo (x &aux y)
  (setq y (bar x)))
