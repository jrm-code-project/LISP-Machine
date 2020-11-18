;;; -*- Mode:LISP; Package:GNOG; Readtable:CL; Base:10 -*-

(defun blimp ()
  (let (*print-array*)
     #'(lambda ()                               ; $$$ blarg <17-Nov-88 smh>
        (multiple-value-setq (*print-array*)
          (foo)))))


(defun foo ()
  (CATCH
    'foo
    (bar 123)))

(defun foo ()
  (hoo (CATCH
         'foo
         (bar 123)
         1))
  2)

(defun foo ()
  (CATCH
    'foo
    (prog1 (one 1)
           (two 2))))

(defun foo ()
  (CATCH
    'foo
    1))

(defun foo ()
  (prog nil
        (catch 'foo
          (bar 3)
          (go a)
          (far 4))
        a
        (hoo)))


(defun foo ()
  (CATCH
    'foo
    (values 1 2)))

(defun foo (x)
  (catch 'foo
    (if x
        111
      222)))

;; This loses in pass1 (even native compiling for the lambda) when
;; P1LET tries to remove the nugatory binding ot A to itself.
;; (Is it possible that this optimization isn't legal if one but
;; not the other of the two variables were special?)
#+never ; I don't want to be bothered with this right now. -smh
(DEFUN BOO (NAME)
  (LET
    ((A A))
    1))

(DEFUN DF1 (OPTIONS)
  (WITHOUT-INTERRUPTS
     (DOLIST (X options)
      1))
  options)

(defun df2a ()
  (LET ((INHIBIT-SCHEDULING-FLAG T))
    (prog ((S 1))
      (CAR Ss)
      2))
  3)

(defun df3 ()
  (LET ((INHIBIT-SCHEDULING-FLAG T))
    (block gnarg
      (let ((S 1))
        (CAR S))))
  3)


(defun df4 ()
  (LET ((foo T))
    (declare (special foo))                     ;
    (block gnarg
      (let (foo)
        )))
  3)

(defun df4a ()
  (LET ((foo T))
    (declare (special foo))
    (let (foo)
      ))
  3)

(defun df4b ()
  (LET ((foo T))
    (declare (special foo))
    (tagbody
        (prog ()
              (bar foo)
              (go a))
        a))
  3)

(defun df5 ()
  (let ((a t))
    (declare (special a))
    (boo a)
    (let ((b a))
      (goo b)))
  5)

(defun df7 ()
  (let ((a t) (aa nil))
    (declare (special a a1 bb aa b c d))
    (list b c d)))

(defun go-1 ()
  (prog ((aa     3))                            ; $$$ foo <21-Nov-88 smh>
     top
        (catch 'foobar
          (bar d)
          (go top))
        (win)))

(DEFUN METHOD-FUNCTION-SPEC-hammer ()
  (BLOCK G4820
    (MULTIPLE-VALUE-CALL #'foobar
                         (CATCH 'DWIMIFY-PACKAGE
                           (RETURN-FROM G4822
                             (MULTIPLE-VALUE-CALL #'bar
                                                  (FOO)))))))

(DEFUN METHOD-FUNCTION-SPEC-hummer ()
  (xyz
    (BLOCK G4834
      (MULTIPLE-VALUE-CALL #'foobar
                           (CATCH 'DWIMIFY-PACKAGEs
                             (RETURN-FROM G4835
                               (FOO)))))))

(defun m-f-s-h ()
  (multiple-value-call
    'car
    (catch 'bar
      (foo))))

(defun m-f-s-k ()
  (multiple-value-list
    (catch 'barbar
      (foo))))

(defun m-v-l ()
  (xyz (multiple-value-list (foo))))

(defun progn-lossage-1 ()
  (cons 2
        (progn (foo)
               (if (bar) (return-from progn-lossage-1 2) 3)
               4)))

(DEFUN METHOD-FUNCshun-SPEC-HANDLER ()
  (BLOCK STOR4561
    (MULTIPLE-VALUE-CALL 'foo
                         (CATCH T
                           (RETURN-FROM STOR4561
                             (MULTIPLE-VALUE-CALL (GLOG)))))))

(DEFUN METHOD-FUNCshun-SPEC-HANDLER ()
  (MULTIPLE-VALUE-CALL 'foo
                       (RETURN-FROM METHOD-FUNCshun-SPEC-HANDLER (GLOG))))

(DEFUN F-M-E (FUNCTION-SPEC fl)
  (LET ((*standard-output* t))
    (COND (fl)
          (T
           (LET ((METH (LIST FUNCTION-SPEC NIL)))
             (NULLIFY-METHOD-DEFINITION METH)
             METH)))))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((*print-base* 10))
    (RETURN-FROM FLAVOR-NOTICE-M
      (foo))))

(DEFUN torture-bind (a b)
  (LET ((*print-base*  (if a 123 (RETURN-FROM torture-bind 234)))
        (*print-radix* (if b 456 (RETURN-FROM torture-bind 567))))
    (bar)))

(DEFUN torture-bind-1 (a b)
  (LET ((*print-base*  1)
        (*print-radix* (RETURN-FROM torture-bind-1 567)))
    (bar)))

(DEFUN torture-bind-* (a b)
  (LET* ((*print-base*  (if a 123 (RETURN-FROM torture-bind-* 234)))
         (*print-radix* (if b 456 (RETURN-FROM torture-bind-* 567))))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((*print-base* (RETURN-FROM FLAVOR-NOTICE-M (foo))))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((*print-base* (RETURN-FROM FLAVOR-NOTICE-M 123)))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET* ((*print-base* (RETURN-FROM FLAVOR-NOTICE-M 123)))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((*print-base* (if x (RETURN-FROM FLAVOR-NOTICE-M 123) 456)))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((*print-base* (foo)))
    (bar)))

(DEFUN FLAVOR-NOTICE-M ()
  (LET ((xyzzy (return-from flavor-notice-m (foo))))
    (declare (special xyzzy))
    xyzzy))

(DEFUN torture-multiple ()
  (multiple-value-bind (*print-base* x)
      (RETURN-FROM torture-multiple 567)
    (bar x)))

(defun torture-s-recover ()
  (let* ((*print-base* 10)
         (*print-length* (return-from torture-s-recover (cons *print-base* 20))))
    (foo)))

(defun torture-s-recover-1 (x)
  (let* ((*print-base* 10)
         (*print-length* (if x (return-from torture-s-recover-1 (cons *print-base* 20)) 12)))
    (foo)))

(defun no-call (x)
  (cons (foo (return-from no-call 3)) x))

(defun no-call-1 (x)
  (cons (foo 2 (return-from no-call-1 3)) x))

(defun no-call-2 (x)
  (cons x (foo 2 (return-from no-call-2 3))))

(defun return-catch ()
  (catch (return-from return-catch 123)                         ;values not set
    (lose)))

(defun return-catch-3 ()
  (catch (return-from return-catch-3 (values 1 2 3))
    (lose)))

(defun multiple-value-check (&aux a b)
  (multiple-value (a b) (foo))
  (list a b))

(defun unwind-protect-vs-setq ()
  (let (success)
    (unwind-protect
        (progn (bloop 23)
               (setq success t))
      (print success))))

(defun bitblt-decode-aray (x)
  (BLOCK .DISPATCH.
    (DISP #'(LAMBDA NIL (RETURN-FROM .DISPATCH. x))
          ))
  x
  )

(defun bitblt-decode-aray (x)
  (BLOCK .DISPATCH.
    (DISP #'(LAMBDA NIL (RETURN-FROM .DISPATCH. x))
          ))
  ;;x
  )

(DEFUN VANILLA-OPERATION-HANDLED-P ()
  (nth-value 1
    (fun)))

(DEFUN COMPILE-AT-inAPPROPRIATE-TIME ()
  (UNWIND-PROTECT
      (MULTIPLE-VALUE-PROG1 (glibglib)
                            (foofoofoo))
    (cleanup))
  2)

(DEFUN COMPILE-AT-very-inAPPROPRIATE-TIME ()
  (MULTIPLE-VALUE-PROG1 (glibglib)
                        (foofoofoo))
  2)

;; multiple-value-setq doesn't know how to store into a special
(DEFUN TIME-IN-60THS (&aux QUOTIENT)
  (MULTIPLE-VALUE-SETQ (QUOTIENT inc) (foo)))

(DEFUN TIME-IN-30THS (&aux QUOTIENT other)
  (declare (special inc))
  (MULTIPLE-VALUE-SETQ (QUOTIENT inc other) (foo))
  (list quotient other))
