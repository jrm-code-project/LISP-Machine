;;; -*- Mode:LISP; Base:10; Readtable:ZL -*-


(defun foo (a)
  (bar a))

(defconstant *c* 'five)
(defvar *v*)
(defconst *cc* 'seven)
(defun foo-1 (*v*)
  (bar *cc*))


(defun foo-2 (*v* &optional (b *v*))
  (bar *v*))


(defun yuck (x)
  (+ x 'b))

(defun parallel-bind-test-1 ()
  (let ((a 3)
        (b 4))
    (print (list a b))))

(defun parallel-bind-test-2 ()
  (let ((a 7))
;    (print a)
    (let ((a 3)
          (b a)
          (c 5))
      (declare (special b))
;    (print c)
      (print (list a b c)))))


;;; This has the potential to lose open frames from the pool if we aren't
;;; careful to return them when returning out from where they were started.

(defun return-test (flag)
  (block foo
    (foo 3 (if flag
               (return-from foo 'lose)
             2))))



;;; This has to clean up a tail-open.  To do this, we do:
;;; (TAIL-CALL (0 0) NIL NEXT-PC-PC+1)
;;; (TAIL-OPEN-CALL (0 0) NIL NEXT-PC-PC+1)
;;; The analysis is:
;;;                   O A R
;;; Initial state:    A A R  ; O had better have the same as A, on *ANY* tail-call.
;;; Tail open:        F A R  ; F is new from heap.
;;; Tail open:        F F A  ; R goes to heap.
;;; Tail open-call:   A A F  ; Once again in a valid state; with a different R reg.

(defun tail-punt (flag)
  (tagbody loop
      (cons 'a (when flag (go loop)))))

(defun multiple-value-test-1 (a b c)
  (values a b c))

(defun multiple-value-return-out-andor (flag)
 (values (foo 1)
         (when flag (return-from multiple-value-return-out-andor 8))
         (bar 2)
         (baz 3)))

(defun multiple-value-return-out-to-block-andor (flag)
  (let ((yow (block yow
               (values (foo 1)
                       (when flag (return-from yow 8))
                       (bar 2)
                       (baz 3)))))
    yow))

(defun prog1-return-out-to-block-andor (flag)
  (let ((yow (block yow
               (prog1 (foo 1)
                      (when flag (return-from yow 8))
                      (bar 2)
                      (baz 3)))))
    (yow yow)))

;;; This is a case of P2VALUES-FOR-K that doesn't work yet.
(defun multiple-value-return-out-andor-simpler (flag)
 (values
   (baz 3)
   (when flag (return-from multiple-value-return-out-andor-simpler 8))))



(defun xx-close-for-k (frame-level dest)
    (foo (open-frame-cleanup-generator frame-level) dest))



(defun throw-multiple-value (a b c)
  (throw 'foo (values a b c)))

(defun catch-throw-multiple-value (a b c)
  (catch 'foo
    (throw-multiple-value a b c)))


(defun many-arguments-return-out (flag)
  (foo 0 1 2 3 4 5 6 7 7 8 9 10 11 12 13 14 15 'stack-0 'stack-1
       (when flag (return-from many-arguments-return-out nil))))
