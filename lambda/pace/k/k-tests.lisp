;;; -*- Mode:LISP; Package:SIM; Base:10; Readtable:CL -*-
(defkfun test1 (x)
  (setq a1 (+ a0 1))
  (%k-return a1))

(defkfun utest4 (x)
  (%k-open)
  (setq o0 a0)
  (%k-call 'test1 1 (locf a1))
  (%k-return a1))

(defkfun t-test (x)
  (%k-t-open)
  (setq o0 a0)
  (%k-t-call 'test1 1))

(defkfun t-test-2 ()
  (when (or (not (numberp a0))
            (not (numberp a1))
            (not (numberp a2)))
    (ferror nil "bad"))
  (%k-t-open)
  (cond ((> a0 0)
         (%k-open)
         (setq o0 (1- a0))
         (setq o1 a1)
         (setq o2 a2)
         (%k-call #'t-test-2 3 (locf a3))))
  (setq o0 a0)
  (setq o1 a1)
  (setq o2 a2)
  (%k-t-call #'t-test-2 3))

(defkfun no-op ()
  (%k-return nil))

(defkfun foo1 ()
  (%k-open)
  (%k-call #'no-op 0 (locf a0))
  (%k-return a0))



(defkfun foo ()
  (%k-open)
  (%k-open)
  (%k-call #'no-op 0 (locf a0))
  (%k-call #'no-op 0 (locf a0))
  (%k-return nil))


(defvar *bind-test-var* 123)

(defkfun bind-test ()
  (catch 1
    (setq a15 *state-stack*)
    (push (cons (locf (symbol-value '*bind-test-var*))
                *bind-test-var*)
          *state-stack*)
    (push *k-frame-stack-pointer* *state-stack*)
    (setq *bind-test-var* 321)
    (%k-open)
    (%k-call #'bind-test-aux 0 (locf a0)))
  (unwind-state-stack-to-level a15)
  (%k-return a0))


(defkfun bind-test-aux ()
  (throw 1 'foo)
  (print *bind-test-var*)
  (ferror nil "foo")
  (%k-return 5))
