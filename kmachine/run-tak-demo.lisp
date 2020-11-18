;;; -*- Mode:LISP; Package:LAMBDA; Base:10; Readtable:CL -*-

(zl:fasload "jb:kbug;debug-board.qfasl#>")

(init-debug-board)

(defpackage kbug)

(zl::fasload "jb:k;tak-with-interrupts-cold-load.qfasl#>")

(defun test ()
  (mapc #'(lambda (pair)
            (if (consp pair)
                (debug-write-word (car pair) (cdr pair))
                (sleep 10.))) kbug::foo)
  nil)

(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

(defun k-write-mode (val)
  (debug-write-word #xF7FFF7FC val))

(defun tacky-demo ()
  (loop
    (lam::k-write-mode 0)
    (tak 18. 12. 6.)
    (lam::k-write-mode 4.)
    (tak 18. 12. 6.)))

(eval-when (load eval)
  (compile 'test)
  (compile 'tak)
  (test)
  (tacky-demo))
