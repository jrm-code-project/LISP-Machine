;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:ZL -*-

(defun fib (x)
  (if (< x 2)
      x
    (+ (fib (1- x)) (fib (- x 2)))))

(defun fibber-demo ()
  (loop
    (lam::k-write-mode 0)
    (fib 20.)
    (lam::k-write-mode 4.)
    (fib 20.)))


(defun do-fib ()
  (lam:k-reset)
  (load-fcns '(test-fib fib))
  (kbug 'test-fib))

;----------------------------------------------------------------



(defun test-fib ()
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (compiler-let ((nc:*allow-setq-of-global-constant-register* t))
    (setq gr:*one* 1)
    (setq gr:*two* 2))
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (do () (())
    (hw:write-processor-control 0)              ;icache off
    (hw:nop)
    (hw:nop)
    (hw:write-processor-control 7)              ; icache on
    (hw:nop)
    (hw:nop)
    (setq gr:*trap-temp1* (hw:32-1+ gr:*trap-temp1*))
    (hw:write-memory-control (hw:dpb gr:*trap-temp1* (prims:byte 3 5) (hw:read-memory-control)))
    (when (not (= (fib 20.) 6765.))
      (do () (())))
    ))

; fib 20  -- 6765.
; fib 25  -- 75025.
