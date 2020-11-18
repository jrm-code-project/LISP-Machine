;;; -*- Mode:LISP; Package:KBUG; Base:10; Readtable:ZL -*-

(defun tak (x y z)
  (if (not (< y x))
      z
    (tak (tak (1- x) y z)
         (tak (1- y) z x)
         (tak (1- z) x y))))

(defun tacky-demo ()
  (loop
    (lam::k-write-mode 0)
    (tak 18. 12. 6.)
    (lam::k-write-mode 4.)
    (tak 18. 12. 6.)))


(defun do-tak ()
  (lam:k-reset)
  (load-fcns '(test-tak tak))
  (kbug 'test-tak))

;----------------------------------------------------------------

(defun test-tak ()
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
  (hw:nop)
    (hw:write-processor-control 0)              ;icache off
    (hw:nop)
    (hw:nop)
    (hw:write-processor-control 7)              ; icache on
    (hw:nop)
    (hw:nop)
  (do () (())
    (setq gr:*trap-temp1* (hw:32-1+ gr:*trap-temp1*))
    (hw:write-memory-control (hw:dpb gr:*trap-temp1* (prims:byte 3 5) (hw:read-memory-control)))
    (when (not (= (tak 18. 12. 6.) 7.))
      (do () (())))
    ))

(tak 18. 12. 6.) ;--> 7.

(tak 20. 11. 3.) ;---> 11.

(tak 19. 12. 5.) ;---> 12.
