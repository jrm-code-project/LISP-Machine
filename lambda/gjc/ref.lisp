;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

(defun test-stuff (&optional filename)
  (let ((tests '(test-%instance test-symeval-in-instance test-aref test-control)))
    (if filename
        (with-open-file (standard-output filename :direction :output)
          (mapc 'funcall tests))
      (mapc 'funcall tests))))


(defun simple-timer (name f &optional &key loops args interrupts)
  (format t "~&;Running ~S~%" name)
  (let ((time (time)))
    (cond (interrupts
           (apply f args)
           (setq time (quotient (time-difference (time) time) 60.0)))
          ('else
           (without-interrupts
             (apply f args)
             (setq time (quotient (time-difference (time) time) 60.0)))))
    (cond (loops
           (format t "~&;Finished ~D loops of ~S, ~$ seconds, ~E seconds per loop"
                   loops name time (quotient time loops)))
          ('else
           (format t "~&;Finished ~S, ~$ seconds." name time)))))

(defflavor foo
         ((a nil))
         ())

(defun test-%instance (&optional (loops 100000))
  (let ((foo (make-instance 'foo)))
    (let ((f #'(lambda (obj n)
                 (dotimes (j n)
                   (si:%instance-ref obj 1))))
          (g #'(lambda (obj n)
                 (dotimes (j n)
                   (si:%instance-set nil obj 1)))))
    (simple-timer "%instance-ref" f :args (list foo loops) :loops loops)
    (disassemble f)
    (simple-timer "%instance-set" g :args (list foo loops) :loops loops)
    (disassemble g))))



(defun test-symeval-in-instance (&optional (loops 100000))
  (let ((foo (make-instance 'foo)))
    (let ((f #'(lambda (obj n)
                 (dotimes (j n)
                   (symeval-in-instance obj 'a))))
          (g #'(lambda (obj n)
                 (dotimes (j n)
                   (set-in-instance obj 'a nil)))))
    (simple-timer "symeval-in-instance" f :args (list foo loops) :loops loops)
    (disassemble f)
    (simple-timer "set-in-instance" g :args (list foo loops) :loops loops)
    (disassemble g))))


(defun test-aref (&optional (loops 100000))
  (let ((foo (make-array 1)))
    (let ((f #'(lambda (obj n)
                 (dotimes (j n)
                   (aref obj 0))))
          (g #'(lambda (obj n)
                 (dotimes (j n)
                   (aset nil obj 0)))))
    (simple-timer "aref" f :args (list foo loops) :loops loops)
    (disassemble f)
    (simple-timer "aset" g :args (list foo loops) :loops loops)
    (disassemble g))))


(defun test-control (&optional (loops 500000))
  (let ((f #'(lambda (n)
               (dotimes (j n)))))
    (simple-timer "control" f :args (list loops) :loops loops)
    (disassemble f)))
