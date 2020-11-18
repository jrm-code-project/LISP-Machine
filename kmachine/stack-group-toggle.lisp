;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-

(defvar *original-control-pdl* ())
(defvar *new-control-pdl* ())
(defvar *old-control-pdl* ())

(defun stack-group-preset (sg function)
  (let* ((control-pdl (make-control-pdl sg))
         (ptr (hw:24+
                (+ control-pdl-frame-offset-to-registers control-pdl-base)
                (cons:make-pointer vinc:$$dtp-unboxed-locative control-pdl))))
    (setf (sg-control-pdl sg) control-pdl)
    (dotimes (i 16.)
      (array:%vm-write32 ptr i (hw:unboxed-constant 0)))
    (setq ptr (hw:24+
                (-  control-pdl-frame-offset-to-registers)
                ptr))
    (array:%vm-write32 ptr 0 (hw:dpb-unboxed $$cpdl0-type-open-call %%cpdl0-type-code (hw:unboxed-constant 0)))
    (setq function (find-function function))
    (array:%vm-write32 ptr 1
                       (if (k2:%compiled-function-p function)
                           (k2:%compiled-function-code function)
                         (error "Stack group preset can't cope with this" function)))
    (set-control-pdl-pointer control-pdl (+ control-pdl-frame-size control-pdl-base))
    nil))

(defun stack-group-toggle ()

  (setq gr:*next-control-pdl* *old-control-pdl*)
  (setq *old-control-pdl* gr:*control-pdl*)
  (dump-call-hardware)
  )

(defun set-up-test-stack-group-switch ()
  (setq *original-control-pdl* gr:*control-pdl*)
  (let ((other-sg (make-stack-group "ANOTHER-STACK-GROUP")))
    (stack-group-preset other-sg 'funny-factorial-280)
    (setq gr:*next-control-pdl* (sg-control-pdl other-sg))
    (setq *old-control-pdl* gr:*next-control-pdl*)
    (setq *new-control-pdl* (sg-control-pdl other-sg)))
  (loop))

(defun reset-old-control-pdl ()
  (setq gr:*next-control-pdl* *new-control-pdl*)
  (setq gr:*control-pdl* *original-control-pdl*)
  (loop))

(defun funny-factorial (x)
  (stack-group-toggle)
  (if (= 1 x)
      1
    (* x (funny-factorial (- x 1)))))

(defun funny-factorial-280 ()
  (funny-factorial 5)
  (loop))
