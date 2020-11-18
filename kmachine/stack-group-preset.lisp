;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-


(defun stack-group-preset (sg function)
  (let ((control-pdl (make-control-pdl sg))
        (ptr (hw:24+ control-pdl-frame-offset-to-registers (control-pdl-pointer control-pdl))))
    (setf (sg-control-pdl sg) control-pdl)
    (dotimes (i 16.)
      (array:%vm-write32 ptr i (hw:unboxed-constant 0)))
    (array:%vm-write32 ptr 0 (hw:dpb-unboxed $$cpdl0-type-open-call %%cpdl0-type-code (hw:unboxed-constant 0)))
    (setq function (find-function function))
    (array:%vm-write32 ptr 1
                       (if (k2:%compiled-function-p function)
                           (k2:%compiled-function-code function)
                         (error "Stack group preset can't cope with this" function)))
    (set-control-pdl-pointer (hw:24+ control-pdl-frame-size ptr))
    nil))
