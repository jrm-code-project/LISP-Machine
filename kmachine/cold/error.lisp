;;; -*- Mode:LISP; Package:LISP-INTERNALS; Base:10; Readtable:CL -*-




(defun no-case-error (proceedable function place value typespec)
  (error "NO-CASE-ERROR" proceedable function place value typespec))


; This function will invoke the debugger with all of its arguments
; sitting around in the active registers.
; Proceeding from the debugger will make it return.

(defun error (a0 &optional a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15)
  (k2:kbug-vm-write k2:kbug-flag-addr
                    (hw:dpb-unboxed 1 k2:%%kbug-error-flag (k2:kbug-vm-read k2:kbug-flag-addr)))
  (if (zerop (hw:ldb (k2:kbug-vm-read k2:kbug-flag-addr) k2:%%kbug-entered-flag 0))
      ;; not inside KBUG trap handler, safe to invoke KBUG
      (break a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
    ;; we are running inside KBUG trap handler
    ;; most likely we are running the fasloaded and got an error
    (loop)))

(defun break (a0 &optional a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14)
  (trap:trap-on)
  (nubus-stuff:cause-debugger-trap)
  (hw:nop)
  (hw:read-md)
  nil)


;; Tail-error moved to Trap-handlers.lisp so that cold-files could use it.  --wkf
