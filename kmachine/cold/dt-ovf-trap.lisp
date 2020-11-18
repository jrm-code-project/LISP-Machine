;;; -*- Mode:LISP; Package:NEW-MATH; Base:10; Readtable:CL -*-

;***************************************************************************
;  Main handler for datatype and overflow traps
;***************************************************************************

(defun trap:dt-and-ovf-trap-handler-1 ()
  (let* ((save-sstep   (vinc:disable-single-step-trap))
         (save-async   (vinc:disable-asynchronous-traps))
         (save-pc      gr::*save-trap-pc*)
         (save-pc+     gr::*save-trap-pc+*)
         (save-oreg    gr::*save-oreg*)
         (save-jcond   gr::*save-jcond*)
         (save-status  gr::*save-status*)
         (save-left    gr::*save-left*)
         (save-right   gr::*save-right*)
         (save-q       (hw:read-q-register))
         (memstat      (hw:read-memory-status))
         (vma          (hw:read-vma))
         (md (progn
               (when (= hw:$$wmd-valid (hw:ldb memstat hw:%%memory-status-md-written-lately 0))
                 (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
                 (hw:memory-wait)
                 (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed trap:*magic-garbage-location*)
                 (hw:read-md))
               (trap:trap-on)
               (hw:read-md))))
    (if (k2:this-is-a-breakpoint? save-pc)
        (progn
;         (nubus-stuff:acknowledge-debugger-trap)
;         (hw:read-md)                          ;force ack
          (restore-asynchronous-traps save-async)
          (k2:kbug-trap-handler-2 save-pc save-left save-right save-status vma md memstat)
          (hw:read-md)
          (hw:nop)
          (hw:trap-off)
          (vinc:restore-single-step-trap
            (hw:ldb (k2:kbug-vm-read k2:kbug-flag-addr) k2:%%kbug-trace-flag 0));;+++This pattern, repeats abstract out. --wkf
          (setq gr:*kbug-trap* 1))
      (progn
        (restore-asynchronous-traps save-async)
        (multiple-value-bind (answer result-status)
            (trap:dt-and-ovf-trap-handler-2 save-pc save-left save-right save-status)
          (hw:read-md)
          (hw:nop)
          (hw:trap-off)
          (vinc:restore-single-step-trap save-sstep)
          (setq gr:*kbug-trap* nil)
          (setq gr::*save-right* answer)
          (setq gr::*trap-temp3* answer)
          (setq gr::*trap-temp4* gr:*save-right*)
          (setq gr::*save-status*  result-status))))
    (setq gr::*save-trap-pc*      save-pc)
    (setq gr::*save-trap-pc+*     save-pc+)
    (setq gr::*save-oreg*    save-oreg)
    (setq gr::*save-jcond*   save-jcond)
    (hw:load-q-register save-q)
    (hw:write-md-unboxed md)
    (hw:vma-start-write-no-gc-trap-unboxed trap:*magic-garbage-location*)
    (vmem:write-md-generic md
                           (hw:ldb-not memstat hw:%%memory-status-md-not-boxed-bit 0))
    (vmem:write-vma-generic vma
                            (hw:ldb-not memstat hw:%%memory-status-vma-not-boxed-bit 0))))



;; This is just for convenience till the real datatype trap handler get warm loaded.
;;
(defun trap:dt-and-ovf-trap-handler-2 (save-pc save-left save-right save-status)
;  (li:error "Datatype trap handling not loaded yet!" save-pc save-left save-right)
  (trap:illop "Datatype trap in cold load"))
