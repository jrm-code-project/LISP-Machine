;;; -*- Mode:LISP; Package:(ktv USE (K prims)); Readtable:CL; Base:10 -*-

(import '(nc:defafun nc:ndisassemble nc:link))

(defun munch ()
  (do ((time  (hw:unboxed-constant 0))
       (x     (hw:unboxed-constant 0)  (hw:32logand (hw:32-1+ x) (hw:unboxed-constant #x1FF)))
       (xword (hw:unboxed-constant 0))
       (xbit  (hw:unboxed-constant 0)  (hw:32logical-shift-up xbit 1.)))
      (())
    (when (hw:32zerop xbit)
      (setq xbit (hw:unboxed-constant 1))
      (when (hw:32zerop (setq xword (hw:32logand (hw:32-1+ xword)
                                                 (hw:unboxed-constant #x1F))))
        (setq time (hw:32+ time (hw:unboxed-constant #o52525)))))
    ;; use xor mode
    (write-vcmem (hw:32+ (hw:32logical-shift-up (hw:32logand (hw:unboxed-constant #x1FF)
                                                             (hw:32logxor x time))
                                                5.)
                         xword)
                 xbit)))


(defun write-vcmem (addr data)
  (hw:write-vma-unboxed (hw:32+ addr debugger:::kbug::screen-virtual-address))
  (hw:md-start-write-unboxed data)
  data)
