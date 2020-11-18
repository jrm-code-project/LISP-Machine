;;; -*- Mode:LISP; Package:GC-RAM; Base:10; Readtable:CL -*-

(in-package 'gc-ram)

(export '(
          dump-ram
          flip
          initialize-quantum
          load-ram
          quantum-volatility-and-oldspace
          zero-ram
          ))

(defconstant *number-of-gc-ram-entries* (vinc:field-maximum hw:%%gc-ram-md-byte))

(defsubst address-gc-ram (quantum)
  (hw:write-md-unboxed (hw:dpb quantum hw:%%gc-ram-md-byte 0.))
  ;; Wait for md to be loaded.
  (hw:nop)
  (hw:nop)
  (hw:nop)
  nil)

(defun read-gc-ram (quantum)
  ;; Bottom 4 bits are data, next 4 are garbage.
  (address-gc-ram quantum)
  (hw:read-gc-ram))

(defun write-gc-ram (quantum new-value)
  ;; Don't make this a subst, you will break LOAD-RAM.
  (trap::without-traps
    #'(lambda ()
        (address-gc-ram quantum)
        (hw:write-gc-ram new-value)
        ;; Let the mmfio bus clear out.
        (hw:nop)
        (hw:nop)
        nil)))

(defmacro modify-gc-ram (quantum thunk)
  `(LET ((QUANTUM    ,quantum)
         (THUNK      ,thunk))
     (TRAP::WITHOUT-TRAPS
       #'(LAMBDA ()
           (ADDRESS-GC-RAM QUANTUM)
           (LET ((NEW-VALUE (FUNCALL THUNK (HW:READ-GC-RAM))))
             (HW:WRITE-GC-RAM NEW-VALUE)
             ;; Let the bus clear out
             (HW:NOP)
             (HW:NOP)
             nil)))))

(defun initialize-quantum (quantum volatility)
  ;; Cannot initialize oldspace quanta.
  (write-gc-ram quantum
                (vinc::dpb-multiple-unboxed
                  hw:$$not-oldspace hw:%%gc-ram-quantum-oldspace
                  volatility        hw:%%gc-ram-quantum-volatility
                  0.)))

(defun flip (quantum)                           ;You found it!
  (modify-gc-ram quantum
    #'(lambda (old-value)
        (hw:dpb hw:$$oldspace hw:%%gc-ram-quantum-oldspace old-value))))

(defun zero-ram ()
  (dotimes (quantum *number-of-gc-ram-entries*)
    (initialize-quantum quantum 0.)))

(defun dump-ram (virtual-location)
  (dotimes (quantum *number-of-gc-ram-entries*)
    (hw:write-md-unboxed (read-gc-ram quantum))
    (hw:vma-start-write-no-gc-trap-unboxed (+ quantum virtual-location))))

(defun load-ram (virtual-location)
  ;; This probably needs some work.
  (dotimes (quantum *number-of-gc-ram-entries*)
    (hw:vma-start-read-no-transport-vma-unboxed-md-unboxed (+ quantum virtual-location))
    (write-gc-ram quantum (hw:read-md))))

(defun quantum-volatility-and-oldspace (quantum)
  (hw:ldb (read-gc-ram quantum) (byte 4. 0.) 0.))
