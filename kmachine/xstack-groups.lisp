;;; -*- Mode:LISP; Package:LISP-INTERNALS; Readtable:CL; Base:10 -*-
;;;
;;;; Stack Groups
;;;

(defstruct (stack-group (:conc-name sg-)
                        (:constructor %make-stack-group
                                      (name
                                        special-pdl special-pdl-ptr special-pdl-limit
                                        extraneous-pdl extraneous-pdl-ptr extraneous-pdl-limit))
                        (:copier   nil))
  name
  special-pdl
  special-pdl-ptr
  special-pdl-limit
  extraneous-pdl
  extraneous-pdl-ptr
  extraneous-pdl-limit
  control-pdl)


(defun make-stack-group (name &key (special-pdl-size 1000.)
                                   (extraneous-pdl-size 1000.))
  (let ((spdl ;(array:make-vector (1+ special-pdl-size))
              (array:make-1d-array (1+ special-pdl-size)
                                   array:art-special-pdl
                                   gr:*special-pdl-area*))
        (epdl (array:make-1d-array (1+ extraneous-pdl-size)
                                   array:art-extraneous-pdl
                                   gr:*special-pdl-area*)))     ;maybe should be different area, maybe not
    (let ((sg (%make-stack-group
                name
                spdl
                (cons:make-pointer vinc:$$dtp-unboxed-locative
                                   (hw:24+ 2 spdl))
                (cons:make-pointer vinc:$$dtp-unboxed-locative
                                   (hw:32+ special-pdl-size spdl))
                epdl
                (cons:make-pointer vinc:$$dtp-unboxed-locative
                                   (hw:24+ 2 epdl))
                (cons:make-pointer vinc:$$dtp-unboxed-locative
                                   (hw:32+ extraneous-pdl-size epdl)))))
      (array:svset spdl 0 sg)
      (array:svset epdl 0 sg)
      (setf (sg-control-pdl sg) (make-control-pdl sg))
      sg)))


(defun boot-stack-groups ()
  ;; this should not be neccessary, these initial areas
  ;; should have constant area numbers
  (setq gr::*special-pdl-area*
        (area-data:make-area 5.
                             (region-bits:encode-region-bits
                               region-bits:$$region-fixed
                               region-bits:$$region-new-space
                               region-bits:$$region-space-unboxed
                               region-bits:$$region-read-write
                               region-bits:$$scavenge-enabled
                               region-bits:$$region-internal-memory
                               0.
                               )
                             1.))
  (let ((initial-sg (make-stack-group "Initial Stack Group")))
    (setq gr:*special-pdl-ptr*   (sg-special-pdl-ptr initial-sg))
    (setq gr:*special-pdl-limit* (sg-special-pdl-limit initial-sg))
    (setq gr:*stack-pointer*     (sg-extraneous-pdl-ptr initial-sg))
    (setq gr:*stack-limit*       (sg-extraneous-pdl-limit initial-sg))
    (setq gr:*control-pdl*       (sg-contro-pdl initial-sg))
    (load-control-pdl-state)
;   (setq gr:*current-stack-group* initial-sg)
    nil
    ))

;;; Must also unwind and bind special pdl
(defun context-switch (new-stack-group)
  (when (zerop gr:*allow-sequence-break*)
    (let ((current-stack-group (control-pdl-stack-group gr:*control-pdl*)))
      (progn (unwind-special-pdl)
             (setf (sg-special-pdl-ptr      current-stack-group) gr:*special-pdl-pointer*)
             (setf (sg-special-pdl-limit    current-stack-group) gr:*special-pdl-limit*)
             (setf (sg-extraneous-pdl-ptr   current-stack-group) gr:*stack-pointer*)
             (setf (sg-extraneous-pdl-limit current-stack-group) gr:*stack-limit*))
      (setq gr:*next-control-pdl* (sg-control-pdl new-stack-group))
      (dump-call-hardware)
      (progn (setq gr:*special-pdl-ptr* (sg-special-pdl-ptr new-stack-group))
             (setq gr:*special-pdl-limit* (sg-special-pdl-limit new-stack-group))
             (setq gr:*stack-pointer* (sg-extraneous-pdl-ptr new-stack-group))
             (setq gr:*stack-limit*   (sg-extraneous-pdl-limit new-stack-group))
             (rewind-special-pdl)))))
