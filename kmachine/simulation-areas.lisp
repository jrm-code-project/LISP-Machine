;;; -*- Mode:LISP; Package:SIMULATOR; Base:10; Readtable:CL -*-

(defmacro define-k-memory (name size-in-qs)
  (let ((region-name (intern (string-upcase (string-append "*" name "-region*"))))
        (origin-name (intern (string-upcase (string-append "*" name "-origin*"))))
        (initializer (intern (string-upcase (string-append "initialize-" name)))))
    `(PROGN (DEFVAR ,name)
            (DEFVAR ,region-name)
            (DEFVAR ,origin-name)
            (DEFUN ,initializer ()
              (UNLESS (BOUNDP (QUOTE ,name))
                (MAKE-AREA :NAME (QUOTE ,name)
                           :GC   :STATIC)
                (SETF (SI::%AREA-REGION-BITS ,name)
                      (%LOGDPB 0 GC:%%REGION-SCAVENGE-ENABLE
                               (%LOGDPB 0 SI:%%REGION-VOLATILITY
                                        (SI::%AREA-REGION-BITS ,name))))
                (SETQ ,region-name
                      (SI:%MAKE-REGION ,name (SI::%AREA-REGION-BITS ,name) ,size-in-qs))
                (SETF (SI::%REGION-FREE-POINTER ,region-name) ,size-in-qs)
                (SETQ ,origin-name (SI::%REGION-ORIGIN ,region-name))))
            (EVAL-WHEN (LOAD)
              (,initializer)))))


(define-k-memory bootprom         (expt 2. 13.))        ;8K
(define-k-memory memory-map       (expt 2. 16.))
(define-k-memory physical-memory  (expt 2. 22.))        ;16 Meg
;;(define-k-memory boot-prom       (expt 2. ??.)
(define-k-memory register-frames  (* 256. 16. 2.))
(define-k-memory saved-o                   4096.)
(define-k-memory saved-a                   4096.)
(define-k-memory call-heap                 4096.)
(define-k-memory return-pc                 4096.)
(define-k-memory return-dest               4096.)
(define-k-memory return-global-frame       4096.)
(define-k-memory gc-ram                    4096.)
(define-k-memory transporter-ram           4096.)
