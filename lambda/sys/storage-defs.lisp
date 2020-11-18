;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:CL -*-

;;; (c) Copyright 1985, Lisp Machine Incorporated (LMI).

(defsubst page-number (address) (ldb (byte #o21 #o10) (%pointer address)))
(defsubst page-index (address) (ldb (byte #o10 #o00) (%pointer address)))

(defsubst page-volatility (page) (aref #'virtual-page-volatility page))

(defsubst page-structure-handle (page)
  (%p-pointer (%pointer-plus (%region-origin virtual-page-data) page)))

(defsubst page-first-header (page)
  (%p-ldb (byte #o11 #o11) (%pointer-plus (%region-origin virtual-page-data) page)))

(defsubst page-initial-qs (page)
  (%p-ldb (byte #o11 #o00) (%pointer-plus (%region-origin virtual-page-data) page)))

;;; Area table accessors.

(defsubst %area-region-bits (area) (aref #'area-region-bits area))
(defsubst %area-region-list (area) (aref #'area-region-list area))
(defsubst %region-list-thread (region) (aref #'region-list-thread region))
(defsubst %area-region-size (area) (aref #'area-region-size area))

;;;|||Removed reader flag to "#.%%region..." constant occurrences.
;;;The compiler will open-code these system constants. --Keith 21-oct-88

(defsubst %area-type (region)
  (%logldb %%region-space-type (%area-region-bits region)))

(defsubst %area-flip-enable (region)
  (%logldb %%region-flip-enable (%area-region-bits region)))

(defsubst %area-scavenge-enable (region)
  (%logldb %%region-scavenge-enable (%area-region-bits region)))

(defsubst %area-scavenge-carefully (region)
  (%logldb %%region-scavenge-carefully (%area-region-bits region)))

(defsubst %area-volatility (region)
  (%logldb %%region-volatility (%area-region-bits region)))

(defsubst %area-swap-recommendations (region)
  (%logldb %%region-swapin-quantum (%area-region-bits region)))

(defsubst %area-map-status (region)
  (%logldb %%region-map-status-code (%area-region-bits region)))


;;; Region table accessors.

(defsubst %region-origin (region) (aref #'region-origin region))
(defsubst %region-area (region) (aref #'region-area-map region))
(defsubst %region-bits (region) (aref #'region-bits region))
(defsubst %region-length (region) (aref #'region-length region))
(defsubst %region-gc-pointer (region) (aref #'region-gc-pointer region))

(defsubst %region-type (region)
  (%logldb %%region-space-type (%region-bits region)))

(defsubst %region-representation-type (region)
  (%logldb %%region-representation-type (%region-bits region)))

(defsubst %region-flip-enable (region)
  (%logldb %%region-flip-enable (%region-bits region)))

(defsubst %region-scavenge-enable (region)
  (%logldb %%region-scavenge-enable (%region-bits region)))

(defsubst %region-scavenge-carefully (region)
  (%logldb %%region-scavenge-carefully (%region-bits region)))

(defsubst %region-volatility (region)
  (%logldb %%region-volatility (%region-bits region)))

(defsubst %region-swap-recommendations (region)
  (%logldb %%region-swapin-quantum (%region-bits region)))

(defsetf %region-free-pointer set-%region-free-pointer)

(make-obsolete region-origin "use %REGION-ORIGIN")
(make-obsolete region-size "use %REGION-SIZE")
(make-obsolete region-free-pointer "use %REGION-FREE-POINTER")
(make-obsolete region-bits "use %REGION-BITS")
(make-obsolete region-length "use %REGION-LENGTH")



(defsubst %pointer-volatility (pointer)
  "Volatility of the object pointed to by POINTER, as a fixnum from 0 to 3"
  (%region-volatility (%region-number pointer)))

(defmacro for-every-structured-area ((area) &body body)
  "Execute BODY interatively with AREA bound to each area that contains LISP objects."
  `(do ((,area 0 (1+ ,area)))
       ((>= ,area number-of-areas))
     (when (and (area-name ,area)
                (= (ldb %%region-representation-type (%area-region-bits area))
                   %region-representation-type-lisp))
       ,@body)))

(defmacro for-every-region-in-area ((region area) &body body)
  "Execute BODY with REGION bound iteratively to every region in AREA."
  (once-only (area)
    `(do ((,region (%area-region-list ,area) (%region-list-thread ,region)))
         ((minusp ,region))
       ,@body)))

(defmacro for-every-region ((region) &body body)
  "Execute BODY with REGION bound iteratively to every memory region."
  (let ((bound (gensym)))
    `(do ((,bound sys:number-of-regions)
          (,region 0 (1+ ,region)))
         (( ,region ,bound))
       ,@body)))

(defun last-object-in-region (region)
  (%pointer-info (%pointer-plus (%region-origin region)
                                (%make-pointer-offset dtp-fix (%region-free-pointer region) -1)))
  (%find-structure-leader (%pop)))

(defun map-over-all-objects-in-newspace-region (region fun &aux last-object)
  (gc:without-flipping
    (select (ldb %%region-space-type (%region-bits region))
      ((%REGION-SPACE-FREE
         %REGION-SPACE-OLD
         %REGION-SPACE-EXTRA-PDL
         %REGION-SPACE-MOBY-FIXED
         %REGION-SPACE-MOBY-NEW)
       (ferror nil "can't handle this region space type" ))
      ((%REGION-SPACE-NEW
         %REGION-SPACE-STATIC
         %REGION-SPACE-FIXED
         %REGION-SPACE-COPY))
      (t
       (ferror nil "unknown region space type")))
    (cond ((zerop (%region-free-pointer region)))
          (t
           (setq last-object (%make-pointer dtp-locative (last-object-in-region region)))
           (do-named main-loop
                     ((adr (%make-pointer dtp-locative (%region-origin region))))
                     (())
             ;;skip over list headers
             ;; find-structure-header doesn't currently work on them, but
             ;; structure-info says they are 1 word long
             (do ()
                 ((not (and (= (%p-data-type adr) dtp-header)
                            (= (%p-ldb %%header-type-field adr) %header-type-list))))
               (when (eq last-object adr)
                 (return-from main-loop nil))
               (setq adr (%make-pointer-offset dtp-locative adr 1)))
             ;;if this is a pointer to an array leader, skip to array header word
             (cond ((and (= (%p-data-type adr) dtp-header)
                         (= (%p-ldb %%header-type-field adr) %header-type-array-leader))
                    (funcall fun
                             (%make-pointer-offset dtp-array-pointer
                                                   adr
                                                   (%p-ldb %%array-leader-length adr))))
                   (t
                    (funcall fun (%find-structure-header adr))))

             (when (eq last-object adr)
               (return-from main-loop nil))

             (setq adr (%make-pointer-offset dtp-locative adr (%structure-total-size adr))))))))

(defun map-over-all-objects-in-area (area fun)
  (when (not (= area wireable-structures-area))
    (gc:without-flipping
      (gc:reclaim-oldspace)
      (for-every-region-in-area (region area)
        (select (ldb %%region-space-type (%region-bits region))
          ((%REGION-SPACE-FREE
             %REGION-SPACE-OLD
             %REGION-SPACE-EXTRA-PDL
             %REGION-SPACE-MOBY-FIXED
             %REGION-SPACE-MOBY-NEW)
           )
          ((%REGION-SPACE-NEW
             %REGION-SPACE-STATIC
             %REGION-SPACE-FIXED
             %REGION-SPACE-COPY)
           (map-over-all-objects-in-newspace-region region fun))
          (t
           (ferror nil "unknown region space type")))))))

;;; this should be used rarely, and should probably not remain
;;; in the system for long.  Right now it needs to exist to
;;; fix 3.0 band for Beta release. -dg
(defun bash-objects-in-every-pdl (data-type new-data-type new-pointer
                                  &key silent
                                  &aux pdl-list)
  (gc:without-flipping
    (gc:without-scavenging
      (map-over-all-objects-in-area
        pdl-area
        #'(lambda (object)
            (pushnew object pdl-list)))
      (dolist (pdl pdl-list)
        (dotimes (c (array-length pdl))
          (let* ((locative (%make-pointer-offset dtp-locative pdl c))
                 (obj-data-type (%p-data-type locative)))
            (when (= obj-data-type data-type)
              (unless silent
                (format t "~&Bashing data type ~a found in ~s"
                        (nth data-type q-data-types) (array-leader pdl 0)))
              (%p-store-tag-and-pointer locative new-data-type new-pointer))))))))

(defmacro with-quick-region-area-accessors (&body body)
  "This is a generalized version of an idiom I've been using to speed up region-table
hacking inside loops.  The basic idea is to keep the origins of the tables in
local variables for the duration of the loop, and to use %p-pointer to get at
the elements.  This scheme is 4-5 times faster than doing arefs on the displaced
arrays in the function cells of the area names.  The syntax for all the accessors
is the same (i.e. (%region-length region-number)), and setf works on them."
  ;; If you don't like the infringements on your namespace, fix this to use gensyms.
  ;; I for one don't really care, since I never use dots in my names.
  `(let ((.origins. (%region-origin sys:region-origin)))
     ;; There are two constraints that this code depends on for speed.  First, for these
     ;; low areas, the region number is the same as the area number, so we can find the
     ;; base addresses just by looking in sys:region-origin (quickly).  Second, all of
     ;; these addresses are very low, so we can use + instead of %pointer-plus without
     ;; fear of bignum ultra-lossage.
     (let ((region-bits.origin (%p-pointer (+ .origins. sys:region-bits)))
           (region-free-pointer.origin (%p-pointer (+ .origins. sys:region-free-pointer)))
           (region-gc-pointer.origin (%p-pointer (+ .origins. sys:region-gc-pointer)))
           (region-length.origin (%p-pointer (+ .origins. sys:region-length)))
           (region-area.origin (%p-pointer (+ .origins. sys:region-area-map)))
           (area-region-list.origin (%p-pointer (+ .origins. sys:area-region-list)))
           (region-list-thread.origin (%p-pointer (+ .origins. sys:region-list-thread))))
       region-bits.origin
       region-free-pointer.origin
       region-gc-pointer.origin
       region-length.origin
       region-area.origin
       area-region-list.origin
       region-list-thread.origin
       (macrolet ((%region-bits (region)
                    `(%p-pointer (+ region-bits.origin ,region)))
                  (%region-free-pointer (region)
                    `(%p-pointer (+ region-free-pointer.origin ,region)))
                  (%region-gc-pointer (region)
                    `(%p-pointer (+ region-gc-pointer.origin ,region)))
                  (%region-length (region)
                    `(%p-pointer (+ region-length.origin ,region)))
                  (%region-area (region)
                    `(%p-pointer (+ region-area.origin ,region)))
                  (%area-region-list (area)
                    `(%p-pointer (+ area-region-list.origin ,area)))
                  (%region-list-thread (region)
                    `(%p-pointer (+ region-list-thread.origin ,region))))
         ,@body))))
