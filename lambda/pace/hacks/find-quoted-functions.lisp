;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

(defun quoted-args-p (fef)
  (let ((args-info (args-info fef)))
    (or (ldb-test %%arg-desc-quoted-rest args-info)
        (ldb-test %%arg-desc-fef-quote-hair args-info))))

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

(defun find-fexpers ()
  (let (fexper-list)
    (map-over-all-objects-in-area
      macro-compiled-program
      #'(lambda (object)
          (typecase object
            (compiled-function
             (when (and (symbolp (function-name object))
                        (fboundp (function-name object))
                        (eq (symbol-function (function-name object)) object)
                        (quoted-args-p object))
               (push object fexper-list))))))
    fexper-list))

(defvar *fef-list* nil)

(defun find-fefs ()
  (setq *fef-list* nil)
  (map-over-all-objects-in-area
    macro-compiled-program
    #'(lambda (object)
        (typecase object
          (compiled-function
           (let ((name (function-name object)))
             (cond ((and (symbolp name)
                         (fboundp name))
                    (when (eq (symbol-function name)
                              object)
                      (push object *fef-list*)))
                   (t
                    (push object *fef-list*))))))))
  nil)

(defvar *fef-0* nil)
(defvar *fef-1* nil)
(defvar *fef-2* nil)
(defvar *fef-3* nil)
(defvar *fef-4* nil)

(defun find-small-fefs ()
  (dolist (fef *fef-list*)
    (let ((size (- (%structure-total-size fef)
                   (%structure-boxed-size fef))))
      (when (< size 5)
        (push fef (symbol-value (nth size '(*fef-0* *fef-1* *fef-2* *fef-3* *fef-4* ))))))))

(defun show-small-fefs (list)
  (dolist (f list)
    (when (filter-open-coded-stuff f)
      (format t "~5&~s" f)
      (disassemble f))))

(defun filter-open-coded-stuff (x)
  (cond ((get x 'defstruct-slot) nil)
        ((eq 'named-subst (caadr (assq 'interpreted-definition (debugging-info x)))) nil)
        (t x)))



(defun map-over-all-objects (fun)
  (gc:without-flipping
    (gc:reclaim-oldspace)
    (for-every-structured-area (area)
      (map-over-all-objects-in-area area fun))))

(defvar *32b-arrays* nil)

(defun find-32b-arrays ()
  (setq *32b-arrays* nil)
  (map-over-all-objects #'(lambda (object)
                            (print object)
                            (push object *32b-arrays*))))

(defun average-instructions-per-fef ()
  (let ((instructions 0))
    (dolist (fef *fef-list*)
      (incf instructions (ceiling (- (%structure-total-size fef)
                                     (%structure-boxed-size fef))
                                  2)))
    (// (float instructions) (length *fef-list*))))
