

other versions of cons

#||||||
;;; this is version with end pointer
(defafun cons (car cdr)
  (alu r+1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break*)
  (move md a1) ;or pass cdr in md
  (alu r+1 vma-start-write ignore gr:*cons-cache-free*)
  (alu l-r nop gr:*cons-cache-free* gr:*cons-cache-end*)
  (alu r+2 gr:*cons-cache-free* gr:*cons-cache-free* br-greater-or-equal)
  (branch region-full (alu r-2 vma ignore gr:*cons-cache-free*))
  (move md-start-write a0)
  (alu r-1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break*)
  (alu-field field-pass return *dtp-cons* vma vinc:%%data-type ch-return next-pc-return)
 region-full
  (move md-start-write a0)
  (alu field-pass a2 *dtp-cons* vma vinc:%%data-type) ;can read vma here??
  (cons-new-region)
  (return a2)


;;; This assumes that *cons-cache-end* is always pointing to a free cons
;;; *cons-cache-end* points to the last cons in the region
;;; we get a new region when we use that last cons
(defafun cons (car cdr)
  (alu r+1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break*)
  (move md a0)
  (alu-field field-pass vma-start-write *dtp-cons* *cons-cache-free* vinc:%%data-type)
  (alu l-r nop *cons-cache-free* *cons-cache-end*)
  (alu r+2 *cons-cache-free* ignore *cons-cache-free* br-less-than)
  (branch region-not-full (move a2 vma))
    (open-call (cons-new-region 0) ())
 region-not-full
  (alu r-1 gr:*allow-sequence-break* ignore gr:*allow-sequence-break*)
  (move md a1)
  (alu r+1 vma-start-write ignore a2)
  (return a2))

;;; Cons assumes that the default cons area
;;; is the cached area,  switching default cons area
;;; must change cache, no binding of *default-cons-area*
(defun cons (car cdr)
  (setq gr:*allow-sequence-break* (1+ gr:*allow-sequence-break*))
  (when (hw:32= gr:*cons-cache-free* gr:*cons-cache-end*)
    (cons-new-region))
  (let ((pointer (make-pointer vinc:$$dtp-list gr:*cons-cache-free*)))
    (setq gr:*cons-cache-free* (hw:32-2+ gr:*cons-cache-free*))
    (setq gr:*allow-sequence-break* (1- gr:*allow-sequence-break*))
    ;; (setf (car pointer) car)
    (store-contents pointer car)
    ;; (setf (cdr pointer) cdr)
    (store-contents-offset pointer 1 cdr)
    pointer))


;;; Get a new region to cons in and setup cons cache
;;; This should be called with sequence breaks off
(defun cons-new-region (area)
  ;; Write out the cons cache into the region data
  (setf (region-data:region-free-pointer gr:*cons-cache-region*)
        gr:*cons-cache-free*)
  (let ((region (area-data:get-active-region area 2)))
    (setq gr:*cons-cache-free* (region-data:region-free-pointer region))
    (setq gr:*cons-cache-end*  (region-data:region-end region))
    (setq gr:*cons-cache-region* region))
  (unless (zerop gr:*scavenge-work-while-consing*)
    (gc:scavenge-while-consing)))
||||||#
