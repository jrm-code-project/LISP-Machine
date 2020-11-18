;-*-Mode: Lisp; Base: 8; Package: SI; Readtable: ZL -*-

(defun disk-save-incremental (dumped-partition-base
                              &aux
                              (booted-band-name
                                (format nil "LOD~C"
                                        (ldb 2010
                                             (if (not (zerop %loaded-band))
                                                 %loaded-band
                                               current-loaded-band))))
                              booted-partition-base)
  "Returns number of pages we can omit from this inc band, minus pages for bitmap and inc band data."
  (multiple-value-bind (base-band-name valid-flag)
      (inc-band-base-band booted-band-name 0)
    (when base-band-name
      (unless valid-flag
        (ferror nil "Base band ~A is no longer valid." base-band-name))
      (setq booted-band-name base-band-name)))
  (setq booted-partition-base (find-disk-partition booted-band-name))
  (let (system-communication-area-rqb)
    (unless booted-partition-base
      (ferror nil "Cannot find partition ~A, which was the one booted."
              booted-band-name))
    (unwind-protect
        (progn
          (setq system-communication-area-rqb (get-disk-rqb))
          (disk-read system-communication-area-rqb 0
                     ;; Disk address of partition's SYSTEM-COMMUNICATION-AREA
                     (+ booted-partition-base
                        (floor (%region-origin system-communication-area)
                               page-size)))
          (unless (= 1000 (aref (rqb-buffer system-communication-area-rqb)
                                (* 2 %sys-com-band-format)))
            (ferror nil "Band ~A, which you booted, is not a complete load in compressed format."
                    booted-band-name))
          ;; If a flip has happened since the time we booted the band,
          ;; our regions now and then are not really comparable,
          ;; and an incremental disk-save has no chance of winning.
          (unless (= %gc-generation-number
                     (%logdpb (aref (rqb-buffer system-communication-area-rqb)
                                    (1+ (* 2 %sys-com-gc-generation-number)))
                              2010
                              (aref (rqb-buffer system-communication-area-rqb)
                                    (* 2 %sys-com-gc-generation-number))))
            (ferror nil "A garbage collection has been done since band ~A was booted."
                    booted-band-name)))
      (and system-communication-area-rqb
           (return-disk-rqb system-communication-area-rqb))))
  ;; Compute the mask of pages not needing saving and store it on disk
  ;; in the partition we are going to dump, in its final resting place.
  (let (mask-rqb
        (n-mask-pages (ceiling (ceiling virtual-memory-size page-size)
                               (* 32. page-size)))
        region-free-pointer-rqb
        base-band-data-rqb)
    (unwind-protect
        (progn
          (setq mask-rqb (get-disk-rqb n-mask-pages))
          (setq region-free-pointer-rqb (get-disk-rqb 1))
          (setq base-band-data-rqb (get-disk-rqb 1))
          (let* ((mask (compare-memory-with-partition booted-band-name))
                 (mask-indirect-array
                   (make-array (ceiling (array-length mask) 16.) ':type art-16b
                               ':displaced-to mask)))
            ;; Dump the mask starting at block 5 in the new band.
            (copy-array-contents mask-indirect-array (rqb-buffer mask-rqb))
            (disk-write mask-rqb 0 (+ 5 dumped-partition-base))

            ;; Copy the base band's REGION-FREE-POINTER into block 4 of the new band
            ;; for use in error checking when this new band gets loaded.
            (disk-read region-free-pointer-rqb 0
                       ;; Disk address of partition's REGION-FREE-POINTER area
                       (+ booted-partition-base
                          (floor (- (%region-origin region-free-pointer)
                                    (%region-length micro-code-symbol-area))
                                 page-size)))
            (disk-write region-free-pointer-rqb 0
                        (+ dumped-partition-base 4))

            ;; Now create the block that describes the base band.
            (put-disk-string base-band-data-rqb
                             booted-band-name
                             0 4)
            ;; Put in the length of the mask that follows
            ;; so loader can know where to find the first dumped page.
            (put-disk-fixnum base-band-data-rqb
                             (array-length mask)
                             10)
            ;; Do we want any other data in this block?
            (disk-write base-band-data-rqb 0 (+ dumped-partition-base 3))
            ;; Return number of pages we DON'T have to put in the dumped band.
            (multiple-value-bind (nil tem)
                (count-changed-pages mask)
              (- tem n-mask-pages 2))))
      (and mask-rqb (return-disk-rqb mask-rqb))
      (and base-band-data-rqb (return-disk-rqb base-band-data-rqb))
      (and region-free-pointer-rqb (return-disk-rqb region-free-pointer-rqb)))))

(defun inc-band-base-band (band unit)
  "Find and check base band of an incremental band named BAND on UNIT (a decoded unit arg).
First value is name of base band, a string.
Second value is T if such a band exists and its data
matches the error check info in the incremental band.
Both values are NIL if the specified band doesn't appear
 to be an incremental world load, or does not exist."
  (declare (values base-band-name valid-flag))
  (let (rqb1 rqb2 base-band-name base-band-base base-band-size
        (part-base (find-disk-partition band nil unit)))
    (unwind-protect
        (when part-base
          (setq rqb1 (get-disk-rqb))
          (setq rqb2 (get-disk-rqb))
          (disk-read rqb1 unit (+ part-base 1))
          (when (= (get-disk-fixnum rqb1 %sys-com-band-format) 1001)
            (disk-read rqb1 unit (+ part-base 3))
            (setq base-band-name (get-disk-string rqb1 0 4))
            (multiple-value (base-band-base base-band-size)
              (find-disk-partition base-band-name nil unit))
            (values base-band-name
                    (when base-band-base
                      (disk-read rqb1 unit (+ part-base 4))
                      (disk-read rqb2 unit (+ base-band-base
                                              (floor (- (%region-origin region-free-pointer)
                                                        (%region-length micro-code-symbol-area))
                                                     page-size)))
                      (string-equal (rqb-8-bit-buffer rqb1) (rqb-8-bit-buffer rqb2))))))
      (and rqb1 (return-disk-rqb rqb1))
      (and rqb2 (return-disk-rqb rqb2)))))

(defvar compare-page-indirect-array :unbound
  "Indirect array used to access an arbitrary page as an ART-16B array, in COMPARE-ALL-REGIONS.")

(defconst comparison-page-quantum 64.)

(defun compare-memory-with-partition (partition &optional (unit 0))
  "Compares each page of memory with corresponding page in PARTITION on UNIT.
The partition is assumed to be a complete world load in compressed format..
Returns an ART-1B array with a bit for each virtual page which is 1
if the page matches data in the disk partition.  It is 0 for mismatching pages,
pages not currently in use, and pages which have no data in the disk partition."
  (let (region-free-pointer-rqb
        region-bits-rqb
        comparison-data-rqb
        (region-table-pages (ceiling (array-length #'region-bits) page-size))
        (part-base (find-disk-partition-for-read partition nil unit))
        (compare-page-indirect-array
         (make-array (* 2 page-size)
                     ':type art-16b
                     ':displaced-to 0))
        (mask (make-array (ceiling virtual-memory-size page-size)
                          ':type art-1b)))
    (unwind-protect
        (progn
          ;; Get rqb just once, used to read each batch of blocks to compare.
          (setq comparison-data-rqb (get-disk-rqb comparison-page-quantum))
          ;; Get these areas as stored in the disk partition,
          ;; since that is how we tell where each region is found.
          (setq region-free-pointer-rqb (get-disk-rqb region-table-pages))
          (setq region-bits-rqb (get-disk-rqb region-table-pages))
          (disk-read region-free-pointer-rqb unit
                     ;; Disk address of partition's REGION-FREE-POINTER area
                     (+ part-base
                        (floor (- (%region-origin region-free-pointer)
                                  (%region-length micro-code-symbol-area))
                               page-size)))
          (disk-read region-bits-rqb unit
                     ;; Disk address of partition's REGION-BITS area
                     (+ part-base
                        (floor (- (%region-origin region-bits)
                                  (%region-length micro-code-symbol-area))
                               page-size)))
          (do ((region-number 0 (1+ region-number))
               (region-page-on-disk part-base))
              ((= region-number (array-length #'region-origin)))
            (let* ((region-free-pointer-on-disk
                    (%logdpb (aref (rqb-buffer region-free-pointer-rqb)
                                   (1+ (* 2 region-number)))
                             2010
                             (aref (rqb-buffer region-free-pointer-rqb)
                                   (* 2 region-number))))
                   ;; Number of pages allocated to this region in the disk partition.
                   (region-disk-pages
                    (ceiling region-free-pointer-on-disk page-size))
                   ;; This tells us whether this region is present at all
                   ;; in the disk partition.
                   (region-bits-on-disk
                    (%logdpb (aref (rqb-buffer region-bits-rqb)
                                   (1+ (* 2 region-number)))
                             2010
                             (aref (rqb-buffer region-bits-rqb)
                                   (* 2 region-number)))))
              (when (ldb-test %%region-space-type region-bits-on-disk)
                (when (and (ldb-test %%region-space-type
                                     (%region-bits region-number))
                           (plusp region-disk-pages))
                  ;; Partition exists now and in the disk partition, so compare.
                  (unless (= %region-space-fixed
                             (ldb %%region-space-type (%region-bits region-number)))
                    (si:page-in-region region-number))
                  (compare-range comparison-data-rqb unit region-page-on-disk
                                 (lsh (%region-origin region-number)
                                      (- 1 (haulong page-size)))
                                 (+ (lsh (%region-origin region-number)
                                         (- 1 (haulong page-size)))
                                    region-disk-pages)
                                 mask)
                  (unless (= %region-space-fixed
                             (ldb %%region-space-type (%region-bits region-number)))
                    ;; This UNLESS avoids a bug in PAGE-OUT-REGION
                    ;; fixed in system 95.
                    ;(unless (= (%area-number (%region-origin region-number))
                    ;            chaos:chaos-buffer-area))
                    (si:page-out-region region-number)))
                ;; Partition exists in the disk partition,
                ;; so advance over it to find address on disk of following region.
                (setq region-page-on-disk
                      (+ region-page-on-disk region-disk-pages)))))
          mask)
      ;; Return all our RQBs that we succeeded in getting.
      (and region-free-pointer-rqb
           (return-disk-rqb region-free-pointer-rqb))
      (and region-bits-rqb
           (return-disk-rqb region-bits-rqb))
      (and comparison-data-rqb
           (return-disk-rqb comparison-data-rqb)))))

(defconst page-size-bits (1- (haulong page-size))
  "Number of bits in word address within a page.")

;Compare a range of pages of memory with a range of pages on disk.
;Pages in memory are PAGE to STOP-PAGE, and on disk start at block ADDRESS on UNIT.
;RQB is an rqb COMPARISON-PAGE-QUANTUM pages long, used to read disk data into.
;All pages that match are marked by storing a 1 into the array MASK
;at an index equal to the page number in memory.
(defun compare-range (rqb unit address page stop-page mask)
  (do ((page-number page (+ page-number comparison-page-quantum)))
      ((>= page-number stop-page))
    (disk-read rqb unit (+ address (- page-number page)))
    (do ((i 0 (1+ i))) ((or (= i comparison-page-quantum) (= (+ page-number i) stop-page)))
      ;; Redirect compare-page-indirect-array to point at this page of virtual memory.
      (%p-dpb-offset (lsh (+ i page-number) page-size-bits)
                     %%q-pointer compare-page-indirect-array
                     (%p-ldb-offset %%array-number-dimensions compare-page-indirect-array 0))
      (if (%string-equal (rqb-buffer rqb)
                         (lsh i (1+ page-size-bits))  ;(* i page-size 2)
                         compare-page-indirect-array 0
                         (lsh page-size 1))
          (setf (aref mask (+ i page-number)) 1)))))

(defun count-changed-pages (mask)
  "Return number of pages in use and marked with zero in MASK.
This will be the number of pages needing to be dumped in an incremental band.
MASK should be a value returned by COMPARE-MEMORY-WITH-PARTITION.
A second value is the number of pages in use but not needing to be dumped."
  (do ((region-number 0 (1+ region-number))
       (count-zeros 0)
       (count-ones 0))
      ((= region-number (array-length #'region-origin))
       (values count-zeros count-ones))
    (when (ldb-test %%region-space-type (%region-bits region-number))  ;Ignore free regions.
      (let ((region-start-page
              (lsh (%region-origin region-number)
                   (- 1 (haulong page-size))))
            (region-n-pages
              (ceiling (%region-free-pointer region-number) page-size)))
        ;; Loop over pages in this region.
        (do ((i 0 (1+ i)))
            ((= i region-n-pages))
          (if (zerop (aref mask (+ region-start-page i)))
              (incf count-zeros)
            (incf count-ones)))))))

(defun report-changed-pages (mask)
  "Print a region-by-region accounting of pages marked with 0 or 1 in MASK.
For debugging only.
Intended for use with MASK as returned by COMPARE-MEMORY-WITH-PARTITION."
  (do ((region-number 0 (1+ region-number))
       (count-zeros 0)
       (count-ones 0))
      ((= region-number (array-length #'region-origin))
       (format t "~&Total ~D. ones, ~D. zeros.~%"
               count-ones count-zeros)
       nil)
    (when (ldb-test %%region-space-type (%region-bits region-number))   ;Ignore free regions.
      (let ((region-start-page
              (lsh (%region-origin region-number)
                   (- 1 (haulong page-size))))
            (region-n-pages
              (ceiling (%region-free-pointer region-number) page-size))
            (region-start-zeros count-zeros)
            (region-start-ones count-ones))
        ;; Loop over pages in this region.
        (do ((i 0 (1+ i)))
            ((= i region-n-pages))
          (if (zerop (aref mask (+ region-start-page i)))
              (incf count-zeros)
            (incf count-ones)))
        (format t "~&Region ~O (in ~S): ~D. ones, ~D. zeros.~%"
                region-number
                (area-name (%area-number (%region-origin region-number)))
                (- count-ones region-start-ones)
                (- count-zeros region-start-zeros))))))
