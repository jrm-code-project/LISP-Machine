
(defun find-plausible-partitions (unit start end &aux rqb dont-dispose)
  "Search disk unit UNIT from block START to just before block END for valid-looking Lisp worlds.
Use this if a disk label is clobbered.
Each time a block is found that looks like it could be
where the beginning of a partition ought to be,
an entry is printed.  Ignore those whose printed Ucode versions are unreasonable.
If there are two valid-looking entries close together on the disk,
the one with the higher disk address is more likely to be followed
by an actual good Lisp world."
  (setf (values unit dont-dispose)
        (decode-unit-argument unit (format nil "searching for partitions")))
  (unwind-protect
      (progn
        (setq rqb (get-disk-rqb 20))
        (do ((rqb-base start (+ rqb-base 20)))
            ((>= rqb-base end))
          (disk-read rqb unit rqb-base)
          (do ((idx 0 (1+ idx)))
              ((= idx 20))
            (when (= #o1000
                     (aref (rqb-buffer rqb)
                           (+ (* page-size 2 idx) (* 2 %sys-com-band-format))))
              (format t "~%Possible at block ~d:~%" (+ rqb-base idx -1))
              (format t "Ucode version ~d~%"
                      (aref (rqb-buffer rqb)
                            (+ (* page-size 2 idx)
                               (* 2 %sys-com-desired-microcode-version))))))))
    (when rqb (return-disk-rqb rqb))
    (unless dont-dispose (dispose-of-unit unit))))
