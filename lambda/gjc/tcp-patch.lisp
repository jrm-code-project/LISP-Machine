;;; -*- Mode:LISP; Package:TCP; Base:10; Patch-File:T -*-


; From file DRIVER-LISPM.LISP#> TCP.KERNEL; LAMH: (43)
(DEFUN MAKE-WIRED-WD-ARRAY (SIZE &OPTIONAL REUSABLE)
  (LET ((ARRAY (OR (AND REUSABLE
                        (EQ 'ART-8B (ARRAY-TYPE REUSABLE))
                        (= (LENGTH REUSABLE) SIZE)
                        REUSABLE)
                   (MAKE-ARRAY SIZE :TYPE :ART-8B
                               ; comment out the :AREA spec here.
                               ;:area SI::DISK-BUFFER-AREA
                               ))))
    (ARRAY-INITIALIZE ARRAY 0)
    (SI:WIRE-ARRAY ARRAY)
    ARRAY))
