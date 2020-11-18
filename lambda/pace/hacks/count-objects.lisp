;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

(defvar *number-of-symbols* (make-array 4 :initial-element 0))
(defvar *number-of-numbers* (make-array (list 4 (length q-header-types) 100.) :initial-element 0))
(defvar *number-of-cons-cells* (make-array 4 :initial-element 0))
(defvar *number-of-list-segments* (make-array '(4 100.) :initial-element 0))
(defvar *number-of-fef-exit-vectors* (make-array '(4 100.) :initial-element 0))
(defvar *number-of-fef-macroinstructions* (make-array '(4 1000.) :initial-element 0))
(defvar *number-of-arrays* (make-array '(4 100.) :initial-element 0))
(defvar *number-of-instances* (make-array 4 :initial-element nil))

(defun dump-counts ()
  (compiler:dump-forms-to-file
    "dj:pace.hacks;object-counts"
    `((defvar *number-of-symbols* ,*number-of-symbols*)
      (defvar *number-of-numbers* ,*number-of-numbers*)
      (defvar *number-of-cons-cells* ,*number-of-cons-cells*)
      (defvar *number-of-list-segments* ,*number-of-list-segments*)
      (defvar *number-of-fef-exit-vectors* ,*number-of-fef-exit-vectors*)
      (defvar *number-of-fef-macroinstructions* ,*number-of-fef-macroinstructions*)
      (defvar *number-of-arrays* ,*number-of-arrays*)
      (defvar *number-of-instances* ,*number-of-instances*))))



(defun clear-counts ()
  (array-initialize *number-of-symbols* 0)
  (array-initialize *number-of-numbers* 0)
  (array-initialize *number-of-cons-cells* 0)
  (array-initialize *number-of-list-segments* 0)
  (array-initialize *number-of-fef-exit-vectors* 0)
  (array-initialize *number-of-fef-macroinstructions* 0)
  (array-initialize *number-of-arrays* 0)
  (array-initialize *number-of-instances* nil))

(defun print-counts ()
  (format t "~&Symbols:~30t~10d~10d~10d~10d"
          (aref *number-of-symbols* 0)
          (aref *number-of-symbols* 1)
          (aref *number-of-symbols* 2)
          (aref *number-of-symbols* 3))
  (dotimes (number-type (length q-header-types))
    (dotimes (i (array-dimension *number-of-numbers* 2))
      (when (or (not (zerop (aref *number-of-numbers* 0 number-type i)))
                (not (zerop (aref *number-of-numbers* 1 number-type i)))
                (not (zerop (aref *number-of-numbers* 2 number-type i)))
                (not (zerop (aref *number-of-numbers* 3 number-type i))))
        (format t "~&~a numbers (~a~3d):~30t~10d~10d~10d~10d"
                (substring (nth number-type q-header-types) 13.)
                (if (= i (1- (array-dimension *number-of-numbers* 2))) ">=" "")
                i
                (aref *number-of-numbers* 0 number-type i)
                (aref *number-of-numbers* 1 number-type i)
                (aref *number-of-numbers* 2 number-type i)
                (aref *number-of-numbers* 3 number-type i)))))
  (format t "~&Cons cells:~30t~10d~10d~10d~10d"
          (aref *number-of-cons-cells* 0)
          (aref *number-of-cons-cells* 1)
          (aref *number-of-cons-cells* 2)
          (aref *number-of-cons-cells* 3))
  (dotimes (i (array-dimension *number-of-list-segments* 1))
    (when (or (not (zerop (aref *number-of-list-segments* 0 i)))
              (not (zerop (aref *number-of-list-segments* 1 i)))
              (not (zerop (aref *number-of-list-segments* 2 i)))
              (not (zerop (aref *number-of-list-segments* 3 i))))
      (format t "~&List Segments (~a~3d):~30t~10d~10d~10d~10d"
              (if (= i (1- (array-dimension *number-of-list-segments* 1))) ">=" "")
              i
              (aref *number-of-list-segments* 0 i)
              (aref *number-of-list-segments* 1 i)
              (aref *number-of-list-segments* 2 i)
              (aref *number-of-list-segments* 3 i))))
  (dotimes (i (array-dimension *number-of-fef-exit-vectors* 1))
    (when (or (not (zerop (aref *number-of-fef-exit-vectors* 0 i)))
              (not (zerop (aref *number-of-fef-exit-vectors* 1 i)))
              (not (zerop (aref *number-of-fef-exit-vectors* 2 i)))
              (not (zerop (aref *number-of-fef-exit-vectors* 3 i))))
      (format t "~&FEF exit vectors (~a~3d):~30t~10d~10d~10d~10d"
              (if (= i (1- (array-dimension *number-of-fef-exit-vectors* 1))) ">=" "")
              i
              (aref *number-of-fef-exit-vectors* 0 i)
              (aref *number-of-fef-exit-vectors* 1 i)
              (aref *number-of-fef-exit-vectors* 2 i)
              (aref *number-of-fef-exit-vectors* 3 i))))
  (dotimes (i (array-dimension *number-of-fef-macroinstructions* 1))
    (when (or (not (zerop (aref *number-of-fef-macroinstructions* 0 i)))
              (not (zerop (aref *number-of-fef-macroinstructions* 1 i)))
              (not (zerop (aref *number-of-fef-macroinstructions* 2 i)))
              (not (zerop (aref *number-of-fef-macroinstructions* 3 i))))
      (format t "~&FEF macro (~a~3d):~30t~10d~10d~10d~10d"
              (if (= i (1- (array-dimension *number-of-fef-macroinstructions* 1))) ">=" "")
              i
              (aref *number-of-fef-macroinstructions* 0 i)
              (aref *number-of-fef-macroinstructions* 1 i)
              (aref *number-of-fef-macroinstructions* 2 i)
              (aref *number-of-fef-macroinstructions* 3 i))))
  (dotimes (i (array-dimension *number-of-arrays* 1))
    (when (or (not (zerop (aref *number-of-arrays* 0 i)))
              (not (zerop (aref *number-of-arrays* 1 i)))
              (not (zerop (aref *number-of-arrays* 2 i)))
              (not (zerop (aref *number-of-arrays* 3 i))))
      (format t "~&Arrays < ~3d:~30t~10d~10d~10d~10d"
              (^ 2 i)
              (aref *number-of-arrays* 0 i)
              (aref *number-of-arrays* 1 i)
              (aref *number-of-arrays* 2 i)
              (aref *number-of-arrays* 3 i))))
  )

(defun sum ()
  (let ((total 0))
    (dotimes (vol 4)
      (incf total (* 5 (aref *number-of-symbols* vol))))
    (dotimes (vol 4)
      (dotimes (i (array-dimension *number-of-numbers* 2))
        (dotimes (number-type (length q-header-types))
          (incf total (* i (aref *number-of-numbers* vol number-type i))))))
    (dotimes (vol 4)
      (incf total (* 2 (aref *number-of-cons-cells* vol))))
    (dotimes (vol 4)
      (dotimes (seg-length (array-dimension *number-of-list-segments* 1))
        (incf total (* seg-length (aref *number-of-list-segments* vol seg-length)))))
    (dotimes (vol 4)
      (dotimes (len (array-dimension *number-of-fef-exit-vectors* 1))
        (incf total (* len (aref *number-of-fef-exit-vectors* vol len)))))
    (dotimes (vol 4)
      (dotimes (len (array-dimension *number-of-fef-macroinstructions* 1))
        (incf total (* len (aref *number-of-fef-macroinstructions* vol len)))))
    (dotimes (vol 4)
      (dotimes (exp (array-dimension *number-of-arrays* 1))
        (incf total (* (ash 1 exp) (aref *number-of-arrays* vol exp)))))
    (dotimes (vol 4)
      (let ((instances (aref *number-of-instances* vol)))
        (dolist (i instances)
          (incf total (* (cdr i) (si:flavor-instance-size (get (car i) 'si:flavor)))))))
    total))





(defun count-object (object)
  (ecase (%data-type object)
    ;;DTP-TRAP
    ;;DTP-NULL
    ;;DTP-UNRECONCILED
    ;;DTP-SYMBOL-HEADER
    ;;DTP-HEADER
    ;;DTP-GC-FORWARD
    ;;DTP-EXTERNAL-VALUE-CELL-POINTER
    ;;DTP-ONE-Q-FORWARD
    ;;DTP-HEADER-FORWARD
    ;;DTP-BODY-FORWARD
    ;;DTP-ARRAY-HEADER
    ;;DTP-INSTANCE-HEADER
    ;;DTP-RPLACD-FORWARD
    ;;#.DTP-FIX #.DTP-LOCATIVE #.DTP-U-ENTRY #.DTP-SMALL-FLONUM #.DTP-SELF-REF-POINTER #.DTP-CHARACTER
    (#.DTP-SYMBOL
     (incf (aref *number-of-symbols* (%pointer-volatility object))))
    (#.DTP-EXTENDED-NUMBER
     (incf (aref *number-of-numbers*
                 (%pointer-volatility object)
                 (%p-ldb %%header-type-field object)
                 (%structure-total-size object))))
    ((#.DTP-LIST #.dtp-closure #.dtp-select-method #.dtp-entity #.dtp-stack-closure)
     (cond ((and (= (%p-ldb %%q-cdr-code object) cdr-normal)
                 (= (%p-ldb %%q-cdr-code (%pointer-plus object 1)) cdr-error))
            (incf (aref *number-of-cons-cells* (%pointer-volatility object))))
           (t
            (incf (aref *number-of-list-segments*
                        (%pointer-volatility object)
                        (min (%structure-total-size object)
                             (1- (array-dimension *number-of-list-segments* 1))
                             ))))))
    (#.DTP-FEF-POINTER
     (incf (aref *number-of-fef-exit-vectors*
                 (%pointer-volatility object)
                 (min (%structure-boxed-size object)
                      (1- (array-dimension *number-of-fef-exit-vectors* 1)))))
     (incf (aref *number-of-fef-macroinstructions*
                 (%pointer-volatility object)
                 (min (- (%structure-total-size object)
                         (%structure-boxed-size object))
                      (1- (array-dimension *number-of-fef-macroinstructions* 1))))))
    ((#.DTP-ARRAY-POINTER #.dtp-stack-group)
     (incf (aref *number-of-arrays*
                 (%pointer-volatility object)
                 (min (ceiling (log (%structure-total-size object) 2))
                      (1- (array-dimension *number-of-arrays* 1))))))
    (#.DTP-INSTANCE
     (let ((pair (assq (type-of object) (aref *number-of-instances* (%pointer-volatility object)))))
       (when (null pair)
         (push (setq pair (cons (type-of object) 0))
               (aref *number-of-instances* (%pointer-volatility object))))
       (incf (cdr pair))))))

(defun count-all-objects ()
  (clear-counts)
  (map-over-all-objects #'count-object))

(defun count-objects-in-area (area)
  (clear-counts)
  (map-over-all-objects-in-area area #'count-object))

(defun size-of-area (area)
  (let ((used 0)
        (total 0))
    (without-interrupts
      (do ((region (si:%area-region-list area)
                   (si:%region-list-thread region)))
          ((< region 0)
           (values used total))
        (incf used (si:%region-free-pointer region))
        (incf total (si:%region-length region))))))

(defun report ()
  (let ((total (size-of-area working-storage-area))
        (real (sum)))
    (format t "~&total~20taccounted for")
    (format t "~&#o ~o~20t~o" total real)
    (format t "~&#10r ~d~20t~d   ~d%" total real (round (* 100 (si:%div (float real) (float total)))))))
