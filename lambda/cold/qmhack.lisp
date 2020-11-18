;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

(defconst %%quantum-byte (byte 11. 14.))
(defvar quantum-map-area-number nil "the area number of the quantum-map")

(defun find-quantum-map ()
  "find the area number of the quantum map"
  (or quantum-map-area-number
      (setq quantum-map-area-number
            (do ((i 0 (1+ i)))
                ((>= i si:working-storage-area) nil)
              (when (string-equal (symbol-name (area-name i))
                                  "QUANTUM-MAP")
                (return i))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun quantum-map-index (quantum-number)
  "compute the virtual address of the entry in the quantum map corresponding to QUANTUM-NUMBER"
  (if (>= quantum-number %number-of-address-space-quanta)
      (ferror nil "Quantum number out of bounds")
    (+ (si:%region-origin (find-quantum-map))
       (* 2 quantum-number))))

(defun read-quantum-map (quantum-number)
  "read a quantum map entry.
Values are one of
        NOT-VALID
        MEMORY copy-first page-offset boot-pages partition-number
        A-MEMORY
        DEVICE nubus-words l2-control l2-page"
  (let ((address (quantum-map-index quantum-number)))
    (if (zerop (%p-ldb si:%%pq1-quantum-is-valid address))
        'not-valid
      (if (zerop (%p-ldb si:%%pq1-quantum-is-device address))
          (values 'memory
                  (if (zerop (%p-ldb si:%%pq1m-page-out-copy-first address))
                      'no-need-to-copy
                    'copy-first)
                  (%p-ldb si:%%pq1m-page-offset address)
                  (%p-ldb si:%%pq2m-boot-pages-allocated (1+ address))
                  (%p-ldb si:%%pq2m-partition-number (1+ address)))
        (if (zerop (%p-ldb si:%%pq1d-quantum-is-special-a-memory address))
            (values 'device
                    (%p-ldb si:%%pq1d-quantum-nubus-words address)
                    (%p-ldb si:%%pq2d-quantum-l2mc-except-meta-bits (1+ address))
                    (%p-ldb si:%%pq2d-quantum-l2mpp (1+ address)))
          (values 'a-memory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun %enable-quantum-map-on-next-boot (p)
  "Enables (T) or dissables (NIL) use of quantum map for the next time this processor is booted."
  (setf (%processor-conf-starting-processor-switches *my-proc-conf*)
        (dpb (if p 1 0) %%processor-switch-fast-boot-enable
             (%processor-conf-starting-processor-switches *my-proc-conf*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-quantum-map (&optional (first 0) (n 1_11.))
  (dotimes (idx n)
    (format t "~A " (read-quantum-map (+ first idx)))
    (if (zerop (logand idx #o7)) (terpri))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar device-quantum-area nil
  "This is the area in which arrays which refer to quantum-mapped NuBus space are consed")

(defun initialize-device-quantum-area ()
  "Creates the area where quantum mapped device arrays are allocated and sets device-quantum-area to its area number."
  (when (null device-quantum-area)
    (setq device-quantum-area
          (make-area :name 'device-quantum-area
                     :gc :static))))

(defun make-device-array (nubus-page nubus-words array-type write-enable)
  "Make an array of the type specified whose data is mapped to the area of nubus space described by NUBUS-PAGE
and NUBUS-WORDS.  If WRITE-ENABLE is nil then the array will be read-only."
  (let* ((words-per-quantum (* 64. page-size))
         (number-of-quanta (ceiling nubus-words words-per-quantum))
         (waste-array (make-array (* words-per-quantum (1+ number-of-quanta))
                                  :type art-32b :area device-quantum-area))
         (device-quantum-start
           (ldb %%quantum-byte
                (%pointer-plus (%pointer-plus waste-array
                                              (si:array-data-offset waste-array))
                               (1- words-per-quantum))))
         device-array
         (boxed-words-per-element (assq array-type array-boxed-words-per-element))
         elements-per-word)
    (cond ((null boxed-words-per-element)
           (ferror nil "~s is not a known array type" array-type))
          ((not (zerop (cdr boxed-words-per-element)))
           (ferror nil "Device arrays must have unboxed data"))
          (t (setq elements-per-word (cdr (assq array-type array-elements-per-q)))
             (setq device-array (make-array (if (< elements-per-word 0)
                                                (ceiling nubus-words elements-per-word)
                                              (* nubus-words elements-per-word))
                                            :type array-type
                                            :displaced-to (* words-per-quantum device-quantum-start)))
             (let ((nb-page nubus-page)
                   (words-remaining nubus-words))
               (dotimes (i number-of-quanta)
                 (compiler:%map-device-quantum (+ device-quantum-start i)
                                               nb-page
                                               (min words-remaining words-per-quantum)
                                               (if write-enable #o14 #o10))
                 (incf nb-page 64.)
                 (decf words-remaining words-per-quantum)))     ;only drops below 0 when we're done
             device-array))))

;;; tell the compiler what's going on
; compiler:(defmic %map-device-quantum #o1164 (quantum-number nubus-page nubus-words l2-control) t)

(defun test-map-device-quantum (a b c d)
  (format t "~&quantum number ~o~&nubus-page ~o ~x~&words ~d~&l2c ~o"
          a b b c d)
  (terpri)
  (compiler:%map-device-quantum a b c d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some useful functions

(defun nubus-address-to-nubus-word (adr)
  (ash adr -2))

(defun nubus-word-to-nubus-page (w-adr)
  (ash w-adr -8))

(defun nubus-address-to-nubus-page (adr)
  (nubus-word-to-nubus-page
    (nubus-address-to-nubus-word adr)))

(defun read-nubus-word (array offset)
  (let ((w0 (aref array (* 2 offset)))
        (w1 (aref array (1+ (* 2 offset)))))
    (dpb w1 #o2020 w0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; some code for testing device mapping

#|
(defun nils-nubus-address ()
  (si:%processor-conf-memory-base-0 si:*my-proc-conf*))

(setq foo
      (si:make-device-array (si:nubus-address-to-nubus-page (si:nils-nubus-address))
                            20. 'art-16b nil))

;;; these should give you symbol pointers to NIL and T respecively:
(read-nubus-word foo 1)                         ;NIL's value cell, pointer to NIL
(read-nubus-word foo 6)                         ;T's value cell, pointer to T
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Some simple quantum map statistics

(defun boot-pages-not-yet-copied ()
  (let ((memory-quanta 0)
        (not-yet-copied 0)
        type copy-first)
    (dotimes (i %number-of-address-quanta)
      (multiple-value (type copy-first)
        (read-quantum-map i))
      (when (eq 'memory type)
        (incf memory-quanta)
        (when (eq copy-first 'copy-first)
          (incf not-yet-copied))))
    (format t "~&~d memory quanta, ~d not yet copied." memory-quanta not-yet-copied)))
