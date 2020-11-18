;;; -*- Mode:LISP; Package:SDU; Base:10 -*-

; bobp
; read and print sdu config file
;
; requires unix-fs.lisp and c-funcs.lisp
;
; (print-config-file)
;    prints most useful info from config file.
; (print-config-file t)
;    prints everything.
;
; (get-list-of-boards)
;    returns a list of the per-slot structures for all nubus slots.
;    use the per-slot defstruct to access them.
;
; (all-disabled-memory-boards)
;    returns a list of the disabled memory boards
;       each element of list is a list of (slot-number board-type)
;    see board-types for the board types.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; per-slot structure in "resource map" section of config file
; see /usr/86include/sys/lmi-config.h

(defstruct (per-slot)
  ps-board-type
  ps-disabled
  ps-slot-number
  ps-mem-size-if-processor
  ps-options
  ps-options-size
  ps-major-version
  ps-minor-version)

; sections of the file
(defvar config-image)
(defvar sys-conf-image)
(defvar slot-array-image (make-array 32))

; board names
; use (nth (ps-board-type ps) board-types)
(defconst board-types '(unknown none lambda mc68000 sdu vcmem half-meg
                                two-meg medium-color buscoupler ti-eight-meg
                                lmi-four-meg lmi-sixteen-meg quad-video
                                nubus-disk lmi-eight-meg lambda-avp lmi-twelve-meg))

(assign-values board-types)
(mapc #'(lambda (x) (putprop x t 'special)) board-types)

(putprop 'half-meg 512. 'memory-size)
(putprop 'two-meg 2048. 'memory-size)
(putprop 'ti-eight-meg 8192. 'memory-size)
(putprop 'lmi-four-meg 4096. 'memory-size)
(putprop 'lmi-sixteen-meg 16380. 'memory-size)
(putprop 'lmi-eight-meg 8192. 'memory-size)
(putprop 'lmi-twelve-meg 12288. 'memory-size)

; (mapcar '(lambda (x) (format t "~&~(~s~) ~a" x (get x 'memory-size))) board-type-strings)

(defvar console-types '("serial-port-A" "vcmem" "quad" "sharetty" "serial-port-B"))

(defun mem-board-p (slot)
  "size in pages if memory board, nil if not"
  (get (nth (ps-board-type slot) board-types) 'memory-size))

(defun console-type-string (type)
  (nth type console-types))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar verbose nil)

(defun print-config-file (&optional verbose)
  "read and print contents of sdu config file"
  (get-config-file)
  (set-up-config-arrays)
  (print-config-header config-image)
  (print-slot-info))

(defun get-list-of-boards ()
  "return a list of per-slot structures for all slots"
  (get-config-file)
  (set-up-config-arrays)
  (let ((return-list nil))
    (dotimes (i 32)
      (push (aref slot-array-image (- 31 i)) return-list))
    return-list))

; top level function for memory diagnostic

(defun all-disabled-mem-boards ()
  "return list of lists of car slot-number, cdr board-type symbol, for disabled memory boards"
  (set-up-config-arrays)
  (do ((i 0 (1+ i))
       (return-list nil))
      ((= i 32)
       return-list)
    (let ((slot (aref slot-array-image i)))
      (if (and (= 1 (ps-disabled slot))
               (mem-board-p slot))
          (push (cons (ps-slot-number slot) (nth (ps-board-type slot) board-types))
                return-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; accessors for config file header

(defun ch-version (h)
  (get-n-bytes h 0 4))

(defun ch-bootable-p (h)
  (get-n-bytes h 4 4))

(defun ch-whole-shared-area-addr (h)
  (get-n-bytes h 8 4))

(defun ch-whole-shared-area-size (h)
  (get-n-bytes h 12 4))

(defun ch-sys-config-addr (h)
  (get-n-bytes h 16 4))

(defun ch-sys-config-size (h)
  (get-n-bytes h 20 4))

(defun ch-slot-array-file-offset (h)
  (get-n-bytes h 24 4))

(defun ch-slot-array-per-slot-size (h)
  (get-n-bytes h 28 4))

(defun ch-sys-config-file-offset (h)
  (get-n-bytes h 32 4))

(defun ch-slot-map-file-offset (h)
  (get-n-bytes h 36 4))

(defun ch-slot-map-size (h)
  (get-n-bytes h 40 4))

(defun print-config-header (h)
  (cond (verbose
         (format t "~&~20a ~d." "version number" (ch-version h))
         (format t "~&~20a ~d." "bootable-p" (ch-bootable-p h))))
  (format t "~&~20a ~x" "shared area addr" (ch-whole-shared-area-addr h))
  (format t "~&~20a ~x" "shared area size" (ch-whole-shared-area-size h))       ;
  (format t "~&~20a ~x" "sys-conf addr" (ch-sys-config-addr h))
  (format t "~&~20a ~x" "sys-conf size" (ch-sys-config-size h))
  (cond (verbose
         (format t "~&~20a ~d." "slot array offs" (ch-slot-array-file-offset h))
         (format t "~&~20a ~d." "per-slot size" (ch-slot-array-per-slot-size h))
         (format t "~&~20a ~d." "sys-conf file offs" (ch-sys-config-file-offset h))
         (format t "~&~20a ~d." "slot-map file offs" (ch-slot-map-file-offset h))
         (format t "~&~20a ~d." "slot-map size" (ch-slot-map-size h))))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar config-file-array nil)

(defun get-config-file ()
  (let ((file (open-unix-file "//sdu//lambda//shr-config.1")))
    (if (null config-file-array)
        (setq config-file-array (make-array (unix-file-size file) :type :art-8b)))
    (rw-file :read file config-file-array (unix-file-size file)))
  config-file-array)

(defun set-up-config-arrays ()
  (setq config-image (get-config-file))
  (setq sys-conf-image (make-array (ch-sys-config-size config-image)
                         :type :art-8b
                         :displaced-to config-image
                         :displaced-index-offset (ch-sys-config-file-offset config-image)))
  (set-up-slot-info))


(defun set-up-slot-info ()
  (dotimes (i 32)
    (let ((image (make-array (ch-slot-array-per-slot-size config-image)
                             :type :art-8b
                             :displaced-to config-image
                             :displaced-index-offset (+ (ch-slot-array-file-offset config-image)
                                               (* i (ch-slot-array-per-slot-size config-image)))))
          (x (make-per-slot)))
      (setf (ps-board-type x) (get-n-bytes image 0 2))
      (setf (ps-disabled x) (get-n-bytes image 2 2))
      (setf (ps-slot-number x) (get-n-bytes image 4 2))
      (setf (ps-mem-size-if-processor x) (get-n-bytes image 12 4))
      (setf (ps-options x) (get-n-bytes image 16 4))
      (setf (ps-options-size x) (get-n-bytes image 20 4))
      (setf (ps-major-version x) (get-n-bytes image 24 2))
      (setf (ps-minor-version x) (get-n-bytes image 28 2))
      (aset x slot-array-image i))))

(defun print-slot-info ()
  (dotimes (i 32)
    (let ((slot (aref slot-array-image i)))
      (if (or verbose
              (not (= 1 (ps-board-type slot))))
          (print-one-info-slot slot)))))

(defun yes-no-string (n)
   (if (= 0 n)
      "no"
    "yes"))

(defun print-one-info-slot (slot)
  (format t "~& ")
  (format t "~&~15a ~d" "slot number" (ps-slot-number slot))
  (format t "~&~15a ~(~s~)" "board type" (nth (ps-board-type slot) board-types))
  (if (not (= 0 (ps-disabled slot)))
      (format t "~&~15a" "disabled"))
  (if (or verbose
          (and (not (= 0 (ps-mem-size-if-processor slot)))
               (not (= #xffffffff (ps-mem-size-if-processor slot)))))
      (format t "~&~15a ~x" "memory size" (ps-mem-size-if-processor slot)))
  (if verbose
      (format t "~&~15a ~d" "option offset" (ps-options slot)))
  (if verbose
      (format t "~&~15a ~d" "option size" (ps-options-size slot)))
  (if (not (= 0 (ps-major-version slot)))
      (format t "~&~15a ~d.~d" "board version"
              (ps-major-version slot)
              (ps-minor-version slot)))
  (cond ((not (= 0 (ps-options slot)))
         (if verbose
             (format t "~&  ~(~s~) option structure:"
                     (nth (ps-board-type slot) board-types)))
         (select (ps-board-type slot)
           (mc68000
            (print-68000-options slot))
           (lambda
            (print-lambda-options slot))
           (vcmem
            (print-vcmem-options slot))
           (quad-video
            (print-quad-options slot))
           (sdu
            (print-sdu-options slot))
           (t
            (if (not (= 0 (ps-mem-size-if-processor slot)))
                (print-memory-options slot)
              (format t "~&~a has an option structure!"
                      (nth (ps-board-type slot) board-types)))))))
  )

(defun option-image (per-slot)
  (make-array (ps-options-size per-slot)
              :type :art-8b
              :displaced-to config-image
              :displaced-index-offset (ps-options per-slot)))

(defun print-68000-options (slot)
  (let* ((x (option-image slot))
        (skip-devmap (+ 8 (get-n-bytes x 4 4) (get-n-bytes x 8 4))))
    (format t "~&    ~15a~a" "screen" (vcm-slot-string (get-n-bytes x 0 4)))
    (if verbose
        (format t "~&    ~15a~d" "devmap size" (get-n-bytes x 4 4)))
    (if verbose
        (format t "~&    ~15a~d" "n sharettys" (get-n-bytes x (+ 0 skip-devmap) 2)))
    (format t "~&    ~15a~a" "console type" (console-type-string
                                              (get-n-bytes x (+ 2 skip-devmap) 2)))))

(defun print-lambda-options (slot)
  (let ((x (option-image slot)))
    (format t "~&    ~15a~d-~d" "speed" (get-n-bytes x 0 2) (get-n-bytes x 4 2))
    (format t "~&    ~15a~a" "screen" (vcm-slot-string (get-n-bytes x 8 4)))
    (format t "~&    ~15a0~o" "switches" (get-n-bytes x 12 4))
    (format t "~&    ~15a~a" "t-ram name" (ascii-string (c-str-copy x 20)))
    (format t "~&    ~15a~a" "micro-load" (ascii-string (c-str-copy x 80)))
    (format t "~&    ~15a~a" "load band" (ascii-string (c-str-copy x 86)))
    (format t "~&    ~15a~a" "page" (ascii-string (c-str-copy x 92)))
    (format t "~&    ~15a~a" "file" (ascii-string (c-str-copy x 98)))
    (if verbose
        (format t "~&    ~15a0~o" "base map reg" (get-n-bytes x 104 4)))
    (if (or verbose
            (not (= 0 (get-n-bytes x 108 4))))
        (format t "~&    ~15a0~o" "parity enables" (get-n-bytes x 108 4)))
    (if verbose
        (format t "~&    ~15a0~o" "map size" (get-n-bytes x 112 2)))
    (if (or verbose
            (not (= 32 (get-n-bytes x 114 2))))
        (format t "~&    ~15a~d." "scan line size" (get-n-bytes x 114 2)))
    ))

(defun vcs-slot-number (vcm-slot)
  (ldb (byte 8 0) vcm-slot))

(defun vcs-screen-number (vcm-slot)
  (ldb (byte 8 16) vcm-slot))

(defun vcs-present-p (vcm-slot)
  (= 0 (ldb (byte 8 16) vcm-slot)))

(defun vcm-slot-string (vcm-slot)
  (with-output-to-string (*standard-output*)
    (cond ((vcs-present-p vcm-slot)
           (let ((board-type (ps-board-type (aref slot-array-image (vcs-slot-number vcm-slot)))))
             (if (= board-type quad-video)
                 (format t "screen ~d of quad-video" (vcs-screen-number vcm-slot))
               (format t "vcmem"))
             (format t " in slot ~d" (vcs-slot-number vcm-slot))))
          (t
           (format t "none assigned")))))

(defun vcm-location (slot index)
  (let ((x (option-image slot)))
    (ascii-string (c-str-copy x (+ 4 (* 84. index))))))

(defun print-vcmem-options (slot)
  (format t "~&    ~15a~a" "location" (vcm-location slot 0)))

(defun print-quad-options (slot)
  (dotimes (i 4)
    (format t "~&    ~15a~a" "location" (vcm-location slot i))))

(defun print-sdu-options (slot)
  (let ((x (option-image slot)))
    (format t "~&    ~20a~a" "nubus code size" (get-n-bytes x 0 4))
    (format t "~&    ~20a~a" "user-def area size" (get-n-bytes x 4 4))
    (format t "~&    ~20a~a" "user-def map pages" (get-n-bytes x 8 4))))

(defun bad-mem-addr (opt index)
  (get-n-bytes opt (+ 12. (* 8 index)) 4))

(defun bad-mem-size (opt index)
  (get-n-bytes opt (+ 16. (* 8 index)) 4))

(defun print-memory-options (slot)
  (let ((x (option-image slot)))
    (format t "~&    ~20a~a" "bad-list size" (get-n-bytes x 0 2))
    (format t "~&    ~20a~a" "n bad sections" (get-n-bytes x 2 2))
    (dotimes (i (get-n-bytes x 2 2))
      (format t "~&     addr=~16r size=~d" (bad-mem-addr x i) (bad-mem-size x i)))))
