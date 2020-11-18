;;; -*- Mode:LISP; Package:METER; Base:8; Readtable:CL -*-

;;; Easy use of this file:
;;; read into buffer and compile, or load an up to data qfasl
;;; then, call (write-page-trace-to-file [file-name])
;;; After a while, do (setq *stop-page-tracer* t) and
;;; the output file will automatically be closed.
;;; The file should probably be on the local file system, and
;;; certainly has to be a lisp machine file system.
;;; Format of the file:
;;;
;;;    32bits   virtual page number
;;;    16bits   area number of the pointer at the time of the fault
;;;             or 0 if it was ambiguous due to GC

;;; You can do (print-page-trace-file [file-name]) to see the results.
;;; For the area-name printout to be valid, you should do it on
;;; the same machine that wrote the page-trace file, and perhaps
;;; save the output of the print-page-trace-file program.

;;; When the system does a multiple page swap, each page is entered
;;; separatly in the file.  Therefore, you can't tell the difference
;;; between pages from a multiple page swap, and sequential faults.
;;; Almost all regions have swaping quantum 3.  You can find the
;;; other by running (get-region-swap-quantums).

(defun region-number->area-name (region-number)
  (let ((bits (%p-ldb %%q-pointer (+ (si:%region-origin si:region-bits) region-number))))
    (when (not (= (ldb si:%%region-space-type bits) si:%region-space-free))
      (aref #'area-name (%p-ldb (byte 16. 0) (+ (si:%region-origin si:region-area-map) region-number))))))

(defun get-region-swap-quantums ()
  (let ((rb (si:%region-origin si:region-bits)))
    (dotimes (i (si:%region-length si:region-bits))
      (let ((q (%p-ldb si:%%region-swapin-quantum (+ rb i)))
            (type (%p-ldb si:%%region-space-type (+ rb i))))
      (when (and (not (= type si:%region-space-free))
                 (not (= q 3)))
        (format t "~&~s ~d."
                (aref #'area-name
                      (%p-ldb (byte 16. 0) (+ (si:%region-origin si:region-area-map) i)))
                q))))))


;;callers of %create-physical-page:
;;  set-memory-size si:%write-page si:page-in-words-new si:page-in-words-old

;;callers of %change-page-status
; CLEAR-MAR SET-MAR SYSTEM:PAGE-OUT-WORDS
;SI::%MAKE-REGION-READ-ONLY SI::%UNWIRE-PAGE
;SI::%MAKE-PAGE-READ-ONLY SI::%WIRE-PAGE
;SI::REALLY-PAGE-OUT-PAGE SI::%INVALIDATE-REGION-MAPPING
;SI::%DEALLOCATE-PAGES SI::%MAKE-REGION-NOT-READ-ONLY

(defvar *page-trace-buffer* (si:make-wireable-array 128. 'art-32b nil))
(defvar *page-trace-out-ptr*)

(defun turn-on-page-trace ()
  (when (or (and (> (%pointer *page-trace-buffer*) 0)
                 (< (%pointer-plus *page-trace-buffer*
                                   (+ (array-length *page-trace-buffer*)
                                      (si:array-data-offset *page-trace-buffer*))) 0))
            (and (< (%pointer *page-trace-buffer*) 0)
                 (> (%pointer-plus *page-trace-buffer*
                                   (+ (array-length *page-trace-buffer*)
                                      (si:array-data-offset *page-trace-buffer*))) 0)))
    (ferror nil "bad page-trace-buffer"))
  (without-interrupts
    (si:%wire-structure *page-trace-buffer*)
    (si:%page-trace *page-trace-buffer*)
    (setq *page-trace-out-ptr*
          (%pointer-plus *page-trace-buffer*
                         (si:array-data-offset *page-trace-buffer*)))))

(defun turn-off-page-trace ()
  (si:%page-trace nil)
  (si:%unwire-structure *page-trace-buffer*))

;       Microsecond clock value
;       Virtual address
;       Miscellany:
;        bit 31: swap-out flag,
;        bit 30: stack-group-switch flag
;        bit 29: transport flag
;        bit 28: scavenge flag
;        bits 15-0: micro-pc
;       Current function (just randomly picks up @M-AP, hopefully reasonable)

(defun print-page-trace ()
  (labels ((aref-32 (array index)
             (let ((data-offset (+ index (si:array-data-offset array))))
               (dpb (%p-ldb-offset (byte 16. 16.) array data-offset)
                    (byte 16. 16.)
                    (%p-ldb-offset (byte 16. 0) array data-offset)))))
    (do ((i 0 (+ i 4)))
        ((= i (array-length *page-trace-buffer*)))
      (format t "~&~10d ~15o ~15a ~15o"
              (aref-32 *page-trace-buffer* i)
              (aref-32 *page-trace-buffer* (+ i 1))
              (lam:lam-find-closest-sym
                (+ lam:racmo (ldb (byte 16. 0) (aref-32 *page-trace-buffer* (+ i 2)))))
              (aref-32 *page-trace-buffer* (+ i 3))))))


micro:
(define-micro-function meter:page-trace-ptr ()
  (popj-after-next (m-tem) a-page-trace-ptr)
  ((m-t) q-pointer m-tem (a-constant (byte-value q-data-type dtp-fix))))

micro:
(define-micro-function meter:page-trace-start ()
  (popj-after-next (m-tem) a-page-trace-start)
  ((m-t) q-pointer m-tem (a-constant (byte-value q-data-type dtp-fix))))

micro:
(define-micro-function meter:page-trace-end ()
  (popj-after-next (m-tem) a-page-trace-end)
  ((m-t) q-pointer m-tem (a-constant (byte-value q-data-type dtp-fix))))

(defun next-page-trace-record ()
  (without-interrupts
    (let ((out-ptr *page-trace-out-ptr*)
          (in-ptr (page-trace-ptr)))
      (cond ((eq out-ptr in-ptr) nil)
            (t
             (let ((vadr (%p-ldb (byte 17. 8) (+ out-ptr 1)))
                   (padr (%p-ldb %%q-pointer (+ out-ptr 3)))
                   (swap-out-flag (if (zerop (%p-ldb (byte 1 31.) (+ out-ptr 2))) :in :out)))
               (when (eq swap-out-flag :in)
                 (setq padr (ldb (byte 14. 8) padr)))
               (incf out-ptr 4)
               (when (eq out-ptr (page-trace-end))
                 (setq out-ptr (page-trace-start)))
               (setq *page-trace-out-ptr* out-ptr)
               (values vadr padr swap-out-flag)))))))

(defun page-trace-record-available ()
  (not (eq *page-trace-out-ptr* (page-trace-ptr))))

(defun page-trace ()
  (do-forever
    (process-wait "Page Fault" #'page-trace-record-available)
    (print (multiple-value-list (next-page-trace-record)))))

(defvar *page-trace-window* (make-instance 'tv:window
                                           :inside-size '(256. 512.)))

(defun process-page-fault (vadr padr direction)
    padr
    (let ((screen-array (send *page-trace-window* :screen-array))
          (top-margin (send *page-trace-window* :top-margin-size))
          (left-margin (send *page-trace-window* :left-margin-size)))
      (when (and vadr
                 screen-array
                 (send *page-trace-window* :exposed-p))
        (setf (ar-2-reverse screen-array
                            (+ (ldb (byte 8 0) vadr) left-margin)
                            (+ (ldb (byte 9 8) vadr) top-margin))
              (if (eq direction :out) 0 1)))))



(defun initialize-page-trace-window ()
  (let ((phys-pages (floor (aref #'sys:system-communication-area sys:%sys-com-memory-size) 256.))
        (screen-array (send *page-trace-window* :screen-array))
        (top-margin (send *page-trace-window* :top-margin-size))
        (left-margin (send *page-trace-window* :left-margin-size)))
    (when (and screen-array
               (send *page-trace-window* :exposed-p))
      (do ((pht-index 0 (+ pht-index 2))
           (pht-adr (si:%region-origin si:page-table-area))
           (end-pht-index  (cond ((< %microcode-version-number 1660.)
                                 (* phys-pages 4))
                                (t
                                 (si:%region-length si:page-table-area)))))
          ((= pht-index end-pht-index))
        (when (not (zerop (%p-ldb si:%%pht1-valid-bit (+ pht-index pht-adr))))
          (let ((vpage (%p-ldb si:%%pht1-virtual-page-number (+ pht-index pht-adr))))
            (setf (ar-2-reverse screen-array
                                (+ (ldb (byte 8 0) vpage) left-margin)
                                (+ (ldb (byte 9 8) vpage) top-margin))
                  1)))))))

(defun watch-page-faults ()
  (send *page-trace-window* :expose)
  (tv:turn-off-sheet-blinkers *page-trace-window*)
  (send *page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-page-trace-window))
  (do-forever
    (process-wait "Page Fault" #'page-trace-record-available)
    (process-page-fault)))

(defvar *phys-page-trace-window* (make-instance 'tv:window
                                                :inside-size '(128. 128.)))


(defun watch-physical-page-activity ()
  (send *phys-page-trace-window* :expose)
  (send (car (send *phys-page-trace-window* :blinker-list))
        :set-visibility :off)
  (send *phys-page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-phys-page-trace-window)
    (setq *page-trace-out-ptr* (page-trace-ptr))
    )
  (do-forever
    (process-wait "Page Fault" #'page-trace-record-available)
    (process-page-fault-physical)))

(defun watch-both ()
  (send *phys-page-trace-window* :expose)
  (send (car (send *phys-page-trace-window* :blinker-list))
        :set-visibility :off)
  (send *phys-page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-phys-page-trace-window)
    (setq *page-trace-out-ptr* (page-trace-ptr))
    )
  (send *page-trace-window* :expose)
  (tv:turn-off-sheet-blinkers *page-trace-window*)
  (send *page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-page-trace-window))

  (do-forever
    (process-wait "Page Fault" #'page-trace-record-available)
    (multiple-value-bind (vpage ppage direction)
        (next-page-trace-record)
      (process-page-fault-physical vpage ppage direction)
      (process-page-fault vpage ppage direction))))

(defun watch-both-clock-function ()
  (send *phys-page-trace-window* :expose)
  (send (car (send *phys-page-trace-window* :blinker-list))
        :set-visibility :off)
  (send *phys-page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-phys-page-trace-window)
    (setq *page-trace-out-ptr* (page-trace-ptr))
    )
  (send *page-trace-window* :expose)
  (tv:turn-off-sheet-blinkers *page-trace-window*)
  (send *page-trace-window* :clear-screen)
  (without-interrupts
    (initialize-page-trace-window))

  (pushnew 'page-trace-clock-function
           si:clock-function-list))

(defun setup-both-windows ()
  (without-interrupts
    (send *phys-page-trace-window* :expose)
    (send (car (send *phys-page-trace-window* :blinker-list))
          :set-visibility :off)
    (send *phys-page-trace-window* :clear-screen)
    (without-interrupts
      (initialize-phys-page-trace-window)
      (setq *page-trace-out-ptr* (page-trace-ptr))
      )
    (send *page-trace-window* :expose)
    (tv:turn-off-sheet-blinkers *page-trace-window*)
    (send *page-trace-window* :clear-screen)
    (without-interrupts
      (initialize-page-trace-window))))

(defun page-trace-clock-function (&rest ignore)
  (multiple-value-bind (nil error-p)
      (ignore-errors
        (do-forever
          (multiple-value-bind (vpage ppage direction)
              (next-page-trace-record)
            (when (null vpage)
              (return nil))
            (process-page-fault-physical vpage ppage direction)
            (process-page-fault vpage ppage direction))))
    (when error-p
      (setq si:clock-function-list
            (remq 'page-trace-clock-function si:clock-function-list)))))

(defun phys-page-bit (page)
  (let ((screen-array (send *phys-page-trace-window* :screen-array))
        (top-margin (send *phys-page-trace-window* :top-margin-size))
        (left-margin (send *phys-page-trace-window* :left-margin-size)))
    (ar-2-reverse screen-array
                  (+ (ldb (byte 7 0) page) left-margin)
                  (+ (ldb (byte 7 7) page) top-margin))))

(defun set-phys-page-bit (page bit)
  (let ((screen-array (send *phys-page-trace-window* :screen-array))
        (top-margin (send *phys-page-trace-window* :top-margin-size))
        (left-margin (send *phys-page-trace-window* :left-margin-size)))
    (as-2-reverse bit
                  screen-array
                  (+ (ldb (byte 7 0) page) left-margin)
                  (+ (ldb (byte 7 7) page) top-margin))))



(defun initialize-phys-page-trace-window (&optional clear-p)
  (when clear-p (send *phys-page-trace-window* :clear-screen))
  (let ((phys-pages (floor (aref #'sys:system-communication-area sys:%sys-com-memory-size)
                           256.))
        (screen-array (send *phys-page-trace-window* :screen-array))
        (top-margin (send *phys-page-trace-window* :top-margin-size))
        (left-margin (send *phys-page-trace-window* :left-margin-size)))
    (when (and screen-array
               (send *phys-page-trace-window* :exposed-p))
      (do ((pht-index 0 (+ pht-index 2))
           (pht-adr (si:%region-origin si:page-table-area))
           (end-pht-index (cond ((< %microcode-version-number 1660.)
                                 (* phys-pages 4))
                                (t
                                 (si:%region-length si:page-table-area)))))
          ((= pht-index end-pht-index))
        (when (and (not (zerop (%p-ldb si:%%pht1-valid-bit (+ pht-index pht-adr))))
                   (not (= (%p-ldb si:%%pht1-virtual-page-number (+ pht-index pht-adr))
                           si:%PHT-DUMMY-VIRTUAL-ADDRESS)))
          (let ((ppage (%p-ldb si:%%pht2-physical-page-number (+ 1 pht-index pht-adr))))
            (setf (ar-2-reverse screen-array
                                (+ (ldb (byte 7 0) ppage) left-margin)
                                (+ (ldb (byte 7 7) ppage) top-margin))
                  1))))
      )))

(defun show-dummy-pages ()
  (let ((phys-pages (floor (aref #'sys:system-communication-area sys:%sys-com-memory-size)
                           256.))
        (screen-array (send *phys-page-trace-window* :screen-array))
        (top-margin (send *phys-page-trace-window* :top-margin-size))
        (left-margin (send *phys-page-trace-window* :left-margin-size)))
    (when (and screen-array
               (send *phys-page-trace-window* :exposed-p))
      (do ((pht-index 0 (+ pht-index 2))
           (pht-adr (si:%region-origin si:page-table-area))
           (end-pht-index  (cond ((< %microcode-version-number 1660.)
                                 (* phys-pages 4))
                                (t
                                 (si:%region-length si:page-table-area)))))
          ((= pht-index end-pht-index))
        (when (and (not (zerop (%p-ldb si:%%pht1-valid-bit (+ pht-index pht-adr))))
                   (= (%p-ldb si:%%pht1-virtual-page-number (+ pht-index pht-adr))
                      si:%PHT-DUMMY-VIRTUAL-ADDRESS))
          (let ((ppage (%p-ldb si:%%pht2-physical-page-number (+ 1 pht-index pht-adr))))
            (setf (ar-2-reverse screen-array
                                (+ (ldb (byte 7 0) ppage) left-margin)
                                (+ (ldb (byte 7 7) ppage) top-margin))
                  1))))
      )))

(defun process-page-fault-physical (vpage ppage direction)
    (let ((screen-array (send *phys-page-trace-window* :screen-array))
          (top-margin (send *phys-page-trace-window* :top-margin-size))
          (left-margin (send *phys-page-trace-window* :left-margin-size)))
      (when (and vpage
                 screen-array
                 (send *phys-page-trace-window* :exposed-p))
        (setf (ar-2-reverse screen-array
                            (+ (ldb (byte 7 0) ppage) left-margin)
                            (+ (ldb (byte 7 7) ppage) top-margin))
              (if (eq direction :out) 0 1))
        )))

(defun read-a-mem (a-mem-loc)
  (let ((loc (micro:a-mem-lookup (intern (string a-mem-loc) "MICRO"))))
    (when (null loc)
      (ferror nil "unknown a-mem name"))
    (micro:%read-a-mem loc)))

(defun print-ptrs ()
  (do-forever
    (format t "~&~10s ~10s ~10s ~10s "
            (read-a-mem 'a-findcore-scan-pointer)
            (read-a-mem 'a-aging-scan-pointer)
            (si:read-meter 'si:%count-disk-page-reads)
            (si:read-meter 'si:%count-disk-page-writes))))

(defun show-findcore-pointer ()
  (let ((screen-array (send *phys-page-trace-window* :screen-array))
        (top-margin (send *phys-page-trace-window* :top-margin-size))
        (left-margin (send *phys-page-trace-window* :left-margin-size))
        (page-number (read-a-mem 'a-findcore-scan-pointer)))
    (unwind-protect
        (do-forever
          (setf (ar-2-reverse screen-array
                              (+ (ldb (byte 7 0) page-number) left-margin)
                              (+ (ldb (byte 7 7) page-number) top-margin))
                1)
          (process-sleep 30.)
          (setf (ar-2-reverse screen-array
                              (+ (ldb (byte 7 0) page-number) left-margin)
                              (+ (ldb (byte 7 7) page-number) top-margin))
                0)
          (process-sleep 30.))
      (setf (ar-2-reverse screen-array
                          (+ (ldb (byte 7 0) page-number) left-margin)
                          (+ (ldb (byte 7 7) page-number) top-margin))
            1))))


(defvar *pht* (si:make-wireable-array #o400 'art-32b nil))
(defvar *ppd* (si:make-wireable-array #o100 'art-32b nil))

(defun page-table-snapshot ()
  (si:%wire-structure *pht*)
  (si:%wire-structure *ppd*)
  (without-interrupts
    (%blt (si:%region-origin si:page-table-area)
          (%pointer-plus *pht* (si:array-data-offset *pht*))
          (array-length *pht*)
          1)
    (%blt (si:%region-origin si:physical-page-data)
          (%pointer-plus *ppd* (si:array-data-offset *ppd*))
          (array-length *ppd*)
          1))
  (si:%unwire-structure *pht*)
  (si:%unwire-structure *ppd*))

(defun page-table-ref (a index)
  (let ((offset (si:array-data-offset a)))
    (dpb (%p-ldb-offset (byte 16. 16.) a (+ offset index))
         (byte 16. 16.)
         (%p-ldb-offset (byte 16. 0) a (+ offset index)))))

(defun find-all-phys-pages ()
  (let (
        (unusable 0)
        (odd-ppd-entries 0)
        (ppd-entries-pointing-to-invalid 0)
        (inconsistancies 0)
        (dummies 0)
        (used 0)
        )
    (send *phys-page-trace-window* :clear-screen)
    (dotimes (page (array-length *ppd*))
      (let ((ppd-entry (ldb (byte 16. 0) (page-table-ref *ppd* page) )))
        (cond ((= ppd-entry #o177777)
               (set-phys-page-bit page 1)
               (incf unusable))
              ((oddp ppd-entry)
               (incf odd-ppd-entries))
              (t
               (let ((pht1 (page-table-ref *pht* ppd-entry))
                     (pht2 (page-table-ref *pht* (1+ ppd-entry))))
                 (cond ((zerop (ldb si:%%pht1-valid-bit pht1))
                        (incf ppd-entries-pointing-to-invalid))
                       ((not (= page (ldb si:%%pht2-physical-page-number pht2)))
                        (incf inconsistancies))
                       ((= (ldb si:%%pht1-virtual-page-number pht1)
                           si:%PHT-DUMMY-VIRTUAL-ADDRESS)
                        (incf dummies))
                       (t
                        (set-phys-page-bit page 1)
                        (incf used))))))))
    (format t "~&Used ~d; Dummies ~d; Inconsistancies ~d; odd ~d; unusable ~d; Invalid ~d"
            used dummies inconsistancies odd-ppd-entries unusable
            ppd-entries-pointing-to-invalid)))



;; evaluate this to turn off multiple page swaps
;; (setf (ldb (byte 1 3) %disk-switches) 0)


(defvar *stop-page-tracer*)

(defun write-page-trace-to-file (&optional (file-name "lm:pace;page-trace-out.bin"))
  (setq si:clock-function-list
        (remq 'page-trace-clock-function si:clock-function-list))
  (setq *stop-page-tracer* nil)
  (process-run-function
    "Page Tracer"
    #'(lambda ()
        (with-open-file (stream file-name :direction :output :characters nil)
          (turn-on-page-trace)
          (do-forever
            (process-wait "Page Fault" #'(lambda ()
                                           (or *stop-page-tracer*
                                               (page-trace-record-available))))
            (when *stop-page-tracer*
              (return nil))
            (multiple-value-bind (vpage ppage direction)
                (next-page-trace-record)
              ppage direction
              (when (and vpage (not (= vpage #o377777)))
                (send stream :tyo (ldb (byte 16. 0) vpage))
                (send stream :tyo (ldb (byte 16. 16.) vpage))
                (send stream :tyo (or (%area-number (lsh vpage 8)) 0))
                ))
            )))))

(defun print-page-trace-file (&optional (file-name "lm:pace;page-trace-out.bin"))
  (with-open-file (stream file-name :characters nil)
    (do ((column 0))
        (())
      (let* ((c0 (send stream :tyi))
             (c1 (send stream :tyi))
             (area-number (send stream :tyi)))
        (when (or (null c0) (null c1) (null area-number))
          (return nil))
        (when (zerop (ldb (byte 1 0) column))
          (setq column 0)
          (format t "~&"))
        (let ((page (dpb c1 (byte 16. 16.) c0)))
          (incf column)
          (format t "~8d ~a~40t" page (area-name area-number)))))))


;  shared area 177300000 .. 64 pages

;(defflavor memory-trace
;        ((output-stream nil)
;         )
;        (si:buffered-input-stream)
;  :settable-instance-variables)

;(defmethod (memory-trace :after :init) (ignore)
;  (when (null output-stream)
;    (ferror nil "must provide output-stream"))


(defvar *dirty-page-timer-start*)
(defvar *dirty-page-accumulator*)
(defvar *page-read-in-times* (make-array (ash 1 17.) :initial-element 0))

(defun print-read-in-times ()
  (dotimes (i (array-length *page-read-in-times*))
    (when (not (zerop (aref *page-read-in-times* i)))
      (format t "<~o ~o>" i (aref *page-read-in-times* i)))))

(defun dirty-page-timer ()
  (mapcar #'(lambda (p)
              (when (string-equal (send p :name) "Page Timer")
                (send p :kill)))
          si:all-processes)
  (si:process-allow-schedule)
  (array-initialize *page-read-in-times* 0)
  (setq si:clock-function-list
        (remq 'page-trace-clock-function si:clock-function-list))
  (setq *dirty-page-timer-start* (time))
  (setq *dirty-page-accumulator* 0)
  (process-run-function
    "Page Timer"
    #'(lambda ()
        (turn-on-page-trace)
        (do-forever
          (process-wait "Page Fault" #'(lambda () (page-trace-record-available)))
          (multiple-value-bind (vpage ppage direction)
              (next-page-trace-record)
            vpage
            (cond ((eq direction :in)
                   (setf (aref *page-read-in-times* ppage) (time)))
                  ((zerop (aref *page-read-in-times* ppage)))
                  (t
                   (incf *dirty-page-accumulator*
                         (time-difference (time) (aref *page-read-in-times* ppage))))))))))

;(DEFUN CLEAN-DIRTY-PAGES ()
;  (DO ((ADR (%REGION-ORIGIN PAGE-TABLE-AREA) (+ ADR 2))
;       (N (TRUNCATE (AREF (SYMBOL-FUNCTION 'SYSTEM-COMMUNICATION-AREA) %SYS-COM-PAGE-TABLE-SIZE)
;                   2)
;         (1- N))
;       (N-DIRTY 0))
;      ((ZEROP N)
;       N-DIRTY)
;    (COND ((AND (NOT (ZEROP (%P-LDB %%PHT1-VALID-BIT ADR)))
;               (NOT (= (%P-LDB %%PHT1-SWAP-STATUS-CODE ADR) %PHT-SWAP-STATUS-WIRED))
;               (OR (= (%P-LDB %%PHT1-MODIFIED-BIT ADR) 1)
;                   (= (%P-LDB %%PHT2-MAP-STATUS-CODE (1+ ADR))
;                      %PHT-MAP-STATUS-READ-WRITE)))
;          (SETQ N-DIRTY (1+ N-DIRTY))
;          (REALLY-PAGE-OUT-PAGE (LSH (%P-LDB %%PHT1-VIRTUAL-PAGE-NUMBER ADR) 8.))))))

(defun print-dirty-page-timer ()
  (let ((start *dirty-page-timer-start*)
        (acc *dirty-page-accumulator*)
        (now (time)))
    (float (/ acc (* (ash (si:system-communication-area si:%sys-com-memory-size) -8)
                     (time-difference now start))))))

