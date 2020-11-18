;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:ZL -*-

(defvar *ppd* (make-wireable-array (floor (%region-length physical-page-data) page-size)
                                   'art-32b
                                   nil))

(defvar *pht* (make-wireable-array (floor (%region-length page-table-area) page-size)
                                   'art-32b
                                   nil))

(defun read-ppd-and-pht ()
  (without-interrupts
    (wire-wireable-array *ppd* 0 (array-length *ppd*) nil nil)
    (wire-wireable-array *pht* 0 (array-length *pht*) nil nil)
    (%blt (%region-origin physical-page-data)
          (%pointer-plus *ppd* (array-data-offset *ppd*))
          (array-length *ppd*)
          1)
    (%blt (%region-origin page-table-area)
          (%pointer-plus *pht* (array-data-offset *pht*))
          (array-length *pht*)
          1)
    (unwire-wireable-array *ppd* 0 (array-length *ppd*))
    (unwire-wireable-array *pht* 0 (array-length *pht*))))

(defun ppd-pht-index (phys-page-number)
  (%p-ldb-offset (byte 16. 0)
                 *ppd*
                 (+ phys-page-number
                    (array-data-offset *ppd*))))

;  %%PHT1-VIRTUAL-PAGE-NUMBER 1021              ;ALIGNED SAME AS VMA
;  %PHT-DUMMY-VIRTUAL-ADDRESS 377777            ;ALL ONES MEANS THIS IS DUMMY ENTRY
                                                ;WHICH JUST REMEMBERS A FREE CORE PAGE
(defun pht-virtual-page-number (pht-index)
  (%p-ldb-offset %%pht1-virtual-page-number
                 *pht*
                 (+ pht-index
                    (array-data-offset *pht*))))

;  %%PHT1-SWAP-STATUS-CODE 0003
;  %PHT-SWAP-STATUS-NORMAL 1                    ;ORDINARY PAGE
;  %PHT-SWAP-STATUS-FLUSHABLE 2                 ;SAFELY REUSABLE TO SWAP PAGES INTO
;                                               ;MAY NEED TO BE WRITTEN TO DISK FIRST
;  %PHT-SWAP-STATUS-PREPAGE 3                   ;SAME AS FLUSHABLE, BUT CAME IN VIA PREPAGE
;  %PHT-SWAP-STATUS-AGE-TRAP 4                  ;LIKE NORMAL BUT TRYING TO MAKE FLUSHABLE
;  %PHT-SWAP-STATUS-WIRED 5                     ;NOT SWAPPABLE

(defun pht-swap-status-code (pht-index)
  (%p-ldb-offset %%pht1-swap-status-code
                 *pht*
                 (+ pht-index
                    (array-data-offset *pht*))))

;  %%PHT1-AGE 0302                              ;NUMBER OF TIMES AGED
(defun pht-age (pht-index)
  (%p-ldb-offset %%pht1-age
                 *pht*
                 (+ pht-index
                    (array-data-offset *pht*))))

;  %%PHT1-MODIFIED-BIT 0501                     ;1 IF PAGE MODIFIED, BUT THE FACT NOT RECORDED
                                                ; IN THE MAP-STATUS, BECAUSE IT IS NOMINALLY
                                                ;  READ-ONLY OR NOMINALLY READ-WRITE-FIRST.
(defun pht-modified-bit (pht-index)
  (%p-ldb-offset %%pht1-modified-bit
                 *pht*
                 (+ pht-index
                    (array-data-offset *pht*))))

;  %%PHT1-VALID-BIT 0601                                ;1 IF THIS HASH TABLE SLOT IS OCCUPIED.
(defun pht-valid-bit (pht-index)
  (%p-ldb-offset %%pht1-valid-bit
                 *pht*
                 (+ pht-index
                    (array-data-offset *pht*))))

;  %%PHT2-MAP-STATUS-CODE 3403
;  %PHT-MAP-STATUS-MAP-NOT-VALID 0              ;LEVEL 1 OR 2 MAP NOT SET UP
;  %PHT-MAP-STATUS-META-BITS-ONLY 1             ;HAS META BITS BUT NO PHYSICAL ADDRESS
;  %PHT-MAP-STATUS-READ-ONLY 2                  ;GARBAGE COLLECTOR CAN STILL WRITE IN IT
;  %PHT-MAP-STATUS-READ-WRITE-FIRST 3           ;READ/WRITE BUT NOT MODIFIED
;  %PHT-MAP-STATUS-READ-WRITE 4                 ;READ/WRITE AND MODIFIED
;  %PHT-MAP-STATUS-PDL-BUFFER 5                 ;MAY RESIDE IN PDL BUFFER
;  %PHT-MAP-STATUS-MAR 6                                ;MAR SET SOMEWHERE ON THIS PAGE
(defun pht-map-status-code (pht-index)
  (%p-ldb-offset %%pht2-map-status-code
                 *pht*
                 (+ pht-index 1 (array-data-offset *pht*))))

;  %%PHT2-MAP-ACCESS-CODE 3602
(defun pht-map-access-code (pht-index)
  (%p-ldb-offset %%pht2-map-access-code
                 *pht*
                 (+ pht-index 1 (array-data-offset *pht*))))

;  %%PHT2-PHYSICAL-PAGE-NUMBER 0026
(defun pht-physical-page-number (pht-index)
  (%p-ldb-offset %%pht2-physical-page-number
                 *pht*
                 (+ pht-index 1 (array-data-offset *pht*))))

(defun dump-ppd ()
  (format t "~&s system; - free; . normal; f flushable; p prepage; a age trap; w wired")
  (dotimes (phys-page-number (floor (aref #'system-communication-area  %sys-com-memory-size) page-size))
    (if (zerop (ldb (byte 6 0) phys-page-number))
        (format t "~&~6o: " phys-page-number))
    (let ((pht-index (ppd-pht-index phys-page-number)))
      (cond ((= pht-index #o177777) (format t "s"))
            ((oddp pht-index) (ferror nil "bad pht-index"))
            ((= (pht-virtual-page-number pht-index) %pht-dummy-virtual-address) (format t "-"))
            (t (select (pht-swap-status-code pht-index)
                 (%PHT-SWAP-STATUS-NORMAL (format t "."))
                 (%PHT-SWAP-STATUS-FLUSHABLE (format t "f"))
                 (%PHT-SWAP-STATUS-PREPAGE (format t "p"))
                 (%PHT-SWAP-STATUS-AGE-TRAP (format t "a"))
                 (%PHT-SWAP-STATUS-WIRED (format t "w"))
                 (t (ferror nil "unknown swap status"))))))))
