;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:8; Readtable:T -*-

; replace old remote-disk-handler with one in this file.
; use pace's eval-server-on goop to find version of remote system.
; barf if remote system is <= 104.
; <= 102 has fatal bug in remote disk,
; if not "READ" then is "WRITE"

; bobp 10/85
; receive a root-image from a remote machine
;   copy the first 7965 blocks of the disk
;   clear the block-10 mini-label

(defconst root-image-size 7965.)
(defconst transfer-size 75.)
(defconst log-page-size 10.)

(defvar root-image-array nil)

(defun make-root-image-array (size)
  (if (or (null root-image-array)
          (< (length root-image-array)
             (ash size log-page-size)))
      (setq root-image-array (make-array (ash (max root-image-size size) log-page-size)
                                         :type :art-8b))))

(defun receive-root-image (from-machine &optional (size root-image-size))
  "Read the root-image from FROM-MACHINE onto our disk.
   The transfer must complete in one attempt, or must be restarted from scratch."
  (make-root-image-array size)
  (raw-partition-op :read from-machine size)
  (clear-root-image-mini-label)
  (raw-partition-op :write 0 size))

(defun transmit-root-image (to-machine &optional (size root-image-size))
  "Write the root-image from our disk to TO-MACHINE.
   The transfer must complete in one attempt, or must be restarted from scratch."
  (make-root-image-array size)
  (raw-partition-op :read 0 size)
  (clear-root-image-mini-label)
  (raw-partition-op :write to-machine size))

(defun read-partition-into-array (part-name)
  (multiple-value-bind (starting-block blocks-left)
      (find-disk-partition-for-read part-name)
    (make-root-image-array blocks-left)
    (let ((unit 0)
          rqb)
      (unwind-protect
          (progn
            (setq rqb (get-disk-rqb transfer-size))
            (do* ((n-blocks (min transfer-size blocks-left) (min transfer-size blocks-left))
                  (blocks-left blocks-left (- blocks-left n-blocks))
                  (block-number 0 (+ block-number n-blocks)))
                 ((<= blocks-left 0))
              (setf (rqb-npages rqb) n-blocks)
              (format t " ~d" (+ block-number starting-block))
              (si:disk-read rqb unit (+ block-number starting-block))
              (copy-array-portion (rqb-8-bit-buffer rqb)
                                  0
                                  (ash n-blocks log-page-size)
                                  root-image-array
                                  (ash block-number log-page-size)
                                  (ash (+ block-number n-blocks) log-page-size)))

            )
        (return-disk-rqb rqb))
      )))

(defun clear-root-image-mini-label ()
  (fillarray (make-array (ash 1 log-page-size)
                         :type :art-8b
                         :displaced-to root-image-array
                         :displaced-index-offset (ash 10. log-page-size))
             nil))

(defun raw-partition-op (op unit-name blocks-left &optional (starting-block 0))
  (let ((unit (decode-unit-argument unit-name
                                    (selectq op
                                      (:write "writing root-image")
                                      (:read "reading root-image")
                                      (:verify "verifying root-image"))))
        rqb)
    (unwind-protect
        (progn
          (setq rqb (get-disk-rqb transfer-size))
          (do* ((n-blocks (min transfer-size blocks-left) (min transfer-size blocks-left))
                (blocks-left blocks-left (- blocks-left n-blocks))
                (block-number 0 (+ block-number n-blocks)))
               ((<= blocks-left 0))
            (setf (rqb-npages rqb) n-blocks)
            (format t " ~d" (+ block-number starting-block))
            (selectq op
              (:read
               (si:disk-read-physical rqb unit (+ block-number starting-block))
               (copy-array-portion (rqb-8-bit-buffer rqb)
                                   0
                                   (ash n-blocks log-page-size)
                                   root-image-array
                                   (ash block-number log-page-size)
                                   (ash (+ block-number n-blocks) log-page-size)))
              (:write
               (copy-array-portion root-image-array
                                   (ash block-number log-page-size)
                                   (ash (+ block-number n-blocks) log-page-size)
                                   (rqb-8-bit-buffer rqb)
                                   0
                                   (ash n-blocks log-page-size))
               (si:disk-write-physical rqb unit (+ block-number starting-block)))
              (:verify
               (si:disk-read-physical rqb unit (+ block-number starting-block))
               (compare-array-portion (rqb-8-bit-buffer rqb)
                                      0
                                      (ash n-blocks log-page-size)
                                      root-image-array
                                      (ash block-number log-page-size)
                                      (ash (+ block-number n-blocks) log-page-size)))
              )
            ))
      (return-disk-rqb rqb))
    ))

(defun compare-array-portion (from-array from-start from-end
                              to-array to-start to-end)
  (if (not (= (- from-end from-start)
              (- to-end to-start)))
      (ferror nil "compare different size arrays"))
  (do ((i from-start (1+ i))
       (j to-start (1+ j)))
      ((>= i from-end))
    (if (not (= (aref from-array i)
                (aref to-array j)))
        (format t "~&[~d]=~d [~d]=~d"
                i
                (aref from-array i)
                j
                (aref to-array j)))))

; copied from dj:l.io;disk.lisp#362
(DEFUN REMOTE-DISK-HANDLER (OP &REST ARGS)
  (DECLARE (SPECIAL REMOTE-DISK-CONN REMOTE-DISK-STREAM REMOTE-DISK-UNIT))
  (SELECTQ OP
    ((:READ :read-physical)
     (LET ((RQB (CAR ARGS)))
       (LET ((BLOCK (CADR ARGS))
             (N-BLOCKS (rqb-npages rqb)))
         (FORMAT REMOTE-DISK-STREAM "READ~:[~;-PHYSICAL~] ~D ~D ~D~%"
                 (eq op :read-physical)
                 REMOTE-DISK-UNIT BLOCK N-BLOCKS)
         (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
         (DO ((BLOCK BLOCK (1+ BLOCK))
              (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
              (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
              (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
              (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
             ((ZEROP N-BLOCKS)
              (RETURN-ARRAY (PROG1 BLOCK-PKT-3 (SETQ BLOCK-PKT-3 NIL)))
              (RETURN-ARRAY (PROG1 BLOCK-PKT-2 (SETQ BLOCK-PKT-2 NIL)))
              (RETURN-ARRAY (PROG1 BLOCK-PKT-1 (SETQ BLOCK-PKT-1 NIL))))
           ;; Get 3 packets and form a block in the buffer
           ;; RECEIVE-PARTITION-PACKET will throw if it gets to eof.
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
           (RECEIVE-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
           ;; Advance magic strings to next block
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-1 3)
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-2 3)
           (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
                                        (* 4 PAGE-SIZE))
                                     BLOCK-PKT-3 3)))))
    (:WRITE (LET ((RQB (CAR ARGS)))
              (LET ((BLOCK (CADR ARGS))
                    (N-BLOCKS (rqb-npages rqb)))
                (FORMAT REMOTE-DISK-STREAM "WRITE ~D ~D ~D~%" REMOTE-DISK-UNIT BLOCK N-BLOCKS)
                (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT)
                (DO ((BLOCK BLOCK (1+ BLOCK))
                     (N-BLOCKS N-BLOCKS (1- N-BLOCKS))
                     (BLOCK-PKT-1 (GET-DISK-STRING RQB 0 484. T))
                     (BLOCK-PKT-2 (GET-DISK-STRING RQB 121. 484. T))
                     (BLOCK-PKT-3 (GET-DISK-STRING RQB 242. 56. T)))
                    ((ZEROP N-BLOCKS)
                     (RETURN-ARRAY (PROG1 BLOCK-PKT-3 (SETQ BLOCK-PKT-3 NIL)))
                     (RETURN-ARRAY (PROG1 BLOCK-PKT-2 (SETQ BLOCK-PKT-2 NIL)))
                     (RETURN-ARRAY (PROG1 BLOCK-PKT-1 (SETQ BLOCK-PKT-1 NIL))))
                  ;; Transmit three packets from block in buffer
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-1)
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-2)
                  (TRANSMIT-PARTITION-PACKET REMOTE-DISK-CONN BLOCK-PKT-3)
                  ;; Advance magic strings to next block
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-1 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-1 3)
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-2 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-2 3)
                  (%P-STORE-CONTENTS-OFFSET (+ (%P-CONTENTS-OFFSET BLOCK-PKT-3 3)
                                               (* 4 PAGE-SIZE))
                                            BLOCK-PKT-3 3)))))
    (:DISPOSE (CHAOS:CLOSE-CONN REMOTE-DISK-CONN))
    (:UNIT-NUMBER REMOTE-DISK-UNIT)
    (:MACHINE-NAME
     (GET-HOST-FROM-ADDRESS (CHAOS:FOREIGN-ADDRESS REMOTE-DISK-CONN) ':CHAOS))
    (:SAY
      (FORMAT REMOTE-DISK-STREAM "SAY ~A~%" (CAR ARGS))
      (SEND REMOTE-DISK-STREAM ':FORCE-OUTPUT))
    (:HANDLES-LABEL NIL)
    ))
