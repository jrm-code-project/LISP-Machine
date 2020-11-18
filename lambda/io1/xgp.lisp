;;; -*- Mode:LISP; Package:System-Internals; Base:8; Lowercase:T -*-
;;; ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;; Routines for sending scan files to the XGP from bit arrays in the LISP machine

;;; TODO:  Use TRUENAME in XGP-QUEUE (when TRUENAME exists)
;;;        Use a separate file connection
;;;        Make (XGP) work without relying on HACKS QFASL

;;; There are screen, window, and array hardcopy functions, and alternate versions
;;; which run in a background process (scanline encoding is SLOW).
;;; Below <scan-file> is optional and defaults to "AI:.TEMP.;LMSCN >"
;;;
;;; (SCREEN-XGP-HARDCOPY <screen> <scan-file>)  - foreground hardcopy of <screen>
;;;                                               (defaults to tv:default-screen)
;;; (SCREEN-XGP-HARDCOPY-BACKGROUND <screen> <scan-file>) - background hardcopy
;;;
;;; (WINDOW-XGP-HARDCOPY <window> <scan-file>)  - <window> defaults to selected-window
;;;
;;; (WINDOW-XGP-HARDCOPY-BACKGROUND <window> <scan-file>) - background hardcopy of <window>
;;;
;;; Note: the WINDOW and SCREEN versions are identical now except for the argument default.
;;;
;;; (XGP-WRITE-SCAN-FILE <scan-file> <array> ...)
;;;
;;; (XGP-QUEUE-SCAN-FILE <scan-file>) - write a queue request file for <scan-file>

(defvar xgp-delete-scan-file t)                 ;delete the scan file after printing
(defvar xgp-queue-scan-file t)                  ;queue the scan file
(defvar xgp-queue-filename "AI:.XGPR.;    Q1 >")    ;filename for queue requests
(defvar xgp-scan-file-name "AI:.TEMP.;LMSCN >") ;Filename for scan file
                                                ;Note that is auto deleted when 1 day old

;Interface to the printer-type XGP.  PRINTER is (:XGP scan-file-name queue-file-name).
(defun (:xgp si:print-bit-array) (printer array left top right bottom &rest options)
  options
  (let ((xgp-queue-file-name (caddr printer))
        (xgp-scan-file-name (cadr printer))
        file)
    (setq file
      (xgp-write-scan-file xgp-scan-file-name array left top right bottom))
    (if xgp-queue-file-name
        (xgp-queue-scan-file file))))

;;; These functions are mostly subsumed by the general ESC Q mechanism.
;;; But somebody might want them.
(defun screen-xgp-hardcopy (&optional (screen tv:default-screen)
                                      (file xgp-scan-file-name))
   (window-xgp-hardcopy screen file))

(defun screen-xgp-hardcopy-background (&optional (screen tv:default-screen)
                                                 (file xgp-scan-file-name))
   (window-xgp-hardcopy-background screen file))

(defun window-xgp-hardcopy (&optional (window tv:selected-window)
                                      (file xgp-scan-file-name))
  (multiple-value-bind (width height) (funcall window ':size)
    (let ((array (tv:sheet-screen-array window)))
      (cond ((null array) (ferror nil "Window ~S has no bits now" window))
            (t (setq file (xgp-write-scan-file file array 0 0 width height))
               (beep)   ;Bits have been copied now
               (if xgp-queue-scan-file (xgp-queue-scan-file file))
               "Hardcopy Done")))))

(defun window-xgp-hardcopy-background (&optional (window tv:selected-window)
                                                 (file xgp-scan-file-name))
  (multiple-value-bind (width height) (funcall window ':size)
    (let ((array (tv:sheet-screen-array window)))
      (cond ((null array) (ferror nil "Window ~S has no bits now" window))
            (t (start-xgp-hardcopy-background-process file array width height))))))

(defun start-xgp-hardcopy-background-process (file array xdim ydim)
   (let ((hcarray (allocate-resource 'tv:hardcopy-bit-array)))
     (tv:snapshot-screen array hcarray xdim ydim)
     (process-run-function "XGP Hardcopy"
        #'xgp-hardcopy-snapshot-array file hcarray xdim ydim)
     "Hardcopy Process Started"))

(defun xgp-hardcopy-snapshot-array (file array xdim ydim)
  (unwind-protect
    (progn
      (setq file (xgp-write-scan-file file array 0 0 xdim ydim))
      (if xgp-queue-scan-file (xgp-queue-scan-file file)))
    (deallocate-resource 'tv:hardcopy-bit-array array)))

(defun (:xgp tv:kbd-esc-q-function) (array xdim ydim)
  (xgp-hardcopy-snapshot-array xgp-scan-file-name array xdim ydim))

;;; Write a print request file for the XGP queue
;;; Note, this file should use TRUENAME on the file name when that function
;;; exists.
(defun xgp-queue-scan-file (&optional (file xgp-scan-file-name))
   (let ((q-stream (open xgp-queue-filename ':out))
         (date-time (time:print-current-time nil))
         (midpt))
     (setq midpt (string-search-char #\sp date-time)
           date-time (string-append (substring date-time (1+ midpt)) " "
                                    (substring date-time 0 midpt)))
     (format q-stream ";Status ~A       LM ~C ~A  1 page        ~A ~%"
             user-id fs:user-group-affiliation date-time file)
     (if xgp-delete-scan-file (format q-stream ";DELETE~%"))
     (format q-stream ";SCAN~2%~A~%" file)
     (close q-stream)))

;;; SCAN compress an array into a file with optional offsets
(defun xgp-write-scan-file (file array
                            &optional (left 0) (top 0)
                                      (right  (array-dimension array 0))
                                      (bottom (array-dimension array 1))
                                      (topmar 128.)
                                      (lftmar 90.))
     (if (not (eq 'art-1b (array-type array)))
         (ferror nil "~s is not a bit array." array))
     (if (not (and (array-in-bounds-p array left top)
                   (array-in-bounds-p array (1- right) (1- bottom))))
         (ferror nil "(~s,~s) or (~s,~s) is out of array bounds." top left bottom right))
     (let ((8buf (make-array 208. ':type 'art-8b ':leader-list '(0)))
           (16buf)
           (xgp-stream (open file '(:out :fixnum))))
          (setq 16buf (make-array 104.
                                  ':type 'art-16b
                                  ':displaced-to 8buf
                                  ':leader-list '(0)))
          (do ((i top (1+ i)) (lineno topmar (1+ lineno)))
              (( i bottom))
              (if (*catch 'run-length-failed
                          (xgp-encode-scanline 8buf array i left right lftmar))
                  (xgp-encode-image-scanline 8buf array i left right lftmar))
              (if (oddp (array-active-length 8buf)) (array-push 8buf 0))
              (aset (floor (array-active-length 8buf) 2) 16buf 0)
              (aset lineno 16buf 1)
              (setf (array-leader 16buf 0) (aref 16buf 0))
              (funcall xgp-stream ':string-out 16buf)
              (aset (setq lineno (1+ lineno)) 16buf 1)
              (funcall xgp-stream ':string-out 16buf))
          (aset 2 16buf 0)
          (aset (+ 2112. 2_16. 1_15.) 16buf 1)                          ;final page cut
          (setf (array-leader 16buf 0) 2)
          (funcall xgp-stream ':string-out 16buf)
          (close xgp-stream)
          (funcall xgp-stream ':truename)))

;;; Scan line encoding

(defmacro try-bit-encode (bitc)
    `(or (xgp-bitc-push buf ,bitc)
         (*throw 'run-length-failed t)))

(defun xgp-encode-scanline (buf array line left right lftmar)
   (setf (array-leader buf 0) 4)
   (array-push buf 0)                                   ;enter runlength mode
   (array-push buf 0)
   (do ((i left (1+ i)) (color 0) (bitc (floor lftmar 2)))
       (( i right)
        (cond ((= 1 color)
               (try-bit-encode bitc)
               (setq bitc 0)))
        (try-bit-encode (+ bitc (- 850. (+ (floor lftmar 2) right)))))
       (cond ((= color (aref array i line))
              (setq bitc (1+ bitc)))
             (t (try-bit-encode bitc)
                (setq bitc 1 color (logxor color 1)))))
   nil)

(defun xgp-bitc-push (buf bitc)
   (do ((bitc (* 2 bitc) (- bitc 377)))
       (( bitc 0) t)
       (or (array-push buf (min 377 bitc)) (return nil))
       (if (> bitc 377)
           (or (array-push buf 0) (return nil)))))

;;; here encode the scanline as image data
(defun xgp-encode-image-scanline (buf array line left right lftmar &aux ref)
   (setf (array-leader buf 0) 4)
   (array-push buf 0)                                   ;enter image mode
   (array-push buf 2)
   (setq lftmar (floor lftmar 2))
   (do i (floor lftmar 4) (1- i) (= i 0)
       (array-push buf 0))
   (if (> (\ lftmar 4) 0)
       (do ((ppss (+ 0001 (lsh (\ lftmar 4) 7)) (+ ppss 0200))
            (word 0))
           ((= ppss 1001)
            (array-push buf word))
         (setq word (dpb (setq ref (aref array left line)) ppss word))
         (setq word (dpb ref (+ ppss 0100) word))
         (setq left (1+ left))))
   (do ((done))
       (done)
     (do ((ppss 0001 (+ ppss 0200))
          (word 0))
         ((or (= ppss 1001) (setq done ( left right)))
          (array-push buf word))
       (setq word (dpb (setq ref (aref array left line)) ppss word))
       (setq word (dpb ref (+ ppss 0100) word))
       (setq left (1+ left))))
   (do nil
       ((null (array-push buf 0))))
   )
