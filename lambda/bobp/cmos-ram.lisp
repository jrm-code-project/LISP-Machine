;;; -*- Mode:LISP; Package:UNIX; Base:10 -*-

; obselete code from "unix-fs"

; file "/sdu/monitor/conf/std":
; read strings; ignore if not DISK
;
;cpu:   FB000000
;vcmem: F8000000
;ram:   FC000000        200
;port:  FF000014        FFFFDFFC
;disk:  0       3       6C7800          FFC00
;disk:  0       3       1FCC00          7C7400
;disk:  0       3       19A28000        0
;disk:  0       3       0               0
;disk:  0       3       2710000         9C4000
;disk:  0       3       0               0
;disk:  0       3       FA000           5C00
;disk:  0       3       0               0
;logport:               0
;logchannel:            1

(defun set-cmos-ram-array-from-std-file ()
  "re-set-up the cmos ram disk partition info from the std file"
  (do ((file (open-unix-file "//sdu//monitor//conf//std"))
       (line (make-array 100 :type :art-string :fill-pointer 0))
       (line-num 0))
      ((null (read-file-line file line)))
    (let ((line-list (parse-string-into-list line)))
      (cond ((string-equal (car line-list) "disk:")
             (set-cmos-ram-partition-offset line-num (parse-integer (fifth line-list) :radix 16.))
             (set-cmos-ram-partition-size line-num (parse-integer (fourth line-list) :radix 16.))
             (incf line-num))))))

(defun read-file-line (file line)
  "read ascii-newline terminated line from file into line; set fill-pointer; return line or nil"
  (do ((i 0 (1+ i))
       (ch (make-array 1 :type :art-string)))
      ((not (eq 1 (read-file file ch 1))))
    (aset (aref char-map (aref ch 0)) line i)
    (cond ((= (aref line i) #\RETURN)
           (setf (fill-pointer line) i)
           (return line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cmos ram is read into local 2k byte array

(defvar cmos-ram-array nil)

; find "root"; if not present find "uroot"

(defun get-cmos-ram (&optional get-std-file)
  (setq cmos-ram-array (make-array 2048. :type :art-string))
  (do ((i 0 (1+ i)))
      ((= i 1024.))
    (aset (%multibus-read-8 (+ (* 4 i) #x1e000)) cmos-ram-array i))
  (if get-std-file
      (set-cmos-ram-array-from-std-file)))

; accessors for local copy of cmos ram

(defun cmos-ram-crc (&optional (cr cmos-ram-array))
  (get-n-bytes cr 0 2))

(defun cmos-ram-partition-name (num &optional (cr cmos-ram-array))
  (c-str-copy cr (cmos-ram-disk-struct-offs num) 8))

(defun cmos-ram-partition-unit (num &optional (cr cmos-ram-array))
  (get-n-bytes cr (+ 8 (cmos-ram-disk-struct-offs num)) 1))

(defun cmos-ram-partition-offset (num &optional (cr cmos-ram-array))
  (get-n-bytes cr (+ 10 (cmos-ram-disk-struct-offs num)) 4))

(defun cmos-ram-partition-size (num &optional (cr cmos-ram-array))
  (get-n-bytes cr (+ 14 (cmos-ram-disk-struct-offs num)) 4))

(defun cmos-ram-disk-struct-offs (partition-number)
  (+ 306. (* partition-number 18.)))

(defvar sdu-file-system-names '("uroot" "" "" "" "" "" "root" ""))

(defun unix-partition-offset (num)
  (let ((cmos-num (unix-part-num num)))
    (if (not (null cmos-num))
        (cmos-ram-partition-offset cmos-num))))

(defun unix-part-num (num &optional (cr cmos-ram-array))
  (dotimes (i 8)
    (if (string-equal (nth num sdu-file-system-names) (cmos-ram-partition-name i cr))
        (return-from unix-part-num i)))
  nil)

(defun set-cmos-ram-partition-offset (num val &optional (cr cmos-ram-array))
  (set-n-bytes cr (+ 10 (cmos-ram-disk-struct-offs num)) 4 val))

(defun set-cmos-ram-partition-size (num val &optional (cr cmos-ram-array))
  (set-n-bytes cr (+ 14 (cmos-ram-disk-struct-offs num)) 4 val))

(defun print-disk-partition-info ()
  (if (null cmos-ram-array)
      (get-cmos-ram))
  (dotimes (i 10)
    (format t "~%~10s ~d ~9x ~9x"
            (cmos-ram-partition-name i)
            (cmos-ram-partition-unit i)
            (cmos-ram-partition-offset i)
            (cmos-ram-partition-size i))))
