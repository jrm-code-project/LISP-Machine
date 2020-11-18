;;; -*- Mode:LISP; Package:SDU; Base:10 -*-

; stuff to help with unix / C strings

(defconst white-space-chars '(#/SPACE #/TAB #/RETURN #o15 #o12 #o11))

(defconst char-map (make-array 256. :type :art-string))

; set up char-map ascii <=> lisp translation table
(dotimes (i 256.)
  (aset (selectq i
          (#xd #x8a)
          (#xa #x8d)
          (#x9 #x89)
          (#x8d #xa)
          (#x8a #xd)
          (#x89 #x9)
          (t i))
        char-map i))

(defun parse-string-into-list (str)
  "parse ascii string; return list of white-space-separated components"
  (do ((return-list nil)
       (start)
       (end 0))
      ((or (null end)
           (null (setq start (string-search-not-set white-space-chars str end))))
       return-list)
    (setq end (string-search-set white-space-chars str start))
    (setq return-list (append return-list (list (substring str start end))))))

(defun c-str-copy (ar &optional (offs 0) (max-n (array-length ar)))
  "return copy of string at ar sub offs up to null byte or max"
  (make-array (min (c-string-length ar offs) max-n)
              :type :art-string
              :displaced-to ar
              :displaced-index-offset offs))

(defun c-string-length (s &optional (from 0))
  "length of string; n-chars up to null or end"
  (let ((len (string-search 0 s from)))
    (if len
        (- len from)
      (- (array-length s) from))))

(defvar c-print-buf nil)

(defun ascii-string (str &optional buf len)
  (if (or (null len)
          (> len (array-length str)))
      (setq len (array-length str)))
  (let ((l (string-search 0 str 0 len)))
    (if (null l)
        (setq l len))
    (if buf
        (setq l (min l (array-length buf)))
      (setq buf (make-array l :type :art-string :fill-pointer 0)))
    (setf (fill-pointer buf) l)
    (dotimes (i l)
      (aset (aref char-map (aref str i)) buf i))
    buf))

(defun print-array-portions (ar &optional (n-portions 100.) (incr 1024.))
  (dotimes (i n-portions)
    (let ((buf (make-array 10.
                           :type :art-8b
                           :displaced-to ar
                           :displaced-index-offset (* i incr))))
      (format t "~d=~a  " i (ascii-string buf)))))

(defun array-to-string (ar &optional len (offs 0))
  (make-array (if len
                  (min len (array-length ar))
                (array-length ar))
              :type :art-string
              :displaced-to ar
              :displaced-index-offset offs))

; translate between lisp machine and ascii char set
(defsubst unix-char (c)
  (aref char-map c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; unix-time <> universal-time

(defconst unix-time-offset 2208988800.)

(defsubst unix-time-to-universal-time (time)
  (+ time unix-time-offset))

(defun unix-time (time)
  "return string rep of unix-time"
  (time:print-universal-time (+
                               (* 3600 time:*timezone*)
                               (unix-time-to-universal-time time))
                             nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; byte-swap and 68000 / 8086 primitives

(defun get-68k-nbytes (byte-array offs size)
  "get a size-bytes 68000 byte-order number starting at array sub offs"
  (do ((i 0 (1+ i))
       (n 0 (+ (* n 256.) (aref byte-array (+ i offs)))))
      ((= i size)
       n)))

(defun get-n-bytes (byte-array offs size)
  "get a size-bytes lambda-byte-order number starting at array sub offs"
  (do ((i size (1- i))
       (n 0 (+ (* n 256.) (aref byte-array (+ i offs -1)))))
      ((= i 0)
       n)))

(defun set-n-bytes (byte-array offs size val)
  "store a size-bytes lambda-byte-order number starting at array sub offs"
  (do ((i 0 (1+ i)))
      ((= i size))
    (aset (ldb (byte 8 0) val) byte-array (+ offs i))
    (setf val (ash val -8))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-constant (s)
  "make a number equivalent to string s viewed as 4-byte integer in memory"
  (if (> (string-length s) 0)
      (dpb (string-constant (substring s 1))
           (byte 24 8)
           (aref s 0))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun c-read-file-into-array (file ar &optional (size (array-length ar)))
  (with-open-file (s file :raw t)
    (do ((i 0 (1+ i))
         (ch (send s :tyi) (send s :tyi)))
        ((or (null ch)
             (= i size))
         i)
      (aset ch ar i))))
