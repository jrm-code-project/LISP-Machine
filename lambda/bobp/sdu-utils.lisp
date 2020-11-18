;;; -*- Mode:LISP; Package:LAMBDA; Last-File-Plist:(MODE LISP PACKAGE LAMBDA); Input-List:(#<ART-Q-400 51237026> #<ART-Q-400 51231350> #<ART-Q-400 51224311> #<ART-Q-400 47627726>); Base:10 -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst sdu-addr #xfd000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst mb-trace nil)

(defconst mb-read-funcs '(mb-read-8 mb-read-16 mb-read-32))
(defconst mb-write-funcs '(mb-write-8 mb-write-16 mb-write-32))
(defconst mb-rw-funcs '(mb-rw-8 mb-rw-16 mb-rw-32))
(defconst mb-inv-funcs '(mb-inv-8 mb-inv-16 mb-inv-32))

; to = t means ignore timeouts

(defvar nubus-timeout-verbosep nil)

(defun nubus-timeout-condition-handler (object how)
  (when nubus-timeout-verbosep
    (format t "~&~A handled with ~S"
            (send object :report-string) how))
  how)

(defmacro handling-nubus-timeouts (how &body body)
  "How can be :retry-bus-cycle, :loop-until-it-works, :ignore-bus-error"
  `(condition-bind ((nubus-timeout #'nubus-timeout-condition-handler ,how))
     ,@body))

(defun mb-loop (dir &key (count 1) (incr 1) (size 8) (addr #x80000) (data 0) (to nil))
  (format t "~&dir=~a count=~a incr=~a size=~a addr=~16r data=~16r to=~a"
          dir count incr size addr data to)
  (if to
      (handling-nubus-timeouts (if (memq to '(:retry-bus-cycle :loop-until-it-works
                                                               :ignore-bus-error))
                                   to
                                 :loop-until-it-works)
        (mb-loop-internal dir count incr size addr data))
        (mb-loop-internal dir count incr size addr data)))

(defun mb-loop-internal (dir count incr size addr data)
  (let* ((mb-funcs
           (selectq dir
             (:read mb-read-funcs)
             (:write mb-write-funcs)
             (:read-write mb-rw-funcs)
             (:invert mb-inv-funcs)
             (t (ferror nil "first arg must be :read, :write, :read-write, or :invert"))))
         (rw-func
           (selectq size
             ((8 1) (car mb-funcs))
             ((16 2) (cadr mb-funcs))
             ((32 4) (caddr mb-funcs))
             (t (ferror nil ":size must be 8, 16 or 32")))))
    (format t "~&~a" rw-func)
    (setq data (if (listp data)
                   data
                 (list data)))
    (format t "~&data sequence: ~16r" data)
    (rplacd (last data) data)
    (setq addr (if (listp addr)
                   addr
                 (list addr)))
    (format t "~&address sequence: ~16r" addr)
    (do ((mb-trace t nil))
        (nil)
      (do ((base-addr addr (cdr base-addr)))
          ((null base-addr))
        (do ((i 0 (1+ i))
             (a (car base-addr) (+ a incr)))
            ((>= i count))
          (funcall rw-func a (car data))
        (setq data (cdr data))))
      )))

(defun loop-read-8 (&optional (range 1))
  "8-bit loop reading a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-read-8 (+ #x80000 i)))))

(defun loop-read-16 (&optional (range 1))
  "16-bit loop reading a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-read-16 (+ #x80000 i)))))

(defun loop-read-32 (&optional (range 1))
  "32-bit loop reading a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-read-32 (+ #x80000 i)))))

(defun loop-write-8 (&optional (range 1) (data 0))
  "8-bit write a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-write-8 (+ #x80000 i) data))))

(defun loop-write-16 (&optional (range 1) (data 0))
  "16-bit write a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-write-16 (+ #x80000 i) data))))

(defun loop-write-32 (&optional (range 1) (data 0))
  "32-bit write a range of locations starting at 1/2 meg"
  (do-forever
    (dotimes (i range)
      (mb-write-32 (+ #x80000 i) data))))


(defun loop-two-8 (a1 a2)
  "8-bit read two locations"
  (do-forever
    (mb-read-8 a1)
    (mb-read-8 a2)))

(defun loop-two-16 (a1 a2)
  "16-bit read two locations"
  (do-forever
    (mb-read-16 a1)
    (mb-read-16 a2)))

(defun loop-two-32 (a1 a2)
  "32-bit read two locations"
  (do-forever
    (mb-read-32 a1)
    (mb-read-32 a2)))

(defun loop-two-8-write (a1 a2 &optional (data 0))
  "8-bit write to two locations"
  (do-forever
    (mb-write-8 a1 data)
    (mb-write-8 a2 data)))

(defun loop-two-16-write (a1 a2 &optional (data 0))
  "16-bit write to two locations"
  (do-forever
    (mb-write-16 a1 data)
    (mb-write-16 a2 data)))

(defun loop-two-32-write (a1 a2 &optional (data 0))
  "32-bit write to two locations"
  (do-forever
    (mb-write-32 a1 data)
    (mb-write-32 a2 data)))

(defun mb-mem-loop-8 (&optional (addr #x8000) (size #x4000))
  "8-bit loop reading multibus ram"
  (do-forever
    (dotimes (i size)
      (mb-read-8 (+ addr i)))))

(defun mb-mem-loop-16 (&optional (addr #x8000) (size #x4000))
  "16-bit loop reading multibus ram"
  (do-forever
    (do ((i 0 (+ i 2)))
        ((>= i size))
      (mb-read-16 (+ addr i)))))

(defun mb-mem-loop-32 (&optional (addr #x8000) (size #x4000))
  "32-bit loop reading multibus ram"
  (do-forever
    (do ((i 0 (+ i 4)))
        ((>= i size))
      (mb-read-32 (+ addr i)))))

(defun mb-mem-write-loop-8 (&optional (addr #x8000) (size #x4000) data)
  "8-bit loop writing multibus ram"
  (do-forever
    (dotimes (i size)
      (mb-write-8 (+ addr i)
                           (if data
                               data
                             i)))))

(defun mb-mem-write-loop-16 (&optional (addr #x8000) (size #x4000) data)
  "16-bit loop writing multibus ram"
  (do-forever
    (do ((i 0 (+ i 2)))
        ((>= i size))
      (mb-write-16 (+ addr i)
                           (if data
                               data
                             i)))))

(defun mb-mem-write-loop-32 (&optional (addr #x8000) (size #x4000) data)
  "32-bit loop writing multibus ram"
  (do-forever
    (do ((i 0 (+ i 4)))
        ((>= i size))
      (mb-write-32 (+ addr i)
                           (if data
                               data
                             i)))))

;(defun find-mem (&optional (inc #x10000))
;  (do ((adr 0 (+ adr inc)))
;      ((>= adr #x100000))
;    (let ((ret (mb-read-8 adr) t))
;      (if (not (= ret -1))
;         (format t "~&~16r returns ~16r" adr ret)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; cmos ram functions

(defconst cmos-ram-array (make-array 2048. :type :art-string))
(defconst nd-cmos-ram-array (make-array 2048. :type :art-string))

(defun good-cmos-ram ()
  "write good data to the cmos ram"
  (get-cmos-ram)
  (nd-set-cmos-ram cmos-ram-array))

(defun check-cmos-ram ()
  "compare the cmos ram with the local lambda's (good) cmos ram"
  (get-cmos-ram)
  (nd-get-cmos-ram)
  (compare-array cmos-ram-array nd-cmos-ram-array))

(defun loop-set-cmos-ram ()
  "write good data to the cmos ram forever"
  (get-cmos-ram)
  (do-forever
    (nd-set-cmos-ram cmos-ram-array)))

(defun loop-get-cmos-ram ()
  "read the cmos ram forever"
  (do-forever
    (nd-get-cmos-ram)))

(defun get-cmos-ram (&optional (cmos cmos-ram-array))
  "read the cmos ram from the local lambda"
  (dotimes (i 2048.)
    (aset (%multibus-read-8 (+ (* 4 i) #x1e000)) cmos i)))

(defun nd-get-cmos-ram (&optional (cmos nd-cmos-ram-array))
  "read the cmos ram from the sdu being debugged"
  (dotimes (i 2048.)
    (aset (mb-read-8 (+ (* 4 i) #x1e000)) cmos i)))

(defun nd-set-cmos-ram (&optional (cmos nd-cmos-ram-array))
  "write the cmos ram to the sdu being debugged"
  (dotimes (i 2048.)
    (mb-write-8 (+ (* 4 i) #x1e000)
                         (aref cmos i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compare-array (a b)
  (compare-array-portion a 0 (length a) b 0 (length b)))

(defun compare-array-portion (from-array from-start from-end
                              to-array to-start to-end)
  (if (not (= (- from-end from-start)
              (- to-end to-start)))
      (ferror nil "compare different size arrays"))
  (let ((diff t))
    (do ((i from-start (1+ i))
         (j to-start (1+ j)))
        ((>= i from-end))
      (when (not (= (aref from-array i)
                    (aref to-array j)))
        (format t "~&[~d]=~d [~d]=~d"
                i
                (aref from-array i)
                j
                (aref to-array j))
        (setq diff nil)))
    diff))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mb-read-8 (addr &optional ignore)
  (if mb-trace
      (format t "~&multibus-read-8 addr=#x~16r" addr))
  (bus-read-byte (+ sdu-addr addr)))

(defun mb-write-8 (addr data)
  (setq data (ldb (byte 8 0) data))
  (if mb-trace
      (format t "~&multibus-write-8 addr=#x~16r data=#x~16r" addr data))
  (bus-write-byte (+ sdu-addr addr) data))

(defun mb-read-16 (addr &optional ignore)
  (ferror nil "not yet")
  (if mb-trace
      (format t "~&multibus-read-16 addr=#x~16r" addr))
  (bus-read (+ sdu-addr addr)))

(defun mb-write-16 (addr data)
  (ferror nil "not yet")
  (setq data (ldb (byte 16. 0) data))
  (if mb-trace
      (format t "~&multibus-write-16 addr=#x~16r data=#x~16r" addr data))
  (bus-write (+ sdu-addr addr) data))

(defun mb-read-32 (addr &optional ignore)
  (if mb-trace
      (format t "~&multibus-read-32 addr=#x~16r" addr))
  (bus-read (+ sdu-addr addr)))

(defun mb-write-32 (addr data)
  (if mb-trace
      (format t "~&multibus-write-32 addr=#x~16r data=#x~16r" addr data))
  (bus-write (+ sdu-addr addr) data))

(defun mb-rw-8 (addr data)
  (mb-read-8 addr)
  (mb-write-8 addr data))

(defun mb-rw-16 (addr data)
  (mb-read-16 addr)
  (mb-write-16 addr data))

(defun mb-rw-32 (addr data)
  (mb-read-32 addr)
  (mb-write-32 addr data))

(defun mb-inv-8 (addr &optional ignore)
  (mb-write-8 addr (logxor #xff (mb-read-8 addr))))

(defun mb-inv-16 (addr &optional ignore)
  (mb-write-16 addr (logxor #xffff (mb-read-16 addr))))

(defun mb-inv-32 (addr &optional ignore)
  (mb-write-32 addr (logxor #xffffffff (mb-read-32 addr))))
