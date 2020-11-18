;;; -*- Mode:LISP; Package:MAC; Base:10; Readtable:ZL -*-
;;;
;;;  Bus coupler memory interface.
;;;


;
; Data-type for the expression in data-structure
;
(defconst ISA-NIL      0)
(defconst ISA-T        1)
(defconst ISA-KEYWORD  2)
(defconst ISA-STRING   3)
(defconst ISA-INTEGER  4)
(defconst ISA-TEXT     5)


(defun test-bc-address (adr)
  (if (or (< adr #xFB000000)
          (> adr #xFB01FFFF))
      (ferror "oups... j'ai faillit geler")))

(defun test-bc-address-and-value (adr value)
  (test-bc-address adr)
  (if (or (not (fixp value))
          (< value 0))
      (ferror "oup... j'ai faillit geler")))


(defun read-BC-byte (adr)
  (test-bc-address adr)
  (let* ((no-slot (ldb (byte 8 24.)adr))
         (no-byte (ldb (byte 2 0) adr))
         (address (logand #xfffffc (ldb (byte 24. 0) adr)))
         (long-word (%nubus-read no-slot address)))
    (case no-byte
      (0 (ldb (byte 8 0) long-word))
      (1 (ldb (byte 8 8.) long-word))
      (2 (ldb (byte 8 16.) long-word))
      (3 (ldb (byte 8 24.) long-word)))))



(defun write-BC-byte (adr byte)
  (test-bc-address-and-value adr byte)
  (let* ((no-slot (ldb (byte 8 24.)adr))
         (no-byte (ldb (byte 2 0) adr))
         (address (logand #xfffffc (ldb (byte 24. 0) adr)))
         (long-word (%nubus-read no-slot address)))
    (%nubus-write no-slot
                  address
                  (dpb byte (case no-byte
                              (0 (byte 8 0))
                              (1 (byte 8 8.))
                              (2 (byte 8 16.))
                              (3 (byte 8 24.))) long-word))))


(defun read-BC-word (adr)
  (test-bc-address adr)
  (let* ((no-slot (ldb (byte 8 24.)adr))
         (no-byte (ldb (byte 2 0) adr))
         (address (logand #xfffffc (ldb (byte 24. 0) adr)))
         (long-word (%nubus-read no-slot address)))
    (case no-byte
      (0 (logior (ash (ldb (byte 8 0) long-word) 8)  (ldb (byte 8 8.) long-word)))
      (2 (logior (ash (ldb (byte 8 16.) long-word) 8)(ldb (byte 8 24.) long-word))))))



(defun write-BC-word (adr word)
  (test-bc-address-and-value adr word)
  (let* ((no-slot (ldb (byte 8 24.)adr))
         (no-byte (ldb (byte 2 0) adr))
         (address (logand #xfffffc (ldb (byte 24. 0) adr)))
         (long-word (%nubus-read no-slot address)))
    (%nubus-write no-slot
                  address
                  (dpb (logior (ash (ldb (byte 8 0) word) 8) (ldb (byte 8 8) word))
                       (case no-byte
                         (0 (byte 16. 0))
                         (2 (byte 16. 16.))) long-word))))


(defun read-BC-long (adr)
  (test-bc-address adr)
  (if (= 0 (ldb (byte 2 0) adr))
      (lam:swap32 (%nubus-read (ldb (byte 8 24.) adr) (ldb (byte 24. 0) adr)))
    (logior (ash (read-BC-word adr)16.) (read-BC-word (+ 2 adr)))))



(defun write-BC-long (adr val)
  (test-bc-address-and-value adr val)
  (if (= 0 (ldb (byte 2 0) adr))
      (%nubus-write (ldb (byte 8 24.) adr) (ldb (byte 24. 0) adr) (lam:swap32 val))
    (error "Attempt to write a long-word to a non-aligned address #o~O". adr)))


;;;
;;; mac strings...
;;;
(defun read-BC-mac-string (adr)
  (let* ((length (read-BC-byte adr))
         (string (make-string length)))
    (loop for x from 1 to length
          do (aset (read-BC-byte (+ x adr)) string (1- x)))
    string))

(defun write-BC-mac-string (adr string length)
  (write-BC-byte adr length)
  (incf adr)
  (loop for x from 0 to (1- length)
        do (write-BC-byte (+ x adr) (aref string x))))


;;;
;;; standard strings...
;;;
(defun read-BC-string2 (adr length)
  (let ((string (make-string length)))
    (loop for x from 0 to (1- length)
          do (aset (read-BC-byte (+ x adr)) string x))
    string))


(defun write-BC-string2 (adr string length)
  (loop for x from 0 to (1- length)
        do (write-BC-byte (+ x adr) (aref string x))))

;;;
;;; standard strings...
;;;
(defun read-BC-in-string (adr string length)
  (loop for x from 0 to (1- length)
        do (aset (read-BC-byte (+ x adr)) string x)))


(defun write-BC-from-string (string adr length)
  (loop for x from 0 to (1- length)
        do (write-BC-byte (+ x adr) (aref string x))))


;
;   Other types of expression
;
(defun expr->num (expr)
  (cond ((eq t expr)     (dpb ISA-T (byte 8 24.) 0))
        ((null expr)     (dpb ISA-NIL (byte 8 24.) 0))
        ((integerp expr) (dpb ISA-INTEGER (byte 8 24.) expr))
        ((keywordp expr) (dpb ISA-KEYWORD (byte 8 24.)(keyword->num expr)))
        (t (ferror "Bad expression to translate"))))


(defun num->expr (num &optional (struct 0))
  (select (ldb (byte 8 24.) num)
    (ISA-T t)
    (ISA-NIL nil)
    (ISA-INTEGER (ldb (byte 16 0) num))
    (ISA-KEYWORD (num->keyword (ldb (byte 16 0) num)))
    (ISA-STRING (read-BC-mac-string (+ struct (ldb (byte 24. 0.) num))))
    (t (ferror "Bad expression to translate"))))
