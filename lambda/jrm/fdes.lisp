;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

(defvar *chars-in-key* 8.)

(defvar *salted-expansion-function*)

(defun crypt (password salt)
  (let ((key-bits (key->bit-string password))
        (salt-string (pad-string salt 2.)))
    (with-salted-expansion-function salt-string
      #'(lambda ()
          (let ((output-bits
                  (concat-bit-strings
                    (des (make-bit-string 64.) key-bits 25.)
                    (make-bit-string 2.))))
            (do ((output-string salt-string
                                (string-append output-string
                                               (six-bit->ascii
                                                 (ldb-from-bit-string (byte 6 (* i 6)) output-bits))))
                 (i 0 (1+ i)))
                ((= i 11.) (when (zerop (elt output-string 1))
                             (setf (elt output-string 1)
                                   (elt output-string 0)))
                 output-string)))))))

(defun key->bit-string (key)
  (let ((padded-key (pad-key key)))
    (do ((i 0 (1+ i))
         (result-bit-string
           (make-bit-string 0)
           (concat-bit-strings
             result-bit-string
             (key-char->des-format (elt padded-key i)))))
        ((= i *chars-in-key*) result-bit-string))))

(defun pad-key (key)
  (pad-string key *chars-in-key*))

(defun pad-string (string pad)
  (substring (string-append string (make-string pad)) 0 pad))

(defun key-char->des-format (char)
  (let ((char-bits (%char->bit-string char)))
    (concat-bit-strings
      (mirror-bit-string
        (ldb-from-bit-string (byte 7 0) char-bits))
      (ldb-from-bit-string (byte 1 7) char-bits))))

(defun with-salted-expansion-function (salt receiver)
  (let ((*salted-expansion-function* (copy-seq expansion-function)))
    (dotimes (i 2)
      (let ((six-bit (ascii->six-bit (elt salt i))))
        (dotimes (j 6)
          (when (not (zerop (elt six-bit j)))
            (swapf (elt *salted-expansion-function* (+ (* 6 i) j))
                   (elt *salted-expansion-function* (+ (* 6 i) j 24.)))))))
    (funcall receiver)))

(defun des (block key times)
  (multiple-value-bind (left right)
      (block->initial-halves block)
    (let ((key-schedule (generate-key-schedule key)))
      (dotimes (i times)
        (multiple-value-setq (left right)
          (des-loop left right key-schedule)))
      (apply-permutation final-permutation (concat-bit-strings left right)))))

(defun block->initial-halves (block)
  (let ((p-block (apply-permutation initial-permutation block)))
    (values (ldb-from-bit-string (byte 32 0) p-block)
            (ldb-from-bit-string (byte 32 32) p-block))))

(defun generate-key-schedule (key-bit-string)
  (multiple-value-bind (c0 d0)
      (key-bits->c-and-d key-bit-string)
    (let ((key-schedule (make-array 16.))
          (c c0)
          (d d0)
          (shift-schedule key-shift-schedule))
      (dotimes (i 16.)
        (setq c (rotate-bit-string c (- (first shift-schedule))))
        (setq d (rotate-bit-string d (- (first shift-schedule))))
        (setq shift-schedule (rest shift-schedule))
        (setf (elt key-schedule i)
              (apply-permutation permuted-choice-2 (concat-bit-strings c d))))
      key-schedule)))

(defun key-bits->c-and-d (key-bit-string)
  (let ((permuted-key (apply-permutation permuted-choice-1 key-bit-string)))
    (values (ldb-from-bit-string (byte 28 0) permuted-key)
            (ldb-from-bit-string (byte 28 28) permuted-key))))

(defun des-loop (left right key-schedule)
  (dotimes (i 16)
    (multiple-value-setq (left right)
      (des-round left right (elt key-schedule i)))
;    (format t "~&~s ~s" left right)
    )
  (values right left))

(defun des-round (left right key )
  (values right (bit-xor left (des-f right key))))

(defun des-f (right key)
  (apply-permutation p-permutation
                     (pass-through-s-boxes
                       (expand-block-half right key))))

(defun expand-block-half (block-half key)
  (bit-xor key (apply-permutation *salted-expansion-function* block-half)))

(defun pass-through-s-boxes (expanded-bit-string)
;  (terpri)
;  (dotimes (i 48.) (format t "~b" (elt expanded-bit-string i)))
  (do ((i 0 (1+ i))
       (sbox-output (make-bit-string 0.)
                    (concat-bit-strings
                      sbox-output
                      (sbox-ref (elt s-boxes i)
                                (ldb-from-bit-string (byte 6 (* i 6)) expanded-bit-string)))))
      ((= i 8.) sbox-output)))

(defun sbox-ref (sbox bit-string)
  (elt sbox
       (bit-string->fixnum
         (concat-bit-strings
           (mirror-bit-string
             (ldb-from-bit-string (byte 4 1) bit-string))
           (ldb-from-bit-string (byte 1 5) bit-string)
           (ldb-from-bit-string (byte 1 0) bit-string)))))

(defun apply-permutation (p bit-string)
  (let ((dest (make-bit-string (length p))))
    (for-elements-in-vector p
      #'(lambda (index value)
          (setf (elt dest index) (elt bit-string value))))
        dest))

(defun ascii->six-bit (c)
  ;; Snarfed from some C code.
  (when (char> c #\Z)
    (decf c 6))
  (when (char> c #\9)
    (decf c 7))
  (ldb-from-bit-string (byte 6 0) (%char->bit-string (decf c #\.))))

(defun six-bit->ascii (i)
  (let ((c (%bit-string->char
             (concat-bit-strings
               (mirror-bit-string i)
               (make-bit-string 2.)))))
    ;; More C code.
    (incf c #\.)
    (when (char> c #\9) (incf c 7))
    (when (char> c #\Z) (incf c 6))
    c))

;;;; Using the above code, des takes about 37 seconds, or .03 passwords/sec

;;;; First optimization:  Shuffle S-boxes to avoid bit extraction in
;;;; s-box ref.

;(defvar *new-s-boxes* (make-array (length s-boxes)))

;(defun compute-new-s-box-value (index old-s-box new-s-box)
;  (let ((bit-string (%fixnum->bit-string index)))
;    (setf (elt new-s-box index)
;         (elt old-s-box
;              (bit-string->fixnum
;                (concat-bit-strings
;                  (mirror-bit-string
;                    (ldb-from-bit-string (byte 4 1) bit-string))
;                  (ldb-from-bit-string (byte 1 5) bit-string)
;                  (ldb-from-bit-string (byte 1 0) bit-string)))))))

;(defun compute-new-s-box (old-s-box)
;  (let ((length (length old-s-box)))
;    (let ((new-s-box (make-array length)))
;      (dotimes (i length)
;       (compute-new-s-box-value i old-s-box new-s-box))
;      new-s-box)))

;(defun compute-new-s-boxes ()
;  (dotimes (i (length s-boxes))
;    (setf (elt *new-s-boxes* i) (compute-new-s-box (elt s-boxes i)))))

;(defun new-s-box-ref (s-box bit-string)
;  (elt s-box (bit-string->fixnum bit-string)))

;(defun pass-through-s-boxes (expanded-bit-string)
;  (do ((i 0 (1+ i))
;       (sbox-output (make-bit-string 0.)
;                   (concat-bit-strings
;                     sbox-output
;                     (new-s-box-ref (elt *new-s-boxes* i)
;                               (ldb-from-bit-string (byte 6 (* i 6)) expanded-bit-string)))))
;      ((= i 8.) sbox-output)))

;;;; This gains a factor of about 30% now we do .04 passwords/sec
;;;; Next optimization is to make salting be an xor with a computed mask
;;;; instead of a swapping of elements in the E permutation.

;(defvar *salt-bits*)

;(defun with-salted-expansion-function (salt receiver)
;  (let ((*salt-bits*
;         (concat-bit-strings
;           (ascii->six-bit (elt salt 0))
;           (ascii->six-bit (elt salt 1))
;           (make-bit-string 12.))))
;    (funcall receiver)))

;(defun expand-block-half (block-half key)
;  (let ((bottom
;         (concat-bit-strings
;           (ldb-from-bit-string (byte 1. 31.) block-half)
;           (ldb-from-bit-string (byte 5.  0.) block-half)
;           (ldb-from-bit-string (byte 6.  3.) block-half)
;           (ldb-from-bit-string (byte 6.  7.) block-half)
;           (ldb-from-bit-string (byte 6. 11.) block-half)))
;       (top
;         (concat-bit-strings
;           (ldb-from-bit-string (byte 6. 15.) block-half)
;           (ldb-from-bit-string (byte 6. 19.) block-half)
;           (ldb-from-bit-string (byte 6. 23.) block-half)
;           (ldb-from-bit-string (byte 5. 27.) block-half)
;           (ldb-from-bit-string (byte 1.  0.) block-half))))
;      (let ((interesting-bits (bit-xor top bottom)))
;       (let ((salt-mask (bit-and *salt-bits* interesting-bits)))
;         (bit-xor key
;                  (concat-bit-strings
;                    (bit-xor salt-mask bottom)
;                    (bit-xor salt-mask top))))))))

;;;; This didn't help.  Time to pull out the big guns.
