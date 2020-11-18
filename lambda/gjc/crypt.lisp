;;; -*- Mode:LISP; Package:(CRYPT GLOBAL); Readtable:ZL; Base:10 -*-

;; This program implements the
;; Proposed Federal Information Processing
;; Data Encryption Standard.
;; See Federal Register, March 17, 1975 (40FR12134)

(defmacro define-array (name dimlist &optional contents)
  (let ((v (intern (string-append "*" name "*")))
        (l (mapcar #'(lambda (ignore) (gentemp "i")) dimlist)))
    `(progn 'compile
            (defparameter ,v (char-array-init ',dimlist ',contents))
            (defmacro ,name ,l (list 'aref ',v ,@l)))))


(defun char-array-init (dimlist contents)
  (let ((a (if (not (numberp (car dimlist)))
               (make-array (length contents))
             (make-array dimlist))))
    (fillarray a (if (not contents) (list 0) contents))
    a))

;; Initial permutation,

(define-array IP (NIL)
  (58 50 42 34 26 18 10  2
   60 52 44 36 28 20 12  4
   62 54 46 38 30 22 14  6
   64 56 48 40 32 24 16  8
   57 49 41 33 25 17  9  1
   59 51 43 35 27 19 11  3
   61 53 45 37 29 21 13  5
   63 55 47 39 31 23 15  7))



;; Final permutation, FP = IP^(-1)

(define-array FP (NIL)
  (40  8 48 16 56 24 64 32
   39  7 47 15 55 23 63 31
   38  6 46 14 54 22 62 30
   37  5 45 13 53 21 61 29
   36  4 44 12 52 20 60 28
   35  3 43 11 51 19 59 27
   34  2 42 10 50 18 58 26
   33  1 41  9 49 17 57 25))

;; Permuted-choice 1 from the key bits
;; to yield C and D.
;; Note that bits 8,16... are left out:
;; They are intended for a parity check.

(define-array PC1_C (NIL)
  (57 49 41 33 25 17  9
    1 58 50 42 34 26 18
    10  2 59 51 43 35 27
    19 11  3 60 52 44 36))


(define-array  PC1_D (NIL)
  (63 55 47 39 31 23 15
    7 62 54 46 38 30 22
   14  6 61 53 45 37 29
   21 13  5 28 20 12  4))


;; Sequence of shifts used for the key schedule.

(define-array shifts (NIL)
  (1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1))

;; Permuted-choice 2, to pick out the bits from
;; the CD array that generate the key schedule.

(define-array  PC2_C (NIL)
  (14 17 11 24  1  5
    3 28 15  6 21 10
   23 19 12  4 26  8
   16  7 27 20 13  2))


(define-array  PC2_D (NIL)
  (41 52 31 37 47 55
   30 40 51 45 33 48
   44 49 39 56 34 53
   46 42 50 36 29 32))

;; The C and D arrays used to calculate the key schedule.

(define-array C (28))
(define-array D (28))

;; The key schedule.
;; Generated from the key.

(define-array ks (16 48))

;; Set up the key schedule from the key.

(defun setkey (key &aux temp)
  (macrolet ((key (index) `(aref key ,index)))
    ;; First, generate C and D by permuting
    ;; the key.  The low order bit of each
    ;; 8-bit char is not used, so C and D are only 28
    ;; bits apiece.
    (dotimes (i 28)
      (setf (c i) (key (1- (pc1_c i))))
      (setf (d i) (key (1- (pc1_d i)))))
    ;; To generate Ki, rotate C and D according
    ;; to schedule and pick up a permutation
    ;; using PC2.
    (dotimes (i 16)
      ;; rotate.
      (dotimes (k (shifts i))
        (setq temp (c 0))
        (dotimes (j 26)
          (setf (c j) (c (1+ j))))
        (setf (c 27) temp)
        (setq temp (d 0))
        (dotimes (j 26)
          (setf (d j) (d (1+ j))))
        (setf (d 27) temp))
      ;; get Ki. Note C and D are concatenated.
      (dotimes (j 14)
        (setf (ks i j) (c (- (pc2_c j) 1)))
        (setf (ks i (+ j 24)) (d (- (pc2_d j) 28 1)))))))


;; The E bit-selection table.

(define-array EB (48))

(define-array e (NIL)
  (32  1  2  3  4  5
    4  5  6  7  8  9
    8  9 10 11 12 13
   12 13 14 15 16 17
   16 17 18 19 20 21
   20 21 22 23 24 25
   24 25 26 27 28 29
   28 29 30 31 32  1))



;; The 8 selection functions.
;; For some reason, they give a 0-origin
;; index, unlike everything else.

(define-array S (8 64)
  (14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7
    0 15  7  4 14  2 13  1 10  6 12 11  9  5  3  8
    4  1 14  8 13  6  2 11 15 12  9  7  3 10  5  0
   15 12  8  2  4  9  1  7  5 11  3 14 10  0  6 13

   15  1  8 14  6 11  3  4  9  7  2 13 12  0  5 10
    3 13  4  7 15  2  8 14 12  0  1 10  6  9 11  5
    0 14  7 11 10  4 13  1  5  8 12  6  9  3  2 15
   13  8 10  1  3 15  4  2 11  6  7 12  0  5 14  9

   10  0  9 14  6  3 15  5  1 13 12  7 11  4  2  8
   13  7  0  9  3  4  6 10  2  8  5 14 12 11 15  1
   13  6  4  9  8 15  3  0 11  1  2 12  5 10 14  7
    1 10 13  0  6  9  8  7  4 15 14  3 11  5  2 12

    7 13 14  3  0  6  9 10  1  2  8  5 11 12  4 15
   13  8 11  5  6 15  0  3  4  7  2 12  1 10 14  9
   10  6  9  0 12 11  7 13 15  1  3 14  5  2  8  4
    3 15  0  6 10  1 13  8  9  4  5 11 12  7  2 14

    2 12  4  1  7 10 11  6  8  5  3 15 13  0 14  9
   14 11  2 12  4  7 13  1  5  0 15 10  3  9  8  6
    4  2  1 11 10 13  7  8 15  9 12  5  6  3  0 14
   11  8 12  7  1 14  2 13  6 15  0  9 10  4  5  3

   12  1 10 15  9  2  6  8  0 13  3  4 14  7  5 11
   10 15  4  2  7 12  9  5  6  1 13 14  0 11  3  8
    9 14 15  5  2  8 12  3  7  0  4 10  1 13 11  6
    4  3  2 12  9  5 15 10 11 14  1  7  6  0  8 13

    4 11  2 14 15  0  8 13  3 12  9  7  5 10  6  1
   13  0 11  7  4  9  1 10 14  3  5 12  2 15  8  6
    1  4 11 13 12  3  7 14 10 15  6  8  0  5  9  2
    6 11 13  8  1  4 10  7  9  5  0 15 14  2  3 12

   13  2  8  4  6 15 11  1 10  9  3 14  5  0 12  7
    1 15 13  8 10  3  7  4 12  5  6 11  0 14  9  2
    7 11  4  1  9 12 14  2  0  6 10 13 15  3  5  8
    2  1 14  7  4 10  8 13 15 12  9  0  3  5  6 11 ))



;; P is a permutation on the selected combination
;; of the current L and key.

(define-array p (nil)
  (16  7 20 21
   29 12 28 17
    1 15 23 26
    5 18 31 10
    2  8 24 14
   32 27  3  9
   19 13 30  6
   22 11  4 25))



;; The current block, divided into 2 halves.

(define-array l (64))
(define-array r (64))
(define-array templ (32))
(define-array f (32))


;; The combination of the key and the input, before selection.

(define-array pres (48))




;; The payoff: encrypt a block.


(defun encrypt (block edflag &aux temp i k)
  (macrolet ((block (index) `(aref block ,index)))
    (dotimes (j 64)
      (setf (l j) (block (- (ip j) 1))))
    ;; Perform an encryption operation 16 times.
    (dotimes (ii 16)
      ;; Set direction
      (if edflag (setq i (- 15 ii)) (setq i ii))
      ;; Save the R array,
      ;; which will be the new L.
      (dotimes (j 32)
        (setf (templ j) (r j)))
      ;; Expand R to 48 bits using the E selector;
      ;; exclusive-or with the current key bits.
      (dotimes (j 48)
        (setf (pres j) (logxor (r (- (e j) 1))
                               (ks i j))))
      ;; The pre-select bits are now considered
      ;; in 8 groups of 6 bits each.
      ;; The 8 selection functions map these
      ;; 6-bit quantities into 4-bit quantities
      ;; and the results permuted
      ;; to make an f(R, K).
      ;; The indexing into the selection functions
      ;; is peculiar; it could be simplified by
      ;; rewriting the tables.
      (dotimes (j 8)
        (setq temp (* 6 j))
        (setq k (s j (+ (ash (pres (+ temp 0)) 5)
                        (ash (pres (+ temp 1)) 3)
                        (ash (pres (+ temp 2)) 2)
                        (ash (pres (+ temp 3)) 1)
                        (ash (pres (+ temp 4)) 0)
                        (ash (pres (+ temp 5)) 4))))
        (setq temp (* 4 j))
        (setf (f (+ temp 0)) (logand (ash k -3) 1))
        (setf (f (+ temp 1)) (logand (ash k -2) 1))
        (setf (f (+ temp 2)) (logand (ash k -1) 1))
        (setf (f (+ temp 3)) (logand (ash k -0) 1)))
      ;; The new R is L ^ f(R, K).
      ;; The f here has to be permuted first, though.
      (dotimes (j 32)
        (setf (r j) (logxor (l j) (f (- (p j) 1)))))
      ;; Finally, the new L (the original R)
      ;; is copied back.
      (dotimes (j 32)
        (setf (l j) (templ j))))
    ;; The output L and R are reversed.
    (dotimes (j 32)
      (setq temp (l j))
      (setf (l j) (r j))
      (setf (r j) temp))
    ;; The final output
    ;; gets the inverse permutation of the very original.
    (dotimes (j 64)
      (setf (block j) (l (- (fp j) 1))))))


(define-array bblock (66))
(define-array iobuf (16))

(defun crypt (pw salt &aux c temp)
  (macrolet ((pw (index) `(aref pw ,index))
             (salt (index) `(aref salt ,index)))
    (dotimes (i 65)
      (setf (bblock i) 0))
    (do ((i 0)
         (pwi 0 (1+ pwi))
         (n (length pw)))
        ((or (= pwi n)
             (= i 64)))
      (setq c (pw pwi))
      (dotimes (j 7)
        (setf (bblock i) (logand (ash c (- j 6)) 1))
        (incf i))
      (incf i))
    (setkey *bblock*)
    (dotimes (i 65)
      (setf (bblock i) 0))

    (dotimes (i 48)
      (setf (eb i) (e i)))

    (dotimes (i 2)
      (setq c (salt i))
      (setf (iobuf i) c)
      (if (> c #.(char-code #\Z)) (setq c (- c 6)))
      (if (> c #.(char-code #\9)) (setq c (- c 7)))
      (setq c (- c #.(char-code #\.)))
      (dotimes (j 6)
        (when (not (zerop (logand (ash c (- j)) 1)))
          (setq temp (eb (+ (* 6 i) j)))
          (setf (eb (+ (* 6 i) j)) (eb (+ (* 6 i) j 24)))
          (setf (eb (+ (* 6 i) j 24)) temp))))

    (dotimes (i 25)
      (encrypt *bblock* nil))

    (dotimes (i 11)
      (setq c 0)
      (dotimes (j 6)
        (setq c (ash c 1))
        (setq c (logior c (bblock (+ (* 6 i) j))))
        (setq c (+ c #.(char-code #\.)))
        (if (> c #\9) (setq c (+ c 7)))
        (if (> c #\Z) (setq c (+ c 6)))
        (setf (iobuf (+ i 2)) c)))

;    (setf (iobuf (+ i 2)) 0)

    (if (= (iobuf 1) 0)
        (setf (iobuf 1) (iobuf 0)))

    *iobuf*))
