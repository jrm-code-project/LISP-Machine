;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-

#8r si:
(DEFUN PRINT-BIGNUM (BIGNUM STREAM &AUX (base (current-print-base)))
  (declare (unspecial base))
  (cond ((< bignum 1e100)
         (when *print-radix*
           (print-print-radix-prefix base stream))
         (WHEN (MINUSP BIGNUM)
           (SEND STREAM :TYO (PTTBL-MINUS-SIGN *READTABLE*)))
         (IF (FIXNUMP BASE)
             (PRINT-RAW-BIGNUM BIGNUM BASE STREAM)
           (FUNCALL (GET BASE 'PRINC-FUNCTION) (- BIGNUM) STREAM))
         (WHEN (AND (OR *PRINT-RADIX* (NOT *NOPOINT))
                    (EQ BASE 10.))
           (SEND STREAM :TYO (PTTBL-DECIMAL-POINT *READTABLE*)))
         BIGNUM)
        (t
         (printing-random-object (bignum stream)
           (format stream "Bignum with about ~d digits"
                   (round (haulong bignum) (log base 2)))))))


(defun find-d (u v)
  ;; (gcd e m) == 1
  ;; therefore, there exists x0 and y0 so that e*x0+m*y0=1
  ;; d = x0 mod m
  ;;from Knuth vol2 p302 Algorithm X
  (let ((u1 1) (u2 0) (u3 u)
        (v1 0) (v2 1) (v3 v)
        )
    (do ()
        ((zerop v3)
         (values u1 u2 u3))
      (let ((q (floor u3 v3)))
        (psetq u1 v1
               u2 v2
               u3 v3
               v1 (- u1 (* v1 q))
               v2 (- u2 (* v2 q))
               v3 (- u3 (* v3 q)))))))

;       (setq t1 (- u1 (* v1 q)))
;       (setq t2 (- u2 (* v2 q)))
;       (setq t3 (- u3 (* v3 q)))
;       (setq u1 v1)
;       (setq u2 v2)
;       (setq u3 v3)
;       (setq v1 t1)
;       (setq v2 t2)
;       (setq v3 t3)))))

;;numbers right around (isqrt m) seem to make the encryption function its
;;own inverse!!
(defun find-e (m)
  (do ((e (round (* 11/10 (isqrt m))) (1+ e)))
      ((= (gcd e m) 1) e)))

(defun find-weight (p q)
  (do ((w q (+ w q)))
      ((= (mod w p) 1) w)))

(defvar *p* 5)
(defvar *q* 11.)

(defvar *n*)
(defvar *m*)

(defvar *e*)
(defvar *d*)

(defvar *x1* nil)
(defvar *x2* nil)

(defun print-numbers ()
  (format t "~&P=~d Q=~d N=~d M=~d E=~d D=~d"
          *p* *q* *n* *m* *e* *d*))

(defun setup-numbers ()
  (setq *n* (* *p* *q*))
  (setq *m* (* (- *p* 1) (- *q* 1)))
  (setq *e* (find-e *m*))
  (setq *d* (mod (find-d *e* *m*) *m*))
  (setq *x1* (find-weight *p* *q*))
  (setq *x2* (find-weight *q* *p*))
  )


(defun slow-rsa-encrypt (s)
  (setup-numbers)
  (mod (^ s *e*) *n*))

(defun slow-rsa-decrypt (s)
  (setup-numbers)
  (mod (^ s *d*) *n*))

(defun exp1 (s a n)
  ;;(s ^ a) mod n
  (do ((prod 1)
       (r 0 (1+ r))
       (squared s (mod (* squared squared) n))
       (end (haulong a)))
      ((= r end)
       prod)
    (if (ldb-test (byte 1 r) a)
        (setq prod (mod (* prod squared) n)))))

(defun test1 ()
  (print-numbers)
  (dotimes (i 20)
    (format t "~&~d ~d ~d ~d ~d"
            i (slow-rsa-encrypt i) (exp2 i *e* *n*)
            (slow-rsa-encrypt (slow-rsa-encrypt i))
            (slow-rsa-decrypt (slow-rsa-encrypt i))

    )))

(defun test2 ()
  (print-numbers)
  (dotimes (i 20)
    (let* ((crypted (exp2 i *e* *n*))
           (crypted2 (exp2 crypted *e* *n*))
           (clear (exp2 crypted *d* *n*)))
      (format t "~&~d ~d ~d ~d"
              i crypted crypted2 clear))))


(defun fast-times (a b)
  (mod (+ (mod (* (mod (* (mod a *p*) (mod b *p*)) *p*) *x1*) *n*)
          (mod (* (mod (* (mod a *q*) (mod b *q*)) *q*) *x2*) *n*))
       *n*))

(defun exp2 (s a n)
  ;;(s ^ a) mod n
  (do ((prod 1)
       (r 0 (1+ r))
       (squared s (fast-times squared squared))
       (end (haulong a)))
      ((= r end)
       prod)
    (if (ldb-test (byte 1 r) a)
        (setq prod (fast-times prod squared)))))

(defun rabin-composite-p (n a)
  (when (not (<= 1 a (- n 1)))
    (ferror nil "bad a"))
  (let ((m (- n 1))
        (k 0))
    (do ()
        ((oddp m))
      (setq m (ash m -1))
      (incf k))
    (let ((x0 (exp1 a m n)))
      (cond ((= x0 1)
             nil)
            (t
             (do ((i 1 (1+ i))
                  (xi (mod (* x0 x0) n) (mod (* xi xi) n))
                  (xi-1 x0 xi))
                 ((> i k)
                  t)
               (when (= xi 1)
                 (cond ((= xi-1 -1)
                        (return nil))
                       (t
                        (return t))))))))))

(defun find-small-primes ()
  (let* ((sieve-length 3000)
         (sieve (make-array sieve-length :type :art-1b))
         prime-list
         )
    (do ((start 2 (+ start 1)))
        ((>= start sieve-length)
         (reverse prime-list))
      (when (zerop (aref sieve start))
        (push start prime-list)
        (do ((index (+ start start) (+ index start)))
            ((>= index sieve-length))
          (aset 1 sieve index))))))

(defvar *small-primes* (find-small-primes))

(defun divisible-by-small-prime-p (n)
  (dolist (p *small-primes*)
    (cond ((>= p n)
           (return nil))
          ((zerop (remainder n p))
           (return t)))))

(defvar *rsa-random-state* (make-random-state))

(defun choose-a (n)
  (do ((a (random n *rsa-random-state*)
          (random n *rsa-random-state*)))
      ((>= a 1) a)))

(defun rabin-iterations-needed (probability)
  (values (ceiling (- 3 (log (float probability) 4)))))

(defun probably-prime-p (n probability)
  (cond ((divisible-by-small-prime-p n)
         nil)
        (t
         (dotimes (i (rabin-iterations-needed probability))
           (when (rabin-composite-p n (choose-a n))
             (return nil)))
         t)))

(defconst *prime-probability-tolerance* 1e-5)

(defun find-next-probable-prime (lower-bound)
  (do ((n (if (evenp lower-bound)
              (+ lower-bound 1)
            (+ lower-bound 2))
          (+ n 2)))
      ((probably-prime-p n *prime-probability-tolerance*)
       n)))

(defvar *c12*)
(defvar *c21*)

(defun find-cs ()
  (setq *c12* (find-d *p* *q*))
  (setq *c21* (find-d *q* *p*)))

(defun fast-times (a b)
  (let ((u1 (mod (* (mod a *p*) (mod b *p*)) *p*))
        (u2 (mod (* (mod a *q*) (mod b *q*)) *q*)))
    (+ u1
       (* *p*
          (mod (* *c12* (- u2 u1)) *q*)))))

(defun decimal-digits-to-bits (digits)
  (round (* digits (log 10. 2))))

(defconst *number-of-bits-in-keys* (decimal-digits-to-bits 10.))

(defun string-to-key-base (string)
  (let ((number 0))
    (let ((expansion-factor (max (round (/ *number-of-bits-in-keys*
                                           (* 7 (string-length string))))
                                 1)))
      (dotimes (i (string-length string))
        (dotimes (j 7)
          (let ((bit-pos (* expansion-factor (+ (* i 7) j))))
            (when (> bit-pos *number-of-bits-in-keys*)
              (return nil))
            (setq number (dpb (ldb (byte 1 j) (aref string i))
                              (byte 1 bit-pos)
                              number))))))
    number))

(defun string-to-private-key (string)
  (let ((lower-bound (string-to-key-base string)))
    (find-next-probable-prime lower-bound)))

(defun generate-random-prime ()
  (let ((biggest (ash 1 *number-of-bits-in-keys*)))
    (find-next-probable-prime (+ (ash biggest -10)
                                 (random (- biggest (ash biggest -10)) *rsa-random-state*)))))
