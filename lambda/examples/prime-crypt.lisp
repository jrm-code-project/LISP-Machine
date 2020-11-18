;;; -*- Mode:LISP; Package:USER; Base:10; Readtable:CL -*-

#||

Copyright LISP Machine, Inc. 1985, 1986
   See filename "Copyright.Text" for
licensing and release information.

*********************************************************
*********************************************************
*** NOTE: This is an EXAMPLE, not LMI supported code. ***
*** information contained in this example is subject  ***
*** to change without notice. The ways of doing       ***
*** the things contained in the example may change    ***
*** between system releases. Some techniques which    ***
*** are mere examples in one release may become built ***
*** in system features in the next release. Use good  ***
*** judgement when copying these techniques. Most     ***
*** examples have been motivated by specific customer ***
*** requests, and may not be the best engineered      ***
*** or most efficient solution for someone else.      ***
*********************************************************
*********************************************************


 Encryption/Decryption functions. We might find these useful
 sometime. 2-Mar-86 11:22:32 -GJC

||#

(defun crypt (message key)
  "Message is a list of integers, key is a pair (<bigprime1> . <bigprime2>)"
  (mapcar (function
            (lambda (mblock)
              (crypt-block mblock key)))
          message))

(defun crypt-block (message-block key)
  (exponent-modulo message-block (car key) (cdr key)))

(defun exponent-modulo (a b c)
  (do ((foo 1)
       (imax (1+ (haulong b)))
       (i 1 (1+ i)))
      ((= i imax) foo)
    (setq foo ((lambda (temp)
                 (cond ((oddp (haipart b i))
                        (remainder (times temp a) c))
                       (t temp)))
               (remainder (times foo foo) c)))))

;;;
;;; Functions for defining keys.
;;;

(defun make-keys (max-digits)
  (do ((p (make-suitable-prime max-digits))
       (q (make-suitable-prime (+ max-digits (random 10) 1))
          (make-suitable-prime (+ max-digits (random 10) 1))))
      ((lessp (gcd (sub1 p) (sub1 q)) 1000)
       (prog (e d n phi)
             (setq n (times p q)
                   phi (difference n p q -1))
          loop (setq d (make-prime-bignum (* max-digits 2))
                     e ((lambda (trial-e)
                          (cond ((minusp trial-e)
                                 (plus phi trial-e))
                                (t trial-e)))
                        (inverse-modulo d phi)))
             (and (lessp e (haulong n)) (go loop))
             (return (cons (cons e n) (cons d n)))))))


(defun inverse-modulo (a b)
  (do ((olderx 0 oldx)
       (oldx b x)
       (x a (remainder oldx x))
       (oldb 0 b)
       (b 1  (plus oldb
                    (times b
                           (quotient (difference (remainder oldx x)
                                                 oldx)
                                     x)))))
      ((zerop x) oldb)))


(defun congruentp (a b c)
  (equal (remainder b c)
         (remainder a c)))

(defun make-suitable-prime (max-digits)
  (do ((u (make-prime-bignum max-digits))
       (i 2 (+ i 2))
       (foo 4 (add1 (times i u))))
      ((test-primality foo) foo)))


(defun make-prime-bignum (max-digits)
  (do ((bignum-to-test (make-bignum-to-test max-digits)
                       (make-bignum-to-test max-digits)))
      ((test-primality bignum-to-test) bignum-to-test)))


(defun test-primality (prime-candidate)
  (do ((i 1 (1+ i))
       (a (big-random prime-candidate)
          (big-random prime-candidate)))
      ((= i 30) t)
      ;;2^30 is about 1 billion
      (or (and (equal (gcd a prime-candidate) 1)
               (equal (cond ((= (Jacobi a prime-candidate) -1)
                             (sub1 prime-candidate))
                            (t 1))
                      (exponent-modulo a
                                       (quotient (sub1 prime-candidate)
                                                 2)
                                       prime-candidate)))
          (return nil))))

(defun big-random (limit)
  ;;1<=big-random<=limit-1
  (do ((foo (quotient (times limit (random 10000)) 10000)
            (quotient (times limit (random 10000)) 10000)))
      ((not (zerop foo)) foo)))

(defun Jacobi (a b)
  (cond ((equal a 1) 1)
        ((not (oddp a))
         (times (Jacobi (quotient a 2) b)
                (sign-power (quotient (sub1 (times b b)) 8))))
        (t
         (times (Jacobi (remainder b a) a)
                (sign-power (quotient (times (sub1 a) (sub1 b)) 4))))))

(defun sign-power (n)
  (cond ((oddp n) -1)
        (t 1)))

(defun make-bignum-to-test (max-digits)
  (do ((i 1 (1+ i))
       (next-digit (random-first-digit) (random-digit))
       (foo 0 (plus (times foo 10) next-digit)))
      ((= i max-digits)
       (plus (times foo 10) (random-final-digit)))))


(defun random-digit ()
  (random 10))

(defun random-first-digit ()
  (do ((foo (random-digit) (random-digit)))
      ((not (zerop foo)) foo)))


(defun random-final-digit ()
  (do ((foo (random-digit) (random-digit)))
      ((member foo '(1 3 7 9))
       foo)))
