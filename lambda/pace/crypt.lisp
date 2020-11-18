;;; -*- Mode:LISP; Package:CRYPT; Base:10; Readtable:CL -*-


;/* @(#)crypt.c 4.1 (Berkeley) 12/21/80 */
;/*
; * This program implements the
; * Proposed Federal Information Processing
; *  Data Encryption Standard.
; * See Federal Register, March 17, 1975 (40FR12134)
; */

(defvar *IP*)
(defvar *FP*)
(defvar *PC1_C*)
(defvar *PC1_D*)
(defvar *shifts*)
(defvar *PC2_C*)
(defvar *PC2_D*)
(defvar *C*)
(defvar *D*)
(defvar *KS*)
(defvar *big-E*)
(defvar *little-e*)
(defvar *S*)
(defvar *P*)
(defvar *L*)
(defvar *R*)
(defvar *tempL*)
(defvar *F*)
(defvar *block*)
(defvar *iobuf*)
(defvar *preS*)

;/*
; * The combination of the key and the input, before selection.
; */
(defun initialize ()
;/*
; * Initial permutation,
; */
  (setq *IP* (make-array 64. :leader-list '(nil *IP*)))
  (fillarray *IP*
             '(58 50 42 34 26 18 10  2
                  60 52 44 36 28 20 12  4
                  62 54 46 38 30 22 14  6
                  64 56 48 40 32 24 16  8
                  57 49 41 33 25 17  9  1
                  59 51 43 35 27 19 11  3
                  61 53 45 37 29 21 13  5
                  63 55 47 39 31 23 15  7 ))
;/*
; * Final permutation, FP = IP^(-1)
; */
  (setq *FP* (make-array 64. :leader-list '(nil *FP*)))
  (fillarray *FP*
             '(40  8 48 16 56 24 64 32
                   39  7 47 15 55 23 63 31
                   38  6 46 14 54 22 62 30
                   37  5 45 13 53 21 61 29
                   36  4 44 12 52 20 60 28
                   35  3 43 11 51 19 59 27
                   34  2 42 10 50 18 58 26
                   33  1 41  9 49 17 57 25 ))
;/*
; * Permuted-choice 1 from the key bits
; * to yield C and D.
; * Note that bits 8,16... are left out:
; * They are intended for a parity check.
; */
  (setq *PC1_C* (make-array 28. :leader-list '(nil *PC1_C*)))
  (fillarray *PC1_C*
             '(57 49 41 33 25 17  9
                  1 58 50 42 34 26 18
                  10  2 59 51 43 35 27
                  19 11  3 60 52 44 36 ))
  (setq *PC1_D* (make-array 28. :leader-list '(nil *PC1_D*)))
  (fillarray *PC1_D*
             '(63 55 47 39 31 23 15
                  7 62 54 46 38 30 22
                  14  6 61 53 45 37 29
                  21 13  5 28 20 12  4 ))
;/*
; * Sequence of shifts used for the key schedule.
; */
  (setq *shifts* (make-array 16. :leader-list '(nil *shifts*)))
  (fillarray *shifts*
             '(1 1 2 2 2 2 2 2 1 2 2 2 2 2 2 1 ))
;/*
; * Permuted-choice 2, to pick out the bits from
; * the CD array that generate the key schedule.
; */
  (setq *PC2_C* (make-array 24. :leader-list '(nil *PC2_C*)))
  (fillarray *PC2_C*
             '(14 17 11 24  1  5
                  3 28 15  6 21 10
                  23 19 12  4 26  8
                  16  7 27 20 13  2 ))
  (setq *PC2_D* (make-array 24. :leader-list '(nil *PC2_D*)))
  (fillarray *PC2_D*
             '(41 52 31 37 47 55
                  30 40 51 45 33 48
                  44 49 39 56 34 53
                  46 42 50 36 29 32 ))

;/*
; * The C and D arrays used to calculate the key schedule.
; */

  (setq *C* (make-array 28. :leader-list '(nil *C*)))
  (setq *D* (make-array 28. :leader-list '(nil *D*)))
;/*
; * The key schedule.
; * Generated from the key.
; */
  (setq *KS* (make-array '(16. 48.) :leader-list '(nil *KS*)))

;/*
; * The E bit-selection table.
; */
  (setq *big-E* (make-array 48. :leader-list '(nil *big-E*)))
  (setq *little-e* (make-array 48. :leader-list '(nil *little-e*)))
  (fillarray *little-e* '(32  1  2  3  4  5
                              4  5  6  7  8  9
                              8  9 10 11 12 13
                              12 13 14 15 16 17
                              16 17 18 19 20 21
                              20 21 22 23 24 25
                              24 25 26 27 28 29
                              28 29 30 31 32  1 ))
;/*
; * The 8 selection functions.
; * For some reason, they give a 0-origin
; * index, unlike everything else.
; */
  (setq *S* (make-array '(8 64.) :leader-list '(nil *S*)))
  (fillarray *S* '(14  4 13  1  2 15 11  8  3 10  6 12  5  9  0  7
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
;/*
; * P is a permutation on the selected combination
; * of the current L and key.
; */
  (setq *P* (make-array 32. :leader-list '(nil *P*)))
  (fillarray *P* '(16  7 20 21
                       29 12 28 17
                       1 15 23 26
                       5 18 31 10
                       2  8 24 14
                       32 27  3  9
                       19 13 30  6
                       22 11  4 25 ))
;/*
; * The current block, divided into 2 halves.
; */
  (setq *L* (make-array 32. :leader-list '(nil *L*)))
  (setq *R* (make-array 32. :leader-list '(nil *R*)))
  (setq *tempL* (make-array 32. :leader-list '(nil *tempL*)))
  (setq *F* (make-array 32. :leader-list '(nil *F*)))

;/*
; * The combination of the key and the input, before selection.
; */
  (setq *preS* (make-array 48. :leader-list '(nil *preS*)))

  (setq *block* (make-array 66. :leader-list '(nil *block*)))
  (setq *iobuf* (make-array 13. :leader-list '(nil *iobuf*) :type :art-string))
  )

;/*
; * Set up the key schedule from the key.
; */

(defun setkey (key)
;       /*
;        * First, generate C and D by permuting
;        * the key.  The low order bit of each
;        * 8-bit char is not used, so C and D are only 28
;        * bits apiece.
;        */
;       for (i=0; i<28; i++) {
;               C[i] = key[PC1_C[i]-1];
;               D[i] = key[PC1_D[i]-1];
;       }
  (dotimes (i 28.)
    (setf (aref *C* i) (aref key (- (aref *PC1_C* i) 1)))
    (setf (aref *D* i) (aref key (- (aref *PC1_D* i) 1))))
;       /*
;        * To generate Ki, rotate C and D according
;        * to schedule and pick up a permutation
;        * using PC2.
;        */
;       for (i=0; i<16; i++) {
  (dotimes (i 16.)
;               /*
;                * rotate.
;                */
;               for (k=0; k<shifts[i]; k++) {
;                       t = C[0];
;                       for (j=0; j<28-1; j++)
;                               C[j] = C[j+1];
;                       C[27] = t;
;                       t = D[0];
;                       for (j=0; j<28-1; j++)
;                               D[j] = D[j+1];
;                       D[27] = t;
;               }
    (dotimes (k (aref *shifts* i))
      (let ((temp (aref *C* 0)))
        (dotimes (j 27.)
          (setf (aref *C* j) (aref *C* (1+ j))))
        (setf (aref *C* 27.) temp))
      (let ((temp (aref *D* 0)))
        (dotimes (j 27.)
          (setf (aref *D* j) (aref *D* (1+ j))))
        (setf (aref *D* 27.) temp))
      )
;               /*
;                * get Ki. Note C and D are concatenated.
;                */
;               for (j=0; j<24; j++) {
;                       KS[i][j] = C[PC2_C[j]-1];
;                       KS[i][j+24] = D[PC2_D[j]-28-1];
;               }
    (dotimes (j 24.)
      (setf (aref *KS* i j) (aref *C* (- (aref *PC2_C* j) 1)))
      (setf (aref *KS* i (+ j 24.)) (aref *D* (- (aref *PC2_D* j) 28. 1))))
;       }
    )
;}
  )

;/*
; * The payoff: encrypt a block.
; */

(defun encrypt (block edflag &aux i)
;{
;       int i, ii;
;       register t, j, k;

;       /*
;        * First, permute the bits in the input
;        */
;       for (j=0; j<64; j++)
;               L[j] = block[IP[j]-1];
  (dotimes (j 64.)
    (cond ((< j 32.)
           (setf (aref *L* j) (aref block (1- (aref *IP* j)))))
          (t
           (setf (aref *R* (- j 32.)) (aref block (1- (aref *IP* j)))))))
;       /*
;        * Perform an encryption operation 16 times.
;        */
;       for (ii=0; ii<16; ii++) {
  (dotimes (ii 16.)
;               /*
;                * Set direction
;                */
;               if (edflag)
;                       i = 15-ii;
;               else
;                       i = ii;
    (cond ((null edflag)
           (setq i ii))
          (t
           (setq i (- 15. ii))))
;               /*
;                * Save the R array,
;                * which will be the new L.
;                */
;               for (j=0; j<32; j++)
;                       tempL[j] = R[j];
    (dotimes (j 32.)
      (setf (aref *tempL* j) (aref *R* j)))
;               /*
;                * Expand R to 48 bits using the E selector;
;                * exclusive-or with the current key bits.
;                */
;               for (j=0; j<48; j++)
;                       preS[j] = R[E[j]-1] ^ KS[i][j];
    (dotimes (j 48.)
      (setf (aref *preS* j) (logxor (aref *R* (1- (aref *big-E* j)))
                                    (aref *KS* i j))))
;               /*
;                * The pre-select bits are now considered
;                * in 8 groups of 6 bits each.
;                * The 8 selection functions map these
;                * 6-bit quantities into 4-bit quantities
;                * and the results permuted
;                * to make an f(R, K).
;                * The indexing into the selection functions
;                * is peculiar; it could be simplified by
;                * rewriting the tables.
;                */
;               for (j=0; j<8; j++) {
;                       t = 6*j;
;                       k = S[j][(preS[t+0]<<5)+
;                               (preS[t+1]<<3)+
;                               (preS[t+2]<<2)+
;                               (preS[t+3]<<1)+
;                               (preS[t+4]<<0)+
;                               (preS[t+5]<<4)];
;                       t = 4*j;
;                       f[t+0] = (k>>3)&01;
;                       f[t+1] = (k>>2)&01;
;                       f[t+2] = (k>>1)&01;
;                       f[t+3] = (k>>0)&01;
;               }
    (dotimes (j 8)
      (let (temp k)
        (setq temp (* 6 j))
        (setq k (aref *S* j (+ (ash (aref *preS* (+ temp 0)) 5)
                               (ash (aref *preS* (+ temp 1)) 3)
                               (ash (aref *preS* (+ temp 2)) 2)
                               (ash (aref *preS* (+ temp 3)) 1)
                               (ash (aref *preS* (+ temp 4)) 0)
                               (ash (aref *preS* (+ temp 5)) 4))))
        (setq temp (* 4 j))
        (setf (aref *F* (+ temp 0)) (logand (ash k -3) 1))
        (setf (aref *F* (+ temp 1)) (logand (ash k -2) 1))
        (setf (aref *F* (+ temp 2)) (logand (ash k -1) 1))
        (setf (aref *F* (+ temp 3)) (logand (ash k -0) 1))))
;               /*
;                * The new R is L ^ f(R, K).
;                * The f here has to be permuted first, though.
;                */
;               for (j=0; j<32; j++)
;                       R[j] = L[j] ^ f[P[j]-1];
    (dotimes (j 32.)
      (setf (aref *R* j) (logxor (aref *L* j)
                                 (aref *F* (1- (aref *P* j))))))
;               /*
;                * Finally, the new L (the original R)
;                * is copied back.
;                */
;               for (j=0; j<32; j++)
;                       L[j] = tempL[j];
    (dotimes (j 32.)
      (setf (aref *L* j) (aref *tempL* j)))
;       }
    )
;       /*
;        * The output L and R are reversed.
;        */
;       for (j=0; j<32; j++) {
;               t = L[j];
;               L[j] = R[j];
;               R[j] = t;
;       }
  (dotimes (j 32.)
    (swapf (aref *L* j) (aref *R* j)))
;       /*
;        * The final output
;        * gets the inverse permutation of the very original.
;        */
;       for (j=0; j<64; j++)
;               block[j] = L[FP[j]-1];
  (dotimes (j 64.)
    (let ((index (1- (aref *FP* j))))
      (cond ((< index 32.)
             (setf (aref block j) (aref *L* index)))
            (t
             (setf (aref block j) (aref *R* (- index 32.)))))))
;}
  )

;char *
(defun crypt (pw salt)
;char *pw;
;char *salt;
;{
;       register i, j, c;
;       int temp;
;       static char block[66], iobuf[16];
;       for(i=0; i<66; i++)
;               block[i] = 0;
;       for(i=0; (c= *pw) && i<64; pw++){
;               for(j=0; j<7; j++, i++)
;                       block[i] = (c>>(6-j)) & 01;
;               i++;
;       }
;  (do ((i 0)
;       (char-index 0 (1+ char-index))
;       (end (string-length pw)))
;      ((not (and (not (= char-index end))
;                (< i 64.))))
;    (do ((j 0 (1+ j)))
;       ((= j 7))
;      (setf (aref *block* i) (logand (ash (aref pw char-index) (- 6 j)) 1))
;      (incf i)))
  (array-initialize *block* 0)
  (dotimes (char-index (array-length pw))
    (dotimes (bit 7)
      (setf (aref *block* (+ (* char-index 8) bit)) (ldb (byte 1 (- 6 bit)) (aref pw char-index)))))

;       setkey(block);
  (setkey *block*)

;       for(i=0; i<66; i++)
;               block[i] = 0;
  (array-initialize *block* 0)

;       for(i=0;i<48;i++)
;               E[i] = e[i];
  (copy-array-contents *little-e* *big-E*)

;       for(i=0;i<2;i++){
  (dotimes (i 2)
;               c = *salt++;
;               iobuf[i] = c;
;               if(c>'Z') c -= 6;
;               if(c>'9') c -= 7;
;               c -= '.';
    (let ((c (aref salt i)))
      (setf (aref *iobuf* i) c)
      (when (char> c #\Z)
        (decf c 6))
      (when (char> c #\9)
        (decf c 7))
      (decf c #\.)
;               for(j=0;j<6;j++){
;                       if((c>>j) & 01){
;                               temp = E[6*i+j];
;                               E[6*i+j] = E[6*i+j+24];
;                               E[6*i+j+24] = temp;
;                               }
;                       }
;               }
      (dotimes (j 6)
        (when (not (zerop (logand (ash c (- j)) 1)))
          (swapf (aref *big-E* (+ (* 6 i) j)) (aref *big-E* (+ (* 6 i) j 24.)))))

      ))
;       for(i=0; i<25; i++)
;               encrypt(block,0);

  (dotimes (i 25.)
    (encrypt *block* nil))

;       for(i=0; i<11; i++){
;               c = 0;
;               for(j=0; j<6; j++){
;                       c <<= 1;
;                       c |= block[6*i+j];
;                       }
;               c += '.';
;               if(c>'9') c += 7;
;               if(c>'Z') c += 6;
;               iobuf[i+2] = c;
;       }
  (dotimes (i 11.)
    (let ((c 0))
      (dotimes (j 6.)
        (setq c (logior (ash c 1)
                        (aref *block* (+ (* 6 i) j)))))
      (incf c #\.)
      (when (char> c #\9) (incf c 7))
      (when (char> c #\Z) (incf c 6))
      (setf (aref *iobuf* (+ i 2)) c)))

;       iobuf[i+2] = 0;
;       if(iobuf[1]==0)
;               iobuf[1] = iobuf[0];
;       return(iobuf);
  (when (zerop (aref *iobuf* 1))
    (setf (aref *iobuf* 1) (aref *iobuf* 0)))
;}
  *iobuf*
  )


;----
;static char *sccsid = "@(#)crypt.c     4.2 (Berkeley) 7/9/81";
;
;/*
; *     A one-rotor machine designed along the lines of Enigma
; *     but considerably trivialized.
; */

;#define ECHO 010
;#include <stdio.h>
(defconst ROTORSZ 256)
(defconst MASK 0377)

(defvar *t1*)
(defvar *t2*)
(defvar *t3*)

(defun initialize-2 ()
  (setq *t1* (make-array ROTORSZ :initial-element 0))
  (setq *t2* (make-array ROTORSZ :initial-element 0))
  (setq *t3* (make-array ROTORSZ :initial-element 0))
  )

(defun 32-bit-* (a b)
  (logand #o37777777777 (* a b)))

(defun setup-2 (pw)
  (let ((pw-8 (make-array 8 :type :art-8b))
        (salt (make-array 2 :type :art-8b)))
    (copy-array-contents pw pw-8)
    (copy-array-contents pw salt)
    (let ((key (crypt pw-8 salt))
          (seed 123.))
      (dotimes (i 13.)
        (setq seed (+ (32-bit-* seed (aref key i)) i)))
      (dotimes (i ROTORSZ)
        (setf (aref *t1* i) i))
      (dotimes (i ROTORSZ)
        (setq seed (logand #o377777777
                           (+ (32-bit-* 5 seed)
                              (aref key (mod i 13.)))))
        (let* ((random (mod seed 65521))
               (k (- (- ROTORSZ 1) i))
               (ic (mod (logand random MASK)
                        (+ k 1))))
          (setq random (ash random -8))
          (swapf (aref *t1* k) (aref *t1* ic))
          (when (zerop (aref *t3* k))
            (setq ic (mod (logand random MASK) k))
            (loop until (not (zerop (aref *t3* ic)))
                  do (setq ic (mod (+ ic 1) k)))
            (setf (aref *t3* k) ic)
            (setf (aref *t3* ic) k))))))

  (dotimes (i ROTORSZ)
    (setf (aref *t2* (logand (aref *t1* i) MASK)) i)))

(defun main-crypt (pw text)
  (setup-2 pw)

  (with-output-to-string (output)
    (let ((n1 0)
          (n2 0)
          (nr2 0)
          nr1)
      (dotimes (char-index (array-length text))
        (setq nr1 n1)
        (send output :tyo
              (- (aref *t2*
                       (logand (- (aref *t3* (logand (+ (aref *t1* (logand (+ (aref text char-index) nr1) MASK))
                                                        nr2)
                                                     MASK))
                                  nr2)
                               MASK))
                 nr1))
        (incf n1)
        (when (= n1 ROTORSZ)
          (setq n1 0)
          (incf n2)
          (when (= n2 ROTORSZ)
            (setq n2 0))
          (setq nr2 0))))))
