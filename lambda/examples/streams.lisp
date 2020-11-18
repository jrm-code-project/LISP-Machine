;;; -*- Mode:LISP; Package:(STE global); Fonts:(cptfont); Base:8 -*-

;; Copyright LISP Machine, Inc. 1984, 1985, 1986
;;   See filename "Copyright.Text" for
;; licensing and release information.

#||

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

||#

;; A self-contained example of streams software usage for
;; testing the performance of and documenting the LAMBA<->UNIX interface.
;; This code runs in system version 1.120, unix-interface version 12.
;; 10/13/84 00:10:24 -George Carrette.
;; modified for release II beta-test 2/26/85 13:23:09 -George Carrette.

;; To run the tests:
;; (1) Create the C programs by running (CREATE-C-PROGRAMS)
;;     These functions illustrate some of the higher level protocals.
;; (2) Create a split-screen with two lisp listeners.
;;     Use RUN-C-PROGRAM in the top window, switch to the bottom and
;;     use the corresponding lisp function.

(defun attached-unix-host ()
  "Returns host object for attached unix-host if it exits otherwise NIL"
;; Relevant variable:
;; si:*other-processors* list of structures of type SI:OTHER-PROCESSOR
  (dolist (op si:*other-processors*)
    (let ((host (SI:GET-HOST-FROM-ADDRESS
                  (si:%processor-conf-chaos-address (si:op-proc-conf op))
                  ':CHAOS)))
      (if (typep host 'fs:unix-host)
          (return host)))))

(defun temp-unix-path (name type)
  (fs:make-pathname ':host (attached-unix-host)
                    ':directory "TMP"
                    ':name (string-append (string-upcase si:user-id)
                                          "-"
                                          name)
                    ':type type))

;; a simple "null-device" for testing.

(defconst *p* (open "UNIX-STREAM-4:"))

(defun null-device (message &rest ignored)
  (selectq message
    (:tyi 0)
    (:tyipeek 0)
    (:which-operations '(:tyo :tyi :tyipeek :untyi :string-out))))

(defconst *null* (closure () #'null-device))

;; This uses the FILE protocal and EVAL protocal.

(defun share-compile-string (name string)
  "writes out the string as name.c and C compiles it to name"
  (with-open-file (stream (temp-unix-path name "C") ':out)
    (princ string stream))
  (simple-unix-eval (attached-unix-host)
                    (format nil "cc ~A -o ~A -lshare"
                            (send (temp-unix-path name "C") ':string-for-host)
                            (send (temp-unix-path name ':unspecific)
                                  ':string-for-host))))

(defun simple-unix-eval (host command)
  (with-open-stream (s (chaos:open-stream host
                                          (format nil "EVAL ~a" command)))
    (format t "~&% ~A~%" command)
    (do ((c (send s ':tyi) (send s ':tyi)))
        ((null c))
      (send standard-output ':tyo
            (selectq c
              ((12 15) #\return)
              (11 #\tab)
              (t c))))))


(defvar *c-programs* ())

(defun enter-c-program (name string)
  (setq *c-programs* (delq (ass #'string-equal name *c-programs*)
                           *c-programs*))
  (push (list name string) *c-programs*)
  name)

(defun create-c-programs ()
  (dolist (p *c-programs*)
    (create-c-program (car p))))

(defun create-c-program (x)
  (let ((p (ass #'string-equal x *c-programs*)))
    (format t "~&;Writing and compiling ~A.C" (car p))
    (apply #'share-compile-string p)))

(defun run-c-program (name)
  (simple-unix-eval (attached-unix-host)
                    (send (temp-unix-path name ':unspecific)
                          ':string-for-host)))


;;; The tests

;;; open loop frequency

(defun test-olf (&optional (n 1000.) (stream *p*) &aux time)
  (setq time (time))
  (do ((j 0 (1+ j)))
      ((= j n)
       (send stream ':tyo #/S))
    (send stream ':tyo #/?))
  (list (quotient n (quotient (time-difference (time) time) 60.0))
        "cycles per second"))


(enter-c-program "OLFT" '
|//*  program for open-loop sink response *//
#include <stdio.h>
main()
 {int f,n; char c[1];
  f = open("//dev//ttyl4",2);
  if (f < 0) {printf(/"open lost\n/"); exit(0);}
   while(1)
   { n = read(f,c,1);
     if (n == 0) {printf("got end of file\n"); exit(1);}
     if (n < 0) {printf("read lost\n"); exit(0);}
     if (*c == 'S') {printf("Been told to stop\n"); exit(1);}}}
|)

;; the closed loop frequence is the basic "remote-function-call"
;; overhead time. With this implementation it is highly dependant on,
;; and usually limited by the lisp scheduler timing because
;; of the process-wait which encumbers the ':tyi to the unix share tty.
;; As things stand, without adding an interrupt driven process wakeup
;; feature to the lispmachine system, the unix processor can
;; affect a process on the lispmachine in no less than 1/60'th of
;; a second. realtime programming applications needing faster response
;; times should consider more low-level clock-break and scheduler
;; modifications. A faster speciallized remote function call mechanism
;; itself calls for a special microcoded function. However, the
;; following is more than reasonable for any job that takes more
;; than half a second in the unix processor.

(defun test-clf (&optional (n 100.) (stream *p*) &aux time)
  (setq time (time))
  (do ((j 0 (1+ j)))
      ((= j n)
       (send stream ':tyo #/S)
       (print (if (eq (send stream ':tyi) #/O)
                  "Unix process stopped ok"
                "Unix process failed to reply to stop")))
    (send stream ':tyo #/?)
    (send stream ':tyi))
  (list (quotient n (quotient (time-difference (time) time) 60.0))
        "cycles per second"))

(enter-c-program "CLFT" '
|//* program freq.c for closed-loop. *//
#include <stdio.h>
main()
 {int f,n; char c[1];
  f = open("//dev//ttyl4",2);
  if (f < 0) {printf("open lost\n"); exit(0);}
  while(1)
  { n = read(f,c,1);
    if (n == 0) {printf("got end of file\n"); exit(1);}
    if (n < 0) {printf("read lost\n");exit(0);}
    if (*c == 'S') {printf("Been told to stop\n");
                    c[0] = 'O';
                    write(f,c,1);
                    exit(1);}
    n = write(f,c,1);
    if (n < 0) {printf("write lost\n"); exit(0);}}}
|)


(defsetf si:share-mem-read si:share-mem-write)

(defun share-mem-read-single-float (addr)
  (float-68000-32b (si:share-mem-read addr)))


(defmacro share-mem-read-bit (j)
  `(ldb (byte 1 (remainder ,j 32))
        (si:share-mem-read (quotient ,j 32))))

(defun float-68000-32b (x)
  "Take 32bits, a 68000 float, and return a lisp float object"
  ;; note: This takes byte reversal into account. It doesnt try to be
  ;; efficient in its use of lispmachine arithmetic.
  (// (* (expt -1 (ldb #o3701 x))
         (expt 2.0 (- (ldb #o3007 x) #o100))
         (+ (ldb #o2010 x)
            (ash (+ (ldb #o1010 x)
                    (ash (ldb #o0010 x)
                         8.))
                 8.)))
      #o100000000))


(defun 68000-32b-float (x &aux sign exp frac)
  "Take a lispmachine floating point number and return 32 bits suitable for the 68000"
  (cond ((zerop x)
         0)
        ('else
         (cond ((small-floatp x)
                (cond ((< x 0.0)
                       (setq sign 1)
                       (setq x (- x)))
                      (t
                       (setq sign 0)))
                (setq exp (+ (- (si:%short-float-exponent x) #o101) #o100))
                (setq frac (ash (- (si:%short-float-mantissa x) (expt 2 16))
                                (- 23 16))))
               ((floatp x)
                (cond ((< x 0.0)
                       (setq sign 1)
                       (setq x (- x)))
                      (t
                       (setq sign 0)))
                (setq exp (+ (- (si:%single-float-exponent x) #o2001) 127))
                (setq frac (ash (- (si:%single-float-mantissa x) (expt 2 30))
                                (- 23 30))))
               (t
                (ferror nil "Not a floating point number: ~S" x)))
         (ferror nil "foo, work on this tommorow"))))


(defun test-inc-loop  (&optional (n 100.) &aux time)
  (setq time (time))
  (do ((j 0 (1+ j))(value))
      ((= j n)
       (send *p* ':tyo #/S)
       (print (if (eq (send *p* ':tyi) #/O)
                  "Unix process stopped ok"
                "Unix process failed to reply to stop")))
    (setq value (test-inc-1 j))
    (or (= value (1+ j))
        (format t "~&;Error, expecting ~D, got ~D"
                (1+ j) value)))
  (list (quotient n (quotient (time-difference (time) time) 60.0))
        "cycles per second"))

(defun test-inc-1 (integer)
  "When the program test-inc is compiled on a unix system with
cc test-inc.c -lshare
and then executed, you can call (test-inc n) and it will
write the integer n into the shared-array area, signal the
unix process to do the computation, wait for the computation
to complete, then return the result, which is N+1 in this case."
  (si:share-mem-write 0 integer)
  (send *p* ':tyo #/?)
  (send *p* ':tyi)
  (si:share-mem-read 0))


(enter-c-program "INCT" '

|//*  program test-inc *//

#include <stdio.h>
#include <share.h>

main()
 {int f,n,*p,val;
  char c[1];
  f = open("//dev//ttyl4",2);
  if (f < 0) {printf("open lost\n"); exit(0);}
  if (share_setup() < 0) {printf("share setup lost\n"); exit(0);}
  p = (int *) sharebase;
  while(1)
  { n = read(f,c,1);
    if (n == 0) {printf("got end of file\n"); exit(1);}
    if (n < 0) {printf("read lost\n");exit(0);}
    if (*c == 'S') {printf("Been told to stop\n");
                    c[0] = 'O';
                    write(f,c,1);
                    exit(1);}
    val = p[0];
    val = SWAB32(val)+1;
    p[0] = SWAB32(val);
    n = write(f,c,1);
    if (n < 0) {printf("write lost\n"); exit(0);}}}
|)


;; these test values are probably wrong (i.e. TOO LOW) for Release II.
;; 2/26/85 13:33:07 -gjc

;; (test-inc-loop)  20 Hz.


;; results:
;; (test-clf 100. *p*)             52.2 Hz. using freq.c
;; (test-clf 10000. *null*)        6.5 KHz.
;; (test-olf 1000. *p*)  303.3 Hz. Using cat /dev/ttyl4 > /dev/null
;; (test-olf 1000. *p*)  220.0 Hz  Using freqc.c
;; (test-olf 10000. *null*)  9.8 Khz.
