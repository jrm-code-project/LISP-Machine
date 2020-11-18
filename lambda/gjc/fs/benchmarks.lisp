;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10 -*-


(DEFUN TIMEIT (NAME F)
  (LET ((TIME (TIME)))
    (PROG1 (FUNCALL F)
           (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))
           (COND ((LISTP NAME)
                  (FORMAT T "~&~A took ~\scientific\seconds, ~\scientific\seconds per loop~%"
                          (CAR NAME)
                          TIME
                          (QUOTIENT TIME (CADR NAME))))
                 ('ELSE
                  (FORMAT T "~&~A took ~\scientific\seconds.~%"
                          NAME TIME))))))


(DEFUN OPEN-READ-TEST (FILENAME)
  (LET ((STREAM)
        (L (LIST "READ THE FILE" NIL)))
    (UNWIND-PROTECT
        (PROGN (TIMEIT "OPEN FOR READ" #'(LAMBDA ()
                                           (SETQ STREAM (OPEN FILENAME))))
               (TIMEIT L
                       #'(LAMBDA ()
                           (do ((buf)
                                (from-stream stream)
                                (offset)
                                (limit)
                                (N 0))
                               (())
                             (multiple-value (buf offset limit)
                               (send from-stream :read-input-buffer))
                             (cond ((null buf)
                                    (return (setf (cadr l) n))))
                             (incf n (- limit offset))
                             (send from-stream :advance-input-buffer))))
               (TIMEIT "CLOSE THE FILE"
                       #'(LAMBDA ()
                           (CLOSE STREAM)
                           (SETQ STREAM NIL))))
      (AND STREAM (CLOSE STREAM)))))


(DEFUN OPEN-WRITE-TEST (FILENAME)
  (LET ((STREAM)
        (ST (MAKE-STRING 1000)))
    (UNWIND-PROTECT
        (PROGN (TIMEIT "OPEN FOR WRITE" #'(LAMBDA ()
                                            (SETQ STREAM (OPEN FILENAME :DIRECTION :OUTPUT))))
               (TIMEIT '("WRITE 10^6 BYTES" 1000000)
                       #'(LAMBDA ()
                           (DOTIMES (J 1000)
                             (SEND STREAM :STRING-OUT ST))))
               (TIMEIT "CLOSE FILE AFTER WRITE"
                       #'(LAMBDA ()
                           (CLOSE STREAM)
                           (SETQ STREAM NIL))))
      (AND STREAM (CLOSE STREAM)))))




(EVAL-WHEN (EVAL COMPILE LOAD)
  (IF (= 102 (SI:GET-SYSTEM-VERSION))
      (SSTATUS FEATURE LMI-RELEASE-2)))

#+LMI-RELEASE-2
FORMAT:
(PROGN 'COMPILE

(DEFFORMAT SCIENTIFIC (:ONE-ARG) (X IGNORE)
  (MULTIPLE-VALUE-BIND (NUMBER POWER NAME)
      (SCIENTIFIC-NUMBER X)
    (COND ((NOT NAME)
           (FORMAT T "~$*10^~D " NUMBER POWER))
          ('ELSE
           (FORMAT T "~$ ~A" NUMBER NAME)))))

(DEFVAR *SCIENTIFIC-NUMBER-POWER-TABLE*
        '((0 "")
          (-3 "milli")
          (-6 "micro")
          (-9 "nano")
          (-12 "fempto")
          (-15 "atto")
          (3 "kilo")
          (6 "mega")
          (9 "giga")
          (12 "tera")))

(DEFUN SCIENTIFIC-NUMBER (X)
  "return a number 1<ABS(N)<1000, and power of ten, and a power name such as `mega'"
  (DECLARE (VALUES NUMBER POWER NAME))
  (COND ((ZEROP X)
         (VALUES 0 0 ""))
        ('ELSE
         (LET ((POW (TIMES 3 (FLOOR (LOG (ABS X) 10) 3))))
           (VALUES (TIMES X (EXPT 10.0 (- POW)))
                   POW
                   (CADR (ASSQ POW *SCIENTIFIC-NUMBER-POWER-TABLE*)))))))

)

(DEFUN OPEN-AND-ACCEPT (CONTACT)
  (LET ((S (CHAOS:OPEN-STREAM NIL CONTACT)))
    (CHAOS:ACCEPT (SEND S :CONNECTION))
    S))

(DEFUN READ-STREAM-TEST (STREAM &OPTIONAL (BUFFERS 10))
  (LET ((L (LIST "READ BYTES FROM THE STREAM" NIL)))
    (TIMEIT L
            #'(LAMBDA ()
                (DO ((BUF)
                     (FROM-STREAM STREAM)
                     (OFFSET)
                     (LIMIT)
                     (N 0)
                     (NB BUFFERS)
                     (J 0 (1+ J)))
                    ((= NB J)
                     (SETF (CADR L) N))
                  (MULTIPLE-VALUE (BUF OFFSET LIMIT)
                    (SEND FROM-STREAM :READ-INPUT-BUFFER))
                  (COND ((NULL BUF)
                         (RETURN (SETF (CADR L) N))))
                  (INCF N (- LIMIT OFFSET))
                  (SEND FROM-STREAM :ADVANCE-INPUT-BUFFER))))))

;; closed loop response test.
;;

(DEFUN ONE-CHAR-ECHO-SERVER (STREAM &OPTIONAL CONTACT)
  CONTACT
  (DO ((C))
      ((NOT (SETQ C (SEND STREAM :TYI))))
    (SEND STREAM :TYO C)
    (SEND STREAM :FORCE-OUTPUT)))

(CHAOS:ADD-VANILLA-CHAOS-SERVER "IECHO" 'ONE-CHAR-ECHO-SERVER)

(TCP-APPLICATION:DEFINE-TCP-SERVER IECHO "ECHO CHARACTERS WITH FORCE-OUTPUT"
  :TOPLEVEL-FUNCTION 'ONE-CHAR-ECHO-SERVER
  :LISTEN-PORT 103.)

(DEFUN CLOSED-LOOP-ECHO-TEST (HOST &OPTIONAL &KEY (LOOPS 100) (PROTOCOL :CHAOS) &AUX STREAM)
  (UNWIND-PROTECT
      (PROGN (TIMEIT (FORMAT NIL "~A STREAM OPEN" PROTOCOL)
                     #'(LAMBDA ()
                         (SETQ STREAM (ECASE PROTOCOL
                                        (:CHAOS
                                         (CHAOS:OPEN-STREAM HOST "IECHO"))
                                        (:TCP
                                         (OPEN (FORMAT NIL "TCP-HOST:~A#103" HOST)))))))
             (TIMEIT (LIST (FORMAT NIL "ECHO ~D TIMES" LOOPS) LOOPS)
                     #'(LAMBDA ()
                         (DO ((J 0 (1+ J))
                              (N LOOPS)
                              (S STREAM)
                              (C))
                             ((= J N))
                           (SETQ C (LOGAND #o377 J))
                           (SEND S :TYO C)
                           (SEND S :FORCE-OUTPUT)
                           (OR (EQ C (SEND S :TYI))
                               (PRINT "ECHO OF WRONG CHARACTER")))))
             (TIMEIT "CLOSE OF STREAM"
                     #'(LAMBDA ()
                         (CLOSE STREAM)
                         (SETQ STREAM NIL))))
    (AND STREAM (CLOSE STREAM))))


(DEFUN RECURSIVE-LISTF (TOP)
  (LABELS ((RECURSE (PATH)
                    (PRINT PATH)
                    (DO ((L (FS:DIRECTORY-LIST PATH) (CDR L))
                         (DIRS))
                        ((NULL L)
                         (MAPC #'RECURSE DIRS))
                      (COND ((NULL (CAAR L)))
                            ((GET (CAR L) :DIRECTORY)
                             (PUSH (SEND (SEND (CAAR L) :PATHNAME-AS-DIRECTORY) :NEW-PATHNAME
                                         :NAME :WILD :TYPE :WILD :VERSION :WILD)
                                   DIRS))))))
    (TIMEIT "RECURSIVE-LISTF" #'(LAMBDA ()
                                  (RECURSE (SEND (FS:PARSE-PATHNAME TOP)
                                                 :NEW-PATHNAME
                                                 :NAME :WILD :TYPE :WILD :VERSION :WILD))))))
