;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-

;;; Regular side of a simple bi-directional stream called COLD-BI-STREAM.
;;; Plus a server process to support MINI-FASLOAD etc.

(DEFFLAVOR COLD-COMMAND-STREAM
         (SECTION (TAKEN NIL))
         (BUFFERED-INPUT-CHARACTER-STREAM)
  (:INITABLE-INSTANCE-VARIABLES SECTION))


(DEFFLAVOR COLD-DATA-STREAM-MIXIN
         (SECTION)
         ()
  (:INITABLE-INSTANCE-VARIABLES SECTION))

(DEFFLAVOR COLD-CHARACTER-DATA-STREAM
         (SECTION SIZE)
         (COLD-DATA-STREAM-MIXIN BUFFERED-OUTPUT-CHARACTER-STREAM)
  (:INITABLE-INSTANCE-VARIABLES SECTION SIZE))

(DEFFLAVOR COLD-BINARY-DATA-STREAM
         (SECTION SIZE)
         (COLD-DATA-STREAM-MIXIN BUFFERED-OUTPUT-STREAM)
  (:INITABLE-INSTANCE-VARIABLES SECTION SIZE))

(DEFMETHOD (COLD-COMMAND-STREAM :NEXT-INPUT-BUFFER) (&OPTIONAL NO-HANG-P)
  (WHEN TAKEN
    (SETF (AREF *GLOBAL-SHARED-MEMORY-16* SECTION) 0)
    (SETQ TAKEN NIL))
  (BLOCK NO-HANG
    (COND ((= 1 (AREF *GLOBAL-SHARED-MEMORY-16* SECTION)))
          (NO-HANG-P
           (RETURN-FROM NO-HANG NIL))
          ('ELSE
           (PROCESS-WAIT "shared command input"
                         #'(lambda (a b)
                             (= 1 (aref a b)))
                         *global-shared-memory-16*
                         section)))
    (LET ((OFFSET (AREF *GLOBAL-SHARED-MEMORY-16* (+ SECTION 2)))
          (LIMIT (AREF *GLOBAL-SHARED-MEMORY-16* (+ SECTION 3))))
      (COND ((= OFFSET LIMIT)
             (SETF (AREF *GLOBAL-SHARED-MEMORY-16* SECTION) 0)
             ())
            ('ELSE
             (SETQ TAKEN T)
             (VALUES *GLOBAL-SHARED-MEMORY-8*
                     (+ (* SECTION 2) 8 OFFSET)
                     (+ (* SECTION 2) 8 LIMIT)))))))

(DEFMETHOD (COLD-COMMAND-STREAM :DISCARD-INPUT-BUFFER) (ARRAY)
  ARRAY
  (SETF (AREF *GLOBAL-SHARED-MEMORY-16* SECTION) 0)
  (SETQ TAKEN NIL))


(DEFMETHOD (COLD-CHARACTER-DATA-STREAM :NEW-OUTPUT-BUFFER) ()
  (PROCESS-WAIT "shared output" #'(lambda (a b) (= 1 (aref a b)))
                *global-shared-memory-16* section)
  (values *global-shared-memory-8*
          (+ (* section 2) 8)
          (+ (* section 2) 8 size)))

(DEFMETHOD (COLD-CHARACTER-DATA-STREAM :SEND-OUTPUT-BUFFER) (array end)
  array
  (LET ((LIMIT (- END (+ (* SECTION 2) 8))))
    (COND ((ZEROP LIMIT))
          ('ELSE
           (SETF (AREF *GLOBAL-SHARED-MEMORY-16* (+ SECTION 1)) 0)
           (setf (aref *global-shared-memory-16* (+ section 2)) 0)
           (setf (aref *global-shared-memory-16* (+ section 3)) LIMIT)
           (setf (aref *global-shared-memory-16* section) 0)))))



(DEFMETHOD (COLD-BINARY-DATA-STREAM :NEW-OUTPUT-BUFFER) ()
  (PROCESS-WAIT "shared output" #'(lambda (a b) (= 1 (aref a b)))
                *global-shared-memory-16* section)
  (values *global-shared-memory-16*
          (+ section 4)
          (+ SECTION 4 SIZE)))


(DEFMETHOD (COLD-BINARY-DATA-STREAM :SEND-OUTPUT-BUFFER) (array end)
  array
  (LET ((LIMIT (- END (+ SECTION 4))))
    (COND ((ZEROP LIMIT))
          ('ELSE
           (SETF (AREF *GLOBAL-SHARED-MEMORY-16* (+ SECTION 1)) 1)
           (setf (aref *global-shared-memory-16* (+ section 2)) 0)
           (setf (aref *global-shared-memory-16* (+ section 3)) LIMIT)
           (setf (aref *global-shared-memory-16* section) 0)))))

(DEFMETHOD (COLD-DATA-STREAM-MIXIN :DISCARD-OUTPUT-BUFFER ) (array)
  array
  nil)

(DEFMETHOD (COLD-DATA-STREAM-MIXIN :EOF) ()
  (PROCESS-WAIT "shared output" #'(lambda (a b) (= 1 (aref a b)))
                *global-shared-memory-16* section)
  (setf (aref *global-shared-memory-16* (+ section 2)) 0)
  (setf (aref *global-shared-memory-16* (+ section 3)) 0)
  (setf (aref *global-shared-memory-16* section) 0))

(DEFVAR *COMMAND-STREAM* NIL)
(DEFVAR *CHARACTER-DATA* NIL)
(DEFVAR *BINARY-DATA* NIL)

(DEFUN SETUP-COLD-SERVER-STREAMS ()
  (DOTIMES (J (+ *COLD-BI-STREAM-OUTPUT-SECTION* 4))
    (SETF (AREF *GLOBAL-SHARED-MEMORY-16* J) 0))
  (SETF (AREF *GLOBAL-SHARED-MEMORY-16* *COLD-BI-STREAM-INPUT-SECTION*) 1)
  (SETQ *COMMAND-STREAM* (MAKE-INSTANCE 'COLD-COMMAND-STREAM
                                        :SECTION *COLD-BI-STREAM-OUTPUT-SECTION*))
  (SETQ *CHARACTER-DATA* (MAKE-INSTANCE 'COLD-CHARACTER-DATA-STREAM
                                        :SECTION *COLD-BI-STREAM-INPUT-SECTION*
                                        :SIZE
                                        (- (* 2 (- *COLD-BI-STREAM-OUTPUT-SECTION*
                                                   *COLD-BI-STREAM-INPUT-SECTION*))
                                           8)))
  (SETQ *BINARY-DATA* (MAKE-INSTANCE 'COLD-BINARY-DATA-STREAM
                                     :SECTION *COLD-BI-STREAM-INPUT-SECTION*
                                     :SIZE (- *COLD-BI-STREAM-OUTPUT-SECTION*
                                              *COLD-BI-STREAM-INPUT-SECTION*
                                              4)))
  NIL)


(DEFVAR MINI-OPEN-SELECTIVE-P NIL)

(DEFUN MINI-SERVER-PROCESS (&OPTIONAL SELECTIVE)
  (IF SELECTIVE
      (SETQ MINI-OPEN-SELECTIVE-P 'MINI-OPEN-SELECTIVE-QUERY))
  (SETUP-COLD-SERVER-STREAMS)
  (DO-FOREVER
    (LET ((COMMAND (READ *COMMAND-STREAM*)))
      (FUNCALL (GET (CAR COMMAND) 'MINI-SERVER-COMMAND)
               COMMAND))))


(DEFUN MINI-PARSE-PATHNAME-STRING (X)
  (COND ((STRING-EQUAL "SYS:" X :end2 4)
         (FS:PARSE-PATHNAME X))
        ('ELSE
         (FS:PARSE-PATHNAME (STRING-APPEND (SEND (SEND (FS:GET-PATHNAME-HOST "SYS")
                                                       :HOST)
                                                 :NAME)
                                           ":"
                                           X)))))


(DEFUN MINI-OPEN-SELECTIVE-QUERY (FILENAME)
  (LET ((ANS (FQUERY '(:CHOICES (((T "Yes.") #/y #/t #\SP #\HAND-UP)
                                 ((() "No.") #/n #\RUBOUT #\HAND-DOWN)
                                 ((:PROCEED "Proceed.") #/p #\RESUME)))
                     "load ~S?" FILENAME)))
    (COND ((EQ ANS :PROCEED)
           (SETQ MINI-OPEN-SELECTIVE-P NIL)
           T)
          (ANS))))


(DEFUN (:OPEN MINI-SERVER-COMMAND) (COMMAND)
  (FORMAT T "~&COMMAND=> ~S" COMMAND)
  (LET ((FILENAME (MINI-PARSE-PATHNAME-STRING (CADR COMMAND)))
        (BINARYP (CADDR COMMAND))
        (BYTES 0)
        (TIME NIL))
    (WITH-OPEN-FILE (STREAM (COND ((NOT MINI-OPEN-SELECTIVE-P)
                                   FILENAME)
                                  ((FUNCALL MINI-OPEN-SELECTIVE-P FILENAME)
                                   FILENAME)
                                  (BINARYP
                                   "SYS:COLD;NULL-FILE QFASL >")
                                  ('ELSE
                                   "SYS:COLD;NULL-FILE LISP >"))
                            :CHARACTERS (NOT BINARYP)
                            ;;;@@@This might lose with 8-bit qfasl's
                            :BYTE-SIZE (IF BINARYP 16 8))
      (LET ((I (SEND STREAM :INFO)))
        (SETQ I (CONS (SEND (CAR I) :STRING-FOR-PRINTING)
                      ;; the function FS:PARSE-DIRECTORY-DATE-PROPERTY
                      ;; will get called on this string eventually.
                      ;;(TIME:PRINT-UNIVERSAL-TIME (CDR I) NIL NIL :MM//DD//YY)
                      ;; we can just pass a decimal string.
                      (format nil "~D" (cdr i))
                      ))
        (FORMAT T " Transmitting ~S" I)
        (let ((*readtable* (find-readtable-named "ZL")))
          (PRIN1 I *CHARACTER-DATA*)))
      (SEND *CHARACTER-DATA* :FORCE-OUTPUT)
      (LET ((O (IF BINARYP *BINARY-DATA* *CHARACTER-DATA*)))
        (SETQ TIME (TIME))
        (do ((buf) (offset) (limit)
             (FROM-STREAM STREAM)(TO-STREAM O))
            (())
          (multiple-value (buf offset limit)
            (send from-stream :read-input-buffer))
          (cond ((null buf) (return nil)))
          (send to-stream :string-out buf offset limit)
          (INCF BYTES (- LIMIT OFFSET))
          (PRINC ".")
          (send from-stream :advance-input-buffer))
        (SEND O :FORCE-OUTPUT)
        (SEND O :EOF)
        (SETQ TIME (QUOTIENT (TIME-DIFFERENCE (TIME) TIME) 60.0))))
    (FORMAT T " complete ~\scientific\bytes per second.~%"
            (QUOTIENT (* (IF BINARYP 2 1) BYTES) TIME))))



(DEFFLAVOR cold-debug-frame
         ()
         (TV:BORDERED-CONSTRAINT-FRAME)
  (:DEFAULT-INIT-PLIST
    :PANES '((L2 TV:LISP-LISTENER
                 :BLINKER-P  T
                 :DEEXPOSED-TYPEIN-ACTION :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION :PERMIT
                 :MORE-P NIL
                 :LABEL "Mini Server Window"
                 :SAVE-BITS T
                 :PROCESS (MINI-SERVER-WINDOW
                            :REGULAR-PDL-SIZE #o40000
                            :SPECIAL-PDL-SIZE #o4000))
             (L1 LAM:SENSITIVE-LISP-LISTENER
                 :BLINKER-P T
                 :DEEXPOSED-TYPEIN-ACTION :NORMAL
                 :DEEXPOSED-TYPEOUT-ACTION :PERMIT
                 :MORE-P NIL
                 :LABEL "Lam Debugger Window"
                 :SAVE-BITS T
                 :PROCESS (LAM-DEBUG-WINDOW
                            :REGULAR-PDL-SIZE #o40000
                            :SPECIAL-PDL-SIZE #o4000)))
    :CONSTRAINTS '((LFRAME (WHOLE)
                           ((WHOLE :HORIZONTAL
                                   (:EVEN)
                                   (L1 L2)
                                   ((L1 :EVEN) (L2 :EVEN))))))
    :BORDERS 3))

(DEFMETHOD (COLD-DEBUG-FRAME :AFTER :INIT) (&REST IGNORE)
  (FUNCALL-SELF :SET-SELECTION-SUBSTITUTE (FUNCALL-SELF :GET-PANE 'L1)))


(TV:ADD-SYSTEM-KEY #\7 'COLD-DEBUG-FRAME "FOR DOING COLD LOADS")


(DEFUN MINI-SERVER-WINDOW (X)
  (LET ((*PACKAGE* (FIND-PACKAGE "SI"))
        (*READTABLE* (FIND-READTABLE-NAMED "CL")))
    (LISP-TOP-LEVEL1 X)))


(DEFUN LAM-DEBUG-WINDOW (X)
  (LET ((*PACKAGE* (FIND-PACKAGE "LAM"))
        (*READTABLE* (FIND-READTABLE-NAMED "CL"))
        (BASE 8.)
        (IBASE 8.))
    (LISP-TOP-LEVEL1 X)))


(COMPILE-FLAVOR-METHODS COLD-COMMAND-STREAM
                        COLD-CHARACTER-DATA-STREAM
                        COLD-BINARY-DATA-STREAM
                        COLD-DEBUG-FRAME)
