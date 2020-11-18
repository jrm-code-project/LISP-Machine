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


Here is something that is probably good enough for allowing some
personal privacy on a public shared filesystem. Reducing the chance
of accidental disclosure of personal mail etc. Be sure to turn off
your TELNET and EVAL servers if you want to protect your buffers
and passwords etc while in your running virtual memory environment.
9-Mar-86 17:06:39 -George Carrette

Example usage:

 (add-crypt-host "LAMBDA-A")

From then on (open "crypt-lambda-a:foo;bar.lisp") will return a crypt
input stream, and (open "crypt-lambda-a:foo;bar.lisp" :direction :output) will return
a crypt output stream. There is sufficient support here to allow one to read
and write files from the editor as well.

The default settings of *KEY-MAKER* and *ENCRYPTOR* give a simple
algorithm that is easy to break when used with a short key.

One might want to rework this to have different keys and encyptors for
different hosts. Encryption of filenames may also be amusing, but
potentially troublesome in practice since the target names must still
be syntactically legal on the system.

||#

;;; section I, the Streams.

(DEFVAR *KEY-MAKER* 'XOR-KEY-MAKER)

(DEFVAR *ENCRYPTOR* 'XOR-CRYPT-STRING)

(DEFFLAVOR CRYPT-MIXIN
         (KEY HANDLE BYTE-SIZE)
         ()
  :initable-instance-variables)

(DEFMETHOD (CRYPT-MIXIN :AFTER :INIT) (&REST IGNORED)
  (or (variable-boundp key)
      (setq key (FUNCALL *KEY-MAKER* (GET-KEYSTRING-LIST))))
  nil)

(DEFMETHOD (CRYPT-MIXIN :ENCRYPT-BUFFER) (ARRAY END)
  (SETQ KEY (FUNCALL *ENCRYPTOR* ARRAY END KEY))
  NIL)

(DEFMETHOD (CRYPT-MIXIN :ALLOCATE-BUFFER) ()
  (ECASE BYTE-SIZE
    (8
     (ALLOCATE-RESOURCE 'FS:SIMPLE-STRING-BUFFER 1024))
    (16
     (ALLOCATE-RESOURCE 'FS:SIMPLE-ART-16B-BUFFER 512))))

(DEFMETHOD (CRYPT-MIXIN :DEALLOCATE-BUFFER) (BUFFER)
  (ECASE BYTE-SIZE
    (8
     (DEALLOCATE-RESOURCE 'FS:SIMPLE-STRING-BUFFER BUFFER))
    (16
     (DEALLOCATE-RESOURCE 'FS:SIMPLE-ART-16B-BUFFER BUFFER))))

(DEFMETHOD (CRYPT-MIXIN :INFO) ()
  (IF (VARIABLE-BOUNDP HANDLE)
      (FUNCALL HANDLE :INFO)))

(DEFMETHOD (CRYPT-MIXIN :PATHNAME) ()
  (IF (VARIABLE-BOUNDP HANDLE)
      (FUNCALL HANDLE :PATHNAME)))

(DEFMETHOD (CRYPT-MIXIN :TRUENAME) ()
  (IF (VARIABLE-BOUNDP HANDLE)
      (FUNCALL HANDLE :TRUENAME)))

(DEFMETHOD (CRYPT-MIXIN :GENERIC-PATHNAME) ()
  (IF (VARIABLE-BOUNDP HANDLE)
      (FUNCALL HANDLE :GENERIC-PATHNAME)))

(defflavor crypt-input-stream-mixin
         (input)
         (CRYPT-MIXIN)
  :initable-instance-variables)

(defflavor crypt-character-input-stream
         ()
         (crypt-input-stream-mixin si:buffered-input-character-stream))

(defflavor crypt-binary-input-stream
         ()
         (crypt-input-stream-mixin si:buffered-input-stream))

(defmethod (crypt-input-stream-mixin :close) (&rest args)
  (and input (lexpr-send input :close args)))

(defmethod (crypt-input-stream-mixin :next-input-buffer) (&optional no-hang-p)
  no-hang-p
  (and input
       (let ((buffer)
             (normalp nil))
         (unwind-protect
             (let ((amount (send input :string-in nil
                                 (setq buffer (SEND SELF :ALLOCATE-BUFFER)))))
               (if (zerop amount)
                   (close (prog1 input (setq input nil))))
               (SEND SELF :ENCRYPT-BUFFER BUFFER AMOUNT)
               (setq normalp t)
               (values buffer 0 amount))
           (and (not normalp) buffer
                (SEND SELF :DEALLOCATE-BUFFER BUFFER))))))

(defmethod (crypt-input-stream-mixin :discard-input-buffer) (array)
  (SEND SELF :DEALLOCATE-BUFFER ARRAY))

(defflavor crypt-output-stream-mixin
         (output)
         (CRYPT-MIXIN)
  :initable-instance-variables)

(defflavor crypt-character-output-stream
         ()
         (crypt-output-stream-mixin si:buffered-output-character-stream))

(defflavor crypt-binary-output-stream
         ()
         (crypt-output-stream-mixin si:buffered-output-stream))


(defmethod (crypt-output-stream-mixin :new-output-buffer) ()
  (LET ((BUFFER (SEND SELF :ALLOCATE-BUFFER)))
    (VALUES BUFFER 0 (LENGTH BUFFER))))

(defmethod (crypt-output-stream-mixin :send-output-buffer) (array end)
  (SEND SELF :ENCRYPT-BUFFER ARRAY END)
  (send output :string-out array 0 end)
  (SEND SELF :DEALLOCATE-BUFFER ARRAY))

(defmethod (crypt-output-stream-mixin :discard-output-buffer) (array)
  (SEND SELF :DEALLOCATE-BUFFER ARRAY))

(defmethod (crypt-output-stream-mixin :close) (&rest ignored)
  (and output (close output)))

(compile-flavor-methods
  crypt-character-input-stream
  crypt-binary-input-stream
  crypt-character-output-stream
  crypt-binary-output-stream)

;;; Section II, a utility function using only the streams.

(defun crypt-file (input-filename output-filename &optional (on-input t))
  (with-open-file (from-stream input-filename)
    (with-open-file (to-stream output-filename :direction :output)
      (with-open-stream (crypt (if on-input
                                   (make-instance 'crypt-character-input-stream
                                                  :input from-stream
                                                  :BYTE-SIZE 8)
                                 (make-instance 'crypt-character-output-stream
                                                :output to-stream
                                                :BYTE-SIZE 8)))
        (if on-input
            (stream-copy-until-eof crypt to-stream)
          (stream-copy-until-eof from-stream crypt))))))


;;; Section III, the default key maker and encryptor.

(DEFUN XOR-KEY-MAKER (KEYS)
  (let ((e))
    (dolist (k keys)
      (dotimes (j (length k))
        (DO ((C (AREF K J) (MOD (1+ C) 256))
             (N 0 (1+ N)))
            ((OR (NOT (MEM #'= C E)) (= N 256))
             (PUSH (CHAR-CODE C) E)))))
    (RPLACD (LAST E) E)))

(DEFUN XOR-CRYPT-STRING (A N L)
  (DO ((J 0 (1+ J)))
      ((= J N)
       L)
    (SETF (AREF A J) (LOGXOR (AREF A J) (POP L)))))


(defvar *static-keystring-list* T
  "Set this to your keystring list if you dont want to be prompted, or to T
if you want to be prompted only once")

(add-initialization "flush xor-crypt keys"
                    '(setq *static-keystring-list* nil)
                    '(:logout))


(defun get-keystring-list ()
  (cond ((eq *static-keystring-list* t)
         (setq *static-keystring-list*
               (let ((*static-keystring-list* nil))
                 (get-keystring-list))))
        (*static-keystring-list*)
        ('else
         (labels ((prompt-and-readline
                    (message)
                    (terpri)
                    (princ message)
                    (do ((a (make-string 80 :fill-pointer 0))
                         (c (send standard-input :tyi) (send standard-input :tyi)))
                        ((= c #\return) a)
                      (vector-push-extend c a)))
                  (prompt-and-readline-secretly
                    (message)
                    (do ((linin (prompt-and-readline message)
                                (prompt-and-readline message)))
                        ((equal linin
                                (prompt-and-readline '"Once more to verify: "))
                         linin)
                      (princ "They didn't match.  Try again."))))
           (do ((keys (ncons (prompt-and-readline-secretly '"First key = "))
                      (cons (prompt-and-readline-secretly '"Next key = ") keys)))
               ((zerop (string-length (car keys)))
                (setq keys (cdr keys))
                (cond ((zerop (length keys))
                       (format t "You have to supply at least ONE key!")
                       (get-keystring-list))
                      (t (format t "~% ~D keystring~p entered."
                                 (length keys) (length keys))
                         (nreverse keys)))))))))


;; Section IV, host and pathname interface.

(DEFFLAVOR CRYPT-PROBE-STREAM
         (SUBSTREAM
          HOST)
         ()
    :initable-instance-variables)

(DEFMETHOD (CRYPT-PROBE-STREAM :INFO) ()
  (LET ((I (SEND SUBSTREAM :INFO)))
    (AND I
         (CONS (SEND (CAR I) :NEW-PATHNAME :HOST HOST) (CDR I)))))


(DEFMETHOD (CRYPT-PROBE-STREAM :CREATION-DATE) ()
  (SEND SUBSTREAM :CREATION-DATE))

(DEFMETHOD (CRYPT-PROBE-STREAM :CLOSE) (&REST IGNORED)
  ())



(DEFFLAVOR CRYPT-HOST
        (NAME
         PHYSICAL-HOST)
        (FS:PATHNAME-HOST-MIXIN SI:BASIC-HOST)
  (:SETTABLE-INSTANCE-VARIABLES))

(DEFMETHOD (CRYPT-HOST :PATHNAME-FLAVOR) () 'CRYPT-PATHNAME)

(DEFMETHOD (CRYPT-HOST :SYSTEM-TYPE) ()
  :CRYPT)

(DEFMETHOD (CRYPT-HOST :NETWORK-TYPE) ()
  (SEND PHYSICAL-HOST :NETWORK-TYPE))

(DEFMETHOD (CRYPT-HOST :NETWORK-TYPEP) (TYPE)
  (SEND PHYSICAL-HOST :NETWORK-TYPEP TYPE))

(DEFMETHOD (CRYPT-HOST :ENABLE-CAPABILITIES) (&REST L)
  (LEXPR-SEND PHYSICAL-HOST :ENABLE-CAPABILITIES L))

(DEFMETHOD (CRYPT-HOST :DISABLE-CAPABILITIES) (&REST L)
  (LEXPR-SEND PHYSICAL-HOST :DISABLE-CAPABILITIES L))

(DEFFLAVOR CRYPT-PATHNAME
         ()
         (FS:PATHNAME))

(DEFMETHOD (CRYPT-PATHNAME :INTERNAL-TRANSLATED-PATHNAME) ()
  (MAKE-PATHNAME :HOST (SEND (SEND SELF :HOST) :PHYSICAL-HOST)
                 :DEVICE (SEND SELF :DEVICE)
                 :DIRECTORY (SEND SELF :DIRECTORY)
                 :NAME (SEND SELF :NAME)
                 :TYPE (SEND SELF :TYPE)
                 :VERSION (SEND SELF :VERSION)))


(DEFMETHOD (CRYPT-PATHNAME :PASS-ON) (MESSAGE &OPTIONAL L)
  (LEXPR-SEND (SEND SELF :INTERNAL-TRANSLATED-PATHNAME) MESSAGE L))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-PRINTING) ()
  (LET ((S (SEND (SEND SELF :INTERNAL-TRANSLATED-PATHNAME)
                 :STRING-FOR-PRINTING)))
    (STRING-APPEND (SEND (SEND SELF :HOST) :NAME)
                   (SUBSTRING S (STRING-SEARCH ":" S)))))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-DIRECTORY) ()
  (SEND SELF :PASS-ON :STRING-FOR-DIRECTORY))

(DEFMETHOD (CRYPT-PATHNAME :PARSE-NAMESTRING) (&REST L)
  (SEND SELF :PASS-ON :PARSE-NAMESTRING L))

(DEFMETHOD (CRYPT-PATHNAME :QUOTE-CHARACTER) ()
  (SEND SELF :PASS-ON :QUOTE-CHARACTER))

(DEFMETHOD (CRYPT-PATHNAME :CHARACTER-NEEDS-QUOTING-P) (&REST L)
  (SEND SELF :PASS-ON :CHARACTER-NEEDS-QUOTING-P L))

(DEFMETHOD (CRYPT-PATHNAME :PARSE-DIRECTORY-SPEC) (&REST L)
  (SEND SELF :PASS-ON :PARSE-DIRECTORY-SPEC L))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-HOST) ()
  (SEND SELF :PASS-ON :STRING-FOR-HOST))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-WHOLINE) ()
  (LET ((S (SEND (SEND SELF :INTERNAL-TRANSLATED-PATHNAME)
                 :STRING-FOR-WHOLINE)))
    (STRING-APPEND (SEND (SEND SELF :HOST) :NAME)
                   (SUBSTRING S (STRING-SEARCH ":" S)))))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-EDITOR) ()
  (STRING-APPEND (SEND SELF :PASS-ON :STRING-FOR-EDITOR)
                 (SEND (SEND SELF :HOST) :NAME)
                 ":"))

(DEFMETHOD (CRYPT-PATHNAME :CHANGE-PROPERTIES) (&REST L)
  (SEND SELF :PASS-ON :CHANGE-PROPERTIES L))

(DEFMETHOD (CRYPT-PATHNAME :EXPUNGE) (&REST L)
  (SEND SELF :PASS-ON :EXPUNGE L))

(DEFMETHOD (CRYPT-PATHNAME :DELETE) (&REST L)
  (SEND SELF :PASS-ON :DELETE L))

(DEFMETHOD (CRYPT-PATHNAME :UNDELETE) (&REST L)
  (SEND SELF :PASS-ON :UNDELETE L))

(DEFMETHOD (CRYPT-PATHNAME :RENAME) (&REST L)
  (SEND SELF :PASS-ON :RENAME L))

(DEFMETHOD (CRYPT-PATHNAME :UNDELETABLE-P) (&REST L)
  (SEND SELF :PASS-ON :UNDELETABLE-P L))

(DEFMETHOD (CRYPT-PATHNAME :HOMEDIR) (&REST L)
  (SEND (SEND SELF :PASS-ON :HOMEDIR L) :NEW-PATHNAME :HOST (SEND SELF :HOST)))

(DEFMETHOD (CRYPT-PATHNAME :MULTIPLE-FILE-PLISTS) (&REST L)
  (MAPCAR #'(LAMBDA (PLIST)
              (DO ((NEW (CONS (SEND (CAR PLIST) :NEW-PATHNAME :HOST (SEND SELF :HOST)) NIL))
                   (L (CDR PLIST) (CDDR PLIST)))
                  ((NULL L) NEW)
                (COND ((EQ (CAR L) :TRUENAME)
                       (PUTPROP NEW (SEND (CADR L) :NEW-PATHNAME :HOST (SEND SELF :HOST)) :TRUENAME))
                      ('ELSE
                       (PUTPROP NEW (CADR L) (CAR L))))))
          (SEND SELF :PASS-ON :MULTIPLE-FILE-PLISTS L)))

(DEFMETHOD (CRYPT-PATHNAME :PARSE-TRUENAME) (&REST L)
  (SEND (SEND SELF :PASS-ON :PARSE-TRUENAME L)
        :NEW-PATHNAME :HOST (SEND SELF :HOST)))

(DEFMETHOD (CRYPT-PATHNAME :DIRECTORY-LIST) (&REST L)
  (MAPCAR #'(LAMBDA (X)
              (COND ((CAR X)
                     (CONS (SEND (CAR X) :NEW-PATHNAME :HOST (SEND SELF :HOST)) (CDR X)))
                    ('ELSE
                     X)))
          (SEND SELF :PASS-ON :DIRECTORY-LIST L)))


(DEFMETHOD (CRYPT-PATHNAME :DIRECTORY-LIST-STREAM) (&REST L)
  (LET ((STREAM (SEND SELF :PASS-ON :DIRECTORY-LIST-STREAM L))
        (NSELF SELF))
    (IF (ERRORP STREAM)
        STREAM
      #'(LAMBDA (OPERATION &REST ARGS)
          (SI:SELECTQ-WITH-WHICH-OPERATIONS OPERATION
            (:ENTRY
              (LET ((X (SEND STREAM :ENTRY)))
                (COND ((CAR X)
                       (CONS (SEND (CAR X) :NEW-PATHNAME :HOST (SEND NSELF :HOST)) (CDR X)))
                      ('ELSE
                       X))))
            (:CLOSE
              (LEXPR-SEND STREAM :CLOSE ARGS)))))))

(DEFMETHOD (CRYPT-PATHNAME :ALL-DIRECTORIES) ()
  (MAPCAR #'(LAMBDA (X)
              (SEND X :NEW-PATHNAME :HOST (SEND SELF :HOST)))
          (SEND SELF :PASS-ON :ALL-DIRECTORIES)))

(DEFMETHOD (CRYPT-PATHNAME :STRING-FOR-DIRED) ()
  (SEND SELF :PASS-ON :STRING-FOR-DIRED))

(DEFMETHOD (CRYPT-PATHNAME :OPEN) (&REST OPTIONS)
  (CRYPT-OPEN SELF :OPEN OPTIONS))

(DEFMETHOD (CRYPT-PATHNAME :OPEN-CANONICAL-TYPE) (&REST OPTIONS)
  (CRYPT-OPEN SELF :OPEN-CANONICAL-TYPE OPTIONS))

(DEFUN CRYPT-OPEN (P OPERATION OPTIONS)
  (LET (SUBSTREAM NORMALP)
    (UNWIND-PROTECT
        (PROG1 (COND ((ERRORP (SETQ SUBSTREAM (LEXPR-SEND (SEND P :INTERNAL-TRANSLATED-PATHNAME)
                                                          OPERATION OPTIONS)))
                      SUBSTREAM)
                     ('ELSE
                      (ECASE (SEND SUBSTREAM :DIRECTION)
                        (NIL
                         (MAKE-INSTANCE 'CRYPT-PROBE-STREAM :SUBSTREAM SUBSTREAM :HOST (SEND P :HOST)))
                        (:INPUT
                         (MAKE-INSTANCE (IF (SEND SUBSTREAM :CHARACTERS)
                                            'CRYPT-CHARACTER-INPUT-STREAM
                                          'CRYPT-BINARY-INPUT-STREAM)
                                        :INPUT SUBSTREAM
                                        :BYTE-SIZE (IF (SEND SUBSTREAM :CHARACTERS)
                                                       8
                                                     ;; QFILE ACCESS STREAMS DONT ACCEPT ANY
                                                     ;; :BYTE-SIZE KIND OF MESSAGE. SO WE GUESS
                                                     16)
                                        :HANDLE #'(LAMBDA (OPERATION)
                                                    (CRYPT-STREAM-HANDLE OPERATION P SUBSTREAM))))
                        (:OUTPUT
                         (MAKE-INSTANCE (IF (SEND SUBSTREAM :CHARACTERS)
                                            'CRYPT-CHARACTER-OUTPUT-STREAM
                                          'CRYPT-BINARY-OUTPUT-STREAM)
                                        :OUTPUT SUBSTREAM
                                        :BYTE-SIZE (IF (SEND SUBSTREAM :CHARACTERS)
                                                       8
                                                     ;; QFILE ACCESS STREAMS DONT ACCEPT ANY
                                                     ;; :BYTE-SIZE KIND OF MESSAGE. SO WE GUESS
                                                     16)
                                        :HANDLE #'(LAMBDA (OPERATION)
                                                    (CRYPT-STREAM-HANDLE OPERATION P SUBSTREAM)))))))
               (SETQ NORMALP T))
      (AND SUBSTREAM (NOT NORMALP) (CLOSE SUBSTREAM :ABORT T)))))


(DEFUN CRYPT-STREAM-HANDLE (OPERATION PATHNAME STREAM)
  (ECASE OPERATION
    (:INFO
     (LET ((I (SEND STREAM :INFO)))
       (AND I
            (CONS (SEND (CAR I) :NEW-PATHNAME :HOST (SEND PATHNAME :HOST))
                  (CDR I)))))
    (:TRUENAME
     (SEND (SEND STREAM :TRUENAME) :NEW-PATHNAME :HOST (SEND PATHNAME :HOST)))
    (:PATHNAME
     (SEND (SEND STREAM :PATHNAME) :NEW-PATHNAME :HOST (SEND PATHNAME :HOST)))
    (:GENERIC-PATHNAME
     (SEND (SEND STREAM :GENERIC-PATHNAME) :NEW-PATHNAME :HOST (SEND PATHNAME :HOST)))))

(COMPILE-FLAVOR-METHODS CRYPT-HOST CRYPT-PATHNAME CRYPT-PROBE-STREAM)


(DEFUN ADD-CRYPT-HOST (PHYSICAL-HOST &OPTIONAL CRYPT-NAME)
  (LET* ((PH (SI:PARSE-HOST PHYSICAL-HOST))
         (NAME (OR CRYPT-NAME (STRING-APPEND "CRYPT-" (SEND PH :SHORT-NAME)))))
    (LET ((CH (MAKE-INSTANCE 'CRYPT-HOST
                             :PHYSICAL-HOST PH
                             :NAME NAME)))
      (SETQ FS:*PATHNAME-HOST-LIST* (DELQ (CAR (MEM #'(LAMBDA (NAME HOST)
                                                        (AND (TYPEP HOST 'CRYPT-HOST)
                                                             (STRING-EQUAL (SEND HOST :NAME) NAME)))
                                                    NAME
                                                    FS:*PATHNAME-HOST-LIST*))
                                          FS:*PATHNAME-HOST-LIST*))
      (PUSH CH FS:*PATHNAME-HOST-LIST*)
      CH)))
