;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10 -*-


(defvar *obj-node-table* nil)
(DEFVAR *OBJ-INDEX* 0)

(defresource obj-table ()
  :constructor (make-hash-table)
  :deinitializer (send object :clear-hash))

;; The *OBJ-NODE-TABLE* will have key entered for each node in an object.
;; Value:
;;   T ... object seen once
;;  -N ... object seen more than once, index in table assigned as N.
;;         must be transmited.
;;   N ... available as index N.

(defvar *only-share-symbols* nil)

(defun save-object (obj filename &KEY (circularp T) (VERBOSE T) (symbolp t))
  (let ((*obj-node-table* nil)
        (*OBJ-INDEX* 0)
        (*only-share-symbols* (not circularp)))
    (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :OUTPUT :CHARACTERS NIL :BYTE-SIZE 8)
      (IF VERBOSE
          (FORMAT T "~&Saving object to ~S~%" filename))
      (cond ((or circularp symbolp)
             (using-resource (x obj-table)
               (setq *obj-node-table* x)
               (walk-object obj)
               (if verbose
                   (format t "~&~D shared subexpression~p.~%" *obj-index* *obj-index*))
               (save-object-1 stream)
               (transmit-object obj stream)))
            ('else
             (save-object-1 stream)
             (transmit-object obj stream))))))

(defvar *save-object-file-prefix* "SAVED LISP OBJECT ")

(defun save-object-1 (stream)
  (princ *save-object-file-prefix* stream)
  (transmit-object *obj-index* stream))

(defvar *obj-array* nil)

(defresource obj-array (size)
  :constructor (let ((a (make-array (* 100 (ceiling size 100))
                                    :fill-pointer 0 :adjustable t)))
                 (setf (fill-pointer a) size)
                 a)
  :MATCHER (NOT (< (ARRAY-LENGTH OBJECT) SIZE))
  :INITIALIZER (SETF (FILL-POINTER OBJECT) SIZE)
  :deinitializer (progn (fill object nil)
                        (setf (fill-pointer object) 0)))

(defun restore-object (filename &key (verbose t))
  (with-open-file (stream filename :characters nil :byte-size 8)
    (if verbose (format t "~&Loading object from ~S~%" (send stream :truename)))
    (let ((max-index (restore-object-1 stream)))
      (if verbose (format t "~&~D shared subexpression~p.~%" max-index max-index))
      (using-resource (*obj-array* obj-array (1+ MAX-INDEX))
        (receive-object stream)))))

(defun restore-object-1 (stream)
  (do ((j 0 (1+ j))
       (n (length *save-object-file-prefix*)))
      ((= j n)
       (receive-object stream))
    (or (= (send stream :tyi) (aref *save-object-file-prefix* j))
        (ferror nil "stream doesnt begin with ~S" *save-object-file-prefix*))))

(defun walk-object (OBJ &AUX TEMP)
  (IF (SETQ TEMP (GETHASH OBJ *OBJ-NODE-TABLE*))
      (RETURN-FROM walk-object
        (IF (EQ TEMP T)
            (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) (- (INCF *OBJ-INDEX*))))))
  (TYPECASE OBJ
    (INTEGER NIL)
    (CHARACTER NIL)
    (SYMBOL
     (WHEN (NOT (MEMQ OBJ '(T NIL)))
       (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T)
       (WALK-OBJECT (PACKAGE-NAME (SYMBOL-PACKAGE OBJ)))))
    (STRING
     (or *only-share-symbols* (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T)))
    (ARRAY
     (COND ((not (= (array-rank obj) 1))
            (WALK-OBJECT-UNHANDLED OBJ))
           ((NAMED-STRUCTURE-P OBJ)
            (or *only-share-symbols* (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T))
            (DOTIMES (J (LENGTH OBJ))
              (WALK-OBJECT (AREF OBJ J))))
           ((ARRAY-HAS-LEADER-P OBJ)
            (WALK-OBJECT-UNHANDLED OBJ))
           ((EQ (SETQ TEMP (ARRAY-TYPE OBJ)) 'ART-Q)
            (or *only-share-symbols* (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T))
            (DOTIMES (J (LENGTH OBJ))
              (walk-OBJECT (AREF OBJ J))))
           ((EQ  TEMP 'ART-8B)
            (or *only-share-symbols* (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T)))
           ('ELSE
            (WALK-OBJECT-UNHANDLED OBJ))))
    (CONS
     (or *only-share-symbols* (SETF (GETHASH OBJ *OBJ-NODE-TABLE*) T))
     (DO ((J 0 (1+ J))
          (L OBJ (CDR L)))
         ((Atom L)
          (COND ((NULL L))
                ('ELSE
                 (walk-object l))))
       (walk-object (car l))))
    (INSTANCE
     (walk-OBJECT-UNHANDLED OBJ))
    (T
     (walk-OBJECT-UNHANDLED OBJ))))

(DEFUN walk-OBJECT-UNHANDLED (OBJ)
  (CERROR "transmit NIL instead"
          "object type or makeup not handled: ~S" OBJ))


(EVAL-WHEN (EVAL COMPILE LOAD)

(DEFVAR *OPCODES*
        '((0 OBJECT-NIL)
          (1 OBJECT-T)
          (2 POSITIVE-INTEGER)
          (3 NEGATIVE-INTEGER)
          (4 SHORT-FLOAT)
          (5 SINGLE-FLOAT)
          (6 SYMBOL)
          (7 LIST)
          (8 LIST*)
          (9 SIMPLE-ARRAY-ART-Q)
          (10 SIMPLE-ARRAY-ART-8B)
          (11 SIMPLE-STRING)
          (12 SIMPLE-NAMED-STRUCTURE)
          (13 SIMPLE-CHARACTER)
          (14 HAIRY-CHARACTER)
          (15 OBJECT-ASSIGN-INDEX)
          (16 OBJECT-OF-INDEX)))
)

(DEFMACRO OPCODE (NAME)
  (OR (CAR (CAR (MEM #'(LAMBDA (X E) (EQ X (CADR E))) NAME *OPCODES*)))
      (FERROR NIL "UNKNOWN OBJECT OPCODE NAME: ~S" NAME)))

(defun transmit-object (OBJ stream &AUX TEMP1 TEMP2)
  (WHEN (AND *OBJ-NODE-TABLE* (SETQ temp1 (GETHASH OBJ *OBJ-NODE-TABLE*)))
    (cond ((eq temp1 t))
          ((> temp1 0)
           (send stream :tyo (opcode object-of-index))
           (return-from transmit-object (transmiti temp1 stream)))
          ('else
           (send stream :tyo (opcode object-assign-index))
           (setq temp1 (- temp1))
           (setf (GETHASH OBJ *OBJ-NODE-TABLE*) temp1)
           (transmiti temp1 stream))))
  (TYPECASE OBJ
    (INTEGER
     (COND ((< OBJ 0)
            (SEND STREAM :TYO (OPCODE NEGATIVE-INTEGER))
            (TRANSMITI (- OBJ) STREAM))
           ('ELSE
            (SEND STREAM :TYO (OPCODE POSITIVE-INTEGER))
            (TRANSMITI OBJ STREAM))))
    (CHARACTER
     (SETQ TEMP1 (CHAR-BITS OBJ))
     (SETQ TEMP2 (CHAR-FONT OBJ))
       (COND ((AND (ZEROP TEMP1) (ZEROP TEMP2))
              (SEND STREAM :TYO (OPCODE SIMPLE-CHARACTER))
              (SEND STREAM :TYO (CHAR-CODE OBJ)))
             ('ELSE
              (SEND STREAM :TYO (OPCODE HAIRY-CHARACTER))
              (SEND STREAM :TYO (CHAR-CODE OBJ))
              (SEND STREAM :TYO TEMP1)
              (SEND STREAM :TYO TEMP2))))
    (SYMBOL
     (COND ((NULL OBJ)
            (SEND STREAM :TYO (OPCODE OBJECT-NIL)))
           ((EQ OBJ T)
            (SEND STREAM :TYO (OPCODE OBJECT-T)))
           ('ELSE
            (SEND STREAM :TYO (OPCODE SYMBOL))
            (TRANSMIT-OBJECT (SYMBOL-NAME OBJ) STREAM)
            (TRANSMIT-OBJECT (PACKAGE-NAME (SYMBOL-PACKAGE OBJ)) STREAM))))

    (STRING
     (COND ((AND (ARRAY-HAS-LEADER-P OBJ)
                 ;; allow a string with fill pointer to show up as a simple
                 ;; string on the receiving end.
                 (NOT (= (ARRAY-LEADER-LENGTH OBJ) 1)))
            (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))
           ('ELSE
            (SEND STREAM :TYO (OPCODE SIMPLE-STRING))
            (SETQ TEMP2 (LENGTH OBJ))
            (TRANSMITI TEMP2 STREAM)
            (SEND STREAM :STRING-OUT OBJ 0 TEMP2))))
    (ARRAY
     (COND ((not (= (array-rank obj) 1))
            (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))
           ((SETQ TEMP1 (NAMED-STRUCTURE-P OBJ))
            (SETQ TEMP2 (LENGTH OBJ))
            (SEND STREAM :TYO (OPCODE SIMPLE-NAMED-STRUCTURE))
            (TRANSMITI TEMP2 STREAM)
            (TRANSMIT-OBJECT TEMP1 STREAM)
            (DOTIMES (J TEMP2)
              (TRANSMIT-OBJECT (AREF OBJ J) STREAM)))
           ((ARRAY-HAS-LEADER-P OBJ)
            (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))
           ((EQ (SETQ TEMP1 (ARRAY-TYPE OBJ)) 'ART-Q)
            (SETQ TEMP2 (LENGTH OBJ))
            (SEND STREAM :TYO (OPCODE SIMPLE-ARRAY-ART-Q))
            (TRANSMITI TEMP2 STREAM)
            (DOTIMES (J TEMP2)
              (TRANSMIT-OBJECT (AREF OBJ J) STREAM)))
           ((EQ TEMP1 'ART-8B)
            (SETQ TEMP2 (LENGTH OBJ))
            (SEND STREAM :TYO (OPCODE SIMPLE-ARRAY-ART-8B))
            (TRANSMITI TEMP2 STREAM)
            (SEND STREAM :STRING-OUT OBJ 0 TEMP2))
           ('ELSE
            (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))))
    (CONS
     (DO ((J 0 (1+ J))
          (TEMP1 OBJ (CDR TEMP1)))
         ((Atom TEMP1)
          (COND ((NULL TEMP1)
                 (SEND STREAM :TYO (OPCODE LIST))
                 (TRANSMITI J STREAM)
                 (DOLIST (TEMP2 OBJ)
                   (TRANSMIT-OBJECT TEMP2 STREAM)))
                ('ELSE
                 (SEND STREAM :TYO (OPCODE LIST*))
                 (TRANSMITI (1+ J) STREAM)
                 (DO ((TEMP2 OBJ (CDR TEMP2)))
                     ((ATOM TEMP2)
                      (TRANSMIT-OBJECT TEMP2 STREAM))
                   (TRANSMIT-OBJECT (CAR TEMP2) STREAM)))))))
    (INSTANCE
     (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))
    (T
     (TRANSMIT-OBJECT-UNHANDLED OBJ STREAM))))

(DEFUN RECEIVE-OBJECT (STREAM &AUX TEMP1 TEMP2 INDEX)
  (SETQ TEMP1 (SEND STREAM :TYI))
  (COND ((= TEMP1 (OPCODE OBJECT-ASSIGN-INDEX))
         (SETQ INDEX (RECEIVEI STREAM))
         (SETQ TEMP1 (SEND STREAM :TYI))))
  (COND ((= TEMP1 (OPCODE OBJECT-OF-INDEX))
         (AREF *OBJ-ARRAY* (RECEIVEI STREAM)))
        ((= TEMP1 (OPCODE OBJECT-NIL))
         NIL)
        ((= TEMP1 (OPCODE OBJECT-T))
         T)
        ((= TEMP1 (OPCODE POSITIVE-INTEGER))
         (RECEIVEI STREAM))
        ((= TEMP1 (OPCODE NEGATIVE-INTEGER))
         (- (RECEIVEI STREAM)))
        ((= TEMP1 (OPCODE SYMBOL))
         (SETQ TEMP1 (INTERN (RECEIVE-OBJECT STREAM)
                             (RECEIVE-OBJECT STREAM)))
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP1))
         TEMP1)
        ((= TEMP1 (OPCODE SIMPLE-STRING))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (MAKE-ARRAY TEMP1 :TYPE 'ART-STRING))
         (SEND STREAM :STRING-IN NIL TEMP2 0 TEMP1)
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         TEMP2)
        ((= TEMP1 (OPCODE SIMPLE-ARRAY-ART-8B))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (MAKE-ARRAY TEMP1 :TYPE 'ART-8B))
         (SEND STREAM :STRING-IN NIL TEMP2 0 TEMP1)
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         TEMP2)
        ((= TEMP1 (OPCODE SIMPLE-NAMED-STRUCTURE))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (RECEIVE-OBJECT STREAM))
         (SETQ TEMP2 (MAKE-ARRAY TEMP1 :NAMED-STRUCTURE-SYMBOL TEMP2
                                 :TYPE 'ART-Q))
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         (RECEIVE-OBJECT STREAM) ; THROW IT AWAY.
         ;; SOMETHING VERY SCREWY ABOUT NAMED STRUCTURES
         (DO ((J 1 (1+ J)))
             ((= J TEMP1))
           (SETF (AREF TEMP2 J) (RECEIVE-OBJECT STREAM)))
         TEMP2)
        ((= TEMP1 (OPCODE SIMPLE-ARRAY-ART-Q))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (MAKE-ARRAY TEMP1))
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         (DOTIMES (J TEMP1)
           (SETF (AREF TEMP2 J) (RECEIVE-OBJECT STREAM)))
         TEMP2)
        ((= TEMP1 (OPCODE SIMPLE-CHARACTER))
         (CODE-CHAR (SEND STREAM :TYI)))
        ((= TEMP1 (OPCODE HAIRY-CHARACTER))
         (CODE-CHAR (SEND STREAM :TYI)
                    (SEND STREAM :TYI)
                    (SEND STREAM :TYI)))
        ((= TEMP1 (OPCODE LIST))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (MAKE-LIST TEMP1))
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         (DO ((TEMP1 TEMP2 (CDR TEMP1)))
             ((NULL TEMP1))
           (RPLACA TEMP1 (RECEIVE-OBJECT STREAM)))
         TEMP2)
        ((= TEMP1 (OPCODE LIST*))
         (SETQ TEMP1 (RECEIVEI STREAM))
         (SETQ TEMP2 (MAKE-LIST* TEMP1))
         (IF INDEX (SETF (AREF *OBJ-ARRAY* INDEX) TEMP2))
         (DO ((TEMP1 TEMP2 (CDR TEMP1)))
             (NIL)
           (RPLACA TEMP1 (RECEIVE-OBJECT STREAM))
           (IF (NULL (CDR TEMP1))
               (RETURN  (RPLACD TEMP1 (RECEIVE-OBJECT STREAM)))))
         TEMP2)
        (T
         (CERROR "return NIL"
                 "unknown object opcode received: ~D" temp1)
         nil)))


(DEFUN TRANSMITI (POSITIVE-INTEGER STREAM &AUX LENGTH)
  (COND ((< POSITIVE-INTEGER 128)
         (RETURN-FROM TRANSMITI (SEND STREAM :TYO POSITIVE-INTEGER)))
        ((< (SETQ LENGTH (CEILING (HAULONG POSITIVE-INTEGER) 8)) 128)
         (SEND STREAM :TYO (+ LENGTH 127)))
        ('ELSE
         (SEND STREAM :TYO 255)
         (TRANSMITI LENGTH STREAM)))
  (DOTIMES (J LENGTH)
    (SEND STREAM :TYO (LDB (BYTE 8 (* 8 J)) POSITIVE-INTEGER))))

(DEFUN RECEIVEI (STREAM &AUX NUMBER)
  (LET ((LENGTH (SEND STREAM :TYI)))
    (COND ((< LENGTH 128)
           (RETURN-FROM RECEIVEI LENGTH))
          ((< (SETQ LENGTH (- LENGTH 127)) 128))
          ('ELSE
           (SETQ LENGTH (RECEIVEI STREAM))))
    (SETQ NUMBER 0)
    (DOTIMES (J LENGTH)
      (SETQ NUMBER (DPB (SEND STREAM :TYI)
                        (BYTE 8 (* 8 J))
                        NUMBER)))
    NUMBER))


(DEFUN TRANSMIT-OBJECT-UNHANDLED (OBJ STREAM)
  (CERROR "transmit NIL instead"
          "OBJECT TYPE OR MAKEUP NOT HANDLED: ~S" OBJ)
  (TRANSMIT-OBJECT NIL STREAM))


(DEFUN MAKE-LIST* (N)
  (CASEQ N
    (2 (LIST* NIL NIL))
    (3 (LIST* NIL NIL NIL))
    (4 (LIST* NIL NIL NIL NIL))
    (T (MAKE-LIST (1- N)))))


(defun hexdump (filename)
  (with-open-file (stream filename :characters nil :byte-size 8)
    (do ((j 0 (1+ j))
         (A #(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))
         (c))
        ((null (setq c (send stream :tyi))))
      (if (zerop (mod j 32)) (terpri))
      ; grossly slow (format t "~16,2,'0R" C)
      (SEND STANDARD-OUTPUT :TYO (AREF A (LDB #o0404 C)))
      (SEND STANDARD-OUTPUT :TYO (AREF A (LDB #o0004 C))))))


(DEFUN OBJCOMP-FILE (FILENAME)
  (LET ((FORMS)(INPUT-TRUENAME))
    (WITH-OPEN-FILE (STREAM FILENAME :DIRECTION :INPUT :ERROR :REPROMPT)
      (SETQ INPUT-TRUENAME (SEND STREAM :TRUENAME))
      (FS:READING-FROM-STREAM (X STREAM)
        (PUSH X FORMS)))
    (SAVE-OBJECT `(PROGN ,@(NREVERSE FORMS))
                 (SEND INPUT-TRUENAME :NEW-PATHNAME :TYPE "SAVE" :VERSION :NEWEST))))

