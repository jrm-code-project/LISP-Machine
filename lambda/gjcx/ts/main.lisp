;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:CL; Base:10 -*-

;;; this code has been modified for the new one-q-forward instead of
;;; external-value-cell-pointer in fefs. it will only run in systems >= 121
;;;

(DEFUN FORWARD-VARIABLE-TO-A-MEMORY (SYMBOL LOCATION)
  ;; usually an a-mem location like this is fixed at cold load time.
  ;; so this function is used only when experimenting with new microcodes
  ;; in an old system.
  (LET ((A-MEM-LOC (COND ((FIXP LOCATION)
                          LOCATION)
                         ('ELSE
                          (LAM:ASSURE-LAM-SYMBOLS-LOADED)
                          (- (OR (lam:lam-lookup-name (INTERN (STRING LOCATION) "LAM"))
                                 (FERROR NIL "SYMBOL NOT FOUND: ~S" LOCATION))
                             LAM:RAAMO)))))
    (%P-STORE-TAG-AND-POINTER (+ A-MEM-LOC SYS:A-MEMORY-VIRTUAL-ADDRESS)
                              (%DATA-TYPE (SYMEVAL SYMBOL))
                              (SYMEVAL SYMBOL))
    (%P-STORE-TAG-AND-POINTER (VALUE-CELL-LOCATION SYMBOL)
                              DTP-ONE-Q-FORWARD
                              (+ A-MEM-LOC SYS:A-MEMORY-VIRTUAL-ADDRESS)))
  SYMBOL)


(DEFUN ALLOCATE-INDEX (TYPE)
  (COND ((GETHASH TYPE *INDEX-ALLOCATION-TABLE*)
         (FERROR NIL "ALREADY HAVE INDEX FOR ~S" TYPE))
        ('ELSE
         (GET-INDEX TYPE))))

;; WE KEEP THE INDEX CELL-ARRAYS AS SIMPLE UNFORWARDED ARRAYS FOR EFFICIENCY.

(DEFUN GET-INDEX (TYPE)
  "Type is a cell specification, e.g. (:function foo) for the function cell of foo, (:plist bar)"
  (CHECK-ARG TYPE (AND (CONSP TYPE)
                       (MEMQ (CAR TYPE) '(:FUNCTION :VALUE :PLIST))
                       (SYMBOLP (CADR TYPE)))
             "a legal cell type specification")
  (LET ((INDEX (GETHASH TYPE *INDEX-ALLOCATION-TABLE*)))
    (COND (INDEX)
          ('ELSE
           (WITH-LOCK ((GET 'GET-INDEX 'LOCK))
             (SETQ INDEX *INDEX-UNALLOCATED*)
             (PUTHASH TYPE INDEX *INDEX-ALLOCATION-TABLE*)
             (UNLESS (< INDEX (ARRAY-LENGTH *INDEX-NAME-TABLE*))
               (LET ((OLD *INDEX-NAME-TABLE*)
                     (N (ARRAY-LENGTH *INDEX-NAME-TABLE*)))
                 (SETQ *INDEX-NAME-TABLE* (MAKE-ARRAY (* 2 N)))
                 (COPY-ARRAY-PORTION OLD 0 N
                                     *INDEX-NAME-TABLE* 0 N)))
             (SETF (AREF *INDEX-NAME-TABLE* INDEX) TYPE)
             (DOLIST (A *CELL-ARRAYS*)
               (UNLESS (< INDEX (ARRAY-LENGTH (CELL-PLACE.ARRAY A)))
                 (GROW-CELL-ARRAY A)))
             (SETQ *INDEX-UNALLOCATED* (1+ INDEX))
             INDEX)))))




(DEFUN GROW-CELL-ARRAY (A)
  (LET ((OLD (CELL-PLACE.ARRAY A))
        (N (ARRAY-LENGTH (CELL-PLACE.ARRAY A))))
    (SETF (CELL-PLACE.ARRAY A) (MAKE-ARRAY (* 2 N)))
    ;; THIS IS USED INSTEAD OF COPY-ARRAY-PORTION
    ;; BECAUSE OF THE UNBOUND MARKERS (DTP-NULL'S) IN THE
    ;; ARRAY.
    (%BLT-TYPED (%POINTER-PLUS OLD
                               (ARRAY-DATA-OFFSET OLD))
                (%POINTER-PLUS (CELL-PLACE.ARRAY A)
                               (ARRAY-DATA-OFFSET (CELL-PLACE.ARRAY A)))
                N
                1)
    (WHEN (EQ OLD *INDEXED-CELL-ARRAY*)
      (SETQ *INDEXED-CELL-ARRAY* (CELL-PLACE.ARRAY A)))
    (WHEN (EQ OLD *GLOBAL-INDEXED-CELL-ARRAY*)
      (SETQ *GLOBAL-INDEXED-CELL-ARRAY* (CELL-PLACE.ARRAY A))))
  A)


(DEFUN DE (&OPTIONAL (N *INDEX-UNALLOCATED*))
  (FORMAT T "~&~D ALLOCATED INDEX NUMBERS~%" *INDEX-UNALLOCATED*)
  (DOTIMES (J N)
    (FORMAT T "~D = ~S ~:[un~]bound~%" J
            (AREF *INDEX-NAME-TABLE* J)
            (location-boundp (locf (aref *indexed-cell-array* j))))))

(DEFUN NEW-INDEXED-CELL-ARRAY ()
  "RETURN A COPY OF THE GLOBAL INDEXED CELL ARRAY"
  (LET ((A (COPY-OBJECT *GLOBAL-INDEXED-CELL-ARRAY*)))
    (PUSH (MAKE-CELL-PLACE :ARRAY A) *CELL-ARRAYS*)
    A))

(DEFUN FLUSH-INDEXED-CELL-ARRAY (A)
  (SETQ *CELL-ARRAYS* (DELQ A *CELL-ARRAYS*)))


(DEFUN STORE-INDEXED-FORWARD (POINTER INDEX)
  (%P-STORE-TAG-AND-POINTER POINTER DTP-INDEXED-FORWARD INDEX))

(DEFUN INDEXIFY-SYMBOL-CELLS (SYMBOL)
  (INDEXIFY-SYMBOL-CELLS-1 SYMBOL #'ALLOCATE-INDEX))

(DEFUN RE-INDEXIFY-SYMBOL-CELLS (SYMBOL)
  (INDEXIFY-SYMBOL-CELLS-1 SYMBOL #'GET-INDEX))

(DEFUN INDEXIFY-SYMBOL-CELLS-1 (SYMBOL ALLOCATE-INDEX)
  (LET ((V-I (FUNCALL ALLOCATE-INDEX `(:VALUE ,SYMBOL)))
        (F-I (FUNCALL ALLOCATE-INDEX `(:FUNCTION ,SYMBOL)))
        (P-I (FUNCALL ALLOCATE-INDEX `(:PLIST ,SYMBOL))))
    (LET* ((VP (BOUNDP SYMBOL))
           (V (IF VP (SYMEVAL SYMBOL)))
           (FP (FBOUNDP SYMBOL))
           (F (IF FP (FSYMEVAL SYMBOL)))
           (P (PLIST SYMBOL)))
      (DOLIST (CP *CELL-ARRAYS*)
        (LET ((A (CELL-PLACE.ARRAY CP)))
          (COND (VP
                 (SETF (AREF A V-I) V))
                ('ELSE
                 (LOCATION-MAKUNBOUND (LOCF (AREF A V-I)) SYMBOL)))
          (COND (FP
                 (SETF (AREF A F-I) F))
                ('ELSE
                 (LOCATION-MAKUNBOUND (LOCF (AREF A F-I)) SYMBOL)))
          (SETF (AREF A P-I) (COPY-LIST P)))))
    (STORE-INDEXED-FORWARD (LOCF (SYMBOL-VALUE SYMBOL)) V-I)
    (STORE-INDEXED-FORWARD (LOCF (SYMBOL-FUNCTION SYMBOL)) F-I)
    (STORE-INDEXED-FORWARD (LOCF (SYMBOL-PLIST SYMBOL)) P-I)
    SYMBOL))

(DEFUN PKG-INTERN-AND-INDEXIFY-STORE (HASH SYM PKG)
  (MULTIPLE-VALUE-BIND (SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE)
      (PKG-INTERN-STORE HASH SYM PKG)
    (WHEN (EQ (SYMBOL-PACKAGE SYMBOL) PKG)
      (INDEXIFY-SYMBOL-CELLS SYMBOL))
    (VALUES SYMBOL ALREADY-INTERNED-FLAG ACTUAL-PACKAGE)))


(DEFUN SET-PACKAGE-AUTO-INDEXIFY (PKG)
  (CHECK-TYPE PKG PACKAGE)
  (SETF (PKG-NEW-SYMBOL-FUNCTION PKG) 'PKG-INTERN-AND-INDEXIFY-STORE)
  (MAPATOMS #'(LAMBDA (X)
                (WHEN (EQ (SYMBOL-PACKAGE X) PKG)
                  (INDEXIFY-SYMBOL-CELLS X)))
            PKG
            NIL)
  PKG)



(DEFUN ASSOCIATED-INDEXED-CELL-ARRAY (X &OPTIONAL COPIER-FUNCTION)
  (OR (CDR (ASSQ X *ASSOCIATED-INDEXED-CELL-ARRAYS*))
      (LET ((A (NEW-INDEXED-CELL-ARRAY))
            (COPIER (OR COPIER-FUNCTION #'TOP-LEVEL-CELL-VALUE-COPIER)))
        (PUSH (CONS X A) *ASSOCIATED-INDEXED-CELL-ARRAYS*)
        (DOTIMES (J *INDEX-UNALLOCATED*)
          (LET ((LOC (LOCF (AREF A J))))
            (WHEN (LOCATION-BOUNDP LOC)
              (LET ((VALUE (CONTENTS LOC)))
                (SETF (CONTENTS LOC) (FUNCALL COPIER VALUE (AREF *INDEX-NAME-TABLE* J)))))))
        A)))


(DEFUN TOP-LEVEL-CELL-VALUE-COPIER (VALUE NAME)
  ;; standard hacker. for example in MACSYMA we know we have to copy the
  ;; toplevel of all list structures, and throw in arrays for good keeping.
  ;; anything not top level should have been copied into a pure area previously
  ;; for safe keeping.
  NAME
  ;; SHOULD FIRST CHECK TO SEE IF VALUE IS IN A READ-ONLY AREA. THEN DONT BOTHER TO COPY IT.
  (COND ((CONSP VALUE)
         (COND ((CIRCULARP VALUE)
                VALUE)
               ('ELSE
                (COPYLIST VALUE))))
        ((ARRAYP VALUE)
         (COPY-OBJECT VALUE))
        ('ELSE
         ;; CONSIDER ALL OTHER ATOMS. E.G. PATHNAMES, FEF'S, SYMBOLS
         ;; AS NOT NEEDING TO BE COPIED.
         VALUE)))

(DEFUN CIRCULARP (X)
  (DO ((L1 X (CDR L1))
       (L2 (CDR X) (CDDR L2)))
      ((ATOM L2)
       ())
    (IF (EQ L1 L2) (RETURN T))))


(defun map-fef-exit-vector-index (fef f)
  (CHECK-TYPE FEF COMPILED-FUNCTION)
  (DO ((I %FEF-HEADER-LENGTH (1+ I))
       (LIM (TRUNCATE (FEF-INITIAL-PC FEF) 2)))
      (( I LIM) NIL)
    (funcall f i)))



(DEFUN DESCRIBE-FEF-EXIT-VECTOR (FEF)
  (map-fef-exit-vector-index
    fef
    #'(lambda (i)
        (let ((dt (%p-ldb-offset %%Q-DATA-TYPE fef i)))
          (format t "~&~A(~D)~%" (nth dt q-data-types)
                  (%p-ldb-offset %%q-pointer fef i))))))


(DEFUN DESCRIBE-FEF-EXIT-VECTOR-2 (FEF)
  (map-fef-exit-vector-index
    fef
    #'(lambda (i)
        (let ((dt (%p-ldb-offset %%Q-DATA-TYPE fef i))
              (n  (%p-ldb-offset %%q-pointer fef i)))
          (format t "~&~A(~D)" (nth dt q-data-types) n)
          (cond ((= dt dtp-one-q-forward)
                 (let* ((loc (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF i))
                        (ptr (%find-structure-header loc))
                        (offset (%POINTER-DIFFERENCE loc PTR)))
                   (cond ((symbolp ptr)
                          (format t " ~A cell of ~S (is ~A)~%"
                                  (nth offset '("??" "value" "function" "plist" "package"))
                                  ptr
                                  (nth (%p-data-type loc) q-data-types)))
                         ('else
                          (terpri)))))
                ((= dt dtp-indexed-forward)
                 (format t "~S~%" (aref *index-name-table* n)))
                ('else
                 (terpri)))))))

(DEFUN RELINK-FEF-EXIT-VECTOR (OBJ)
  (RELINK-FEF-EXIT-VECTOR-INTERNAL (TYPECASE OBJ
                                     (CONS
                                      (CASE (CAR OBJ)
                                        (MACRO (CDR OBJ))
                                        (T (RETURN-FROM RELINK-FEF-EXIT-VECTOR NIL))))
                                     (COMPILED-FUNCTION OBJ)
                                     (T (RETURN-FROM RELINK-FEF-EXIT-VECTOR NIL)))))

(DEFUN RELINK-FEF-EXIT-VECTOR-INTERNAL (FEF)
  "Change all one-q-forwards that point to indexed symbols to dtp-index-forwards"
  (map-fef-exit-vector-index
    fef
    #'(lambda (i)
        (WHEN (= (%P-LDB-OFFSET %%Q-DATA-TYPE FEF I) DTP-ONE-Q-FORWARD)
          (let* ((loc (%P-CONTENTS-AS-LOCATIVE-OFFSET FEF i))
                 (ptr (%find-structure-header loc)))
            (when (and (symbolp ptr)
                       (= dtp-indexed-forward (%p-data-type loc)))
              (let ((%INHIBIT-READ-ONLY T))
                (%p-store-tag-and-pointer (%make-pointer-offset dtp-locative
                                                                fef
                                                                i)
                                          dtp-indexed-forward
                                          (%p-pointer loc)))))))))


(DEFUN UNLINK-FEF-EXIT-VECTOR (FEF)
  (map-fef-exit-vector-index
    fef
    #'(lambda (i)
        (when (= (%p-ldb-offset %%Q-DATA-TYPE fef i) dtp-indexed-forward)
          (let ((name (aref *index-name-table* (%p-ldb-offset %%q-pointer fef i))))
            (let ((%INHIBIT-READ-ONLY T))
              (%p-store-tag-and-pointer (%make-pointer-offset dtp-locative
                                                              fef
                                                              i)
                                        DTP-ONE-Q-FORWARD
                                        (%pointer-plus (cadr name)
                                                       (ecase (car name)
                                                         (:value 1)
                                                         (:function 2))))))))))

(defun relink-fefs-in-package (pkg)
  (mapatoms #'(lambda (sym)
                (when (fboundp sym)
                  (let ((f (fsymeval sym)))
                    (when (typep f 'compiled-function)
                      (relink-fef-exit-vector f)))))
            pkg
            nil))


(defun unlink-fefs-in-package (pkg)
  (mapatoms #'(lambda (sym)
                (when (fboundp sym)
                  (let ((f (fsymeval sym)))
                    (when (typep f 'compiled-function)
                      (unlink-fef-exit-vector f)))))
            pkg
            nil))



(defun sumarize-cell-array-data (&OPTIONAL PRINT-TYPES)
  (LET ((VALUES NIL)
        (FUNCTIONS NIL)
        (PLISTS 0))
    (DOTIMES (J *INDEX-UNALLOCATED*)
      (LET ((LOC (locf (aref *indexed-cell-array* j))))
        (WHEN (LOCATION-BOUNDP LOC)
          (CASE (CAR (AREF *INDEX-NAME-TABLE* J))
            (:PLIST
             (INCF PLISTS (FLOOR (LENGTH (CONTENTS LOC)) 2)))
            (:FUNCTION
             (LET ((TYPE (TYPE-OF (CONTENTS LOC))))
               (WHEN (MEMQ TYPE PRINT-TYPES)
                 (FORMAT T "~&~S = ~S~%"
                         (AREF *INDEX-NAME-TABLE* J)
                         (CONTENTS LOC)))
               (INCF (CDR (OR (ASSQ TYPE FUNCTIONS) (CAR (PUSH (CONS TYPE 0) FUNCTIONS)))))))
            (:VALUE
             (LET ((TYPE (TYPE-OF (CONTENTS LOC))))
               (WHEN (MEMQ TYPE PRINT-TYPES)
                 (FORMAT T "~&~S = ~S~%"
                         (AREF *INDEX-NAME-TABLE* J)
                         (CONTENTS LOC)))
               (INCF (CDR (OR (ASSQ TYPE VALUES)
                              (CAR (PUSH (CONS TYPE 0) VALUES)))))))))))
    (LIST :FUNCTION FUNCTIONS
          :PLIST PLISTS
          :VALUE VALUES)))


(DEFUN DE-SYM (X)
  (FORMAT T "~&~S has:~%" x)
  (dolist (c '((:value 1) (:function 2) (:plist 3)))
    (format t "~9S = ~S ~A(~D)~%"
            (car c)
            (gethash (list (car c) x) *index-allocation-table*)
            (nth (%p-data-type (%pointer-plus x (cadr c))) q-data-types)
            (%p-pointer (%pointer-plus x (cadr c))))))


(defun describe-fefs-in-package (pkg)
  (mapatoms #'(lambda (x)
                (when (and (fboundp x)
                           (typep (fsymeval x) 'compiled-function))
                  (format t "~&  ~A~%" x)
                  (describe-fef-exit-vector-2 (fsymeval x))))
            pkg
            nil))

;; used by EH:TRANS-TRAP-DECODE

(defun IDENTIFY-LOCATIVE-TO-INDEX-CELL-ARRAY (locative)
  (DOLIST (A *CELL-ARRAYS*)
    (let* ((ar (CELL-PLACE.ARRAY A))
           (k (- (%pointer-difference locative ar)
                 (array-data-offset ar))))
      (WHEN (AND (NOT (< K 0))
                 (< K *INDEX-UNALLOCATED*))
        (return (car (aref *index-name-table* k)))))))
