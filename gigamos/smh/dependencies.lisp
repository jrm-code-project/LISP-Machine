;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10; Fonts:(TVFONT) -*-

(defmacro d-callers (x) `(car ,x))
(defmacro d-callees (x) `(cdr ,x))

(defun pht (ht)
  (maphash #'(lambda (k v) (format t "~%~a~@[~30tfrom:~{ ~a~}~]~:[~;~%~]~@[~30tto:~35t~{ ~a~}~]"
                                   k (d-callers v) (and (d-callees v) (d-callers v)) (d-callees v))) ht)
  (values))

(defun enq (ht fnc)
  (let ((v (gethash fnc ht)))
    (format t "~%~@[from:~{ ~a~}~]~:[~;~%~]~@[to:  ~{ ~a~}~]"
            (d-callers v) (and (d-callees v) (d-callers v)) (d-callees v)))
  (values))

(defun paths (ht from to &optional (limit 5))
  (paths-1 ht from to limit (if (equal from to) nil from)))

(defun paths-1 (ht from to limit exclude &aux paths)
  (dolist (callee (d-callees (gethash from ht)) paths)
    (cond ((equal callee exclude))
          ((equal callee to)
           (pushnew (list from to) paths))
          ((> limit 0)
           (let ((conts (paths-1 ht callee to (1- limit) exclude)))
             (do ((path conts (cdr path)))
                 ((null path))
               (push from (car path)))
             (setq paths (nconc conts paths)))))))

(defun print-paths-to (ht sym &optional (limit 5))
  (dolist (path (paths-to ht sym limit))
    (print path))
  (values))

(defun paths-to (ht to &optional (limit 5))
  (paths-to-1 ht (list to) limit))

(defun paths-to-1 (ht path limit &aux paths)
  (let ((callers (d-callers (gethash (car path) ht))))
    (cond ((null callers) (list path))
          ((= limit 0) (list path))
          (t (dolist (caller callers paths)
               (unless (eq caller (car path))
                 (setq paths
                       (nconc paths
                              (paths-to-1 ht (cons caller path) (1- limit))))))))))

(defun build-dependency-hash-table (&rest packages)
  (LET ((*hashtab* (make-hash-table :test #'equal))
        (*packages* (mapcar #'PKG-FIND-PACKAGE packages))
        (*function* #'dependency-store))
    (DECLARE (SPECIAL *hashtab* *packages* *function*))
    (dolist (pkg packages)
      (mapatoms #'build-dependency-hash-table-1 pkg nil))
    *hashtab*))

(defun dependency-store (caller callee)
  (declare (special *hashtab*))
  (let ((caller-entry (gethash caller *hashtab*))
        (callee-entry (gethash callee *hashtab*)))
    (when (null caller-entry)
      (setf (gethash caller *hashtab*)
            (setq caller-entry (cons nil nil))))
    (when (null callee-entry)
      (setf (gethash callee *hashtab*)
            (setq callee-entry (cons nil nil))))
    (pushnew callee (d-callees caller-entry) :test #'equal)
    (pushnew caller (d-callers callee-entry) :test #'equal)))

(DEFUN build-dependency-hash-table-1 (CALLER)
  ;; Ignore all symbols which are forwarded to others, to avoid duplication.
  (AND ( (%P-DATA-TYPE (LOCF (SYMBOL-FUNCTION CALLER))) DTP-ONE-Q-FORWARD)
       (FBOUNDP CALLER)
       (build-dependency-hash-table-2 CALLER (SYMBOL-FUNCTION CALLER)))
  #+ignore
  (UNLESS (= (%P-DATA-TYPE (LOCF (SYMBOL-PLIST CALLER))) DTP-ONE-Q-FORWARD)
    #+ignore
    (DO ((L (SYMBOL-PLIST CALLER) (CDDR L)))                    ; Also look for properties
        ((NULL L))
      (IF (TYPEP (CADR L) 'COMPILED-FUNCTION)
          (FIND-CALLERS-OF-SYMBOLS-AUX-FEF
            `(:PROPERTY ,CALLER ,(CAR L))
            (CADR L))))
    #+ignore
    (let ((fl (GET CALLER 'SI:FLAVOR)))                         ; Also look for flavor methods
      (AND FL
           (ARRAYP FL)                                          ;Could be T
           (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
             (DOLIST (METH (CDDDR MTE))
               (IF (METH-DEFINEDP METH)
                   (build-dependency-hash-table-2 (METH-FUNCTION-SPEC METH)
                                                  (METH-DEFINITION METH)))))))
    #+ignore
    (IF (GET CALLER 'INITIALIZATION-LIST)                       ; Also look for initializations
        (DOLIST (INIT-LIST-ENTRY (SYMBOL-VALUE CALLER))
          (FIND-CALLERS-OF-SYMBOLS-AUX-LIST CALLER (INIT-FORM INIT-LIST-ENTRY))))))

(DEFUN build-dependency-hash-table-2 (CALLER DEFN)
  ;; Don't be fooled by macros, interpreted or compiled.
  (IF (EQ (CAR-SAFE DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
  (TYPECASE DEFN
    (COMPILED-FUNCTION (build-dependency-hash-table-FEF CALLER DEFN))
    #+ignore (CONS (FIND-CALLERS-OF-SYMBOLS-AUX-LAMBDA CALLER DEFN))
    #+ignore (SELECT (FIND-CALLERS-OF-SYMBOLS-AUX-SELECT-METHOD CALLER (%MAKE-POINTER DTP-LIST DEFN))))
  ;; this function is traced, advised, etc.
  ;; then look through the actual definition.
  (IF (OR (CONSP DEFN) (TYPEP DEFN 'COMPILED-FUNCTION))
      (LET* ((DEBUG-INFO (DEBUGGING-INFO DEFN))
             (INNER (ASSQ 'SI::ENCAPSULATED-DEFINITION DEBUG-INFO)))
        (AND INNER (build-dependency-hash-table-1 (CADR INNER))))))

(DEFUN build-dependency-hash-table-FEF (CALLER DEFN &AUX TEM OFFSET SYM)
  (DECLARE (SPECIAL *FUNCTION* *packages*))
  (DO ((I si:%FEF-HEADER-LENGTH (1+ I))
       (LIM (TRUNCATE (si:FEF-INITIAL-PC DEFN) 2)))
      (( I LIM) NIL)
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-ONE-Q-FORWARD)
           (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
                 SYM (%FIND-STRUCTURE-HEADER TEM)
                 OFFSET (%POINTER-DIFFERENCE TEM SYM))
           (COND ((NOT (SYMBOLP SYM)))
                 ((= OFFSET 2)                                  ;Function cell reference
                  (when (memq (symbol-package SYM) *packages*)
                    (FUNCALL *FUNCTION* CALLER SYM)))))
          ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) si:DTP-INDEXED-FORWARD)
           (SETQ TEM (AREF SI:*INDEX-NAME-TABLE* (%P-LDB-OFFSET %%Q-POINTER DEFN I)))
           (SETQ SYM (CADR TEM))
           (SETQ TEM (CAR TEM))
           (COND ((EQ TEM :FUNCTION)
                  (when (memq (symbol-package SYM) *packages*)
                    (FUNCALL *FUNCTION* CALLER SYM)))))))
  ;; See if the fef uses the symbol as a macro.
  (LET ((DI (DEBUGGING-INFO DEFN)))
    (DOLIST (M (CADR (ASSQ ':MACROS-EXPANDED DI)))
      (if (consp m) (setq m (car m)))
      (when (memq (symbol-package SYM) *packages*)
        (FUNCALL *FUNCTION* CALLER M))))
  ;; See if we have a function reference compiled into a misc instruction
  #+ignore
  (DOLIST (SYM *SYMBOLS*)
    (IF (FEF-CALLS-MISC-FUNCTION DEFN SYM)
        (FUNCALL *FUNCTION* CALLER SYM :MISC-FUNCTION)))
  (AND (si:FEF-DEBUGGING-INFO-PRESENT-P DEFN)
       (SETQ TEM (CDR (ASSQ ':INTERNAL-FEF-OFFSETS (si:FEF-DEBUGGING-INFO DEFN))))
       (LOOP FOR OFFSET IN TEM
             FOR I FROM 0
             DO (build-dependency-hash-table-FEF `(:INTERNAL ,CALLER ,I)
                                                 (%P-CONTENTS-OFFSET DEFN OFFSET)))))
#+ignore
(defun find-callers-of-symbols-aux-select-method (caller list)
  (declare (special *symbols* *function*))
  (do ((l list (cdr l)))
      ((atom l)
       (if l
           (find-callers-of-symbols-aux1 caller l)))
    (let ((sym (caar l)))
      (if (memq sym *symbols*)
          (funcall *function* caller sym :constant)))
    (find-callers-of-symbols-aux1 caller (cdar l))))

;;; See if this FEF uses a certain MISC instruction
#+ignore
(DEFUN FEF-CALLS-MISC-FUNCTION (FEF SYM &AUX TEM INST)
  (AND (GET SYM 'COMPILER::QINTCMP)
       (SETQ TEM (GET SYM 'COMPILER::QLVAL))
       (DO ((MISCINST                           ;Misc instruction sought
              (IF ( TEM #o1000)
                  (+ #o35000 (LOGAND #o777 TEM))
                  (+ #o15000 TEM)))
            (MISCMASK #o37777)                  ;Masks out destination
            (LONGJUMP #o14777)                  ;First word of 2-word jump instruction
            (LONGJUMP1 #o34777)                 ;First word of 2-word jump instruction
            (PC (FEF-INITIAL-PC FEF) (1+ PC))
            (MAXPC (FEF-LIMIT-PC FEF)))
           (( PC MAXPC) NIL)
         (SETQ INST (LOGAND (%P-LDB-OFFSET (IF (ODDP PC) %%Q-HIGH-HALF %%Q-LOW-HALF)
                                           FEF (TRUNCATE PC 2))
                            MISCMASK))
         (COND ((= INST MISCINST) (RETURN T))
               ((= INST LONGJUMP) (INCF PC))
               ((= INST LONGJUMP1) (INCF PC))))))

;;; Tree-walk CALLER looking for *FUNCTION*.  CALLER should be the function name,
;;; and DEFN should be its definition.  Avoids listing symbols twice.
#+ignore
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LIST (CALLER DEFN)
  (LET ((*SUPPRESS* ()))
    (DECLARE (SPECIAL *SUPPRESS*))
    (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER DEFN)))

#+ignore
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LAMBDA (CALLER DEFN)
  (LET ((*SUPPRESS* ()))
    (DECLARE (SPECIAL *SUPPRESS*))
    (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER (LAMBDA-EXP-ARGS-AND-BODY DEFN))))

#+ignore
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 (CALLER DEFN)
  (DECLARE (SPECIAL *SUPPRESS* *SYMBOLS* *FUNCTION*))
  (DO ((L DEFN (CDR L)))
      ((ATOM L))
    (COND ((AND (SYMBOLP (CAR L))
                (NOT (MEMQ (CAR L) *SUPPRESS*))
                (MEMQ (CAR L) *SYMBOLS*))
           (PUSH (CAR L) *SUPPRESS*)
           (FUNCALL *FUNCTION* CALLER (CAR L) NIL))
          ((CONSP (CAR L))
           (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER (CAR L))))))



(import 'compiler:limited-backtrace)                            ;hack hack hack

(defun limited-backtrace (&optional (n 4) (pkg nil))
  (let ((limited-package (when pkg (PKG-FIND-PACKAGE PKG))))
    (declare (special limited-package))
    (limited-backtrace-1 n)))

(DEFUN limited-backtrace-1 (n)
  "Returns a list of the last N most recent callers."
  (declare (special limited-package))
  (LET* ((SG si::%CURRENT-STACK-GROUP)
         (RP (si::SG-REGULAR-PDL SG))
         ;;(FNVAL (FDEFINITION FUNCTION-SPEC))
         (INIFN (si::SG-INITIAL-FUNCTION-INDEX SG))
         ret)
    (DO ((i 0)
         (AP (%POINTER-DIFFERENCE (%STACK-FRAME-POINTER) RP)
             (- AP (si::RP-DELTA-TO-ACTIVE-BLOCK RP AP))))
        ((or (= i n)
             ( AP INIFN)))
      (let ((fname (function-name (si::RP-FUNCTION-WORD RP AP))))
        (when (or (null limited-package)
                  (eq limited-package (typecase fname
                                        (symbol (symbol-package fname))
                                        (cons (and (symbolp (cadr fname))
                                                   (symbol-package (cadr fname))))      ;a guess at a function spec
                                        (t nil))))
          (push fname ret)
          (incf i))))
    (nreverse ret)))



(defun print-file-package-properties (directory-pathname)
  (let* ((directory-pathname (pathname directory-pathname))
         (generic-source (make-pathname :defaults nil
                                        :host (pathname-host directory-pathname)
                                        :name :WILD
                                        :version :NEWEST
                                        :type :LISP)))
    (with-open-stream (output (zwei:open-editor-stream :buffer-name "file-packages"
                                                       :create-p T
                                                       :kill T
                                                       :start :beginning :end :end
                                                       ))
      (labels ((doit (dir-pn)
                     (dolist (lisp-file (cdr (fs:directory-list (merge-pathnames generic-source dir-pn))))
                       (let* ((pn (car lisp-file))
                              (pack (getf (fs:file-attribute-list pn) :package)))
                         (format output "~%(~s ~s)" (send pn :string-for-printing)
                                 (if (consp pack) (car pack) pack))))
                     (let ((dirlst (pathname-directory dir-pn)))
                       (unless (listp dirlst) (setq dirlst (list dirlst)))
                       (dolist (dir (cdr (fs:directory-list (format nil "~a:~{~a~^.~};*.DIRECTORY"
                                                                    (pathname-host dir-pn) dirlst))))
                         (doit (pathname (format nil "~a:~{~a~^.~}.~a;"
                                                 (pathname-host dir-pn)
                                                 dirlst
                                                 (pathname-name (car dir)))))))))
        (doit directory-pathname)))))

(defun index-files-by-package (&aux (alist nil) item)
  (with-open-stream (input (zwei:open-editor-stream :buffer-name "file-packages"
                                                    :create-p :error
                                                    :start :beginning :end :end
                                                    ))
    (loop while (neq (setq item (read input 'EOF)) 'EOF)
          do (let ((entry (assq (cadr item) alist)))
               (if entry
                   (push (car item) (cdr entry))
                 (push (list (cadr item) (car item)) alist)))))
  alist)
