;-*- Mode:LISP; Package:SI; Base:10; Readtable:CL -*-

;; Where else to put this function?
(export 'report-elapsed-time)
;;;Merge from LAD sources on 9-18-86, mrc
(defun report-elapsed-time (stream indentation operation-name function &rest arguments)
  "Print \"Starting <operation-name>\" on STREAM, call FUNCTION on ARGUMENTS
and then print \"...Finished <operation-name> -- process took <number-of-seconds-taken>\"
Returns the values returned by FUNCTION."
  (declare (zwei:indentation 3 1))
  (setq stream (decode-print-arg stream))
  (cond ((eq stream #'si:null-stream)
         (apply function arguments))
        (t
         (format stream "~&~:[~;~:*~VT~]Starting ~~A...~"
                 (and (> indentation 0) indentation)
                 operation-name)
         (multiple-value-bind (x y)
             (send-if-handles stream :read-cursorpos :character)
           (let* (utime
                  (time (get-universal-time))
                  (xtime (%microsecond-time))
                  (otime (%microsecond-time))
                  (dw (read-meter '%disk-wait-time)))
             (multiple-value-prog1
               (apply function arguments)
               (setq utime (%microsecond-time)
                     time (- (get-universal-time) time)
                     dw (- (read-meter '%disk-wait-time) dw))
               (format stream
                       "~:[~&~:[~;~:*~VT~]...~;~* ~]Finished ~2:*~:[~*~~A~ ~]-- process took "
                       (multiple-value-bind (xx yy)
                           (send-if-handles stream :read-cursorpos :character)
                         (and x y xx yy (= x xx) (= y yy)))
                       (and (> indentation 0) indentation)
                       operation-name)
               (cond ((< time 30.)
                      (setq utime (- (%32-bit-difference utime otime)
                                     (%32-bit-difference otime xtime)))
                      (if (< utime 10000)
                          (format stream "~D microseconds, ~$ microseconds disk wait" utime dw)
                        (format stream "~:[~,1,-6F~;~,3,-6F~] seconds, ~$ seconds disk wait"
                                (< time 10)
                                utime
                                (quotient dw 1.0E6))))
                     (t
                      (time:print-interval-or-never time stream)
                      (princ ", " stream)
                      (if (< dw 0)
                          (format stream "indeterminate ")
                        (time:print-interval-or-never (round dw 1.0E6) stream))
                      (PRINC " disk wait." stream)))))))))

;;;Old DJ version-- commented out on 9-18-86; mrc
;(defun report-elapsed-time (stream indentation operation-name function &rest arguments)
;  "Print \"Starting <operation-name>\" on STREAM, call FUNCTION on ARGUMENTS
;and then print \"...Finished <operation-name> -- process took <number-of-seconds-taken>\"
;Returns the values returned by FUNCTION."
;  (declare (zwei:indentation 3 1))
;  (setq stream (decode-print-arg stream))
;  (if (eq stream #'si:null-stream)
;      (apply function arguments)
;    (format stream "~&~:[~;~:*~VT~]Starting ~~A...~"
;           (and (> indentation 0) indentation)
;           operation-name)
;    (multiple-value-bind (x y)
;       (send-if-handles stream :read-cursorpos :character)
;      (let* (utime
;            (time (get-universal-time))
;            (xtime (%microsecond-time))
;            (otime (%microsecond-time))
;            (dw (read-meter '%disk-wait-time)))
;       (multiple-value-prog1
;         (apply function arguments)
;         (setq utime (%microsecond-time)
;               time (- (get-universal-time) time)
;               dw (- (read-meter '%disk-wait-time) dw))
;         (format stream
;                 "~:[~&~:[~;~:*~VT~]...~;~* ~]Finished ~2:*~:[~*~~A~ ~]-- process took "
;                 (multiple-value-bind (xx yy)
;                     (send-if-handles stream :read-cursorpos :character)
;                   (and x y xx yy (= x xx) (= y yy)))
;                 (and (> indentation 0) indentation)
;                 operation-name)
;         (if (< time 30.)
;             (progn
;               (setq utime (- (%32-bit-difference utime otime) (%32-bit-difference otime xtime)))
;               (if (< utime 10000)
;                   (format stream "~D microseconds ~$ microseconds disk wait" utime dw)
;                 (format stream "~:[~,1,-6F~;~,3,-6F~] seconds ~$ seconds disk wait"
;                         (< time 10)
;                         utime
;                         (quotient dw 1.0E6))))
;           (time:print-interval-or-never time stream)
;           (princ " " stream)
;           (time:print-interval-or-never (round dw 1.0E6) stream)
;           (PRINC " disk wait." stream)))))))


;;;; who-calls

(export '(who-calls find-callers-of-symbols))

(DEFUN WHO-CALLS (SYMBOL-OR-SYMBOLS &OPTIONAL PKG (INHERITORS T) (INHERITED T))
  "Find all symbols in package PKG whose values, definitions or properties
 use SYMBOL-OR-SYMBOLS, which should be either a single symbol or a list of symbols.
PKG defaults to NIL, which means search all packages.
The packages which inherit from PKG are processed also, unless INHERITORS is NIL.
The packages PKG inherits from are processed also, unless INHERITED is NIL.
\(Other packages which merely inherit from the same ones are NOT processed.)
The symbols are printed and a list of them is returned."
  (LET ((RETURN-LIST ()))
    (FIND-CALLERS-OF-SYMBOLS SYMBOL-OR-SYMBOLS PKG
      (LAMBDA (CALLER CALLEE HOW)
        (FORMAT T "~&~S" CALLER)
        (FORMAT T (ECASE HOW
                    (:VARIABLE " uses ~S as a variable.")
                    (:FUNCTION " calls ~S as a function.")
                    (:MISC-FUNCTION " calls ~S via a \"misc\" instruction.")
                    (:CONSTANT " uses ~S as a constant.")
                    (:FLAVOR " uses ~S's flavor definition.")
                    (:UNBOUND-FUNCTION " calls ~S, an undefined function.")
                    (:MACRO ", when it was compiled, expanded the macro ~S.")
                    ((NIL) ", an interpreted function, uses ~S somehow."))
                CALLEE)
        (PUSHNEW CALLER RETURN-LIST :TEST #'EQ))
      INHERITORS INHERITED)
    RETURN-LIST))
(DEFF WHO-USES 'WHO-CALLS)                      ;old name

(DEFUN WHAT-FILES-CALL (SYMBOL-OR-SYMBOLS &OPTIONAL PKG (INHERITORS T) (INHERITED T))
  "Find all files in package PKG which use SYMBOL.
PKG defaults to NIL, which means search all packages.
The packages which inherit from PKG are processed also, unless INHERITORS is NIL.
The packages PKG inherits from are processed also, unless INHERITED is NIL.
\(Other packages which merely inherit from the same ones are NOT processed.)
The files are printed and a list of them is returned."
  (IF (ATOM SYMBOL-OR-SYMBOLS) (SETQ SYMBOL-OR-SYMBOLS (LIST SYMBOL-OR-SYMBOLS)))
  (LET ((L NIL))
    (FIND-CALLERS-OF-SYMBOLS SYMBOL-OR-SYMBOLS PKG
      (LAMBDA (CALLER IGNORE IGNORE)
        (AND (SETQ CALLER (GET-SOURCE-FILE-NAME CALLER 'DEFUN))
             (PUSHNEW CALLER L :TEST #'EQ)))
      INHERITORS INHERITED)
    L))

(DEFUN FIND-CALLERS-OF-SYMBOLS (SYMBOLS PKG FUNCTION
                                &OPTIONAL (INHERITORS T) (INHERITED T))
  "This is the main driving function for WHO-CALLS and friends.
Looks at all symbols in PKG and USErs (if INHERITORS is T)
  and the ones it USEs (if INHERITED is T).
If PKG is NIL, looks at all packages.
Looks at each symbol's function definition and if it
refers to SYMBOL calls FUNCTION with the function name, the symbol used,
and the type of use (:VARIABLE, :FUNCTION, :MISC-FUNCTION, :CONSTANT, :UNBOUND-FUNCTION,
 :FLAVOR, :MACRO or NIL if used in an unknown way in an interpreted function.)
SYMBOLS may be a single symbol or a list of symbols.
The symbol :UNBOUND-FUNCTION is treated specially."
  ;; Sorting first, in order of function definitions, didn't help much when
  ;; tried in the previous generation of this function.
  (WHEN PKG (SETQ PKG (PKG-FIND-PACKAGE PKG)))
  (IF (ATOM SYMBOLS) (SETQ SYMBOLS (LIST SYMBOLS)))
  (ASSERT (LOOP FOR SYM IN SYMBOLS ALWAYS (SYMBOLP SYM))
          (SYMBOLS)
          "~A should be a symbol or a list of symbols" 'SYMBOLS)
;  (DOLIST (SYM SYMBOLS)
;    (SETQ SYMBOLS (ADD-SYMBOLS-OPTIMIZED-INTO SYM SYMBOLS)))
  (LET ((*SYMBOLS* SYMBOLS)
        (*FUNCTION* FUNCTION))
    (DECLARE (SPECIAL *SYMBOLS* *FUNCTION*))
    (COND (PKG
           (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX PKG INHERITED)
           (AND INHERITORS
                (DOLIST (P (PACKAGE-USED-BY-LIST PKG))
                  (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX P NIL))))
          (T (DOLIST (P *ALL-PACKAGES*)
               (MAPATOMS #'FIND-CALLERS-OF-SYMBOLS-AUX P NIL))))
    NIL))

;(DEFUN ADD-SYMBOLS-OPTIMIZED-INTO (SYM LIST)
;  (declare (ignore sym))
;>> bogoid
;  (DOLIST (SYM1 (GET SYM 'COMPILER::OPTIMIZED-INTO))
;    (UNLESS (MEMQ SYM1 LIST)
;      (SETQ LIST (ADD-SYMBOLS-OPTIMIZED-INTO SYM1 (CONS SYM1 LIST)))))
;  LIST)

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX (CALLER &AUX FL)
  ;; Ignore all symbols which are forwarded to others, to avoid duplication.
  (AND ( (%P-DATA-TYPE (LOCF (SYMBOL-FUNCTION CALLER))) DTP-ONE-Q-FORWARD)
       (FBOUNDP CALLER)
       (FIND-CALLERS-OF-SYMBOLS-AUX1 CALLER (SYMBOL-FUNCTION CALLER)))
  (UNLESS (= (%P-DATA-TYPE (LOCF (SYMBOL-PLIST CALLER))) DTP-ONE-Q-FORWARD)
    ;; Also look for properties
    (DO ((L (SYMBOL-PLIST CALLER) (CDDR L)))
        ((NULL L))
      (IF (TYPEP (CADR L) 'COMPILED-FUNCTION)
          (FIND-CALLERS-OF-SYMBOLS-AUX-FEF
            `(:PROPERTY ,CALLER ,(CAR L))
            (CADR L))))
    ;; Also look for flavor methods
    (AND (SETQ FL (GET CALLER 'SI:FLAVOR))
         (ARRAYP FL)                            ;Could be T
         (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
           (DOLIST (METH (CDDDR MTE))
             (IF (METH-DEFINEDP METH)
                 (FIND-CALLERS-OF-SYMBOLS-AUX1 (METH-FUNCTION-SPEC METH)
                                               (METH-DEFINITION METH))))))
    ;; Also look for initializations
    (IF (GET CALLER 'INITIALIZATION-LIST)
        ;; It is an initialization list.
        (DOLIST (INIT-LIST-ENTRY (SYMBOL-VALUE CALLER))
          (FIND-CALLERS-OF-SYMBOLS-AUX-LIST CALLER (INIT-FORM INIT-LIST-ENTRY))))))

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX1 (CALLER DEFN)
  ;; Don't be fooled by macros, interpreted or compiled.
  (IF (EQ (CAR-SAFE DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
  (TYPECASE DEFN
    (COMPILED-FUNCTION (FIND-CALLERS-OF-SYMBOLS-AUX-FEF CALLER DEFN))
    (CONS (FIND-CALLERS-OF-SYMBOLS-AUX-LAMBDA CALLER DEFN))
    (SELECT (FIND-CALLERS-OF-SYMBOLS-AUX-SELECT-METHOD CALLER (%MAKE-POINTER DTP-LIST DEFN))))
  ;; this function is traced, advised, etc.
  ;; then look through the actual definition.
  (IF (OR (CONSP DEFN) (TYPEP DEFN 'COMPILED-FUNCTION))
      (LET* ((DEBUG-INFO (DEBUGGING-INFO DEFN))
             (INNER (ASSQ 'SI::ENCAPSULATED-DEFINITION DEBUG-INFO)))
        (AND INNER (FIND-CALLERS-OF-SYMBOLS-AUX (CADR INNER))))))

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

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-FEF (CALLER DEFN &AUX TEM OFFSET SYM)
  (DECLARE (SPECIAL *SYMBOLS* *FUNCTION*))
  (DO ((I %FEF-HEADER-LENGTH (1+ I))
       (LIM (TRUNCATE (FEF-INITIAL-PC DEFN) 2)))
      (( I LIM) NIL)
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-ONE-Q-FORWARD)
           (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
                 SYM (%FIND-STRUCTURE-HEADER TEM)
                 OFFSET (%POINTER-DIFFERENCE TEM SYM))
           (COND ((NOT (SYMBOLP SYM)))
                 ((= OFFSET 2)                  ;Function cell reference
                  (IF (MEMQ SYM *SYMBOLS*)
                      (FUNCALL *FUNCTION* CALLER SYM :FUNCTION)
                      (AND (MEMQ ':UNBOUND-FUNCTION *SYMBOLS*)
                           (NOT (FBOUNDP SYM))
                           (FUNCALL *FUNCTION* CALLER SYM :UNBOUND-FUNCTION))))
                 (T                             ;Value reference presumably
                  (IF (MEMQ SYM *SYMBOLS*)
                      (FUNCALL *FUNCTION* CALLER SYM :VARIABLE)))))
          ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-SELF-REF-POINTER)
           (LET* ((FN (FEF-FLAVOR-NAME DEFN)))
             (IF FN
                 (MULTIPLE-VALUE-BIND (SYM USE)
                     (FLAVOR-DECODE-SELF-REF-POINTER FN (%P-LDB-OFFSET %%Q-POINTER DEFN I))
                   (IF (MEMQ SYM *SYMBOLS*)
                       (FUNCALL *FUNCTION* CALLER SYM (IF USE :FLAVOR :VARIABLE)))))))
          ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-INDEXED-FORWARD)
           (SETQ TEM (AREF SI:*INDEX-NAME-TABLE* (%P-LDB-OFFSET %%Q-POINTER DEFN I)))
           (SETQ SYM (CADR TEM))
           (SETQ TEM (CAR TEM))
           (COND ((EQ TEM :FUNCTION)
                  (IF (MEMQ SYM *SYMBOLS*)
                      (FUNCALL *FUNCTION* CALLER SYM :FUNCTION)
                    (AND (MEMQ ':UNBOUND-FUNCTION *SYMBOLS*)
                         (NOT (FBOUNDP SYM))
                         (FUNCALL *FUNCTION* CALLER SYM :UNBOUND-FUNCTION))))
                 ((EQ TEM :VALUE)
                  (IF (MEMQ SYM *SYMBOLS*)
                      (FUNCALL *FUNCTION* CALLER SYM :VARIABLE)))))
          ((SYMBOLP (SETQ SYM (%P-CONTENTS-OFFSET DEFN I)))
           (IF (MEMQ SYM *SYMBOLS*)
               (FUNCALL *FUNCTION* CALLER SYM :CONSTANT)))))
  ;; See if the fef uses the symbol as a macro.
  (LET ((DI (DEBUGGING-INFO DEFN)))
    (DOLIST (M (CADR (ASSQ ':MACROS-EXPANDED DI)))
      (if (consp m) (setq m (car m)))
      (IF (MEMQ M *SYMBOLS*)
          (FUNCALL *FUNCTION* CALLER M :MACRO))))
  ;; See if we have a function reference compiled into a misc instruction
  (DOLIST (SYM *SYMBOLS*)
    (IF (FEF-CALLS-MISC-FUNCTION DEFN SYM)
        (FUNCALL *FUNCTION* CALLER SYM :MISC-FUNCTION)))
  (AND (FEF-DEBUGGING-INFO-PRESENT-P DEFN)
       (SETQ TEM (CDR (ASSQ ':INTERNAL-FEF-OFFSETS (FEF-DEBUGGING-INFO DEFN))))
       (LOOP FOR OFFSET IN TEM
             FOR I FROM 0
          DO (FIND-CALLERS-OF-SYMBOLS-AUX-FEF `(:INTERNAL ,CALLER ,I)
                                              (%P-CONTENTS-OFFSET DEFN OFFSET)))))

;;; See if this FEF uses a certain MISC instruction
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
(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LIST (CALLER DEFN)
  (LET ((*SUPPRESS* ()))
    (DECLARE (SPECIAL *SUPPRESS*))
    (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER DEFN)))

(DEFUN FIND-CALLERS-OF-SYMBOLS-AUX-LAMBDA (CALLER DEFN)
  (LET ((*SUPPRESS* ()))
    (DECLARE (SPECIAL *SUPPRESS*))
    (FIND-CALLERS-OF-SYMBOLS-AUX-LIST1 CALLER (LAMBDA-EXP-ARGS-AND-BODY DEFN))))

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

;;;; RMS' object-user analysis hack (are we masterscoping yet?)  See stuff in QFASL loader also.

(export '(analyze-all-files analyze-changed-files analyze-system
          find-users-of-objects find-files-using-objects))

(defvar *analyzed-files* nil)
(defvar *unanalyzed-files* nil)

;; used by file loader (sys; qfasl)
; If this is non-nil, typically takes about an extra 10% (extreme, rare, case about 30%)
;  time to load a file.
(defvar *analyze-files-when-loaded* nil
  "T means do SI::ANALYZE-FILE object-use analysis to a file's definitions
immediately after loading each file.")

; set by analyze-all-files.  Is that the right thing?
(defvar *analyze-files-before-cold* nil
  "T means do SI::ANALYZE-CHNAGED-FILES object-use analysis before disk-saving.")

#||

;; removed because the code is buggy and the functionality is more efficiently
;; hooked into FASLOAD in any case. -gjc 6-Feb-86 09:36:50

(add-initialization "Analyze changed files"
                    '(and *analyzed-files*
                          *analyze-files-before-cold*
                          (report-elapsed-time t 2 "object-use analysis of changed files"
                            #'analyze-changed-files))
                    ':before-cold)


(delete-initialization "Analyze changed files" '(:before-cold))

||#


(defparameter *analyze-dont-record-references-to* '(t nil lambda quote)
  "Don't record the fact that any of these elements is referenced when doing object-use analysis")

; icky kludgey magic list
(defparameter *dont-analyze-symbol-values*
              '(;; contains all the random evaluations which the very cold-load was too
                ;; stupid to perform itself. (such as defvar initializations, etc)
                original-lisp-crash-list
                ;; list of symbols to be yanked out of the cold-load's si package
                ;; and stuck into sys by package initialization
                initial-system-symbols
                ;; ditto, but for global symbols
                initial-global-symbols
                ;; compiled with different version of macro from one loaded.
                ;; there may be some of these in the cold-load
                initial-lisp-symbols
                ;;
                macro-mismatch-functions
                ;;
                si::cold-load-function-property-lists
                ;; what it sounds like
                *all-flavor-names*
                ;; ditto
                *all-resources*
                ;; all zmacs commands end up in this
                zwei::*command-alist*
                ;; tv:defwindow-resource
                tv::window-resource-names
                ;; more as I find think of them
                )
  "Don't analyze the values of these symbols.")

(defvar *analyze-area*)

(defvar *analyze-filename*)
(defvar *analyze-table*)
(defvar *analyze-definitions*)
(defvar *analyze-object-name*)
(defvar *analyze-object-type*)

(defun analyze-all-files ()
  (report-elapsed-time nil 0 "object-use analysis of all loaded files"
    (lambda ()
      (setq *analyzed-files* nil)
      (maphash (lambda (ignore pathname)
                 (and (get pathname ':definitions)
                      (not (memq pathname *analyzed-files*))
                      (not (get pathname ':patch-file))
                      (analyze-file pathname)))
               fs:*pathname-hash-table*)
      ;; Put them in the same order that LINEARIZE-PATHNAME-PLISTS copies their plists.
      (setq *analyzed-files* (nreverse *analyzed-files*))
      ;;>>??  Will users who load systems get pissed by this?
      ;;>>  If anybody objects, this should probably be diked out. -- Mly 1-Oct-85
      (setq *analyze-files-before-cold* t)
      ))
  nil)

(defun analyze-system (system)
  (report-elapsed-time nil 0 (format nil "object-use analysis of files of system ~A" system)
    (lambda () (mapc #'analyze-file (system-source-files system))))
  nil)

(defun sort-analyzed-files ()
  ;;>> not exactly sure what good this is
  (setq *analyzed-files* (sort *analyzed-files*
                               (lambda (f1 f2)
                                 (%pointer-lessp (get f1 ':foreign-objects-referenced)
                                                 (get f2 ':foreign-objects-referenced))))))

#||

(add-initialization "Sort Analyzed Files" '(sort-analyzed-files) '(:before-cold))

(delete-initialization "Sort Analyzed Files" '(:before-cold))

||#


(defun analyze-changed-files ()
  "Reanalyze all files that have been changed."
  (report-elapsed-time nil 0 "object-use analysis of changed files"
    (lambda ()
      (dolist (pathname *unanalyzed-files*)
        (unless (or (get pathname ':patch-file)
                    ;; some stupid bug somewhere in the cold-load builder
                    (eq pathname 'si::mini-plist-receiver))
          (analyze-file pathname)))))
  nil)

(defun analyze-file (pathname &optional (whole-file t))
  "Look through all the definitions in this file
and record what objects not defined in the file are referenced by the file.
The record made is the :FOREIGN-OBJECTS-REFERENCED property of the generic pathname;
 its value is an ART-Q-LIST array whose contents are a list of the objects.
The generic pathname is put on *ANALYZED-FILES* if not there already."
  (check-type pathname pathname)
  (unless (variable-boundp *analyze-area*)
    (setq *analyze-area* (make-area :name 'analyze-area
                                    :region-size #o100000
                                    :swap-recommendations 8.)))
  (let* ((generic-pathname (send pathname :generic-pathname))
         (table (or (and (not whole-file)
                         (get generic-pathname ':foreign-objects-referenced))
                    (zl:make-array #o100 :type 'art-q-list
                                   :area *analyze-area* :fill-pointer 0))))
    (analyze-file-definitions generic-pathname #'analyze-record-used-object table nil)
    (unless (zerop (fill-pointer table))
      (pushnew generic-pathname *analyzed-files* :test #'eq)
      (setf (get generic-pathname ':foreign-objects-referenced) table)))
  (without-interrupts
    (setq *unanalyzed-files* (delq pathname *unanalyzed-files*)))
  pathname)

; Return a list of all the files that contain references to any of the objects specified.
; Also add on any files that define those objects,
; since the analysis tables for those files will not contain those objects
; (since the tables only mention objects refered to but not defined).
(defun find-files-using-objects (objects &aux using-files)
  (check-type objects list)
;  (dolist (obj objects)
;    (setq objects (add-symbols-optimized-into obj objects)))
  (dolist (generic *analyzed-files*)
    (let ((table (get generic ':foreign-objects-referenced)))
      (if (mem (lambda (objects obj) (memq obj objects))
               objects (g-l-p table))
          (push generic using-files))))
  (dolist (obj objects)
    (dolist (def-type-files (get-all-source-file-names obj))
      (dolist (file (cdr def-type-files))
        (pushnew file using-files :test #'eq))))
  using-files)

(defun find-users-of-objects (objects &optional type)
  "Given a list of objects, return an alist of all the objects that use them
\(but only includes using objects that are in files that have been analyzed).
The format of the value is:
\((used-object-1 (how-used-1a using-object-1a defn-type-1a) ...)
\ (used-object-2 ...)
\ ...)
TYPE should be one of :FUNCTION, :VALUE, :CONSTANT or :FLAVOR to only find usages
of that particular type, or NIL, meaning to find usages of any type."
  (check-type objects list)
  (check-type type (or null (member :function :value :constant :flavor)))
;  (dolist (obj objects)
;    (setq objects (add-symbols-optimized-into obj objects)))
  (let ((table (mapcar #'ncons objects)))
    ;; First we find which files might refer to these objects.
    (dolist (generic-pathname (find-files-using-objects objects))
      ;; Now analyze those files, but record callers of these objects.
      (let ((*analyze-filename* (send generic-pathname :new-pathname
                                                       :type :lisp :version :newest)))
        (analyze-file-definitions generic-pathname
                                  (if (eq type 'nil)
                                      #'analyze-record-user-of-object
                                      #'(lambda (x y)
                                          (if (or (eq type y) (eq y 'nil))
                                              (analyze-record-user-of-object x y))))
                                  table nil)))
    table))

; Look at all the definitions in the specified file.
; For each time a definition references some object,
; call the RECORD-FUNCTION.  The arguments will be the object referenced
; and the type of reference (:FUNCTION, :VALUE, :CONSTANT, :FLAVOR, or NIL if unknown).
(defun analyze-file-definitions (generic-pathname record-function *analyze-table* pkg)
  (let* ((all-packages-definitions (get generic-pathname ':definitions))
         (*analyze-definitions* (or (cdr (assoc-equal pkg all-packages-definitions))
                                    (cdar all-packages-definitions))))
    ;; Record any random forms to be evaluated, present in this file.
    ;; List them as object nil, definition type nil.
    (analyze-definition nil nil (rem-if (lambda (elt)
                                          (eq (car-safe elt) 'fasl-record-file-macros-expanded))
                                        (get generic-pathname ':random-forms))
                        record-function)
    (dolist (def *analyze-definitions*)
      (analyze-object (car def) record-function))))

; These are two RECORD-FUNCTIONs:
; one to record all the objects that are used but not defined in this file,
; and one to record all the objects that use certain specified ones.
(defun analyze-record-used-object (used-object ignore)
  (if (symbolp used-object)
      (or (memq used-object *analyze-dont-record-references-to*)
          (assq used-object *analyze-definitions*)
          (memq used-object (g-l-p *analyze-table*))
          (vector-push-extend used-object *analyze-table*))
    (or (member-equal used-object *analyze-dont-record-references-to*)
        (assoc-equal used-object *analyze-definitions*)
        (member-equal used-object (g-l-p *analyze-table*))
        (vector-push-extend used-object *analyze-table*))))

(defun analyze-record-user-of-object (used-object how-used)
  (let ((slot (assoc-equal used-object *analyze-table*)))
    (if slot
        (let ((use (list how-used
                         (or *analyze-object-name* *analyze-filename*)
                         *analyze-object-type*)))
          (pushnew use (cdr slot) :test #'equal)))))

(defun analyze-object (object-name record-function)
  (or (not (condition-case () (fdefinedp object-name) (error nil)))
      ;; Don't count symbols forwarded to others.
      (and (symbolp object-name)
           (neq (locf (symbol-function object-name))
                (follow-cell-forwarding (locf (symbol-function object-name)) t)))
      (analyze-definition object-name :function
                          (fdefinition (unencapsulate-function-spec object-name))
                          record-function))
  (and (symbolp object-name)
       (boundp object-name)
       (not (memq object-name *dont-analyze-symbol-values*))
       ;; Don't count symbols forwarded to others.
       (eq (locf (symbol-value object-name))
           (follow-cell-forwarding (locf (symbol-value object-name)) t))
       (analyze-definition object-name :value
                           (symbol-value object-name)
                           record-function))
  (and (symbolp object-name)
       (do ((plist (plist object-name) (cddr plist)))
           ((null plist))
         (analyze-definition object-name :property-name (car plist) record-function)
         (or (typep (cadr plist) 'compiled-function)
             (analyze-definition object-name `(:property ,(car plist))
                                 (cadr plist)
                                 record-function)))))

; Record about one definition of one object.
; The first arg is the name of the object.
; The second is the type of definition (:FUNCTION, :VALUE, :PROPERTY, etc.)
; The third is the value of that definition.
(defun analyze-definition (*analyze-object-name* *analyze-object-type*
                           definition record-function)
  (and (eq *analyze-object-type* :function)
       (eq (car-safe definition) 'macro)
       (pop definition))
  (typecase definition
    (compiled-function (analyze-compiled-function definition record-function))
    (si:flavor (analyze-flavor definition record-function))
    (symbol (analyze-list definition record-function))
    (list (funcall (if (eq *analyze-object-type* ':function)
                       #'analyze-lambda
                       #'analyze-list)
                   definition record-function))
    (select-method (analyze-list (%make-pointer dtp-list definition) record-function))
    (closure (analyze-definition (closure-function definition) :function
                                 (closure-function definition) record-function))))

(defun analyze-flavor (definition record-function &aux (*analyze-object-type* :flavor))
  (analyze-list (flavor-local-instance-variables definition) record-function)
  (analyze-list (flavor-init-keywords definition) record-function)
  (analyze-list (flavor-inittable-instance-variables definition) record-function)
  (do ((plist (flavor-plist definition) (cddr plist))) ((null plist))
    (or (memq (car plist) '(additional-instance-variables
                            compile-flavor-methods
                            unmapped-instance-variables
                            mapped-component-flavors
                            all-instance-variables-special
                            instance-variable-initializations
                            all-inittable-instance-variables
                            remaining-default-plist
                            remaining-init-keywords))
        (analyze-definition *analyze-object-name* :flavor
                            (cadr plist) record-function))
    (analyze-list (car plist) record-function)))

(defun analyze-compiled-function (definition record-function
                                  &aux tem sym offset
                                  (debug-info (debugging-info definition)))
  (do ((i %fef-header-length (1+ i))
       (lim (%structure-boxed-size definition)))
      (( i lim) nil)
    (cond ((= (%p-ldb-offset %%q-data-type definition i) dtp-one-q-forward)
           (setq tem (%p-contents-as-locative-offset definition i)
                 sym (%find-structure-header tem)
                 offset (%pointer-difference tem sym))
           (if (consp sym) (setq sym (car sym)))
           (funcall record-function
                    sym
                    (ecase offset
                      (2 :function)
                      (1 :value))))
          ((= (%p-ldb-offset %%q-data-type definition i) dtp-self-ref-pointer)
           (let* ((fn (fef-flavor-name definition)))
             (if fn
                 (multiple-value-bind (symbol use)
                     (flavor-decode-self-ref-pointer fn (%p-ldb-offset %%q-pointer definition i))
                   (funcall record-function symbol (if use :flavor :value))))))
          ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFINITION I) DTP-INDEXED-FORWARD)
           (SETQ TEM (AREF SI:*INDEX-NAME-TABLE* (%P-LDB-OFFSET %%Q-POINTER DEFINITION I)))
           (FUNCALL RECORD-FUNCTION (CADR TEM) (CAR TEM)))
          ((eq (%p-contents-offset definition i) debug-info))
          ((symbolp (%p-contents-offset definition i))
           (funcall record-function (%p-contents-offset definition i) :constant))
          ((consp (%p-contents-offset definition i))
           (analyze-list (%p-contents-offset definition i) record-function))))
  ;; Now we should see if there is a reference compiled into a misc instruction,
  ;; except that we don't know which ones are worth checking for
  ;; and don't want to take the time to check them all.
  ;; So we just decide that misc functions are too widely used to be worth it.

  ;; Now record any macros that were expanded in compiling this function.
  (analyze-list (assq ':macros-expanded debug-info) record-function)
  ;; Now analyze any internal functions that are part of this one.
  (when (setq tem (cdr (assq ':internal-fef-offsets debug-info)))
    (loop for offset in tem
          for i from 0
       do (analyze-compiled-function (%p-contents-offset definition offset)
                                     record-function))))

(defun analyze-lambda (definition record-function)
  (analyze-list (lambda-exp-args-and-body definition)
                record-function))

;;; For now, give up on circular lists.
(defun analyze-list (list record-function)
  (when (list-length list)
    (do ((rest list (cdr rest)))
        ((atom rest)
         (if (symbolp rest)
             (funcall record-function rest nil)))
      (let ((element (car rest)))
        (cond ((symbolp element)
               (funcall record-function element nil))
              ((list-match-p element `(function ,ignore))
               (funcall record-function (cadr element) :function))
              ((consp element)
               (analyze-list element record-function)))))))
