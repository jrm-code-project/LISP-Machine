#|| -*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Fonts:(CPTFONTB) -*-

code porting aids, 6/21/85 12:58:25 -George Carrette.
For porting between major releases of systems. These tools are intended to be
run first in the environment of the working system in the old release, and
then again in the new release.

first find:
Symbols which are defined as special variables or macros or functions.
(1) using the generic pathname provided data.
(2) check for extra symbols defined via non-toplevel forms.


The package analysis functions assume a simple package under global.

||#

(special *stream*)

(defstruct (symbol-info (:conc-name symbol-info.) :list)
  pname
  package
  special
  function)

(defun dump-package-database (a-package filename)
  "Use this on packages which are defined and used by the system being ported.
Then run ANALYZE-PACKAGE-DATABASE in the new major release"
  (let ((pkg (pkg-find-package a-package)))
    (with-open-file (*stream* filename ':out)
      (format *stream* ";;; -*-mode:lisp;package:user; readtable: t -*-~%")
      (format *stream* ";;; database for package ~S dumped by ~S~%"
              (pkg-name pkg)
              si:user-id)
      (format *stream* "(setq *package-database* '(~%")
      (mapatoms #'(lambda (sym)
                    (format *stream* " (~S ~S ~S ~A)~%"
                            (get-pname sym)
                            (si:pkg-name (symbol-package sym))
                            (and (get sym 'special) t)
                            (cond ((functionp sym nil)
                                   ":LAMBDA")
                                  ((functionp sym t)
                                   ":MACRO"))))
                PKG NIL)
      (format *stream* "))~2%"))))


(defvar user:*package-database* nil)

(defun sortcar-user-package-database ()
  (setq user:*package-database* (sortcar user:*package-database*
                                         #'string-lessp)))

(defun analyze-package-database (database-filename &optional &key notes-filename (verbose t) activate dont-load)
  "Reports on all new symbol conflicts. If Activate is T then
create package with the proper shadowing and setup special declarations and
macro gaurdians."
  (let (astream)
    (unless dont-load
      (load database-filename)
      (sortcar-user-package-database))
    (with-open-file (fstream (or notes-filename (send (fs:parse-pathname
                                                        database-filename)
                                                      ':new-type "NOTES"))
                             ':out)
      (setq astream fstream)
      (if verbose (setq astream (make-broadcast-stream astream standard-output)))
      (format astream ";;;Package ~S has ~D symbols~%"
              (symbol-info.package (car user:*package-database*))
              (length user:*package-database*))
      (dolist (s user:*package-database*)
        (if (intern-soft (symbol-info.pname s) "GLOBAL")
            (format astream "(SHADOW ~S ~S)~%"
                    (symbol-info.pname s)
                    (symbol-info.package s))))
      (dolist (s user:*package-database*)
        (if (symbol-info.special s)
            (format astream "(SPECIAL |~A|)~%" (symbol-info.pname s))))
      (dolist (s user:*package-database*)
        (if (eq ':macro (symbol-info.function s))
            (format astream "(FSET '|~A| '(MACRO . AS-YET-UNDEFINED-MACRO))~%"
                    (symbol-info.pname s))))))
  (when activate
    (activate-current-package-database)))


(defun activate-current-package-database ()
  (let ((pkg (or (find-package (symbol-info.package (car user:*package-database*)))
                 (make-package (symbol-info.package (car user:*package-database*))
                               ':size (length user:*package-database*)))))
    (dolist (s user:*package-database*)
      (if (intern-soft (symbol-info.pname s) "GLOBAL")
          (shadow (symbol-info.pname s) pkg))
      (let ((sym (intern (symbol-info.pname s) pkg)))
        (if (symbol-info.special s) (putprop sym t 'special))
        (if (eq ':macro (symbol-info.function s))
            (fset sym '(macro . as-yet-undefined-macro)))))))


(defun as-yet-undefined-macro (form)
  ;; this way the compiler will punt with this form, and not
  ;; generate spurious other messages due to bad semantic analysis.
  (ferror nil "Trying to expand an as-yet-undefined macro named ~S" (car form)))


(defun analyze-system-dependancies (name &optional &key notes-filename (verbose t))
  (let ((sys (si:find-system-named name nil t)))
    ;; for now just look for macros expanded and not go into the toplevel eval forms
    ;; for order of loading dependency.
    ;; order of loading is almost always in good shape in a system definition.
    (let ((gps (mapcar #'(lambda (x) (send x ':generic-pathname))
                       (si:system-source-files sys))))
      (cond (notes-filename
             (with-open-file (stream notes-filename ':out)
               (analyze-file-dependancies gps (if verbose
                                                  (make-broadcast-stream stream standard-output)
                                                stream))))
            (verbose
             (analyze-file-dependancies gps standard-output))
            ('else
             (analyze-file-dependancies gps nil))))))

(defun analyze-file-dependancies (files report &aux result)
  ;; this assumes macros defined in only one file,
  ;; the from-source oriented tool for porting crufty old franzlisp and maclisp
  ;; programs would need to deal with the multiple defintion problem
  (dolist (f files)
    (let (depends-alist)
      (dolist (m (get f ':macros-expanded))
        (or (atom m) (setq m (car m)))
        (let ((depend (memq (si:get-source-file-name m) files)))
          (when depend
            (let ((cell (assq (car depend) depends-alist)))
              (cond ((null cell)
                     (push (list* (car depend) m nil) depends-alist))
                    ('else
                     (push m (cdr cell))))))))
      (when depends-alist
        (push f depends-alist)
        (push depends-alist result)
        (when report
          (format report "~&(~S ;; depends on:~%~{ ~S~^~%~})~%"
                  (car depends-alist) (cdr depends-alist)))))))



(defun rid-file-of-font-shifts (filename)
  (with-open-file (in filename)
    (with-open-file (out (send (send in ':truename) ':new-version ':newest) ':out)
      (do ((c))
          ((null (setq c (send in ':tyi))))
        (cond ((not (= c #\epsilon))
               (send out ':tyo c))
              ((memq (setq c (send in ':tyi))
                     '(#\* #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #o265)))
              ('else
               (send out ':tyo #\epsilon)
               (send out ':tyo c)))))))


(defun expunge-filesystem ()
  (mapcar #'expunge-directory-and-recurse (mapcar 'car (fs:all-directories))))


(defun subdirectories (p)
  (mapcar 'compose-subdirectory
          (mapcar 'car
                  (subset #'(lambda (pl) (get pl ':directory))
                          (fs:directory-list (send p ':new-pathname
                                                   :name :wild
                                                   :version :newest
                                                   :type :wild))))))
(defun expunge-directory-and-recurse (dir)
  (let ((p (send dir ':new-pathname ':version ':wild ':name ':wild ':type ':wild)))
    (format t "~&Expunging ~S" p)
    (format t " ~D blocks saved." (fs:expunge-directory p))
    (mapcar 'expunge-directory-and-recurse (subdirectories dir))))


(defun directory-component-as-list (pathname)
  (if (atom (send pathname ':directory))
      (list (send pathname ':directory))
    (send pathname ':directory)))

(defun compose-subdirectory (pathname)
  (send pathname
        ':new-directory
        (append (directory-component-as-list pathname)
                (list (send pathname ':name)))))



(defun delete-all-in-directory-and-recurse (dir)
  (let ((p (send (fs:parse-pathname dir)
                 ':new-pathname ':version ':wild ':name ':wild ':type ':wild)))
    (format t "~&Deleting ~S" p)
    (deletef p)
    (format t "~&Expunging ~S" p)
    (format t " ~D blocks saved." (fs:expunge-directory p))
    (mapcar 'delete-all-in-directory-and-recurse (subdirectories p))))


(defun copy-source-hi (from to)
  (let ((to-host (send (fs:parse-pathname to) ':host))
        (to-root (directory-component-as-list (fs:parse-pathname to))))
    (do-copy-source-hi (all-source-files-in (fs:parse-pathname from))
                       to-host
                       to-root)))

(defun do-copy-source-hi (directory-list to-host to-root)
  (dolist (f directory-list)
    (FS:FS-COPY-FILE (CAR F)
                  (send (car f)
                        :new-pathname :host to-host
                        :directory (append to-root
                                           (nthcdr (length to-root)
                                                   (directory-component-as-list (car f)))))
                  :DIRECTORY-LIST (CDR F))))




(defvar *copy-source-file-types* '("LISP" "TEXT" "DOC" "AST" "DATA"))

(defun all-source-files-in (pathname)
  (do ((l (cdr (fs:directory-list (send pathname ':new-pathname :name :wild
                                        :version :newest
                                        :type :wild)))
          (cdr l))
       (sources)
       (directories))
      ((null l)
       (nreconc sources (mapcan #'all-source-files-in (nreverse directories))))
    (cond ((get (car l) ':directory)
           (push (compose-subdirectory (caar l)) directories))
          ((mem #'string-equal (send (caar l) :type) *copy-source-file-types*)
           (push (car l) sources)))))




(defun find-unexpanded-substs (pkg)
  (LET (LOSERS
        (SYMBOLS 0)
        (ITEMS 0)
        (FUNS 0))
    (MAPATOMS #'(LAMBDA (SYM)
                  (INCF SYMBOLS)
                  (if (fboundp sym) (incf funs))
                  (MAP-FEF-FUNS-OF-SYMBOL
                    #'(LAMBDA (POTENTIAL-LOSER POTENTIAL-LOSSAGE)
                        (INCF ITEMS)
                        (LET ((F (AND (FBOUNDP POTENTIAL-LOSSAGE)
                                      (FSYMEVAL POTENTIAL-LOSSAGE))))
                          (IF (NULL F)
                              (FORMAT T "~&~S CALLS UNDEFIND FUNCTION ~S~%"
                                      POTENTIAL-LOSER POTENTIAL-LOSSAGE))
                          (COND ((AND (NOT (ATOM F))
                                      (EQ (CAR F) 'SUBST))
                                 (LET ((CELL (ASSQ POTENTIAL-LOSER LOSERS)))
                                   (WHEN (NOT CELL)
                                     (SETQ CELL (NCONS POTENTIAL-LOSER))
                                     (PUSH CELL LOSERS))
                                   (WHEN (NOT (MEMQ POTENTIAL-LOSSAGE
                                                    CELL))
                                     (PUSH POTENTIAL-LOSSAGE (CDR CELL))
                                     (FORMAT T "~&~S CALLS UNEXPANDED SUBST ~S~%"
                                             POTENTIAL-LOSER POTENTIAL-LOSSAGE)))))))
                    SYM))
              PKG
              NIL)
    (FORMAT T "~&~D Symbols observed, ~D toplevel defuns, ~D calls processed~%"
            symbols funs items)
    LOSERS))


(DEFUN PRINT-FEF-FUNS-OF-SYMBOL (OBJ)
  (MAP-FEF-FUNS-OF-SYMBOL #'(LAMBDA (X Y) (FORMAT T "~S CALLS ~S~%" X Y))
                          OBJ))

(DEFUN MAP-FEF-FUNS-OF-SYMBOL (F OBJ &AUX FL)
  ;; Ignore all symbols which are forwarded to others, to avoid duplication.
  (AND ( (%P-LDB-OFFSET %%Q-DATA-TYPE OBJ 2) DTP-ONE-Q-FORWARD)
       (FBOUNDP OBJ)
       (MAP-FEF-FUNS-2 F OBJ (FSYMEVAL OBJ)))
  (COND (( (%P-LDB-OFFSET %%Q-DATA-TYPE OBJ 3) DTP-ONE-Q-FORWARD)
         ;; Also look for properties
         (DO ((L (PLIST OBJ) (CDDR L)))
             ((NULL L))
           (COND ((= (%DATA-TYPE (CADR L)) DTP-FEF-POINTER)
                  (MAP-FEF-FUNS-FEF F
                                    (LIST :PROPERTY OBJ (CAR L))
                                    (CADR L)))))
         ;; Also look for flavor methods
         (AND (SETQ FL (GET OBJ 'FLAVOR))
              (ARRAYP FL)               ;Could be T
              (DOLIST (MTE (FLAVOR-METHOD-TABLE FL))
                (DOLIST (METH (CDDDR MTE))
                  (IF (METH-DEFINEDP METH)
                      (MAP-FEF-FUNS-2 F
                                      (METH-FUNCTION-SPEC METH)
                                      (METH-DEFINITION METH)))))))))

(DEFUN MAP-FEF-FUNS-2 (F CALLER DEFN)
  ;; Don't be fooled by macros, interpreted or compiled.
  (IF (EQ (CAR-SAFE DEFN) 'MACRO) (SETQ DEFN (CDR DEFN)))
  (TYPECASE DEFN
    (COMPILED-FUNCTION (MAP-FEF-FUNS-FEF F CALLER DEFN))))

(DEFUN MAP-FEF-FUNS-FEF (F CALLER DEFN &AUX TEM OFFSET SYM)
  (DO ((I %FEF-HEADER-LENGTH (1+ I))
       (LIM (TRUNCATE (FEF-INITIAL-PC DEFN) 2)))
      (( I LIM) NIL)
    (COND ((= (%P-LDB-OFFSET %%Q-DATA-TYPE DEFN I) DTP-EXTERNAL-VALUE-CELL-POINTER)
           (SETQ TEM (%P-CONTENTS-AS-LOCATIVE-OFFSET DEFN I)
                 SYM (%FIND-STRUCTURE-HEADER TEM)
                 OFFSET (%POINTER-DIFFERENCE TEM SYM))
           (COND ((NOT (SYMBOLP SYM)))
                 ((= OFFSET 2)                  ;Function cell reference
                  (FUNCALL F CALLER SYM))))))
  (AND (LDB-TEST %%FEFHI-MS-DEBUG-INFO-PRESENT
                 (%P-CONTENTS-OFFSET DEFN %FEFHI-MISC))
       (SETQ TEM (CDR (ASSQ :INTERNAL-FEF-OFFSETS
                            (%P-CONTENTS-OFFSET DEFN (1- (%P-LDB %%FEFH-PC-IN-WORDS DEFN))))))
       (LOOP FOR OFFSET IN TEM
             FOR I FROM 0
             DO (MAP-FEF-FUNS-FEF F `(:INTERNAL ,CALLER ,I)
                                  (%P-CONTENTS-OFFSET DEFN OFFSET)))))



(defun system-definition-summary (sys)
  (let ((sum 0)
        (n))
    (dolist (x (mapcar #'(lambda (x) (send x ':generic-pathname))
                       (si:system-source-files sys)))
      (setq n (apply '+ (mapcar #'length
                                (mapcar #'(lambda (z)
                                            (subset #'(lambda (x)
                                                        (eq (cdr x) 'defun))
                                                    z))
                                        (mapcar #'cdr (get x :definitions))))))
      (format t "~&~3D defuns in ~S~%" n x)
      (incf sum n))
    sum))
