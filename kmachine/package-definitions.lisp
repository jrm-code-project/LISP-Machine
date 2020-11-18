;;; -*- Mode:LISP; Package:USER; Readtable:ZL; Base:10. -*-

;;Symbols to be investigated: multiple-value-list pairlist rplaca values-list and find-package
;;multiple-value-list has conflicting definitions in lisp-internals.lisp (a macro) and lambda version.
;; k-sys:k;lambda-list.lisp uses both versions one for k and one for lambda and has no way to
;;  conditionalize it easily other than by hand.

;;____________________________________________________________________________________________________________________

;; Currently these macros agree with this file (see exception below) but are not used by it.
;;  This should be changed.  For now the macros are only used by warm-boot.

(defmacro pkg-vinc-imports () "A list of package names which have vinc-imports."
  (quote
    '("TIMERS" "MAP" "GC-RAM" "DATATYPE-RAM" "PAGING-DEVICES"
      "VIRTUAL-MEMORY" "PHYSICAL-CLUSTER-DATA" "QUANTUM-MAP" "MEMORY-MANAGEMENT" "REGION-BITS" "MAP-FAULT"
      "GC-FAULT" "REGION-DATA" "AREA-DATA" "MEMORY-MANAGEMENT-INTERFACE" "BOOT" "TRANSPORTER-RAM" "CONS"
      "NEW-MATH" "ARRAY" "TRAP" "SYMBOL" "LISP-INTERNALS" "NUBUS-STUFF" "K2")))
   ;; "kbug-streams" "k-debug" not on K.  This violates the complete mapping between here and Warm-Boot --wkf


(defmacro pkg-prims-imports () "A list of package names which have prims-imports."
  (quote
    `("GLOBAL-REGISTERS" "VINCULUM" . ,(pkg-vinc-imports))));; "NLISP" not on K

(defmacro pkg-global-imports () "A list of package names which have global-imports and cli-imports."
  (quote
    `("PRIMITIVES" . ,(pkg-prims-imports))))

(defmacro pkg-cli-imports () '(pkg-global-imports))

(defmacro pkg-cons-imports () "A list of package names which have array, symbol, new-math and cons imports."
  (quote '("LISP-INTERNALS")))

(defmacro pkg-array-imports    () '(pkg-cons-imports))
(defmacro pkg-symbol-imports   () '(pkg-cons-imports))
(defmacro pkg-new-math-imports () '(pkg-cons-imports))

;;______________________________________________________________________________________________________________________

;;removed follwing from global-imports since also in prims (DEFSUBST BYTE DEFMACRO BYTE-SIZE BYTE-POSITION INCF DECF) wkf.
(defmacro global-imports ()
  (quote
    '(:import-from global "UNSPECIAL" "*" "+" "-" "<" "<=" "=" ">" ">=" "1+" "1-" "&AUX" "&REST" "&BODY"
                          "&ENVIRONMENT" "&WHOLE" "&OPTIONAL" "&KEY" "ASH" "AND" "APPEND" "BLOCK" "CASE" "CEILING"
                          "COMPILE" "COMPILER-LET" "COND" "DECLARE" "DEFCONSTANT" "DEFUN" "DEFVAR" "DEFPARAMETER"
                          "DO" "DO*" "DOTIMES" "EQ" "EQL" "ETYPECASE" "EVAL" "EVAL-WHEN" "EXPORT" "FERROR" "FLET"
                          "FLOOR" "FUNCALL" "FUNCTION" "GO" "IMPORT" "IN-PACKAGE" "LAMBDA" "LABELS" "LET"
                          "LET*" "LOAD" "LOGAND" "LOGANDC1" "LOGANDC2" "LOGEQV" "LOGIOR" "LOGNAND" "LOGNOR" "LOGNOT"
                          "LOGORC1" "LOGORC2" "LOGXOR" "LOOP" "MACROLET" "MAX" "MINUSP" "MOD" "MULTIPLE-VALUE-BIND"
                          "MULTIPLE-VALUE-SETQ" "NULL" "NOT" "OR" "PLUSP" "PROCLAIM" "PROG" "PROG1" "PROGN" "PSETQ"
                          "QUOTE" "REMAINDER" "REST" "RETURN-FROM" "SETQ" "T" "TAGBODY" "UNLESS" "VALUES" "WHEN"
                          "ZEROP" "CATCH" "THROW"
                          "NIL" "SETF" "DEFSETF" "DEFINE-SETF-METHOD" "IGNORE" "RETURN" "VALUES-LIST")))
;;This above list of global imports was created by finding all symbols in the system which were imported and then
;;taking those which had a home only in the global package (ignoring K-xxx packages) in a working system. wkf.

(defmacro cli-imports ()
  (quote
    '(:import-from cli "IF" "AREF")))
;;This above list of cli imports was created by finding all symbols in the system which were imported and then
;;taking those which had a home in the cli package in a working system. wkf.
;; removed from cli-imports: "LISTP", "CHARACTER", "REM", "GETHASH", "MACROEXPAND-1", "MACROEXPAND", "TIME",
;;                           "MAKE-ARRAY", "MEMBER", "SUBST", "MAP", "CLOSE", "REMOVE", "READ", "DELETE",
;;                           "DEFSTRUCT", "ASSOC", "NLISTP", "NINTERSECTION", "INTERSECTION", "FUNCTIONP",
;;                           "ATAN", "AR-1-FORCE", "AR-1", "MAKE-PACKAGE", "UNION", "SOME", "READ-FROM-STRING",
;;                           "RASSOC", "NUNION", "EVERY", "//"

(defmacro new-math-imports ()
  (quote
    '(:import-from new-math "TRUNCATE" "ROUND" "REM"))) ;;"NUMERATOR" "DENOMINATOR" "GCD" "LCM" "DECODE-FLOAT"
                                                        ;;"INTEGER-DECODE-FLOAT" "REALPART" "IMAGPART" "CONJGATE"

(defmacro vinc-imports ()
  (quote
    '(:import-from vinc "ATOM" "COMPLEXP" "ARRAYP" "COMPILED-FUNCTION-P" "CONSP" "INTEGERP" "RATIONALP" "FLOATP" "NUMBERP"
                        "LISTP" "SYMBOLP" "COMMONP" "CHARACTERP"

                        "DEFINE-CONTROL-REGISTER-MODIFIER"
                        "FLUSH-ICACHE"

                        "DISABLE-ICACHE-ENABLES"        "MODIFY-ICACHE-ENABLES"
                        "DISABLE-LEDS"                  "MODIFY-LEDS"
                        "DISABLE-LOWCORE-CACHE-ENABLE"  "MODIFY-LOWCORE-CACHE-ENABLE"
                        "DISABLE-MEMORY-CONTROL"        "MODIFY-MEMORY-CONTROL"
                        "DISABLE-PROCESSOR-CONTROL"     "MODIFY-PROCESSOR-CONTROL"

                        "DISABLE-ICACHE-TRAPS"          "RESTORE-ICACHE-TRAPS"
                        "DISABLE-1024-INTERRUPT"        "RESTORE-1024-INTERRUPT"
                        "DISABLE-16384-INTERRUPT"       "RESTORE-16384-INTERRUPT"
                        "DISABLE-DRAM-PARITY-TRAP"      "RESTORE-DRAM-PARITY-TRAP"
                        "DISABLE-SYNCHRONOUS-TRAPS"     "RESTORE-SYNCHRONOUS-TRAPS"
                        "DISABLE-DATATYPE-TRAPS"        "RESTORE-DATATYPE-TRAPS"
                        "DISABLE-OVERFLOW-TRAPS"        "RESTORE-OVERFLOW-TRAPS"
                        "DISABLE-ASYNCHRONOUS-TRAPS"    "RESTORE-ASYNCHRONOUS-TRAPS"
                        "DISABLE-SINGLE-STEP-TRAP"      "RESTORE-SINGLE-STEP-TRAP"
                        "DISABLE-HEAP-UNDERFLOW-TRAP"   "RESTORE-HEAP-UNDERFLOW-TRAP"
                        "DISABLE-FLOATING-POINT-TRAP"   "RESTORE-FLOATING-POINT-TRAP"
                        )))

(defmacro cons-imports ()
  (quote
    '(:import-from cons "RPLACA" "RPLACD" "CAAAAR" "CAAADR" "CAAAR" "CAADAR" "CAADDR" "CAADR" "CAAR" "CADAAR"
                        "CADADR" "CADAR" "CADDAR" "CADDDR" "CADDR" "CADR" "CAR" "CDAAAR" "CDAADR" "CDAAR" "CDADAR" "CDADDR"
                        "CDADR" "CDAR" "CDDAAR" "CDDADR" "CDDAR" "CDDDAR" "CDDDDR" "CDDDR" "CDDR" "CDR" "CONS" "ENDP")))

(defmacro array-imports ()
  (quote
    '(:import-from array  "ADJUSTABLE-ARRAY-P"       ;; "AREF" already in array and li by cli-imports. --wkf
                          "ARRAY-DIMENSION"
                          "ARRAY-DIMENSION-LIMIT"    "ARRAY-DIMENSIONS"      "ARRAY-ELEMENT-TYPE"
                          "ARRAY-HAS-FILL-POINTER-P" "ARRAY-IN-BOUNDS-P"     "ARRAY-RANK"
                          "ARRAY-RANK-LIMIT"         "ARRAY-ROW-MAJOR-INDEX" "ARRAY-TOTAL-SIZE"
                          "ARRAY-TOTAL-SIZE-LIMIT"   "ASET"                  "BIT-VECTOR-P"
                          "FILL-POINTER"             "LENGTH"                "MAKE-ARRAY"
                          "MAKE-STRING"              "SIMPLE-BIT-VECTOR-P"   "SIMPLE-STRING-P"
                          "SIMPLE-VECTOR-P"          "STRING="               "STRINGP"
                          "SVREF"                    "VECTOR-POP"            "VECTOR-PUSH"
                          ;; added VECTOR-PUSH-EXTEND
                          ;; (what a pain in the ass: if this import does not appear in LI:
                          ;; when the file calling VECTOR-PUSH-EXTEND is compiled
                          ;; the K will not ARRAY:VECTOR-PUSH-EXTEND when it tries to call it
                          ;; it will be looking for LI:VECTOR-PUSH-EXTEND
                          ;; lose lose lose
                          ;; --pfc
                          "VECTOR-PUSH-EXTEND"       "VECTORP"                  "VECTOR"
                          "NAMED-STRUCTURE-INVOKE")))

(defmacro symbol-imports ()
  (quote
    '(:import-from symbol "BOUNDP"         "FBOUNDP"        "FMAKUNBOUND"  "GET"
                          "GET-PROPERTIES" "GETF"           "MAKE-SYMBOL"  "MAKUNBOUND"
                          "REMF"           "REMPROP"        "SET"          "SYMBOL-FUNCTION"
                          "SYMBOL-NAME"    "SYMBOL-PACKAGE" "SYMBOL-PLIST" "SYMBOL-VALUE")))

(defmacro prims-imports ()
  (quote
    '(:import-from prims "DISPATCH" "BODY" "SUBST"
                         "DEFSUBST" "BYTE" "DEFMACRO" "BYTE-SIZE" "BYTE-POSITION" "INCF" "DECF"
                         "SELECT-PROCESSOR"   ;;wkf added 5/4/88.
                         ;;above line gleaned from imported symbols in old method which were in global and prims.
                         "VALUE"
                         "DEFINE-GLOBAL-FRAME" "DEFINE-GLOBAL-VARIABLE" "DEFINE-GLOBAL-CONSTANT")))

;;______________________________________________________________________________________________________________________

;;$$$NC should not have been created in Lambda system file; isn't anymore. <11Nov88 keith>

(defpackage nc
  :use '(lisp)
  (:import-from zetalisp "DEFSUBST")
  )

(defpackage k
  :use '()
  (:import-from lisp "OPEN" "RETURN" "OR" "AND")
  (:import-from global "IGNORE"))                 ;;added IGNORE wkf 5/4/88

(eval
  `(defpackage primitives
     :use '()
     (:nicknames prims)
     ;; $$$ Removed shadow since it was creating symbols like prims::|PRIMS::DEFAFUN| <17-Nov-88 wkf>
     ,(global-imports)
     ,(cli-imports)
     ))

(defpackage hardware
  :use '()
  (:nicknames hw)
  (:import-from primitives "BYTE")
  (:import-from global "DEFCONSTANT")
  ;; $$$ Removed shadow since it was creating symbols like hw::|:LDB| <17-Nov-88 wkf>
  )

(eval
  `(defpackage global-registers
     :use '(prims)
     (:nicknames GR)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage vinculum
     :use '(prims)
     (:nicknames vinc)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage timers
     :use '(vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))


(eval
  `(defpackage map
     :use '(vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage gc-ram
     :use '(vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage datatype-ram
     :use '(vinculum prims k)
     (:nicknames dt-ram)
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage paging-devices
     :use '(vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage virtual-memory
     :use '(vinculum prims)
     (:nicknames vmem)
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage physical-cluster-data
     :use '(virtual-memory vinculum prims)
     (:nicknames pcd)
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage quantum-map
     :use '(virtual-memory vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage memory-management
     :use '(vinculum prims)
     (:nicknames memlow)
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage region-bits
     :use '(memory-management virtual-memory vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage map-fault
     :use '(virtual-memory physical-cluster-data map vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage gc-fault
     :use '(map vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage region-data
     :use '(memory-management vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage area-data
     :use '(region-data memory-management vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage memory-management-interface
     :use '(vinculum prims)
     (:nicknames mem)
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage boot
     :use '(vinculum virtual-memory prims k)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage transporter-ram
     :use '(vinculum virtual-memory prims)
     (:nicknames t-ram)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage cons
     :use '(memory-management-interface vinculum prims k)
     (:shadow "CONS")
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage new-math
     :use '(vinculum prims k)
     (:nicknames nm)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage array
     :use '(vinculum prims k)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage trap
     :use '(vinculum prims k)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

;;; This package has to have access to a
;;; running lisp to build an environment
;;; for booting.
;(defpackage sim-debug
;  :use '(vinculum global)) efh says not used

;(defpackage kbug2-common                       ; definitions for KBUG2 and K2
;  :use '(vinculum prims k))

;;;; Sorta similar to the above.
;;;; Things depend upon kbug being common to both package environments, but
;;;; we can't USE both versions of KBUG2-common.
;(if (boundp 'si::*current-package-environment*)
;    (if (eq si::*current-package-environment*
;           (si::find-package-environment "DEBUGGER" #'identity
;                                         #'(lambda () (error "Package-environment not set up."))))

;       (defpackage kbug
;         :use '(lisp k lam kbug2-common)
;         (:import-from "ZETALISP" "DEFSUBST"))
;       (defpackage kbug
;         :use '(lisp k lam)
;         (:import-from "ZETALISP" "DEFSUBST")))
;    (defpackage kbug
;      :use '(lisp k lam)
;      (:import-from "ZETALISP" "DEFSUBST")))

;(defmacro hairy-defpackage (package-name k-use-list lambda-use-list &rest stuff)
;  `(eval-when (compile load)
;     (if (or (not (boundp 'si::*current-package-environment*))
;            (eq si::*current-package-environment*
;                (si::find-package-environment "COMPILER" #'identity
;                                              #'(lambda () (error "Package-environment not set up.")))))
;        (defpackage ,package-name
;          :use ,k-use-list
;          ,@stuff)
;       (if (eq si::*current-package-environment*
;              (si::find-package-environment "DEBUGGER" #'identity
;                                            #'(lambda () (error "Package-environment not set up."))))
;          (defpackage ,package-name
;            :use ,lambda-use-list
;            ,@stuff)
;        (ferror nil "Random package environment!")))))

(defpackage kbug
  :use '(lisp k lam)
  (:import-from zetalisp "DEFSUBST"))

(eval
  `(defpackage symbol
     :use '(cons vinculum prims)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage lisp-internals
     :use '(prims k)
     (:nicknames li)
     ,(prims-imports)
     ,(vinc-imports)
     ,(cons-imports)
     ,(array-imports)
     ,(symbol-imports)
     ,(new-math-imports)
     (:import-from k "SINGLE-FLOAT" "DOUBLE-FLOAT") ;;"IGNORE" removed by wkf.
     ,(global-imports)
     ,(cli-imports)
     (:import-from user "PARSE-LAMBDA-LIST" "PARSE-OPTIONAL-PARAMETER" "PARSE-AUX-PARAMETER")
     ;;wkf: should li:PARSE-LAMBDA-LIST be overwriting user:P-L-L???
     ))

(eval
  `(defpackage nlisp
     :use '(prims)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(defpackage setf
  :use '(lisp)
  (:import-from user "PARSE-LAMBDA-LIST")
  (:shadow "DEFINE-MODIFY-MACRO"                ; $$$ Removed  ":PUSH" ":POP" ":GETF" since bad symbols <17-Nov-88 wkf>
           "DEFINE-SETF-METHOD"
           "DEFSETF"
           "GET-SETF-METHOD"
           "GET-SETF-METHOD-MULTIPLE-VALUE"
           "SETF"))

(eval
  `(defpackage k-debug
     :use '(vinc prims k)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage nubus-stuff
     :use '(vinc prims k)
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(defpackage cold
  :use '(k lisp))

(eval
  `(defpackage kbug-streams
     :use '(vinculum prims k)                   ; kbug2-common
     ,(vinc-imports)
     ,(global-imports)
     ,(prims-imports)
     ,(cli-imports)
     ))

(eval
  `(defpackage k2                               ; KBUG2/K2 stuff
     :use '(kbug-streams vinculum prims k)      ; kbug2-common
     ,(vinc-imports)
     ,(prims-imports)
     ,(global-imports)
     ,(cli-imports)
     ))

(defpackage fasdump
  :use '(;kbug2-common
         lisp))

(defvar *k-packages*
        '(k-global (k primitives hardware global-registers vinculum timers map gc-ram datatype-ram
                      paging-devices virtual-memory physical-cluster-data quantum-map
                      memory-management region-bits map-fault gc-fault region-data area-data
                      memory-management-interface boot transporter-ram cons new-math array trap
                      ;kbug2-common
                      kbug lisp-internals setf k-debug nubus-stuff cold
                      kbug-streams k2 symbol fasdump
                      user))) ; compiler system-internals system

(defun place-corr-package-to-new-environment (prefix pkg-name parent-pkg)
;;;this function takes a package and copies it to new
;;;environment, the parent package's. The pkg has prefix added to old name
  (let ((old-package (find-package pkg-name))
        (new-package (if (find-package (intern (string-append prefix pkg-name)))
                         (find-package (intern (string-append prefix pkg-name)))
                       (make-package (intern (string-append prefix pkg-name))
                                     :use () :import '(global:DEFSETF)))))
                    ;;wkf: 5/5/88 added :import '(global:DEFSETF), arbitrary hack.
    (setf (get new-package ':root) parent-pkg)
    (setf (get new-package ':prefix) prefix)
    (unless (null old-package)
      (use-package
        (mapcar #'(lambda (pkg)
                    (if (find-package
                          (string-append prefix
                                         (package-name pkg)))
                        (string-append prefix
                                       (package-name pkg))
                      pkg))
                (package-use-list old-package))
                   new-package)
      (setf (si:pkg-nicknames new-package)
            (mapcar #'(lambda (nick)
                        (string-append prefix nick))
                    (package-nicknames old-package))))
    new-package))

(defun make-alist-of-package-names (pkg pre)
;;;takes a corresponding package and makes an alist out of the string for all possible names
;;;and the package
;;;     ((pkg-name-or-nickname . package) (pkg-name-or-nickname . package) ....)
  (mapcar #'(lambda (name)
              (cons (subseq name
                            (length (string pre)))
                    pkg))
          (cons (package-name pkg)
                (package-nicknames pkg))))

(defun make-refname-alist-of-packages (packages pre)
;;; takes all corresponding packages and creates refname-alist
  (loop for pkg in packages
        appending (make-alist-of-package-names pkg pre)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun def-corresponding-packages (prefix pkg-tree)
;;;this function takes a tree of package-names and creates new tree
;;;essentially used to copy local environment to remote.
  (let ((new-pkg (if (find-package (car pkg-tree))
                     (find-package (car pkg-tree))
                   (make-package (car pkg-tree) :use nil :import '(global:DEFSETF)))))
                    ;;wkf: 5/5/88 added :import '(global:DEFSETF), arbitrary hack.
    (setf (get new-pkg ':root) new-pkg)
    (setf (get new-pkg ':prefix) prefix)
    (let* ((all-corresponding-packages
             (def-corresponding-packages-help prefix (cadr pkg-tree) new-pkg (list new-pkg)))
           (refname-alist (make-refname-alist-of-packages all-corresponding-packages prefix)))
      (loop for pkg in all-corresponding-packages
            (setf (si:pkg-refname-alist pkg)
                  refname-alist))
      all-corresponding-packages)))

(defun def-corresponding-packages-help (prefix pkg-tree-children parent-pkg all-packages-so-far)
  (loop for child-pkg in pkg-tree-children
        do (if (listp child-pkg)
               (let ((new-pkg (place-corr-package-to-new-environment prefix
                                                               child-pkg
                                                               parent-pkg)))
                 (setq all-packages-so-far
                       (cons new-pkg (def-corresponding-packages-help prefix
                                                                      (cadr child-pkg)
                                                                       new-pkg
                                                                       all-packages-so-far))))
             (setq all-packages-so-far
                   (cons
                     (place-corr-package-to-new-environment prefix child-pkg parent-pkg)
                     all-packages-so-far))))
  all-packages-so-far)

(def-corresponding-packages "K-" *k-packages*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun survey-for-symbol (symbol)
  (let ((string (cond ((stringp symbol) symbol)
                      (t (symbol-name symbol))))
        (ans nil)  ;a list of lists, ea is (<symbol> <package names accessible from>)
        sym
        tem)
    (dolist (pkg si:*all-packages*)
      (cond ((setq sym (intern-soft string pkg))
             ;got something, what is it?
             (cond ((setq tem (assq sym ans))
                    (rplacd tem (nconc (cdr tem) (list (si:package-name pkg)))))
                   (t (push (list sym (si:package-name pkg)) ans))))))
    ans))

(defun survey-global-imports ()
  (let ((ans nil))
    (dolist (sym '(
                   ;found out the hard way these needed from global.
                   "NIL" "T" "&KEY" "&BODY" "&AUX" "&REST" "&OPTIONAL" "QUOTE" "CATCH" "THROW"
                   "PROGN" "FUNCTION" "LAMBDA" "DEFUN" "DEFVAR" "DEFCONSTANT" "EXPORT" "IMPORT"
                   "VALUES" "+" "-" "DO" "COND"

                   ;contents of CLI package.
                   "//" "*DEFAULT-PATHNAME-DEFAULTS*" "AR-1" "AR-1-FORCE"
                   "AREF" "ASSOC" "ATAN" "CHARACTER" "CLOSE" "DEFSTRUCT" "DELETE"
                   "FUNCTIONP" "EVERY" "INTERSECTION" "LISTP" "MAP" "MEMBER" "NINTERSECTION"
                   "NLISTP" "NUNION" "RASSOC" "READ" "READ-FROM-STRING" "REM" "REMOVE"
                   "SOME" "SUBST" "UNION" "MAKE-ARRAY" "GETHASH" "MAKE-PACKAGE" "TIME" "IF"
                     ))
      (let ((survey (survey-for-symbol sym)))
        (dolist (elem ans (push (cons (list sym) (list survey)) ans))
          (cond ((survey-equal survey (cadr elem))
                 (rplaca elem (cons sym (car elem)))
                 (return nil))))
        ))
    ans))

(defun survey-equal (s1 s2)
  (cond ((equal s1 s2) t)
        ((or (not (consp s1)) (not (consp s2))) nil)
        (t (and (survey-element-equal (car s1) (car s2))
                (survey-equal (cdr s1) (cdr s2))))))

(defun survey-element-equal (e1 e2)
  (equal (cdr e1) (cdr e2)))
