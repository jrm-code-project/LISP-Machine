;-*- Mode:LISP; Package:SI; Readtable:ZL; Cold-Load:T; Base:8; Lowercase:T -*-

;;; Multiple package-root feature, 7/18/87, RG and ERRIC
;;;  allows multiple independant package hierarchies.
;;;  they can inherit, export, import, in usual ways, potentially including across hierarchies.
;;;  FIND-PACKAGE, etc, can effect arbitrary transfer either within a hierarchy or between them.
;;;  for example:
;;;   FIND-PACKAGE now has additional argument, use-local-names-package,
;;;   which when true finds the package in the current hierarchy
;;;   whose local name corresponds with the name specified.
;;;  *PACKAGE* contains full state as to which hierarchy is current as well as the package.
;;;  Read time conditionalization on package hierarchy available.
;;;   conditionalization can be done so as to be compatible with non-extended COMMON-LISP.
;;;  Format of *ALL-PACKAGES* not affected.
;;;  LOAD automatically loads compiled files into specified ROOTS, S-expression files otherwise.
;;;  Symbols in current or specified hierarchy are available with : specifiers.
;;;  Implemented via the REFNAME-ALIST feature of packages.

;;;  new properties on the plists are :root and :prefix
;;;  all packages in a given hierarchy should share the same root package
;;;  all packages in a given hierarchy should have the same prefix as part of their package name
;;;  this way corresponding packages in different hierarchies can have the same local names but
;;;   globally names differ by prefix.

;;;  the function PACKAGE-PRIMARY-NAME returns the package's local name, that is,
;;;    the name without the prefix

;;;  The function (package-root) always evaluates to the current root package (GLOBAL, initially).
;;;  (package-root-name)  -> (package-name (package-root))
;;;  #<+ or ->(:package-root-name <string>) <s-exp>  conditionally reads <s-exp> if the current
;;;    package root is <string>.  Thus, #+(:package-root-name "K-GLOBAL") foo
;;;    will read foo only if loading into the K-GLOBAL hierarchy, etc.
;;;  :compile-in-roots ("K-GLOBAL" "GLOBAL")) in the file attribute line specifies that compiled
;;;    form of this file is desired in hierarchies K-GLOBAL and GLOBAL, for example.  If an
;;;    attempt was made to FASLOAD the compiled file in some other hierarchy, a READFILE would
;;;    be forced.

;;; Also zwei command SET PACKAGE ROOT "Change the package root associated with buffer or file.
;;; If a package with the same name as the current package exists in that hierarchy,
;;; select it, otherwise, offer to create it.  Then you will be asked whether to
;;; resectionize the buffer.  If you say yes, all function definitions in the buffer
;;; will be recorded under symbols in the new package." in file SYS:ZWEI;ZMNEW.LISP

;;; Full list of user incompatibilities:
;;;  PKG-FIND-PACKAGE, some minor change.
;;;  PKG-SUPER-PACKAGE is kludged, PKG-SUBPACKAGES will lose.
;;;  Hairy undocumented PACKAGE-DECLARE features no longer supported.
;;;  Non-EQness of :COMPILE and GLOBAL:COMPILE. Beware of :NIL !!
;;;  Changed keyword arg names for APROPOS and WHO-CALLS and WHAT-FILES-CALL.
;;;  LOCF and SETF properties put on in USER package now require a prefix.
;;;  PKG-CONTAINED-IN is gone.

(defvar *package* :unbound
  "The current package, the default for most package operations including INTERN.")

(defvar package :unbound
  "The current package, the default for most package operations including INTERN.")
(forward-value-cell 'package '*package*)

(defvar *all-packages* nil
  "List of all packages that exist.")

(defvar pkg-keyword-package :unbound
  "The Keyword package.")

(defvar pkg-user-package :unbound
  "The default package for user code.")

(defvar pkg-global-package :unbound
  "The Lisp package.")

(defvar pkg-lisp-package :unbound
  "The Lisp package.")

(defvar pkg-system-package :unbound
  "The System package")

(defvar pkg-system-internals-package :unbound
  "The System-internals package")

;;; Any property name which is in the Compiler package
;;; is assumed to be related to the function definition
;;; of the symbol that has the property.
(defvar pkg-compiler-package :unbound
  "The Compiler package.")

(defvar pkg-area :unbound
  "The area which packages are consed in.")

(defun list-all-packages ()
  "Returns a list of all existing packages."
  (copy-list *all-packages*))

;;;; Low level data structure and interface definitions.

;;; This defines the format of a PACKAGE.
;;; SYS:UCADR;QF knows about it!
(cl:defstruct (package :named-array-leader
                       (:callable-constructors nil)
                       (:conc-name pkg-)
                       (:constructor pkg-make-package))
                       ;; (constructor klapaucius)
  (refname-alist nil :documentation
    "Alist of local nicknames, available in this package, for other packages.
Each element is (STRING . PACKAGE)")
  (name nil :documentation
    "Official name for this package (a string)")
  (nicknames nil :documentation
    "List of nicknames for this package (strings)")
  (use-list nil :documentation
    "List of packages this one has done USE-PACKAGE to")
  (all-packages-pointer '*all-packages* :documentation
    "Pointer to the symbol *ALL-PACKAGES*.
Here only for the sake of QF running on another machine.")

  ;; Slots beyond here not known about by QF.

  (used-by-list nil :documentation
    "List of packages that have done USE-PACKAGE to this package")
  (shadowing-symbols nil :documentation
    "List of symbols explicitly shadowed in this package")
  (number-of-symbols nil :documentation
    "Current number of symbols in this package")
  (max-number-of-symbols nil :documentation
    "Threshold for rehashing. This is the specified size of the package array")
  (prefix-print-name nil :documentation
    "Name to print in package prefixes.  NIL means use PKG-NAME")
  (plist nil :documentation
    "Random properties asscoiated with this package.
Properties used include:
SI:READ-LOCK (non-NIL means that READ will not attempt to intern new symbols in this package)
:SOURCE-FILE-NAME
SI:SUPER-PACKAGE (value is the superpackage, if there is one)
SI:ROOT-PACKAGE  If null, root is considered to be GLOBAL. ")
  (new-symbol-function nil :documentation
    "Function called to store a new symbol in this package.
NIL means PKG-INTERN-STORE is used.")
  (auto-export-p nil :documentation
    "Non-NIL means this package EXPORTs all symbols put in it.")
  )

(defsubst packagep (arg)
  "T if ARG is a package object."
  (eq (named-structure-p arg) 'package))

;;; The rest of the package is a 2 by (3/2 * specified size) array,
;;; whose contents are the interned symbols and their hash codes.
;;; If I is the slot number,
;;;  (AR-2-REVERSE PKG 0 I) and (AR-2-REVERSE PKG 1 I) are both NIL for an empty slot;
;;; for a filled slot the first is the hash code in low 23 bits,
;;;  sign bit set if symbol is external,
;;;  and the second is the symbol.
;;; For a REMOB'd slot, the first is T and the second is NIL.

(defsubst pkg-number-of-slots (pkg)
  (ash (array-length pkg) -1))

(defsubst pkg-slot-code (pkg slotnum)
  (ar-2-reverse pkg 0 slotnum))

(defsubst pkg-code-hash-code (code)
  (ldb (1- %%q-pointer) code))

(defsubst pkg-code-external-p (code)
  (minusp code))


(defsubst pkg-code-valid-p (code)
  (numberp code))

(defsubst pkg-make-code (external-flag hash-code)
  (%logdpb external-flag %%q-boxed-sign-bit
           (pkg-code-hash-code hash-code)))

(defsubst pkg-slot-symbol (pkg slotnum)
  (ar-2-reverse pkg 1 slotnum))

(defsubst pkg-string-hash-code (string)
  (%sxhash-string string #o377))

(defsubst pkg-declared-p (pkg)
  (getf (pkg-plist pkg) 'declared-p))

(defsubst pkg-read-lock-p (pkg)
  (getf (pkg-plist pkg) 'read-lock))

(defsubst pkg-root (pkg)
  (getf (pkg-plist pkg) :root))

(defsubst pkg-pfx (pkg)
  (getf (pkg-plist pkg) :prefix))

;; May not use nonatomic function specs in the cold load.
(defprop package package-named-structure-invoke named-structure-invoke)
(defun package-named-structure-invoke (op pkg &rest args)
  (selectq-with-which-operations op        ;;;;****SELECTQ... fails ((:get...
    (:describe
     (describe-package pkg))
    (:print-self
     (let ((stream (car args)))
       (if *print-escape*
           (sys:printing-random-object (pkg stream)
             (princ "Package " stream)
             (princ (pkg-name pkg) stream))
         (princ (pkg-name pkg) stream))))
    ((:get :get-location-or-nil :get-location :getl :putprop :putprop-in-area :remprop :push-property :plist
           :property-list :plist-location :property-list-location :setplist :set-property-list :set)
     (apply #'property-list-handler op (locf (pkg-plist pkg)) args))
    (:dereconcile (moby-dereconcile-package pkg (car args))))) ;this defined in MOBY-INIT.

(defmacro pkg-bind (pkg &body body)
  "Executes BODY with PKG as current package.  PKG is a package or the name of one."
  (if (equal pkg "USER")
      `(let ((*package* pkg-user-package))      ;Optimize most common(!?) case.
         . ,body)
    `(let ((*package* (pkg-find-package ,pkg)))
       . ,body)))

(defmacro do-symbols ((variable pkg result-form) &body body)
  "Executes BODY repeatedly with VARIABLE being each symbol available in package PKG.
Finally RESULT-FORM is evaluated and its value(s) returned.
All symbols inherited by PKG are included."
  (let ((index (gensym))
        (limit (gensym))
        (pkg-list-var (gensym))
        (pkg-var (gensym))
        (first-var (gensym))
        (p (gensym)))
    `(do* ((,p (find-package ,pkg))
           (,pkg-list-var (cons ,p (pkg-use-list ,p)) (cdr ,pkg-list-var))
           (,first-var t nil))
          ((null ,pkg-list-var)
           ,result-form)
       (do*-named t ((,index 0 (1+ ,index))
                     (,pkg-var (car ,pkg-list-var))
                     (,limit (pkg-number-of-slots ,pkg-var)))
                    ((= ,index ,limit))
         (when (and (pkg-code-valid-p (pkg-slot-code ,pkg-var ,index))
                    (or ,first-var
                        (pkg-code-external-p (pkg-slot-code ,pkg-var ,index))))
           (let ((,variable (pkg-slot-symbol ,pkg-var ,index)))
             . ,body))))))

(defmacro do-local-symbols ((variable pkg result-form code) &body body)
  "Executes BODY repeatedly with VARIABLE being each symbol present in package PKG.
Finally RESULT-FORM is evaluated and its value(s) returned.
Symbols inherited by PKG are not included."
  (let ((index (gensym))
        (limit (gensym))
        (pkg-var (gensym)))
    `(do* ((,index 0 (1+ ,index))
           (,pkg-var (find-package ,pkg))
           (,limit (pkg-number-of-slots ,pkg-var)))
          ((= ,index ,limit)
           ,result-form)
       (when (pkg-code-valid-p (pkg-slot-code ,pkg-var ,index))
         (let ((,variable (pkg-slot-symbol ,pkg-var ,index))
               (,(or code 'ignore)
                (pkg-slot-code ,pkg-var ,index)))
           . ,body)))))

(defmacro do-local-external-symbols ((variable pkg result-form) &body body)
  "Executes BODY repeatedly with VARIABLE being each external symbol present in package PKG.
Finally RESULT-FORM is evaluated and its value(s) returned.
Internal symbols and inherited symbols are not included."
  (let ((index (gensym))
        (limit (gensym))
        (pkg-var (gensym)))
    `(do* ((,index 0 (1+ ,index))
           (,pkg-var (find-package ,pkg))
           (,limit (pkg-number-of-slots ,pkg-var)))
          ((= ,index ,limit)
           ,result-form)
       (when (and (pkg-code-valid-p (pkg-slot-code ,pkg-var ,index))
                  (pkg-code-external-p (pkg-slot-code ,pkg-var ,index)))
         (let ((,variable (pkg-slot-symbol ,pkg-var ,index)))
           . ,body)))))

(defmacro do-external-symbols ((variable pkg result-form) &body body)
  "Executes BODY repeatedly with VARIABLE being each external symbol available in package PKG.
Finally RESULT-FORM is evaluated and its value(s) returned.
Symbols inherited by PKG are included."
  (let ((index (gensym))
        (limit (gensym))
        (pkg-list-var (gensym))
        (pkg-var (gensym)))
    (once-only (pkg)
      `(do ((,pkg-list-var (cons ,pkg (pkg-use-list ,pkg))
             (cdr ,pkg-list-var)))
           ((null ,pkg-list-var)
            ,result-form)
         (do*-named t ((,index 0 (1+ ,index))
                       (,pkg-var (car ,pkg-list-var))
                       (,limit (pkg-number-of-slots ,pkg-var)))
                      ((= ,index ,limit))
           (when (and (pkg-code-valid-p (pkg-slot-code ,pkg-var ,index))
                      (pkg-code-external-p (pkg-slot-code ,pkg-var ,index)))
             (let ((,variable (pkg-slot-symbol ,pkg-var ,index)))
               . ,body)))))))

(defmacro do-all-symbols ((variable result-form) &body body)
  "Executes BODY repeatedly with VARIABLE being each symbol present in any package.
Finally RESULT-FORM is evaluated and its value(s) returned.
A symbol may be processed more than once."
  (let ((index (gensym))
        (limit (gensym))
        (pkg-list-var (gensym))
        (pkg-var (gensym)))
    `(do ((,pkg-list-var *all-packages*
           (cdr ,pkg-list-var)))
         ((null ,pkg-list-var)
          ,result-form)
       (do*-named t ((,index 0 (1+ ,index))
                     (,pkg-var (car ,pkg-list-var))
                     (,limit (pkg-number-of-slots ,pkg-var)))
                    ((= ,index ,limit))
         (when (pkg-code-valid-p (pkg-slot-code ,pkg-var ,index))
           (let ((,variable (pkg-slot-symbol ,pkg-var ,index)))
             . ,body))))))

;;; Code outside of this file should use these
;;; rather than the DEFSTRUCT accessors directly,
;;; just to avoid compile-time depencency on package structure.

(defun package-name (pkg)
  "Returns the name of the specified package."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-name pkg))

(defun package-prefix-print-name (pkg)
  "Returns the name of the specified package for printing package prefixes."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (or (pkg-prefix-print-name pkg) (pkg-name pkg)))

(defun package-nicknames (pkg)
  "Returns the list of nicknames (as strings) of the specified package.
The package's name is not included."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-nicknames pkg))

(defun package-use-list (pkg)
  "Returns the list of packages (not names) USEd by specified package."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-use-list pkg))

(defun package-used-by-list (pkg)
  "Returns the list of packages (not names) that USE the specified package."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-used-by-list pkg))

(defun package-auto-export-p (pkg)
  "Returns T if PKG automatically exports all symbols inserted in it."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-auto-export-p pkg))

(defun package-shadowing-symbols (pkg)
  "Returns the list of symbols explicitly shadowed in the specified package."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (pkg-shadowing-symbols pkg))

(defun package-external-symbols (pkg)
  "Returns a list of all symbols present locally in PKG and external there."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (let (result)
    (do-local-external-symbols (sym pkg)
      (push sym result))
    result))

;;; For compatibility.
(defun pkg-super-package (pkg &optional no-default)
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (or (getf (pkg-plist pkg) 'super-package)
      (and (memq pkg-global-package (pkg-use-list pkg))
           (not no-default)
           pkg-global-package)))

(defun pkg-shortest-name (pkg &optional (with-respect-to-pkg *package*))
  "Return the shortest of PKG's name and nicknames."
  (unless (packagep pkg)
    (setq pkg (pkg-find-package pkg)))
  (let ((shortest-name (pkg-name pkg)))
    (dolist (nick (pkg-nicknames pkg))
      (if (and (not (equal nick ""))
               (not (ass #'equal nick (pkg-refname-alist with-respect-to-pkg)))
               (< (string-length nick)
                  (string-length shortest-name)))
          (setq shortest-name nick)))
    shortest-name))

(defun describe-package (pkg)
  "Describes thoroughly the package PKG (a package or the name of one).
The only thing not mentioned is what symbols are in the package.
Use MAPATOMS for that."
    (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
    (format t "~%Package ~A" (pkg-name pkg))
    (when (pkg-nicknames pkg)
      (princ " (")
      (do ((names (pkg-nicknames pkg) (cdr names))
           (first t nil))
          ((null names))
        (unless first (princ ", "))
        (princ (car names)))
      (princ ")"))
    (princ ".")
    (format t "~&   ~D. symbols out of ~D.  Hash modulus = ~D.~&"
            (pkg-number-of-symbols pkg)
            (pkg-max-number-of-symbols pkg)
            (pkg-number-of-slots pkg))
    (when (pkg-refname-alist pkg)
      (format t "Refname alist:~%")
      (dolist (l (pkg-refname-alist pkg))
        (format t "   ~20A~S~%" (car l) (cdr l))))
    (format t "~@[Packages which USE this one:~&~{   ~A~&~}~]" (pkg-used-by-list pkg))
    (format t "~@[Super-package ~A~&~]" (pkg-super-package pkg t))
    (format t "~@[Packages which are USEd by this one:~&~{   ~A~&~}~]" (pkg-use-list pkg))
    (format t "~@[Shadowed symbols:~&~{   ~S~&~}~]" (pkg-shadowing-symbols pkg))
    (format t "~@[New symbols are interned in this package using ~S~&~]"
            (pkg-new-symbol-function pkg))
    (format t "~@[Symbols interned in this package are automatically exported.~%~]"
            (pkg-auto-export-p pkg))
    (format t "~@[Additional properties of this package:~%~{   ~S:~33T~S~%~}~]"
            (pkg-plist pkg))
    pkg)

(defun find-package (name &optional use-local-names-package)
  "Returns the package whose name is NAME.
We look first for local names defined in package USE-LOCAL-NAMES-PACKAGE,
then for global package names.  USE-LOCAL-NAMES-PACKAGE is recommended
for use only in parsing package prefixes.
Returns NIL if there is no such package."
  (check-type name (or string cons symbol package))   ;;;thinks STRING CONS SYMBOL special vars
  (check-type use-local-names-package (or null package)) ;;;;thinks null is a special var
  (cond ((packagep name) name)
        ((consp name)
         (or (find-package (car name) use-local-names-package)
             (if (or (null (cddr name))  ;List has length 2 -- cannot be new style.
                     (and (null (cdddr name))  ;Length 3 -- may be old style or new.
                          (or (consp (cadr name))  ;so see if it makes sense in old style.
                              (and (symbolp (cadr name))  ;For that, 2nd elt must be a package
                                   (find-package (cadr name))))))  ;name, or a list of them.
                 (make-package (car name)
                               (if (and (cadr name) (atom (cadr name))) :super)
                               (cadr name)
                               :use (if (consp (cadr name)) (cadr name))
                               (if (caddr name) :size) (caddr name))
               (apply #'make-package name))))
        ((or (stringp name) (symbolp name))
         (or (and use-local-names-package
                  (do ((p use-local-names-package (pkg-super-package p t)))
                      ((null p))
                    (let ((elt (ass #'equal (string name)
                                    (pkg-refname-alist use-local-names-package))))
                      (when elt (return (cdr elt))))))
             (dolist (pkg *all-packages*)                           ;;;****DO-LIST failure
               (when (or (equal (pkg-name pkg) (string name))
                         (member-equal (string name) (pkg-nicknames pkg)))
                 (return pkg)))))))

(defun pkg-find-package (thing &optional create-p use-local-names-package)
  "Find or possibly create a package named THING.
If FIND-PACKAGE can find a package from the name THING, we return that package.
Otherwise, we may create such a package, depending on CREATE-P.
This should only happen if THING is a string or symbol.
Possible values of CREATE-P:
 NIL means get an error,
 :FIND means return NIL,
 :ASK means ask whether to create the package, and returns it if so.
 T means create the package (with default characteristics if they are unspecified)
   and return it."
  (or (and (packagep thing) thing)
      (find-package thing use-local-names-package)
      (case create-p
        (:find nil)
        ((nil :error)
         (signal-proceed-case ((new-name) 'package-not-found      ;;;***thinks
                                                                  ;;;***NEW-NAME is a function
                                          :package-name thing
                                          :relative-to use-local-names-package)
           (:create-package (or (find-package thing)
                                (make-package thing)))
           (:new-name
            (let* ((*package* pkg-user-package)
                   (string1 (string (cl:read-from-string new-name))))
              (pkg-find-package string1 create-p nil)))
           (:retry (pkg-find-package thing create-p use-local-names-package))))
        (:ask
         (if (fquery format:yes-or-no-p-options
                     "~&Package ~A not found.  Create it? "
                     thing)
             (make-package thing)
           (cerror "Continue"
                   "Please load package ~A declaration then continue." thing)
           (pkg-find-package thing create-p use-local-names-package)))
        ((t)
         (make-package thing)))))

;;; Compatibility only.
(defun pkg-create-package (name &optional (super package) (size #o200))
  "Creates and returns a new package.  Obsolete; use MAKE-PACKAGE instead."
  (make-package name :super super :size size :use nil))
(make-obsolete pkg-create-package "use MAKE-PACKAGE instead")

(defprop defpackage "Package" definition-type-name)
(defmacro defpackage (name &body alist-of-options)
  "Defines (creates or alters) a package named NAME.
Each element of ALIST-OF-OPTIONS looks like (OPTION ARGS...)
Options are:
/(:NICKNAMES names...) specifies alternate names for this package.
/(:PREFIX-NAME name) specifies string to print for this package when printing prefixes.
/(:USE packages...) specifies packages for this one to directly inherit external symbols from.
/(:SHADOW names...) specifies names of symbols to shadow in this package.
/(:EXPORT names...) specifies names of symbols to export in this package.
/(:IMPORT symbols...) specifies symbols to import in this package.
/(:IMPORT-FROM package names...) specifies a package and names of symbols
 to import in this package from that package.
/(:SHADOWING-IMPORT symbols...) specifies symbols to import in this package,
 overriding any name conflicts.
/(:RELATIVE-NAMES (name package)...) specifies local nicknames for
 other packages to be in effect in this package.
/(:RELATIVE-NAMES-FOR-ME (package name)...) specifies local nicknames
 for this package to be in effect in other packages.
/(:AUTO-EXPORT-P t-or-nil) non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
/(:SIZE int) specifies the number of symbols to allocate space for initially."
  ;; brainless documentation means that we have to accept this.
  (if (list-match-p name `(quote ,ignore)) (setq name (cadr name)))
  (check-type name (or string symbol))
  (setq name (intern (string name) pkg-keyword-package))
  `(progn
     (record-source-file-name ,name 'defpackage)
     (defpackage-internal ,name ',alist-of-options)))

;;; Foo. documentational braindamage (DayGloUal) says that defpackage takes &key-style args,
;;;  instead of an alist of options. So we have to deal with both. -- Mly
(defun defpackage-internal (name alist-of-options)
  ;; this hair deals with the fact that although having more than one :import-from is a
  ;;  perfectly reasonable thing package-wise, a simple keyword call would only see the
  ;;  first one. So we bundle them all up into one call to IMPORT
  (let ((import nil) (l nil))
    (do ((tail alist-of-options (cdr tail))
         k args)
        ((null tail))
      (setq k (car tail))
      (if (keywordp k)
          (setq args (cadr tail) tail (cdr tail))
        (psetq k (car k)
               args
               (if (memq (car k) '(:prefix-name :size :auto-export-p))
                   (cadr k)
                 (cdr k))))
      (if (list-match-p args `(quote ,ignore)) (setq args (cadr args)))
      (cond ((eq k :import)
             (dolist (s args)
               (push s import)))                                     ;;;; as usual DO-LIST fails
            ((eq k :import-from)
             (let ((p (pkg-find-package (car args)  () *package*)))
               (dolist (s (cdr args))
                 (push (intern (string s) p) import))))
            (t (push k l) (push args l))))
    (apply (if (if (pkg-root *package*)
                   (let ((local-pack (find-package name *package*)))
                     (and local-pack
                          (or (null (pkg-root local-pack))
                              (eq (pkg-root local-pack) (pkg-root *package*)))))
                 (find-package name))

               #'alter-package #'make-package)    ;;;bombs out here
           name
           :import (nreverse import)
           (nreverse l))))

(defun alter-package (name &key &optional nicknames                ;;;;thinks this is a malformed
                      (use '("GLOBAL")) ((:size ignore))           ;;;; list
                      shadow export prefix-name auto-export-p
                      import shadowing-import import-from
                      relative-names relative-names-for-me
                      ((:hash-inherited-symbols ignore))
                      properties new-symbol-function
                      ((:include ignore)) ((:colon-mode ignore))
                      (external-only nil external-only-p))
  (setq auto-export-p (or auto-export-p external-only)) ;brand s bd
  (let ((pkg (find-package name (and (boundp '*package*) *package*))))
    (unless pkg (ferror "Package ~A not found." name))
    (unless (cl:listp nicknames) (setq nicknames (list nicknames)))
    (rename-package pkg (pkg-name pkg) nicknames)
    (unless (or (null prefix-name) (string= prefix-name name)
                (mem #'string= prefix-name nicknames))
      (ferror "The prefix name ~A is not a name or nickname of the package." prefix-name))
    (setf (pkg-prefix-print-name pkg) (and prefix-name (string prefix-name)))
    (loop for (prop val) on properties by 'cddr
          do (setf (getf (pkg-plist pkg) prop) val))
    (shadow shadow pkg)
    (shadowing-import shadowing-import pkg)
    (mapc (lambda (s) (export (or (intern (string s) pkg) '(nil)) pkg)) export)
    (let ((desired-use (if (cl:listp use)
                           (mapcar #'find-package use)
                           (list (find-package use)))))
      (dolist (elt (pkg-use-list pkg))
        (unless (memq elt desired-use)
          (unuse-package elt pkg)))
      (use-package desired-use pkg))
    (import import pkg)
    (when import-from
      (dolist (name (cdr import-from))
        (let ((sym (intern (string name) (car import-from))))
          (import (if (null sym) (list sym) sym) pkg))))
    (if auto-export-p
        (setf (pkg-auto-export-p pkg) t
              (pkg-new-symbol-function pkg) (or new-symbol-function #'pkg-auto-export-store))
        (setf (pkg-auto-export-p pkg) nil
              (pkg-new-symbol-function pkg) new-symbol-function))
    (setf (pkg-refname-alist pkg)
          (loop with refname-alist = (and (pkg-root pkg)
                                          (pkg-refname-alist (pkg-root pkg)))
                for (nick p) in relative-names
                do (push (cons (string nick)
                               (find-package p))
                         refname-alist)
                finally (return refname-alist)))
    ;; First delete any other local nicknames, in any package, for this one.
    (dolist (p *all-packages*)
      (unless (and (pkg-root pkg)
                   (eq (pkg-root p) (pkg-root pkg)))
        (setf (pkg-refname-alist p)
              (cl:delete pkg (pkg-refname-alist p) :key 'cdr))))
    ;; Then add the ones that are requested.
    (dolist (elt relative-names-for-me)
      (pkg-add-relative-name (car elt) (cadr elt) pkg))
    pkg))

;;;this is from alex:kbug;def-corr-pkg
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


(defun make-package (name &key &optional nicknames (use '("GLOBAL")) super (size #o200)
                     ((nil ignore)) shadow export prefix-name auto-export-p invisible
                     import shadowing-import import-from
                     relative-names relative-names-for-me
                     ((:hash-inherited-symbols ignore))
                     properties new-symbol-function
                     ((:include ignore)) ((:colon-mode ignore))
                     external-only)
  (declare (arglist name &key &optional nicknames (use '("GLOBAL")) (size 200) shadow super
                    export prefix-name auto-export-p invisible
                    import shadowing-import import-from
                    relative-names relative-names-for-me properties new-symbol-function))
  "Creates and returns a package named NAME with nicknames NICKNAMES.
If any of these names already exists as a global name, we err.
Other keywords:
:USE specifies a list of names of packages for this one to USE-PACKAGE.
:SUPER specifies a superpackage (USE that package, and all the ones it USEs).
:SHADOW specifies a list of names of symbols to shadow in this package.
:EXPORT specifies a list of names of symbols to export in this package.
:IMPORT specifies a list of symbols to import in this package.
:IMPORT-FROM specifies a list of a package and names of symbols
 to import in this package from that package.
:SHADOWING-IMPORT specifies a list of symbols to import in this package,
 overriding any name conflicts.
:RELATIVE-NAMES specifies a list of pairs (NICKNAME . PACKAGENAME)
 specifying local nicknames to be in effect in this package.
:RELATIVE-NAMES-FOR-ME specifies a list of pairs (PACKAGENAME NICKNAME)
 specifying local nicknames to be in effect in those packages, for this one.
:AUTO-EXPORT-P non-NIL specifies that all symbols placed in this package
 should be exported automatically at that time.
:PREFIX-NAME specifies string to print for this package when printing prefixes.
:SIZE specifies the number of symbols to allocate space for initially.
:INVISIBLE non-NIL means do not record this package in table of package names."
  (unless (cl:listp nicknames) (setq nicknames (list nicknames)))
  (setq auto-export-p (or auto-export-p external-only))
  (let* ((table-size (pkg-good-size size))
         (default-cons-area working-storage-area)
         (pkg nil))
    (unless (or (null prefix-name)
                (equal prefix-name name)
                (member-equal prefix-name nicknames))
      (ferror "The prefix print name ~A for package ~A is not one of its nicknames."
              prefix-name name))
    ;;if *package* is part of a hierarchy give all package names and nicks the prefix name
    (let ((root (pkg-root *package*))
          (prefix (pkg-pfx *package*)))
      (unless (null root)
        (setq name (string-append prefix name))
        (unless (null prefix-name)
          (setq prefix-name (string-append prefix prefix-name)))
        (setq nicknames (mapcar #'(lambda (nickname)
                                    (string-append prefix
                                                   nickname))
                                nicknames))))
    (without-interrupts
      (unless invisible
        (when (find-package name)
          (ferror "A package named ~A already exists." name))
        (dolist (nick nicknames)
          (when (find-package nick)
            (ferror "A package named ~A already exists." nick))))
      (setq pkg (pkg-make-package
                  :make-array (:length (list table-size 2)
                                       :area pkg-area)
                  :name (string name)
                  :nicknames (mapcar #'string nicknames)
                  :prefix-print-name (and prefix-name (string prefix-name))
                  :all-packages-pointer '*all-packages*
                  :number-of-symbols 0
                  :max-number-of-symbols size
                  :plist properties))
      (when super
        (setq super (pkg-find-package super))
        (unless (or (eq super pkg-global-package)
                    (eq super pkg-system-package))
          (setf (pkg-auto-export-p super) t
                (pkg-new-symbol-function super) (or (pkg-new-symbol-function super)
                                                    #'pkg-auto-export-store))
          (setf (getf (pkg-plist pkg) 'super-package) super)))
      (unless invisible (push pkg *all-packages*))
      ;;check see if *package* is part of a hierarchy if it is install pkg into same
      ;; and change refname alists accordingly
      (if (pkg-root *package*)
          (let* ((old-alist (si:pkg-refname-alist *package*))
                 (new-alist (append (make-alist-of-package-names pkg (pkg-pfx *package*))
                                    old-alist)))
            (loop for package in (remove-duplicates (loop for alist in old-alist
                                                          collecting (cdr alist)))
                  do (setf (pkg-refname-alist package) new-alist))
            (setf (pkg-refname-alist pkg) new-alist)
            (setf (pkg-root pkg) (pkg-root *package*))
            (setf (pkg-pfx pkg) (pkg-pfx *package*)))))
    ;; Now do any requested shadowing, using or exporting
    ;; and kill this package if the user aborts.
    (let ((success nil))
      (unwind-protect
          (progn
            (when use (use-package use pkg))
            (when super
              (use-package (list super) pkg)
              (use-package (pkg-use-list (pkg-find-package super)) pkg))
            (when shadowing-import (shadowing-import shadowing-import pkg))
            (when import (import import pkg))
            (dolist (name (cdr import-from))
              (dolist (name (cdr import-from))
                (let ((sym (intern (string name) (car import-from))))
                  (import (if (null sym) (list sym) sym) pkg))))
            (when shadow (shadow shadow pkg))
            (mapc (lambda (s) (export (or (intern (string s) pkg) '(nil)) pkg)) export)
            (when relative-names
              (setf (pkg-refname-alist pkg)
                    (loop for (nick p) in relative-names
                          collect (cons (string nick)
                                        (find-package p)))))
            (dolist (elt relative-names-for-me)
              (pkg-add-relative-name (car elt) (cadr elt) pkg))
            (if auto-export-p
                (setf (pkg-auto-export-p pkg) t
                      (pkg-new-symbol-function pkg) (or new-symbol-function
                                                        #'pkg-auto-export-store))
              (setf (pkg-auto-export-p pkg) nil
                    (pkg-new-symbol-function pkg) new-symbol-function))
            (setq success t))
        (unless success
          (setq *all-packages* (delq pkg *all-packages*)))))
    pkg))

(defun cli:make-package (package-name &key nicknames (use '("LISP")))
  (zl:make-package package-name :nicknames nicknames :use use))

(defconst pkg-good-sizes
          '#o(103 111 123 141 161 203 225 243 263 301 337 357 373
              415 433 445 463 475 521 547 577 631 661 711 747
              1011 1043 1101 1145 1203 1245 1317 1357
              1423 1473 1537 1555 1627 1707 1761
              2033 2077 2131 2223 2275 2353 2447 2535 2631 2721
              3021 3123 3235 3337 3437 3541 3665 3767
              4101 4203 4313 4435 4553 4707
              5037 5201 5331 5477 5667
              6045 6163 6343 6517 6667 7065 7261 7461 7663
              10077 10301 10601 11123 11503 12033 12343 12701 13303 13711
              14321 14733 15343 16011 16465 17155 17657
              20343 21003 21603 22401 23303 24201 25117 26011 27001
              30125 31215 32311 33401 34501 35601 37005
              40207 41527 43001 44315 45713 47301
              51011 52407 54003 55401 57007 60607 62413 64207 66005 67603))

;;; Given a number of symbols, return a good length of hash table to hold that many.
(defun pkg-good-size (number-of-symbols)
  "Return a good hash table size which is bigger than 5/4 times NUMBER-OF-SYMBOLS."
  (let ((tem (truncate (* number-of-symbols 5) 4)))     ;Allow hash table to become 80% full
    (or (dolist (l pkg-good-sizes)
          (and (> l tem) (return l)))
        ;; Beyond the list of good sizes => avoid multiples of small primes.
        (do ((n (+ tem 1 (\ tem 2)) (+ n 2)))
            ((not (or (zerop (\ n 3))
                      (zerop (\ n 5))
                      (zerop (\ n 7))
                      (zerop (\ n 11.))
                      (zerop (\ n 13.))
                      (zerop (\ n 17.))))
             n)))))

;;; Rehash a package into a larger has table.
(defun pkg-rehash (pkg &optional (size (* 2 (pkg-max-number-of-symbols pkg)))
                   &aux new-pkg)
  (setq new-pkg (pkg-make-package make-array (:length (list (pkg-good-size size) 2)
                                              :area pkg-area)))
  (dotimes (i (array-leader-length pkg))
    (setf (array-leader new-pkg i) (array-leader pkg i)))
  (setf (pkg-number-of-symbols new-pkg) 0)
  (setf (pkg-max-number-of-symbols new-pkg) size)
  (do-local-symbols (sym pkg nil code)
    (let ((hash (pkg-string-hash-code (string sym))))
      (multiple-value-bind (nil nil nil index)
          (pkg-intern-store hash sym new-pkg)
        (setf (pkg-slot-code new-pkg index) code))))    ;Propagate externalness.
  (pkg-forward pkg new-pkg))

(defun pkg-forward (old new)
  (structure-forward old new))

(defun rename-package (pkg new-name &optional new-nicknames)
  "Change the name(s) of a package."
  (unless (packagep pkg)
    (setf pkg (pkg-find-package pkg nil (and (boundp '*package*) *package*))))
  (setq new-nicknames (if (cl:listp new-nicknames)
                          (mapcar #'string new-nicknames)
                          (list (string new-nicknames)))
        new-name (string new-name))
  ;;check see if *package* is part of a hierarchy if it is install pkg into same
  ;; and change refname alists accordingly
  (if (pkg-root pkg)
      (let* ((old-alist (si:pkg-refname-alist pkg))
             (new-alist (append (mapcar #'(lambda (name)
                                            (cons name pkg))
                                        new-nicknames)
                                old-alist)))
        (dolist (package (remove-duplicates (mapcar #'cdr old-alist)))
          (setf (pkg-refname-alist package) new-alist))
        (setf (pkg-refname-alist pkg) new-alist)
        (let ((prefix (pkg-pfx pkg)))
          (setq new-nicknames (mapcar #'(lambda (nickname)
                                          (string-append prefix
                                                         nickname))
                                      new-nicknames)))))

  (without-interrupts
    (let ((tem (find-package new-name)))
      (when (and tem (neq tem pkg))
        (ferror "A package named ~A already exists." new-name)))
    (dolist (nick new-nicknames)
      (let ((tem (find-package nick)))
        (when (and tem (neq tem pkg))
          (ferror "A package named ~A already exists." nick))))
    (setf (pkg-name pkg) new-name)
    (setf (pkg-nicknames pkg) new-nicknames))
  pkg)

(defun pkg-add-relative-name (from-pkg name to-pkg)
  "Make NAME be a local nickname for TO-PKG, effective in FROM-PKG."
  (push (cons (string name) (find-package to-pkg))
        (pkg-refname-alist (find-package from-pkg))))

(defun pkg-delete-relative-name (from-pkg name)
  "Make NAME cease to be a local nickname for anything in FROM-PKG."
  (let ((from-package (find-package from-pkg)))
    (setf (pkg-refname-alist from-package)
          (delete-if (lambda (elt) (string-equal (car elt) name))
                     (pkg-refname-alist from-package)))))

(defun kill-package (pkg)
  "Kill a package."
  (setq pkg (pkg-find-package pkg nil *package*))
  (dolist (p (pkg-use-list pkg))
    (unuse-package-1 p pkg))
  (dolist (p (pkg-used-by-list pkg))
    (unuse-package-1 pkg p))
  (dolist (p *all-packages*)
    (let ((tem (rassq pkg (pkg-refname-alist p))))
      (when tem (setf (pkg-refname-alist p)
                      (delq tem (pkg-refname-alist p))))))
  (setq *all-packages* (delq pkg *all-packages*)))
(deff pkg-kill 'kill-package)
;(make-obsolete pkg-kill "Use KILL-PACKAGE instead.")  -- sigh. brand S still use this

(defsignal symbol-name-conflict ferror (conflict-list)
  "A call to EXPORT was about to create one or more name conflicts.
CONFLICT-LIST has an element for each conflict about to happen.
The element looks like
 (NEW-CONFLICTING-SYMBOL CONFLICT-PACKAGE
   (OTHER-PACKAGE-SYMBOL OTHER-PACKAGE)...)")

(defun export (symbols &optional (pkg *package*) force-flag)
  "Makes SYMBOLS external in package PKG.
If the symbols are not already present in PKG, they are imported first.
Error if this causes a name conflict in any package that USEs PKG.
FORCE-FLAG non-NIL turns off checking for name conflicts, for speed
in the case where you know there cannot be any."
  (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (unless force-flag
    (let ((conflicts))
      ;; Find all conflicts there are.
      ;; Each element of CONFLICTS looks like
      ;; (new-conflicting-symbol conflict-package .
      ;;   ((other-package-symbol . other-package) ...))
      (dolist (p (pkg-used-by-list pkg))
        (dolist (symbol (if (cl:listp symbols) symbols (list symbols)))
          (let ((candidates
                  (check-for-name-conflict (if (symbolp symbol) (symbol-name symbol) symbol)
                                           p
                                           nil symbol pkg)))
            (when candidates
              (push (list* symbol p candidates) conflicts)))))
;      (when conflicts
;      ;; Now report whatever conflicts we found.
;       (cerror :no-action nil 'symbol-name-conflict
;               "Name conflicts created by EXPORT in package ~A:
;~:{~S causes a conflict in package ~A.~%~}"
;               pkg conflicts))
      (when conflicts
        (let ((symbol-package-pairs-to-remove nil)
              (unresolved-conflicts nil))
          (dolist (conflict conflicts)
            (let ((unresolved-conflict nil))
              (dolist (symbol-package-pair (cddr conflict))
                (cond ((eq (cdr symbol-package-pair) pkg))
                      (t
                       (let ((sym (car symbol-package-pair))
                             foundp)
                         (multiple-value (sym foundp)
                           (intern-local-soft sym (cdr symbol-package-pair)))
                         (cond ((null foundp)
                                (push symbol-package-pair unresolved-conflict))
                               ((and (not (boundp sym))
                                     (not (fboundp sym))
                                     (null (symbol-plist sym)))
                                (push symbol-package-pair symbol-package-pairs-to-remove))
                               (t
                                (push symbol-package-pair unresolved-conflict)))))))
              (when unresolved-conflict
                (push (list* (car conflict) (cadr conflict) unresolved-conflict)
                      unresolved-conflicts))))
          (cond ((null unresolved-conflicts)
                 (cerror "Remove offending symbols."
                         (format:outfmt
                           "This export causes name conflicts; however, none of
the conflicting symbols have values, function definitions, or properties.
It is probably safe to remove them.  The conflicting symbols are:"
                           (terpri)
                           (dolist (pair symbol-package-pairs-to-remove)
                             (format:tab 8)
                             (format:oprint (car pair))
                             (format:tab 30.)
                             (format:oprint (cdr pair))
                             (terpri))))
                 (dolist (pair symbol-package-pairs-to-remove)
                   (remob (car pair) (cdr pair))))
                (t
                 (ferror nil "Name conflicts created by EXPORT in package ~A:
~:{~S causes a conflict in package ~A.~%~}"
                         pkg conflicts)))))))
  (dolist (sym (if (cl:listp symbols) symbols (list symbols)))
    (unless (and (symbolp sym)
                 (eq (symbol-package sym) pkg))
      (setq sym (intern-local sym pkg)))
    (import sym pkg)
    (multiple-value-bind (nil index)
        (pkg-intern-internal (symbol-name sym)
                             (pkg-string-hash-code (symbol-name sym))
                             pkg)
      (setf (pkg-slot-code pkg index)
            (pkg-make-code 1 (pkg-slot-code pkg index)))))
  t)

(defun unexport (symbols &optional (pkg *package*) force-flag)
  "Makes SYMBOLS no longer external in package PKG.
Error if any symbol is not already present in PKG,
or if PKG is USEd by other packages.
FORCE-FLAG non-NIL prevents error due to PKG having been USEd."
  (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (and (pkg-used-by-list pkg)
       (not force-flag)
       (cerror "Unexport the symbol(s) anyway"
               "Package ~A is USEd by other packages." pkg))
  (dolist (sym (if (and symbols (symbolp symbols)) (list symbols) symbols))
    (import sym pkg)
    (multiple-value-bind (nil index)
        (pkg-intern-internal (symbol-name sym)
                             (pkg-string-hash-code (symbol-name sym))
                             pkg)
      (if index
          (setf (pkg-slot-code pkg index)
                (pkg-make-code 0 (pkg-slot-code pkg index)))
        (cerror "Ignore it" "Symbol ~S is not present in package ~A." sym pkg))))
  t)

(defun import (symbols &optional (pkg *package*))
  "Makes SYMBOLS be present in package PKG.
They then belong to PKG directly, rather than by inheritance.
Error if this produces a name conflict
/(a distinct symbol with the same name is already accesible in PKG)."
  (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (dolist (sym (if (cl:listp symbols) symbols (list symbols)))
    (unless (symbolp sym)
      (ferror "The argument ~S to IMPORT was not a symbol" sym))
    (multiple-value-bind (tem foundp)
        (intern-soft (symbol-name sym) pkg)
      (when (and foundp
                 (neq tem sym))
        (let ((*query-io* *debug-io*))
          (multiple-cerror '(symbol-name-conflict) ()
                           ("A symbol named ~S is already accessible in package ~A." (symbol-name sym) pkg)
            ("Import the new symbol."
             (unintern tem pkg))
            ("Retain the old symbol."
             (go punt))))))
    (intern sym pkg)
    punt)
  t)

(defun shadowing-import (symbols &optional (pkg *package*))
  "Makes SYMBOLS be present in package PKG.
They then belong to PKG directly, rather than by inheritance.
If any symbol with the same name is already available,
it gets replaced; this is rather drastic."
  (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (dolist (sym (if (cl:listp symbols) symbols (list symbols)))
    (unintern (symbol-name sym) pkg)
    (intern-local sym pkg)
    (unless (memq sym (pkg-shadowing-symbols pkg))
      (setf (pkg-shadowing-symbols pkg)
            (cons-in-area sym (pkg-shadowing-symbols pkg) background-cons-area))))
  t)

(defun shadow (names &optional (pkg *package*))
  "Makes the symbols in package PKG with names NAMES be shadowed symbols.
This means that symbols with these names are created directly in PKG
if none were present already.
Any symbols with these names previously available by inheritance become hidden."
  (or (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (dolist (sym (if (cl:listp names) names (list names)))
    (let ((string (string sym)))
      (multiple-value-bind (sym oldp)
          (intern-local string pkg)
        (unless (or oldp (memq sym (pkg-shadowing-symbols pkg)))
          (setf (pkg-shadowing-symbols pkg)
                (cons-in-area sym (pkg-shadowing-symbols pkg) background-cons-area))))))
  t)

(defun use-package (pkgs-to-use &optional (pkg *package*) &aux packages)
  "Adds PKGS-TO-USE to the inheritance list of the current package.
The external symbols in those packages become available
/(but not present or external) in the package that is now current.
PKGS-TO-USE is a list of packages or names of packages or lists which specifies a root and a
package so that you can specify a package in another hierarchy ((global x) ...) specifies
package x in the global root hierarchy ;
PKGS-TO-USE can also be a single package or name.
PKG is the package that should now USE those other packages."
  (let ((default-package (and (boundp '*package*) *package*)))
    (or (packagep pkg) (setq pkg (pkg-find-package pkg nil default-package)))
    (if (cl:listp pkgs-to-use)
        (dolist (pkg pkgs-to-use) (pushnew
                                    (if (cl:listp pkg)
                                        (pkg-find-package (cadr pkg) nil
                                                          (pkg-find-package (car pkg)))
                                      (pkg-find-package pkg nil default-package))
                                    packages :test 'eq))
      (setq packages (list (pkg-find-package pkgs-to-use nil default-package)))))
  (dolist (use-pkg packages)
    (when (eq use-pkg pkg-keyword-package)
      (ferror "Attempt to USE-PACKAGE the Keyword package."))
    (when (memq use-pkg (pkg-use-list pkg))
      (setq packages (remq use-pkg packages))))
  (let ((local-conflicts)                       ;inheriting a symbol same as locally present
        (inherited-conflicts)                   ;inheriting a symbol same as already inherited
        (external-conflicts))                   ;inheriting multiple same symbols
    ;; Each element of each conflicts list looks like
    ;;  ((other-package-symbol . other-package)...))
    (do-local-symbols (symbol pkg)
      (let ((candidates
              (check-for-name-conflict (symbol-name symbol) pkg nil nil nil packages)))
        (setq candidates (del-if (lambda (x) (eq (cdr x) pkg)) candidates))
        (when candidates
          (push candidates local-conflicts))))
    (unless (zerop (pkg-number-of-symbols pkg))
      (dolist (p (pkg-use-list pkg))
        (do-local-external-symbols (symbol p)
          (let ((candidates
                  (check-for-name-conflict (symbol-name symbol) pkg t nil nil packages)))
            (when candidates
              (setq candidates (nbutlast candidates))
              (push (cons symbol p) candidates)
              (push candidates inherited-conflicts))))))
    (let ((possible-conflict-packages (copy-list packages)))
      ;; Avoid checking for inheritance conflict between used packages
      ;; in the most common case: pkg contains only shadowing symbols
      ;; and the packages being used are global
      ;; and perhaps one other package which itself uses global.
      (unless (and (= (pkg-number-of-symbols pkg)
                      (length (pkg-shadowing-symbols pkg)))
                   (null (pkg-use-list pkg))
                   (or (and (= (length possible-conflict-packages) 1)
                            (eq (car possible-conflict-packages) pkg-global-package))
                       (and (= (length possible-conflict-packages) 2)
                            (memq pkg-global-package possible-conflict-packages)
                            (memq pkg-global-package
                                  (pkg-use-list (car (remq pkg-global-package
                                                           possible-conflict-packages)))))))
        (do ((p possible-conflict-packages (cdr p)))
            ((null p))
          (do-local-external-symbols (sym (car p))
            (unless (or (dolist (c external-conflicts nil)
                          (when (assq sym c) (return t)))
                        (dolist (c inherited-conflicts nil)
                          (when (assq sym c) (return t))))
              (let ((candidates (check-for-name-conflict
                                  (symbol-name sym) pkg t nil nil p)))
                (when candidates
                  (push candidates external-conflicts))))))))
    (when (or local-conflicts inherited-conflicts external-conflicts)
      ;; Now report whatever conflicts we found.
      (signal-proceed-case (() 'use-package-name-conflict
                               :in-package pkg
                               :using-packages pkgs-to-use
                               :local-conflicts local-conflicts
                               :inherited-conflicts inherited-conflicts
                               :external-conflicts external-conflicts)
        (:punt (return-from use-package t)))))
  (dolist (use-pkg packages)
    (unless (memq use-pkg (pkg-use-list pkg))
      (without-interrupts
        (let ((default-cons-area working-storage-area))
          (push pkg (pkg-used-by-list use-pkg))
          (push use-pkg (pkg-use-list pkg))))))
  t)

(defun unuse-package (pkgs-to-unuse &optional (pkg *package*) &aux packages)
  "Removes PKGS-TO-UNUSE from the inheritance list of the current package.
Their external symbols are no longer inherited by this package.
PKGS-TO-USE is a list of packages or names of packages;
or a single package or name.
PKG is the package that formerly USEd those other packages
and which should cease to do so."
  (let ((default-package (and (boundp '*package*) *package*)))
    (or (packagep pkg) (setq pkg (pkg-find-package pkg nil default-package)))
    (setq packages
          (if (cl:listp pkgs-to-unuse)
              (mapcar #'pkg-find-package pkgs-to-unuse) ;******!!!!***** fix this sometime.
            (list (pkg-find-package pkgs-to-unuse nil default-package)))))
  (dolist (use-pkg packages)
    (unuse-package-1 use-pkg pkg))
  t)

(defun unuse-package-1 (used-package using-package)
  (without-interrupts
    (setf (pkg-used-by-list used-package)
          (delq using-package (pkg-used-by-list used-package)))
    (setf (pkg-use-list using-package)
          (delq used-package (pkg-use-list using-package)))))

(defun pkg-add-nicknames (pkg nicknames)
  (dolist (nick nicknames)
    (let ((p1 (find-package nick)))
      (cond ((null p1)
             (push (string nick) (pkg-nicknames pkg)))
            ((eq p1 pkg) nil)
            (t (ferror "There is already a package named ~A."))))))

;;; Given a symbol, moves it into the Lisp package, or to whatever package
;;; is specified, from all packages which USE that one.
;;; All symbols with those names in other packages are forwarded.
;;; Values, properties and function definitions are all merged from
;;; those other symbols into these ones.  Multiple values or function
;;; definitions, such as cannot properly be merged, cause errors.

;;; Given a string instead of a symbol,
;;; it takes the symbol from the Keyword package if any, or creates a new one.
(defun globalize (string &optional (into-package pkg-global-package)
                   &aux globalize-fn-pkg globalize-val-pkg sys tem)
  "Make there be only one symbol with name STRING.
If INTO-PACKAGE is specified, we apply only to that package
and its subpackages."
  (declare (special globalize-fn-pkg globalize-val-pkg))
  (setq into-package (pkg-find-package into-package))
  (dolist (p (pkg-used-by-list into-package))
    (if (setq tem (intern-local-soft string p))
        ;; Don't use PUSHNEW, because ADJOIN is loaded late in system build.
        (unless (memq tem (pkg-shadowing-symbols p))
          (push tem (pkg-shadowing-symbols p)))))
  (setq sys (intern string into-package))
  (globalize-1 sys into-package)
  (mapc #'globalize-1 (circular-list sys)
        (pkg-used-by-list into-package))
  (export sys into-package t)
  (setf (symbol-package sys) into-package))

;;; Given a newly created symbol in GLOBAL, makes all symbols
;;; down below with that name forward to it, after merging in
;;; their definitions (barfing at multiple definitions).
(defun globalize-1 (global pkg &aux local)
  (declare (special globalize-fn-pkg globalize-val-pkg))
  (when (and (setq local (intern-local-soft global pkg))
             (neq local global)
             ( (%p-ldb-offset %%q-data-type local 3) dtp-one-q-forward))
    (when (boundp local)
      (and (boundp global)
           (neq (symbol-value local) (symbol-value global))
           (ferror "Multiple values for ~S, in ~A and ~A"
                   global (symbol-package global) (symbol-package local)))
      (setq globalize-val-pkg pkg)
      (set global (symbol-value local)))
    (when (fboundp local)
      (and (fboundp global)
           (neq (symbol-function local) (symbol-function global))
           (ferror "Multiple function definitions for ~S, in ~A and ~A"
                   global (symbol-package global) (symbol-package local)))
      (setq globalize-fn-pkg pkg)
      (fset global (symbol-function local)))
    (do ((plist (plist local) (cddr plist))) ((null plist))
      (and (get global (car plist))
           (neq (get global (car plist)) (cadr plist))
           (ferror "Multiple values for ~S property of ~S" (car plist) global))
      (putprop global (cadr plist) (car plist)))
    (do ((i 1 (1+ i))) ((= i 4))
      (%p-store-tag-and-pointer (%make-pointer-offset dtp-locative local i)
                                dtp-one-q-forward
                                (%make-pointer-offset dtp-locative global i)))))

;;; This is the normal INTERN function, once packages are installed.
;;; Value 1 is the interned symbol.
;;; Value 2 is T if the symbol was already interned.
;;; Value 3 is the package that the symbol is actually present in.
(defun intern (sym &optional pkg &aux hash str)
  "Interns the string or symbol SYM in package PKG (or the current package).
If the package has a symbol whose pname matches SYM, that symbol is returned.
The USEd packages are also searched.
Otherwise, if SYM is a symbol, it is put in the package and returned.
Otherwise (SYM is a string), a new symbol is constructed, put in the package and returned.

The second value is non-NIL if a preexisting symbol was found.
 More specifically, it can be :INTERNAL, :EXTERNAL or :INHERITED.

The third value is the package the symbol was actually found or inserted in.
This may be PKG or one of the packages USEd by PKG."
  (declare (values symbol already-interned-flag actual-package))
  (let ((default-package (and (boundp '*package*) *package*)))
    (cond ((null pkg) (setq pkg default-package))
          ((not (packagep pkg)) (setq pkg (pkg-find-package pkg nil default-package)))))
  (if (stringp sym) (setq str sym)
    (if (symbolp sym) (setq str (symbol-name sym))
      (setq str (string sym))))
  (setq hash (pkg-string-hash-code str))
  ;; Prevent interrupts in case two people intern symbols with the same pname,
  ;; both find that there is no such symbol yet,
  ;; and both try to stick them in the obarray simultaneously.
  (without-interrupts
    ;; Search this package.
    (let ((len (pkg-number-of-slots pkg))
          x y)
      (do ((i (\ hash len)))
          ((null (setq x (pkg-slot-code pkg i)))
           nil)
        (when (and (pkg-code-valid-p x)
                   (= hash (pkg-code-hash-code x))
                   (equal str (symbol-name (setq y (pkg-slot-symbol pkg i)))))
          (return-from intern (values y
                                      (if (pkg-code-external-p x)
                                          :external :internal)
                                      pkg)))
        (if (= (incf i) len) (setq i 0))))
    ;; Search USEd packages.
    (dolist (pkg (pkg-use-list pkg))
      (let ((len (pkg-number-of-slots pkg))
            x y)
        (do ((i (\ hash len)))
            ((null (setq x (pkg-slot-code pkg i)))
             nil)
          (when (and (pkg-code-valid-p x)
                     (= hash (pkg-code-hash-code x))
                     (equal str (symbol-name (setq y (pkg-slot-symbol pkg i)))))
            (if (pkg-code-external-p x)
                (return-from intern (values y
                                            :inherited
                                            pkg))
              (return nil)))                    ;Not inheritable from this package.
          (if (= (incf i) len) (setq i 0)))))
    ;; Must install a new symbol.
    ;; Make a symbol if the arg is not one.
    (unless (symbolp sym)
      (when (and (stringp sym)
                 (neq (array-type sym) 'art-string))
        (setq sym (string-remove-fonts sym)))
      (setq sym (make-symbol sym t)))
    (funcall (or (pkg-new-symbol-function pkg) #'pkg-intern-store)
             hash sym pkg)))

(defun find-external-symbol (string &optional pkg)
  "Returns the external symbol available in package PKG whose name is STRING, if any.
Unlike INTERN, FIND-EXTERNAL-SYMBOL never creates a new symbol;
it returns NIL if none was found, or an internal symbol was found.
PKG can be a package or a package name; NIL means use *PACKAGE*.

The second value is non-NIL if a symbol was found.
 More specifically, it can be :EXTERNAL or :INHERITED."
  (declare (values symbol found-flag))
  (multiple-value-bind (symbol found-flag)
      (intern-soft (string string) pkg)
    (if (memq found-flag '(:external :inherited))
        (values symbol found-flag))))

;;; Must lock out interrupts because otherwise someone might rehash the
;;; package while we are scanning through it.
(defun intern-soft (sym &optional pkg &aux hash str)
  "Like INTERN but returns NIL for all three values if no suitable symbol found.
Does not ever put a new symbol into the package."
  (declare (values symbol actually-found-flag actual-package))
  (let ((default-package (and (boundp '*package*) *package*)))
    (cond ((null pkg) (setq pkg default-package))
          ((not (packagep pkg)) (setq pkg (pkg-find-package pkg nil default-package)))))
  (if (stringp sym) (setq str sym)
    (if (symbolp sym) (setq str (symbol-name sym))
      (setq str (string sym))))
  (setq hash (pkg-string-hash-code str))
  (without-interrupts
    (block intern
      ;; Search this package.
      (let ((len (pkg-number-of-slots pkg))
            x y)
        (do ((i (\ hash len)))
            ((null (setq x (pkg-slot-code pkg i)))
             nil)
          (and (pkg-code-valid-p x)
               (= hash (pkg-code-hash-code x))
               (equal str (symbol-name (setq y (pkg-slot-symbol pkg i))))
               (return-from intern
                 (values y
                         (if (pkg-code-external-p x)
                             :external :internal)
                         pkg)))
          (if (= (incf i) len) (setq i 0))))
      ;; Search USEd packages.
      (dolist (pkg (pkg-use-list pkg))
        (let ((len (pkg-number-of-slots pkg))
              x y)
          (do ((i (\ hash len)))
              ((null (setq x (pkg-slot-code pkg i)))
               nil)
            (and (pkg-code-valid-p x)
                 (= hash (pkg-code-hash-code x))
                 (equal str (symbol-name (setq y (pkg-slot-symbol pkg i))))
                 (if (pkg-code-external-p x)
                     (return-from intern
                       (values y
                               :inherited
                               pkg))
                   (return)))                   ;Not inheritable from this package.
            (if (= (incf i) len) (setq i 0))))))))
(deff find-symbol 'intern-soft)

(defun intern-local (sym &optional (pkg *package*) &aux hash tem found str)
  "Like INTERN but does not search the superiors of the package PKG.
If no symbol is found in that package itself, SYM or a new symbol
is put into it, even if a superior package contains a matching symbol."
  (declare (values symbol already-interned-flag actual-package))
  (unless (packagep pkg) (setq pkg (pkg-find-package pkg nil *package*)))
  (if (stringp sym) (setq str sym)
    (if (symbolp sym) (setq str (symbol-name sym))
      (setq str (string sym))))
  (setq hash (pkg-string-hash-code str))
  (without-interrupts
    (let ((len (pkg-number-of-slots pkg))
          x y)
      (do ((i (\ hash len)))
          ((null (setq x (pkg-slot-code pkg i)))
           nil)
        (when (and (pkg-code-valid-p x)
                   (= hash (pkg-code-hash-code x))
                   (equal str (symbol-name (setq y (pkg-slot-symbol pkg i)))))
          (return-from intern-local
            (values y
                    (if (pkg-code-external-p x)
                        :external :internal)
                    pkg)))
        (if (= (incf i) len) (setq i 0))))
    (if found (values tem t pkg)
      (unless (symbolp sym)
        (when (and (stringp sym)
                   (neq (array-type sym) 'art-string))
          (setq sym (string-remove-fonts sym)))
        (setq sym (make-symbol sym t)))
      (funcall (or (pkg-new-symbol-function pkg) #'pkg-intern-store)
               hash sym pkg))))

;;; Check whether a symbol is present in a given package (not inherited).
;;; The values match those of INTERN.
(defun intern-local-soft (sym &optional (pkg *package*) &aux hash tem found str)
  "Like INTERN but checks only the specified package and returns NIL if nothing found."
  (declare (values symbol actually-found-flag))
  (unless (packagep pkg) (setq pkg (pkg-find-package pkg)))
  (setq hash (pkg-string-hash-code (setq str (string sym))))
  (without-interrupts
    (multiple-value-setq (tem found)
      (pkg-intern-internal str hash pkg)))
  (when found (values tem t pkg)))

;;;; Internals of INTERN.

;;; Search a given package for a given symbol with given hash code.
;;; If it is found, return it and the index it was at.
;;; Otherwise, return NIL NIL.
(defun pkg-intern-internal (string hash pkg
                            &aux x y (len (pkg-number-of-slots pkg))
                                 (alphabetic-case-affects-string-comparison t))
  (do ((i (\ hash len)))
      ((null (setq x (pkg-slot-code pkg i)))
       nil)
    (and (pkg-code-valid-p x)
         (= hash (pkg-code-hash-code x))
         (equal string (symbol-name (setq y (pkg-slot-symbol pkg i))))
         (return (values y i)))
    (if (= (incf i) len) (setq i 0))))

(defun pkg-intern-external (string hash pkg
                             &aux x y (len (pkg-number-of-slots pkg))
                             (alphabetic-case-affects-string-comparison t))
  (do ((i (\ hash len)))
      ((null (setq x (pkg-slot-code pkg i)))
       nil)
    (when (and (pkg-code-valid-p x)
               (= hash (pkg-code-hash-code x))
               (pkg-code-external-p x)
               (equal string (symbol-name (setq y (pkg-slot-symbol pkg i)))))
      (return (values y i)))
    (if (= (incf i) len) (setq i 0))))

;;; Store the symbol SYM into the package PKG, given a precomputed hash,
;;; assuming that no symbol with that pname is present in the package.
;;; If number of symbols exceeds the maximum (which is less than
;;; the length of the array by 4/5), we rehash.
;;; Call only if interrupts are locked out.
(defun pkg-intern-store (hash sym pkg &aux len final-index)
  (declare (values sym nil pkg index))
  (when (symbolp (symbol-package sym))          ;Either NIL (uninterned)
                                                ;or 'COMPILER or '|| etc in cold load.
    (setf (symbol-package sym) pkg))
  (setq len (pkg-number-of-slots pkg))
  (do ((i (\ hash len) (\ (1+ i) len)))
      ((not (pkg-code-valid-p (pkg-slot-code pkg i)))
       (setq final-index i)
       (setf (pkg-slot-code pkg i) hash)
       (setf (pkg-slot-symbol pkg i) sym)))
  (when (> (incf (pkg-number-of-symbols pkg))
           (pkg-max-number-of-symbols pkg))
    (pkg-rehash pkg)
    (multiple-value-setq (nil final-index)
      (pkg-intern-internal (symbol-name sym) hash pkg)))
  (values sym nil pkg final-index))

(defun pkg-code (sym pkg &aux hash str)
  "Returns the PKG-CODE of SYM in PKG or nil if SYM is not directly in PKG.
The PKG-CODE is negative if SYM is exportable.  The remaining bits are the hash code"
  (prog pkg-code ()
        (cond ((null pkg) (setq pkg *package*))
              ((not (packagep pkg)) (setq pkg (pkg-find-package pkg))))
        (if (stringp sym) (setq str sym)
          (if (symbolp sym) (setq str (symbol-name sym))
            (setq str (string sym))))
        (setq hash (pkg-string-hash-code str))
        ;; Prevent interrupts in case two people intern symbols with the same pname,
        ;; both find that there is no such symbol yet,
        ;; and both try to stick them in the obarray simultaneously.
        (without-interrupts
          ;; Search this package.
          (let ((len (pkg-number-of-slots pkg))
                x y)
            (do ((i (\ hash len)))
                ((null (setq x (pkg-slot-code pkg i)))
                 nil)
              (when (and (pkg-code-valid-p x)
                         (= hash (pkg-code-hash-code x))
                         (equal str (symbol-name (setq y (pkg-slot-symbol pkg i)))))
                (return-from pkg-code x))
              (if (= (incf i) len) (setq i 0)))))))

(defun pkg-set-external-flag (sym pkg external-flag &aux hash str)
  "Low level function to recover when things screwwed up.  Sets exportable
flag for SYM in PKG (ie sign bit of pkg-slot-code)"
  (prog pkg-code ()
        (cond ((null pkg) (setq pkg *package*))
              ((not (packagep pkg)) (setq pkg (pkg-find-package pkg))))
        (if (stringp sym) (setq str sym)
          (if (symbolp sym) (setq str (symbol-name sym))
            (setq str (string sym))))
        (setq hash (pkg-string-hash-code str))
        ;; Prevent interrupts in case two people intern symbols with the same pname,
        ;; both find that there is no such symbol yet,
        ;; and both try to stick them in the obarray simultaneously.
        (without-interrupts
          ;; Search this package.
          (let ((len (pkg-number-of-slots pkg))
                x y)
            (do ((i (\ hash len)))
                ((null (setq x (pkg-slot-code pkg i)))
                 nil)
              (when (and (pkg-code-valid-p x)
                         (= hash (pkg-code-hash-code x))
                         (equal str (symbol-name (setq y (pkg-slot-symbol pkg i)))))
                (setf (pkg-slot-code pkg i) (setq x (pkg-make-code external-flag x)))
                (return-from pkg-code x))
              (if (= (incf i) len) (setq i 0)))))))

(defun pkg-auto-export-store (hash sym pkg)
  (pkg-intern-store hash sym pkg)
  (if (null sym) (export '(nil) pkg)
    (export sym pkg))
  (values sym nil pkg))

;;; used by FONTS
(defun pkg-specialize-and-auto-export-store (hash sym pkg)
  (pkg-intern-store hash sym pkg)
  (if (null sym) (export '(nil) pkg)
    (export sym pkg))
  (unless (get sym 'special)
    (setf (get sym 'special) t))
  (values sym nil pkg))

(defun pkg-keyword-store (hash sym pkg &aux index)
  (set sym sym)                                 ;make keywords self-evaluate
  (setq index (nth-value 3 (pkg-intern-store hash sym pkg)))
  (setf (pkg-slot-code pkg index)               ;Keywords are always external
        (pkg-make-code 1 (pkg-slot-code pkg index)))
  (values sym nil pkg))


;;; Remove a symbol from a package.  Leaves T as the "hash code" where the
;;; symbol was, so that PKG-INTERN-INTERNAL will search past that point.
;;; Put NIL into the package-cell of the symbol so that we know it
;;; is uninterned.  If the user then interns it someplace else, its package
;;; cell will then be set to that as it should be.
;;; Returns T if the symbol was previously interned in that package, NIL if not.
(defun unintern (sym &optional (pkg *package*) force-flag
                 &aux hash tem str must-replace replacement-sym)
  "Removes (uninterns) any symbol whose pname matches SYM from package PKG.
SYM may be a symbol or a string.
PKG defaults to *PACKAGE* (common-lisp braindamage)
FORCE-FLAG says do not check for name conflicts.
Returns T if a symbol was removed, NIL if there was no such symbol present."
  (when pkg
    (or (packagep pkg) (setq pkg (pkg-find-package pkg)))
    (when (and (not force-flag)
               (memq sym (pkg-shadowing-symbols pkg)))
      ;; Check for name-conflict being uncovered.
      ;; If there is one, decide what we will do to fix it.
      ;; We can't actually do it until after uninterning our argument.
      ;; Each element of CONFLICT looks like (other-package-symbol . other-package)
      (let ((conflict (check-for-name-conflict (string sym) pkg t)))
        (when conflict
          (setf (values must-replace replacement-sym)
                (report-name-conflict sym pkg conflict)))))
    (setq hash (pkg-string-hash-code (setq str (string sym))))
    (without-interrupts
      (multiple-value (sym tem)
        (pkg-intern-internal str hash pkg))
      (when tem
        (and (eq (symbol-package sym) pkg)
             (setf (symbol-package sym) nil))
        (setf (pkg-slot-code pkg tem) t)
        (setf (pkg-slot-symbol pkg tem) nil)
        (setf (pkg-shadowing-symbols pkg)
              (delq sym (pkg-shadowing-symbols pkg)))
        (when must-replace
          (intern-local replacement-sym pkg)
          (push replacement-sym (pkg-shadowing-symbols pkg)))
        t))))

(defun remob (sym &optional (pkg (symbol-package sym)) force-flag)
  "Removes any symbol whose pname matches SYM from package PKG.
SYM may be a symbol or a string.
PKG defaults to SYM's package (in which case, SYM must be a symbol,
 and it is the symbol that gets removed).
FORCE-FLAG says do not check for name conflicts.
Returns T if a symbol was removed, NIL if there was no such symbol present."
  (unintern sym pkg force-flag))

;;; return list ((other-package-symbol . other-package) ...)
(defun check-for-name-conflict (string pkg &optional not-local-symbols
                                                     additional-symbol additional-symbol-pkg
                                                     additional-used-packages)
  (let ((candidates))
    (unless not-local-symbols
      (multiple-value-bind (sym foundp)
          (intern-local-soft string pkg)
        (when foundp
          (if (cli:member sym (pkg-shadowing-symbols pkg)
                          :test #'string-equal) ;shadowing symbol can't conflict
              (return-from check-for-name-conflict nil)
            (push (cons sym pkg) candidates)))))
    (when (and additional-symbol
               (dolist (elt candidates t)
                 (when (eq (car elt) additional-symbol) (return nil))))
      (push (cons additional-symbol additional-symbol-pkg) candidates))
    (dolist (p (pkg-use-list pkg))
      (multiple-value-bind (sym foundp)
          (intern-local-soft string p)
        (when (and (eq foundp :external)
                   (dolist (elt candidates t)
                     (when (eq (car elt) sym) (return nil))))
          (push (cons sym p) candidates))))
    (dolist (p additional-used-packages)
      (multiple-value-bind (sym foundp)
          (intern-local-soft string p)
        (when (and (eq foundp :external)
                   (dolist (elt candidates t)
                     (when (eq (car elt) sym) (return nil))))
          (push (cons sym p) candidates))))
    (and (cdr candidates)                       ;one conflict ain't no conflict
         candidates)))

;;; this is losing. fix.
(defprop report-name-conflict t :error-reporter)
(defun report-name-conflict (symbol pkg available-syms)
  (tagbody
   lose
      (cerror :no-action nil 'symbol-name-conflict
              "UNINTERN of ~1@*~S from package ~A causing discovered name conflict.
Symbols from packages ~A all want to be inherited."
              (list (list symbol pkg available-syms))
              symbol pkg (mapcar #'car available-syms))
      (let* ((desired-pkg (find-package
                            (prompt-and-read :string "~&Type the name of the package whose symbol you want ~A to contain: " symbol)))
             (elt (rassq desired-pkg available-syms)))
    (values (cdr elt) (car elt)))))

;;; Old interface still called.
(defun pkg-prefix (sym fcn &optional (pkg *package*))
  (let ((prefix (pkg-printing-prefix sym pkg)))
    (when prefix (funcall fcn
                          (if (stringp prefix) prefix (pkg-name prefix))
                          0))))

(defun pkg-printing-prefix (symbol &optional (pkg *package*))
  "Returns info on how to print SYMBOL with PKG as the current package.
First value is package or a refname string to print, or NIL for none.
Second value is T if symbol should print with #: (ie, if it is
uninterned or not external)."
  (let ((sym-pkg (symbol-package symbol)))
    (cond ((null sym-pkg)
           (values nil t))
          ((eq sym-pkg pkg-keyword-package)
           (values "" nil))
          ((eq pkg sym-pkg)
           nil)
          ((and pkg
                (multiple-value-bind (foundsym foundp)
                    (find-symbol symbol pkg)
                  (and foundp (eq foundsym symbol))))
           nil)
          (t
           ;; Symbol is not in a package the current package can inherit from, or is shadowed.
           ;; Print it with a prefix, and see whether we need a #: prefix.
           (multiple-value-bind (nil flag)
               (find-symbol symbol sym-pkg)
             (values sym-pkg (eq flag :internal)))))))

;; Copied from LAD: RELEASE-3.SYS; CLPACK.LISP#212 on 26-Mar-87 16:16:39
(defun find-all-symbols (string)
  "Returns a list of all symbols in any packages whose names match STRING, counting case."
  (let (accum)
    (dolist (pkg *all-packages*)
      (multiple-value-bind (sym foundp)
          (intern-local-soft string pkg)
        (when foundp (pushnew sym accum :test 'eq))))
    accum))

(defun where-is (pname &optional (under-pkg *all-packages*)
                 &aux found-in-pkg from-pkgs return-list table)
  "Find all symbols with a given pname, which packages they are in,
and which packages they are accessible from.
If UNDER-PKG is specified, search only packages inheriting from UNDER-PKG.
If PNAME is a string, it is converted to upper case."
  ;; Given a string, it should probably be uppercased.  But given a symbol copy it exactly.
  (setq pname (if (stringp pname) (string-upcase pname) (string pname)))
  (format t "~&")
  ;; Each entry in TABLE is (from-pkg found-in-pkg).  Highest package first.
  (dolist (pkg (if (atom under-pkg)
                   (package-used-by-list under-pkg)
                 under-pkg))
    (multiple-value-bind (sym found found-in-pkg)
        (intern-soft pname pkg)
      (when found
        (push (list pkg found-in-pkg) table)
        (pushnew sym return-list :test 'eq))))
  (setq table (nreverse table))
  (if (null table) (format t "No symbols named ~S exist.~%" pname)
    (do () ((null table))
      (setq found-in-pkg (cadar table)
            from-pkgs (sort (mapcan (lambda (x)
                                      (when (eq (cadr x) found-in-pkg)
                                        (setq table (delq x table 1))
                                        (ncons (pkg-name (car x)))))
                                    table)
                            #'string-lessp))
      (format t "~A:~A is accessible from package~P ~{~<~%~10@T~2:;~A~>~^, ~}~%"
              (pkg-name found-in-pkg) pname (length from-pkgs) from-pkgs)))
  return-list)

;;;; Just for compatibility.

(defun mapatoms (function &optional (pkg *package*) (inherited-p t))
  "Call FUNCTION on each symbol in package PKG.
If INHERITED-P is supplied as NIL, symbols inherited from other packages are not included."
  (if inherited-p
      (do-symbols (sym (pkg-find-package pkg))
        (funcall function sym))
    (do-local-symbols (sym (pkg-find-package pkg))
      (funcall function sym))))

;;; MAPATOMS over all packages in the world.
(defun mapatoms-all (function &optional (top-pkg pkg-global-package))
  "Call FUNCTION on each symbol in TOP-PKG and all packages that USE it.
TOP-PKG defaults to Lisp.  Packages USEd by TOP-PKG are not included."
  (setq top-pkg (pkg-find-package top-pkg))
  (dolist (pkg (cons top-pkg (pkg-used-by-list top-pkg)))
    (do-local-symbols (sym pkg)
      (funcall function sym))))

(defun pkg-goto (&optional (pkg pkg-user-package) globallyp
                 ;reference is a new feature 7/9/87 which allows you to
                 ;stay within an environment.  It is passed thru to PKG-FIND-PACKAGE.
                 reference)     ;Go to type-in package.
  "Set the current binding of *PACKAGE* to the package you specify (by name).
If GLOBALLY-P is non-NIL, then we do a PKG-GOTO-GLOBALLY as well."
  (let ((pk (pkg-find-package pkg () reference)))
    (when (or ;(pkg-auto-export-p pk)  ;let the guy go ahead, if he gets errors ...
              (pkg-read-lock-p pk))
      (ferror nil "Package ~A is ~:[read locked~;auto-exporting~]; it should not be the current package."
              pk (pkg-auto-export-p pk)))
    (and globallyp (pkg-goto-globally pk))
    (setq *package* pk)))

(defun in-package (name &rest options &key use nicknames (reference *package*))
  (declare (arglist name &key nicknames (use '("GLOBAL")) (size #o200) shadow export))
  (let ((pkg (pkg-find-package name () reference)))
    (cond ((and pkg options)
           (progn (use-package use pkg)
                  (pkg-add-nicknames pkg nicknames)))
          ;; if no options are supplied, and the package already exists, just do a pkg-goto
          (pkg)
          (t (setq pkg (apply #'make-package name options))))
    (pkg-goto pkg () reference)))

(defun pkg-goto-globally (&optional (pkg pkg-user-package))
  "Set the global binding of *PACKAGE* used by new lisp listeners
and by random processes that don't bind *PACKAGE*."
  (let ((*package* *package*))                  ;do error check
    (setq pkg (pkg-goto pkg)))
  (setq-globally *package* pkg))

(defmacro package-declare (&rest declaration)
  "Obsolete function used to define packages. Use DEFPACKAGE instead"
  (declare (arglist &quote package-name superior-name size nil &rest body))
  (pkg-process-declaration declaration)
  `(pkg-process-declaration ',declaration))
(make-obsolete package-declare "Use DEFPACKAGE instead.")

(defun pkg-process-declaration (declaration &aux name super size file-alist body tem
                                            (*package* *package*)
                                            ;; Make sure nothing happens while rehashing, etc.
                                            (inhibit-scheduling-flag t))
    (setq name (first declaration)
          super (if (second declaration)
                    (unless (string-equal (second declaration) "NONE")
                      (pkg-find-package (second declaration)))
                  pkg-global-package)
          size (or (third declaration) 1000)
          file-alist (fourth declaration)
          body (cddddr declaration))
    ;; Look for any existing package with the same name and superior,
    ;; and if there is one make this declaration apply to it
    ;; (and make it larger if it isn't as large as this decl says).
    ;; Otherwise, create a package.
    (if (setq tem (find-package name))
        (setq *package* (if ( (pkg-max-number-of-symbols tem) size)
                            tem
                          (pkg-rehash tem size)))
      (setq *package* (make-package name :super super :use nil :size size)))
    (when file-alist
      (ferror "Non-null file-alist in declaration of package ~A." *package*))
    ;; Process the body only if this is the first time this package is declared.
    (unless (pkg-declared-p *package*)
      (dolist (elt body)
        (selector (car elt) string-equal
          ("SHADOW" (shadow (cdr elt)))
          ("EXTERNAL" nil)
          ("INTERN"
           (dolist (str (cdr elt))
             (intern (string str))))
          ("BORROW"
           (let ((otherpkg (pkg-find-package (cadr elt))))
             (dolist (str (cddr elt))
               (intern-local (intern (string str) otherpkg)))))
          ("REFNAME"
           (setf (pkg-refname-alist *package*)
                 (del (lambda (x y) (eq x (car y)))
                      (string (cadr elt))
                      (pkg-refname-alist *package*)))
           (push (cons (string (cadr elt))
                       (pkg-find-package (caddr elt)))
                 (pkg-refname-alist *package*)))
          ("MYREFNAME"
           (if (equal "GLOBAL" (string (cadr elt)))
               ;; (MYREFNAME GLOBAL FOO) means make FOO a nickname of this package.
               (unless (or (equal (string (caddr elt)) (pkg-name *package*))
                           (member-equal (string (caddr elt))
                                         (pkg-nicknames *package*)))
                 (if (find-package (string (caddr elt)) nil)
                     (ferror "A package named ~S already exists." (caddr elt)))
                 (push (string (caddr elt))
                       (pkg-nicknames *package*)))
             ;; (MYREFNAME BAR FOO) means make FOO a local nickname
             ;; in BAR for this package.
             (setf (pkg-refname-alist (pkg-find-package (cadr elt)))
                   (del (lambda (x y) (eq x (car y)))
                        (string (caddr elt))
                        (pkg-refname-alist (pkg-find-package (cadr elt)))))
             (push (cons (string (caddr elt)) package)
                   (pkg-refname-alist (pkg-find-package (cadr elt))))))
          (t (ferror "~S is not supported as a PACKAGE-DECLARE option."
                     (car elt)))))
      (setf (pkg-declared-p *package*) t)))


;;;; Installation

(defconst initial-packages
  ;Note! See also kludgey list below if adding new packages.
   '((KEYWORD :nicknames ("") :size 5600. :use () :new-symbol-function pkg-keyword-store)
     (GLOBAL :nicknames ("ZETALISP" "ZL") :size 2000. :use () :auto-export-p t)
     (SYSTEM :nicknames ("SYS") :size 2000. :auto-export-p t)
     (LISP :nicknames (CL COMMON-LISP) :use () :size 800.
           :auto-export-p t :properties (read-lock t))
     (SYSTEM-INTERNALS :size 9600. :use ("GLOBAL" "SYS") :nicknames ("SI") :prefix-name "SI")
     (COMPILER :size 2800. :use (sys global) :shadowing-import (compiler:warn))
     (USER :size 2000.)
     (GARBAGE-COLLECTOR :nicknames (GC) :prefix-name "GC"
                        :size 300. :use (GLOBAL SYSTEM SI)
                        :shadow ("BASE" "STATUS")
                        :import-from (SI clear-meter %transporter-words-copied
                                         %region-origin %region-area %region-bits
                                         %region-length %region-type %region-gc-pointer
                                         %region-flip-enable %region-scavenge-enable
                                         %region-scavenge-carefully %region-volatility
                                         %region-free-pointer
                                         %free-region %32-bit-difference %deallocate-end-of-region
                                         %region-list-thread
                                         %area-region-list
                                         with-quick-region-area-accessors
                                         for-every-region-in-area
                                         unallocated-space for-every-region))
     ("CHAOS" :size 1200. :use ("GLOBAL" "SYS")
              :shadow ("OPEN" "STATUS" "CLOSE" "LISTEN" "FINISH"))
     ("ETHERNET" :size 800. :use ("GLOBAL" "SYS"))
     ("COLOR" :size 250. :use ("GLOBAL" "SYS"))
     ("FILE-SYSTEM" :size 3200. :use ("GLOBAL" "SYS") :nicknames ("FS") :prefix-name "FS")
     ("TAPE" :size 800. :use ("GLOBAL"))
     ("QFASL-REL" :size 10. :use ("GLOBAL" "SYS")
                  :shadow ("READ-BYTE" "WRITE-BYTE" "WRITE-STRING"))
     ("METER" :size 400. :use ("GLOBAL" "SYS"))
     ("TV" :size 4000. :use ("GLOBAL" "SYS"))
     ("EH" :size 1500. :use ("GLOBAL" "SYS") :nicknames ("DBG" "DEBUGGER")
           :shadow ("ARG"))
     ("FED" :size 800. :use ("GLOBAL" "SYS"))
     ("FONTS" :size 200. :use nil :auto-export-p t
              :new-symbol-function pkg-specialize-and-auto-export-store)
     ("TIME" :size 100.)
     ("SUPDUP" :size 500.)
     ("PRESS" :size 10.)
     ("FORMAT" :size 400.)
     ("ZWEI" :size 7000. :shadow ("SEARCH")); "FIND"
     ("MICRO-ASSEMBLER" :size 100. :nicknames ("UA") :prefix-name "UA"
                        :shadow ("FIXNUM" "INCLUDE" "MERGE" "AREA-LIST"))
     ("MATH" :size 200.)
     ("HACKS" :size 1500.)
     ("SRCCOM" :size 100. :shadow ("FILE-LENGTH"))
     ("UNIX" :size 100. :use ("GLOBAL"))
     ("CLI" :size 64. :use () :auto-export-p t :properties (read-lock t))
           ;;; No longer needed :import-from (lisp error terpri)) ; temporary
     ("ZLI" :size 64. :use () :auto-export-p t :properties (read-lock t))
     ;; Brain death
     ("ARRAY" :size 32.)
     ("CADR" :size 10. :use ("GLOBAL" "SYS") :nicknames ("CC"))
     ))


;;; Create the packages that should initially exist,
;;; and fill them with the appropriate symbols.
(defun pkg-initialize ()
  (unless (variable-boundp pkg-area) (make-area :name 'pkg-area :region-size #o200000))
  ;; Make these first two by hand since variables must be bound for rest of the stuff to work.
  (flet ((create (names use size)
           (let* ((table-size (pkg-good-size size))
                  (pkg (pkg-make-package
                         :make-array (:length (list table-size 2)
                                      :area pkg-area)
                         :name (string-append (car names))
                         :nicknames (mapcar #'string-append (cdr names))
                         :use-list use
                         :all-packages-pointer '*all-packages*
                         :number-of-symbols 0
                         :max-number-of-symbols size)))
             (push pkg *all-packages*)
             (dolist (p use) (push pkg (pkg-used-by-list p)))
             pkg)))
    (setq pkg-keyword-package (create '(KEYWORD "") () 5600.))
    (setf (pkg-new-symbol-function pkg-keyword-package) 'pkg-keyword-store)
    (setq pkg-global-package (create '(GLOBAL ZETALISP ZL) () 2000.))
    (setq pkg-system-package (create '(SYSTEM SYS) (list pkg-global-package) 2000.))
    (setq pkg-lisp-package (create '(LISP CL COMMON-LISP) () 800.))
    (setq pkg-system-internals-package (create '(SYSTEM-INTERNALS SI)
                                               (list pkg-global-package pkg-system-package)
                                               9600.))
    (setq pkg-compiler-package (create '(COMPILER) (list pkg-global-package pkg-system-package) 2800.))
    (setq pkg-user-package (create '(USER) (list pkg-global-package) 2000.))

    (flet ((initial-intern (s p)
             (let ((hash (pkg-string-hash-code (symbol-name s)))
                   index)
               (multiple-value-bind (foo bar)
                   (pkg-intern-internal (symbol-name s) hash p)
                 (if bar
                     (if (neq s foo) (ferror "Foo" s))
                   ;; not already interned
                   (setq index (nth-value 3 (pkg-intern-store hash s p)))
                   ;; export it withoutdoing any checking
                   (setf (pkg-slot-code p index)
                         (pkg-make-code 1 (pkg-slot-code p index)))))
               s)))
      ;; There really should be a better way of doing this stuff
      (let ((cli (create '(CLI) () 64.))
            (zli (create '(ZLI) () 64.)))
  ;*** when adding things here be sure to add CL: prefix in SYS:COLD;LISP.
        (dolist (s '(cli://     cli:*DEFAULT-PATHNAME-DEFAULTS*
                     cli:AR-1   cli:AR-1-FORCE
                     cli:AREF   cli:ASSOC
                     cli:ATAN   cli:CHARACTER
                     cli:CLOSE  cli:DEFSTRUCT
                     cli:DELETE cli:FUNCTIONP
                     cli:EVERY  cli:INTERSECTION
                     cli:LISTP  cli:MAP
                     cli:MEMBER cli:NINTERSECTION
                     cli:NLISTP cli:NUNION
                     cli:RASSOC cli:READ
                     cli:READ-FROM-STRING
                     cli:REM    cli:REMOVE
                     cli:SOME   cli:SUBST
                     cli:UNION
                     cli:MAKE-ARRAY cli:GETHASH cli:MAKE-PACKAGE cli:TIME cli:IF
                     ))
          (initial-intern s pkg-lisp-package)
          (initial-intern s cli))
  ;*** when adding things here be sure to add ZL: prefix in SYS:COLD;GLOBAL.
        (dolist (s '(zl://      zl:*DEFAULT-PATHNAME-DEFAULTS*
                     zl:AR-1    zl:AR-1-FORCE
                     zl:AREF    zl:ASSOC
                     zl:ATAN    zl:CHARACTER
                     zl:CLOSE   zl:DEFSTRUCT
                     zl:DELETE  zl:FUNCTIONP
                     zl:EVERY   zl:INTERSECTION
                     zl:LISTP   zl:MAP
                     zl:MEMBER  zl:NINTERSECTION
                     zl:NLISTP  zl:NUNION
                     zl:RASSOC  zl:READ
                     zl:READ-FROM-STRING
                     zl:REM     zl:REMOVE
                     zl:SOME    zl:SUBST
                     zl:UNION
                     zl:MAKE-ARRAY zl:GETHASH zl:MAKE-PACKAGE zl:TIME zl:IF
                     ))
          (initial-intern s pkg-global-package)
          (initial-intern s zli)))

      ;; Put in the symbols that are supposed to be in GLOBAL and SYSTEM.
      (dolist (s (symbol-value 'initial-global-symbols))
        (initial-intern s pkg-global-package))
      (makunbound 'initial-global-symbols)
      (dolist (s (symbol-value 'initial-lisp-symbols))
        (initial-intern s pkg-lisp-package))
      (makunbound 'initial-lisp-symbols)
      (dolist (s (symbol-value 'initial-system-symbols))
        (initial-intern s pkg-system-package))
      (makunbound 'initial-system-symbols))

    ;; Put system variables and system constants on the SYSTEM package
    ;; (unless they are already in the GLOBAL package.)
    (dolist (list system-variable-lists)
      (dolist (var (symbol-value list))
        (export var pkg-system-package)))
    (dolist (list system-constant-lists)
      (dolist (var (symbol-value list))
        (export var pkg-system-package)))
    (dolist (var a-memory-counter-block-names)
      (export var pkg-system-package))
    (dolist (var (g-l-p (symbol-function 'micro-code-symbol-name-area)))
      (export var pkg-system-package))

    ;; Now all other system symbols go in the SYSTEM-INTERNALS package, unless
    ;; the cold-load has specified a different place for them to go.
    (mapatoms-nr-sym
      (lambda (sym)
        (when (null (symbol-package sym))
          (intern-local sym pkg-system-internals-package))))

    ;; we do this followed by alter-package rather than a single make-package to
    ;;   avoid lossage of various flavours.  Think about it.
    (mapc (lambda (x)
            (create (car x) (mapcar #'find-package (cadr x)) (caddr x)))
          `(((GARBAGE-COLLECTOR GC) (global sys si) 300.)
            ((CHAOS) (GLOBAL SYS) 1200.)
            ((ETHERNET) (GLOBAL SYS) 800.)
            ((NETWORK NET) (GLOBAL SYS) 800.)
            ((COLOR) (GLOBAL SYS) 250.)
            ((FILE-SYSTEM FS) (GLOBAL SYS) 3200.)
            ((TAPE) (GLOBAL) 800.)
            ((QFASL-REL) (GLOBAL SYS) 10.)
            ((METER) (GLOBAL SYS) 400.)
            ((TV) (GLOBAL SYS) 4000.)
            ((EH DBG DEBUGGER) (GLOBAL SYS) 1500.)
            ((FED) (GLOBAL SYS) 800.)
            ((FONTS) () 200.)
            ((TIME) (GLOBAL) 100.)
            ((SUPDUP) (GLOBAL) 500.)
            ((PRESS) (GLOBAL) 10.)
            ((FORMAT) (GLOBAL) 400.)
            ((ZWEI) (GLOBAL) 7000.)
            ((MICRO-ASSEMBLER UA) (GLOBAL) 100.)
            ((MATH) (GLOBAL) 200.)
            ((HACKS) (GLOBAL) 1500.)
            ((SRCCOM) (GLOBAL) 100.)
            ((UNIX) (GLOBAL) 100.)
            ((CADR CC) (GLOBAL) 10.)
            ((ARRAY) () 10.)
            ; NO!
            ;((LAMBDA LAM) (GLOBAL) 4000.)
            )))
  (setf (pkg-new-symbol-function (find-package 'fonts)) 'pkg-specialize-and-auto-export-store)

  (mapatoms-nr-sym
    (lambda (sym &aux sym1 pkg)
      (cond ((arrayp (setq pkg (symbol-package sym)))) ;already interned in a package
            ((or (null pkg)
                 (null (setq pkg (find-package pkg))))
             (ferror "Loses" sym))
            (t
             (setf (symbol-package sym) nil)))
      (setq sym1 (intern-local sym pkg))
      (unless (eq sym1 sym)
        (ferror "Lossage" sym sym1))))

  (dolist (description initial-packages) (apply #'alter-package description))

  ;; We have packages!!
  (setq *package* pkg-user-package)
  (setf (aref (symbol-function 'system-communication-area) %sys-com-obarray-pntr)
        '*package*)                             ;For Console program

  (setq array-type-keywords                     ;Huh?!
        (loop for a in array-types
              collect (intern (symbol-name a) pkg-keyword-package)))
  t)

(defun linearize-package-data ()
  (without-interrupts
    (let ((default-cons-area pkg-area))
      (setq *all-packages* (copy-list *all-packages*))
      ;; Put all nickname lists on one page, all names and nicknames on one page.
      (dolist (pkg *all-packages*)
        (setf (pkg-name pkg)
              (copy-object-tree (pkg-name pkg)))
        (setf (pkg-nicknames pkg)
              (copy-object-tree (pkg-nicknames pkg))))
      ;; Put all lists of local names together with the above, and also all the local names.
      (dolist (pkg *all-packages*)
        (setf (pkg-refname-alist pkg)
              (copy-alist (pkg-refname-alist pkg)))
        (dolist (elt (pkg-refname-alist pkg))
          (setf (car elt)
                (copy-object-tree (car elt)))))
      (dolist (pkg *all-packages*)
        (setf (pkg-use-list pkg)
              (copy-list (pkg-use-list pkg)))
        (setf (pkg-used-by-list pkg)
              (copy-list (pkg-used-by-list pkg)))
        (setf (pkg-shadowing-symbols pkg)
              (copy-list (pkg-shadowing-symbols pkg)))
        (setf (pkg-plist pkg)
              (copy-list (pkg-plist pkg)))))))

(add-initialization 'linearize-package-data '(linearize-package-data) '(:full-gc))

;;; Useful for forking things once the world has been built
(defun make-incompatible-cl-symbols (names)
  (let ((lisp (find-package 'lisp))
        (global (find-package 'global))
        (cli (find-package 'cli))
        (new-symbols nil))
    (mapc #'(lambda (name)
              (let ((symbol (find-symbol name lisp)))
                (if (eq (symbol-package symbol) lisp)
                    (format *terminal-io* "~&~S already in LISP package" symbol)
                  (unintern symbol lisp)
                  (push (intern (intern name lisp) cli) new-symbols))))
          names)
    (let ((cl-readtables (list (find-readtable-named "CL")
                               common-lisp-readtable
                               initial-common-lisp-readtable))
          (%inhibit-read-only t)) ;; Grr..
      (mapc #'(lambda (rt)
                (setf (getf (rdtbl-plist rt) 'symbol-substitutions)
                      (nconc (mapcar #'(lambda (lisp-symbol)
                                         (cons (find-symbol (symbol-name lisp-symbol) global)
                                               lisp-symbol))
                                     new-symbols)
                             (getf (rdtbl-plist rt) 'symbol-substitutions))))
            cl-readtables))
    new-symbols))

(defun package-root (&optional pkg)
  (if (null pkg) (setq pkg *package*))
  (or (pkg-root pkg)
      (pkg-find-package 'global nil pkg)))

(defun package-root-name (&optional pkg)
  (if (null pkg) (setq pkg *package*))
  (package-name (package-root pkg)))

(defun package-primary-name (&optional pkg)
  (if (null pkg) (setq pkg *package*))
  (let ((prefix (pkg-pfx pkg)))
    (cond ((or (null prefix)
               (eq pkg (pkg-root pkg)))
           (package-name pkg))
          (t
           (substring (package-name pkg) (string-length prefix))))))
