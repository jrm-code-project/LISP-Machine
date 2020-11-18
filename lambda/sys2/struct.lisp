;;;  -*- Mode:LISP; Package:SYSTEM-INTERNALS; Lowercase:NIL; Fonts:(CPTFONT CPTFONTSH); Readtable:ZL; Base:8  -*-

;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **
;;;     ** (c) Copyright 1981 Massachusetts Institute of Technology **
;;;     ** (foo) 1983

;;; The current Lisp machine copy is in SYS:SYS2;STRUCT LISP
;;;    (OZ:PS:<L.SYS2>STRUCT.LISP at MIT)
;;; this version implements common-lisp-style-defstruct for mit lisp machine lisp

;;; These are the original bawden sources for this code
;;;   -- they have not been munged for common lisp
;;; The master copy of this file is in MC:ALAN;NSTRUCT >
;;; The current PDP10 MacLisp copy is in MC:ALAN;STRUCT >
;;; The current Multics MacLisp copy is in >udd>Mathlab>Bawden>defstruct.lisp
;;;  on MIT-Multics
;;; The current VMS-NIL copy is in [NIL.SRC.SPEC]STRUCT.LSP on HTJR

;;; *****  READ THIS PLEASE!  *****
;;; If you are thinking of munging anything in this file you might want to
;;; consider finding me (ALAN) and asking me to mung it for you.  There is more
;;; than one copy of this file in the world (it runs in PDP10 and Multics MacLisp,
;;; NIL, Franz, PSL and on LispMachines) and whatever amazing features you are
;;; considering adding might be usefull to those people as well.  If you still
;;; cannot contain yourself long enough to find me, AT LEAST send me a piece of
;;; mail describing what you did and why.  Thanks for reading this flame.
;;;                                Alan Bawden (ALAN@MC)


;;; **** I READ THAT! ****
;;;  Date: Monday, 26 September 1983  18:59-EDT
;;;  From: Alan Bawden <ALAN at MIT-MC>
;;;  To:   Mly at MIT-ML
;;;  Re:   structural problems
;;;
;;;         ... I expect to make a common-lisp compatible defstruct myself
;;;  someday, which will probably run in the LMIT system just as it will in the
;;;  Franz, NIL, symbolics, PSL and MacLisp dialects.
;;;
;;;  Since I refuse to cooperate with RMS on anything, and the LMIT system is
;;;  RMS's personal project, I don't care what you do to defstruct, just as long
;;;  as you don't modify any of my copies.


;;; **** SO WHY DID I BOTHER? ****
;;; The following code is only tested to run in the mit lispmachine system (sys 98 and later)
;;; I have tried not to incorporate much code that will render future compatibility-
;;;   hacking too difficult
;;; Fascism and bickering are alive and well. Sigh.
;;; I just frobbed the code.


1#-(and lispm mit)
(eval-when (eval compile)*
  1(cond ((and (status feature MacLisp) (status feature PDP10))*
        1(sstatus feature MacLisp-10))*
       1(t (sstatus nofeature MacLisp-10))))

#-(and lispm mit)
(eval-when (compile)*
  1(cond ((status feature ITS)*
        1(load '|alan;lspenv init|))*
       1((status feature Multics)*
        1(load '|>udd>Mathlab>Bawden>lspenv.lisp|))))

#+MacLisp-10
(cond ((status nofeature noldmsg)
       (terpri msgfiles)
       (princ '#.(and (status feature MacLisp-10)*
                 1(maknam (nconc (exploden ";Loading DEFSTRUCT ")*
                             1(exploden (caddr (truename infile))))))*
           1msgfiles)))

#+NIL
(herald defstruct)

#+Multics
(declare (genprefix defstruct-internal-)*
       1(macros t))

#+MacLisp
(eval-when (eval compile)*
  1(setsyntax #/: (ascii #\space) nil))

#-(and lispm mit)
(eval-when (eval)*
  1;;So we may run the thing interpreted we need the simple*
  1;;defstruct that lives here:*
  1(cond ((status feature ITS)*
        1(load '|alan;struct initial|))*
       1((status feature Multics)*
        1(load '|>udd>Mathlab>Bawden>initial_defstruct|))))

#-(and lispm mit)
(eval-when (compile)*
  1;;To compile the thing this probably is an old fasl: (!)*
  1(cond ((status feature ITS)*
        1(load '|alan;struct boot|))*
       1((status feature Multics)*
        1(load '|>udd>Mathlab>Bawden>boot_defstruct|))))

#+Multics
(defun nth (n l)*
  1(do ((n n (1- n))*
      1(l l (cdr l)))*
     1((zerop n) (car l))))

#+Multics
(defun nthcdr (n l)*
  1(do ((n n (1- n))*
      1(l l (cdr l)))*
     1((zerop n) l)))

#+Multics
(defun displace (x y)*
  1(cond ((atom y)*
        1(rplaca x 'progn)*
        1(rplacd x (list y)))*
       1(t*
        1 (rplaca x (car y))*
        1 (rplacd x (cdr y))))*
  1x)*

(eval-when (eval compile load)

1#+MacLisp
(defun defstruct-retry-keyword (x)*
  1(let ((l (exploden x)))*
    1(if (= (car l) #/:)*
       1(implode (cdr l))*
      1x)))*

#+LispM
(defun defstruct-retry-keyword (x)
  (intern (symbol-name x) si:pkg-keyword-package))

1#+NIL
(defmacro defstruct-retry-keyword (x)*
  1`(to-keyword ,x))*

#+(or (and LispM MIT) NIL)
(defsubst defstruct-divide (x y) (truncate x y))

1#-(or (and LispM MIT) nil)
(defsubst defstruct-divide (x y) (// x y))*

#+(AND LISPM MIT)
(DEFSUBST DEFSTRUCT-LISTP (X) (CLI:LISTP X))

1#+(AND LISPM (NOT MIT))
(DEFSUBST DEFSTRUCT-LISTP (X) (CL-LISTP X))

#+NIL
(DEFSUBST DEFSTRUCT-LISTP (X) (LISTP X))*

);End of eval-when (eval compile load)

(eval-when (eval compile load)
#+LispM
(defun defstruct-append-symbols (&rest args)
  (intern (apply 'string-append args)))
)

;;; Eval this before attempting incremental compilation
(eval-when #+(AND LISPM MIT) (EVAL COMPILE LOAD)        ;less lossage for patching
           1#-(AND LISPM MIT) (eval compile)

#+MacLisp-10
(defmacro defstruct-append-symbols args*
  1(do ((l (reverse args) (cdr l))*
      1(x)*
      1(a nil (if (or (atom x)*
                  1(not (eq (car x) 'quote)))*
               1(if (null a)*
                  1`(exploden ,x)*
                 1`(nconc (exploden ,x) ,a))*
             1(let ((l (exploden (cadr x))))*
               1(cond ((null a) `',l)*
                    1((= 1 (length l)) `(cons ,(car l) ,a))*
                    1(t `(append ',l ,a)))))))*
     1((null l) `(implode ,a))*
    1(setq x (car l))))

#+Multics
(defmacro defstruct-append-symbols args*
  1`(make_atom (catenate ,@args)))

#+NIL
(defmacro defstruct-append-symbols args*
  1`(symbolconc ,@args))*

#+(and lispm mit)
(defmacro defstruct-putprop-compile-time (sym val ind)
  `(push `(defdecl ,,sym ,,ind ,,val) returns))

#-(and lispm mit)
;;; This loses.  The symbol is somehow re-interned (I think this is
;;; caused by the use of double ``  In any case, the package prefix
;;; is lost and the symbol gets interned in the wrong package.
;;; This shows up when we use the :INCLUDE option with a defstruct that
;;; is located in a package that does not :USE global.
;(defmacro defstruct-putprop-compile-time (sym val ind)
;  `(push `(eval-when (compile load eval) (defprop ,,sym ,,val ,,ind)) returns))

(defmacro defstruct-putprop-compile-time (sym val ind)
  ;; This is rather revolting, and probably can't work.
  ;; If we are cannot print readably here, we will lose.
  ;; Fortunately, I think we never do something like this.
  `(push `(eval-when (compile load eval)
            (putprop (read-from-string ,(format nil "~s" ,sym))
                     (read-from-string ,(format nil "~s" ,val))
              (read-from-string ,(format nil "~s" ,ind)))) returns))


(defmacro defstruct-putprop (sym val ind)
  `(push `(defprop ,,sym ,,val ,,ind) returns))

(defmacro defstruct-putprop (sym val ind)
  `(push `(defprop ,,sym ,,val ,,ind) returns))


1#+Multics
;;;lcp gobbles (defprop ... macro) at compile time, so we have to use
;;;putprop to be certain macro definitions make it into the object:
(defmacro defstruct-put-macro (sym fcn)*
  1`(push `(putprop ',,sym ',,fcn 'macro) returns))

#+MacLisp-10
(defmacro defstruct-put-macro (sym fcn)*
  1`(push `(defprop ,,sym ,,fcn macro) returns))

#+(and LispM (NOT MIT))
(defmacro defstruct-put-macro (sym fcn)*
  1(setq fcn (if (and (not (atom fcn))*
                 1(eq (car fcn) 'quote))*
             1`'(macro . ,(cadr fcn))*
            1`(cons 'macro ,fcn)))*
  1`(push `(fdefine ',,sym ',,fcn t) returns))*

#+(and lispm MIT)
(defmacro defstruct-put-macro (sym fcn)
  (setq fcn (if (and (not (atom fcn))
                     (eq (car fcn) 'quote))
                `'(macro . ,(cadr fcn))
              `(cons 'macro ,fcn)))
  `(push `(deff-macro ,,sym ',,fcn) returns))

1#+NIL
(defmacro defstruct-put-macro (sym fcn)*
  1`(push `(add-macro-definition ',,sym ',,fcn) returns))*

(defmacro defstruct-make-empty () `'%%defstruct-empty%%)

(defmacro defstruct-emptyp (x) `(eq ,x '%%defstruct-empty%%))


;;; Here we must deal with the fact that error reporting works
;;; differently everywhere!

1#+MacLisp-10
;;;first arg is ALWAYS a symbol or a quoted symbol:
(defmacro defstruct-error (message &rest args)*
  1(let* ((chars (nconc (exploden (if (atom message)*
                             1message*
                           1(cadr message)))*
                  1'(#/.)))*                  1;"Bad frob" => "Bad frob."*
        1(new-message*
          1(maknam (if (null args)*
                   1chars*
                  1(let ((c (car chars)))*            1;"Bad frob." => "-- bad frob."*
                    1(or (< c #/A)*
                       1(> c #/Z)*
                       1(rplaca chars (+ c #o40)))*
                    1(append '(#/- #/- #\space) chars))))))*
    1`(error ',new-message*
          1,@(cond ((null args) `())*
                1((null (cdr args)) `(,(car args)))*
                1(t `((list ,@args)))))))

#+Multics
;;;first arg is ALWAYS a string:
(defmacro defstruct-error (message &rest args)*
  1`(error ,(catenate "defstruct: "*
                 1message*
                 1(if (null args)*
                    1"."*
                   1": "))*
        1,@(cond ((null args) `())*
              1((null (cdr args)) `(,(car args)))*
              1(t `((list ,@args))))))*

#+(or LispM NIL)
;;; first arg is ALWAYS a string:
(defmacro defstruct-error (message &rest args)
  (do ((l args (cdr l))
       (fs "")
       (na nil))
      ((null l)
      `(ferror nil
               ,(string-append message
                               (if (null args)
                                   "."
                                   (string-append ":" fs)))
               ,.(nreverse na)))
    (cond ((and (not (atom (car l)))
                (eq (caar l) 'quote)
                (symbolp (cadar l)))
           (setq fs (string-append fs " " (string-downcase (cadar l)))))
          (t
           (push (car l) na)
           (setq fs (string-append fs " ~S"))))))

);End of eval-when (eval compile) [(eval compile load) for mit lispm]

;;; If you mung the the ordering af any of the slots in this structure,
;;; be sure to change the version slot and the definition of the function
;;; get-defstruct-description.  Munging the defstruct-slot-description
;;; structure should also cause you to change the version "number" in this
;;; manner.
(defstruct (defstruct-description
             (:type :list)
             (:default-pointer description)
             (:conc-name defstruct-description-)
             (:alterant ())
             1#+(AND stingy-defstruct (NOT (OR (AND LISPM MIT) NIL)))*
             1(:eval-when (eval compile))*)
  (version 'one)
  type
  dummy                                         ;used to be the displace function
  slot-alist                                    ;format: ((var-1 . slot-desc-1) ...)
  named-p
  constructors
  (default-pointer nil)
  (but-first nil)
  size
  (property-alist nil)
  name
  ;; The MIT Lisp Machine microcode knows the index of this slot ("include")
  ;; for TYPEP-STRUCTURE-OR-FLAVOR.
  ;; ie Don't change slots up to here in mit lispm system without changing ucode!!!
  include
  (initial-offset 0)
  #+(OR (AND LISPM MIT) NIL)
    DUMMY                                       ;would like to flush this loser,
                                                ; (the option itself has already gone)
                                                ; but that would screw up old fasdumped
                                                ; definitions. Sigh.
  1#-(OR (AND LISPM MIT) NIL)*
    1(eval-when '(eval compile load))*
  alterant
  (conc-name nil)
  (callable-accessors 1#-(or LispM NIL) nil* #+(or LispM NIL) t)
  (size-macro nil)
  (size-symbol nil)
  (predicate nil)
  (copier nil)
  (print nil)
  (CALLABLE-CONSTRUCTORS NIL)                   ;defaults to T for common lisp
  (SUBTYPE NIL)
  )

(defun get-defstruct-description (name)
  (let ((description (getdecl name 'defstruct-description)))
    (cond ((null description)
           (defstruct-error
             "A structure with this name has not been defined" name))
          ((not (eq (defstruct-description-version) 'one))
           (defstruct-error "The internal description of this structure is
incompatible with the currently loaded version of DEFSTRUCT,
you will need to recompile its definition"
                  name))
          (t description))))

;;; See note above defstruct-description structure before munging this one.
(defstruct (defstruct-slot-description
             (:type :list)
             (:default-pointer slot-description)
             (:conc-name defstruct-slot-description-)
             (:alterant ())
             1#+(AND stingy-defstruct (NOT (OR (AND LISPM MIT) NIL)))*
             1(:eval-when (eval compile))*)
  number
  (ppss nil)    ;Byte specifier for byte field
  init-code
  (type T)      ;commonlisp universal supertype  -- was NOTYPE in original Bawden defstruct
  (property-alist nil)
  ref-macro-name
  DOCUMENTATION
  (READ-ONLY NIL)
  BITS          ;how many bits needed to store this slot -- not yet used
  )

;;; Perhaps this structure wants a version slot too?
(defstruct (defstruct-type-description
             (:type :list)
             (:default-pointer type-description)
             (:conc-name defstruct-type-description-)
             (:alterant ())
             1#+(AND stingy-defstruct (NOT (OR (AND LISPM MIT) NIL)))*
             1(:eval-when (eval compile))*)
  ref-expander
  ref-no-args
  cons-expander
  cons-flavor
  (cons-keywords nil)
  (named-type nil)
  (overhead 0)
  (defstruct-expander nil)
  (predicate nil)
  (copier nil)
  (DEFSTRUCT-KEYWORDS NIL)
  DOCUMENTATION
  )

#+LispM
(defprop defstruct "Structure" definition-type-name)

;;; The order of forms returned by defstruct is sometimes critical.  Keep this
;;; in mind when munging this code:
1#-(AND LISPM MIT)*                                   1;mit lispm doc is wrong elsewhere
(DEFMACRO DEFSTRUCT (OPTIONS &BODY ITEMS)*
  1(DEFSTRUCT-1 OPTIONS ITEMS NIL))*

#+(AND LISPM MIT)
(DEFMACRO DEFSTRUCT (OPTIONS &BODY ITEMS)
  "(DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
Options:
  :TYPE defaults to :ARRAY
    other useful types include :ARRAY-LEADER :TYPED-ARRAY :FIXNUM-ARRAY :FLONUM-ARRAY
    :LIST :LIST* :TREE
  :CONSTRUCTOR defaults to /"MAKE-<name>/"
    More than one constructor may be specified. The syntax for defining a constructor is
    either: (:CONSTRUCTOR <name> [doc-string]) or (:CONSTRUCTOR <name> <arglist> [doc-string])
    If no arglist is supplied, a constructor is defined which takes alternating slotnames
     and values (ie uses &KEY) as arguments and initializes those slots to those values.
    If an arglist is supplied, then the constructor defined will have this as its arglist.
     Meaningful lambda-list-keywords are &OPTIONAL &REST and &AUX.
     Use &AUX to initialize a slot to a value other then the usual default value.
    The first type of constructor behaves differently if :CALLABLE-CONSTRUCTORS is non-NIL.
    See below.
  :DEFAULT-POINTER defaults to empty (if no value given defaults to /"<name>/")
  :CONC-NAME defaults to empty (if no value given defaults to /"<name>-/")
    This what to prepend to the names of slots to obtain the names of the slot-accessor
    functions.
  :SIZE-SYMBOL defaults to empty (if no value given defaults to /"<name>-SIZE/")
  :SIZE-MACRO defaults to empty (if no value given defaults to /"<name>-SIZE/")
  :ALTERANT defaults to /"ALTER-<name>/"
  :BUT-FIRST see the manual
  :INCLUDE specifies a structure to include as a part of this structure.
  :PROPERTY (:property foo bar) gives the structure a foo property of bar.
            (:property foo) gives a foo property of T.
  :INITIAL-OFFSET can cause defstruct to skip over that many slots.
  :NAMED takes no value.  Tries to make the structure a named type.
  :CALLABLE-ACCESSORS defaults to T, which means that the slot-accessors may be called (they
   are defsubsts in this case) If NIL, the accessors are defined as macros.
  :CALLABLE-CONSTRUCTORS defaults to NIL -- the constructors are defined as macros.
    If non-NIL, they are functions, which is what Common Lisp wants.
    This will affect /"&KEY-style/" constructors, ie those which take alternating slotnames
     and values.
     If :CALLABLE-CONSTRUCTORS is T, those slotnames must be keywords, as the constructor
     is implemented as a function with an &KEY arglist. Keyword slotnames will ALSO work
     even if this is NIL, though non-keyword slotnames WILL NOT work if T.
     This also affects the quoting of arguments to the additional cons-keywords for these
     constructors. See the manual for details.
  :PREDICATE defaults to empty (if no value given defaults to /"<name>-P/").
    Generates a predicate if possible.
  :COPIER defaults to empty (if no value given defaults to /"COPY-<name>/").
    Generates a function to copy this structure.
  :PRINT (:print /"#<spaceship at ~S by ~S>/" (x-pos spaceship) (y-pos spaceship))
    <Name> is bound to the instance of the structure being printed. (/"spaceship/" in this
    example)
  :PRINT-FUNCTION The value should be a function of three arguments, the structure to be
    printed, the stream to print it on, and the current print-depth.
  <type> any type name can be used without a <val> instead of
    saying (:TYPE <type>)
  <other> any symbol which is specified as a :DEFSTRUCT-KEYWORD for the type of structure
    which we are creating."
  (DEFSTRUCT-1 OPTIONS ITEMS NIL))

;;; this should probably be #+nil too
#+(AND LISPM MIT)
(DEFMACRO CLI:DEFSTRUCT (OPTIONS &BODY ITEMS)
  "(DEFSTRUCT (<name> . <options>) . <slots>) or (DEFSTRUCT <name> . <slots>)
Options:
  :TYPE specifies the type of data used to represent the structure.
    It also specifies that you do not get something TYPEP can recognize!
    Possible values are VECTOR, (VECTOR <vector-type>) and LIST, as well as the types
    available for non-common-lisp defstruct.
  :NAMED takes no value.  When this accompanies :TYPE,
    you get a phony named structure which has the type stored in it
    but which TYPEP still cannot recognize.
  :CONSTRUCTOR defaults to /"MAKE-<name>/"
    More than one constructor may be specified. The syntax for defining a constructor
    is either: (:CONSTRUCTOR <name> [doc-string]) or
               (:CONSTRUCTOR <name> <arglist> [doc-string])
    If no arglist is supplied, a constructor is defined which takes alternating
     slotnames and values as arguments and initializes those slots to those values.
    If an arglist is supplied, then the constructor defined will have this as its
     arglist. Meaningful lambda-list-keywords are &OPTIONAL &REST and &AUX.
     Use &AUX to initialize a slot to a value other then the usual default value.
  :CONC-NAME defaults to <name>-. This what to prepend to the names of slots to obtain
    the names of the slot-accessor functions
  :INCLUDE specifies a structure to include as a part of this structure.
  :INITIAL-OFFSET can cause defstruct to skip over that many slots.
  :PREDICATE defaults to <name>-p.  Generates a predicate if possible. Give this option
    a value of NIL if you don't want a predicate.
  :PRINT-FUNCTION The value should be a function of three arguments, the structure to be
    printed, the stream to print it on, and the current print-depth.
  Many other (non-common-lisp, perhaps non-portable) options are available.
    See the documentation for GLOBAL:DEFSTRUCT."
  (DEFSTRUCT-1 OPTIONS ITEMS T))

(DEFUN DEFSTRUCT-1 (OPTIONS ITEMS CLIP)         ;CLIP means "common lisp, incompatible"-p
  (let* ((description (defstruct-parse-options options CLIP))
         (type-description (get (defstruct-description-type)
                                'defstruct-type-description))
         (name (defstruct-description-name))
         (DOC (AND (STRINGP (CAR ITEMS)) (POP ITEMS)))
         (new-slots (defstruct-parse-items items description))  ;now ALL slots -- mly
         (returns nil))
    ;; Keep the returns from this as close to last as possible
    ;;  ie Evaluate this before everything else
    (AND (defstruct-type-description-defstruct-expander)
         (setq returns (funcall (defstruct-type-description-defstruct-expander) description)))
    (SETQ RETURNS
          (APPEND RETURNS
                  ;; This must be the last returned form, since to compile it
                  ;; might require that the structure already be operable:
                  (IF (DEFSTRUCT-DESCRIPTION-PRINT)
                      (LIST (DEFSTRUCT-DEFINE-PRINTER NAME (DEFSTRUCT-DESCRIPTION-PRINT))))
                  ;; Return the name symbol as our value
                  `(',NAME)))
    1#+(and LispM (NOT MIT))*
    1(push `(record-source-file-name ',name 'defstruct) returns)*
    #+(and LispM MIT)
    (push `(eval-when (load eval) (record-source-file-name ',name 'defstruct)) returns)
    #+(OR (AND LISPM MIT) NIL)                  ;really any common lisp
    (AND DOC (PUSH `(SETF (DOCUMENTATION ',NAME 'STRUCTURE) ,DOC) RETURNS))
    (let ((alterant (defstruct-description-alterant))
          (size-macro (defstruct-description-size-macro))
          (size-symbol (defstruct-description-size-symbol))
          (predicate (defstruct-description-predicate))
          (copier (defstruct-description-copier)))
      (when predicate
        (push (funcall (or (defstruct-type-description-predicate)
                           (defstruct-error
                             "This DEFSTRUCT type cannot produce a predicate"
                             (defstruct-description-type) 'in name))
                       description
                       predicate)
              returns))
      (when copier
        (push
          (let ((copy-fun (defstruct-type-description-copier)))
            (cond (copy-fun
                   (funcall copy-fun description copier))
                  ((not (= 1 (defstruct-type-description-ref-no-args)))
                   (defstruct-error
                     "This defstruct type cannot produce a copying function"
                     (defstruct-description-type) 'in name))
                  (t (do ((i (1- (defstruct-description-size)) (1- i))
                          (l nil (cons (cons i
                                             (funcall
                                               (defstruct-type-description-ref-expander)
                                               i description 'x))
                                       l)))
                         ((< i 0)
                          `(defun ,copier (x)
                             ,(invoke-defstruct-constructor-expander
                                description type-description l nil)))))))
          returns))
      (when alterant
        (defstruct-put-macro alterant 'defstruct-expand-alter-macro)
        (defstruct-putprop-compile-time alterant name 'defstruct-name))
      (when size-macro
        (defstruct-put-macro size-macro 'defstruct-expand-size-macro)
        (defstruct-putprop-compile-time size-macro name 'defstruct-name))
      (when size-symbol
        (push `(DEFPARAMETER ,size-symbol
                             ,(+ (defstruct-description-size)
                                 (defstruct-type-description-overhead)))
              returns)))
    ;;what defstruct returns
    1#-(OR (AND LISPM MIT) NIL)*      ;retain eval-when for others so as not to cause hidden screws
    1`(eval-when ,(defstruct-description-eval-when)*
       1,.(defstruct-define-ref-macros new-slots description)*
       1,.(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)*
       1,.returns)*
    #+(OR (AND LISPM MIT) NIL)                  ;losing eval-when flushed!!
    `(PROGN
       ,@(defstruct-define-ref-macros new-slots description)
       ,(let ((returns))
          (defstruct-putprop-compile-time name description 'defstruct-description)
          (car returns))
       ,@(DEFSTRUCT-DEFINE-CONSTRUCTORS DESCRIPTION)
       ,@returns)))

;;; General philosophy on the :print option is to not bother the
;;; user if printing cannot be controlled.  This allows for
;;; portability without pain.  This may prove to be a bogus philosophy.
1#+MacLisp-10
(defun defstruct-define-printer (name rest)*
  1(let ((stream (gensym))*
       1(CLIP (POP REST)))*
    1(IF CLIP*
       1NIL*                                  ;don't know what to do in maclisp
      1`(defun (,name named-hunk-printer) (,name ,stream)*
         1(?format ,stream ,@rest)))))

#+(AND LISPM (NOT MIT))*                               1;losers
(DEFUN DEFSTRUCT-DEFINE-PRINTER (NAME REST &AUX (CLIP (POP REST)))*
  1(LET ((OP (GENSYM))*
       1(ARGS (GENSYM)))*
    1(IF CLIP*
       1`(DEFUN (,NAME NAMED-STRUCTURE-INVOKE) (,OP ,NAME &REST ,ARGS)*
          1(SELECTQ ,OP*
        1     (:PRINT-SELF*
              1(IF PRINT-READABLY (PRINT-NOT-READABLE ,NAME))*        1;not always right... Sigh*
              1(FUNCALL ,(CAR REST) ,NAME (CAR ,ARGS) (CADR ,ARGS)))*
        1     (:WHICH-OPERATIONS '(:PRINT-SELF :WHICH-OPERATIONS))*
        1     (T NIL)))*                              1;don't barf on weird operations*
      1`(DEFUN (,NAME NAMED-STRUCTURE-INVOKE) (,OP ,NAME &REST ,ARGS)*
         1(SELECTQ ,OP*
           1(:PRINT-SELF*
             1(IF PRINT-READABLY (PRINT-NOT-READABLE ,NAME))*
             1(FORMAT (CAR ,ARGS) ,@REST))*
        1    (:WHICH-OPERATIONS '(:PRINT-SELF :WHICH-OPERATIONS))*
        1    (T NIL))))))*

#+(AND LISPM MIT)
(DEFUN DEFSTRUCT-DEFINE-PRINTER (STRUCTURE-NAME REST)
  (LET ((CLIP (CAR REST)))
    (IF CLIP
        (LET ((STRUCTURE (MAKE-SYMBOL "STRUCTURE"))
              (STREAM (MAKE-SYMBOL "STREAM"))
              (DEPTH (MAKE-SYMBOL "DEPTH")))
          `(DEFSELECT ((:PROPERTY ,STRUCTURE-NAME NAMED-STRUCTURE-INVOKE) IGNORE)
             ;; I'd really like to flush the default message handler of IGNORE  here.
             ;; This is a hangover from the days when obscure operations like :sxhash
             ;; were invoked without so much as a glance at :which-operations.
             ;; Maybe change this once people have gotten used to being reasonable
             ;; about invoking operations on structures. Perhaps in system 100.
             (:PRINT-SELF (,STRUCTURE ,STREAM ,DEPTH &OPTIONAL IGNORE)
               ;; the above "ignore" is for old callers who passed *print-escape*
               (IF PRINT-READABLY
                   (PRINT-NOT-READABLE ,STRUCTURE)      ;not always right... Sigh
                 (FUNCALL ',(CADR REST) ,STRUCTURE ,STREAM ,DEPTH)))))
      (LET ((STREAM (MAKE-SYMBOL "STREAM")))
        `(DEFSELECT ((:PROPERTY ,STRUCTURE-NAME NAMED-STRUCTURE-INVOKE) IGNORE)
           ;; see above mumbling about IGNORE
           (:PRINT-SELF (,STRUCTURE-NAME ,STREAM &REST IGNORE)
             (IF PRINT-READABLY (PRINT-NOT-READABLE ,STRUCTURE-NAME)
               (FORMAT ,STREAM ,@(CDR REST)))))))))

1#+NIL
(defun defstruct-define-printer (name rest)*
  1(let ((method-function-name (symbolconc name "->PRINT-SELF#METHOD"))*
       1(stream-var (gensym))*
       1(gubble (gensym))*
       1CLIP (POP REST))*
    1(IF CLIP*
       1NIL*                                  ;don't know anything about nil either...
      1`(progn 'compile*
            1(defun ,method-function-name (,name () () ,stream-var &rest ,gubble)*
              1,gubble*                               1;ignored*
              1(format ,stream-var ,@rest))*
            1(add-flavor-method-info ',name ':print-self ',method-function-name)))))

#-(or LispM MacLisp-10 NIL)
(defun defstruct-define-printer (name rest)*
  1`(comment ,name ,@rest))*

(defun defstruct-parse-options (options CLIP)
  (let ((name (if (atom options) options (car options)))
        (type nil)
        (constructors (defstruct-make-empty))
        (alterant (defstruct-make-empty))
        (included nil)
        (named-p CLIP)                          ;structures named by default in common lisp
        (but-first nil)
        (description (make-defstruct-description))
        (OLD))
    (setf (defstruct-description-name) name)
    (WHEN CLIP
      ;; differing commonlisp defaults
      (SETF (DEFSTRUCT-DESCRIPTION-CONC-NAME) (DEFSTRUCT-APPEND-SYMBOLS NAME "-"))
      (setf (defstruct-description-copier) (defstruct-append-symbols "COPY-" name))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE) (DEFSTRUCT-MAKE-EMPTY))
      (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS) T))
    (do ((op) (val) (vals)
         (options (if (atom options) nil (cdr options))
                  (cdr options)))
        ((null options))
      (if (atom (setq op (car options)))
          (setq vals nil)
          (setq op (prog1 (car op) (setq vals (cdr op)))))
      (setq val (if (null vals) (defstruct-make-empty) (car vals)))
      (SETQ OLD NIL)
      ;; If OP is not a keyword, change it to one and come back here.
    AGAIN
      (selectq op
        (:type
         (if (defstruct-emptyp val)
             (defstruct-error
               "The :TYPE option to DEFSTRUCT must have a value given"
               name))
         ;; In Common Lisp, :TYPE implies it is not a true named structure!
         ;; What a loss!
         ;; It may be a phony one (slot allocated for the type, but not
         ;; marked as a named structure), so if NAMED-P is already :PHONY leave it alone.
         (IF (AND CLIP (EQ NAMED-P T))
             (SETQ NAMED-P NIL))
         (setq type (IF (NULL (CDR VALS)) VAL VALS)))
        (:default-pointer
         (setf (defstruct-description-default-pointer)
               (if (defstruct-emptyp val) name val)))
        (:named
         (or (defstruct-emptyp val)
             (defstruct-error
               "The :NAMED option to DEFSTRUCT doesn't take a value" name))
         ;; In Common Lisp, :NAMED means just allocate a slot for the name,
         ;; but not to make it a true named structure unless no :type is given
         (setq named-p (IF CLIP ':PHONY T)))
        (:conc-name
         (setf (defstruct-description-conc-name)
               (if (defstruct-emptyp val)
                   (IF CLIP NIL
                     (defstruct-append-symbols name "-"))
                 val)))
        (:print
         (if (defstruct-emptyp val)
             (defstruct-error
               "The :PRINT option to DEFSTRUCT requires a value"
               name))
         (setf (defstruct-description-print) (CONS NIL VALS)))  ;commonlisp nil
        (:PRINT-FUNCTION
         (AND (DEFSTRUCT-EMPTYP VAL)
              (DEFSTRUCT-ERROR
                "The :PRINT-FUNCTION option to DEFSTRUCT requires a value"
                NAME))
         (AND (CDR VALS)                        ;gratuitous check against using :print syntax
              (DEFSTRUCT-ERROR
                "The :PRINT-FUNCTION option to DEFSTRUCT takes only one value"))
         (SETF (DEFSTRUCT-DESCRIPTION-PRINT) (CONS T VALS)))    ;commonlisp t
        (:include
         (if (defstruct-emptyp val)
             (defstruct-error
               "The :INCLUDE option to DEFSTRUCT requires a value"
               name))
         (setq included val)
         (setf (defstruct-description-include) vals))
        (:predicate
         (setf (defstruct-description-predicate)
               (if (defstruct-emptyp val)
                   (defstruct-append-symbols name "-P")
                   val)))
        (:constructor
         (cond ((null val)
                (setq constructors nil))
               (t
                (and (defstruct-emptyp val)
                     (setq val (defstruct-append-symbols "MAKE-" name)))
                (setq val (cons val (cdr vals)))
                (if (defstruct-emptyp constructors)
                    (setq constructors (list val))
                  (push val constructors)))))
        (:copier
         (setf (defstruct-description-copier)
               (if (defstruct-emptyp val)
                   (defstruct-append-symbols "COPY-" name)
                 val)))
        1#-(OR (AND LISPM MIT) NIL)*
        1(:eval-when*
        1 (and (defstruct-emptyp val)*
        1      (defstruct-error*
               1"The :EVAL-WHEN option to DEFSTRUCT requires a value"*
               1name))*
          1(setf (defstruct-description-eval-when) val))*
        (:alterant
         (setq alterant val))
        (:but-first
         (if (defstruct-emptyp val)
             (defstruct-error
               "The :BUT-FIRST option to DEFSTRUCT must have a value given"
               name))
         (setq but-first val)
         (setf (defstruct-description-but-first) val))
        (:size-macro
         (setf (defstruct-description-size-macro)
               (if (defstruct-emptyp val)
                   (defstruct-append-symbols name "-SIZE")
                   val)))
        (:size-symbol
         (setf (defstruct-description-size-symbol)
               (if (defstruct-emptyp val)
                   (defstruct-append-symbols name "-SIZE")
                   val)))
        (:callable-accessors
         (setf (defstruct-description-callable-accessors)
               (if (defstruct-emptyp val) t val)))
        (:CALLABLE-CONSTRUCTORS
         (SETF (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS)
               (IF (DEFSTRUCT-EMPTYP VAL) T VAL)))
        (:property
         (if (defstruct-emptyp val)
             (defstruct-error
               "The :PROPERTY option to DEFSTRUCT requires a value"
               name))
         (push (cons val (if (null (cdr vals)) t (cadr vals)))
               (defstruct-description-property-alist)))
        (:initial-offset
         (and (or (defstruct-emptyp val)
                  (not (fixp val)))
              (defstruct-error
                "The :INITIAL-OFFSET option to DEFSTRUCT requires a fixnum"
                name))
         (setf (defstruct-description-initial-offset) val))
        (t
         (cond ((get op 'defstruct-type-description)
                (or (defstruct-emptyp val)
                    (defstruct-error
                      "DEFSTRUCT type used as an option with a value"
                      op 'in name))
                (setq type op))
               (T
                (IF OLD (SETQ OP OLD)
                  (LET ((NEW (DEFSTRUCT-RETRY-KEYWORD OP)))
                    (UNLESS (EQ NEW OP)
                      (SETQ OLD OP OP NEW)
                      (GO AGAIN))))
                (PUSH `(NIL ,OP . ,(IF (DEFSTRUCT-EMPTYP VAL) T VAL))
                      ;;  this NIL flags that not explicitly specified, (ie via :property)
                      ;; and thus may lose if not a valid :defstuct-keyword
                      ;; We use the philospohy that if (:property mumble frob) is specified,
                      ;; then the user REALLY wants a mumble property, and we don't check,
                      ;; but just specifying mumble could be because he/she is losing/typoing.
                      (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST)))))))
    (WHEN (AND CLIP (DEFSTRUCT-EMPTYP (DEFSTRUCT-DESCRIPTION-PREDICATE)))
      (SETF (DEFSTRUCT-DESCRIPTION-PREDICATE)
            (AND NAMED-P (DEFSTRUCT-APPEND-SYMBOLS NAME "-P"))))
    (if (defstruct-emptyp constructors)
        (setq constructors (list (cons (defstruct-append-symbols "MAKE-" name)
                                       nil))))
    (setf (defstruct-description-constructors) constructors)
    ;; defaulting that an alterant IS produced is truly losing, considering how often
    ;; alterants are used. It would be nice to change this...
    (when (defstruct-emptyp alterant)
      (SETQ ALTERANT (IF CLIP NIL
                         (defstruct-append-symbols "ALTER-" name))))
    (setf (defstruct-description-alterant) alterant)
    (WHEN TYPE
      (WHEN (CONSP TYPE)
        (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) (CADR TYPE))
        (PUSH `(:SUBTYPE ,(IF (CDDR TYPE) (CDR TYPE) (CADR TYPE)))
              (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST))
        (SETQ TYPE (CAR TYPE)))
      (UNLESS (KEYWORDP TYPE)
        (SETQ TYPE (DEFSTRUCT-RETRY-KEYWORD TYPE)))
      (LET ((TYPE-DESCRIPTION (OR (GET TYPE 'DEFSTRUCT-TYPE-DESCRIPTION)
                                  (DEFSTRUCT-ERROR
                                    "Unknown type in DEFSTRUCT"
                                    TYPE 'IN NAME))))
        (if named-p
            (setq type
                  (or (defstruct-type-description-named-type)
                      (defstruct-error
                        "There is no way to make a :NAMED defstruct of this type"
                        type 'in name))))))
    (cond (included
             (let ((d (get-defstruct-description included)))
               (if (null type)
                   (setq type (defstruct-description-type d))
                 (or (eq type (defstruct-description-type d))
                     (defstruct-error
                       "defstruct types must agree for :INCLUDE option"
                       included 'included 'by name)))
               (and named-p
                    (NEQ type (defstruct-type-description-named-type
                                (or (get type 'defstruct-type-description)
                                    (defstruct-error
                                      "Unknown type in DEFSTRUCT"
                                      type 'in name 'including included))))
                    (defstruct-error
                      ":INCLUDEd defstruct's type isn't a named type"
                      included 'included 'by name))
               (if (null but-first)
                   (setf (defstruct-description-but-first)
                         (defstruct-description-but-first d))
                 (or (equal but-first (defstruct-description-but-first d))
                     (defstruct-error
                       ":BUT-FIRST options must agree for :INCLUDE option"
                       included 'included 'by name)))))
          ((null type)
           (setq type
             (cond ;; If the :NAMED option is given without :TYPE in Common Lisp,
                   ;; should produce a real named vector, just like the default case.
                   ;; ((EQ NAMED-P ':PHONY)
                   ;; ':PHONY-NAMED-VECTOR)
                   (named-p
                    1#+MacLisp-10 ':named-hunk*
                    1#+Multics ':named-list*
                    #+LispM (IF CLIP ':NAMED-VECTOR ':named-array)
                    1#+NIL ':extend*)
                   (t
                    1#+MacLisp-10 ':hunk*
                    1#+Multics ':list*
                    #+LispM (IF CLIP ':VECTOR ':array)
                    1#+NIL ':vector*)))))
    (let ((type-description (or (get type 'defstruct-type-description)
                                (defstruct-error
                                  "Undefined defstruct type"
                                  type 'in name))))
      (setf (defstruct-description-type) type)
      (setf (defstruct-description-named-p)
            (eq (defstruct-type-description-named-type) type))
      (OR (DEFSTRUCT-DESCRIPTION-NAMED-P)
          (NULL (DEFSTRUCT-DESCRIPTION-PRINT))
          (DEFSTRUCT-EMPTYP (DEFSTRUCT-DESCRIPTION-PRINT))
          (DEFSTRUCT-ERROR
            ":PRINT or :PRINT-FUNCTION is allowed only for recognizable named structures"
            NAME))
      ;; this is where we check for validity of "implicity-specified" defstruct keywords
      ;; see note a above as to the reasoning above this.
      (DO ((X (DEFSTRUCT-DESCRIPTION-PROPERTY-ALIST) (CDR X)))
          ((NULL X))
        (IF (NULL (CAAR X))                     ;specified implicitly
            (IF (OR (MEMQ (CADAR X) (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
                    (GET (CADAR X) ':DEFSTRUCT-OPTION)) ;obsolete, but support for a while
                (SETF (CAR X) (CDAR X))         ;strip off the NIL
              (DEFSTRUCT-ERROR
                "DEFSTRUCT doesn't understand this option"
                (CAR X) 'IN NAME))))
      (OR (MEMQ ':SUBTYPE (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS))
          (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) NIL)))
    description))

(DEFUN DEFSTRUCT-PARSE-ITEMS (ITEMS DESCRIPTION)
  (LET ((NAME (DEFSTRUCT-DESCRIPTION-NAME))
        (OFFSET (DEFSTRUCT-DESCRIPTION-INITIAL-OFFSET))
        (INCLUDE (DEFSTRUCT-DESCRIPTION-INCLUDE))
        (O-SLOT-ALIST NIL)
        (CONC-NAME (DEFSTRUCT-DESCRIPTION-CONC-NAME))
        1#+MACLISP-10 (CHARS (EXPLODEN CONC-NAME))*)
    ;;;Handle INCLUDEd structures:
    (OR (NULL INCLUDE)
        (LET ((D (GET-DEFSTRUCT-DESCRIPTION (CAR INCLUDE))))
          (SETQ OFFSET (+ OFFSET (DEFSTRUCT-DESCRIPTION-SIZE D)))
          (SETQ O-SLOT-ALIST
                (COPYTREE (DEFSTRUCT-DESCRIPTION-SLOT-ALIST D)))
          (DOLIST (L O-SLOT-ALIST)
            (SETF (DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR L))
                  (IF CONC-NAME 1#+MACLISP-10 (IMPLODE (APPEND CHARS (EXPLODEN (CAR L))))*
                                #-MACLISP-10 (DEFSTRUCT-APPEND-SYMBOLS CONC-NAME (CAR L))
                      (CAR L))))
          (DOLIST (L (CDR INCLUDE))
            (LET* ((IT (IF (CONSP L) (CAR L) L))
                   (name-of-it (if (consp it) (car it) it))
                   (REST (IF (CONSP L) (CDR L) NIL))
                   (SLOT-DESCRIPTION (CDR (ASSQ name-of-IT O-SLOT-ALIST))))
              (IF (NULL SLOT-DESCRIPTION)
                (DEFSTRUCT-ERROR
                  "Unknown slot in :INCLUDEd defstruct"
                  IT 'IN INCLUDE 'INCLUDED 'BY NAME))
              (cond ((not (consp it))
                     (DEFSTRUCT-PARSE-ONE-FIELD
                       name-of-IT NIL NIL REST CONC-NAME 1#+MACLISP-10 (EXPLODEN CONC-NAME)*
                       SLOT-DESCRIPTION))
                    (t
    ;Following code allows multiple names for an included slot.  The slot in question
    ; is specified by the first name.  The new names are in "format 3" and may have byte-specs.
                     (DEFSTRUCT-PARSE-ONE-FIELD ;use the original slot-description for the first guy.
                       name-of-IT NIL NIL (cddr it) CONC-NAME 1#+MACLISP-10 (EXPLODEN CONC-NAME)*
                       SLOT-DESCRIPTION)
                     (dolist (e (cdr l))
                       (let ((name-of-it (if (consp e) (car e) e))
                             (rest (if (consp e) (cddr e) nil))
                     ;for the rest, use the original as a default, but mung as necessary.
                             (slot-description (copy-list slot-description)))
                         (defstruct-parse-one-field
                           name-of-it nil (if (consp e) (cadr e) nil)
                           rest conc-name  1#+MACLISP-10 (EXPLODEN CONC-NAME)*
                           slot-description)
                         (push (cons name-of-it slot-description) o-slot-alist)))))
              ))))
    ;;;Handle slots:
    (DO ((I OFFSET (1+ I))
         (L ITEMS (CDR L))
         (SLOT-ALIST NIL)
         )
        ((NULL L)
         (SETQ SLOT-ALIST (NREVERSE SLOT-ALIST))
         (SETF (DEFSTRUCT-DESCRIPTION-SIZE) I)
         (SETF (DEFSTRUCT-DESCRIPTION-SLOT-ALIST)
               (NCONC O-SLOT-ALIST SLOT-ALIST)))        ;Now returns ALL slots, not just new
      ;;;
      (COND ((ATOM (CAR L))                             ;Just slot name, no initial value
             (PUSH (DEFSTRUCT-PARSE-ONE-FIELD
                     (CAR L) I NIL NIL CONC-NAME1 #+MACLISP-10 CHARS*)
                   SLOT-ALIST))
            ((ATOM (CAAR L))                            ;Slot definition list
             (PUSH (DEFSTRUCT-PARSE-ONE-FIELD
                     (CAAR L) I NIL (CDAR L) CONC-NAME1 #+MACLISP-10 CHARS*)
                   SLOT-ALIST))
            (T                                          ;Slot with subslots
             ;;;Currently,
             ;;;BYTE FIELDS are the only kind of supported subslot definition!
             ;;;
             ;;;Possible extension: SUB-STRING FIELDS...
             ;;;
             (let ((subslots (car l))
                   subfield-inits subfield-no-inits
                   whole-word-inits whole-word-no-inits)
               ;;;Order subslots so that constructor works properly:
               ;;; 1) Whole-word slots with inits
               ;;; 2) Subfields with inits
               ;;; 3) Whole-word slots without inits
               ;;; 4) Subfields without inits
               (do* ((slots subslots (cdr slots))
                     (slot (first slots) (first slots)))
                    ((null slots))
                 (if (second slot)              ;Presumably, a subfield
                     (if (third slot)
                         (push slot subfield-inits)
                       (push slot subfield-no-inits))
                   (if (third slot)
                       (push slot whole-word-inits)
                     (push slot whole-word-no-inits))))
               (setq subslots (append whole-word-inits subfield-inits
                                      whole-word-no-inits subfield-no-inits))
               ;;;Parse (reordered) subslots
               (do ((slots subslots (cdr slots)))
                   ((null slots))
                 (let* ((slot (car slots))
                        (name (car slot))
                        (spec (cadr slot))
                        (options (cddr slot)))
                   (PUSH (DEFSTRUCT-PARSE-ONE-FIELD
                           name I spec
                           options
                           CONC-NAME1 #+MACLISP-10 CHARS*)
                         SLOT-ALIST)))))))))

(DEFUN DEFSTRUCT-PARSE-ONE-FIELD (IT NUMBER PPSS REST CONC-NAME1 #+MACLISP-10 CHARS*
                                  &OPTIONAL SLOT-DESCRIPTION)
  (LET* ((MNAME (IF CONC-NAME 1#+MACLISP-10 (IMPLODE (APPEND CHARS (EXPLODEN IT)))*
                              #-MACLISP-10 (DEFSTRUCT-APPEND-SYMBOLS CONC-NAME IT)
                    IT))
         (TYPE T) (ALIST NIL) (READ-ONLY NIL) (BITS NIL) (DOCUMENTATION NIL)
         TYPEP INITP DOCP ROP
         (INIT-CODE (IF (NULL REST) (DEFSTRUCT-MAKE-EMPTY)
                      (SETQ INITP T)
                      (DO ((L (CDR REST) (CDDR L)))
                          ((NULL L) (CAR REST))
                        (SELECTQ (CAR L)
                          (:DOCUMENTATION (SETQ DOCUMENTATION (CADR L) DOCP T))
                          (:READ-ONLY (SETQ READ-ONLY (CADR L) ROP T))
                          (:TYPE (SETQ TYPE (CADR L) TYPEP T))
                          (T (DEFSTRUCT-ERROR
                               "Unknown DEFSTRUCT slot-option" (CAR L))))))))
    ;;;;;;How to lose if porting to machine with different byte-specs:
    ;;;    (LET (PP SS)
    ;;;      (IF (LIST-MATCH-P PPSS `(BYTE ,SS ,PP))
    ;;;   (SETQ PPSS (BYTE SS PP))))
    (IF (NULL SLOT-DESCRIPTION)
        (CONS IT (MAKE-DEFSTRUCT-SLOT-DESCRIPTION
                   :NUMBER NUMBER
                   :PPSS PPSS
                   :INIT-CODE INIT-CODE
                   :REF-MACRO-NAME MNAME
                   :TYPE TYPE
                   :PROPERTY-ALIST ALIST
                   :READ-ONLY READ-ONLY
                   :BITS BITS
                   :DOCUMENTATION DOCUMENTATION))
      (SETF (DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME) MNAME)
      (if ppss
          (setf (defstruct-slot-description-ppss) ppss))
      (IF INITP (SETF (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE) INIT-CODE))
      (IF DOCP  (SETF (DEFSTRUCT-SLOT-DESCRIPTION-DOCUMENTATION) DOCUMENTATION))
      (IF TYPEP (IF (SUBTYPEP TYPE (LET ((TEM (DEFSTRUCT-SLOT-DESCRIPTION-TYPE)))
                                     (IF (EQ TEM 'NOTYPE) T TEM)))
                    (SETF (DEFSTRUCT-SLOT-DESCRIPTION-TYPE) TYPE)
                  (DEFSTRUCT-ERROR
                    "The slot :TYPE specified is incompatible with the :INCLUDEd slot type"
                    IT)))
      (IF ROP (IF (AND (DEFSTRUCT-SLOT-DESCRIPTION-READ-ONLY) (NOT READ-ONLY))
                  (DEFSTRUCT-ERROR "A slot is not :READ-ONLY, but the :INCLUDEd slot is"
                                   IT)
                (SETF (DEFSTRUCT-SLOT-DESCRIPTION-READ-ONLY) READ-ONLY))))))

(defun defstruct-define-ref-macros (new-slots description)
  (let ((name (defstruct-description-name))
        (returns nil))
    (if (not (defstruct-description-callable-accessors))
        (do ((l new-slots (cdr l))
             (mname))
            ((null l))
          (setq mname (defstruct-slot-description-ref-macro-name (cdar l)))
          (defstruct-put-macro mname 'defstruct-expand-ref-macro)
          (defstruct-putprop-compile-time mname (cons name (caar l)) 'defstruct-slot))
      (let* ((type-description
               (get (defstruct-description-type)
                    'defstruct-type-description))
             (code (defstruct-type-description-ref-expander))
             (n (defstruct-type-description-ref-no-args))
             (but-first (defstruct-description-but-first))
             (default-pointer (defstruct-description-default-pointer)))
        (do ((args nil (cons (gensym) args))
             (i n (1- i)))
            ((< i 2)
             ;; Last arg (if it exists) is name of structure, for documentation purposes.
             (and (= i 1)
                  (setq args (cons name args)))
             (let ((body (cons (if but-first
                                   `(,but-first ,(car args))
                                 (car args))
                               (cdr args))))
               (and default-pointer
                    (setq args `((,(car args) ,default-pointer)
                                 &optional ,@(cdr args))))
               (setq args (reverse args))
               (setq body (reverse body))
               (do ((l new-slots (cdr l))
                    (mname))
                   ((null l))
                 (setq mname (defstruct-slot-description-ref-macro-name
                               (cdar l)))
                 #+(AND LISPM MIT)              ;how do other people do this?
                 (IF (DEFSTRUCT-SLOT-DESCRIPTION-READ-ONLY (CDAR L))
                     (DEFSTRUCT-PUTPROP-COMPILE-TIME MNAME
                                                     'UNSETFABLE 'SETF-METHOD))
                 1#+MacLisp*
                 1;;This must come BEFORE the defun. THINK!*
                 1(defstruct-put-macro mname 'defstruct-expand-ref-macro)*
                 (let ((ref (apply
                              code
                              (defstruct-slot-description-number (cdar l))
                              description
                              body))
                       (ppss (defstruct-slot-description-ppss (cdar l)))
                       (DOC (DEFSTRUCT-SLOT-DESCRIPTION-DOCUMENTATION (CDAR L))))
                   (push `(#+LISPM defsubst-with-parent 1#+NIL defsubst #-(or LispM NIL) defun*
                           ,mname #+LISPM ,name ,args
                           ,DOC
                           ,(if (null ppss) ref `(ldb ,ppss ,ref)))
                         returns))
                 (defstruct-putprop mname
                                    (cons name (caar l))
                   'defstruct-slot)))))))
    returns))

#+LispM
(defprop defstruct-expand-cons-macro
         defstruct-function-parent
         macroexpander-function-parent)

#+LispM
(defprop defstruct-expand-size-macro
         defstruct-function-parent
         macroexpander-function-parent)

#+LispM
(defprop defstruct-expand-alter-macro
         defstruct-function-parent
         macroexpander-function-parent)

#+LispM
(defprop defstruct-expand-ref-macro
         defstruct-function-parent
         macroexpander-function-parent)

#+LispM
(defun defstruct-function-parent (sym)
  (values (or (getdecl sym 'defstruct-name)
              (car (getdecl sym 'defstruct-slot)))
          'defstruct))

(defun defstruct-expand-size-macro (x #+Common env)
  #+Common (declare (ignore env))
  (let ((description (get-defstruct-description (getdecl (car x) 'defstruct-name))))
    (let ((type-description (or (get (defstruct-description-type)
                                     'defstruct-type-description)
                                (defstruct-error
                                  "Unknown defstruct type"
                                  (defstruct-description-type)))))
      (+ (defstruct-description-size)
         (defstruct-type-description-overhead)))))

(defun defstruct-expand-ref-macro (x #+Common env)
  #+Common (declare (ignore env))
  (let* ((pair (getdecl (car x) 'defstruct-slot))
         (description (get-defstruct-description (car pair)))
         (type-description (or (get (defstruct-description-type)
                                    'defstruct-type-description)
                               (defstruct-error
                                 "Unknown defstruct type"
                                 (defstruct-description-type))))
         (code (defstruct-type-description-ref-expander))
         (n (defstruct-type-description-ref-no-args))
         (args (reverse (cdr x)))
         (nargs (length args))
         (default (defstruct-description-default-pointer))
         (but-first (defstruct-description-but-first)))
    (cond ((= n nargs)
           (and but-first
                (rplaca args `(,but-first ,(car args)))))
          ((and (= n (1+ nargs)) default)
           (setq args (cons (if but-first
                                `(,but-first ,default)
                                default)
                            args)))
          (t
           (defstruct-error
             "Wrong number of args to an accessor macro" x)))
    (let* ((slot-description
             (cdr (or (assq (cdr pair)
                            (defstruct-description-slot-alist))
                      (defstruct-error
                        "This slot no longer exists in this structure"
                        (cdr pair) 'in (car pair)))))
            (ref (APPLY
                   code
                   (defstruct-slot-description-number)
                   description
                   (nreverse args)))
            (ppss (defstruct-slot-description-ppss)))
      (if (null ppss)
          ref
          `(ldb ,ppss ,ref)))))

(defun defstruct-parse-setq-style-slots (l slots others x)
  (do ((l l (cddr l))
       (kludge (cons nil nil)))
      ((null l) kludge)
    (or (and (cdr l)
             (symbolp (car l)))
        (defstruct-error
          "Bad argument list to constructor or alterant macro" x))
    (defstruct-make-init-dsc kludge (car l) (cadr l) slots others x)))

(defun defstruct-make-init-dsc (kludge name code slots others x)
  (let ((p (OR (assq name slots)
               ;; old defstruct didn't use keywords.
               (ASS 'STRING= (GET-PNAME NAME) SLOTS))))
    (if (null p)
        (if (memq name others)
            (push (cons name code) (cdr kludge))
          (let ((new (defstruct-retry-keyword name)))
            (if (memq new others)
                (push (cons new code) (cdr kludge))
              (defstruct-error
                "Unknown slot to constructor or alterant macro"
                name 'in x))))
      (let* ((slot-description (cdr p))
             (number (defstruct-slot-description-number))
             (ppss (defstruct-slot-description-ppss))
             (dsc (assoc number (car kludge))))
        (cond ((null dsc)
               (setq dsc (list* number nil (defstruct-make-empty) 0 0 nil))
               (push dsc (car kludge))))
        (cond ((null ppss)
               (setf (car (cddr dsc)) code)
               (setf (cadr dsc) t))
              (t (cond ((and (numberp ppss) (numberp code))
                        (setf (ldb ppss (cadr (cddr dsc))) -1)
                        (setf (ldb ppss (caddr (cddr dsc))) code))
                       (t
                        (push (cons ppss code) (cdddr (cddr dsc)))))
                 (or (eq t (cadr dsc))
                     (push name (cadr dsc)))))))))

;;; this hairyness should be made to use BYTE
(defun defstruct-code-from-dsc (dsc)
  (let ((code (car (cddr dsc)))
        (mask (cadr (cddr dsc)))
        (bits (caddr (cddr dsc))))
    (if (defstruct-emptyp code)
        (setq code bits)
        (or (zerop mask)
            (setq code (if (numberp code)
                           (boole 7 bits (boole 2 mask code))
                           (if (zerop (logand mask
                                              (1+ (logior mask (1- mask)))))
                               (let ((ss (haulong (boole 2 mask (1- mask)))))
                                 `(dpb ,(lsh bits (- ss))
                                       ,(logior (lsh ss 6)
                                                (logand #o77
                                                        (- (haulong mask) ss)))
                                       ,code))
                               `(boole 7 ,bits (boole 2 ,mask ,code)))))))
    (do ((l (cdddr (cddr dsc)) (cdr l)))
        ((null l))
      (setq code `(dpb ,(cdar l) ,(caar l) ,code)))
    code))

(defun defstruct-expand-cons-macro (x #+Common env)
  #+Common (declare (ignore env))
  (LET* ((DEFSTRUCT-NAME (getdecl (car x) 'defstruct-name))
         (description (get-defstruct-description defstruct-name))
         (constructor-description
           (cdr (or (assq (car x) (defstruct-description-constructors))
                    (defstruct-error
                      "This constructor is no longer defined for this structure"
                      (car x) 'in (defstruct-description-name))))))
    (DEFSTRUCT-EXPAND-CONS-MACRO-1
      (CAR X)
      DEFSTRUCT-NAME
      CONSTRUCTOR-DESCRIPTION
      (CDR X))))

(DEFUN DEFSTRUCT-EXPAND-CONS-MACRO-1 (MACRO-NAME DEFSTRUCT-NAME CONSTRUCTOR-DESCRIPTION BODY)
  (let* ((description (get-defstruct-description defstruct-name))
         (type-description (or (get (defstruct-description-type)
                                    'defstruct-type-description)
                               (defstruct-error
                                 "Unknown defstruct type"
                                 (defstruct-description-type))))
         (slot-alist (defstruct-description-slot-alist))
         (cons-keywords (defstruct-type-description-cons-keywords))
         (kludge nil)
         (aux nil)
         (aux-init nil))
     (if (null constructor-description)
         (setq kludge (defstruct-parse-setq-style-slots BODY
                                                        slot-alist
                                                        cons-keywords
                                                        (CONS MACRO-NAME BODY)))
       ;; can't do anything useful with doc strings using current scheme of defstruct
       ;; constructor macrology
       (if (stringp (car (last constructor-description)))
           (setq constructor-description (butlast constructor-description)))
       (prog (args l)
             (setq kludge (cons nil nil))
             (setq args BODY)
             (setq l (car constructor-description))
          R  (cond ((null l)
                    (if (null args)
                        (return nil)
                      (go barf-tma)))
                   ((atom l) (go barf))
                   ((eq (car l) '&optional) (go O))
                   ((eq (car l) '&rest) (go S))
                   ((eq (car l) '&aux) (go A))
                   ((null args) (go barf-tfa)))
             (defstruct-make-init-dsc kludge
                                      (pop l)
                                      (pop args)
                                      slot-alist
                                      cons-keywords
                                      (CONS MACRO-NAME BODY))
             (go R)
          O  (and (null args) (go OD))
             (pop l)
             (cond ((null l) (go barf-tma))
                   ((atom l) (go barf))
                   ((eq (car l) '&optional) (go barf))
                   ((eq (car l) '&rest) (go S))
                   ((eq (car l) '&aux) (go barf-tma)))
             (defstruct-make-init-dsc kludge
                                      (if (atom (car l)) (car l) (caar l))
                                      (pop args)
                                      slot-alist
                                      cons-keywords
                                      (CONS MACRO-NAME BODY))
             (go O)
          OD (pop l)
             (cond ((null l) (return nil))
                   ((atom l) (go barf))
                   ((eq (car l) '&optional) (go barf))
                   ((eq (car l) '&rest) (go S))
                   ((eq (car l) '&aux) (go A)))
             (or (atom (car l))
                 (defstruct-make-init-dsc kludge
                                          (caar l)
                                          (cadar l)
                                          slot-alist
                                          cons-keywords
                                          (CONS MACRO-NAME BODY)))
             (go OD)
          S  (and (atom (cdr l)) (go barf))
             (defstruct-make-init-dsc kludge
                                      (cadr l)
                                      `(list ,@args)
                                      slot-alist
                                      cons-keywords
                                      (CONS MACRO-NAME BODY))
             (setq l (cddr l))
             (and (null l) (return nil))
             (and (atom l) (go barf))
             (or (eq (car l) '&aux) (go barf))
          A  (pop l)
             (cond ((null l) (return nil))
                   ((atom l) (go barf))
                   ((atom (car l))
                    (push (car l) aux)
                    (push (defstruct-make-empty) aux-init))
                   (t
                    (push (caar l) aux)
                    (push (cadar l) aux-init)))
             (go A)
          BARF (defstruct-error
                 "Bad format for defstruct constructor arglist"
                 `(,MACRO-NAME ,@(car constructor-description)))
          BARF-TFA (defstruct-error "Too few arguments to constructor macro"
                                    (CONS MACRO-NAME BODY))
          BARF-TMA (defstruct-error "Too many arguments to constructor macro"
                                    (CONS MACRO-NAME BODY))))
     (do ((l slot-alist (cdr l)))
         ((null l))
       (let* ((name (caar l))
              (slot-description (cdar l))
              (code (do ((aux aux (cdr aux))
                         (aux-init aux-init (cdr aux-init)))
                        ((null aux) (defstruct-slot-description-init-code))
                      (and (eq name (car aux)) (return (car aux-init)))))
              (ppss (defstruct-slot-description-ppss)))
         (or (and (defstruct-emptyp code) (null ppss))
             (let* ((number (defstruct-slot-description-number))
                    (dsc (assoc number (car kludge))))
               (cond ((null dsc)
                      (setq dsc (list* number nil (defstruct-make-empty) 0 0 nil))
                      (push dsc (car kludge))))
               (cond ((defstruct-emptyp code))
                     ((eq t (cadr dsc)))
                     ((null ppss)
                      (and (defstruct-emptyp (car (cddr dsc)))
                           (setf (car (cddr dsc)) code)))
                     ((memq name (cadr dsc)))
                     ((and (numberp ppss) (numberp code))
                      (setf (ldb ppss (cadr (cddr dsc))) -1)
                      (setf (ldb ppss (caddr (cddr dsc))) code))
                     (t
                      (push (cons ppss code) (cdddr (cddr dsc)))))))))
     (do ((l (car kludge) (cdr l)))
         ((null l))
       (rplacd (car l) (defstruct-code-from-dsc (car l))))
     (invoke-defstruct-constructor-expander
       description type-description
       (car kludge) (cdr kludge))))


(DEFUN DEFSTRUCT-DEFINE-CONSTRUCTORS (DESCRIPTION)
  (LET ((NAME (DEFSTRUCT-DESCRIPTION-NAME))
        RETURNS)
        (DOLIST (CS (DEFSTRUCT-DESCRIPTION-CONSTRUCTORS))
          (COND ((NOT (DEFSTRUCT-DESCRIPTION-CALLABLE-CONSTRUCTORS))
                 (DEFSTRUCT-PUT-MACRO (CAR CS) 'DEFSTRUCT-EXPAND-CONS-MACRO))
                ('ELSE
                 (PUSH (DEFSTRUCT-DEFINE-CALLABLE-CONSTRUCTOR DESCRIPTION
                                                              CS)
                       RETURNS)))
          (DEFSTRUCT-PUTPROP-COMPILE-TIME (CAR CS) NAME 'DEFSTRUCT-NAME))
        RETURNS))


;; NEW THEORY FOR DOING CALLABLE CONSTRUCTORS: 26-Feb-86 08:18:39 -GJC
;; NORMALIZE THE ARGUMENT LIST, &OPTIONAL,&AUX AS INDICATED IN THE CL MANUAL.
;; THEN LET THE BODY OF THE DEFUN BE A SIMPLE CALL TO AN INTERNAL MACRO THAT
;; CALLS DEFSTRUCT-EXPAND-CONS-MACRO IN THE OBVIOUS WAY. WE ALLOW &KEY IN THE
;; ARGUMENT LIST SINCE THAT IS CONVENIENT FOR THE DEFAULT.
;; ANOTHER THEORY IS TO HAVE THE CONSTRUCTOR FUNCTION BE A CLOSURE OVER ITS OWN
;; NAME, AND DO ALL ITS DISPATCHING AND CONSTRUCTION AT RUN-TIME.
;; THE PREVIOUS ATTEMPT AT THIS HAD THE POOR USER CALLING EVAL AT RUN-TIME!
;; BION (BELIEVE IT OR NOT). THIS NEW VERSION DOESNT SUPPORT SOME MAKE-ARRAY
;; FEATURE THAT THE GROSS-OUT VERSION DID.

(DEFUN DEFSTRUCT-DEFINE-CALLABLE-CONSTRUCTOR (DESCRIPTION SPEC)
  (LET ((FUNCTION-NAME (CAR SPEC))
        (ARGUMENT-LIST (IF (NULL (CDR SPEC))
                           (CONS '&KEY (MAPCAR #'CAR (DEFSTRUCT-DESCRIPTION-SLOT-ALIST)))
                         (CADR SPEC)))
        (MACRO-ARGUMENTS NIL)
        (FUNCTION-ARGUMENTS NIL))
    (DO ((L ARGUMENT-LIST (CDR L))
         (ARG-STATE NIL)
         (ARG))
        ((NULL L))
      (SETQ ARG (CAR L))
      (COND ((MEMQ ARG '(&KEY &AUX &OPTIONAL &REST))
             (SETQ ARG-STATE ARG)
             (SETQ FUNCTION-ARGUMENTS (APPEND FUNCTION-ARGUMENTS (LIST ARG))))
            ((NOT ARG-STATE)
             (SETQ FUNCTION-ARGUMENTS (APPEND FUNCTION-ARGUMENTS (LIST ARG)))
             (SETQ MACRO-ARGUMENTS (APPEND MACRO-ARGUMENTS (LIST ARG
                                                                 ARG))))
            ((MEMQ ARG-STATE '(&KEY &AUX &OPTIONAL))
             (WHEN (ATOM ARG)
               (COND ((EQ ARG-STATE '&AUX)
                      (SETQ ARG (LIST ARG NIL)))
                     ('ELSE
                      (LET ((SLOT-DESCRIPTION
                              (OR (CDR (ASSQ ARG
                                             (DEFSTRUCT-DESCRIPTION-SLOT-ALIST)))
                                  (DEFSTRUCT-ERROR "undefined slot name" ARG))))
                        (SETQ ARG (LIST ARG
                                        (IF (DEFSTRUCT-EMPTYP
                                              (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE))
                                            (DEFSTRUCT-SLOT-DEFAULT-CONSTANT-VALUE
                                              DESCRIPTION
                                              SLOT-DESCRIPTION)
                                          (DEFSTRUCT-SLOT-DESCRIPTION-INIT-CODE))))))))
             (SETQ FUNCTION-ARGUMENTS (APPEND FUNCTION-ARGUMENTS (LIST ARG)))
             (SETQ MACRO-ARGUMENTS (APPEND MACRO-ARGUMENTS (LIST (CAR ARG)
                                                                 (CAR ARG)))))
            ((EQ ARG-STATE '&REST)
             (SETQ FUNCTION-ARGUMENTS (APPEND FUNCTION-ARGUMENTS (LIST ARG)))
             (SETQ MACRO-ARGUMENTS (APPEND MACRO-ARGUMENTS (LIST ARG
                                                                 `(COPY-LIST ,ARG))))
             (WHEN (AND (CDR L) (NOT (EQ (CADR L) '&AUX)))
               (DEFSTRUCT-ERROR "arguments after &REST must be &AUX" (CDR L))))))
    `(DEFUN ,FUNCTION-NAME ,FUNCTION-ARGUMENTS
       (DEFSTRUCT-EXPAND-WITH-CONS-MACRO ,(DEFSTRUCT-DESCRIPTION-NAME)
                                         ,@MACRO-ARGUMENTS))))


(DEFUN DEFSTRUCT-SLOT-DEFAULT-CONSTANT-VALUE (DESCRIPTION SLOT-DESCRIPTION)
  (COND ((DEFSTRUCT-SLOT-DESCRIPTION-PPSS)
         0)
        ((CADR (ASSQ (DEFSTRUCT-DESCRIPTION-TYPE)
                     '((:FLONUM-ARRAY 0.0)
                       (:FIXNUM-ARRAY 0)))))))


(DEFMACRO DEFSTRUCT-EXPAND-WITH-CONS-MACRO (NAME &REST BODY)
  (DEFSTRUCT-EXPAND-CONS-MACRO-1 NIL NAME NIL BODY))



(defun invoke-defstruct-constructor-expander (description type-description arg etc)
  (funcall (defstruct-type-description-cons-expander)
           (selectq (defstruct-type-description-cons-flavor)
             (:list
              (do ((l nil (cons nil l))
                   (i (defstruct-description-size) (1- i)))
                  ((= i 0)
                   (do ((arg arg (cdr arg)))
                       ((null arg))
                     (setf (nth (caar arg) l) (cdar arg)))
                   l)))
             (:alist arg)
             (t
              (defstruct-error
                "Unknown constructor kind in this defstruct type"
                (defstruct-description-type))))
           description etc))

(defun defstruct-expand-alter-macro (x #+Common env)
  #+Common (declare (ignore env))
  (let* ((description (get-defstruct-description (getdecl (car x) 'defstruct-name)))
         (type-description (or (get (defstruct-description-type)
                                    'defstruct-type-description)
                               (defstruct-error
                                 "Unknown defstruct type"
                                 (defstruct-description-type))))
         (ref-code (defstruct-type-description-ref-expander))
         (ref-nargs (defstruct-type-description-ref-no-args)))
    (do ((l (car (defstruct-parse-setq-style-slots
                   (nthcdr (1+ ref-nargs) x)
                   (defstruct-description-slot-alist)
                   nil
                   x))
            (cdr l))
         (but-first (defstruct-description-but-first))
         (body nil)
         (avars (do ((i 0 (1+ i))
                     (l nil (cons (gensym) l)))
                    ((= i ref-nargs) l)))
         (vars nil)
         (vals nil))
        ((null l)
         `((lambda ,avars
             ,@(if (null vars)
                   body
                   `(((lambda ,vars ,@body) ,.vals))))
           ,@(do ((i (1- ref-nargs) (1- i))
                  (l `(,(if but-first
                            `(,but-first ,(nth ref-nargs x))
                            (nth ref-nargs x)))
                     (cons (nth i x) l)))
                 ((= i 0) l))))
      (let ((ref (APPLY ref-code (caar l) description avars)))
        (and (defstruct-emptyp (car (cddr (car l))))
             (setf (car (cddr (car l))) ref))
        (let ((code (defstruct-code-from-dsc (car l))))
          (if (null (cdr l))
              (push `(setf ,ref ,code) body)
              (let ((sym (gensym)))
                (push `(setf ,ref ,sym) body)
                (push sym vars)
                (push code vals))))))))

(defmacro defstruct-define-type (type &body options)
  "Defines a new type of defstruct structure, with name TYPE.
OPTIONS may include:
 (:CONS (init desc kwds) kind body)
   Body returns code to construct a structure of type TYPE. Init, desc and kwds are bound to
     the initialization defaults, the structure description and an alist of values of
     additional keywords arguments supplied to DEFSTRUCT. Kind is either :LIST or :ALIST.
 (:REF (slot-number desc arg-1 arg-2 ...) body)
   Body returns the contents of slot-number in the structure. desc is bound to the structure
    description. The arg-n's are bound to values supplied to the accessor as arguments.
 (:OVERHEAD slots)
   Reserves a given number of slots as overhead to this structure-type, which are thus
     unavailable for storing structure data.
 :NAMED
   Indicates that this is a named structure type that TYPEP can recognize.
 (:NAMED named-type)
   Defines the associated named structure type to this unnamed TYPE.
 (:CONS-KEYWORDS kwd-1 ...) or (:KEYWORDS kwd-1 ...)
   Defines keywords which may be supplied to a constructor to affect the construction of an
     instance of this structure. These keywords will appear in the kwds alist supplied to the
     :CONS code.
 (:DEFSTRUCT-KEYWORDS kwd-1 ...)
   Defines keywords which may be supplied to DEFSTRUCT to affect the definition of a stucture
     of this TYPE. These keywords will appear in the property-alist slot of the desc supplied
     to the :defstruct code
 (:PREDICATE (desc name) body)
   Body is code which generates a predicate named name for a structure with description desc.
 (:COPIER (desc name) body)
   Body defines code to define a structure-copier named name.
 (:DEFSTRUCT (desc) body)
   Body is run whenever DEFSTRUCT expands a structure of this TYPE. It should return a list
     of forms to be included in the DEFSTRUCT expansion, or else NIL."
  (DO* ((DOC (IF (STRINGP (CAR OPTIONS)) (POP OPTIONS)))
        (options options (cdr options))
        (op) (args)
        (type-description (make-defstruct-type-description))
        (cons-expander nil)
        (ref-expander nil)
        (returns))
       ((null options)
        (SETF (DEFSTRUCT-TYPE-DESCRIPTION-DOCUMENTATION) DOC)
        (or cons-expander
            (defstruct-error "No :CONS option in DEFSTRUCT-DEFINE-TYPE" type))
        (or ref-expander
            (defstruct-error "No :REF option in DEFSTRUCT-DEFINE-TYPE" type))
        `(progn 'compile                        ;not needed in common lisps
                #+LISPM
                (LOCAL-DECLARE ((FUNCTION-PARENT ,TYPE DEFSTRUCT-DEFINE-TYPE))
                  ,cons-expander
                  ,ref-expander
                  ,@returns)
                1#-LISPM ,cons-expander*
                1#-LISPM ,ref-expander*
                1#-LISPM ,@RETURNS*
                (defprop ,type ,type-description defstruct-type-description)
                ',TYPE))
    (cond ((atom (setq op (car options)))
           (setq args nil))
          (t
           (setq args (cdr op))
           (setq op (car op))))
 AGAIN
    (selectq op
      (:cons
        (or (> (length args) 2)
            (defstruct-error
              "Bad :CONS option in DEFSTRUCT-DEFINE-TYPE"
              (car options) 'in type))
        (let ((n (length (car args)))
              (name (defstruct-append-symbols type "-DEFSTRUCT-CONS")))
          (or (= n 3)
              (defstruct-error
                "Bad :CONS option in DEFSTRUCT-DEFINE-TYPE"
                (car options) 'in type))
          (setf (defstruct-type-description-cons-flavor)
                (defstruct-retry-keyword (cadr args)))
          (setf (defstruct-type-description-cons-expander) name)
          (setq cons-expander `(defun ,name ,(car args)
                                 ,@(cddr args)))))
      (:ref
        (or (> (length args) 1)
            (defstruct-error
              "Bad :REF option in DEFSTRUCT-DEFINE-TYPE"
              (car options) 'in type))
        (let ((n (length (car args)))
              (name (defstruct-append-symbols type "-DEFSTRUCT-REF")))
          (or (> n 2)
              (defstruct-error
                "Bad :REF option in DEFSTRUCT-DEFINE-TYPE"
                (car options) 'in type))
          (setf (defstruct-type-description-ref-no-args) (- n 2))
          (setf (defstruct-type-description-ref-expander) name)
          (setq ref-expander `(defun ,name ,(car args)
                                ,@(cdr args)))))
      (:predicate
        (or (> (length args) 1)
            (defstruct-error
              "Bad :PREDICATE option in DEFSTRUCT-DEFINE-TYPE"
              (car options) 'in type))
        (let ((name (defstruct-append-symbols type "-DEFSTRUCT-PREDICATE")))
          (setf (defstruct-type-description-predicate) name)
          (push `(defun ,name ,(car args)
                   ,@(cdr args))
                returns)))
      (:copier
        (or (> (length args) 1)
            (defstruct-error
              "Bad :COPIER option in DEFSTRUCT-DEFINE-TYPE"
              (car options) 'in type))
        (let ((name (defstruct-append-symbols type "-DEFSTRUCT-COPIER")))
          (setf (defstruct-type-description-copier) name)
          (push `(defun ,name ,(car args)
                   ,@(cdr args))
                returns)))
      (:overhead
        (setf (defstruct-type-description-overhead)
              (if (null args)
                  (defstruct-error
                    "Bad :OVERHEAD option to DEFSTRUCT-DEFINE-TYPE"
                    (car options) 'in type)
                  (car args))))
      (:named
        (setf (defstruct-type-description-named-type)
              (if (null args)
                  type
                  (car args))))
      ((:CONS-KEYWORDS :keywords)
        (setf (defstruct-type-description-cons-keywords) args))
      (:DEFSTRUCT-KEYWORDS
       (SETF (DEFSTRUCT-TYPE-DESCRIPTION-DEFSTRUCT-KEYWORDS) ARGS))
      (:defstruct
        (or (> (length args) 1)
            (defstruct-error
              "Bad :DEFSTRUCT option in DEFSTRUCT-DEFINE-TYPE"
              (car options) 'in type))
        (let ((name (defstruct-append-symbols type "-DEFSTRUCT-EXPAND")))
          (setf (defstruct-type-description-defstruct-expander) name)
          (push `(defun ,name ,@args) returns)))
      (t
       (let ((new (defstruct-retry-keyword op)))
         (unless (eq op new)
           (setq op new)
           (go AGAIN))
         (defstruct-error
           "Unknown option to DEFSTRUCT-DEFINE-TYPE"
           op 'in type))))))

;#+LispM
;(defprop :make-array t :defstruct-option)

#+LispM
(defstruct-define-type :array
  (:named :named-array)
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
                               description etc nil nil nil 1 NIL))
  (:ref (n description arg)
    `(,(select-aref-form description) ,arg ,n)))

;ideally, this would expand into (AREF-TYPED <type> <array> <index>).  Unfortunately, that might
; break too many things.
(defun select-aref-form (description &aux tem)
  (cond ((setq tem (assq :aref-function (defstruct-description-property-alist description)))
         (cdr tem))
        (t 'AREF)))

#+LispM
(defstruct-define-type :named-array
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :named (:overhead 1)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,(1+ i)))
                               description etc nil t nil 1 NIL))
  (:ref (n description arg)
    description         ;ignored
    `(,(select-aref-form description) ,arg ,(1+ n)))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

1#+MacLisp
(defstruct-define-type :array
  (:cons (arg description etc) :alist
    etc
    (maclisp-array-for-defstruct arg description 't))
  (:ref (n description arg)
    description*               1;ignored
    `(arraycall t ,arg ,n)))

#+NIL
(defstruct-define-type :array
  (:cons (arg description etc) :alist
    etc
    (NIL-array-for-defstruct arg description))
  (:ref (n description arg)
    description*               1;ignored
    `(aref ,arg ,n)))*

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :TYPED-ARRAY             ;an array with the named-structure-symbol
  (:NAMED :NAMED-TYPED-ARRAY)                   ;(if any) in the leader
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC NIL NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N)))


#+LISPM
(DEFSTRUCT-DEFINE-TYPE :NAMED-TYPED-ARRAY       ;type in leader -- data in array
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :NAMED
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC NIL T NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (TYPEP X ',(DEFSTRUCT-DESCRIPTION-NAME)))))

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :VECTOR                  ;same as :TYPED-ARRAY
  (:NAMED :PHONY-NAMED-VECTOR)                  ;except for this
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC NIL NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N)))

#+LISPM
(DEFUN SIMPLER-VECTOR-CONS-FOR-DEFSTRUCT (ARG DESCRIPTION EXTRA-CODE)
  (LET* ((OVERHEAD (DEFSTRUCT-TYPE-DESCRIPTION-OVERHEAD (GET (DEFSTRUCT-DESCRIPTION-TYPE)
                                                             'DEFSTRUCT-TYPE-DESCRIPTION)))
         (SIZE (+ (DEFSTRUCT-DESCRIPTION-SIZE) OVERHEAD))
         (SET-UPS '()) (VECTOR (GENSYM)))
    (DOLIST (ELT ARG)
      (LET ((INITVALUE (cdr ELT)))
        (UNLESS (EQ INITVALUE NIL)
          (PUSH `(ASET ,(CDR ELT) ,VECTOR ,(+ OVERHEAD (CAR ELT))) SET-UPS))))
    (LET ((MAKE-ARRAY `(MAKE-ARRAY ,SIZE)))
      (IF (OR EXTRA-CODE SET-UPS)
          `((LAMBDA (,VECTOR) ,@(NREVERSE SET-UPS) ,(AND EXTRA-CODE (FUNCALL EXTRA-CODE VECTOR)) ,VECTOR) ,MAKE-ARRAY)
        MAKE-ARRAY))))


#+LISPM
(DEFSTRUCT-DEFINE-TYPE :PHONY-NAMED-VECTOR
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:OVERHEAD 1)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    ETC
    (SIMPLER-VECTOR-CONS-FOR-DEFSTRUCT
      ARG DESCRIPTION #'(LAMBDA (V) `(SETF (AREF ,V 0) ',(DEFSTRUCT-DESCRIPTION-NAME)))))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,(+ N 1)))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (AND (VECTORP X) (NOT (ZEROP (ARRAY-LENGTH X)))
            (EQ (AREF X 0) ',(DEFSTRUCT-DESCRIPTION-NAME))))))

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :NAMED-VECTOR            ;same as :NAMED-TYPED-ARRAY
  (:CONS-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :NAMED
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC NIL T NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    DESCRIPTION         ;ignored
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N))
  (:PREDICATE (DESCRIPTION NAME)
    `(DEFSUBST ,NAME (X)
       (TYPEP X ',(DEFSTRUCT-DESCRIPTION-NAME)))))

1#+(or MacLisp-10 NIL)
(defstruct-define-type :vector*
  1(:named :named-vector)*
  1(:cons (arg description etc) :list*
        1description*                         1;ignored*
        1etc*                                 1;ignored*
        1`(vector ,@arg))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(vref ,arg ,n)))

#+(or MacLisp-10 NIL)
(defstruct-define-type :named-vector*
  1:named (:overhead 1)*
  1(:cons (arg description etc) :list*
        1etc*                                 1;ignored*
        1`(vector ',(defstruct-description-name) ,@arg))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(vref ,arg ,(1+ n)))*
  1(:predicate (description name)*
           1`(defun ,name (x)*
              1(and (vectorp x)*
                  1(eq (vref x 0) ',(defstruct-description-name))))))*


#+LispM
(defstruct-define-type :fixnum-array
  (:CONS-KEYWORDS :make-array)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY)
  (:NAMED :NAMED-FIXNUM-ARRAY)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
                               description etc 'art-inum nil nil 1 NIL))
  (:ref (n description arg)
    `(,(select-aref-form description) ,arg ,n)))

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :NAMED-FIXNUM-ARRAY
  (:CONS-KEYWORDS :MAKE-ARRAY)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY)
  :NAMED
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC 'ART-inum NIL NIL 1 NIL))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N)))

1#+MacLisp
(defstruct-define-type :fixnum-array*
  1(:cons (arg description etc) :alist*
        1etc*
        1(maclisp-array-for-defstruct arg description 'fixnum))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(arraycall fixnum ,arg ,n)))*

#+LispM
(defstruct-define-type :flonum-array
  (:CONS-KEYWORDS :make-array)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY)
  (:NAMED :NAMED-FLONUM-ARRAY)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i) `(aset ,v ,a ,i))
                               description etc 'art-float nil nil 1 NIL))
  (:ref (n description arg)
    `(,(select-aref-form description) ,arg ,n)))

#+LISPM
(DEFSTRUCT-DEFINE-TYPE :NAMED-FLONUM-ARRAY
  (:CONS-KEYWORDS :MAKE-ARRAY)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY)
  :NAMED
  (:CONS (ARG DESCRIPTION ETC) :ALIST
    (LISPM-ARRAY-FOR-DEFSTRUCT ARG #'(LAMBDA (V A I) `(ASET ,V ,A ,I))
                               DESCRIPTION ETC 'ART-FLOAT NIL NIL 1 T))
  (:REF (N DESCRIPTION ARG)
    `(,(SELECT-AREF-FORM DESCRIPTION) ,ARG ,N)))

1#+MacLisp
(defstruct-define-type :flonum-array*
  1(:cons (arg description etc) :alist*
        1etc*
        1(maclisp-array-for-defstruct arg description 'flonum))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(arraycall flonum ,arg ,n)))

#+MacLisp-10
(defstruct-define-type :un-gc-array*
  1(:cons (arg description etc) :alist*
        1etc*                                 1;ignored*
        1(maclisp-array-for-defstruct arg description nil))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(arraycall nil ,arg ,n)))*

#+LispM
(defstruct-define-type :array-leader
  (:named :named-array-leader)
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct arg #'(lambda (v a i)
                                       `(store-array-leader ,v ,a ,i))
                               description etc nil nil t 1 NIL))
  (:ref (n description arg)
    description         ;ignored
    `(array-leader ,arg ,n)))

#+LispM
(defstruct-define-type :named-array-leader
  (:CONS-KEYWORDS :make-array :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :SUBTYPE)
  :named (:overhead 1)
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i)
          `(store-array-leader ,v ,a ,(if (zerop i)
                                          0
                                          (1+ i))))
      description etc nil t t 1 T))
  (:ref (n description arg)
    description         ;ignored
    (if (zerop n)
        `(array-leader ,arg 0)
        `(array-leader ,arg ,(1+ n))))
  (:predicate (description name)
    `(defsubst ,name (x)
       (typep x ',(defstruct-description-name)))))

;#+LispM
;(defprop :times t :defstruct-option)

#+LispM
(defstruct-define-type :grouped-array
  (:CONS-KEYWORDS :make-array :times :SUBTYPE)
  (:DEFSTRUCT-KEYWORDS :MAKE-ARRAY :TIMES :SUBTYPE)
  (:DEFSTRUCT (DESCRIPTION)
    (DEFSTRUCT-HACK-ARRAY-SUPERTYPE DESCRIPTION))
  (:cons (arg description etc) :alist
    (lispm-array-for-defstruct
      arg
      #'(lambda (v a i) `(aset ,v ,a ,i))
      description etc nil nil nil
      (or (cdr (or (assq ':times etc)
                   (assq ':times (defstruct-description-property-alist))))
          1)
      NIL))
  (:ref (n description index arg)
    (cond ((numberp index)
           `(,(select-aref-form description) ,arg ,(+ n index)))
          ((zerop n)
           `(,(select-aref-form description) ,arg ,index))
          (t `(,(select-aref-form description) ,arg (+ ,n ,index))))))

#+LISPM
(DEFUN IMPLICIT-ARRAY-ELEMENT-VALUE (ARRAY-TYPE MAKE-ARRAY-ARGUMENTS)
  (IF (OR (GET MAKE-ARRAY-ARGUMENTS :INITIAL-ELEMENT)
          (GET MAKE-ARRAY-ARGUMENTS :INITIAL-VALUE))
      (DEFSTRUCT-MAKE-EMPTY)
    (CASE ARRAY-TYPE
      ((NIL ART-Q ART-Q-LIST) NIL)
      ((ART-32B ART-16B ART-8B ART-4B ART-2B ART-1B ART-HALF-FIX
                ART-STRING ART-FAT-STRING)
       0)
      ((ART-FLOAT ART-FPS-FLOAT)
       0.0)
      (ART-COMPLEX
       0+0i)
      ((ART-COMPLEX-FLOAT ART-COMPLEX-FPS-FLOAT)
       0.0+0.0i)
      (T (DEFSTRUCT-MAKE-EMPTY)))))

;;; this is starting too get too hairy to want to evaluate every time we cons up a structure.
;;; more knowledge should be built into the constructors themselves, for simple cases
#+LispM
(defun lispm-array-for-defstruct (arg
                                  cons-init
                                  description
                                  etc
                                  type
                                  &OPTIONAL (NAMED-P NIL)
                                            (LEADER-P NIL)
                                            (TIMES 1)
                                            (TYPE-IN-LEADER NIL)
                                  &AUX (P (CONS NIL NIL))
                                       NO-OP
                                       ARRAY-TYPE)
  ;; arg is slot arg
  ;; cons-init is code to initialize the structure per-slot
  ;; description is a structure description
  ;; etc is cons-keyword args/values
  ;; type is the array-type to make
  ;; named-p is t if to make a named structure
  ;; leader-p is t if the data is to be stored in the leader (as in :{named-}array-leader)
  ;; times if the #times for :grouped-array
  ;; type-in-leader is t if the structure-type is to be put in array-leader 1 rather than
  ;;  in aref 0
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array (defstruct-description-property-alist)))
    p)
  (defstruct-grok-make-array-args
    (cdr (assq ':make-array etc))
    p)
  (COND (TYPE
         (PUTPROP P TYPE ':TYPE))
        ((SETQ TYPE (CDR (ASSQ ':SUBTYPE ETC)))
         (PUTPROP P `',(SETQ TYPE (OR (IGNORE-ERRORS (ARRAY-CANONICALIZE-TYPE TYPE)) TYPE))
                  ':TYPE))
        ((SETQ TYPE (DEFSTRUCT-DESCRIPTION-SUBTYPE))
         (PUTPROP P `',(SETQ TYPE (OR (IGNORE-ERRORS (ARRAY-CANONICALIZE-TYPE TYPE)) TYPE))
                  ':TYPE)))
  (and named-p (putprop p `',(defstruct-description-name) ':named-structure-symbol))
  (LET* ((S (OR (GET P (IF LEADER-P ':LEADER-LENGTH ':DIMENSIONS)) 0))
         (SIZE (let ((size (if named-p
                               (1+ (defstruct-description-size))
                             (defstruct-description-size))))
                 (if (numberp times)
                     (MAX S (* size times))
                   `(MAX ,S (* ,size ,times))))))
    (putprop p SIZE (if leader-p ':leader-length ':dimensions)))
  (AND TYPE-IN-LEADER (OR (NOT (GET P ':LEADER-LENGTH))
                          (< (GET P ':LEADER-LENGTH) 2))
       (PUTPROP P 2 ':LEADER-LENGTH))
  (SETQ ARRAY-TYPE (OR (LET ((TYPE (GET P ':TYPE)))
                         (IF (EQ (CAR-SAFE TYPE) 'QUOTE)
                             (SETQ TYPE (CADR TYPE)))
                         (OR (IGNORE-ERRORS (ARRAY-CANONICALIZE-TYPE TYPE)) TYPE))
                       'ART-Q))
  (UNLESS LEADER-P
    (SETQ NO-OP (IMPLICIT-ARRAY-ELEMENT-VALUE ARRAY-TYPE P)))
  ;; make sure that we can store the named-structure-symbol safely
  (OR (NOT NAMED-P)
      (MEMQ ARRAY-TYPE '(ART-Q ART-Q-LIST ART-SPECIAL-PDL ART-REG-PDL ART-STACK-GROUP-HEAD))
      (GET P ':LEADER-LENGTH)
      (SETQ ARRAY-TYPE 'ART-Q)
      (PUTPROP P 'ART-Q ':TYPE))
  (do ((creator
         (let ((dims (remprop p ':dimensions)))
           (do ((l (cdr p) (cddr l)))
               ((null l))
             (rplaca l `',(car l)))
           `(make-array ,(if (null dims) 0 (car dims)) ,@(cdr p))))
       (var (gensym))
       (set-ups nil (if (equal (cdar l) no-op)
                        set-ups
                      (PUSH (funcall cons-init (cdar l) var (caar l)) SET-UPS)))
       (l arg (cdr l)))
      ((null l)
       ;; If we want the structure type stored but not a named-structure,
       ;; generate code to store it explicitly.
       (IF (AND TYPE-IN-LEADER (NOT NAMED-P))
           (PUSH `(SETF (ARRAY-LEADER ,VAR 1) ',(DEFSTRUCT-DESCRIPTION-NAME))
                 SET-UPS))
       (if set-ups
           `((lambda (,var)
               ,@(nreverse set-ups)
               ,var)
             ,creator)
         creator))))

#+LISPM
(DEFUN DEFSTRUCT-GROK-MAKE-ARRAY-ARGS (ARGS P)
  (DO ((L ARGS (CDDR L)))
      ((NULL L) P)
    (UNLESS (AND (CDR L)
                 (MEMQ (CAR L) '(:AREA :TYPE :DISPLACED-TO :LEADER-LIST
                                 :LEADER-LENGTH :DISPLACED-INDEX-OFFSET
                                 :NAMED-STRUCTURE-SYMBOL :DIMENSIONS
                                 :LENGTH :INITIAL-VALUE :INITIAL-ELEMENT :FILL-POINTER
                                 :ELEMENT-TYPE)))
      (DEFSTRUCT-ERROR
        "DEFSTRUCT can't grok these MAKE-ARRAY arguments"
        ARGS))
    (PUTPROP P
             (CADR L)
             (IF (EQ (CAR L) ':LENGTH)
                 ':DIMENSIONS
                 (CAR L)))))
#+LISPM
(DEFUN DEFSTRUCT-HACK-ARRAY-SUPERTYPE (DESCRIPTION)
  (OR (DEFSTRUCT-DESCRIPTION-SUBTYPE)
      (DO* ((SL (DEFSTRUCT-DESCRIPTION-SLOT-ALIST) (CDR SL))
            (SLOT-TYPE)
            (TY ART-ERROR))
           ((OR (NULL SL) (MEMQ TY '(ART-Q ART-ERROR)))
            (IF (EQ TY 'ART-ERROR) (SETQ TY 'ART-Q))
            (SETF (DEFSTRUCT-DESCRIPTION-SUBTYPE) TY))
        (SETQ SLOT-TYPE (DEFSTRUCT-SLOT-DESCRIPTION-TYPE (CDAR SL)))
        (IF (MEMQ SLOT-TYPE '(NIL NOTYPE)) (SETQ SLOT-TYPE T))
        (SETQ TY (ARRAY-TYPE-SUPERTYPE TY (ARRAY-CANONICALIZE-TYPE SLOT-TYPE)))))
  NIL)

1#+NIL
(defun nil-array-for-defstruct (arg description)*
  1(do ((creator `(make-array ',(defstruct-description-size)))*
      1(var (gensym))*
      1(set-ups nil (if (null (cdar l))*
                   1set-ups*
                  1(cons `(aset ,(cdar l) ,var ,(caar l))*
                       1set-ups)))*
      1(l arg (cdr l)))*
     1((null l)*
      1(if set-ups*
         1`((lambda (,var)*
             1,@(nreverse set-ups)*
             1,var)*
           1,creator)*
        1creator))))

#+MacLisp
(defun maclisp-array-for-defstruct (arg description type)*
  1(do ((creator `(array nil ,type ,(defstruct-description-size)))*
      1(var (gensym))*
      1(no-op (caseq type*
                 1(fixnum 0)*
                 1(flonum 0.0)*
                 1((t nil) nil)))*
      1(set-ups nil (if (equal (cdar l) no-op)*
                   1set-ups*
                  1(cons `(store (arraycall ,type ,var ,(caar l))*
                             1,(cdar l))*
                       1set-ups)))*
      1(l arg (cdr l)))*
     1((null l)*
      1(if set-ups*
         1`((lambda (,var)*
             1,@(nreverse set-ups)*
             1,var)*
           1,creator)*
        1creator))))*

1;#+(or MacLisp-10 NIL)
;(defprop :sfa-function t :defstruct-option)

;#+(or MacLisp-10 NIL)
;(defprop :sfa-name t :defstruct-option)

#+(or MacLisp-10 NIL)
(defstruct-define-type :sfa*
  1(:CONS-KEYWORDS :sfa-function :sfa-name)*
  1(:DEFSTRUCT-KEYWORDS :SFA-NAME :SFA-FUNCTION)*
  1(:cons (arg description etc) :alist*
        1(do ((creator `(sfa-create ,(or (cdr (or (assq ':sfa-function etc)*
                                       1(assq ':sfa-function (defstruct-description-property-alist))))*
                                1`',(defstruct-description-name))*
                             1,(defstruct-description-size)*
                             1,(or (cdr (or (assq ':sfa-name etc)*
                                        1(assq ':sfa-name (defstruct-description-property-alist))))*
                                 1`',(defstruct-description-name))))*
            1(l arg (cdr l))*
            1(var (gensym))*
            1(set-ups nil (if (null (cdar l))*
                         1set-ups*
                        1(cons `(sfa-store ,var ,(caar l)*
                                      1,(cdar l))*
                             1set-ups))))*
           1((null l)*
            1(if set-ups*
               1`((lambda (,var)*
                   1,@(nreverse set-ups)*
                   1,var)*
                 1,creator)*
              1creator))))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(sfa-get ,arg ,n))*
  1(:predicate (description name)*
           1`(defun ,name (x)*
              1(and (sfap x)*
                  1(eq (sfa-get x 'pname)*
                     1,(or (cdr (assq ':sfa-name (defstruct-description-property-alist)))*
                         1`',(defstruct-description-name)))))))

#+MacLisp-10
(defstruct-define-type :hunk*
  1(:named :named-hunk)*
  1(:cons (arg description etc) :list*
        1description*                         1;ignored*
        1etc*                                 1;ignored*
        1(if arg*
           1`(hunk ,.(nconc (cdr arg) (ncons (car arg))))*
          1(defstruct-error "No slots in hunk type defstruct")))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(cxr ,n ,arg)))

#+MacLisp-10
(defstruct-define-type :named-hunk*
  1:named (:overhead 1)*
  1(:cons (arg description etc) :list*
        1etc*                                 1;ignored*
        1(if arg*
           1`(hunk ',(defstruct-description-name)*
                 1,.(nconc (cdr arg) (ncons (car arg))))*
          1`(hunk ',(defstruct-description-name) nil)))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1(cond ((= n 0) `(cxr 0 ,arg))*
            1(t `(cxr ,(1+ n) ,arg))))*
  1(:predicate (description name)*
           1`(defun ,name (x)*
              1(and (hunkp x)*
                  1(eq (car x) ',(defstruct-description-name))))))*


1;#+NIL
;(defprop :class-symbol t :defstruct-option)

#+NIL
(defstruct-define-type :extend*
  1:named*
  1(:DEFSTUCT-KEYWORDS :CLASS-SYMBOL)*
  1(:defstruct (description)*
    1(if (assq ':class-symbol (defstruct-description-property-alist))*
       1;; if class-symbol is given then assume user is setting up*
       1;; his own class.*
       1()*
      1(let* ((name (defstruct-description-name))*
            1(class-symbol (defstruct-append-symbols name "-CLASS")))*
        1(push (cons ':class-symbol class-symbol)*
             1(defstruct-description-property-alist))*
        1`((defstruct-class-setup ,name ,class-symbol)))))*
  1(:cons (arg description etc) :alist*
        1etc*                                 1;ignored*
        1(do ((l arg (cdr l))*
            1(creator `(si:make-extend*
                      1,(defstruct-description-size)*
                      1,(cdr (assq ':class-symbol*
                               1(defstruct-description-property-alist)))))*
            1(var (gensym))*
            1(set-ups () (if (null (cdar l))*
                        1set-ups*
                       1(cons `(si:xset ,var ,(caar l) ,(cdar l))*
                            1set-ups))))*
           1((null l)*
            1(if set-ups*
               1`((lambda (,var)*
                   1,.(nreverse set-ups)*
                   1,var)*
                 1,creator)*
              1creator))))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(si:xref ,arg ,n))*
  1(:predicate (description name)*
           1`(defsubst ,name (x)*
              1(of-type x ',(defstruct-description-name)))))*

(defstruct-define-type :list
  (:named :named-list)
  (:cons (arg description etc) :list
    description                                 ;ignored
    etc                                         ;ignored
    `(list ,.arg))
  (:ref (n description arg)
    description                                 ;ignored
    1#+Multics*
    1`(,(let ((i (\ n 4)))*
        1(cond ((= i 0) 'car)*
             1((= i 1) 'cadr)*
             1((= i 2) 'caddr)*
             1(t 'cadddr)))*
      1,(do ((a arg `(cddddr ,a))*
           1(i (// n 4) (1- i)))*
          1((= i 0) a)))*
    #-Multics
    `(nth ,n ,arg))
  (:copier (description name)
           (do ((l `((car x)) (cons `(prog1 (car x) (setq x (cdr x))) l))
                (i (defstruct-description-size) (1- i)))
               ((<= i 1)
                `(defun ,name (x)
                   (list ,@l))))))

(defstruct-define-type :named-list
  (:overhead 1)
  (:cons (arg description etc) :list
    etc                                         ;ignored
    `(list ',(defstruct-description-name) ,.arg))
  (:ref (n description arg)
    description                                 ;ignored
    1#+Multics*
    1`(,(let ((i (\ (1+ n) 4)))*
        1(cond ((= i 0) 'car)*
             1((= i 1) 'cadr)*
             1((= i 2) 'caddr)*
             1(t 'cadddr)))*
      1,(do ((a arg `(cddddr ,a))*
           1(i (// (1+ n) 4) (1- i)))*
          1((= i 0) a)))*
    #-Multics
    `(nth ,(1+ n) ,arg))
  (:predicate (description name)
              `(defun ,name (x)
                 (and
                   #-MacLisp-10
                   (not (atom x))
                   1#+MacLisp-10*                     1;Watch out for hunks!*
                   1(eq (typep x) 'list)*
                   (eq (car x) ',(defstruct-description-name)))))
  (:copier (description name)
           (do ((l `((car x)) (cons `(prog1 (car x) (setq x (cdr x))) l))
                (i (defstruct-description-size) (1- i)))
               ((<= i 1)
                `(defun ,name (x)
                   (setq x (cdr x))
                   (list ',(defstruct-description-name) ,@l))))))

(defstruct-define-type :list*
  (:cons (arg description etc) :list
         description                            ;ignored
         etc                                    ;ignored
         `(list* ,.arg))
  (:ref (n description arg)
        (let ((size (1- (defstruct-description-size))))
          1#+Multics*
          1(do ((a arg `(cddddr ,a))*
              1(i (// n 4) (1- i)))*
             1((= i 0)*
              1(let* ((i (\ n 4))*
                    1(a (cond ((= i 0) a)*
                           1((= i 1) `(cdr ,a))*
                           1((= i 2) `(cddr ,a))*
                           1(t `(cdddr ,a)))))*
                1(if (< n size) `(car ,a) a))))*
          #-Multics
          (if (< n size)
              `(nth ,n ,arg)
            `(nthcdr ,n ,arg))))
  (:defstruct (description)
    (and (defstruct-description-include)
         (defstruct-error
           "Structure of type :LIST* cannot include another"
           (defstruct-description-name)))
    nil)
  (:copier (description name)
           (do ((l `(x) (cons `(prog1 (car x) (setq x (cdr x))) l))
                (i (defstruct-description-size) (1- i)))
               ((<= i 1)
                `(defun ,name (x)
                   (list* ,@l))))))

(defstruct-define-type :tree
  (:cons (arg description etc) :list
    etc                                         ;ignored
    (if (null arg) (defstruct-error
                     "defstruct cannot make an empty tree"
                     (defstruct-description-name)))
    (make-tree-for-defstruct arg (defstruct-description-size)))
  (:ref (n description arg)
        (do ((size (defstruct-description-size))
             (a arg)
             (tem))
            (nil)
          (cond ((= size 1) (return a))
                ((< n (setq tem (defstruct-divide size 2)))
                 (setq a `(car ,a))
                 (setq size tem))
                (t (setq a `(cdr ,a))
                   (setq size (- size tem))
                   (setq n (- n tem))))))
  (:defstruct (description)
    (and (defstruct-description-include)
         (defstruct-error
           "Structure of type :TREE cannot include another"
           (defstruct-description-name)))
    nil)
  (:copier (description name)
           `(defun ,name (x)
              ,(copy-tree-for-defstruct nil (defstruct-description-size)))))

(defun make-tree-for-defstruct (arg size)
  (cond ((= size 1) (car arg))
        ((= size 2) `(cons ,(car arg) ,(cadr arg)))
        (t (do ((a (cdr arg) (cdr a))
                (m (defstruct-divide size 2))
                (n (1- (defstruct-divide size 2)) (1- n)))
               ((zerop n)
                `(cons ,(make-tree-for-defstruct arg m)
                       ,(make-tree-for-defstruct a (- size m))))))))

(defun copy-tree-for-defstruct (popx? size)
  (cond ((= size 1)
         (if popx?
             `(prog1 (car x) (setq x (cdr x)))
             `x))
        ((= size 2)
         (if popx?
             `((lambda (x) (cons (car x) (cdr x)))
               (prog1 (car x) (setq x (cdr x))))
             `(cons (car x) (cdr x))))
        (popx?
         `((lambda (x)
             (cons ,(copy-tree-for-defstruct t (defstruct-divide size 2))
                   ,(copy-tree-for-defstruct nil (- size (defstruct-divide size 2)))))
           (prog1 (car x) (setq x (cdr x)))))
        (t
         `(cons ,(copy-tree-for-defstruct t (defstruct-divide size 2))
                ,(copy-tree-for-defstruct nil (- size (defstruct-divide size 2)))))))

(defstruct-define-type :fixnum
  (:cons (arg description etc) :list
    etc                                         ;ignored
    (and (or (null arg)
             (not (null (cdr arg))))
         (defstruct-error
           "Structure of type :FIXNUM must have exactly 1 slot to be constructable"
           (defstruct-description-name)))
    (car arg))
  (:ref (n description arg)
    n                                           ;ignored
    description                                 ;ignored
    arg))

1;#+Multics
;(defprop :external-ptr t :defstruct-option)

#+Multics
(defstruct-define-type :external*
  1(:CONS-KEYWORDS :external-ptr)*
  1(:DEFSTRUCT-KEYWORDS :EXTERNAL-PTR)*
  1(:cons (arg description etc) :alist*
        1(let ((ptr (cdr (or (assq ':external-ptr etc)*
                       1(assq ':external-ptr*
                            1(defstruct-description-property-alist))*
                       1(defstruct-error*
                         1"No pointer given for external array"*
                         1(defstruct-description-name))))))*
          1(do ((creator `(array nil external ,ptr ,(defstruct-description-size)))*
              1(var (gensym))*
              1(alist arg (cdr alist))*
              1(inits nil (cons `(store (arraycall fixnum ,var ,(caar alist))*
                                 1,(cdar alist))*
                           1inits)))*
             1((null alist)*
              1(if (null inits)*
                 1creator*
                1`((lambda (,var) ,.inits ,var)*
                  1,creator))))))*
  1(:ref (n description arg)*
       1description*                                  1;ignored*
       1`(arraycall fixnum ,arg ,n)))*

(defvar *defstruct-examine&deposit-arg*)

(defun defstruct-examine (*defstruct-examine&deposit-arg*
                          name slot-name)
  name slot-name
  (eval (list (defstruct-slot-description-ref-macro-name
                (defstruct-examine&deposit-find-slot-description
                  name slot-name))
              '*defstruct-examine&deposit-arg*)))

(defvar *defstruct-examine&deposit-val*)

(defun defstruct-deposit (*defstruct-examine&deposit-val*
                          *defstruct-examine&deposit-arg*
                          name slot-name)
  name slot-name
  (eval (list 'setf
              (list (defstruct-slot-description-ref-macro-name
                     (defstruct-examine&deposit-find-slot-description
                       name slot-name))
                    '*defstruct-examine&deposit-arg*)
              '*defstruct-examine&deposit-val*)))

#+LispM
(defun defstruct-get-locative (*defstruct-examine&deposit-arg*
                               name slot-name)
  (let ((slot-description (defstruct-examine&deposit-find-slot-description
                            name slot-name)))
    (or (null (defstruct-slot-description-ppss))
        (defstruct-error
          "You cannot get a locative to a byte field"
          slot-name 'in name))
    (eval (list 'locf
                (list (defstruct-slot-description-ref-macro-name)
                      '*defstruct-examine&deposit-arg*)))))

(defun defstruct-examine&deposit-find-slot-description (name slot-name)
  (let ((description (get-defstruct-description name)))
    (let ((slot-description
            (cdr (or (assq slot-name (defstruct-description-slot-alist))
                     (defstruct-error
                       "No such slot in this structure"
                       slot-name 'in name))))
          (type-description
            (or (get (defstruct-description-type) 'defstruct-type-description)
                (defstruct-error
                  "Undefined defstruct type"
                  (defstruct-description-type)))))
      (or (= (defstruct-type-description-ref-no-args) 1)
          (defstruct-error
            "defstruct-examine and defstruct-deposit cannot handle structures of this type"
            (defstruct-description-type)))
      slot-description)))

#+LISPM
(DEFUN DESCRIBE-DEFSTRUCT-DESCRIPTION (NAME)
  (DESCRIBE-DEFSTRUCT (GET-DEFSTRUCT-DESCRIPTION NAME) 'DEFSTRUCT-DESCRIPTION))


1#+MacLisp-10
(defprop defstruct*
       1#.(and (status feature MacLisp-10)*
             1(caddr (truename infile)))*
       1version)*

#+(and lispm mit)
;; Don't use PUSHNEW, as that would probably use PUSH.
(unless (memq :defstruct *features*)
  ;; Don't use PUSH; it's not loaded yet.
  (setq *features* (cons :defstruct *features*)))

1#-(and lispm mit)
(sstatus feature defstruct)
