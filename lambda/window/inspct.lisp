;;; -*- Mode:LISP; Package:TV; Base:8; Readtable:ZL -*-
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;;;; Inspect structures

(DEFSTRUCT (STACK-FRAME :NAMED (:ALTERANT NIL))
  STACK-FRAME-SG
  STACK-FRAME-AP
  STACK-FRAME-FUNCTION-NAME)

(DEFSELECT ((:PROPERTY STACK-FRAME NAMED-STRUCTURE-INVOKE))
  ((:PRINT-SELF) (SF STREAM &REST IGNORE &AUX (AP (STACK-FRAME-AP SF))
                                              (RP (SG-REGULAR-PDL (STACK-FRAME-SG SF)))
                                              (FUNCTION (RP-FUNCTION-WORD RP AP))
                                              (PC (IF (TYPEP FUNCTION 'COMPILED-FUNCTION)
                                                      (RP-EXIT-PC RP AP))))
   (LET ((*PRINT-LENGTH* 5) (*PRINT-LEVEL* 3))
     (SI:PRINTING-RANDOM-OBJECT (SF STREAM :NO-POINTER)
       (FORMAT STREAM "Stack-Frame ~A ~[PC=~D~;microcoded~;interpreted~]"
               (FUNCTION-NAME FUNCTION)
               (COND (PC 0)
                     ((TYPEP FUNCTION 'MICROCODE-FUNCTION) 1)
                     (T 2))
               PC)))))

(DEFFLAVOR INSPECT-WINDOW ()
           (BASIC-INSPECT
            FUNCTION-TEXT-SCROLL-WINDOW MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
            FLASHY-MARGIN-SCROLLING-MIXIN
            BORDERS-MIXIN MARGIN-REGION-MIXIN TOP-LABEL-MIXIN BASIC-SCROLL-BAR
            WINDOW)
  (:DEFAULT-INIT-PLIST :MARGIN-SCROLL-REGIONS '((:TOP "Top of object")
                                                (:BOTTOM "Bottom of object"))
                       :FLASHY-SCROLLING-REGION '((16. 0.40s0 0.60s0)
                                                  (16. 0.40s0 0.60s0))
                       :LABEL (LIST NIL NIL NIL NIL FONTS:HL12B "Empty"))
  (:DOCUMENTATION "Scroll window for the inspector."))

(DEFFLAVOR BASIC-INSPECT ((CURRENT-OBJECT (NCONS NIL))
                          (CURRENT-DISPLAY NIL)
                          ;; For list structure hacking
                          (DISPLAYING-LIST NIL)
                          (MODIFY-MODE NIL)
                          LIST-BLINKER DOCUMENTATION-STRINGS)
           ()
  :SETTABLE-INSTANCE-VARIABLES
  (:REQUIRED-FLAVORS MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW FUNCTION-TEXT-SCROLL-WINDOW)
  (:INIT-KEYWORDS :BUTTON-DOCUMENTATION))

(DEFMETHOD (BASIC-INSPECT :SENSITIVE-ITEM-P) (ITEM)
  (LET ((MODIFYING (NOT (NULL (OR MODIFY-MODE (KEY-STATE :HYPER))))))
    (EQ (NOT (NULL (GET (DISPLAYED-ITEM-TYPE ITEM)
                        (IF MODIFYING 'SET-FUNCTION 'ONLY-WHEN-MODIFY))))
        MODIFYING)))

(DEFMETHOD (BASIC-INSPECT :AFTER :INIT) (PLIST)
  (LET ((DOC (OR (GET PLIST :BUTTON-DOCUMENTATION)
                 '("Right finds function definition."))))
    (SETQ DOCUMENTATION-STRINGS
          (LIST (STRING-APPEND "Choose a CAR to be modified.  " (OR (SECOND DOC) ""))
                (STRING-APPEND "Choose a CAR.  " (OR (FIRST DOC) ""))
                (STRING-APPEND  "Choose a slot to be modified, by pointing at the slot.  "
                                (OR (SECOND DOC) ""))
                (STRING-APPEND "Choose a value by pointing at the value.  "
                               (OR (FIRST DOC) "")))))
  (SETQ LIST-BLINKER (MAKE-BLINKER SELF 'FOLLOW-LIST-STRUCTURE-BLINKER :VISIBILITY NIL)
        SENSITIVE-ITEM-TYPES :SENSITIVE-ITEM-P))

(DEFMETHOD (BASIC-INSPECT :WHO-LINE-DOCUMENTATION-STRING) ()
  (LET ((STRINGS DOCUMENTATION-STRINGS))
    (OR DISPLAYING-LIST (SETQ STRINGS (CDDR STRINGS)))
    (OR MODIFY-MODE (KEY-STATE :HYPER)
        (SETQ STRINGS (CDR STRINGS)))
    (CAR STRINGS)))

;;;Given an object and an inspector window, return a display list for the object.
;;;The elements of this list are:
;;; 1 the object
;;; 2 the printer function
;;; 3 an arg to give to the printer function
;;; 4 the list of /"items/" (for TEXT-SCROLL-WINDOW) or /"lines/".
;;;  Each item describes one line of data,
;;;  and will be passed to the printer function (element 2)
;;;  which should use it to print out the line.
;;;  The normal printer function is INSPECT-PRINT,
;;;  see its documentation for what the items (there called LINEs) can look like.
;;; 5 the top item number
;;; 6 the label
;;; 7 the ITEM-GENERATOR function (for TEXT-SCROLL-WINDOW), or NIL if none.

(defstruct (inspector-display-list (:type :list) (:alterant nil) (:conc-name idlist-))
  object
  print-function
  print-function-arg
  line-display-list
  top-item-number
  label
  item-generator)
(defmacro idlist-display-cruft (idlist) `(cdr ,idlist))

(DEFUN INSPECT-SETUP-OBJECT-DISPLAY-LIST (OBJECT WINDOW
                                          &OPTIONAL TOP-ITEM LABEL
                                          &AUX STR)
  (MULTIPLE-VALUE-BIND (DISPLAY-LIST ARG ALT-PRINT-FUN
                        FIRST-TOP-ITEM OBJ-LABEL ITEM-GENERATOR)
      (SEND WINDOW
            (IF (NAMED-STRUCTURE-P OBJECT)
                :OBJECT-NAMED-STRUCTURE
              (TYPECASE OBJECT
                (STACK-FRAME :OBJECT-STACK-FRAME)
                (INSTANCE :OBJECT-INSTANCE)
                (ARRAY :OBJECT-ARRAY)
                (SYMBOL :OBJECT-SYMBOL)
                (LIST :OBJECT-LIST)
                (SELECT :OBJECT-SELECT-METHOD)
                ((OR CLOSURE ENTITY) :OBJECT-CLOSURE)
                (COMPILED-FUNCTION :OBJECT-FEF)
                (LOCATIVE :OBJECT-LOCATIVE)
                (T :OBJECT-OTHER)))
            OBJECT)
    (make-inspector-display-list
      :object OBJECT
      :print-function (OR ALT-PRINT-FUN 'INSPECT-PRINTER)
      :print-function-arg ARG
      :line-display-list DISPLAY-LIST
      :top-item-number (OR TOP-ITEM FIRST-TOP-ITEM 0)
      :label (OR LABEL
                 OBJ-LABEL
                 (LIST NIL NIL NIL NIL
                       (LABEL-FONT (SEND WINDOW :LABEL))
                       (IF (CONSP OBJECT)
                           "a list"
                         (NSUBSTRING (SETQ STR (FORMAT NIL "~S~%" OBJECT))
                                     0 (STRING-SEARCH-CHAR #/NEWLINE STR)))))
      :item-generator ITEM-GENERATOR)))

(DEFUN INSPECT-SETUP-OBJECT (OBJECT WINDOW &OPTIONAL TOP-ITEM)
  (LET ((DISP (INSPECT-SETUP-OBJECT-DISPLAY-LIST OBJECT WINDOW TOP-ITEM)))
    (SEND WINDOW :SETUP (idlist-display-cruft DISP))
    (SEND WINDOW :SET-CURRENT-OBJECT (idlist-object DISP))
    DISP))

(DEFMETHOD (BASIC-INSPECT :SETUP-OBJECT) (SL)
  (SEND SELF :SETUP (idlist-display-cruft SL))
  (SEND SELF :SET-CURRENT-OBJECT (idlist-object SL))
  SL)

(DEFUN INSPECT-PRINTER (LINE ARG STREAM ITEM-NO)
  "This is the default printer for lines in inspect windows.
ARG is the printer-function-arg as produced by the :OBJECT-... operation.
ITEM-NO is the item number (a la TEXT-SCROLL-WINDOW) of this line.
STREAM is the pane we are printing on.
LINE is a list of elements telling us what to print:
 a number is a column to tab to,
 a string is just printed,
 a list starting with a string is args to FORMAT,
 a list (:FUNCTION function . args) means apply <function>
  to ARG, STREAM, ITEM-NO and the elements of <args>, to print.
 a list (:COLON number) means type a colon and tab to column <number>,
 a list (:ITEM1 type object printer) specifies a mouse-sensitive item
  of type <type> (a symbol) and printed by passing <object> to <printer>.
  If <printer> is omitted, PRINT-ITEM-CONCISELY is used."
  (DOLIST (ELT LINE)
    (COND ((NUMBERP ELT)
           (FORMAT STREAM "~VT" ELT))
          ((STRINGP ELT)
           (PRINC ELT STREAM))
          ((ATOM ELT)
           (FERROR NIL "Unknown element type: ~S" ELT))
          ((STRINGP (CAR ELT))
           (APPLY #'FORMAT STREAM ELT))
          (T
           (CASE (FIRST ELT)
             (:FUNCTION (APPLY (SECOND ELT) ARG STREAM ITEM-NO (CDDR ELT)))
             (:COLON (FORMAT STREAM ":~VT " (SECOND ELT)))
             (:ITEM1 (SEND STREAM :ITEM1 ELT (SECOND ELT)
                                        (LAMBDA (ELT &REST ARGS)
                                          (APPLY (OR (FOURTH ELT) #'PRINT-ITEM-CONCISELY)
                                                 (THIRD ELT) ARGS))))
             (OTHERWISE (FERROR NIL "Unknown item type ~A" (FIRST ELT))))))))

;;; Inspection of each type of object is done by a message, so that some of them
;;; may be redefined for some unspecified application
(DEFMETHOD (BASIC-INSPECT :OBJECT-NAMED-STRUCTURE)
           (OBJ &AUX (MAXL -1) ALIST DEFSTRUCT-ITEMS RESULT NSS D)
  (SETQ NSS (NAMED-STRUCTURE-P OBJ))
  (PUSH `("Named structure of type "
          (:ITEM1 NAMED-STRUCTURE-SYMBOL ,NSS))
        RESULT)
  (PUSH '("") RESULT)
  (COND ((AND (SYMBOLP NSS)
              (GET NSS 'NAMED-STRUCTURE-INVOKE)  ;N-S-I is in GLOBAL.
              (MEMQ :INSPECT-HANDLER
                    (NAMED-STRUCTURE-INVOKE OBJ :WHICH-OPERATIONS)))
         (CATCH-ERROR
           (MULTIPLE-VALUE (RESULT MAXL)
             (NAMED-STRUCTURE-INVOKE :INSPECT-HANDLER OBJ :INSPECT RESULT MAXL))))
        ((SETQ D (GET NSS 'SI::DEFSTRUCT-DESCRIPTION))
         (SETQ ALIST (SI::DEFSTRUCT-DESCRIPTION-SLOT-ALIST D))
         (DO ((L ALIST (CDR L))) ((NULL L))
           (SETQ MAXL (MAX (FLATSIZE (CAAR L)) MAXL)))
         ;; For a named structure, each line contains the name and the value
         (DO L ALIST (CDR L) (NULL L)
             (PUSH `((:ITEM1 NAMED-STRUCTURE-SLOT ,(CAAR L))
                     (:COLON ,(+ 2 MAXL))
                     (:ITEM1 NAMED-STRUCTURE-VALUE
                        ,(CATCH-ERROR
                           (FUNCALL (SI::DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDAR L))
                                    OBJ)
                           NIL)))
                   RESULT)))
        ((SETQ DEFSTRUCT-ITEMS (GET NSS 'SI::DEFSTRUCT-ITEMS))
         (DOLIST (ELT DEFSTRUCT-ITEMS)
           (SETQ MAXL (MAX (FLATSIZE ELT) MAXL)))
         ;; For a named structure, each line contains the name and the value
         (DOLIST (ELT DEFSTRUCT-ITEMS)
           (PUSH `((:ITEM1 NAMED-STRUCTURE-SLOT ,ELT)
                   (:COLON ,(+ 2 MAXL))
                   (:ITEM1 NAMED-STRUCTURE-VALUE ,(CATCH-ERROR (FUNCALL ELT OBJ) NIL)))
                 RESULT))))
  (IF (AND (ARRAYP OBJ)
           (ARRAY-HAS-LEADER-P OBJ))
      (SEND SELF :OBJECT-ARRAY OBJ NIL (NREVERSE RESULT))
    (VALUES (NREVERSE RESULT) OBJ 'INSPECT-PRINTER)))

(DEFUN (:PROPERTY NAMED-STRUCTURE-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT
                                                      &AUX (SLOTNAME (THIRD (SECOND ITEM)))
                                                      (REFMAC SLOTNAME)
                                                      TEM)
  (AND (SETQ TEM (GET (NAMED-STRUCTURE-P OBJECT) 'SI::DEFSTRUCT-DESCRIPTION))
       (SETQ TEM (ASSQ SLOTNAME (SI::DEFSTRUCT-DESCRIPTION-SLOT-ALIST TEM)))
       (SETQ REFMAC (SI::DEFSTRUCT-SLOT-DESCRIPTION-REF-MACRO-NAME (CDR TEM))))
  (EVAL `(SETF (,REFMAC ',OBJECT) ',NEW-VALUE)))
(DEFPROP NAMED-STRUCTURE-SLOT T ONLY-WHEN-MODIFY)


(DEFMETHOD (BASIC-INSPECT :OBJECT-INSTANCE) (OBJ &AUX (MAXL -1) RESULT FLAVOR)
  (SETQ FLAVOR (SI::INSTANCE-FLAVOR OBJ))
  (OR (TYPEP FLAVOR 'SI:FLAVOR) (SETQ FLAVOR NIL))
  (SETQ RESULT (LIST '("")
                     `("An object of flavor "
                       (:ITEM1 flavor ,flavor ,(lambda (f s) (prin1 (si::flavor-name f) s)))
                       ".  Function is "
                       (:ITEM1 FLAVOR-FUNCTION
                               ,(SI::INSTANCE-FUNCTION OBJ)))))
  (LET ((IVARS (IF FLAVOR (SI:FLAVOR-ALL-INSTANCE-VARIABLES FLAVOR)
                   (%P-CONTENTS-OFFSET (%P-CONTENTS-AS-LOCATIVE-OFFSET OBJ 0)
                                       %INSTANCE-DESCRIPTOR-BINDINGS))))
    (DO ((BINDINGS IVARS (CDR BINDINGS))
         (I 1 (1+ I)))
      ((NULL BINDINGS))
      (SETQ MAXL (MAX (FLATSIZE (%FIND-STRUCTURE-HEADER (CAR BINDINGS))) MAXL)))
    (DO ((BINDINGS IVARS (CDR BINDINGS))
         (SYM)
         (I 1 (1+ I)))
        ((NULL BINDINGS))
      (SETQ SYM (%FIND-STRUCTURE-HEADER (CAR BINDINGS)))
      (PUSH `((:ITEM1 INSTANCE-SLOT ,SYM)
              (:COLON ,(+ 2 MAXL))
              ,(IF (= (%P-LDB-OFFSET %%Q-DATA-TYPE OBJ I) DTP-NULL)
                   "void"
                   `(:ITEM1 INSTANCE-VALUE ,(%P-CONTENTS-OFFSET OBJ I))))
            RESULT)))
  (NREVERSE RESULT))

(DEFUN (:PROPERTY INSTANCE-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (LET* ((SLOT (THIRD (SECOND ITEM))))
    (UNLESS (CATCH-ERROR
              (PROGN (SEND OBJECT :SET (INTERN SLOT SI:PKG-KEYWORD-PACKAGE) NEW-VALUE)
                     T)
              NIL)
      (SET-IN-INSTANCE OBJECT SLOT NEW-VALUE))))
(DEFPROP INSTANCE-SLOT T ONLY-WHEN-MODIFY)

(DEFUN INSPECT-FUNCTION-FROM (FROM)
  (DO-FOREVER
    (COND ((SYMBOLP FROM)
           (UNLESS (FBOUNDP FROM)
             (RETURN FROM))
           (SETQ FROM (SYMBOL-FUNCTION FROM)))
          (T (RETURN FROM)))))

(defmacro push-interpreter-binding-env-on-display-list (frame-list
                                                        title
                                                        item-title
                                                        shadowed-binding-type
                                                        unshadowed-binding-type
                                                        result)
  `(progn
     (if ,frame-list (push `((,,title)) ,result))
     (loop for frame in ,frame-list
           for frame-number from 0
           with bindings-already-lexically-visible = '()
;
;list of bindings for which we already have a binding,
;so if you see another attempt to bind it in an earlier
;(outer) frame, then record that binding as shadowed.
;
           do
           (push `(("   Frame ~D:" ,frame-number)) ,result)
           do
           (do ((frame frame (cddr frame)))
               ((null frame)  ,result)
             (let* ((header (%find-structure-header (car frame)))
                    (slot-pointer (locf (cadr frame)))
                    (slot-dtp (%p-data-type slot-pointer))
                    (current-value (cadr frame))
                    (shadowed)
                    (binding-type ,unshadowed-binding-type))
               (if (memq header bindings-already-lexically-visible)
                   (setq shadowed t
                         binding-type ,shadowed-binding-type)
                 (push header bindings-already-lexically-visible))
;
;if the slot points through a one-q-forward, then the item is
;special and not lexically enclosed.  Showing the current value
;as if it belonged to the closure would only cause confusion.
;
               (cond ((= slot-dtp dtp-one-q-forward)
                      (push `((:item1 'special-header
                                      ,header
                                      ,(lambda (header s)
                                         (if (symbolp header)
                                             (format s "         ~S:    *special*" header)))))
                            ,result))
                     (t
                      (push `((:item1 ,binding-type
                                      ,header
                                      ,(lambda (header s)
                                         (if (symbolp header)
                                             (format s "~:[        ~;shadowed~] ~~A~ ~S"
                                                     shadowed ,item-title header))))
                              (:colon 4)
                              (:item1 interpreter-closure-value ,current-value))
                            ,result)))
               )))))

(DEFMETHOD (BASIC-INSPECT :OBJECT-CLOSURE) (OBJ &AUX RESULT)
  (SETQ RESULT `("Function is "
                 (:ITEM1 CLOSURE-FUNCTION ,(INSPECT-FUNCTION-FROM (closure-function obj)))))
  (WHEN (ENTITYP OBJ)
    (PUSH '(".  ") RESULT)
    (PUSH `(:ITEM1 TYPE ,(TYPE-OF OBJ)) RESULT)
    (PUSH '("An object of type ") RESULT))
  (SETQ RESULT (LIST '("") RESULT))
  (let ((bindings (closure-bindings obj)))
    (case (length bindings)
      (0 (push '(("(No bindings.)")) result))
      (1 (if (consp (closure-function obj))
             ;; this is an interpreter closure
             (let* ((env (car bindings))
                    (fns (si::interpreter-environment-functions env))
                    (vars (si::interpreter-environment-variables env))
                    (frames (si::interpreter-environment-frames env))
                    ; not yet used (cache (si::interpreter-environment-macrocache env))
                    )
               (push '(("Interpreter lexical environment")) result)
               (push-interpreter-binding-env-on-display-list vars
                                                             " Variables:"
                                                             "var"
                                                             'shadowed-interpreter-closure-var
                                                             'interpreter-closure-var
                                                             result)
               (push-interpreter-binding-env-on-display-list fns
                                                             " Functions:"
                                                             "fn"
                                                             'shadowed-interpreter-closure-fn
                                                             'interpreter-closure-fn
                                                             result)

               frames
               (push '((" ")) result)
               (push '((" Info on tagbodies and blocks not yet available")) result)
               (push '((" In the meantime, use DESCRIBE on an interpreter closure")) result)
               (push '((" to get complete information ")) result)
               (push '((" (without being able to modify it...)")) result))
           ;; this is a lexical closure of a compiled function
           (push '(("Lexical slots")) result)
           (let ((map ()))
           (labels ((frob (fn depth)
                      (let* ((d (debugging-info fn))
                             (m (cdr (assq 'compiler::lexical-ref-map d)))
                             (o (cdr (assq ':internal-fef-offsets d))))
                        (dolist (x m)
                          (let ((n (dpb (- (ldb (byte 12. 12.) (car x)) depth)
                                        (byte 12. 12.)
                                        (ldb (byte 12. 0) (car x)))))
                            (unless (assq n map)
                              (push (cons n (cadr x)) map))))
                        (dolist (x o)
                          (frob (%p-contents-offset fn x) (1+ depth))))))
             (frob (closure-function obj) 0))
           (loop for frame in (car bindings)
                 for f from 0
              when (eq frame 't) do
                (push `((,(format nil "  (Context ~D empty)~%" f))) result)
              else do
                (push `((,(format nil "  Context ~D~%" f))) result)
                (loop for x in frame
                      for i from 0
                      as code = (dpb f (byte 12. 12.) i)
                      as frob = (or (assq code map) (ncons code))
                   do (push `((:item1 lexical-closure-slot ,frob
                                      ,(lambda (frob s)
                                         (format s "    Slot ~D~@[ (~S)~]"
                                                 (ldb (byte 12. 0) (car frob))
                                                 (cdr frob))))
                              (:colon 4)
                              (:item1 lexical-closure-value ,x))
                            result))))))
      (T (LET ((MAXL (loop for x in bindings by 'cddr
                           as h = (%find-structure-header x)
                           maximize (if (symbolp h)
                                        (case (%pointer-difference x h)
                                          ((0 3 4) 0)                   ;name plist package
                                          (1 (flatsize h))              ;value
                                          (t (+  2 (flatsize h))))      ;function
                                      0))))
           (loop for (x y) on bindings by 'cddr
                 do (push `((:item1 closure-slot ,x
                                    ,(lambda (x s &aux (h (%find-structure-header x)))
                                       (if (symbolp h)
                                           (format s "~[Pname of ~;~;#'~;~
                                                        Plist of ~;Package of ~]~S"
                                                   (%pointer-difference x h) h)
                                         (prin1 x s))))
                            (:COLON ,(+ 4 MAXL))
                            ,(IF (location-boundp y)
                                 `(:ITEM1 CLOSURE-VALUE ,(contents y))
                                 "void"))
                          result))))))
    (NREVERSE RESULT))


;this was trying to use the binding-list as a
;disembodied plist - it was not working, so I
;re-did it in a manner similar to SET-IN-CLOSURE
;    (with-stack-list (s slot)
;   (setf (contents (cadr (getl bindings s))) new-value)))
;
(DEFUN (:PROPERTY CLOSURE-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (DO ((binding-list (closure-bindings object) (CDDR binding-list))
       (SLOT (THIRD (SECOND ITEM))))
      ((NULL binding-list))
    (IF (EQ (CAR binding-list) slot)
        (RETURN (SETF (CONTENTS (CADR binding-list)) new-value)))))

(DEFPROP CLOSURE-SLOT T ONLY-WHEN-MODIFY)

(DEFUN (:PROPERTY LEXICAL-CLOSURE-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (LET* ((CODE (CAR (THIRD (SECOND ITEM))))
         (context (ldb (byte 12. 12.) code))
         (index (ldb (byte 12. 0) code))
         (bindings (closure-bindings object)))
    (setf (elt (elt (car bindings) context) index) new-value)))
(DEFPROP LEXICAL-CLOSURE-SLOT T ONLY-WHEN-MODIFY)

;; shadowed or special variables are not given a set function
(DEFUN (:PROPERTY interpreter-closure-var SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (do* ((frame-list (si::interpreter-environment-variables
                      (car (closure-bindings object)))
                    (cdr frame-list))
        (frame (car frame-list) (car frame-list))
        (var-to-set (third (second item))))
       ((or (null frame-list)
            (do ((frame frame (cddr frame)))
                ((null frame))
              (if (eq (%find-structure-header (car frame)) var-to-set)
                  (return (setf (cadr frame) new-value)))
              )))))

(DEFPROP interpreter-closure-var T ONLY-WHEN-MODIFY)

(DEFMETHOD (BASIC-INSPECT :OBJECT-SELECT-METHOD) (SM &AUX (RESULT NIL))
  (SETQ SM (%MAKE-POINTER DTP-LIST SM))
  (DO ((S SM (CDR S))
       (MAXL -1))
      ((SYMBOLP S)
       (SETQ RESULT (SORT RESULT (LAMBDA (Y X)
                                   (ALPHALESSP (THIRD (FIRST X)) (THIRD (FIRST Y))))))
       (SETQ MAXL (MAX MAXL (STRING-LENGTH "Tail pointer")))
       (DOLIST (R RESULT)
         (SETF (SECOND (SECOND R)) MAXL))
       (PUSH `((:ITEM1 SELECT-METHOD-TAIL-POINTER "Tail pointer" PRINC)
               (:COLON ,(+ 2 MAXL))
               (:ITEM1 SELECT-METHOD-TAIL-FUNCTION ,(AND S (INSPECT-FUNCTION-FROM S))))
             RESULT)
       (NREVERSE RESULT))
    (DO ((KWDS (CAAR S) (CDR KWDS))
         (K))
        ((NULL KWDS))
      (IF (CONSP KWDS)
          (SETQ K (CAR KWDS))
          (SETQ K KWDS)
          (SETQ KWDS NIL))
      (PUSH `((:ITEM1 SELECT-METHOD-KEYWORD ,K)
              ,(LIST :COLON 0)
              (:ITEM1 SELECT-METHOD-FUNCTION ,(CDAR S)))
              RESULT)
      (SETQ MAXL (MAX MAXL (FLATSIZE K))))))

(DEFUN (:PROPERTY SELECT-METHOD-TAIL-POINTER SET-FUNCTION) (IGNORE NEW-VALUE SM)
  (SETF (CDR (LAST (%MAKE-POINTER DTP-LIST SM))) NEW-VALUE))
(DEFPROP SELECT-METHOD-TAIL-POINTER T ONLY-WHEN-MODIFY)

(DEFUN (:PROPERTY SELECT-METHOD-KEYWORD SET-FUNCTION) (ITEM NEW-VALUE SM)
  (SETQ SM (%MAKE-POINTER DTP-LIST SM)
        ITEM (THIRD (SECOND ITEM)))
  (DO ((S SM (CDR S)))
      ((SYMBOLP S) NIL)
    (WHEN (IF (SYMBOLP (CAAR S))
              (EQ (CAAR S) ITEM)
            (MEMQ ITEM (CAAR S)))
      (SETF (CDAR S) NEW-VALUE)
      (RETURN NIL))))
(DEFPROP SELECT-METHOD-KEYWORD T ONLY-WHEN-MODIFY)


(DEFMETHOD (BASIC-INSPECT :OBJECT-SYMBOL) (OBJ)
  `(((:ITEM1 SYMBOL-VALUE-CELL "Value is " PRINC)
     ,(IF (BOUNDP OBJ)
          `(:ITEM1 SYMBOL-VALUE ,(SYMBOL-VALUE OBJ))
          "void"))
    ((:ITEM1 SYMBOL-FUNCTION-CELL "Function is " PRINC)
     ,(IF (FBOUNDP OBJ)
          `(:ITEM1 SYMBOL-FUNCTION ,(SYMBOL-FUNCTION OBJ))
          "void"))
    ((:ITEM1 SYMBOL-PROPERTY-CELL "Property list: " PRINC)
     (:ITEM1 SYMBOL-PROPERTY-LIST ,(SYMBOL-PLIST OBJ)))
    ("Package: "
     (:ITEM1 SYMBOL-PACKAGE ,(SYMBOL-PACKAGE OBJ)))))

(DEFUN (:PROPERTY SYMBOL-VALUE-CELL SET-FUNCTION) (IGNORE NEW-VALUE OBJECT)
  (SETF (SYMBOL-VALUE OBJECT) NEW-VALUE))
(DEFPROP SYMBOL-VALUE-CELL T ONLY-WHEN-MODIFY)

(DEFUN (:PROPERTY SYMBOL-FUNCTION-CELL SET-FUNCTION) (IGNORE NEW-VALUE OBJECT)
  (SETF (SYMBOL-FUNCTION OBJECT) NEW-VALUE))
(DEFPROP SYMBOL-FUNCTION-CELL T ONLY-WHEN-MODIFY)

(DEFUN (:PROPERTY SYMBOL-PROPERTY-CELL SET-FUNCTION) (IGNORE NEW-VALUE OBJECT)
  (SETF (PLIST OBJECT) NEW-VALUE))
(DEFPROP SYMBOL-PROPERTY-CELL T ONLY-WHEN-MODIFY)


(DEFMETHOD (BASIC-INSPECT :OBJECT-FEF) (FEF)
  (FEF-DISPLAY-LIST FEF SELF))

(DEFMETHOD (BASIC-INSPECT :OBJECT-STACK-FRAME) (SF)
  (LET* ((RP (SG-REGULAR-PDL (STACK-FRAME-SG SF)))
         (AP (STACK-FRAME-AP SF))
         (FUNCTION (RP-FUNCTION-WORD RP AP)))
    (TYPECASE FUNCTION
      (CONS (SEND SELF :OBJECT-LIST FUNCTION))
      (COMPILED-FUNCTION
       (FEF-DISPLAY-LIST FUNCTION SELF (RP-EXIT-PC RP AP) (STACK-FRAME-FUNCTION-NAME SF))))))

;  (let ((result))
;    (loop with describe = (with-output-to-string (*standard-output*)
;                           (si::describe-fef fef t))
;         with len = (length describe)
;         as last = 1 then (1+ this) ; ignore initial newline
;         as this = (or (%string-search-char #/newline describe last len) len)
;         do (push `(string ,(substring describe last this)) result)
;         until ( this len))
(DEFUN FEF-DISPLAY-LIST (FEF WINDOW &OPTIONAL PC-NOW LABEL &AUX LIST PC-IDX)
  (DO ((I 0 (1+ I))
       (PC (FEF-INITIAL-PC FEF) (+ PC (COMPILER:DISASSEMBLE-INSTRUCTION-LENGTH FEF PC)))
       (LIM-PC (COMPILER::DISASSEMBLE-LIM-PC FEF)))
      (( PC LIM-PC)
       (WHEN (EQ PC PC-NOW)                     ;PC off the end
         (SETQ PC-IDX I)
         (PUSH T LIST)))
    (AND (EQ PC PC-NOW) (SETQ PC-IDX I))
    (PUSH PC LIST))
  (VALUES (NREVERSE LIST)
          (LIST FEF PC-IDX)
          'PRINT-FEF-INSTRUCTION
          (AND PC-NOW
               (MAX 0 (- PC-IDX (TRUNCATE (* 3 (TRUNCATE (SHEET-INSIDE-HEIGHT WINDOW)
                                                         (SHEET-LINE-HEIGHT WINDOW)))
                                          4))))
          LABEL))

(DEFUN PRINT-FEF-INSTRUCTION (PC FEF-AND-PC-IDX *STANDARD-OUTPUT* ITEM-NO
                              &AUX (FEF (FIRST FEF-AND-PC-IDX))
                                   (PC-IDX (SECOND FEF-AND-PC-IDX)))
  (SEND *STANDARD-OUTPUT* :STRING-OUT (IF (EQ ITEM-NO PC-IDX) "=> " "   "))
  (LET ((COMPILER::DISASSEMBLE-OBJECT-OUTPUT-FUN
          (LAMBDA (OBJ PREFIX LOC FUN-P)
            (IF ( (%P-DATA-TYPE LOC) DTP-SELF-REF-POINTER)
                ;; letting user get self-ref-pointers will crash machine
                (SEND *STANDARD-OUTPUT* :ITEM1 (LIST OBJ LOC)
                      (IF FUN-P 'FEF-FUNCTION 'FEF-CONSTANT)
                      'PRINT-FEF-CONSTANT PREFIX)
              (PRINC PREFIX *STANDARD-OUTPUT*)
              (PRIN1 OBJ *STANDARD-OUTPUT*)))))
    (AND (NUMBERP PC) (COMPILER::DISASSEMBLE-INSTRUCTION FEF PC))))

(DEFUN PRINT-FEF-CONSTANT (ITEM STREAM PREFIX)
  (PRINC PREFIX STREAM)
  (SEND STREAM :ITEM1 (FIRST ITEM) :VALUE #'PRINT-ITEM-CONCISELY))
(DEFPROP FEF-CONSTANT T ONLY-WHEN-MODIFY)

(DEFUN (:PROPERTY FEF-CONSTANT VALUE-FUNCTION) (THING)
  (FIRST (SECOND THING)))

(DEFUN (:PROPERTY FEF-CONSTANT SET-FUNCTION) (ITEM NEW-VALUE IGNORE)
  (RPLACD (SECOND (SECOND ITEM)) NEW-VALUE))

(DEFUN (:PROPERTY FEF-FUNCTION VALUE-FUNCTION) (THING)
  (CDR (SECOND (SECOND THING))))

;(DEFUN (:PROPERTY FEF-FUNCTION SET-FUNCTION) (ITEM NEW-VALUE IGNORE)
;  (%P-STORE-CONTENTS (SECOND (SECOND ITEM)) NEW-VALUE))

;;;; List structure hacking

(DEFFLAVOR FOLLOW-LIST-STRUCTURE-BLINKER
        ((LIST-ITEM NIL))
        (BLINKER)
  (:INITABLE-INSTANCE-VARIABLES LIST-ITEM))

(DEFMETHOD (FOLLOW-LIST-STRUCTURE-BLINKER :SET-LIST-ITEM) (NEW-LIST-ITEM)
  (AND (NEQ LIST-ITEM NEW-LIST-ITEM)
       (WITHOUT-INTERRUPTS
         (OPEN-BLINKER SELF)
         (SETQ LIST-ITEM NEW-LIST-ITEM))))

(DEFMETHOD (FOLLOW-LIST-STRUCTURE-BLINKER :BLINK) (&AUX Y LAST-LEFT-X LAST-RIGHT-X
                                                        ITEM END-ITEM
                                                        START-XPOS END-XPOS MAX-X)
  (SETQ MAX-X (SHEET-INSIDE-RIGHT SHEET))
  (MULTIPLE-VALUE-BIND (ITEM-ARRAY TOP-ITEM BOTTOM-ITEM CHARW LINEH IL IT)
      (SEND SHEET :LIST-BLINKER-INFO)
    (SETQ ITEM (THIRD LIST-ITEM)
          START-XPOS (1- (SECOND LIST-ITEM))
          END-ITEM (FIFTH LIST-ITEM)
          END-XPOS (1+ (FOURTH LIST-ITEM)))
    (SETQ Y (+ (* LINEH (- ITEM TOP-ITEM)) IT -2)
          LAST-LEFT-X (1- IL))
    (COND ((AND ( ITEM TOP-ITEM)
                (< ITEM BOTTOM-ITEM))
           ;; Top is on screen, draw the top line
           (%DRAW-LINE (SETQ LAST-LEFT-X START-XPOS) Y
                       (SETQ LAST-RIGHT-X
                             (MIN MAX-X
                                  (IF ( ITEM END-ITEM)
                                      (+ IL 1 (* CHARW
                                                 (STRING-LENGTH
                                                   (SECOND (AREF ITEM-ARRAY ITEM)))))
                                      END-XPOS)))
                       Y
                       ALU-XOR T SHEET)))
    (DO () (( ITEM BOTTOM-ITEM))
      (COND (( ITEM TOP-ITEM)
             ;; Item is on screen, so there are side bars
             (if last-left-x
                 (%DRAW-LINE LAST-LEFT-X (1+ Y)
                             LAST-LEFT-X (+ Y (1- LINEH))
                             ALU-XOR T SHEET))
             (if last-right-x
                 (%DRAW-LINE LAST-RIGHT-X (1+ Y)
                             LAST-RIGHT-X (+ Y (1- LINEH))
                             ALU-XOR T SHEET))))
      (INCF Y LINEH)
      ;; If we just handled the side-bars for the last item, return
      (AND (OR (= ITEM END-ITEM) ( ITEM (1- BOTTOM-ITEM)))
           (RETURN))
      ;; Onto the next item, and take care of the short horizontal bars on the right and left
      (COND ((> (SETQ ITEM (1+ ITEM)) TOP-ITEM)
             (if last-left-x
                 (%DRAW-LINE LAST-LEFT-X Y
                             (SETQ LAST-LEFT-X (1- IL)) Y
                             ALU-XOR T SHEET)
               (SETQ LAST-LEFT-X (1- IL)))
             (if last-right-x
                 (%DRAW-LINE LAST-RIGHT-X Y
                         (SETQ LAST-RIGHT-X
                               (MIN MAX-X
                                    (IF ( ITEM END-ITEM)
                                        (+ IL 1 (* CHARW
                                                   (STRING-LENGTH
                                                     (SECOND (AREF ITEM-ARRAY ITEM)))))
                                      END-XPOS)))
                         Y
                         ALU-XOR T SHEET)
               (SETQ LAST-RIGHT-X
                               (MIN MAX-X
                                    (IF ( ITEM END-ITEM)
                                        (+ IL 1 (* CHARW
                                                   (STRING-LENGTH
                                                     (SECOND (AREF ITEM-ARRAY ITEM)))))
                                        END-XPOS)))))
            ((= ITEM TOP-ITEM)
             (SETQ LAST-RIGHT-X
                   (MIN MAX-X
                        (IF ( ITEM END-ITEM)
                            (+ IL 1 (* CHARW (STRING-LENGTH (SECOND (AREF ITEM-ARRAY ITEM)))))
                            END-XPOS))))))
    (AND (= ITEM END-ITEM) (< ITEM BOTTOM-ITEM) last-left-x last-right-x
         ;; If didn't run off bottom of screen, draw in bottom line
         (%DRAW-LINE LAST-LEFT-X Y
                     LAST-RIGHT-X Y
                     ALU-XOR T SHEET))))

(DEFMETHOD (FOLLOW-LIST-STRUCTURE-BLINKER :SIZE) ()
  (VALUES (SHEET-INSIDE-WIDTH SHEET)
          (SHEET-INSIDE-HEIGHT SHEET)))


(DEFMETHOD (BASIC-INSPECT :LIST-BLINKER-INFO) ()
  (VALUES ITEMS
          TOP-ITEM
          (+ TOP-ITEM (SHEET-NUMBER-OF-INSIDE-LINES))
          CHAR-WIDTH
          LINE-HEIGHT
          (SHEET-INSIDE-LEFT)
          (SHEET-INSIDE-TOP)))

(DEFMETHOD (BASIC-INSPECT :AFTER :CHANGE-OF-SIZE-OR-MARGINS) (&REST IGNORE)
  (AND DISPLAYING-LIST
       ;; If displaying a list, then must regrind when size changes
       (INSPECT-SETUP-OBJECT CURRENT-OBJECT SELF TOP-ITEM)))

(DEFMETHOD (BASIC-INSPECT :MOUSE-MOVES) (X Y &AUX ITEM TYPE LEFT TOP BWIDTH BHEIGHT)
  (MOUSE-SET-BLINKER-CURSORPOS)
  (MULTIPLE-VALUE (ITEM TYPE LEFT BWIDTH TOP)
    (SEND SELF :MOUSE-SENSITIVE-ITEM X Y))
  (COND ((MEMQ TYPE '(:LIST-STRUCTURE :LIST-STRUCTURE-TOP-LEVEL))
         (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL)
         ;; LEFT, BWIDTH, TOP are invalid
         (SEND LIST-BLINKER :SET-LIST-ITEM ITEM)
         (BLINKER-SET-VISIBILITY LIST-BLINKER T))
        (TYPE
         (BLINKER-SET-VISIBILITY LIST-BLINKER NIL)
         (SETQ BWIDTH (- BWIDTH LEFT)
               BHEIGHT (FONT-BLINKER-HEIGHT CURRENT-FONT))
         (BLINKER-SET-CURSORPOS ITEM-BLINKER (- LEFT (SHEET-INSIDE-LEFT))
                                (- TOP (SHEET-INSIDE-TOP)))
         (BLINKER-SET-SIZE ITEM-BLINKER BWIDTH BHEIGHT)
         (BLINKER-SET-VISIBILITY ITEM-BLINKER T))
        (T (BLINKER-SET-VISIBILITY LIST-BLINKER NIL)
           (BLINKER-SET-VISIBILITY ITEM-BLINKER NIL))))

(DEFMETHOD (BASIC-INSPECT :MOUSE-SENSITIVE-ITEM) (X Y)
  (BLOCK FOUND-ITEM
    (PROG (LILN)
          (MULTIPLE-VALUE-BIND (ITEM TYPE LEFT BWIDTH TOP)
              (MOUSE-SENSITIVE-ITEM X Y)
            (COND (ITEM
                   (RETURN-FROM FOUND-ITEM (VALUES ITEM TYPE LEFT BWIDTH TOP)))
                  ((NOT DISPLAYING-LIST))
                  ((AND ( Y (SHEET-INSIDE-TOP))
                        (< Y (SHEET-INSIDE-BOTTOM)))
                   ;; No explicit item on this line -- find list structure if it exists
                   (LET ((LINE-NO (+ TOP-ITEM (SHEET-LINE-NO NIL Y))))
                     ;; Starting from this line, work backwards until an enclosing
                     ;; piece of structure is found
                     (OR ( LINE-NO (LENGTH ITEMS))
                         (DOLIST (LI (FIRST (AREF ITEMS LINE-NO)))
                           (AND (COND ((= LINE-NO (SETQ LILN (THIRD LI)))
                                       ;; Entry starts on this line -- within range on right?
                                       ( X (SECOND LI)))
                                      ((> LINE-NO LILN)
                                       ;; Entry starts on some previous line -- so we are ok
                                       T))
                                (COND ((= LINE-NO (SETQ LILN (FIFTH LI)))
                                       ;; Entry ends on this line, within range on left?
                                       (< X (FOURTH LI)))
                                      ((< LINE-NO LILN)
                                       ;; Entry starts before -- so this is good
                                       T))
                                (RETURN-FROM FOUND-ITEM
                                  (IF (AND (OR MODIFY-MODE (KEY-STATE :HYPER))
                                           (EQ (FIRST LI) ':TOP-LEVEL))
                                      NIL
                                    (VALUES LI
                                            (IF (EQ (FIRST LI) :TOP-LEVEL)
                                                :LIST-STRUCTURE-TOP-LEVEL
                                                :LIST-STRUCTURE))))))))))))))

(DEFMETHOD (BASIC-INSPECT :OBJECT-OTHER) (OB)
  (DECLARE (IGNORE OB))
  NIL)

(defun print-pointer (locative stream)
  (format stream "#<~S ~O>"
          (or (q-data-types (%p-data-type locative)) (%p-data-type locative))
          (%pointer locative)))

(defmethod (basic-inspect :object-locative) (obj)
  `(((:item1 locative-cell "Contents : " princ)
     ,(if (inspector-%p-contents-safe-p obj)
          `(:item1 locative-contents ,(contents obj))
        (print-pointer obj nil)))
    (" Offset " ,(format nil "~D" (%pointer-difference obj (%find-structure-header obj)))
     " into "
     (:item1 ,(data-type (%find-structure-header obj)) ,(%find-structure-header obj)))
    ("%P-Cdr-Code : " ,(symbol-name (nth (%p-cdr-code obj) sys:q-cdr-codes)))
    ("%P-Data-Type: " ,(symbol-name (q-data-types (%p-data-type obj))))
    ("Area        : " ,(format nil "~O, ~S"
                               (%area-number obj) (area-name (%area-number obj))))))
(defun (:property locative-cell set-function) (ignore new-value object)
  (setf (contents object) new-value))
(defprop locative-cell t only-when-modify)

(DEFCONST INSPECT-PRINLEVEL 8
  "This value of *PRINT-LEVEL* is used while grinding lists in INSPECT.")
(DEFCONST INSPECT-PRINLENGTH 300.
  "This value of *PRINT-LENGTH* is used while grinding lists in INSPECT.")

(DEFMETHOD (BASIC-INSPECT :OBJECT-LIST) (LIST)
  (MULTIPLE-VALUE-BIND (STRING-LIST ATOMIC-ITEMS LIST-ITEMS)
      (LET ((*PRINT-LENGTH* INSPECT-PRINLENGTH)
            (*PRINT-LEVEL* INSPECT-PRINLEVEL))
        (condition-bind (((sys:cell-contents-error)
                            (lambda (cond)
                              (values :new-value
                                      (format nil "#<~S ~O>"
                                              (or (q-data-types (send cond :data-type))
                                                  (send cond :data-type))
                                              (%pointer (send cond :address)))))))
          (GRIND-INTO-LIST LIST (TRUNCATE (SHEET-INSIDE-WIDTH) CHAR-WIDTH) T)))
    ;; Turn STRING-LIST into a list of elements, one for each line, of the form
    ;; (NIL contents-string atom-item-list line-contains-lozenged-characters-p).
    (DO ((L STRING-LIST (CDR L))
         (AIS ATOMIC-ITEMS (CDR AIS)))
        ((NULL L))
      (LET ((LOZENGED-CHARACTERS
              (DOTIMES (I (STRING-LENGTH (CAR L)))
                (IF ( (CHAR (CAR L) I) #o200)
                    (RETURN T)))))
        ;; Convert the start and end indices for each atom-item from characters to pixels.
        ;; If this line contains no lozenged characters,
        ;; this can be done by multiplying.  Otherwise, SHEET-STRING-LENGTH must be used.
        (DOLIST (I (CAR AIS))
          (SETF (THIRD I) (+ (SHEET-INSIDE-LEFT)
                             (IF LOZENGED-CHARACTERS
                                 (SHEET-STRING-LENGTH SELF (CAR L) 0 (THIRD I))
                               (* (THIRD I) CHAR-WIDTH))))
          (SETF (FOURTH I) (+ (SHEET-INSIDE-LEFT)
                              (IF LOZENGED-CHARACTERS
                                  (SHEET-STRING-LENGTH SELF (CAR L) 0 (FOURTH I))
                                (* (FOURTH I) CHAR-WIDTH)))))
        (SETF (CAR L) (LIST NIL (CAR L) (CAR AIS) LOZENGED-CHARACTERS))))
    ;; Convert the starting and ending hpos of each list-item from characters to pixels
    ;; Must find the line which the start or end appears on
    ;; and see whether that line had any lozenged characters
    ;; to decide whether a multiplication is sufficient.
    (DOLIST (I LIST-ITEMS)
      (SETF (SECOND I)
            (+ (SHEET-INSIDE-LEFT)
               (LET ((LINE-DESC (NTH (THIRD I) STRING-LIST)))
                 (IF (FOURTH LINE-DESC)
                     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (SECOND I))
                   (* (SECOND I) CHAR-WIDTH)))))
      (SETF (FOURTH I)
            (+ (SHEET-INSIDE-LEFT)
               (LET ((LINE-DESC (NTH (FIFTH I) STRING-LIST)))
                 (IF (FOURTH LINE-DESC)
                     (SHEET-STRING-LENGTH SELF (SECOND LINE-DESC) 0 (FOURTH I))
                   (* (FOURTH I) CHAR-WIDTH))))))
    (SETQ LIST-ITEMS (SORT LIST-ITEMS
                           (LAMBDA (X Y)
                             (COND ((< (THIRD Y) (THIRD X)) T)
                                   ((> (THIRD Y) (THIRD X)) NIL)
                                   (T (> (SECOND X) (SECOND Y)))))))
    (DO ((LINE (1- (LENGTH STRING-LIST)) (1- LINE))
         (CURRENT LIST-ITEMS))
        ((< LINE 0))
      (DO ()
          ((OR (NULL CURRENT)
               ( (THIRD (CAR CURRENT)) LINE)))
        (SETQ CURRENT (CDR CURRENT)))
      (SETF (CAAR (NTHCDR LINE STRING-LIST)) CURRENT))
    (VALUES STRING-LIST :LIST-STRUCTURE 'INSPECT-LIST-PRINTER)))

(DEFMETHOD (BASIC-INSPECT :BEFORE :SETUP) (SL)
  (SETQ CURRENT-DISPLAY SL
        DISPLAYING-LIST NIL)
  (BLINKER-SET-VISIBILITY LIST-BLINKER NIL))

(DEFMETHOD (BASIC-INSPECT :AFTER :SETUP) (NEW-SETUP)
  (SETQ DISPLAYING-LIST (EQ (SECOND NEW-SETUP) :LIST-STRUCTURE)))

(DEFMETHOD (BASIC-INSPECT :AFTER :HANDLE-MOUSE) (&REST IGNORE)
  (BLINKER-SET-VISIBILITY LIST-BLINKER NIL))

(DEFUN INSPECT-LIST-PRINTER (ITEM IGNORE STREAM ITEM-NO)
  (DECLARE (:SELF-FLAVOR BASIC-INSPECT))
  (SETF (AREF DISPLAYED-ITEMS (- ITEM-NO TOP-ITEM)) (THIRD ITEM))
  (SEND STREAM :STRING-OUT (SECOND ITEM)))

(DEFUN (:PROPERTY :LIST-STRUCTURE SET-FUNCTION) (ITEM NEW-VALUE IGNORE)
  (SETF (CAR (FIRST (SECOND ITEM))) NEW-VALUE))

(DEFUN (:PROPERTY :LOCATIVE SET-FUNCTION) (ITEM NEW-VALUE IGNORE)
  (SETF (CONTENTS (SECOND ITEM)) NEW-VALUE))

;;;; Array hacking

;;; Values are (DISPLAY-LIST ARG ALT-PRINT-FUN FIRST-TOP-ITEM OBJ-LABEL ITEM-GENERATOR)
(DEFMETHOD (BASIC-INSPECT :OBJECT-ARRAY) (OBJ &OPTIONAL (MENTION-LEADER T) INITIAL-ITEMS)
  (SETQ INITIAL-ITEMS (APPEND INITIAL-ITEMS '((""))))
  (VALUES NIL
          (LIST OBJ MENTION-LEADER INITIAL-ITEMS)
          'INSPECT-ARRAY-PRINTER
          0
          NIL
          'INSPECT-ARRAY-ITEM-GENERATOR))

;;; This is the item-generator function for displaying an array;
;;; Our item-list is effectively a list of consecutive integers;
;;; that is, item number n's value is just n-m, where m is the
;;; number of items in PRINT-FUNCTION-ARG that are used for the array leader.
;;; All the work of figuring out how to print item n is done by the print function.
;;; The purpose of our using an item-generator is so we don't have to cons up
;;; a very long list of consecutive integers (or anything else).
(DEFSELECT INSPECT-ARRAY-ITEM-GENERATOR
  (:NUMBER-OF-ITEMS ()
    (DECLARE (:SELF-FLAVOR BASIC-INSPECT))
    (+ (IF (CADR PRINT-FUNCTION-ARG)
           (OR (ARRAY-LEADER-LENGTH (CAR PRINT-FUNCTION-ARG)) 0)
         0)
       (LENGTH (CADDR PRINT-FUNCTION-ARG))
       (ARRAY-LENGTH (CAR PRINT-FUNCTION-ARG))))
  (:NUMBER-OF-ITEM (ITEM)
    (DECLARE (:SELF-FLAVOR BASIC-INSPECT))
    (IF (NUMBERP ITEM) (+ ITEM (LENGTH (CADDR PRINT-FUNCTION-ARG)))
      (FIND-POSITION-IN-LIST ITEM (CADDR PRINT-FUNCTION-ARG))))
  (:ITEM-OF-NUMBER (NUMBER)
    (DECLARE (:SELF-FLAVOR BASIC-INSPECT))
    (IF (< NUMBER (LENGTH (CADDR PRINT-FUNCTION-ARG)))
        (NTH NUMBER (CADDR PRINT-FUNCTION-ARG))
      (- NUMBER (LENGTH (CADDR PRINT-FUNCTION-ARG))))))

(DEFUN INSPECT-ARRAY-PRINTER (ITEM ARG STREAM ITEM-NUMBER
                              &AUX (OBJ (CAR ARG))
                              (LEADER-LENGTH-TO-MENTION
                                (OR (AND (CADR ARG) (ARRAY-LEADER-LENGTH OBJ)) 0)))
  "The print-function used when inspecting an array."
  ;; (CAR ARG) is the array.  (CADR ARG) is T to display the leader.
  ;; ITEM is usually a number.  A small number is an index in the leader.
  ;; Numbers too big for that start moving through the array elements.
  (COND ((NOT (NUMBERP ITEM))
         (INSPECT-PRINTER ITEM OBJ STREAM ITEM-NUMBER))
        ((< ITEM LEADER-LENGTH-TO-MENTION)
         (SEND STREAM :ITEM1 ITEM 'LEADER-SLOT (LAMBDA (ITEM STREAM)
                                                 (FORMAT STREAM "Leader ~D" ITEM)))
         (FORMAT STREAM ":~12T ")
         (if (inspector-%p-contents-safe-p (locf (array-leader obj item)))
             (send stream :item1 (array-leader obj item) :value #'print-item-concisely)
           (print-pointer (locf (array-leader obj item)) stream)))
        (T
         (LET ((ITEM (- ITEM LEADER-LENGTH-TO-MENTION))
               (RANK (ARRAY-RANK OBJ))
               INDICES)
           (OR (= RANK 1) (SETQ INDICES (ARRAY-INDICES-FROM-INDEX OBJ ITEM)))
           (SEND STREAM :ITEM1 (CONS ITEM (IF (= RANK 1) ITEM INDICES)) 'ARRAY-SLOT
                               (LAMBDA (DATUM STREAM)
                                 (FORMAT STREAM "Elt ~D" (CDR DATUM))))
           (FORMAT STREAM ":~9T ")
           (IF (OR (CDR (ASSQ (ARRAY-TYPE OBJ) ARRAY-BITS-PER-ELEMENT))
                   (inspector-%P-CONTENTS-SAFE-P (LOCF (CLI:AR-1-FORCE OBJ ITEM))))
               ;; Deal with data types that are objects, and with numeric arrays.
               (SEND STREAM :ITEM1 (cli:ar-1-force obj item)    ;yes, I really mean cli:
                            :VALUE #'PRINT-ITEM-CONCISELY)
             ;; Deal with data types that aren't really objects.
             (print-pointer (locf (cli:ar-1-force obj item)) stream))))))

(DEFUN (:PROPERTY LEADER-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (SETF (ARRAY-LEADER OBJECT (SECOND ITEM)) NEW-VALUE))
(DEFPROP LEADER-SLOT T ONLY-WHEN-MODIFY)

(defun array-indices-from-index (array index)
  "Given a single INDEX into ARRAY, compute the equivalent set of indices.
The value is a list whose elements could be used with AREF and ARRAY,
and be equivalent to (AR-1-FORCE ARRAY INDEX)."
  (let ((indicies ())
        (index1 index))
    (do ((i (1- (array-rank array)) (1- i)))
        ((< i 0) indicies)
      ;; row-major-order!
      (push (\ index1 (array-dimension array i)) indicies)
      (setq index1 (truncate index1 (array-dimension array i))))))

(DEFUN (:PROPERTY ARRAY-SLOT SET-FUNCTION) (ITEM NEW-VALUE OBJECT)
  (SETF (AR-1-FORCE OBJECT (CAR (SECOND ITEM))) NEW-VALUE))
(DEFPROP ARRAY-SLOT T ONLY-WHEN-MODIFY)

;;;; Other windows needed for the inspector
(DEFFLAVOR INSPECT-HISTORY-WINDOW ((CACHE NIL))
           (LINE-AREA-TEXT-SCROLL-MIXIN
            FUNCTION-TEXT-SCROLL-WINDOW
            BASIC-SCROLL-BAR                    ;outside borders for thermometer effect
            MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
            FLASHY-SCROLLING-MIXIN BORDERS-MIXIN
            MARGIN-REGION-MIXIN
            ANY-TYI-MIXIN WINDOW)
  :SETTABLE-INSTANCE-VARIABLES
  (:DEFAULT-INIT-PLIST :LABEL NIL
                       :FLASHY-SCROLLING-REGION '((16. 0.40s0 0.60s0)
                                                  (16. 0.40s0 0.60s0))
                       :LINE-AREA-WIDTH 18.
                       :SCROLL-BAR-ALWAYS-DISPLAYED T)
  (:DOCUMENTATION "History window for the inspector, but no margin scroll region"))

(DEFFLAVOR INSPECT-HISTORY-WINDOW-WITH-MARGIN-SCROLLING
        ()
        (MARGIN-SCROLL-MIXIN INSPECT-HISTORY-WINDOW)
  (:DEFAULT-INIT-PLIST :MARGIN-SCROLL-REGIONS '((:TOP "Top of History")
                                                (:BOTTOM "Bottom of History")))
  (:DOCUMENTATION :COMBINATION "History window for the inspector."))

(DEFMETHOD (INSPECT-HISTORY-WINDOW :LINE-AREA-MOUSE-DOCUMENTATION) ()
  "L: Inspect the indicated object.  M: Remove it from the history.")

(DEFMETHOD (INSPECT-HISTORY-WINDOW :WHO-LINE-DOCUMENTATION-STRING) ()
  "L: Inspect the indicated object.")

(DEFMETHOD (INSPECT-HISTORY-WINDOW :INSPECT-OBJECT) (OBJECT INSPECTOR
                                                     &OPTIONAL TOP-ITEM-NO -LABEL-
                                                               DONT-PROPOGATE)
  ;; First, remember current TOP-ITEM of inspector
  (LET ((DISP (SEND INSPECTOR :CURRENT-DISPLAY)))
    (AND DISP
         (SETF (FOURTH DISP) (SEND INSPECTOR :TOP-ITEM)))
    (OR (DOTIMES (I (ARRAY-ACTIVE-LENGTH ITEMS))
          (COND ((NEQ OBJECT (AREF ITEMS I)))
                (DONT-PROPOGATE (RETURN T))
                (T (SEND SELF :DELETE-ITEM I)
                   (RETURN NIL))))
        (SEND SELF :APPEND-ITEM OBJECT))
    (SEND SELF :PUT-ITEM-IN-WINDOW OBJECT)
    (LET ((CE (ASSQ OBJECT CACHE)))
      (OR CE
          (PUSH (SETQ CE (INSPECT-SETUP-OBJECT-DISPLAY-LIST
                           OBJECT INSPECTOR TOP-ITEM-NO -LABEL-))
                CACHE))
      (OR (EQ (CDR CE) DISP)
          (SEND INSPECTOR :SETUP-OBJECT CE)))))

(DEFMETHOD (INSPECT-HISTORY-WINDOW :FLUSH-OBJECT) (OBJ)
  (SEND SELF :FLUSH-OBJECT-FROM-CACHE OBJ)
  (DOTIMES (I (ARRAY-ACTIVE-LENGTH ITEMS))
    (AND (EQ OBJ (AREF ITEMS I))
         (RETURN (SEND SELF :DELETE-ITEM I)))))

(DEFMETHOD (INSPECT-HISTORY-WINDOW :AFTER :INIT) (IGNORE)
  (SETQ PRINT-FUNCTION (LAMBDA (LINE IGNORE STREAM IGNORE)
                         (SEND STREAM :ITEM1 LINE :VALUE #'PRINT-ITEM-CONCISELY))
        PRINT-FUNCTION-ARG NIL))

(DEFMETHOD (INSPECT-HISTORY-WINDOW :FLUSH-OBJECT-FROM-CACHE) (OBJECT)
  (SETQ CACHE (DELQ (ASSQ OBJECT CACHE) CACHE)))

(DEFMETHOD (INSPECT-HISTORY-WINDOW :FLUSH-CONTENTS) ()
  (SETQ CACHE NIL
        TOP-ITEM 0)
  (SETF (FILL-POINTER ITEMS) 0)
  (FILL DISPLAYED-ITEMS NIL)
  (SEND SELF :NEW-SCROLL-POSITION)
  (SHEET-FORCE-ACCESS (SELF :NO-PREPARE)
    (SEND SELF :CLEAR-WINDOW)))

(DEFFLAVOR INSPECT-HISTORY-PANE () (INSPECT-HISTORY-WINDOW)
  :ALIAS-FLAVOR)

(DEFFLAVOR INSPECT-HISTORY-PANE-WITH-MARGIN-SCROLLING
        ()
        (INSPECT-HISTORY-WINDOW-WITH-MARGIN-SCROLLING)
  :ALIAS-FLAVOR)

(DEFFLAVOR INSPECT-PANE () (INSPECT-WINDOW)
  :ALIAS-FLAVOR)

(DEFFLAVOR INSPECT-WINDOW-WITH-TYPEOUT () (TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN INSPECT-WINDOW)
  (:DEFAULT-INIT-PLIST :TYPEOUT-WINDOW '(TYPEOUT-WINDOW
                                         :DEEXPOSED-TYPEOUT-ACTION (:EXPOSE-FOR-TYPEOUT))))

(DEFWRAPPER (INSPECT-WINDOW-WITH-TYPEOUT :MOUSE-SENSITIVE-ITEM) (IGNORE . BODY)
  `(COND ((NOT (SHEET-EXPOSED-P TYPEOUT-WINDOW))
          . ,BODY)))

(DEFFLAVOR INSPECT-PANE-WITH-TYPEOUT () (INSPECT-WINDOW-WITH-TYPEOUT)
  :ALIAS-FLAVOR)


(DEFFLAVOR INTERACTION-PANE () (PREEMPTABLE-READ-ANY-TYI-MIXIN NOTIFICATION-MIXIN
                                AUTOEXPOSING-MORE-MIXIN WINDOW))

(DEFFLAVOR INSPECT-FRAME (INSPECTORS TYPEOUT-WINDOW (MENU NIL))
           (PROCESS-MIXIN
            FULL-SCREEN-HACK-MIXIN
            FRAME-DONT-SELECT-INFERIORS-WITH-MOUSE-MIXIN
            BASIC-CONSTRAINT-FRAME CONSTRAINT-FRAME-FORWARDING-MIXIN
            BORDERS-MIXIN LABEL-MIXIN BASIC-FRAME)
  (:DEFAULT-INIT-PLIST :SAVE-BITS :DELAYED :PROCESS '(INSPECT-TOP-LEVEL))
  :GETTABLE-INSTANCE-VARIABLES
  (:INITABLE-INSTANCE-VARIABLES MENU)
  (:INIT-KEYWORDS :NUMBER-OF-INSPECTORS))

(DEFCONST INSPECT-FRAME-ITEM-LIST
     '(("Exit" :VALUE :EXIT
        :DOCUMENTATION "Exit the inspector, returning last thing inspected.")
       ("Return" :VALUE :RETURN
        :DOCUMENTATION "Exit the inspector, returning a value.")
       ("Clear" :VALUE :CLEAR
        :DOCUMENTATION "Remove all objects from the history.")
       ("DeCache" :VALUE :FLUSH-CACHE
        :DOCUMENTATION
        "Delete saved display info.  Useful if you are looking at objects that have changed.")
       ("Modify" :VALUE :MODIFY
        :DOCUMENTATION "Modify a slot by pointing at it then choosing a new value.")
       ("Set \" :VALUE :SET-\
        :DOCUMENTATION "Set the value of the symbol \ by choosing an object."))
  "Menu item-alist for the menu in the INSPECT frame.")

(DEFMETHOD (INSPECT-FRAME :BEFORE :INIT) (PLIST &AUX IO-BUFFER)
  (LET ((NOI (OR (GET PLIST :NUMBER-OF-INSPECTORS) 3))
        (NAMES NIL))
    (SETQ IO-BUFFER (MAKE-DEFAULT-IO-BUFFER))
    (SETQ PANES (LIST `(INTERACTOR INTERACTION-PANE :LABEL NIL
                                                    :IO-BUFFER ,IO-BUFFER
                                                    :MORE-P NIL)
                      `(HISTORY INSPECT-HISTORY-PANE-WITH-MARGIN-SCROLLING
                                :IO-BUFFER ,IO-BUFFER
                                :SCROLL-BAR 3)
                      `(MENU COMMAND-MENU-PANE
                             :FONT-MAP ,(LIST FONTS:CPTFONT)
                             :ITEM-LIST ,INSPECT-FRAME-ITEM-LIST
                             :IO-BUFFER ,IO-BUFFER)))
    (DOTIMES (I NOI)
      (LET ((NAME1 (INTERN (FORMAT NIL "INSPECTOR-~D" I) "TV")))
        (PUSH `(,NAME1 ,(IF (= I (1- NOI))
                            'INSPECT-PANE-WITH-TYPEOUT
                          'INSPECT-PANE)
                :SCROLL-BAR 3
                :IO-BUFFER ,IO-BUFFER) PANES)
        (PUSH NAME1 NAMES)))
    (SETQ INSPECTORS NAMES)
    (SETQ CONSTRAINTS `((MAIN . ((INTERACTOR-AND-HISTORY MENU . ,(REVERSE NAMES))
                                 ((INTERACTOR-AND-HISTORY
                                    :HORIZONTAL (:LIMIT (3 NIL :LINES HISTORY)
                                                        0.20s0 :LINES HISTORY)
                                    (INTERACTOR HISTORY)
                                    ((HISTORY 0.55s0))
                                    ((INTERACTOR :EVEN))))
                                 ((MENU :ASK :PANE-SIZE))
                                 (,@(MAPCAR (LAMBDA (NAME1)
                                              `(,NAME1 :LIMIT (1 30. :LINES)
                                                ,(// 0.30s0 (1- NOI))
                                                :LINES))
                                            (CDR NAMES)))
                                  ((,(CAR NAMES) :EVEN))))))))

(DEFMETHOD (INSPECT-FRAME :AFTER :INIT) (IGNORE &AUX INT)
  (SEND SELF :SELECT-PANE (SETQ INT (SEND SELF :GET-PANE 'INTERACTOR)))
  (DO ((IS INSPECTORS (CDR IS)))
      ((NULL IS))
    (RPLACA IS (SEND SELF :GET-PANE (CAR IS))))
  (SETQ TYPEOUT-WINDOW (SEND (CAR INSPECTORS) :TYPEOUT-WINDOW))
  (SEND TYPEOUT-WINDOW :SET-IO-BUFFER (SEND INT :IO-BUFFER)))

(DEFMETHOD (INSPECT-FRAME :NAME-FOR-SELECTION) ()
  NAME)

;;; When we have allocated a background inspector from the resource,
;;; tell it about an initial object to inspect,
;;; This message is only sent to inspectors from the resource, by INSPECT.
(DEFMETHOD (INSPECT-FRAME :PREPARE-FOR-USE) (OBJECT NEW-LABEL)
  (SEND SELF :SET-LABEL NEW-LABEL)
  (LET ((HW (SEND SELF :GET-PANE 'HISTORY)))
    (WHEN OBJECT
      (WITH-SHEET-DEEXPOSED (SELF)
        (SEND HW :FLUSH-CONTENTS)
        (SEND HW :APPEND-ITEM OBJECT)
        (DOLIST (IW (SEND SELF :INSPECTORS))
          (SEND IW :SET-CURRENT-DISPLAY
                (SEND IW :SETUP
                      `(INSPECT-PRINTER NIL NIL NIL
                                        (NIL NIL NIL NIL
                                             ,(LABEL-FONT (SEND IW :LABEL))
                                             "Empty"))))
          (SEND IW :SET-CURRENT-OBJECT (NCONS NIL)))))
    (SEND (SEND SELF :TYPEOUT-WINDOW) :MAKE-COMPLETE)
    (SEND HW :CLEAR-INPUT)))

(COMPILE-FLAVOR-METHODS INSPECT-FRAME INTERACTION-PANE
                        INSPECT-HISTORY-WINDOW INSPECT-HISTORY-WINDOW-WITH-MARGIN-SCROLLING
                        INSPECT-WINDOW
                        INSPECT-WINDOW-WITH-TYPEOUT
                        FOLLOW-LIST-STRUCTURE-BLINKER)

(DEFUN INSPECT (&OPTIONAL OBJECT)
  "Call the inspector to inspect OBJECT.  Selects an inspect window.
The inspector runs in this process, so our special variable bindings are visible.
If you type End in the inspector, the last value inspected
will be returned from the function INSPECT."
  (LET ((IFRAME (ALLOCATE-RESOURCE 'INSPECT-FRAME-RESOURCE DEFAULT-SCREEN)))
    (SEND IFRAME :PREPARE-FOR-USE OBJECT
                 (FORMAT NIL "Inspector for ~A" (SEND CURRENT-PROCESS :NAME)))
    (WINDOW-CALL (IFRAME :DEACTIVATE)
      (INSPECT-COMMAND-LOOP IFRAME))))

;; Copied from LAD: RELEASE-3.WINDOW; INSPCT.LISP#182 on 2-Oct-86 04:20:03
;;; Resource for process-less inspectors.
(DEFWINDOW-RESOURCE INSPECT-FRAME-RESOURCE ()
  :MAKE-WINDOW (INSPECT-FRAME :PROCESS NIL :LABEL "foo")
  :REUSABLE-WHEN :DEACTIVATED
  :initial-copies 0
  :initializer (when (or ( (send object :width) (send tv:main-screen :inside-width))
                         ( (send object :height) (send tv:main-screen :inside-height)))
                 (send object :change-of-size-or-margins
                       :width (send tv:main-screen :inside-width)
                       :height (send tv:main-screen :inside-height))))

;;; This is the top-level function for inspectors with processes
;;; (the kind you get if you just say (MAKE-WINDOW 'INSPECT-FRAME)).
(DEFUN INSPECT-TOP-LEVEL (FRAME)
  (DO-FOREVER
    (ERROR-RESTART ((SYS:ABORT EH:DEBUGGER-CONDITION) "Return to inspector command level.")
      (INSPECT-COMMAND-LOOP FRAME))
    (SEND FRAME :BURY)))

;;; The inspector top-level
(DEFUN INSPECT-COMMAND-LOOP (FRAME &AUX USER IS HISTORY)
  (SEND (SETQ USER (SEND FRAME :GET-PANE 'INTERACTOR)) :CLEAR-WINDOW)
  (SEND (CAR (SETQ IS (SEND FRAME :INSPECTORS))) :FLUSH-TYPEOUT)
  (SEND USER :SET-OLD-TYPEAHEAD NIL)
  (SETQ HISTORY (SEND FRAME :GET-PANE 'HISTORY))
  ;; Flush remnants of modify mode
  (SEND HISTORY :SET-SENSITIVE-ITEM-TYPES T)
  (DOLIST (I IS)
    (SEND I :SET-MODIFY-MODE NIL))
  (LET* ((TYPEOUT-WINDOW (SEND FRAME :TYPEOUT-WINDOW))
         (*TERMINAL-IO* TYPEOUT-WINDOW)
         * ** *** + ++ +++ \
         (*PRINT-ARRAY* NIL)
         (*STANDARD-INPUT* SI:SYN-TERMINAL-IO)
         (*STANDARD-OUTPUT* SI:SYN-TERMINAL-IO)
         (TV:KBD-INTERCEPTED-CHARACTERS
; character lossage
           (REMOVE (ASSQ #/BREAK TV:KBD-INTERCEPTED-CHARACTERS)
                   TV:KBD-INTERCEPTED-CHARACTERS))
         (THING) (TOP-ITEM))
    (DECLARE (SPECIAL \))
    (BLOCK INSPECTOR
      (DO-FOREVER
        (LET ((ITEMS (SEND HISTORY :ITEMS))
              (IW)
              (IDX))
          (SETQ IDX (ARRAY-ACTIVE-LENGTH ITEMS))
          ;; Make sure the inspection windows reflect the state of the history buffer
          (DOLIST (I IS)
            ;; Update datastructure to reflect current TOP-ITEMs
            (LET ((DISP (SEND I :CURRENT-DISPLAY)))
              (AND DISP (SETF (FOURTH DISP) (SEND I :TOP-ITEM)))))
          (DOTIMES (I (LENGTH IS))
            (SETQ IDX (1- IDX))
            (SETQ IW (NTH I IS))
            (COND ((< IDX 0)
                   (SEND IW :SET-CURRENT-DISPLAY
                         (SEND IW :SETUP
                               `(INSPECT-PRINTER NIL NIL NIL
                                                 (NIL NIL NIL NIL
                                                            ,(LABEL-FONT (SEND IW :LABEL))
                                                            "Empty"))))
                   (SEND IW :SET-CURRENT-OBJECT (NCONS NIL)))
                  (T (SEND HISTORY :INSPECT-OBJECT (AREF ITEMS IDX) IW TOP-ITEM NIL T)
                     (SETQ TOP-ITEM NIL)))))

        ;; Insure last item in history is on the screen
        (SEND HISTORY :PUT-LAST-ITEM-IN-WINDOW)

        ;; Give *, ** and *** the right values.
        (SETQ *PRINT-ARRAY* NIL)
        (LET* ((ITEMS (SEND HISTORY :ITEMS))
               (NITEMS (IF ITEMS (ARRAY-ACTIVE-LENGTH ITEMS) 0)))
          (AND ( NITEMS 1) (SETQ * (AREF ITEMS (- NITEMS 1))))
          (AND ( NITEMS 2) (SETQ ** (AREF ITEMS (- NITEMS 2))))
          (AND ( NITEMS 3) (SETQ *** (AREF ITEMS (- NITEMS 3)))))

        ;; Get input.
        ;; Keyboard commands are processed inside this loop.
        ;; Mouse commands exit the loop and go round the outer loop.
        (DO-FOREVER
          (SETQ THING -1)
          (SEND (CAR IS) :FLUSH-TYPEOUT)
          (SEND FRAME :SELECT-PANE USER)
          (SEND USER :FRESH-LINE)
          (OR (SEND USER :OLD-TYPEAHEAD)
              (SETQ THING (SEND USER :ANY-TYI)))
          (TYPECASE THING
; character lossage
            (CHARACTER (SETQ THING (CHAR-INT THING)))
            (FIXNUM)
            ;; Some sort of mouse command, just process
            (T (RETURN)))
          (CASE THING
            ((#/C-Z #/ABORT)
             (SIGNAL EH:*ABORT-OBJECT*))
            (#/C-V
             (SEND (CAR IS) :SCROLL-TO
                   (- (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)) 2)
                   :RELATIVE))
            (#/M-V
             (SEND (CAR IS) :SCROLL-TO
                   (- 2 (TV:SHEET-NUMBER-OF-INSIDE-LINES (CAR IS)))
                   :RELATIVE))
            (#/BREAK
             (SEND FRAME :SELECT-PANE (CAR IS))
             (SEND *TERMINAL-IO* :EXPOSE-FOR-TYPEOUT)
             (CATCH-ERROR-RESTART ((SYS:ABORT EH:DEBUGGER-CONDITION)
                                   "Return to inspector command loop.")
               (BREAK 'INSPECT))
             (SEND *TERMINAL-IO* :MAKE-COMPLETE))
            ;; Clear-Screen decaches.
            (#/CLEAR-SCREEN
             (SEND HISTORY :SET-CACHE NIL)
             (SEND FRAME :CLEAR-WINDOW)
             (SEND FRAME :REFRESH :COMPLETE-REDISPLAY))
            ;; End returns *.
            (#/END
             (RETURN-FROM INSPECTOR *))
            (#/HELP
             (INSPECT-HELP)
             (FORMAT *TERMINAL-IO* "~%Type any character to continue:")
             (LET ((CH (SEND USER :ANY-TYI)))
               (OR (= CH #/SP)
                   (SEND USER :UNTYI CH))))
            (#/DELETE
             (RETURN (SEND HISTORY :FLUSH-CONTENTS)))
            ;;set \
            (#/C-\
             (FORMAT USER "~&Value to set ~S to " '\)
             (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
                 (INSPECT-GET-VALUE-FROM-USER USER)
               (OR PUNT-P (SETQ \ VALUE))))
            (#/RUBOUT)
            (#/QUOTE
             (LET ((*TERMINAL-IO* USER)
                   FLAG)
               (FORMAT USER "Eval: ")
               (MULTIPLE-VALUE-SETQ (THING FLAG)
                 (SEND USER :RUBOUT-HANDLER
                          '((:FULL-RUBOUT :FULL-RUBOUT) (:ACTIVATION = #/END))
                          'SI:READ-FOR-TOP-LEVEL))
               (UNLESS (EQ FLAG :FULL-RUBOUT)
                 (SETQ +++ ++ ++ + + THING)
                 (MULTIPLE-VALUE-SETQ (THING FLAG)
                   (CATCH-ERROR (SI:EVAL-SPECIAL-OK THING)))
                 (OR FLAG
                     (LET ((*PRINT-LEVEL* 3) (*PRINT-LENGTH* 5))
                       (PRINT THING USER))))))
            (OTHERWISE
             (LET ((*TERMINAL-IO* USER)
                   FLAG)
               (AND ( THING 0) (SEND USER :UNTYI THING))
               (MULTIPLE-VALUE-SETQ (THING FLAG)
                 (SEND USER :PREEMPTABLE-READ
                       '((:FULL-RUBOUT :FULL-RUBOUT) (:ACTIVATION = #/END))
                       'SI:READ-FOR-TOP-LEVEL))
               (COND ((EQ FLAG ':MOUSE-CHAR) (RETURN))
                     ((NEQ FLAG :FULL-RUBOUT)
                      (SETQ +++ ++ ++ + + THING)
                      (MULTIPLE-VALUE-SETQ (THING FLAG)
                        (CATCH-ERROR (SI:EVAL-SPECIAL-OK THING)))
                      (OR FLAG
                          (RETURN (SETQ THING `(:VALUE ,THING ,HISTORY))))))))))
        (CATCH-ERROR-RESTART (SYS:ABORT "Return to inspector command loop.")
          (COND
            ((ATOM THING))
            ((EQ (CAR THING) :MOUSE-BUTTON))    ;random rodentry
            ((EQ (CAR THING) :MENU)
             (SETF (SECOND THING) (SEND (FOURTH THING) :EXECUTE (SECOND THING)))
             (CASE (SECOND THING)
               (:EXIT (RETURN *))
               (:RETURN
                (FORMAT USER "~&Value to return ")
                (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
                    (INSPECT-GET-VALUE-FROM-USER USER)
                  (OR PUNT-P (RETURN VALUE))))
               (:FLUSH-CACHE
                (SEND HISTORY :SET-CACHE NIL))
               (:MODIFY
                (SETQ TOP-ITEM (INSPECT-MODIFY-OBJECT USER HISTORY IS)))
               (:CLEAR
                (SEND HISTORY :FLUSH-CONTENTS))
               (:SET-\
                (FORMAT USER "~&Value to which to set /"~S/": " '\)
                (MULTIPLE-VALUE-BIND (VALUE PUNT-P)
                    (INSPECT-GET-VALUE-FROM-USER USER)
                  (OR PUNT-P (SETQ \ VALUE))))
               (OTHERWISE (FORMAT USER "~&Unimplemented menu command ~A~%" (SECOND THING)))))
            (T
             (COND ((NULL (FIRST THING))
                    ;; Type is NIL -- nothing under mouse
                    (BEEP))
;character lossage
                   ((LIST-MATCH-P THING `(:LINE-AREA ,IGNORE ,IGNORE #/MOUSE-2-1))
                    ;; Delete from line area
                    (SEND HISTORY :FLUSH-OBJECT (INSPECT-REAL-VALUE THING)))
;character lossage
                   ((AND (EQ (FOURTH THING) #/MOUSE-2-1)
                         (MEMQ (THIRD THING) IS))
                    ;; Middle click means leave source in one of the windows
                    (LET ((1ST-THING (INSPECT-REAL-VALUE THING))
                          (2ND-THING (SEND (THIRD THING) :CURRENT-OBJECT)))
                      ;; First flush item we will be inspecting
                      (INSPECT-FLUSH-FROM-HISTORY 1ST-THING HISTORY)
                      (INSPECT-FLUSH-FROM-HISTORY 2ND-THING HISTORY)
                      (SEND HISTORY :APPEND-ITEM 2ND-THING)
                      (SEND HISTORY :APPEND-ITEM 1ST-THING)))
;character lossage
                   ((EQ (FOURTH THING) #/MOUSE-3-1)
                    ;; Click on right button -- try to find function
                    (SETQ THING (INSPECT-FIND-FUNCTION (INSPECT-REAL-VALUE THING)))
                    (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
                    (SEND HISTORY :APPEND-ITEM THING))
                   ((CHAR-BIT (FOURTH THING) :HYPER)
                    ;; HYPER means modify the slot we are pointing at.
                    (LET ((*TERMINAL-IO* (THIRD THING)))
                      (IF (OR (NULL (FIRST THING)) (NULL (GET (FIRST THING) 'SET-FUNCTION)))
                          (FORMAT *TERMINAL-IO* "~&Cannot set this component.")
                        (INSPECT-SET-SLOT THING HISTORY USER))))
                   (T
                    ;; Otherwise inspect the thing we are pointing at.
                    (SETQ THING (INSPECT-REAL-VALUE THING))
                    (INSPECT-FLUSH-FROM-HISTORY THING HISTORY)
                    (SEND HISTORY :APPEND-ITEM THING))))))))))

(DEFUN INSPECT-SET-SLOT (SLOT HISTORY *TERMINAL-IO*)
  "Set the contents of SLOT to a value we obtain with the mouse or by reading.
SLOT is a blip produced by clicking on a mouse-sensitive item.
HISTORY should be the INSPECT-HISTORY-PANE;
we tell it to forget cached data on the slot."
  (LET ((SET-FUNCTION (GET (FIRST SLOT) 'SET-FUNCTION)))
    (FORMAT *TERMINAL-IO* "~&New value ")
    (MULTIPLE-VALUE-BIND (NEW-VALUE PUNT-P)
        (INSPECT-GET-VALUE-FROM-USER *TERMINAL-IO*)
      (OR PUNT-P
          (FUNCALL SET-FUNCTION SLOT NEW-VALUE
                   (SEND (THIRD SLOT) :CURRENT-OBJECT))))
    ;; We must recompute object we modified
    (SEND HISTORY :FLUSH-OBJECT-FROM-CACHE
             (SEND (THIRD SLOT) :CURRENT-OBJECT))
    (PROG1 (SEND (THIRD SLOT) :TOP-ITEM)
           (SEND (THIRD SLOT) :SET-CURRENT-OBJECT (NCONS NIL)))))

(DEFUN INSPECT-MODIFY-OBJECT (*TERMINAL-IO* HISTORY &OPTIONAL (INSPECTORS NIL) &AUX THING OSIT)
  "Handle the menu's MODIFY command.
Lets user pick a slot with the mouse, then does INSPECT-SET-SLOT."
  (SETQ OSIT (SEND HISTORY :SENSITIVE-ITEM-TYPES))
  (UNWIND-PROTECT
    (PROGN
      (SEND HISTORY :SET-SENSITIVE-ITEM-TYPES NIL)
      (DOLIST (I INSPECTORS)
        (SEND I :SET-MODIFY-MODE T))
      (FORMAT *TERMINAL-IO* "~&Pick a slot, with the mouse, to modify")
      (SETQ THING (SEND *TERMINAL-IO* :LIST-TYI)))
    (SEND HISTORY :SET-SENSITIVE-ITEM-TYPES OSIT)
    (DOLIST (I INSPECTORS)
      (SEND I :SET-MODIFY-MODE NIL)))
  (LET ((SET-FUNCTION (GET (FIRST THING) 'SET-FUNCTION)))
;character lossage
    (IF (OR (NULL (FIRST THING)) (NULL SET-FUNCTION) (EQ (FOURTH THING) #/MOUSE-3-1))
        (FORMAT *TERMINAL-IO* "~&Aborted.~%")
      (INSPECT-SET-SLOT THING HISTORY *TERMINAL-IO*))))

(DEFUN INSPECT-FLUSH-FROM-HISTORY (THING HISTORY)
  "Remove object THING from the history.  HISTORY should be the INSPECT-HISTORY-PANE."
  (LET ((ITEMS (SEND HISTORY :ITEMS)))
    (DOTIMES (I (ARRAY-ACTIVE-LENGTH ITEMS))
      (AND (EQ THING (AREF ITEMS I))
           (RETURN (SEND HISTORY :DELETE-ITEM I))))))

(DEFUN INSPECT-REAL-VALUE (SLOT &AUX FUN)
  "Return the current contents of SLOT, a blip made by clicking on a mouse-sensitive item."
  (IF (SETQ FUN (GET (FIRST SLOT) 'VALUE-FUNCTION))
      (SEND FUN SLOT)
    (CASE (FIRST SLOT)
      ((:VALUE :LINE-AREA 1D-ARRAY-SLOT LEADER-SLOT :FUNCTION) (SECOND SLOT))
      (:LOCATIVE (CONTENTS (SECOND SLOT)))
      (:LIST-STRUCTURE-TOP-LEVEL (SEND (THIRD SLOT) :CURRENT-OBJECT))
      (:LIST-STRUCTURE (CDR (FIRST (SECOND SLOT))))
      (OTHERWISE (THIRD (SECOND SLOT))))))

(DEFUN INSPECT-GET-VALUE-FROM-USER (*TERMINAL-IO*)
  "Get a value either by the mouse pointing at it or by read and eval on *TERMINAL-IO*."
  (FORMAT *TERMINAL-IO* "(type a form to be evalled~%or select something with mouse):~&")
  (BLOCK FRED
    (LET ((THING (SEND *TERMINAL-IO* :ANY-TYI)) ERROR)
      (IF (CONSP THING)
          ;; Choose somthing with the mouse -- display it truncated and proceed
          (COND ((EQ (FIRST THING) :MENU)
                 (FORMAT *TERMINAL-IO* "~&Cannot set value from the menu~%")
                 (RETURN-FROM FRED (VALUES NIL T)))
                (T
                 (LET ((*PRINT-LEVEL* 3) (*PRINT-LENGTH* 5))
                   (PRIN1 (SETQ THING (INSPECT-REAL-VALUE THING)) *TERMINAL-IO*))))
        (SEND *TERMINAL-IO* :UNTYI THING)
        (MULTIPLE-VALUE-SETQ (THING ERROR)
          (CATCH-ERROR (SI:EVAL-SPECIAL-OK (LET ((*STANDARD-INPUT* *TERMINAL-IO*))
                                             (SI:READ-FOR-TOP-LEVEL)))))
        (IF ERROR (RETURN-FROM FRED (VALUES NIL T))))   ;Failed to eval, punt
      (TERPRI *TERMINAL-IO*)
      THING)))

(DEFUN INSPECT-FIND-FUNCTION (THING)
  "Given any object THING, return its /"function definition/", or anything like one."
  (LOOP
    (SETQ THING
          (TYPECASE THING
            (SYMBOL
             (IF (FBOUNDP THING)
                 (SYMBOL-FUNCTION THING)
                 (RETURN THING)))
            (INSTANCE
             (SI::INSTANCE-FUNCTION THING))
            ((OR ENTITY CLOSURE)
             (CLOSURE-FUNCTION THING))
            (CONS
             (IF (AND (VALIDATE-FUNCTION-SPEC THING)
                      (FDEFINEDP THING))
                 (FDEFINITION THING)
               ;; takes car of inspect-kludge case
               (RETURN THING)))
            (T (RETURN THING))))))

(DEFUN INSPECT-HELP ()
  (FORMAT *TERMINAL-IO*
          "~&~
You are in the /"INSPECTOR/", looking at the contents of some objects.
Click left on an object with the mouse to display that object's contents.
You can also type an expression to be evaluated; the contents of the
value object will be displayed.  The values of ~S, ~S and ~S are always
the three last displayed objects, from the bottom up.

You can scroll through an object's display with the scroll bar
by moving the mouse against the left margin until the cursor becomes
an up-and-down arrow.  Refer then to the mouse documentation line.
Or you can push the mouse against the /"More Below/" or /"More Above/"
when they appear.  You can scroll the bottom pane a full screenful
by typing ~:C or ~:C.

To modify an object, hold down Hyper and click left on the slot to modify.
You will then be asked for a value to store.  Type an expression whose value
will be used, or point at the value to store with the mouse.

The inspector remembers the contents of objects and does not constantly
check to see if they change.  Type ~:C to display the current contents.

To exit the inspector, type ~:C.  The value of ~S will be returned
from the function INSPECT, if you called it.

Clicking Right instead of Left on an object displays that object's
function definition instead of the object itself.  This is useful with
symbols, lists (function specs), closures and entities, and instances.

Type ~:C to discard everything from the history pane.
Type ~:C to read, evaluate and **print** a value instead of
displaying it.  Type ~:C to get a break loop.

The menu items Exit, DeCache and Clear are equivalent to the
keyboard commands ~:C, ~:C and ~:C.  Modify is analogous
to using the Hyper key.  Return exits returning a value
you specify with the keyboard or mouse, rather than the value of *.
Set ~S sets the variable ~S, bound inside the inspector, to the
value you specify.
" '* '** '***
#/Control-V #/Meta-V #/Clear-screen #/End #/Delete #/Quote #/Break
  #/End '* #/Clear-screen #/Delete '\ '\))


(defvar *inspector-reconciles* t)

(defun inspector-%p-contents-safe-p (pointer)
  (or (%p-contents-safe-p pointer)
      (and (= (%p-data-type pointer) dtp-unreconciled)  ;dtp-free
           (memq :MOBY *features*)
           *inspector-reconciles*)))

(tv:add-system-key #/I 'INSPECT-FRAME "Inspector" T)
