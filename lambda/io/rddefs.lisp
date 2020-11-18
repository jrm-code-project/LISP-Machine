;;; Definitions for the -*- Mode:LISP; Package:SI; Base:8; Readtable:ZL -*- machine reader

;;; WARNING! if you change anything in this file, not only might you
;;; have to recompile READ and PRINT, but RTC as well, and then you
;;; may have to load RTC and recompile the readtable from RDTBL
;;; using RTC-FILE.

(DEFSTRUCT (READTABLE :ARRAY-LEADER :NAMED
                      (:CONSTRUCTOR MAKE-RDTBL)
                      (:MAKE-ARRAY (:DIMENSIONS (RDTBL-ARRAY-DIMS) :TYPE 'ART-16B))
                      (:DEFAULT-POINTER RDTBL)
                      (:SIZE-MACRO RDTBL-SIZE))
  ;; first slot is sacred
  (RDTBL-FSM NIL :DOCUMENTATION
    "The readtable's finite-state machine")
  (RDTBL-N-STATES 0 :DOCUMENTATION
    "Number of states in RDTBL-FSM")
  (RDTBL-N-BUCKETS 0 :DOCUMENTATION
   "Number of buckets in RDTBL-FSM")
  (RDTBL-STARTING-STATE 0 :DOCUMENTATION
   "Starting state for the RDTBL-FSM")
  (RDTBL-SLASH-CODE NIL :DOCUMENTATION
   "Code used for escape-quoted characters")
  (RDTBL-EOF-CODE NIL :DOCUMENTATION
   "Code used to indicate EOF read")
  (RDTBL-BREAK-CODE NIL :DOCUMENTATION
   "Code used to indicate breaks")
  (RDTBL-MACRO-ALIST NIL :DOCUMENTATION
   "Alist of character and macro reader it invokes.")
  (RDTBL-READ-FUNCTION-PROPERTY NIL :DOCUMENTATION
   "")
  (RDTBL-PLIST NIL :DOCUMENTATION
   "Random useful information about this readtable.")
  (RDTBL-DEFINITION NIL :DOCUMENTATION
   "The list structure from which RTC made this readtable.")
  (RDTBL-MAKE-SYMBOL NIL :DOCUMENTATION
   "")
  (RDTBL-MAKE-SYMBOL-BUT-LAST NIL :DOCUMENTATION
   "")
; (RDTBL-SLASH nil :documentation "Not used")
  ;; used in xr-xrtyi and peek-char
  (RDTBL-WHITESPACE NIL :DOCUMENTATION
   "Stuff which is to be counted a whitespace by the reader when using this readtable")
; (RDTBL-CIRCLECROSS nil :documentation "Not used")
  (RDTBL-ESCAPE-CODE            #// :DOCUMENTATION
   "The code representing the character used to quote single characters")
  (RDTBL-MULTIPLE-ESCAPE-CODE   #/| :DOCUMENTATION
   "The code representing the character used to quote multiple characters")
  (RDTBL-CHARACTER-CODE-ESCAPE-CODE #/ :DOCUMENTATION
   "The code representing the character used to read
arbitrary character codes as an octal number")
  (RDTBL-NAMES NIL :DOCUMENTATION
    "A list of strings which are names for this readtable")
  (PTTBL-SPACE                  #/SPACE  :DOCUMENTATION
   "The character for printing a space")
  (PTTBL-NEWLINE                #/RETURN :DOCUMENTATION
   "The character to use for printing a newline")
  (PTTBL-CONS-DOT               " . " :DOCUMENTATION
   "The string to print meaning /"cons-dot/" in lists")
  (PTTBL-MINUS-SIGN             #/- :DOCUMENTATION
   "The character to print for a minus sign")
  (PTTBL-DECIMAL-POINT          #/. :DOCUMENTATION
   "The character to print for a decimal point")
  (PTTBL-SLASH                  #// :DOCUMENTATION
   "The character to print to escape a single character")
  (PTTBL-PRINLEVEL              "#" :DOCUMENTATION
   "The string to print to indicate the list printing is being abbreviated
because the list structure was too deeply nested.")
  (PTTBL-PRINLENGTH             "..." :DOCUMENTATION
   "The string to print to indicate that list printing is being truncated
because the list was too long.")
  (PTTBL-RANDOM                 '("#<" . ">") :DOCUMENTATION
   "Car is the string to print to indicate the start of a random (unreadable) object.
Cdr is string to print to indicate end.")
  (PTTBL-OPEN-PAREN             #/( :DOCUMENTATION
   "The character to print for an open parenthesis")
  (PTTBL-CLOSE-PAREN            #/) :DOCUMENTATION
   "The character to print for a close parenthesis")
  (PTTBL-OPEN-QUOTE-STRING      #/" :DOCUMENTATION
   "The character to print at the start of a quoted string")
  (PTTBL-CLOSE-QUOTE-STRING     #/" :DOCUMENTATION
   "The character to print at the end of a quoted string")
  (PTTBL-OPEN-QUOTE-SYMBOL      #/| :DOCUMENTATION
   "The character to print at the start of a quoted symbol")
  (PTTBL-CLOSE-QUOTE-SYMBOL     #/| :DOCUMENTATION
   "The character to print at the end of a quoted symbol")
  (PTTBL-PACKAGE-PREFIX         ":" :DOCUMENTATION
   "The string to print between a package name and the symbol's pname
for symbols which are /"directly accessible/" from the current")
  (PTTBL-PACKAGE-INTERNAL-PREFIX":" :DOCUMENTATION
   "The string to print between a package name and the symbol's pname
for symbols which are not /"directly accessible/" from the current package")
  (PTTBL-CHARACTER              '("#" "//") :DOCUMENTATION
   "Cons of The string to print to start a character,
and the string to print between the character font and the character name")
  (PTTBL-RATIONAL-INFIX         #/\ :DOCUMENTATION
   "The character to print between the numerator and denominator of a ratio")
  (PTTBL-COMPLEX                '("" NIL "i") :DOCUMENTATION
   "A three-element list describing how to print complex numbers.
The CAR is a string to print before the number.
The CADR is the string to print between the real imaginary parts,
 or NIL, meaning to print either a /"+/" if the imaginary part is positive, else a /"-/"
The CADDR is the string to print after the number")
; (PTTBL-RATIONAL-RADIX         10. :documentation
;  "The radix in which to print ratios, or NIL, meaning to use the value of *PRINT-BASE*")
  (PTTBL-VECTOR                 '("#(" . ")") :DOCUMENTATION
   "Cons of the string to start vectors and the string to terminate vectors")
  (PTTBL-ARRAY                  '("#" :RANK "A" :SEQUENCES) :DOCUMENTATION
   "A list describing how to print arrays when *print-array* is non-NIL.
Look in the code for SI:PRINT-ARRAY to find out how this is used.")
  (PTTBL-BIT-VECTOR             '("#*" :SEQUENCES "") :DOCUMENTATION
   "The string to print to start a bit-vector")
  (PTTBL-UNINTERNED-SYMBOL-PREFIX "#:" :DOCUMENTATION
   "The string to print to start unintered symbols, if *PRINT-GENSYM* is non-NIL")
  (PTTBL-STRUCTURE              '("#S(" :TYPE :SLOTS ")") :DOCUMENTATION
   "Don't know what this should mean yet.")
  )

(DEFSUBST RDTBL-SYMBOL-SUBSTITUTIONS (&OPTIONAL (READTABLE RDTBL))
  (GETF (RDTBL-PLIST READTABLE) 'SYMBOL-SUBSTITUTIONS))
(DEFSUBST RDTBL-NAME (&OPTIONAL (READTABLE RDTBL))
  (CAR (RDTBL-NAMES READTABLE)))
(DEFSUBST RDTBL-SHORT-NAME (&OPTIONAL (READTABLE RDTBL))
  (OR (CADR (RDTBL-NAMES READTABLE))
      (CAR (RDTBL-NAMES READTABLE))))

;;; old names in case there are any old callers of these
(DEFSUBST RDTBL-SLASH (&OPTIONAL (READTABLE RDTBL))
  (RDTBL-ESCAPE-CODE READTABLE))
(DEFSUBST RDTBL-CIRCLECROSS (&OPTIONAL (READTABLE RDTBL))
  (RDTBL-CHARACTER-CODE-ESCAPE-CODE READTABLE))

(DEFVAR RDTBL-ARRAY-SIZE #o240)

(DEFMACRO RDTBL-ARRAY (&OPTIONAL (P 'RDTBL))
  P)

(DEFMACRO RDTBL-ARRAY-DIMS ()
  `',(LIST 3 RDTBL-ARRAY-SIZE))

(DEFMACRO RDTBL-BITS (RDTBL CHAR)
  `(AREF ,RDTBL 0 ,CHAR))

(DEFMACRO RDTBL-CODE (RDTBL CHAR)
  `(AREF ,RDTBL 1 ,CHAR))

(DEFMACRO RDTBL-TRANS (RDTBL CHAR)
  `(AREF ,RDTBL 2 ,CHAR))

;;; Names of special characters, as an a-list.  FORMAT searches this list to
;;; get the inverse mapping (numbers to names), so the preferred name for a value
;;; should be earliest in the list.  New-keyboard names are preferred.  Names (not
;;; necessarily the prefered ones) should include those in the manual, in "The Lisp Machine
;;; Character Set".  This variable is used by quite a few other programs as well, even though
;;; it may look like it is internal to READ.  Here rather than in READ, because this
;;; expression cannot be evaluated in the cold-load.

(DEFCONST %%XR-SPECIAL-CHARACTER-NAMES-MOUSE-BIT #o2401
  "This should be used ONLY for defining XR-SPECIAL-CHARACTER-NAMES.")

(DEFCONST XR-SPECIAL-CHARACTER-NAMES
  (APPEND '((:NULL . #o200) (:NULL-CHARACTER . #o200)
            (:BREAK . #o201) (:SUSPEND . #o201)
            (:CLEAR-INPUT . #o202) (:CLEAR . #o202) (:CLR . #o202)
            (:CALL . #o203)
            (:TERMINAL . #o204) (:ESC . #o204) (:ESCAPE . #o204) (:TERMINAL-ESCAPE . #o204)
                                (:FUNCTION . #o204)
            (:MACRO . #o205) (:BACK-NEXT . #o205) (:BACKNEXT . #o205)
            (:HELP . #o206)
            (:RUBOUT . #o207)
            (:OVERSTRIKE . #o210) (:BACKSPACE . #o210) (:BS . #o210)
            (:TAB . #o211)
            (:LINE . #o212) (:LF . #o212) (:LINEFEED . #o212) (:LINE-FEED . #o212)
            (:DELETE . #o213) (:VT . #o213)
            ;; The keyboard says "CLEAR SCREEN", but it should type out as "PAGE".
            (:PAGE . #o214) (:CLEAR-SCREEN . #o214) (:FORM . #o214) (:FF . #o214)
            (:RETURN . #o215) (:NEWLINE . #o215) (:CR . #o215)
            (:QUOTE . #o216)
            (:HOLD-OUTPUT . #o217)
            (:STOP-OUTPUT . #o220)
            (:ABORT . #o221)
            (:RESUME . #o222)
            (:STATUS . #o223)
            (:END . #o224)
            (:ROMAN-I . #o225) (:ROMAN-II . #o226) (:ROMAN-III . #o227) (:ROMAN-IV . #o230)
            (:HAND-UP . #o231) (:HAND-DOWN . #o232)
            (:HAND-LEFT . #o233) (:HAND-RIGHT . #o234)
            (:SYSTEM . #o235) (:SELECT . #o235)
            (:NETWORK . #o236)

            (:CENTER-DOT . 0) (:CENTRE-DOT . 0) ;Amerikans can't spell...
            (:DOWN-ARROW . 1)
            (:ALPHA . 2) (:BETA . 3) (:AND-SIGN . 4) (:NOT-SIGN . 5)
            (:EPSILON . 6) (:PI . 7) (:LAMBDA . #o10) (:GAMMA . #o11) (:DELTA . #o12)
            (:UP-ARROW . #o13) (:UPARROW . #o13)
            (:PLUS-MINUS . #o14) (:CIRCLE-PLUS . #o15)
            (:INFINITY . #o16) (:PARTIAL-DELTA . #o17)
            (:LEFT-HORSESHOE . #o20) (:RIGHT-HORSESHOE . #o21)
            (:UP-HORSESHOE . #o22) (:DOWN-HORSESHOE . #o23)
            (:UNIVERSAL-QUANTIFIER . #o24) (:FOR-ALL . #o24)
            (:EXISTENTIAL-QUANTIFIER . #o25) (:THERE-EXISTS . #o25)
            (:CIRCLE-X . #o26) (:CIRCLE-CROSS . #o26) (:TENSOR . #o26)
            (:DOUBLE-ARROW . #o27) (:LEFT-ARROW . #o30) (:RIGHT-ARROW . #o31)
            (:NOT-EQUAL . #o32)(:NOT-EQUALS . #o32)
            (:ALTMODE . #o33) (:ALT . #o33) (:DIAMOND . #o33)
            (:LESS-OR-EQUAL . #o34) (:GREATER-OR-EQUAL . #o35) (:EQUIVALENCE . #o36)
            (:OR-SIGN . #o37) (:OR . #o37)

            (:SPACE . #o40) (:SP . #o40)
            (:INTEGRAL . #o177)
            (:COKE-BOTTLE . 259.) (:COKEBOTTLE . 259.)
            )
          (MAPCAR (LAMBDA (X) (CONS (CAR X)
                                    (DPB 1 %%XR-SPECIAL-CHARACTER-NAMES-MOUSE-BIT
                                         (CDR X))))
              '((:MOUSE-L . 0) (:MOUSE-L-1 . 0) (:MOUSE-L-2 . #o10) (:MOUSE-L-3 . #o20)
                (:MOUSE-M . 1) (:MOUSE-M-1 . 1) (:MOUSE-M-2 . #o11) (:MOUSE-M-3 . #o21)
                (:MOUSE-R . 2) (:MOUSE-R-1 . 2) (:MOUSE-R-2 . #o12) (:MOUSE-R-3 . #o22)
                (:MOUSE-1-1 . 0) (:MOUSE-1-2 . #o10)
                (:MOUSE-2-1 . 1) (:MOUSE-2-2 . #o11)
                (:MOUSE-3-1 . 2) (:MOUSE-3-2 . #o12))))
  "Alist of names of special characters, in the form of symbols in the keyword pkg,
and the character values they correspond to.")

(DEFMACRO PRINTING-RANDOM-OBJECT ((OBJECT STREAM . OPTIONS) &BODY BODY)
  "A macro for aiding in the printing of /"random/" /(unreadable) objects.
This macro generates a form which:
   1.  Uses the print-table to find the things in which to enclose the
       printed representation of OBJECT.
   2.  By default, includes the virtual address in the printed representation.
   3.  If SI:PRINT-READABLY is non-NIL, signals an error, since OBJECT cannot
       be read back in.
/
Valid OPTIONS include:
        :NO-POINTER to suppress printing the virtual address.
        :TYPE to PRIN1 the TYPE-OF of the object first.
/
Example:
/
/(defun print-ship (ship stream ignore)
  (flet ((maybe-tab ()
                    (format stream /"~:[ ~;~&~5T~]/" *print-pretty*)))
    (si:printing-random-object (ship stream :type :no-pointer)
      (format stream /":X-POSITION ~S/" (ship-x-position ship)) (maybe-tab)
      (format stream /":Y-POSITION ~S/" (ship-y-position ship)) (maybe-tab)
      (format stream /":X-VELOCITY ~S/" (ship-x-velocity ship)) (maybe-tab)
      (format stream /":Y-VELOCITY ~S/" (ship-y-velocity ship))
      (format stream /")/"))))
/
/(defstruct (ship :named (:print-function print-ship))
  (x-position 0.0)
  (y-position 0.0)
  (x-velocity 0.0)
  (y-velocity 0.0))
/
/(let ((*print-pretty* t))
  (print (make-ship)))
/
#<USER::SHIP
     :X-POSITION 0.0
     :Y-POSITION 0.0
     :X-VELOCITY 0.0
     :Y-VELOCITY 0.0)>
"
  (declare (arglist ((object stream &rest options) &body body)))
  (LET ((%POINTER T)
        (TYPE NIL))
    (IF (EQ STREAM T) (SETQ STREAM '*STANDARD-OUTPUT*)) ;inconsistent with decode-print-arg...
    (DO ((L OPTIONS (CDR L)))
        ((NULL L))
      (CASE (CAR L)
        (:NO-POINTER (SETQ %POINTER NIL))
        ((:TYPE :TYPEP) (SETQ TYPE T))
        ;; hysterical lossage.  Perhaps nobody uses this.  They shouldn't!
        (:FASTP (SETQ L (CDR L)))
        (T (FERROR "~S is an unknown keyword in ~S"
                   (CAR L) 'PRINTING-RANDOM-OBJECT))))
    `(progn
       (print-random-object-prefix ,object ,stream ,type ,(not (not (and type body))))
       ,@body
       (print-random-object-suffix ,object ,stream ,(not %pointer))
       ,object)))

(DEFSUBST DECODE-PRINT-ARG (ARG)
  (COND ((EQ ARG NIL) *STANDARD-OUTPUT*)
        ((EQ ARG T) *TERMINAL-IO*)
        (T ARG)))

(DEFSUBST DECODE-READ-ARG (ARG)
  (COND ((EQ ARG NIL) *STANDARD-INPUT*)
        ((EQ ARG T) *TERMINAL-IO*)
        (T ARG)))

(DEFSELECT ((:PROPERTY READTABLE NAMED-STRUCTURE-INVOKE) IGNORE)
  (:DESCRIBE (RDTBL) (DESCRIBE-DEFSTRUCT RDTBL))
  (:PRINT-SELF (RDTBL STREAM IGNORE &OPTIONAL IGNORE)
    (IF *PRINT-ESCAPE*
        (SYS:PRINTING-RANDOM-OBJECT (RDTBL STREAM :TYPE :NO-POINTER)
          (FORMAT STREAM "~@[~A ~]~O" (RDTBL-NAME RDTBL) (%POINTER RDTBL)))
      (IF (RDTBL-NAME RDTBL)
          (FORMAT STREAM "~A readtable" (RDTBL-NAME RDTBL))
        (FORMAT STREAM "#<Readtable ~O>" (%POINTER RDTBL)))))
  ((:GET :GET-LOCATION-OR-NIL :GET-LOCATION :GETL :PUTPROP :REMPROP :PUSH-PROPERTY :PLIST
    :PROPERTY-LIST :PLIST-LOCATION :PROPERTY-LIST-LOCATION :SETPLIST :SET)
   . READTABLE-PROPERTY-LIST-HANDLER)
  (:FASD-FIXUP (RDTBL)
    (IF (RDTBL-NAMES RDTBL)
        (PUSHNEW RDTBL *ALL-READTABLES* :TEST #'EQ))))
(DEFUN READTABLE-PROPERTY-LIST-HANDLER (OP RDTBL &REST ARGS)
  (APPLY 'PROPERTY-LIST-HANDLER OP (LOCF (RDTBL-PLIST RDTBL)) ARGS))
