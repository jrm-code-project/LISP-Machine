;;; -*- Mode:LISP; Package:FORMAT; Base:10; Readtable:CL -*-
;;; Function for printing or creating nicely formatted strings.

;;; (c) Copyright 1980 Massachusetts Institute of Technology.
;;; (c) Enhancements Copyright 1985, Lisp Machines, Incorporated.

;;; -> I cleaned up most of the explicit storage management, since it was
;;; -> interacting with the GC in obscure ways.  The only remaining cruft
;;; -> is the format-params resource, but this isn't too bad, and can't
;;; -> crash the machine no matter how you abuse it.  All internal format
;;; -> structures now go in format:format-area (volatility 3).  During
;;; -> normal programming activity (Zmacs, compiler, error-handler),
;;; -> FORMAT-AREA tends to grow about 10000. words in 10 minutes.  The GC
;;; -> should be able to deal with this easily.  KHS 850812.

;;; FORMAT prints several arguments according to a control argument.
;;; The control argument is either a string or a list of strings and lists.
;;; The strings and lists are interpreted consecutively.
;;; Strings are for the most part just printed, except that the character ~
;;; starts an escape sequence which directs other actions.
;;; A ~ escape sequence has an (optional) numeric parameter followed by a mode character.
;;; These escape actions can use up one or more of the non-control arguments.
;;; A list in the control-argument list is also interpreted as an escape.
;;; Its first element is the mode, a symbol which may be any length,
;;; and its remaining elements are parameters.  The list (D 5) is equivalent
;;; to the ~ escape "~5D";  similarly, each ~ escape has an equivalent list.
;;; However, there are list escapes which have no ~ equivalent.

;;; Any undefined list escape is simply evaluated.

;;; Further documentation is at the head of the function FORMAT.

;;; (FORMAT <stream> <control arg> &REST <args>)
;;; If <stream> is NIL, cons up and return a string.
;;; If <stream> is T, use *STANDARD-OUTPUT* (saves typing).

;;; *FORMAT-STRING* is used independently of FORMAT by FORMAT-STRING-STREAM, so
;;; it should be globally ().  FORMAT lambda-binds this when outputting to a string.
(DEFVAR *FORMAT-STRING* ())                            ;The string used for output by (FORMAT NIL ...)

(DEFVAR *FORMAT-PACKAGE* (pkg-find-package 'format))    ;Where format commands are interned.
;; don't use "(SYMBOL-PACKAGE 'FOO)" as symbol-package isn't in the cold-load

(DEFVAR *CTL-STRING*)                           ;The control string.
(DEFVAR *CTL-LENGTH*)                           ;LENGTH of *CTL-STRING*.
(DEFVAR *CTL-INDEX*)                            ;Our current index into the control string.  This
                                                ;  is used by the conditional command.
(DEFVAR *ATSIGN-FLAG*)                          ;Modifier
(defvar atsign-flag)
(forward-value-cell 'atsign-flag '*atsign-flag*); Users know about these old names
(DEFVAR *COLON-FLAG*)                           ;Modifier
(defvar colon-flag)
(forward-value-cell 'colon-flag '*colon-flag*)
(DEFVAR *FORMAT-PARAMS*)                        ;Array for pushing parameters
(DEFVAR *FORMAT-ARGLIST*)                       ;The original arg list, for ~@*.
(DEFVAR *LOOP-ARGLIST*)                         ;Loop arglist, for ~:^.
(DEFVAR *FORMAT-CHAR-TABLE*)                    ;Table of single-char symbols, for fast interning.

(defvar format-area ())
(add-initialization "Create FORMAT area."
                    '(make-area :name 'format-area :gc :dynamic :volatility 3)
                    :once)

(DEFMACRO DEFFORMAT (DIRECTIVE (ARG-TYPE) LAMBDA-LIST &BODY BODY)
  "Define a format directive named DIRECTIVE (a symbol).
If DIRECTIVE is in the FORMAT package and its name is one character long,
you can use this directive by writing that character after a ~ in a format string.
Otherwise, then you must use the ~\\...\\ syntax to use this directive.
ARG-TYPE is a keyword saying how many format arguments this directive uses up.
It can be :NO-ARG, :ONE-ARG or :MULTI-ARG.
:NO-ARG means this directive doesn't use any of the format args (like ~T or ~%).
 LAMBDA-LIST should receive one argument,
 which will be a list of the parameters given (such as 3 and 5 in ~3,5T).
:ONE-ARG means this directive uses one format arg (like ~D).
 LAMBDA-LIST should receive two args,
 the first being one format arg and the second being the list of parameters given.
:MULTI-ARG means this directive decides how many format args to use up (like ~n*).
 LAMBDA-LIST should receive two args,
 the first being the list of format args and the second being the list of parameters given.
 Then the BODY should return as a value the list of format args left over."
  (let ((property (ecase arg-type
                    (:no-arg 'format-ctl-no-arg)
                    (:one-arg 'format-ctl-one-arg)
                    (:multi-arg 'format-ctl-multi-arg))))
    `(progn
       (si:record-source-file-name ',directive 'defformat)
       ,(if (and (atom lambda-list) (null body))
            `(defprop ,directive ,lambda-list ,property)
          `(defun (:property ,directive ,property)
                  ;; *FORMAT-OUTPUT* is Brand S compatibility (and a good idea in any case)
                  (,@lambda-list)
             (declare (function-parent ,directive defformat))
             (let ((*format-output* *standard-output*))
               *format-output*                  ;Prevent unused variable warning.
               . ,body))))))

;;; Make *FORMAT-CHAR-TABLE* into an array whose i'th element is the
;;; symbol, in *FORMAT-PACKAGE*, whose pname is the character whose code is i.
;;; This provides a fast alternative to INTERN-SOFT, for single-character symbols.
;;; CHARS is the list of symbols to put in the table.
;;; All single-character FORMAT operations must be included.
(DEFUN INIT-CHAR-TABLE (CHARS)
  (SETQ *FORMAT-CHAR-TABLE* (MAKE-ARRAY #o200))
  (DO ((CHARS CHARS (CDR CHARS))) ((NULL CHARS))
    (SETF (AREF *FORMAT-CHAR-TABLE*
                (CHAR-CODE (CHAR (SYMBOL-NAME (CAR CHARS)) 0)))
          (CAR CHARS))))

(INIT-CHAR-TABLE '(A B C D E F G O P R Q S T V X [ ] \; % \| < > * &   ^  { } ~ $ ? \( \)))


;;; Little arrays in which to cons up lists of parameters
(DEFRESOURCE FORMAT-PARAMS ()
  :CONSTRUCTOR (zl:MAKE-ARRAY 10. :TYPE 'ART-Q-LIST :FILL-POINTER 0 :area format-area)
  :free-list-cell (aloc object 0)
  :deinitializer (setf (array-leader object 0) 0))   ; :DEINITIALIZER SI::WIPE-STRUCTURE

;(DEFRESOURCE FORMAT-PARAMS ()
;  :CONSTRUCTOR (MAKE-ARRAY 10. :TYPE 'ART-Q-LIST :FILL-POINTER 0 :area format-area)
;  :INITIAL-COPIES 6 :DEINITIALIZER SI::WIPE-STRUCTURE)


(DEFUN MAKE-FORMAT-STRING ()
  (MAKE-STRING 128. :FILL-POINTER 0 :AREA FORMAT-AREA))

(DEFUN MAKE-STRING-OUTPUT-STREAM (&OPTIONAL STRING START-INDEX EXTRA-ARG)
  "Return a stream that accumulates output in a string.
If STRING is specified, output is STRING-NCONC'd onto it.
Otherwise a new string is created and used;
GET-OUTPUT-STREAM-STRING can be used on the stream to get the accumulated string."
  ;; We need hair here to detect calls that use the old calling sequence
  ;; where the first argument was a Common Lisp thing not really used
  ;; and STRING was the second argument.
  (IF (STRINGP START-INDEX)
      (LET ((STRING START-INDEX)
            (START-INDEX EXTRA-ARG))
        (LET-CLOSED ((*FORMAT-STRING* (OR STRING (MAKE-FORMAT-STRING))))
          (IF START-INDEX
              (SETF (FILL-POINTER *FORMAT-STRING*) START-INDEX))
          #'FORMAT-STRING-STREAM))
    (LET-CLOSED ((*FORMAT-STRING* (OR STRING (MAKE-FORMAT-STRING))))
      (IF START-INDEX
          (SETF (FILL-POINTER *FORMAT-STRING*) START-INDEX))
      #'FORMAT-STRING-STREAM)))

(DEFUN GET-OUTPUT-STREAM-STRING (STREAM)
  "Return the string of characters accumulated so far by STREAM.
STREAM must be a stream made by MAKE-OUTPUT-STRING-STREAM.
This clears the stream's data, so that if GET-OUTPUT-STREAM-STRING is called
a second time it will only get the data output after the first time it was called."
  (SEND STREAM 'EXTRACT-STRING))


(DEFPROP FORMAT-STRING-STREAM T SI:IO-STREAM-P)
;;; (FORMAT NIL ...) outputs to this stream, which just puts the characters
;;; into the string *FORMAT-STRING*.
(DEFSELECT (FORMAT-STRING-STREAM FORMAT-STRING-STREAM-DEFAULT-HANDLER)
  ((:TYO :WRITE-CHAR) (CH)
   (OR *FORMAT-STRING* (SETQ *FORMAT-STRING* (MAKE-FORMAT-STRING)))
   (VECTOR-PUSH-EXTEND CH *FORMAT-STRING*))

  (:STRING-OUT (STRING &OPTIONAL (FIRST 0) LAST &AUX NEW-LENGTH)
   (OR *FORMAT-STRING* (SETQ *FORMAT-STRING* (MAKE-FORMAT-STRING)))
   (SETQ LAST (OR LAST (LENGTH STRING)))
   (SETQ NEW-LENGTH (+ (FILL-POINTER *FORMAT-STRING*) (- LAST FIRST)))
   (AND (< (ARRAY-LENGTH *FORMAT-STRING*) NEW-LENGTH)
        (ADJUST-ARRAY-SIZE *FORMAT-STRING* NEW-LENGTH))
   (COPY-ARRAY-PORTION STRING FIRST LAST
                       *FORMAT-STRING* (FILL-POINTER *FORMAT-STRING*) NEW-LENGTH)
   (SETF (FILL-POINTER *FORMAT-STRING*) NEW-LENGTH))

  (:READ-CURSORPOS (&OPTIONAL (MODE :CHARACTER) &AUX POS)
   (OR *FORMAT-STRING* (SETQ *FORMAT-STRING* (MAKE-FORMAT-STRING)))
   (OR (EQ MODE ':CHARACTER)
       (FERROR "Strings only have a width in ~S, not ~S" :CHARACTER MODE))
   (SETQ POS (STRING-REVERSE-SEARCH-CHAR #\NEWLINE *FORMAT-STRING*))
   (VALUES (- (LENGTH *FORMAT-STRING*) (IF POS (+ POS 1) 0))
           0))

  (:INCREMENT-CURSORPOS (DX DY &OPTIONAL (MODE :CHARACTER) &AUX NEWLEN)
   (OR *FORMAT-STRING* (SETQ *FORMAT-STRING* (MAKE-FORMAT-STRING)))
   (UNLESS (EQ MODE ':CHARACTER)
     (FERROR "Strings can only have a width in ~S, not ~S" :CHARACTER MODE))
   (OR (AND (ZEROP DY) (NOT (MINUSP DX)))
       (FERROR "Cannot do this ~S" :INCREMENT-CURSORPOS))
   (SETQ NEWLEN (+ (LENGTH *FORMAT-STRING*) DX))
   (AND (< (ARRAY-LENGTH *FORMAT-STRING*) NEWLEN)
        (ADJUST-ARRAY-SIZE *FORMAT-STRING* NEWLEN))
   (DO ((I (LENGTH *FORMAT-STRING*) (1+ I)))
       (( I NEWLEN))
     (SETF (CHAR *FORMAT-STRING* I) #\SPACE))
   (SETF (FILL-POINTER *FORMAT-STRING*) NEWLEN))

  (:SET-CURSORPOS (X Y &OPTIONAL (MODE :CHARACTER) &AUX POS DELTA NEWLEN)
   (OR *FORMAT-STRING* (SETQ *FORMAT-STRING* (MAKE-FORMAT-STRING)))
   (UNLESS (EQ MODE :CHARACTER)
     (FERROR "Strings can only have a width in ~S, not ~S" :CHARACTER MODE))
   (SETQ POS (STRING-REVERSE-SEARCH-SET '(#\NEWLINE #\LINE #\FORM) *FORMAT-STRING*)
         DELTA (- X (- (LENGTH *FORMAT-STRING*) (IF POS (+ POS 1) 0))))
   (OR (AND (ZEROP Y) (PLUSP DELTA))
       (FERROR "Cannot do this ~S" :SET-CURSORPOSE))
   (SETQ NEWLEN (+ (LENGTH *FORMAT-STRING*) DELTA))
   (AND (< (ARRAY-LENGTH *FORMAT-STRING*) NEWLEN)
        (ADJUST-ARRAY-SIZE *FORMAT-STRING* NEWLEN))
   (DO ((I (LENGTH *FORMAT-STRING*) (1+ I)))
       (( I NEWLEN))
     (SETF (CHAR *FORMAT-STRING* I) #\SPACE))
   (SETF (FILL-POINTER *FORMAT-STRING*) NEWLEN))
  (:UNTYO-MARK () (FILL-POINTER *FORMAT-STRING*))
  (:UNTYO (MARK) (SETF (FILL-POINTER *FORMAT-STRING*) MARK))
  (EXTRACT-STRING ()
   (PROG1 *FORMAT-STRING*
          (SETQ *FORMAT-STRING* NIL)))
  (:FRESH-LINE ()
   (WHEN (NOT (OR (NULL *FORMAT-STRING*)
                  (ZEROP (LENGTH *FORMAT-STRING*))
                  (CHAR= (CHAR *FORMAT-STRING* (1- (LENGTH *FORMAT-STRING*))) #\NEWLINE)))
     (VECTOR-PUSH-EXTEND #\NEWLINE *FORMAT-STRING*)
     T))
  (:beep (ignore)))

(DEFUN FORMAT-STRING-STREAM-DEFAULT-HANDLER (OP &OPTIONAL ARG1 &REST REST)
  (STREAM-DEFAULT-HANDLER 'FORMAT-STRING-STREAM OP ARG1 REST))


(DEFUN FORMAT (STREAM CTL-STRING &REST ARGS)
  "Format arguments according to a control string and print to a stream.
\(If the stream is T, *STANDARD-OUTPUT* is used;
 if NIL, a string is returned containing the formatted text.)
The control string is copied to the stream, but ~ indicates special formatting commands.
~D  ~mincol,padchar,commacharD   Print number as a decimal integer.
    ~:D  Print the comma character every three digits.
    ~@D  Always print the sign.   ~:@D  Both.
~O  Analogous to ~D, but prints in octal.
~X  Analogous to ~D, but prints in hex.
~B  Analogous to ~X, but prints in binary.
~F  ~w,d,s,overflowchar,padcharF  Print float in nonexponential notation.
    Multiplies by 10^s before printing if s is specified.
    Prints in w positions, with d digits after the decimal point.
    Pads on left with padchar if nec.  If number doesn't fit in w positions,
    and overflowchar is specified, just fills the w positions with that character.
~E  ~w,d,e,s,overflowchar,padchar,exptcharE   Print float in exponential notation.
    Prints in w positions, with e digits of exponent.
    If s (default is 1) is positive, prints s digits before point, d-s+1 after.
    If s is zero, prints d digits after the point, and a zero before if there's room.
    If s is negative, prints d digits after the point, of which the first -s are zeros.
    If exptchar is specified, it is used to delimit the exponent
    (instead of \"e\" or whatever.)
    If overflowchar is specified, then if number doesn't fit in specified width,
    or if exponent doesn't fit in e positions, field is filled with overflowchar instead.
~G  Like ~E, but if number fits without exponent, prints without one.
~$  ~w,x,y,z$ prints a floating-point number with exactly w (default 2) digits to right of
     decimal, at least x (default 1) to left of decimal, right-justified in field y wide
     padded with z.  @ print + sign.  : sign to left of padding.
~R  ~R  Print number as an English cardinal number.
    ~:R  English ordinal number.   ~@R  Roman numeral.   ~:@R  Old Roman numeral.
    ~nR  Print number in radix n.  Thus ~8R = ~O, and ~10R = ~D.
    Extra parameters are as for ~D (~n,mincol,padchar,commacharR).
~A  Ascii output (PRINC).  Good for printing strings.  ~mincol,colinc,minpad,padcharA.
    ~@A  Right-justify the string.   ~:A  Make NIL print as ().  ~:@A  Both.
~S  Analogous to ~A, but uses PRIN1, not PRINC.
~C  Print a character.  Mouse characters print in standard format.
    ~C  Actual character, preceded by \"c-\", \"m-\", \"s-\" or \"h-\" if necessary.
    ~:C  Format effectors print as names.  Names of control bits (\"Control-\") precede.
    ~@C  Prints the character in READ format, using #\\.
    ~:@C  Like ~:C, but top/front/greek characters are followed by remark, e.g. \" (Top-S)\".
~*  Ignore an argument.   ~n*  Ignore n arguments.   ~:n*  Back up n arguments (default 1).
    ~n@* Go to argument n.
~%  Insert a newline.     ~n%  Insert n newlines.
~~  Insert a tilde.       ~n~  Insert n tildes.
~|  Insert a form.        ~n|  Insert n forms.
    ~:|  Do :CLEAR-WINDOW if the stream supports it, otherwise insert a form.   ~:n|  Similar.
~<cr>  Ignore a CR and following whitespace in the control string.
    ~:<cr> Ignore the CR, retain the whitespace.  ~@<cr> Retain the CR, ignore the whitespace.
~&  Do a :FRESH-LINE.     ~n&  Do a FRESH-LINE, then insert n-1 newlines.
~^  Terminate processing if no more arguments.  Within ~{...~}, just terminate the loop.
    ~n;  Terminate if n is zero.  ~n,m;  Terminate if n=m.  ~n,m,p;  Terminate if nmp.
    ~:^  When within ~:{...~}, ~^ terminates this iteration.  Use ~:^ to exit the loop.
~T  ~mincol,colincT  Tab to column mincol+p*colinc, for the smallest integer p possible.
    ~mincol,colinc:T  Same, but tabs in TV pixels rather than characters.
    ~n@T  Insert n spaces.
    ~n,colinc@T   Insert n spaces, then move 0 or more up to multiple of colinc.
~Q  Apply next argument to no arguments.  ~a,b,c,...,zQ  Apply next argument to parameters
    a,b,c,...z.  In (Q ...) form, apply argument to unevaled parameters.
~P  Pluralize.  Insert \"s\", unless argument is 1.
    ~:P  Use previous argument, not next one (i.e. do ~:* first).
    ~@P  Insert \"y\" if argument is 1, otherwise insert \"ies\".   ~:@P  Both.
~(  ~(...~)  Force lower case for the output generated within.
    ~:(...~)  Similar but capitalize each word.
    ~@(...~)  Similar but capitalize the first word.
    ~:@(...~)  Similar but force all upper case.
    ~1(...~)  Force first letter of first word to upper case, leave all else alone.
~?  Indirect.  Uses up two args; first is a format string, second is args for it.
    ~@? uses up one arg directly, as a format string, but it operates on
    the remaining format args and can use them up.
~<  ~mincol,colinc,minpad,padchar<str0~;str1~;...~;strn~>  Do formatting for all formatting
    strings strj; then output all strings with padding between them at the ~; points.
    Each padding point must have at least minpad padding characters.  Subject to that,
    the total width must be at least mincol, and must be mincol+p*colinc for some p.
    If str0 is followed by ~:; instead of ~;, then str0 is not normally output, and the
    ~:; is not a padding point.  Instead, after the total width has been determined,
    if the text will not fit into the current line of output, then str0 is output before
    outputting the rest.  (Doesn't work when producing a string.)  An argument n (~:n;)
    means that the text plus n more columns must fit to avoid outputting str0.  A second
    argument m (~n,m:;) provides the line width to use instead of the stream's width.
    ~:<  Also have a padding point at the left.  Hence ~n:<x~> right-justifies x in n columns.
    ~@<  Also have a padding point at the right.   ~:@<  Both.   Hence ~n:@<x~> centers x.
~[  ~[str0~;str1~;...~;strn~]  Select.  Argument selects one clause to do.  If argument is not
    between 0 and n inclusive, then no alternative is performed.  If a parameter is given,
    then use the parameter instead of an argument.  (The only useful one is \"#\".)
    If the last string is preceded by ~:;, it is an \"else\" clause, and is processed if
    no other string is selected.
    One can also tag the clauses explicitly by giving arguments to ~;.  In this case the
    first string must be null, and arguments to ~; tag the following string.  The
    argument is matched against the list of parameters for each ~;.  One can get ranges
    of tags by using ~:;.  Pairs of parameters serve as inclusive range limits.
    A ~:; with no parameters is still an \"else\" clause.
    Example:  ~[~'+,'-,'*,'/;operator~:'A,'Z,'a,'z;letter~:'0,'9;digit~:;other~]
    will produce \"operator\", \"letter\", \"digit\", or \"other\" as appropriate.
    ~:[iffalse~;iftrue~]  The argument selects the first clause if nil, the second if non-nil.
    ~@[str~]  If the argument is non-nil, then it is not swallowed, and str is processed.
    Otherwise, the nil is swallowed and str is ignored.  Thus ~@[~S~] will PRIN1 a
    non-null thing.
~{  ~{str~}  Use str as a format string for each element in the argument.  More generally,
    the argument is a list of things to be used as successive arguments, and str is used
    repeatedly as a format string until the arguments are exhausted (or ~^ is used).
    Within the iteration the commands ~* and ~@* move among the iteration arguments,
    not among all the arguments given to FORMAT.
    ~n{str~} repeats the string at most n times.
    Terminating with ~:} forces str to be processed at least once.
    ~:{str}  The argument is a list of lists, and each repetition sees one sublist.
    ~@{str}  All remaining arguments are used as the list.
    ~:@{str}  Each remaining argument is a list.
    If the str within a ~{ is empty, then an argument (which must be a string) is used.
    This argument precedes any that are iterated over as loop arguments.
~  ~str~ Successive lines within str are indented to align themselves with the column
    at which str began. ie all text within str will lie to the right of the beginning of str
In place of a numeric parameter, one may use V, which uses an argument to supply the number;
or one may use #, which represents the number of arguments remaining to be processed;
or one may use 'x, which uses the ascii value of x (good for pad characters).
The control string may actually be a list of intermixed strings and sublists.
In that case, the strings are printed literally.  The first atom in a sublist should be
the name of a command, and remaining elements are parameters."
  (declare (unspecial ctl-string))                     ;>>for recompilation
  (check-type ctl-string (or string cons error) "a string")
  (if (stringp stream)
      (assert (array-has-fill-pointer-p stream) (stream)
              "If the first argument, ~S, to ~S is a string, it must have a fill-pointer"
              'stream 'format))
  (let ((*ctl-string* ctl-string)
        (default-cons-area format-area))
    (let-if (typep stream '(or null string))
            ;; Only bind *FORMAT-STRING* if STREAM is NIL.  This avoids lossage if
            ;; FORMAT with a first arg of NIL calls FORMAT recursively (e.g. if
            ;; printing a named structure).
            ((*FORMAT-STRING* (if (stringp stream) stream (make-format-string))))
      (LET ((*STANDARD-OUTPUT* (si:follow-all-syn-streams
                                 (COND ((NULL STREAM) 'FORMAT-STRING-STREAM)
                                       ((STRINGP STREAM) 'FORMAT-STRING-STREAM)
                                       ((EQ STREAM T) *STANDARD-OUTPUT*)
                                       (T STREAM))))
            (*FORMAT-ARGLIST* ARGS)
            (*LOOP-ARGLIST* NIL))
        (CATCH 'FORMAT-\:^-POINT
          (CATCH 'FORMAT-^-POINT
            (TYPECASE CTL-STRING
              (STRING (FORMAT-CTL-STRING ARGS CTL-STRING))
              (SYMBOL (FORMAT-CTL-STRING ARGS (SYMBOL-NAME CTL-STRING)))
              ;; pretty bloody marginal
              (ERROR (PRINC CTL-STRING))
              (T (DO ((CTL-STRING CTL-STRING (CDR CTL-STRING))) ((NULL CTL-STRING))
                   (IF (STRINGP (CAR CTL-STRING))
                       (SEND *STANDARD-OUTPUT* :STRING-OUT (CAR CTL-STRING))
                     (SETQ ARGS (FORMAT-CTL-LIST ARGS (CAR CTL-STRING))))))))))
      ;; Copy returned string out of temporary area and reclaim
      (WHEN (NULL STREAM)                                      ;return string or nil
        ;; this should return a simple-string (ie without fill-pointer)
        (let* ((len (length *format-string*))
               ;; consing this in format-area is the wrong thing, really.
               ;;  Many things (eg pathname code) expect to be able to bind
               ;;  default-cons-area and then call format, expecting to find the resulting
               ;;  string in the area they specified.
               ;; On the other kettle of fish, most people who call (format nil ...)
               ;;  tend to use the resulting string for a very brief time only, and
               ;;  having it end up in the often-flipped format-area is just the right
               ;;  thing in that case.
               ;; I (Mly) think that the following behaviour is WRONG and should probably
               ;;  be changed, though it seems to win so much in practice that perhaps the
               ;;  right thing is to change any callers who care to copy the result into
               ;;  an area about which they care.
               (new (make-string len :area format-area)))
          (copy-array-portion *format-string* 0 len new 0 len)
       new)))))

;;; Call this to signal an error in FORMAT processing.  If *CTL-STRING* is a string, then
;;; *CTL-INDEX* should point one beyond the place to be indicated in the error message.

(DEFUN FORMAT-ERROR (STRING &REST ARGS)
  (DECLARE (DBG:ERROR-REPORTER))
  (IF (STRINGP *CTL-STRING*)
      (FERROR "~1{~:}~%~VT~%   \"~A\"~%" STRING ARGS
              (- *CTL-INDEX*
                 1
                 (OR (STRING-REVERSE-SEARCH-CHAR #\NEWLINE *CTL-STRING* *CTL-INDEX*)
                     -4))
              *CTL-STRING*)
    (FERROR "~1{~:}" STRING ARGS)))

(DEFUN FORMAT-CTL-LIST (ARGS CTL-LIST &AUX (*ATSIGN-FLAG* NIL) (*COLON-FLAG* NIL))
  (FORMAT-CTL-OP (IF (GETL (CAR CTL-LIST)
                           '(FORMAT-CTL-ONE-ARG FORMAT-CTL-NO-ARG
                             FORMAT-CTL-MULTI-ARG FORMAT-CTL-REPEAT-CHAR))
                     (CAR CTL-LIST)
                   (INTERN-LOCAL-SOFT (CAR CTL-LIST) *FORMAT-PACKAGE*))
                 ARGS (CDR CTL-LIST)))

(DEFUN FORMAT-CTL-STRING (ARGS *CTL-STRING* &AUX (*FORMAT-PARAMS* NIL))
  (UNWIND-PROTECT
      (DO ((*CTL-INDEX* 0)
           (*CTL-LENGTH* (LENGTH *CTL-STRING*))
           (TEM))
          (( *CTL-INDEX* *CTL-LENGTH*))
        (SETQ TEM (%STRING-SEARCH-CHAR #\~ *CTL-STRING* *CTL-INDEX* *CTL-LENGTH*))
        (UNLESS (EQ TEM *CTL-INDEX*)                   ;Put out some literal string
          (SEND *STANDARD-OUTPUT* :STRING-OUT *CTL-STRING* *CTL-INDEX* TEM)
          (IF (NULL TEM) (RETURN))
          (SETQ *CTL-INDEX* TEM))
        ;; (CHAR *CTL-STRING* *CTL-INDEX*) is a tilde.
        (LET ((*ATSIGN-FLAG* NIL)
              (*COLON-FLAG* NIL))
          (IF (NULL *FORMAT-PARAMS*)
              (SETQ *FORMAT-PARAMS* (ALLOCATE-RESOURCE 'FORMAT-PARAMS)))
          (SETF (FILL-POINTER *FORMAT-PARAMS*) 0)
          (MULTIPLE-VALUE-SETQ (TEM ARGS) (FORMAT-PARSE-COMMAND ARGS T))
          (SETQ ARGS (FORMAT-CTL-OP TEM ARGS (G-L-P *FORMAT-PARAMS*)))))
    (AND *FORMAT-PARAMS* (DEALLOCATE-RESOURCE 'FORMAT-PARAMS *FORMAT-PARAMS*)))
  ARGS)

;;; Expects *ATSIGN-FLAG*, *COLON-FLAG*, and *FORMAT-PARAMS* to be bound.
;;; *CTL-INDEX* points to a tilde.  Returns command name and new ARGS,
;;; leaving *CTL-INDEX* after the command.  NIL for the command name
;;; means no command there
;;; If SWALLOW-ARGS is NIL, we are not executing commands, just parsing,
;;; e.g. to find a matching ~}, ~], or ~>.  So don't swallow any args (e.g. for ~V).
(DEFUN FORMAT-PARSE-COMMAND (ARGS SWALLOW-ARGS)
  (DO ((PARAM-FLAG NIL)                                ;If T, a parameter has been started in PARAM
       (START *CTL-INDEX*)                             ;for error message
       CH
       TEM
       SYM
       (SIGN NIL)                                      ;Sign of parameter currently being constructed.
       (PARAM NIL))                                    ;PARAM is the parameter currently being constructed
      (( (INCF *CTL-INDEX*) *CTL-LENGTH*)
       (SETQ *CTL-INDEX* (1+ START))
       (FORMAT-ERROR "Command fell off end of control string"))
    (SETQ CH (CHAR-UPCASE (CHAR *CTL-STRING* *CTL-INDEX*)))
    (COND ((CHAR #\0 CH #\9)   ;DIGIT-CHAR-P not loaded yet
           (SETQ TEM (- CH #\0))
           (SETQ PARAM (+ (* (OR PARAM 0) 10.) TEM)
                 PARAM-FLAG T))
          ((CHAR= CH #\-)
           (SETQ SIGN (NOT SIGN)))
          ((CHAR= CH #\+) NIL)
          ((CHAR= CH #\@)
           (SETQ *ATSIGN-FLAG* T))
          ((CHAR= CH #\:)
           (SETQ *COLON-FLAG* T))
          ((CHAR= CH #\V)
           (WHEN (AND (NULL ARGS) SWALLOW-ARGS)
             (INCF *CTL-INDEX*)
             (FORMAT-ERROR "No argument for \"V\" parameter to use"))
           (SETQ PARAM (POP ARGS) PARAM-FLAG T))
          ((CHAR= CH #\#)
           (SETQ PARAM (LENGTH ARGS) PARAM-FLAG T))
          ((CHAR= CH #\')
           (SETQ PARAM (CHAR-CODE (CHAR *CTL-STRING* (INCF *CTL-INDEX*))) PARAM-FLAG T))
          ((CHAR= CH #\,)                              ;comma, begin another parameter
           (AND SIGN PARAM (SETQ PARAM (- PARAM)))
           (VECTOR-PUSH PARAM *FORMAT-PARAMS*)
           (SETQ PARAM NIL PARAM-FLAG T SIGN NIL))     ;omitted arguments made manifest by
                                                       ; presence of comma are NIL
          ((CHAR= CH #\NEWLINE)                        ;No command, just ignoring a CR
           (INCF *CTL-INDEX*)                          ;Skip the newline
           (UNLESS *COLON-FLAG*                        ;Unless colon, skip whitespace on next line
             (DO () ((OR ( *CTL-INDEX* *CTL-LENGTH*)
                         (NOT (MEMQ (CHAR *CTL-STRING* *CTL-INDEX*) '(#\SPACE #\TAB)))))
               (INCF *CTL-INDEX*)))
           (RETURN (values 'CRLF ARGS)))
          (T                                           ;Must be a command character
           (INCF *CTL-INDEX*)                          ;Advance past command character
           (AND SIGN PARAM (SETQ PARAM (- PARAM)))
           (AND PARAM-FLAG (VECTOR-PUSH PARAM *FORMAT-PARAMS*))
           (SETQ PARAM-FLAG NIL PARAM NIL TEM NIL)
           ;; SYM gets the symbol for the operation to be performed.
           (IF (CHAR= CH #\\)
               ;; There's no way to quote a "\"  Who could possibly care?
               (LET ((I (%STRING-SEARCH-CHAR #\\ *CTL-STRING* *CTL-INDEX* *CTL-LENGTH*))
                     (*PACKAGE* *FORMAT-PACKAGE*))
                 (IF (NULL I) (FORMAT-ERROR "Unmatched \"\\\" in control string."))
                 (SETQ SYM (IF (OR (%STRING-SEARCH-CHAR #\: *CTL-STRING* *CTL-INDEX* I)
                                   (%STRING-SEARCH-CHAR #\| *CTL-STRING* *CTL-INDEX* I)
                                   )
                               (let ((*readtable* si::initial-common-lisp-readtable)
                                     (default-cons-area format-area))
                                 (CL:READ-FROM-STRING *CTL-STRING* T NIL
                                                      :START *CTL-INDEX* :END I))
                               (INTERN-SOFT (NSTRING-UPCASE (SUBSTRING *CTL-STRING* *CTL-INDEX* I
                                                                       FORMAT-AREA))
                                            *FORMAT-PACKAGE*)))
                 (SETQ *CTL-INDEX* (1+ I)))
             (SETQ SYM (OR (AND (< (CHAR-INT CH) (LENGTH *FORMAT-CHAR-TABLE*))
                                (AREF *FORMAT-CHAR-TABLE* (CHAR-INT CH)))
                           (INTERN-SOFT CH *FORMAT-PACKAGE*))))
           (RETURN (VALUES SYM ARGS))))))

;;; Perform a single formatted output operation on specified args.
;;; Return the remaining args not used up by the operation.
(DEFUN FORMAT-CTL-OP (OP ARGS PARAMS &AUX TEM)
  (COND ((NULL OP)                                     ;eg not interned
         (FORMAT-ERROR "Undefined ~S command." 'FORMAT)
         ARGS)
        ((SETQ TEM (GET OP 'FORMAT-CTL-ONE-ARG))
         (FUNCALL TEM (CAR ARGS) PARAMS)
         (CDR ARGS))
        ((SETQ TEM (GET OP 'FORMAT-CTL-NO-ARG))
         (FUNCALL TEM PARAMS)
         ARGS)
        ((SETQ TEM (GET OP 'FORMAT-CTL-MULTI-ARG))
         (FUNCALL TEM ARGS PARAMS))
        ((SETQ TEM (GET OP 'FORMAT-CTL-REPEAT-CHAR))
         (FORMAT-CTL-REPEAT-CHAR (OR (CAR PARAMS) 1) TEM)
         ARGS)
        (T
         (FORMAT-ERROR "\"~S\" is not defined as a ~S command." OP 'FORMAT)
         ARGS)))

; ~<newline>
(DEFFORMAT CRLF (:NO-ARG) (IGNORE)
  (AND *ATSIGN-FLAG* (SEND *STANDARD-OUTPUT* :TYO #\NEWLINE)))

;; Several commands have a SIZE long object which they must print
;; in a WIDTH wide field.  If WIDTH is specified and is greater than
;; the SIZE of the thing to be printed, this put out the right
;; number of  CHARs to fill the field.  You can call this before
;; or after printing the thing, to get leading or trailing padding.
(DEFUN FORMAT-CTL-JUSTIFY (WIDTH SIZE &OPTIONAL (CHAR #\SPACE))
  (AND WIDTH (> WIDTH SIZE) (FORMAT-CTL-REPEAT-CHAR (- WIDTH SIZE) CHAR)))

;;; Fixed point output.

;;; princ in base 10.
(DEFFORMAT D (:ONE-ARG) FORMAT-CTL-DECIMAL)

(DEFUN FORMAT-CTL-DECIMAL (ARG PARAMS &OPTIONAL (*PRINT-BASE* 10.)      ;Also called for octal
                           &AUX (*NOPOINT T)    ;God I hate this variable
                           (*PRINT-RADIX* NIL)
                           (WIDTH (FIRST PARAMS))
                           (PADCHAR (SECOND PARAMS))
                           (COMMACHAR (THIRD PARAMS))
                           (PLUS-P (AND *ATSIGN-FLAG*
                                        (NUMBERP ARG)
                                        (NOT (MINUSP ARG)))))
  (SETQ PADCHAR (COND ((NULL PADCHAR) #\SPACE)
                      ((NUMBERP PADCHAR) (INT-CHAR PADCHAR))
                      ((CHARACTERP PADCHAR) PADCHAR)
                      (T (CHAR (STRING PADCHAR) 0))))
  (SETQ COMMACHAR (COND ((NULL COMMACHAR) #\,)
                        ((NUMBERP COMMACHAR) (INT-CHAR COMMACHAR))
                        ((CHARACTERP COMMACHAR) COMMACHAR)
                        (T (CHAR (STRING COMMACHAR) 0))))
  (AND WIDTH (FORMAT-CTL-JUSTIFY WIDTH
                                 (+ (IF (FIXNUMP ARG)
                                        (+ (LOOP FOR X = (ABS ARG) THEN (FLOOR X *PRINT-BASE*)
                                                 COUNT T
                                                 UNTIL (< X *PRINT-BASE*))
                                           (IF (MINUSP ARG) 1 0))
                                      (FLATC ARG))
                                    (IF PLUS-P 1 0)
                                    (IF (AND *COLON-FLAG* (INTEGERP ARG))
                                        (FLOOR (1- (FLATC (ABS ARG))) 3)   ;Number of commas
                                      0))
                                 PADCHAR))
  (AND PLUS-P (SEND *STANDARD-OUTPUT* :TYO #\+))
  (COND ((AND *COLON-FLAG* (INTEGERP ARG))
         ;; Random hair with commas.  I'm not going to bother not consing.
         (COND ((MINUSP ARG) (SEND *STANDARD-OUTPUT* :TYO #\-) (SETQ ARG (- ARG))))
           (SETQ ARG (NREVERSE (INHIBIT-STYLE-WARNINGS  ;Give up!
                                 (EXPLODEN ARG))))
           (DO ((L ARG (CDR L))
                (I 2 (1- I)))
               ((NULL (CDR L)))
             (COND ((ZEROP I)
                    (RPLACD L (CONS COMMACHAR (CDR L)))
                    (SETQ I 3 L (CDR L)))))
           (DOLIST (CH (NREVERSE ARG))
             (SEND *STANDARD-OUTPUT* :TYO CH)))
        ((FIXNUMP ARG) (SI::PRINT-FIXNUM ARG *STANDARD-OUTPUT*))
        ;; This is PRINC rather than PRIN1 so you can have a string instead of a number
        (T (PRINC ARG))))

;;; princ in base 8
(DEFFORMAT O (:ONE-ARG) (ARG PARAMS)
  (FORMAT-CTL-DECIMAL ARG PARAMS 8))

;;; princ in base 2
(DEFFORMAT B (:ONE-ARG) (ARG PARAMS)
  (FORMAT-CTL-DECIMAL ARG PARAMS 2))

;;; princ in base 16  --- used to mean ~@T
(DEFFORMAT X (:ONE-ARG) (ARG PARAMS)
  (FORMAT-CTL-DECIMAL ARG PARAMS 16.))

;;; princ in roman numerals or in a specified base
(DEFFORMAT R (:ONE-ARG) (ARG PARAMS)
  (COND ((CAR PARAMS) (FORMAT-CTL-DECIMAL ARG (CDR PARAMS) (CAR PARAMS)))
        ((AND *ATSIGN-FLAG*
              (INTEGERP ARG)
              (< ARG 4000.)
              (> ARG 0))
         (ROMAN-STEP ARG 0 *COLON-FLAG*))
        ((OR *ATSIGN-FLAG*
             (NOT (INTEGERP ARG)))
         (LET ((*PRINT-BASE* 10.) (*NOPOINT T) (*PRINT-RADIX* NIL))
           (PRIN1 ARG)))
        ((NOT *COLON-FLAG*)
         (ENGLISH-PRINT ARG))
        (T (ENGLISH-ORDINAL-PRINT ARG))))

(DEFCONST ENGLISH-SMALL
          #("one" "two" "three" "four" "five" "six"
            "seven" "eight" "nine" "ten" "eleven" "twelve"
            "thirteen" "fourteen" "fifteen" "sixteen"
            "seventeen" "eighteen" "nineteen"))

(DEFCONST ENGLISH-ORDINAL-SMALL
          #("first" "second" "third" "fourth" "fifth"
            "sixth" "seventh" "eighth" "ninth"
            "tenth" "eleventh" "twelfth" "thirteenth"
            "fourteenth" "fifteenth" "sixteenth"
            "seventeenth" "eighteenth" "nineteenth"))

(DEFCONST ENGLISH-MEDIUM
          #("twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))

(DEFCONST ENGLISH-ORDINAL-MEDIUM
          #("twentieth" "thirtieth" "fortieth" "fiftieth"
            "sixtieth" "seventieth" "eightieth" "ninetieth"))

;; Not really "english," actually "american."  Yuck
(DEFCONST ENGLISH-LARGE
          #("" "thousand" "million" "billion" "trillion"
               "quadrillion" "quintillion" "sextillion"
               "septillion" "octillion" "nonillion" "decillion"
               "undecillion" "duodecillion"))

(DEFCONST ENGLISH-ORDINAL-LARGE
          #("" "thousandth" "millionth" "billionth"
               "trillionth" "quadrillionth" "quintillionth"
               "sextillionth" "septillionth" "octillionth" "nonillionth"
               "decillionth" "undecillionth" "duodecillionth"))

(DEFCONST ENGLISH-100 "hundred")

(DEFCONST ENGLISH-ORDINAL-100 "hundredth")

;;; Returns T if it printed anything, else NIL.
(DEFUN ENGLISH-PRINT-THOUSAND (N STREAM)
  (LET ((FLAG NIL)
        (N (CL:REM N 100.))
        (H (FLOOR N 100.)))
    (WHEN (> H 0)
      (SETQ FLAG T)
      (SEND STREAM :STRING-OUT (AREF ENGLISH-SMALL (1- H)))
      (SEND STREAM :TYO #\SPACE)
      (SEND STREAM :STRING-OUT ENGLISH-100)
      (AND (> N 0) (SEND STREAM :TYO #\SPACE)))
    (COND ((= N 0))
          ((< N 20.)
           (SETQ FLAG T)
           (SEND STREAM :STRING-OUT (AREF ENGLISH-SMALL (1- N))))
          (T
           (SETQ FLAG T)
           (SEND STREAM :STRING-OUT (AREF ENGLISH-MEDIUM (- (FLOOR N 10.) 2)))
           (UNLESS (ZEROP (SETQ H (CL:REM N 10.)))
             (WRITE-CHAR #\- STREAM)
             (SEND STREAM :STRING-OUT (AREF ENGLISH-SMALL (1- H))))))
    FLAG))

;;; Returns T if it printed anything, else NIL.
(DEFUN ENGLISH-PRINT (N &OPTIONAL (STREAM *STANDARD-OUTPUT*) (TRIAD 0))
  (COND ((ZEROP N)
         (WHEN (ZEROP TRIAD)
           (SEND STREAM :STRING-OUT "zero")
           T))
        ((< N 0)
         (SEND STREAM :STRING-OUT "minus")
         (SEND STREAM :TYO #\SPACE)
         (ENGLISH-PRINT (MINUS N) STREAM)
         T)
        (T
         (LET ((FLAG (ENGLISH-PRINT (FLOOR N 1000.) STREAM (1+ TRIAD))))
           (LET ((THIS-TRIPLET (CL:REM N 1000.)))
             (cond ((ZEROP THIS-TRIPLET)
                    FLAG)
                   (t
                    (IF FLAG (SEND STREAM :TYO #\SPACE))
                    (IF (EQ FLAG 'EXPT) (SEND STREAM :STRING-OUT "plus "))
                    (ENGLISH-PRINT-THOUSAND THIS-TRIPLET STREAM)
                    (COND ((ZEROP TRIAD) T)
                          ((> TRIAD 13.)
                           (SEND STREAM :STRING-OUT " times ten to the ")
                           (ENGLISH-ORDINAL-PRINT (* 3 TRIAD))
                           (SEND STREAM :STRING-OUT " power")
                           'EXPT)
                          (T
                           (WRITE-CHAR #\SPACE STREAM)
                           (SEND STREAM :STRING-OUT (AREF ENGLISH-LARGE TRIAD))
                           T)))))))))

(DEFUN ENGLISH-ORDINAL-PRINT (N &OPTIONAL (STREAM *STANDARD-OUTPUT*))
  (IF (ZEROP N)
      (SEND STREAM :STRING-OUT "zeroth")
    (DO ((I (IF (= (CL:REM (FLOOR N 10.) 10.) 0)
                10. 100.)
            (* I 10.))
         (TEM) (TEM1))
        (( (SETQ TEM (CL:REM N I)) 0)
         (WHEN ( (SETQ TEM1 (- N TEM)) 0)
           (ENGLISH-PRINT (- N TEM) STREAM)
           (SEND STREAM :TYO #\SPACE))
         ;>> Yucko!
         (LET ((ENGLISH-SMALL (IF (AND (= (CL:REM TEM 10.) 0)  ( TEM 10.))
                                  ENGLISH-SMALL
                                  ENGLISH-ORDINAL-SMALL))
               (ENGLISH-MEDIUM (IF (= (CL:REM TEM 10.) 0)
                                   ENGLISH-ORDINAL-MEDIUM
                                   ENGLISH-MEDIUM))
               (ENGLISH-100 ENGLISH-ORDINAL-100)
               (ENGLISH-LARGE ENGLISH-ORDINAL-LARGE))
           (ENGLISH-PRINT TEM STREAM))))))

(DEFUN ROMAN-STEP (X N ROMAN-OLD)
  (declare (unspecial roman-old))                      ;>>for recompilation
  (WHEN (> X 9.)
    (ROMAN-STEP (FLOOR X 10.) (1+ N) roman-old)
    (SETQ X (CL:REM X 10.)))
  (COND ((AND (= X 9) (NOT ROMAN-OLD))
         (ROMAN-CHAR 0 N)
         (ROMAN-CHAR 0 (1+ N)))
        ((= X 5)
         (ROMAN-CHAR 1 N))
        ((AND (= X 4) (NOT ROMAN-OLD))
         (ROMAN-CHAR 0 N)
         (ROMAN-CHAR 1 N))
        (T (WHEN (> X 5)
             (ROMAN-CHAR 1 N)
             (SETQ X (- X 5)))
           (DOTIMES (I X)
             (ROMAN-CHAR 0 N)))))

(DEFUN ROMAN-CHAR (I X)
  (SEND *STANDARD-OUTPUT* :TYO (CHAR "IVXLCDM" (+ I X X))))


;;; Funny bases
(DEFUN (:PROPERTY :ENGLISH SI:PRINC-FUNCTION) (X STREAM)
  (FORMAT STREAM "~R" (- X)))

(DEFUN (:PROPERTY :ROMAN SI:PRINC-FUNCTION) (X STREAM)
  (FORMAT STREAM "~@R" (- X)))

(DEFUN (:PROPERTY :ROMAN-OLD SI:PRINC-FUNCTION) (X STREAM)
  (FORMAT STREAM "~:@R" (- X)))

(DEFFORMAT F (:ONE-ARG) FORMAT-CTL-HAIRY-F-FORMAT)
(DEFUN FORMAT-CTL-HAIRY-F-FORMAT (ARG PARAMS)
  (TYPECASE ARG
    (RATIONAL (SETQ ARG (FLOAT ARG)))
    (FLOAT)
    (T (RETURN-FROM FORMAT-CTL-HAIRY-F-FORMAT
         (WITH-STACK-LIST (WIDTH (CAR PARAMS))
           (FORMAT-CTL-DECIMAL ARG WIDTH)))))
  (LET* ((WIDTH (CAR PARAMS))
         (AFTER-DECIMAL (CADR PARAMS))
         (SCALE (CADDR PARAMS))
         (OVERFLOWCHAR (FOURTH PARAMS))
         (PADCHAR (FIFTH PARAMS))
         (WIDTH-AFTER-SIGN
           (AND WIDTH
                (IF (OR (MINUSP ARG) *ATSIGN-FLAG*) (- WIDTH 1) WIDTH))))
    (WHEN SCALE
      ;; can you say "loss of precision" boys and girls?
      (if (> (abs scale) (length si::powers-of-10f0-table))
          (format-error "10^~D is too ~:[large~;small~] a multiplier" scale (< scale 0))
        (setq arg (if ( scale 0)
                      (* arg (aref si::powers-of-10f0-table scale))
                      (cl:/ arg (aref si::powers-of-10f0-table (- scale)))))))
    (MULTIPLE-VALUE-BIND (BUFFER)
        (SI::FLONUM-TO-STRING (ABS ARG) NIL
                              (AND WIDTH (1- WIDTH-AFTER-SIGN))
                              AFTER-DECIMAL T)
      (WHEN WIDTH
        (WHEN (AND OVERFLOWCHAR
                   (> (LENGTH BUFFER) WIDTH-AFTER-SIGN))
          ;; Does not fit in specified width => print overflow chars.
          (DOTIMES (I WIDTH)
            (SEND *STANDARD-OUTPUT* :TYO OVERFLOWCHAR))
          (RETURN-FROM FORMAT-CTL-HAIRY-F-FORMAT NIL))
        ;; Space left over => print padding.
        (DOTIMES (I (- WIDTH-AFTER-SIGN (LENGTH BUFFER)))
          (SEND *STANDARD-OUTPUT* :TYO (OR PADCHAR #\SPACE))))
      (COND ((MINUSP ARG) (SEND *STANDARD-OUTPUT* :TYO #\-))
            (*ATSIGN-FLAG* (SEND *STANDARD-OUTPUT* :TYO #\+)))
      (SEND *STANDARD-OUTPUT* :STRING-OUT BUFFER))))

;(DEFPROP F FORMAT-CTL-F-FORMAT FORMAT-CTL-ONE-ARG)
;(DEFUN FORMAT-CTL-F-FORMAT (ARG PARAMS)
;  (AND (NUMBERP ARG) (NOT (FLOATP ARG)) (SETQ ARG (FLOAT ARG)))
;  (IF (NOT (FLOATP ARG))
;      (FORMAT-CTL-DECIMAL ARG NIL)
;    (SI::PRINT-FLONUM ARG STANDARD-OUTPUT NIL (TYPEP ARG 'SHORT-FLOAT)
;                     (CAR PARAMS) NIL)))

(DEFFORMAT E (:ONE-ARG) FORMAT-CTL-HAIRY-E-FORMAT)
;; Copied from LAD: RELEASE-3.IO; FORMAT.LISP#266 on 27-Mar-87 10:37:35
(DEFUN FORMAT-CTL-HAIRY-E-FORMAT (ORIGINAL-ARG PARAMS)
  (TYPECASE ORIGINAL-ARG
    (RATIONAL (SETQ ORIGINAL-ARG (FLOAT ORIGINAL-ARG)))
    (FLOAT)
    (T (RETURN-FROM FORMAT-CTL-HAIRY-E-FORMAT
         (WITH-STACK-LIST (WIDTH (CAR PARAMS))
           (FORMAT-CTL-DECIMAL ORIGINAL-ARG WIDTH)))))
  (PROG* ((WIDTH (CAR PARAMS))
          (AFTER-DECIMAL (CADR PARAMS))
          (EXPONENT-DIGITS (THIRD PARAMS))
          (SCALE (OR (FOURTH PARAMS) 1))
          (OVERFLOWCHAR (FIFTH PARAMS))
          (PADCHAR (SIXTH PARAMS))
          (EXPONENTCHAR (SEVENTH PARAMS))
          (NEGATIVE (MINUSP ORIGINAL-ARG)))
      RETRY
         (MULTIPLE-VALUE-BIND (ARG EXPONENT)
             (if (zerop original-arg)
                 (values original-arg 0)
               (SI::SCALE-FLONUM (IF NEGATIVE ORIGINAL-ARG (- ORIGINAL-ARG))))
           ;; If user does not specify number of exponent digits, guess.
           (UNLESS EXPONENT-DIGITS
             (SETQ EXPONENT-DIGITS
                   (COND ((> (ABS EXPONENT) 99.) 3)
                         ((> (ABS EXPONENT) 9) 2)
                         (T 1))))
           (LET ((WIDTH-AFTER-SIGN-AND-EXPONENT
                   (AND WIDTH
                        (- (IF (OR NEGATIVE *ATSIGN-FLAG*) (- WIDTH 1) WIDTH)
                           EXPONENT-DIGITS
                           2))))
             (MULTIPLE-VALUE-BIND (BUFFER DECIMAL-PLACE)
                 (SI::FLONUM-TO-STRING ARG NIL
                                       (AND WIDTH (1- WIDTH-AFTER-SIGN-AND-EXPONENT))
                                       (AND AFTER-DECIMAL
                                            (IF (PLUSP SCALE)
                                                AFTER-DECIMAL
                                                (1- AFTER-DECIMAL))))
               ;; Correct "10.0", caused by carry, into "1.0"
               (WHEN (= DECIMAL-PLACE 2)
                 (SETF (CHAR BUFFER 2) (CHAR BUFFER 1))
                 (SETF (CHAR BUFFER 1) #\.)
                 (IF (CHAR= (CHAR BUFFER (1- (LENGTH BUFFER))) #\0)
                     (DECF (FILL-POINTER BUFFER)))
                 (DECF DECIMAL-PLACE)
                 (INCF EXPONENT))
               (DECF EXPONENT (- SCALE 1))
               (LET ((EXTRA-ZERO (AND ( SCALE 0)
                                      (> WIDTH-AFTER-SIGN-AND-EXPONENT (LENGTH BUFFER)))))
                 (WHEN WIDTH
                   (WHEN (AND OVERFLOWCHAR
                              (OR (> (LENGTH BUFFER) WIDTH-AFTER-SIGN-AND-EXPONENT)
                                  (AND (THIRD PARAMS)
                                       ( (ABS EXPONENT)
                                          (EXPT 10. EXPONENT-DIGITS)))))
                     ;; Does not fit in specified width => print overflow chars.
                     ;; Do not bomb out on an exponent that doesn't fit
                     ;; unless the number of exponent digits was explicitly specified.
                     (RETURN
                       (DOTIMES (I WIDTH)
                         (SEND *STANDARD-OUTPUT* :TYO OVERFLOWCHAR))))
                   ;; If exponent needs extra digits but we aren't bombing out,
                   ;; allocate more space to exponent and try again.
                   ;; This way we try to stay within the specified field width
                   ;; by taking away from other things.
                   (DO ((I 1 (1+ I))
                        (X 10. (* X 10.)))
                       ((> X (ABS EXPONENT))
                        (WHEN (> I EXPONENT-DIGITS)
                          (SETQ EXPONENT-DIGITS I)
                          (GO RETRY))))
                   ;; Space left over => print padding.
                   (DOTIMES (I (- WIDTH-AFTER-SIGN-AND-EXPONENT (LENGTH BUFFER)
                                  (IF EXTRA-ZERO 1 0)))
                     (SEND *STANDARD-OUTPUT* :TYO (OR PADCHAR #\SPACE))))
                 (COND (NEGATIVE (SEND *STANDARD-OUTPUT* :TYO #\-))
                       (*ATSIGN-FLAG* (SEND *STANDARD-OUTPUT* :TYO #\+)))
                 (WHEN EXTRA-ZERO
                   (SEND *STANDARD-OUTPUT* :TYO #\0)))
               (WHEN (MINUSP SCALE)
                 (SEND *STANDARD-OUTPUT* :TYO (SI::PTTBL-DECIMAL-POINT *READTABLE*))
                 (DOTIMES (I (- SCALE))
                   (SEND *STANDARD-OUTPUT* :TYO #\0))
                 (DECF (FILL-POINTER BUFFER) (- SCALE)))
               (DOTIMES (I (1- (LENGTH BUFFER)))
                 (WHEN (= I SCALE)
                   (SEND *STANDARD-OUTPUT* :TYO (SI::PTTBL-DECIMAL-POINT *READTABLE*)))
                 (SEND *STANDARD-OUTPUT* :TYO
                       (CHAR BUFFER (IF ( I DECIMAL-PLACE) (1+ I) I))))))
           (SEND *STANDARD-OUTPUT* :TYO
                 (OR EXPONENTCHAR
                     (COND ((EQ (NOT (TYPEP ARG 'SHORT-FLOAT))
                                (NEQ *READ-DEFAULT-FLOAT-FORMAT* 'SHORT-FLOAT))
                            #\e)
                           ((TYPEP ARG 'SHORT-FLOAT) #\s)
                           (T #\f))))
           (SEND *STANDARD-OUTPUT* :TYO
                 (IF (MINUSP EXPONENT) #\- #\+))
           (LET ((*ATSIGN-FLAG* NIL)
                 (*COLON-FLAG* NIL))
             (WITH-STACK-LIST (PARAMS EXPONENT-DIGITS #\0)
               (FORMAT-CTL-DECIMAL (ABS EXPONENT) PARAMS))))))

;(DEFPROP E FORMAT-CTL-E-FORMAT FORMAT-CTL-ONE-ARG)
;(DEFUN FORMAT-CTL-E-FORMAT (ARG PARAMS)
;  (AND (NUMBERP ARG) (NOT (FLOATP ARG)) (SETQ ARG (FLOAT ARG)))
;  (IF (NOT (FLOATP ARG))
;      (FORMAT-CTL-DECIMAL ARG NIL)
;    (SI::PRINT-FLONUM ARG STANDARD-OUTPUT NIL (TYPEP ARG 'SHORT-FLOAT)
;                      (CAR PARAMS) T)))

;(DEFUN FORMAT-CTL-GOTO (IGNORE PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
;    (NTHCDR COUNT *FORMAT-ARGLIST*))
;(DEFPROP G FORMAT-CTL-HAIRY-G-FORMAT FORMAT-CTL-COMMON-LISP-ONE-ARG)

;; Copied from LAD: RELEASE-3.IO; FORMAT.LISP#266 on 27-Mar-87 10:37:38
;>> This is multi-arg to hack old ~G calls
(DEFFORMAT G (:MULTI-ARG) (ARGS PARAMS)
  (block nil
    (let ((arg (car args)))
      (typecase arg
        ((integer 0 3)
         ;;>> try to catch old callers, who used ~G to mean ~@*
         (if params
             (return (nthcdr (car params) *format-arglist*))
           (setq arg (float arg))))
        (rational (setq arg (float arg)))
        (float)
        (t (return (with-stack-list (width (car params))
                     (format-ctl-decimal arg width)))))
      (BLOCK NIL
        (LET* ((WIDTH (CAR PARAMS))
               (AFTER-DECIMAL (CADR PARAMS))
               (EXPONENT-DIGITS (OR (THIRD PARAMS) 2))
               (OVERFLOWCHAR (FIFTH PARAMS))
               (PADCHAR (SIXTH PARAMS))
               (EXPONENT-WIDTH (+ EXPONENT-DIGITS 2))
               (WIDTH-AFTER-EXPONENT
                 (AND WIDTH
                      (- WIDTH EXPONENT-WIDTH)))
               (NEGATIVE (MINUSP ARG))
               )
          (MULTIPLE-VALUE-BIND (NIL EXPONENT)
              (if (zerop arg)
                  (values arg 0)
                (SI::SCALE-FLONUM (IF NEGATIVE ARG (- ARG))))
            (UNLESS AFTER-DECIMAL
              ;; If number of sig figs not specified, compute # digits needed for fixed format.
              (IF (> (ABS EXPONENT) (or WIDTH 0))
                  ;; If it's going to be gross, don't bother.
                  ;; We know that E format will be used, so go use it.
                  (RETURN (FORMAT-CTL-HAIRY-E-FORMAT ARG PARAMS)))
              (MULTIPLE-VALUE-BIND (BUFFER)
                  (SI::FLONUM-TO-STRING (ABS ARG) NIL
                                        (AND WIDTH (- WIDTH-AFTER-EXPONENT
                                                      (IF (OR NEGATIVE *ATSIGN-FLAG*) 2 1)))
                                        NIL T)
                (SETQ AFTER-DECIMAL
                      (MAX (1- (LENGTH BUFFER)) (MIN (1+ EXPONENT) 7)))))
            (LET ((DECIMALS-NEEDED-IF-FIXED (- AFTER-DECIMAL EXPONENT 1)))
              (IF ( 0 DECIMALS-NEEDED-IF-FIXED AFTER-DECIMAL)
                  (WITH-STACK-LIST (PARAMS WIDTH-AFTER-EXPONENT
                                           DECIMALS-NEEDED-IF-FIXED
                                           NIL OVERFLOWCHAR PADCHAR)
                    (FORMAT-CTL-HAIRY-F-FORMAT
                      ARG
                      PARAMS)
                    (DOTIMES (I EXPONENT-WIDTH)
                      (SEND *STANDARD-OUTPUT* :TYO #\SPACE)))
                (FORMAT-CTL-HAIRY-E-FORMAT ARG PARAMS))))))
      (CDR ARGS))))

;;; This doesn't support RDIG being 0.  That would be nice, but is complicated.
(DEFFORMAT $ (:ONE-ARG) (ARG PARAMS)
  (LET ((RDIG (OR (FIRST PARAMS) 2))            ;This many digits after decimal point
        (LDIG (OR (SECOND PARAMS) 1))           ;At least this many to left of decimal
        (FIELD (THIRD PARAMS))                  ;Right-justify in field this wide
        (PADCHAR (OR (FOURTH PARAMS) #\SPACE))) ;Padding with this
    (COND ((OR (NOT (NUMBERP ARG)) (> (ABS ARG) 1e50))
           (FORMAT-CTL-JUSTIFY FIELD (FLATC ARG) PADCHAR)
           (PRINC ARG))
          (T (OR (FLOATP ARG) (SETQ ARG (FLOAT ARG)))
             (MULTIPLE-VALUE-BIND (STR IDIG)
                 (SI::FLONUM-TO-STRING (ABS ARG) (TYPEP ARG 'SHORT-FLOAT) NIL RDIG)
               (LET ((WIDTH (+ (IF (OR *ATSIGN-FLAG* (MINUSP ARG)) 1 0)
                               (MAX (- LDIG IDIG) 0)
                               (LENGTH STR))))
                 (IF (NOT *COLON-FLAG*) (FORMAT-CTL-JUSTIFY FIELD WIDTH PADCHAR))
                 (COND ((MINUSP ARG)
                        (SEND *STANDARD-OUTPUT* :TYO (SI::PTTBL-MINUS-SIGN *READTABLE*)))
                       (*ATSIGN-FLAG* (SEND *STANDARD-OUTPUT* :TYO #\+)))
                 (IF *COLON-FLAG* (FORMAT-CTL-JUSTIFY FIELD WIDTH PADCHAR))
                 (DOTIMES (I (- LDIG IDIG)) (SEND *STANDARD-OUTPUT* :TYO #\0))
                 (SEND *STANDARD-OUTPUT* :STRING-OUT STR)))))))


(DEFFORMAT A (:ONE-ARG) FORMAT-CTL-ASCII)
(DEFFORMAT S (:ONE-ARG) (ARG PARAMS)
  (FORMAT-CTL-ASCII ARG PARAMS T))

(DEFUN FORMAT-CTL-ASCII (ARG PARAMS &OPTIONAL PRIN1P)
  (LET ((EDGE (CAR PARAMS))
        (PERIOD (CADR PARAMS))
        (MIN (CADDR PARAMS))
        (PADCHAR (CADDDR PARAMS)))
    (COND ((NULL PADCHAR)
           (SETQ PADCHAR #\SPACE))
          ((CHARACTERP PADCHAR))
          ((FIXNUMP PADCHAR) (SETQ PADCHAR (INT-CHAR PADCHAR)))
          (T (SETQ PADCHAR (CHAR (STRING PADCHAR) 0))))
    (COND (*ATSIGN-FLAG*)                               ;~@5nA right justifies
          ((AND *COLON-FLAG* (NULL ARG)) (SEND *STANDARD-OUTPUT* :STRING-OUT "()"))
          (PRIN1P (PRIN1 ARG))
          ((STRINGP ARG) (SEND *STANDARD-OUTPUT* :STRING-OUT ARG))
          (T (PRINC ARG)))
    (WHEN EDGE
      (LET ((WIDTH (FUNCALL (COND (PRIN1P #'FLATSIZE)
                                  ((STRINGP ARG) #'LENGTH)
                                  (T #'FLATC))
                            ARG)))
        (WHEN MIN
          (FORMAT-CTL-REPEAT-CHAR MIN PADCHAR)
          (INCF WIDTH MIN))
        (IF PERIOD
            (FORMAT-CTL-REPEAT-CHAR
              (- (+ EDGE (* (FLOOR (+ (- (MAX EDGE WIDTH) EDGE 1)
                                      PERIOD)
                                   PERIOD)
                            PERIOD))
                 WIDTH)
              PADCHAR)
          (FORMAT-CTL-JUSTIFY EDGE WIDTH PADCHAR))))
    (COND ((NOT *ATSIGN-FLAG*))
          ((AND *COLON-FLAG* (NULL ARG)) (SEND *STANDARD-OUTPUT* :STRING-OUT "()"))
          (PRIN1P (PRIN1 ARG))
          ((STRINGP ARG) (SEND *STANDARD-OUTPUT* :STRING-OUT ARG))
          (T (PRINC ARG)))))

;;;; Character output modes

(DEFFORMAT LOZENGED-CHARACTER (:ONE-ARG) FORMAT-CTL-LOZENGED-CHAR)
(DEFFORMAT LOZENGED-CHAR (:ONE-ARG) FORMAT-CTL-LOZENGED-CHAR)

(defun format-ctl-lozenged-char (char ignore)
  (if (operation-handled-p *standard-output* :display-lozenged-string)
      (send *standard-output* :display-lozenged-string
                              (or (char-name char)
                                  (make-string 1 :initial-element char)))
    (format-ctl-character char nil)))

(DEFFORMAT LOZENGED-STRING (:ONE-ARG) (STRING PARAMS)
  (SETQ STRING (STRING STRING))
  (IF (AND (OPERATION-HANDLED-P *STANDARD-OUTPUT* :DISPLAY-LOZENGED-STRING)
           (DOTIMES (I (LENGTH STRING) T)
             (UNLESS (GRAPHIC-CHAR-P (CHAR STRING I)) (RETURN NIL))))
      (SEND *STANDARD-OUTPUT* :DISPLAY-LOZENGED-STRING STRING)
    (FORMAT-CTL-ASCII STRING PARAMS)))

;;; Prevent error calling TV:CHAR-MOUSE-P before window system loaded.
(UNLESS (FBOUNDP 'TV:CHAR-MOUSE-P)
  (FSET 'TV:CHAR-MOUSE-P 'IGNORE))

(DEFFORMAT C (:ONE-ARG) FORMAT-CTL-CHARACTER)
(DEFUN FORMAT-CTL-CHARACTER (ARG IGNORE &AUX CHNAME BITS LOWER-CASE)
  (WHEN (EQ (CAR-SAFE ARG) ':MOUSE-BUTTON) (SETQ ARG (CADR ARG)))
  (SETQ ARG (CLI:CHARACTER ARG)
        BITS (CHAR-BITS ARG))
  (FLET ((PRINT-BITS (BITS)
                     (AND (BIT-TEST CHAR-HYPER-BIT BITS)
                          (SEND *STANDARD-OUTPUT* :STRING-OUT "Hyper-"))
                     (AND (BIT-TEST CHAR-SUPER-BIT BITS)
                          (SEND *STANDARD-OUTPUT* :STRING-OUT "Super-"))
                     (AND (BIT-TEST CHAR-CONTROL-BIT BITS)
                          (SEND *STANDARD-OUTPUT* :STRING-OUT "Control-"))
                     (AND (BIT-TEST CHAR-META-BIT BITS)
                          (SEND *STANDARD-OUTPUT* :STRING-OUT "Meta-"))))
    (COND ((TV:CHAR-MOUSE-P ARG)
           (IF (AND (NOT *COLON-FLAG*) *ATSIGN-FLAG*)
               (PRINC "#\\"))
           (PRINT-BITS BITS)
           (SETF (CHAR-BITS ARG) 0)
           (IF (AND (NOT *COLON-FLAG*) *ATSIGN-FLAG*)
               (IF (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
                   (PRINC CHNAME)
                 (FORMAT-ERROR "~O unknown mouse character given to ~~@C" ARG))
             (progn
               (SEND *STANDARD-OUTPUT* :STRING-OUT "Mouse-")
               (if (setq chname (nth (setq bits (LDB %%KBD-MOUSE-BUTTON ARG))
                                     '("Left" "Middle" "Right")))
                   (SEND *STANDARD-OUTPUT* :STRING-OUT chname)
                 (progn
                   (send *standard-output* :string-out "Button:")
                   (prin1 bits)))
               (IF (SETQ CHNAME (nth (SETQ BITS (LDB %%KBD-MOUSE-N-CLICKS ARG))
                                     '("" "-Twice" "-Thrice")))
                   (SEND *STANDARD-OUTPUT* :STRING-OUT CHNAME)
                 (progn
                   (SEND *STANDARD-OUTPUT* :TYO #\-)
                   (ENGLISH-PRINT (1+ BITS))
                   (SEND *STANDARD-OUTPUT* :STRING-OUT "-times"))))))
          ((NOT *COLON-FLAG*)
           ;; If @ flag or if control bits, we want to use characters' names.
           (IF (OR *ATSIGN-FLAG* (NOT (ZEROP BITS)))
               (SETQ CHNAME (FORMAT-GET-CHARACTER-NAME (CHAR-CODE ARG))))
           ;; Print an appropriate reader macro if @C.
           (IF *ATSIGN-FLAG* (PRINC "#\\"))
           (UNLESS (ZEROP BITS)
             (SEND *STANDARD-OUTPUT*
                   :STRING-OUT (AREF #(""     "c-"     "m-"     "c-m-"
                                       "s-"   "c-s-"   "m-s-"   "c-m-s-"
                                       "h-"   "c-h-"   "m-h-"   "c-m-h-"
                                       "s-h-" "c-s-h-" "m-s-h-" "c-m-s-h-")
                                     BITS))
             (IF ( (CHAR-CODE #\a) (SETQ LOWER-CASE (CHAR-CODE ARG)) (CHAR-CODE #\z))
                 (SEND *STANDARD-OUTPUT* :STRING-OUT "sh-")
               (SETQ LOWER-CASE NIL)))
           (COND (CHNAME
                  (SETQ CHNAME (SYMBOL-NAME CHNAME))
                  ;; are we CONSING yet?
                  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (CHAR CHNAME 0)))
                  (DO ((LEN (LENGTH CHNAME))
                       (I 1 (1+ I)))
                      ((= I LEN))
                    (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (CHAR CHNAME I)))))
                 (T (IF *ATSIGN-FLAG*
                        (IF (SI::CHARACTER-NEEDS-QUOTING-P (CHAR-CODE ARG))
                            (SEND *STANDARD-OUTPUT* :TYO (SI::PTTBL-SLASH *READTABLE*)))
                      (IF LOWER-CASE (SETQ ARG (CHAR-UPCASE (INT-CHAR LOWER-CASE)))))
                    (SEND *STANDARD-OUTPUT* :TYO (CHAR-CODE ARG)))))
          (T
           (PRINT-BITS BITS)
           (SETQ ARG (INT-CHAR (CHAR-CODE ARG)))
           (COND ((SETQ CHNAME (FORMAT-GET-CHARACTER-NAME ARG))
                  (SETQ CHNAME (SYMBOL-NAME CHNAME))
                  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE (CHAR CHNAME 0)))
                  (DO ((LEN (LENGTH CHNAME))
                       (I 1 (1+ I)))
                      ((= I LEN))
                    (SEND *STANDARD-OUTPUT* :TYO (CHAR-DOWNCASE (CHAR CHNAME I))))
                  (AND *ATSIGN-FLAG* (FORMAT-PRINT-TOP-CHARACTER ARG)))
                 ((AND *ATSIGN-FLAG* (CHAR< ARG #\SPACE) (CHAR ARG #\))
                  (SEND *STANDARD-OUTPUT* :TYO ARG)
                  (FORMAT-PRINT-TOP-CHARACTER ARG))
                 ((AND (LOWER-CASE-P ARG)
                       (NOT (ZEROP BITS)))
                  (SEND *STANDARD-OUTPUT* :STRING-OUT "Shift-")
                  (SEND *STANDARD-OUTPUT* :TYO (CHAR-UPCASE ARG)))
                 (T (SEND *STANDARD-OUTPUT* :TYO ARG)))))))

(DEFUN FORMAT-GET-CHARACTER-NAME (CHAR)
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (UNLESS (AND (GRAPHIC-CHAR-P CHAR)
               (CHAR CHAR #\SPACE)
               (CHAR CHAR #\))
    (DO ((L SI::XR-SPECIAL-CHARACTER-NAMES (CDR L)))
        ((NULL L) NIL)
;character lossage
      (WHEN (CHAR= (INT-CHAR (CDAR L)) CHAR)
        (RETURN (CAAR L))))))

;character lossage
(DEFUN FORMAT-PRINT-TOP-CHARACTER (CHAR &AUX NAME CHNAME)
  (IF (FIXNUMP CHAR) (SETQ CHAR (INT-CHAR CHAR)))
  (LET ((CODE (CHAR-INT CHAR)))
    (COND ((SETQ CHNAME (DOTIMES (I #o200)
                          (WHEN (= CODE (AREF SI::KBD-NEW-TABLE 2 I))
                            (RETURN (INT-CHAR (AREF SI::KBD-NEW-TABLE 1 I))))))
           (SETQ NAME " (Top-"))
          ((SETQ CHNAME (DOTIMES (I #o200)
                          (WHEN (= CODE (AREF SI::KBD-NEW-TABLE 3 I))
                            (RETURN (INT-CHAR (AREF SI::KBD-NEW-TABLE 0 I))))
                          (WHEN (= CODE (AREF SI::KBD-NEW-TABLE 4 I))
                            (RETURN (INT-CHAR (AREF SI::KBD-NEW-TABLE 1 I))))))
           (SETQ NAME (IF (ALPHA-CHAR-P CHNAME) " (Greek-" " (Front-")))))
  (WHEN (AND CHNAME (NEQ CHNAME CHAR))
    (SEND *STANDARD-OUTPUT* :STRING-OUT NAME)
    ;; I'm not sure what to pass for the second arg, since it is not used.
    ;; It currently doesn't matter.
    (LET ((*ATSIGN-FLAG* NIL))
      (FORMAT-CTL-CHARACTER CHNAME NIL))
    (SEND *STANDARD-OUTPUT* :TYO #\))))

(DEFFORMAT T (:NO-ARG) (PARAMS &AUX (DEST (OR (FIRST PARAMS) 1)) (EXTRA (OR (SECOND PARAMS) 1))
                                    (OPS (SEND *STANDARD-OUTPUT* :WHICH-OPERATIONS))
                                    INCR-OK)
  (COND ((OR (SETQ INCR-OK (MEMQ :INCREMENT-CURSORPOS OPS))
             (MEMQ :SET-CURSORPOS OPS))
         (LET ((FLAVOR (IF *COLON-FLAG* :PIXEL :CHARACTER)))
           (MULTIPLE-VALUE-BIND (X Y) (SEND *STANDARD-OUTPUT* :READ-CURSORPOS FLAVOR)
             (LET ((NEW-X (IF *ATSIGN-FLAG*
                              (IF ( EXTRA 1)
                                  (+ DEST X)
                                  (* (CEILING (+ DEST X) EXTRA) EXTRA))
                            (COND ((< X DEST)
                                   DEST)
                                  ((ZEROP EXTRA)
                                   X)
                                  (T
                                   (+ X EXTRA (- (CL:REM (- X DEST) EXTRA))))))))
               (COND ((= NEW-X X))
                     (INCR-OK
                      ;; Use :INCREMENT-CURSORPOS preferentially
                      ;; because it will do a **MORE** if we need one.
                      (SEND *STANDARD-OUTPUT* :INCREMENT-CURSORPOS (- NEW-X X) 0 FLAVOR))
                     (T
                      (SEND *STANDARD-OUTPUT* :SET-CURSORPOS NEW-X Y FLAVOR)))))))
        (*ATSIGN-FLAG*
         (DOTIMES (I DEST)
           (SEND *STANDARD-OUTPUT* :TYO #\SPACE)))
        (T (SEND *STANDARD-OUTPUT* :STRING-OUT "  "))))

; plural
(DEFFORMAT P (:MULTI-ARG) (ARGS IGNORE)
  (WHEN *COLON-FLAG*
    (LET ((*ATSIGN-FLAG* NIL))
      (SETQ ARGS (FORMAT-CTL-IGNORE ARGS NIL))))
  (IF *ATSIGN-FLAG* (IF (EQUAL (CAR ARGS) 1)
                        (SEND *STANDARD-OUTPUT* :TYO #\y)
                      (SEND *STANDARD-OUTPUT* :STRING-OUT "ies"))
    (OR (EQUAL (CAR ARGS) 1) (SEND *STANDARD-OUTPUT* :TYO #\s)))
  (CDR ARGS))

; skip arguments
(DEFFORMAT * (:MULTI-ARG) FORMAT-CTL-IGNORE)
(DEFUN FORMAT-CTL-IGNORE (ARGS PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
  (COND (*ATSIGN-FLAG*
         (NTHCDR COUNT *FORMAT-ARGLIST*))
        (*COLON-FLAG*
         (DO ((A *FORMAT-ARGLIST* (CDR A))
              (B (NTHCDR COUNT *FORMAT-ARGLIST*) (CDR B)))
             ((NULL A) (FORMAT-ERROR "Can't back up properly for a ~~:*"))
           (AND (EQ B ARGS) (RETURN A))))
        (T (NTHCDR COUNT ARGS))))

;newline
(DEFFORMAT % (:NO-ARG) (PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
  (DOTIMES (I COUNT) (SEND *STANDARD-OUTPUT* :TYO #\NEWLINE)))

;fresh-line
(DEFFORMAT & (:NO-ARG) (PARAMS &AUX (COUNT (OR (CAR PARAMS) 1)))
  (when (plusp count)
    (SEND *STANDARD-OUTPUT* :FRESH-LINE)
    (DOTIMES (I (1- COUNT))
      (SEND *STANDARD-OUTPUT* :TYO #\NEWLINE))))

;(DEFPROP X #\SPACE FORMAT-CTL-REPEAT-CHAR)
(DEFPROP ~ #\~ FORMAT-CTL-REPEAT-CHAR)

(DEFUN FORMAT-CTL-REPEAT-CHAR (COUNT CHAR)
  (DOTIMES (I COUNT)
    (SEND *STANDARD-OUTPUT* :TYO CHAR)))

(DEFFORMAT \| (:NO-ARG) (PARAMS)
  (IF (AND *COLON-FLAG* (OPERATION-HANDLED-P *STANDARD-OUTPUT* :CLEAR-WINDOW))
      (SEND *STANDARD-OUTPUT* :CLEAR-WINDOW)
    (FORMAT-CTL-REPEAT-CHAR (OR (CAR PARAMS) 1) #\FORM)))

(DEFFORMAT Q (:ONE-ARG) (ARG PARAMS)
  (APPLY ARG PARAMS))

;;; Parse a set of clauses separated by ~; and terminated by ~closechar.
;;; (If SEMIP is nil, however, then ~; is ignored.)
;;; Returns an array; G-L-P of this array is a list whose length is a multiple of 3.
;;; Every three elements are <string> <bits> <paramarray>, where <string> is a control
;;; string separated by ~;, <bits> encodes the : and @ flags for the ~; or ~closechar
;;; that followed this string (: = 1, @ = 2), and <paramarray> is () or the parameter array
;;; for that ~; or ~closechar.  The arrays and strings are consed in the temporary area.
;;; FORMAT-RECLAIM-CLAUSES should be used to return the arrays and strings.

(defun format-parse-clauses (closechar semip)
  (let ((start (+ 3 *ctl-index*))
        (clauses (zl:make-array 30. :type art-q-list :fill-pointer 0))
        (stack (zl:make-array 10. :type art-q-list :fill-pointer 0))
        i j tem *atsign-flag* *colon-flag* command)
    (setf (fill-pointer clauses) 0 (fill-pointer stack) 0)
    (setq i *ctl-index*)
    (do-forever
      (unless (setq *ctl-index* (%string-search-char #\~ *ctl-string* *ctl-index* *ctl-length*))
        (ferror ;; Yow!
                "Missing ~{~*~~~A and ~} ~~~A in format string:~%~{~VT~*~}~VT~%~3@T\"~A\"~%"
                (g-l-p stack) closechar (g-l-p stack) start *ctl-string*))
      (setq j *ctl-index*)
      (setq *atsign-flag* nil *colon-flag* nil)
      (let ((*format-params* (allocate-resource 'format-params)))
        (setf (fill-pointer *format-params*) 0)
        (setq command (format-parse-command nil nil))
        ;; Now I points to beginning of clause, J to ~, and *CTL-INDEX* after command.
        (cond ((setq tem (get command 'format-matching-delimiter))
               (vector-push-extend start stack)
               (vector-push-extend closechar stack)
               (setq closechar tem start (+ 3 *ctl-index*)))
              ((< (fill-pointer stack) 2)                       ;at top level
               (when (or (eq command closechar) (and (eq command '\;) semip))
                 ;; SUBSTRING used below because NSUBSTRING would cause pointers to
                 ;; format control string to be retained in an obscure manner.  If
                 ;; someone return-array'ed or otherwise clobbered the control string
                 ;; the GC would die.  KHS 850812.
                 (vector-push-extend (substring *ctl-string* i j)
                                     clauses)
                 (vector-push-extend (+ (if *colon-flag* 1 0) (if *atsign-flag* 2 0))
                                     clauses)
                 (vector-push-extend (when (g-l-p *format-params*)
                                       (prog1 *format-params* (setq *format-params* nil)))
                                     clauses)
                 (setq i *ctl-index*)
                 (when (eq command closechar)
                   (when *format-params* (deallocate-resource 'format-params *format-params*))
                   (return clauses))))
              ((eq command closechar)                           ;pop off a level
               (setq closechar (vector-pop stack))
               (setq start (vector-pop stack))))
        ;; Unless the parameters were saved away in the clauses table, free them
        (if *format-params* (deallocate-resource 'format-params *format-params*))))))

(defun format-reclaim-clauses (clauses)
  (do ((i (fill-pointer clauses) (- i 3)))
      ((= i 0))
    (and (aref clauses (1- i))
         (deallocate-resource 'format-params (aref clauses (1- i))))))

(DEFFORMAT \; (:NO-ARG) (IGNORE)
  (FORMAT-ERROR "Stray ~~; in ~S control string" 'FORMAT))


(defprop case-convert-stream t si:io-stream-p)
(defun case-convert-stream (op &rest args)
  (declare (special *case-converted-stream* *case-convert* *prev-char*))
  (case op
    ((:tyo :write-char)
     (case *case-convert*
       (uppercase (send *case-converted-stream* :tyo (char-upcase (car args))))
       (lowercase (send *case-converted-stream* :tyo (char-downcase (car args))))
       (cap-all-words
        (send *case-converted-stream* :tyo
              (setq *prev-char*
                    (if (alphanumericp *prev-char*)
                        (char-downcase (car args))
                      (char-upcase (car args))))))
       (cap-first-word
        (send *case-converted-stream* :tyo
              (if (alphanumericp *prev-char*)
                  (char-downcase (car args))
                (setq *prev-char*
                      (char-upcase (car args))))))
       (just-first-word
        (send *case-converted-stream* :tyo
              (if (alphanumericp *prev-char*)
                  (car args)
                (setq *prev-char*
                      (char-upcase (car args))))))))
    ((:string-out :line-out)
     (stream-default-handler 'case-convert-stream op (car args) (cdr args)))
    (:which-operations (remove :print (send *case-converted-stream* :which-operations)))
    (t (lexpr-send *case-converted-stream* op args))))

(defprop \( \) format-matching-delimiter)
;>> This is amazingly slow
(defformat \( (:multi-arg) (args params)
  (let ((clauses (format-parse-clauses '\) nil))
        (*case-convert*
          (if (eq (car params) 1)
              'just-first-word
            (if *colon-flag*
                (if *atsign-flag*
                    'uppercase 'cap-all-words)
              (if *atsign-flag*
                  'cap-first-word 'lowercase))))
        (*prev-char* 0)
        (*case-converted-stream*
          (if (variable-boundp *case-convert*) *case-converted-stream* *standard-output*))
        (*standard-output* 'case-convert-stream))
    (declare (special *case-converted-stream* *case-convert* *prev-char*))
    (unwind-protect
        (format-ctl-string args (aref clauses 0))
      (format-reclaim-clauses clauses))))

(defformat \) (:no-arg) (ignore)
  (format-error "Stray ~~) in ~S control string" 'format))

;upcase (note: this is or-sign, not caret
(defformat  (:one-arg) (thing ignore)
  (let ((*downcase-flag* *atsign-flag*)
        (*once-only-flag* *colon-flag*)
        (*old-stream* *standard-output*))
    (declare (special *downcase-flag* *once-only-flag* *old-stream*))
    (princ thing 'upcase-stream)))

(defprop upcase-stream t si:io-stream-p)
(defun upcase-stream (op &optional arg1 &rest rest)
  (declare (special *downcase-flag* *once-only-flag* *old-stream*))
  (case op
    (:which-operations
     '(:write-char :tyo :write-char))
    ((:tyo :write-char)
     (if *downcase-flag*
         (if (upper-case-p arg1) (setq arg1 (char-downcase arg1)))
         (if (lower-case-p arg1) (setq arg1 (char-upcase arg1))))
     (if *once-only-flag*
         (setq *once-only-flag* nil *downcase-flag* (not *downcase-flag*)))
     (send *old-stream* :tyo arg1))
    (otherwise
     (stream-default-handler 'upcase-stream op arg1 rest))))

(defvar indent-convert nil)
(defvar indent-converted-stream nil)

(defun indent-convert-stream (op &rest args)
  (case op
    ((:tyo :write-char)
     (send indent-converted-stream :tyo (car args))
;character lossage
     (when (char= (car args) #\newline)
       (dotimes (i indent-convert)
         (write-char #\space indent-converted-stream))))
    (:fresh-line
     (send indent-converted-stream :tyo #\newline)
     (dotimes (i indent-convert)
       (write-char #\space indent-converted-stream)))
    ((:string-out :line-out)
     (stream-default-handler 'indent-convert-stream op (car args) (cdr args)))
    (:which-operations (remove :print (send indent-converted-stream :which-operations)))
    (t (lexpr-send indent-converted-stream op args))))

(defprop   format-matching-delimiter)
(defformat  (:multi-arg) (args params)
  (let ((clauses (format-parse-clauses ' nil))
        (indent-convert (or (car params)
                            (send-if-handles *standard-output* :read-cursorpos :character)
                            0))
        (indent-converted-stream
          (if indent-convert indent-converted-stream *standard-output*))
        (*standard-output* 'indent-convert-stream))
    (unwind-protect
        (format-ctl-string args (aref clauses 0))
      (format-reclaim-clauses clauses))))
(defformat  (:no-arg) (ignore)
  (format-error "Stray ~~ in ~S control string" 'format))

(DEFPROP [ ] FORMAT-MATCHING-DELIMITER)
(DEFFORMAT [ (:MULTI-ARG) (ARGS PARAMS &AUX (ARG (CAR ARGS)))
  (COND (*COLON-FLAG*
         (cond (*ATSIGN-FLAG*
                (FORMAT-ERROR "~~:@[ is not a defined ~S command" 'FORMAT))
               (t
                (SETQ ARG (IF ARG 1 0))
                (POP ARGS))))
        (*ATSIGN-FLAG* (SETQ ARG (cond (ARG
                                        0)
                                       (t
                                        (POP ARGS)
                                        -1))))
        ((CAR PARAMS) (SETQ ARG (CAR PARAMS)))
        (T (POP ARGS)))
  (OR (NUMBERP ARG)
      (FORMAT-ERROR "The argument to the ~S \"~~[\" command must be a number" 'FORMAT))
  (LET ((START *CTL-INDEX*)                            ;for error message only
        (CLAUSES (FORMAT-PARSE-CLAUSES '] T)))
    (DO ((L (G-L-P CLAUSES) (CDDDR L))
         (STATE (AND (NOT (ZEROP (LENGTH (CAR (G-L-P CLAUSES))))) 'SIMPLE)))
        ((NULL (CDDDR L))
         (LET ((STRING (IF (EQ STATE 'HAIRY)
                           (DO ((Z (G-L-P CLAUSES) (CDDDR Z)))
                               ((NULL (CDDDR Z)) NIL)
                             (AND (COND ((NULL (CADDR Z)) T)
                                        ((ODDP (CADR Z))
                                         (DO ((Q (G-L-P (CADDR Z)) (CDDR Q)))
                                             ((NULL Q) NIL)
                                           (AND (OR (NULL (CAR Q)) (NOT (< ARG (CAR Q))))
                                                (OR (NULL (CADR Q)) (NOT (> ARG (CADR Q))))
                                                (RETURN T))))
                                        (T (MEMQ ARG (G-L-P (CADDR Z)))))
                                  (RETURN (CADDDR Z))))
                           (DO ((Z (G-L-P CLAUSES) (CDDDR Z))
                                (A ARG (1- A)))
                               ((NULL Z) NIL)
                             (AND (ZEROP A) (RETURN (CAR Z)))
                             (AND (ODDP (CADR Z))
                                  (NOT (NULL (CDDDR Z)))
                                  (RETURN (CADDDR Z)))))))
           (LET ((NEWARGS (IF STRING
                              (FORMAT-CTL-STRING ARGS STRING)
                              ARGS)))
             (FORMAT-RECLAIM-CLAUSES CLAUSES)
             NEWARGS)))
      (COND ((NOT (NULL (CADDR L)))
             (WHEN (EQ STATE 'SIMPLE)
               (SETQ *CTL-INDEX* START)
               (FORMAT-ERROR "Mixture of simple and tagged clauses in ~~["))
             (SETQ STATE 'HAIRY))
            ((NOT (ODDP (CADR L)))
             (WHEN (EQ STATE 'HAIRY)
               (SETQ *CTL-INDEX* START)
               (FORMAT-ERROR "Mixture of simple and tagged clauses in ~~["))
             (SETQ STATE 'SIMPLE))))))
(DEFFORMAT ] (:NO-ARG) (IGNORE)
  (FORMAT-ERROR "Stray ~~] in ~S control string" 'FORMAT))

; not: this is caret, not or-sign
(DEFFORMAT ^ (:MULTI-ARG) (ARGS PARAMS)
  (AND (IF (CAR PARAMS)
           (IF (CADR PARAMS)
               (IF (CADDR PARAMS)
                   (AND (NOT (> (CAR PARAMS) (CADR PARAMS)))
                        (NOT (> (CADDR PARAMS) (CADR PARAMS))))
                 (= (CAR PARAMS) (CADR PARAMS)))
             (ZEROP (CAR PARAMS)))
         (NULL (IF *COLON-FLAG* *LOOP-ARGLIST* ARGS)))
       (THROW (IF *COLON-FLAG* 'FORMAT-\:^-POINT 'FORMAT-^-POINT) NIL))
  ARGS)

(defprop { } format-matching-delimiter)
(DEFFORMAT { (:MULTI-ARG) (ARGS PARAMS)
  (LET ((LIMIT (OR (FIRST PARAMS) -1))
        (CLAUSES (FORMAT-PARSE-CLAUSES '} NIL)))
    (OR (NULL (CDDDR (G-L-P CLAUSES))) (FORMAT-ERROR "Bug in ~S's \"~{\" processor" 'FORMAT))
    (LET ((STR (CAR (G-L-P CLAUSES))))
      (AND (ZEROP (LENGTH STR))
           (OR (STRINGP (SETQ STR (POP ARGS)))
               (FORMAT-ERROR "~~{~~} argument not a string")))
      (LET ((*LOOP-ARGLIST* (IF *ATSIGN-FLAG* ARGS (CAR ARGS))))
        (CATCH 'FORMAT-\:^-POINT
          (CATCH 'FORMAT-^-POINT
            (DO ((OKAY-TO-EXIT (NOT (ODDP (CADR (G-L-P CLAUSES)))) T))
                ((OR (AND OKAY-TO-EXIT (NULL *LOOP-ARGLIST*)) (= LIMIT 0)))
              (IF (NOT *COLON-FLAG*)
                  (LET ((*FORMAT-ARGLIST* *LOOP-ARGLIST*))
                    (SETQ *LOOP-ARGLIST* (FORMAT-CTL-STRING *LOOP-ARGLIST* STR)))
                  (LET ((*FORMAT-ARGLIST* (POP *LOOP-ARGLIST*)))
                    (CATCH 'FORMAT-^-POINT
                      (FORMAT-CTL-STRING *FORMAT-ARGLIST* STR))))
              (SETQ LIMIT (1- LIMIT)))))
        (FORMAT-RECLAIM-CLAUSES CLAUSES)
        (IF *ATSIGN-FLAG* *LOOP-ARGLIST* (CDR ARGS))))))
(DEFFORMAT } (:NO-ARG) (IGNORE)
  (FORMAT-ERROR "Stray ~~} in ~S control string" 'FORMAT))

(DEFFORMAT ? (:MULTI-ARG) (ARGS IGNORE)
  (LET ((STR (POP ARGS)))
    (LET ((*LOOP-ARGLIST* (IF *ATSIGN-FLAG* ARGS (CAR ARGS))))
      (CATCH 'FORMAT-\:^-POINT
        (CATCH 'FORMAT-^-POINT
          (LET ((*FORMAT-ARGLIST* *LOOP-ARGLIST*))
            (SETQ *LOOP-ARGLIST* (FORMAT-CTL-STRING *LOOP-ARGLIST* STR)))))
      (IF *ATSIGN-FLAG* *LOOP-ARGLIST* (CDR ARGS)))))

;;; This function is like FORMAT-CTL-STRING except that instead of sending to
;;; *STANDARD-OUTPUT* it sends to a string and returns that as its second value.
;;; The returned string is in the temporary area.
(DEFUN FORMAT-CTL-STRING-TO-STRING (ARGS STR)
  (LET ((*FORMAT-STRING* (MAKE-FORMAT-STRING))
        (*STANDARD-OUTPUT* 'FORMAT-STRING-STREAM))
    (VALUES (FORMAT-CTL-STRING ARGS STR)
            (ADJUST-ARRAY-SIZE *FORMAT-STRING* (LENGTH *FORMAT-STRING*)))))

;;; This is not so hairy as to work with ~T, tabs, crs.  I really don't see how to do that.
;;; It makes a list of strings, then decides how much spacing to put in,
;;; then goes back and outputs.
(defprop < > format-matching-delimiter)
(DEFFORMAT < (:MULTI-ARG) (ARGS PARAMS)
  (LET ((MINCOL (OR (FIRST PARAMS) 0))
        (COLINC (OR (SECOND PARAMS) 1))
        (MINPAD (OR (THIRD PARAMS) 0))
        (PADCHAR (OR (FOURTH PARAMS) #\SPACE))
        (W-O (SEND *STANDARD-OUTPUT* :WHICH-OPERATIONS))
        (NEWLINE NIL)
        (EXTRA 0)
        (LINEWIDTH NIL)
        (STRINGS NIL)
        (STRING-NCOL 0)
        (CLAUSES)
        (N-PADDING-POINTS -1)
        (TOTAL-PADDING)
        (N-PADS)
        (N-EXTRA-PADS))
    (AND *COLON-FLAG* (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS)))
    (AND *ATSIGN-FLAG* (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS)))
    (CATCH 'FORMAT-^-POINT
      (PROGN (SETQ CLAUSES (FORMAT-PARSE-CLAUSES '> T))
             (DO ((SPECS (G-L-P CLAUSES) (CDDDR SPECS)) (STR))
                 ((NULL SPECS))
               (MULTIPLE-VALUE-SETQ (ARGS STR) (FORMAT-CTL-STRING-TO-STRING ARGS (CAR SPECS)))
               (SETQ STRING-NCOL (+ (LENGTH STR) STRING-NCOL))
               (SETQ N-PADDING-POINTS (1+ N-PADDING-POINTS))
               (SETQ STRINGS (CONS STR STRINGS)))))
    (SETQ STRINGS (NREVERSE STRINGS))
    (WHEN (AND (G-L-P CLAUSES) (ODDP (CADR (G-L-P CLAUSES))))
      (SETQ NEWLINE (POP STRINGS))
      (AND (CADDR (G-L-P CLAUSES))
           (SETQ EXTRA (OR (CAR (G-L-P (CADDR (G-L-P CLAUSES)))) 0)
                 LINEWIDTH (CADR (G-L-P (CADDR (G-L-P CLAUSES))))))
      (SETQ STRING-NCOL (- STRING-NCOL (LENGTH NEWLINE)))
      (SETQ N-PADDING-POINTS (1- N-PADDING-POINTS)))
    (WHEN (ZEROP N-PADDING-POINTS)      ;With no options and no ~; right-justify
      (SETQ *COLON-FLAG* T N-PADDING-POINTS 1))
    ;; Get the amount of space needed to print the strings and MINPAD padding
    (SETQ TOTAL-PADDING (+ (* N-PADDING-POINTS MINPAD) STRING-NCOL))
    ;; Now bring in the MINCOL and COLINC constraint, i.e. the total width is
    ;; at least MINCOL and exceeds MINCOL by a multiple of COLINC, and
    ;; get the total amount of padding to be divided among the padding points
    (SETQ TOTAL-PADDING (- (+ MINCOL (* COLINC (CEILING (MAX (- TOTAL-PADDING MINCOL) 0)
                                                        COLINC)))
                           STRING-NCOL))
    ;; Figure out whether a newline is called for or not.
    (WHEN (AND NEWLINE
               (MEMQ :READ-CURSORPOS W-O)
               (> (+ (SEND *STANDARD-OUTPUT* :READ-CURSORPOS :CHARACTER)
                     STRING-NCOL TOTAL-PADDING EXTRA)
                  (OR LINEWIDTH
                      (AND (MEMQ :SIZE-IN-CHARACTERS W-O)
                           (SEND *STANDARD-OUTPUT* :SIZE-IN-CHARACTERS))
                      72.)))                           ;This is what commonlisp specifies
      (SEND *STANDARD-OUTPUT* :STRING-OUT NEWLINE))
    ;; Decide how many pads at each padding point + how many of the leftmost
    ;; padding points need one extra pad.
    (SETF (VALUES N-PADS N-EXTRA-PADS) (FLOOR TOTAL-PADDING N-PADDING-POINTS))
    (OR (ZEROP N-EXTRA-PADS) (SETQ N-PADS (1+ N-PADS)))
    ;; Output the stuff
    (DO ((STRINGS STRINGS (CDR STRINGS))
         (PAD-BEFORE-P *COLON-FLAG* T))
        ((NULL STRINGS))
      (WHEN PAD-BEFORE-P
        (FORMAT-CTL-REPEAT-CHAR N-PADS PADCHAR)
        (AND (ZEROP (SETQ N-EXTRA-PADS (1- N-EXTRA-PADS))) (SETQ N-PADS (1- N-PADS))))
      (SEND *STANDARD-OUTPUT* :STRING-OUT (CAR STRINGS)))
    ;; Finally spacing at the right
    (AND *ATSIGN-FLAG* (FORMAT-CTL-REPEAT-CHAR N-PADS PADCHAR))
    ;; Reclamation
;    (DOLIST (STR (NREVERSE STRINGS))
;      (RETURN-ARRAY (PROG1 STR (SETQ STR NIL))))
;    (AND NEWLINE (RETURN-ARRAY (PROG1 NEWLINE (SETQ NEWLINE NIL))))
    (FORMAT-RECLAIM-CLAUSES CLAUSES)
    ARGS))
(DEFFORMAT > (:NO-ARG) (IGNORE)
  (FORMAT-ERROR "Stray ~~> in ~S control string") 'FORMAT)

;;; Less messy interface to list-printing stuff -- but it conses
(DEFUN PRINT-LIST (DESTINATION ELEMENT-FORMAT-STRING LIST
                   &OPTIONAL (SEPARATOR-FORMAT-STRING ", ")
                             (START-LINE-FORMAT-STRING "   ")
                             (TILDE-BRACE-OPTIONS ""))
  "Print the elements of list without lapping across line boundaries"
  (LET ((FSTRING (FORMAT NIL "~~~A{~~<~~%~A~~~D:;~A~~>~~^~A~~}"
                         TILDE-BRACE-OPTIONS
                         START-LINE-FORMAT-STRING
                         (LENGTH SEPARATOR-FORMAT-STRING)
                         ELEMENT-FORMAT-STRING
                         SEPARATOR-FORMAT-STRING)))
    (FORMAT DESTINATION FSTRING LIST)))

(DEFFORMAT TIME-INTERVAL (:ONE-ARG) (INTERVAL IGNORE)
  (TIME:PRINT-INTERVAL-OR-NEVER INTERVAL))

(DEFFORMAT DATIME (:NO-ARG) (IGNORE)
  (TIME:PRINT-CURRENT-TIME))

(DEFFORMAT zl:TIME (:ONE-ARG) (UT IGNORE)
  (TIME:PRINT-UNIVERSAL-TIME UT))

(DEFFORMAT DATE (:ONE-ARG) (UT IGNORE)
  (TIME:PRINT-UNIVERSAL-DATE UT))

(DEFFORMAT SCIENTIFIC (:ONE-ARG) (X IGNORE)
  (MULTIPLE-VALUE-BIND (NUMBER POWER NAME)
      (SCIENTIFIC-NUMBER X)
    (COND ((NOT NAME)
           (FORMAT T "~$*10^~D " NUMBER POWER))
          ('ELSE
           (FORMAT T "~$ ~A" NUMBER NAME)))))

#|
Test for scientific-number:

(loop for i from -18 to 12
      do (format t "~%~14e ~:*~14f ~:*~\\scientific\\" (* 1.2 (^ 10.0 i))))
|#

(DEFVAR *SCIENTIFIC-NUMBER-POWER-TABLE*
        '((0 "")
          (-3 "milli")
          (-6 "micro")
          (-9 "nano")
          (-12 "pico")
          (-15 "femto")
          (-18 "atto")
          (3 "kilo")
          (6 "mega")
          (9 "giga")
          (12 "tera")))

(DEFUN SCIENTIFIC-NUMBER (X)
  "return a number 1<ABS(N)<1000, and power of ten, and a power name such as `mega'"
  (DECLARE (VALUES NUMBER POWER NAME))
  (COND ((ZEROP X)
         (VALUES 0 0 ""))
        ('ELSE
         (LET ((POW (TIMES 3 (FLOOR (LOG (ABS X) 10) 3))))
           (VALUES (TIMES X (EXPT 10.0 (- POW)))
                   POW
                   (CADR (ASSQ POW *SCIENTIFIC-NUMBER-POWER-TABLE*)))))))
