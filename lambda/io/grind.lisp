;;; -*- Mode:LISP; Package:SI; Readtable:ZL; Base:8 -*-
;;; EXTREMELY Simple-minded, tasteful(?!) Grind
;;;     ** (c) Copyright 1980 Massachusetts Institute of Technology **

;>> knows nothing about common-lisp printer flags, esp *print-circle*

;;; Specials

(DEFVAR GRIND-IO)               ;Stream for output.
(DEFVAR GRIND-REAL-IO)          ;Stream which GRIND-PRINT-IO calls.
(DEFVAR GRIND-UNTYO-P)          ;T if that stream can do :UNTYO.
(DEFVAR GRIND-WIDTH)            ;Width to attempt to print within.
(DEFVAR GRIND-INDENT)           ;Current indentation for lines.
(DEFVAR GRIND-HPOS)             ;Current hpos (or next char to be printed).
(DEFVAR GRIND-VPOS)             ;Current vpos.
(DEFVAR GRIND-DEPTH)            ;Current depth in list structure.

;When DISPLACED should be checked for, the value of this variable
;is DISPLACED.  Otherwise, the value is taken from GRIND-DUMMY-DISPLACED
(DEFVAR GRIND-DISPLACED 'DISPLACED)
(DEFVAR GRIND-DUMMY-DISPLACED (NCONS NIL))

(DEFVAR GRIND-NOTIFY-FUN)       ;Function to tell about interesting characters

(DEFVAR GRIND-RENAMING-ALIST NIL);Alist of renamings that were performed
                                ;on the function being ground.
                                ;We should undo the renamings when we grind
                                ;so that the code comes out as originally written.
                                ;Each element of this alist is (original-name new-name).

(DEFPROP DEFSELECT GRIND-DEFSELECT GRIND-MACRO)
(DEFPROP QUOTE GRIND-QUOTE GRIND-MACRO)
(DEFPROP FUNCTION GRIND-FUNCTION GRIND-MACRO)
(DEFPROP DEFUN GRIND-DEFUN GRIND-MACRO)
(DEFPROP DEFSUBST GRIND-DEFUN GRIND-MACRO)
(DEFPROP MACRO GRIND-DEFUN GRIND-MACRO)
(DEFPROP DEFMACRO GRIND-DEFUN GRIND-MACRO)
(DEFPROP DEFMETHOD GRIND-DEFUN GRIND-MACRO)
(DEFPROP LAMBDA GRIND-LAMBDA GRIND-MACRO)
(DEFPROP NAMED-LAMBDA GRIND-NAMED-LAMBDA GRIND-MACRO)
(DEFPROP SUBST GRIND-LAMBDA GRIND-MACRO)
(DEFPROP NAMED-SUBST GRIND-NAMED-LAMBDA GRIND-MACRO)
(DEFPROP PROG GRIND-PROG GRIND-MACRO)
(DEFPROP PROG* GRIND-PROG GRIND-MACRO)
(DEFPROP PROGV GRIND-PROGV GRIND-MACRO)
(DEFPROP PROGW GRIND-PROGW GRIND-MACRO)
(DEFPROP DO GRIND-DO GRIND-MACRO)
(DEFPROP DO* GRIND-DO GRIND-MACRO)
(DEFPROP DO-NAMED GRIND-DO-NAMED GRIND-MACRO)
(DEFPROP DO*-NAMED GRIND-DO-NAMED GRIND-MACRO)
(DEFPROP COND GRIND-COND GRIND-MACRO)
(DEFPROP SETQ GRIND-SETQ GRIND-MACRO)
(DEFPROP PSETQ GRIND-SETQ GRIND-MACRO)
(DEFPROP AND GRIND-AND GRIND-MACRO)
(DEFPROP OR GRIND-AND GRIND-MACRO)
(DEFPROP LAMBDA GRIND-LAMBDA-COMPOSITION GRIND-L-MACRO)
(DEFPROP LET GRIND-LET GRIND-MACRO)
(DEFPROP LET* GRIND-LET GRIND-MACRO)
(DEFPROP COMPILER-LET GRIND-COMPILER-LET GRIND-MACRO)
(DEFPROP TRACE GRIND-TRACE GRIND-MACRO)

(DEFPROP CL:SUBST GRIND-LAMBDA GRIND-MACRO)

;;; Macro to typeout a constant character

(DEFMACRO GTYO (CH &OPTIONAL NOTIFY)
  `(PROGN
     ,(AND NOTIFY `(AND GRIND-NOTIFY-FUN
                        (NEQ GRIND-IO #'GRIND-COUNT-IO)
                        (FUNCALL GRIND-NOTIFY-FUN ,CH ,NOTIFY NIL)))
     (SEND GRIND-IO ':TYO ,CH)))

(DEFMACRO GTYO-OPEN (&OPTIONAL NOTIFY)
  `(GTYO (PTTBL-OPEN-PAREN *READTABLE*) ,NOTIFY))

(DEFMACRO GTYO-CLOSE (&OPTIONAL NOTIFY)
  `(GTYO (PTTBL-CLOSE-PAREN *READTABLE*) ,NOTIFY))

(DEFMACRO GTYO-SPACE (&OPTIONAL NOTIFY)
  `(GTYO (PTTBL-SPACE *READTABLE*) ,NOTIFY))

(DEFUN GSTRING (STRING &OPTIONAL NOTIFY)
  (WHEN (AND NOTIFY GRIND-NOTIFY-FUN (NEQ GRIND-IO #'GRIND-COUNT-IO))
    (FUNCALL GRIND-NOTIFY-FUN STRING NOTIFY NIL))
  (SEND GRIND-IO ':STRING-OUT STRING))

;;; Macro to do something with indentation bound to current HPOS.

(DEFMACRO GIND BODY
  `((LAMBDA (GRIND-INDENT GRIND-DEPTH)
      ,@BODY)
    GRIND-HPOS (1+ GRIND-DEPTH)))

;;; Macro to do something and not check for DISPLACED in it
;;; (because it is quoted list structure, etc., not evaluated).

(DEFMACRO GRIND-QUOTED BODY
  `((LAMBDA (GRIND-DISPLACED) ,@BODY)
    GRIND-DUMMY-DISPLACED))

;;; CRLF then indent to GRIND-INDENT

(DEFUN GRIND-TERPRI ()
  (COND ((EQ GRIND-IO (FUNCTION GRIND-COUNT-IO))
         (SETQ GRIND-VPOS (1+ GRIND-VPOS)
               GRIND-HPOS GRIND-INDENT))
        (T
         (GTYO #/CR)
         (DO ((I GRIND-INDENT (1- I))) ((ZEROP I))
           (GTYO-SPACE)))))

;;; I/O stream which counts VPOS and HPOS, and THROWs to GRIND-DOESNT-FIT-CATCH
;;; if the width overflows.
;;; For now at least, doesn't hack tabs and backspaces and font changes and cetera.

;; Bind this to non-NIL tells streams it is OK to throw to GRIND-DOESNT-FIT-CATCH
;; Doing it this way avoids the incredible slowness of CATCH-ERROR and THROW's interaction.
(DEFVAR GRIND-DOESNT-FIT-CATCH NIL)

(DEFMACRO CATCH-IF-DOESNT-FIT (&BODY BODY)
  `(LET ((GRIND-DOESNT-FIT-CATCH T))
     (*CATCH 'GRIND-DOESNT-FIT-CATCH . ,BODY)))

(DEFPROP GRIND-COUNT-IO T IO-STREAM-P)

(DEFUN GRIND-COUNT-IO (OPERATION &OPTIONAL ARG1 &REST REST)
  (COND ((EQ OPERATION ':WHICH-OPERATIONS) '(:TYO))
        ((NEQ OPERATION ':TYO)
         (STREAM-DEFAULT-HANDLER #'GRIND-COUNT-IO OPERATION ARG1 REST))
        ((= ARG1 #/CR)
         (SETQ GRIND-VPOS (1+ GRIND-VPOS) GRIND-HPOS 0))
        ((AND GRIND-DOESNT-FIT-CATCH
              ( GRIND-HPOS GRIND-WIDTH))                       ;Line overflow
         (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))
        (T (SETQ GRIND-HPOS (1+ GRIND-HPOS)))))

;;; I/O stream which counts VPOS and HPOS and prints (to GRIND-REAL-IO).
;;; This has to do the throw if width overflows also, for untyoable GRIND-REAL-IOs.

(DEFPROP GRIND-PRINT-IO T IO-STREAM-P)

(DEFUN GRIND-PRINT-IO (OPERATION &OPTIONAL &REST REST)
  (COND ((EQ OPERATION ':WHICH-OPERATIONS) '(:TYO))
        ((NEQ OPERATION ':TYO)
         (STREAM-DEFAULT-HANDLER #'GRIND-PRINT-IO OPERATION
                                 (CAR REST) (CDR REST)))
        (T (COND ((= (CAR REST) #/CR)
                  (SETQ GRIND-VPOS (1+ GRIND-VPOS) GRIND-HPOS 0))
                 ((AND GRIND-DOESNT-FIT-CATCH
                       (>= GRIND-HPOS GRIND-WIDTH))     ;Line overflow
                  (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))
                 (T (SETQ GRIND-HPOS (1+ GRIND-HPOS))))
           (LEXPR-FUNCALL GRIND-REAL-IO OPERATION REST))))

(DEFUN GRIND-THROW-ERROR (&REST IGNORE)
  (*THROW 'THROW-ERROR-CATCH NIL))

;;;Use this to print normally when there's nothing better.
(defmacro grind-punt (exp stream)
  `(let ((*print-pretty* nil))
     (if *print-escape*
         (prin1 ,exp ,stream)
       (princ ,exp ,stream))))

(DEFUN GRIND-ATOM (ATOM STREAM LOC)
  (AND GRIND-RENAMING-ALIST
       (DOLIST (ELT GRIND-RENAMING-ALIST)
         (AND (EQ (CADR ELT) ATOM)
              (RETURN (SETQ ATOM (CAR ELT))))))
  (AND GRIND-NOTIFY-FUN
       (NEQ STREAM #'GRIND-COUNT-IO)
       (FUNCALL GRIND-NOTIFY-FUN ATOM LOC T))
  (IF (ARRAYP ATOM)
      (COND ((NAMED-STRUCTURE-P ATOM)
             (IF (MEMQ ':PRINT-SELF (NAMED-STRUCTURE-INVOKE ':WHICH-OPERATIONS ATOM))
                 (NAMED-STRUCTURE-INVOKE ':PRINT-SELF ATOM STREAM GRIND-DEPTH *PRINT-ESCAPE*)
               ;;;;;;;;;;;;;;;;;;;; temporary
               (PRINT-NAMED-STRUCTURE (NAMED-STRUCTURE-P ATOM) ATOM GRIND-DEPTH STREAM)))
            ((AND *PRINT-ARRAY*
                  (NOT (AND (= (ARRAY-RANK ATOM) 1)
                            (OR (STRINGP ATOM)
                                (EQ (ARRAY-TYPE ATOM) 'ART-1B))))
                  (GRIND-ARRAY ATOM LOC)))
            (T (IF *PRINT-ESCAPE* (PRIN1 ATOM STREAM) (PRINC ATOM STREAM))))
    (IF *PRINT-ESCAPE* (PRIN1 ATOM STREAM) (PRINC ATOM STREAM))))

(DEFUN GRIND-ARRAY (EXP LOC)
  ;; +++ This code must have worked once upon a time, but
  ;;     now it causes an infinite loop in GRIND-PRINT-IO.
  ;;     <17-Nov-88 keith>
  #+never
  (IF (= (ARRAY-RANK EXP) 1)
      (IF (EQ (ARRAY-TYPE EXP) 'ART-1B)
          (GRIND-ATOM EXP GRIND-IO LOC)
        (GRIND-AS-BLOCK (LISTARRAY EXP) NIL
                        (CAR (PTTBL-VECTOR *READTABLE*))
                        (CDR (PTTBL-VECTOR *READTABLE*))))
    (DOLIST (ELT (PTTBL-ARRAY *READTABLE*))
      (COND ((STRINGP ELT)
             (GSTRING ELT))
            ((EQ ELT ':RANK)
             (LET ((*PRINT-BASE* 10.) (*PRINT-RADIX* NIL) (*NOPOINT T))
               (GRIND-ATOM (ARRAY-RANK EXP) GRIND-IO NIL)))
            ((EQ ELT ':SEQUENCES)
             (OR (GRIND-TRY 'GRIND-ARRAY-CONTENTS
                            EXP 0 0 T)
                 (GRIND-ARRAY-CONTENTS EXP 0 0))))))
  ;;;  $$$ Just PRINT arrays until above code can be fixed. <17-Nov-88 keith>
  (ignore loc)
  (grind-punt exp grind-io))

(DEFUN GRIND-ARRAY-CONTENTS (ARRAY DIMENSION INDEX-SO-FAR &OPTIONAL LINEAR)
  (IF (AND *PRINT-LEVEL* (>= GRIND-DEPTH *PRINT-LEVEL*))
      (GRIND-ATOM (PTTBL-PRINLEVEL *READTABLE*) GRIND-IO NIL)
    (IF (ZEROP (ARRAY-RANK ARRAY))
        (LET ((ELT (AREF ARRAY))
              (ELTLOC (ALOC ARRAY)))
          (COND (LINEAR
                 (GTYO-SPACE)
                 (GRIND-LINEAR-FORM ELT ELTLOC))
                (T                              ;Won't fit, start another line
                 (GRIND-STANDARD-FORM ELT ELTLOC))))
      (GTYO-OPEN T)
      (GIND
        (LET ((INDEX (* INDEX-SO-FAR (ARRAY-DIMENSION ARRAY DIMENSION)))
              (FRESHLINEP T)
              VP)
          (DOTIMES (I (ARRAY-DIMENSION ARRAY DIMENSION))
            (UNLESS (ZEROP I)
              (COND ((AND (= VP GRIND-VPOS)     ;if still on same line, need a space
                          (< GRIND-HPOS GRIND-WIDTH))   ;unless at end of line
                     (GTYO-SPACE)
                     (SETQ FRESHLINEP NIL))
                    (T                          ;If this was moby, don't put
                     (GRIND-TERPRI)             ; anything else on the same line
                     (SETQ FRESHLINEP T))))
            (SETQ VP GRIND-VPOS)
            (COND ((AND *PRINT-LENGTH* (= I *PRINT-LENGTH*))
                   (GRIND-ATOM (PTTBL-PRINLENGTH *READTABLE*) GRIND-IO NIL)
                   (RETURN))
                  ((= (1+ DIMENSION) (ARRAY-RANK ARRAY))
                   (LET ((ELT (AR-1-FORCE ARRAY (+ INDEX I)))
                         (ELTLOC (AR-1-FORCE ARRAY (+ INDEX I))))
                     (COND (LINEAR (GRIND-LINEAR-FORM ELT ELTLOC))
                           ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC))
                           ((AND FRESHLINEP
                                 (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) ELT ELTLOC)))
                           (T                   ;Won't fit, start another line
                            (OR FRESHLINEP (GRIND-TERPRI))
                            (SETQ VP GRIND-VPOS)
                            (OR (GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC)
                                (GRIND-STANDARD-FORM ELT ELTLOC))))))
                  ((AND *PRINT-LEVEL*
                        (>= GRIND-DEPTH *PRINT-LEVEL*))
                   (GRIND-ATOM (PTTBL-PRINLEVEL *READTABLE*) GRIND-IO NIL))
                  (LINEAR
                   (GRIND-ARRAY-CONTENTS ARRAY (1+ DIMENSION) (+ INDEX I)) T)
                  (T
                   ;; Always start each row on a new line.
                   (UNLESS (OR FRESHLINEP LINEAR (ZEROP I))
                     (GRIND-TERPRI))
                   (OR (GRIND-TRY 'GRIND-ARRAY-CONTENTS
                                  ARRAY (1+ DIMENSION) (+ INDEX I) T)
                       (AND (NOT FRESHLINEP)
                            (PROGN (GRIND-TERPRI)
                                   (GRIND-TRY 'GRIND-ARRAY-CONTENTS
                                              ARRAY (1+ DIMENSION) (+ INDEX I))))
                       (GRIND-ARRAY-CONTENTS ARRAY (1+ DIMENSION) (+ INDEX I))))))))
      (GTYO-CLOSE T))))

;;;; Basic Grinding Forms

;Grind an expression all on one line
;************ THE RIGHT WAY TO DO THIS IS IF A CRLF TRIES TO ************
;************ COME OUT IN LINEAR MODE THROW BACK TO THE TOPMOST *********
;************ INSTANCE OF LINEAR MODE.  THEN CALL MACROS SAME ***********
;************ AS IN GRIND-FORM.     -- dam                   ************

;;; Note that LOC is a locative to the thing being ground, and is used
;;; so that it is possible to replace the thing being printed under
;;; program control.  This is currently used by the Inspector
(DEFUN GRIND-LINEAR-FORM (EXP LOC &OPTIONAL (CHECK-FOR-MACROS T) &AUX TEM)
  (COND ((ATOM EXP)                                     ;Atoms print very simply
         (GRIND-ATOM EXP GRIND-IO LOC))
        ((AND PRINLEVEL ( GRIND-DEPTH PRINLEVEL))
         (GRIND-ATOM (PTTBL-PRINLEVEL *READTABLE*) GRIND-IO LOC))
        ;; Prevent errors taking CADR below.
        ((ATOM (CDR EXP))
         (GRIND-LINEAR-TAIL EXP LOC))
        ((MEMQ (CAR EXP) '(GRIND-COMMA GRIND-COMMA-ATSIGN GRIND-COMMA-DOT GRIND-DOT-COMMA))
         (SELECTQ (CAR EXP)
           (GRIND-COMMA (GTYO #/,))
           (GRIND-COMMA-ATSIGN (GTYO #/,) (GTYO #/@))
           (GRIND-COMMA-DOT (GTYO #/,) (GTYO #/.))
           (GRIND-DOT-COMMA (GTYO #/.) (GTYO-SPACE) (GTYO #/,)))
         (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP))))
        ((AND CHECK-FOR-MACROS
              (OR (AND (SYMBOLP (CAR EXP))                      ;Check for GRIND-MACRO
                       (NOT (EQ (CAR EXP) 'QUOTE))              ;(KLUDGE)
                       (NOT (EQ (CAR EXP) 'FUNCTION))           ;(KLUDGE)
                       (SETQ TEM (GET (CAR EXP) 'GRIND-MACRO)))
                  (AND (CONSP (CAR EXP))                        ;Check for LAMBDA
                       (SYMBOLP (CAAR EXP))
                       (SETQ TEM (GET (CAAR EXP) 'GRIND-L-MACRO)))))
         (*THROW 'GRIND-DOESNT-FIT-CATCH NIL))          ;Macro, don't use linear form
        ((EQ (CAR EXP) 'QUOTE)                          ;(KLUDGE)
         (GRIND-QUOTE EXP LOC))
        ((EQ (CAR EXP) 'FUNCTION)                               ;(KLUDGE)
         (GRIND-FUNCTION EXP LOC))
        ((EQ (CAR EXP) GRIND-DISPLACED)
         (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP))))
        (T (GRIND-LINEAR-TAIL EXP LOC))))

(DEFUN GRIND-LINEAR-TAIL (EXP LOC)
   (GTYO-OPEN LOC)                              ;Do linear list
   (DO ((X EXP (CDR X))
        (LOC1 LOC (LOCF (CDR X))))
       ((ATOM X)
        (COND ((NOT (NULL X))
               (GSTRING (PTTBL-CONS-DOT *READTABLE*))
               (GIND (GRIND-LINEAR-FORM X LOC1))))
        (GTYO-CLOSE T))
      (GIND (GRIND-LINEAR-FORM (CAR X) (LOCF (CAR X))))
      (OR (ATOM (CDR X))
          (GTYO-SPACE))))

;;; First item on the left and the rest stacked vertically to its right,
;;; except if the first item won't fit one line, stack the rest below it.
;;; Items are processed through the full hair of GRIND-FORM
(DEFUN GRIND-STANDARD-FORM (EXP LOC)
 (COND ((ATOM EXP) (GRIND-ATOM EXP GRIND-IO LOC))
       ((EQ (CAR EXP) GRIND-DISPLACED)
        (GRIND-STANDARD-FORM (CADR EXP) (LOCF (CADR EXP))))
       (T
        (GTYO-OPEN LOC)
        (GIND (COND ((GRIND-FORM-VER (CAR EXP) (LOCF (CAR EXP)))
                     (GRIND-TERPRI))
                    (T
                     (GTYO-SPACE)))
              (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))))

;;; Similar to above except without the left parenthesis
(DEFUN GRIND-STANDARD-FORM-1 (EXP LOC)
  LOC
  (GIND (COND ((GRIND-FORM-VER (CAR EXP) (LOCF (CAR EXP)))
               (GRIND-TERPRI))
              (T
               (GTYO-SPACE)))
        (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))

;;;; Minimal width form.
;;; This is applied from the outside in if the expression is too wide
;;; when printed in normal form.
(DEFUN GRIND-MISER-FORM (EXP LOC)
 (GTYO-OPEN LOC)
 (GRIND-REST-OF-LIST EXP LOC (FUNCTION GRIND-OPTI-MISER)))

;;; {Recursive} top level for miser mode from the outside in.
(DEFUN GRIND-OPTI-MISER (EXP LOC)
  (COND ((ATOM EXP)                                     ;Atoms -- no optimization anyway
         (GRIND-ATOM EXP GRIND-IO LOC))
        ((EQ (CAR EXP) GRIND-DISPLACED)                 ;Undisplace displaced forms.
         (GRIND-OPTI-MISER (CADR EXP) (LOCF (CADR EXP))))
        ((GRIND-TRY 'GRIND-FORM EXP LOC))               ;Use normal mode if it wins
        (T (GRIND-MISER-FORM EXP LOC))))                ;Loses, use miser form

;;; Vertical form looks the same as miser form, but if
;;; it doesn't fit anyway we throw out and miser at a higher level rather
;;; than misering the forms inside of this form.
(DEFUN GRIND-VERTICAL-FORM (EXP LOC &OPTIONAL (FN (FUNCTION GRIND-FORM)))
  (COND ((ATOM EXP) (GRIND-ATOM EXP GRIND-IO LOC))
        (T (GTYO-OPEN LOC)
           (GRIND-REST-OF-LIST EXP LOC FN))))

;;; Grind rest of a list vertically using indicated form for the members
(DEFUN GRIND-REST-OF-LIST (TAIL LOC FORM)
  (GIND (DO ((X TAIL (CDR X))
             (COUNT 0 (1+ COUNT))
             (LOC LOC (LOCF (CDR X))))
            (())
          (COND ((ATOM X)
                 (RETURN (GRIND-DOTTED-CDR X LOC)))
                ((EQ COUNT PRINLENGTH)
                 (FUNCALL FORM (PTTBL-PRINLENGTH *READTABLE*) LOC)
                 (GTYO-CLOSE T)
                 (RETURN T))
                ((ATOM (CDR X))
                 (LET ((GRIND-WIDTH (1- GRIND-WIDTH)))  ;last form needs room for right paren
                   (FUNCALL FORM (CAR X) (LOCF (CAR X))))
                 (RETURN (GRIND-DOTTED-CDR (CDR X) (LOCF (CDR X))))))
          (FUNCALL FORM (CAR X) (LOCF (CAR X)))
          (GRIND-TERPRI))))                     ;not last form, terpri before next

(DEFUN GRIND-DOTTED-CDR (X LOC &OPTIONAL END-STRING)
 (COND ((NOT (NULL X))
        (COND ((GRIND-TRY (FUNCTION (LAMBDA (X LOC)
                                      (GSTRING (PTTBL-CONS-DOT *READTABLE*))
                                      (GRIND-ATOM X GRIND-IO LOC)))
                          X LOC))
              (T (GRIND-TERPRI)
                 (GSTRING (PTTBL-CONS-DOT *READTABLE*))
                 (GRIND-ATOM X GRIND-IO LOC)))))
 (IF END-STRING (GSTRING END-STRING T)
   (GTYO-CLOSE T)))

;;;; Handle backquotes.

;;;They are recognizable as calls to one of these four functions.
(DEFPROP XR-BQ-CONS GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-LIST GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-LIST* GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-APPEND GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-NCONC GRIND-BQ GRIND-MACRO)
(DEFPROP XR-BQ-VECTOR GRIND-BQ GRIND-MACRO)

;;; The first thing to do is convert the backquote form
;;; into a list containing sublists like (grind-comma x) or (grind-comma-atsign x).
;;; Then we grind that list with a backquote in front.
;;; The symbols grind-comma, grind-comma-atsign and grind-comma-dot
;;; at the front of a list are recognized and print out as "," or ",@" or ",.".
;;; they are recognized in two ways: as grind-macros, by functions which look for such;
;;; and specially, by grind-as-block and grind-linear-form, which lose with grind-macros.
(DEFUN GRIND-BQ (EXP LOC)
  (IF (NEQ (GET-MACRO-CHARACTER #/`) 'XR-BACKQUOTE-MACRO)
      (*THROW 'GRIND-MACRO-FAILED NIL)
    (GTYO #/`)
    (GRIND-AS-BLOCK (GRIND-UNBACKQUOTIFY EXP) LOC)))

(DEFPROP GRIND-COMMA GRIND-COMMA GRIND-MACRO)
(DEFUN GRIND-COMMA (EXP LOC)
  LOC
  (GTYO #/,)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-COMMA-ATSIGN GRIND-COMMA-ATSIGN GRIND-MACRO)
(DEFUN GRIND-COMMA-ATSIGN (EXP LOC)
  LOC
  (GTYO #/,) (GTYO #/@)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-COMMA-DOT GRIND-COMMA-DOT GRIND-MACRO)
(DEFUN GRIND-COMMA-DOT (EXP LOC)
  LOC
  (GTYO #/,) (GTYO #/.)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

(DEFPROP GRIND-DOT-COMMA GRIND-DOT-COMMA GRIND-MACRO)
(DEFUN GRIND-DOT-COMMA (EXP LOC)
  LOC
  (GTYO #/.) (GTYO-SPACE) (GTYO #/,)
  (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))

;;; Convert the backquote form to a list resembling what the user typed in,
;;; with "calls" to grind-comma, etc., representing the commas.
(DEFUN GRIND-UNBACKQUOTIFY (EXP)
  (COND ((OR (NUMBERP EXP) (EQ EXP T) (NULL EXP) (STRINGP EXP)) EXP)
        ((SYMBOLP EXP) `(GRIND-COMMA ,EXP))
        ((ATOM EXP) EXP)
        ((EQ (CAR EXP) 'QUOTE) (CADR EXP))
        ((EQ (CAR EXP) 'XR-BQ-VECTOR)
         (FILLARRAY NIL (MAPCAR 'GRIND-UNBACKQUOTIFY (CDR EXP))))
        ((EQ (CAR EXP) 'XR-BQ-CONS)
         (CONS (GRIND-UNBACKQUOTIFY (CADR EXP))
               (GRIND-UNBACKQUOTIFY-SEGMENT (CDDR EXP) NIL T)))
        ((EQ (CAR EXP) 'XR-BQ-LIST)
         (MAPCAR 'GRIND-UNBACKQUOTIFY (CDR EXP)))
        ((EQ (CAR EXP) 'XR-BQ-LIST*)
         (NCONC (MAPCAR 'GRIND-UNBACKQUOTIFY (BUTLAST (CDR EXP)))
                (GRIND-UNBACKQUOTIFY-SEGMENT (LAST EXP) NIL T)))
        ((EQ (CAR EXP) 'XR-BQ-APPEND)
         (MAPCON 'GRIND-UNBACKQUOTIFY-SEGMENT (CDR EXP)
                 (CIRCULAR-LIST T) (CIRCULAR-LIST NIL)))
        ((EQ (CAR EXP) 'XR-BQ-NCONC)
         (MAPCON 'GRIND-UNBACKQUOTIFY-SEGMENT (CDR EXP)
                 (CIRCULAR-LIST NIL) (CIRCULAR-LIST NIL)))
        (T `(GRIND-COMMA ,EXP))))

;;; Convert a thing in a backquote-form which should appear as a segment, not an element.
;;; The argument is the list whose car is the segment-form,
;;; and the value is the segment to be appended into the resulting list.
(DEFUN GRIND-UNBACKQUOTIFY-SEGMENT (LOC COPY-P TAIL-P)
  (COND ((AND TAIL-P (ATOM (CDR LOC)))
         (LET ((TEM (GRIND-UNBACKQUOTIFY (CAR LOC))))
           (COND ((EQ (CAR-SAFE TEM) 'GRIND-COMMA)
                  (LIST `(GRIND-DOT-COMMA ,(CAR LOC))))
                 (T TEM))))
        ((AND (EQ (CAAR-SAFE LOC) 'QUOTE)
              (CONSP (CADAR LOC)))
         (CADAR LOC))
        (T (LIST (LIST (IF COPY-P 'GRIND-COMMA-ATSIGN 'GRIND-COMMA-DOT)
                       (CAR LOC))))))

;;; Grind a form, choosing appropriate method
;;; The catch for miser mode is at a higher level than this, but
;;; the catch for linear mode is here.  Thus miser mode gets applied
;;; from the outside in, while linear mode gets applied from the
;;; inside out.

(DEFUN GRIND-FORM (EXP LOC &AUX TEM GMF)
  (COND ((ATOM EXP)                                     ;Atoms print very simply
         (GRIND-ATOM EXP GRIND-IO LOC))
        ((EQ (CAR EXP) GRIND-DISPLACED)
         (GRIND-FORM (CADR EXP) (LOCF (CADR EXP))))
        ((AND (SYMBOLP (CAR EXP))                       ;Check for GRIND-MACRO
              (OR (NULL (CDR EXP)) (NOT (ATOM (CDR EXP)))) ; but try not to get faked out
              (SETQ TEM (GET (CAR EXP) 'GRIND-MACRO))
              (NOT (SETQ GMF (*CATCH 'GRIND-MACRO-FAILED
                               (PROGN (FUNCALL TEM EXP LOC) NIL))))))
        ((AND (CONSP (CAR EXP))                         ;Check for LAMBDA
              (SYMBOLP (CAAR EXP))
              (SETQ TEM (GET (CAAR EXP) 'GRIND-L-MACRO))
              (NOT (SETQ GMF (*CATCH 'GRIND-MACRO-FAILED
                               (PROGN (FUNCALL TEM EXP LOC) NIL))))))
        ((AND (MEMQ GMF '(NIL NOT-A-FORM))
              ;; If linear form works, use it
              (GRIND-TRY 'GRIND-LINEAR-FORM EXP LOC (NULL GMF))))
        (T (GRIND-STANDARD-FORM EXP LOC))))             ;Loses, go for standard form

;;; GRIND-FORM and return T if it takes more than one line.
(DEFUN GRIND-FORM-VER (EXP LOC &AUX TEM)
  (SETQ TEM GRIND-VPOS)
  (GIND (GRIND-FORM EXP LOC))
  (NOT (= GRIND-VPOS TEM)))

;;; Grind with a certain form if it wins and return T,
;;; or generate no output and return NIL if that form won't fit.
(DEFUN GRIND-TRY (FORM EXP LOC &REST ARGS &AUX MARK VP HP)
  (COND (GRIND-UNTYO-P                                  ;UNTYO able, so
         (SETQ VP GRIND-VPOS                            ; save current place
               HP GRIND-HPOS
               MARK (FUNCALL GRIND-REAL-IO ':UNTYO-MARK))
         (OR (CATCH-IF-DOESNT-FIT                       ;Then try doing it
               (LEXPR-FUNCALL FORM EXP LOC ARGS)
               T)
             (PROGN                                     ;Lost, back up to saved place
               (SETQ GRIND-VPOS VP
                     GRIND-HPOS HP)
               (FUNCALL GRIND-REAL-IO ':UNTYO MARK)
               NIL)))                                   ;Return NIL to indicate lossage

        ((EQ GRIND-IO (FUNCTION GRIND-COUNT-IO))        ;Only counting, so
         (SETQ VP GRIND-VPOS                            ; save current place
               HP GRIND-HPOS)
         (OR (CATCH-IF-DOESNT-FIT
               (LEXPR-FUNCALL FORM EXP LOC ARGS)
               T)                                       ;Then try doing it
             (PROGN                                     ;Lost, back up to saved place
               (SETQ GRIND-VPOS VP
                     GRIND-HPOS HP)
               NIL)))                                   ;Return NIL to indicate lossage

        ((CATCH-IF-DOESNT-FIT                           ;Have to use do-it-twice mode
             (LET ((GRIND-IO (FUNCTION GRIND-COUNT-IO))
                   (GRIND-VPOS GRIND-VPOS)
                   (GRIND-HPOS GRIND-HPOS))
               (LEXPR-FUNCALL FORM EXP LOC ARGS)        ;So first try it tentatively
               T))
         (LEXPR-FUNCALL FORM EXP LOC ARGS)              ;Won, do it for real
         T)))                                           ;Lost, return NIL

;;;; Grind Top Level

;;; Top level grinding function.
;;; GRIND-WIDTH used to default to 95.  Now, it defaults to NIL, meaning
;;; try to figure it out and use 95. if you can't.
(DEFUN GRIND-TOP-LEVEL (EXP &OPTIONAL (GRIND-WIDTH NIL)
                                      (GRIND-REAL-IO STANDARD-OUTPUT)
                                      (GRIND-UNTYO-P NIL)
                                      (GRIND-DISPLACED 'DISPLACED)
                                      (TERPRI-P T)
                                      (GRIND-NOTIFY-FUN NIL)
                                      (LOC (NCONS EXP))
                                      (GRIND-FORMAT 'GRIND-OPTI-MISER)
                                      (INITIAL-INDENTATION 0))
  "Pretty-print the list EXP on stream GRIND-REAL-IO.
GRIND-WIDTH is the width to fit within; NIL is the default,
 meaning try to find out the stream's width or else use 95. characters.
GRIND-UNTYO-P is T if GRIND should try to use the :UNTYO operation.
GRIND-DISPLACED should be 'SI:DISPLACED if displacing is to be ignored,
 (displaced macros print just the original code)
 or NIL if displacing should be printed out.
TERPRI-P non-NIL says go to a fresh line before printing.
GRIND-NOTIFY-FUN, if non-NIL, is called for each cons-cell processed.
 Use this to keep records of how list structure was traversed during printing.
LOC is the location where EXP was found, for passing to GRIND-NOTIFY-FUN.
GRIND-FORMAT is the format to use for printing EXP.
 It should be a suitable subroutine of GRIND.
INITIAL-INDENTATION is the horizontal indent to use for the first line.
 Additional lines are indented relative to the first."
  (IF (NULL GRIND-WIDTH)
      (SETQ GRIND-WIDTH (GRIND-WIDTH-OF-STREAM GRIND-REAL-IO)))
  (WHEN TERPRI-P
    (SEND GRIND-REAL-IO ':FRESH-LINE)
    (DO ((I INITIAL-INDENTATION (1- I)))
        ((ZEROP I))
      (SEND GRIND-REAL-IO ':TYO #/SP)))
  (LET ((GRIND-IO (FUNCTION GRIND-PRINT-IO))
        (GRIND-INDENT INITIAL-INDENTATION)
        (GRIND-DEPTH 0)
        (GRIND-HPOS INITIAL-INDENTATION)
        (GRIND-VPOS 0))
     (COND ((CONSP EXP)
            (FUNCALL GRIND-FORMAT EXP LOC))
           (T (GRIND-ATOM EXP GRIND-IO LOC)))))

;;; Given a stream, try to figure out a good grind-width for it.
(DEFUN GRIND-WIDTH-OF-STREAM (STREAM)
    (COND ((MEMQ ':SIZE-IN-CHARACTERS (FUNCALL STREAM ':WHICH-OPERATIONS))
           ;; Aha, this stream handles enough messages that we can figure
           ;; out a good size.  I suppose there ought to be a new message
           ;; just for this purpose, but...  And yes, I know it only works
           ;; with fixed-width fonts, but that is inherent in GRIND-WIDTH.
           (FUNCALL STREAM ':SIZE-IN-CHARACTERS))
          (T
           ;; No idea, do the old default thing.  Better than nothing.
           95.)))

;;;; Grind Definitions

(DEFVAR GRINDEF NIL "Holds argument list to last invocation of GRINDEF")

;;; Grind the definitions of one or more functions.  With no arguments,
;;; repeat the last operation.

;;;Needn't be a Special form.
;(DEFUN GRINDEF (&QUOTE &REST FCNS)
;  "Pretty print the definitions of each of FCNS.
;This prints expressions such as could be evaluated to give
;each of FCNS its current value and//or function definition."
;  (AND FCNS (SETQ GRINDEF (COPY-LIST FCNS)))
;  (MAPC 'GRIND-1 GRINDEF)                      ;Grind each function
;  '*)                                          ;Return silly result

;;;@@@Shouldn't set GRINDEF globally until return from GRIND-1.
;;;Currently,  (GRINDEF A B . C) blows up, and (GRINDEF) after that
;;;will too. --Keith 21-Oct-88

(defmacro grindef (&rest fcns)
  "Nicely print out the definitions of each of FCNS.
This prints expressions such as could be evaluated to give
each of FCNS its current value and//or function definition."
  (let ((forms '((mapc 'grind-1 grindef) '*)))
    (when fcns (push `(setq grindef ',fcns) forms))
    `(progn ,@forms)))

;;; Grind the definition of a function.
;;; (See comments at GRIND-TOP-LEVEL re the WIDTH argument.)
(DEFUN GRIND-1 (FCN &OPTIONAL (WIDTH NIL)
                              (REAL-IO STANDARD-OUTPUT)
                              (UNTYO-P NIL)
                &AUX EXP EXP1 TEM GRIND-RENAMING-ALIST)
  (IF (NULL WIDTH)
      (SETQ WIDTH (GRIND-WIDTH-OF-STREAM REAL-IO)))
  (PROG GRIND-1 ()
        (COND ((AND (SYMBOLP FCN) (BOUNDP FCN))
               (GRIND-TOP-LEVEL `(SETQ ,FCN ',(SYMEVAL FCN)) WIDTH REAL-IO UNTYO-P)
               (TERPRI REAL-IO)))
        (OR (FDEFINEDP FCN) (RETURN NIL))
        (SETQ EXP (FDEFINITION FCN))
        ;; Grind any levels of encapsulation, as they want it.
        (DO-FOREVER
          (SETQ EXP1 EXP)
          (AND (EQ (CAR-SAFE EXP) 'MACRO)
               (SETQ EXP1 (CDR EXP)))
          (OR (AND (NOT (SYMBOLP EXP1))
                   (SETQ TEM (ASSQ 'SI::ENCAPSULATED-DEFINITION
                                   (DEBUGGING-INFO EXP1))))
              (RETURN NIL))
          (FUNCALL (GET (CADDR TEM) 'ENCAPSULATION-GRIND-FUNCTION)
                   FCN EXP1 WIDTH REAL-IO UNTYO-P)
          (AND (EQ (CADDR TEM) 'RENAME-WITHIN)
               (SETQ GRIND-RENAMING-ALIST
                     (CADR (ASSQ 'RENAMINGS (DEBUGGING-INFO EXP1)))))
          (OR (FDEFINEDP (CADR TEM)) (RETURN-FROM GRIND-1 NIL))
          (SETQ EXP (FDEFINITION (CADR TEM))))
        ;; Now process the basic definition.
        (SETQ TEM (IF (AND (EQ (CAR-SAFE EXP) 'MACRO))
                      (CDR EXP) EXP))
        (AND (TYPEP TEM 'COMPILED-FUNCTION)
             (COND ((ASSQ 'INTERPRETED-DEFINITION (DEBUGGING-INFO TEM))
                    (SETQ EXP (CADR (ASSQ 'SI::INTERPRETED-DEFINITION (DEBUGGING-INFO TEM)))))))
        (AND (EQ (CAR-SAFE FCN) ':WITHIN)
             (EQ EXP (CADDR FCN))
             (RETURN NIL))
        (GRIND-TOP-LEVEL (COND ((TYPEP EXP 'SELECT)
                                (SETQ TEM (%MAKE-POINTER DTP-LIST EXP))
                                (LET ((TAIL-POINTER (CDR (LAST TEM))) BODY)
                                  (SETQ BODY
                                   `(,(IF TAIL-POINTER `(,FCN ,TAIL-POINTER) FCN)
                                     . ,(DO ((ELTS TEM (CDR ELTS))
                                             (RESULT))
                                            ((ATOM ELTS) (NREVERSE RESULT))
                                          (LET ((ELT (CAR ELTS)))
                                            (PUSH (COND ((ATOM (CDR ELT)) ELT)
                                                        ((EQ (CADR ELT) 'LAMBDA)
                                                         `(,(CAR ELT) ,(CDR (CADDR ELT))
                                                           . ,(CDDDR ELT)))
                                                        ((EQ (CADR ELT) 'NAMED-LAMBDA)
                                                         `(,(CAR ELT) ,(CDR (CADDDR ELT))
                                                           . ,(CDDDDR ELT)))
                                                        (T ELT))
                                                  RESULT)))))
                                  (CONS 'DEFSELECT BODY)))
                               ((ATOM EXP)
                                `(DEFF ,FCN ',EXP))
                               ((EQ (CAR EXP) 'MACRO)
                                (COND ((TYPEP (CDR EXP) 'COMPILED-FUNCTION)
                                       `(DEFF ,FCN ',EXP))
                                      (T `(MACRO ,FCN
                                                 . ,(GRIND-FLUSH-LAMBDA-HEAD (CDR EXP))))))
                               ((MEMQ (CAR EXP) '(SUBST NAMED-SUBST CL:SUBST))
                                `(DEFSUBST ,FCN . ,(GRIND-FLUSH-LAMBDA-HEAD EXP)))
                               ((NOT (MEMQ (CAR EXP) '(LAMBDA NAMED-LAMBDA)))
                                `(FDEFINE ',FCN ',EXP))
                               ((EQ (CAR-SAFE FCN) ':METHOD)
                                (SETQ TEM (GRIND-FLUSH-LAMBDA-HEAD EXP))
                                (SETQ TEM (CONS (CDAR TEM) (CDR TEM)))  ;Remove OPERATION arg
                                `(DEFMETHOD ,(CDR FCN) . ,TEM))
                               (T
                                `(DEFUN ,FCN . ,(GRIND-FLUSH-LAMBDA-HEAD EXP))))
                         WIDTH
                         REAL-IO
                         UNTYO-P)
         (TERPRI REAL-IO)
         ))

(DEFUN GRIND-FLUSH-LAMBDA-HEAD (LAMBDA-EXP)
    (COND ((ATOM LAMBDA-EXP) LAMBDA-EXP)
          (T (SI::LAMBDA-EXP-ARGS-AND-BODY LAMBDA-EXP))))

;;;; Grind Macros

(DEFUN GRIND-QUOTE (EXP LOC)
  (COND ((AND (CDR EXP) (CONSP (CDR EXP)) (NULL (CDDR EXP)))
         (GTYO #/' LOC)
         (GIND (GRIND-AS-BLOCK (CADR EXP) (LOCF (CADR EXP)))))
        (T (GRIND-AS-BLOCK EXP LOC))))

(DEFUN GRIND-FUNCTION (EXP LOC)
  (COND ((AND (CDR EXP) (CONSP (CDR EXP)) (NULL (CDDR EXP)))
         (GTYO #/# LOC)
         (GTYO #/' LOC)
         (GIND (GRIND-AS-BLOCK (CADR EXP) (LOCF (CADR EXP)))))
        (T (GRIND-AS-BLOCK EXP LOC))))

;NOTE- DEFUN looks bad in miser mode, so we have a slight kludge
; to bypass it.  (Would only gain two spaces anyway).  Probably
; this should be generalized to some property on the atom?
(DEFUN GRIND-DEFUN (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 3 (FUNCTION GRIND-OPTI-MISER)))

(DEFUN GRIND-LAMBDA (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 2))

(DEFUN GRIND-NAMED-LAMBDA (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 3))

;;; DEFUN either craps out and uses miser form, or puts fcn and lambda list
;;; on the first line and the rest aligned under the E
;;; Second arg to GRIND-DEF-FORM is number of items on the first line.
;;; The last one is ground as a block.
(DEFUN GRIND-DEF-FORM (EXP LOC N &OPTIONAL (FORM (FUNCTION GRIND-FORM)))
  ;; Make a prepass over the list and make sure it looks like a form (i.e. not dotted,
  ;; and long enough)
  (LOOP FOR LS = EXP THEN (CDR LS)
        AND I FROM 0
        WHEN (AND (ATOM LS) (OR (< I N) LS))
        DO (*THROW 'GRIND-MACRO-FAILED 'NOT-A-FORM)
        WHEN (NULL LS)
        DO (RETURN))
  (GTYO-OPEN LOC)
  (DOTIMES (I (1- N))
    (GRIND-QUOTED (GRIND-LINEAR-FORM (CAR EXP) (LOCF (CAR EXP))))
    (GTYO-SPACE)
    (SETQ LOC (LOCF (CDR EXP)))
    (SETQ EXP (CDR EXP)))
  (IF (CAR EXP)
      (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
      (GTYO-OPEN (LOCF (CAR EXP)))
      (GTYO-CLOSE T))
  (COND ((CDR EXP)
         (GRIND-TERPRI)
         (DOTIMES (I 2) (GTYO-SPACE))
         (GIND (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) FORM)))
        (T (GTYO-CLOSE T))))

;;; BLOCK FORM: As many frobs per line as will fit;
;;;  and don't undisplace DISPLACED in them, since they aren't forms, just lists.
;;; Don't check for grind macros.  Do recognize GRIND-COMMA, etc.,
;;; because this function is used for printing the body of a backquote.
(DEFUN GRIND-AS-BLOCK (EXP LOC &OPTIONAL START-STRING END-STRING
                       &AUX (GRIND-DISPLACED GRIND-DUMMY-DISPLACED))
   (COND ((ATOM EXP)
          (GRIND-ATOM EXP GRIND-IO LOC))
         (T (IF START-STRING (GSTRING START-STRING LOC)
              (GTYO-OPEN LOC))
            (GIND (DO ((X EXP (CDR X))
                       (LOC LOC (LOCF (CDR X)))
                       (COUNT 0 (1+ COUNT))
                       (ELT) (ELTLOC)
                       (FRESHLINEP T)
                       (VP GRIND-VPOS GRIND-VPOS))
                      ((ATOM X)
                       (GRIND-DOTTED-CDR X LOC END-STRING))
                    (AND (EQ COUNT PRINLENGTH)
                         (RETURN
                           (PROGN (GRIND-ATOM (PTTBL-PRINLENGTH *READTABLE*) GRIND-IO LOC)
                                  (IF END-STRING (GSTRING END-STRING T)
                                    (GTYO-CLOSE T)))))
                    (SETQ ELT (CAR X) ELTLOC (LOCF (CAR X)))
                    (COND ((AND (CONSP ELT)
                                (MEMQ (CAR ELT) '(GRIND-COMMA
                                                  GRIND-COMMA-DOT
                                                  GRIND-DOT-COMMA
                                                  GRIND-COMMA-ATSIGN)))
                           (SELECTQ (CAR ELT)
                             (GRIND-COMMA (GTYO #/,))
                             (GRIND-COMMA-ATSIGN (GSTRING ",@"))
                             (GRIND-COMMA-DOT (GSTRING ",."))
                             (GRIND-DOT-COMMA (GSTRING ". ,")))
                           (SETQ ELTLOC (LOCF (CADR ELT)) ELT (CADR ELT))))
                    (COND ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC))
                          ((AND FRESHLINEP
                                (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) ELT ELTLOC)))
                          (T                            ;Won't fit, start another line
                           (OR FRESHLINEP (GRIND-TERPRI))
                           (SETQ VP GRIND-VPOS)
                           (OR (GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) ELT ELTLOC)
                               (GRIND-STANDARD-FORM ELT ELTLOC))))
                    (AND (CONSP (CDR X))                ;If not done,
                         (COND ((AND (= VP GRIND-VPOS)  ;if still on same line, need a space
                                     (< GRIND-HPOS GRIND-WIDTH))        ;unless at end of line
                                (GTYO-SPACE)
                                (SETQ FRESHLINEP NIL))
                               (T                       ;If this was moby, don't put
                                (GRIND-TERPRI)          ; anything else on the same line
                                (SETQ FRESHLINEP T)))) )))))

(DEFUN GRIND-DEFSELECT (EXP LOC)
  (GTYO-OPEN LOC)
  ;; Output the DEFSELECT
  (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
  (GTYO-SPACE)
  (SETQ LOC (LOCF (CDR EXP)))
  (POP EXP)
  ;; Output the function name, on the same line.
  (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
  (GTYO-SPACE)
  (SETQ LOC (LOCF (CDR EXP)))
  (POP EXP)
  ;; Output the clauses.
  (IF (NULL EXP)
      (PROGN (GTYO-CLOSE) T)
    (GRIND-TERPRI)
    (GTYO-SPACE) (GTYO-SPACE)
    (GIND (GRIND-REST-OF-LIST EXP LOC 'GRIND-DEFSELECT-CLAUSE))))

(DEFUN GRIND-DEFSELECT-CLAUSE (EXP LOC)
  (GTYO-OPEN LOC)
  (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
  (SETQ LOC (LOCF (CDR EXP)))
  (POP EXP)
  (COND ((ATOM EXP)
         (GSTRING (PTTBL-CONS-DOT *READTABLE*))
         (GRIND-QUOTED (GRIND-AS-BLOCK EXP LOC)))
        (T
         (GTYO-SPACE)
         ;; Output the argument list.
         (IF (CAR EXP)
             (GRIND-QUOTED (GRIND-AS-BLOCK (CAR EXP) (LOCF (CAR EXP))))
           (GTYO-OPEN (LOCF (CAR EXP)))
           (GTYO-CLOSE T))
         (SETQ LOC (LOCF (CDR EXP)))
         (POP EXP)
         ;; Output the body.
         (IF (NULL EXP)
             (PROGN (GTYO-CLOSE) T)
           (GRIND-TERPRI)
           (GTYO-SPACE) (GTYO-SPACE)
           (GIND (GRIND-REST-OF-LIST EXP LOC 'GRIND-OPTI-MISER))))))

;;; PROG form is similar, but with exdented tags
(DEFUN GRIND-PROG (EXP LOC)
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO-SPACE)
  (GRIND-AS-BLOCK (CADR EXP) (LOCF (CADR EXP)))
  (GRIND-TERPRI)
  (GTYO-SPACE)                                  ;Tags fall under the P of PROG
  (GRIND-REST-OF-PROG (CDDR EXP) (LOCF (CDDR EXP)) (+ GRIND-INDENT 6)))

(DEFUN GRIND-REST-OF-PROG (EXP LOC INDENT)
  (GIND (DO ((X EXP (CDR X))
             (LOC LOC (LOCF (CDR X))))
            ((ATOM X)
             (GRIND-DOTTED-CDR X LOC))
          (COND ((ATOM (CAR X))                 ;Tag
                 (GRIND-ATOM (CAR X) GRIND-IO (LOCF (CAR X)))
                 (COND ((OR ( GRIND-HPOS INDENT)
                            (ATOM (CDR X))
                            (ATOM (CADR X)))
                        ;; Put the next form on the same line if it fits.
                        (GRIND-TERPRI))))
                (T                              ;Statement
                 (DO ((I (- INDENT GRIND-HPOS) (1- I)))
                     ((<= I 0))
                   (GTYO-SPACE))
                 (GRIND-FORM (CAR X) (LOCF (CAR X)))
                 (OR (ATOM (CDR X))
                     (GRIND-TERPRI)))))))

(DEFUN GRIND-PROGV (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 3))

(DEFUN GRIND-PROGW (EXP LOC)
  (GRIND-DEF-FORM EXP LOC 2))

;;; DO: determine whether old or new format, grind out the header,
;;; then do the body like PROG
(DEFUN GRIND-DO (EXP LOC)
  (GRIND-CHECK-DO EXP)
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GRIND-REST-OF-DO EXP LOC))

(DEFUN GRIND-DO-NAMED (EXP LOC)
  (GRIND-CHECK-DO (CDR EXP))
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO-SPACE)
  (GRIND-ATOM (CADR EXP) GRIND-IO LOC)
  (GRIND-REST-OF-DO (CDR EXP) (LOCF (CDR EXP))))

(DEFUN GRIND-CHECK-DO (EXP)
  (when
    (or
      (atom (cadr exp))                         ;Dont lose on '(do dogs eat snails)...
      (< (LENGTH EXP)
         ;; Don't get faked into losing by the old format.
         (IF (OR (CONSP (CADR EXP)) (NULL (CADR EXP))) 3 4)))
    (*THROW 'GRIND-MACRO-FAILED 'NOT-A-FORM)))

(DEFUN GRIND-REST-OF-DO (EXP LOC)
  (GTYO-SPACE)
  (COND ((OR (CONSP (CADR EXP)) (NULL (CADR EXP)))      ;New format
         (GIND (PROGN (GRIND-VERTICAL-FORM (CADR EXP)   ;Var list vertically
                                           (LOCF (CADR EXP))
                                           (FUNCTION GRIND-DO-VAR))
                      (GRIND-TERPRI)
                      ;; End test / results as COND clause
                      (GRIND-COND-CLAUSE (CADDR EXP) (LOCF (CADDR EXP)))))
         (SETQ LOC (LOCF (CDDDR EXP)))
         (SETQ EXP (CDDDR EXP)))
        (T                                      ;Old format
         (GRIND-LINEAR-FORM (CADR EXP) (LOCF (CADR EXP)))       ;Var
         (GTYO-SPACE)
         (GRIND-LINEAR-FORM (CADDR EXP) (LOCF (CADDR EXP)))     ;Initial
         (GTYO-SPACE)
         (GRIND-LINEAR-FORM (CADDDR EXP) (LOCF (CADDDR EXP)))   ;Step
         (GTYO-SPACE)
         (GRIND-LINEAR-FORM (CAR (SETQ EXP (CDDDDR EXP))) (LOCF (CAR EXP)))     ;Endtest
         (SETQ LOC (LOCF (CDR EXP)))
         (SETQ EXP (CDR EXP))))
  (GRIND-TERPRI)
  (GTYO-SPACE)
  (GRIND-REST-OF-PROG EXP LOC (+ GRIND-INDENT 2)))

(DEFUN GRIND-DO-VAR (EXP LOC)
    (COND ((ATOM EXP) (GRIND-ATOM EXP GRIND-IO LOC))
          ((GRIND-TRY (FUNCTION GRIND-LINEAR-TAIL) EXP LOC))    ;If linear form works, use it
          (T (GTYO-OPEN LOC)
             (GRIND-STANDARD-FORM-1 EXP LOC))))

(DEFUN GRIND-LET (EXP LOC)
  (GIND (GTYO-OPEN LOC)
        (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
        (GTYO-SPACE)
        (GIND (GRIND-VERTICAL-FORM (CADR EXP) (LOCF (CADR EXP)) (FUNCTION GRIND-DO-VAR)))
        (GRIND-TERPRI)
        (GTYO-SPACE)
        (GTYO-SPACE)
        (GRIND-REST-OF-PROG (CDDR EXP) (LOCF (CDDR EXP)) GRIND-HPOS)))

(DEFUN GRIND-COMPILER-LET (EXP LOC)
  (GRIND-LET EXP LOC))


;;; COND: Print clauses in standard form.  Expressions within
;;; clauses are normally stacked vertically, but if there is one
;;; consequent and it is an atom or a GO, put it to the side if it fits.
;;; Also, if the antecedent is T and there is one consequent, put it to the
;;; side in order to save lines.

(DEFUN GRIND-COND (EXP LOC)
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO-SPACE)
  (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-COND-CLAUSE)))

(DEFUN GRIND-COND-CLAUSE (EXP LOC)
  (COND ((ATOM EXP)
         (GRIND-ATOM EXP GRIND-IO LOC))
        ((AND (CONSP (CDR EXP))
              (NULL (CDDR EXP))
              (OR (EQ (CAR EXP) 'T)
                  (GRIND-SIMPLE-P (CADR EXP)))
              (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM) EXP LOC)))
        (T (GRIND-VERTICAL-FORM EXP LOC))))

;;; AND and OR: Stack vertically unless only two long and
;;; one or the other is an atom or the second is a GO.  This
;;; is analagous to the rule for COND clauses.
(DEFUN GRIND-AND (EXP LOC)
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO-SPACE)
  (GIND (COND ((AND (CDR EXP)                   ;this and the next three forms really mean (= 2 (length (cdr exp)))
                    (CDDR EXP)                  ;but length is a potentially expensive operation.
                    (consp (cddr exp))          ;deal with the "second" argument being " . something"
                    (NULL (CDDDR EXP))          ;if we got past here we thing there are exactly 2 arguments.
                    (OR (GRIND-SIMPLE-P (CADR EXP))
                        (GRIND-SIMPLE-P (CADDR EXP)))
                    (GRIND-TRY (FUNCTION GRIND-STANDARD-FORM-1) (CDR EXP) (LOCF (CDR EXP)))))
              (T (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM))))))

;;; Predicate for whether something is simple enough to go
;;; "on the same line" in COND, AND, and OR.
(DEFUN GRIND-SIMPLE-P (EXP)
  (OR (ATOM EXP)
      (EQ (CAR EXP) 'GO)
      (EQ (CAR EXP) 'QUOTE)))

;;; Trace.  If it won't fit on one line, put each trace option and argument on a line.
(DEFUN GRIND-TRACE (EXP LOC)
  (COND ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) EXP LOC))      ;Fits on one line, OK
        (T (GTYO-OPEN LOC)                                      ;Doesn't fit
           (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
           (GTYO-SPACE)
           (GIND (DO ((L (CDR EXP) (CDR L))
                      (CLAUSE)
                      (LOC))
                     ((NULL L)
                      (GTYO-CLOSE T))
                   (SETQ CLAUSE (CAR L)
                         LOC (LOCF (CAR L)))
                   (COND ((ATOM CLAUSE) (GRIND-ATOM CLAUSE GRIND-IO LOC))
                         ((GRIND-TRY (FUNCTION GRIND-LINEAR-FORM) CLAUSE LOC)) ;Try to use 1 line
                         (T (GTYO-OPEN LOC)
                            (COND ((NEQ (CAR CLAUSE) ':FUNCTION)
                                   ;; Name of function
                                   (GRIND-FORM (CAR CLAUSE) (LOCF (CAR CLAUSE)))
                                   (SETQ LOC (LOCF (CDR CLAUSE)))
                                   (SETQ CLAUSE (CDR CLAUSE))
                                   (GTYO-SPACE)))
                            (GIND (DO ((X CLAUSE (CDR X)))
                                      ((NULL X))        ;Print each grind option
                                    (GRIND-ATOM (CAR X) GRIND-IO LOC)   ;First name of option
                                    (COND ((EQ (CAR X) ':STEP)) ;STEP takes no arg
                                          ((MEMQ (CAR X) '(:BOTH :ARG :VALUE NIL))
                                           (GTYO-SPACE)
                                           (GRIND-FORM (CDR X) (LOCF (CDR X)))  ;These take N args
                                           (SETQ X NIL))
                                          ((NULL (CDR X)))      ;Don't print what isn't there
                                          (T (GTYO-SPACE)
                                             (GRIND-FORM (CADR X) (LOCF (CADR X)))      ;Most take 1 arg
                                             (SETQ X (CDR X))))
                                    (AND (CDR X) (GRIND-TERPRI))
                                    (SETQ LOC (LOCF (CDR X)))))
                            (GTYO-CLOSE T)))
                   (AND (CDR L) (GRIND-TERPRI)))))))

;;; SETQ: print the args two per line.  If a pair don't fit on the line,
;;; for now just craps out through normal miser mode mechanism.
(DEFUN GRIND-SETQ (EXP LOC)
  (GTYO-OPEN LOC)
  (GRIND-ATOM (CAR EXP) GRIND-IO (LOCF (CAR EXP)))
  (GTYO-SPACE)
  (GIND (DO ((X (CDR EXP) (CDDR X)))
            ((NULL X))
          (COND ((GRIND-FORM-VER (CAR X) (LOCF (CAR X)))
                 (GRIND-TERPRI))
                (T (GTYO-SPACE)))
          (IF (NOT (NULL (CDR X))) ;don't bust totally on odd-length SETQ bodies.
              (GRIND-FORM (CADR X) (LOCF (CADR X))))
          (AND (CDDR X) (GRIND-TERPRI))))
  (GTYO-CLOSE T))

;;; EXP is ((LAMBDA (...) ...) ...)
;;; Grind the Lambda as a form and the arguments underneath it.
(DEFUN GRIND-LAMBDA-COMPOSITION (EXP LOC)
  (GTYO-OPEN LOC)
  (GIND (PROGN
         (GRIND-FORM (CAR EXP) (LOCF (CAR EXP)))
         (GRIND-TERPRI)
         (GRIND-REST-OF-LIST (CDR EXP) (LOCF (CDR EXP)) (FUNCTION GRIND-FORM)))))
