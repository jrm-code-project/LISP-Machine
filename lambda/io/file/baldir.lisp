;;;-*- Mode:LISP; Package:FILE-SYSTEM; Base:8; Readtable:ZL -*-
;;; Directory Hacks program by HANSON -- feel free to use it.

;;;; Pathname Operations

#+SYMBOLICS                                     ;Already one in MIT system.
(DEFSUBST FAST-NEW-PATHNAME (OLD-PATHNAME NEW-DEVICE NEW-DIRECTORY
                             NEW-NAME NEW-TYPE NEW-VERSION)
  (LET ((NARGS 1))
    (IF (NOT (NULL NEW-DEVICE)) (INCF NARGS 2))
    (IF (NOT (NULL NEW-DIRECTORY)) (INCF NARGS 2))
    (IF (NOT (NULL NEW-NAME)) (INCF NARGS 2))
    (IF (NOT (NULL NEW-TYPE)) (INCF NARGS 2))
    (IF (NOT (NULL NEW-VERSION)) (INCF NARGS 2))
    (%START-FUNCTION-CALL OLD-PATHNAME RETURN NARGS NIL)
    (%PUSH ':NEW-PATHNAME)
    (COND (NEW-DEVICE
           (%PUSH ':DEVICE)
           (%PUSH NEW-DEVICE)))
    (COND (NEW-DIRECTORY
           (%PUSH ':DIRECTORY)
           (%PUSH NEW-DIRECTORY)))
    (COND (NEW-NAME
           (%PUSH ':NAME)
           (%PUSH NEW-NAME)))
    (COND (NEW-TYPE
           (%PUSH ':TYPE)
           (%PUSH NEW-TYPE)))
    (COND (NEW-VERSION
           (%PUSH ':VERSION)
           (%PUSH NEW-VERSION)))
    (%FINISH-FUNCTION-CALL OLD-PATHNAME RETURN NARGS NIL)))

(DEFSUBST UNSPECIFIED-PATHNAME-COMPONENT? (COMPONENT)
  (MEMQ COMPONENT '(NIL :UNSPECIFIC)))

(DEFSUBST WILD-PATHNAME-COMPONENT? (COMPONENT)
  (MEMQ COMPONENT '(NIL :WILD)))

(DEFSUBST WILD-PATHNAME-VERSION-COMPONENT? (COMPONENT)
  (MEMQ COMPONENT '(NIL :WILD :NEWEST)))

(DEFUN DEFAULT-EMPTY-PATHNAME-COMPONENTS (PATH1 PATH2)
  "Fill in the unspecified parts of PATH1 with the corresponding parts of PATH2."

  (FAST-NEW-PATHNAME PATH1
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-DEVICE PATH1))
                          (PATHNAME-DEVICE PATH2))
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-DIRECTORY PATH1))
                          (PATHNAME-DIRECTORY PATH2))
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-NAME PATH1))
                          (PATHNAME-NAME PATH2))
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-TYPE PATH1))
                          (PATHNAME-TYPE PATH2))
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-VERSION PATH1))
                          (PATHNAME-VERSION PATH2))))

(DEFUN DEFAULT-WILD-PATHNAME-COMPONENTS (PATH1 PATH2)
  "Fill in the unspecified or wild parts of PATH1 with the corresponding parts of PATH2."

  (FAST-NEW-PATHNAME PATH1
                     (AND (WILD-PATHNAME-COMPONENT? (PATHNAME-DEVICE PATH1))
                          (PATHNAME-DEVICE PATH2))
                     (AND (WILD-PATHNAME-COMPONENT? (PATHNAME-DIRECTORY PATH1))
                          (PATHNAME-DIRECTORY PATH2))
                     (AND (WILD-PATHNAME-COMPONENT? (PATHNAME-NAME PATH1))
                          (PATHNAME-NAME PATH2))
                     (AND (WILD-PATHNAME-COMPONENT? (PATHNAME-TYPE PATH1))
                          (PATHNAME-TYPE PATH2))
                     (AND (WILD-PATHNAME-VERSION-COMPONENT? (PATHNAME-VERSION PATH1))
                          (PATHNAME-VERSION PATH2))))

(DEFSUBST MATCHING-PATHNAME-COMPONENTS? (ACCESS-MSG PATHNAME1 PATHNAME2
                                         CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
  "Two components match if one maps into the other."
  (OR (EQUALP (SEND PATHNAME1 ACCESS-MSG)
              (SEND CONVERTED-PATHNAME2 ACCESS-MSG))
      (EQUALP (SEND PATHNAME2 ACCESS-MSG)
              (SEND CONVERTED-PATHNAME1 ACCESS-MSG))))

(DEFUN MATCHING-NAME-AND-TYPE? (PATHNAME1 PATHNAME2 &OPTIONAL CANONICAL-TYPES-P)
  (AND (LET ((NAME1 (PATHNAME-NAME PATHNAME1)) (NAME2 (PATHNAME-NAME PATHNAME2)))
         (OR (EQUALP NAME1 NAME2)
             (AND (STRINGP NAME1) (LISTP NAME2) (NULL (CDR NAME2))
                  (EQUALP NAME1 (CAR NAME2)))
             (AND (STRINGP NAME2) (LISTP NAME1) (NULL (CDR NAME1))
                  (EQUALP NAME2 (CAR NAME1)))))
       (LET ((HOST1 (PATHNAME-HOST PATHNAME1))
             (HOST2 (PATHNAME-HOST PATHNAME2))
             (CONVERTED-PATHNAME1 PATHNAME1)
             (CONVERTED-PATHNAME2 PATHNAME2))
         (UNLESS (EQ HOST1 HOST2)
           (SETQ CONVERTED-PATHNAME1
                 (SEND PATHNAME1 ':NEW-PATHNAME ':HOST HOST2)
                 CONVERTED-PATHNAME2
                 (SEND PATHNAME2 ':NEW-PATHNAME ':HOST HOST1)))
         (OR (MATCHING-PATHNAME-COMPONENTS? ':TYPE PATHNAME1 PATHNAME2
                                            CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
             (AND CANONICAL-TYPES-P
                  (MATCHING-PATHNAME-COMPONENTS?
                    ':CANONICAL-TYPE PATHNAME1 PATHNAME2
                    CONVERTED-PATHNAME1 CONVERTED-PATHNAME2))))))

(DEFSUBST MATCHING-OR-MISSING-PATHNAME-COMPONENTS?
          (ACCESS-MSG PATHNAME1 PATHNAME2)
  "Two components match if one maps into the other or if either is NIL or :WILD."
  (OR (MEMQ (SEND PATHNAME1 ACCESS-MSG) '(NIL :WILD))
      (MEMQ (SEND PATHNAME2 ACCESS-MSG) '(NIL :WILD))
      (EQUALP (SEND PATHNAME1 ACCESS-MSG)
              (SEND PATHNAME2 ACCESS-MSG))))

;Used only to compare against the :IGNORE filenames.
(DEFUN MATCHING-PATHNAMES? (PATHNAME1 PATHNAME2)
  (AND (MATCHING-OR-MISSING-PATHNAME-COMPONENTS? ':DEVICE PATHNAME1 PATHNAME2)
       (MATCHING-OR-MISSING-PATHNAME-COMPONENTS? ':DIRECTORY PATHNAME1 PATHNAME2)
       (MATCHING-OR-MISSING-PATHNAME-COMPONENTS? ':NAME PATHNAME1 PATHNAME2)
       (MATCHING-OR-MISSING-PATHNAME-COMPONENTS? ':TYPE PATHNAME1 PATHNAME2)
       (MATCHING-OR-MISSING-PATHNAME-COMPONENTS? ':VERSION PATHNAME1 PATHNAME2)))

(DEFUN MATCHING-PATHNAME-COMPONENTS-WITH-WILD?
       (ACCESS-MSG PATHNAME1 PATHNAME2 CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
  "Two components match if one maps into the other or one is WILD."
  (LET ((P1-COMP (SEND PATHNAME1 ACCESS-MSG))
        (P2-C-COMP (SEND CONVERTED-PATHNAME2 ACCESS-MSG))
        (P2-COMP (SEND PATHNAME2 ACCESS-MSG))
        (P1-C-COMP (SEND CONVERTED-PATHNAME1 ACCESS-MSG)))
    (OR (EQ P1-COMP ':WILD)
        (EQ P2-C-COMP ':WILD)
        (EQUAL P1-COMP P2-C-COMP)
        (EQ P2-COMP ':WILD)
        (EQ P1-C-COMP ':WILD)
        (EQUAL P2-COMP P1-C-COMP))))

(DEFUN MATCHING-PATHNAMES-WITH-WILD? (PATHNAME1 PATHNAME2)
  (LET ((CONVERTED-PATHNAME1 (SEND PATHNAME1 ':NEW-PATHNAME ':HOST (PATHNAME-HOST PATHNAME2)))
        (CONVERTED-PATHNAME2 (SEND PATHNAME2 ':NEW-PATHNAME ':HOST (PATHNAME-HOST PATHNAME1))))
    (AND (MATCHING-PATHNAME-COMPONENTS-WITH-WILD? ':DEVICE PATHNAME1 PATHNAME2
                                                  CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
         (MATCHING-PATHNAME-COMPONENTS-WITH-WILD? ':DIRECTORY PATHNAME1 PATHNAME2
                                                  CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
         (MATCHING-PATHNAME-COMPONENTS-WITH-WILD? ':NAME PATHNAME1 PATHNAME2
                                                  CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
         (MATCHING-PATHNAME-COMPONENTS-WITH-WILD? ':TYPE PATHNAME1 PATHNAME2
                                                  CONVERTED-PATHNAME1 CONVERTED-PATHNAME2)
         (MATCHING-PATHNAME-COMPONENTS-WITH-WILD? ':VERSION PATHNAME1 PATHNAME2
                                                  CONVERTED-PATHNAME1 CONVERTED-PATHNAME2))))






(COMMENT
(DEFUN MATCHING-PATHNAME-COMPONENTS? (ACCESS-MSG MAPPING-MSG PATHNAME1 PATHNAME2)
  (LET ((COMPONENT1 (SEND PATHNAME1 ACCESS-MSG))
        (COMPONENT2 (SEND PATHNAME2 ACCESS-MSG)))
    (OR (EQUAL COMPONENT1 (SEND PATHNAME1 MAPPING-MSG COMPONENT2))
        (EQUAL COMPONENT2 (SEND PATHNAME2 MAPPING-MSG COMPONENT1))
        (AND (UNSPECIFIED-PATHNAME-COMPONENT? COMPONENT1)
             (UNSPECIFIED-PATHNAME-COMPONENT? COMPONENT2)))))

(DEFUN MATCHING-PATHNAMES? (PATHNAME1 PATHNAME2)
  "True if all of the components of the pathnames match."
  (AND (EQ (PATHNAME-HOST PATHNAME1) (PATHNAME-HOST PATHNAME2))
       (MATCHING-PATHNAME-COMPONENTS? ':DEVICE ':VALID-DEVICE PATHNAME1 PATHNAME2)
       (MATCHING-PATHNAME-COMPONENTS? ':DIRECTORY ':VALID-DIRECTORY PATHNAME1 PATHNAME2)
       (MATCHING-PATHNAME-COMPONENTS? ':NAME ':VALID-NAME PATHNAME1 PATHNAME2)
       (MATCHING-PATHNAME-COMPONENTS? ':TYPE ':VALID-TYPE PATHNAME1 PATHNAME2)
       (MATCHING-PATHNAME-COMPONENTS? ':VERSION ':VALID-VERSION PATHNAME1 PATHNAME2)))



(DEFUN MATCHING-NAME-AND-TYPE? (PATHNAME1 PATHNAME2)
  (AND (MATCHING-PATHNAME-COMPONENTS? ':NAME ':VALID-NAME PATHNAME1 PATHNAME2)
       (MATCHING-PATHNAME-COMPONENTS? ':TYPE ':VALID-TYPE PATHNAME1 PATHNAME2)))
) ;end comment

;;;; CFile Abstraction
;;; Covers up the structure of the object returned by DIRECTORY-LIST.

(DEFSUBST CFILE-PLIST (CFILE) CFILE)
(DEFSUBST CFILE-TRUENAME (CFILE) (CAR CFILE))
(DEFSUBST CFILE-HOST (CFILE) (PATHNAME-HOST (CFILE-TRUENAME CFILE)))
(DEFSUBST CFILE-DEVICE (CFILE) (PATHNAME-DEVICE (CFILE-TRUENAME CFILE)))
(DEFSUBST CFILE-DIRECTORY (CFILE) (PATHNAME-DIRECTORY (CFILE-TRUENAME CFILE)))
(DEFSUBST CFILE-NAME (CFILE) (PATHNAME-NAME (CFILE-TRUENAME CFILE)))
(DEFSUBST CFILE-TYPE (CFILE) (PATHNAME-TYPE (CFILE-TRUENAME CFILE)))
(DEFSUBST CFILE-CANONICAL-TYPE (CFILE) (SEND (CFILE-TRUENAME CFILE) ':CANONICAL-TYPE))
(DEFSUBST CFILE-VERSION (CFILE) (PATHNAME-VERSION (CFILE-TRUENAME CFILE)))

(DEFSUBST CFILE-CREATION-DATE (CFILE)
  (GET (CFILE-PLIST CFILE) ':CREATION-DATE))

(DEFSUBST CFILE-AUTHOR (CFILE)
  (GET (CFILE-PLIST CFILE) ':AUTHOR))

(DEFSUBST CFILE-TRANSFER-DESTINATIONS (CFILE)
  (GET (CFILE-PLIST CFILE) 'TRANSFER-DESTINATIONS))

(DEFSUBST CFILE-TRANSFER-MODE (CFILE)
  (GET (CFILE-PLIST CFILE) 'TRANSFER-MODE))

(DEFSUBST CFILE-CLASS-REPRESENTATIVE (CFILE)
  (GET (CFILE-PLIST CFILE) 'CLASS-REPRESENTATIVE))

(DEFSUBST CFILE-REPRESENTED-CLASS (REPRESENTATIVE)
  (GET (CFILE-PLIST REPRESENTATIVE) 'REPRESENTATIVE-CLASS))

(DEFSUBST CFILE-BYTE-SIZE (CFILE)
  (GET (CFILE-PLIST CFILE) ':BYTE-SIZE))

(DEFSUBST CFILE-IS-SUBDIRECTORY? (CFILE)
  (GET (CFILE-PLIST CFILE) ':DIRECTORY))

(DEFSUBST SAME-NAME-AND-TYPE? (CFILE1 CFILE2)
  (MATCHING-NAME-AND-TYPE? (CFILE-TRUENAME CFILE1)
                           (CFILE-TRUENAME CFILE2)))

(DEFSUBST SAME-NAME-AND-CANONICAL-TYPE? (CFILE1 CFILE2)
  (MATCHING-NAME-AND-TYPE? (CFILE-TRUENAME CFILE1)
                           (CFILE-TRUENAME CFILE2)
                           T))

(DEFSUBST SAME-VERSION? (CFILE1 CFILE2)
  (EQUAL (CFILE-VERSION CFILE1)
         (CFILE-VERSION CFILE2)))

(DEFSUBST SAME-CREATION-DATE? (CFILE1 CFILE2)
  (= (CFILE-CREATION-DATE CFILE1)
     (CFILE-CREATION-DATE CFILE2)))

(DEFUN SAME-NAME-TYPE-AND-VERSION? (CFILE1 CFILE2)
  (AND (SAME-NAME-AND-TYPE? CFILE1 CFILE2)
       (SAME-VERSION? CFILE1 CFILE2)))

;;;; CDirectory Abstraction

(DEFSTRUCT (CDIRECTORY (:TYPE :NAMED-ARRAY) :CONC-NAME
                       (:PRINT "#<~S ~S>" (TYPE-OF CDIRECTORY) (%POINTER CDIRECTORY)))
  PATHNAME                                      ;Specification of directory.
  TRUENAME                                      ;Pathname, with wildcards inserted.
  PLIST                                         ;Directory-List information.
  SUBDIRECTORIES                                ;Subdirectory plists.
  CFILES                                        ;Component files.
  )

(DEFUN CREATE-CDIRECTORY (PATHNAME REMOVABLE-NAMES ERROR-P)
  (LET* ((WILDCARDED-PATHNAME (MAKE-DIRECTORY-LIST-PATHNAME PATHNAME))
         (DIRECTORY (IF (NOT ERROR-P)
                        (DIRECTORY-LIST WILDCARDED-PATHNAME ':SORTED ':NOERROR)
                        (DIRECTORY-LIST WILDCARDED-PATHNAME ':SORTED))))
    (IF (ERRORP DIRECTORY)
        DIRECTORY
        (LET ((SUBDIRS '())
              (CFILES '()))
          (DOLIST (CFILE (CDR DIRECTORY))
            (COND ((CFILE-IS-SUBDIRECTORY? CFILE)
                   (PUSH CFILE SUBDIRS))
                  ((NOT (DOLIST (PATHNAME REMOVABLE-NAMES)
                          (IF (MATCHING-PATHNAMES-WITH-WILD? (CFILE-TRUENAME CFILE) PATHNAME)
                              (RETURN T))))
                   (PUSH CFILE CFILES))))
          (LET ((CDIRECTORY (MAKE-CDIRECTORY PATHNAME PATHNAME
                                             TRUENAME WILDCARDED-PATHNAME
                                             PLIST (CAR DIRECTORY)
                                             SUBDIRECTORIES (NREVERSE SUBDIRS)
                                             CFILES (NREVERSE CFILES))))
            (PUTPROP (CAR DIRECTORY) CDIRECTORY 'ASSOCIATED-CDIRECTORY)
            CDIRECTORY)))))

(DEFUN CREATE-FAKE-CDIRECTORY (PATHNAME)
  (MAKE-CDIRECTORY PATHNAME PATHNAME
                   TRUENAME PATHNAME
                   PLIST NIL
                   SUBDIRECTORIES NIL
                   CFILES NIL))

(DEFUN CDIRECTORY-LIST (DIRECTORY)
  (CONS (CDIRECTORY-PLIST DIRECTORY)
        (CDIRECTORY-CFILES DIRECTORY)))

(DEFUN ASSOCIATED-CDIRECTORY (DIRECTORY-PLIST)
  (GET DIRECTORY-PLIST 'ASSOCIATED-CDIRECTORY))

(DEFUN MAKE-DIRECTORY-LIST-PATHNAME (PATHNAME)
  (FAST-NEW-PATHNAME PATHNAME
                     ;; DIRECTORY-LIST should hack the device/directory being wild.
                     (PATHNAME-DEVICE PATHNAME)
                     (PATHNAME-DIRECTORY PATHNAME)
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-NAME PATHNAME))
                          ':WILD)
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-TYPE PATHNAME))
                          ':WILD)
                     (AND (UNSPECIFIED-PATHNAME-COMPONENT? (PATHNAME-VERSION PATHNAME))
                          ':NEWEST)))

;;;; IsoSet Abstraction

;;; A set of functions for manipulating sets and equivalence classes sorted
;;; by means of an isomorphism into their intersection/difference parts.

(DEFSTRUCT (ISOSET (:TYPE :LIST) :CONC-NAME)
  IN-1-ONLY                                     ;difference set1 set2
  IN-BOTH                                       ;intersection set1 set2
  IN-2-ONLY                                     ;difference set2 set1
  )

(DEFUN SORT-ISOMORPHIC-CLASSES (ISOMORPHISM CROSS-ISOMORPHISM SET1 SET2)
  "Given an isomorphism and two sets, return three values:
1.  A list of equivalence classes from SET1 that were not isomorphic
    to any element in SET2.
2.  A list of pairs of isomorphic equivalence classes; car=SET1, cdr=SET2.
3.  A list of equivalence classes from SET2 that were not isomorphic
    to any element in SET1.
ISOMORPHISM is used to make equivalence classes, while
CROSS-ISOMORPHISM is used to compare an element of SET1 with one of SET2."
  (SORT-ISOMORPHIC-ELEMENTS (LET-CLOSED ((ISOMORPHISM CROSS-ISOMORPHISM))
                              #'(LAMBDA (CLASS1 CLASS2)
                                  (FUNCALL ISOMORPHISM (CAR CLASS1) (CAR CLASS2))))
                            (EQUIVALENCE-CLASSES-OF ISOMORPHISM SET1)
                            (EQUIVALENCE-CLASSES-OF ISOMORPHISM SET2)))

(DEFUN EQUIVALENCE-CLASSES-OF (ISOMORPHISM SET)
  "Give a set and an isomorphism, return the set of equivalence-classes."
  (LET ((EQV-CLASSES '()))
    (DOLIST (ITEM SET)
      (DO ((SUBSET EQV-CLASSES (CDR SUBSET)))
          ((NULL SUBSET)
           (PUSH (LIST ITEM) EQV-CLASSES))
        (COND ((FUNCALL ISOMORPHISM ITEM (CAAR SUBSET))
               (PUSH ITEM (CAR SUBSET))
               (RETURN T)))))
    (DO ((SUBSET EQV-CLASSES (CDR SUBSET)))
        ((NULL SUBSET) (NREVERSE EQV-CLASSES))
      (RPLACA SUBSET (NREVERSE (CAR SUBSET))))))

(DEFUN SORT-ISOMORPHIC-ELEMENTS (ISOMORPHISM SET1 SET2)
  "Given an isomorphism and two sets, return three values:
1.  A list of elements from SET1 that were not isomorphic
    to any element in SET2.
2.  A list of pairs of isomorphic elements; car=SET1, cdr=SET2.
3.  A list of elements from SET2 that were not isomorphic
    to any element in SET1.
It is assumed that no element in an input set is isomorphic to
any other element in the same set.  If this is possible, use
SORT-ISOMORPHIC-CLASSES instead."
  (LET ((ELEMENTS-IN-SET1-ONLY '())
        (ELEMENTS-IN-SET2-ONLY (COPYLIST SET2))
        (ELEMENTS-IN-BOTH-SETS '()))
    (DOLIST (ELEMENT-IN-SET1 SET1)
      (LET ((ISOMORPHIC-ELEMENT
              (DOLIST (ELEMENT-IN-SET2 ELEMENTS-IN-SET2-ONLY)
                (IF (FUNCALL ISOMORPHISM ELEMENT-IN-SET1 ELEMENT-IN-SET2)
                    (RETURN ELEMENT-IN-SET2)))))
        (COND ((NOT (NULL ISOMORPHIC-ELEMENT))
               (PUSH (CONS ELEMENT-IN-SET1 ISOMORPHIC-ELEMENT)
                     ELEMENTS-IN-BOTH-SETS)
               (SETQ ELEMENTS-IN-SET2-ONLY
                     (DELQ ISOMORPHIC-ELEMENT
                           ELEMENTS-IN-SET2-ONLY)))
              (T
               (PUSH ELEMENT-IN-SET1 ELEMENTS-IN-SET1-ONLY)))))
    (MAKE-ISOSET IN-1-ONLY (NREVERSE ELEMENTS-IN-SET1-ONLY)
                 IN-BOTH (NREVERSE ELEMENTS-IN-BOTH-SETS)
                 IN-2-ONLY ELEMENTS-IN-SET2-ONLY)))

;;;; Transfer Operators

#+SYMBOLICS                                     ;Already one in MIT system.
(DEFCONST *COPY-FILE-KNOWN-TEXT-TYPES* '("LISP" "TEXT")
  "Files whose names have these type fields are normally copied as characters.")

#+SYMBOLICS                                     ;Already one in MIT system.
(DEFCONST *COPY-FILE-KNOWN-BINARY-TYPES* '("QFASL" "QBIN" "FASL")
  "Files whose names have these type fields are normally copied as binary.")

(DEFUN SETUP-CDIRECTORY-TRANSFER-MODE (DIRECTORY &OPTIONAL COPY-MODE)
  (DOLIST (CFILE (CDIRECTORY-CFILES DIRECTORY))
    (IF (AND (NOT (NULL (CFILE-TRANSFER-DESTINATIONS CFILE)))
             (NULL (CFILE-TRANSFER-MODE CFILE)))
        (SETF (CFILE-TRANSFER-MODE CFILE)
              (LET ((BYTE-SIZE (CFILE-BYTE-SIZE CFILE))
                    (INTYPE (CFILE-CANONICAL-TYPE CFILE))
                    (TRUENAME (CFILE-TRUENAME CFILE)))
                (COND ((MEMQ COPY-MODE '(:CHARACTERS BINARY)) COPY-MODE)
                      ((EQ COPY-MODE ':ASK)
                       (IF (FQUERY NIL "~&Is ~A a text file? " TRUENAME)
                           ':CHARACTERS
                           ':BINARY))
                      ((MEMQ BYTE-SIZE '(7 8)) ':CHARACTERS)
                      ((EQ BYTE-SIZE 16.) ':BINARY)
                      ((MEMBER INTYPE *COPY-FILE-KNOWN-TEXT-TYPES*) ':CHARACTERS)
                      ((MEMBER INTYPE *COPY-FILE-KNOWN-BINARY-TYPES*) ':BINARY)
                      ((EQ COPY-MODE ':NEVER-ASK) ':CHARACTERS)
                      ((FQUERY '(:BEEP T) "~&Is ~A a text file? " TRUENAME)
                       ':CHARACTERS)
                      (T ':BINARY)))))))

(DEFUN MARK-CFILE-FOR-TRANSFER (CFILE TARGET-DIRECTORY &OPTIONAL IGNORE)
  (LET ((CURRENT-BINDING (ASSQ TARGET-DIRECTORY (CFILE-TRANSFER-DESTINATIONS CFILE)))
        (TARGET-PATHNAME
          (DEFAULT-WILD-PATHNAME-COMPONENTS (CDIRECTORY-PATHNAME TARGET-DIRECTORY)
                                            (CFILE-TRUENAME CFILE))))
    (IF (NULL CURRENT-BINDING)
        (PUSH (CONS TARGET-DIRECTORY TARGET-PATHNAME)
              (CFILE-TRANSFER-DESTINATIONS CFILE))
        (RPLACD CURRENT-BINDING TARGET-PATHNAME))))

(DEFVAR CDIRECTORY-TRANSFER-QUERY-CHOICES
        `(,@FORMAT:Y-OR-N-P-CHOICES
          ((:PROCEED "Proceed.") #/P #\HAND-RIGHT)))

(DEFUN CDIRECTORY-TRANSFER-QUERY (DIRECTORY)
  (*CATCH 'CDIRECTORY-TRANSFER-QUERY-PROCEEDED
    (DOLIST (CFILE (CDIRECTORY-CFILES DIRECTORY))
      (LET ((TRUENAME (CFILE-TRUENAME CFILE)))
        (DOLIST (TARGET-PAIR (CFILE-TRANSFER-DESTINATIONS CFILE))
          (LET ((QRESULT (FQUERY `(:CHOICES ,CDIRECTORY-TRANSFER-QUERY-CHOICES)
                                 "~&Copy ~A to ~A? "
                                 TRUENAME
                                 (CDR TARGET-PAIR))))
            (COND ((NULL QRESULT)
                   (SETF (CFILE-TRANSFER-DESTINATIONS CFILE)
                         (DELQ TARGET-PAIR (CFILE-TRANSFER-DESTINATIONS CFILE))))
                  ((EQ QRESULT ':PROCEED)
                   (*THROW 'CDIRECTORY-TRANSFER-QUERY-PROCEEDED T)))))))))

(DEFUN CDIRECTORY-TRANSFER (DIRECTORY &OPTIONAL (ERROR-P T))
  (DOLIST (CFILE (CDIRECTORY-CFILES DIRECTORY))
    (DO () (())
      (LET ((DESTINATIONS (CFILE-TRANSFER-DESTINATIONS CFILE)))
        (IF (NULL (CFILE-TRANSFER-DESTINATIONS CFILE)) (RETURN T))
        (LET ((TARGET-PAIR (CAR DESTINATIONS)))
          (CFILE-TRANSFER-1 CFILE (CDR TARGET-PAIR) (CAR TARGET-PAIR) ERROR-P)
          (SETF (CFILE-TRANSFER-DESTINATIONS CFILE) (CDR DESTINATIONS)))))))

(DEFUN CFILE-TRANSFER-1 (CFILE DESTINATION TARGET-DIRECTORY ERROR-P)
  (LET ((CHARACTERS? (EQ (CFILE-TRANSFER-MODE CFILE) ':CHARACTERS)))
    (LET ((RESULT (CFILE-TRANSFER-2 CFILE DESTINATION CHARACTERS?
                                    TARGET-DIRECTORY ERROR-P)))
      (IF (ERRORP RESULT)
          (FORMAT T "~&Copy Error: ~A" RESULT)
          (FORMAT T "~&Copied ~A to ~A (~:[binary~;text~])"
                  (CFILE-TRUENAME CFILE)
                  DESTINATION
                  CHARACTERS?)))))

(DEFUN CFILE-TRANSFER-2 (INPUT-CFILE OUTPUT-TRUENAME CHARACTERS?
                         TARGET-DIRECTORY ERROR-P)
  TARGET-DIRECTORY                              ;Use this to decide which props to change.
  (WITH-OPEN-FILE (INSTREAM (CFILE-TRUENAME INPUT-CFILE)
                            ':DIRECTION ':INPUT
                            ':CHARACTERS CHARACTERS?
                            ':ERROR ERROR-P)
    (IF (ERRORP INSTREAM)
        INSTREAM
        (WITH-OPEN-FILE (OUTSTREAM OUTPUT-TRUENAME
                                   ':DIRECTION ':OUTPUT
                                   ':CHARACTERS CHARACTERS?
                                   ':ERROR ERROR-P)
          (COND ((ERRORP OUTSTREAM)
                 OUTSTREAM)
                (T
                 (SEND OUTSTREAM ':CHANGE-PROPERTIES NIL
                       ':CREATION-DATE (CFILE-CREATION-DATE INPUT-CFILE)
                       ':AUTHOR (CFILE-AUTHOR INPUT-CFILE))
                 (STREAM-COPY-UNTIL-EOF INSTREAM OUTSTREAM)
                 (CLOSE OUTSTREAM)))))))

;;;; Directory Comparison

(DEFUN COMPARE-CDIRECTORIES (CDIR1 CDIR2 &OPTIONAL TAPE-LOSSAGE)
  (LET ((ISOCLASSES (SORT-ISOMORPHIC-CLASSES 'SAME-NAME-AND-TYPE?
                                             'SAME-NAME-AND-CANONICAL-TYPE?
                                             (CDIRECTORY-CFILES CDIR1)
                                             (CDIRECTORY-CFILES CDIR2)))
        (NEWEST? (NOT (OR (EQ (PATHNAME-VERSION (CDIRECTORY-PATHNAME CDIR1)) ':WILD)
                          (EQ (PATHNAME-VERSION (CDIRECTORY-PATHNAME CDIR2)) ':WILD)))))
    (DOLIST (CLASS (ISOSET-IN-1-ONLY ISOCLASSES))
      (COMPARE-CDIRECTORIES-1 CLASS CDIR2 NEWEST? TAPE-LOSSAGE))
    (DOLIST (CLASS (ISOSET-IN-2-ONLY ISOCLASSES))
      (COMPARE-CDIRECTORIES-1 CLASS CDIR1 NEWEST? TAPE-LOSSAGE))
    (DOLIST (CLASS-PAIR (ISOSET-IN-BOTH ISOCLASSES))
      (LET ((REP1 (CLASS-REPRESENTATIVE (CAR CLASS-PAIR)))
            (REP2 (CLASS-REPRESENTATIVE (CDR CLASS-PAIR)))
            (ISOSET (SORT-ISOMORPHIC-ELEMENTS #'SAME-VERSION?
                                              (CAR CLASS-PAIR)
                                              (CDR CLASS-PAIR))))
        (COND (NEWEST?
               (LET ((V1 (CFILE-VERSION REP1))
                     (CD1 (CFILE-CREATION-DATE REP1))
                     (V2 (CFILE-VERSION REP2))
                     (CD2 (CFILE-CREATION-DATE REP2)))
                 (COND ((OR (NOT (FIXP V1))
                            (NOT (FIXP V2)))
                        (COND ((> CD1 CD2)
                               (MARK-CFILE-FOR-TRANSFER REP1 CDIR2 TAPE-LOSSAGE))
                              ((> CD2 CD1)
                               (MARK-CFILE-FOR-TRANSFER REP2 CDIR1 TAPE-LOSSAGE))))
                       ((> V1 V2)
                        (MARK-CFILE-FOR-TRANSFER REP1 CDIR2 TAPE-LOSSAGE))
                       ((> V2 V1)
                        (MARK-CFILE-FOR-TRANSFER REP2 CDIR1 TAPE-LOSSAGE))
                       ((> CD1 CD2)
                        (MARK-CFILE-FOR-TRANSFER REP1 CDIR2 TAPE-LOSSAGE))
                       ((> CD2 CD1)
                        (MARK-CFILE-FOR-TRANSFER REP2 CDIR1 TAPE-LOSSAGE)))))
              (T
               (DOLIST (CFILE (ISOSET-IN-1-ONLY ISOSET))
                 (MARK-CFILE-FOR-TRANSFER CFILE CDIR2 TAPE-LOSSAGE))
               (DOLIST (CFILE (ISOSET-IN-2-ONLY ISOSET))
                 (MARK-CFILE-FOR-TRANSFER CFILE CDIR1 TAPE-LOSSAGE))))))))

(DEFUN COMPARE-CDIRECTORIES-1 (CLASS TARGET-DIRECTORY NEWEST? TAPE-LOSSAGE)
  (LET ((REP (CLASS-REPRESENTATIVE CLASS)))
    (COND (NEWEST?
           (MARK-CFILE-FOR-TRANSFER REP TARGET-DIRECTORY TAPE-LOSSAGE))
          (T
           (DOLIST (CFILE CLASS)
             (MARK-CFILE-FOR-TRANSFER CFILE TARGET-DIRECTORY TAPE-LOSSAGE))))))

(DEFUN CLASS-REPRESENTATIVE (CFILE-CLASS)
  (LET ((REP (FIND-NEWEST-CFILE CFILE-CLASS)))
    (DOLIST (CFILE CFILE-CLASS)
      (SETF (CFILE-CLASS-REPRESENTATIVE CFILE) REP))
    (SETF (CFILE-REPRESENTED-CLASS REP) CFILE-CLASS)
    REP))

(DEFUN FIND-NEWEST-CFILE (CFILES)
  (LET ((NEWEST NIL) (NV 0))
    (DOLIST (CFILE CFILES)
      (LET ((V (CFILE-VERSION CFILE)))
        (COND ((NOT (FIXP V))
               (SETQ NEWEST CFILE)
               (RETURN NIL))
              ((> V NV)
               (SETQ NEWEST CFILE NV V)))))
    NEWEST))

;;;; Consistency Checks
;;; reintegrate this stuff later.

(COMMENT

(DEFUN CONSISTENCY-CHECK-COPY-LIST (COPY-LIST)
  (CONSISTENCY-CHECK-DIRECTORY-CLASSES (COPY-LIST-ISOCLASSES COPY-LIST)))

(DEFUN REPORT-INCONSISTENCY (INCONSISTENCY)
  (FORMAT T "~&Version//Date conflict:~%~A ~A ~A~%~A ~A ~A"
          (CFILE-TRUENAME (CAR INCONSISTENCY))
          (TIME:PRINT-UNIVERSAL-TIME (CFILE-CREATION-DATE (CAR INCONSISTENCY)) NIL)
          (CFILE-AUTHOR (CAR INCONSISTENCY))
          (CFILE-TRUENAME (CDR INCONSISTENCY))
          (TIME:PRINT-UNIVERSAL-TIME (CFILE-CREATION-DATE (CDR INCONSISTENCY)) NIL)
          (CFILE-AUTHOR (CDR INCONSISTENCY))))

(DEFUN CONSISTENCY-CHECK-DIRECTORY-CLASSES (ISOCLASSES)
  "Given sorted directory classes, determine if any inconsistencies
exist between version and creation-date information.  This is a simple
way to help detect readers-writers bugs."

  (NCONC (MAPCAN #'CONSISTENCY-CHECK-CLASS (ISOSET-IN-1-ONLY ISOCLASSES))
         (MAPCAN #'CONSISTENCY-CHECK-CLASS (ISOSET-IN-2-ONLY ISOCLASSES))
         (MAPCAN #'(LAMBDA (PAIR)
                     (NCONC (CONSISTENCY-CHECK-CLASS (CAR PAIR))
                            (CONSISTENCY-CHECK-CLASS (CDR PAIR))
                            (CONSISTENCY-CHECK-BETWEEN-CLASSES (CAR PAIR) (CDR PAIR))))
                 (ISOSET-IN-BOTH ISOCLASSES))))

(DEFUN CONSISTENCY-CHECK-CLASS (CLASS)
  (LET ((INCONSISTENCIES '()))
    (DO ((SUBSET CLASS (CDR SUBSET)))
        ((NULL SUBSET))
      (DOLIST (ITEM (CDR SUBSET))
        (IF (NOT (CONSISTENT-CFILES? (CAR SUBSET) ITEM))
            (PUSH (CONS (CAR SUBSET) ITEM) INCONSISTENCIES))))
    (REVERSE INCONSISTENCIES)))

(DEFUN CONSISTENCY-CHECK-BETWEEN-CLASSES (CLASS1 CLASS2)
  (LET ((INCONSISTENCIES '()))
    (DOLIST (ITEM1 CLASS1)
      (DOLIST (ITEM2 CLASS2)
        (IF (NOT (CONSISTENT-CFILES? ITEM1 ITEM2))
            (PUSH (CONS ITEM1 ITEM2) INCONSISTENCIES))))
    (REVERSE INCONSISTENCIES)))

(DEFUN CONSISTENT-CFILES? (CFILE1 CFILE2)
  (LET ((V1 (CFILE-VERSION CFILE1))
        (V2 (CFILE-VERSION CFILE2))
        (C1 (CFILE-CREATION-DATE CFILE1))
        (C2 (CFILE-CREATION-DATE CFILE2)))
    (AND (EQ (< V1 V2) (< C1 C2))
         (EQ (= V1 V2) (= C1 C2)))))
)

;;;; Balance Two Directories
;;; A user level function that ties together everything.

(DEFUN BALANCE-DIRECTORIES (DIRSPEC1 DIRSPEC2
                            &KEY &OPTIONAL ((:IGNORE REMOVABLE-NAMES))
                            DIRECTION QUERY-MODE COPY-MODE
                            (DEFAULTS "*.*#>") (ERROR T))
                            ;IGNORE-DESTINATION-DIRECTORY

  "Balance the files on two different directories.  Files are compared
by name and type.  The files in each directory are compared, and files
are copied so that both directories have the newest version of every
file.  The keywords are:

:IGNORE <list> is a list of pathname specifications which should
    be ignored.  These specs are parsed relative to each directory [default: '()].

:DIRECTION <flag> specifies which way to transfer:
    NIL == both ways [the default].
    :1->2 or :2->1 == only the specified way.

:QUERY-MODE <flag> has the values:
    NIL == don't ever ask [the default].
    :1->2 == ask about any files transferred from DIRSPEC2 to DIRSPEC1 only.
    :2->1 == ask about any files transferred from DIRSPEC1 to DIRSPEC2 only.
    :ALWAYS == ask about any files transferred.

:COPY-MODE <flag> has the values:
    :CHARACTERS == copy all files as characters.
    :BINARY == copy all files as binary data.
    :ASK == ask about each file.
    :NEVER-ASK == try to figure out which mode, defaulting to characters.
    NIL == try to figure out which mode, asking if can't [the default].

:DEFAULTS <defaults> is the DEFAULTS argument to FS:MERGE-PATHNAME-DEFAULTS
    when it is applied to DIRSPEC1. The result of that application is in
    turn used as the defaults argument to fs:merge-pathname-defaults when
    it is applied to dirspec2. [default: /"*.*#>/"].


:ERROR <boolean> if false, don't cause errors, just print string [default: T]."

;":IGNORE-DESTINATION-DIRECTORY <boolean> if true considers destination directory
;    to be null for balancing purposes.  :DIRECTION must be non null! Use for magtape."
;
;  (if (and ignore-destination-directory (null direction))
;      (ferror nil "direction must be non-NIL if ignore-destination-directory is non-NIL"))
  (CHECK-ARG DIRECTION (MEMQ DIRECTION '(NIL :1->2 :2->1)) "NIL, :1->2 or :2->1")
  (CHECK-ARG QUERY-MODE (MEMQ QUERY-MODE '(NIL :ALWAYS :1->2 :2->1))
             "NIL, :ALWAYS, :1->2 or :2->1")
  (CHECK-ARG COPY-MODE (MEMQ COPY-MODE '(:CHARACTERS :BINARY :ASK :NEVER-ASK NIL))
             ":CHARACTERS, :BINARY, :ASK, :NEVER-ASK or NIL")
  (SETQ REMOVABLE-NAMES
        (MAPCAR 'PARSE-PATHNAME REMOVABLE-NAMES))
  (LET* ((DIRPATH1 (MERGE-PATHNAME-DEFAULTS DIRSPEC1 DEFAULTS NIL NIL))
         (DIR1 (CREATE-CDIRECTORY DIRPATH1 REMOVABLE-NAMES ERROR))
         (DIRPATH2 (MERGE-PATHNAME-DEFAULTS DIRSPEC2 DIRPATH1 NIL NIL))
         (DIR2 (CREATE-CDIRECTORY DIRPATH2 REMOVABLE-NAMES ERROR)))
    (COND ((ERRORP DIR1)
           DIR1)
          ((ERRORP DIR2)
           DIR2)
          (T
           (COMPARE-CDIRECTORIES DIR1 DIR2)
           (IF (MEMQ QUERY-MODE '(:ALWAYS :2->1))
               (CDIRECTORY-TRANSFER-QUERY DIR1))
           (IF (MEMQ QUERY-MODE '(:ALWAYS :1->2))
               (CDIRECTORY-TRANSFER-QUERY DIR2))
           (SETUP-CDIRECTORY-TRANSFER-MODE DIR1 COPY-MODE)
           (SETUP-CDIRECTORY-TRANSFER-MODE DIR2 COPY-MODE)
           (WHEN (MEMQ DIRECTION '(NIL :1->2))
             (CDIRECTORY-TRANSFER DIR1))
           (WHEN (MEMQ DIRECTION '(NIL :2->1))
             (CDIRECTORY-TRANSFER DIR2))
           (LIST DIR1 DIR2)))))

(DEFUN BALANCE-TO-NON-DIRECTORY-DEVICE (DIRSPEC1 TO-DEV
                                                 &KEY &OPTIONAL ((:IGNORE REMOVABLE-NAMES))
                                                 QUERY-MODE COPY-MODE
                                                 (DEFAULTS *DEFAULT-PATHNAME-DEFAULTS*) (ERROR T))
  "Balance the files on a directory to tape.  Selects most recent versions
and supports ':IGNORE option.  The keywords are:

:IGNORE <list> is a list of pathname specifications which should
    be ignored.  These specs are parsed relative to each directory [default: '()].

:QUERY-MODE <flag> has the values:
    NIL == don't ever ask [the default].
    :1->2 == ask about any files transferred from DIRSPEC2 to DIRSPEC1 only.
    :2->1 == ask about any files transferred from DIRSPEC1 to DIRSPEC2 only.
    :ALWAYS == ask about any files transferred.

:COPY-MODE <flag> has the values:
    :CHARACTERS == copy all files as characters.
    :BINARY == copy all files as binary data.
    :ASK == ask about each file.
    :NEVER-ASK == try to figure out which mode, defaulting to characters.
    NIL == try to figure out which mode, asking if can't [the default].

:DEFAULTS <defaults> is a defaults argument for FS:MERGE-PATHNAME-DEFAULTS
    [default: FS:*DEFAULT-PATHNAME-DEFAULTS*].

:ERROR <boolean> if false, don't cause errors, just print string [default: T]."

  (LET* ((DIRPATH1 (MERGE-PATHNAME-DEFAULTS DIRSPEC1 DEFAULTS NIL NIL))
         (DIR1 (CREATE-CDIRECTORY DIRPATH1 REMOVABLE-NAMES ERROR))
         (DIR2 (CREATE-FAKE-CDIRECTORY TO-DEV)))
    (COND ((ERRORP DIR1)
           DIR1)
          (T
           (COMPARE-CDIRECTORIES DIR1 DIR2 T)
           (IF (MEMQ QUERY-MODE '(:ALWAYS :1->2))
               (CDIRECTORY-TRANSFER-QUERY DIR2))
           (SETUP-CDIRECTORY-TRANSFER-MODE DIR1 COPY-MODE)
           (SETUP-CDIRECTORY-TRANSFER-MODE DIR2 COPY-MODE)
           (CDIRECTORY-TRANSFER DIR1)
           (LIST DIR1 DIR2)))))
