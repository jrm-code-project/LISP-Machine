;;; -*-Mode:LISP; Base:8; Package:USER -*-

(SPECIAL READTABLE STANDARD-INPUT READ-COMPARE-OBJECT READ-COMPARE-STOP READ-COMPARE-STRING)

;This is like READ-COMPARE, but eats the terminating space after an atom.
(DEFUN READ-COMPARE-FOR-TOP-LEVEL (OBJECT PLACE-TO-STOP &AUX CH)
  (PROG1 (READ-COMPARE OBJECT STANDARD-INPUT)
         (AND (MEMQ ':LISTEN (FUNCALL STANDARD-INPUT ':WHICH-OPERATIONS))
              (SETQ CH (FUNCALL STANDARD-INPUT ':LISTEN))
              (OR (>= CH (RDTBL-ARRAY-SIZE))
                  (NOT (ZEROP (LOGAND 1 (RDTBL-BITS READTABLE CH)))))
              (FUNCALL STANDARD-INPUT ':TYI))))  ;Eat the space or other useless character

;READ-COMPARE reads from a stream comparing against a supplied object.
;When we reach the place in the object specified by READ-COMPARE-STOP
;(which should be a CONS, since a symbol can occur in many places)
;READ-COMPARE returns, leaving the input stream positioned to the text
;for that object.
;READ-COMPARE's value is NIL if the text and object do not match,
;T if they do but the place to stop was not encountered,
;or STOP if READ-COMPARE is returning having reached the stopping place.
(DEFUN READ-COMPARE (OBJECT READ-COMPARE-STOP &OPTIONAL (STREAM STANDARD-INPUT))
    (OR (BOUNDP 'READ-COMPARE-STRING)
        (SETQ READ-COMPARE-STRING
              (MAKE-ARRAY 100 ':TYPE ART-STRING ':LEADER-LENGTH 1)))
    (CATCH
       (READ-COMPARE-INTERNAL OBJECT STREAM)
       READ-COMPARE))

;Read compare one object, processing nonsplicing macros,
;and handling splicing macros only if they don't match anything (like ;).
(DEFUN READ-COMPARE-INTERNAL (OBJECT STREAM)
    (PROG (THING TYPE SPLICEP)
        A (MULTIPLE-VALUE (THING TYPE) (READ-COMPARE-THING OBJECT STREAM))
        (COND ((EQ TYPE 'READER-MACRO)
               (MULTIPLE-VALUE (THING TYPE SPLICEP)
                               (FUNCALL THING OBJECT NIL STREAM))
               (AND SPLICEP (GO A))
               (RETURN T))
              ((EQ TYPE 'SPECIAL-TOKEN)
               (FERROR NIL "The special token ~S was read in where not expected" THING))
              (T (RETURN T)))))

;READ-COMPARE-THING is analogous to XR-READ-THING.
;It is passed an object to compare and a stream to read from.
;The readtable finite state machine is used to gobble a string from the stream
;(using READ-COMPARE-STRING, which is reused each time).
;The final state of the FSM determines what function to call to
;compare the string (and perhaps more text which follows) with the object;
;except that some things are SPECIAL-TOKENs which just return
;the two values <token-name> and SPECIAL-TOKEN to the caller.
;In that case, nothing has been matched, and the caller must take special action.
;Sometimes we detect a read macro.  In that case, we return
;the function to call and READ-MACRO.  Normally, we (that is, the
;comparison function) return T for succesful match, or throw to READ-COMPARE.

;When a read macro is processed by our caller, it should be passed these
;arguments:  first, an object to match, if the macro is nonsplicing.
;Second, the list of the remaining objects to match, in case the
;macro is splicing.  Third, the stream.
;The macro handler returns two values.  The second is whether it was
;splicing.  The first, in the case that it was splicing,
;is the list of things not matched (a tail of the original second argument).
;If the macro doesn't match, it throws, so there is no need to worry about that.

;Splicing macros, and nonsplicing ones which "do consing",
;must watch for READ-COMPARE-STOP and throw if it is encountered.

(DEFUN READ-COMPARE-THING (READ-COMPARE-OBJECT STREAM)
       (PROG (CH NUM A B (STRING READ-COMPARE-STRING) REAL-CH
                 (READTABLE-FSM (RDTBL-FSM READTABLE))
                 (FNPROP (GET (CONS NIL (RDTBL-PLIST READTABLE))
                              'READ-COMPARE-FUNCTION-PROPERTY))
                 (STATE (RDTBL-STARTING-STATE READTABLE)))
             (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI-WHITE-OUT STREAM))
             (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
             (SETF (ARRAY-LEADER STRING 0) 0)
             (COND ((NOT (NUMBERP STATE))
                    (LET ((FLAG (CAR STATE))
                          (TODO (CDR STATE)))
                         (SELECTQ FLAG
                                  (NO-UNTYI-QUOTE
                                   (RETURN TODO 'SPECIAL-TOKEN))
                                  (LAST-CHAR
                                   (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP)
                                                                  STREAM NIL REAL-CH))
                                   (RETURN A B))
                                  (NO-UNTYI-FUNCTION
                                   (ARRAY-PUSH STRING CH)
                                   (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP)
                                                                  STREAM STRING))
                                   (RETURN A B))
                                  ((UNTYI-QUOTE UNTYI-FUNCTION)
                                   (FERROR NIL
                                     "Reader in infinite loop reading character: /"~C/""
                                     REAL-CH))
                                  (OTHERWISE
                                   (FERROR NIL
                                     "The reader found ~S in the finite state machine"
                                     FLAG))))))
           L (ARRAY-PUSH-EXTEND STRING CH)
             (MULTIPLE-VALUE (CH NUM REAL-CH) (XR-XRTYI STREAM))
             (SETQ STATE (AR-2 READTABLE-FSM STATE NUM))
             (COND ((NUMBERP STATE)
                    (GO L)))
             (LET ((FLAG (CAR STATE))
                   (TODO (CDR STATE)))
                  (SELECTQ FLAG
                           (UNTYI-FUNCTION
                            (XR-XRUNTYI STREAM REAL-CH NUM)
                            (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP) STREAM STRING))
                            (RETURN A B))
                           (LAST-CHAR
                            (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP)
                                                           STREAM STRING REAL-CH))
                            (RETURN A B))
                           (NO-UNTYI-FUNCTION
                            (ARRAY-PUSH-EXTEND STRING CH)
                            (MULTIPLE-VALUE (A B) (FUNCALL (GET TODO FNPROP) STREAM STRING))
                            (RETURN A B))
                           (UNTYI-QUOTE
                            (XR-XRUNTYI STREAM REAL-CH NUM)
                            (RETURN TODO 'SPECIAL-TOKEN))
                           (NO-UNTYI-QUOTE
                            (RETURN TODO 'SPECIAL-TOKEN))
                           (OTHERWISE
                            (FERROR NIL
                              "The reader found ~S in the finite state machine"
                              FLAG))))))

;This ends the reader proper. The things from here on are called only if they appear in
;the readtable itself. Although this is somewhat special in that it handles splicing
;macros. Note that the second arg (FIFTY) should be a number (50) rather than a string ("(")
;due to the LAST-CHAR hack.
(DEFUN (LIST STANDARD-READ-COMPARE-FUNCTION) (STREAM IGNORE FIFTY)
       (PROG ((LIST READ-COMPARE-OBJECT) THING TYPE SPLICEP)
             A
             ;; Handle the end of the list.
             (COND ((ATOM LIST)
                    (MULTIPLE-VALUE (THING TYPE) (READ-COMPARE-THING NIL STREAM))
                    ;; Allow a splicing macro there.
                    (COND ((EQ TYPE 'READER-MACRO)
                           (MULTIPLE-VALUE (THING SPLICEP)
                                           (FUNCALL THING NIL LIST STREAM))
                           (COND ((NOT SPLICEP) (THROW NIL READ-COMPARE)))
                           (SETQ LIST THING)
                           (GO A)))
                    ;; Aside from that, we must see a ")" or a ".".
                    (OR (EQ TYPE 'SPECIAL-TOKEN) (THROW NIL READ-COMPARE))
                    (AND (EQ THING 'CONSING-DOT) (GO RDOT))
                    (AND (NULL LIST) (EQ THING 'CLOSE)
                         (RETURN T))
                    (THROW NIL READ-COMPARE)))
             (AND (EQ LIST READ-COMPARE-STOP)
                  (THROW 'STOP READ-COMPARE))
             ;; Match the next element of the list.
             (MULTIPLE-VALUE (THING TYPE) (READ-COMPARE-THING (CAR LIST) STREAM))
             (COND ((EQ TYPE 'READER-MACRO)
                    (MULTIPLE-VALUE (THING SPLICEP)
                                    (FUNCALL THING (CAR LIST) LIST STREAM))
                    (COND (SPLICEP (SETQ LIST THING) (GO A))))
                   ((EQ TYPE 'SPECIAL-TOKEN)
                    (COND ((EQ THING 'CONSING-DOT)
                           (GO RDOT))
                          ((EQ THING 'CLOSE)
                           (THROW NIL READ-COMPARE)
                          (T (FERROR NIL
                                 "The special token ~S was read in the middle of a list"
                                 THING))))))
             (SETQ LIST (CDR LIST))
             (GO A)

        RDOT (MULTIPLE-VALUE (THING TYPE) (READ-COMPARE-THING LIST STREAM))
             (AND (EQ TYPE 'SPECIAL-TOKEN)
                  (FERROR NIL
                    "The special token ~S was read after a dot"
                    THING))
             (COND ((EQ TYPE 'READER-MACRO)
                    (MULTIPLE-VALUE (THING TYPE SPLICEP)
                                    (FUNCALL THING LIST NIL STREAM))
                    (AND SPLICEP (GO RDOT))))
             (MULTIPLE-VALUE (THING TYPE) (READ-COMPARE-THING NIL STREAM))
             (COND ((AND (EQ THING 'CLOSE) (EQ TYPE 'SPECIAL-TOKEN))
                    (RETURN LIST 'LIST))
                   (T (FERROR NIL
                        "~S was read instead of a close paren"
                        THING)))))

(DEFUN (SHARP-LESSTHAN STANDARD-READ-COMPARE-FUNCTION) (STREAM STRING)
       STREAM
       (FERROR NIL "BARF! the reader just encountered a ~S" STRING))

(DEFPROP SC-SYMBOL READ-COMPARE-SYMBOL STANDARD-READ-COMPARE-FUNCTION)
(DEFPROP SYMBOL READ-COMPARE-SYMBOL STANDARD-READ-COMPARE-FUNCTION)
(DEFUN READ-COMPARE-SYMBOL (STREAM STRING &AUX SYMBOL TEM)
    (PROG ()
          (AND (SETQ SYMBOL (INTERN-SOFT STRING))
               (SETQ TEM (GET SYMBOL (GET (CONS NIL (RDTBL-PLIST READTABLE))
                                          'READ-COMPARE-MACRO-PROPERTY)))
               (RETURN TEM 'READER-MACRO))
          (OR (AND (SYMBOLP READ-COMPARE-OBJECT)
                   (STRING-EQUAL STRING READ-COMPARE-OBJECT))
              (THROW NIL READ-COMPARE))
          (RETURN T)))

;FOO: switches us to the package associated with the string "FOO"
(DEFUN (PACKAGE-PREFIX STANDARD-READ-COMPARE-FUNCTION) (STREAM STRING IGNORE)
       (PROG (THING TYPE (PACKAGE (PKG-FIND-PACKAGE (OR STRING "") NIL PACKAGE)))
             (MULTIPLE-VALUE (THING TYPE)
                             (READ-COMPARE-THING READ-COMPARE-OBJECT STREAM))
             (RETURN THING TYPE)))

(DEFPROP QUOTED-SYMBOL READ-COMPARE-STRING STANDARD-READ-COMPARE-FUNCTION)
(DEFPROP STRING READ-COMPARE-STRING STANDARD-READ-COMPARE-FUNCTION)

;We use the winning LAST-CHAR hack to get the character to match
(DEFUN READ-COMPARE-STRING (STREAM IGNORE MATCH &AUX TEM)
    (FUNCALL STREAM ':UNTYI MATCH)
    (OR (PROG1 (EQUAL READ-COMPARE-OBJECT (SETQ TEM (READ STREAM)))
               (AND (STRINGP TEM) (RETURN-ARRAY TEM)))
        (THROW NIL READ-COMPARE))
    T)

(DEFPROP SLASHED-SYMBOL READ-COMPARE-NUMBER STANDARD-READ-COMPARE-FUNCTION)
(DEFPROP FIXNUM READ-COMPARE-NUMBER STANDARD-READ-COMPARE-FUNCTION)
(DEFPROP FLONUM READ-COMPARE-NUMBER STANDARD-READ-COMPARE-FUNCTION)
(DEFPROP SMALL-FLONUM READ-COMPARE-NUMBER STANDARD-READ-COMPARE-FUNCTION)
(DEFUN READ-COMPARE-NUMBER (STREAM STRING)
       (OR (EQUAL READ-COMPARE-OBJECT (READ-FROM-STRING STRING))
           (THROW NIL READ-COMPARE)))

;Standard reader macros:
(PUTPROP '/' 'READ-COMPARE-QUOTE-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/#/# 'READ-COMPARE-/#/#-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/; 'READ-COMPARE-COMMENT-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/` 'READ-COMPARE-BACKQUOTE-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/, 'READ-COMPARE-COMMA-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/#/Q 'READ-COMPARE-/#/Q-MACRO 'STANDARD-READ-COMPARE-MACRO)

(PUTPROP '/#/M 'READ-COMPARE-/#/M-MACRO 'STANDARD-READ-COMPARE-MACRO)

(DEFUN READ-COMPARE-QUOTE-MACRO (OBJECT IGNORE STREAM)
    (AND (EQ OBJECT READ-COMPARE-STOP)
         (THROW 'STOP READ-COMPARE))
    (AND (LISTP OBJECT) (EQ (CADR OBJECT) READ-COMPARE-STOP)
         (THROW 'STOP READ-COMPARE))
    (OR (AND (LISTP OBJECT)
             (EQ (CAR OBJECT) 'QUOTE)
             (NULL (CDDR OBJECT))
             (READ-COMPARE-INTERNAL (CADR OBJECT) STREAM))
        (THROW NIL READ-COMPARE))
    T)

(DEFUN READ-COMPARE-/#/#-MACRO (OBJECT IGNORE STREAM)
    (FUNCALL STREAM ':TYI)
    (OR (EQUAL OBJECT (FUNCALL STREAM ':TYI))
        (THROW NIL READ-COMPARE))
    T)

(DEFUN READ-COMPARE-COMMENT-MACRO (IGNORE REST STREAM)
       (PROG (CH)
           A (SETQ CH (FUNCALL STREAM ':TYI))
             (AND (OR (NULL CH)
                      (EQ CH 215))
                  (RETURN REST T))
             (GO A)))

(DEFUN READ-COMPARE-BACKQUOTE-MACRO (OBJECT IGNORE STREAM
                                    &AUX OLD-COMMA-SYNTAX0
                                         OLD-COMMA-SYNTAX1
                                         VAL TYPE)
    (PROG NIL
       (SETQ OLD-COMMA-SYNTAX0 (AR-2 READTABLE 0 54)
             OLD-COMMA-SYNTAX1 (AR-2 READTABLE 1 54))   ;POOR MAN'S BIND.
       (AS-2 (AR-2 READTABLE 0 140) READTABLE 0 54)     ;GIVE COMMA THE SYNTAX
       (AS-2 (AR-2 READTABLE 1 140) READTABLE 1 54)     ;   OF BACKQUOTE
       (SETQ VAL (READ STREAM '*EOF*))          ;*** FIGURE OUT EOF...
       (AND (EQ VAL '*EOF*) (THROW NIL READ-COMPARE))
       (AS-2 OLD-COMMA-SYNTAX0 READTABLE 0 54)
       (AS-2 OLD-COMMA-SYNTAX1 READTABLE 1 54)
       (RETURN T)))

(DEFUN READ-COMPARE-/#/Q-MACRO (OBJECT IGNORE STREAM)   ;IN LISPM, GOBBLE FROB
    (READ-COMPARE-INTERNAL OBJECT STREAM))

(DEFUN READ-COMPARE-/#/M-MACRO (IGNORE REST STREAM)     ;IN LISPM, FLUSH FROB
   (OR (EQ '*EOF* (READ STREAM '*EOF*))
       (THROW NIL READ-COMPARE))
   REST)
