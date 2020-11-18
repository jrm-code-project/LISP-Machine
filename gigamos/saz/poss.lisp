;-*- Mode:LISP; Package:ZWEI; Readtable:ZL; Base:10 -*-
;;; Written in 1982 by RMS -- feel free to use it,
;;; if you return all improvements for redistribution.

;;; This file deals with manipulating lists of "possibilities" -- things to look at or go to.
;;; Possibilities are used for recording functions to visit for List Callers, etc.
;;; and also for recording places to find definitions in Edit Definition.
;;; The list is recorded in a ZWEI buffer; each possibility is attached to one line
;;; of the buffer, and you can go to that line and type a command
;;; to go to that possibility.

;;; A possibility is a line whose plist contains properties :POSSIBILITY and :LEVEL.
;;; When one possibility generates others, it gives those others lower :LEVELs.

;;; Executing a possibility can call make-buffer-current, so you cannot
;;; in general use possibilities except in ZMACS.
;;; Also, the possibilities themselves live in ZMACS buffers.
;;; But you can use possibilities outside ZMACS as long as you know
;;; that you are using a possibilities buffer which cannot contain
;;; any possibilities that might try to switch buffers when executed.

;;; Going to a possibility can insert more possibilities, as new lines,
;;; following that line.

(DEFVAR POSSIBILITY-BUFFER NIL
  "While hacking possibility buffers, this is the buffer being hacked.
If this is non-NIL, it is a buffer whose point should be relocated
when possibilities are taken from it.")

(DEFMACRO POSSIBILITY-MOVE-BP ((BP) &BODY BODY)
  "Execute BODY, and afterwards move POSSIBILITY-BUFFER's point to BP's new value.
All possibility-functions that can alter the BP passed to them
should use this macro around the code which can do so."
  `(UNWIND-PROTECT
       (PROGN . ,BODY)
     (IF POSSIBILITY-BUFFER (MOVE-BP (BUFFER-POINT POSSIBILITY-BUFFER) ,BP))))

(DEFUN POSSIBILITY-LEVEL (BP)
  "Return the level of the possibility on BP's line, or NIL if none."
  (GETF (LINE-PLIST (BP-LINE BP)) ':LEVEL))

(DEFUN EXECUTE-POSSIBILITY (BP POSS)
  "Execute the possibility POSS and pass it BP."
  (APPLY (CAR POSS) BP (CDR POSS)))

(DEFUN GO-TO-POSSIBILITY (BP)
  "Execute the possibility on the line BP points to."
  (LET ((POSSIBILITY (GETF (LINE-PLIST (BP-LINE BP)) ':POSSIBILITY)))
    (IF POSSIBILITY
        (EXECUTE-POSSIBILITY BP POSSIBILITY)
      (BARF))))

(DEFUN GO-TO-NEXT-POSSIBILITY (BP)
  "Execute the next possibility after BP and move BP past it.
If BP is not at the beginning of a line, scanning starts on the next line.
BP is left at the end of the line on which the possibility is found."
  (POSSIBILITY-MOVE-BP (BP)
    (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
      ;; Advance to the end of the next line that has a :possibility property.
      ;; Don't count the line we start on unless we start at column 0.
      (DO ()
          ((AND (ZEROP (BP-INDEX BP))
                (GETF (LINE-PLIST (BP-LINE BP)) ':POSSIBILITY)))
        (LET ((NEXT (FORWARD-LINE BP 1)))
          (OR NEXT (BARF))
          ;; Now advance to the end of that line,
          ;; so we don't go to it again if we call this function again.
          (MOVE-BP BP NEXT))))
    (MOVE-BP BP (END-OF-LINE (BP-LINE BP))))
  ;; Now go to the possibility we have reached,
  ;; with *interval* not bound.
  (GO-TO-POSSIBILITY BP))

(DEFUN GO-TO-NEXT-LEVEL-0-POSSIBILITY (BP)
  "Execute the next level-zero possibility after BP and move BP past it.
If BP is not at the beginning of a line, scanning starts on the next line.
BP is left at the end of the line on which the possibility is found."
  (let (thing-to-find)
  (POSSIBILITY-MOVE-BP (BP)
    (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP)))
      ;; Advance to the end of the next line that has a :possibility property.
      ;; Don't count the line we start on unless we start at column 0.
      (DO ()
          ((AND (ZEROP (BP-INDEX BP))
                (setq thing-to-find (GETF (LINE-PLIST (BP-LINE BP)) ':POSSIBILITY))
                (ZEROP (GETF (LINE-PLIST (BP-LINE BP)) ':LEVEL))))
        (LET ((NEXT (FORWARD-LINE BP 1)))
          (OR NEXT (BARF))
          ;; Now advance to the end of that line,
          ;; so we don't go to it again if we call this function again.
          (MOVE-BP BP NEXT))))
    (MOVE-BP BP (END-OF-LINE (BP-LINE BP))))
  ;; Now go to the possibility we have reached,
  ;; with *interval* not bound.
  (format *query-io* "Editing next definition of ~A"
          (cond ((listp thing-to-find)
                 (string-append ":" (string (first (last (second thing-to-find))))))
                ((symbolp thing-to-find)
                 thing-to-find)
                (t "item in buffer *Possibilities*")))
  (GO-TO-POSSIBILITY BP)))

(DEFUN INSERT-POSSIBILITY-AFTER (BP LEVEL FORMAT-STRING POSSIBILITY type)
  "Insert a new possibility after the one BP points at.
The text of the line is made from FORMAT-STRING and the cdr of POSSIBILITY.
The :POSSIBILITY property is just POSSIBILITY and its level is LEVEL.
The value is a BP to the inserted line."
  (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
         (NEXT (FORWARD-LINE BP 1)))
    (OR NEXT (FERROR NIL "BP is on the last line of its node"))
    (INSERT NEXT #/NEWLINE)
    (SETQ NEXT (FORWARD-LINE BP 1))
    (INSERT NEXT (APPLY #'FORMAT NIL FORMAT-STRING (CDR POSSIBILITY)))
    (INSERT-CHARS NEXT #/SP (- LEVEL))
    (SETF (BP-INDEX NEXT) 0)
    (setf (GETF (LINE-PLIST (BP-LINE NEXT)) ':type) type)
    (SETF (GETF (LINE-PLIST (BP-LINE NEXT)) ':LEVEL) LEVEL)
    (SETF (GETF (LINE-PLIST (BP-LINE NEXT)) ':POSSIBILITY) POSSIBILITY)
    NEXT))

(DEFUN INSERT-POSSIBILITIES-AFTER (BP LEVEL POSSIBILITIES)
  "Insert the possibilities POSSIBILITIES all with level LEVEL after BP.
Each element of POSSIBILITIES is one possibility, a list.
The car of the element should be a symbol with a POSSIBILITY-FORMAT-STRING property.
That format string, with the element as arglist, generates the text of a line.
The insertions go after the line BP points to.
A BP to the last inserted line is returned."
  (DOLIST (POSSIBILITY POSSIBILITIES)
    (SETQ BP (INSERT-POSSIBILITY-AFTER BP LEVEL
                                       (GET (CAR POSSIBILITY) 'POSSIBILITY-FORMAT-STRING)
                                       POSSIBILITY))))

(DEFUN KILL-FOLLOWING-LOWER-LEVEL-POSSIBILITIES (BP)
  "Kill all possibilities starting with the line after where BP points.
Killing stops at the next possibility whose level is lower than
 the one on BP's line.
A possibility which inserts lower level possibilities after itself
 should call this first."
  (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
         (STARTING-LEVEL (GETF (LINE-PLIST (BP-LINE BP)) ':LEVEL))
         (NEXT (FORWARD-LINE BP 1))
         (STOP-LINE (BP-LINE (INTERVAL-LAST-BP *INTERVAL*))))
    (DO ((LINE (AND NEXT (BP-LINE NEXT)) (LINE-NEXT LINE)))
        ((OR (NULL LINE) (EQ LINE STOP-LINE))
         (MOVE-BP NEXT STOP-LINE 0))
      (MOVE-BP NEXT LINE 0)
      (AND (GETF (LINE-PLIST LINE) ':POSSIBILITY)
           ( (GETF (LINE-PLIST LINE) ':LEVEL)
              STARTING-LEVEL)
           (RETURN)))
    (DELETE-INTERVAL (FORWARD-LINE BP 1) NEXT T)))

(DEFUN INSERT-POSSIBILITY-BEFORE-AND-GO (BP POSSIBILITY)
  "Add POSSIBILITY BEFORE the line BP points to, and then execute it.
The text of the line is made using the POSSIBILITY-FORMAT-STRING property
of the car of POSSIBILITY."
  (LET ((LEVEL (1- (POSSIBILITY-LEVEL BP))))
    (POSSIBILITY-MOVE-BP (BP)
      (MOVE-BP BP (FORWARD-LINE BP -1)))
    (INSERT-POSSIBILITIES-AFTER
      BP LEVEL
      (LIST POSSIBILITY))
    (POSSIBILITY-MOVE-BP (BP)
      (MOVE-BP BP (FORWARD-LINE BP 1)))
    (GO-TO-NEXT-POSSIBILITY BP)))

(DEFUN INDIRECT-TO-POSSIBILITIES (BP POSSIBILITIES)
  "Handle one possibility by means of a list of subpossibilities.
This is used when one possibility's definition is /"try those other things/"
BP is the BP passed to the original possibility, and POSSIBILITIES
is a list of possibilities to indirect to.
They are inserted if necessary."
  (KILL-FOLLOWING-LOWER-LEVEL-POSSIBILITIES BP)
  (COND ((NULL POSSIBILITIES) (GO-TO-NEXT-POSSIBILITY BP))
        ((NULL (CDR POSSIBILITIES))
         (EXECUTE-POSSIBILITY BP (CAR POSSIBILITIES)))
        (T
         (INSERT-POSSIBILITIES-AFTER BP (1- (POSSIBILITY-LEVEL BP))
                                     POSSIBILITIES)
         (GO-TO-NEXT-POSSIBILITY BP))))

(DEFUN REPLACE-POSSIBILITY (BP THIS-POSSIBILITY POSSIBILITY)
  "Replace one possibility in the buffer with another, POSSIBILITY.
This should be called by a possibility whose execution has
changed the state of the world so that the original possibility
is no longer appropriate, and POSSIBILITY is appropriate instead.
Notice the difference between replacing and indirecting.

The possibility supposedly to be replaced is supplied as THIS-POSSIBILITY.
If that is not what is actually in the buffer, then the buffer is not changed.
In any case, the replacement possibility is executed."
  (LET* ((LINE (BP-LINE BP))
         (OLD-POSS (GETF (LINE-PLIST LINE) ':POSSIBILITY)))
    (IF (EQUAL OLD-POSS THIS-POSSIBILITY)
        (PROGN
          (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
                (AT-BEG-P (ZEROP (BP-INDEX BP))))
            (SETF (GETF (LINE-PLIST LINE) ':POSSIBILITY) POSSIBILITY)
            (POSSIBILITY-MOVE-BP (BP)
              (DELETE-INTERVAL (BEG-OF-LINE LINE) (END-OF-LINE LINE))
              (SETF (BP-INDEX BP) 0)
              (INSERT BP (APPLY #'FORMAT NIL
                                (GET (CAR POSSIBILITY) 'POSSIBILITY-FORMAT-STRING)
                                (CDR POSSIBILITY)))
              (INSERT-CHARS BP #/SP (- (POSSIBILITY-LEVEL BP)))
              (IF AT-BEG-P
                  (SETF (BP-INDEX BP) 0)
                (SETF (BP-INDEX BP) (LINE-LENGTH (BP-LINE BP))))))
          (GO-TO-POSSIBILITY BP))
      (EXECUTE-POSSIBILITY BP POSSIBILITY))))

;;;; Commands for going to possibilities, either pointed at or remembered.

(DEFCOM COM-GO-TO-POSSIBILITY "Go to the possibility on this line." ()
  (LET (POSSIBILITY-BUFFER)
    (GO-TO-POSSIBILITY (COPY-BP (POINT))))
  DIS-TEXT)

(DEFCOM COM-GO-TO-NEXT-POSSIBILITY "Go to the next possibility
/(the one after the last one you went to)." ()
  (LET ((POSSIBILITY-BUFFER (FIND-POSSIBILITIES-LISTS-BUFFER)))
    (GO-TO-NEXT-POSSIBILITY (COPY-BP (BUFFER-POINT POSSIBILITY-BUFFER))))
  DIS-TEXT)
;; Copied from LAD: RELEASE-3.ZWEI; POSS.LISP#97 on 2-Oct-86 02:49:56
(defcom com-get-all-possibilities "Do a silent C-. until no more possibilities."
        ()
  (do-forever (com-go-to-next-possibility)))


(DEFCOM COM-GO-TO-NEXT-TOP-LEVEL-POSSIBILITY "Go to the next possibility
/(the one after the last one you went to)." ()
  (LET ((POSSIBILITY-BUFFER (FIND-POSSIBILITIES-LISTS-BUFFER)))
    (GO-TO-NEXT-LEVEL-0-POSSIBILITY (COPY-BP (BUFFER-POINT POSSIBILITY-BUFFER))))
  DIS-TEXT)

(DEFUN FIND-POSSIBILITIES-LISTS-BUFFER (&OPTIONAL (NAME "*Possibilities Lists*"))
  (OR (FIND-BUFFER-NAMED NAME)
      (LET ((BUFFER (FIND-BUFFER-NAMED NAME T)))
        (SETF (BUFFER-SAVED-MAJOR-MODE BUFFER) 'POSSIBILITIES-MODE)
        (SETF (GET BUFFER 'SPECIAL-PURPOSE) :POSSIBILITIES-LISTS)
        BUFFER)))

(DEFMAJOR COM-POSSIBILITIES-MODE POSSIBILITIES-MODE "Possibilities"
  "Major mode for editing lists of things to visit." ()
  (SET-COMTAB *MODE-COMTAB* '(#/C-// COM-GO-TO-POSSIBILITY))
  (SET-MODE-LINE-LIST
    (APPEND (MODE-LINE-LIST) '("    (Control-// to try a possibility)"))))

(DEFCOM COM-EDIT-FLAVOR-COMPONENTS "Edit definitions of the component flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "Edit components of flavor:"
                                    "You are typing a flavor name, to edit the definitions
of its component flavors."))))
    (EDIT-DEFINITIONS NIL 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (PROGN
                (OR (DONT-OPTIMIZE (SI::FLAVOR-DEPENDS-ON-ALL FLAVOR))
                    (SI::COMPOSE-FLAVOR-COMBINATION FLAVOR))
                (DONT-OPTIMIZE (SI::FLAVOR-DEPENDS-ON-ALL FLAVOR))))
      "Components of flavor ~S:"
      "No components of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-LIST-FLAVOR-COMPONENTS "List all the component flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "List components of flavor:"
                                    "You are typing a flavor name, to list its component flavors."))))
    (EDIT-DEFINITIONS T 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (PROGN
                (OR (DONT-OPTIMIZE (SI::FLAVOR-DEPENDS-ON-ALL FLAVOR))
                    (SI::COMPOSE-FLAVOR-COMBINATION FLAVOR))
                (DONT-OPTIMIZE (SI::FLAVOR-DEPENDS-ON-ALL FLAVOR))))
      "Components of flavor ~S:"
      "No components of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-EDIT-FLAVOR-DEPENDENTS "Edit definitions of the dependent flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "Edit dependents of flavor:"
                                    "You are typing a flavor name, to edit the definitions of its dependent flavors."))))
    (EDIT-DEFINITIONS NIL 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (SI::FLAVOR-DEPENDED-ON-BY-ALL FLAVOR))
      "Dependents of flavor ~S:"
      "No dependents of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-LIST-FLAVOR-DEPENDENTS "List all the dependent flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "List dependents of flavor:"
                                    "You are typing a flavor name, to list its dependent flavors."))))
    (EDIT-DEFINITIONS T 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (SI::FLAVOR-DEPENDED-ON-BY-ALL FLAVOR))
      "Dependents of flavor ~S:"
      "No dependents of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-EDIT-FLAVOR-DIRECT-DEPENDENTS "Edit definitions of the direct dependent flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "Edit direct dependents of flavor:"
                                    "You are typing a flavor name, to edit the definitions of its dependent flavors."))))
    (EDIT-DEFINITIONS NIL 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (SI::FLAVOR-DEPENDED-ON-BY FLAVOR))
      "Direct dependents of flavor ~S:"
      "No dependents of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-LIST-FLAVOR-DIRECT-DEPENDENTS "List all the direct dependent flavors of a flavor." ()
  (LET ((FLAVOR (SI::COMPILATION-FLAVOR
                  (READ-FLAVOR-NAME "List direct dependents of flavor:"
                                    "You are typing a flavor name, to list its dependent flavors."))))
    (EDIT-DEFINITIONS T 'DEFFLAVOR
      (MAPCAR (LAMBDA (FLAVORNAME)
                (CONS (STRING FLAVORNAME)
                      FLAVORNAME))
              (SI::FLAVOR-DEPENDED-ON-BY FLAVOR))
      "Direct dependents of flavor ~S:"
      "No dependents of flavor ~S"
      (SI::FLAVOR-NAME FLAVOR))))

(DEFCOM COM-EDIT-FLAVOR-METHODS "Edit definitions of the methods of a flavor." ()
  (LET ((FLAVOR (READ-FLAVOR-NAME "Edit methods of flavor:"
                                  "You are typing a flavor name, to edit the definitions of its methods.")))
    (EDIT-DEFINITIONS NIL 'DEFUN
      (MAPCAR (LAMBDA (OBJ)
                (CONS (FORMAT NIL "~S" OBJ) OBJ))
              (SI::FLAVOR-METHOD-FUNCTION-SPECS FLAVOR))
      "Methods of flavor ~S:"
      "No methods of flavor ~S"
      FLAVOR)))

(DEFCOM COM-LIST-FLAVOR-METHODS "List all the methods of a flavor." ()
  (LET ((FLAVOR (READ-FLAVOR-NAME "List methods of flavor:"
                                  "You are typing a flavor name, to list its methods.")))
    (EDIT-DEFINITIONS T 'DEFUN
      (MAPCAR (LAMBDA (OBJ)
                (CONS (FORMAT NIL "~S" OBJ) OBJ))
              (SI::FLAVOR-METHOD-FUNCTION-SPECS FLAVOR))
      "Methods of flavor ~S:"
      "No methods of flavor ~S"
      FLAVOR)))

(DEFCOM COM-LIST-OBJECT-USERS "List the objects that use one specified object." ()
  (LET ((OBJECT (READ-FUNCTION-NAME "List users of object" (RELEVANT-FUNCTION-NAME (POINT))
                                    NIL 'ALWAYS-READ)))
    (EDIT-DEFINITIONS T NIL
      (MAPCAN (LAMBDA (USES) (MAPCAR (LAMBDA (OBJ)
                                       (SETQ OBJ (CADR OBJ))
                                       (IF (TYPEP OBJ 'PATHNAME)
                                           (CONS (STRING OBJ) OBJ)
                                         (CONS (FORMAT NIL "~S" OBJ) OBJ)))
                                     (CDR USES)))
              (SI::FIND-USERS-OF-OBJECTS (LIST OBJECT)))
      "Objects using ~S:" "No objects using ~S"
      OBJECT)))

(DEFCOM COM-MULTIPLE-LIST-OBJECT-USERS "List the objects that use certain specified objects." ()
  (LET (OBJECTS)
    (DO ((OBJECT)  (END "Stop")) (NIL)
      (SETQ OBJECT (READ-FUNCTION-NAME
                       (FORMAT NIL "List users of object")
                       (IF (NULL OBJECTS) (RELEVANT-FUNCTION-NAME (POINT)) END)
                       NIL
                       'ALWAYS-READ))
      (COND ((NEQ OBJECT END) (PUSH OBJECT OBJECTS))
            (T (RETURN))))
    (SETQ OBJECTS (NREVERSE OBJECTS))
    (EDIT-DEFINITIONS T NIL
      (MAPCAN (LAMBDA (USES) (MAPCAR (LAMBDA (OBJ)
                                       (SETQ OBJ (CADR OBJ))
                                       (IF (TYPEP OBJ 'PATHNAME)
                                           (CONS (STRING OBJ) OBJ)
                                         (CONS (FORMAT NIL "~S" OBJ) OBJ)))
                                     (CDR USES)))
              (SI::FIND-USERS-OF-OBJECTS OBJECTS))
      "Objects using one of ~S:" "No objects using one of ~S"
      OBJECTS)))

(DEFCOM COM-EDIT-OBJECT-USERS "Edit the objects that use one specified object." ()
  (LET ((OBJECT (READ-FUNCTION-NAME "Edit users of object" (RELEVANT-FUNCTION-NAME (POINT))
                                    NIL 'ALWAYS-READ)))
    (EDIT-FUNCTIONS-DISPLAY
      (MAPCAN (LAMBDA (USES) (MAPCAR (LAMBDA (OBJ)
                                       (SETQ OBJ (CADR OBJ))
                                       (IF (TYPEP OBJ 'PATHNAME)
                                           (CONS (STRING OBJ) OBJ)
                                         (CONS (FORMAT NIL "~S" OBJ) OBJ)))
                                     (CDR USES)))
              (SI::FIND-USERS-OF-OBJECTS (LIST OBJECT)))
      "Objects using ~S:" "No objects using ~S"
      OBJECT)))

(DEFCOM COM-MULTIPLE-EDIT-OBJECT-USERS "Edit the objects that use certain specified objects." ()
  (LET (OBJECTS)
    (DO ((OBJECT)  (END "Stop")) (NIL)
      (SETQ OBJECT (READ-FUNCTION-NAME
                       (FORMAT NIL "Edit users of object")
                       (IF (NULL OBJECTS) (RELEVANT-FUNCTION-NAME (POINT)) END)
                       NIL
                       'ALWAYS-READ))
      (COND ((NEQ OBJECT END) (PUSH OBJECT OBJECTS))
            (T (RETURN))))
    (SETQ OBJECTS (NREVERSE OBJECTS))
    (EDIT-FUNCTIONS-DISPLAY
      (MAPCAN (LAMBDA (USES) (MAPCAR (LAMBDA (OBJ)
                                       (SETQ OBJ (CADR OBJ))
                                       (IF (TYPEP OBJ 'PATHNAME)
                                           (CONS (STRING OBJ) OBJ)
                                         (CONS (FORMAT NIL "~S" OBJ) OBJ)))
                                     (CDR USES)))
              (SI::FIND-USERS-OF-OBJECTS OBJECTS))
      "Objects using one of ~S:" "No objects using one of ~S"
      OBJECTS)))

(DEFCOM COM-COMPILE-FILE-OBJECT-USERS "Offer to compile files that use one specified object." ()
  (LET ((OBJECT (READ-FUNCTION-NAME "Edit users of object" (RELEVANT-FUNCTION-NAME (POINT))
                                    NIL 'ALWAYS-READ)))
    (COMPILE-FILE-LIST-OFFER (SI::FIND-FILES-USING-OBJECTS (LIST OBJECT)))
    DIS-NONE))

(DEFCOM COM-MULTIPLE-COMPILE-FILE-OBJECT-USERS "Offer to compile files that use certain specified objects." ()
  (LET (OBJECTS)
    (DO ((OBJECT)  (END "Stop")) (NIL)
      (SETQ OBJECT (READ-FUNCTION-NAME
                       (FORMAT NIL "Edit users of object")
                       (IF (NULL OBJECTS) (RELEVANT-FUNCTION-NAME (POINT)) END)
                       NIL
                       'ALWAYS-READ))
      (COND ((NEQ OBJECT END) (PUSH OBJECT OBJECTS))
            (T (RETURN))))
    (SETQ OBJECTS (NREVERSE OBJECTS))
    (COMPILE-FILE-LIST-OFFER (SI::FIND-FILES-USING-OBJECTS OBJECTS))
    DIS-NONE))

(defun compile-file-list-offer (list-of-files)
  (let ((files-to-compile nil))
    (dolist (file list-of-files)
      (cond ((y-or-n-p "Compile file ~s? " (send file :string-for-printing))
             (push (send file :new-type "LISP") files-to-compile))))
    (dolist (file files-to-compile)
      (compile-file file))))


;;; Subroutines for list-flavor-components, etc., and also for List Callers, etc.
;;; Anything that provides a list of definitions to step through.

(DEFUN EDIT-DEFINITIONS (DISPLAY-NAMES-FLAG DEFINITION-TYPE ITEM-LIST
                         HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "List and//or edit definitions of a bunch of objects.
DISPLAY-NAMES-FLAG says display a mouse sensitive list of the objects;
 otherwise go ahead right away and edit the first one.
DEFINITION-TYPE, if not nil, says the type of definition we should edit.
ITEM-LIST is a list of elements (printed-string . object-name),
 one for each object to be edited.  A file name can be used instead
 of an object, to mean /"some unidentified thing in that file/".
HEADING-STRING is a format control string for printing a heading for the list.
NOT-FOUND-STRING is a format control string for printing a message
 saying that the list was empty.
FORMAT-ARGS are available to either format control string when it is used."
  (AND DISPLAY-NAMES-FLAG
       (SEND *STANDARD-OUTPUT* :FRESH-LINE))
  (COND ((NULL ITEM-LIST)
         (IF DISPLAY-NAMES-FLAG
             (APPLY #'FORMAT T NOT-FOUND-STRING FORMAT-ARGS)
             (APPLY #'FORMAT *QUERY-IO* NOT-FOUND-STRING FORMAT-ARGS))
         DIS-NONE)
        (DISPLAY-NAMES-FLAG
         (APPLY #'FORMAT T HEADING-STRING FORMAT-ARGS)
         (SEND *STANDARD-OUTPUT* :FRESH-LINE)
         (SEND *STANDARD-OUTPUT* :TYO #/NEWLINE)       ;Blank line after heading
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FUNCTION-NAME ITEM-LIST)
         (FORMAT T
                 "~&Type ~A to ~:[start editing these~;edit this~].~%"
                 (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
                 (= (LENGTH ITEM-LIST) 1))
         (INSERT-DEFINITIONS-POSSIBILITIES
           (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
           (MAPCAR #'CDR ITEM-LIST)
           DEFINITION-TYPE)
         DIS-NONE)
        (T (INSERT-DEFINITIONS-POSSIBILITIES
             (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
             (MAPCAR #'CDR ITEM-LIST)
             DEFINITION-TYPE)
           (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
           (COM-GO-TO-NEXT-POSSIBILITY))))

;;; Just start editing a bunch of functions; don't bother to display their names first.
;;; Each element of ITEM-LIST is actually a pair, (printed-string . function-spec).
(DEFUN EDIT-FUNCTIONS-NO-DISPLAY (ITEM-LIST HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  (APPLY #'EDIT-DEFINITIONS NIL NIL ITEM-LIST
         HEADING-STRING NOT-FOUND-STRING FORMAT-ARGS))

;;; Set up the mouse-sensitive display of a bunch of functions
;;; and offer to start editing these with control-.
;;; Each element of ITEM-LIST is actually a pair, (printed-string . function-spec).
(DEFUN EDIT-FUNCTIONS-DISPLAY (ITEM-LIST HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  (APPLY #'EDIT-DEFINITIONS T NIL ITEM-LIST
         HEADING-STRING NOT-FOUND-STRING FORMAT-ARGS))

(DEFUN INSERT-POSSIBILITIES (STARTING-STRING POSSIBILITIES
                             NO-MORE-POSSIBILITY &AUX NO-MORE-STRING)
  "Insert a whole group of possibilities, the result of one search, etc.
The group is inserted at the beginning of the possibilities buffer,
on a separate page.
STARTING-STRING is a string to insert on the first line of the page.
It should end with a colon, to look pretty.
Then the POSSIBILITIES are inserted,
and then the NO-MORE-POSSIBILITY is inserted."
  (SETQ NO-MORE-STRING (STRING-APPEND (STRING-RIGHT-TRIM '(#/: #/SPACE) STARTING-STRING)
                                      #/.))
  (SETF (AREF NO-MORE-STRING 0)
        (CHAR-DOWNCASE (AREF NO-MORE-STRING 0)))
  (LET* ((POSSIBILITY-BUFFER (FIND-POSSIBILITIES-LISTS-BUFFER))
         (OPOINT (COPY-BP (BUFFER-POINT POSSIBILITY-BUFFER)))
         (BP (COPY-BP OPOINT)))
    (POSSIBILITY-MOVE-BP (BP)
      (LET ((*INTERVAL* POSSIBILITY-BUFFER)
            POINT-HERE
            PREVIOUS-POINT-BP)
        (MOVE-BP BP (INTERVAL-FIRST-BP (BP-TOP-LEVEL-NODE BP)))
        (SETQ POINT-HERE (BP-= BP OPOINT))
        (INSERT-MOVING BP STARTING-STRING)
        (INSERT-MOVING BP #/NEWLINE)
        (SETF (GETF (LINE-PLIST (LINE-PREVIOUS (BP-LINE BP))) 'PREVIOUS-POINT)
              (SETQ PREVIOUS-POINT-BP (COPY-BP OPOINT)))
        (INSERT-MOVING BP #/NEWLINE)
        (INSERT-MOVING BP #/PAGE)
        (INSERT-MOVING BP #/NEWLINE)
        ;; If POINT used to be at where BP started out,
        ;; our PREVIOUS-POINT would end up pointing at the front of
        ;; this page, whereas it should point at the end.  So fix it now.
        (IF POINT-HERE
            (MOVE-BP PREVIOUS-POINT-BP BP))
        (MOVE-BP BP (FORWARD-LINE BP -2))
        (INSERT-POSSIBILITIES-AFTER BP 0 `((,NO-MORE-POSSIBILITY ,NO-MORE-STRING)))
        (INSERT-MOVING BP #/NEWLINE)
        (DBP BP)
        (INSERT-POSSIBILITIES-AFTER BP 0 POSSIBILITIES)
        (MOVE-BP BP (INTERVAL-FIRST-BP (BP-TOP-LEVEL-NODE BP)))))))

;;; This kind of possibility says that there are no more possibilities.
;;; It moves BP back to the start of the line so if you ask for the next possibility
;;; you get this one again and again.
(DEFUN NO-MORE-POSSIBILITY (BP ARG &REST IGNORE)
  (POSSIBILITY-MOVE-BP (BP)
    (MOVE-BP BP (BP-LINE BP) 0)
    (LET ((RESUME-BP (RESUME-POSSIBILITY-BP BP)))
      (IF (AND RESUME-BP (EQ *LAST-COMMAND-TYPE* 'NO-MORE-POSSIBILITY))
          (PROGN (MOVE-BP BP RESUME-BP)
                 (GO-TO-NEXT-POSSIBILITY BP))
        (SETQ *CURRENT-COMMAND-TYPE* 'NO-MORE-POSSIBILITY)
        (IF RESUME-BP
            (BARF "No more ~A
Repeat this command now to go back~%to ~A."
                  ARG
                  (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
                         (PREVPAGE (FORWARD-PAGE RESUME-BP -1))
                         (LINE (BP-LINE
                                 (IF PREVPAGE (FORWARD-LINE PREVPAGE)
                                   (INTERVAL-FIRST-BP *INTERVAL*)))))
                    (IF (ZEROP (STRING-LENGTH LINE))
                        LINE
                        (STRING-APPEND (CHAR-DOWNCASE (AREF LINE 0))
                                       (STRING-RIGHT-TRIM
                                        #/SP
                                        (SUBSTRING LINE 1 (1- (STRING-LENGTH LINE))))))))
          (BARF "No more ~A" ARG))))))
(DEFPROP NO-MORE-POSSIBILITY "No more ~A"
         POSSIBILITY-FORMAT-STRING)

(DEFUN RESUME-POSSIBILITY-BP (BP)
  "Return a bp giving point to resume previous possibility-group after end of this one.
BP identifies the group we are now in."
  (LET* ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
         (THISPAGE (FORWARD-PAGE BP -1))
         (PREVIOUS (GETF (LINE-PLIST
                           (BP-LINE
                             (IF THISPAGE (FORWARD-LINE THISPAGE)
                               (INTERVAL-FIRST-BP *INTERVAL*))))
                        'PREVIOUS-POINT))
         (NEXTPAGE (FORWARD-LINE (FORWARD-PAGE BP 1 T) 1 T)))
    (UNLESS (BP-= (OR PREVIOUS NEXTPAGE) (INTERVAL-LAST-BP *INTERVAL*))
      (OR PREVIOUS NEXTPAGE))))

(DEFUN EDIT-DEFINITION-POSSIBILITY (BP OBJECT &OPTIONAL
                                    DEFINITION-GENERIC-PATHNAME DEFINITION-TYPE)
  (DECLARE (IGNORE BP))
  (EDIT-DEFINITION-1 OBJECT T NIL DEFINITION-GENERIC-PATHNAME DEFINITION-TYPE))
(DEFPROP EDIT-DEFINITION-POSSIBILITY "Edit definition of ~S"
         POSSIBILITY-FORMAT-STRING)

(DEFUN INSERT-DEFINITIONS-POSSIBILITIES (STARTING-STRING OBJECTS &OPTIONAL DEFINITION-TYPE)
  "Insert a group of possibilities to find the definitions of OBJECTS.
The elements of OBJECTS are function specs, or other sorts of defined names.
DEFINITION-TYPE if non-NIL is the type of definition to look for;
otherwise all types of definitions are looked for.
STARTING-STRING is put on the first line of the page, to identify
the purpose of the lines that follow."
  (INSERT-POSSIBILITIES STARTING-STRING
                        (MAPCAR (LAMBDA (OBJECT)
                                  (IF (TYPEP OBJECT 'FS:PATHNAME)
                                      `(GO-TO-BEGINNING-OF-FILE-POSSIBILITY
                                         NIL ,OBJECT)
                                    `(EDIT-DEFINITION-POSSIBILITY
                                       ,OBJECT NIL ,DEFINITION-TYPE)))
                                OBJECTS)
                        'NO-MORE-POSSIBILITY))

;;;; Edit a list of section-nodes (for Edit Changed Sections, etc.)

(DEFCOM COM-LIST-SECTIONS "List all sections in a specified buffer.
Each DEFUN, DEFVAR, DEFSTRUCT, etc. is one section." ()
  (LET ((BUFFER (READ-BUFFER-NAME "List sections in buffer:" *INTERVAL*)))
    (RESECTIONIZE-BUFFER BUFFER)
    (FORMAT T "Sections in buffer ~A:~2%" BUFFER)
    (SEND *STANDARD-OUTPUT* :ITEM-LIST 'SECTION-NAME
          (MAPCAR (LAMBDA (SECTION)
                    (CONS (FORMAT:OUTPUT NIL (PRIN1 (SECTION-NODE-NAME SECTION)))
                          SECTION))
                  (NODE-INFERIORS BUFFER))))
  DIS-NONE)

(DEFCOM COM-LIST-CHANGED-FUNCTIONS "List any sections which have been edited" ()
  (COM-LIST-CHANGED-SECTIONS))

(DEFCOM COM-LIST-CHANGED-SECTIONS "List any sections which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
 ()
  (EDIT-SECTIONS-DISPLAY (CHANGED-SECTIONS) "~A:" "No ~A found." "Changed sections")
  DIS-NONE)

(DEFCOM COM-EDIT-CHANGED-FUNCTIONS "Edit any sections which have been edited" ()
  (COM-EDIT-CHANGED-SECTIONS))

(DEFCOM COM-EDIT-CHANGED-SECTIONS "Edit any sections which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
 ()
  (EDIT-SECTIONS-NO-DISPLAY (CHANGED-SECTIONS) "~A:" "No ~A found." "Changed sections")
  DIS-NONE)

(DEFCOM COM-LIST-BUFFER-CHANGED-FUNCTIONS "List any sections which have been edited" ()
  (COM-LIST-BUFFER-CHANGED-SECTIONS))

(DEFCOM COM-LIST-BUFFER-CHANGED-SECTIONS "List any sections which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
 ()
  (EDIT-SECTIONS-DISPLAY (CHANGED-SECTIONS-1 *INTERVAL*) "~A:" "No ~A found."
                         (FORMAT NIL "Changed sections in buffer ~A" *INTERVAL*))
  DIS-NONE)

(DEFCOM COM-EDIT-BUFFER-CHANGED-FUNCTIONS "Edit any sections which have been edited" ()
  (COM-EDIT-BUFFER-CHANGED-SECTIONS))

(DEFCOM COM-EDIT-BUFFER-CHANGED-SECTIONS "Edit any sections which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
  ()
  (EDIT-SECTIONS-NO-DISPLAY (CHANGED-SECTIONS-1 *INTERVAL*) "~A :" "No ~A found."
                            (FORMAT NIL "Changed sections in buffer ~A" *INTERVAL*))
  DIS-NONE)

(DEFCOM COM-TAGS-SEARCH-LIST-SECTIONS
        "List sections of files in the tag table containing specified string.
Reads a string in the minibuffer (extended search characters allowed)
and searches the files in the tag table, recording the sections that
contain the string as a list of possibilities to visit." ()
  (LET ((*MINI-BUFFER-DEFAULT-STRING* *ZMACS-TAGS-SEARCH-KEY-STRING*))
    (MULTIPLE-VALUE (*ZMACS-TAGS-SEARCH-FUNCTION* *ZMACS-TAGS-SEARCH-KEY*)
      (GET-EXTENDED-STRING-SEARCH-STRINGS NIL "Tags search (list sections):"
                                          *SEARCH-MINI-BUFFER-COMTAB*)))
  (SETQ *ZMACS-TAGS-SEARCH-KEY-STRING*
        (STRING-INTERVAL (WINDOW-INTERVAL (GET-SEARCH-MINI-BUFFER-WINDOW))))
  (LET (NODES)
    (DOLIST (*INTERVAL* (TAG-TABLE-BUFFERS T))
      (DO ((BP (INTERVAL-FIRST-BP *INTERVAL*)))
          (())
        (SETQ BP (FUNCALL *ZMACS-TAGS-SEARCH-FUNCTION* BP *ZMACS-TAGS-SEARCH-KEY*))
        (UNLESS BP (RETURN))
        (LET ((NODE (BP-NODE BP)))
          (PUSH NODE NODES)
          (SETQ BP (INTERVAL-LAST-BP NODE)))))
    (EDIT-SECTIONS-DISPLAY NODES "~A ~A:" "No ~A ~A found." "Sections containing"
                           *ZMACS-TAGS-SEARCH-KEY-STRING*))
  DIS-NONE)

(DEFCOM COM-TAGS-LIST-CHANGED-SECTIONS "List any sections in tag table which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
  ()
  (EDIT-SECTIONS-DISPLAY (CHANGED-SECTIONS (TAG-TABLE-BUFFERS NIL)) "~A:" "No ~A found."
                         (FORMAT NIL "Changed sections in buffers in tag table ~A"
                                 *ZMACS-CURRENT-TAG-TABLE*))
  DIS-NONE)

(DEFCOM COM-TAGS-EDIT-CHANGED-SECTIONS "Edit any sections in tag table which have been edited.
A section is changed if it has been modified since
 the file was read in (numeric arg 1 or no arg)
 the file was read in or saved (numeric arg 2)
 the section was compiled (numeric arg 3)."
  ()
  (EDIT-SECTIONS-NO-DISPLAY (CHANGED-SECTIONS (TAG-TABLE-BUFFERS NIL)) "~A :" "No ~A found."
                            (FORMAT NIL "Changed sections in buffers in tag table ~A"
                                    *ZMACS-CURRENT-TAG-TABLE*))
  DIS-NONE)

(DEFUN CHANGED-SECTIONS (&OPTIONAL
                         (BUFFERS (SUBSET (LAMBDA (BUFFER)
                                            (EQ (IF (EQ BUFFER *INTERVAL*) *MAJOR-MODE*
                                                  (BUFFER-SAVED-MAJOR-MODE BUFFER))
                                                'LISP-MODE))
                                          *ZMACS-BUFFER-LIST*))
                         &AUX SECTIONS)
  "Return a list of all changed sections in buffers BUFFERS.
Default for BUFFERS is all Lisp-mode buffers.
Implicitly uses *NUMERIC-ARG* to specify the criterion for being /"changed/"."
  (DOLIST (BUFFER BUFFERS)
    (SETQ SECTIONS (NCONC SECTIONS
                          (CHANGED-SECTIONS-1 BUFFER))))
  SECTIONS)

;;; Return a list of the changed sections in the specified buffer.
(DEFUN CHANGED-SECTIONS-1 (BUFFER &AUX RESULT)
  (RESECTIONIZE-BUFFER BUFFER)
  (DOLIST (SECTION (NODE-INFERIORS BUFFER))
    (AND (TYPEP SECTION 'SECTION-NODE)
         (> (NODE-TICK SECTION)
            (SELECTQ *NUMERIC-ARG*
              (2 (BUFFER-TICK BUFFER))
              (3 (SECTION-NODE-COMPILE-TICK SECTION))
              (OTHERWISE
               (BUFFER-FILE-READ-TICK BUFFER))))
         (SECTION-NODE-DEFUN-LINE SECTION)
         (PUSH SECTION RESULT)))
  (NREVERSE RESULT))

(DEFUN EDIT-SECTIONS-DISPLAY (SECTIONS HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "Print the names of SECTIONS mouse sensitively, and also insert possibilities to visit them.
SECTIONS is a list of section-nodes.
HEADING-STRING and NOT-FOUND-STRING are two format strings, both of which
operate on FORMAT-ARGS, to make a string announcing the results
and a string saying that there are no more results.
They could be /"~A:/" and /"No more ~A./"."
  (SEND *STANDARD-OUTPUT* :FRESH-LINE)
  (COND ((NULL SECTIONS)
         (APPLY #'FORMAT T NOT-FOUND-STRING FORMAT-ARGS))
        (T
         ;; Insert the possibilities in the possibilities buffer first
         ;; so that if the list is so long you don't want to watch it print out
         ;; you can abort and still have it for later.
         (INSERT-SECTIONS-POSSIBILITIES
           (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
           SECTIONS)
         (APPLY #'FORMAT T HEADING-STRING FORMAT-ARGS)
         (SEND *STANDARD-OUTPUT* :FRESH-LINE)
         (SEND *STANDARD-OUTPUT* :TYO #/NEWLINE)       ;Blank line after heading
         (SEND *STANDARD-OUTPUT* :ITEM-LIST 'SECTION-NAME
               (MAPCAR (LAMBDA (SECTION)
                         (CONS (FORMAT:OUTPUT NIL (PRIN1 (SECTION-NODE-NAME SECTION)))
                               SECTION))
                       SECTIONS))
         (FORMAT T
                 "~&Type ~A to ~:[start editing these~;edit this~].~%"
                 (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
                 (= (LENGTH SECTIONS) 1)))))

(DEFUN EDIT-SECTIONS-NO-DISPLAY (SECTIONS HEADING-STRING NOT-FOUND-STRING &REST FORMAT-ARGS)
  "Insert a group of possibilities to visit SECTIONS, and visit the first one.
SECTIONS is a list of section-nodes.
HEADING-STRING and NOT-FOUND-STRING are two format strings, both of which
operate on FORMAT-ARGS, to make a string announcing the results
and a string saying that there are no more results.
They could be /"~A:/" and /"No more ~A./"."
  (COND ((NULL SECTIONS)
         (APPLY #'FORMAT *QUERY-IO* NOT-FOUND-STRING FORMAT-ARGS)
         DIS-NONE)
        (T (INSERT-SECTIONS-POSSIBILITIES
             (APPLY #'FORMAT NIL HEADING-STRING FORMAT-ARGS)
             SECTIONS)
           (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #/C-SH-P)
           (COM-GO-TO-NEXT-POSSIBILITY))))

(DEFUN INSERT-SECTIONS-POSSIBILITIES (STARTING-STRING SECTIONS)
  "Insert a group of possibilities to visit SECTIONS.
SECTIONS is a list of section-nodes.
STARTING-STRING goes on the first line of the page the possibilities occupy."
  (INSERT-POSSIBILITIES STARTING-STRING
                        (MAPCAR (LAMBDA (SECTION)
                                  `(SECTION-POSSIBILITY ,SECTION))
                                SECTIONS)
                        'NO-MORE-POSSIBILITY))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* SECTION-NAME "Edit"
                          EDIT-SECTION-FOR-MOUSE T
                          "Edit this section.")

;;; This is what is called if you click on a section name
;;; presented in a list of changed sections, etc.
(DEFUN EDIT-SECTION-FOR-MOUSE (SECTION)
  (TYPEOUT-ABORT-MINI-BUFFER)
  (SECTION-POSSIBILITY NIL SECTION)
  NIL)                                          ;Cause typeout to be flushed.

;;; This is the possibility used for visiting a specific section.
(DEFPROP SECTION-POSSIBILITY "Section ~A" POSSIBILITY-FORMAT-STRING)
(DEFUN SECTION-POSSIBILITY (BP SECTION)
  (DECLARE (IGNORE BP))
  (DISPLAY-SECTION SECTION T NIL))

(DEFUN DISPLAY-SECTION (SECTION POINT-PDL-PUSH REDISPLAY-NOW)
  "Move point to the start of SECTION and redisplay.
If POINT-PDL-PUSH, then push (POINT) onto the point-pdl-buffer
if REDISPLAY-NOW, force redisplay once we have moved (POINT)."
  (IF (NOT (MEMQ (NODE-SUPERIOR SECTION) *ZMACS-BUFFER-LIST*))
      (FORMAT *QUERY-IO* "~&The section ~A is in a buffer that has been killed." SECTION)
    (LET* ((SECTION-NODE-DEFUN-LINE (SECTION-NODE-DEFUN-LINE SECTION))
           (SECTION-BP (AND SECTION-NODE-DEFUN-LINE
                            (IF (NEQ (LINE-TICK SECTION-NODE-DEFUN-LINE) 'DELETED)
                                (CREATE-BP SECTION-NODE-DEFUN-LINE 0)
                              (INTERVAL-FIRST-BP SECTION)))))
      (IF (NOT SECTION-BP)
          (FORMAT *QUERY-IO* "Cannot find section ~A." SECTION) ;unreal section (deleted, eg)
        (IF POINT-PDL-PUSH (POINT-PDL-PUSH (POINT) *WINDOW* T))
        (MAKE-BUFFER-CURRENT (NODE-SUPERIOR SECTION))
        (MOVE-BP (POINT) SECTION-BP)
        (LET ((DIS (RECENTER-WINDOW *WINDOW* :START (BACKWARD-OVER-COMMENT-LINES (POINT) NIL))))
          (IF REDISPLAY-NOW (REDISPLAY *WINDOW*) DIS))))))

;;;; EDIT-DEFINITION and its subroutines.
;;;; This uses a special buffer of possibilities, called *Definitions*.

(DEFCOM COM-EDIT-DEFINITION "Go to the definition of a specified function.
The name of the function is read from the mini-buffer." ()
  (IF *NUMERIC-ARG-P*
      (EDIT-NEXT-DEFINITION)
    (LET (SPEC STRING EXPLICIT-PACKAGE-P)
      (SETF (VALUES SPEC STRING EXPLICIT-PACKAGE-P)
            (READ-FUNCTION-NAME "Edit definition" (RELEVANT-FUNCTION-NAME (POINT))
                                'AARRAY-OK))
      (SETQ SPEC (LIST SPEC))
      ;; If there's only one entry in the aarray, and its for a different package,
      ;; but the symbol in the current package has some sort of definition in a file,
      ;; include them both.
      (IF (AND (NOT EXPLICIT-PACKAGE-P) (SYMBOLP (CAR SPEC)))
          (MULTIPLE-VALUE-BIND (THIS-PKG-SYMBOL FOUNDP)
              (INTERN-SOFT (STRING-UPCASE (STRING (CAR SPEC))))
            (IF (AND FOUNDP
                     (NEQ THIS-PKG-SYMBOL (CAR SPEC))
                     (GET THIS-PKG-SYMBOL :SOURCE-FILE-NAME))
                (PUSH THIS-PKG-SYMBOL SPEC))))
      (EDIT-DEFINITION-1 (CAR SPEC) (IF EXPLICIT-PACKAGE-P T SPEC) STRING)))
  DIS-TEXT)

(DEFF EDIT-DEFINITION 'EDIT-DEFINITION-1)

(DEFUN EDIT-NEXT-DEFINITION ()
  (LET ((POSSIBILITY-BUFFER (FIND-POSSIBILITIES-LISTS-BUFFER "*Definitions*")))
    (GO-TO-NEXT-POSSIBILITY (COPY-BP (BUFFER-POINT POSSIBILITY-BUFFER)))))

(TV:ADD-TYPEOUT-ITEM-TYPE *TYPEOUT-COMMAND-ALIST* FUNCTION-NAME "Edit"
                          EDIT-DEFINITION-FOR-MOUSE T
                          "Edit this function.")

;;; This is what is called if you click on a function name presented in a list of callers, etc
(DEFUN EDIT-DEFINITION-FOR-MOUSE (OBJECT)
  (TYPEOUT-ABORT-MINI-BUFFER)
  (EDIT-DEFINITION-1 OBJECT T)
  NIL)                                          ;Cause typeout to be flushed.

(DEFUN EDIT-DEFINITION-1 (OBJECT &OPTIONAL (OBJECTS (LIST OBJECT)) STRING
                          DEFINITION-GENERIC-PATHNAME DEFINITION-TYPE
                          &AUX DEF)
  "Visit the definition(s) of OBJECT, or OBJECTS.
STRING will be used eventually to look for look-alike objects,
so it should be a printed representation of OBJECT.
If OBJECTS is T, only OBJECT precisely is used, no matter how desperate the user gets.
Then STRING is not needed.
DEFINITION-GENERIC-PATHNAME restricts to definitions in that file,
and DEFINITION-TYPE restricts to that type of definition.
DEFINITION-TYPE should be something like DEFUN, DEFVAR, etc., or NIL."
  (SETQ OBJECT (FLUSH-INTERNAL-SPEC OBJECT))
  (IF (CONSP OBJECTS)
      (SETQ OBJECTS (MAPCAR 'FLUSH-INTERNAL-SPEC OBJECTS)))
  (AND (OR (EQ OBJECTS T)
           (AND (NULL (CDR OBJECTS))
                (EQUAL (CAR OBJECTS) OBJECT)))
       (SETQ DEF (ONLY-DEFINITION-OR-NIL OBJECT DEFINITION-TYPE)))
  (IF DEF
      ;; If there is only one definition of this object that could possibly be meant,
      ;; and it is in a buffer and still real, just go there.
      ;; Don't do any hacking with the possibilities buffer.
      ;; This is probably the most common case, so it should be fast.
      ;; If there is only one definition but it is in a file,
      ;; that is going to be slow enough anyway so no need to special case it.
      (PROGN (SOURCE-FILE-NAMES OBJECT T DEFINITION-TYPE)  ;Print names of any patch files.
             (POINT-PDL-PUSH (POINT) *WINDOW* T)
             (MAKE-BUFFER-CURRENT (CAR DEF))
             (MOVE-BP (POINT) (CDR DEF) 0)
             (RECENTER-WINDOW *WINDOW* :START
                              (BACKWARD-OVER-COMMENT-LINES (OR (BACKWARD-OVER-PACKAGE-PREFIX (POINT))
                                                               (POINT))
                                                           NIL)))
    (INSERT-EDIT-DEFINITION-POSSIBILITY OBJECT OBJECTS STRING
                                        DEFINITION-GENERIC-PATHNAME
                                        DEFINITION-TYPE)))

(DEFUN FLUSH-INTERNAL-SPEC (OBJECT)
  "Given an :INTERNAL function spec, return the function spec it is contained in.  Otherwise identity."
  (IF (EQ (CAR-SAFE OBJECT) ':INTERNAL)
      (CADR OBJECT)
    OBJECT))

(DEFUN ONLY-DEFINITION-OR-NIL (SPEC &OPTIONAL DEFINITION-TYPE)
  "Return the only place in ZWEI that SPEC is defined, or else NIL.
NIL is returned if SPEC has zero, or two or more, definitions.
If DEFINITION-TYPE is non-NIL, only definitions of that type count.
When the value is non-NIL, it is a cons of (BUFFER . LINE)."
  (LET ((ZBP (SI:FUNCTION-SPEC-GET SPEC 'ZMACS-BUFFERS)))
    (AND ZBP (NULL (CDR ZBP))
         (NOT (BUFFER-IS-NOT-ONLY-SOURCE-FILE-P (CAAR ZBP) SPEC DEFINITION-TYPE))
         (DEFINITION-STILL-REAL-P-NEW (CAAR ZBP) (CDAR ZBP) SPEC)
         (CAR ZBP))))

(DEFUN BUFFER-IS-NOT-ONLY-SOURCE-FILE-P (BUFFER SPEC &OPTIONAL DEFINITION-TYPE)
  (PROG TOP ((SOURCES (SI:GET-ALL-SOURCE-FILE-NAMES SPEC)))
        (DOLIST (DEF SOURCES)
          (AND (IF DEFINITION-TYPE (EQ DEFINITION-TYPE (CAR DEF))
                 T)
               (DOLIST (FILE (CDR DEF))
                 (AND (NEQ (BUFFER-GENERIC-PATHNAME BUFFER) FILE)
                      (NOT (SEND FILE :GET ':PATCH-FILE))
                      (RETURN-FROM TOP T)))))))

(DEFUN MORE-DEFINITIONS-NOTIFICATION (BP &AUX POSS)
  "If the possibility on the line after BP is not a no-more, print /"More definitions/"."
  (LET ((*INTERVAL* (BP-TOP-LEVEL-NODE BP))
        (LINE (BP-LINE (FORWARD-LINE BP 1))))
    (IF (AND LINE (SETQ POSS (GETF (LINE-PLIST LINE) ':POSSIBILITY))
             (NEQ (CAR POSS) 'LAST-RESORT-POSSIBILITY))
        (FORMAT *QUERY-IO* ".  More definitions remain.
Use the command ~A with numeric argument to see the next one."
                     (OR (KEY-FOR-COMMAND 'COM-EDIT-DEFINITION *COMTAB* NIL NIL #/M-/.)
                         "Edit Definition")
                     ))))

;;; Insert an edit-definitions-possibility at the end of the buffer,
;;; leave the buffer's bp at the end of that line,
;;; then execute it.
;;; OBJECTS can be T to mean insist on precisely the object OBJECT, no look alikes.
;;; In that case, both now and on retries, EDIT-DEFINITION-INTERNAL-POSSIBILITY
;;; is passed a list containing just the specified object.
;;; Otherwise, we passing OBJECTS now, but STRING if it is retried.
(DEFUN INSERT-EDIT-DEFINITION-POSSIBILITY (OBJECT OBJECTS STRING
                                           &OPTIONAL DEFINITION-GENERIC-PATHNAME
                                           DEFINITION-TYPE)
  (LET* ((POSSIBILITY-BUFFER (FIND-POSSIBILITIES-LISTS-BUFFER "*Definitions*"))
         (BP (COPY-BP (BUFFER-POINT POSSIBILITY-BUFFER))))
    (POSSIBILITY-MOVE-BP (BP)
      (LET ((*INTERVAL* POSSIBILITY-BUFFER))
        (MOVE-BP BP (INTERVAL-LAST-BP (BP-TOP-LEVEL-NODE BP)))
        (OR (ZEROP (BP-INDEX BP))
            (INSERT-MOVING BP #/NEWLINE))
        (INSERT-MOVING BP #/NEWLINE)
        (DBP BP)
        (INSERT-POSSIBILITIES-AFTER
          BP 0
          (LIST `(EDIT-DEFINITION-INTERNAL-POSSIBILITY
                   ,OBJECT
                   ,(IF (EQ OBJECTS T) (LIST OBJECT) STRING)
                   ,DEFINITION-GENERIC-PATHNAME ,DEFINITION-TYPE)
                `(LAST-RESORT-POSSIBILITY ,OBJECT)))
        (MOVE-BP BP (END-OF-LINE (BP-LINE (FORWARD-LINE BP 1))))))
    (EDIT-DEFINITION-INTERNAL-POSSIBILITY BP OBJECT
                                          (IF (EQ OBJECTS T) (LIST OBJECT) OBJECTS)
                                          DEFINITION-GENERIC-PATHNAME
                                          DEFINITION-TYPE)))

;;; This is the edit-definition-internal-possibility itself.
;;; It does all the work of finding definitions, through subpossibilities.
;;; OBJECTS is a list of look-alike function specs
;;; that the user "might have meant", or else it is a string
;;; and we should recompute the look-alikes here.
;;; If DEFINITION-GENERIC-PATHNAME is specified, we look in that file *ONLY*.
(DEFUN EDIT-DEFINITION-INTERNAL-POSSIBILITY (BP OBJECT &OPTIONAL OBJECTS-OR-STRING
                                             DEFINITION-GENERIC-PATHNAME DEFINITION-TYPE
                                             &AUX OBJECTS BUFFERS FILES)
  (COND ((STRINGP OBJECTS-OR-STRING)
         (SETQ OBJECTS (LOOKALIKE-SPECS OBJECT OBJECTS-OR-STRING)))
        ((NULL OBJECTS-OR-STRING)
         (SETQ OBJECTS (LOOKALIKE-SPECS OBJECT)))
        (T
         (SETQ OBJECTS OBJECTS-OR-STRING)))
  (SETQ OBJECTS (ADD-PARENT-SPECS OBJECTS))
  (IF DEFINITION-GENERIC-PATHNAME
      (DO ((BUFS *ZMACS-BUFFER-LIST* (CDR BUFS)))
          ((NULL BUFFERS)
           (SETQ FILES
                 (LIST (FS:GENERIC-PATHNAME-SOURCE-PATHNAME DEFINITION-GENERIC-PATHNAME))))
        (IF (EQ (BUFFER-GENERIC-PATHNAME (CAR BUFFERS)) DEFINITION-GENERIC-PATHNAME)
            (RETURN (SETQ BUFFERS (LIST (CAR BUFS))))))
    (SETF (VALUES BUFFERS FILES)
          (FIND-DEFINITIONS-BUFFERS-AND-FILES OBJECTS DEFINITION-TYPE)))
  ;; If the current buffer is in the list, handle it first.
  (IF (MEMQ *INTERVAL* BUFFERS)
      (SETQ BUFFERS (CONS *INTERVAL* (DELQ *INTERVAL* BUFFERS))))
  (INSERT-BUFFER-POSSIBILITIES BP OBJECT OBJECTS BUFFERS FILES))
(DEFPROP EDIT-DEFINITION-INTERNAL-POSSIBILITY "Edit definition of ~S"
         POSSIBILITY-FORMAT-STRING)

;;; Go to the end of a specific file.  This is used as a "location-function"
;;; for certain pseudo-objects to which warnings are attached.
(DEFUN GO-TO-END-OF-FILE-POSSIBILITY (BP IGNORE FILE)
  (DECLARE (IGNORE BP))
  (SETQ FILE (SEND (SEND FILE :TRANSLATED-PATHNAME) :SOURCE-PATHNAME))
  (POINT-PDL-PUSH (POINT) *WINDOW* T)
  (IF (FIND-FILE-BUFFER FILE)
      (MAKE-BUFFER-CURRENT (FIND-FILE-BUFFER FILE))
    (FIND-FILE FILE))
  (MOVE-BP (POINT) (INTERVAL-LAST-BP *INTERVAL*)))
(DEFPROP GO-TO-END-OF-FILE-POSSIBILITY "Something in file ~*~A"
         POSSIBILITY-FORMAT-STRING)

;;; Go to the beginning of a specific file.  This is used as a "location-function"
;;; for certain pseudo-objects to which warnings are attached.
(DEFUN GO-TO-BEGINNING-OF-FILE-POSSIBILITY (BP IGNORE FILE)
  (DECLARE (IGNORE BP))
  (SETQ FILE (SEND (SEND FILE :TRANSLATED-PATHNAME) :SOURCE-PATHNAME))
  (POINT-PDL-PUSH (POINT) *WINDOW* T)
  (IF (FIND-FILE-BUFFER FILE)
      (MAKE-BUFFER-CURRENT (FIND-FILE-BUFFER FILE))
    (FIND-FILE FILE))
  (MOVE-BP (POINT) (INTERVAL-FIRST-BP *INTERVAL*)))
(DEFPROP GO-TO-BEGINNING-OF-FILE-POSSIBILITY "Something in file ~*~A"
         POSSIBILITY-FORMAT-STRING)

;;; Given a BP to an edit-definitions-possibility, the object it is for,
;;; and a list of buffers and a list of files to visit,
;;; indirect to possibilities for those buffers and files.
;;; OBJECTS is a list of look-alike function specs.
;;; DEFINITION-GENERIC-PATHNAME is a file to try first regardless of whether it is loaded now.
(DEFUN INSERT-BUFFER-POSSIBILITIES (BP OBJECT OBJECTS BUFFERS FILES)
  (INDIRECT-TO-POSSIBILITIES
    BP
    (NCONC
      (MAPCAR (LAMBDA (FILE)
                `(DEFINITIONS-IN-BUFFER-POSSIBILITY ,OBJECT ,FILE ,OBJECTS))
              BUFFERS)
      (MAPCAR (LAMBDA (FILE) `(FETCH-SOURCE-FILE-POSSIBILITY ,OBJECT ,FILE))
              FILES))))

(DEFUN FIND-DEFINITIONS-BUFFERS-AND-FILES (SPECS DEFINITION-TYPE &AUX BUFFERS-TO-RETURN FILES)
  "Find all ZMACS buffers, and all non-visited files, that have definitions of SPECS.
SPECS is a list of function specs, and definitions of any of them count.
DEFINITION-TYPE is the type of definition (DEFUN, DEFVAR, etc). to look for,
 unless it is NIL.
The first value is a list of ZMACS buffers; the second, a list of pathnames
of files not visited in ZMACS."
  (DOLIST (SPEC SPECS)
    (LET ((BUFFERS (SI:FUNCTION-SPEC-GET SPEC 'ZMACS-BUFFERS)))
      (DOLIST (ELT BUFFERS)
        (LET ((BUFFER (CAR ELT)))
          (IF (MEMQ BUFFER *ZMACS-BUFFER-LIST*)
              (OR (MEMQ BUFFER BUFFERS-TO-RETURN)
                  (PUSH BUFFER BUFFERS-TO-RETURN))
            (IF (TYPEP BUFFER 'FILE-BUFFER)
                (PUSH (OR (BUFFER-PATHNAME BUFFER) (BUFFER-NAME BUFFER))
                      FILES))))))
    (SETQ FILES (NCONC (SOURCE-FILE-NAMES SPEC T DEFINITION-TYPE) FILES)))
  (VALUES BUFFERS-TO-RETURN
          (SUBSET (LAMBDA (FILE &AUX (GENERIC-PATHNAME (SEND FILE :GENERIC-PATHNAME)))
                    (NOT (DOLIST (BUF BUFFERS-TO-RETURN)
                           (AND (EQ (BUFFER-GENERIC-PATHNAME BUF)
                                    GENERIC-PATHNAME)
                                (RETURN T)))))
                  FILES)))

(DEFUN SOURCE-FILE-NAMES (FUNCTION-SPEC &OPTIONAL PRINT-P TYPE &AUX LIST)
  "Return a list of source file pathnames containing definitions of FUNCTION-SPEC.
PRINT-P says also print the names of any patch files that have redefined it.
TYPE is the type of definition (DEFUN, DEFVAR, etc). to look for,
 unless it is NIL."
  (SETQ LIST (SI:GET-ALL-SOURCE-FILE-NAMES FUNCTION-SPEC))
  (SETQ LIST (MAPCAN (LAMBDA (ELT)
                       (AND (OR (NULL TYPE) (EQ (CAR ELT) TYPE))
                            (COPY-LIST (CDR ELT))))
                     LIST))
  ;; All patch files that have defined the function since the real file should be noted.
  (LOOP FOR FILE IN LIST
        AS PATCH-FILE-P = (SEND FILE :GET ':PATCH-FILE)
        AS SOURCE-FILE = (SEND (SEND FILE :TRANSLATED-PATHNAME) :SOURCE-PATHNAME)
     WHEN (NOT PATCH-FILE-P)
       COLLECT SOURCE-FILE INTO REAL-FILES
     WHEN (AND PATCH-FILE-P (NULL REAL-FILES))
       COLLECT SOURCE-FILE INTO PATCH-FILES
     FINALLY (AND PRINT-P PATCH-FILES
                  (FORMAT *QUERY-IO* "~&~S ~:[also~;only~] defined by patch file~P ~{~A~^, ~}"
                          FUNCTION-SPEC (NOT REAL-FILES)
                          (LENGTH PATCH-FILES)
                          PATCH-FILES))
     (RETURN (OR REAL-FILES PATCH-FILES))))

;;; Possibility for a particular ZMACS buffer.
;;; It contains a list of all look-alike specs in that buffer,
;;; so we can check over the sections with EQUAL.
(DEFUN DEFINITIONS-IN-BUFFER-POSSIBILITY (BP OBJECT BUFFER OBJECTS)
  (INDIRECT-TO-POSSIBILITIES BP (BUFFER-DEFINITIONS-POSSIBILITIES BUFFER OBJECT OBJECTS)))
(DEFPROP DEFINITIONS-IN-BUFFER-POSSIBILITY "Definitions in buffer ~1@*~A"
         POSSIBILITY-FORMAT-STRING)

;;; Process the buffer possibility by finding the appropriate sections
;;; and making possibilities for them.
;;; If any section doesn't seem to be still real, insert a possibility for
;;; resectionizing to find where the definition went.
;;; If we find nothing at all, insert a possibility for searching through the buffer.
(DEFUN BUFFER-DEFINITIONS-POSSIBILITIES (BUFFER OBJECT OBJECTS
                                         &AUX POSSIBILITIES RESECTIONIZE
                                              (*PACKAGE* *PACKAGE*))
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  (DOLIST (SECTION (NODE-INFERIORS BUFFER))
    (IF (MEMBER (SECTION-NODE-NAME SECTION) OBJECTS)
        (IF (DEFINITION-STILL-REAL-P-NEW BUFFER (SECTION-NODE-DEFUN-LINE SECTION)
                                         (SECTION-NODE-NAME SECTION))
            (PUSH `(DEFINITION-SECTION-POSSIBILITY ,OBJECT ,SECTION)
                  POSSIBILITIES)
          (SETQ RESECTIONIZE T))))
  (WHEN (OR RESECTIONIZE (NULL POSSIBILITIES))
    (AND (SEND BUFFER :GET ':DONT-SECTIONIZE)
         (FQUERY NIL "Sectionize buffer ~A? " BUFFER)
         (SEND BUFFER :REMPROP ':DONT-SECTIONIZE))
    (COND ((RESECTIONIZE-BUFFER BUFFER)
           ;; Sectionization actually changed.
           ;; Recompute list of sections to visit from scratch.
           (SETQ POSSIBILITIES NIL)
           (DOLIST (SECTION (NODE-INFERIORS BUFFER))
             (IF (MEMBER (SECTION-NODE-NAME SECTION) OBJECTS)
                 (PUSH `(DEFINITION-SECTION-POSSIBILITY ,OBJECT ,SECTION)
                       POSSIBILITIES))))
          (T
           ;; Sectionization did not change???
           ;; Well, ok, keep list of sections found before,
           ;; but also try searching textually.
           (PUSH `(SEARCH-THROUGH-BUFFER-POSSIBILITY ,OBJECT ,BUFFER)
                 POSSIBILITIES))))
  (NREVERSE POSSIBILITIES))

(DEFPROP DEFINITION-SECTION-POSSIBILITY "Section ~A" POSSIBILITY-FORMAT-STRING)
(DEFUN DEFINITION-SECTION-POSSIBILITY (BP OBJECT SECTION)
  (DECLARE (IGNORE OBJECT))
  (IF (LET ((*PACKAGE* *PACKAGE*))
        (COMPUTE-BUFFER-PACKAGE (NODE-SUPERIOR SECTION))
        (DEFINITION-STILL-REAL-P-NEW
          (NODE-SUPERIOR SECTION)
          (SECTION-NODE-DEFUN-LINE SECTION)
          (SECTION-NODE-NAME SECTION)))
      (PROGN
        (POINT-PDL-PUSH (POINT) *WINDOW* T)
        (MORE-DEFINITIONS-NOTIFICATION BP)
        (MAKE-BUFFER-CURRENT (NODE-SUPERIOR SECTION))
        (MOVE-BP (POINT) (CREATE-BP (SECTION-NODE-DEFUN-LINE SECTION) 0))
        (RECENTER-WINDOW *WINDOW* :START
                         (BACKWARD-OVER-COMMENT-LINES (OR (BACKWARD-OVER-PACKAGE-PREFIX (POINT))
                                                          (POINT))
                                                      NIL)))))
#|
;If any definitions in the a buffer fail to be still real,
;this possibility is inserted.  We offer to resectionize;
;if the user confirms, we insert a new list of sections and try them.
;Otherwise, we try searching through the buffer for something that looks right.
(DEFUN RESECTIONIZE-BUFFER-POSSIBILITY (BP OBJECT BUFFER &AUX (*PACKAGE* *PACKAGE*))
  (COMPUTE-BUFFER-PACKAGE BUFFER)
  (AND (SEND BUFFER :GET ':DONT-SECTIONIZE)
       (FQUERY NIL "Sectionize buffer ~A? " BUFFER)
       (SEND BUFFER :REMPROP ':DONT-SECTIONIZE))
  (COND ((RESECTIONIZE-BUFFER BUFFER)
         (REPLACE-POSSIBILITY
           BP
           (GETF (LINE-PLIST (BP-LINE BP)) ':POSSIBILITY)
           `(DEFINITIONS-IN-BUFFER-POSSIBILITY
              ,OBJECT ,BUFFER
              ,(ADD-PARENT-SPECS (LOOKALIKE-SPECS OBJECT))
              T)))
        (T (SEARCH-THROUGH-BUFFER-POSSIBILITY BP OBJECT BUFFER))))
(DEFPROP RESECTIONIZE-BUFFER-POSSIBILITY "Resectionize buffer ~1*@~A"
         POSSIBILITY-FORMAT-STRING)
|#

(DEFUN SEARCH-THROUGH-BUFFER-POSSIBILITY (BP OBJECT BUFFER)
  (FORMAT *QUERY-IO* "~&Can't find a definition of ~S in ~A;~%doing textual search."
               OBJECT (BUFFER-NAME BUFFER))
  (POINT-PDL-PUSH (POINT) *WINDOW* T)
  (MAKE-BUFFER-CURRENT BUFFER)
  (MORE-DEFINITIONS-NOTIFICATION BP)
  (MOVE-BP (POINT)
    (DEFINITION-LIKELY-POSITION BUFFER NIL T OBJECT))
  (RECENTER-WINDOW *WINDOW* ':START (BACKWARD-OVER-COMMENT-LINES (POINT) NIL)))
(DEFPROP SEARCH-THROUGH-BUFFER-POSSIBILITY "Search for likely string in buffer ~A"
         POSSIBILITY-FORMAT-STRING)

;;; Possibility for reading in a new source file.
(DEFUN FETCH-SOURCE-FILE-POSSIBILITY (BP OBJECT FILE)
  (LET ((BUF (OR (FIND-FILE-BUFFER FILE)
                 (PROGN (FIND-FILE FILE NIL)
                        (FIND-FILE-BUFFER FILE)))))
    (REPLACE-POSSIBILITY BP
                         (GETF (LINE-PLIST (BP-LINE BP)) ':POSSIBILITY)
                         `(DEFINITIONS-IN-BUFFER-POSSIBILITY
                            ,OBJECT ,BUF
                            ,(ADD-PARENT-SPECS (LOOKALIKE-SPECS OBJECT))))))
(DEFPROP FETCH-SOURCE-FILE-POSSIBILITY "Read in source file ~1@*~A"
         POSSIBILITY-FORMAT-STRING)

(DEFUN LOOKALIKE-SPECS (OBJECT &OPTIONAL (STRING (FORMAT NIL "~A" OBJECT)))
  "Return a list of look-alikes of OBJECT, based on STRING.
STRING defaults to the result of printing OBJECT,
but for best results, specify it yourself so you leave out
package prefixes in the right places.
A look-alike is anything in ZWEI's definition completion aarray
which matches STRING."
  (LET ((OBJECTS (CDR (ASS #'EQUALP STRING
                           (G-L-P *ZMACS-COMPLETION-AARRAY*)))))
    (OR (CONSP OBJECTS) (NULL OBJECTS) (SETQ OBJECTS (NCONS OBJECTS)))
    (PUSHNEW OBJECT OBJECTS :TEST #'EQ)
    (SI::ELIMINATE-DUPLICATES OBJECTS #'EQUAL)))

(DEFUN ADD-PARENT-SPECS (OBJECTS)
  "Add to OBJECTS any parent functions of any functions in OBJECTS.
OBJECTS is a list of function specs; the returned value contains
all the elements of OBJECTS, and also all recorded parents of elements."
  ;; Now see if this spec has a known parent.
  ;; If it does, include parent's buffers and files.
  (DOLIST (SPEC OBJECTS)
    (LET ((PARENT (CAR (ERRSET (SI:FUNCTION-PARENT SPEC) NIL))))
      (AND PARENT (NOT (MEMBER PARENT OBJECTS))
           (SETQ OBJECTS (NCONC OBJECTS (LIST PARENT))))))
  OBJECTS)


(DEFUN LAST-RESORT-POSSIBILITY (BP OBJECT &REST IGNORE &AUX DWIMIFIED-OBJECT processor-special)
  (MOVE-BP BP (BEG-OF-LINE (BP-LINE BP)))
  (INSERT-POSSIBILITY-BEFORE-AND-GO BP
    (COND ((AND (SETQ DWIMIFIED-OBJECT (SYS:DWIMIFY-PACKAGE-0 OBJECT 'ZMACS-DEFINEDP))
                (NOT (EQUAL OBJECT DWIMIFIED-OBJECT)))
           `(EDIT-DEFINITION-POSSIBILITY ,DWIMIFIED-OBJECT))
          ((AND (FDEFINEDP OBJECT)
                (LET ((DEF (FDEFINITION OBJECT)))
                  (COND
                    ;;if the objects definition is found in a file that is written in a
                    ;;corresponding package
                    ((and (symbolp object)
                          (get (symbol-package object) :prefix)
                          (not (get object 'zmacs-buffers)))
                     (pkg-goto (symbol-package object))
                     (resectionize-buffer
                       (find-file
                         (send (cadar (get object :source-file-name))
                               :new-pathname
                               :type "LISP"
                               :version :Newest)))
                     `(edit-definition-possibility ,object))
                    ;; If the object's definition is a symbol, offer that symbol's definition.
                    ((AND (SYMBOLP DEF)
                          (FQUERY NIL "The definition of ~S is ~S.  Visit ~1@*~S? " OBJECT DEF))
                     `(EDIT-DEFINITION-POSSIBILITY ,DEF))
                    ;; If the definition has a different name, offer that name.
                    ((AND (ATOM DEF)
                          (NOT (FUNCTIONP DEF)))
                     (BARF "The definition of is ~S is weird: ~S" OBJECT DEF))
                    ((AND (NOT (EQUAL (FUNCTION-NAME DEF) OBJECT))
                          (FQUERY NIL "The definition of ~S has the name ~S.  Visit ~1@*~S? "
                                  OBJECT (FUNCTION-NAME DEF)))
                     `(EDIT-DEFINITION-POSSIBILITY ,(FUNCTION-NAME DEF)))
                    ;; If object has an expr definition, offer to grind it into a new buffer.
                    ((AND (OR (CONSP DEF)
                              (SI:FUNCTION-SPEC-GET OBJECT ':PREVIOUS-EXPR-DEFINITION))
                          (FQUERY NIL "Grind the definition of ~S into a new buffer? " OBJECT))
                     (MAKE-BUFFER-CURRENT (CREATE-ONE-BUFFER-TO-GO (FORMAT NIL "~S" OBJECT)))
                     (RETURN-FROM LAST-RESORT-POSSIBILITY
                       (SI:GRIND-1 OBJECT 90.
                                   (INTERVAL-STREAM-INTO-BP (INTERVAL-LAST-BP *INTERVAL*)))))
                    ;;Make room for cleaning this up a bit -Keith
                    ((setq processor-special
                           (SELECT-PROCESSOR
                             (:CADR     (and (typep def 'microcode-function) 'UCODE))
                             (:LAMBDA   (and (typep def 'microcode-function) 'LAMBDA-UCODE))
                             (:EXPLORER (and (typep def 'microcode-function) 'LAMBDA-UCODE))
                             (:FALCON)))
                     (FQUERY NIL "~S is a processor-special function.  Search through ~A? "
                                 OBJECT processor-special)
                     `(TAGS-SEARCH-SYSTEM-POSSIBILITY
                        ,processor-special
                        ,(FORMAT NIL "(misc-inst-entry ~A)" OBJECT)))
                    (T
                     NIL)))))
          (T
           (OR *MINI-BUFFER-COMMAND*
               (SETQ *MINI-BUFFER-COMMAND*
                     `((COM-EDIT-DEFINITION NIL 1)
                       ,(FORMAT NIL "~S" OBJECT))))
           (LET ((FILE (READ-DEFAULTED-PATHNAME (FORMAT NIL "Read ~S from what file:"
                                                        (STRING-FROM-SPEC OBJECT))
                                                (PATHNAME-DEFAULTS))))
             ;; Now that we know the filename, insert a fetch-source-file possibility
             ;; for it BEFORE the last-resort-possibility, and then do it.
             ;; So the last-resort-possibility remains the last thing.
             `(FETCH-SOURCE-FILE-POSSIBILITY ,OBJECT ,FILE))))))

(DEFPROP LAST-RESORT-POSSIBILITY "Try other packages, grind definition, or ask for filename"
         POSSIBILITY-FORMAT-STRING)

(DEFUN TAGS-SEARCH-SYSTEM-POSSIBILITY (BP SYSTEM &REST OBJECTS &aux system-name)
  (DECLARE (IGNORE BP))
  (setq system-name (etypecase system
                      (si:system (si:system-name system))
                      ((or string symbol) (string system))))
  (SELECT-FILE-LIST-AS-TAG-TABLE (SI::SYSTEM-SOURCE-FILES SYSTEM) system-name)
  (APPLY #'TAGS-SEARCH-ALTERNATIVE-STRINGS OBJECTS))

(DEFPROP TAGS-SEARCH-SYSTEM-POSSIBILITY "Tags Search through ~A for ~@{ ~S~^,~}"
         POSSIBILITY-FORMAT-STRING)

(DEFPROP ZMACS-DEFINEDP
         (SI::VALIDATE-FUNCTION-SPEC ZMACS-DEFINEDP NIL "definition" SI::FUNCTION-SPEC-DWIMIFY)
         SI::DWIMIFY)

(DEFUN ZMACS-DEFINEDP (FUNCTION-SPEC)
  "T if FUNCTION-SPEC is something that Meta-. could find a definition for."
  (AND (SI:VALIDATE-FUNCTION-SPEC FUNCTION-SPEC)
       (OR (FDEFINEDP FUNCTION-SPEC)
           (special-form-p function-spec)
           (SI:FUNCTION-SPEC-GET FUNCTION-SPEC 'ZMACS-BUFFERS))))

(DEFUN DEFINITION-STILL-REAL-P-NEW (BUFFER LINE SPEC)
  "T if LINE is not deleted and really appears to contain a definition of SPEC."
  (AND LINE
       (NEQ (LINE-TICK LINE) 'DELETED)          ;Assure not deleted
       (OR (NULL SPEC)                          ;User did not really give the name
           (EQUAL SPEC
                  (LET ((*PACKAGE* (BUFFER-PACKAGE BUFFER)))
                    (GET-SECTION-NAME (BUFFER-SAVED-MAJOR-MODE BUFFER)
                                      LINE (CREATE-BP LINE 0)))))))

;;;; Find, or create and initialize, but do not select,
;;;; the warnings buffer for a particular file.
;;;; Does NOT update the contents.
;(DEFUN FIND-WARNINGS-BUFFER (GENERIC-PATHNAME
;                            &AUX NAME)
;  (OR GENERIC-PATHNAME (SETQ GENERIC-PATHNAME T))
;  (SETQ NAME (FORMAT NIL "Warnings for ~A"
;                    (IF (EQ GENERIC-PATHNAME T)
;                        "non-file operations"
;                      (SEND GENERIC-PATHNAME :STRING-FOR-EDITOR))))
;  (OR (FIND-BUFFER-NAMED NAME)
;      (LET ((*INTERVAL* (FIND-BUFFER-NAMED NAME T)))
;       (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) 'WARNINGS-MODE)
;       (COND ((NEQ GENERIC-PATHNAME T)
;              (SEND GENERIC-PATHNAME :PUTPROP *INTERVAL* ':WARNINGS-BUFFER)
;              (SETF (BUFFER-FILE-ID *INTERVAL*) '(:SPECIAL-BUFFER WARNINGS))
;              (SETF (BUFFER-PATHNAME *INTERVAL*) GENERIC-PATHNAME)
;              (SETF (BUFFER-GENERIC-PATHNAME *INTERVAL*) GENERIC-PATHNAME)))
;       ;; Initialize the contents of this buffer, if it is empty.
;       (COND ((BP-= (INTERVAL-FIRST-BP *INTERVAL*) (INTERVAL-LAST-BP *INTERVAL*))
;              (INSERT (COPY-BP (INTERVAL-FIRST-BP *INTERVAL*))
;                      (FORMAT NIL "Warnings for ~A~%"
;                              (IF (EQ GENERIC-PATHNAME T) "non-file operations"
;                                (SEND GENERIC-PATHNAME :TRANSLATED-PATHNAME))))
;              ;; Give it a section at the front containg the "Warnings for file FOO" lines
;              ;; and an empty section at the end.  That empty section simplifies
;              ;; the rest of the code.
;              (LET* ((BEGSECT
;                       (ADD-SECTION-NODE (INTERVAL-FIRST-BP *INTERVAL*)
;                                         (INTERVAL-LAST-BP *INTERVAL*)
;                                         NIL
;                                         NIL
;                                         *INTERVAL*
;                                         NIL 0 0))
;                     (ENDSECT
;                       (ADD-SECTION-NODE (INTERVAL-LAST-BP *INTERVAL*)
;                                         (INTERVAL-LAST-BP *INTERVAL*)
;                                         NIL
;                                         NIL
;                                         *INTERVAL*
;                                         BEGSECT 0 0)))
;                (SETF (NODE-INFERIORS *INTERVAL*)
;                      (LIST BEGSECT ENDSECT)))))
;       *INTERVAL*)))

;;;; Update the ZWEI section that lists the warnings from a particular operation
;;;; on a particular file.  We assume that the section already exists and contains
;;;; an obsolete list of warnings.
;(DEFUN UPDATE-WARNINGS-SECTION (SECTION FILE-WARNINGS-DATUM)
;  (SETF (SI::FILE-WARNINGS-EDITOR-BUFFER FILE-WARNINGS-DATUM)
;       SECTION)
;  (LET ((STREAM (INTERVAL-STREAM-INTO-BP (INTERVAL-FIRST-BP SECTION)))
;       ;; Don't allow motion, searching, etc. outside this section.
;       (*INTERVAL* SECTION)
;       (OPERATION (SECTION-NODE-NAME SECTION)))
;    ;; Go to the end of the second line of the buffer (the empty line).
;    (SEND STREAM :SET-BP (DBP (FORWARD-LINE (INTERVAL-FIRST-BP SECTION) 3)))
;    (DO ((OBJECT-WARNINGS-LEFT
;          (SI::FILE-WARNINGS-OBJECT-ALIST FILE-WARNINGS-DATUM)
;          (CDR OBJECT-WARNINGS-LEFT)))
;       (())
;      ;; Throw away any following objects' info in the buffer
;      ;; that is no longer useful.
;      (DO ((I 0 (1+ I))) (())
;       (LET ((NEXT-LINE-BP (IBP (SEND STREAM :READ-BP))))
;         (IF (OR (BP-= (INTERVAL-LAST-BP SECTION)
;                       NEXT-LINE-BP)
;                 (MEMBER (SEND STREAM :NEXT-LINE-GET ':WARNINGS)
;                         OBJECT-WARNINGS-LEFT))
;             (RETURN NIL))
;         (DELETE-INTERVAL NEXT-LINE-BP
;                          (NEXT-LINE-WITH-PROPERTY-BP
;                            (FORWARD-CHAR NEXT-LINE-BP)
;                            ':OBJECT))))
;      ;; We check for end of warnings AFTER deleting useless old text.
;      (IF (NULL OBJECT-WARNINGS-LEFT)
;         (RETURN))
;      ;; If what follows in the buffer now is the correct data for this object,
;      ;; skip over it.  Otherwise, write the data in the buffer.
;      (LET ((OBJW (CAR OBJECT-WARNINGS-LEFT)))
;       (IF (EQUAL OBJW (SEND STREAM :NEXT-LINE-GET ':WARNINGS))
;           (SEND STREAM :SET-BP (DBP (NEXT-LINE-WITH-PROPERTY-BP
;                                       (FORWARD-CHAR (SEND STREAM :READ-BP) 2)
;                                       ':OBJECT)))
;         (SI::PRINT-OBJECT-WARNINGS-HEADER STREAM (SI::OBJECT-WARNINGS-NAME OBJW) OPERATION)
;         (SEND STREAM :LINE-PUT ':OBJECT (SI::OBJECT-WARNINGS-NAME OBJW))
;         (SEND STREAM :LINE-PUT ':WARNINGS (SI::COPY-OBJECT-WARNINGS OBJW))
;         (SEND STREAM :LINE-PUT ':LEVEL 0)
;         (DOLIST (W (SI::OBJECT-WARNINGS-WARNINGS OBJW))
;           (FORMAT STREAM "~% Warning: ")
;           (APPLY #'FORMAT STREAM
;                          (SI::WARNING-FORMAT-STRING W) (SI::WARNING-FORMAT-ARGS W))
;           (SEND STREAM :LINE-PUT ':WARNING W)))))))

(DEFVAR *LAST-WARNINGS-BUFFER* NIL
  "This is the buffer into which the most recent Edit Warnings or similar command put its warnings.")

(DEFUN LAST-WARNINGS-BUFFER ()
  "Return the most recent buffer of warnings made by Edit Warnings.
Returns NIL if Edit Warnings has never been done or the buffer has been killed."
  (AND *LAST-WARNINGS-BUFFER*
       (MEMQ *LAST-WARNINGS-BUFFER* *ZMACS-BUFFER-LIST*)
       *LAST-WARNINGS-BUFFER*))

(DEFCOM COM-EDIT-COMPILER-WARNINGS "Synonym for Edit Warnings." ()
  (COM-EDIT-WARNINGS))

;;; Insert a list of possibilities for all the files that have warnings;
;;; then go to the first one.  Make the visited file first if it is one of them.
(DEFCOM COM-EDIT-WARNINGS "Edit warnings from compilations and similar operations.
First you are asked which files' warnings you wish to edit.
Then a list of all warnings for those files is placed in the buffer *Warnings*,
and you move through that buffer in a small top window, with the corresponding
code appearing in the bottom window." ()
  (MAPC #'SI::FILTER-WARNINGS (SI::WARNINGS-PATHNAMES))
  ;; Call si::warnings-pathnames again in case some files no longer have any warnings.
  (LET ((FILES (SI::WARNINGS-PATHNAMES)))
    (IF (MEMQ (BUFFER-GENERIC-PATHNAME *INTERVAL*) FILES)
        (SETQ FILES (CONS (BUFFER-GENERIC-PATHNAME *INTERVAL*)
                          (REMQ (BUFFER-GENERIC-PATHNAME *INTERVAL*) FILES))))
    (COND ((NULL FILES)
           (FORMAT *QUERY-IO* "~&No files have warnings.")
           DIS-NONE)
          ((NULL (SETQ FILES (SUBSET 'COM-EDIT-WARNINGS-QUERY FILES)))
           (FORMAT T "~&No other files have warnings.")
           DIS-NONE)
          (T (SEND *STANDARD-OUTPUT* :MAKE-COMPLETE)
;Appears not to be necessary because PREPARE-WINDOW-FOR-REDISPLAY called from
;DISPLAY-WARNING-TWO-WINDOWS does it.
;            (SEND (WINDOW-SHEET *WINDOW*) ':EXPOSE)
;            (SEND (SEND (WINDOW-SHEET *WINDOW*) ':TYPEOUT-WINDOW)
;                     :DEACTIVATE)
             (EDIT-FILES-WARNINGS FILES)))))

(DEFCOM COM-EDIT-SYSTEM-WARNINGS "Edit warnings for the files in a specified system.
Like Edit Warnings, but you specify the name of a system
and the warnings for the source files of that system are processed." ()
  (LET* ((SYSTEM (READ-SYSTEM-NAME "Edit warnings of what system:"))
         (WARNINGS-PATHNAMES (SI::WARNINGS-PATHNAMES))
         (FILES (LOOP FOR F IN (SI::SYSTEM-SOURCE-FILES SYSTEM)
                      AS GENERIC = (SEND F :GENERIC-PATHNAME)
                   WHEN
                     (MEMQ GENERIC WARNINGS-PATHNAMES)
                     COLLECT GENERIC)))
    (IF FILES
        (EDIT-FILES-WARNINGS FILES)
      (BARF "No warnings for system ~A." SYSTEM))))

(DEFCOM COM-EDIT-FILE-WARNINGS "Edit warnings for a particular file.
Like Edit Warnings, but you specify one filename and only the warnings
for that file are processed." ()
  (LET ((GENERIC-PATHNAME
          (IF *NUMERIC-ARG-P* T
            (SEND (READ-DEFAULTED-PATHNAME "Edit warnings for file:" (PATHNAME-DEFAULTS))
                  :GENERIC-PATHNAME))))
    (IF (MEMQ GENERIC-PATHNAME (SI::WARNINGS-PATHNAMES))
        (EDIT-FILES-WARNINGS (LIST GENERIC-PATHNAME))
      (BARF "No warnings for ~A." (WARNINGS-PATHNAME-AS-STRING GENERIC-PATHNAME)))))

(DEFUN EDIT-FILES-WARNINGS (FILES)
  "Begin editing the recorded warnings of FILES.
Initializes the warnings buffer with possibilities
and visits the first one in two-window mode."
  (SET-BUFFER-CONTENTS-TO-WARNINGS
    (OR (FIND-BUFFER-NAMED "*Warnings*")
        (LET ((*INTERVAL* (FIND-BUFFER-NAMED "*Warnings*" T)))
          (SETF (BUFFER-SAVED-MAJOR-MODE *INTERVAL*) 'WARNINGS-MODE)
          *INTERVAL*))
    FILES)
  (FORMAT *QUERY-IO* "~&Use ~A to go to the next function that has warnings."
          (KEY-FOR-COMMAND-SET-C-. 'COM-EDIT-NEXT-WARNING #/C-SH-W))
  (COM-EDIT-NEXT-WARNING))

(DEFUN WARNINGS-PATHNAME-AS-STRING (PATHNAME-OR-NIL)
  "Return a string to describe what PATHNAME-OR-NIL means as an arg to the warnings database."
  (IF (MEMQ PATHNAME-OR-NIL '(T NIL))
      "non-file operations"
    (STRING-APPEND "file "
                   (SEND (SEND (SEND PATHNAME-OR-NIL :SOURCE-PATHNAME)
                               :TRANSLATED-PATHNAME)
                         :STRING-FOR-PRINTING))))

(DEFUN COM-EDIT-WARNINGS-QUERY (PATHNAME)
  (LET ((*QUERY-IO* *TERMINAL-IO*))
    (FQUERY () "Edit warnings for ~A? " (WARNINGS-PATHNAME-AS-STRING PATHNAME))))

(DEFUN KEY-FOR-COMMAND-SET-C-. (COMMAND &OPTIONAL SUGGESTION)
  "Return a string describing a key that invokes COMMAND, and make Control-. invoke it too.
SUGGESTION is a character that might invoke COMMAND.
If it does, it saves the time of searching for one."
  (SET-COMTAB *ZMACS-COMTAB* '(#/C-. COM-BEEP))
  (PROG1 (OR (KEY-FOR-COMMAND COMMAND *COMTAB* NIL NIL SUGGESTION) "Control-.")
         (SET-COMTAB *ZMACS-COMTAB* `(#/C-. ,COMMAND))))

(DEFCOM COM-EDIT-PREVIOUS-WARNING "Edit the previous warning's function.
Once you have started editing a list of warnings with Edit Warnings
or Edit File Warnings, this command moves back to the previous warning." ()
  (LET ((*INTERVAL* (LAST-WARNINGS-BUFFER)))
    (OR *INTERVAL* (BARF "You have not done Edit Warnings."))
    (DO ((LINE (BP-LINE (FORWARD-LINE (BUFFER-POINT *INTERVAL*) -1 T))
               (LINE-PREVIOUS LINE))
         (STOP-LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*))))
        ((EQ LINE STOP-LINE))
      (IF (GETF (LINE-PLIST LINE) ':WARNINGS)
          (RETURN (MOVE-BP (BUFFER-POINT *INTERVAL*) LINE 0)))))
  (COM-EDIT-NEXT-WARNING))

(DEFCOM COM-EDIT-NEXT-WARNING "Edit the next function that has a warning.
Once you have started editing a list of warnings with Edit Warnings
or Edit File Warnings, this command moves to the next warning." ()
  (LET ((BUFFER (LAST-WARNINGS-BUFFER)))
    (OR BUFFER (BARF "You have not done Edit Warnings."))
    (MULTIPLE-VALUE-BIND (START-BP END-BP)
        (NEXT-WARNING-START-AND-END BUFFER)
      (COND (START-BP
             (DISPLAY-WARNING-TWO-WINDOWS START-BP END-BP)
             DIS-TEXT)
            (T (FORMAT *QUERY-IO* "~&No more warnings.")
               (MAKE-WINDOW-FULL-SCREEN *WINDOW*)
               DIS-NONE)))))

(DEFUN NEXT-WARNING-START-AND-END (BUFFER &OPTIONAL (START-BP (BUFFER-POINT BUFFER)))
  "Find the next group of warning in BUFFER, returning beginning and end.
A group of warnings is warnings for one object (function.
This finds the next group of warnings for one object, following START-BP,
and returns BPs to the beginning and end of the group."
  (LET* ((*INTERVAL* BUFFER))
    ;; Advance to the end of the next line that has a :possibility property.
    ;; Don't count the line we start on unless we start at column 0.
    (DO ()
        ((AND (ZEROP (BP-INDEX START-BP))
              (GETF (LINE-PLIST (BP-LINE START-BP)) ':WARNINGS)))
      (LET ((NEXT (FORWARD-LINE START-BP 1)))
        (OR NEXT (RETURN))
        ;; Now advance to the end of that line,
        ;; so we don't go to it again if we call this function again.
        (MOVE-BP START-BP NEXT)))
    (MOVE-BP START-BP (END-OF-LINE (BP-LINE START-BP)))
    (IF (AND START-BP (GETF (LINE-PLIST (BP-LINE START-BP)) ':WARNINGS))
        (VALUES START-BP
                (OR (NEXT-LINE-WITH-PROPERTY-BP (FORWARD-LINE START-BP 1 T) ':WARNINGS)
                    (INTERVAL-LAST-BP *INTERVAL*))))))

(DEFCONST *EDIT-COMPILER-WARNINGS-MINIMUM-TOP-WINDOW-SIZE* 5
  "Minimum lines for top window while editing warnings.")
(DEFCONST *EDIT-COMPILER-WARNINGS-MAXIMUM-TOP-WINDOW-SIZE* 14.
  "Maximum lines for top window while editing warnings.")

(DEFUN DISPLAY-WARNING-TWO-WINDOWS (START-BP END-BP &AUX (BUFFER (BP-TOP-LEVEL-NODE START-BP)))
  "Given BPs to a range of the warnings buffer, display the warnings and code in two windows.
START-BP and END-BP should delimit the part of the warnings buffer to display,
and the first posibility after START-BP should find the code."
  (PREPARE-WINDOW-FOR-REDISPLAY *WINDOW*)
  (MULTIPLE-VALUE-BIND (TOP-WINDOW BOTTOM-WINDOW)
      (SEND (WINDOW-FRAME *WINDOW*) :TWO-EDITOR-WINDOWS)
    (AND (EQ TOP-WINDOW *WINDOW*)
         (PSETQ TOP-WINDOW BOTTOM-WINDOW BOTTOM-WINDOW TOP-WINDOW))
    (LET ((NLINES (COUNT-LINES START-BP END-BP T)))
      ;; Redivide windows so top one is just right size,
      ;; unless it is currently just a little bigger than needed.
      (UNLESS (AND ( NLINES (WINDOW-N-PLINES TOP-WINDOW) (+ NLINES 3))
                   (SEND TOP-WINDOW :EXPOSED-P)
                   (SEND BOTTOM-WINDOW :EXPOSED-P))
        (SPLIT-SCREEN-BETWEEN-TWO-WINDOWS
          TOP-WINDOW BOTTOM-WINDOW
          (RANGE NLINES
                 *EDIT-COMPILER-WARNINGS-MINIMUM-TOP-WINDOW-SIZE*
                 *EDIT-COMPILER-WARNINGS-MAXIMUM-TOP-WINDOW-SIZE*))))
    (SEND TOP-WINDOW :SET-INTERVAL BUFFER)
    (MOVE-BP (WINDOW-POINT TOP-WINDOW) (END-OF-LINE (BP-LINE START-BP)))
    (RECENTER-WINDOW TOP-WINDOW :START (BEG-OF-LINE (BP-LINE START-BP)))
    (MAKE-WINDOW-CURRENT BOTTOM-WINDOW T)
    (REDISPLAY TOP-WINDOW :POINT NIL NIL T)
    (GO-TO-POSSIBILITY START-BP)))

;;; We visit the code in the other window if in two window mode and
;;; the warnings are in window one.
(DEFCOM COM-GO-TO-WARNING "If point is at a warning, go to the text the warning is about." ()
  (LET* ((BP (CREATE-BP (FIND-WARNING-OBJECT-LINE) 0))
         (WINDOWS (FRAME-EXPOSED-WINDOWS)))
    (IF (AND (CDR WINDOWS)
             (EQ (WINDOW-INTERVAL (SEND (CAR WINDOWS) :ZWEI-WINDOW))
                 (BP-TOP-LEVEL-NODE BP)))
        (MULTIPLE-VALUE-BIND (START-BP END-BP)
            (NEXT-WARNING-START-AND-END (BP-TOP-LEVEL-NODE BP) BP)
          (DISPLAY-WARNING-TWO-WINDOWS START-BP END-BP))
      (GO-TO-POSSIBILITY BP)))
  DIS-TEXT)

(DEFUN FIND-WARNING-OBJECT-LINE ()
  "Return the line above POINT which heads a group of warnings and gives the function name."
  (DO ((LINE (BP-LINE (POINT)) (LINE-PREVIOUS LINE)))
      ((EQ LINE (BP-LINE (INTERVAL-FIRST-BP *INTERVAL*)))
       (BARF "Point is not in a warning."))
    (COND ((GETF (LINE-PLIST LINE) ':OBJECT)
           (RETURN LINE))
          ((NOT (GETF (LINE-PLIST LINE) ':WARNING))
           (RETURN NIL)))))

(DEFMAJOR COM-WARNINGS-MODE WARNINGS-MODE
  "Warnings" "Set up for editing a list of warnings" ()
  (SET-COMTAB *MODE-COMTAB* '(#/C-// COM-GO-TO-WARNING))
  (SET-MODE-LINE-LIST
    (APPEND (MODE-LINE-LIST) '("    (Control-// to visit the code)"))))

;;;; Auxiliary commands to insert a complete list of warnings.

(DEFCOM COM-INSERT-FILE-WARNINGS "Insert the warnings about a file at point.
You specify the file with the mini buffer.
A numeric arg specifies the buffer of warnings for operations
not associated with files (such as, calls to COMPILE).
Leaves the mark after the inserted text." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (MOVE-BP (MARK) (POINT))
  (SETQ *CURRENT-COMMAND-TYPE* ':YANK)
  (INSERT-FILE-WARNINGS
    (IF *NUMERIC-ARG-P* T
      (SEND (READ-DEFAULTED-PATHNAME "Insert warnings for file:" (PATHNAME-DEFAULTS))
            :GENERIC-PATHNAME)))
  (SWAP-BPS (MARK) (POINT))
  DIS-TEXT)

(DEFCOM COM-INSERT-WARNINGS "Insert all the warnings about all files, at point.
Leaves the mark after the inserted text." ()
  (POINT-PDL-PUSH (POINT) *WINDOW* NIL NIL)
  (MOVE-BP (MARK) (POINT))
  (SETQ *CURRENT-COMMAND-TYPE* ':YANK)
  (DOLIST (PATHNAME (SI::WARNINGS-PATHNAMES))
    (INSERT-FILE-WARNINGS PATHNAME)
    (INSERT-MOVING (POINT) #/PAGE))
  (SWAP-BPS (MARK) (POINT))
  DIS-TEXT)

(DEFUN SET-BUFFER-CONTENTS-TO-WARNINGS (BUFFER FILES)
  "Make BUFFER contain precisely a list of warnings for FILES."
  (LET ((*INTERVAL* BUFFER))
    (DELETE-INTERVAL BUFFER))
  (SETQ *LAST-WARNINGS-BUFFER* BUFFER)
  (LET ((STREAM (INTERVAL-STREAM-INTO-BP (COPY-BP (INTERVAL-LAST-BP BUFFER)))))
    (DOLIST (FILE FILES)
      (PRINT-FILE-WARNINGS FILE STREAM)
      (WRITE-CHAR #/PAGE STREAM))))

(DEFUN INSERT-FILE-WARNINGS (GENERIC-PATHNAME
                             &AUX (STREAM (INTERVAL-STREAM-INTO-BP (COPY-BP (POINT)))))
  "Insert into *INTERVAL* a list of warnings for GENERIC-PATHNAME.
Leaves point at the end."
  (PRINT-FILE-WARNINGS GENERIC-PATHNAME STREAM)
  (MOVE-BP (POINT) (SEND STREAM :READ-BP)))

(DEFUN PRINT-FILE-WARNINGS (GENERIC-PATHNAME STREAM
                            &AUX (LINE-PUT-FLAG
                                   (OPERATION-HANDLED-P STREAM :LINE-PUT))
                            (*PACKAGE* SI:PKG-USER-PACKAGE))
  "Print a description of the warnings for GENERIC-PATHNAME onto STREAM.
If STREAM supports :LINE-PUT, we use it to attach properties to lines."
  (OR GENERIC-PATHNAME (SETQ GENERIC-PATHNAME T))
  (FORMAT STREAM "~&Warnings for ~A~%" (WARNINGS-PATHNAME-AS-STRING GENERIC-PATHNAME))
  ;; Print with the right value of *PACKAGE*, *PRINT-BASE*, etc.
  (MULTIPLE-VALUE-BIND (VARS VALS)
      (AND (NOT (SYMBOLP GENERIC-PATHNAME))
           (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME))
    (PROGV VARS VALS
      (SI::FILTER-WARNINGS GENERIC-PATHNAME)
      (DOLIST (OPERATION (SI::FILE-WARNINGS-OPERATIONS GENERIC-PATHNAME))
        (IF (EQ GENERIC-PATHNAME T)
            (FORMAT STREAM "~%Warnings for non-file ~A"
                    (STRING-PLURALIZE (GET OPERATION 'SI::NAME-AS-ACTION)))
          (FORMAT STREAM "~%Warnings for ~A of ~A~%"
                  (GET OPERATION 'SI::NAME-AS-ACTION)
                  (WARNINGS-PATHNAME-AS-STRING GENERIC-PATHNAME)))
        (DOLIST (OBJW (SI::FILE-WARNINGS-OBJECT-ALIST
                        (SI::EXAMINE-FILE-WARNINGS GENERIC-PATHNAME OPERATION)))
          (SI::PRINT-OBJECT-WARNINGS-HEADER STREAM (SI::OBJECT-WARNINGS-NAME OBJW) OPERATION)
          (WHEN LINE-PUT-FLAG
            (SEND STREAM :LINE-PUT ':OBJECT (SI::OBJECT-WARNINGS-NAME OBJW))
            (SEND STREAM :LINE-PUT ':WARNINGS (SI::COPY-OBJECT-WARNINGS OBJW))
            (SEND STREAM :LINE-PUT ':POSSIBILITY
                                   `(,(OR (SI::OBJECT-WARNINGS-LOCATION-FUNCTION OBJW)
                                          'EDIT-DEFINITION-POSSIBILITY)
                                     ,(SI::OBJECT-WARNINGS-NAME OBJW)
                                     ,(AND (NEQ GENERIC-PATHNAME T)
                                           GENERIC-PATHNAME)))
            (SEND STREAM :LINE-PUT ':LEVEL 0))
          (DOLIST (W (SI::OBJECT-WARNINGS-WARNINGS OBJW))
            (TERPRI STREAM)
            (WRITE-CHAR #/SPACE STREAM)
            (LET ((*PRINT-LENGTH* SI::WARNINGS-PRINLENGTH)
                  (*PRINT-LEVEL* SI::WARNINGS-PRINLEVEL))
              (APPLY #'FORMAT STREAM
                     (SI::WARNING-FORMAT-STRING W) (SI::WARNING-FORMAT-ARGS W)))
            (SEND STREAM :LINE-PUT ':WARNING W)))
        (TERPRI STREAM)))))
