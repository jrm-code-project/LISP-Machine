;;; -*- Mode:LISP; Package:ZWEI; Base:10; Readtable:ZL -*-


(DEFVAR *ALL-THINGS* '("FOO" "BAR" "BAZ"))
(DEFVAR *THINGP-FUNCTION* 'STRINGP)
(DEFVAR *FIND-THING-FUNCTION* #'IDENTITY)
(DEFVAR *THING-SOURCE-FILES-FUNCTION* NIL)


(DEFCOM COM-SELECT-THING-AS-TAG-TABLE "Make the files in a THING behave like a tags file"
        ()
  (LET ((THING-NAME (READ-THING-NAME "Thing to select as tag table:")))
    (SELECT-FILE-LIST-AS-TAG-TABLE
      (AND *THING-SOURCE-FILES-FUNCTION*
           (FUNCALL *THING-SOURCE-FILES-FUNCTION* THING-NAME))
      THING-NAME))
  DIS-NONE)





(DEFUN READ-THING-NAME (PROMPT)
  "Read a thing name in the mini buffer, defaulting to DEFAULT. Prompts with PROMPT."
  (LET ((THING-NAME (COMPLETING-READ-FROM-MINI-BUFFER PROMPT
                                                      (MAPCAR #'(LAMBDA (X)
                                                                  (CONS X X))
                                                              *ALL-THINGS*)
                                                      T)))
    (IF (CONSP THING-NAME) (SETQ THING-NAME (CDR THING-NAME)))
    (COND ((NOT (AND *THINGP-FUNCTION* (FUNCALL *THINGP-FUNCTION* THING-NAME)))
           (BARF))
          ('ELSE
           (CONDITION-CASE (X)
               (AND *FIND-THING-FUNCTION*
                    (FUNCALL *FIND-THING-FUNCTION* THING-NAME))
             (ERROR (BARF "~A" X)))))))
