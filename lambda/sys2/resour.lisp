;;;-*- Mode:LISP; Package:SYSTEM-INTERNALS; Base:10; Readtable:CL -*-
;;; ** (c) Copyright 1982 Massachusetts Institute of Technology **

;;;NOTE: The is quite a bit of confusion in terminlogy below wrt "FREE-LIST".
;;;  When the old code and documentation says FREE-LIST, it really means "Array which holds
;;;  all instances in existance.  Another component of the array has a flag which records
;;;  which instances are really free, etc".  The new FREE-LIST-CELL option indicates
;;;  that a new-style real (chained) free list is in use.

;;; New version of resource package, subsumes system-window facility
;;; Note that WITH-RESOURCE is obsolete because it takes its "arguments"
;;; in the wrong order.  It has been replaced by USING-RESOURCE.

;;; Old form of DEFRESOURCE:
;;;     (DEFRESOURCE [name | (name dont-make-initial-copy)] . creator-body)
;;; New form of DEFRESOURCE:
;;;     (DEFRESOURCE name parameters [docstring] keyword value keyword value ...)
;;;  Keywords are:
;;;     :CONSTRUCTOR form   (this is required)
;;;             Sees parameters as arguments.
;;;     :FINDER form
;;;             Sees resource-structure and parameters as arguments.
;;;     :MATCHER form
;;;             Sees OBJECT (in current package) and parameters as arguments.
;;;             Note that if the FREE-LIST-CELL option is in use, :MATCHER must be
;;;             supplied unless there are no parameters.
;;;     :CHECKER form
;;;             Sees OBJECT and IN-USE-P (in current package) and parameters as arguments.
;;;     :INITIALIZER form
;;;             Sees OBJECT and parameters as arguments.
;;;       In the above options starting with :CONSTRUCTOR, form may also be a symbol
;;;       which is a function to call.  It gets the resource data structure as its first
;;;       argument then the specified args.
;;;     :DEINITIALIZER form
;;;             Sees OBJECT as argument.  This was initially added to preserve storage
;;;             integrity and avoid unnecessary data retention by the GC, by storing NILs
;;;             into boxed Qs when an object was deallocated.  SI:WIPE-STRUCTURE is a
;;;             general function for setting all slots of a structure to NIL.
;;;     :FREE-LIST-CELL form
;;;        Giving this option specifies that objects are to be chained in a "real" free
;;;        list instead of the funny array otherwise used.
;;;             Sees OBJECT as argument.   Returns locative pointer to a cell (presumably
;;;             withing OBJECT) which is to be used for free-list chaining.
;;;        Note that the :MATCHER option should be used with the :FREE-LIST-CELL option
;;;        unless parameters is always to be NIL.  This is due to the fact the parameters are not
;;;        retained in the resource data structure for EQUAL testing as a default :MATCHER.
;;;      In the above two options, form may also be a symbol which is a function to call.
;;;      However, the args are always as specified; the resource data structure is never passed.
;;;     :INITIAL-COPIES number  (default 0)
;;;             If this is specified, all parameters must be &optional and
;;;             have suitable defaults.  This is generally a good idea anyway.
;;;             Specifying NIL here is the same as zero.
;;;     :FREE-LIST-SIZE number  (default 20.)
;;;             If this is specified, the size of the free-list for this resource
;;;             will initially be that number.
;;;  If :FINDER is specified, we keep no list of free objects and use :FINDER
;;;  to find a free one by looking through the general environment.
;;;  Otherwise we keep a table of objects and whether they are free.
;;;  If :MATCHER is specified, we use it to check them against the parameters.
;;;  Otherwise the a-list also includes the parameter values, which are checked
;;;  with EQUAL (not EQ).
;;;  If :CHECKER is specified, then it gets to pass on each object to decide whether
;;;  or not to reuse it, whether or not it is already marked as in-use.
;;;
;;;  The finder, matcher, and checker are called without-interrupts.
;;;
;;;  Possible features that might be added: ability to keep a free list threaded
;;;  through the objects.  Code to be run when something is deallocated, e.g.
;;;  to deactivate a window.
;;;
;;;  Note: for windows, you typically want to use DEFWINDOW-RESOURCE,
;;;  which supplies the right options to DEFRESOURCE.
;;;
;;; DEFRESOURCE no longer uses the value and function cells of the resource's name.
;;; It puts on a DEFRESOURCE property of the following defstruct.  Note: only the
;;; functions right here are "allowed" to know what is in this structure.

(ZL:DEFSTRUCT (RESOURCE (:TYPE :NAMED-ARRAY-LEADER) (:ALTERANT NIL)
                        :CONC-NAME)
  NAME                          ;Symbol which names it
  (N-OBJECTS 0)                 ;Total number of objects ever created
  PARAMETIZER                   ;Function which defaults the parameters and returns list
  CONSTRUCTOR                   ;Constructor function
  FINDER                        ;Optional finder function
  MATCHER                       ;Optional matcher function
  CHECKER                       ;Optional checker function
  INITIALIZER                   ;Optional initializer function
  DEINITIALIZER                 ;Optional deinitializer function
  FREE-LIST-CELL                ;Optional function, returns cell for free list chaining
  FREE-LIST)                    ;Available for use as real free list if FREE-LIST-CELL in use.

;;; The free list is the (n x 3) array itself, with the following fields:
(DEFSUBST RESOURCE-OBJECT (RESOURCE I) (CL:AREF RESOURCE I 0))
(DEFSUBST RESOURCE-IN-USE-P (RESOURCE I) (CL:AREF RESOURCE I 1))
(DEFSUBST RESOURCE-PARAMETERS (RESOURCE I) (CL:AREF RESOURCE I 2))

(defun resource-name-p (s)
  (and (symbolp s) (not (null (get s 'defresource)))))
(deftype resource-name ()
  "a symbol which is the name of a resource"
  `(satisfies resource-name-p))

(DEFSELECT ((:PROPERTY RESOURCE NAMED-STRUCTURE-INVOKE))
  (:DESCRIBE (RESOURCE &AUX (N-OBJECTS (RESOURCE-N-OBJECTS RESOURCE)))
    (DESCRIBE-DEFSTRUCT RESOURCE)
    (cond ((ZEROP N-OBJECTS)
           (FORMAT T "~&There are currently no objects.~%"))
          (t
           (FORMAT T "~&There ~[~;is~:;are~] currently ~:*~D object~:P:~@
                Object~40TParameters~60TIn Use"
                   N-OBJECTS)
           (unless (or (resource-finder resource) (resource-free-list-cell resource))
             (dotimes (i n-objects)
               (FORMAT T "~%~S~40T~S~60T~:[No~;Yes~]"
                       (RESOURCE-OBJECT RESOURCE I)
                       (RESOURCE-PARAMETERS RESOURCE I)
                       (RESOURCE-IN-USE-P RESOURCE I))))
           (terpri)))))

(DEFMACRO DEFRESOURCE (NAME PARAMETERS &REST OPTIONS)
  "Define a resource named NAME, with parameters PARAMETERS for constructing objects.
OPTIONS can specify how to create objects and how to tell when old objects
can be reused."
  (DECLARE (ARGLIST NAME PARAMETERS \[DOCUMENTATION-STRING\]
                    &KEY :CONSTRUCTOR :FINDER :MATCHER :CHECKER
                         :INITIALIZER :DEINITIALIZER
                         :FREE-LIST-CELL :INITIAL-COPIES :FREE-LIST-SIZE))
  (DECLARE (ZWEI:INDENTATION 2 1))
  (LET ((CONSTRUCTOR-FORM NIL) (FINDER-FORM NIL) (MATCHER-FORM NIL) (CHECKER-FORM NIL)
        (CONSTRUCTOR-FUNCTION NIL) (FINDER-FUNCTION NIL) (MATCHER-FUNCTION NIL)
        (PARAMETIZER-FUNCTION NIL) (CHECKER-FUNCTION NIL) (INITIAL-COPIES 0)
        (INITIALIZER-FORM NIL) (INITIALIZER-FUNCTION NIL)
        (DEINITIALIZER-FORM NIL) (DEINITIALIZER-FUNCTION NIL)
        (FREE-LIST-CELL-FORM NIL) (FREE-LIST-CELL-FUNCTION NIL) (FREE-LIST-SIZE 20.) (PARAMS NIL)
        (DOCUMENTATION NIL))
    (UNLESS (CL:LISTP PARAMETERS)
      (FERROR "~S invalid parameter list" PARAMETERS))
    (SETQ PARAMS (LOOP FOR P IN PARAMETERS
                       UNLESS (MEMQ P LAMBDA-LIST-KEYWORDS)
                     COLLECT (IF (SYMBOLP P) P (CAR P))))
    ;; if first option is a string, use it as documentation instead
    (WHEN (STRINGP (CAR OPTIONS))
      (SETQ DOCUMENTATION (POP OPTIONS)))
    (LOOP FOR (KEYWORD VALUE) ON OPTIONS BY 'CDDR
       DO (CASE KEYWORD
            (:CONSTRUCTOR (SETQ CONSTRUCTOR-FORM VALUE))
            (:FINDER (SETQ FINDER-FORM VALUE))
            (:MATCHER (SETQ MATCHER-FORM VALUE))
            (:CHECKER (SETQ CHECKER-FORM VALUE))
            (:INITIALIZER (SETQ INITIALIZER-FORM VALUE))
            (:DEINITIALIZER (SETQ DEINITIALIZER-FORM VALUE))
            (:FREE-LIST-CELL (SETQ FREE-LIST-CELL-FORM VALUE))
            (:INITIAL-COPIES
             (SETQ INITIAL-COPIES
                   (COND ((NULL VALUE) 0)
                         ((NUMBERP VALUE) VALUE)
                         (T (FERROR "~S ~S - number required" :INITIAL-VALUES VALUE)))))
            (:FREE-LIST-SIZE
             (SETQ FREE-LIST-SIZE
                   (COND ((NULL VALUE) 20.)
                         ((NUMBERP VALUE) VALUE)
                         (T (FERROR "~S ~S - number required" :FREE-LIST-SIZE FREE-LIST-SIZE)))))
               (OTHERWISE (FERROR "~S illegal option in ~S" KEYWORD 'DEFRESOURCE))))
    (OR CONSTRUCTOR-FORM (FERROR "~S requires the ~S option" 'DEFRESOURCE :CONSTRUCTOR))
    ;; Pick function names.  Note that NIL is SYMBOLP.
    (SETQ CONSTRUCTOR-FUNCTION (IF (SYMBOLP CONSTRUCTOR-FORM) CONSTRUCTOR-FORM
                                 `(:PROPERTY ,NAME RESOURCE-CONSTRUCTOR)))
    (SETQ FINDER-FUNCTION (IF (SYMBOLP FINDER-FORM) FINDER-FORM
                            `(:PROPERTY ,NAME RESOURCE-FINDER)))
    (SETQ MATCHER-FUNCTION (IF (SYMBOLP MATCHER-FORM) MATCHER-FORM
                             `(:PROPERTY ,NAME RESOURCE-MATCHER)))
    (SETQ CHECKER-FUNCTION (IF (SYMBOLP CHECKER-FORM) CHECKER-FORM
                             `(:PROPERTY ,NAME RESOURCE-CHECKER)))
    (SETQ INITIALIZER-FUNCTION (IF (SYMBOLP INITIALIZER-FORM) INITIALIZER-FORM
                                 `(:PROPERTY ,NAME RESOURCE-INITIALIZER)))
    (SETQ DEINITIALIZER-FUNCTION (IF (SYMBOLP DEINITIALIZER-FORM) DEINITIALIZER-FORM
                                 `(:PROPERTY ,NAME RESOURCE-DEINITIALIZER)))
    (SETQ FREE-LIST-CELL-FUNCTION (IF (SYMBOLP FREE-LIST-CELL-FORM) FREE-LIST-CELL-FORM
                                    `(:PROPERTY ,NAME RESOURCE-FREE-LIST-CELL)))
    (SETQ PARAMETIZER-FUNCTION (IF (AND PARAMETERS (NOT MATCHER-FORM) (NOT FINDER-FORM))
                                   `(:PROPERTY ,NAME RESOURCE-PARAMETIZER)))
    `(LOCAL-DECLARE ((SYS:FUNCTION-PARENT ,NAME DEFRESOURCE))
       ,(IF (NOT (SYMBOLP CONSTRUCTOR-FORM))
            `(DEFUN ,CONSTRUCTOR-FUNCTION (IGNORE ,@PARAMETERS)
               ,@PARAMS
               ,CONSTRUCTOR-FORM))
       ,(IF (NOT (SYMBOLP FINDER-FORM))
            `(DEFUN ,FINDER-FUNCTION (IGNORE ,@PARAMETERS)
               ,@PARAMS
               ,FINDER-FORM))
       ,(IF (NOT (SYMBOLP MATCHER-FORM))
            `(DEFUN ,MATCHER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
               ,@PARAMS
               ,MATCHER-FORM))
       ,(IF (NOT (SYMBOLP CHECKER-FORM))
            `(DEFUN ,CHECKER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
                                       ,@PARAMETERS)
               ,@PARAMS ,(INTERN "OBJECT") ,(INTERN "IN-USE-P")
               ,CHECKER-FORM))
       ,(IF (NOT (SYMBOLP INITIALIZER-FORM))
            `(DEFUN ,INITIALIZER-FUNCTION (IGNORE ,(INTERN "OBJECT") ,@PARAMETERS)
               ,@PARAMS ,(INTERN "OBJECT")
               ,INITIALIZER-FORM))
       ,(IF (NOT (SYMBOLP DEINITIALIZER-FORM))
            `(DEFUN ,DEINITIALIZER-FUNCTION (,(INTERN "OBJECT"))
               ,DEINITIALIZER-FORM))
       ,(IF (NOT (SYMBOLP FREE-LIST-CELL-FORM))
            `(DEFUN ,FREE-LIST-CELL-FUNCTION (,(INTERN "OBJECT"))
               ,FREE-LIST-CELL-FORM))
       ,(IF PARAMETIZER-FUNCTION
            `(DEFUN ,PARAMETIZER-FUNCTION ,PARAMETERS
               (LIST ,@PARAMS)))
       (INITIALIZE-RESOURCE ',NAME ',CONSTRUCTOR-FUNCTION ',FINDER-FUNCTION
                            ',MATCHER-FUNCTION ',CHECKER-FUNCTION
                            ',PARAMETIZER-FUNCTION ',INITIAL-COPIES ',FREE-LIST-SIZE
                            ',INITIALIZER-FUNCTION ',DEINITIALIZER-FUNCTION
                            ',FREE-LIST-CELL-FUNCTION)
       ,(IF DOCUMENTATION
          `(SET-DOCUMENTATION ',NAME 'RESOURCE ,DOCUMENTATION)))))

(DEFPROP DEFRESOURCE "Resource" DEFINITION-TYPE-NAME)

(DEFVAR *ALL-RESOURCES* NIL
  "List of all symbols that are names of DEFRESOURCEs.")

(DEFUN INITIALIZE-RESOURCE (NAME CONSTRUCTOR-FUNCTION FINDER-FUNCTION MATCHER-FUNCTION
                            CHECKER-FUNCTION PARAMETIZER-FUNCTION INITIAL-COPIES
                            ;; Keep this &OPTIONAL for the time being so old QFASLs work.
                            &OPTIONAL (FREE-LIST-SIZE 20.) INITIALIZER-FUNCTION
                            DEINITIALIZER-FUNCTION FREE-LIST-CELL-FUNCTION)
  (OR (SYMBOLP CONSTRUCTOR-FUNCTION)
      (SETQ CONSTRUCTOR-FUNCTION (GET (SECOND CONSTRUCTOR-FUNCTION)
                                      (THIRD CONSTRUCTOR-FUNCTION))))
  (OR (SYMBOLP FINDER-FUNCTION)
      (SETQ FINDER-FUNCTION (GET (SECOND FINDER-FUNCTION) (THIRD FINDER-FUNCTION))))
  (OR (SYMBOLP MATCHER-FUNCTION)
      (SETQ MATCHER-FUNCTION (GET (SECOND MATCHER-FUNCTION) (THIRD MATCHER-FUNCTION))))
  (OR (SYMBOLP CHECKER-FUNCTION)
      (SETQ CHECKER-FUNCTION (GET (SECOND CHECKER-FUNCTION) (THIRD CHECKER-FUNCTION))))
  (OR (SYMBOLP INITIALIZER-FUNCTION)
      (SETQ INITIALIZER-FUNCTION (GET (SECOND INITIALIZER-FUNCTION)
                                      (THIRD INITIALIZER-FUNCTION))))
  (OR (SYMBOLP DEINITIALIZER-FUNCTION)
      (SETQ DEINITIALIZER-FUNCTION (GET (SECOND DEINITIALIZER-FUNCTION)
                                        (THIRD DEINITIALIZER-FUNCTION))))
  (OR (SYMBOLP FREE-LIST-CELL-FUNCTION)
      (SETQ FREE-LIST-CELL-FUNCTION (GET (SECOND FREE-LIST-CELL-FUNCTION)
                                         (THIRD FREE-LIST-CELL-FUNCTION))))
  (OR (SYMBOLP PARAMETIZER-FUNCTION)
      (SETQ PARAMETIZER-FUNCTION (GET (SECOND PARAMETIZER-FUNCTION)
                                      (THIRD PARAMETIZER-FUNCTION))))
  (AND (RECORD-SOURCE-FILE-NAME NAME 'DEFRESOURCE)
       (LET ((OLD-RESOURCE (GET NAME 'DEFRESOURCE)) RESOURCE)
         ;; Be careful that there's enough room for all objects in the old resource
         ;; when replacing it.
         (AND OLD-RESOURCE (NOT FINDER-FUNCTION) (NOT FREE-LIST-CELL-FUNCTION)
              (SETQ FREE-LIST-SIZE (MAX (RESOURCE-N-OBJECTS OLD-RESOURCE)
                                        FREE-LIST-SIZE)))
         (AND (OR FINDER-FUNCTION FREE-LIST-CELL-FUNCTION) (SETQ FREE-LIST-SIZE 0))
         (SETQ RESOURCE (MAKE-RESOURCE :NAME NAME
                                       :MAKE-ARRAY (:LENGTH (LIST FREE-LIST-SIZE 3)
                                                    :AREA PERMANENT-STORAGE-AREA)
                                       :PARAMETIZER PARAMETIZER-FUNCTION
                                       :CONSTRUCTOR CONSTRUCTOR-FUNCTION
                                       :FINDER FINDER-FUNCTION
                                       :MATCHER MATCHER-FUNCTION
                                       :CHECKER CHECKER-FUNCTION
                                       :INITIALIZER INITIALIZER-FUNCTION
                                       :DEINITIALIZER DEINITIALIZER-FUNCTION
                                       :FREE-LIST-CELL FREE-LIST-CELL-FUNCTION))
         ;; Save any old objects when reloading a DEFRESOURCE
         (IF OLD-RESOURCE
             (COND ((and FREE-LIST-CELL-FUNCTION
                         (resource-free-list-cell old-resource))
                    (SETF (RESOURCE-N-OBJECTS RESOURCE)
                          (RESOURCE-N-OBJECTS OLD-RESOURCE))
                    (SETF (RESOURCE-FREE-LIST RESOURCE)
                          (RESOURCE-FREE-LIST OLD-RESOURCE)))
                   ((NOT FINDER-FUNCTION)
                    (COPY-ARRAY-CONTENTS OLD-RESOURCE RESOURCE)
                    (SETF (RESOURCE-N-OBJECTS RESOURCE)
                          (RESOURCE-N-OBJECTS OLD-RESOURCE)))))
         (SETF (GET NAME 'DEFRESOURCE) RESOURCE)
         (LOOP FOR OBJECT IN (LOOP REPEAT INITIAL-COPIES COLLECT (ALLOCATE-RESOURCE NAME))
            DO (DEALLOCATE-RESOURCE NAME OBJECT))))
  (PUSHNEW NAME *ALL-RESOURCES* :TEST #'EQ)
  NAME)

;Don't record this in qfasl files because it always does a RECORD-SOURCE-FILE-NAME.
(DEFPROP INITIALIZE-RESOURCE T QFASL-DONT-RECORD)

(DEFUN CLEAR-RESOURCE (RESOURCE-NAME &AUX RESOURCE)
  "Throw away all objects allocated from the resource RESOURCE-NAME.
This is useful if you discover they were all constructed wrong,
and you fix the constructor, to make sure newly constructed objects will be used."
  (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
  (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
  (WITHOUT-INTERRUPTS
    ;; Clear the actual cells so the old objects can be garbage collected immediately.
    (cond ((or (resource-finder resource) (resource-free-list-cell resource))
           (setf (resource-free-list resource) nil)
           (setf (resource-n-objects resource) 0))
          (t
           (DOTIMES (I (RESOURCE-N-OBJECTS RESOURCE))
             (WHEN (RESOURCE-IN-USE-P RESOURCE I)
               (FORMAT *ERROR-OUTPUT* "~%[Warning: ~S still in use]"
                       (RESOURCE-OBJECT RESOURCE I))
               (SETF (RESOURCE-OBJECT RESOURCE I) NIL)))
           (SETF (RESOURCE-N-OBJECTS RESOURCE) 0)))))

(DEFUN MAP-RESOURCE (FUNCTION RESOURCE-NAME &REST EXTRA-ARGS &AUX RESOURCE)
  "Call FUNCTION on each object created in resource RESOURCE-NAME.
FUNCTION gets three args at each call: the object, whether the resource
believes it is in use, and RESOURCE-NAME."
  (CHECK-TYPE RESOURCE-NAME resource-name "the name of a resource")
  (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
  ;; Windows are the user's problem....
  (unless (or (resource-finder resource) (resource-free-list-cell resource))
    (LOOP FOR I FROM 0 BELOW (RESOURCE-N-OBJECTS RESOURCE)
          FOR OBJECT = (RESOURCE-OBJECT RESOURCE I)
       WHEN OBJECT
         DO (APPLY FUNCTION OBJECT (RESOURCE-IN-USE-P RESOURCE I) RESOURCE-NAME EXTRA-ARGS))))

(DEFUN ALLOCATE-RESOURCE (RESOURCE-NAME &REST PARAMETERS
                          &AUX RESOURCE (PARAMS PARAMETERS)  ;Note PARAMS is UNSAFE!
                          TEM INDEX (OLD INHIBIT-SCHEDULING-FLAG) INITIALIZER)
  "Allocate an object from resource RESOURCE-NAME according to PARAMETERS.
An old object is reused if possible; otherwise a new one is created.
The significance of the PARAMETERS is determined by the individual resource."
 ;the following CHECK-TYPE is amazingly slow, assume anything with a DEFRESOURCE must be OK.
 ;  (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
  (cond ((null (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE)))
         (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
      ;The second try, it is a resource for sure
         (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))))
  (AND (SETQ TEM (RESOURCE-PARAMETIZER RESOURCE))
       (< (LENGTH PARAMS) (LDB %%ARG-DESC-MAX-ARGS (%ARGS-INFO TEM)))
       (SETQ PARAMS (APPLY TEM PARAMS)))
  (WITHOUT-INTERRUPTS
    (COND ((SETQ TEM (RESOURCE-FINDER RESOURCE))
           (SETQ TEM (APPLY TEM RESOURCE PARAMS)))
          ((RESOURCE-FREE-LIST-CELL RESOURCE)
           (DO ((CHECKER (RESOURCE-CHECKER RESOURCE))
                (MATCHER (RESOURCE-MATCHER RESOURCE))
                (CELL (LOCF (RESOURCE-FREE-LIST RESOURCE))
                      (FUNCALL (RESOURCE-FREE-LIST-CELL RESOURCE) (CONTENTS CELL))))
               ((NULL (CONTENTS CELL))
                ;; make new object.  PARAMS not copied since it is assumed they will not be stored.
                (SETQ INHIBIT-SCHEDULING-FLAG OLD)
                (SETQ TEM (APPLY (RESOURCE-CONSTRUCTOR RESOURCE) RESOURCE PARAMS))
                (SETQ INHIBIT-SCHEDULING-FLAG T))
             (LET ((OBJ (CONTENTS CELL)))
               (WHEN (AND (or (null CHECKER)
                              (APPLY CHECKER RESOURCE OBJ NIL PARAMS))  ;IN-USE-P NIL
                          (IF MATCHER
                              (APPLY MATCHER RESOURCE OBJ PARAMS)
                              (NULL PARAMS)))   ;PARAMS not retained.
                 (SETF (CONTENTS CELL) (CONTENTS (FUNCALL (RESOURCE-FREE-LIST-CELL RESOURCE) OBJ)))
                 (RETURN (SETQ TEM OBJ))))))
          ((LOOP WITH CHECKER = (RESOURCE-CHECKER RESOURCE)
                 WITH MATCHER = (RESOURCE-MATCHER RESOURCE)
                 WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
                 FOR N FROM (1- N-OBJECTS) DOWNTO 0
                 AS IN-USE-P = (RESOURCE-IN-USE-P RESOURCE N)
                 AS OBJ = (RESOURCE-OBJECT RESOURCE N)
              WHEN (AND (IF CHECKER
                            (APPLY CHECKER RESOURCE OBJ IN-USE-P PARAMS)
                          (NOT IN-USE-P))
                        (IF MATCHER (APPLY MATCHER RESOURCE OBJ PARAMS)
                          (OR (NULL PARAMS)
                              (EQUAL (RESOURCE-PARAMETERS RESOURCE N) PARAMS))))
                DO (SETF (RESOURCE-IN-USE-P RESOURCE N) T)
                   (RETURN (SETQ TEM OBJ))))
          (T (SETQ INHIBIT-SCHEDULING-FLAG OLD)
             (SETQ PARAMS (COPY-LIST PARAMS))
             (SETQ TEM (APPLY (RESOURCE-CONSTRUCTOR RESOURCE) RESOURCE PARAMS))
             (SETQ INHIBIT-SCHEDULING-FLAG T)
             (SETF (RESOURCE-N-OBJECTS RESOURCE)
                   (1+ (SETQ INDEX (RESOURCE-N-OBJECTS RESOURCE))))
             (WHEN ( INDEX (ARRAY-DIMENSION RESOURCE 0))
               (SETF (GET (RESOURCE-NAME RESOURCE) 'DEFRESOURCE)
                     (SETQ RESOURCE (ARRAY-GROW RESOURCE
                                                (+ INDEX (MAX 20. (TRUNCATE INDEX 2)))
                                                3))))
             (SETF (RESOURCE-OBJECT RESOURCE INDEX) TEM)
             (SETF (RESOURCE-IN-USE-P RESOURCE INDEX) T)
             (SETF (RESOURCE-PARAMETERS RESOURCE INDEX)                 ;Avoid lossage with
                   (IF (EQ PARAMS PARAMETERS) (COPY-LIST PARAMS)        ;as little consing
                     PARAMS)))))                                        ;as possible.
  ;; TEM now is the object
  (WHEN (SETQ INITIALIZER (RESOURCE-INITIALIZER RESOURCE))
    (APPLY INITIALIZER RESOURCE TEM PARAMS))
  TEM)

(DEFUN DEALLOCATE-RESOURCE (RESOURCE-NAME OBJECT &AUX RESOURCE)
  "Return OBJECT to the free pool of resource RESOURCE-NAME.
OBJECT should have been returned by a previous call to ALLOCATE-RESOURCE."
 ;Amazingly slow, see ALLOCATE-RESOURCE..
 ;  (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
  (cond ((null (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE)))
         (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
         (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))))
  (COND ((RESOURCE-DEINITIALIZER RESOURCE)
         (FUNCALL (RESOURCE-DEINITIALIZER RESOURCE) OBJECT)))
  (COND ((RESOURCE-FREE-LIST-CELL RESOURCE)
         (WITHOUT-INTERRUPTS
           (RPLACA (FUNCALL (RESOURCE-FREE-LIST-CELL RESOURCE) OBJECT)
                   (RESOURCE-FREE-LIST RESOURCE))
           (SETF (RESOURCE-FREE-LIST RESOURCE) OBJECT)))
        ((NOT (RESOURCE-FINDER RESOURCE))
         (LOOP WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
               FOR N FROM (1- N-OBJECTS) DOWNTO 0
            WHEN (EQ (RESOURCE-OBJECT RESOURCE N) OBJECT)
              ;; Note that this doesn't need any locking.
              DO (RETURN (SETF (RESOURCE-IN-USE-P RESOURCE N) NIL))
            FINALLY (FERROR "~S is not an object from the ~S resource"
                            OBJECT RESOURCE-NAME)))))

(DEFUN DEALLOCATE-WHOLE-RESOURCE (RESOURCE-NAME &AUX RESOURCE)
  "Return all objects allocated from resource RESOURCE-NAME to the free pool."
  (CHECK-TYPE RESOURCE-NAME RESOURCE-NAME "the name of a resource")
  (SETQ RESOURCE (GET RESOURCE-NAME 'DEFRESOURCE))
  (COND ((NOT (OR (RESOURCE-FINDER RESOURCE) (RESOURCE-FREE-LIST-CELL RESOURCE)))
         (LOOP WITH N-OBJECTS = (RESOURCE-N-OBJECTS RESOURCE)
               FOR N FROM 0 BELOW N-OBJECTS
            DO (SETF (RESOURCE-IN-USE-P RESOURCE N) NIL)))))

(DEFMACRO USING-RESOURCE (&ENVIRONMENT ENV (VAR RESOURCE-NAME &REST PARAMETERS) &BODY BODY)
  "Execute BODY with VAR bound to an object allocated from resource RESOURCE-NAME.
PARAMETERS are used in selecting or creating the object,
according to the definition of the resource."
  (CHECK-TYPE RESOURCE-NAME SYMBOL)
  (MULTIPLE-VALUE-BIND (BODY DECLARATIONS)
      (EXTRACT-DECLARATIONS BODY NIL NIL ENV)
    `(LET ((,VAR NIL))
       (DECLARE . ,DECLARATIONS)
       (UNWIND-PROTECT
           (PROGN
             (SETQ ,VAR (ALLOCATE-RESOURCE ',RESOURCE-NAME . ,PARAMETERS))
             . ,BODY)
         (AND ,VAR (DEALLOCATE-RESOURCE ',RESOURCE-NAME ,VAR))))))

;For compatibility with old programs
;(DEFF WITH-RESOURCE 'USING-RESOURCE)
