;;;-*- Mode:LISP; Package:ZWEI; Base:10; Readtable:CL -*-

;;; The file SYS:ZWEI;POSS.LISP already defines a version of these:
;;;
;;;    COM-LIST-OBJECT-USERS
;;;    COM-MULTIPLE-LIST-OBJECT-USERS
;;;    COM-EDIT-OBJECT-USERS
;;;    COM-MULTIPLE-EDIT-OBJECT-USERS
;;;    COM-COMPILE-FILE-OBJECT-USERS
;;;    COM-MULTIPLE-COMPILE-FILE-OBJECT-USERS


;;; Here's my version which displays the usage-type information for each
;;; caller we find, and also sorts the results.

(DEFCOM COM-SHOW-CALLERS
        "Show the callers of an object."
        ()
  (LET* ((OBJECT (READ-FUNCTION-NAME "Edit definition"
                                     (RELEVANT-FUNCTION-NAME (POINT) NIL NIL T)
                                     'AARRAY-OK))
         (CALLERS (CDAR (SI:FIND-USERS-OF-OBJECTS (LIST OBJECT)))))
    (COND ((NULL CALLERS)
           (FORMAT T "~&No callers of ~S found." OBJECT))
          (T
           (COMMAND-STORE 'COM-GO-TO-NEXT-TOP-LEVEL-POSSIBILITY #\C-. *ZMACS-COMTAB*)
           (FORMAT T "~&Callers of ~S:~%" OBJECT)
           (LET ((ITEMS (STABLE-SORTCAR
                          (LET ((*PACKAGE* NIL))
                            (MAPCAR
                              #'(LAMBDA (X)
                                  (LET ((OBJ (SECOND X)))
                                    (CONS (TYPECASE OBJ
                                            (PATHNAME (FORMAT NIL "File ~A" OBJ))
                                            (LIST (FORMAT NIL "Method ~S" OBJ))
                                            (T (FORMAT NIL "~:[Variable~*~;~@(~A~)~] ~S"
                                                       (FIRST X) (FIRST X) OBJ)))
                                          OBJ)))
                              CALLERS))
                          #'STRING-LESSP)))
             (SEND *STANDARD-OUTPUT* :ITEM-LIST 'FUNCTION-NAME ITEMS)
             (FORMAT T "~2&Type ~A to ~:[start editing these~;edit this~].~%"
                     (KEY-FOR-COMMAND-SET-C-. 'COM-GO-TO-NEXT-POSSIBILITY #\C-SH-P)
                     (= (LENGTH ITEMS) 1))
             (INSERT-DEFINITIONS-POSSIBILITIES
               (FORMAT NIL "~&Callers of ~A:~A" (PACKAGE-NAME (SYMBOL-PACKAGE OBJECT)) OBJECT)
               (MAPCAR #'CDR ITEMS))))))
  DIS-NONE)

(SET-COMTAB *ZMACS-COMTAB* NIL (MAKE-COMMAND-ALIST '(COM-SHOW-CALLERS)))
