;;; -*- Mode:LISP; Package:SYSTEM-INTERNALS; Readtable:ZL; Base:10 -*-


(DEFUN MAP-FORMS-IN-FILE (F FILE-NAME)
  "Call the function F on each form in the file"
  (WITH-OPEN-STREAM (STREAM (SEND (FS:MERGE-PATHNAME-DEFAULTS
                                    FILE-NAME FS:LOAD-PATHNAME-DEFAULTS NIL)
                                  :OPEN-CANONICAL-DEFAULT-TYPE :LISP
                                  :ERROR :REPROMPT))
    (MAP-FORMS-IN-STREAM F STREAM)))

(DEFUN MAP-FORMS-IN-STREAM (F *STANDARD-INPUT*)
  (LET* ((FILE-ID (SEND *STANDARD-INPUT* :INFO))
         (PATHNAME (SEND *STANDARD-INPUT* :PATHNAME))
         (GENERIC-PATHNAME (SEND PATHNAME :GENERIC-PATHNAME))
         (*PACKAGE* *PACKAGE*))
    (BINDING-INTERPRETER-ENVIRONMENT (())
      (FS:READ-ATTRIBUTE-LIST GENERIC-PATHNAME *STANDARD-INPUT*)
      ;; Enter appropriate environment for the file
      (MULTIPLE-VALUE-BIND (VARS VALS)
          (FS:FILE-ATTRIBUTE-BINDINGS GENERIC-PATHNAME)
        (PROGV VARS VALS
          (DO ((FORM)
               (EOF (LIST NIL)))
              ((EQ (SETQ FORM (FUNCALL (OR *READFILE-READ-FUNCTION* #'READ)
                                       *STANDARD-INPUT*
                                       EOF))
                   EOF))
            (FUNCALL F FORM)))))))


(DEFUN PRINT-TOPLEVEL-ATOMS (FILE)
  (MAP-FORMS-IN-FILE #'(LAMBDA (X)
                         (IF (ATOM X) (PRINT X)))
                     FILE))
